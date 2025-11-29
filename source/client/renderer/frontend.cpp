/*
Copyright (C) 2007 Victor Luchits

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include "local.h"
#include "frontend.h"
#include "materiallocal.h"
#include "backendstate.h"
#include "backendactiontape.h"
#include "texturemanagement.h"
#include <common/helpers/singletonholder.h>
#include <common/helpers/links.h>
#include <common/facilities/profilerscope.h>
#include <common/facilities/syspublic.h>

namespace wsw {

auto RendererFrontend::getDefaultFarClip( const refdef_s *fd ) const -> float {
	float dist;

	if( fd->rdflags & RDF_NOWORLDMODEL ) {
		dist = 1024;
	} else if( rsh.worldModel && rsh.worldBrushModel->globalfog ) {
		dist = rsh.worldBrushModel->globalfog->shader->fog_dist;
	} else {
		// TODO: Restore computations of world bounds
		dist = (float)( 1 << 16 );
	}

	return wsw::max( Z_NEAR, dist ) + Z_BIAS;
}

auto RendererFrontend::getFogForBounds( const StateForCamera *stateForCamera, const float *mins, const float *maxs ) -> mfog_t * {
	if( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) {
		return nullptr;
	}
	if( !rsh.worldModel || !rsh.worldBrushModel || !rsh.worldBrushModel->numfogs ) {
		return nullptr;
	}
	if( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) {
		return nullptr;
	}
	if( rsh.worldBrushModel->globalfog ) {
		return rsh.worldBrushModel->globalfog;
	}
	for( unsigned i = 0; i < rsh.worldBrushModel->numfogs; i++ ) {
		mfog_t *const fog = rsh.worldBrushModel->fogs;
		if( fog->shader ) {
			if( BoundsIntersect( mins, maxs, fog->mins, fog->maxs ) ) {
				return fog;
			}
		}
	}

	return nullptr;
}

auto RendererFrontend::getFogForSphere( const StateForCamera *stateForCamera, const vec3_t centre, const float radius ) -> mfog_t * {
	vec3_t mins, maxs;
	for( unsigned i = 0; i < 3; i++ ) {
		mins[i] = centre[i] - radius;
		maxs[i] = centre[i] + radius;
	}
	return getFogForBounds( stateForCamera, mins, maxs );
}

void RendererFrontend::enter2DMode( SimulatedBackendState *backendState, int width, int height ) {
	assert( !rf.in2D );
	assert( width > 0 && height > 0 );

	rf.width2D  = width;
	rf.height2D = height;

	mat4_t projectionMatrix;

	Matrix4_OrthogonalProjection( 0, width, height, 0, -99999, 99999, projectionMatrix );

	backendState->setScissor( 0, 0, width, height );
	backendState->setViewport( 0, 0, width, height );

	backendState->loadProjectionMatrix( projectionMatrix );
	backendState->loadCameraMatrix( mat4x4_identity );
	backendState->loadObjectMatrix( mat4x4_identity );

	backendState->setShaderStateMask( ~0, GLSTATE_NO_DEPTH_TEST );

	backendState->setRenderFlags( 0 );

	beginAddingAuxiliaryDynamicMeshes( UPLOAD_GROUP_2D_MESH );

	rf.in2D = true;
}

void RendererFrontend::leave2DMode( SimulatedBackendState *backendState ) {
	assert( rf.in2D );

	// render previously batched 2D geometry, if any
	// TODO: Call this explicitly
	flushAuxiliaryDynamicMeshes( UPLOAD_GROUP_2D_MESH, backendState );

	// TODO: Should we care? Are we going to continue using the backend state this frame?
	backendState->setShaderStateMask( ~0, 0 );

	rf.in2D = false;
}

void RendererFrontend::set2DScissor( SimulatedBackendState *backendState, int x, int y, int w, int h ) {
	assert( rf.in2D );
	backendState->setScissor( x, y, w, h );
}

void RendererFrontend::bindRenderTargetAndViewport( SimulatedBackendState *backendState,
													RenderTargetComponents *components,
													const StateForCamera *stateForCamera ) {
	// TODO: This is for the default render target
	const int width  = components ? components->texture->width : glConfig.width;
	const int height = components ? components->texture->height : glConfig.height;

	rf.frameBufferWidth  = width;
	rf.frameBufferHeight = height;

	if( backendState ) {
		backendState->bindRenderTarget( components );
	} else {
		assert( components == nullptr );
		R_BindRenderTarget( nullptr );
	}

	if( stateForCamera ) {
		backendState->setViewport( stateForCamera->viewport[0], stateForCamera->viewport[1], stateForCamera->viewport[2], stateForCamera->viewport[3] );
		backendState->setScissor( stateForCamera->scissor[0], stateForCamera->scissor[1], stateForCamera->scissor[2], stateForCamera->scissor[3] );
	} else {
		// TODO????
		if( backendState ) {
			backendState->setViewport( 0, 0, glConfig.width, glConfig.height );
			backendState->setScissor( 0, 0, glConfig.width, glConfig.height );
		}
	}
}

void RendererFrontend::beginDrawingScenes() {
	m_mainThreadChecker.checkCallingThread();
	R_ClearSkeletalCache();
	recycleFrameCameraStates();
	m_drawSceneFrame++;
	m_cameraIndexCounter = 0;
	m_sceneIndexCounter = 0;
	m_tmpPortalScenesAndStates.clear();
}

auto RendererFrontend::createDrawSceneRequest( const refdef_t &refdef ) -> DrawSceneRequest * {
	m_mainThreadChecker.checkCallingThread();
	return new( m_drawSceneRequestsHolder.unsafe_grow_back() )DrawSceneRequest( refdef, m_sceneIndexCounter++ );
}

auto RendererFrontend::coBeginProcessingDrawSceneRequests( CoroTask::StartInfo si, RendererFrontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask {
	std::pair<Scene *, StateForCamera *> scenesAndCameras[kMaxDrawSceneRequests];
	unsigned numScenesAndCameras = 0;

	for( DrawSceneRequest *request: requests ) {
		if( StateForCamera *const stateForSceneCamera = self->setupStateForCamera( &request->m_refdef, request->m_index ) ) {
			if( stateForSceneCamera->refdef.minLight < 0.1f ) {
				stateForSceneCamera->refdef.minLight = 0.1f;
			}
			request->stateForCamera = stateForSceneCamera;
			scenesAndCameras[numScenesAndCameras++] = std::pair<Scene *, StateForCamera *> { request, stateForSceneCamera };
		}
	}

	const std::span<std::pair<Scene *, StateForCamera *>> spanOfScenesAndCameras { scenesAndCameras, numScenesAndCameras };
	co_await si.taskSystem->awaiterOf( self->beginPreparingRenderingFromTheseCameras( spanOfScenesAndCameras, false ) );
}

auto RendererFrontend::coEndProcessingDrawSceneRequests( CoroTask::StartInfo si, RendererFrontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask {
	std::pair<Scene *, StateForCamera *> scenesAndCameras[kMaxDrawSceneRequests];
	unsigned numScenesAndCameras = 0;

	for( DrawSceneRequest *request: requests ) {
		if( auto *const stateForSceneCamera = (StateForCamera *)request->stateForCamera ) {
			scenesAndCameras[numScenesAndCameras++] = std::pair<Scene *, StateForCamera *> { request, stateForSceneCamera };
		}
	}

	// Note: Supplied dependencies are empty as they are taken in account during spawning of the coro instead
	co_await si.taskSystem->awaiterOf( self->endPreparingRenderingFromTheseCameras(
		{ scenesAndCameras, scenesAndCameras + numScenesAndCameras }, {}, false ) );
}

auto RendererFrontend::beginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle {
	// Don't pin it to the main thread as the CGame coroutine is already pinned to the main thread
	CoroTask::StartInfo si { &m_taskSystem, {}, CoroTask::AnyThread };
	// Note: passing the span should be safe as it resides in cgame code during task system execution
	return m_taskSystem.addCoro( [=, this]() { return coBeginProcessingDrawSceneRequests( si, this, requests ); } );
}

auto RendererFrontend::endProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests, std::span<const TaskHandle> dependencies ) -> TaskHandle {
	CoroTask::StartInfo si { &m_taskSystem, dependencies, CoroTask::OnlyMainThread };
	// Note: passing the span should be safe as it resides in cgame code during task system execution
	return m_taskSystem.addCoro( [=, this]() { return coEndProcessingDrawSceneRequests( si, this, requests ); } );
}

void RendererFrontend::commitProcessedDrawSceneRequest( DrawSceneRequest *request ) {
	if( auto *const stateForSceneCamera = (StateForCamera *)request->stateForCamera ) {
		performPreparedRenderingFromThisCamera( request, stateForSceneCamera );
	} else {
		// TODO what to do
	}
}

void RendererFrontend::endDrawingScenes() {
	m_mainThreadChecker.checkCallingThread();
	bindRenderTargetAndViewport( nullptr, nullptr, nullptr );
	recycleFrameCameraStates();
	m_drawSceneRequestsHolder.clear();
}

auto RendererFrontend::createDraw2DRequest() -> Draw2DRequest * {
	m_mainThreadChecker.checkCallingThread();
	assert( !m_isDraw2DRequestInUse );
	assert( m_draw2DRequest.m_cmds.empty() );
	m_isDraw2DRequestInUse = true;
	return &m_draw2DRequest;
}

void RendererFrontend::commitDraw2DRequest( Draw2DRequest *request ) {
	m_mainThreadChecker.checkCallingThread();

	BackendActionTape actionTape;

	SimulatedBackendState backendState( &actionTape, glConfig.width, glConfig.height );
	RB_BeginUsingBackendState( &backendState );

	enter2DMode( &backendState, glConfig.width, glConfig.height );
	set2DScissor( &backendState, 0, 0, glConfig.width, glConfig.height );
	for( const auto &cmd: request->m_cmds ) {
		if( const auto *drawPicCmd = std::get_if<Draw2DRequest::DrawPicCmd>( &cmd ) ) {
			submitRotatedStretchPic( &backendState, drawPicCmd->x, drawPicCmd->y, drawPicCmd->w, drawPicCmd->h,
									 drawPicCmd->s1, drawPicCmd->t1, drawPicCmd->s2, drawPicCmd->t2,
									 drawPicCmd->angle, drawPicCmd->color, drawPicCmd->shader );
		} else if( const auto *setScissorCmd = std::get_if<Draw2DRequest::SetScissorCmd>( &cmd ) ) {
			set2DScissor( &backendState, setScissorCmd->x, setScissorCmd->y, setScissorCmd->w, setScissorCmd->h );
		} else {
			wsw::failWithRuntimeError( "Unreachable" );
		}
	}
	// TODO .... Flush dynamic meshes explicitly
	leave2DMode( &backendState );
	RB_EndUsingBackendState( &backendState );

	RuntimeBackendState rbs {};
	actionTape.exec( &rbs );
}

void RendererFrontend::recycleDraw2DRequest( Draw2DRequest *request ) {
	m_mainThreadChecker.checkCallingThread();
	assert( m_isDraw2DRequestInUse );
	assert( request == &m_draw2DRequest );
	request->m_cmds.clear();
	m_isDraw2DRequestInUse = false;
}

RendererFrontend::RendererFrontend() : m_taskSystem( { .profilingGroup  = wsw::ProfilingSystem::ClientGroup,
									   .numExtraThreads = suggestNumExtraWorkerThreads( {} ) } ) {
	m_mainThreadChecker.markCurrentThreadForFurtherAccessChecks();
	const auto features = Sys_GetProcessorFeatures();
	if( Q_CPU_FEATURE_SSE41 & features ) {
		m_collectVisibleWorldLeavesArchMethod = &RendererFrontend::collectVisibleWorldLeavesSse41;
		m_collectVisibleOccludersArchMethod   = &RendererFrontend::collectVisibleOccludersSse41;
		m_buildFrustaOfOccludersArchMethod    = &RendererFrontend::buildFrustaOfOccludersSse41;
		if( Q_CPU_FEATURE_AVX & features ) {
			m_cullSurfacesByOccludersArchMethod = &RendererFrontend::cullSurfacesByOccludersAvx;
		} else {
			m_cullSurfacesByOccludersArchMethod = &RendererFrontend::cullSurfacesByOccludersSse41;
		}
		m_cullEntriesWithBoundsArchMethod   = &RendererFrontend::cullEntriesWithBoundsSse41;
		m_cullEntryPtrsWithBoundsArchMethod = &RendererFrontend::cullEntryPtrsWithBoundsSse41;
	} else {
		m_collectVisibleWorldLeavesArchMethod = &RendererFrontend::collectVisibleWorldLeavesSse2;
		m_collectVisibleOccludersArchMethod   = &RendererFrontend::collectVisibleOccludersSse2;
		m_buildFrustaOfOccludersArchMethod    = &RendererFrontend::buildFrustaOfOccludersSse2;
		m_cullSurfacesByOccludersArchMethod   = &RendererFrontend::cullSurfacesByOccludersSse2;
		m_cullEntriesWithBoundsArchMethod     = &RendererFrontend::cullEntriesWithBoundsSse2;
		m_cullEntryPtrsWithBoundsArchMethod   = &RendererFrontend::cullEntryPtrsWithBoundsSse2;
	}
}

RendererFrontend::~RendererFrontend() {
	m_mainThreadChecker.checkCallingThread();
	disposeCameraStates();
}

alignas( 32 ) static SingletonHolder<RendererFrontend> sceneInstanceHolder;

void RendererFrontend::init() {
	sceneInstanceHolder.init();
}

void RendererFrontend::shutdown() {
	sceneInstanceHolder.shutdown();
}

RendererFrontend *RendererFrontend::instance() {
	return sceneInstanceHolder.instance();
}

auto RendererFrontend::getMiniviewRenderTarget() -> RenderTargetComponents * {
	return TextureCache::instance()->getMiniviewRenderTarget();
}

void RendererFrontend::initVolatileAssets() {
	m_coronaShader = MaterialCache::instance()->loadDefaultMaterial( "$corona"_asView, SHADER_TYPE_CORONA );
}

void RendererFrontend::destroyVolatileAssets() {
	m_coronaShader = nullptr;
	disposeCameraStates();
}

auto RendererFrontend::allocStateForCamera() -> StateForCamera * {
	StateForCameraStorage *resultStorage = nullptr;
	do {
		// Portal drawing code may perform concurrent allocation of states
		[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_stateAllocLock );
		if( m_cameraIndexCounter >= MAX_REF_CAMERAS ) [[unlikely]] {
			return nullptr;
		}
		if( m_freeStatesForCamera ) {
			resultStorage = wsw::unlink( m_freeStatesForCamera, &m_freeStatesForCamera );
		} else {
			try {
				resultStorage = new StateForCameraStorage;
			} catch( ... ) {
				return nullptr;
			}
		}
		wsw::link( resultStorage, &m_usedStatesForCamera );
	} while( false );

	assert( !resultStorage->isStateConstructed );

	auto *stateForCamera = new( resultStorage->theStateStorage )StateForCamera;
	resultStorage->isStateConstructed = true;

	stateForCamera->shaderParamsStorage = &resultStorage->shaderParamsStorage;
	stateForCamera->materialParamsStorage = &resultStorage->materialParamsStorage;
	stateForCamera->shaderParamsStorage->clear();
	stateForCamera->materialParamsStorage->clear();

	stateForCamera->sortList       = &resultStorage->meshSortList;
	stateForCamera->drawActionTape = &resultStorage->drawActionTape;

	stateForCamera->preparePolysWorkload     = &resultStorage->preparePolysWorkloadBuffer;
	stateForCamera->prepareCoronasWorkload   = &resultStorage->prepareCoronasWorkloadBuffer;
	stateForCamera->prepareParticlesWorkload = &resultStorage->prepareParticlesWorkloadBuffer;
	stateForCamera->batchedSurfVertSpans     = &resultStorage->batchedSurfVertSpansBuffer;

	stateForCamera->prepareSpritesWorkload            = &resultStorage->prepareSpritesWorkloadBuffer;
	stateForCamera->preparedSpriteVertElemAndVboSpans = &resultStorage->spriteVertElemAndVboSpansBuffer;

	stateForCamera->visibleLeavesBuffer            = &resultStorage->visibleLeavesBuffer;
	stateForCamera->visibleOccludersBuffer         = &resultStorage->visibleOccludersBuffer;
	stateForCamera->sortedOccludersBuffer          = &resultStorage->sortedOccludersBuffer;
	stateForCamera->leafSurfTableBuffer            = &resultStorage->leafSurfTableBuffer;
	stateForCamera->leafSurfNumsBuffer             = &resultStorage->leafSurfNumsBuffer;
	stateForCamera->drawSurfSurfSpansBuffer        = &resultStorage->drawSurfSurfSpansBuffer;
	stateForCamera->bspDrawSurfacesBuffer          = &resultStorage->bspDrawSurfacesBuffer;
	stateForCamera->surfVisTableBuffer             = &resultStorage->bspSurfVisTableBuffer;
	stateForCamera->drawSurfSurfSubspansBuffer     = &resultStorage->drawSurfSurfSubspansBuffer;
	stateForCamera->drawSurfMultiDrawIndicesBuffer = &resultStorage->drawSurfMultiDrawIndicesBuffer;
	stateForCamera->drawSurfMultiDrawCountsBuffer  = &resultStorage->drawSurfMultiDrawCountsBuffer;
	stateForCamera->visTestedModelsBuffer          = &resultStorage->visTestedModelsBuffer;
	stateForCamera->leafLightBitsOfSurfacesBuffer  = &resultStorage->leafLightBitsOfSurfacesBuffer;

	resultStorage->particleDrawSurfacesBuffer.reserve( Scene::kMaxParticleAggregates * Scene::kMaxParticlesInAggregate );
	stateForCamera->particleDrawSurfaces = resultStorage->particleDrawSurfacesBuffer.get();

	resultStorage->dynamicMeshDrawSurfacesBuffer.reserve( Scene::kMaxCompoundDynamicMeshes * Scene::kMaxPartsInCompoundMesh );
	stateForCamera->dynamicMeshDrawSurfaces = resultStorage->dynamicMeshDrawSurfacesBuffer.get();
	assert( stateForCamera->numDynamicMeshDrawSurfaces == 0 );

	resultStorage->lightSpansForParticleAggregatesBuffer.resize( Scene::kMaxParticleAggregates );
	stateForCamera->lightSpansForParticleAggregates   = resultStorage->lightSpansForParticleAggregatesBuffer.data();
	stateForCamera->lightIndicesForParticleAggregates = &resultStorage->lightIndicesForParticleAggregatesBuffer;

	stateForCamera->debugLines = &resultStorage->debugLinesBuffer;

	return stateForCamera;
}

void RendererFrontend::recycleFrameCameraStates() {
	for( StateForCameraStorage *used = m_usedStatesForCamera, *next; used; used = next ) { next = used->next;
		wsw::unlink( used, &m_usedStatesForCamera );
		( (StateForCamera *)used->theStateStorage )->~StateForCamera();
		used->isStateConstructed = false;
		wsw::link( used, &m_freeStatesForCamera );
	}
	assert( m_usedStatesForCamera == nullptr );
}

void RendererFrontend::disposeCameraStates() {
	recycleFrameCameraStates();
	assert( m_usedStatesForCamera == nullptr );
	for( StateForCameraStorage *storage = m_freeStatesForCamera, *next; storage; storage = next ) { next = storage->next;
		wsw::unlink( storage, &m_freeStatesForCamera );
		delete storage;
	}
	assert( m_freeStatesForCamera == nullptr );
}

auto RendererFrontend::setupStateForCamera( const refdef_t *fd, unsigned sceneIndex,
									std::optional<CameraOverrideParams> overrideParams ) -> StateForCamera * {
	auto *const stateForCamera = allocStateForCamera();
	if( !stateForCamera ) [[unlikely]] {
		return nullptr;
	}

	stateForCamera->refdef      = *fd;
	stateForCamera->farClip     = getDefaultFarClip( fd );
	stateForCamera->cameraId    = m_cameraIdCounter++;
	stateForCamera->sceneIndex  = sceneIndex;

	stateForCamera->renderFlags = 0;
	if( v_lightmap.get() ) {
		stateForCamera->renderFlags |= RF_LIGHTMAP;
	}

	if( v_drawFlat.get() ) {
		stateForCamera->renderFlags |= RF_DRAWFLAT;
	}

	if( fd->rdflags & RDF_DRAWBRIGHT ) {
		stateForCamera->renderFlags |= RF_DRAWBRIGHT;
	}

	if( overrideParams ) {
		stateForCamera->renderFlags |= overrideParams->renderFlagsToAdd;
		stateForCamera->renderFlags &= ~overrideParams->renderFlagsToClear;
	}

	VectorCopy( stateForCamera->refdef.vieworg, stateForCamera->viewOrigin );
	Matrix3_Copy( stateForCamera->refdef.viewaxis, stateForCamera->viewAxis );

	stateForCamera->fovLodScale = std::tan( stateForCamera->refdef.fov_x * ( M_PI / 180 ) * 0.5f );
	if( fd->rdflags & RDF_USEAUTOLODSCALE ) {
		stateForCamera->viewLodScale = wsw::max( fd->width / (float)rf.width2D, fd->height / (float)rf.height2D );
		assert( stateForCamera->viewLodScale > 0.0f && stateForCamera->viewLodScale <= 1.0f );
	} else {
		assert( stateForCamera->viewLodScale == 1.0f );
	}

	Vector4Set( stateForCamera->scissor, fd->scissor_x, fd->scissor_y, fd->scissor_width, fd->scissor_height );
	Vector4Set( stateForCamera->viewport, fd->x, fd->y, fd->width, fd->height );

	if( overrideParams && overrideParams->pvsOrigin ) {
		VectorCopy( overrideParams->pvsOrigin, stateForCamera->pvsOrigin );
	} else {
		VectorCopy( fd->vieworg, stateForCamera->pvsOrigin );
	}
	if( overrideParams && overrideParams->lodOrigin ) {
		VectorCopy( overrideParams->lodOrigin, stateForCamera->lodOrigin );
	} else {
		VectorCopy( fd->vieworg, stateForCamera->lodOrigin );
	}

	stateForCamera->numPortalSurfaces      = 0;

	Matrix4_Modelview( fd->vieworg, fd->viewaxis, stateForCamera->cameraMatrix );

	if( fd->rdflags & RDF_USEORTHO ) {
		Matrix4_OrthogonalProjection( -fd->ortho_x, fd->ortho_x, -fd->ortho_y, fd->ortho_y,
									  -stateForCamera->farClip,
									  +stateForCamera->farClip,
									  stateForCamera->projectionMatrix );
	} else {
		Matrix4_PerspectiveProjection( fd->fov_x, fd->fov_y, Z_NEAR, stateForCamera->farClip,
									   stateForCamera->projectionMatrix );
	}

	Matrix4_Multiply( stateForCamera->projectionMatrix,
					  stateForCamera->cameraMatrix,
					  stateForCamera->cameraProjectionMatrix );

	bool shouldDrawWorldModel = false;
	if( !( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
		if( rsh.worldModel && rsh.worldBrushModel ) {
			shouldDrawWorldModel = true;
		}
	}

	stateForCamera->sortList->clear();
	if( shouldDrawWorldModel ) {
		stateForCamera->sortList->reserve( rsh.worldBrushModel->numMergedSurfaces );
	}

	if( shouldDrawWorldModel ) {
		const mleaf_t *const leaf   = Mod_PointInLeaf( stateForCamera->pvsOrigin, rsh.worldModel );
		stateForCamera->viewCluster = leaf->cluster;
		stateForCamera->viewArea    = leaf->area;
	} else {
		stateForCamera->viewCluster = -1;
		stateForCamera->viewArea    = -1;
	}

	// TODO: Add capping planes
	stateForCamera->frustum.setupFor4Planes( fd->vieworg, fd->viewaxis, fd->fov_x, fd->fov_y );

	return stateForCamera;
}

auto RendererFrontend::coExecPassUponInitialCullingOfLeaves( CoroTask::StartInfo si, RendererFrontend *self, StateForCamera *stateForCamera ) -> CoroTask {
	std::span<const unsigned> surfNumsSpan;
	if( stateForCamera->useWorldBspOcclusionCulling ) {
		std::span<const unsigned> leavesInFrustumAndPvs = stateForCamera->leavesInFrustumAndPvs;
		if( !leavesInFrustumAndPvs.empty() ) {
			const unsigned numAllSurfaces = rsh.worldBrushModel->numModelSurfaces;

			// We preallocate it earlier so this is actually a memset call
			uint8_t *__restrict leafSurfTable = stateForCamera->leafSurfTableBuffer->reserveZeroedAndGet( numAllSurfaces );

			auto fillTableFn = [=]( unsigned, unsigned start, unsigned end ) {
				const auto leaves = rsh.worldBrushModel->visleafs;
				for( unsigned index = start; index < end; ++index ) {
					const auto *leaf                        = leaves[leavesInFrustumAndPvs[index]];
					const unsigned *__restrict leafSurfaces = leaf->visSurfaces;
					const unsigned numLeafSurfaces          = leaf->numVisSurfaces;
					unsigned surfIndex = 0;
					do {
						leafSurfTable[leafSurfaces[surfIndex]] = 1;
					} while( ++surfIndex < numLeafSurfaces );
				}
			};

			TaskHandle fillTask = si.taskSystem->addForSubrangesInRange( { 0, leavesInFrustumAndPvs.size() }, 48,
																		 std::span<const TaskHandle> {},
																		 std::move( fillTableFn ) );
			co_await si.taskSystem->awaiterOf( fillTask );

			// The left-pack can be faster with simd, but it is not a bottleneck - we have to wait for occluders anyway

			unsigned *__restrict surfNums = stateForCamera->leafSurfNumsBuffer->get();
			unsigned numSurfNums = 0;
			unsigned surfNum     = 0;
			do {
				surfNums[numSurfNums] = surfNum;
				numSurfNums += leafSurfTable[surfNum];
			} while( ++surfNum < numAllSurfaces );

			surfNumsSpan = { surfNums, numSurfNums };
		}
	} else {
		// We aren't even going to use occluders for culling world surfaces.
		// Mark surfaces of leaves in the primary frustum visible in this case.
		MergedSurfSpan *const mergedSurfSpans = stateForCamera->drawSurfSurfSpansBuffer->get();
		uint8_t *const surfVisTable           = stateForCamera->surfVisTableBuffer->get();
		self->markSurfacesOfLeavesAsVisible( stateForCamera->leavesInFrustumAndPvs, mergedSurfSpans, surfVisTable );
	}

	stateForCamera->surfsInFrustumAndPvs = surfNumsSpan;
}

auto RendererFrontend::coExecPassUponPreparingOccluders( CoroTask::StartInfo si, RendererFrontend *self, StateForCamera *stateForCamera ) -> CoroTask {
	const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };

	// Otherwise, we have marked surfaces of leaves immediately upon initial frustum culling of leaves
	if( stateForCamera->useWorldBspOcclusionCulling ) {
		MergedSurfSpan *const mergedSurfSpans   = stateForCamera->drawSurfSurfSpansBuffer->get();
		uint8_t *const surfVisTable             = stateForCamera->surfVisTableBuffer->get();
		// We were aiming to use occluders, but did not manage to build any
		if( occluderFrusta.empty() ) [[unlikely]] {
			// Just mark every surface that falls into the primary frustum visible in this case.
			self->markSurfacesOfLeavesAsVisible( stateForCamera->leavesInFrustumAndPvs, mergedSurfSpans, surfVisTable );
		} else {
			const std::span<const unsigned> surfNums = stateForCamera->surfsInFrustumAndPvs;
			if( !surfNums.empty() ) {
				std::span<const Frustum> bestFrusta( occluderFrusta.data(), wsw::min<size_t>( 24, occluderFrusta.size() ) );

				auto cullSubrangeFn = [=]( unsigned, unsigned start, unsigned end ) {
					std::span<const unsigned> workloadSpan { surfNums.data() + start, surfNums.data() + end };
					self->cullSurfacesByOccluders( stateForCamera, workloadSpan, bestFrusta, mergedSurfSpans, surfVisTable );
				};

				TaskHandle cullTask = si.taskSystem->addForSubrangesInRange( { 0, surfNums.size() }, 384,
																			 std::span<const TaskHandle>{},
																			 std::move( cullSubrangeFn ) );
				co_await si.taskSystem->awaiterOf( cullTask );
			}
		}
	}
}

auto RendererFrontend::coPrepareOccluders( CoroTask::StartInfo si, RendererFrontend *self, StateForCamera *stateForCamera ) -> CoroTask {
	std::span<const Frustum> occluderFrusta;
	if( stateForCamera->useOcclusionCulling ) {
		// Collect occluder surfaces of leaves that fall into the primary frustum and that are "good enough"
		const std::span<const unsigned> visibleOccluders = self->collectVisibleOccluders( stateForCamera );
		if( !visibleOccluders.empty() ) {
			co_await si.taskSystem->awaiterOf( self->calcOccluderScores( stateForCamera, visibleOccluders ) );

			const std::span<const SortedOccluder> sortedOccluders = self->pruneAndSortOccludersByScores( stateForCamera,
																										 visibleOccluders );
			if( !sortedOccluders.empty() ) {
				// Build frusta of occluders, while performing some additional frusta pruning
				occluderFrusta = self->buildFrustaOfOccluders( stateForCamera, sortedOccluders );
			}
		}
	}

	stateForCamera->numOccluderFrusta = occluderFrusta.size();
}

auto RendererFrontend::coBeginPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, RendererFrontend *self,
														  std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														  bool areCamerasPortalCameras ) -> CoroTask {
	assert( scenesAndCameras.size() <= MAX_REF_CAMERAS );

	wsw::StaticVector<StateForCamera *, MAX_REF_CAMERAS> statesForValidCameras;
	wsw::StaticVector<Scene *, MAX_REF_CAMERAS> scenesForValidCameras;
	for( unsigned cameraIndex = 0; cameraIndex < scenesAndCameras.size(); ++cameraIndex ) {
		StateForCamera *const stateForCamera = scenesAndCameras[cameraIndex].second;
		if( !( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
			if( v_drawWorld.get() && rsh.worldModel ) {
				statesForValidCameras.push_back( stateForCamera );
				scenesForValidCameras.push_back( scenesAndCameras[cameraIndex].first );
			}
		}
	}

	TaskHandle prepareBuffersTasks[MAX_REF_CAMERAS];
	for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
		StateForCamera *const stateForCamera = statesForValidCameras[cameraIndex];
		auto prepareBuffersFn = [=]( [[maybe_unused]] unsigned workerIndex ) {
			const unsigned numMergedSurfaces = rsh.worldBrushModel->numMergedSurfaces;
			const unsigned numWorldSurfaces  = rsh.worldBrushModel->numModelSurfaces;
			const unsigned numWorldLeaves    = rsh.worldBrushModel->numvisleafs;
			const unsigned numOccluders      = rsh.worldBrushModel->numOccluders;

			// Put the allocation code here, so we don't bloat the arch-specific code
			stateForCamera->visibleLeavesBuffer->reserve( numWorldLeaves );

			stateForCamera->drawSurfSurfSpansBuffer->reserve( numMergedSurfaces );
			stateForCamera->bspDrawSurfacesBuffer->reserve( numMergedSurfaces );

			// Try guessing the required size
			const unsigned estimatedNumSubspans = wsw::max( 8 * numMergedSurfaces, numWorldSurfaces );
			// Two unsigned elements per each subspan TODO: Allow storing std::pair in this container
			stateForCamera->drawSurfSurfSubspansBuffer->reserve( 2 * estimatedNumSubspans );
			stateForCamera->drawSurfMultiDrawCountsBuffer->reserve( estimatedNumSubspans );
			stateForCamera->drawSurfMultiDrawIndicesBuffer->reserve( estimatedNumSubspans );

			stateForCamera->surfVisTableBuffer->reserveZeroed( numWorldSurfaces );

			stateForCamera->useOcclusionCulling = numOccluders > 0 && !( stateForCamera->renderFlags & RF_NOOCCLUSIONCULLING );
			if( stateForCamera->useOcclusionCulling ) {
				stateForCamera->useWorldBspOcclusionCulling = !( stateForCamera->refdef.rdflags & RDF_NOBSPOCCLUSIONCULLING );
			}

			if( stateForCamera->useOcclusionCulling ) {
				stateForCamera->visibleOccludersBuffer->reserve( numOccluders );
				stateForCamera->sortedOccludersBuffer->reserve( numOccluders );
			}

			if( stateForCamera->useWorldBspOcclusionCulling ) {
				stateForCamera->leafSurfTableBuffer->reserve( numWorldSurfaces );
				stateForCamera->leafSurfNumsBuffer->reserve( numWorldSurfaces );
			}

			MergedSurfSpan *const mergedSurfSpans = stateForCamera->drawSurfSurfSpansBuffer->get();
			for( unsigned i = 0; i < numMergedSurfaces; ++i ) {
				mergedSurfSpans[i].mdSpan          = { .counts = nullptr, .indices = nullptr, .numDraws = 0 };
				mergedSurfSpans[i].firstSurface    = std::numeric_limits<int>::max();
				mergedSurfSpans[i].lastSurface     = std::numeric_limits<int>::min();
				mergedSurfSpans[i].subspansOffset  = 0;
				mergedSurfSpans[i].numSubspans     = 0;
			}

			stateForCamera->drawWorld = true;
		};

		prepareBuffersTasks[cameraIndex] = si.taskSystem->add( std::span<const TaskHandle>(), std::move( prepareBuffersFn ) );
	}

	TaskHandle execPassUponInitialCullingOfLeavesTasks[MAX_REF_CAMERAS];
	TaskHandle collectOccludersTasks[MAX_REF_CAMERAS];

	for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
		StateForCamera *const stateForCamera = statesForValidCameras[cameraIndex];

		const TaskHandle initialDependencies[1] { prepareBuffersTasks[cameraIndex] };

		const TaskHandle performInitialCullingOfLeavesTask = si.taskSystem->add( initialDependencies, [=]( unsigned ) {
			stateForCamera->leavesInFrustumAndPvs = self->collectVisibleWorldLeaves( stateForCamera );
		});

		collectOccludersTasks[cameraIndex] = si.taskSystem->addCoro( [=]() {
			return coPrepareOccluders( { si.taskSystem, initialDependencies, CoroTask::AnyThread }, self, stateForCamera );
		});

		const TaskHandle cullLeavesAsDependencies[1] { performInitialCullingOfLeavesTask };

		execPassUponInitialCullingOfLeavesTasks[cameraIndex] = si.taskSystem->addCoro( [=]() {
			return coExecPassUponInitialCullingOfLeaves( { si.taskSystem, cullLeavesAsDependencies, CoroTask::AnyThread },
														 self, stateForCamera );
		});
	}

	TaskHandle execPassUponPreparingOccludersTasks[MAX_REF_CAMERAS];
	for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
		StateForCamera *const stateForCamera = statesForValidCameras[cameraIndex];
		const TaskHandle dependencies[2] {
			execPassUponInitialCullingOfLeavesTasks[cameraIndex], collectOccludersTasks[cameraIndex]
		};
		execPassUponPreparingOccludersTasks[cameraIndex] = si.taskSystem->addCoro( [=]() {
			return coExecPassUponPreparingOccluders( { si.taskSystem, dependencies, CoroTask::AnyThread }, self, stateForCamera );
		});
	}

	TaskHandle calcSubspansTasks[MAX_REF_CAMERAS];
	for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
		StateForCamera *stateForCamera = statesForValidCameras[cameraIndex];
		assert( execPassUponPreparingOccludersTasks[cameraIndex] );
		const TaskHandle dependencies[1] { execPassUponPreparingOccludersTasks[cameraIndex] };
		calcSubspansTasks[cameraIndex] = si.taskSystem->add( dependencies, [=]( unsigned ) {
			self->calcSubspansOfMergedSurfSpans( stateForCamera );
		});
	}

	TaskHandle processWorldPortalSurfacesTasks[MAX_REF_CAMERAS];
	for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
		StateForCamera *stateForCamera = statesForValidCameras[cameraIndex];
		Scene *sceneForCamera          = scenesForValidCameras[cameraIndex];
		const TaskHandle dependencies[1] { calcSubspansTasks[cameraIndex] };
		processWorldPortalSurfacesTasks[cameraIndex] = si.taskSystem->add( dependencies, [=]( unsigned ) {
			// If we don't draw portals or are in portal state (and won't draw portals recursively)
			// we still have to update respective surfaces for proper sorting
			self->processWorldPortalSurfaces( stateForCamera, sceneForCamera, areCamerasPortalCameras );
		});
	}

	co_await si.taskSystem->awaiterOf( { processWorldPortalSurfacesTasks, statesForValidCameras.size() } );

	if( !areCamerasPortalCameras ) {
		self->m_tmpPortalScenesAndStates.clear();
		for( unsigned cameraIndex = 0; cameraIndex < statesForValidCameras.size(); ++cameraIndex ) {
			for( StateForCamera *stateForPortalCamera: statesForValidCameras[cameraIndex]->portalCameraStates ) {
				// Portals share scene with the parent state
				self->m_tmpPortalScenesAndStates.push_back( { scenesForValidCameras[cameraIndex], stateForPortalCamera } );
			}
		}
		if( !self->m_tmpPortalScenesAndStates.empty() ) {
			co_await si.taskSystem->awaiterOf( self->beginPreparingRenderingFromTheseCameras( self->m_tmpPortalScenesAndStates, true ) );
		}
	}
}

auto RendererFrontend::beginPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														bool areCamerasPortalCameras ) -> TaskHandle {
	return m_taskSystem.addCoro( [=, this]() {
		CoroTask::StartInfo startInfo { &m_taskSystem, {}, CoroTask::AnyThread };
		return coBeginPreparingRenderingFromTheseCameras( startInfo, this, scenesAndCameras, areCamerasPortalCameras );
	});
}

auto RendererFrontend::endPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
													  std::span<const TaskHandle> dependencies,
													  bool areCamerasPortalCameras ) -> TaskHandle {
	return m_taskSystem.addCoro( [=, this]() {
		CoroTask::StartInfo startInfo { &m_taskSystem, dependencies, CoroTask::OnlyMainThread };
		return coEndPreparingRenderingFromTheseCameras( startInfo, this, scenesAndCameras, areCamerasPortalCameras );
	});
}

auto RendererFrontend::coEndPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, RendererFrontend *self,
														std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														bool areCamerasPortalCameras ) -> CoroTask {
	DynamicStuffWorkloadStorage *workloadStorage;
	if( areCamerasPortalCameras ) [[unlikely]] {
		workloadStorage = &self->m_dynamicStuffWorkloadStorage[1];
	} else {
		// Caution! There is an implication that preparing uploads is executed in a serial fashion
		// by primary and portal camera groups. The primary call starts uploads.
		// Make sure the following portal stage does not reset values.

		self->m_dynamicMeshCountersOfVerticesAndIndices = { 0, 0 };
		self->m_batchedSurfCountersOfVerticesAndIndices = { 0, 0 };

		self->m_spriteSurfOffsetOfVertices = 0;
		self->m_spriteSurfCounterOfIndices = 0;

		R_BeginMeshUploads( UPLOAD_GROUP_DYNAMIC_MESH );
		R_BeginMeshUploads( UPLOAD_GROUP_BATCHED_MESH );
		R_BeginMeshUploads( UPLOAD_GROUP_BATCHED_MESH_EXT );

		workloadStorage = &self->m_dynamicStuffWorkloadStorage[0];
	}

	workloadStorage->dynamicMeshFillDataWorkload.clear();
	if( v_drawEntities.get() ) {
		for( auto [scene, stateForCamera] : scenesAndCameras ) {
			const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };
			self->collectVisibleDynamicMeshes( stateForCamera, scene, occluderFrusta, &self->m_dynamicMeshCountersOfVerticesAndIndices );

			for( unsigned i = 0; i < stateForCamera->numDynamicMeshDrawSurfaces; ++i ) {
				DynamicMeshDrawSurface *drawSurface = &stateForCamera->dynamicMeshDrawSurfaces[i];
				workloadStorage->dynamicMeshFillDataWorkload.append( DynamicMeshFillDataWorkload {
					.scene          = scene,
					.stateForCamera = stateForCamera,
					.drawSurface    = drawSurface,
				});
			}
		}
	}

	// Collect lights as well
	// Note: Dynamically submitted entities may add lights
	std::span<const uint16_t> visibleProgramLightIndices[MAX_REF_CAMERAS];
	std::span<const uint16_t> visibleCoronaLightIndices[MAX_REF_CAMERAS];
	if( v_dynamicLight.get() ) {
		for( unsigned cameraIndex = 0; cameraIndex < scenesAndCameras.size(); ++cameraIndex ) {
			auto [scene, stateForCamera] = scenesAndCameras[cameraIndex];
			const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };
			std::tie( visibleProgramLightIndices[cameraIndex], visibleCoronaLightIndices[cameraIndex] ) =
				self->collectVisibleLights( stateForCamera, scene, occluderFrusta );

			// Precache light indices for particle aggregates
			const unsigned numParticleAggregates = scene->m_particles.size();
			if( numParticleAggregates && stateForCamera->numAllVisibleLights ) {
				std::pair<unsigned, unsigned> *const spansTable = stateForCamera->lightSpansForParticleAggregates;
				uint16_t *const lightIndicesBuffer              = stateForCamera->lightIndicesForParticleAggregates
					->reserveAndGet( numParticleAggregates * stateForCamera->numAllVisibleLights );
				const std::span<uint16_t> availableLightIndices {
					stateForCamera->allVisibleLightIndices, stateForCamera->numAllVisibleLights
				};
				unsigned indicesOffset = 0;
				for( unsigned aggregateIndex = 0; aggregateIndex < numParticleAggregates; ++aggregateIndex ) {
					const Scene::ParticlesAggregate *aggregate = scene->m_particles.data() + aggregateIndex;
					const unsigned numAffectingLights = findLightsThatAffectBounds( scene->m_dynamicLights.data(),
																					availableLightIndices,
																					aggregate->mins, aggregate->maxs,
																					lightIndicesBuffer + indicesOffset );
					spansTable[aggregateIndex].first  = indicesOffset;
					spansTable[aggregateIndex].second = numAffectingLights;
					indicesOffset += numAffectingLights;
				}
				// Save this flag to reduce the amount of further tests for individual batches
				stateForCamera->canAddLightsToParticles = true;
			}
		}
	}

	TaskHandle fillMeshBuffersTask;
	if( !workloadStorage->dynamicMeshFillDataWorkload.empty() ) {
		auto fn = [=]( unsigned, unsigned elemIndex ) {
			self->prepareDynamicMesh( workloadStorage->dynamicMeshFillDataWorkload.data() + elemIndex );
		};
		std::pair<unsigned, unsigned> rangeOfIndices { 0, workloadStorage->dynamicMeshFillDataWorkload.size() };
		fillMeshBuffersTask = si.taskSystem->addForIndicesInRange( rangeOfIndices, std::span<const TaskHandle> {}, std::move( fn ) );
	}

	for( unsigned i = 0; i < scenesAndCameras.size(); ++i ) {
		auto [scene, stateForCamera] = scenesAndCameras[i];

		const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };

		self->collectVisiblePolys( stateForCamera, scene, occluderFrusta );

		if( const int dynamicLightValue = v_dynamicLight.get() ) {
			if( dynamicLightValue & 2 ) {
				self->addCoronaLightsToSortList( stateForCamera, scene->m_polyent, scene->m_dynamicLights.data(), visibleCoronaLightIndices[i] );
			}
			if( dynamicLightValue & 1 ) {
				std::span<const unsigned> spansStorage[1] { stateForCamera->leavesInFrustumAndPvs };
				std::span<std::span<const unsigned>> spansOfLeaves = { spansStorage, 1 };
				self->markLightsOfSurfaces( stateForCamera, scene, spansOfLeaves, visibleProgramLightIndices[i] );
			}
		}

		if( stateForCamera->drawWorld ) {
			// We must know lights at this point
			self->addVisibleWorldSurfacesToSortList( stateForCamera, scene );
		}

		if( v_drawEntities.get() ) {
			self->collectVisibleEntities( stateForCamera, scene, occluderFrusta );
		}

		self->collectVisibleParticles( stateForCamera, scene, occluderFrusta );
	}

	TaskHandle endPreparingRenderingFromPortalsTask;
	if( !areCamerasPortalCameras ) {
		if( !self->m_tmpPortalScenesAndStates.empty() ) {
			endPreparingRenderingFromPortalsTask = self->endPreparingRenderingFromTheseCameras( self->m_tmpPortalScenesAndStates, {}, true );
		}
	}

	// TODO: Can be run earlier in parallel with portal surface processing
	for( auto [scene, stateForCamera] : scenesAndCameras ) {
		self->processSortList( stateForCamera, scene );
	}

	self->markBuffersOfVariousDynamicsForUpload( scenesAndCameras, workloadStorage );

	for( PrepareBatchedSurfWorkload *workload: workloadStorage->selectedPolysWorkload ) {
		self->prepareBatchedQuadPolys( workload );
	}
	for( PrepareBatchedSurfWorkload *workload: workloadStorage->selectedCoronasWorkload ) {
		self->prepareBatchedCoronas( workload );
	}
	for( PrepareBatchedSurfWorkload *workload: workloadStorage->selectedParticlesWorkload ) {
		self->prepareBatchedParticles( workload );
	}
	for( PrepareSpriteSurfWorkload *workload: workloadStorage->selectedSpriteWorkload ) {
		self->prepareLegacySprites( workload );
	}

	// If there's processing of portals, finish it prior to awaiting uploads
	if( endPreparingRenderingFromPortalsTask ) {
		co_await si.taskSystem->awaiterOf( endPreparingRenderingFromPortalsTask );
	}

	if( fillMeshBuffersTask ) {
		co_await si.taskSystem->awaiterOf( fillMeshBuffersTask );
	}

	// The primary group of cameras ends uploads
	if( !areCamerasPortalCameras ) {
		const VboSpanLayout *dynamicMeshLayout = RB_VBOSpanLayoutForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH );
		const unsigned dynamicMeshVertexDataSize = dynamicMeshLayout->vertexSize * self->m_dynamicMeshCountersOfVerticesAndIndices.first;
		const unsigned dynamicMeshIndexDataSize = sizeof( elem_t ) * self->m_dynamicMeshCountersOfVerticesAndIndices.second;
		R_EndMeshUploads( UPLOAD_GROUP_DYNAMIC_MESH, dynamicMeshVertexDataSize, dynamicMeshIndexDataSize );

		const VboSpanLayout *batchedMeshLayout = RB_VBOSpanLayoutForFrameUploads( UPLOAD_GROUP_BATCHED_MESH );
		const unsigned batchedMeshVertexDataSize = batchedMeshLayout->vertexSize * self->m_batchedSurfCountersOfVerticesAndIndices.first;
		const unsigned batchedMeshIndexDataSize = sizeof( elem_t ) * self->m_batchedSurfCountersOfVerticesAndIndices.second;
		R_EndMeshUploads( UPLOAD_GROUP_BATCHED_MESH, batchedMeshVertexDataSize, batchedMeshIndexDataSize );

		const unsigned spriteVertexDataSize = self->m_spriteSurfOffsetOfVertices;
		const unsigned spriteIndexDataSize  = sizeof( elem_t ) * self->m_spriteSurfCounterOfIndices;
		R_EndMeshUploads( UPLOAD_GROUP_BATCHED_MESH_EXT, spriteVertexDataSize, spriteIndexDataSize );
	}
}

void RendererFrontend::performPreparedRenderingFromThisCamera( Scene *scene, StateForCamera *stateForCamera ) {

	if( stateForCamera->stateForSkyPortalCamera ) {
		performPreparedRenderingFromThisCamera( scene, stateForCamera->stateForSkyPortalCamera );
	}

	const unsigned renderFlags = stateForCamera->renderFlags;
	for( unsigned i = 0; i < stateForCamera->numPortalSurfaces; ++i ) {
		for( void *stateForPortalCamera : stateForCamera->portalSurfaces[i].statesForCamera ) {
			if( stateForPortalCamera ) {
				performPreparedRenderingFromThisCamera( scene, (StateForCamera *)stateForPortalCamera );
			}
		}
	}
	if( v_portalOnly.get() ) {
		return;
	}

	bool drawWorld = false;

	if( !( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
		if( v_drawWorld.get() && rsh.worldModel ) {
			drawWorld = true;
		}
	}

	BackendActionTape actionTape;

	SimulatedBackendState backendState( &actionTape, glConfig.width, glConfig.height );
	RB_BeginUsingBackendState( &backendState );

	backendState.setTime( stateForCamera->refdef.time, rf.frameTime.time );

	bindRenderTargetAndViewport( &backendState, stateForCamera->refdef.renderTarget, stateForCamera );

	const int *const scissor = stateForCamera->scissor;
	// TODO: Allow supplying an array of integers
	backendState.setScissor( scissor[0], scissor[1], scissor[2], scissor[3] );

	const int *const viewport = stateForCamera->viewport;
	backendState.setViewport( viewport[0], viewport[1], viewport[2], viewport[3] );

	backendState.setZClip( Z_NEAR, stateForCamera->farClip );
	backendState.setCamera( stateForCamera->viewOrigin, stateForCamera->viewAxis );
	backendState.setLightParams( stateForCamera->refdef.minLight, !drawWorld );
	backendState.setRenderFlags( renderFlags );
	backendState.loadProjectionMatrix( stateForCamera->projectionMatrix );
	backendState.loadCameraMatrix( stateForCamera->cameraMatrix );
	backendState.loadObjectMatrix( mat4x4_identity );

	if( renderFlags & RF_SHADOWMAPVIEW ) {
		backendState.setShaderStateMask( ~0, GLSTATE_NO_COLORWRITE );
	}

	// Unused?
	const bool isDrawingRgbShadow =
		( renderFlags & ( RF_SHADOWMAPVIEW | RF_SHADOWMAPVIEW_RGB ) ) == ( RF_SHADOWMAPVIEW | RF_SHADOWMAPVIEW_RGB );

	const bool didDrawADepthMask =
		( renderFlags & ( RF_MIRRORVIEW | RF_PORTALVIEW ) ) != 0 && ( renderFlags & RF_PORTAL_CAPTURE ) == 0;

	bool shouldClearColor = false;
	vec4_t clearColor;
	if( isDrawingRgbShadow ) {
		shouldClearColor = true;
		Vector4Set( clearColor, 1, 1, 1, 1 );
	} else if( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) {
		shouldClearColor = stateForCamera->refdef.renderTarget != nullptr;
		Vector4Set( clearColor, 1, 1, 1, 0 );
	} else {
		// /*stateForCamera->numDepthPortalSurfaces == 0 ||*/ r_fastsky->integer || stateForCamera->viewCluster < 0;
		shouldClearColor = stateForCamera->stateForSkyPortalCamera == nullptr;
		if( rsh.worldBrushModel && rsh.worldBrushModel->globalfog && rsh.worldBrushModel->globalfog->shader ) {
			Vector4Scale( rsh.worldBrushModel->globalfog->shader->fog_color, 1.0 / 255.0, clearColor );
		} else if( rsh.worldBrushModel && rsh.worldBrushModel->skyShader ) {
			Vector4Scale( rsh.worldBrushModel->skyShader->skyColor, 1.0 / 255.0, clearColor );
		} else {
			Vector4Scale( mapConfig.environmentColor, 1.0 / 255.0, clearColor );
		}
	}

	int bitsToClear = 0;
	if( !didDrawADepthMask ) {
		bitsToClear |= GL_DEPTH_BUFFER_BIT;
	}
	if( shouldClearColor ) {
		bitsToClear |= GL_COLOR_BUFFER_BIT;
	}

	backendState.clear( bitsToClear, clearColor[0], clearColor[1], clearColor[2], clearColor[3] );

	submitDrawActionsList( &backendState, stateForCamera, scene );

	if( v_showTris.get() && !( renderFlags & RF_SHADOWMAPVIEW ) ) {
		backendState.enableWireframe( true );

		submitDrawActionsList( &backendState, stateForCamera, scene );

		backendState.enableWireframe( false );
	}

	backendState.transformForWorld();

	if( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) {
		backendState.setShaderStateMask( ~0, 0 );
	}

	backendState.setShaderStateMask( ~0, GLSTATE_NO_DEPTH_TEST );

	submitDebugStuffToBackend( &backendState, stateForCamera, scene );

	backendState.setShaderStateMask( ~0, 0 );

	RB_EndUsingBackendState( &backendState );

	RuntimeBackendState rbs {};
	actionTape.exec( &rbs );
}


void RendererFrontend::dynLightDirForOrigin( const vec_t *origin, float radius, vec3_t dir, vec3_t diffuseLocal, vec3_t ambientLocal ) {
}

}

Scene::Scene( unsigned index ) : m_index( index ) {
	m_worldent = m_localEntities.unsafe_grow_back();
	memset( m_worldent, 0, sizeof( entity_t ) );
	m_worldent->rtype = RT_MODEL;
	m_worldent->number = 0;
	m_worldent->model = rsh.worldModel;
	m_worldent->scale = 1.0f;
	Matrix3_Identity( m_worldent->axis );
	m_entities.push_back( m_worldent );

	m_polyent = m_localEntities.unsafe_grow_back();
	memset( m_polyent, 0, sizeof( entity_t ) );
	m_polyent->rtype = RT_MODEL;
	m_polyent->number = 1;
	m_polyent->model = nullptr;
	m_polyent->scale = 1.0f;
	Matrix3_Identity( m_polyent->axis );
	m_entities.push_back( m_polyent );
}

void DrawSceneRequest::addEntity( const entity_t *ent ) {
	assert( ent->rtype != RT_PORTALSURFACE );
	if( !m_entities.full() ) [[likely]] {
		entity_t *added = nullptr;

		if( ent->rtype == RT_MODEL ) {
			if( const model_t *__restrict model = ent->model ) [[likely]] {
				if( model->type == mod_alias ) {
					m_aliasModelEntities.push_back( *ent );
					added = std::addressof( m_aliasModelEntities.back() );
				} else if( model->type == mod_skeletal ) {
					m_skeletalModelEntities.push_back( *ent );
					added = std::addressof( m_skeletalModelEntities.back() );
				} else if( model->type == mod_brush ) {
					m_brushModelEntities.push_back( *ent );
					added = std::addressof( m_brushModelEntities.back() );
				}
			} else {
				m_nullModelEntities.push_back( *ent );
				added = std::addressof( m_nullModelEntities.back() );
			}
		} else if( ent->rtype == RT_SPRITE ) [[unlikely]] {
			m_spriteEntities.push_back( *ent );
			added = std::addressof( m_spriteEntities.back() );
			// simplifies further checks
			added->model = nullptr;
		}

		if( added ) {
			if( v_outlinesScale.get() <= 0 ) {
				added->outlineHeight = 0;
			}

			if( !v_lerpModels.get() ) {
				added->backlerp = 0;
			}

			if( added->renderfx & RF_ALPHAHACK ) {
				if( added->shaderRGBA[3] == 255 ) {
					added->renderfx &= ~RF_ALPHAHACK;
				}
			}

			m_entities.push_back( added );
			added->number = m_entities.size() - 1;

			// add invisible fake entity for depth write
			// TODO: This should belong to the CGame code
			if( ( added->renderfx & ( RF_WEAPONMODEL | RF_ALPHAHACK ) ) == ( RF_WEAPONMODEL | RF_ALPHAHACK ) ) {
				entity_t tent = *ent;
				tent.renderfx &= ~RF_ALPHAHACK;
				tent.renderfx |= RF_NOCOLORWRITE | RF_NOSHADOW;
				addEntity( &tent );
			}
		}
	}
}

void DrawSceneRequest::addPortalEntity( const entity_t *ent ) {
	assert( ent->rtype == RT_PORTALSURFACE );
	if( !m_portalSurfaceEntities.full() ) [[likely]] {
		m_portalSurfaceEntities.push_back( *ent );
		// Make sure we don't try using it
		// TODO: Use a completely different type, the current one is kept for structural compatiblity with existing code
		m_portalSurfaceEntities.back().number = ~0u / 2;
	}
}

void DrawSceneRequest::addLight( const float *origin, float programRadius, float coronaRadius, float r, float g, float b ) {
	assert( ( r >= 0.0f && r >= 0.0f && b >= 0.0f ) && ( r > 0.0f || g > 0.0f || b > 0.0f ) );
	if( !m_dynamicLights.full() ) [[likely]] {
		if( const int cvarValue = v_dynamicLight.get() ) [[likely]] {
			const bool hasProgramLight = programRadius > 0.0f && ( cvarValue & 1 ) != 0;
			const bool hasCoronaLight = coronaRadius > 0.0f && ( cvarValue & 2 ) != 0;
			if( hasProgramLight | hasCoronaLight ) [[likely]] {
				// Bounds are used for culling and also for applying lights to particles
				const float maxRadius = wsw::max( programRadius, coronaRadius );
				m_dynamicLights.emplace_back( DynamicLight {
					.origin          = { origin[0], origin[1], origin[2] },
					.programRadius   = programRadius,
					.coronaRadius    = coronaRadius,
					.maxRadius       = maxRadius,
					.color           = { r, g, b },
					.mins            = { origin[0] - maxRadius, origin[1] - maxRadius, origin[2] - maxRadius, 0.0f },
					.maxs            = { origin[0] + maxRadius, origin[1] + maxRadius, origin[2] + maxRadius, 1.0f },
					.hasProgramLight = hasProgramLight,
					.hasCoronaLight  = hasCoronaLight
				});
			}
		}
	}
}

void DrawSceneRequest::addParticles( const float *mins, const float *maxs,
									 const Particle::AppearanceRules &appearanceRules,
									 const Particle *particles, unsigned numParticles ) {
	assert( numParticles <= kMaxParticlesInAggregate );
	assert( mins[3] == 0.0f && maxs[3] == 1.0f );
	if( !m_particles.full() ) [[likely]] {
		m_particles.emplace_back( ParticlesAggregate {
			.particles       = particles,
			.appearanceRules = appearanceRules,
			.mins            = { mins[0], mins[1], mins[2], mins[3] },
			.maxs            = { maxs[0], maxs[1], maxs[2], maxs[3] },
			.numParticles    = numParticles
		});
	}
}

void DrawSceneRequest::addDynamicMesh( const DynamicMesh *mesh ) {
	if( !m_dynamicMeshes.full() ) [[likely]] {
		m_dynamicMeshes.push_back( mesh );
	}
}

void DrawSceneRequest::addCompoundDynamicMesh( const float *mins, const float *maxs,
											   const DynamicMesh **parts, unsigned numParts,
											   const float *meshOrderDesignators ) {
	assert( numParts <= kMaxPartsInCompoundMesh );
	if( !m_compoundDynamicMeshes.full() ) [[likely]] {
		m_compoundDynamicMeshes.emplace_back( CompoundDynamicMesh {
			.cullMins             = { mins[0], mins[1], mins[2], mins[3] },
			.cullMaxs             = { maxs[0], maxs[1], maxs[2], maxs[3] },
			.parts                = parts,
			.meshOrderDesignators = meshOrderDesignators,
			.numParts             = numParts,
		});
	}
}

void Draw2DRequest::setScissor( int x, int y, int w, int h ) {
	m_cmds.emplace_back( SetScissorCmd { .x = x, .y = y, .w = w, .h = h } );
}

void Draw2DRequest::drawStretchPic( int x, int y, int w, int h, float s1, float t1, float s2, float t2,
									const vec_t *color, const shader_s *shader ) {
	drawRotatedStretchPic( x, y, w, h, s1, t1, s2, t2, 0.0f, color, shader );
}

void Draw2DRequest::drawRotatedStretchPic( int x, int y, int w, int h, float s1, float t1, float s2, float t2,
										   float angle, const vec_t *color, const shader_s *shader ) {
	m_cmds.emplace_back( DrawPicCmd {
		.x = x, .y = y, .w = w, .h = h,
		.s1 = s1, .t1 = t1, .s2 = s2, .t2 = t2,
		.angle = angle, .color = { color[0], color[1], color[2], color[3] }, .shader = shader,
	});
}