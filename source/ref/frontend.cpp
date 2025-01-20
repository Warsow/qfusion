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
#include "../common/singletonholder.h"
#include "../common/links.h"
#include "../common/profilerscope.h"

/*
* R_Set2DMode
*
* Note that this sets the viewport to size of the active framebuffer.
*/
void R_Set2DMode( bool enable ) {
	wsw::ref::Frontend::instance()->set2DMode( enable );
}

void RF_Set2DScissor( int x, int y, int w, int h ) {
	wsw::ref::Frontend::instance()->set2DScissor( x, y, w, h );
}

void R_TransformForWorld();
void R_TranslateForEntity( const entity_t *e );
void R_TransformForEntity( const entity_t *e );

namespace wsw::ref {

auto Frontend::getDefaultFarClip( const refdef_s *fd ) const -> float {
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

auto Frontend::getFogForBounds( const StateForCamera *stateForCamera, const float *mins, const float *maxs ) -> mfog_t * {
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

auto Frontend::getFogForSphere( const StateForCamera *stateForCamera, const vec3_t centre, const float radius ) -> mfog_t * {
	vec3_t mins, maxs;
	for( unsigned i = 0; i < 3; i++ ) {
		mins[i] = centre[i] - radius;
		maxs[i] = centre[i] + radius;
	}
	return getFogForBounds( stateForCamera, mins, maxs );
}

void Frontend::set2DMode( bool enable ) {
	const int width  = rf.frameBufferWidth;
	const int height = rf.frameBufferHeight;

	if( rf.in2D == true && enable == true && width == rf.width2D && height == rf.height2D ) {
		return;
	} else if( rf.in2D == false && enable == false ) {
		return;
	}

	rf.in2D = enable;

	// TODO: We have to use a different camera!

	if( enable ) {
		rf.width2D  = width;
		rf.height2D = height;

		mat4_t projectionMatrix;

		Matrix4_OrthogonalProjection( 0, width, height, 0, -99999, 99999, projectionMatrix );

		// set 2D virtual screen size
		RB_Scissor( 0, 0, width, height );
		RB_Viewport( 0, 0, width, height );

		RB_LoadProjectionMatrix( projectionMatrix );
		RB_LoadCameraMatrix( mat4x4_identity );
		RB_LoadObjectMatrix( mat4x4_identity );

		RB_SetShaderStateMask( ~0, GLSTATE_NO_DEPTH_TEST );

		RB_SetRenderFlags( 0 );
	} else {
		// render previously batched 2D geometry, if any
		RB_FlushDynamicMeshes();

		RB_SetShaderStateMask( ~0, 0 );
	}
}

void Frontend::set2DScissor( int x, int y, int w, int h ) {
	assert( rf.in2D );
	RB_Scissor( x, y, w, h );
}

void Frontend::bindRenderTargetAndViewport( RenderTargetComponents *components, const StateForCamera *stateForCamera ) {
	// TODO: This is for the default render target
	const int width  = components ? components->texture->width : glConfig.width;
	const int height = components ? components->texture->height : glConfig.height;

	rf.frameBufferWidth  = width;
	rf.frameBufferHeight = height;

	RB_BindFrameBufferObject( components );

	RB_Viewport( stateForCamera->viewport[0], stateForCamera->viewport[1], stateForCamera->viewport[2], stateForCamera->viewport[3] );
	RB_Scissor( stateForCamera->scissor[0], stateForCamera->scissor[1], stateForCamera->scissor[2], stateForCamera->scissor[3] );
}

void Frontend::beginDrawingScenes() {
	R_ClearSkeletalCache();
	recycleFrameCameraStates();
	m_drawSceneFrame++;
	m_cameraIndexCounter = 0;
	m_sceneIndexCounter = 0;
	m_tmpPortalScenesAndStates.clear();
}

auto Frontend::createDrawSceneRequest( const refdef_t &refdef ) -> DrawSceneRequest * {
	return new( m_drawSceneRequestsHolder.unsafe_grow_back() )DrawSceneRequest( refdef, m_sceneIndexCounter++ );
}

auto Frontend::coBeginProcessingDrawSceneRequests( CoroTask::StartInfo si, Frontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask {
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

auto Frontend::coEndProcessingDrawSceneRequests( CoroTask::StartInfo si, Frontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask {
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

auto Frontend::beginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle {
	// Don't pin it to the main thread as the CGame coroutine is already pinned to the main thread
	CoroTask::StartInfo si { &m_taskSystem, {}, CoroTask::AnyThread };
	// Note: passing the span should be safe as it resides in cgame code during task system execution
	return m_taskSystem.addCoro( [=, this]() { return coBeginProcessingDrawSceneRequests( si, this, requests ); } );
}

auto Frontend::endProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests, std::span<const TaskHandle> dependencies ) -> TaskHandle {
	CoroTask::StartInfo si { &m_taskSystem, dependencies, CoroTask::OnlyMainThread };
	// Note: passing the span should be safe as it resides in cgame code during task system execution
	return m_taskSystem.addCoro( [=, this]() { return coEndProcessingDrawSceneRequests( si, this, requests ); } );
}

void Frontend::commitProcessedDrawSceneRequest( DrawSceneRequest *request ) {
	set2DMode( false );

	RB_SetTime( request->m_refdef.time );

	if( auto *const stateForSceneCamera = (StateForCamera *)request->stateForCamera ) {
		// TODO: Is this first call really needed
		bindRenderTargetAndViewport( nullptr, stateForSceneCamera );

		performPreparedRenderingFromThisCamera( request, stateForSceneCamera );

		bindRenderTargetAndViewport( nullptr, stateForSceneCamera );
	} else {
		// TODO what to do
	}

	set2DMode( true );
}

void Frontend::endDrawingScenes() {
	recycleFrameCameraStates();
	m_drawSceneRequestsHolder.clear();
}

Frontend::Frontend() : m_taskSystem( { .profilingGroup  = wsw::ProfilingSystem::ClientGroup,
									   .numExtraThreads = suggestNumExtraWorkerThreads( {} ) } ) {
	const auto features = Sys_GetProcessorFeatures();
	if( Q_CPU_FEATURE_SSE41 & features ) {
		m_collectVisibleWorldLeavesArchMethod = &Frontend::collectVisibleWorldLeavesSse41;
		m_collectVisibleOccludersArchMethod   = &Frontend::collectVisibleOccludersSse41;
		m_buildFrustaOfOccludersArchMethod    = &Frontend::buildFrustaOfOccludersSse41;
		if( Q_CPU_FEATURE_AVX & features ) {
			m_cullSurfacesByOccludersArchMethod = &Frontend::cullSurfacesByOccludersAvx;
		} else {
			m_cullSurfacesByOccludersArchMethod = &Frontend::cullSurfacesByOccludersSse41;
		}
		m_cullEntriesWithBoundsArchMethod   = &Frontend::cullEntriesWithBoundsSse41;
		m_cullEntryPtrsWithBoundsArchMethod = &Frontend::cullEntryPtrsWithBoundsSse41;
	} else {
		m_collectVisibleWorldLeavesArchMethod = &Frontend::collectVisibleWorldLeavesSse2;
		m_collectVisibleOccludersArchMethod   = &Frontend::collectVisibleOccludersSse2;
		m_buildFrustaOfOccludersArchMethod    = &Frontend::buildFrustaOfOccludersSse2;
		m_cullSurfacesByOccludersArchMethod   = &Frontend::cullSurfacesByOccludersSse2;
		m_cullEntriesWithBoundsArchMethod     = &Frontend::cullEntriesWithBoundsSse2;
		m_cullEntryPtrsWithBoundsArchMethod   = &Frontend::cullEntryPtrsWithBoundsSse2;
	}
}

Frontend::~Frontend() {
	disposeCameraStates();
}

alignas( 32 ) static SingletonHolder<Frontend> sceneInstanceHolder;

void Frontend::init() {
	sceneInstanceHolder.init();
}

void Frontend::shutdown() {
	sceneInstanceHolder.shutdown();
}

Frontend *Frontend::instance() {
	return sceneInstanceHolder.instance();
}

void Frontend::initVolatileAssets() {
	m_coronaShader = MaterialCache::instance()->loadDefaultMaterial( "$corona"_asView, SHADER_TYPE_CORONA );
}

void Frontend::destroyVolatileAssets() {
	m_coronaShader = nullptr;
	disposeCameraStates();
}

auto Frontend::allocStateForCamera() -> StateForCamera * {
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

	stateForCamera->sortList         = &resultStorage->meshSortList;
	stateForCamera->drawActionsList  = &resultStorage->drawActionsList;

	stateForCamera->preparePolysWorkload     = &resultStorage->preparePolysWorkloadBuffer;
	stateForCamera->prepareCoronasWorkload   = &resultStorage->prepareCoronasWorkloadBuffer;
	stateForCamera->prepareParticlesWorkload = &resultStorage->prepareParticlesWorkloadBuffer;
	stateForCamera->batchedSurfVertSpans     = &resultStorage->batchedSurfVertSpansBuffer;
	stateForCamera->prepareSpritesWorkload   = &resultStorage->prepareSpritesWorkloadBuffer;
	stateForCamera->preparedSpriteMeshes     = &resultStorage->preparedSpriteMeshesBuffer;

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

void Frontend::recycleFrameCameraStates() {
	for( StateForCameraStorage *used = m_usedStatesForCamera, *next; used; used = next ) { next = used->next;
		wsw::unlink( used, &m_usedStatesForCamera );
		( (StateForCamera *)used->theStateStorage )->~StateForCamera();
		used->isStateConstructed = false;
		wsw::link( used, &m_freeStatesForCamera );
	}
	assert( m_usedStatesForCamera == nullptr );
}

void Frontend::disposeCameraStates() {
	recycleFrameCameraStates();
	assert( m_usedStatesForCamera == nullptr );
	for( StateForCameraStorage *storage = m_freeStatesForCamera, *next; storage; storage = next ) { next = storage->next;
		wsw::unlink( storage, &m_freeStatesForCamera );
		delete storage;
	}
	assert( m_freeStatesForCamera == nullptr );
}

auto Frontend::setupStateForCamera( const refdef_t *fd, unsigned sceneIndex,
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
	if( r_lightmap->integer ) {
		stateForCamera->renderFlags |= RF_LIGHTMAP;
	}

	if( r_drawflat->integer ) {
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

auto Frontend::coExecPassUponInitialCullingOfLeaves( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask {
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

auto Frontend::coExecPassUponPreparingOccluders( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask {
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

auto Frontend::coPrepareOccluders( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask {
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

auto Frontend::coBeginPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, Frontend *self,
														  std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														  bool areCamerasPortalCameras ) -> CoroTask {
	assert( scenesAndCameras.size() <= MAX_REF_CAMERAS );

	wsw::StaticVector<StateForCamera *, MAX_REF_CAMERAS> statesForValidCameras;
	wsw::StaticVector<Scene *, MAX_REF_CAMERAS> scenesForValidCameras;
	for( unsigned cameraIndex = 0; cameraIndex < scenesAndCameras.size(); ++cameraIndex ) {
		StateForCamera *const stateForCamera = scenesAndCameras[cameraIndex].second;
		if( !( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
			if( r_drawworld->integer && rsh.worldModel ) {
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

auto Frontend::beginPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														bool areCamerasPortalCameras ) -> TaskHandle {
	return m_taskSystem.addCoro( [=, this]() {
		CoroTask::StartInfo startInfo { &m_taskSystem, {}, CoroTask::AnyThread };
		return coBeginPreparingRenderingFromTheseCameras( startInfo, this, scenesAndCameras, areCamerasPortalCameras );
	});
}

auto Frontend::endPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
													  std::span<const TaskHandle> dependencies,
													  bool areCamerasPortalCameras ) -> TaskHandle {
	return m_taskSystem.addCoro( [=, this]() {
		CoroTask::StartInfo startInfo { &m_taskSystem, dependencies, CoroTask::OnlyMainThread };
		return coEndPreparingRenderingFromTheseCameras( startInfo, this, scenesAndCameras, areCamerasPortalCameras );
	});
}

auto Frontend::coEndPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, Frontend *self,
														std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														bool areCamerasPortalCameras ) -> CoroTask {
	DynamicStuffWorkloadStorage *workloadStorage;
	if( areCamerasPortalCameras ) [[unlikely]] {
		workloadStorage = &self->m_dynamicStuffWorkloadStorage[1];
	} else {
		// Caution! There is an implication that preparing uploads is executed in a serial fashion
		// by primary and portal camera groups. The primary call starts uploads.
		// Make sure the following portal stage does not reset values.

		self->m_dynamicMeshOffsetsOfVerticesAndIndices     = { 0, 0 };
		self->m_variousDynamicsOffsetsOfVerticesAndIndices = { 0, 0 };

		R_BeginFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH );
		R_BeginFrameUploads( UPLOAD_GROUP_BATCHED_MESH );

		workloadStorage = &self->m_dynamicStuffWorkloadStorage[0];
	}

	workloadStorage->dynamicMeshFillDataWorkload.clear();
	if( r_drawentities->integer ) {
		for( auto [scene, stateForCamera] : scenesAndCameras ) {
			const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };
			self->collectVisibleDynamicMeshes( stateForCamera, scene, occluderFrusta, &self->m_dynamicMeshOffsetsOfVerticesAndIndices );

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
	if( r_dynamiclight->integer ) {
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

		if( const int dynamicLightValue = r_dynamiclight->integer ) {
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

		if( r_drawentities->integer ) {
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

	self->markBuffersOfBatchedDynamicsForUpload( scenesAndCameras, workloadStorage );

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
		self->prepareLegacySprite( workload );
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
		R_EndFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH );
		R_EndFrameUploads( UPLOAD_GROUP_BATCHED_MESH );
	}
}

void Frontend::performPreparedRenderingFromThisCamera( Scene *scene, StateForCamera *stateForCamera ) {
	// TODO: Is rendering depth mask first worth it

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
	if( r_portalonly->integer ) {
		return;
	}

	bool drawWorld = false;

	if( !( stateForCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
		if( r_drawworld->integer && rsh.worldModel ) {
			drawWorld = true;
		}
	}

	bindRenderTargetAndViewport( stateForCamera->refdef.renderTarget, stateForCamera );

	const int *const scissor = stateForCamera->scissor;
	RB_Scissor( scissor[0], scissor[1], scissor[2], scissor[3] );

	const int *const viewport = stateForCamera->viewport;
	RB_Viewport( viewport[0], viewport[1], viewport[2], viewport[3] );

	RB_SetZClip( Z_NEAR, stateForCamera->farClip );
	RB_SetCamera( stateForCamera->viewOrigin, stateForCamera->viewAxis );
	RB_SetLightParams( stateForCamera->refdef.minLight, !drawWorld );
	RB_SetRenderFlags( renderFlags );
	RB_LoadProjectionMatrix( stateForCamera->projectionMatrix );
	RB_LoadCameraMatrix( stateForCamera->cameraMatrix );
	RB_LoadObjectMatrix( mat4x4_identity );

	if( renderFlags & RF_SHADOWMAPVIEW ) {
		RB_SetShaderStateMask( ~0, GLSTATE_NO_COLORWRITE );
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

	int bits = 0;
	if( !didDrawADepthMask ) {
		bits |= GL_DEPTH_BUFFER_BIT;
	}
	if( shouldClearColor ) {
		bits |= GL_COLOR_BUFFER_BIT;
	}

	RB_Clear( bits, clearColor[0], clearColor[1], clearColor[2], clearColor[3] );

	submitDrawActionsList( stateForCamera, scene );

	if( r_showtris->integer && !( renderFlags & RF_SHADOWMAPVIEW ) ) {
		RB_EnableWireframe( true );

		submitDrawActionsList( stateForCamera, scene );

		RB_EnableWireframe( false );
	}

	R_TransformForWorld();

	if( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) {
		RB_SetShaderStateMask( ~0, 0 );
	}

	RB_SetShaderStateMask( ~0, GLSTATE_NO_DEPTH_TEST );

	submitDebugStuffToBackend( stateForCamera, scene );

	RB_SetShaderStateMask( ~0, 0 );
}


void Frontend::dynLightDirForOrigin( const vec_t *origin, float radius, vec3_t dir, vec3_t diffuseLocal, vec3_t ambientLocal ) {
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
			if( r_outlines_scale->value <= 0 ) {
				added->outlineHeight = 0;
			}

			if( !r_lerpmodels->integer ) {
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
		if( const int cvarValue = r_dynamiclight->integer ) [[likely]] {
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