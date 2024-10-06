/*
Copyright (C) 2007 Victor Luchits
Copyright (C) 2023 Chasseur de bots

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
#include "program.h"
#include "materiallocal.h"

#include <algorithm>

void R_TransformForWorld( void ) {
	RB_LoadObjectMatrix( mat4x4_identity );
}

void R_TranslateForEntity( const entity_t *e ) {
	mat4_t objectMatrix;

	Matrix4_Identity( objectMatrix );

	objectMatrix[0] = e->scale;
	objectMatrix[5] = e->scale;
	objectMatrix[10] = e->scale;
	objectMatrix[12] = e->origin[0];
	objectMatrix[13] = e->origin[1];
	objectMatrix[14] = e->origin[2];

	RB_LoadObjectMatrix( objectMatrix );
}

void R_TransformForEntity( const entity_t *e ) {
	assert( e->rtype == RT_MODEL && e->number != kWorldEntNumber );

	mat4_t objectMatrix;

	if( e->scale != 1.0f ) {
		objectMatrix[0] = e->axis[0] * e->scale;
		objectMatrix[1] = e->axis[1] * e->scale;
		objectMatrix[2] = e->axis[2] * e->scale;
		objectMatrix[4] = e->axis[3] * e->scale;
		objectMatrix[5] = e->axis[4] * e->scale;
		objectMatrix[6] = e->axis[5] * e->scale;
		objectMatrix[8] = e->axis[6] * e->scale;
		objectMatrix[9] = e->axis[7] * e->scale;
		objectMatrix[10] = e->axis[8] * e->scale;
	} else {
		objectMatrix[0] = e->axis[0];
		objectMatrix[1] = e->axis[1];
		objectMatrix[2] = e->axis[2];
		objectMatrix[4] = e->axis[3];
		objectMatrix[5] = e->axis[4];
		objectMatrix[6] = e->axis[5];
		objectMatrix[8] = e->axis[6];
		objectMatrix[9] = e->axis[7];
		objectMatrix[10] = e->axis[8];
	}

	objectMatrix[3] = 0;
	objectMatrix[7] = 0;
	objectMatrix[11] = 0;
	objectMatrix[12] = e->origin[0];
	objectMatrix[13] = e->origin[1];
	objectMatrix[14] = e->origin[2];
	objectMatrix[15] = 1.0;

	RB_LoadObjectMatrix( objectMatrix );
}

namespace wsw::ref {

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

	stateForCamera->lodScaleForFov = std::tan( stateForCamera->refdef.fov_x * ( M_PI / 180 ) * 0.5f );

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
					self->cullSurfacesByOccluders( workloadSpan, bestFrusta, mergedSurfSpans, surfVisTable );
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
			stateForCamera->drawSurfVertElemSpansBuffer->reserve( estimatedNumSubspans );

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
				mergedSurfSpans[i].firstSurface    = std::numeric_limits<int>::max();
				mergedSurfSpans[i].lastSurface     = std::numeric_limits<int>::min();
				mergedSurfSpans[i].subspansOffset  = 0;
				mergedSurfSpans[i].vertSpansOffset = 0;
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
	// TODO: We cannot use this with portals due to reallocations!
	// Caution! There is an implication that preparing uploads is executed in serial fashion by primary and portal camera groups
	wsw::PodVector<DynamicMeshFillDataWorkload> *tmpDynamicMeshFillDataWorkload = &self->m_tmpDynamicMeshFillDataWorkload;
	wsw::PodVector<DynamicMeshData> *tmpDynamicMeshData = &self->m_tmpDynamicMeshData;
	// The primary call starts uploads, make sure the following portal call dones not reset values
	if( !areCamerasPortalCameras ) {
		tmpDynamicMeshFillDataWorkload->clear();
		tmpDynamicMeshData->clear();

		self->m_selectedPolysWorkload.clear();
		self->m_selectedCoronasWorkload.clear();
		self->m_selectedParticlesWorkload.clear();

		self->m_dynamicMeshOffsetsOfVerticesAndIndices     = { 0, 0 };
		self->m_variousDynamicsOffsetsOfVerticesAndIndices = { 0, 0 };

		R_BeginFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH );
		R_BeginFrameUploads( UPLOAD_GROUP_BATCHED_MESH );
	}

	if( r_drawentities->integer ) {
		for( auto [scene, stateForCamera] : scenesAndCameras ) {
			const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };
			self->collectVisibleDynamicMeshes( stateForCamera, scene, occluderFrusta, &self->m_dynamicMeshOffsetsOfVerticesAndIndices );

			for( unsigned i = 0; i < stateForCamera->numDynamicMeshDrawSurfaces; ++i ) {
				DynamicMeshDrawSurface *drawSurface = &stateForCamera->dynamicMeshDrawSurfaces[i];
				tmpDynamicMeshFillDataWorkload->append( DynamicMeshFillDataWorkload {
					.scene          = scene,
					.stateForCamera = stateForCamera,
					.drawSurface    = drawSurface,
				});
			}
		}
		if( !tmpDynamicMeshFillDataWorkload->empty() ) {
			// TODO: Is this safe wrt drawing portals
			tmpDynamicMeshData->resize( tmpDynamicMeshFillDataWorkload->size() );
			for( unsigned i = 0; i < tmpDynamicMeshFillDataWorkload->size(); ++i ) {
				tmpDynamicMeshFillDataWorkload->operator[]( i ).destData = tmpDynamicMeshData->data() + i;
			}
		}
	}

	// Collect lights as well
	// Note: Dynamically submitted entities may add lights
	std::span<const uint16_t> visibleProgramLightIndices[MAX_REF_CAMERAS];
	std::span<const uint16_t> visibleCoronaLightIndices[MAX_REF_CAMERAS];
	if( r_dynamiclight->integer ) {
		for( unsigned i = 0; i < scenesAndCameras.size(); ++i ) {
			auto [scene, stateForCamera] = scenesAndCameras[i];
			const std::span<const Frustum> occluderFrusta { stateForCamera->occluderFrusta, stateForCamera->numOccluderFrusta };
			std::tie( visibleProgramLightIndices[i], visibleCoronaLightIndices[i] ) =
				self->collectVisibleLights( stateForCamera, scene, occluderFrusta );
		}
	}

	TaskHandle fillMeshBuffersTask;
	// TODO: Check how much is actually added during portal stage!!!!!!!
	if( !tmpDynamicMeshFillDataWorkload->empty() ) {
		auto fn = [=]( unsigned, unsigned elemIndex ) {
			self->prepareDynamicMesh( tmpDynamicMeshFillDataWorkload->data() + elemIndex );
		};
		// TODO: This is a bug!!!! limit the range of entries in the shared buffer!!!!!
		fillMeshBuffersTask = si.taskSystem->addForIndicesInRange( { 0u, (unsigned)tmpDynamicMeshFillDataWorkload->size() },
																   std::span<const TaskHandle> {}, std::move( fn ) );
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

	const unsigned oldPolysWorkloadSize     = self->m_selectedPolysWorkload.size();
	const unsigned oldCoronasWorkloadSize   = self->m_selectedCoronasWorkload.size();
	const unsigned oldParticlesWorkloadSize = self->m_selectedParticlesWorkload.size();

	self->markBuffersOfBatchedDynamicsForUpload( scenesAndCameras );

	for( unsigned i = oldPolysWorkloadSize; i < self->m_selectedPolysWorkload.size(); ++i ) {
		self->prepareBatchedQuadPolys( self->m_selectedPolysWorkload[i] );
	}
	for( unsigned i = oldCoronasWorkloadSize; i < self->m_selectedCoronasWorkload.size(); ++i ) {
		self->prepareBatchedCoronas( self->m_selectedCoronasWorkload[i] );
	}
	for( unsigned i = oldParticlesWorkloadSize; i < self->m_selectedParticlesWorkload.size(); ++i ) {
		self->prepareBatchedParticles( self->m_selectedParticlesWorkload[i] );
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

// TODO: Make these dynamics view-dependent?

[[nodiscard]]
auto getParticleSpanStorageRequirements( std::span<const sortedDrawSurf_t> batchSpan ) -> std::optional<std::pair<unsigned, unsigned>> {
	return std::make_optional<std::pair<unsigned, unsigned>>( 4 * batchSpan.size(), 6 * batchSpan.size() );
}

[[nodiscard]]
auto getCoronaSpanStorageRequirements( std::span<const sortedDrawSurf_t> batchSpan ) -> std::optional<std::pair<unsigned, unsigned>> {
	return std::make_optional<std::pair<unsigned, unsigned>>( 4 * batchSpan.size(), 6 * batchSpan.size() );
}

[[nodiscard]]
auto getQuadPolySpanStorageRequirements( std::span<const sortedDrawSurf_t> batchSpan ) -> std::optional<std::pair<unsigned, unsigned>> {
	return std::make_optional<std::pair<unsigned, unsigned>>( 4 * batchSpan.size(), 6 * batchSpan.size() );
}

[[nodiscard]]
auto getSpriteSpanStorageRequirements( std::span<const sortedDrawSurf_t> batchSpan ) -> std::optional<std::pair<unsigned, unsigned>> {
	return std::nullopt;
}

void Frontend::markBuffersOfBatchedDynamicsForUpload( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras ) {
	const auto markAndAdvanceOffsets = [this]( const std::pair<unsigned, unsigned> &storageRequirements,
											   StateForCamera *stateForCamera,
											   const PrepareBatchedSurfWorkload &workload ) -> bool {
		// TODO: Protect against overflow
		unsigned *offsetOfVertices = &m_variousDynamicsOffsetsOfVerticesAndIndices.first;
		unsigned *offsetOfIndices  = &m_variousDynamicsOffsetsOfVerticesAndIndices.second;
		stateForCamera->batchedSurfVertSpans->data()[workload.vertSpanOffset] = VertElemSpan {
			.firstVert = *offsetOfVertices,
			.numVerts  = storageRequirements.first,
			.firstElem = *offsetOfIndices,
			.numElems  = storageRequirements.second,
		};
		*offsetOfVertices += storageRequirements.first;
		*offsetOfIndices  += storageRequirements.second;
		return true;
	};

	// TODO: Just return?
	bool overflow = false;
	// We have to join the execution flow for this
	for( auto [scene, stateForCamera] : scenesAndCameras ) {
		if( !m_selectedPolysWorkload.full() ) {
			for( PrepareBatchedSurfWorkload &workload: *stateForCamera->preparePolysWorkload ) {
				if( auto maybeRequirements = getQuadPolySpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( !markAndAdvanceOffsets( *maybeRequirements, stateForCamera, workload ) ) [[unlikely]] {
						overflow = true;
						break;
					}
					m_selectedPolysWorkload.push_back( std::addressof( workload ) );
					if( m_selectedPolysWorkload.full() ) [[unlikely]] {
						break;
					}
				}
			}
		}
		if( !overflow && !m_selectedCoronasWorkload.full() ) {
			for( PrepareBatchedSurfWorkload &workload: *stateForCamera->prepareCoronasWorkload ) {
				if( auto maybeRequirements = getCoronaSpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( !markAndAdvanceOffsets( *maybeRequirements, stateForCamera, workload ) ) [[unlikely]] {
						overflow = true;
						break;
					}
					this->m_selectedCoronasWorkload.push_back( std::addressof( workload ) );
					if( this->m_selectedCoronasWorkload.full() ) [[unlikely]] {
						break;
					}
				}
			}
		}
		if( !overflow && !m_selectedParticlesWorkload.full() ) {
			for( PrepareBatchedSurfWorkload &workload: *stateForCamera->prepareParticlesWorkload ) {
				if( auto maybeRequirements = getParticleSpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( !markAndAdvanceOffsets( *maybeRequirements, stateForCamera, workload ) ) [[unlikely]] {
						overflow = true;
						break;
					}
					m_selectedParticlesWorkload.push_back( std::addressof( workload ) );
					if( m_selectedParticlesWorkload.full() ) [[unlikely]] {
						break;
					}
				}
			}
		}
		if( overflow ) {
			break;
		}
	}

	// Process legacy sprites regardless of upload overflow
	for( auto [scene, stateForCamera] : scenesAndCameras ) {
		/*
		for( const PrepareSpriteSurfWorkload &workload: *stateForCamera->prepareSpritesWorkload ) {
			if( auto maybeRequirements = getSpriteSpanStorageRequirements( workload.batchSpan ) ) {
				markAndAdvanceOffsets( self, *maybeRequirements, stateForCamera, workload );
			}
		}*/
	}
}

void Frontend::performPreparedRenderingFromThisCamera( Scene *scene, StateForCamera *stateForCamera ) {
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
		shouldClearColor = /*stateForCamera->numDepthPortalSurfaces == 0 ||*/ r_fastsky->integer || stateForCamera->viewCluster < 0;
		if( rsh.worldBrushModel && rsh.worldBrushModel->globalfog && rsh.worldBrushModel->globalfog->shader ) {
			Vector4Scale( rsh.worldBrushModel->globalfog->shader->fog_color, 1.0 / 255.0, clearColor );
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

	submitDebugStuffToBackend( scene );

	RB_SetShaderStateMask( ~0, 0 );
}

void Frontend::submitDrawActionsList( StateForCamera *stateForCamera, Scene *scene ) {
	FrontendToBackendShared fsh;
	fsh.dynamicLights               = scene->m_dynamicLights.data();
	fsh.particleAggregates          = scene->m_particles.data();
	fsh.batchedVertElemSpans        = stateForCamera->batchedSurfVertSpans->data();
	fsh.allVisibleLightIndices      = { stateForCamera->allVisibleLightIndices, stateForCamera->numAllVisibleLights };
	fsh.visibleProgramLightIndices  = { stateForCamera->visibleProgramLightIndices, stateForCamera->numVisibleProgramLights };
	fsh.renderFlags                 = stateForCamera->renderFlags;
	fsh.fovTangent                  = stateForCamera->lodScaleForFov;
	fsh.cameraId                    = stateForCamera->cameraId;
	fsh.sceneIndex                  = stateForCamera->sceneIndex;
	std::memcpy( fsh.viewAxis, stateForCamera->viewAxis, sizeof( mat3_t ) );
	VectorCopy( stateForCamera->viewOrigin, fsh.viewOrigin );

	for( wsw::Function<void( FrontendToBackendShared * )> &drawAction: *stateForCamera->drawActionsList ) {
		drawAction( &fsh );
	}
}

auto Frontend::findNearestPortalEntity( const portalSurface_t *portalSurface, Scene *scene ) -> const entity_t * {
	vec3_t center;
	VectorAvg( portalSurface->mins, portalSurface->maxs, center );

	const entity_t *bestEnt = nullptr;
	float bestDist          = std::numeric_limits<float>::max();
	for( const entity_t &ent: scene->m_portalSurfaceEntities ) {
		if( std::fabs( PlaneDiff( ent.origin, &portalSurface->untransformed_plane ) ) < 64.0f ) {
			const float centerDist = Distance( ent.origin, center );
			if( centerDist < bestDist ) {
				bestDist = centerDist;
				bestEnt  = std::addressof( ent );
			}
		}
	}

	return bestEnt;
}

void Frontend::prepareDrawingPortalSurface( StateForCamera *stateForPrimaryCamera, Scene *scene, portalSurface_t *portalSurface ) {
	const shader_s *surfaceMaterial = portalSurface->shader;

	int startFromTextureIndex = -1;
	bool shouldDoReflection   = true;
	bool shouldDoRefraction   = ( surfaceMaterial->flags & SHADER_PORTAL_CAPTURE2 ) != 0;

	if( surfaceMaterial->flags & SHADER_PORTAL_CAPTURE ) {
		startFromTextureIndex = 0;

		for( unsigned i = 0; i < surfaceMaterial->numpasses; i++ ) {
			const shaderpass_t *const pass = &portalSurface->shader->passes[i];
			if( pass->program_type == GLSL_PROGRAM_TYPE_DISTORTION ) {
				if( ( pass->alphagen.type == ALPHA_GEN_CONST && pass->alphagen.args[0] == 1 ) ) {
					shouldDoRefraction = false;
				} else if( ( pass->alphagen.type == ALPHA_GEN_CONST && pass->alphagen.args[0] == 0 ) ) {
					shouldDoReflection = false;
				}
				break;
			}
		}
	}

	cplane_t *const portalPlane   = &portalSurface->plane;
	const float distToPortalPlane = PlaneDiff( stateForPrimaryCamera->viewOrigin, portalPlane );
	const bool canDoReflection    = shouldDoReflection && distToPortalPlane > BACKFACE_EPSILON;
	if( !canDoReflection ) {
		if( shouldDoRefraction ) {
			// Even if we're behind the portal, we still need to capture the second portal image for refraction
			startFromTextureIndex = 1;
			if( distToPortalPlane < 0 ) {
				VectorInverse( portalPlane->normal );
				portalPlane->dist = -portalPlane->dist;
			}
		} else {
			return;
		}
	}

	// default to mirror view
	unsigned drawPortalFlags = DrawPortalMirror;

	// TODO: Shouldn't it be performed upon loading?

	const entity_t *portalEntity = findNearestPortalEntity( portalSurface, scene );
	if( !portalEntity ) {
		if( startFromTextureIndex < 0 ) {
			return;
		}
	} else {
		if( !VectorCompare( portalEntity->origin, portalEntity->origin2 ) ) {
			drawPortalFlags &= ~DrawPortalMirror;
		}
		// TODO Prevent reusing the entity
	}

	if( startFromTextureIndex >= 0 ) {
		std::optional<std::pair<StateForCamera *, Texture *>> sideResults[2];
		unsigned numExpectedResults = 0;
		drawPortalFlags |= DrawPortalToTexture;
		if( startFromTextureIndex > 0 ) {
			drawPortalFlags |= DrawPortalRefraction;
		}
		sideResults[startFromTextureIndex] = prepareDrawingPortalSurfaceSide( stateForPrimaryCamera, portalSurface, scene, portalEntity, drawPortalFlags );
		numExpectedResults++;
		if( shouldDoRefraction && startFromTextureIndex < 1 && ( surfaceMaterial->flags & SHADER_PORTAL_CAPTURE2 ) ) {
			drawPortalFlags |= DrawPortalRefraction;
			sideResults[1] = prepareDrawingPortalSurfaceSide( stateForPrimaryCamera, portalSurface, scene, portalEntity, drawPortalFlags );
			numExpectedResults++;
		}
		const unsigned numActualResults = ( sideResults[0] != std::nullopt ) + ( sideResults[1] != std::nullopt );
		if( numActualResults == numExpectedResults ) {
			for( unsigned textureIndex = 0; textureIndex < 2; ++textureIndex ) {
				if( sideResults[textureIndex] ) {
					portalSurface->statesForCamera[textureIndex] = sideResults[textureIndex]->first;
					portalSurface->texures[textureIndex] = sideResults[textureIndex]->second;
					stateForPrimaryCamera->portalCameraStates.push_back( sideResults[textureIndex]->first );
				}
			}
		}
	} else {
		// TODO: Figure out what to do with mirrors (they aren't functioning properly at the moment of rewrite)
		//auto result = prepareDrawingPortalSurfaceSide( stateForPrimaryCamera, portalSurface, scene, portalEntity, drawPortalFlags );
	}
}

auto Frontend::prepareDrawingPortalSurfaceSide( StateForCamera *stateForPrimaryCamera,
												portalSurface_t *portalSurface, Scene *scene,
												const entity_t *portalEntity, unsigned drawPortalFlags )
	-> std::optional<std::pair<StateForCamera *, Texture *>> {
	vec3_t origin;
	mat3_t axis;

	unsigned renderFlagsToAdd   = 0;
	unsigned renderFlagsToClear = 0;

	const float *newPvsOrigin = nullptr;
	const float *newLodOrigin = nullptr;

	cplane_s *const portalPlane = &portalSurface->plane;
	if( drawPortalFlags & DrawPortalRefraction ) {
		VectorInverse( portalPlane->normal );
		portalPlane->dist = -portalPlane->dist;
		CategorizePlane( portalPlane );
		VectorCopy( stateForPrimaryCamera->viewOrigin, origin );
		Matrix3_Copy( stateForPrimaryCamera->refdef.viewaxis, axis );

		newPvsOrigin = stateForPrimaryCamera->viewOrigin;

		renderFlagsToAdd |= RF_PORTALVIEW;
	} else if( drawPortalFlags & DrawPortalMirror ) {
		VectorReflect( stateForPrimaryCamera->viewOrigin, portalPlane->normal, portalPlane->dist, origin );

		VectorReflect( &stateForPrimaryCamera->viewAxis[AXIS_FORWARD], portalPlane->normal, 0, &axis[AXIS_FORWARD] );
		VectorReflect( &stateForPrimaryCamera->viewAxis[AXIS_RIGHT], portalPlane->normal, 0, &axis[AXIS_RIGHT] );
		VectorReflect( &stateForPrimaryCamera->viewAxis[AXIS_UP], portalPlane->normal, 0, &axis[AXIS_UP] );

		Matrix3_Normalize( axis );

		newPvsOrigin = stateForPrimaryCamera->viewOrigin;

		renderFlagsToAdd = stateForPrimaryCamera->renderFlags | RF_MIRRORVIEW;
	} else {
		vec3_t tvec;
		mat3_t A, B, C, rot;

		// build world-to-portal rotation matrix
		VectorNegate( portalPlane->normal, tvec );
		NormalVectorToAxis( tvec, A );

		// build portal_dest-to-world rotation matrix
		ByteToDir( portalEntity->frame, tvec );
		NormalVectorToAxis( tvec, B );
		Matrix3_Transpose( B, C );

		// multiply to get world-to-world rotation matrix
		Matrix3_Multiply( C, A, rot );

		// translate view origin
		VectorSubtract( stateForPrimaryCamera->viewOrigin, portalEntity->origin, tvec );
		Matrix3_TransformVector( rot, tvec, origin );
		VectorAdd( origin, portalEntity->origin2, origin );

		Matrix3_Transpose( A, B );
		// TODO: Why do we use a view-dependent axis TODO: Check Q3 code
		Matrix3_Multiply( stateForPrimaryCamera->viewAxis, B, rot );
		Matrix3_Multiply( portalEntity->axis, rot, B );
		Matrix3_Transpose( C, A );
		Matrix3_Multiply( B, A, axis );

		// set up portalPlane
		VectorCopy( &axis[AXIS_FORWARD], portalPlane->normal );
		portalPlane->dist = DotProduct( portalEntity->origin2, portalPlane->normal );
		CategorizePlane( portalPlane );

		// for portals, vis data is taken from portal origin, not
		// view origin, because the view point moves around and
		// might fly into (or behind) a wall
		newPvsOrigin = portalEntity->origin2;
		newLodOrigin = portalEntity->origin2;

		renderFlagsToAdd |= RF_PORTALVIEW;

		// ignore entities, if asked politely
		if( portalEntity->renderfx & RF_NOPORTALENTS ) {
			renderFlagsToAdd |= RF_ENVVIEW;
		}
	}

	refdef_t newRefdef = stateForPrimaryCamera->refdef;
	newRefdef.rdflags &= ~( RDF_UNDERWATER | RDF_CROSSINGWATER );
	// Note: Inheritting RDF_NOBSPOCCLUSIONCULLING

	renderFlagsToAdd   |= RF_CLIPPLANE;
	renderFlagsToClear |= RF_SOFT_PARTICLES;

	if( newPvsOrigin ) {
		// TODO: Try using a different frustum for selection of occluders in this case?
		if( Mod_PointInLeaf( origin, rsh.worldModel )->cluster < 0 ) {
			renderFlagsToAdd |= RF_NOOCCLUSIONCULLING;
		}
	}

	Texture *captureTexture = nullptr;
	if( drawPortalFlags & DrawPortalToTexture ) {
		RenderTargetComponents *components;
		do {
			[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_portalTextureLock );
			components = TextureCache::instance()->getPortalRenderTarget( m_drawSceneFrame );
		} while( false );
		// TODO: Should not it be limited per-viewport, not per-frame?
		if( components ) {
			newRefdef.renderTarget = components;
			captureTexture         = components->texture;

			newRefdef.x      = newRefdef.scissor_x      = 0;
			newRefdef.y      = newRefdef.scissor_y      = 0;
			newRefdef.width  = newRefdef.scissor_width  = components->texture->width;
			newRefdef.height = newRefdef.scissor_height = components->texture->height;

			renderFlagsToAdd |= RF_PORTAL_CAPTURE;
		} else {
			return std::nullopt;
		}
	} else {
		renderFlagsToClear |= RF_PORTAL_CAPTURE;
	}

	VectorCopy( origin, newRefdef.vieworg );
	Matrix3_Copy( axis, newRefdef.viewaxis );

	auto *stateForPortalCamera = setupStateForCamera( &newRefdef, stateForPrimaryCamera->sceneIndex, CameraOverrideParams {
		.pvsOrigin          = newPvsOrigin,
		.lodOrigin          = newLodOrigin,
		.renderFlagsToAdd   = renderFlagsToAdd,
		.renderFlagsToClear = renderFlagsToClear,
	});

	if( !stateForPortalCamera ) {
		return std::nullopt;
	}

	return std::make_pair( stateForPortalCamera, captureTexture );
}

void Frontend::submitDebugStuffToBackend( Scene *scene ) {
	// TODO: Reduce this copying
	vec4_t verts[2];
	byte_vec4_t colors[2] { { 0, 0, 0, 1 }, { 0, 0, 0, 1 } };
	elem_t elems[2] { 0, 1 };

	mesh_t mesh {};
	mesh.colorsArray[0] = colors;
	mesh.xyzArray = verts;
	mesh.numVerts = 2;
	mesh.numElems = 2;
	mesh.elems = elems;
	verts[0][3] = verts[1][3] = 1.0f;
	for( const DebugLine &line: m_debugLines ) {
		VectorCopy( line.p1, verts[0] );
		VectorCopy( line.p2, verts[1] );
		std::memcpy( colors[0], &line.color, 4 );
		std::memcpy( colors[1], &line.color, 4 );
		RB_AddDynamicMesh( scene->m_worldent, rsh.whiteShader, nullptr, nullptr, 0, &mesh, GL_LINES, 0.0f, 0.0f );
	}

	RB_FlushDynamicMeshes();

	m_debugLines.clear();
}

void Frontend::addDebugLine( const float *p1, const float *p2, int color ) {
	int rgbaColor = color;
	if( !COLOR_A( rgbaColor ) ) {
		rgbaColor = COLOR_RGBA( COLOR_R( color ), COLOR_G( color ), COLOR_B( color ), 255 );
	}
	m_debugLines.emplace_back( DebugLine {
		{ p1[0], p1[1], p1[2] }, { p2[0], p2[1], p2[2] }, rgbaColor
	});
}

void Frontend::prepareDynamicMesh( DynamicMeshFillDataWorkload *workload ) {
	assert( workload->drawSurface );
	const DynamicMesh *dynamicMesh = workload->drawSurface->dynamicMesh;
	assert( dynamicMesh );

	unsigned numAffectingLights     = 0;
	uint16_t *affectingLightIndices = nullptr;
	std::span<const uint16_t> lightIndicesSpan;
	if( dynamicMesh->applyVertexDynLight && r_dynamiclight->integer && workload->stateForCamera->numAllVisibleLights ) {
		std::span<const uint16_t> availableLights { workload->stateForCamera->allVisibleLightIndices,
													workload->stateForCamera->numAllVisibleLights };

		affectingLightIndices = (uint16_t *)alloca( sizeof( uint16_t ) * workload->stateForCamera->numAllVisibleLights );
		numAffectingLights = findLightsThatAffectBounds( workload->scene->m_dynamicLights.data(), availableLights,
														 dynamicMesh->cullMins, dynamicMesh->cullMaxs, affectingLightIndices );

		lightIndicesSpan = { affectingLightIndices, numAffectingLights };
	}

	auto [numVertices, numIndices] = dynamicMesh->fillMeshBuffers( workload->stateForCamera->viewOrigin,
																   workload->stateForCamera->viewAxis,
																   workload->stateForCamera->lodScaleForFov,
																   workload->stateForCamera->cameraId,
																   workload->scene->m_dynamicLights.data(),
																   lightIndicesSpan,
																   workload->drawSurface->scratchpad,
																   workload->destData->positions,
																   workload->destData->normals,
																   workload->destData->texCoords,
																   workload->destData->colors,
																   workload->destData->indices );
	assert( numVertices <= workload->drawSurface->requestedNumVertices );
	assert( numIndices <= workload->drawSurface->requestedNumIndices );
	workload->drawSurface->actualNumVertices = numVertices;
	workload->drawSurface->actualNumIndices  = numIndices;

	// fillMeshBuffers() may legally return zeroes (even if the initially requested numbers were non-zero)
	if( numVertices && numIndices ) {
		mesh_t mesh;

		std::memset( &mesh, 0, sizeof( mesh_t ) );
		mesh.numVerts       = numVertices;
		mesh.numElems       = numIndices;
		mesh.xyzArray       = workload->destData->positions;
		mesh.stArray        = workload->destData->texCoords;
		mesh.colorsArray[0] = workload->destData->colors;
		mesh.elems          = workload->destData->indices;

		R_SetFrameUploadMeshSubdata( UPLOAD_GROUP_DYNAMIC_MESH, workload->drawSurface->verticesOffset,
									 workload->drawSurface->indicesOffset, &mesh );
	}
}

static wsw_forceinline void calcAddedParticleLight( const float *__restrict particleOrigin,
													const Scene::DynamicLight *__restrict lights,
													std::span<const uint16_t> affectingLightIndices,
													float *__restrict addedLight ) {
	assert( !affectingLightIndices.empty() );

	size_t lightNum = 0;
	do {
		const Scene::DynamicLight *light = lights + affectingLightIndices[lightNum];
		const float squareDistance = DistanceSquared( light->origin, particleOrigin );
		// May go outside [0.0, 1.0] as we test against the bounding box of the entire aggregate
		float impactStrength = 1.0f - Q_Sqrt( squareDistance ) * Q_Rcp( light->maxRadius );
		// Just clamp so the code stays branchless
		impactStrength = wsw::clamp( impactStrength, 0.0f, 1.0f );
		VectorMA( addedLight, impactStrength, light->color, addedLight );
	} while( ++lightNum < affectingLightIndices.size() );
}

// TODO: Write to mapped buffers directly
struct BatchedMeshBuilder {
	vec4_t *meshPositions { nullptr };
	vec2_t *meshTexCoords { nullptr };
	byte_vec4_t *meshColors { nullptr };
	uint16_t *meshIndices { nullptr };
	unsigned numVerticesSoFar { 0 };
	unsigned numIndicesSoFar { 0 };

	void reserveForQuads( Frontend *self, unsigned numQuads ) {
		this->meshPositions = self->m_tmpMeshPositions.reserveAndGet( 4 * numQuads );
		this->meshTexCoords = self->m_tmpMeshTexCoords.reserveAndGet( 4 * numQuads );
		this->meshColors    = self->m_tmpMeshColors.reserveAndGet( 4 * numQuads );
		this->meshIndices   = self->m_tmpMeshIndices.reserveAndGet( 6 * numQuads );

		this->numVerticesSoFar = 0;
		this->numIndicesSoFar  = 0;
	}

	void appendQuad( const vec4_t positions[4], const vec2_t texCoords[4], const byte_vec4_t colors[4], const uint16_t indices[6] ) {
		std::memcpy( meshPositions + numVerticesSoFar, positions, 4 * sizeof( vec4_t ) );
		std::memcpy( meshTexCoords + numVerticesSoFar, texCoords, 4 * sizeof( vec2_t ) );
		std::memcpy( meshColors + numVerticesSoFar, colors, 4 * sizeof( byte_vec4_t ) );

		for( unsigned i = 0; i < 6; ++i ) {
			meshIndices[numIndicesSoFar + i] = numVerticesSoFar + indices[i];
		}

		numVerticesSoFar += 4;
		numIndicesSoFar += 6;
	}
};

static void uploadBatchedMesh( BatchedMeshBuilder *builder, VertElemSpan *inOutSpan ) {
	if( builder->numVerticesSoFar && builder->numIndicesSoFar ) {
		mesh_t mesh;
		std::memset( &mesh, 0, sizeof( mesh_t ) );

		mesh.numVerts       = builder->numVerticesSoFar;
		mesh.numElems       = builder->numIndicesSoFar;
		mesh.xyzArray       = builder->meshPositions;
		mesh.stArray        = builder->meshTexCoords;
		mesh.colorsArray[0] = builder->meshColors;
		mesh.elems          = builder->meshIndices;

		const unsigned verticesOffset = inOutSpan->firstVert;
		const unsigned indicesOffset  = inOutSpan->firstElem;

		R_SetFrameUploadMeshSubdata( UPLOAD_GROUP_BATCHED_MESH, verticesOffset, indicesOffset, &mesh );

		// Correct original estimations by final values
		assert( builder->numVerticesSoFar <= inOutSpan->numVerts );
		assert( builder->numIndicesSoFar <= inOutSpan->numElems );
		inOutSpan->numVerts = builder->numVerticesSoFar;
		inOutSpan->numElems = builder->numIndicesSoFar;
	} else {
		// Suppress drawing attempts
		inOutSpan->numVerts = 0;
		inOutSpan->numElems = 0;
	}
}

static void buildMeshForSpriteParticles( BatchedMeshBuilder *meshBuilder,
										 const Scene::ParticlesAggregate *aggregate,
										 std::span<const sortedDrawSurf_t> surfSpan,
										 const float *viewAxis, bool shouldMirrorView,
										 const Scene::DynamicLight *dynamicLights,
										 std::span<const uint16_t> affectingLightIndices ) {
	const auto *__restrict appearanceRules = &aggregate->appearanceRules;
	const auto *__restrict spriteRules     = std::get_if<Particle::SpriteRules>( &appearanceRules->geometryRules );
	const bool applyLight                  = !affectingLightIndices.empty();

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		// TODO: Write directly to mapped buffers
		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		// TODO: Do we need normals?
		vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
		byte_vec4_t colors[4];
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		const auto *drawSurf = (const ParticleDrawSurface *)sds.drawSurf;

		// Ensure that the aggregate is the same
		//assert( fsh->particleAggregates + drawSurf->aggregateIndex == aggregate );

		assert( drawSurf->particleIndex < aggregate->numParticles );
		const Particle *const __restrict particle = aggregate->particles + drawSurf->particleIndex;

		assert( particle->lifetimeFrac >= 0.0f && particle->lifetimeFrac <= 1.0f );

		assert( spriteRules->radius.mean > 0.0f );
		assert( spriteRules->radius.spread >= 0.0f );

		float signedFrac = Particle::kByteSpreadNormalizer * (float)particle->instanceRadiusSpreadFraction;
		float radius     = wsw::max( 0.0f, spriteRules->radius.mean + signedFrac * spriteRules->radius.spread );

		radius *= Particle::kScaleOfByteExtraScale * (float)particle->instanceRadiusExtraScale;

		if( spriteRules->sizeBehaviour != Particle::SizeNotChanging ) {
			radius *= calcSizeFracForLifetimeFrac( particle->lifetimeFrac, spriteRules->sizeBehaviour );
		}

		if( radius < 0.1f ) {
			continue;
		}

		vec3_t v_left, v_up;
		if( particle->rotationAngle != 0.0f ) {
			mat3_t axis;
			Matrix3_Rotate( viewAxis, particle->rotationAngle, &viewAxis[AXIS_FORWARD], axis );
			VectorCopy( &axis[AXIS_RIGHT], v_left );
			VectorCopy( &axis[AXIS_UP], v_up );
		} else {
			VectorCopy( &viewAxis[AXIS_RIGHT], v_left );
			VectorCopy( &viewAxis[AXIS_UP], v_up );
		}

		if( shouldMirrorView ) {
			VectorInverse( v_left );
		}

		vec3_t point;
		VectorMA( particle->origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( particle->origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		vec4_t colorBuffer;
		const RgbaLifespan &colorLifespan = appearanceRules->colors[particle->instanceColorIndex];
		colorLifespan.getColorForLifetimeFrac( particle->lifetimeFrac, colorBuffer );

		if( applyLight ) {
			vec4_t addedLight { 0.0f, 0.0f, 0.0f, 1.0f };
			calcAddedParticleLight( particle->origin, dynamicLights, affectingLightIndices, addedLight );

			// TODO: Pass as a floating-point attribute to a GPU program?
			colorBuffer[0] = wsw::min( 1.0f, colorBuffer[0] + addedLight[0] );
			colorBuffer[1] = wsw::min( 1.0f, colorBuffer[1] + addedLight[1] );
			colorBuffer[2] = wsw::min( 1.0f, colorBuffer[2] + addedLight[2] );
		}

		Vector4Set( colors[0],
					(uint8_t)( 255 * colorBuffer[0] ),
					(uint8_t)( 255 * colorBuffer[1] ),
					(uint8_t)( 255 * colorBuffer[2] ),
					(uint8_t)( 255 * colorBuffer[3] ) );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		meshBuilder->appendQuad( xyz, texcoords, colors, elems );
	}
}

static void buildMeshForSparkParticles( BatchedMeshBuilder *meshBuilder,
										const Scene::ParticlesAggregate *aggregate,
										std::span<const sortedDrawSurf_t> surfSpan,
										const float *viewOrigin, const float *viewAxis,
										const Scene::DynamicLight *dynamicLights,
										std::span<const uint16_t> affectingLightIndices ) {
	const auto *__restrict appearanceRules = &aggregate->appearanceRules;
	const auto *__restrict sparkRules      = std::get_if<Particle::SparkRules>( &appearanceRules->geometryRules );
	const bool applyLight                  = !affectingLightIndices.empty();

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		// TODO: Write directly to mapped buffers
		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		// TODO: Do we need normals?
		vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
		byte_vec4_t colors[4];
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		const auto *drawSurf = (const ParticleDrawSurface *)sds.drawSurf;

		// Ensure that the aggregate is the same
		//assert( fsh->particleAggregates + drawSurf->aggregateIndex == aggregate );

		assert( drawSurf->particleIndex < aggregate->numParticles );
		const Particle *const __restrict particle = aggregate->particles + drawSurf->particleIndex;

		assert( particle->lifetimeFrac >= 0.0f && particle->lifetimeFrac <= 1.0f );

		assert( sparkRules->length.mean >= 0.1f && sparkRules->width.mean >= 0.1f );
		assert( sparkRules->length.spread >= 0.0f && sparkRules->width.spread >= 0.0f );

		const float lengthSignedFrac = Particle::kByteSpreadNormalizer * (float)particle->instanceLengthSpreadFraction;
		const float widthSignedFrac  = Particle::kByteSpreadNormalizer * (float)particle->instanceWidthSpreadFraction;

		float length = wsw::max( 0.0f, sparkRules->length.mean + lengthSignedFrac * sparkRules->length.spread );
		float width  = wsw::max( 0.0f, sparkRules->width.mean + widthSignedFrac * sparkRules->width.spread );

		length *= Particle::kScaleOfByteExtraScale * (float)particle->instanceLengthExtraScale;
		width  *= Particle::kScaleOfByteExtraScale * (float)particle->instanceWidthExtraScale;

		const Particle::SizeBehaviour sizeBehaviour = sparkRules->sizeBehaviour;
		if( sizeBehaviour != Particle::SizeNotChanging ) {
			const float sizeFrac = calcSizeFracForLifetimeFrac( particle->lifetimeFrac, sizeBehaviour );
			if( sizeBehaviour != Particle::SizeBehaviour::Thinning &&
				sizeBehaviour != Particle::SizeBehaviour::Thickening &&
				sizeBehaviour != Particle::SizeBehaviour::ThickeningAndThinning ) {
				length *= sizeFrac;
			}
			width *= sizeFrac;
		}

		if( length < 0.1f || width < 0.1f ) {
			continue;
		}

		vec3_t particleDir;
		float fromFrac, toFrac;
		vec3_t visualVelocity;
		VectorAdd( particle->dynamicsVelocity, particle->artificialVelocity, visualVelocity );
		if( const float squareVisualSpeed = VectorLengthSquared( visualVelocity ); squareVisualSpeed > 1.0f ) [[likely]] {
			const float rcpVisualSpeed = Q_RSqrt( squareVisualSpeed );
			if( particle->rotationAngle == 0.0f ) [[likely]] {
				VectorScale( visualVelocity, rcpVisualSpeed, particleDir );
				fromFrac = 0.0f, toFrac = 1.0f;
			} else {
				vec3_t tmpParticleDir;
				VectorScale( visualVelocity, rcpVisualSpeed, tmpParticleDir );

				mat3_t rotationMatrix;
				const float *rotationAxis = kPredefinedDirs[particle->rotationAxisIndex];
				Matrix3_Rotate( axis_identity, particle->rotationAngle, rotationAxis, rotationMatrix );
				Matrix3_TransformVector( rotationMatrix, tmpParticleDir, particleDir );

				fromFrac = -0.5f, toFrac = +0.5f;
			}
		} else {
			continue;
		}

		assert( std::fabs( VectorLengthSquared( particleDir ) - 1.0f ) < 0.1f );

		// Reduce the viewDir-aligned part of the particleDir
		const float *const __restrict viewDir = &viewAxis[AXIS_FORWARD];
		assert( sparkRules->viewDirPartScale >= 0.0f && sparkRules->viewDirPartScale <= 1.0f );
		const float viewDirCutScale = ( 1.0f - sparkRules->viewDirPartScale ) * DotProduct( particleDir, viewDir );
		if( std::fabs( viewDirCutScale ) < 0.999f ) [[likely]] {
			VectorMA( particleDir, -viewDirCutScale, viewDir, particleDir );
			VectorNormalizeFast( particleDir );
		} else {
			continue;
		}

		vec3_t from, to, mid;
		VectorMA( particle->origin, fromFrac * length, particleDir, from );
		VectorMA( particle->origin, toFrac * length, particleDir, to );
		VectorAvg( from, to, mid );

		vec3_t viewToMid, right;
		VectorSubtract( mid, viewOrigin, viewToMid );
		CrossProduct( viewToMid, particleDir, right );
		if( const float squareLength = VectorLengthSquared( right ); squareLength > wsw::square( 0.001f ) ) [[likely]] {
			const float rcpLength = Q_RSqrt( squareLength );
			VectorScale( right, rcpLength, right );

			const float halfWidth = 0.5f * width;

			VectorMA( from, +halfWidth, right, xyz[0] );
			VectorMA( from, -halfWidth, right, xyz[1] );
			VectorMA( to, -halfWidth, right, xyz[2] );
			VectorMA( to, +halfWidth, right, xyz[3] );
		} else {
			continue;
		}

		vec4_t colorBuffer;
		const RgbaLifespan &colorLifespan = appearanceRules->colors[particle->instanceColorIndex];
		colorLifespan.getColorForLifetimeFrac( particle->lifetimeFrac, colorBuffer );

		if( applyLight ) {
			alignas( 16 ) vec4_t addedLight { 0.0f, 0.0f, 0.0f, 1.0f };
			calcAddedParticleLight( particle->origin, dynamicLights, affectingLightIndices, addedLight );

			// The clipping due to LDR limitations sucks...
			// TODO: Pass as a floating-point attribute to a GPU program?
			colorBuffer[0] = wsw::min( 1.0f, colorBuffer[0] + addedLight[0] );
			colorBuffer[1] = wsw::min( 1.0f, colorBuffer[1] + addedLight[1] );
			colorBuffer[2] = wsw::min( 1.0f, colorBuffer[2] + addedLight[2] );
		}

		Vector4Set( colors[0],
					(uint8_t)( 255 * colorBuffer[0] ),
					(uint8_t)( 255 * colorBuffer[1] ),
					(uint8_t)( 255 * colorBuffer[2] ),
					(uint8_t)( 255 * colorBuffer[3] ) );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		meshBuilder->appendQuad( xyz, texcoords, colors, elems );
	}
}

void Frontend::prepareBatchedParticles( PrepareBatchedSurfWorkload *workload ) {
	const auto *const scene          = workload->scene;
	const auto *const stateForCamera = workload->stateForCamera;
	const auto surfSpan              = workload->batchSpan;
	const auto *const firstDrawSurf  = (const ParticleDrawSurface *)surfSpan.front().drawSurf;

	const Scene::ParticlesAggregate *const aggregate = scene->m_particles.data() + firstDrawSurf->aggregateIndex;
	// Less if the aggregate is visually split by some surfaces of other kinds
	assert( surfSpan.size() <= aggregate->numParticles );

	const Particle::AppearanceRules *const appearanceRules = &aggregate->appearanceRules;

	unsigned numAffectingLights     = 0;
	uint16_t *affectingLightIndices = nullptr;
	std::span<const uint16_t> lightIndicesSpan;

	if( appearanceRules->applyVertexDynLight && r_dynamiclight->integer ) {
		const std::span<uint16_t> allVisibleLightIndices = workload->stateForCamera->allVisibleLightIndices;
		if( !allVisibleLightIndices.empty() ) {
			affectingLightIndices = (uint16_t *)alloca( sizeof( uint16_t ) * allVisibleLightIndices.size() );

			numAffectingLights = findLightsThatAffectBounds( scene->m_dynamicLights.data(), allVisibleLightIndices,
															 aggregate->mins, aggregate->maxs, affectingLightIndices );

			lightIndicesSpan = { affectingLightIndices, numAffectingLights };
		}
	}

	BatchedMeshBuilder meshBuilder;
	meshBuilder.reserveForQuads( this, surfSpan.size() );

	if( std::holds_alternative<Particle::SpriteRules>( appearanceRules->geometryRules ) ) {
		buildMeshForSpriteParticles( &meshBuilder, aggregate, surfSpan, stateForCamera->viewAxis,
									 ( stateForCamera->renderFlags & RF_MIRRORVIEW ) != 0,
									 scene->m_dynamicLights.data(), lightIndicesSpan );
	} else if( std::holds_alternative<Particle::SparkRules>( appearanceRules->geometryRules ) ) {
		// TODO: We don't handle MIRRORVIEW
		buildMeshForSparkParticles( &meshBuilder, aggregate, surfSpan, stateForCamera->viewOrigin,
									stateForCamera->viewAxis, scene->m_dynamicLights.data(), lightIndicesSpan );
	} else {
		wsw::failWithRuntimeError( "Unreachable" );
	}

	uploadBatchedMesh( &meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void Frontend::prepareBatchedCoronas( PrepareBatchedSurfWorkload *workload ) {
	vec3_t v_left, v_up;
	VectorCopy( &workload->stateForCamera->viewAxis[AXIS_RIGHT], v_left );
	VectorCopy( &workload->stateForCamera->viewAxis[AXIS_UP], v_up );

	if( workload->stateForCamera->renderFlags & RF_MIRRORVIEW ) {
		VectorInverse( v_left );
	}

	BatchedMeshBuilder meshBuilder;
	meshBuilder.reserveForQuads( this, workload->batchSpan.size() );

	for( const sortedDrawSurf_t &sds: workload->batchSpan ) {
		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		// TODO: Do we need normals?
		vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
		byte_vec4_t colors[4];
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		const auto *light = (const Scene::DynamicLight *)sds.drawSurf;

		assert( light && light->hasCoronaLight );

		const float radius = light->coronaRadius;

		vec3_t origin;
		VectorCopy( light->origin, origin );

		vec3_t point;
		VectorMA( origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		Vector4Set( colors[0],
					bound( 0, light->color[0] * 96, 255 ),
					bound( 0, light->color[1] * 96, 255 ),
					bound( 0, light->color[2] * 96, 255 ),
					255 );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		meshBuilder.appendQuad( xyz, texcoords, colors, elems );
	}

	uploadBatchedMesh( &meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void Frontend::prepareBatchedQuadPolys( PrepareBatchedSurfWorkload *workload ) {
	BatchedMeshBuilder meshBuilder;
	meshBuilder.reserveForQuads( this, workload->batchSpan.size() );

	[[maybe_unused]] const float *const viewOrigin = workload->stateForCamera->viewOrigin;
	[[maybe_unused]] const float *const viewAxis   = workload->stateForCamera->viewAxis;
	[[maybe_unused]] const bool shouldMirrorView   = ( workload->stateForCamera->renderFlags & RF_MIRRORVIEW ) != 0;

	for( const sortedDrawSurf_t &sds: workload->batchSpan ) {
		uint16_t indices[6] { 0, 1, 2, 0, 2, 3 };

		vec4_t positions[4];
		byte_vec4_t colors[4];
		vec2_t texCoords[4];

		positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

		const auto *__restrict poly = (const QuadPoly *)sds.drawSurf;

		if( const auto *__restrict beamRules = std::get_if<QuadPoly::ViewAlignedBeamRules>( &poly->appearanceRules ) ) {
			assert( std::fabs( VectorLengthFast( beamRules->dir ) - 1.0f ) < 1.01f );
			vec3_t viewToOrigin, right;
			VectorSubtract( poly->origin, viewOrigin, viewToOrigin );
			CrossProduct( viewToOrigin, beamRules->dir, right );

			const float squareLength = VectorLengthSquared( right );
			if( squareLength > wsw::square( 0.001f ) ) [[likely]] {
				const float rcpLength = Q_RSqrt( squareLength );
				VectorScale( right, rcpLength, right );

				const float halfWidth = 0.5f * beamRules->width;

				vec3_t from, to;
				VectorMA( poly->origin, -poly->halfExtent, beamRules->dir, from );
				VectorMA( poly->origin, +poly->halfExtent, beamRules->dir, to );

				VectorMA( from, +halfWidth, right, positions[0] );
				VectorMA( from, -halfWidth, right, positions[1] );
				VectorMA( to, -halfWidth, right, positions[2] );
				VectorMA( to, +halfWidth, right, positions[3] );

				float stx = 1.0f;
				if( beamRules->tileLength > 0 ) {
					const float fullExtent = 2.0f * poly->halfExtent;
					stx = fullExtent * Q_Rcp( beamRules->tileLength );
				}

				Vector2Set( texCoords[0], 0.0f, 0.0f );
				Vector2Set( texCoords[1], 0.0f, 1.0f );
				Vector2Set( texCoords[2], stx, 1.0f );
				Vector2Set( texCoords[3], stx, 0.0f );

				const byte_vec4_t fromColorAsBytes {
					(uint8_t)( beamRules->fromColor[0] * 255 ),
					(uint8_t)( beamRules->fromColor[1] * 255 ),
					(uint8_t)( beamRules->fromColor[2] * 255 ),
					(uint8_t)( beamRules->fromColor[3] * 255 ),
				};
				const byte_vec4_t toColorAsBytes {
					(uint8_t)( beamRules->toColor[0] * 255 ),
					(uint8_t)( beamRules->toColor[1] * 255 ),
					(uint8_t)( beamRules->toColor[2] * 255 ),
					(uint8_t)( beamRules->toColor[3] * 255 ),
				};

				Vector4Copy( fromColorAsBytes, colors[0] );
				Vector4Copy( fromColorAsBytes, colors[1] );
				Vector4Copy( toColorAsBytes, colors[2] );
				Vector4Copy( toColorAsBytes, colors[3] );

				meshBuilder.appendQuad( positions, texCoords, colors, indices );
			}
		} else {
			vec3_t left, up;
			const float *color;

			if( const auto *orientedRules = std::get_if<QuadPoly::OrientedSpriteRules>( &poly->appearanceRules ) ) {
				color = orientedRules->color;
				VectorCopy( &orientedRules->axis[AXIS_RIGHT], left );
				VectorCopy( &orientedRules->axis[AXIS_UP], up );
			} else {
				color = std::get_if<QuadPoly::OrientedSpriteRules>( &poly->appearanceRules )->color;
				VectorCopy( &viewAxis[AXIS_RIGHT], left );
				VectorCopy( &viewAxis[AXIS_UP], up );
			}

			if( shouldMirrorView ) {
				VectorInverse( left );
			}

			vec3_t point;
			const float radius = poly->halfExtent;
			VectorMA( poly->origin, -radius, up, point );
			VectorMA( point, +radius, left, positions[0] );
			VectorMA( point, -radius, left, positions[3] );

			VectorMA( poly->origin, radius, up, point );
			VectorMA( point, +radius, left, positions[1] );
			VectorMA( point, -radius, left, positions[2] );

			Vector2Set( texCoords[0], 0.0f, 0.0f );
			Vector2Set( texCoords[1], 0.0f, 1.0f );
			Vector2Set( texCoords[2], 1.0f, 1.0f );
			Vector2Set( texCoords[3], 1.0f, 0.0f );

			colors[0][0] = ( uint8_t )( color[0] * 255 );
			colors[0][1] = ( uint8_t )( color[1] * 255 );
			colors[0][2] = ( uint8_t )( color[2] * 255 );
			colors[0][3] = ( uint8_t )( color[3] * 255 );

			Vector4Copy( colors[0], colors[1] );
			Vector4Copy( colors[0], colors[2] );
			Vector4Copy( colors[0], colors[3] );

			meshBuilder.appendQuad( positions, texCoords, colors, indices );
		}
	}

	uploadBatchedMesh( &meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void R_SubmitSpriteSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
								   const mfog_t *fog, const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	mesh_t mesh;

	for( size_t surfInSpan = 0; surfInSpan < surfSpan.size(); ++surfInSpan ) {

		vec3_t v_left, v_up;
		if( const float rotation = e->rotation; rotation != 0.0f ) {
			RotatePointAroundVector( v_left, &fsh->viewAxis[AXIS_FORWARD], &fsh->viewAxis[AXIS_RIGHT], rotation );
			CrossProduct( &fsh->viewAxis[AXIS_FORWARD], v_left, v_up );
		} else {
			VectorCopy( &fsh->viewAxis[AXIS_RIGHT], v_left );
			VectorCopy( &fsh->viewAxis[AXIS_UP], v_up );
		}

		if( fsh->renderFlags & RF_MIRRORVIEW ) {
			VectorInverse( v_left );
		}

		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };

		vec3_t point;
		const float radius = e->radius * e->scale;
		VectorMA( e->origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( e->origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		byte_vec4_t colors[4];
		for( unsigned i = 0; i < 4; i++ ) {
			VectorNegate( &fsh->viewAxis[AXIS_FORWARD], normals[i] );
			Vector4Copy( e->color, colors[i] );
		}

		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		mesh.elems = elems;
		mesh.numElems = 6;
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.lmstArray[0] = NULL;
		mesh.lmlayersArray[0] = NULL;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;
		mesh.colorsArray[1] = NULL;
		mesh.sVectorsArray = NULL;

		RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
	}
}

}

auto findLightsThatAffectBounds( const Scene::DynamicLight *lights, std::span<const uint16_t> lightIndicesSpan,
								 const float *mins, const float *maxs, uint16_t *affectingLightIndices ) -> unsigned {
	assert( mins[3] == 0.0f && maxs[3] == 1.0f );

	const uint16_t *lightIndices = lightIndicesSpan.data();
	const auto numLights         = (unsigned)lightIndicesSpan.size();

	unsigned lightIndexNum = 0;
	unsigned numAffectingLights = 0;
	do {
		const uint16_t lightIndex = lightIndices[lightIndexNum];
		const Scene::DynamicLight *light = lights + lightIndex;

		// TODO: Use SIMD explicitly without these redundant loads/shuffles
		const bool overlaps = BoundsIntersect( light->mins, light->maxs, mins, maxs );

		affectingLightIndices[numAffectingLights] = lightIndex;
		numAffectingLights += overlaps;
	} while( ++lightIndexNum < numLights );

	return numAffectingLights;
}