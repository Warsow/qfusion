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
	// Caution! There is an implication that preparing uploads is executed in serial fashion by primary and portal camera groups
	wsw::PodVector<DynamicMeshFillDataWorkload> *tmpDynamicMeshFillDataWorkload = &self->m_tmpDynamicMeshFillDataWorkload;
	wsw::PodVector<DynamicMeshData> *tmpDynamicMeshData = &self->m_tmpDynamicMeshData;
	// The primary call starts uploads, make sure the following portal call dones not reset values
	if( !areCamerasPortalCameras ) {
		tmpDynamicMeshFillDataWorkload->clear();
		tmpDynamicMeshData->clear();
		self->m_dynamicMeshOffsetsOfVerticesAndIndices = { 0, 0 };
		R_BeginFrameUploads();
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
	if( !tmpDynamicMeshFillDataWorkload->empty() ) {
		auto fn = [=]( unsigned, unsigned elemIndex ) {
			DynamicMeshFillDataWorkload &workload = tmpDynamicMeshFillDataWorkload->operator[]( elemIndex );
			assert( workload.drawSurface );
			const DynamicMesh *dynamicMesh = workload.drawSurface->dynamicMesh;
			assert( dynamicMesh );

			unsigned numAffectingLights     = 0;
			uint16_t *affectingLightIndices = nullptr;
			std::span<const uint16_t> lightIndicesSpan;
			if( dynamicMesh->applyVertexDynLight && r_dynamiclight->integer && workload.stateForCamera->numAllVisibleLights ) {
				std::span<const uint16_t> availableLights { workload.stateForCamera->allVisibleLightIndices,
															workload.stateForCamera->numAllVisibleLights };

				affectingLightIndices = (uint16_t *)alloca( sizeof( uint16_t ) * workload.stateForCamera->numAllVisibleLights );
				numAffectingLights = findLightsThatAffectBounds( workload.scene->m_dynamicLights.data(), availableLights,
																 dynamicMesh->cullMins, dynamicMesh->cullMaxs, affectingLightIndices );

				lightIndicesSpan = { affectingLightIndices, numAffectingLights };
			}

			auto [numVertices, numIndices] = dynamicMesh->fillMeshBuffers( workload.stateForCamera->viewOrigin,
																		   workload.stateForCamera->viewAxis,
																		   workload.stateForCamera->lodScaleForFov,
																		   workload.stateForCamera->cameraId,
																		   workload.scene->m_dynamicLights.data(),
																		   lightIndicesSpan,
																		   workload.drawSurface->scratchpad,
																		   workload.destData->positions,
																		   workload.destData->normals,
																		   workload.destData->texCoords,
																		   workload.destData->colors,
																		   workload.destData->indices );
			assert( numVertices <= workload.drawSurface->requestedNumVertices );
			assert( numIndices <= workload.drawSurface->requestedNumIndices );
			workload.drawSurface->actualNumVertices = numVertices;
			workload.drawSurface->actualNumIndices  = numIndices;

			// fillMeshBuffers() may legally return zeroes (even if the initially requested numbers were non-zero)
			if( numVertices && numIndices ) {
				mesh_t mesh;

				std::memset( &mesh, 0, sizeof( mesh_t ) );
				mesh.numVerts       = numVertices;
				mesh.numElems       = numIndices;
				mesh.xyzArray       = workload.destData->positions;
				mesh.stArray        = workload.destData->texCoords;
				mesh.colorsArray[0] = workload.destData->colors;
				mesh.elems          = workload.destData->indices;

				R_SetFrameUploadMeshSubdata( workload.drawSurface->verticesOffset, workload.drawSurface->indicesOffset, &mesh );
			}
		};

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

	// If there's processing of portals, finish it prior to awaiting uploads
	if( endPreparingRenderingFromPortalsTask ) {
		co_await si.taskSystem->awaiterOf( endPreparingRenderingFromPortalsTask );
	}

	if( fillMeshBuffersTask ) {
		co_await si.taskSystem->awaiterOf( fillMeshBuffersTask );
	}

	// The primary group of cameras ends uploads
	if( !areCamerasPortalCameras ) {
		R_EndFrameUploads();
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

}