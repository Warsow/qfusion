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

using drawSurf_cb = void (*)( const FrontendToBackendShared *, const entity_t *, const struct shader_s *,
							  const struct mfog_s *, const struct portalSurface_s *, const void * );

static const drawSurf_cb r_drawSurfCb[ST_MAX_TYPES] = {
	/* ST_NONE */
	nullptr,
	/* ST_BSP */
	( drawSurf_cb ) &R_SubmitBSPSurfToBackend,
	/* ST_ALIAS */
	( drawSurf_cb ) &R_SubmitAliasSurfToBackend,
	/* ST_SKELETAL */
	( drawSurf_cb ) &R_SubmitSkeletalSurfToBackend,
	/* ST_SPRITE */
	nullptr,
	/* ST_QUAD_POLY */
	nullptr,
	/* ST_DYNAMIC_MESH */
	( drawSurf_cb ) &R_SubmitDynamicMeshToBackend,
	/* ST_PARTICLE */
	nullptr,
	/* ST_CORONA */
	nullptr,
	/* ST_NULLMODEL */
	( drawSurf_cb ) & R_SubmitNullSurfToBackend,
};

auto Frontend::registerBuildingBatchedSurf( StateForCamera *stateForCamera, Scene *scene, unsigned surfType,
											std::span<const sortedDrawSurf_t> batchSpan ) -> std::pair<SubmitBatchedSurfFn, unsigned> {
	assert( !r_drawSurfCb[surfType] );

	SubmitBatchedSurfFn resultFn;
	unsigned resultOffset;
	if( surfType != ST_SPRITE ) [[likely]] {
		wsw::PodVector<PrepareBatchedSurfWorkload> *workloadList;
		if( surfType == ST_PARTICLE ) {
			workloadList = stateForCamera->prepareParticlesWorkload;
		} else if( surfType == ST_CORONA ) {
			workloadList = stateForCamera->prepareCoronasWorkload;
		} else if( surfType == ST_QUAD_POLY ) {
			workloadList = stateForCamera->preparePolysWorkload;
		} else {
			wsw::failWithRuntimeError( "Unreachable" );
		}

		resultOffset = stateForCamera->batchedSurfVertSpans->size();
		// Reserve space at [resultOffset]
		stateForCamera->batchedSurfVertSpans->append( {} );

		workloadList->append( PrepareBatchedSurfWorkload {
			.batchSpan      = batchSpan,
			.scene          = scene,
			.stateForCamera = stateForCamera,
			.vertSpanOffset = resultOffset,
		});

		resultFn = R_SubmitBatchedSurfsToBackend;
	} else {
		resultOffset = stateForCamera->preparedSpriteMeshes->size();
		// We need an element for each mesh in span
		stateForCamera->preparedSpriteMeshes->resize( stateForCamera->preparedSpriteMeshes->size() + batchSpan.size() );

		stateForCamera->prepareSpritesWorkload->append( PrepareSpriteSurfWorkload {
			.batchSpan       = batchSpan,
			.stateForCamera  = stateForCamera,
			.firstMeshOffset = resultOffset,
		});

		resultFn = R_SubmitSpriteSurfsToBackend;
	}

	return { resultFn, resultOffset };
}

void Frontend::processSortList( StateForCamera *stateForCamera, Scene *scene ) {
	stateForCamera->drawActionsList->clear();

	stateForCamera->preparePolysWorkload->clear();
	stateForCamera->prepareCoronasWorkload->clear();
	stateForCamera->prepareParticlesWorkload->clear();
	stateForCamera->batchedSurfVertSpans->clear();
	stateForCamera->prepareSpritesWorkload->clear();
	stateForCamera->preparedSpriteMeshes->clear();

	const auto *sortList = stateForCamera->sortList;
	if( sortList->empty() ) [[unlikely]] {
		return;
	}

	const auto cmp = []( const sortedDrawSurf_t &lhs, const sortedDrawSurf_t &rhs ) {
		// TODO: Avoid runtime composition of keys
		const auto lhsKey = ( (uint64_t)lhs.distKey << 32 ) | (uint64_t)lhs.sortKey;
		const auto rhsKey = ( (uint64_t)rhs.distKey << 32 ) | (uint64_t)rhs.sortKey;
		return lhsKey < rhsKey;
	};

	std::sort( stateForCamera->sortList->begin(), stateForCamera->sortList->end(), cmp );
	stateForCamera->drawActionsList->reserve( stateForCamera->sortList->size() );

	auto *const materialCache   = MaterialCache::instance();
	auto *const drawActionsList = stateForCamera->drawActionsList;

	unsigned prevShaderNum                 = ~0;
	unsigned prevEntNum                    = ~0;
	int prevPortalNum                      = ~0;
	int prevFogNum                         = ~0;
	unsigned prevMergeabilitySeparator     = ~0;
	unsigned prevSurfType                  = ~0;
	bool prevIsDrawSurfBatched             = false;
	const sortedDrawSurf_t *batchSpanBegin = nullptr;

	bool depthHack = false, cullHack = false;
	bool prevInfiniteProj = false;
	int prevEntityFX = -1;

	const mfog_t *prevFog = nullptr;
	const portalSurface_t *prevPortalSurface = nullptr;

	const size_t numDrawSurfs = sortList->size();
	const sortedDrawSurf_t *const drawSurfs = sortList->data();
	for( size_t i = 0; i < numDrawSurfs; i++ ) {
		const sortedDrawSurf_t *sds = drawSurfs + i;
		const unsigned sortKey      = sds->sortKey;
		const unsigned surfType     = sds->surfType;

		assert( surfType > ST_NONE && surfType < ST_MAX_TYPES );

		const bool isDrawSurfBatched = ( r_drawSurfCb[surfType] == nullptr );

		unsigned shaderNum, entNum;
		int fogNum, portalNum;
		// decode draw surface properties
		R_UnpackSortKey( sortKey, &shaderNum, &fogNum, &portalNum, &entNum );

		const shader_t *shader    = materialCache->getMaterialById( shaderNum );
		const entity_t *entity    = scene->m_entities[entNum];
		const mfog_t *fog         = fogNum >= 0 ? rsh.worldBrushModel->fogs + fogNum : nullptr;
		const auto *portalSurface = portalNum >= 0 ? stateForCamera->portalSurfaces + portalNum : nullptr;
		const int entityFX        = entity->renderfx;

		// TODO?
		// const bool depthWrite     = shader->flags & SHADER_DEPTHWRITE ? true : false;

		// see if we need to reset mesh properties in the backend

		// TODO: Use a single 64-bit compound mergeability key

		bool reset = false;
		if( !prevIsDrawSurfBatched ) {
			reset = true;
		} else if( surfType != prevSurfType ) {
			reset = true;
		} else if( shaderNum != prevShaderNum ) {
			reset = true;
		} else if( sds->mergeabilitySeparator != prevMergeabilitySeparator ) {
			reset = true;
		} else if( fogNum != prevFogNum ) {
			reset = true;
		} else if( portalNum != prevPortalNum ) {
			reset = true;
		} else if( entNum != prevEntNum ) {
			reset = true;
		} else if( entityFX != prevEntityFX ) {
			reset = true;
		}

		if( reset ) {
			if( batchSpanBegin ) {
				const shader_s *prevShader           = materialCache->getMaterialById( prevShaderNum );
				const entity_t *prevEntity           = scene->m_entities[prevEntNum];
				const sortedDrawSurf_t *batchSpanEnd = sds;

				assert( batchSpanEnd > batchSpanBegin );
				const auto [submitFn, offset] = registerBuildingBatchedSurf( stateForCamera, scene, prevSurfType,
																			 { batchSpanBegin, batchSpanEnd } );

				drawActionsList->append( [=]( FrontendToBackendShared *fsh ) {
					RB_FlushDynamicMeshes();
					submitFn( fsh, prevEntity, prevShader, prevFog, prevPortalSurface, offset );
					RB_FlushDynamicMeshes();
				});
			}

			if( isDrawSurfBatched ) {
				batchSpanBegin = sds;
			} else {
				batchSpanBegin = nullptr;
			}

			// hack the depth range to prevent view model from poking into walls
			if( entity->flags & RF_WEAPONMODEL ) {
				if( !depthHack ) {
					depthHack = true;
					drawActionsList->append([=]( FrontendToBackendShared *fsh ) {
						RB_FlushDynamicMeshes();
						float depthmin = 0.0f, depthmax = 0.0f;
						RB_GetDepthRange( &depthmin, &depthmax );
						RB_SaveDepthRange();
						RB_DepthRange( depthmin, depthmin + 0.3f * ( depthmax - depthmin ) );
					});
				}
			} else {
				if( depthHack ) {
					depthHack = false;
					drawActionsList->append([=]( FrontendToBackendShared * ) {
						RB_FlushDynamicMeshes();
						RB_RestoreDepthRange();
					});
				}
			}

			if( entNum != prevEntNum ) {
				// backface culling for left-handed weapons
				bool oldCullHack = cullHack;
				cullHack = ( ( entity->flags & RF_CULLHACK ) ? true : false );
				if( cullHack != oldCullHack ) {
					drawActionsList->append( [=]( FrontendToBackendShared * ) {
						RB_FlushDynamicMeshes();
						RB_FlipFrontFace();
					});
				}
			}

			// sky and things that don't use depth test use infinite projection matrix
			// to not pollute the farclip
			const bool infiniteProj = entity->renderfx & RF_NODEPTHTEST ? true : false;
			if( infiniteProj != prevInfiniteProj ) {
				if( infiniteProj ) {
					drawActionsList->append( [=]( FrontendToBackendShared * ) {
						RB_FlushDynamicMeshes();
						mat4_t projectionMatrix;
						Matrix4_Copy( stateForCamera->projectionMatrix, projectionMatrix );
						Matrix4_PerspectiveProjectionToInfinity( Z_NEAR, projectionMatrix, glConfig.depthEpsilon );
						RB_LoadProjectionMatrix( projectionMatrix );
					});
				} else {
					drawActionsList->append( [=]( FrontendToBackendShared * ) {
						RB_FlushDynamicMeshes();
						RB_LoadProjectionMatrix( stateForCamera->projectionMatrix );
					});
				}
			}

			if( isDrawSurfBatched ) {
				// don't transform batched surfaces
				if( !prevIsDrawSurfBatched ) {
					drawActionsList->append( [=]( FrontendToBackendShared * ) {
						RB_LoadObjectMatrix( mat4x4_identity );
					});
				}
			} else {
				if( ( entNum != prevEntNum ) || prevIsDrawSurfBatched ) {
					drawActionsList->append( [=]( FrontendToBackendShared * ) {
						if( entity->number == kWorldEntNumber ) [[likely]] {
							R_TransformForWorld();
						} else if( entity->rtype == RT_MODEL ) {
							R_TransformForEntity( entity );
						} else if( shader->flags & SHADER_AUTOSPRITE ) {
							R_TranslateForEntity( entity );
						} else {
							R_TransformForWorld();
						}
					});
				}
			}

			if( !isDrawSurfBatched ) {
				drawActionsList->append( [=]( FrontendToBackendShared *fsh ) {
					assert( r_drawSurfCb[surfType] );

					RB_BindShader( entity, shader, fog );
					RB_SetPortalSurface( portalSurface );

					r_drawSurfCb[surfType]( fsh, entity, shader, fog, portalSurface, sds->drawSurf );
				});
			}

			prevInfiniteProj = infiniteProj;
		}

		prevShaderNum             = shaderNum;
		prevEntNum                = entNum;
		prevFogNum                = fogNum;
		prevIsDrawSurfBatched     = isDrawSurfBatched;
		prevSurfType              = surfType;
		prevMergeabilitySeparator = sds->mergeabilitySeparator;
		prevPortalNum             = portalNum;
		prevEntityFX              = entityFX;
		prevFog                   = fog;
		prevPortalSurface         = portalSurface;
	}

	if( batchSpanBegin ) {
		const shader_t *prevShader           = materialCache->getMaterialById( prevShaderNum );
		const entity_t *prevEntity           = scene->m_entities[prevEntNum];
		const sortedDrawSurf_t *batchSpanEnd = drawSurfs + numDrawSurfs;

		assert( batchSpanEnd > batchSpanBegin );
		const auto [submitFn, offset] = registerBuildingBatchedSurf( stateForCamera, scene, prevSurfType,
																	 { batchSpanBegin, batchSpanEnd } );

		drawActionsList->append( [=]( FrontendToBackendShared *fsh ) {
			RB_FlushDynamicMeshes();
			submitFn( fsh, prevEntity, prevShader, prevFog, prevPortalSurface, offset );
			RB_FlushDynamicMeshes();
		});
	}

	if( depthHack ) {
		drawActionsList->append( [=]( FrontendToBackendShared * ) {
			RB_RestoreDepthRange();
		});
	}
	if( cullHack ) {
		drawActionsList->append( [=]( FrontendToBackendShared * ) {
			RB_FlipFrontFace();
		});
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

void Frontend::markBuffersOfBatchedDynamicsForUpload( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras ) {
	const auto markAndAdvanceBatchedOffsets = [this]( const std::pair<unsigned, unsigned> &storageRequirements,
													  StateForCamera *stateForCamera,
													  const PrepareBatchedSurfWorkload &workload ) -> bool {
		unsigned *const offsetOfVertices = &m_variousDynamicsOffsetsOfVerticesAndIndices.first;
		unsigned *const offsetOfIndices  = &m_variousDynamicsOffsetsOfVerticesAndIndices.second;
		if( *offsetOfVertices + storageRequirements.first <= MAX_UPLOAD_VBO_VERTICES &&
			*offsetOfIndices + storageRequirements.second <= MAX_UPLOAD_VBO_INDICES ) {
			stateForCamera->batchedSurfVertSpans->data()[workload.vertSpanOffset] = VertElemSpan {
				.firstVert = *offsetOfVertices,
				.numVerts  = storageRequirements.first,
				.firstElem = *offsetOfIndices,
				.numElems  = storageRequirements.second,
			};
			*offsetOfVertices += storageRequirements.first;
			*offsetOfIndices  += storageRequirements.second;
			return true;
		}
		return false;
	};

	// We have to join the execution flow for this
	for( auto [scene, stateForCamera] : scenesAndCameras ) {
		for( PrepareBatchedSurfWorkload &workload: *stateForCamera->preparePolysWorkload ) {
			bool succeeded = false;
			if( !m_selectedPolysWorkload.full() ) [[likely]] {
				if( const auto maybeRequirements = getQuadPolySpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( markAndAdvanceBatchedOffsets( *maybeRequirements, stateForCamera, workload ) ) [[likely]] {
						m_selectedPolysWorkload.push_back( std::addressof( workload ) );
						succeeded = true;
					}
				}
			}
			if( !succeeded ) [[unlikely]] {
				// Ensure that we won't try to draw it
				stateForCamera->batchedSurfVertSpans->data()[workload.vertSpanOffset] = VertElemSpan { .numVerts = 0, .numElems = 0 };
			}
		}
		for( PrepareBatchedSurfWorkload &workload: *stateForCamera->prepareCoronasWorkload ) {
			bool succeeded = false;
			if( !m_selectedCoronasWorkload.full() ) [[likely]] {
				if( const auto maybeRequirements = getCoronaSpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( markAndAdvanceBatchedOffsets( *maybeRequirements, stateForCamera, workload ) ) [[likely]] {
						this->m_selectedCoronasWorkload.push_back( std::addressof( workload ) );
						succeeded = true;
					}
				}
			}
			if( !succeeded ) [[unlikely]] {
				// Ensure that we won't try to draw it
				stateForCamera->batchedSurfVertSpans->data()[workload.vertSpanOffset] = VertElemSpan { .numVerts = 0, .numElems = 0 };
			}
		}
		for( PrepareBatchedSurfWorkload &workload: *stateForCamera->prepareParticlesWorkload ) {
			bool succeeded = false;
			if( !m_selectedParticlesWorkload.full() ) [[likely]] {
				if( const auto maybeRequirements = getParticleSpanStorageRequirements( workload.batchSpan ) ) [[likely]] {
					if( markAndAdvanceBatchedOffsets( *maybeRequirements, stateForCamera, workload ) ) [[likely]] {
						m_selectedParticlesWorkload.push_back( std::addressof( workload ) );
						succeeded = true;
					}
				}
			}
			if( !succeeded ) [[unlikely]] {
				stateForCamera->batchedSurfVertSpans->data()[workload.vertSpanOffset] = VertElemSpan { .numVerts = 0, .numElems = 0 };
			}
		}
	}

	// Process legacy sprites regardless of upload overflow
	for( auto [scene, stateForCamera] : scenesAndCameras ) {
		for( PrepareSpriteSurfWorkload &workload: *stateForCamera->prepareSpritesWorkload ) {
			if( !m_selectedSpriteWorkload.full() ) [[likely]] {
				m_selectedSpriteWorkload.push_back( std::addressof( workload ) );
			} else {
				// Ensure that we won't try to draw these sprites
				for( unsigned spriteInSpan = 0; spriteInSpan < workload.batchSpan.size(); ++spriteInSpan ) {
					auto *preparedMesh = stateForCamera->preparedSpriteMeshes->data() + workload.firstMeshOffset + spriteInSpan;
					preparedMesh->mesh.numVerts = preparedMesh->mesh.numElems = 0;
				}
			}
		}
	}
}

void Frontend::submitDrawActionsList( StateForCamera *stateForCamera, Scene *scene ) {
	FrontendToBackendShared fsh;
	fsh.dynamicLights               = scene->m_dynamicLights.data();
	fsh.particleAggregates          = scene->m_particles.data();
	fsh.batchedVertElemSpans        = stateForCamera->batchedSurfVertSpans->data();
	fsh.preparedSpriteMeshes        = stateForCamera->preparedSpriteMeshes->data();
	fsh.preparedSpriteMeshStride    = sizeof( PreparedSpriteMesh );
	fsh.allVisibleLightIndices      = { stateForCamera->allVisibleLightIndices, stateForCamera->numAllVisibleLights };
	fsh.visibleProgramLightIndices  = { stateForCamera->visibleProgramLightIndices, stateForCamera->numVisibleProgramLights };
	fsh.renderFlags                 = stateForCamera->renderFlags;
	fsh.cameraId                    = stateForCamera->cameraId;
	fsh.sceneIndex                  = stateForCamera->sceneIndex;
	std::memcpy( fsh.viewAxis, stateForCamera->viewAxis, sizeof( mat3_t ) );
	VectorCopy( stateForCamera->viewOrigin, fsh.viewOrigin );

	for( wsw::Function<void( FrontendToBackendShared * )> &drawAction: *stateForCamera->drawActionsList ) {
		drawAction( &fsh );
	}
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

// TODO: Write to mapped buffers directly
struct MeshBuilder {
	PodBufferHolder<vec4_t> meshPositions;
	PodBufferHolder<vec4_t> meshNormals;
	PodBufferHolder<vec2_t> meshTexCoords;
	PodBufferHolder<byte_vec4_t> meshColors;
	PodBufferHolder<uint16_t> meshIndices;

	unsigned numVerticesSoFar { 0 };
	unsigned numIndicesSoFar { 0 };

	void reserveForNumQuads( unsigned numQuads ) {
		reserveForNumVertsAndIndices( 4 * numQuads, 6 * numQuads );
	}

	void reserveForNumVertsAndIndices( unsigned numVertices, unsigned numIndices ) {
		meshPositions.reserve( numVertices );
		meshNormals.reserve( numVertices );
		meshTexCoords.reserve( numVertices );
		meshColors.reserve( numVertices );
		meshIndices.reserve( numIndices );

		this->numVerticesSoFar = 0;
		this->numIndicesSoFar  = 0;
	}

	void appendQuad( const vec4_t positions[4], const vec2_t texCoords[4], const byte_vec4_t colors[4], const uint16_t indices[6] ) {
		std::memcpy( meshPositions.get() + numVerticesSoFar, positions, 4 * sizeof( vec4_t ) );
		std::memcpy( meshTexCoords.get() + numVerticesSoFar, texCoords, 4 * sizeof( vec2_t ) );
		std::memcpy( meshColors.get() + numVerticesSoFar, colors, 4 * sizeof( byte_vec4_t ) );

		for( unsigned i = 0; i < 6; ++i ) {
			meshIndices.get()[numIndicesSoFar + i] = numVerticesSoFar + indices[i];
		}

		numVerticesSoFar += 4;
		numIndicesSoFar += 6;
	}
};

static thread_local MeshBuilder tl_meshBuilder;

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

	MeshBuilder *const meshBuilder = &tl_meshBuilder;
	meshBuilder->reserveForNumVertsAndIndices( workload->drawSurface->requestedNumVertices, workload->drawSurface->requestedNumIndices );

	auto [numVertices, numIndices] = dynamicMesh->fillMeshBuffers( workload->stateForCamera->viewOrigin,
																   workload->stateForCamera->viewAxis,
																   workload->scene->m_dynamicLights.data(),
																   lightIndicesSpan,
																   workload->drawSurface->scratchpad,
																   meshBuilder->meshPositions.get(),
																   meshBuilder->meshNormals.get(),
																   meshBuilder->meshTexCoords.get(),
																   meshBuilder->meshColors.get(),
																   meshBuilder->meshIndices.get() );
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
		mesh.xyzArray       = meshBuilder->meshPositions.get();
		mesh.stArray        = meshBuilder->meshTexCoords.get();
		mesh.colorsArray[0] = meshBuilder->meshColors.get();
		mesh.elems          = meshBuilder->meshIndices.get();

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

static void uploadBatchedMesh( MeshBuilder *builder, VertElemSpan *inOutSpan ) {
	if( builder->numVerticesSoFar && builder->numIndicesSoFar ) {
		mesh_t mesh;
		std::memset( &mesh, 0, sizeof( mesh_t ) );

		mesh.numVerts       = builder->numVerticesSoFar;
		mesh.numElems       = builder->numIndicesSoFar;
		mesh.xyzArray       = builder->meshPositions.get();
		mesh.stArray        = builder->meshTexCoords.get();
		mesh.colorsArray[0] = builder->meshColors.get();
		mesh.elems          = builder->meshIndices.get();

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

static void buildMeshForSpriteParticles( MeshBuilder *meshBuilder,
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
		// vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
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

static void buildMeshForSparkParticles( MeshBuilder *meshBuilder,
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
		// vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
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

	std::span<const uint16_t> lightIndicesSpan;
	if( appearanceRules->applyVertexDynLight && stateForCamera->canAddLightsToParticles ) {
		const std::span<const uint16_t> allVisibleLightIndices = stateForCamera->allVisibleLightIndices;
		assert( !allVisibleLightIndices.empty() );
		const auto [offset, numLights] = stateForCamera->lightSpansForParticleAggregates[firstDrawSurf->aggregateIndex];
		lightIndicesSpan = { stateForCamera->lightIndicesForParticleAggregates->get() + offset, numLights };
	}

	MeshBuilder *const meshBuilder = &tl_meshBuilder;
	meshBuilder->reserveForNumQuads( surfSpan.size() );

	if( std::holds_alternative<Particle::SpriteRules>( appearanceRules->geometryRules ) ) {
		buildMeshForSpriteParticles( meshBuilder, aggregate, surfSpan, stateForCamera->viewAxis,
									 ( stateForCamera->renderFlags & RF_MIRRORVIEW ) != 0,
									 scene->m_dynamicLights.data(), lightIndicesSpan );
	} else if( std::holds_alternative<Particle::SparkRules>( appearanceRules->geometryRules ) ) {
		// TODO: We don't handle MIRRORVIEW
		buildMeshForSparkParticles( meshBuilder, aggregate, surfSpan, stateForCamera->viewOrigin,
									stateForCamera->viewAxis, scene->m_dynamicLights.data(), lightIndicesSpan );
	} else {
		wsw::failWithRuntimeError( "Unreachable" );
	}

	uploadBatchedMesh( meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void Frontend::prepareBatchedCoronas( PrepareBatchedSurfWorkload *workload ) {
	vec3_t v_left, v_up;
	VectorCopy( &workload->stateForCamera->viewAxis[AXIS_RIGHT], v_left );
	VectorCopy( &workload->stateForCamera->viewAxis[AXIS_UP], v_up );

	if( workload->stateForCamera->renderFlags & RF_MIRRORVIEW ) {
		VectorInverse( v_left );
	}

	MeshBuilder *const meshBuilder = &tl_meshBuilder;
	meshBuilder->reserveForNumQuads( workload->batchSpan.size() );

	for( const sortedDrawSurf_t &sds: workload->batchSpan ) {
		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		// TODO: Do we need normals?
		// vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
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

		meshBuilder->appendQuad( xyz, texcoords, colors, elems );
	}

	uploadBatchedMesh( meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void Frontend::prepareBatchedQuadPolys( PrepareBatchedSurfWorkload *workload ) {
	MeshBuilder *const meshBuilder = &tl_meshBuilder;
	meshBuilder->reserveForNumQuads( workload->batchSpan.size() );

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

				meshBuilder->appendQuad( positions, texCoords, colors, indices );
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

			meshBuilder->appendQuad( positions, texCoords, colors, indices );
		}
	}

	uploadBatchedMesh( meshBuilder, workload->stateForCamera->batchedSurfVertSpans->data() + workload->vertSpanOffset );
}

void Frontend::prepareLegacySprite( PrepareSpriteSurfWorkload *workload ) {
	StateForCamera *const stateForCamera     = workload->stateForCamera;
	PreparedSpriteMesh *const preparedMeshes = stateForCamera->preparedSpriteMeshes->data() + workload->firstMeshOffset;
	const float *const viewAxis              = stateForCamera->viewAxis;
	const bool shouldMirrorView              = ( stateForCamera->renderFlags & RF_MIRRORVIEW ) != 0;

	for( size_t surfInSpan = 0; surfInSpan < workload->batchSpan.size(); ++surfInSpan ) {
		const sortedDrawSurf_t &sds = workload->batchSpan.data()[surfInSpan];
		const auto *const e         = (const entity_t *)sds.drawSurf;

		vec3_t v_left, v_up;
		if( const float rotation = e->rotation; rotation != 0.0f ) {
			RotatePointAroundVector( v_left, &viewAxis[AXIS_FORWARD], &viewAxis[AXIS_RIGHT], rotation );
			CrossProduct( &viewAxis[AXIS_FORWARD], v_left, v_up );
		} else {
			VectorCopy( &viewAxis[AXIS_RIGHT], v_left );
			VectorCopy( &viewAxis[AXIS_UP], v_up );
		}

		if( shouldMirrorView ) {
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
			VectorNegate( &viewAxis[AXIS_FORWARD], normals[i] );
			Vector4Copy( e->color, colors[i] );
		}

		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		PreparedSpriteMesh *const preparedMesh = preparedMeshes + surfInSpan;
		std::memset( &preparedMesh->mesh, 0, sizeof( mesh_t ) );

		// TODO: Reduce copying
		std::memcpy( &preparedMesh->positions, xyz, 4 * sizeof( vec4_t ) );
		std::memcpy( &preparedMesh->normals, normals, 4 * sizeof( vec4_t ) );
		std::memcpy( &preparedMesh->texCoords, texcoords, 4 * sizeof( vec2_t ) );
		std::memcpy( &preparedMesh->colors, colors, 4 * sizeof( byte_vec4_t ) );
		std::memcpy( &preparedMesh->indices, elems, 6 * sizeof( elem_t ) );

		preparedMesh->mesh.xyzArray       = preparedMesh->positions;
		preparedMesh->mesh.normalsArray   = preparedMesh->normals;
		preparedMesh->mesh.stArray        = preparedMesh->texCoords;
		preparedMesh->mesh.colorsArray[0] = preparedMesh->colors;
		preparedMesh->mesh.elems          = preparedMesh->indices;

		preparedMesh->mesh.numVerts = 4;
		preparedMesh->mesh.numElems = 6;
	}
}

}