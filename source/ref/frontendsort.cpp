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
#include "../common/wswsortbyfield.h"

static unsigned R_PackOpaqueOrder( const mfog_t *fog, const shader_t *shader, int numLightmaps, bool dlight ) {
	unsigned order = 0;

	// shader order
	if( shader != nullptr ) {
		order = R_PackShaderOrder( shader );
	}
	// group by dlight
	if( dlight ) {
		order |= 0x40;
	}
	if( fog != nullptr ) {
		order |= 0x80;
	}
	// group by lightmaps
	order |= ( (MAX_LIGHTMAPS - numLightmaps) << 10 );

	return order;
}

static int R_PackDistKey( int renderFx, const shader_t *shader, float dist, unsigned order ) {
	int shaderSort = shader->sort;

	if( renderFx & RF_WEAPONMODEL ) {
		const bool depthWrite = ( shader->flags & SHADER_DEPTHWRITE ) != 0;

		if( renderFx & RF_NOCOLORWRITE ) {
			// depth-pass for alpha-blended weapon:
			// write to depth but do not write to color
			if( !depthWrite ) {
				return 0;
			}
			// reorder the mesh to be drawn after everything else
			// but before the blend-pass for the weapon
			shaderSort = SHADER_SORT_WEAPON;
		} else if( renderFx & RF_ALPHAHACK ) {
			// blend-pass for the weapon:
			// meshes that do not write to depth, are rendered as additives,
			// meshes that were previously added as SHADER_SORT_WEAPON (see above)
			// are now added to the very end of the list
			shaderSort = depthWrite ? SHADER_SORT_WEAPON2 : SHADER_SORT_ADDITIVE;
		}
	} else if( renderFx & RF_ALPHAHACK ) {
		// force shader sort to additive
		shaderSort = SHADER_SORT_ADDITIVE;
	}

	return ( shaderSort << 26 ) | ( wsw::max( 0x400 - (int)dist, 0 ) << 15 ) | ( order & 0x7FFF );
}

#define WORLDSURF_DIST 1024.0f                  // hack the draw order for world surfaces

namespace wsw::ref {

static drawSurfaceType_t spriteDrawSurf = ST_SPRITE;

void Frontend::addAliasModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *aliasModelEntities,
												std::span<const VisTestedModel> models, std::span<const uint16_t> indices ) {
	for( const auto modelIndex: indices ) {
		const VisTestedModel &__restrict visTestedModel = models[modelIndex];
		const entity_t *const __restrict entity          = aliasModelEntities + visTestedModel.indexInEntitiesGroup;

		float distance;
		// make sure weapon model is always closest to the viewer
		if( entity->renderfx & RF_WEAPONMODEL ) {
			distance = 0;
		} else {
			distance = Distance( entity->origin, stateForCamera->viewOrigin ) + 1;
		}

		const mfog_t *const fog = getFogForBounds( stateForCamera, visTestedModel.absMins, visTestedModel.absMaxs );

		const auto *const aliasmodel = (const maliasmodel_t *)visTestedModel.selectedLod->extradata;
		for( int meshNum = 0; meshNum < aliasmodel->nummeshes; meshNum++ ) {
			const maliasmesh_t *mesh = &aliasmodel->meshes[meshNum];
			const shader_t *shader = nullptr;

			if( entity->customSkin ) {
				shader = R_FindShaderForSkinFile( entity->customSkin, mesh->name );
			} else if( entity->customShader ) {
				shader = entity->customShader;
			} else if( mesh->numskins ) {
				for( int skinNum = 0; skinNum < mesh->numskins; skinNum++ ) {
					shader = mesh->skins[skinNum].shader;
					if( shader ) {
						void *drawSurf = aliasmodel->drawSurfs + meshNum;
						const unsigned drawOrder = R_PackOpaqueOrder( fog, shader, 0, false );
						addEntryToSortList( stateForCamera, entity, fog, shader, distance, drawOrder, nullptr, drawSurf, ST_ALIAS );
					}
				}
				continue;
			}

			if( shader ) {
				void *drawSurf = aliasmodel->drawSurfs + meshNum;
				const unsigned drawOrder = R_PackOpaqueOrder( fog, shader, 0, false );
				addEntryToSortList( stateForCamera, entity, fog, shader, distance, drawOrder, nullptr, drawSurf, ST_ALIAS );
			}
		}
	}
}

void Frontend::addSkeletalModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *skeletalModelEntities,
												   std::span<const VisTestedModel> models, std::span<const uint16_t> indices ) {
	for( const auto modelIndex: indices ) {
		const VisTestedModel &__restrict visTestedModel = models[modelIndex];
		const entity_t *const __restrict entity         = skeletalModelEntities + visTestedModel.indexInEntitiesGroup;

		float distance;
		// make sure weapon model is always closest to the viewer
		if( entity->renderfx & RF_WEAPONMODEL ) {
			distance = 0;
		} else {
			distance = Distance( entity->origin, stateForCamera->viewOrigin ) + 1;
		}

		const mfog_t *const fog = getFogForBounds( stateForCamera, visTestedModel.absMins, visTestedModel.absMaxs );

		const model_t *const mod = visTestedModel.selectedLod;
		R_AddSkeletalModelCache( entity, mod );

		const auto *const skmodel = ( const mskmodel_t * )mod->extradata;
		for( unsigned meshNum = 0; meshNum < skmodel->nummeshes; meshNum++ ) {
			const mskmesh_t *const mesh = &skmodel->meshes[meshNum];
			shader_t *shader = nullptr;

			if( entity->customSkin ) {
				shader = R_FindShaderForSkinFile( entity->customSkin, mesh->name );
			} else if( entity->customShader ) {
				shader = entity->customShader;
			} else {
				shader = mesh->skin.shader;
			}

			if( shader ) {
				void *drawSurf = skmodel->drawSurfs + meshNum;
				const unsigned drawOrder = R_PackOpaqueOrder( fog, shader, 0, false );
				addEntryToSortList( stateForCamera, entity, fog, shader, distance, drawOrder, nullptr, drawSurf, ST_SKELETAL );
			}
		}
	}
}

void Frontend::addNullModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *nullModelEntities,
											   std::span<const uint16_t> indices ) {
	for( const auto index: indices ) {
		addEntryToSortList( stateForCamera, nullModelEntities + index, nullptr, rsh.whiteShader, 0, 0, nullptr, nullptr, ST_NULLMODEL );
	}
}

void Frontend::addBrushModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *brushModelEntities,
												std::span<const VisTestedModel> models,
												std::span<const uint16_t> indices, std::span<const Scene::DynamicLight> lights ) {
	MergedBspSurface *const mergedSurfaces = rsh.worldBrushModel->mergedSurfaces;
	drawSurfaceBSP_t *const drawSurfaces   = stateForCamera->bspDrawSurfacesBuffer->get();
	msurface_s *const surfaces             = rsh.worldBrushModel->surfaces;

	for( const auto modelIndex: indices ) {
		const VisTestedModel &visTestedModel = models[modelIndex];
		const auto *const model              = visTestedModel.selectedLod;
		const auto *const entity             = brushModelEntities + visTestedModel.indexInEntitiesGroup;
		const auto *const brushModel         = ( mbrushmodel_t * )model->extradata;
		assert( brushModel->numModelMergedSurfaces );

		vec3_t origin;
		VectorAvg( visTestedModel.absMins, visTestedModel.absMaxs, origin );

		for( unsigned i = 0; i < brushModel->numModelMergedSurfaces; i++ ) {
			const unsigned surfNum            = brushModel->firstModelMergedSurface + i;
			drawSurfaceBSP_t *drawSurface     = drawSurfaces + surfNum;
			MergedBspSurface *mergedSurface   = mergedSurfaces + surfNum;
			drawSurface->mergedBspSurf        = mergedSurface;
			msurface_t *const firstVisSurface = surfaces + mergedSurface->firstWorldSurface;
			msurface_t *const lastVisSurface  = firstVisSurface + mergedSurface->numWorldSurfaces - 1;
			addMergedBspSurfToSortList( stateForCamera, entity, drawSurface, firstVisSurface, lastVisSurface, origin, lights );
		}
	}
}

void Frontend::addSpriteEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *spriteEntities,
											std::span<const uint16_t> indices ) {
	for( const unsigned index: indices ) {
		const entity_t *const __restrict entity = spriteEntities + index;

		vec3_t eyeToSprite;
		VectorSubtract( entity->origin, stateForCamera->refdef.vieworg, eyeToSprite );
		if( const float dist = DotProduct( eyeToSprite, &stateForCamera->viewAxis[0] ); dist > 0 ) [[likely]] {
			const mfog_t *const fog = getFogForSphere( stateForCamera, entity->origin, entity->radius );
			addEntryToSortList( stateForCamera, entity, fog, entity->customShader, dist, 0, nullptr, &spriteDrawSurf, ST_SPRITE );
		}
	}
}

void Frontend::addMergedBspSurfToSortList( StateForCamera *stateForCamera, const entity_t *entity, drawSurfaceBSP_t *drawSurf,
										   msurface_t *firstVisSurf, msurface_t *lastVisSurf, const float *maybeOrigin,
										   std::span<const Scene::DynamicLight> lightsSpan ) {
	const MergedBspSurface *const mergedSurf = drawSurf->mergedBspSurf;
	const shader_t *const surfMaterial       = mergedSurf->shader;

	portalSurface_t *portalSurface = nullptr;
	if( surfMaterial->flags & SHADER_PORTAL ) {
		portalSurface = tryAddingPortalSurface( stateForCamera, entity, surfMaterial, drawSurf );
	}

	const mfog_t *fog        = mergedSurf->fog;
	const unsigned drawOrder = R_PackOpaqueOrder( fog, surfMaterial, mergedSurf->numLightmaps, false );

	drawSurf->dlightBits = 0;
	drawSurf->listSurf = addEntryToSortList( stateForCamera, entity, fog, surfMaterial, WORLDSURF_DIST,
											 drawOrder, portalSurface, drawSurf, ST_BSP );
	if( !drawSurf->listSurf ) {
		return;
	}

	if( portalSurface && !( surfMaterial->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) ) ) {
		//addEntryToSortList( m_state.portalmasklist, e, nullptr, nullptr, 0, 0, nullptr, drawSurf );
	}

	float resultDist = 0;
	if( surfMaterial->flags & ( SHADER_PORTAL ) ) [[unlikely]] {
		for( msurface_s *surf = firstVisSurf; surf <= lastVisSurf; ++surf ) {
			if( const auto maybeDistance = tryUpdatingPortalSurfaceAndDistance( stateForCamera,
																				drawSurf, surf, maybeOrigin ) ) {
				resultDist = wsw::max( resultDist, *maybeDistance );
			}
		}
	}

	unsigned dlightBits = 0;
	// TODO: This should belong to state
	if( stateForCamera->numVisibleProgramLights ) {
		const unsigned *const surfaceDlightBits = stateForCamera->leafLightBitsOfSurfacesBuffer->get();
		const msurface_t *const worldSurfaces   = rsh.worldBrushModel->surfaces;
		const unsigned numLights                = lightsSpan.size();

		msurface_t *surf = firstVisSurf;
		do {
			// Coarse bits are combined light bits of leaves.
			// Check against individual surfaces if a coarse bit is set.
			// TODO: Iterate over all bits in the outer loop?
			// TODO: Think of using SIMD-friendly indices + left-packing instead of integer bits
			if( const unsigned coarseBits = surfaceDlightBits[surf - worldSurfaces] ) {
				assert( numLights );
				unsigned lightNum = 0;
				unsigned testedBits = 0;
				do {
					const unsigned lightBit = 1u << lightNum;
					testedBits |= lightBit;
					if( coarseBits & lightBit ) [[likely]] {
						const auto *__restrict lightDop = &stateForCamera->lightBoundingDops[lightNum];
						if( doOverlapTestFor14Dops( lightDop->mins, lightDop->maxs, surf->mins, surf->maxs ) ) {
							dlightBits |= lightBit;
						}
					}
				} while( ++lightNum < numLights && testedBits != coarseBits );
			}
			// TODO: Exclude culled/occluded surfaces in range [firstVisSurf, lastVisSurf] from light bits computation
			// TODO: Compute an accumulative table of light bits of visible surfaces for merged surfaces
		} while( ++surf < lastVisSurf + 1 );
	}

	drawSurf->firstSpanVert = firstVisSurf->firstDrawSurfVert;
	drawSurf->firstSpanElem = firstVisSurf->firstDrawSurfElem;
	drawSurf->numSpanVerts = lastVisSurf->mesh.numVerts + lastVisSurf->firstDrawSurfVert - firstVisSurf->firstDrawSurfVert;
	drawSurf->numSpanElems = lastVisSurf->mesh.numElems + lastVisSurf->firstDrawSurfElem - firstVisSurf->firstDrawSurfElem;

	// update the distance sorting key if it's a portal surface or a normal dlit surface
	if( resultDist != 0 || dlightBits != 0 ) {
		drawSurf->dlightBits = dlightBits;
		const unsigned order = R_PackOpaqueOrder( mergedSurf->fog, surfMaterial, mergedSurf->numLightmaps, false );
		if( resultDist == 0 ) {
			resultDist = WORLDSURF_DIST;
		}
		sortedDrawSurf_t *const sds = (sortedDrawSurf_t *)drawSurf->listSurf;
		sds->distKey = R_PackDistKey( 0, surfMaterial, resultDist, order );
	}
}

void Frontend::addParticlesToSortList( StateForCamera *stateForCamera, const entity_t *particleEntity,
									   const Scene::ParticlesAggregate *particles,
									   std::span<const uint16_t> aggregateIndices ) {
	const float *const __restrict forwardAxis = stateForCamera->viewAxis;
	const float *const __restrict viewOrigin  = stateForCamera->viewOrigin;

	unsigned numParticleDrawSurfaces = 0;
	for( const unsigned aggregateIndex: aggregateIndices ) {
		const Scene::ParticlesAggregate *const __restrict pa = particles + aggregateIndex;
		const unsigned mergeabilityKey = aggregateIndex;
		for( unsigned particleIndex = 0; particleIndex < pa->numParticles; ++particleIndex ) {
			const Particle *__restrict particle = pa->particles + particleIndex;

			vec3_t toParticle;
			VectorSubtract( particle->origin, viewOrigin, toParticle );
			const float distanceLike = DotProduct( toParticle, forwardAxis );

			// TODO: Account for fogs
			const mfog_t *fog = nullptr;

			auto *const drawSurf     = &stateForCamera->particleDrawSurfaces[numParticleDrawSurfaces++];
			drawSurf->aggregateIndex = aggregateIndex;
			drawSurf->particleIndex  = particleIndex;

			shader_s *material = pa->appearanceRules.materials[particle->instanceMaterialIndex];

			// TODO: Inline/add some kind of bulk insertion
			addEntryToSortList( stateForCamera, particleEntity, fog, material, distanceLike, 0, nullptr, drawSurf,
								ST_PARTICLE, mergeabilityKey );
		}
	}
}

void Frontend::addDynamicMeshesToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity,
										   const DynamicMesh **meshes, std::span<const uint16_t> indicesOfMeshes ) {
	const float *const __restrict viewOrigin = stateForCamera->viewOrigin;

	for( const unsigned meshIndex: indicesOfMeshes ) {
		const DynamicMesh *const __restrict mesh = meshes[meshIndex];

		vec3_t meshCenter;
		VectorAvg( mesh->cullMins, mesh->cullMaxs, meshCenter );
		const float distance = DistanceFast( meshCenter, viewOrigin );

		const mfog_t *fog = nullptr;
		const void *drawSurf = mesh;
		const shader_s *material = mesh->material ? mesh->material : rsh.whiteShader;
		addEntryToSortList( stateForCamera, meshEntity, fog, material, distance, 0, nullptr, drawSurf, ST_DYNAMIC_MESH );
	}
}

void Frontend::addCompoundDynamicMeshesToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity,
												   const Scene::CompoundDynamicMesh *meshes,
												   std::span<const uint16_t> indicesOfMeshes ) {
	const float *const __restrict viewOrigin = stateForCamera->viewOrigin;

	float distances[Scene::kMaxCompoundDynamicMeshes];
	std::pair<unsigned, float> drawnBehindParts[Scene::CompoundDynamicMesh::kMaxParts];
	std::pair<unsigned, float> drawnInFrontParts[Scene::CompoundDynamicMesh::kMaxParts];

	for( const unsigned compoundMeshIndex: indicesOfMeshes ) {
		const Scene::CompoundDynamicMesh *const __restrict compoundMesh = meshes + compoundMeshIndex;
		const float *const __restrict meshOrderDesignators = compoundMesh->meshOrderDesignators;

		[[maybe_unused]] float minDistance = std::numeric_limits<float>::max();
		[[maybe_unused]] float maxDistance = std::numeric_limits<float>::lowest();
		unsigned numDrawnBehindParts = 0, numDrawnInFrontParts = 0;
		for( unsigned partIndex = 0; partIndex < compoundMesh->numParts; ++partIndex ) {
			const DynamicMesh *const __restrict mesh = compoundMesh->parts[partIndex];
			assert( mesh );

			// This is very incorrect, but still produces satisfiable results.
			// Order-independent transparency is the proper solution.
			// Splitting the hull in two parts, front and back one is also more correct than the present code,
			// but this would have huge performance impact with the current dynamic submission of vertices.

			vec3_t meshCenter;
			VectorAvg( mesh->cullMins, mesh->cullMaxs, meshCenter );
			const float distance = DistanceFast( meshCenter, viewOrigin );
			distances[partIndex] = distance;
			if( meshOrderDesignators ) {
				minDistance = wsw::min( minDistance, distance );
				maxDistance = wsw::max( maxDistance, distance );
				const float drawOrderDesignator = meshOrderDesignators[partIndex];
				if( drawOrderDesignator > 0.0f ) [[likely]] {
					drawnInFrontParts[numDrawnInFrontParts++] = std::make_pair( partIndex, drawOrderDesignator );
				} else if( drawOrderDesignator < 0.0f ) {
					// Add it to the part list;
					drawnBehindParts[numDrawnBehindParts++] = std::make_pair( partIndex, drawOrderDesignator );
				}
			}
		}

		if( numDrawnBehindParts | numDrawnInFrontParts ) {
			wsw::sortByField( drawnBehindParts, drawnBehindParts + numDrawnBehindParts, &std::pair<unsigned, float>::second );
			wsw::sortByField( drawnInFrontParts, drawnInFrontParts + numDrawnInFrontParts, &std::pair<unsigned, float>::second );
		}

		const auto addMeshToSortList = [&]( const DynamicMesh *mesh, float distance ) {
			// TODO: Account for fogs
			const mfog_t *fog        = nullptr;
			const void *drawSurf     = mesh;
			const shader_s *material = mesh->material ? mesh->material : rsh.whiteShader;
			addEntryToSortList( stateForCamera, meshEntity, fog, material, distance, 0, nullptr, drawSurf, ST_DYNAMIC_MESH );
		};

		for( unsigned partNum = 0; partNum < numDrawnBehindParts; ++partNum ) {
			const float distance = maxDistance + (float)( numDrawnBehindParts + 1 - partNum );
			addMeshToSortList( compoundMesh->parts[drawnBehindParts[partNum].first], distance );
		}

		for( unsigned partIndex = 0; partIndex < compoundMesh->numParts; ++partIndex ) {
			if( !meshOrderDesignators || meshOrderDesignators[partIndex] == 0.0f ) {
				addMeshToSortList( compoundMesh->parts[partIndex], distances[partIndex ] );
			}
		}

		for( unsigned partNum = 0; partNum < numDrawnInFrontParts; ++partNum ) {
			const float distance = wsw::max( 0.0f, minDistance - (float)( partNum + 1 ) );
			addMeshToSortList( compoundMesh->parts[drawnInFrontParts[partNum].first], distance );
		}
	}
}

void Frontend::addCoronaLightsToSortList( StateForCamera *stateForCamera, const entity_t *polyEntity,
										  const Scene::DynamicLight *lights, std::span<const uint16_t> indices ) {
	const float *const __restrict forwardAxis = stateForCamera->viewAxis;
	const float *const __restrict viewOrigin  = stateForCamera->viewOrigin;

	for( const unsigned index: indices ) {
		const Scene::DynamicLight *light = &lights[index];

		vec3_t toLight;
		VectorSubtract( light->origin, viewOrigin, toLight );
		const float distanceLike = DotProduct( toLight, forwardAxis );

		// TODO: Account for fogs
		const mfog_t *fog = nullptr;
		void *drawSurf = const_cast<Scene::DynamicLight *>( light );
		addEntryToSortList( stateForCamera, polyEntity, fog, m_coronaShader, distanceLike, 0, nullptr, drawSurf, ST_CORONA );
	}
}

void Frontend::addVisibleWorldSurfacesToSortList( StateForCamera *stateForCamera, Scene *scene ) {
	auto *const worldEnt = scene->m_worldent;

	// TODO: We just have to supply OutlineParamsProvider
	const bool worldOutlines = mapConfig.forceWorldOutlines || ( stateForCamera->refdef.rdflags & RDF_WORLDOUTLINES );
	if( worldOutlines && ( stateForCamera->viewCluster != -1 ) && r_outlines_scale->value > 0 ) {
		// TODO: Shouldn't it affect culling?
		worldEnt->outlineHeight = wsw::max( 0.0f, r_outlines_world->value );
	} else {
		worldEnt->outlineHeight = 0;
	}

	Vector4Copy( mapConfig.outlineColor, worldEnt->outlineColor );

	const auto numWorldModelMergedSurfaces      = rsh.worldBrushModel->numModelMergedSurfaces;
	msurface_t *const surfaces                  = rsh.worldBrushModel->surfaces;
	drawSurfaceBSP_t *const drawSurfaces        = stateForCamera->bspDrawSurfacesBuffer->get();
	MergedBspSurface *const mergedSurfaces      = rsh.worldBrushModel->mergedSurfaces;
	const MergedSurfSpan *const mergedSurfSpans = stateForCamera->drawSurfSurfSpansBuffer->get();
	std::span<const Scene::DynamicLight> dynamicLights { scene->m_dynamicLights.data(), scene->m_dynamicLights.size() };
	// TODO: Left-pack instead of branchy scanning?
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numWorldModelMergedSurfaces; ++mergedSurfNum ) {
		const MergedSurfSpan &surfSpan = mergedSurfSpans[mergedSurfNum];
		if( surfSpan.firstSurface <= surfSpan.lastSurface ) {
			drawSurfaceBSP_t *const drawSurf   = drawSurfaces + mergedSurfNum;
			drawSurf->mergedBspSurf            = mergedSurfaces + mergedSurfNum;
			msurface_t *const firstVisSurf     = surfaces + surfSpan.firstSurface;
			msurface_t *const lastVisSurf      = surfaces + surfSpan.lastSurface;
			addMergedBspSurfToSortList( stateForCamera, worldEnt, drawSurf, firstVisSurf, lastVisSurf, nullptr, dynamicLights );
		}
	}
}

auto Frontend::addEntryToSortList( StateForCamera *stateForCamera, const entity_t *e, const mfog_t *fog,
								   const shader_t *shader, float dist, unsigned order, const portalSurface_t *portalSurf,
								   const void *drawSurf, unsigned surfType, unsigned mergeabilitySeparator ) -> void * {
	if( shader ) [[likely]] {
		// TODO: This should be moved to an outer loop
		if( !( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) || !Shader_ReadDepth( shader ) ) [[likely]] {
			// TODO: This should be moved to an outer loop
			if( !rsh.worldBrushModel ) [[unlikely]] {
				fog = nullptr;
			}

			if( const unsigned distKey = R_PackDistKey( e->renderfx, shader, dist, order ) ) [[likely]] {

				const int fogNum = fog ? (int)( fog - rsh.worldBrushModel->fogs ) : -1;
				const int portalNum = portalSurf ? (int)( portalSurf - stateForCamera->portalSurfaces ) : -1;

				stateForCamera->sortList->emplace_back( sortedDrawSurf_t {
					.drawSurf              = (drawSurfaceType_t *)drawSurf,
					.distKey               = distKey,
					.sortKey               = R_PackSortKey( shader->id, fogNum, portalNum, e->number ),
					.surfType              = surfType,
					.mergeabilitySeparator = mergeabilitySeparator
				});

				return std::addressof( stateForCamera->sortList->back() );
			}
		}
	}

	return nullptr;
}

auto Frontend::tryAddingPortalSurface( StateForCamera *stateForCamera, const entity_t *ent,
									   const shader_t *shader, void *drawSurf ) -> portalSurface_t * {
	if( shader ) {
		// TODO: Should be state-specific
		if( stateForCamera->numPortalSurfaces < MAX_PORTAL_SURFACES ) {
			const bool depthPortal = !( shader->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) );
			// TOOD: ???
			if( !depthPortal || ( !r_fastsky->integer && stateForCamera->viewCluster >= 0 ) ) {

				portalSurface_t *portalSurface = &stateForCamera->portalSurfaces[stateForCamera->numPortalSurfaces++];
				memset( portalSurface, 0, sizeof( portalSurface_t ) );
				portalSurface->entity          = ent;
				portalSurface->shader          = shader;
				ClearBounds( portalSurface->mins, portalSurface->maxs );
				memset( portalSurface->texures, 0, sizeof( portalSurface->texures ) );
				// ?????
				if( depthPortal ) {
					stateForCamera->numDepthPortalSurfaces++;
				}

				return portalSurface;
			}
		}
	}

	return nullptr;
}

void Frontend::updatePortalSurface( StateForCamera *stateForCamera, portalSurface_t *portalSurface, const mesh_t *mesh,
									const float *mins, const float *maxs, const shader_t *shader, void *drawSurf ) {
	vec3_t v[3];
	for( unsigned i = 0; i < 3; i++ ) {
		VectorCopy( mesh->xyzArray[mesh->elems[i]], v[i] );
	}

	const entity_t *ent = portalSurface->entity;

	cplane_t plane, untransformed_plane;
	PlaneFromPoints( v, &untransformed_plane );
	untransformed_plane.dist += DotProduct( ent->origin, untransformed_plane.normal );
	untransformed_plane.dist += 1; // nudge along the normal a bit
	CategorizePlane( &untransformed_plane );

	if( shader->flags & SHADER_AUTOSPRITE ) {
		// autosprites are quads, facing the viewer
		if( mesh->numVerts < 4 ) {
			return;
		}

		vec3_t centre;
		// compute centre as average of 4 vertices
		VectorCopy( mesh->xyzArray[mesh->elems[3]], centre );
		for( unsigned i = 0; i < 3; i++ ) {
			VectorAdd( centre, v[i], centre );
		}

		VectorMA( ent->origin, 0.25, centre, centre );

		VectorNegate( &stateForCamera->viewAxis[AXIS_FORWARD], plane.normal );
		plane.dist = DotProduct( plane.normal, centre );
		CategorizePlane( &plane );
	} else {
		// regular surfaces
		if( !Matrix3_Compare( ent->axis, axis_identity ) ) {
			mat3_t entity_rotation;
			Matrix3_Transpose( ent->axis, entity_rotation );

			vec3_t temp;
			for( unsigned i = 0; i < 3; i++ ) {
				VectorCopy( v[i], temp );
				Matrix3_TransformVector( entity_rotation, temp, v[i] );
				VectorMA( ent->origin, ent->scale, v[i], v[i] );
			}

			PlaneFromPoints( v, &plane );
			CategorizePlane( &plane );
		} else {
			plane = untransformed_plane;
		}
	}

	const float dist = PlaneDiff( stateForCamera->viewOrigin, &plane );
	if( dist <= BACKFACE_EPSILON ) {
		// behind the portal plane
		if( !( shader->flags & SHADER_PORTAL_CAPTURE2 ) ) {
			// TODO: Mark as culled
			return;
		}

		// we need to render the backplane view
	}

	// check if portal view is opaque due to alphagen portal
	if( shader->portalDistance > 0.0f && dist > shader->portalDistance ) {
		// TODO: Mark as culled
		return;
	}

	portalSurface->plane = plane;
	portalSurface->untransformed_plane = untransformed_plane;

	AddPointToBounds( mins, portalSurface->mins, portalSurface->maxs );
	AddPointToBounds( maxs, portalSurface->mins, portalSurface->maxs );
	portalSurface->mins[3] = 0.0f, portalSurface->maxs[3] = 1.0f;
}

auto Frontend::tryUpdatingPortalSurfaceAndDistance( StateForCamera *stateForCamera, drawSurfaceBSP_t *drawSurf,
													const msurface_t *surf, const float *origin ) -> std::optional<float> {
	const shader_t *shader = drawSurf->mergedBspSurf->shader;
	if( shader->flags & SHADER_PORTAL ) {
		const sortedDrawSurf_t *const sds = (sortedDrawSurf_t *)drawSurf->listSurf;

		unsigned shaderNum, entNum;
		int portalNum, fogNum;
		R_UnpackSortKey( sds->sortKey, &shaderNum, &fogNum, &portalNum, &entNum );

		if( portalNum >= 0 ) {
			portalSurface_t *const portalSurface = stateForCamera->portalSurfaces + portalNum;
			vec3_t center;
			if( origin ) {
				VectorCopy( origin, center );
			} else {
				VectorAdd( surf->mins, surf->maxs, center );
				VectorScale( center, 0.5, center );
			}
			float dist = Distance( stateForCamera->refdef.vieworg, center );
			// draw portals in front-to-back order
			dist = 1024 - dist / 100.0f;
			if( dist < 1 ) {
				dist = 1;
			}
			updatePortalSurface( stateForCamera, portalSurface, &surf->mesh, surf->mins, surf->maxs, shader, drawSurf );
			return dist;
		}
	}
	return std::nullopt;
}


}