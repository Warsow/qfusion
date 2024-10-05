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
#include "materiallocal.h"
#include "../common/wswsortbyfield.h"

#include <algorithm>

void R_TransformForWorld();
void R_TranslateForEntity( const entity_t *e );
void R_TransformForEntity( const entity_t *e );

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
		R_AddSkeletalModelCache( entity, mod, stateForCamera->sceneIndex );

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

[[nodiscard]]
inline auto makeVertElemSpan( const MergedBspSurface *mergedSurf, const msurface_t *firstSurf, const msurface_t *lastSurf ) {
	assert( firstSurf <= lastSurf );

	return VertElemSpan {
		.firstVert = mergedSurf->firstVboVert + firstSurf->firstDrawSurfVert,
		.numVerts  = lastSurf->mesh.numVerts + ( lastSurf->firstDrawSurfVert - firstSurf->firstDrawSurfVert ),
		.firstElem = mergedSurf->firstVboElem + firstSurf->firstDrawSurfElem,
		.numElems  = lastSurf->mesh.numElems + ( lastSurf->firstDrawSurfElem - firstSurf->firstDrawSurfElem ),
	};
}

void Frontend::addBrushModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *brushModelEntities,
												std::span<const VisTestedModel> models,
												std::span<const uint16_t> indices, std::span<const Scene::DynamicLight> lights ) {
	const MergedBspSurface *const mergedSurfaces = rsh.worldBrushModel->mergedSurfaces;
	const msurface_t *const surfaces             = rsh.worldBrushModel->surfaces;

	MergedSurfSpan *const mergedSurfSpans  = stateForCamera->drawSurfSurfSpansBuffer->get();
	unsigned *const subspans               = stateForCamera->drawSurfSurfSubspansBuffer->get();
	VertElemSpan *const vertElemSpans      = stateForCamera->drawSurfVertElemSpansBuffer->get();

	for( const auto modelIndex: indices ) {
		const VisTestedModel &visTestedModel = models[modelIndex];
		const auto *const model              = visTestedModel.selectedLod;
		const auto *const entity             = brushModelEntities + visTestedModel.indexInEntitiesGroup;
		const auto *const brushModel         = ( mbrushmodel_t * )model->extradata;
		assert( brushModel->numModelMergedSurfaces );

		vec3_t origin;
		VectorAvg( visTestedModel.absMins, visTestedModel.absMaxs, origin );

		for( unsigned i = 0; i < brushModel->numModelMergedSurfaces; i++ ) {
			const unsigned surfNum                = brushModel->firstModelMergedSurface + i;
			const MergedBspSurface *mergedSurface = mergedSurfaces + surfNum;
			MergedSurfSpan *surfSpan              = mergedSurfSpans + surfNum;

			surfSpan->firstSurface    = (int)mergedSurface->firstWorldSurface;
			surfSpan->lastSurface     = (int)( mergedSurface->firstWorldSurface + mergedSurface->numWorldSurfaces - 1 );
			surfSpan->vertSpansOffset = stateForCamera->drawSurfVertElemSpansOffset;
			surfSpan->numSubspans     = 1;
			surfSpan->subspansOffset  = stateForCamera->drawSurfSurfSubspansOffset;

			assert( stateForCamera->drawSurfVertElemSpansOffset < stateForCamera->drawSurfVertElemSpansBuffer->capacity() );
			vertElemSpans[stateForCamera->drawSurfVertElemSpansOffset++] = makeVertElemSpan( mergedSurface,
																							 surfaces + surfSpan->firstSurface,
																							 surfaces + surfSpan->lastSurface );
			subspans[stateForCamera->drawSurfSurfSubspansOffset++] = surfSpan->firstSurface;
			subspans[stateForCamera->drawSurfSurfSubspansOffset++] = surfSpan->lastSurface;

			addMergedBspSurfToSortList( stateForCamera, entity, *surfSpan, surfNum, origin, lights );
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

void Frontend::calcSubspansOfMergedSurfSpans( StateForCamera *stateForCamera ) {
	const MergedBspSurface *const mergedSurfs = rsh.worldBrushModel->mergedSurfaces;
	const uint8_t *const surfVisTable         = stateForCamera->surfVisTableBuffer->get();
	const msurface_t *const surfaces          = rsh.worldBrushModel->surfaces;
	const unsigned numMergedSurfaces          = rsh.worldBrushModel->numMergedSurfaces;

	const unsigned maxTotalSubspans        = stateForCamera->drawSurfSurfSubspansBuffer->capacity();
	const unsigned maxTotalVertSpans       = stateForCamera->drawSurfVertElemSpansBuffer->capacity();
	MergedSurfSpan *const mergedSurfSpans  = stateForCamera->drawSurfSurfSpansBuffer->get();
	unsigned *const mergedSurfSubspans     = stateForCamera->drawSurfSurfSubspansBuffer->get();
	VertElemSpan *const surfVertElemSpans  = stateForCamera->drawSurfVertElemSpansBuffer->get();

	unsigned subspanDataOffset      = 0;
	unsigned vertElemSpanDataOffset = 0;
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numMergedSurfaces; ++mergedSurfNum ) {
		MergedSurfSpan *const surfSpan = mergedSurfSpans + mergedSurfNum;
		if( surfSpan->firstSurface <= surfSpan->lastSurface ) {
			const unsigned maxSpansForThisSurface = surfSpan->lastSurface - surfSpan->firstSurface + 1;
			// Check once accounting for extreme partitioning.
			// We *have* to check it in runtime.
			if( subspanDataOffset + 2 * maxSpansForThisSurface > maxTotalSubspans ) [[unlikely]] {
				wsw::failWithRuntimeError( "Too many surf subspans" );
			}
			if( vertElemSpanDataOffset + maxSpansForThisSurface > maxTotalVertSpans ) [[unlikely]] {
				wsw::failWithRuntimeError( "Too many surf vert/elem spans");
			}

			assert( surfVisTable[surfSpan->firstSurface] );
			assert( surfVisTable[surfSpan->lastSurface] );

			const MergedBspSurface *mergedSurf = mergedSurfs + mergedSurfNum;
#if 0

			assert( surfSpan->firstSurface >= mergedSurf->firstWorldSurface );
			assert( surfSpan->lastSurface < mergedSurf->firstWorldSurface + mergedSurf->numWorldSurfaces );
			// Make sure no visibility values are set outside of the merged span
			for( unsigned surfNum = mergedSurf->firstWorldSurface; surfNum < surfSpan->firstSurface; ++surfNum ) {
				assert( !surfVisTable[surfNum] );
			}
			for( unsigned surfNum = surfSpan->lastSurface + 1u; surfNum < mergedSurf->firstWorldSurface +
				mergedSurf->numWorldSurfaces; ++surfNum ) {
				assert( !surfVisTable[surfNum] );
			}
#endif

#if 0
			for( int surfNum = surfSpan->firstSurface; surfNum <= surfSpan->lastSurface; ++surfNum ) {
				if( !surfVisTable[surfNum] ) {
					const auto &surf = rsh.worldBrushModel->surfaces[surfNum];
					addDebugLine( surf.mins, surf.maxs );
				}
			}
#endif

			const auto oldSubspanDataOffset      = subspanDataOffset;
			const auto oldVertElemSpanDataOffset = vertElemSpanDataOffset;

			unsigned numSubspans = 0;
#if 1
			int subspanStart     = surfSpan->firstSurface;
			bool isInVisSubspan  = true;
			for( int surfNum = surfSpan->firstSurface + 1; surfNum <= surfSpan->lastSurface; ++surfNum ) {
				if( isInVisSubspan != ( surfVisTable[surfNum] != 0 ) ) {
					if( isInVisSubspan ) {
						assert( subspanStart <= surfNum - 1 );
						assert( subspanDataOffset + 2 <= stateForCamera->drawSurfSurfSubspansBuffer->capacity() );
						mergedSurfSubspans[subspanDataOffset++]     = subspanStart;
						mergedSurfSubspans[subspanDataOffset++]     = surfNum - 1;
						surfVertElemSpans[vertElemSpanDataOffset++] = makeVertElemSpan( mergedSurf, surfaces + subspanStart,
																						surfaces + surfNum - 1 );
						isInVisSubspan = false;
						numSubspans++;
					} else {
						subspanStart   = surfNum;
						isInVisSubspan = true;
					}
				}
			}

			assert( isInVisSubspan );
			assert( subspanStart <= surfSpan->lastSurface );
			assert( subspanDataOffset + 2 <= stateForCamera->drawSurfSurfSubspansBuffer->capacity() );
			mergedSurfSubspans[subspanDataOffset++]     = subspanStart;
			mergedSurfSubspans[subspanDataOffset++]     = surfSpan->lastSurface;
			surfVertElemSpans[vertElemSpanDataOffset++] = makeVertElemSpan( mergedSurf, surfaces + subspanStart,
																			surfaces + surfSpan->lastSurface );
			numSubspans++;
#else
			// Extreme partitioning for test purposes
			for( int surfNum = surfSpan->firstSurface; surfNum <= surfSpan->lastSurface; ++surfNum ) {
				if( surfVisTable[surfNum] ) {
					mergedSurfSubspans[subspanDataOffset++]     = surfNum;
					mergedSurfSubspans[subspanDataOffset++]     = surfNum;
					surfVertElemSpans[vertElemSpanDataOffset++] = makeVertElemSpan( mergedSurf, surfaces + surfNum,
																					surfaces + surfNum );
					++numSubspans;
				}
			}
#endif

			surfSpan->subspansOffset  = oldSubspanDataOffset;
			surfSpan->vertSpansOffset = oldVertElemSpanDataOffset;
			surfSpan->numSubspans     = numSubspans;

			assert( subspanDataOffset - oldSubspanDataOffset == 2 * numSubspans );
			assert( vertElemSpanDataOffset - oldVertElemSpanDataOffset == numSubspans );
		}
	}

	// Save for further use for supplying brush models
	stateForCamera->drawSurfSurfSubspansOffset  = subspanDataOffset;
	stateForCamera->drawSurfVertElemSpansOffset = vertElemSpanDataOffset;
}

void Frontend::addMergedBspSurfToSortList( StateForCamera *stateForCamera, const entity_t *entity,
										   const MergedSurfSpan &surfSpan, unsigned mergedSurfNum,
										   const float *maybeOrigin, std::span<const Scene::DynamicLight> lightsSpan ) {
	assert( surfSpan.firstSurface <= surfSpan.lastSurface );
	assert( surfSpan.numSubspans );

	msurface_t *const surfaces                  = rsh.worldBrushModel->surfaces;
	drawSurfaceBSP_t *const drawSurfaces        = stateForCamera->bspDrawSurfacesBuffer->get();
	MergedBspSurface *const mergedSurfaces      = rsh.worldBrushModel->mergedSurfaces;

	drawSurfaceBSP_t *const drawSurf = drawSurfaces + mergedSurfNum;
	drawSurf->mergedBspSurf          = mergedSurfaces + mergedSurfNum;

	const MergedBspSurface *const mergedSurf = drawSurf->mergedBspSurf;
	const shader_t *const surfMaterial       = mergedSurf->shader;

	// Must be set earlier
	portalSurface_t *portalSurface = drawSurf->portalSurface;

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

	drawSurf->vertElemSpans = stateForCamera->drawSurfVertElemSpansBuffer->get() + surfSpan.vertSpansOffset;
	drawSurf->numSpans      = surfSpan.numSubspans;

	unsigned dlightBits = 0;
	if( stateForCamera->numVisibleProgramLights ) {
		const unsigned *const surfaceDlightBits = stateForCamera->leafLightBitsOfSurfacesBuffer->get();
		const msurface_t *const worldSurfaces   = rsh.worldBrushModel->surfaces;
		const unsigned *const subspans          = stateForCamera->drawSurfSurfSubspansBuffer->get();
		const unsigned numLights                = lightsSpan.size();

		unsigned subspanNum = 0;
		do {
			const msurface_t *const firstVisSurf = surfaces + subspans[surfSpan.subspansOffset + 2 * subspanNum + 0];
			const msurface_t *const lastVisSurf  = surfaces + subspans[surfSpan.subspansOffset + 2 * subspanNum + 1];
			const msurface_t *surf = firstVisSurf;
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
			} while( ++surf <= lastVisSurf );
		} while( ++subspanNum < surfSpan.numSubspans );
	}

	float resultDist = drawSurf->portalDistance;
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
										   const DynamicMesh **meshes, std::span<const uint16_t> indicesOfMeshes,
										   std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices ) {
	const float *const __restrict viewOrigin = stateForCamera->viewOrigin;

	for( const unsigned meshIndex: indicesOfMeshes ) {
		const DynamicMesh *const mesh = meshes[meshIndex];

		vec3_t meshCenter;
		VectorAvg( mesh->cullMins, mesh->cullMaxs, meshCenter );
		const float distance = DistanceFast( meshCenter, viewOrigin );

		addDynamicMeshToSortList( stateForCamera, meshEntity, mesh, distance, offsetsOfVerticesAndIndices );
	}
}

void Frontend::addCompoundDynamicMeshesToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity,
												   const Scene::CompoundDynamicMesh *meshes,
												   std::span<const uint16_t> indicesOfMeshes,
												   std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices ) {
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

		const auto addMesh = [&]( const DynamicMesh *mesh, float distance ) -> void {
			addDynamicMeshToSortList( stateForCamera, meshEntity, mesh, distance, offsetsOfVerticesAndIndices );
		};

		for( unsigned partNum = 0; partNum < numDrawnBehindParts; ++partNum ) {
			const float distance = maxDistance + (float)( numDrawnBehindParts + 1 - partNum );
			addMesh( compoundMesh->parts[drawnBehindParts[partNum].first], distance );
		}

		for( unsigned partIndex = 0; partIndex < compoundMesh->numParts; ++partIndex ) {
			if( !meshOrderDesignators || meshOrderDesignators[partIndex] == 0.0f ) {
				addMesh( compoundMesh->parts[partIndex], distances[partIndex] );
			}
		}

		for( unsigned partNum = 0; partNum < numDrawnInFrontParts; ++partNum ) {
			const float distance = wsw::max( 0.0f, minDistance - (float)( partNum + 1 ) );
			addMesh( compoundMesh->parts[drawnInFrontParts[partNum].first], distance );
		}
	}
}

void Frontend::addDynamicMeshToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity,
										 const DynamicMesh *mesh, float distance,
										 std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices ) {
	DynamicMeshDrawSurface *const drawSurface = stateForCamera->dynamicMeshDrawSurfaces +
												stateForCamera->numDynamicMeshDrawSurfaces;

	std::optional<std::pair<unsigned, unsigned>> maybeStorageRequirements;
	maybeStorageRequirements = mesh->getStorageRequirements( stateForCamera->viewOrigin, stateForCamera->viewAxis,
															 stateForCamera->lodScaleForFov, stateForCamera->cameraId,
															 drawSurface->scratchpad );
	if( maybeStorageRequirements ) [[likely]] {
		const auto [numVertices, numIndices] = *maybeStorageRequirements;
		assert( numVertices && numIndices );
		// TODO: Allow more if we draw using base vertex
		// TODO: Protect against indices overflow
		if( offsetsOfVerticesAndIndices->first + numVertices <= std::numeric_limits<uint16_t>::max() ) {
			drawSurface->requestedNumVertices = numVertices;
			drawSurface->requestedNumIndices  = numIndices;
			drawSurface->verticesOffset       = offsetsOfVerticesAndIndices->first;
			drawSurface->indicesOffset        = offsetsOfVerticesAndIndices->second;
			drawSurface->dynamicMesh          = mesh;

			const mfog_t *fog        = nullptr;
			const shader_s *material = mesh->material ? mesh->material : rsh.whiteShader;
			addEntryToSortList( stateForCamera, meshEntity, fog, material, distance, 0, nullptr, drawSurface, ST_DYNAMIC_MESH );

			stateForCamera->numDynamicMeshDrawSurfaces++;
			offsetsOfVerticesAndIndices->first += numVertices;
			offsetsOfVerticesAndIndices->second += numIndices;
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

	const bool worldOutlines = mapConfig.forceWorldOutlines || ( stateForCamera->refdef.rdflags & RDF_WORLDOUTLINES );
	if( worldOutlines && ( stateForCamera->viewCluster != -1 ) && r_outlines_scale->value > 0 ) {
		// TODO: Shouldn't it affect culling?
		worldEnt->outlineHeight = wsw::max( 0.0f, r_outlines_world->value );
	} else {
		worldEnt->outlineHeight = 0;
	}

	Vector4Copy( mapConfig.outlineColor, worldEnt->outlineColor );

	const auto numWorldModelMergedSurfaces      = rsh.worldBrushModel->numModelMergedSurfaces;
	const MergedSurfSpan *const mergedSurfSpans = stateForCamera->drawSurfSurfSpansBuffer->get();
	std::span<const Scene::DynamicLight> dynamicLights { scene->m_dynamicLights.data(), scene->m_dynamicLights.size() };
	// TODO: Left-pack instead of branchy scanning?
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numWorldModelMergedSurfaces; ++mergedSurfNum ) {
		const MergedSurfSpan &surfSpan = mergedSurfSpans[mergedSurfNum];
		if( surfSpan.firstSurface <= surfSpan.lastSurface ) {
			addMergedBspSurfToSortList( stateForCamera, worldEnt, surfSpan, mergedSurfNum, nullptr, dynamicLights );
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

void Frontend::processWorldPortalSurfaces( StateForCamera *stateForCamera, Scene *scene, bool isCameraAPortalCamera ) {
	const auto numWorldModelMergedSurfaces       = rsh.worldBrushModel->numModelMergedSurfaces;
	const MergedSurfSpan *const mergedSurfSpans  = stateForCamera->drawSurfSurfSpansBuffer->get();
	drawSurfaceBSP_t *const drawSurfaces         = stateForCamera->bspDrawSurfacesBuffer->get();
	const MergedBspSurface *const mergedSurfaces = rsh.worldBrushModel->mergedSurfaces;
	const msurface_t *const surfaces             = rsh.worldBrushModel->surfaces;

	// TODO: Left-pack during calculation of surf subspans
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numWorldModelMergedSurfaces; ++mergedSurfNum ) {
		const MergedSurfSpan &surfSpan = mergedSurfSpans[mergedSurfNum];
		// TODO: Left-pack during calculation of surf subspans
		if( surfSpan.firstSurface <= surfSpan.lastSurface ) {
			const MergedBspSurface &mergedSurf = mergedSurfaces[mergedSurfNum];
			// TODO: Save portal surface in draw surface
			portalSurface_t *portalSurface = nullptr;
			if( mergedSurf.shader->flags & SHADER_PORTAL ) [[unlikely]] {
				if( stateForCamera->numPortalSurfaces < MAX_PORTAL_SURFACES ) {
					// We currently don't support depth-masked portals, only capturing ones
					if( mergedSurf.shader->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) ) {
						portalSurface = &stateForCamera->portalSurfaces[stateForCamera->numPortalSurfaces++];
						memset( portalSurface, 0, sizeof( portalSurface_t ) );
						portalSurface->entity          = scene->m_worldent;
						portalSurface->shader          = mergedSurf.shader;
						portalSurface->portalNumber    = stateForCamera->numPortalSurfaces - 1;
						ClearBounds( portalSurface->mins, portalSurface->maxs );
						memset( portalSurface->texures, 0, sizeof( portalSurface->texures ) );
					}
				}
			}
			if( portalSurface ) [[unlikely]] {
				// For brush models, currently unsupported
				constexpr const float *modelOrigin = nullptr;
				// TODO: Save result dist
				float resultDist = 0.0;
				const msurface_t *const firstVisSurf = surfaces + surfSpan.firstSurface;
				const msurface_t *const lastVisSurf  = surfaces + surfSpan.lastSurface;
				for( const msurface_s *surf = firstVisSurf; surf <= lastVisSurf; ++surf ) {
					vec3_t center;
					if( modelOrigin ) {
						VectorCopy( modelOrigin, center );
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
					updatePortalSurface( stateForCamera, portalSurface, &surf->mesh, surf->mins, surf->maxs, mergedSurf.shader );
					resultDist = wsw::max( resultDist, dist );
				}
				drawSurfaces[mergedSurfNum].portalSurface  = portalSurface;
				drawSurfaces[mergedSurfNum].portalDistance = resultDist;
			} else {
				drawSurfaces[mergedSurfNum].portalSurface  = nullptr;
				drawSurfaces[mergedSurfNum].portalDistance = 0.0f;
			}
		}
	}

	// Note: Looks like we have to properly update portal surfaces even if portals are disabled (for sorting reasons).
	// Check whether actual drawing is enabled upon doing that.
	if( !isCameraAPortalCamera && stateForCamera->viewCluster >= 0 && !r_fastsky->integer ) {
		for( unsigned i = 0; i < stateForCamera->numPortalSurfaces; ++i ) {
			prepareDrawingPortalSurface( stateForCamera, scene, &stateForCamera->portalSurfaces[i] );
		}
	}
}

void Frontend::updatePortalSurface( StateForCamera *stateForCamera, portalSurface_t *portalSurface, const mesh_t *mesh,
									const float *mins, const float *maxs, const shader_t *shader ) {
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

static const drawSurf_cb r_drawSurfCb[ST_MAX_TYPES] =
	{
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

static const batchDrawSurf_cb r_batchDrawSurfCb[ST_MAX_TYPES] =
	{
		/* ST_NONE */
		nullptr,
		/* ST_BSP */
		nullptr,
		/* ST_ALIAS */
		nullptr,
		/* ST_SKELETAL */
		nullptr,
		/* ST_SPRITE */
		( batchDrawSurf_cb ) & R_SubmitSpriteSurfsToBackend,
		/* ST_QUAD_POLY */
		( batchDrawSurf_cb ) & R_SubmitQuadPolysToBackend,
		/* ST_DYNAMIC_MESH */
		nullptr,
		/* ST_PARTICLE */
		( batchDrawSurf_cb ) & R_SubmitParticleSurfsToBackend,
		/* ST_CORONA */
		( batchDrawSurf_cb ) & R_SubmitCoronaSurfsToBackend,
		/* ST_NULLMODEL */
		nullptr,
	};

void Frontend::processSortList( StateForCamera *stateForCamera, Scene *scene ) {
	stateForCamera->drawActionsList->clear();

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

		const bool isDrawSurfBatched = ( r_batchDrawSurfCb[surfType] ? true : false );

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
				batchDrawSurf_cb callback            = r_batchDrawSurfCb[batchSpanBegin->surfType];
				const shader_s *prevShader           = materialCache->getMaterialById( prevShaderNum );
				const entity_t *prevEntity           = scene->m_entities[prevEntNum];
				const sortedDrawSurf_t *batchSpanEnd = sds;

				assert( batchSpanEnd > batchSpanBegin );

				drawActionsList->append( [=]( FrontendToBackendShared *fsh ) {
					RB_FlushDynamicMeshes();
					callback( fsh, prevEntity, prevShader, prevFog, prevPortalSurface, { batchSpanBegin, batchSpanEnd } );
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

		prevShaderNum = shaderNum;
		prevEntNum = entNum;
		prevFogNum = fogNum;
		prevIsDrawSurfBatched = isDrawSurfBatched;
		prevSurfType = surfType;
		prevMergeabilitySeparator = sds->mergeabilitySeparator;
		prevPortalNum = portalNum;
		prevEntityFX = entityFX;
		prevFog = fog;
		prevPortalSurface = portalSurface;
	}

	if( batchSpanBegin ) {
		batchDrawSurf_cb callback            = r_batchDrawSurfCb[batchSpanBegin->surfType];
		const shader_t *prevShader           = materialCache->getMaterialById( prevShaderNum );
		const entity_t *prevEntity           = scene->m_entities[prevEntNum];
		const sortedDrawSurf_t *batchSpanEnd = drawSurfs + numDrawSurfs;

		assert( batchSpanEnd > batchSpanBegin );

		drawActionsList->append( [=]( FrontendToBackendShared *fsh ) {
			RB_FlushDynamicMeshes();
			callback( fsh, prevEntity, prevShader, prevFog, prevPortalSurface, { batchSpanBegin, batchSpanEnd } );
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

}