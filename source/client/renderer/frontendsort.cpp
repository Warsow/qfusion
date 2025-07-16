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
#include <common/types/smallassocarray.h>
#include <common/helpers/sortbyfield.h>

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

void Frontend::addAliasModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *aliasModelEntities,
												std::span<const VisTestedModel> models, std::span<const uint16_t> indices ) {
	MaterialCache *const materialCache = MaterialCache::instance();

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
				shader = materialCache->findMeshMaterialInSkin( entity->customSkin, wsw::StringView( mesh->name ) );
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
	MaterialCache *const materialCache = MaterialCache::instance();

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
				shader = materialCache->findMeshMaterialInSkin( entity->customSkin, wsw::StringView( mesh->name ) );
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

struct MultiDrawSpanBuilder {
	const GLvoid **m_indices { nullptr };
	GLsizei *m_counts { nullptr };
	unsigned m_offset { 0 };
	unsigned m_numCallsInSpan { 0 };

	void addSurfSpan( const MergedBspSurface *mergedSurf, const msurface_t *firstSurf, const msurface_t *lastSurf ) {
		assert( firstSurf <= lastSurf );
		const auto firstElem = mergedSurf->firstVboElem + firstSurf->firstDrawSurfElem;
		const auto numElems  = lastSurf->mesh.numElems + ( lastSurf->firstDrawSurfElem - firstSurf->firstDrawSurfElem );
		m_indices[m_offset] = (GLvoid *)( firstElem * sizeof( elem_t ) );
		m_counts[m_offset]  = (GLint)numElems;
		m_offset++;
		m_numCallsInSpan++;
	}

	[[nodiscard]]
	auto buildAndReset() -> MultiDrawElemSpan {
		assert( m_numCallsInSpan && m_offset && m_numCallsInSpan <= m_offset );
		MultiDrawElemSpan result {
			.counts   = m_counts  + ( m_offset - m_numCallsInSpan ),
			.indices  = m_indices + ( m_offset - m_numCallsInSpan ),
			.numDraws = (GLsizei)m_numCallsInSpan,
		};
		m_numCallsInSpan = 0;
		return result;
	}
};

void Frontend::addBrushModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *brushModelEntities,
												std::span<const VisTestedModel> models,
												std::span<const uint16_t> indices, std::span<const Scene::DynamicLight> lights ) {
	const MergedBspSurface *const mergedSurfaces = rsh.worldBrushModel->mergedSurfaces;
	const msurface_t *const surfaces             = rsh.worldBrushModel->surfaces;

	drawSurfaceBSP_t *const bspDrawSurfaces = stateForCamera->bspDrawSurfacesBuffer->get();
	MergedSurfSpan *const mergedSurfSpans   = stateForCamera->drawSurfSurfSpansBuffer->get();
	unsigned *const subspans                = stateForCamera->drawSurfSurfSubspansBuffer->get();

	MultiDrawSpanBuilder multiDrawSpanBuilder {
		.m_indices = stateForCamera->drawSurfMultiDrawIndicesBuffer->get(),
		.m_counts  = stateForCamera->drawSurfMultiDrawCountsBuffer->get(),
		.m_offset  = stateForCamera->drawSurfSurfSubspansOffset,
	};

	for( const auto modelIndex: indices ) {
		const VisTestedModel &visTestedModel = models[modelIndex];
		const auto *const model              = visTestedModel.selectedLod;
		const auto *const entity             = brushModelEntities + visTestedModel.indexInEntitiesGroup;
		const auto *const brushModel         = ( mbrushmodel_t * )model->extradata;
		assert( brushModel->numModelMergedSurfaces );

		vec3_t origin;
		VectorAvg( visTestedModel.absMins, visTestedModel.absMaxs, origin );

		for( unsigned i = 0; i < brushModel->numModelMergedSurfaces; i++ ) {
			const unsigned mergedSurfNum          = brushModel->firstModelMergedSurface + i;
			const MergedBspSurface *mergedSurface = mergedSurfaces + mergedSurfNum;
			MergedSurfSpan *surfSpan              = mergedSurfSpans + mergedSurfNum;
			drawSurfaceBSP_t *drawSurf            = bspDrawSurfaces + mergedSurfNum;

			// processWorldPortalSurfaces() does not handle brush model surfaces.
			// TODO: Should we care of portal surfaces in brush models?
			drawSurf->portalSurface  = nullptr;
			drawSurf->portalDistance = 0.0f;

			surfSpan->firstSurface    = (int)mergedSurface->firstWorldSurface;
			surfSpan->lastSurface     = (int)( mergedSurface->firstWorldSurface + mergedSurface->numWorldSurfaces - 1 );
			surfSpan->numSubspans     = 1;
			surfSpan->subspansOffset  = stateForCamera->drawSurfSurfSubspansOffset;

			multiDrawSpanBuilder.addSurfSpan( mergedSurface, surfaces + surfSpan->firstSurface, surfaces + surfSpan->lastSurface );
			surfSpan->mdSpan = multiDrawSpanBuilder.buildAndReset();

			subspans[stateForCamera->drawSurfSurfSubspansOffset++] = surfSpan->firstSurface;
			subspans[stateForCamera->drawSurfSurfSubspansOffset++] = surfSpan->lastSurface;

			addMergedBspSurfToSortList( stateForCamera, entity, *surfSpan, mergedSurfNum, origin, lights );
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
			addEntryToSortList( stateForCamera, entity, fog, entity->customShader, dist, 0, nullptr, entity, ST_SPRITE );
		}
	}
}

void Frontend::calcSubspansOfMergedSurfSpans( StateForCamera *stateForCamera ) {
	const MergedBspSurface *const mergedSurfs = rsh.worldBrushModel->mergedSurfaces;
	const uint8_t *const surfVisTable         = stateForCamera->surfVisTableBuffer->get();
	const msurface_t *const surfaces          = rsh.worldBrushModel->surfaces;
	const unsigned numMergedSurfaces          = rsh.worldBrushModel->numMergedSurfaces;

	const unsigned maxTotalSubspans        = stateForCamera->drawSurfSurfSubspansBuffer->capacity();
	MergedSurfSpan *const mergedSurfSpans  = stateForCamera->drawSurfSurfSpansBuffer->get();
	unsigned *const mergedSurfSubspans     = stateForCamera->drawSurfSurfSubspansBuffer->get();

	MultiDrawSpanBuilder multiDrawSpanBuilder {
		.m_indices = stateForCamera->drawSurfMultiDrawIndicesBuffer->get(),
		.m_counts  = stateForCamera->drawSurfMultiDrawCountsBuffer->get(),
	};

	unsigned subspanDataOffset  = 0;
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numMergedSurfaces; ++mergedSurfNum ) {
		MergedSurfSpan *const surfSpan = mergedSurfSpans + mergedSurfNum;
		if( surfSpan->firstSurface <= surfSpan->lastSurface ) {
			const unsigned maxSpansForThisSurface = surfSpan->lastSurface - surfSpan->firstSurface + 1;
			// Check once accounting for extreme partitioning.
			// We *have* to check it in runtime.
			if( subspanDataOffset + 2 * maxSpansForThisSurface > maxTotalSubspans ) [[unlikely]] {
				wsw::failWithRuntimeError( "Too many surf subspans" );
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
					addDebugLine( stateForCamera, surf.mins, surf.maxs );
				}
			}
#endif

			const auto oldSubspanDataOffset   = subspanDataOffset;

			unsigned numSubspans = 0;
#if 1
			int subspanStart     = surfSpan->firstSurface;
			bool isInVisSubspan  = true;
			for( int surfNum = surfSpan->firstSurface + 1; surfNum <= surfSpan->lastSurface; ++surfNum ) {
				if( isInVisSubspan != ( surfVisTable[surfNum] != 0 ) ) {
					if( isInVisSubspan ) {
						assert( subspanStart <= surfNum - 1 );
						assert( subspanDataOffset + 2 <= stateForCamera->drawSurfSurfSubspansBuffer->capacity() );
						mergedSurfSubspans[subspanDataOffset++] = subspanStart;
						mergedSurfSubspans[subspanDataOffset++] = surfNum - 1;
						multiDrawSpanBuilder.addSurfSpan( mergedSurf, surfaces + subspanStart, surfaces + surfNum - 1 );
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
			multiDrawSpanBuilder.addSurfSpan( mergedSurf, surfaces + subspanStart, surfaces + surfSpan->lastSurface );
			numSubspans++;
#else
			// Extreme partitioning for test purposes
			for( int surfNum = surfSpan->firstSurface; surfNum <= surfSpan->lastSurface; ++surfNum ) {
				if( surfVisTable[surfNum] ) {
					mergedSurfSubspans[subspanDataOffset++]     = surfNum;
					mergedSurfSubspans[subspanDataOffset++]     = surfNum;
					multiDrawSpanBuilder.addSurfSpan( mergedSurf, surfaces + surfNum, surfaces + surfNum );
					++numSubspans;
				}
			}
#endif

			surfSpan->subspansOffset = oldSubspanDataOffset;
			surfSpan->numSubspans    = numSubspans;
			surfSpan->mdSpan         = multiDrawSpanBuilder.buildAndReset();

			assert( subspanDataOffset - oldSubspanDataOffset == 2 * numSubspans );
		}
	}

	// Save for further use for supplying brush models
	stateForCamera->drawSurfSurfSubspansOffset  = subspanDataOffset;
	stateForCamera->drawSurfMultiDrawDataOffset = multiDrawSpanBuilder.m_offset;
}

void Frontend::addMergedBspSurfToSortList( StateForCamera *stateForCamera, const entity_t *entity,
										   const MergedSurfSpan &surfSpan, unsigned mergedSurfNum,
										   const float *maybeOrigin, std::span<const Scene::DynamicLight> lightsSpan ) {
	assert( mergedSurfNum < rsh.worldBrushModel->numMergedSurfaces );
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
	assert( !portalSurface || portalSurface - stateForCamera->portalSurfaces < stateForCamera->numPortalSurfaces );

	const mfog_t *fog        = mergedSurf->fog;
	const unsigned drawOrder = R_PackOpaqueOrder( fog, surfMaterial, mergedSurf->numLightmaps, false );

	drawSurf->dlightBits = 0;

	const auto maybeIndexInSortList = addEntryToSortList( stateForCamera, entity, fog, surfMaterial, WORLDSURF_DIST,
														  drawOrder, portalSurface, drawSurf, ST_BSP );
	if( maybeIndexInSortList == std::nullopt ) [[unlikely]] {
		drawSurf->mdSpan.numDraws = 0;
		return;
	}

	if( portalSurface && !( surfMaterial->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) ) ) {
		//addEntryToSortList( m_state.portalmasklist, e, nullptr, nullptr, 0, 0, nullptr, drawSurf );
	}

	drawSurf->mdSpan = stateForCamera->drawSurfSurfSpansBuffer->get()[mergedSurfNum].mdSpan;

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

		sortedDrawSurf_t *const sds = stateForCamera->sortList->data() + *maybeIndexInSortList;
		sds->distKey = R_PackDistKey( 0, surfMaterial, resultDist, order );
	}
}

void Frontend::addParticlesToSortList( StateForCamera *stateForCamera, const entity_t *particleEntity,
									   const Scene::ParticlesAggregate *particles,
									   std::span<const uint16_t> aggregateIndices ) {
	const float *const __restrict forwardAxis = stateForCamera->viewAxis;
	const float *const __restrict viewOrigin  = stateForCamera->viewOrigin;

	auto *const shaderParamsStorage   = stateForCamera->shaderParamsStorage;
	auto *const materialParamsStorage = stateForCamera->materialParamsStorage;

	unsigned numParticleDrawSurfaces = 0;
	for( const unsigned aggregateIndex: aggregateIndices ) {
		const Scene::ParticlesAggregate *const __restrict pa = particles + aggregateIndex;

		wsw::SmallAssocArray<unsigned, int, MAX_SHADER_IMAGES> overrideParamsIndicesForImageIndices;
		const auto addOverrideParams = [&]( float frac ) -> int {
			const int resultIndex = (int)shaderParamsStorage->size();
			shaderParamsStorage->emplace_back( ShaderParams {
				.materialComponentIndex = (int)materialParamsStorage->size(),
			});
			materialParamsStorage->emplace_back( ShaderParams::Material {
				.shaderFrac = frac,
			});
			return resultIndex;
		};

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

			unsigned mergeabilityKey = aggregateIndex;
			int overrideParamsIndex  = -1;

			shader_s *material = pa->appearanceRules.materials[particle->instanceMaterialIndex];
			if( material->flags & SHADER_ANIM_FRAC ) {
				// Try binning by resuling image index in this case
				// (particles with small difference in their fractions but with matching selected images
				// can use any fraction as long as it yields the same image during shader parameter setup)
				if( material->numpasses == 1 ) {
					unsigned imageIndex = 0;
					for( unsigned i = 1; i < material->passes[0].anim_numframes; ++i ) {
						if( material->passes[0].timelineFracs[i] < particle->lifetimeFrac ) {
							imageIndex = i;
						} else {
							break;
						}
					}
					const auto it = overrideParamsIndicesForImageIndices.find( imageIndex );
					if( it != overrideParamsIndicesForImageIndices.end() ) {
						overrideParamsIndex = ( *it ).second;
					} else {
						overrideParamsIndex = addOverrideParams( particle->lifetimeFrac );
						(void)overrideParamsIndicesForImageIndices.insert( imageIndex, overrideParamsIndex );
					}
				} else {
					overrideParamsIndex = addOverrideParams( particle->lifetimeFrac );
				}
				static_assert( Scene::kMaxParticleAggregates <= ( 1 << 8 ) );
				mergeabilityKey |= ( 1 + (unsigned)overrideParamsIndex ) << 8;
			}

			// TODO: Inline/add some kind of bulk insertion
			addEntryToSortList( stateForCamera, particleEntity, fog, material, distanceLike, 0, nullptr, drawSurf,
								ST_PARTICLE, mergeabilityKey, overrideParamsIndex );
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
	// Dynamic mesh lod code is special
	const float cameraViewTangent = stateForCamera->fovLodScale;
	std::optional<std::pair<unsigned, unsigned>> maybeStorageRequirements;
	const unsigned drawFlags = ( stateForCamera->refdef.rdflags & RDF_LOWDETAIL ) ? DynamicMesh::ForceLowDetail : 0;
	maybeStorageRequirements = mesh->getStorageRequirements( stateForCamera->viewOrigin, stateForCamera->viewAxis,
															 cameraViewTangent, stateForCamera->viewLodScale,
															 stateForCamera->cameraId, drawSurface->scratchpad,
															 drawFlags );
	if( maybeStorageRequirements ) [[likely]] {
		const auto [numVertices, numIndices] = *maybeStorageRequirements;
		assert( numVertices && numIndices );
		// TODO: Allow more if we draw using base vertex
		if( offsetsOfVerticesAndIndices->first + numVertices <= MAX_UPLOAD_VBO_VERTICES &&
			offsetsOfVerticesAndIndices->second + numIndices <= MAX_UPLOAD_VBO_INDICES ) {
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
								   const void *drawSurf, unsigned surfType,
								   unsigned mergeabilitySeparator, int overrideParamsIndex )
								   -> std::optional<unsigned> {
	if( shader ) [[likely]] {
		// TODO: This should be moved to an outer loop
		if( !( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) || !Shader_ReadDepth( shader ) ) [[likely]] {
			// TODO: This should be moved to an outer loop
			if( !rsh.worldBrushModel ) [[unlikely]] {
				fog = nullptr;
			}

			if( const unsigned distKey = R_PackDistKey( e->renderfx, shader, dist, order ) ) [[likely]] {
				const int fogNum       = fog ? (int)( fog - rsh.worldBrushModel->fogs ) : -1;
				const int portalNum    = portalSurf ? (int)( portalSurf - stateForCamera->portalSurfaces ) : -1;
				const auto oldListSize = stateForCamera->sortList->size();

				stateForCamera->sortList->emplace_back( sortedDrawSurf_t {
					.sortKey               = R_PackSortKey( shader->id, fogNum, portalNum, e->number, overrideParamsIndex ),
					.drawSurf              = (drawSurfaceType_t *)drawSurf,
					.distKey               = distKey,
					.surfType              = surfType,
					.mergeabilitySeparator = mergeabilitySeparator
				});

				return (unsigned)oldListSize;
			}
		}
	}

	return std::nullopt;
}

void Frontend::processWorldPortalSurfaces( StateForCamera *stateForCamera, Scene *scene, bool isCameraAPortalCamera ) {
	const auto numWorldModelMergedSurfaces       = rsh.worldBrushModel->numModelMergedSurfaces;
	const MergedSurfSpan *const mergedSurfSpans  = stateForCamera->drawSurfSurfSpansBuffer->get();
	drawSurfaceBSP_t *const drawSurfaces         = stateForCamera->bspDrawSurfacesBuffer->get();
	const MergedBspSurface *const mergedSurfaces = rsh.worldBrushModel->mergedSurfaces;
	const msurface_t *const surfaces             = rsh.worldBrushModel->surfaces;

	static_assert( MAX_PORTAL_SURFACES <= 32 );
	[[maybe_unused]] unsigned validPortalSurfacesMask = 0;

	// TODO: Left-pack during calculation of surf subspans
	for( unsigned mergedSurfNum = 0; mergedSurfNum < numWorldModelMergedSurfaces; ++mergedSurfNum ) {
		const MergedSurfSpan &surfSpan = mergedSurfSpans[mergedSurfNum];
		// TODO: Left-pack during calculation of surf subspans
		if( surfSpan.firstSurface <= surfSpan.lastSurface ) {
			const MergedBspSurface &mergedSurf = mergedSurfaces[mergedSurfNum];
			// TODO: Save portal surface in draw surface
			portalSurface_t *portalSurface = nullptr;
			unsigned portalSurfaceIndex    = 0;
			if( mergedSurf.shader->flags & SHADER_PORTAL ) [[unlikely]] {
				if( stateForCamera->numPortalSurfaces < MAX_PORTAL_SURFACES ) {
					// We currently don't support depth-masked portals, only capturing ones
					if( mergedSurf.shader->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) ) {
						portalSurfaceIndex = stateForCamera->numPortalSurfaces;
						stateForCamera->numPortalSurfaces++;
						portalSurface = &stateForCamera->portalSurfaces[portalSurfaceIndex];
						memset( portalSurface, 0, sizeof( portalSurface_t ) );
						portalSurface->entity          = scene->m_worldent;
						portalSurface->shader          = mergedSurf.shader;
						ClearBounds( portalSurface->mins, portalSurface->maxs );
						memset( portalSurface->texures, 0, sizeof( portalSurface->texures ) );
					}
				}
			}
			if( portalSurface ) [[unlikely]] {
				float resultDist                     = 0.0;
				const msurface_t *const firstVisSurf = surfaces + surfSpan.firstSurface;
				const msurface_t *const lastVisSurf  = surfaces + surfSpan.lastSurface;
				for( const msurface_s *surf = firstVisSurf; surf <= lastVisSurf; ++surf ) {
					vec3_t center;
					// For brush models, currently unsupported
					constexpr const float *modelOrigin = nullptr;
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
					// Update result distance regardless of geometry setup, so it gets sorted correctly
					resultDist = wsw::max( resultDist, dist );
					// Mark the portal surface as valid once we have succeeded with update by any mesh in surf range.
					// Note: It looks like that actual portal surfaces are backed by a single surface.
					if( updatePortalSurfaceUsingMesh( stateForCamera, portalSurface, &surf->mesh,
													  surf->mins, surf->maxs, mergedSurf.shader ) ) {
						validPortalSurfacesMask |= ( 1u << portalSurfaceIndex );
					}
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
	if( !isCameraAPortalCamera && !( stateForCamera->refdef.rdflags & RDF_LOWDETAIL ) ) {
		if( stateForCamera->viewCluster >= 0 && !r_fastsky->integer ) {
			for( unsigned i = 0; i < stateForCamera->numPortalSurfaces; ++i ) {
				if( validPortalSurfacesMask & ( 1u << i ) ) {
					prepareDrawingPortalSurface( stateForCamera, scene, &stateForCamera->portalSurfaces[i] );
				}
			}
			if( stateForCamera->refdef.rdflags & RDF_SKYPORTALINVIEW ) {
				prepareDrawingSkyPortal( stateForCamera, scene );
			}
		}
	}
}

bool Frontend::updatePortalSurfaceUsingMesh( StateForCamera *stateForCamera, portalSurface_t *portalSurface, const mesh_t *mesh,
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
			return false;
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
			return false;
		}

		// we need to render the backplane view
	}

	// check if portal view is opaque due to alphagen portal
	if( shader->portalDistance > 0.0f && dist > shader->portalDistance ) {
		return false;
	}

	portalSurface->plane = plane;
	portalSurface->untransformed_plane = untransformed_plane;

	AddPointToBounds( mins, portalSurface->mins, portalSurface->maxs );
	AddPointToBounds( maxs, portalSurface->mins, portalSurface->maxs );
	portalSurface->mins[3] = 0.0f, portalSurface->maxs[3] = 1.0f;

	return true;
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
					// Won't overflow as we won't allocate that much in the first place
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
	newRefdef.rdflags |= RDF_LOWDETAIL;
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

void Frontend::prepareDrawingSkyPortal( StateForCamera *stateForPrimaryCamera, Scene *scene ) {
	refdef_t newRefdef = stateForPrimaryCamera->refdef;
	const skyportal_t *skyportal = &newRefdef.skyportal;

	if( skyportal->scale ) {
		vec3_t center, diff;
		VectorAdd( rsh.worldModel->mins, rsh.worldModel->maxs, center );
		VectorScale( center, 0.5f, center );
		VectorSubtract( center, stateForPrimaryCamera->viewOrigin, diff );
		VectorMA( skyportal->vieworg, -skyportal->scale, diff, newRefdef.vieworg );
	} else {
		VectorCopy( skyportal->vieworg, newRefdef.vieworg );
	}

	// FIXME
	if( !VectorCompare( skyportal->viewanglesOffset, vec3_origin ) ) {
		vec3_t angles;
		mat3_t axis;

		Matrix3_Copy( stateForPrimaryCamera->viewAxis, axis );
		VectorInverse( &axis[AXIS_RIGHT] );
		Matrix3_ToAngles( axis, angles );

		VectorAdd( angles, skyportal->viewanglesOffset, angles );
		AnglesToAxis( angles, axis );
		Matrix3_Copy( axis, newRefdef.viewaxis );
	}

	if( skyportal->fov ) {
		newRefdef.fov_x = skyportal->fov;
		newRefdef.fov_y = CalcFov( skyportal->fov, newRefdef.width, newRefdef.height );
		AdjustFov( &newRefdef.fov_x, &newRefdef.fov_y, glConfig.width, glConfig.height, false );
	}

	auto *stateForSkyPortalCamera = setupStateForCamera( &newRefdef, stateForPrimaryCamera->sceneIndex, CameraOverrideParams {
		.pvsOrigin          = skyportal->vieworg,
		.lodOrigin          = skyportal->vieworg,
		.renderFlagsToAdd   = (unsigned)( RF_PORTALVIEW | RF_NOOCCLUSIONCULLING | ( skyportal->noEnts ? RF_ENVVIEW : 0 ) ),
		.renderFlagsToClear = ( RDF_UNDERWATER | RDF_CROSSINGWATER | RDF_SKYPORTALINVIEW ),
	});

	if( stateForSkyPortalCamera ) {
		// Won't overflow as we won't allocate that much in the first place
		stateForPrimaryCamera->portalCameraStates.push_back( stateForSkyPortalCamera );
		stateForPrimaryCamera->stateForSkyPortalCamera = stateForSkyPortalCamera;
	}
}

}