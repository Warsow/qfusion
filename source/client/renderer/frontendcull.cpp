/*
Copyright (C) 2007 Victor Luchits
Copyright (C) 2021 Chasseur de bots

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
#include <common/facilities/profilerscope.h>
#include <common/helpers/sortbyfield.h>
#include <common/helpers/algorithm.h>

#ifdef DEBUG_OCCLUDERS
// See frontendcull.inc
bool g_isLockingOccluders = false;
vec3_t g_lockedViewOrigin;
vec3_t g_lockedViewAxis;
int g_lockedOccluderNum;
#endif

void Frustum::setPlaneComponentsAtIndex( unsigned index, const float *n, float d ) {
	const uint32_t blendsForSign[2] { 0, ~( (uint32_t)0 ) };

	const float nX = planeX[index] = n[0];
	const float nY = planeY[index] = n[1];
	const float nZ = planeZ[index] = n[2];

	planeD[index] = d;

	xBlendMasks[index] = blendsForSign[nX < 0];
	yBlendMasks[index] = blendsForSign[nY < 0];
	zBlendMasks[index] = blendsForSign[nZ < 0];
}

void Frustum::fillComponentTails( unsigned indexOfPlaneToReplicate ) {
	// Sanity check
	assert( indexOfPlaneToReplicate >= 3 && indexOfPlaneToReplicate < 8 );
	for( unsigned i = indexOfPlaneToReplicate; i < 8; ++i ) {
		planeX[i] = planeX[indexOfPlaneToReplicate];
		planeY[i] = planeY[indexOfPlaneToReplicate];
		planeZ[i] = planeZ[indexOfPlaneToReplicate];
		planeD[i] = planeD[indexOfPlaneToReplicate];
		xBlendMasks[i] = xBlendMasks[indexOfPlaneToReplicate];
		yBlendMasks[i] = yBlendMasks[indexOfPlaneToReplicate];
		zBlendMasks[i] = zBlendMasks[indexOfPlaneToReplicate];
	}
}

void Frustum::setupFor4Planes( const float *viewOrigin, const mat3_t viewAxis, float fovX, float fovY ) {
	const float *const forward = &viewAxis[AXIS_FORWARD];
	const float *const left    = &viewAxis[AXIS_RIGHT];
	const float *const up      = &viewAxis[AXIS_UP];

	const vec3_t right { -left[0], -left[1], -left[2] };

	const float xRotationAngle = 90.0f - 0.5f * fovX;
	const float yRotationAngle = 90.0f - 0.5f * fovY;

	vec3_t planeNormals[4];
	RotatePointAroundVector( planeNormals[0], up, forward, -xRotationAngle );
	RotatePointAroundVector( planeNormals[1], up, forward, +xRotationAngle );
	RotatePointAroundVector( planeNormals[2], right, forward, +yRotationAngle );
	RotatePointAroundVector( planeNormals[3], right, forward, -yRotationAngle );

	for( unsigned i = 0; i < 4; ++i ) {
		setPlaneComponentsAtIndex( i, planeNormals[i], DotProduct( viewOrigin, planeNormals[i] ) );
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

namespace wsw {

void RendererFrontend::collectVisiblePolys( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta ) {
	VisTestedModel *tmpModels = stateForCamera->visTestedModelsBuffer->reserveAndGet( MAX_QUAD_POLYS );
	QuadPoly **quadPolys      = scene->m_quadPolys.data();

	uint16_t tmpIndices[MAX_QUAD_POLYS];
	const auto visibleIndices = cullQuadPolys( stateForCamera, quadPolys, scene->m_quadPolys.size(), frusta, tmpIndices, tmpModels );

	auto *const shaderParamsStorage   = stateForCamera->shaderParamsStorage;
	auto *const materialParamsStorage = stateForCamera->materialParamsStorage;

	const auto *polyEntity = scene->m_polyent;
	for( const unsigned index: visibleIndices ) {
		QuadPoly *const p = quadPolys[index];
		unsigned mergeabilitySeparator = 0;
		int overrideParamsIndex        = -1;
		assert( p->animFrac >= 0.0f && p->animFrac <= 1.0f );
		if( p->material->flags & SHADER_ANIM_FRAC ) {
			overrideParamsIndex = (int)shaderParamsStorage->size();
			shaderParamsStorage->emplace_back( ShaderParams {
				.materialComponentIndex = (int)materialParamsStorage->size(),
			});
			materialParamsStorage->emplace_back( ShaderParams::Material {
				.shaderFrac = p->animFrac,
			});
			mergeabilitySeparator = 1 + (unsigned)overrideParamsIndex;
		}
		addEntryToSortList( stateForCamera, polyEntity, nullptr, p->material, 0, index, nullptr, quadPolys[index],
							ST_QUAD_POLY, mergeabilitySeparator, overrideParamsIndex );
	}
}

void RendererFrontend::collectVisibleEntities( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta ) {
	uint16_t indices[MAX_ENTITIES], indices2[MAX_ENTITIES];

	VisTestedModel *const visModels = stateForCamera->visTestedModelsBuffer->reserveAndGet( MAX_ENTITIES );

	const std::span<const entity_t> nullModelEntities = scene->m_nullModelEntities;
	const auto nullModelIndices = cullNullModelEntities( stateForCamera, nullModelEntities, frusta, indices, visModels );
	addNullModelEntitiesToSortList( stateForCamera, nullModelEntities.data(), nullModelIndices );

	const std::span<const entity_t> aliasModelEntities = scene->m_aliasModelEntities;
	const auto aliasModelIndices = cullAliasModelEntities( stateForCamera, aliasModelEntities, frusta, indices, visModels );
	addAliasModelEntitiesToSortList( stateForCamera,  aliasModelEntities.data(),
									 { visModels, aliasModelIndices.size() }, aliasModelIndices );

	const std::span<const entity_t> skeletalModelEntities = scene->m_skeletalModelEntities;
	const auto skeletalModelIndices = cullSkeletalModelEntities( stateForCamera, skeletalModelEntities, frusta, indices, visModels );
	addSkeletalModelEntitiesToSortList( stateForCamera, skeletalModelEntities.data(),
										{ visModels, skeletalModelIndices.size() }, skeletalModelIndices );

	const std::span<const entity_t> brushModelEntities = scene->m_brushModelEntities;
	const auto brushModelIndices = cullBrushModelEntities( stateForCamera, brushModelEntities, frusta, indices, visModels );
	const std::span<const Scene::DynamicLight> dynamicLights { scene->m_dynamicLights.data(), scene->m_dynamicLights.size() };
	const std::span<const VisTestedModel> brushVisModels { visModels, brushModelIndices.size() };
	addBrushModelEntitiesToSortList( stateForCamera, brushModelEntities.data(), brushVisModels, brushModelIndices, dynamicLights );

	const std::span<const entity_t> spriteEntities = scene->m_spriteEntities;
	const auto spriteModelIndices = cullSpriteEntities( stateForCamera, spriteEntities, frusta, indices, indices2, visModels );
	addSpriteEntitiesToSortList( stateForCamera, spriteEntities.data(), spriteModelIndices );
}

void RendererFrontend::collectVisibleParticles( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta ) {
	uint16_t tmpIndices[1024];
	const auto visibleAggregateIndices = cullParticleAggregates( stateForCamera, scene->m_particles, frusta, tmpIndices );
	addParticlesToSortList( stateForCamera, scene->m_polyent, scene->m_particles.data(), visibleAggregateIndices );
}

void RendererFrontend::collectVisibleDynamicMeshes( StateForCamera *stateForCamera,  Scene *scene,
													std::span<const Frustum> occluderFrusta,
													std::pair<unsigned, unsigned> *countersOfVerticesAndIndices ) {
	uint16_t tmpIndices[wsw::max( Scene::kMaxDynamicMeshes, Scene::kMaxCompoundDynamicMeshes )];

	const std::span<const DynamicMesh *> meshes = scene->m_dynamicMeshes;
	const auto visibleDynamicMeshIndices = cullDynamicMeshes( stateForCamera, meshes.data(),
															  meshes.size(), occluderFrusta, tmpIndices );
	addDynamicMeshesToSortList( stateForCamera, scene->m_polyent, scene->m_dynamicMeshes.data(),
								visibleDynamicMeshIndices, countersOfVerticesAndIndices );

	const std::span<const Scene::CompoundDynamicMesh> compoundMeshes = scene->m_compoundDynamicMeshes;
	const auto visibleCompoundMeshesIndices = cullCompoundDynamicMeshes( stateForCamera, compoundMeshes,
																		 occluderFrusta, tmpIndices );
	addCompoundDynamicMeshesToSortList( stateForCamera, scene->m_polyent, scene->m_compoundDynamicMeshes.data(),
										visibleCompoundMeshesIndices, countersOfVerticesAndIndices );
}

auto RendererFrontend::collectVisibleLights( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> occluderFrusta )
	-> std::pair<std::span<const uint16_t>, std::span<const uint16_t>> {
	static_assert( decltype( Scene::m_dynamicLights )::capacity() == kMaxLightsInScene );

	const auto [allVisibleLightIndices, visibleCoronaLightIndices, visibleProgramLightIndices] =
		cullLights( stateForCamera, scene->m_dynamicLights, occluderFrusta,
					stateForCamera->allVisibleLightIndices, stateForCamera->visibleCoronaLightIndices,
					stateForCamera->visibleProgramLightIndices );

	assert( stateForCamera->numVisibleProgramLights == 0 );
	assert( stateForCamera->numAllVisibleLights == 0 );

	stateForCamera->numAllVisibleLights     = allVisibleLightIndices.size();
	stateForCamera->numVisibleProgramLights = visibleProgramLightIndices.size();

	// Prune m_visibleProgramLightIndices in-place
	if( visibleProgramLightIndices.size() > kMaxProgramLightsInView ) {
		const float *const __restrict viewOrigin = stateForCamera->viewOrigin;
		const Scene::DynamicLight *const lights  = scene->m_dynamicLights.data();
		wsw::StaticVector<std::pair<unsigned, float>, kMaxLightsInScene> lightsHeap;
		const auto cmp = []( const std::pair<unsigned, float> &lhs, const std::pair<unsigned, float> &rhs ) {
			return lhs.second > rhs.second;
		};
		for( const unsigned index: visibleProgramLightIndices ) {
			const Scene::DynamicLight *light = &lights[index];
			const float squareDistance = DistanceSquared( light->origin, viewOrigin );
			const float score = light->programRadius * Q_RSqrt( squareDistance );
			lightsHeap.emplace_back( { index, score } );
			wsw::push_heap( lightsHeap.begin(), lightsHeap.end(), cmp );
		}
		unsigned numVisibleProgramLights = 0;
		do {
			wsw::pop_heap( lightsHeap.begin(), lightsHeap.end(), cmp );
			const unsigned lightIndex = lightsHeap.back().first;
			stateForCamera->visibleProgramLightIndices[numVisibleProgramLights++] = lightIndex;
			lightsHeap.pop_back();
		} while( numVisibleProgramLights < kMaxProgramLightsInView );
		stateForCamera->numVisibleProgramLights = numVisibleProgramLights;
	}

	for( unsigned i = 0; i < stateForCamera->numVisibleProgramLights; ++i ) {
		Scene::DynamicLight *const light = scene->m_dynamicLights.data() + stateForCamera->visibleProgramLightIndices[i];
		float *const mins = stateForCamera->lightBoundingDops[i].mins;
		float *const maxs = stateForCamera->lightBoundingDops[i].maxs;
		createBounding14DopForSphere( mins, maxs, light->origin, light->programRadius );
	}

	return { { stateForCamera->visibleProgramLightIndices, stateForCamera->numVisibleProgramLights }, visibleCoronaLightIndices };
}

void RendererFrontend::markSurfacesOfLeavesAsVisible( std::span<const unsigned> indicesOfLeaves,
													  MergedSurfSpan *mergedSurfSpans, uint8_t *surfVisTable ) {
	const auto surfaces = rsh.worldBrushModel->surfaces;
	const auto leaves   = rsh.worldBrushModel->visleafs;
	for( const unsigned leafNum: indicesOfLeaves ) {
		const auto *__restrict leaf = leaves[leafNum];
		for( unsigned i = 0; i < leaf->numVisSurfaces; ++i ) {
			const unsigned surfNum = leaf->visSurfaces[i];
			assert( surfaces[surfNum].mergedSurfNum > 0 );
			surfVisTable[surfNum] = 1;
			const unsigned mergedSurfNum = surfaces[surfNum].mergedSurfNum - 1;
			MergedSurfSpan *const __restrict span = &mergedSurfSpans[mergedSurfNum];
			// TODO: Branchless min/max
			span->firstSurface = wsw::min( span->firstSurface, (int)surfNum );
			span->lastSurface = wsw::max( span->lastSurface, (int)surfNum );
		}
	}
}

void RendererFrontend::markLightsOfSurfaces( StateForCamera *stateForCamera, const Scene *scene,
											 std::span<std::span<const unsigned>> spansOfLeaves,
											 std::span<const uint16_t> visibleLightIndices ) {
	unsigned *const lightBitsOfSurfaces = stateForCamera->leafLightBitsOfSurfacesBuffer
		->reserveZeroedAndGet( rsh.worldBrushModel->numsurfaces );

	if( !visibleLightIndices.empty() ) {
		for( const std::span<const unsigned> &leaves: spansOfLeaves ) {
			markLightsOfLeaves( stateForCamera, scene, leaves, visibleLightIndices, lightBitsOfSurfaces );
		}
	}
}

void RendererFrontend::markLightsOfLeaves( StateForCamera *stateForCamera, const Scene *scene,
										   std::span<const unsigned> indicesOfLeaves,
										   std::span<const uint16_t> visibleLightIndices,
										   unsigned *leafLightBitsOfSurfaces ) {
	const unsigned numVisibleLights = visibleLightIndices.size();
	const auto leaves               = rsh.worldBrushModel->visleafs;

	assert( numVisibleLights && numVisibleLights <= kMaxProgramLightsInView );

	for( const unsigned leafIndex: indicesOfLeaves ) {
		const mleaf_t *const __restrict leaf = leaves[leafIndex];
		unsigned leafLightBits = 0;

		unsigned programLightNum = 0;
		do {
			const auto *__restrict lightDop = &stateForCamera->lightBoundingDops[programLightNum];
			if( doOverlapTestFor14Dops( lightDop->mins, lightDop->maxs, leaf->mins, leaf->maxs ) ) {
				leafLightBits |= ( 1u << programLightNum );
			}
		} while( ++programLightNum < numVisibleLights );

		if( leafLightBits ) [[unlikely]] {
			const unsigned *const leafSurfaceNums = leaf->visSurfaces;
			const unsigned numLeafSurfaces = leaf->numVisSurfaces;
			assert( numLeafSurfaces );
			unsigned surfIndex = 0;
			do {
				const unsigned surfNum = leafSurfaceNums[surfIndex];
				leafLightBitsOfSurfaces[surfNum] |= leafLightBits;
			} while( ++surfIndex < numLeafSurfaces );
		}
	}
}

auto RendererFrontend::cullNullModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> entitiesSpan,
											  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices,
											  VisTestedModel *tmpModels ) -> std::span<const uint16_t> {
	const auto *const entities  = entitiesSpan.data();
	const unsigned numEntities  = entitiesSpan.size();

	for( unsigned entIndex = 0; entIndex < numEntities; ++entIndex ) {
		const entity_t *const __restrict entity = &entities[entIndex];
		VisTestedModel *const __restrict model  = &tmpModels[entIndex];
		Vector4Set( model->absMins, -16, -16, -16, -0 );
		Vector4Set( model->absMaxs, +16, +16, +16, +1 );
		VectorAdd( model->absMins, entity->origin, model->absMins );
		VectorAdd( model->absMaxs, entity->origin, model->absMaxs );
	}

	// The number of bounds has an exact match with the number of entities in this case
	return cullEntriesWithBounds( stateForCamera, tmpModels, numEntities, offsetof( VisTestedModel, absMins ),
								  sizeof( VisTestedModel ), &stateForCamera->frustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullAliasModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> entitiesSpan,
											   std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
											   VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t> {
	const auto *const entities = entitiesSpan.data();
	const unsigned numEntities = entitiesSpan.size();

	unsigned weaponModelFlagEnts[16];
	const model_t *weaponModelFlagLods[16];
	unsigned numWeaponModelFlagEnts = 0;

	unsigned numSelectedModels = 0;
	for( unsigned entIndex = 0; entIndex < numEntities; ++entIndex ) {
		const entity_t *const __restrict entity = &entities[entIndex];
		if( entity->flags & RF_VIEWERMODEL ) [[unlikely]] {
			if( !( stateForCamera->renderFlags & ( RF_MIRRORVIEW | RF_SHADOWMAPVIEW ) ) ) {
				continue;
			}
		}
		if( entity->flags & RF_WEAPONMODEL ) [[unlikely]] {
			if( stateForCamera->renderFlags & RF_NONVIEWERREF ) {
				continue;
			}
		}

		const model_t *mod = R_AliasModelLOD( entity, stateForCamera->lodOrigin, stateForCamera->fovLodScale,
											  stateForCamera->viewLodScale );
		const auto *aliasmodel = ( const maliasmodel_t * )mod->extradata;
		// TODO: Could this ever happen
		if( !aliasmodel ) [[unlikely]] {
			continue;
		}
		if( !aliasmodel->nummeshes ) [[unlikely]] {
			continue;
		}

		// Don't let it to be culled away
		// TODO: Keep it separate from other models?
		if( entity->flags & RF_WEAPONMODEL ) [[unlikely]] {
			assert( numWeaponModelFlagEnts < std::size( weaponModelFlagEnts ) );
			weaponModelFlagEnts[numWeaponModelFlagEnts] = entIndex;
			weaponModelFlagLods[numWeaponModelFlagEnts] = mod;
			numWeaponModelFlagEnts++;
			continue;
		}

		VisTestedModel *__restrict visTestedModel = &selectedModelsBuffer[numSelectedModels++];
		visTestedModel->selectedLod               = mod;
		visTestedModel->indexInEntitiesGroup      = entIndex;

		R_AliasModelLerpBBox( entity, mod, visTestedModel->absMins, visTestedModel->absMaxs );
		VectorAdd( visTestedModel->absMins, entity->origin, visTestedModel->absMins );
		VectorAdd( visTestedModel->absMaxs, entity->origin, visTestedModel->absMaxs );
		visTestedModel->absMins[3] = 0.0f, visTestedModel->absMaxs[3] = 1.0f;
	}

	const size_t numPassedOtherEnts = cullEntriesWithBounds( stateForCamera, selectedModelsBuffer, numSelectedModels,
															 offsetof( VisTestedModel, absMins ), sizeof( VisTestedModel ),
															 &stateForCamera->frustum, occluderFrusta, tmpIndicesBuffer ).size();

	for( unsigned i = 0; i < numWeaponModelFlagEnts; ++i ) {
		VisTestedModel *__restrict visTestedModel = &selectedModelsBuffer[numSelectedModels];
		visTestedModel->selectedLod               = weaponModelFlagLods[i];
		visTestedModel->indexInEntitiesGroup      = weaponModelFlagEnts[i];

		const entity_t *const __restrict entity = &entities[visTestedModel->indexInEntitiesGroup];

		// Just for consistency?
		R_AliasModelLerpBBox( entity, visTestedModel->selectedLod, visTestedModel->absMins, visTestedModel->absMaxs );
		VectorAdd( visTestedModel->absMins, entity->origin, visTestedModel->absMins );
		VectorAdd( visTestedModel->absMaxs, entity->origin, visTestedModel->absMaxs );
		visTestedModel->absMins[3] = 0.0f, visTestedModel->absMaxs[3] = 1.0f;

		tmpIndicesBuffer[numPassedOtherEnts + i] = numSelectedModels;
		numSelectedModels++;
	}

	return { tmpIndicesBuffer, numPassedOtherEnts + numWeaponModelFlagEnts };
}

auto RendererFrontend::cullSkeletalModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> entitiesSpan,
												  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
												  VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t> {
	const auto *const entities = entitiesSpan.data();
	const unsigned numEntities = entitiesSpan.size();

	unsigned numSelectedModels = 0;
	for( unsigned entIndex = 0; entIndex < numEntities; entIndex++ ) {
		const entity_t *const __restrict entity = &entities[entIndex];
		if( entity->flags & RF_VIEWERMODEL ) [[unlikely]] {
			if( !( stateForCamera->renderFlags & (RF_MIRRORVIEW | RF_SHADOWMAPVIEW ) ) ) {
				continue;
			}
		}

		const model_t *mod = R_SkeletalModelLOD( entity, stateForCamera->lodOrigin, stateForCamera->fovLodScale,
												 stateForCamera->viewLodScale );
		const mskmodel_t *skmodel = ( const mskmodel_t * )mod->extradata;
		if( !skmodel ) [[unlikely]] {
			continue;
		}
		if( !skmodel->nummeshes ) [[unlikely]] {
			continue;
		}

		VisTestedModel *__restrict visTestedModel = &selectedModelsBuffer[numSelectedModels++];
		visTestedModel->selectedLod               = mod;
		visTestedModel->indexInEntitiesGroup      = entIndex;

		R_SkeletalModelLerpBBox( entity, mod, visTestedModel->absMins, visTestedModel->absMaxs );
		VectorAdd( visTestedModel->absMins, entity->origin, visTestedModel->absMins );
		VectorAdd( visTestedModel->absMaxs, entity->origin, visTestedModel->absMaxs );
		visTestedModel->absMins[3] = 0.0f, visTestedModel->absMaxs[3] = 1.0f;
	}

	return cullEntriesWithBounds( stateForCamera, selectedModelsBuffer, numSelectedModels, offsetof( VisTestedModel, absMins ),
								  sizeof( VisTestedModel ), &stateForCamera->frustum, occluderFrusta, tmpIndicesBuffer );
}

auto RendererFrontend::cullBrushModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> entitiesSpan,
											   std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
											   VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t> {
	const auto *const entities = entitiesSpan.data();
	const unsigned numEntities = entitiesSpan.size();

	unsigned numSelectedModels = 0;
	for( unsigned entIndex = 0; entIndex < numEntities; ++entIndex ) {
		const entity_t *const __restrict entity = &entities[entIndex];
		const model_t *const model              = entity->model;
		const auto *const brushModel            = ( mbrushmodel_t * )model->extradata;
		if( !brushModel->numModelMergedSurfaces ) [[unlikely]] {
			continue;
		}

		VisTestedModel *__restrict visTestedModel = &selectedModelsBuffer[numSelectedModels++];
		visTestedModel->selectedLod               = model;
		visTestedModel->indexInEntitiesGroup      = entIndex;

		// Returns absolute bounds
		R_BrushModelBBox( entity, visTestedModel->absMins, visTestedModel->absMaxs );
		visTestedModel->absMins[3] = 0.0f, visTestedModel->absMaxs[3] = 1.0f;
	}

	return cullEntriesWithBounds( stateForCamera, selectedModelsBuffer, numSelectedModels, offsetof( VisTestedModel, absMins ),
								  sizeof( VisTestedModel ), &stateForCamera->frustum, occluderFrusta, tmpIndicesBuffer );
}

auto RendererFrontend::cullSpriteEntities( StateForCamera *stateForCamera, std::span<const entity_t> entitiesSpan,
										   std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices,
										   uint16_t *tmpIndices2, VisTestedModel *tmpModels ) -> std::span<const uint16_t> {
	const auto *const entities = entitiesSpan.data();
	const unsigned numEntities = entitiesSpan.size();

	unsigned numResultEntities    = 0;
	unsigned numVisTestedEntities = 0;
	for( unsigned entIndex = 0; entIndex < numEntities; ++entIndex ) {
		const entity_t *const __restrict entity = &entities[entIndex];
		// TODO: This condition should be eliminated from this path
		if( entity->flags & RF_NOSHADOW ) [[unlikely]] {
			if( stateForCamera->renderFlags & RF_SHADOWMAPVIEW ) [[unlikely]] {
				continue;
			}
		}

		if( entity->radius <= 0 || entity->customShader == nullptr || entity->scale <= 0 ) [[unlikely]] {
			continue;
		}

		// Hacks for ET_RADAR indicators
		if( entity->flags & RF_NODEPTHTEST ) [[unlikely]] {
			tmpIndices[numResultEntities++] = entIndex;
			continue;
		}

		VisTestedModel *__restrict visTestedModel = &tmpModels[numVisTestedEntities++];
		visTestedModel->indexInEntitiesGroup      = entIndex;

		const float *origin = entity->origin;
		const float halfRadius = 0.5f * entity->radius;

		Vector4Set( visTestedModel->absMins, origin[0] - halfRadius, origin[1] - halfRadius, origin[2] - halfRadius, 0.0f );
		Vector4Set( visTestedModel->absMaxs, origin[0] + halfRadius, origin[1] + halfRadius, origin[2] + halfRadius, 1.0f );
	}

	const auto passedTestIndices = cullEntriesWithBounds( stateForCamera, tmpModels, numVisTestedEntities,
														  offsetof( VisTestedModel, absMins ), sizeof( VisTestedModel ),
														  &stateForCamera->frustum, occluderFrusta, tmpIndices2 );

	for( const auto testedModelIndex: passedTestIndices ) {
		tmpIndices[numResultEntities++] = tmpModels[testedModelIndex].indexInEntitiesGroup;
	}

	return { tmpIndices, numResultEntities };
}

auto RendererFrontend::cullLights( StateForCamera *stateForCamera,
								   std::span<const Scene::DynamicLight> lightsSpan,
								   std::span<const Frustum> occluderFrusta,
								   uint16_t *tmpAllLightIndices,
								   uint16_t *tmpCoronaLightIndices,
								   uint16_t *tmpProgramLightIndices )
	-> std::tuple<std::span<const uint16_t>, std::span<const uint16_t>, std::span<const uint16_t>> {

	const auto *const lights = lightsSpan.data();
	const unsigned numLights = lightsSpan.size();

	static_assert( offsetof( Scene::DynamicLight, mins ) + 4 * sizeof( float ) == offsetof( Scene::DynamicLight, maxs ) );
	const auto visibleAllLightIndices = cullEntriesWithBounds( stateForCamera, lights, numLights,
															   offsetof( Scene::DynamicLight, mins ),
															   sizeof( Scene::DynamicLight ), &stateForCamera->frustum,
															   occluderFrusta, tmpAllLightIndices );

	unsigned numPassedCoronaLights  = 0;
	unsigned numPassedProgramLights = 0;

	for( const auto index: visibleAllLightIndices ) {
		const Scene::DynamicLight *light               = &lights[index];
		tmpCoronaLightIndices[numPassedCoronaLights]   = index;
		tmpProgramLightIndices[numPassedProgramLights] = index;
		numPassedCoronaLights  += light->hasCoronaLight;
		numPassedProgramLights += light->hasProgramLight;
	}

	return { visibleAllLightIndices,
			 { tmpCoronaLightIndices,  numPassedCoronaLights },
			 { tmpProgramLightIndices, numPassedProgramLights } };
}

auto RendererFrontend::cullParticleAggregates( StateForCamera *stateForCamera,
											   std::span<const Scene::ParticlesAggregate> aggregatesSpan,
											   std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
   -> std::span<const uint16_t> {
	static_assert( offsetof( Scene::ParticlesAggregate, mins ) + 16 == offsetof( Scene::ParticlesAggregate, maxs ) );
	return cullEntriesWithBounds( stateForCamera, aggregatesSpan.data(), aggregatesSpan.size(),
								  offsetof( Scene::ParticlesAggregate, mins ), sizeof( Scene::ParticlesAggregate ),
								  &stateForCamera->frustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullCompoundDynamicMeshes( StateForCamera *stateForCamera,
												  std::span<const Scene::CompoundDynamicMesh> meshesSpan,
												  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	static_assert( offsetof( Scene::CompoundDynamicMesh, cullMins ) + 16 == offsetof( Scene::CompoundDynamicMesh, cullMaxs ) );
	return cullEntriesWithBounds( stateForCamera, meshesSpan.data(), meshesSpan.size(),
								  offsetof( Scene::CompoundDynamicMesh, cullMins ), sizeof( Scene::CompoundDynamicMesh ),
								  &stateForCamera->frustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullQuadPolys( StateForCamera *stateForCamera, QuadPoly **polys, unsigned numPolys,
							  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices,
							  VisTestedModel *tmpModels ) -> std::span<const uint16_t> {
	for( unsigned polyNum = 0; polyNum < numPolys; ++polyNum ) {
		const QuadPoly *const __restrict poly  = polys[polyNum];
		VisTestedModel *const __restrict model = &tmpModels[polyNum];

		Vector4Set( model->absMins, -poly->halfExtent, -poly->halfExtent, -poly->halfExtent, -0 );
		Vector4Set( model->absMaxs, +poly->halfExtent, +poly->halfExtent, +poly->halfExtent, +1 );
		VectorAdd( model->absMins, poly->origin, model->absMins );
		VectorAdd( model->absMaxs, poly->origin, model->absMaxs );
	}

	return cullEntriesWithBounds( stateForCamera, tmpModels, numPolys, offsetof( VisTestedModel, absMins ),
								  sizeof( VisTestedModel ), &stateForCamera->frustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullDynamicMeshes( StateForCamera *stateForCamera,
										  const DynamicMesh **meshes, unsigned numMeshes,
										  std::span<const Frustum> occluderFrusta,
										  uint16_t *tmpIndices ) -> std::span<const uint16_t> {
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
#endif
	constexpr unsigned boundsFieldOffset = offsetof( DynamicMesh, cullMins );
	static_assert( offsetof( DynamicMesh, cullMins ) + 4 * sizeof( float ) == offsetof( DynamicMesh, cullMaxs ) );
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

	return cullEntryPtrsWithBounds( stateForCamera, (const void **)meshes, numMeshes, boundsFieldOffset,
									&stateForCamera->frustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::collectVisibleWorldLeaves( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	WSW_PROFILER_SCOPE();
	return ( this->*m_collectVisibleWorldLeavesArchMethod )( stateForCamera );
}

auto RendererFrontend::collectVisibleOccluders( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	WSW_PROFILER_SCOPE();
	return ( this->*m_collectVisibleOccludersArchMethod )( stateForCamera );
}

auto RendererFrontend::buildFrustaOfOccluders( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
	-> std::span<const Frustum> {
	WSW_PROFILER_SCOPE();
	return ( this->*m_buildFrustaOfOccludersArchMethod )( stateForCamera, sortedOccluders );
}

void RendererFrontend::buildBitMasksOfLeafOccluders( StateForCamera *stateForCamera, std::span<const unsigned> indicesOfLeaves,
													 std::span<const Frustum> occluderFrusta, uint64_t *bitMaskOfLeafOccluders ) {
	WSW_PROFILER_SCOPE();
	return ( this->*m_buildBitMasksOfLeafOccludersMethod )( stateForCamera, indicesOfLeaves,
															occluderFrusta, bitMaskOfLeafOccluders );
}


void RendererFrontend::cullSurfacesByOccluders( StateForCamera *stateForCamera, std::span<const unsigned> indicesOfSurfaces,
												std::span<const Frustum> occluderFrusta, const uint64_t *surfBitMaskTable,
												MergedSurfSpan *mergedSurfSpans, uint8_t *surfVisTable ) {
	WSW_PROFILER_SCOPE();
	return ( this->*m_cullSurfacesByOccludersArchMethod )( stateForCamera, indicesOfSurfaces, occluderFrusta,
														   surfBitMaskTable, mergedSurfSpans, surfVisTable );
}

auto RendererFrontend::cullEntriesWithBounds( StateForCamera *stateForCamera, const void *entries,
											  unsigned numEntries, unsigned boundsFieldOffset,
											  unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
											  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	WSW_PROFILER_SCOPE();
	return ( this->*m_cullEntriesWithBoundsArchMethod )( stateForCamera, entries, numEntries, boundsFieldOffset,
														 strideInBytes, primaryFrustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullEntryPtrsWithBounds( StateForCamera *stateForCamera, const void **entryPtrs,
												unsigned numEntries, unsigned boundsFieldOffset,
												const Frustum *__restrict primaryFrustum,
												std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	WSW_PROFILER_SCOPE();
	return ( this->*m_cullEntryPtrsWithBoundsArchMethod )( stateForCamera, entryPtrs, numEntries, boundsFieldOffset,
														   primaryFrustum, occluderFrusta, tmpIndices );
}

[[nodiscard]]
static auto calcOccluderAreaScore( const OccluderDataEntry &occluder, const float *mvpMatrix ) -> float;

auto RendererFrontend::calcOccluderScores( StateForCamera *stateForCamera, std::span<const unsigned> visibleOccluders ) -> TaskHandle {
	SortedOccluder *const sortedOccluders = stateForCamera->sortedOccludersBuffer->get();
	const auto *const occluderEntries     = rsh.worldBrushModel->occluderDataEntries;
	const float *const mvpMatrix          = stateForCamera->cameraProjectionMatrix;
	auto fn = [=]( unsigned, unsigned startIndex, unsigned endIndex ) {
		assert( startIndex < endIndex );
		unsigned index = startIndex;
		do {
			const unsigned occluderNum = visibleOccluders[index];
			sortedOccluders[index] = { occluderNum, calcOccluderAreaScore( occluderEntries[occluderNum], mvpMatrix ) };
		} while( ++index < endIndex );
	};
	return m_taskSystem.addForSubrangesInRange( { 0u, (unsigned)visibleOccluders.size() }, 16, {}, std::move( fn ) );
}

auto RendererFrontend::pruneAndSortOccludersByScores( StateForCamera *stateForCamera,
													  std::span<const unsigned> visibleOccluders )
	-> std::span<const SortedOccluder> {
	// Note: The area units are in NDC, also we don't divide cross products by 2 to get triangle areas,
	// hence the area of the entire screen appears to be equal to len([-1,+1]) * len([-1,+1]) * 2 = 8
#if 1
	const float screenWidthFrac  = (float)stateForCamera->refdef.width * Q_Rcp( (float)glConfig.width );
	const float screenHeightFrac = (float)stateForCamera->refdef.height * Q_Rcp( (float)glConfig.height );
	const float screenDimFrac    = wsw::max( screenWidthFrac, screenHeightFrac );
	const float epsilon          = 2e-2f * Q_Rcp( screenDimFrac );
	assert( std::isfinite( epsilon ) && epsilon > 0.0f );
#else
	const float epsilon = Cvar_Value( "epsilon" );
#endif

	// Prune non-feasible occluders prior to sorting
	// TODO: Experiment with doing it afterwards using binary search?
	SortedOccluder *const sortedOccluders = stateForCamera->sortedOccludersBuffer->get();
	unsigned numSortedOccluders           = 0;
	for( unsigned index = 0; index < visibleOccluders.size(); ++index ) {
		if( sortedOccluders[index].score > epsilon ) [[likely]] {
			sortedOccluders[numSortedOccluders++] = sortedOccluders[index];
		}
	}

	// TODO: Don't sort, build a heap instead?
	wsw::sortByFieldDescending( sortedOccluders, sortedOccluders + numSortedOccluders, &SortedOccluder::score );

#ifdef SHOW_OCCLUDERS
	const auto *const occluderEntries = rsh.worldBrushModel->occluderDataEntries;
	for( unsigned i = 0; i < numSortedOccluders; ++i ) {
		const OccluderDataEntry &occluder = occluderEntries[sortedOccluders[i].occluderNum];

		for( unsigned vertIndex = 0; vertIndex < occluder.numVertices; ++vertIndex ) {
			const float *const v1 = occluder.data[vertIndex + 0];
			const float *const v2 = occluder.data[( vertIndex + 1 != occluder.numVertices ) ? vertIndex + 1 : 0];
			const float frac = Q_Sqrt( (float)( i + 1 ) * Q_Rcp( (float)numSortedOccluders ) );
			addDebugLine( stateForCamera, v1, v2, COLOR_RGB( (int)( 255 * frac ), (int)( 255 * ( 1.0f - frac ) ), 0 ) );
		}
	}
#endif

	return { sortedOccluders, sortedOccluders + numSortedOccluders };
}

// This code is borrowed from https://github.com/zauonlok/renderer and is modified for our purposes

/*
MIT License

Copyright (c) 2020 Zhou Le

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

struct WPlaneTraits {
	static constexpr float kEpsilon = 1e-5;
	static wsw_forceinline bool isInside( const vec4_t point ) {
		return point[3] >= kEpsilon;
	}
	static wsw_forceinline auto getIntersectionFrac( const vec4_t prev, const vec4_t curr ) -> float {
		return ( prev[3] - kEpsilon ) * Q_Rcp( prev[3] - curr[3] );
	}
};

struct XyzPlaneTraits {
	unsigned coord;
	float sign;
	wsw_forceinline bool isInside( const vec4_t point ) {
		// P: return point[coord] <= +point[3];
		// N: return point[coord] >= -point[3];
		// N: return -point[coord] < point[3];
		// TODO: This is not an exact equivalent
		return sign * point[coord] <= point[3];
	}
	wsw_forceinline auto getIntersectionFrac( const vec4_t prev, const vec4_t curr ) -> float {
		// P: return ( prev[3] - prev[coord] ) * Q_Rcp( ( prev[3] - prev[coord] ) - ( curr[3] - curr[coord] ) );
		// N: return ( prev[3] + prev[coord] ) * Q_Rcp( ( prev[3] + prev[coord] ) - ( curr[3] + curr[coord] ) );
		const float expr = prev[3] - sign * prev[coord];
		return expr * Q_Rcp( expr - ( curr[3] - sign * curr[coord] ) );
	}
};

template <typename PlaneTraits>
[[nodiscard]]
wsw_forceinline
// Pass the traits by value for providing better optimization opportunities
static auto clipAgainstPlane( PlaneTraits planeTraits, unsigned numInVertices, const vec4_t *__restrict inCoords, vec4_t *__restrict outCoords ) -> unsigned {
	unsigned numOutVertices = 0;

	unsigned prevIndex = numInVertices - 1;
	unsigned currIndex = 0;
	do {
		assert( prevIndex != currIndex );
		const float *__restrict prevCoord = inCoords[prevIndex];
		const float *__restrict currCoord = inCoords[currIndex];

		const bool isPrevInside = planeTraits.isInside( prevCoord );
		const bool isCurrInside = planeTraits.isInside( currCoord );

		if( isPrevInside != isCurrInside ) {
			const float frac = planeTraits.getIntersectionFrac( prevCoord, currCoord );
			// Note: We allow the frac to go slightly above 1.0f due to use of coarse reciprocals.
			// This is fine as we only need estimation of a screenspace triangle area.
			assert( frac >= 0.0f && frac <= 1.0f + 1e-3f );

			float *__restrict destCoord = outCoords[numOutVertices];
			Vector4Lerp( prevCoord, frac, currCoord, destCoord );
			numOutVertices += 1;
		}

		if( isCurrInside ) {
			float *__restrict destCoord = outCoords[numOutVertices];
			Vector4Copy( currCoord, destCoord );
			numOutVertices += 1;
		}

		prevIndex = currIndex;
	} while( ++currIndex < numInVertices );

	return numOutVertices;
}

[[nodiscard]]
static auto clipTriangle( vec4_t inCoords[], vec4_t outCoords[] ) -> unsigned {
	const auto isVertexVisible = []( const vec4_t v ) {
		return std::fabs( v[0] ) <= v[3] && std::fabs( v[1] ) <= v[3] && std::fabs( v[2] ) <= v[3];
	};
	if( isVertexVisible( inCoords[0] ) && isVertexVisible( inCoords[1] ) && isVertexVisible( inCoords[2] ) ) {
		Vector4Copy( inCoords[0], outCoords[0] );
		Vector4Copy( inCoords[1], outCoords[1] );
		Vector4Copy( inCoords[2], outCoords[2] );
		return 3;
	} else {
		unsigned numVertices = clipAgainstPlane( WPlaneTraits {}, 3, inCoords, outCoords );
		if( numVertices < 3 ) {
			return 0;
		}
		// Out->In; 0, +1.0
		// In->Out; 0, -1.0
		// Out->In; 1, +1.0
		// In->Out; 1, -1.0
		// Out->In; 2, +1.0
		// In->Out; 2, -1.0
		vec4_t *turnCoords[2] { outCoords, inCoords };
		const float turnSigns[2] { +1.0f, -1.0f };
		unsigned turn = 0;
		do {
			unsigned inIndex = turn % 2;
			const vec4_t *in = turnCoords[inIndex];
			vec4_t *out      = turnCoords[( turn + 1 ) % 2];
			unsigned coord   = turn / 2;
			float sign       = turnSigns[inIndex];
			numVertices      = clipAgainstPlane( XyzPlaneTraits { coord, sign }, numVertices, in, out );
			if( numVertices < 3 ) {
				return 0;
			}
		} while( ++turn < 6 );
		assert( turnCoords[turn % 2] == outCoords );
		return numVertices;
	}
}

static auto calcOccluderAreaScore( const OccluderDataEntry &occluder, const float *mvpMatrix ) -> float {
	const unsigned numOccluderVertices = occluder.numVertices;
	vec4_t clipspaceOccluderVertices[7];

	unsigned inVertexNum = 0;
	do {
		vec4_t temp;
		temp[0] = occluder.data[inVertexNum][0];
		temp[1] = occluder.data[inVertexNum][1];
		temp[2] = occluder.data[inVertexNum][2];
		temp[3] = 1.0f;

		Matrix4_Multiply_Vector( mvpMatrix, temp, clipspaceOccluderVertices[inVertexNum] );
		if( clipspaceOccluderVertices[inVertexNum][3] == 0.0f ) [[unlikely]] {
			return 0.0f;
		}

		assert( std::isfinite( clipspaceOccluderVertices[inVertexNum][0] ) );
		assert( std::isfinite( clipspaceOccluderVertices[inVertexNum][1] ) );
		assert( std::isfinite( clipspaceOccluderVertices[inVertexNum][2] ) );
		assert( std::isfinite( clipspaceOccluderVertices[inVertexNum][3] ) );
	} while( ++inVertexNum < numOccluderVertices );

	// Must be of the same size as we exchange them in ping-pong fashion during clipping
	vec4_t inTriCoords[64];
	vec4_t outTriCoords[64];

	float areaScore = 0.0f;
	inVertexNum       = 1;
	do {
		const float *const unclippedVert1 = clipspaceOccluderVertices[0];
		const float *const unclippedVert2 = clipspaceOccluderVertices[inVertexNum + 0];
		// Notice the "1" in the case of wrapping
		const float *const unclippedVert3 = clipspaceOccluderVertices[inVertexNum + 1 < numOccluderVertices ? inVertexNum + 1 : 1];

		Vector4Copy( unclippedVert1, inTriCoords[0] );
		Vector4Copy( unclippedVert2, inTriCoords[1] );
		Vector4Copy( unclippedVert3, inTriCoords[2] );

		if( const unsigned numOutVertices = clipTriangle( inTriCoords, outTriCoords ) ) {
			assert( numOutVertices >= 3 );
			unsigned outVertexNum = 0;
			do {
				const float *clipspaceVert1 = outTriCoords[outVertexNum + 0];
				const float *clipspaceVert2 = outTriCoords[outVertexNum + 1];
				const float *clipspaceVert3 = outTriCoords[outVertexNum + 2];

				assert( std::isfinite( clipspaceVert1[0] ) && std::isfinite( clipspaceVert1[1] ) );
				assert( std::isfinite( clipspaceVert1[2] ) && std::isfinite( clipspaceVert1[3] ) );

				assert( std::isfinite( clipspaceVert2[0] ) && std::isfinite( clipspaceVert2[1] ) );
				assert( std::isfinite( clipspaceVert2[2] ) && std::isfinite( clipspaceVert2[3] ) );

				assert( std::isfinite( clipspaceVert3[0] ) && std::isfinite( clipspaceVert3[1] ) );
				assert( std::isfinite( clipspaceVert3[2] ) && std::isfinite( clipspaceVert3[3] ) );

				const float w1 = clipspaceVert1[3], w2 = clipspaceVert2[3], w3 = clipspaceVert3[3];
				if( std::fabs( w1 ) < 1e-6f || std::fabs( w2 ) < 1e-6f || std::fabs( w3 ) < 1e-6f ) [[unlikely]] {
					return 0.0f;
				}

				const float rcpW1 = Q_Rcp( w1 );
				const float rcpW2 = Q_Rcp( w2 );
				const float rcpW3 = Q_Rcp( w3 );
				assert( std::isfinite( rcpW1 ) && std::isfinite( rcpW2 ) && std::isfinite( rcpW3 ) );

				vec2_t ndcVert1, ndcVert2, ndcVert3;
				Vector2Scale( clipspaceVert1, rcpW1, ndcVert1 );
				Vector2Scale( clipspaceVert2, rcpW2, ndcVert2 );
				Vector2Scale( clipspaceVert3, rcpW3, ndcVert3 );

				// We don't care of actually mapping NDC to viewport coord units.
				// We just need some score which linearly depends of screen-space area.
				const vec3_t _1To2 { ndcVert2[0] - ndcVert1[0], ndcVert2[1] - ndcVert1[1], 0.0f };
				const vec3_t _1To3 { ndcVert3[0] - ndcVert1[0], ndcVert3[1] - ndcVert1[1], 0.0f };

				vec3_t cross;
				CrossProduct( _1To2, _1To3, cross );
				assert( std::isfinite( cross[0] ) && std::isfinite( cross[1] ) && std::isfinite( cross[2] ) );
				// We have to take the square root for proper summation of triangle areas into resulting area
				areaScore += VectorLengthFast( cross );
			} while( ++outVertexNum < numOutVertices - 2 );
		}
	} while( ++inVertexNum < numOccluderVertices );

	return areaScore;
}

}