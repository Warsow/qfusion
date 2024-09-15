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
#include "program.h"
#include "materiallocal.h"
#include "../common/singletonholder.h"
#include "../common/links.h"

extern cvar_t *cl_multithreading;

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

void Frontend::beginDrawingScenes() {
	R_ClearSkeletalCache();
	recycleFrameCameraStates();
	m_drawSceneFrame++;
	m_cameraIndexCounter = 0;
	m_sceneIndexCounter = 0;
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

	co_await si.taskSystem->awaiterOf( self->beginPreparingRenderingFromTheseCameras( { scenesAndCameras, scenesAndCameras + numScenesAndCameras } ) );
}

auto Frontend::coEndProcessingDrawSceneRequests( CoroTask::StartInfo si, Frontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask {
	std::pair<Scene *, StateForCamera *> scenesAndCameras[kMaxDrawSceneRequests];
	unsigned numScenesAndCameras = 0;

	for( DrawSceneRequest *request: requests ) {
		if( auto *const stateForSceneCamera = (StateForCamera *)request->stateForCamera ) {
			scenesAndCameras[numScenesAndCameras++] = std::pair<Scene *, StateForCamera *> { request, stateForSceneCamera };
		}
	}

	co_await si.taskSystem->awaiterOf( self->endPreparingRenderingFromTheseCameras( { scenesAndCameras, scenesAndCameras + numScenesAndCameras } ) );
}

auto Frontend::beginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle {
	CoroTask::StartInfo si { &m_taskSystem, {}, CoroTask::OnlyMainThread };
	// Note: passing the span should be safe as it resides in cgame code during task system execution
	return m_taskSystem.addCoro( [=, this]() { return coBeginProcessingDrawSceneRequests( si, this, requests ); } );
}

auto Frontend::endProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle {
	CoroTask::StartInfo si { &m_taskSystem, {}, CoroTask::OnlyMainThread };
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

[[nodiscard]]
static auto suggestNumExtraThreads() -> unsigned {
	if( cl_multithreading->integer ) {
		unsigned numPhysicalProcessors = 0, numLogicalProcessors = 0;
		Sys_GetNumberOfProcessors( &numPhysicalProcessors, &numLogicalProcessors );
		if( numPhysicalProcessors > 3 ) {
			// Not more than kMaxWorkers - 1 (accounting for the main thread which is a worker as well),
			// starting from 1 extra worker thread in addition to the main one on a 4-core machine.
			// TODO: Reserve more, park threads dynamically depending on whether the builtin server is really running.
			return wsw::min<unsigned>( Frontend::kMaxWorkers - 1, numPhysicalProcessors - 3 );
		}
	}
	return 0;
}

Frontend::Frontend() : m_taskSystem( { .numExtraThreads = suggestNumExtraThreads() } ) {
	const auto features = Sys_GetProcessorFeatures();
	if( Q_CPU_FEATURE_SSE41 & features ) {
		m_collectVisibleWorldLeavesArchMethod          = &Frontend::collectVisibleWorldLeavesSse41;
		m_collectVisibleOccludersArchMethod            = &Frontend::collectVisibleOccludersSse41;
		m_buildFrustaOfOccludersArchMethod             = &Frontend::buildFrustaOfOccludersSse41;
		m_cullLeavesByOccludersArchMethod              = &Frontend::cullLeavesByOccludersSse41;
		if( Q_CPU_FEATURE_AVX & features ) {
			m_cullSurfacesInVisLeavesByOccludersArchMethod = &Frontend::cullSurfacesInVisLeavesByOccludersAvx;
			m_cullLeavesByOccludersArchMethod              = &Frontend::cullLeavesByOccludersAvx;
		} else {
			m_cullSurfacesInVisLeavesByOccludersArchMethod = &Frontend::cullSurfacesInVisLeavesByOccludersSse41;
			m_cullLeavesByOccludersArchMethod              = &Frontend::cullLeavesByOccludersSse41;
		}
		m_cullEntriesWithBoundsArchMethod              = &Frontend::cullEntriesWithBoundsSse41;
		m_cullEntryPtrsWithBoundsArchMethod            = &Frontend::cullEntryPtrsWithBoundsSse41;
	} else {
		m_collectVisibleWorldLeavesArchMethod          = &Frontend::collectVisibleWorldLeavesSse2;
		m_collectVisibleOccludersArchMethod            = &Frontend::collectVisibleOccludersSse2;
		m_buildFrustaOfOccludersArchMethod             = &Frontend::buildFrustaOfOccludersSse2;
		m_cullLeavesByOccludersArchMethod              = &Frontend::cullLeavesByOccludersSse2;
		m_cullSurfacesInVisLeavesByOccludersArchMethod = &Frontend::cullSurfacesInVisLeavesByOccludersSse2;
		m_cullEntriesWithBoundsArchMethod              = &Frontend::cullEntriesWithBoundsSse2;
		m_cullEntryPtrsWithBoundsArchMethod            = &Frontend::cullEntryPtrsWithBoundsSse2;
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
	if( m_cameraIndexCounter >= MAX_REF_CAMERAS ) [[unlikely]] {
		return nullptr;
	}

	StateForCameraStorage *resultStorage = nullptr;
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
	assert( !resultStorage->isStateConstructed );

	auto *stateForCamera = new( resultStorage->theStateStorage )StateForCamera;
	resultStorage->isStateConstructed = true;

	stateForCamera->sortList                                 = &resultStorage->meshSortList;
	stateForCamera->visibleLeavesBuffer                      = &resultStorage->visibleLeavesBuffer;
	stateForCamera->occluderPassFullyVisibleLeavesBuffer     = &resultStorage->occluderPassFullyVisibleLeavesBuffer;
	stateForCamera->occluderPassPartiallyVisibleLeavesBuffer = &resultStorage->occluderPassPartiallyVisibleLeavesBuffer;
	stateForCamera->visibleOccludersBuffer                   = &resultStorage->visibleOccludersBuffer;
	stateForCamera->sortedOccludersBuffer                    = &resultStorage->sortedOccludersBuffer;
	stateForCamera->drawSurfSurfSpansBuffer                  = &resultStorage->drawSurfSurfSpansBuffer;
	stateForCamera->drawSurfMinMaxSpansBuffers               = &resultStorage->drawSurfMinMaxSpansBuffers;
	stateForCamera->bspDrawSurfacesBuffer                    = &resultStorage->bspDrawSurfacesBuffer;
	stateForCamera->surfVisTableBuffer                       = &resultStorage->bspSurfVisTableBuffer;
	stateForCamera->drawSurfSurfSubspansBuffer               = &resultStorage->drawSurfSurfSubspansBuffer;
	stateForCamera->drawSurfVertElemSpansBuffer              = &resultStorage->drawSurfVertElemSpansBuffer;
	stateForCamera->visTestedModelsBuffer                    = &resultStorage->visTestedModelsBuffer;
	stateForCamera->leafLightBitsOfSurfacesBuffer            = &resultStorage->leafLightBitsOfSurfacesBuffer;

	resultStorage->particleDrawSurfacesBuffer.reserve( Scene::kMaxParticleAggregates * Scene::kMaxParticlesInAggregate );
	stateForCamera->particleDrawSurfaces = resultStorage->particleDrawSurfacesBuffer.get();

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
	if( !m_entities.full() && ent ) [[likely]] {
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
		} else if( ent->rtype == RT_PORTALSURFACE ) [[unlikely]] {
			m_portalSurfaceEntities.push_back( *ent );
			added = std::addressof( m_portalSurfaceEntities.back() );
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