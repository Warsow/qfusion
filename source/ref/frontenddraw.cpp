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

#include <algorithm>

static void R_TransformForWorld( void ) {
	RB_LoadObjectMatrix( mat4x4_identity );
}

static void R_TranslateForEntity( const entity_t *e ) {
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

static void R_TransformForEntity( const entity_t *e ) {
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

void Frontend::bindFrameBufferAndViewport( int, const StateForCamera *stateForCamera ) {
	// TODO: This is for the default render target
	const int width  = glConfig.width;
	const int height = glConfig.height;

	rf.frameBufferWidth  = width;
	rf.frameBufferHeight = height;

	RB_BindFrameBufferObject();

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
		m_stateForActiveCamera = nullptr;

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
		nullptr,
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
		( batchDrawSurf_cb ) & R_SubmitDynamicMeshesToBackend,
		/* ST_PARTICLE */
		( batchDrawSurf_cb ) & R_SubmitParticleSurfsToBackend,
		/* ST_CORONA */
		( batchDrawSurf_cb ) & R_SubmitCoronaSurfsToBackend,
		/* ST_NULLMODEL */
		nullptr,
	};

void Frontend::submitSortedSurfacesToBackend( Scene *scene ) {
	const auto *list = m_stateForActiveCamera->list;
	if( list->empty() ) {
		return;
	}

	FrontendToBackendShared fsh;
	fsh.dynamicLights               = scene->m_dynamicLights.data();
	fsh.particleAggregates          = scene->m_particles.data();
	fsh.allVisibleLightIndices      = { m_allVisibleLightIndices, m_numAllVisibleLights };
	fsh.visibleProgramLightIndices  = { m_visibleProgramLightIndices, m_numVisibleProgramLights };
	fsh.renderFlags                 = m_stateForActiveCamera->renderFlags;
	fsh.fovTangent                  = m_stateForActiveCamera->lodScaleForFov;
	std::memcpy( fsh.viewAxis, m_stateForActiveCamera->viewAxis, sizeof( mat3_t ) );
	VectorCopy( m_stateForActiveCamera->viewOrigin, fsh.viewOrigin );

	auto *const materialCache = MaterialCache::instance();

	unsigned prevShaderNum                 = ~0;
	unsigned prevEntNum                    = ~0;
	int prevPortalNum                      = ~0;
	int prevFogNum                         = ~0;
	unsigned prevMergeabilitySeparator     = ~0;
	unsigned prevSurfType                  = ~0;
	bool prevIsDrawSurfBatched             = false;
	const sortedDrawSurf_t *batchSpanBegin = nullptr;

	float depthmin = 0.0f, depthmax = 0.0f;
	bool depthHack = false, cullHack = false;
	bool prevInfiniteProj = false;
	int prevEntityFX = -1;

	const mfog_t *prevFog = nullptr;
	const portalSurface_t *prevPortalSurface = nullptr;

	const size_t numDrawSurfs = list->size();
	const sortedDrawSurf_t *const drawSurfs = list->data();
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
		const auto *portalSurface = portalNum >= 0 ? m_stateForActiveCamera->portalSurfaces + portalNum : nullptr;
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

				RB_FlushDynamicMeshes();
				callback( &fsh, prevEntity, prevShader, prevFog, prevPortalSurface, { batchSpanBegin, batchSpanEnd } );
				RB_FlushDynamicMeshes();
			}

			if( isDrawSurfBatched ) {
				batchSpanBegin = sds;
			} else {
				batchSpanBegin = nullptr;
			}

			// hack the depth range to prevent view model from poking into walls
			if( entity->flags & RF_WEAPONMODEL ) {
				if( !depthHack ) {
					RB_FlushDynamicMeshes();
					depthHack = true;
					RB_GetDepthRange( &depthmin, &depthmax );
					RB_DepthRange( depthmin, depthmin + 0.3f * ( depthmax - depthmin ) );
				}
			} else {
				if( depthHack ) {
					RB_FlushDynamicMeshes();
					depthHack = false;
					RB_DepthRange( depthmin, depthmax );
				}
			}

			if( entNum != prevEntNum ) {
				// backface culling for left-handed weapons
				bool oldCullHack = cullHack;
				cullHack = ( ( entity->flags & RF_CULLHACK ) ? true : false );
				if( cullHack != oldCullHack ) {
					RB_FlushDynamicMeshes();
					RB_FlipFrontFace();
				}
			}

			// sky and things that don't use depth test use infinite projection matrix
			// to not pollute the farclip
			const bool infiniteProj = entity->renderfx & RF_NODEPTHTEST ? true : false;
			if( infiniteProj != prevInfiniteProj ) {
				RB_FlushDynamicMeshes();
				if( infiniteProj ) {
					mat4_t projectionMatrix;
					Matrix4_Copy( m_stateForActiveCamera->projectionMatrix, projectionMatrix );
					Matrix4_PerspectiveProjectionToInfinity( Z_NEAR, projectionMatrix, glConfig.depthEpsilon );
					RB_LoadProjectionMatrix( projectionMatrix );
				} else {
					RB_LoadProjectionMatrix( m_stateForActiveCamera->projectionMatrix );
				}
			}

			if( isDrawSurfBatched ) {
				// don't transform batched surfaces
				if( !prevIsDrawSurfBatched ) {
					RB_LoadObjectMatrix( mat4x4_identity );
				}
			} else {
				if( ( entNum != prevEntNum ) || prevIsDrawSurfBatched ) {
					if( entity->number == kWorldEntNumber ) [[likely]] {
						R_TransformForWorld();
					} else if( entity->rtype == RT_MODEL ) {
						R_TransformForEntity( entity );
					} else if( shader->flags & SHADER_AUTOSPRITE ) {
						R_TranslateForEntity( entity );
					} else {
						R_TransformForWorld();
					}
				}
			}

			if( !isDrawSurfBatched ) {
				assert( r_drawSurfCb[surfType] );

				RB_BindShader( entity, shader, fog );
				RB_SetPortalSurface( portalSurface );

				r_drawSurfCb[surfType]( &fsh, entity, shader, fog, portalSurface, sds->drawSurf );
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

		RB_FlushDynamicMeshes();
		callback( &fsh, prevEntity, prevShader, prevFog, prevPortalSurface, { batchSpanBegin, batchSpanEnd } );
		RB_FlushDynamicMeshes();
	}

	if( depthHack ) {
		RB_DepthRange( depthmin, depthmax );
	}
	if( cullHack ) {
		RB_FlipFrontFace();
	}

	RB_BindFrameBufferObject();
}

void Frontend::renderScene( Scene *scene, const refdef_s *fd ) {
	set2DMode( false );

	RB_SetTime( fd->time );

	std::memset( m_bufferForRegularState, 0, sizeof( StateForCamera ) );
	auto *stateForSceneCamera = new( m_bufferForRegularState )StateForCamera;

	stateForSceneCamera->list = &m_meshDrawList;
	setupStateForCamera( stateForSceneCamera, fd );

	if( stateForSceneCamera->refdef.minLight < 0.1f ) {
		stateForSceneCamera->refdef.minLight = 0.1f;
	}

	bindFrameBufferAndViewport( 0, stateForSceneCamera );

	renderViewFromThisCamera( scene, stateForSceneCamera );

	bindFrameBufferAndViewport( 0, stateForSceneCamera );

	set2DMode( true );
}

void Frontend::setupStateForCamera( StateForCamera *stateForCamera, const refdef_t *fd ) {
	stateForCamera->refdef      = *fd;
	stateForCamera->farClip     = getDefaultFarClip( fd );

	stateForCamera->renderFlags = 0;
	if( r_lightmap->integer ) {
		stateForCamera->renderFlags |= RF_LIGHTMAP;
	}

	if( r_drawflat->integer ) {
		stateForCamera->renderFlags |= RF_DRAWFLAT;
	}

	VectorCopy( stateForCamera->refdef.vieworg, stateForCamera->viewOrigin );
	Matrix3_Copy( stateForCamera->refdef.viewaxis, stateForCamera->viewAxis );

	stateForCamera->lodScaleForFov = std::tan( stateForCamera->refdef.fov_x * ( M_PI / 180 ) * 0.5f );

	Vector4Set( stateForCamera->scissor, fd->scissor_x, fd->scissor_y, fd->scissor_width, fd->scissor_height );
	Vector4Set( stateForCamera->viewport, fd->x, fd->y, fd->width, fd->height );
	VectorCopy( fd->vieworg, stateForCamera->pvsOrigin );
	VectorCopy( fd->vieworg, stateForCamera->lodOrigin );

	stateForCamera->numPortalSurfaces      = 0;
	stateForCamera->numDepthPortalSurfaces = 0;

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

	if( fd->rdflags & RDF_FLIPPED ) {
		stateForCamera->projectionMatrix[0] = -stateForCamera->projectionMatrix[0];
		stateForCamera->renderFlags |= RF_FLIPFRONTFACE;
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

	stateForCamera->list->clear();
	if( shouldDrawWorldModel ) {
		stateForCamera->list->reserve( rsh.worldBrushModel->numDrawSurfaces );
	}

	if( shouldDrawWorldModel ) {
		const mleaf_t *const leaf   = Mod_PointInLeaf( stateForCamera->pvsOrigin, rsh.worldModel );
		stateForCamera->viewCluster = leaf->cluster;
		stateForCamera->viewArea    = leaf->area;
	} else {
		stateForCamera->viewCluster = -1;
		stateForCamera->viewArea    = -1;
	}

	stateForCamera->frustum.setupFor4Planes( fd->vieworg, fd->viewaxis, fd->fov_x, fd->fov_y );
}

void Frontend::renderViewFromThisCamera( Scene *scene, StateForCamera *stateForCamera ) {
	m_stateForActiveCamera = stateForCamera;

	m_visFrameCount++;

	std::span<const Frustum> occluderFrusta;
	std::span<const unsigned> nonOccludedLeaves;
	std::span<const unsigned> partiallyOccludedLeaves;

	m_numAllVisibleLights     = 0;
	m_numVisibleProgramLights = 0;

	bool drawWorld = false;

	if( !( m_stateForActiveCamera->refdef.rdflags & RDF_NOWORLDMODEL ) ) {
		if( r_drawworld->integer && rsh.worldModel ) {
			drawWorld = true;
			std::tie( occluderFrusta, nonOccludedLeaves, partiallyOccludedLeaves ) = cullWorldSurfaces();
			// TODO: Update far clip, update view matrices
		}
	}

	collectVisiblePolys( scene, occluderFrusta );

	if( const int dynamicLightValue = r_dynamiclight->integer ) {
		[[maybe_unused]]
		const auto [visibleProgramLightIndices, visibleCoronaLightIndices] = collectVisibleLights( scene, occluderFrusta );
		if( dynamicLightValue & 2 ) {
			addCoronaLightsToSortList( scene->m_polyent, scene->m_dynamicLights.data(), visibleCoronaLightIndices );
		}
		if( dynamicLightValue & 1 ) {
			std::span<const unsigned> spansStorage[2] { nonOccludedLeaves, partiallyOccludedLeaves };
			std::span<std::span<const unsigned>> spansOfLeaves = { spansStorage, 2 };
			markLightsOfSurfaces( scene, spansOfLeaves, visibleProgramLightIndices );
		}
	}

	if( drawWorld ) {
		// We must know lights at this point
		addVisibleWorldSurfacesToSortList( scene );
	}

	if( r_drawentities->integer ) {
		collectVisibleEntities( scene, occluderFrusta );
		collectVisibleDynamicMeshes( scene, occluderFrusta );
	}

	collectVisibleParticles( scene, occluderFrusta );

	const auto cmp = []( const sortedDrawSurf_t &lhs, const sortedDrawSurf_t &rhs ) {
		// TODO: Avoid runtime coposition of keys
		const auto lhsKey = ( (uint64_t)lhs.distKey << 32 ) | (uint64_t)lhs.sortKey;
		const auto rhsKey = ( (uint64_t)rhs.distKey << 32 ) | (uint64_t)rhs.sortKey;
		return lhsKey < rhsKey;
	};

	std::sort( m_stateForActiveCamera->list->begin(), m_stateForActiveCamera->list->end(), cmp );

	bindFrameBufferAndViewport( m_stateForActiveCamera->renderTarget, m_stateForActiveCamera );

	const int *const scissor = m_stateForActiveCamera->scissor;
	RB_Scissor( scissor[0], scissor[1], scissor[2], scissor[3] );

	const int *const viewport = m_stateForActiveCamera->viewport;
	RB_Viewport( viewport[0], viewport[1], viewport[2], viewport[3] );

	const unsigned renderFlags = m_stateForActiveCamera->renderFlags;

	RB_SetZClip( Z_NEAR, m_stateForActiveCamera->farClip );
	RB_SetCamera( m_stateForActiveCamera->viewOrigin, m_stateForActiveCamera->viewAxis );
	RB_SetLightParams( m_stateForActiveCamera->refdef.minLight, !drawWorld );
	RB_SetRenderFlags( renderFlags );
	RB_LoadProjectionMatrix( m_stateForActiveCamera->projectionMatrix );
	RB_LoadCameraMatrix( m_stateForActiveCamera->cameraMatrix );
	RB_LoadObjectMatrix( mat4x4_identity );

	if( renderFlags & RF_FLIPFRONTFACE ) {
		RB_FlipFrontFace();
	}

	if( renderFlags & RF_SHADOWMAPVIEW ) {
		RB_SetShaderStateMask( ~0, GLSTATE_NO_COLORWRITE );
	}

	//drawPortals();

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
	} else if( drawWorld ) {
		shouldClearColor = m_stateForActiveCamera->renderTarget != 0;
		Vector4Set( clearColor, 1, 1, 1, 0 );
	} else {
		shouldClearColor = !m_stateForActiveCamera->numDepthPortalSurfaces || R_FASTSKY();
		if( rsh.worldBrushModel && rsh.worldBrushModel->globalfog && rsh.worldBrushModel->globalfog->shader ) {
			Vector4Scale( rsh.worldBrushModel->globalfog->shader->fog_color, 1.0 / 255.0, clearColor );
		} else {
			Vector4Scale( mapConfig.environmentColor, 1.0 / 255.0, clearColor );
		}
	}

	int clearBits = 0;
	if( !didDrawADepthMask ) {
		clearBits |= GL_DEPTH_BUFFER_BIT;
	}
	if( shouldClearColor ) {
		clearBits |= GL_COLOR_BUFFER_BIT;
	}

	RB_Clear( clearBits, clearColor[0], clearColor[1], clearColor[2], clearColor[3] );

	submitSortedSurfacesToBackend( scene );

	if( r_showtris->integer && !( m_stateForActiveCamera->renderFlags & RF_SHADOWMAPVIEW ) ) {
		RB_EnableWireframe( true );

		submitSortedSurfacesToBackend( scene );

		RB_EnableWireframe( false );
	}

	R_TransformForWorld();

	if( ( m_stateForActiveCamera->renderFlags & RF_SHADOWMAPVIEW ) ) {
		RB_SetShaderStateMask( ~0, 0 );
	}

	if( m_stateForActiveCamera->renderFlags & RF_FLIPFRONTFACE ) {
		RB_FlipFrontFace();
	}

	RB_SetShaderStateMask( ~0, GLSTATE_NO_DEPTH_TEST );

	submitDebugStuffToBackend( scene );

	RB_SetShaderStateMask( ~0, 0 );
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