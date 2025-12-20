/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2013 Victor Luchits

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

// r_main.c

#include "local.h"
#include "program.h"
#include "buffermanagement.h"
#include "materiallocal.h"
#include "texturemanagement.h"
#include "frontend.h"
#include <common/facilities/sysclock.h>
#include <common/helpers/scopeexitaction.h>
#include <common/facilities/cvar.h>
#include <common/facilities/configvars.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/profilerscope.h>
#include <common/facilities/syspublic.h>
#include <common/common.h>
#include <client/videosystem.h>

using wsw::operator""_asView;

r_globals_t rf;

mapconfig_t mapConfig;

lightstyle_t lightStyles[MAX_LIGHTSTYLES];

glconfig_t glConfig;

r_shared_t rsh;

BoolConfigVar v_fullbright { "r_fullbright"_asView, { .byDefault = false, .flags = CVAR_LATCH_VIDEO } };
BoolConfigVar v_lightmap { "r_lightOnly"_asView, { .byDefault = false } };
BoolConfigVar v_drawEntities { "r_drawEntities"_asView, { .byDefault = true, .flags = CVAR_CHEAT } };
BoolConfigVar v_drawWorld { "r_drawWorld"_asView, { .byDefault = true, .flags = CVAR_CHEAT } };
BoolConfigVar v_lerpModels { "r_lerpModels"_asView, { .byDefault = true } };
BoolConfigVar v_drawElements { "r_drawElements"_asView, { .byDefault = true, .flags = CVAR_CHEAT } };
IntConfigVar v_showTris { "r_showTris"_asView, { .byDefault = 0, .flags = CVAR_CHEAT } };

BoolConfigVar v_detailTextures { "r_detailTextures"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } };
// TODO: Enum|EnumFlags
IntConfigVar v_dynamicLight { "r_dynamicLight"_asView, { .byDefault = -1, .flags = CVAR_ARCHIVE } };
IntConfigVar v_subdivisions { "r_subdivisions"_asView, { .byDefault = SUBDIVISIONS_DEFAULT, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };

BoolConfigVar v_fastSky { "r_fastSky"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } };
BoolConfigVar v_portalOnly { "r_portalOnly"_asView, { .byDefault = false, .flags = CVAR_CHEAT } };
BoolConfigVar v_portalMaps { "r_portalMaps"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
IntConfigVar v_portalMaps_maxTexSize { "r_portalMaps_maxTexSize"_asView, { .byDefault = 1024,.flags = CVAR_ARCHIVE } };

BoolConfigVar v_lighting_deluxeMapping { "r_lighting_deluxemapping"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
BoolConfigVar v_lighting_specular { "r_lighting_specular"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
FloatConfigVar v_lighting_glossIntensity { "r_lighting_glossIntensity"_asView, { .byDefault = 1.5f, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_lighting_glossExponent { "r_lighting_glossExponent"_asView, { .byDefault = 24.0f, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_lighting_ambientScale { "r_lighting_ambientScale"_asView, { .byDefault = 1.0f } };
FloatConfigVar v_lighting_directedScale { "r_lighting_directedScale"_asView, { .byDefault = 1.0f } };

BoolConfigVar v_lighting_packLightmaps { "r_lighting_packLightmaps"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
IntConfigVar v_lighting_maxLmBlockSize { "r_lighting_maxLmBlockSize"_asView, { .byDefault = 2048, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
BoolConfigVar v_lighting_vertexLight { "r_lighting_vertexLight"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
IntConfigVar v_lighting_maxGlslDlights { "r_lighting_maxGlslDlights"_asView, { .byDefault = 16, .flags = CVAR_ARCHIVE } };
BoolConfigVar v_lighting_grayscale { "r_lighting_grayscale"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
FloatConfigVar v_lighting_intensity { "r_lighting_intensity"_asView, { .byDefault = 1.5f, .flags = CVAR_ARCHIVE } };

// TODO: Enum|EnumFlags
IntConfigVar v_offsetMapping { "r_offsetMapping"_asView, { .byDefault = 2, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_offsetMapping_scale { "r_offsetMapping_scale"_asView, { .byDefault = 0.02f, .flags = CVAR_ARCHIVE } };
BoolConfigVar v_offsetMapping_reliefMapping { "r_offsetMapping_reliefMapping"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } };

FloatConfigVar v_outlinesWorld { "r_outlinesWorld"_asView, { .byDefault = 1.8f, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_outlinesScale { "r_outlinesScale"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 3.0f ), .flags = CVAR_ARCHIVE } };
FloatConfigVar v_outlinesCutoff { "r_outlinesCutoff"_asView, { .byDefault = 712.0f, .flags = CVAR_ARCHIVE } };

BoolConfigVar v_softParticles { "r_softParticles"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_softParticles_scale { "r_softParticles_scale"_asView, { .byDefault = 0.02f, .flags = CVAR_ARCHIVE } };

BoolConfigVar v_hdr { "r_hdr"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_hdrGamma { "r_hdrGamma"_asView, { .byDefault = 2.2f, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_hdrExposure { "r_hdrExposure"_asView, { .byDefault = 1.0f, .flags = CVAR_ARCHIVE } };

IntConfigVar v_lodBias { "r_lodbias"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } };
FloatConfigVar v_lodScale { "r_lodscale"_asView, { .byDefault = 5.0f, .flags = CVAR_ARCHIVE } };

FloatConfigVar v_gamma { "r_gamma"_asView, { .byDefault = 1.0f, .flags = CVAR_ARCHIVE } };

StringConfigVar v_textureFilter { "r_textureFilter"_asView, { .byDefault = "trilinear"_asView, .flags = CVAR_ARCHIVE } };
IntConfigVar v_anisoLevel { "r_anisoLevel"_asView, { .byDefault = 4, .flags = CVAR_ARCHIVE } };
BoolConfigVar v_textureCompression { "r_textureCompression"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
// TODO: should we allow configuring it?
IntConfigVar v_stencilBits { "r_stencilBits"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };

BoolConfigVar v_drawFlat { "r_drawFlat"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } };
ColorConfigVar v_wallColor { "r_wallColor"_asView, { .byDefault = COLOR_RGB( 128, 80, 192 ), .flags = CVAR_ARCHIVE } };
ColorConfigVar v_floorColor { "r_floorColor"_asView, { .byDefault = COLOR_RGB( 144, 48, 72 ), .flags = CVAR_ARCHIVE } };

#if defined( GLX_VERSION ) && !defined( USE_SDL2 )
IntConfigVar v_swapInterval { "r_swapInterval"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
#else
IntConfigVar v_swapInterval { "r_swapInterval"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } };
#endif

BoolConfigVar v_showShaderCache { "r_showShaderCache"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } };

static VarModificationTracker g_textureFilterVarTracker { &v_textureFilter };
static VarModificationTracker g_anisoLevelVarTracker { &v_anisoLevel };
static VarModificationTracker g_wallColorVarTracker { &v_wallColor };
static VarModificationTracker g_floorColorVarTracker { &v_floorColor };
static VarModificationTracker g_gammaVarTracker { &v_gamma };

extern BoolConfigVar v_multiThreading;

// Currently it's just a proxy for global function calls, so the lifecycle is trivial
static RenderSystem g_renderSystem;

RenderSystem *RF_GetRenderSystem() {
	return &g_renderSystem;
}

void R_InitCustomColors( void ) {
	memset( rsh.customColors, 255, sizeof( rsh.customColors ) );
}

void RenderSystem::setCustomColor( int num, int r, int g, int b ) {
	if( num < 0 || num >= NUM_CUSTOMCOLORS ) {
		return;
	}
	Vector4Set( rsh.customColors[num], (uint8_t)r, (uint8_t)g, (uint8_t)b, 255 );
}

int R_GetCustomColor( int num ) {
	if( num < 0 || num >= NUM_CUSTOMCOLORS ) {
		return COLOR_RGBA( 255, 255, 255, 255 );
	}
	return *(int *)rsh.customColors[num];
}

void R_ShutdownCustomColors( void ) {
	memset( rsh.customColors, 255, sizeof( rsh.customColors ) );
}


static shader_s g_externalTextureMaterialStorage[2];
static shaderpass_t g_externalTextureMaterialPassStorage[2];

static const wsw::HashedStringView kExternalMenuImage( "$externalmenuimage" );
static const wsw::HashedStringView kExternalHudImage( "$externalhudimage" );

static shader_s *R_WrapExternalTextureHandle( GLuint externalTexNum, int storageIndex ) {
	assert( storageIndex == 0 || storageIndex == 1 );
	shaderpass_t *const p = &g_externalTextureMaterialPassStorage[storageIndex];
	shader_t *const s     = &g_externalTextureMaterialStorage[storageIndex];

	wsw::HashedStringView name;
	Texture *texture;
	if( storageIndex == 0 ) {
		name = kExternalMenuImage;
		texture = TextureCache::instance()->wrapMenuTextureHandle( externalTexNum );
	} else {
		name = kExternalHudImage;
		texture = TextureCache::instance()->wrapHudTextureHandle( externalTexNum );
	}

	s->vattribs  = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT;
	s->sort      = SHADER_SORT_NEAREST;
	s->numpasses = 1;
	s->name      = name;
	s->passes    = p;

	p->rgbgen.type      = RGB_GEN_CONST;
	VectorCopy( colorWhite, p->rgbgen.args );
	p->alphagen.type    = ALPHA_GEN_CONST;
	p->alphagen.args[0] = colorWhite[3];
	p->tcgen            = TC_GEN_BASE;
	p->images[0]        = texture;
	p->flags            = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
	p->program_type     = GLSL_PROGRAM_TYPE_NONE;

	return s;
}

auto RenderSystem::wrapMenuTextureHandleInMaterial( unsigned externalTexNum ) -> shader_s * {
	return R_WrapExternalTextureHandle( externalTexNum, 0 );
}

auto RenderSystem::wrapHudTextureHandleInMaterial( unsigned externalTexNum ) -> shader_s * {
	return R_WrapExternalTextureHandle( externalTexNum, 1 );
}

static shader_s g_miniviewMaterialStorage;
static shaderpass_t g_miniviewMaterialPassStorage;

static const wsw::HashedStringView kExternalMiniviewImage( "$miniviewimage" );

auto RenderSystem::wrapMiniviewRenderTargetInMaterial( RenderTargetComponents *renderTarget ) -> shader_s * {
	// Currently all miniview render targets are shared
	//assert( renderTarget == GetMiniviewRenderTarget() );

	shaderpass_t *const p = &g_miniviewMaterialPassStorage;
	shader_t *const s     = &g_miniviewMaterialStorage;

	s->vattribs  = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT;
	s->sort      = SHADER_SORT_NEAREST;
	s->numpasses = 1;
	s->name      = kExternalMiniviewImage;
	s->passes    = p;

	p->rgbgen.type      = RGB_GEN_CONST;
	VectorCopy( colorWhite, p->rgbgen.args );
	p->alphagen.type    = ALPHA_GEN_CONST;
	p->alphagen.args[0] = colorWhite[3];
	p->tcgen            = TC_GEN_BASE;
	p->images[0]        = renderTarget->texture;
	p->flags            = 0;
	p->program_type     = GLSL_PROGRAM_TYPE_NONE;

	return s;
}

void R_Finish() {
	qglFinish();
}

void R_Flush( void ) {
	qglFlush();
}

int R_SetSwapInterval( int swapInterval, int oldSwapInterval ) {
	if( swapInterval != oldSwapInterval ) {
		GLimp_SetSwapInterval( swapInterval );
	}
	return swapInterval;
}

void R_SetGamma( float gamma ) {
	if( glConfig.hwGamma ) {
		uint16_t gammaRamp[3 * GAMMARAMP_STRIDE];
		uint16_t *const row1 = &gammaRamp[0];
		uint16_t *const row2 = &gammaRamp[GAMMARAMP_STRIDE];
		uint16_t *const row3 = &gammaRamp[2 * GAMMARAMP_STRIDE];

		const double invGamma = 1.0 / bound( 0.5, gamma, 3.0 );
		const double div = (double)( 1 << 0 ) / ( glConfig.gammaRampSize - 0.5 );

		for( int i = 0; i < glConfig.gammaRampSize; i++ ) {
			int v = ( int )( 65535.0 * pow( ( (double)i + 0.5 ) * div, invGamma ) + 0.5 );
			v = wsw::max( 0, wsw::min( 65535, v ) );
			row1[i] = row2[i] = row3[i] = v;
		}

		GLimp_SetGammaRamp( GAMMARAMP_STRIDE, glConfig.gammaRampSize, gammaRamp );
	}
}

void R_SetWallFloorColors( int wallColor, int floorColor ) {
	// TODO: Use some general conversion subroutines
	rsh.wallColor[0] = bound( 0, COLOR_R( wallColor ) / 255.0, 1.0 );
	rsh.floorColor[0] = bound( 0, COLOR_R( floorColor ) / 255.0, 1.0 );
	rsh.wallColor[1] = bound( 0, COLOR_G( wallColor ) / 255.0, 1.0 );
	rsh.floorColor[1] = bound( 0, COLOR_G( floorColor ) / 255.0, 1.0 );
	rsh.wallColor[2] = bound( 0, COLOR_B( wallColor ) / 255.0, 1.0 );
	rsh.floorColor[2] = bound( 0, COLOR_B( floorColor ) / 255.0, 1.0 );
}

auto suggestNumExtraWorkerThreads( const SuggestNumWorkerThreadsArgs &args ) -> unsigned {
	if( v_multiThreading.get() ) {
		// This should be cheap to query as it's cached.
		if( const auto maybeNumberOfProcessors = Sys_GetNumberOfProcessors() ) {
			const auto numPhysicalProcessors = maybeNumberOfProcessors->first;
			// Take the main thread into account as well (hence the +1)
			if( numPhysicalProcessors > ( args.numExcludedCores + 1 ) ) {
				// Disallow more than 3 worker threads.
				return wsw::min<unsigned>( 3, numPhysicalProcessors - ( args.numExcludedCores + 1 ) );
			}
		}
	}
	return 0;
}

void RenderSystem::beginDrawingScenes() {
	WSW_PROFILER_SCOPE();
	wsw::RendererFrontend::instance()->beginDrawingScenes();
}

void RenderSystem::endDrawingScenes() {
	WSW_PROFILER_SCOPE();
	wsw::RendererFrontend::instance()->endDrawingScenes();
}

static std::optional<TaskSystem::ExecutionHandle> g_taskSystemExecutionHandle;

auto RenderSystem::beginProcessingOfTasks() -> TaskSystem * {
	WSW_PROFILER_SCOPE();

	auto *result = wsw::RendererFrontend::instance()->getTaskSystem();
	assert( !g_taskSystemExecutionHandle );

	unsigned numAllowedExtraThreads = 0;
	// The number of workers includes the main thread, so its always non-zero.
	// Values greather than 1 indicate that we actually reserved extra worker threads.
	// We don't return zero-based values as doing that is going to complicate the code
	// which reserves thread-local stuff for workers.
	if( const unsigned numWorkers = result->getNumberOfWorkers(); numWorkers > 1 ) {
		// By default we reserve a single core for the sound backend,
		// and also a core is always implicitly reserved for the main thread.
		assert( ( SuggestNumWorkerThreadsArgs {} ).numExcludedCores == 1 );
		assert( numWorkers == suggestNumExtraWorkerThreads( {} ) + 1 );
		// Keep the same by default (numWorkers == number of allocated extra threads + 1)
		numAllowedExtraThreads = numWorkers - 1;
		// TODO: Use named constants here
		// TODO: Use all available threads if there's no active bots on the server.
		if( Com_ServerState() > 0 ) {
			// If the builtin server is actually running, we have to reserve another core for it,
			// so we activate fewer threads from the initially allocated pool
			// for execution of frame tasks if the amount of available cores is insufficient.
			// Note that another core is still implicitly reserved for the main thread as well.
			numAllowedExtraThreads = suggestNumExtraWorkerThreads( { .numExcludedCores = 2 } );
			assert( numAllowedExtraThreads <= numWorkers - 1 );
		}
	}

	g_taskSystemExecutionHandle = result->startExecution( numAllowedExtraThreads );
	return result;
}

void RenderSystem::endProcessingOfTasks() {
	WSW_PROFILER_SCOPE();

	const bool awaitResult = wsw::RendererFrontend::instance()->getTaskSystem()->awaitCompletion( g_taskSystemExecutionHandle.value() );
	g_taskSystemExecutionHandle = std::nullopt;
	if( !awaitResult ) {
		wsw::failWithLogicError( "Failed to execute rendering tasks" );
	}
}

auto RenderSystem::getMiniviewRenderTarget() -> RenderTargetComponents * {
	return wsw::RendererFrontend::instance()->getMiniviewRenderTarget();
}

auto RenderSystem::getMiniviewRenderTargetTexture() -> unsigned {
	static_assert( std::is_same_v<GLuint, unsigned> );
	return wsw::RendererFrontend::instance()->getMiniviewRenderTarget()->texture->texnum;
}

auto RenderSystem::createDrawSceneRequest( const refdef_t &refdef ) -> DrawSceneRequest * {
	WSW_PROFILER_SCOPE();
	return wsw::RendererFrontend::instance()->createDrawSceneRequest( refdef );
}

auto RenderSystem::beginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle {
	return wsw::RendererFrontend::instance()->beginProcessingDrawSceneRequests( requests );
}

auto RenderSystem::endProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests, std::span<const TaskHandle> dependencies ) -> TaskHandle {
	return wsw::RendererFrontend::instance()->endProcessingDrawSceneRequests( requests, dependencies );
}

void RenderSystem::commitProcessedDrawSceneRequest( DrawSceneRequest *request ) {
	WSW_PROFILER_SCOPE();
	wsw::RendererFrontend::instance()->commitProcessedDrawSceneRequest( request );
}

auto RenderSystem::createDraw2DRequest() -> Draw2DRequest * {
	return wsw::RendererFrontend::instance()->createDraw2DRequest();
}

void RenderSystem::commitDraw2DRequest( Draw2DRequest *request ) {
	wsw::RendererFrontend::instance()->commitDraw2DRequest( request );
}

void RenderSystem::recycleDraw2DRequest( Draw2DRequest *request ) {
	wsw::RendererFrontend::instance()->recycleDraw2DRequest( request );
}

[[nodiscard]]
static auto coPrepareDrawSceneRequest( CoroTask::StartInfo si, RenderSystem *system, DrawSceneRequest *request ) -> CoroTask {
	co_await si.taskSystem->awaiterOf( system->beginProcessingDrawSceneRequests( { &request, 1 } ) );
	co_await si.taskSystem->awaiterOf( system->endProcessingDrawSceneRequests( { &request, 1 }, {} ) );
}

void RenderSystem::executeSingleDrawSceneRequestNonSpeedCritical( DrawSceneRequest *request ) {
	do {
		TaskSystem *taskSystem = beginProcessingOfTasks();
		[[maybe_unused]] volatile wsw::ScopeExitAction callEndProcessingOfTasks( [this]() { endProcessingOfTasks(); } );
		(void)taskSystem->addCoro( [=,this]() {
			return coPrepareDrawSceneRequest( { taskSystem, {}, CoroTask::OnlyMainThread }, this, request );
		});
	} while( false );
	commitProcessedDrawSceneRequest( request );
}

void RenderSystem::getModelBounds( const model_s *model, float *mins, float *maxs ) {
	R_ModelBounds( model, mins, maxs );
}

void RenderSystem::getModelFrameBounds( const model_s *model, int frame, float *mins, float *maxs ) {
	R_ModelFrameBounds( model, frame, mins, maxs );
}

void RenderSystem::registerWorldModel( const char *model ) {
	R_RegisterWorldModel( model );
}

auto RenderSystem::registerModel( const char *name ) -> model_s * {
	return R_RegisterModel( name );
}

auto RenderSystem::getBoneInfo( const model_s *mod, int bonenum, char *name, size_t name_size, int *flags ) -> int {
	return R_SkeletalGetBoneInfo( mod, bonenum, name, name_size, flags );
}

void RenderSystem::getBonePose( const model_s *mod, int bonenum, int frame, bonepose_t *bonepose ) {
	R_SkeletalGetBonePose( mod, bonenum, frame, bonepose );
}

auto RenderSystem::getNumBones( const model_s *mod, int *numFrames ) -> int {
	return R_SkeletalGetNumBones( mod, numFrames );
}

auto RenderSystem::registerPic( const char *name ) -> shader_s * {
	return R_RegisterPic( name );
}

auto RenderSystem::registerRawAlphaMask( const char *name, int width, int height, const uint8_t *data ) -> shader_s * {
	return R_RegisterRawAlphaMask( name, width, height, data );
}

auto RenderSystem::registerSkin( const char *name ) -> shader_s * {
	return R_RegisterSkin( name );
}

auto RenderSystem::registerLinearPic( const char *name ) -> shader_s * {
	return R_RegisterLinearPic( name );
}

void RenderSystem::replaceRawSubPic( shader_s *shader, int x, int y, int width, int height, const uint8_t *data ) {
	R_ReplaceRawSubPic( shader, x, y, width, height, data );
}

auto RenderSystem::registerSkinFile( const char *name ) -> Skin * {
	return R_RegisterSkinFile( name );
}

auto RenderSystem::createExplicitlyManaged2DMaterial() -> shader_s * {
	return R_CreateExplicitlyManaged2DMaterial();
}

void RenderSystem::releaseExplicitlyManaged2DMaterial( shader_s *material ) {
	R_ReleaseExplicitlyManaged2DMaterial( material );
}

bool RenderSystem::updateExplicitlyManaged2DMaterialImage( shader_s *material, const char *name, const ImageOptions &options ) {
	return R_UpdateExplicitlyManaged2DMaterialImage( material, name, options );
}

auto RenderSystem::getMaterialDimensions( const shader_s *shader ) -> std::optional<std::pair<unsigned, unsigned>> {
	return R_GetShaderDimensions( shader );
}

bool RenderSystem::transformVectorToViewport( const refdef_t *rd, const vec3_t in, vec2_t out ) {
	if( !rd || !in || !out ) {
		return false;
	}

	vec4_t temp;
	temp[0] = in[0];
	temp[1] = in[1];
	temp[2] = in[2];
	temp[3] = 1.0f;

	mat4_t p;
	if( rd->rdflags & RDF_USEORTHO ) {
		Matrix4_OrthogonalProjection( rd->ortho_x, rd->ortho_x, rd->ortho_y, rd->ortho_y,
									  -4096.0f, 4096.0f, p );
	} else {
		Matrix4_InfinitePerspectiveProjection( rd->fov_x, rd->fov_y, Z_NEAR, p, glConfig.depthEpsilon );
	}

	mat4_t m;
	Matrix4_Modelview( rd->vieworg, rd->viewaxis, m );

	vec4_t temp2;
	Matrix4_Multiply_Vector( m, temp, temp2 );
	Matrix4_Multiply_Vector( p, temp2, temp );

	if( !temp[3] ) {
		return false;
	}

	out[0] = (float)rd->x + ( temp[0] / temp[3] + 1.0f ) * (float)rd->width * 0.5f;
	out[1] = (float)rd->height + (float)rd->y - ( temp[1] / temp[3] + 1.0f ) * (float)rd->height * 0.5f;
	return true;
}

bool RenderSystem::lerpTag( orientation_t *orient, const model_t *mod, int oldframe, int frame, float lerpfrac, const char *name ) {
	if( !orient ) {
		return false;
	}

	VectorClear( orient->origin );
	Matrix3_Identity( orient->axis );

	if( !name ) {
		return false;
	}

	if( mod->type == mod_skeletal ) {
		return R_SkeletalModelLerpTag( orient, (const mskmodel_t *)mod->extradata, oldframe, frame, lerpfrac, name );
	}
	if( mod->type == mod_alias ) {
		return R_AliasModelLerpTag( orient, (const maliasmodel_t *)mod->extradata, oldframe, frame, lerpfrac, name );
	}

	return false;
}

void RenderSystem::traceAgainstBspWorld( VisualTrace *tr, const float *start, const float *end, int skipSurfMask ) {
	R_TransformedTraceLine( tr, start, end, rsh.worldModel, vec3_origin, axis_identity, skipSurfMask );
}

void RenderSystem::traceAgainstBrushModel( VisualTrace *tr, const model_s *model, const float *origin,
							 const float *axis, const float *start, const float *end, int skipSurfMask ) {
	R_TransformedTraceLine( tr, start, end, model, origin, axis, skipSurfMask );
}

void R_BeginFrame( bool forceClear, int swapInterval ) {
	WSW_PROFILER_SCOPE();

	GLimp_BeginFrame();

	//RB_BeginFrame();

	qglDrawBuffer( GL_BACK );

	if( forceClear ) {
		qglClearColor( 0, 0, 0, 1 );
		qglClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
		assert( qglGetError() == GL_NO_ERROR );
		//RB_Clear( GL_COLOR_BUFFER_BIT, 0, 0, 0, 1 );
	}

	// set swap interval (vertical synchronization)
	rf.swapInterval = R_SetSwapInterval( swapInterval, rf.swapInterval );

	const int64_t time = Sys_Milliseconds();
	// update fps meter
	rf.frameTime.count++;
	rf.frameTime.time = time;
	if( rf.frameTime.time - rf.frameTime.oldTime >= 50 ) {
		rf.frameTime.average = time - rf.frameTime.oldTime;
		rf.frameTime.average = ((float)rf.frameTime.average / ( rf.frameTime.count - rf.frameTime.oldCount )) + 0.5f;
		rf.frameTime.oldTime = time;
		rf.frameTime.oldCount = rf.frameTime.count;
	}

}

void R_EndFrame( void ) {
	WSW_PROFILER_SCOPE();

	GLimp_EndFrame();

	//assert( qglGetError() == GL_NO_ERROR );
}

void RF_AppActivate( bool active, bool minimize, bool destroy ) {
	R_Flush();
	GLimp_AppActivate( active, minimize, destroy );
}

void RF_Shutdown( bool verbose ) {
	if( TextureCache *instance = TextureCache::maybeInstance() ) {
		instance->releasePrimaryRenderTargetAttachments();
	}

	R_Shutdown_( verbose );
}

static void RF_CheckCvars( void ) {
	// update gamma
	if( g_gammaVarTracker.checkAndReset() ) {
		R_SetGamma( v_gamma.get() );
	}

	// Force eager evaluation by using |, otherwise applying second var changes could get postponed to the next frame
	if( g_textureFilterVarTracker.checkAndReset() | g_anisoLevelVarTracker.checkAndReset() ) {
		TextureCache::instance()->applyFilter( v_textureFilter.get(), v_anisoLevel.get() );
	}

	if( g_wallColorVarTracker.checkAndReset() | g_floorColorVarTracker.checkAndReset() ) {
		R_SetWallFloorColors( v_wallColor.get(), v_floorColor.get() );
	}
}

void RF_BeginFrame( bool forceClear, bool forceVsync, bool uncappedFPS ) {
	RF_CheckCvars();

	// TODO: Should it be a boolean variable?
	int swapInterval = ( v_swapInterval.get() || forceVsync ) ? 1 : 0;
	///clamp_low( swapInterval, r_swapinterval_min->integer );

	R_BeginFrame( forceClear, swapInterval );
}

void RF_EndFrame( void ) {
	R_EndFrame();
}

void RF_BeginRegistration( void ) {
	// sync to the backend thread to ensure it's not using old assets for drawing
	R_BeginRegistration_();

	R_SetDefaultGLState();
}

void RF_EndRegistration( void ) {
	R_EndRegistration_();
}

int RF_GetAverageFrametime( void ) {
	return rf.frameTime.average;
}

static void R_InitVolatileAssets( void );
static void R_DestroyVolatileAssets( void );

[[nodiscard]]
static bool isExtensionSupported( const char *ext ) {
	GLint numExtensions;
	qglGetIntegerv( GL_NUM_EXTENSIONS, &numExtensions );
	// CBA to speed it up as this is required only on starting up
	for( GLint i = 0; i < numExtensions; ++i ) {
		if( !Q_stricmp( ext, (const char *)qglGetStringi( GL_EXTENSIONS, (GLuint)i ) ) ) {
			return true;
		}
	}
	return false;
}

static bool R_RegisterGLExtensions( void ) {
	memset( &glConfig.ext, 0, sizeof( glConfig.ext ) );

	glConfig.ext.texture_filter_anisotropic = isExtensionSupported( "GL_EXT_texture_filter_anisotropic" );
	glConfig.ext.gpu_shader5                = isExtensionSupported( "GL_EXT_gpu_shader5" );
	glConfig.ext.multisample                = false;

	int versionMajor = 0, versionMinor = 0;
	sscanf( glConfig.versionString, "%d.%d", &versionMajor, &versionMinor );
	glConfig.version = versionMajor * 100 + versionMinor * 10;

	glConfig.maxTextureSize = 0;
	qglGetIntegerv( GL_MAX_TEXTURE_SIZE, &glConfig.maxTextureSize );
	if( glConfig.maxTextureSize <= 0 ) {
		glConfig.maxTextureSize = 256;
	}
	glConfig.maxTextureSize = 1 << Q_log2( glConfig.maxTextureSize );

	char tmp[128];
	Cvar_Get( "gl_max_texture_size", "0", CVAR_READONLY );
	Cvar_ForceSet( "gl_max_texture_size", va_r( tmp, sizeof( tmp ), "%i", glConfig.maxTextureSize ) );

	/* GL_ARB_texture_cube_map */
	glConfig.maxTextureCubemapSize = 0;
	qglGetIntegerv( GL_MAX_CUBE_MAP_TEXTURE_SIZE, &glConfig.maxTextureCubemapSize );
	glConfig.maxTextureCubemapSize = 1 << Q_log2( glConfig.maxTextureCubemapSize );

	/* GL_ARB_multitexture */
	glConfig.maxTextureUnits = 1;
	qglGetIntegerv( GL_MAX_TEXTURE_IMAGE_UNITS, &glConfig.maxTextureUnits );
	Q_clamp( glConfig.maxTextureUnits, 1, MAX_TEXTURE_UNITS );

	/* GL_EXT_framebuffer_object */
	glConfig.maxRenderbufferSize = 0;
	qglGetIntegerv( GL_MAX_RENDERBUFFER_SIZE, &glConfig.maxRenderbufferSize );
	glConfig.maxRenderbufferSize = 1 << Q_log2( glConfig.maxRenderbufferSize );
	if( glConfig.maxRenderbufferSize > glConfig.maxTextureSize ) {
		glConfig.maxRenderbufferSize = glConfig.maxTextureSize;
	}

	/* GL_EXT_texture_filter_anisotropic */
	glConfig.maxTextureFilterAnisotropic = 0;
	if( isExtensionSupported( "" ) ) {
		qglGetIntegerv( GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &glConfig.maxTextureFilterAnisotropic );
	}

	qglGetIntegerv( GL_MAX_3D_TEXTURE_SIZE, &glConfig.maxTexture3DSize );
	qglGetIntegerv( GL_MAX_ARRAY_TEXTURE_LAYERS, &glConfig.maxTextureLayers );

	versionMajor = versionMinor = 0;
	sscanf( glConfig.shadingLanguageVersionString, "%d.%d", &versionMajor, &versionMinor );
	glConfig.shadingLanguageVersion = versionMajor * 100 + versionMinor;

	glConfig.maxVertexUniformComponents = glConfig.maxFragmentUniformComponents = 0;
	glConfig.maxVaryingFloats = 0;

	qglGetIntegerv( GL_MAX_VERTEX_ATTRIBS, &glConfig.maxVertexAttribs );
	qglGetIntegerv( GL_MAX_VERTEX_UNIFORM_COMPONENTS, &glConfig.maxVertexUniformComponents );
	qglGetIntegerv( GL_MAX_VARYING_FLOATS, &glConfig.maxVaryingFloats );
	qglGetIntegerv( GL_MAX_FRAGMENT_UNIFORM_COMPONENTS, &glConfig.maxFragmentUniformComponents );

	// keep the maximum number of bones we can do in GLSL sane
	// TODO: Check this
	//if( r_maxglslbones->integer > MAX_GLSL_UNIFORM_BONES ) {
	//	Cvar_ForceSet( r_maxglslbones->name, r_maxglslbones->dvalue );
	//}

	glConfig.depthEpsilon = 1.0 / ( 1 << 22 );

	glConfig.sSRGB = false;

	qglGetIntegerv( GL_MAX_SAMPLES, &glConfig.maxFramebufferSamples );

	glConfig.maxObjectLabelLen = 0;
	if( qglObjectLabel ) {
		static_assert( sizeof( GLint ) == sizeof( glConfig.maxObjectLabelLen ) );
		qglGetIntegerv( GL_MAX_LABEL_LENGTH, (GLint *)&glConfig.maxObjectLabelLen );
	}
	static_assert( sizeof( GLint ) == sizeof( glConfig.maxUniformBlockSize ) );
	qglGetIntegerv( GL_MAX_UNIFORM_BLOCK_SIZE, (GLint *)&glConfig.maxUniformBlockSize );

	Cvar_Get( "r_anisolevel_max", "0", CVAR_READONLY );
	Cvar_ForceSet( "r_anisolevel_max", va_r( tmp, sizeof( tmp ), "%i", glConfig.maxTextureFilterAnisotropic ) );

	Cvar_Get( "r_soft_particles_available", "1", CVAR_READONLY );

	// don't allow too high values for lightmap block size as they negatively impact performance
	if( v_lighting_maxLmBlockSize.get() > glConfig.maxTextureSize / 4 &&
		!( glConfig.maxTextureSize / 4 < wsw::min( QF_LIGHTMAP_WIDTH,QF_LIGHTMAP_HEIGHT ) * 2 ) ) {
		v_lighting_maxLmBlockSize.setImmediately( glConfig.maxTextureSize / 4 );
	}

	return true;
}

/*
* R_FillStartupBackgroundColor
*
* Fills the window with a color during the initialization.
*/
static void R_FillStartupBackgroundColor( float r, float g, float b ) {
	qglClearColor( r, g, b, 1.0 );
	GLimp_BeginFrame();
	qglClear( GL_COLOR_BUFFER_BIT );
	qglFinish();
	GLimp_EndFrame();
}

void R_SetDefaultGLState() {
	if( glConfig.stencilBits ) {
		assert( glConfig.stencilBits == 8 );
		qglStencilMask( ( GLuint ) ~0 );
		qglStencilFunc( GL_EQUAL, 128, 0xFF );
		qglStencilOp( GL_KEEP, GL_KEEP, GL_INCR );
	}

	qglDisable( GL_CULL_FACE );
	qglFrontFace( GL_CCW );
	qglDisable( GL_BLEND );
	qglDepthFunc( GL_LEQUAL );
	qglDepthMask( GL_FALSE );
	qglDisable( GL_POLYGON_OFFSET_FILL );
	qglPolygonOffset( -1.0f, 0.0f ); // units will be handled by RB_DepthOffset
	qglColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
	qglEnable( GL_DEPTH_TEST );
	qglPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
	qglFrontFace( GL_CCW );
	qglEnable( GL_SCISSOR_TEST );
	qglScissor( 0, 0, glConfig.width, glConfig.height );

	for( int tmu = 0; tmu < MAX_TEXTURE_UNITS; ++tmu ) {
		qglActiveTexture( GL_TEXTURE0 + tmu );

		qglBindTexture( GL_TEXTURE_CUBE_MAP, 0 );
		qglBindTexture( GL_TEXTURE_2D_ARRAY, 0 );
		qglBindTexture( GL_TEXTURE_3D, 0 );
		qglBindTexture( GL_TEXTURE_2D, 0 );
	}

	qglActiveTexture( GL_TEXTURE0 );

	qglBindBuffer( GL_ARRAY_BUFFER, 0 );
	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );

	qglUseProgram( 0 );
}

rserr_t R_Init( const char *applicationName, const char *screenshotPrefix, int startupColor,
				int iconResource, const int *iconXPM,
				void *hinstance, void *wndproc, void *parenthWnd,
				bool verbose ) {
	const char *dllname = "";
	qgl_initerr_t initerr;

	if( !applicationName ) {
		applicationName = "Qfusion";
	}
	if( !screenshotPrefix ) {
		screenshotPrefix = "";
	}
	(void)screenshotPrefix;

	cvar_t *gl_driver;
	const qgl_driverinfo_t *driver = QGL_GetDriverInfo();
	if( driver && driver->dllcvarname ) {
		gl_driver = Cvar_Get( driver->dllcvarname, driver->dllname, CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	} else {
		gl_driver = NULL;
	}

	memset( &glConfig, 0, sizeof( glConfig ) );

	// initialize our QGL dynamic bindings
	driver = QGL_GetDriverInfo();
	if( driver ) {
		dllname = driver->dllname;
	}

	init_qgl:
	initerr = QGL_Init( gl_driver ? gl_driver->string : dllname );
	if( initerr != qgl_initerr_ok ) {
		QGL_Shutdown();
		Com_Printf( "ref_gl::R_Init() - could not load \"%s\"\n", gl_driver ? gl_driver->string : dllname );

		if( ( initerr == qgl_initerr_invalid_driver ) && gl_driver && strcmp( gl_driver->string, dllname ) ) {
			Cvar_ForceSet( gl_driver->name, dllname );
			goto init_qgl;
		}

		return rserr_invalid_driver;
	}

	// initialize OS-specific parts of OpenGL
	if( !GLimp_Init( applicationName, hinstance, wndproc, parenthWnd, iconResource, iconXPM ) ) {
		QGL_Shutdown();
		return rserr_unknown;
	}

	// FIXME: move this elsewhere?
	glConfig.applicationName = Q_strdup( applicationName );
	glConfig.screenshotPrefix = Q_strdup( screenshotPrefix );
	glConfig.startupColor = startupColor;

	if( rserr_t err = VID_ApplyPendingMode( GLimp_SetMode ); err != rserr_ok ) {
		return err;
	}

	if( QGL_PostInit() != qgl_initerr_ok ) {
		return rserr_unknown;
	}

	glConfig.hwGamma = GLimp_GetGammaRamp( GAMMARAMP_STRIDE, &glConfig.gammaRampSize, glConfig.originalGammaRamp );

	/*
	** get our various GL strings
	*/
	glConfig.vendorString = (const char *)qglGetString( GL_VENDOR );
	glConfig.rendererString = (const char *)qglGetString( GL_RENDERER );
	glConfig.versionString = (const char *)qglGetString( GL_VERSION );
	glConfig.shadingLanguageVersionString = (const char *)qglGetString( GL_SHADING_LANGUAGE_VERSION );

	if( !glConfig.vendorString ) {
		glConfig.vendorString = "";
	}
	if( !glConfig.rendererString ) {
		glConfig.rendererString = "";
	}
	if( !glConfig.versionString ) {
		glConfig.versionString = "";
	}
	if( !glConfig.shadingLanguageVersionString ) {
		glConfig.shadingLanguageVersionString = "";
	}

	memset( &rsh, 0, sizeof( rsh ) );
	memset( &rf, 0, sizeof( rf ) );

	rsh.registrationSequence = 1;
	rsh.registrationOpen = false;

	rsh.worldModelSequence = 1;

	for( int i = 0; i < 256; i++ ) {
		rsh.sinTableByte[i] = std::sin( (float)i / 255.0 * M_TWOPI );
	}

	rf.frameTime.average = 1;
	rf.swapInterval = -1;

	if( !R_RegisterGLExtensions() ) {
		QGL_Shutdown();
		return rserr_unknown;
	}

	R_SetSwapInterval( 0, -1 );

	R_FillStartupBackgroundColor( COLOR_R( glConfig.startupColor ) / 255.0f,
								  COLOR_G( glConfig.startupColor ) / 255.0f, COLOR_B( glConfig.startupColor ) / 255.0f );

	// load and compile GLSL programs
	RP_Init();

	R_InitVBO();

	TextureCache::init();

	TextureCache::instance()->applyFilter( v_textureFilter.get(), v_anisoLevel.get() );

	MaterialCache::init();

	R_InitModels();

	wsw::RendererFrontend::init();

	R_ClearSkeletalCache();

	R_InitVolatileAssets();

	// TODO: This could fail depending of resolution
	TextureCache::instance()->createPrimaryRenderTargetAttachments();

	return rserr_ok;
}

rserr_t R_TrySettingMode( int x, int y, int width, int height, int displayFrequency, const VidModeOptions &options ) {
	// If the fullscreen flag is the single difference, choose the lightweight path
	if( glConfig.width == width && glConfig.height == height ) {
		if( glConfig.fullScreen != options.fullscreen ) {
			return GLimp_SetFullscreenMode( displayFrequency, options.fullscreen );
		}
	}

	TextureCache::instance()->releasePrimaryRenderTargetAttachments();

	rserr_t err = GLimp_SetMode( x, y, width, height, displayFrequency, options );
	if( err != rserr_ok ) {
		rError() << "Could not GLimp_SetMode()";
	}

	if( err != rserr_ok ) {
		return err;
	}

	TextureCache::instance()->createPrimaryRenderTargetAttachments();

	//
	// TODO
	//
	// R_BindFrameBufferObject( 0 );
	//

	return rserr_ok;
}

MeshBuffer *R_InitNullModelVBO( BufferCache *bufferCache ) {
	const vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	MeshBuffer *const buffer = bufferCache->createMeshBuffer( 6, 6, 0, vattribs, VBO_TAG_NONE, vattribs );
	if( buffer ) {
		vec4_t xyz[6];
		byte_vec4_t colors[6];

		constexpr float scale = 15;
		Vector4Set( xyz[0], 0, 0, 0, 1 );
		Vector4Set( xyz[1], scale, 0, 0, 1 );
		Vector4Set( colors[0], 255, 0, 0, 127 );
		Vector4Set( colors[1], 255, 0, 0, 127 );

		Vector4Set( xyz[2], 0, 0, 0, 1 );
		Vector4Set( xyz[3], 0, scale, 0, 1 );
		Vector4Set( colors[2], 0, 255, 0, 127 );
		Vector4Set( colors[3], 0, 255, 0, 127 );

		Vector4Set( xyz[4], 0, 0, 0, 1 );
		Vector4Set( xyz[5], 0, 0, scale, 1 );
		Vector4Set( colors[4], 0, 0, 255, 127 );
		Vector4Set( colors[5], 0, 0, 255, 127 );

		vec4_t normals[6] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
		vec2_t texcoords[6] = { {0,0}, {0,1}, {0,0}, {0,1}, {0,0}, {0,1} };
		elem_t elems[6] = { 0, 1, 2, 3, 4, 5 };

		mesh_t mesh;
		memset( &mesh, 0, sizeof( mesh ) );
		mesh.numVerts = 6;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;
		mesh.numElems = 6;
		mesh.elems = elems;

		const VboSpanLayout *layout = bufferCache->getLayoutForBuffer( buffer );
		bufferCache->getUnderlyingFactory()->uploadVertexData( buffer, layout, 0, vattribs, &mesh );
		bufferCache->getUnderlyingFactory()->uploadIndexData( buffer, 0, 0, &mesh );
	}

	return buffer;
}

MeshBuffer *R_InitPostProcessingVBO( BufferCache *bufferCache ) {
	const vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT;
	MeshBuffer *const buffer = bufferCache->createMeshBuffer( 4, 6, 0, vattribs, VBO_TAG_NONE, vattribs );
	if( buffer ) {
		vec2_t texcoords[4] = { {0,1}, {1,1}, {1,0}, {0,0} };
		elem_t elems[6] = kQuadIndicesInitializer;
		vec4_t xyz[4] = { {0,0,0,1}, {1,0,0,1}, {1,1,0,1}, {0,1,0,1} };

		mesh_t mesh;
		memset( &mesh, 0, sizeof( mesh ) );
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.stArray = texcoords;
		mesh.numElems = 6;
		mesh.elems = elems;

		const VboSpanLayout *layout = bufferCache->getLayoutForBuffer( buffer );
		bufferCache->getUnderlyingFactory()->uploadVertexData( buffer, layout, 0, vattribs, &mesh );
		bufferCache->getUnderlyingFactory()->uploadIndexData( buffer, 0, 0, &mesh );
	}

	return buffer;
}

static void R_InitVolatileAssets() {
	// init volatile data
	R_InitSkeletalCache();
	R_InitCustomColors();

	wsw::RendererFrontend::instance()->initVolatileAssets();

	auto *materialCache = MaterialCache::instance();
	rsh.envShader = materialCache->loadDefaultMaterial( wsw::StringView( "$environment" ), SHADER_TYPE_OPAQUE_ENV );
	rsh.whiteShader = materialCache->loadDefaultMaterial( wsw::StringView( "$whiteimage" ), SHADER_TYPE_2D );
	rsh.emptyFogShader = materialCache->loadDefaultMaterial( wsw::StringView( "$emptyfog" ), SHADER_TYPE_FOG );

	BufferCache *bufferCache = getBufferCache();

	if( !rsh.nullVBO ) {
		rsh.nullVBO = R_InitNullModelVBO( bufferCache );
	} else {
		bufferCache->touchMeshBuffer( rsh.nullVBO );
	}

	if( !rsh.postProcessingVBO ) {
		rsh.postProcessingVBO = R_InitPostProcessingVBO( bufferCache );
	} else {
		bufferCache->touchMeshBuffer( rsh.postProcessingVBO );
	}
}

static void R_DestroyVolatileAssets() {
	wsw::RendererFrontend::instance()->destroyVolatileAssets();

	// kill volatile data
	R_ShutdownCustomColors();
	R_ShutdownSkeletalCache();
}

void R_BeginRegistration_( void ) {
	R_DestroyVolatileAssets();

	rsh.registrationSequence++;
	if( !rsh.registrationSequence ) {
		// make sure assumption that an asset is free it its registrationSequence is 0
		// since rsh.registrationSequence never equals 0
		rsh.registrationSequence = 1;
	}
	rsh.registrationOpen = true;

	R_InitVolatileAssets();
}

void R_EndRegistration_( void ) {
	if( rsh.registrationOpen ) {
		rsh.registrationOpen = false;

		R_FreeUnusedModels();
		getBufferCache()->freeUnusedBuffers();

		MaterialCache::instance()->freeUnusedObjects();

		TextureCache::instance()->freeAllUnusedTextures();
	}
}

void R_Shutdown_( bool verbose ) {
	R_DestroyVolatileAssets();

	wsw::RendererFrontend::shutdown();

	R_ShutdownModels();

	R_ShutdownVBO();

	MaterialCache::shutdown();

	TextureCache::shutdown();

	// destroy compiled GLSL programs
	RP_Shutdown();

	// restore original gamma
	if( glConfig.hwGamma ) {
		GLimp_SetGammaRamp( GAMMARAMP_STRIDE, glConfig.gammaRampSize, glConfig.originalGammaRamp );
	}

	// shut down OS specific OpenGL stuff like contexts, etc.
	GLimp_Shutdown();

	// shutdown our QGL subsystem
	QGL_Shutdown();
}

#ifdef QGL_USE_CALL_WRAPPERS

QGLFunc *QGLFunc::listHead = nullptr;

#ifdef QGL_VALIDATE_CALLS

const char *QGLFunc::checkForError() {
	// Never try to fetch errors for qglGetError itself
	if ( !strcmp( name, "qglGetError" ) ) {
		return nullptr;
	}

	if( strstr( name, "UniformLocation" ) ) {
		return nullptr;
	}

	// Hacks: never try to fetch errors for qglBufferData().
	// This is currently the only routine that has an additional custom error handling logic.
	// We could try using something like `qglBufferData.unchecked().operator()(...`
	// but this loses a structural compatibility with plain (unwrapped functions) code
	if ( !strcmp( name, "qglBufferData" ) ) {
		return nullptr;
	}

	// Get the underlying raw function pointer
	typedef GLenum ( APIENTRY *GetErrorFn )();
	switch( ( (GetErrorFn)qglGetError.address )() ) {
		case GL_NO_ERROR:
			return nullptr;
		case GL_INVALID_ENUM:
			return "GL_INVALID_ENUM";
		case GL_INVALID_VALUE:
			return "GL_INVALID_VALUE";
		case GL_INVALID_OPERATION:
			return "GL_INVALID_OPERATION";
		case GL_INVALID_FRAMEBUFFER_OPERATION:
			return "GL_INVALID_FRAMEBUFFER_OPERATION";
		case GL_OUT_OF_MEMORY:
			return "GL_OUT_OF_MEMORY";
		case GL_STACK_UNDERFLOW:
			return "GL_STACK_UNDERFLOW";
		case GL_STACK_OVERFLOW:
			return "GL_STACK_OVERFLOW";
		default:
			return "UNKNOWN";
	}
}

#endif
#endif