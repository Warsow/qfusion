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
#include "materiallocal.h"
#include "frontend.h"
#include <common/facilities/sysclock.h>
#include <common/helpers/scopeexitaction.h>
#include <common/facilities/cvar.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/profilerscope.h>
#include <common/facilities/syspublic.h>
#include <common/common.h>

r_globals_t rf;

mapconfig_t mapConfig;

lightstyle_t lightStyles[MAX_LIGHTSTYLES];

glconfig_t glConfig;

r_shared_t rsh;

cvar_t *r_norefresh;
cvar_t *r_drawentities;
cvar_t *r_drawworld;
cvar_t *r_speeds;
cvar_t *r_drawelements;
cvar_t *r_fullbright;
cvar_t *r_lightmap;
cvar_t *r_novis;
cvar_t *r_nocull;
cvar_t *r_lerpmodels;
cvar_t *r_brightness;
cvar_t *r_sRGB;

cvar_t *r_dynamiclight;
cvar_t *r_detailtextures;
cvar_t *r_subdivisions;
cvar_t *r_showtris;
cvar_t *r_shownormals;
cvar_t *r_draworder;

cvar_t *r_fastsky;
cvar_t *r_portalonly;
cvar_t *r_portalmaps;
cvar_t *r_portalmaps_maxtexsize;

cvar_t *r_lighting_deluxemapping;
cvar_t *r_lighting_specular;
cvar_t *r_lighting_glossintensity;
cvar_t *r_lighting_glossexponent;
cvar_t *r_lighting_ambientscale;
cvar_t *r_lighting_directedscale;
cvar_t *r_lighting_packlightmaps;
cvar_t *r_lighting_maxlmblocksize;
cvar_t *r_lighting_vertexlight;
cvar_t *r_lighting_maxglsldlights;
cvar_t *r_lighting_grayscale;
cvar_t *r_lighting_intensity;

cvar_t *r_offsetmapping;
cvar_t *r_offsetmapping_scale;
cvar_t *r_offsetmapping_reliefmapping;

cvar_t *r_shadows;
cvar_t *r_shadows_alpha;
cvar_t *r_shadows_nudge;
cvar_t *r_shadows_projection_distance;
cvar_t *r_shadows_maxtexsize;
cvar_t *r_shadows_pcf;
cvar_t *r_shadows_self_shadow;
cvar_t *r_shadows_dither;

cvar_t *r_outlines_world;
cvar_t *r_outlines_scale;
cvar_t *r_outlines_cutoff;

cvar_t *r_soft_particles;
cvar_t *r_soft_particles_scale;

cvar_t *r_hdr;
cvar_t *r_hdr_gamma;
cvar_t *r_hdr_exposure;

cvar_t *r_bloom;

cvar_t *r_fxaa;
cvar_t *r_samples;

cvar_t *r_lodbias;
cvar_t *r_lodscale;

cvar_t *r_stencilbits;
cvar_t *r_gamma;
cvar_t *r_texturefilter;
cvar_t *r_anisolevel;
cvar_t *r_texturecompression;
cvar_t *r_picmip;
cvar_t *r_polyblend;
cvar_t *r_lockpvs;
cvar_t *r_screenshot_fmtstr;
cvar_t *r_screenshot_jpeg;
cvar_t *r_screenshot_jpeg_quality;
cvar_t *r_swapinterval;
cvar_t *r_swapinterval_min;

cvar_t *r_temp1;

cvar_t *r_drawflat;
cvar_t *r_wallcolor;
cvar_t *r_floorcolor;

cvar_t *r_usenotexture;

cvar_t *r_maxglslbones;

cvar_t *gl_driver;
cvar_t *gl_cull;
cvar_t *r_multithreading;

cvar_t *r_showShaderCache;

extern cvar_t *cl_multithreading;

static bool r_verbose;
static bool r_postinit;

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

void R_InitCustomColors( void ) {
	memset( rsh.customColors, 255, sizeof( rsh.customColors ) );
}

void R_SetCustomColor( int num, int r, int g, int b ) {
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

mesh_vbo_t *R_InitNullModelVBO( void ) {
	const vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	mesh_vbo_s *vbo = R_CreateMeshVBO( &rf, 6, 6, 0, vattribs, VBO_TAG_NONE, vattribs );
	if( !vbo ) {
		return NULL;
	}

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

	R_UploadVBOVertexData( vbo, 0, vattribs, &mesh );
	R_UploadVBOElemData( vbo, 0, 0, &mesh );

	return vbo;
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
	if( externalTexNum == 0 ) {
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

shader_s *R_WrapMenuTextureHandleInMaterial( unsigned externalTexNum ) {
	return R_WrapExternalTextureHandle( externalTexNum, 0 );
}

shader_s *R_WrapHudTextureHandleInMaterial( unsigned externalTexNum ) {
	return R_WrapExternalTextureHandle( externalTexNum, 1 );
}

static shader_s g_miniviewMaterialStorage;
static shaderpass_t g_miniviewMaterialPassStorage;

static const wsw::HashedStringView kExternalMiniviewImage( "$miniviewimage" );

shader_s *R_WrapMiniviewRenderTargetInMaterial( RenderTargetComponents *renderTarget ) {
	// Currently all miniview render targets are shared
	assert( renderTarget == GetMiniviewRenderTarget() );

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

mesh_vbo_t *R_InitPostProcessingVBO( void ) {
	const vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT;
	mesh_vbo_t *vbo = R_CreateMeshVBO( &rf, 4, 6, 0, vattribs, VBO_TAG_NONE, vattribs );
	if( !vbo ) {
		return NULL;
	}

	vec2_t texcoords[4] = { {0,1}, {1,1}, {1,0}, {0,0} };
	elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
	vec4_t xyz[4] = { {0,0,0,1}, {1,0,0,1}, {1,1,0,1}, {0,1,0,1} };

	mesh_t mesh;
	memset( &mesh, 0, sizeof( mesh ) );
	mesh.numVerts = 4;
	mesh.xyzArray = xyz;
	mesh.stArray = texcoords;
	mesh.numElems = 6;
	mesh.elems = elems;

	R_UploadVBOVertexData( vbo, 0, vattribs, &mesh );
	R_UploadVBOElemData( vbo, 0, 0, &mesh );

	return vbo;
}

void R_Finish() {
	qglFinish();
}

void R_Flush( void ) {
	qglFlush();
}

void R_DeferDataSync( void ) {
	if( !rsh.registrationOpen ) {
		rf.dataSync = true;
		qglFlush();
		RB_FlushTextureCache();
	}
}

void R_DataSync( void ) {
	if( rf.dataSync ) {
		rf.dataSync = false;
	}
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

void R_SetWallFloorColors( const vec3_t wallColor, const vec3_t floorColor ) {
	for( unsigned i = 0; i < 3; i++ ) {
		rsh.wallColor[i] = bound( 0, floor( wallColor[i] ) / 255.0, 1.0 );
		rsh.floorColor[i] = bound( 0, floor( floorColor[i] ) / 255.0, 1.0 );
	}
}

void R_BeginFrame( bool forceClear, int swapInterval ) {
	WSW_PROFILER_SCOPE();

	GLimp_BeginFrame();

	RB_BeginFrame();

	qglDrawBuffer( GL_BACK );

	if( forceClear ) {
		RB_Clear( GL_COLOR_BUFFER_BIT, 0, 0, 0, 1 );
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

	R_Set2DMode( true );
}

void R_EndFrame( void ) {
	WSW_PROFILER_SCOPE();

	// render previously batched 2D geometry, if any
	RB_FlushDynamicMeshes();

	// reset the 2D state so that the mode will be
	// properly set back again in R_BeginFrame
	R_Set2DMode( false );

	RB_EndFrame();

	GLimp_EndFrame();

	//assert( qglGetError() == GL_NO_ERROR );
}

void BeginDrawingScenes() {
	WSW_PROFILER_SCOPE();
	wsw::ref::Frontend::instance()->beginDrawingScenes();
}

auto suggestNumExtraWorkerThreads( const SuggestNumWorkerThreadsArgs &args ) -> unsigned {
	if( cl_multithreading->integer ) {
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

static std::optional<TaskSystem::ExecutionHandle> g_taskSystemExecutionHandle;

TaskSystem *BeginProcessingOfTasks() {
	WSW_PROFILER_SCOPE();

	auto *result = wsw::ref::Frontend::instance()->getTaskSystem();
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

void EndProcessingOfTasks() {
	WSW_PROFILER_SCOPE();

	const bool awaitResult = wsw::ref::Frontend::instance()->getTaskSystem()->awaitCompletion( g_taskSystemExecutionHandle.value() );
	g_taskSystemExecutionHandle = std::nullopt;
	if( !awaitResult ) {
		wsw::failWithLogicError( "Failed to execute rendering tasks" );
	}
}

RenderTargetComponents *GetMiniviewRenderTarget() {
	return wsw::ref::Frontend::instance()->getMiniviewRenderTarget();
}

unsigned GetMiniviewRenderTargetTexture() {
	return wsw::ref::Frontend::instance()->getMiniviewRenderTarget()->texture->texnum;
}

DrawSceneRequest *CreateDrawSceneRequest( const refdef_t &refdef ) {
	WSW_PROFILER_SCOPE();
	return wsw::ref::Frontend::instance()->createDrawSceneRequest( refdef );
}

TaskHandle BeginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) {
	return wsw::ref::Frontend::instance()->beginProcessingDrawSceneRequests( requests );
}

TaskHandle EndProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests, std::span<const TaskHandle> dependencies ) {
	return wsw::ref::Frontend::instance()->endProcessingDrawSceneRequests( requests, dependencies );
}

void CommitProcessedDrawSceneRequest( DrawSceneRequest *request ) {
	WSW_PROFILER_SCOPE();
	wsw::ref::Frontend::instance()->commitProcessedDrawSceneRequest( request );
}

void EndDrawingScenes() {
	WSW_PROFILER_SCOPE();
	wsw::ref::Frontend::instance()->endDrawingScenes();
}

Draw2DRequest *CreateDraw2DRequest() {
	return wsw::ref::Frontend::instance()->createDraw2DRequest();
}

void CommitDraw2DRequest( Draw2DRequest *request ) {
	wsw::ref::Frontend::instance()->commitDraw2DRequest( request );
}

void RecycleDraw2DRequest( Draw2DRequest *request ) {
	wsw::ref::Frontend::instance()->recycleDraw2DRequest( request );
}

[[nodiscard]]
static auto coPrepareDrawSceneRequest( CoroTask::StartInfo si, DrawSceneRequest *drawSceneRequest ) -> CoroTask {
	co_await si.taskSystem->awaiterOf( BeginProcessingDrawSceneRequests( { &drawSceneRequest, 1 } ) );
	co_await si.taskSystem->awaiterOf( EndProcessingDrawSceneRequests( { &drawSceneRequest, 1 }, {} ) );
}

void ExecuteSingleDrawSceneRequestNonSpeedCritical( DrawSceneRequest *request ) {
	do {
		TaskSystem *taskSystem = BeginProcessingOfTasks();
		[[maybe_unused]] volatile wsw::ScopeExitAction callEndProcessingOfTasks( &EndProcessingOfTasks );
		(void)taskSystem->addCoro( [=]() {
			return coPrepareDrawSceneRequest( { taskSystem, {}, CoroTask::OnlyMainThread }, request );
		});
	} while( false );
	CommitProcessedDrawSceneRequest( request );
}

void RF_AppActivate( bool active, bool minimize, bool destroy ) {
	R_Flush();
	GLimp_AppActivate( active, minimize, destroy );
}

void RF_Shutdown( bool verbose ) {
	if( TextureCache *instance = TextureCache::maybeInstance() ) {
		instance->releasePrimaryRenderTargetAttachments();
	}

	RB_Shutdown();

	R_Shutdown_( verbose );
}

static void RF_CheckCvars( void ) {
	// update gamma
	if( r_gamma->modified ) {
		r_gamma->modified = false;
		R_SetGamma( r_gamma->value );
	}

	if( r_texturefilter->modified || r_anisolevel->modified ) {
		r_texturefilter->modified = false;
		r_anisolevel->modified = false;
		TextureCache::instance()->applyFilter( wsw::StringView( r_texturefilter->string ), r_anisolevel->integer );
	}

	if( r_wallcolor->modified || r_floorcolor->modified ) {
		vec3_t wallColor, floorColor;

		sscanf( r_wallcolor->string,  "%3f %3f %3f", &wallColor[0], &wallColor[1], &wallColor[2] );
		sscanf( r_floorcolor->string, "%3f %3f %3f", &floorColor[0], &floorColor[1], &floorColor[2] );

		r_wallcolor->modified = r_floorcolor->modified = false;

		R_SetWallFloorColors( wallColor, floorColor );
	}

	// keep r_outlines_cutoff value in sane bounds to prevent wallhacking
	if( r_outlines_scale->modified ) {
		if( r_outlines_scale->value < 0 ) {
			Cvar_ForceSet( r_outlines_scale->name, "0" );
		} else if( r_outlines_scale->value > 3 ) {
			Cvar_ForceSet( r_outlines_scale->name, "3" );
		}
		r_outlines_scale->modified = false;
	}
}

void RF_BeginFrame( bool forceClear, bool forceVsync, bool uncappedFPS ) {
	RF_CheckCvars();

	R_DataSync();

	int swapInterval = r_swapinterval->integer || forceVsync ? 1 : 0;
	clamp_low( swapInterval, r_swapinterval_min->integer );

	R_BeginFrame( forceClear, swapInterval );
}

void RF_EndFrame( void ) {
	R_EndFrame();
}

void RF_BeginRegistration( void ) {
	// sync to the backend thread to ensure it's not using old assets for drawing
	R_BeginRegistration_();

	R_DeferDataSync();
	R_DataSync();

	RB_BeginRegistration();
}

void RF_EndRegistration( void ) {
	R_EndRegistration_();

	R_DeferDataSync();
	R_DataSync();

	RB_EndRegistration();
}

const char *RF_GetSpeedsMessage( char *out, size_t size ) {
	Q_strncpyz( out, rf.speedsMsg, size );
	return out;
}

int RF_GetAverageFrametime( void ) {
	return rf.frameTime.average;
}

bool RF_TransformVectorToViewport( const refdef_t *rd, const vec3_t in, vec2_t out ) {
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

bool RF_LerpTag( orientation_t *orient, const model_t *mod, int oldframe, int frame, float lerpfrac, const char *name ) {
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

static void R_FinalizeGLExtensions( void );

static void R_InitVolatileAssets( void );
static void R_DestroyVolatileAssets( void );

//=======================================================================

#define GLINF_FOFS( x ) offsetof( glextinfo_t,x )
#define GLINF_EXMRK() GLINF_FOFS( _extMarker )
#define GLINF_FROM( from,ofs ) ( *( (char *)from + ofs ) )

typedef struct {
	const char *name;               // constant pointer to constant string
	void **pointer;                 // constant pointer to function's pointer (function itself)
} gl_extension_func_t;

typedef struct {
	const char * prefix;            // constant pointer to constant string
	const char * name;
	const char * cvar_default;
	bool cvar_readonly;
	bool mandatory;
	gl_extension_func_t *funcs;     // constant pointer to array of functions
	size_t offset;                  // offset to respective variable
	size_t depOffset;               // offset to required pre-initialized variable
} gl_extension_t;

#define GL_EXTENSION_FUNC_EXT( name,func ) { name, (void ** const)func }
#define GL_EXTENSION_FUNC( name ) GL_EXTENSION_FUNC_EXT( "gl"#name,&( qgl ## name ) )

/* GL_ARB_get_program_binary */
static const gl_extension_func_t gl_ext_get_program_binary_ARB_funcs[] =
	{
		GL_EXTENSION_FUNC( ProgramParameteri )
		,GL_EXTENSION_FUNC( GetProgramBinary )
		,GL_EXTENSION_FUNC( ProgramBinary )

		,GL_EXTENSION_FUNC_EXT( NULL,NULL )
	};

#ifndef USE_SDL2

#ifdef GLX_VERSION

/* GLX_SGI_swap_control */
static const gl_extension_func_t glx_ext_swap_control_SGI_funcs[] =
{
	GL_EXTENSION_FUNC_EXT( "glXSwapIntervalSGI",&qglXSwapIntervalSGI )

	,GL_EXTENSION_FUNC_EXT( NULL,NULL )
};

#endif

#endif // USE_SDL2

#undef GL_EXTENSION_FUNC
#undef GL_EXTENSION_FUNC_EXT

//=======================================================================

#define GL_EXTENSION_EXT( pre,name,val,ro,mnd,funcs,dep ) { #pre, #name, #val, ro, mnd, (gl_extension_func_t * const)funcs, GLINF_FOFS( name ), GLINF_FOFS( dep ) }
#define GL_EXTENSION( pre,name,ro,mnd,funcs ) GL_EXTENSION_EXT( pre,name,1,ro,mnd,funcs,_extMarker )

//
// OpenGL extensions list
//
// short notation: vendor, name, default value, list of functions
// extended notation: vendor, name, default value, list of functions, required extension
static const gl_extension_t gl_extensions_decl[] =
	{
		GL_EXTENSION( ARB, get_program_binary, false, false, &gl_ext_get_program_binary_ARB_funcs )
		,GL_EXTENSION( ARB, ES3_compatibility, false, false, NULL )
		,GL_EXTENSION( EXT, texture_array, false, false, NULL )
		,GL_EXTENSION( ARB, gpu_shader5, false, false, NULL )

		// memory info
		,GL_EXTENSION( NVX, gpu_memory_info, true, false, NULL )
		,GL_EXTENSION( ATI, meminfo, true, false, NULL )

		,GL_EXTENSION( EXT, texture_filter_anisotropic, true, false, NULL )
		,GL_EXTENSION( EXT, bgra, true, false, NULL )

#ifndef USE_SDL2
		#ifdef GLX_VERSION
	,GL_EXTENSION( GLX_SGI, swap_control, true, false, &glx_ext_swap_control_SGI_funcs )
#endif
#endif
	};

static const int num_gl_extensions = sizeof( gl_extensions_decl ) / sizeof( gl_extensions_decl[0] );

#undef GL_EXTENSION
#undef GL_EXTENSION_EXT

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

static bool isPlatformExtensionSupported( const char *ext ) {
	return strstr( qglGetGLWExtensionsString(), ext ) != nullptr;
}

static bool R_RegisterGLExtensions( void ) {
	memset( &glConfig.ext, 0, sizeof( glextinfo_t ) );

	for( int i = 0; i < num_gl_extensions; i++ ) {
		const gl_extension_t *const extension = &gl_extensions_decl[i];

		char name[128];
		Q_snprintfz( name, sizeof( name ), "gl_ext_%s", extension->name );

		// register a cvar and check if this extension is explicitly disabled
		cvar_flag_t cvar_flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO;
#ifdef PUBLIC_BUILD
		if( extension->cvar_readonly ) {
			cvar_flags |= CVAR_READONLY;
		}
#endif

		cvar_t *cvar = Cvar_Get( name, extension->cvar_default ? extension->cvar_default : "0", cvar_flags );
		if( !cvar->integer ) {
			continue;
		}

		char *var;
		// an alternative extension of higher priority is available so ignore this one
		var = &( GLINF_FROM( &glConfig.ext, extension->offset ) );
		if( *var ) {
			continue;
		}

		// required extension is not available, ignore
		if( extension->depOffset != GLINF_EXMRK() && !GLINF_FROM( &glConfig.ext, extension->depOffset ) ) {
			continue;
		}

		// let's see what the driver's got to say about this...
		if( *extension->prefix ) {
			auto testFunc = isExtensionSupported;
			for( const char *prefix : { "WGL", "GLX", "EGL" } ) {
				if( !strncmp( extension->prefix, prefix, 3 ) ) {
					testFunc = isPlatformExtensionSupported;
					break;
				}
			}

			Q_snprintfz( name, sizeof( name ), "%s_%s", extension->prefix, extension->name );
			if( !testFunc( name ) ) {
				continue;
			}
		}

		// initialize function pointers
		const auto *func = extension->funcs;
		if( func ) {
			do {
				*( func->pointer ) = ( void * )qglGetProcAddress( (const GLubyte *)func->name );
				if( !*( func->pointer ) ) {
					break;
				}
			} while( ( ++func )->name );

			// some function is missing
			if( func->name ) {
				gl_extension_func_t *func2 = extension->funcs;

				// whine about buggy driver
				if( *extension->prefix ) {
					rWarning() << "Broken" << wsw::StringView( cvar->name ) << "support, contact your video card vendor";
				}

				// reset previously initialized functions back to NULL
				do {
					*( func2->pointer ) = NULL;
				} while( ( ++func2 )->name && func2 != func );

				continue;
			}
		}

		// mark extension as available
		*var = true;

	}

	for( int i = 0; i < num_gl_extensions; i++ ) {
		auto *extension = &gl_extensions_decl[i];
		if( !extension->mandatory ) {
			continue;
		}

		char *var;
		var = &( GLINF_FROM( &glConfig.ext, extension->offset ) );

		if( !*var ) {
			Sys_Error( "R_RegisterGLExtensions: '%s_%s' is not available, aborting\n",
					   extension->prefix, extension->name );
			return false;
		}
	}

	R_FinalizeGLExtensions();
	return true;
}

static void R_PrintGLExtensionsInfo( void ) {
	int i;
	size_t lastOffset;
	const gl_extension_t *extension;

	for( i = 0, lastOffset = 0, extension = gl_extensions_decl; i < num_gl_extensions; i++, extension++ ) {
		if( lastOffset != extension->offset ) {
			lastOffset = extension->offset;
			if( GLINF_FROM( &glConfig.ext, lastOffset ) ) {
				rNotice() << wsw::StringView( extension->name ) << "enabled";
			} else {
				rNotice() << wsw::StringView( extension->name ) << "disabled";
			}
		}
	}
}

static void R_FinalizeGLExtensions( void ) {
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
	if( isExtensionSupported( "GL_EXT_texture_filter_anisotropic" ) ) {
		qglGetIntegerv( GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, &glConfig.maxTextureFilterAnisotropic );
	}

	/* GL_EXT_texture3D and GL_EXT_texture_array */
	qglGetIntegerv( GL_MAX_3D_TEXTURE_SIZE, &glConfig.maxTexture3DSize );

	glConfig.maxTextureLayers = 0;
	if( isExtensionSupported( "GL_EXT_texture_array" ) ) {
		glConfig.ext.texture_array = true;
		qglGetIntegerv( GL_MAX_ARRAY_TEXTURE_LAYERS_EXT, &glConfig.maxTextureLayers );
	}

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
	if( r_maxglslbones->integer > MAX_GLSL_UNIFORM_BONES ) {
		Cvar_ForceSet( r_maxglslbones->name, r_maxglslbones->dvalue );
	}

	// require GLSL 1.20+ for GPU skinning
	if( glConfig.shadingLanguageVersion >= 120 ) {
		// the maximum amount of bones we can handle in a vertex shader (2 vec4 uniforms per vertex)
		glConfig.maxGLSLBones = bound( 0, glConfig.maxVertexUniformComponents / 8 - 19, r_maxglslbones->integer );
	} else {
		glConfig.maxGLSLBones = 0;
	}

	glConfig.depthEpsilon = 1.0 / ( 1 << 22 );

	glConfig.sSRGB = false;

	cvar_t *cvar = Cvar_Get( "gl_ext_vertex_buffer_object_hack", "0", CVAR_ARCHIVE | CVAR_NOSET );
	if( cvar && !cvar->integer ) {
		Cvar_ForceSet( cvar->name, "1" );
		Cvar_ForceSet( "gl_ext_vertex_buffer_object", "1" );
	}

	qglGetIntegerv( GL_MAX_SAMPLES, &glConfig.maxFramebufferSamples );

	glConfig.maxObjectLabelLen = 0;
	if( qglObjectLabel ) {
		static_assert( sizeof( int ) == sizeof( glConfig.maxObjectLabelLen ) );
		qglGetIntegerv( GL_MAX_LABEL_LENGTH, (int *)&glConfig.maxObjectLabelLen );
	}

	Cvar_Get( "r_anisolevel_max", "0", CVAR_READONLY );
	Cvar_ForceSet( "r_anisolevel_max", va_r( tmp, sizeof( tmp ), "%i", glConfig.maxTextureFilterAnisotropic ) );

	Cvar_Get( "r_soft_particles_available", "1", CVAR_READONLY );

	// don't allow too high values for lightmap block size as they negatively impact performance
	if( r_lighting_maxlmblocksize->integer > glConfig.maxTextureSize / 4 &&
		!( glConfig.maxTextureSize / 4 < wsw::min( QF_LIGHTMAP_WIDTH,QF_LIGHTMAP_HEIGHT ) * 2 ) ) {
		Cvar_ForceSet( "r_lighting_maxlmblocksize", va_r( tmp, sizeof( tmp ), "%i", glConfig.maxTextureSize / 4 ) );
	}
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

static void R_Register( const char *screenshotsPrefix ) {
	char tmp[128];

	r_norefresh = Cvar_Get( "r_norefresh", "0", 0 );
	r_fullbright = Cvar_Get( "r_fullbright", "0", CVAR_LATCH_VIDEO );
	r_lightmap = Cvar_Get( "r_lightmap", "0", 0 );
	r_drawentities = Cvar_Get( "r_drawentities", "1", CVAR_CHEAT );
	r_drawworld = Cvar_Get( "r_drawworld", "1", CVAR_CHEAT );
	r_novis = Cvar_Get( "r_novis", "0", 0 );
	r_nocull = Cvar_Get( "r_nocull", "0", 0 );
	r_lerpmodels = Cvar_Get( "r_lerpmodels", "1", 0 );
	r_speeds = Cvar_Get( "r_speeds", "0", 0 );
	r_drawelements = Cvar_Get( "r_drawelements", "1", 0 );
	r_showtris = Cvar_Get( "r_showtris", "0", CVAR_CHEAT );
	r_lockpvs = Cvar_Get( "r_lockpvs", "0", CVAR_CHEAT );
	r_picmip = Cvar_Get( "r_picmip", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_polyblend = Cvar_Get( "r_polyblend", "1", 0 );

	r_brightness = Cvar_Get( "r_brightness", "0", CVAR_ARCHIVE );
	r_sRGB = Cvar_Get( "r_sRGB", "0", CVAR_DEVELOPER | CVAR_LATCH_VIDEO );

	r_detailtextures = Cvar_Get( "r_detailtextures", "1", CVAR_ARCHIVE );

	r_dynamiclight = Cvar_Get( "r_dynamiclight", "-1", CVAR_ARCHIVE );
	r_subdivisions = Cvar_Get( "r_subdivisions", STR_TOSTR( SUBDIVISIONS_DEFAULT ), CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_shownormals = Cvar_Get( "r_shownormals", "0", CVAR_CHEAT );
	r_draworder = Cvar_Get( "r_draworder", "0", CVAR_CHEAT );

	r_fastsky = Cvar_Get( "r_fastsky", "0", CVAR_ARCHIVE );
	r_portalonly = Cvar_Get( "r_portalonly", "0", 0 );
	r_portalmaps = Cvar_Get( "r_portalmaps", "1", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_portalmaps_maxtexsize = Cvar_Get( "r_portalmaps_maxtexsize", "1024", CVAR_ARCHIVE );

	r_lighting_deluxemapping = Cvar_Get( "r_lighting_deluxemapping", "1", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_specular = Cvar_Get( "r_lighting_specular", "1", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_glossintensity = Cvar_Get( "r_lighting_glossintensity", "1.5", CVAR_ARCHIVE );
	r_lighting_glossexponent = Cvar_Get( "r_lighting_glossexponent", "24", CVAR_ARCHIVE );
	r_lighting_ambientscale = Cvar_Get( "r_lighting_ambientscale", "1", 0 );
	r_lighting_directedscale = Cvar_Get( "r_lighting_directedscale", "1", 0 );

	r_lighting_packlightmaps = Cvar_Get( "r_lighting_packlightmaps", "1", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_maxlmblocksize = Cvar_Get( "r_lighting_maxlmblocksize", "2048", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_vertexlight = Cvar_Get( "r_lighting_vertexlight", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_maxglsldlights = Cvar_Get( "r_lighting_maxglsldlights", "16", CVAR_ARCHIVE );
	r_lighting_grayscale = Cvar_Get( "r_lighting_grayscale", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_lighting_intensity = Cvar_Get( "r_lighting_intensity", "1.75", CVAR_ARCHIVE );

	r_offsetmapping = Cvar_Get( "r_offsetmapping", "2", CVAR_ARCHIVE );
	r_offsetmapping_scale = Cvar_Get( "r_offsetmapping_scale", "0.02", CVAR_ARCHIVE );
	r_offsetmapping_reliefmapping = Cvar_Get( "r_offsetmapping_reliefmapping", "0", CVAR_ARCHIVE );

#ifdef CGAMEGETLIGHTORIGIN
	r_shadows = Cvar_Get( "cg_shadows", "1", CVAR_ARCHIVE );
#else
	r_shadows = Cvar_Get( "r_shadows", "0", CVAR_ARCHIVE );
#endif
	r_shadows_alpha = Cvar_Get( "r_shadows_alpha", "0.7", CVAR_ARCHIVE );
	r_shadows_nudge = Cvar_Get( "r_shadows_nudge", "1", CVAR_ARCHIVE );
	r_shadows_projection_distance = Cvar_Get( "r_shadows_projection_distance", "64", CVAR_CHEAT );
	r_shadows_maxtexsize = Cvar_Get( "r_shadows_maxtexsize", "64", CVAR_ARCHIVE );
	r_shadows_pcf = Cvar_Get( "r_shadows_pcf", "1", CVAR_ARCHIVE );
	r_shadows_self_shadow = Cvar_Get( "r_shadows_self_shadow", "0", CVAR_ARCHIVE );
	r_shadows_dither = Cvar_Get( "r_shadows_dither", "0", CVAR_ARCHIVE );

	r_outlines_world = Cvar_Get( "r_outlines_world", "1.8", CVAR_ARCHIVE );
	r_outlines_scale = Cvar_Get( "r_outlines_scale", "1", CVAR_ARCHIVE );
	r_outlines_cutoff = Cvar_Get( "r_outlines_cutoff", "712", CVAR_ARCHIVE );

	r_soft_particles = Cvar_Get( "r_soft_particles", "1", CVAR_ARCHIVE );
	r_soft_particles_scale = Cvar_Get( "r_soft_particles_scale", "0.02", CVAR_ARCHIVE );

	r_hdr = Cvar_Get( "r_hdr", "1", CVAR_ARCHIVE );
	r_hdr_gamma = Cvar_Get( "r_hdr_gamma", "2.2", CVAR_ARCHIVE );
	r_hdr_exposure = Cvar_Get( "r_hdr_exposure", "1.0", CVAR_ARCHIVE );

	r_bloom = Cvar_Get( "r_bloom", "1", CVAR_ARCHIVE );

	r_fxaa = Cvar_Get( "r_fxaa", "0", CVAR_ARCHIVE );
	r_samples = Cvar_Get( "r_samples", "0", CVAR_ARCHIVE );

	r_lodbias = Cvar_Get( "r_lodbias", "0", CVAR_ARCHIVE );
	r_lodscale = Cvar_Get( "r_lodscale", "5.0", CVAR_ARCHIVE );

	r_gamma = Cvar_Get( "r_gamma", "1.0", CVAR_ARCHIVE );
	r_texturefilter = Cvar_Get( "r_texturefilter", "trilinear", CVAR_ARCHIVE );
	r_anisolevel = Cvar_Get( "r_anisolevel", "4", CVAR_ARCHIVE );
	r_texturecompression = Cvar_Get( "r_texturecompression", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	r_stencilbits = Cvar_Get( "r_stencilbits", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );

	r_screenshot_jpeg = Cvar_Get( "r_screenshot_jpeg", "1", CVAR_ARCHIVE );
	r_screenshot_jpeg_quality = Cvar_Get( "r_screenshot_jpeg_quality", "90", CVAR_ARCHIVE );
	r_screenshot_fmtstr = Cvar_Get( "r_screenshot_fmtstr", va_r( tmp, sizeof( tmp ), "%s%%y%%m%%d_%%H%%M%%S", screenshotsPrefix ), CVAR_ARCHIVE );

#if defined( GLX_VERSION ) && !defined( USE_SDL2 )
	r_swapinterval = Cvar_Get( "r_swapinterval", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
#else
	r_swapinterval = Cvar_Get( "r_swapinterval", "0", CVAR_ARCHIVE );
#endif
	r_swapinterval_min = Cvar_Get( "r_swapinterval_min", "0", CVAR_READONLY ); // exposes vsync support to UI

	r_temp1 = Cvar_Get( "r_temp1", "0", 0 );

	r_drawflat = Cvar_Get( "r_drawflat", "0", CVAR_ARCHIVE );
	r_wallcolor = Cvar_Get( "r_wallcolor", "128 80 192", CVAR_ARCHIVE );
	r_floorcolor = Cvar_Get( "r_floorcolor", "144 48 72", CVAR_ARCHIVE );

	// make sure we rebuild our 3D texture after vid_restart
	r_wallcolor->modified = r_floorcolor->modified = true;

	// set to 1 to enable use of the checkerboard texture for missing world and model images
	r_usenotexture = Cvar_Get( "r_usenotexture", "0", CVAR_ARCHIVE );

	r_maxglslbones = Cvar_Get( "r_maxglslbones", STR_TOSTR( MAX_GLSL_UNIFORM_BONES ), CVAR_LATCH_VIDEO );

	r_multithreading = Cvar_Get( "r_multithreading", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );

	r_showShaderCache = Cvar_Get( "r_showShaderCache", "1", CVAR_ARCHIVE );

	gl_cull = Cvar_Get( "gl_cull", "1", 0 );

	const qgl_driverinfo_t *driver = QGL_GetDriverInfo();
	if( driver && driver->dllcvarname ) {
		gl_driver = Cvar_Get( driver->dllcvarname, driver->dllname, CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
	} else {
		gl_driver = NULL;
	}
}

static void R_PrintInfo() {
	R_PrintGLExtensionsInfo();
}

rserr_t R_Init( const char *applicationName, const char *screenshotPrefix, int startupColor,
				int iconResource, const int *iconXPM,
				void *hinstance, void *wndproc, void *parenthWnd,
				bool verbose ) {
	const qgl_driverinfo_t *driver;
	const char *dllname = "";
	qgl_initerr_t initerr;

	r_verbose = verbose;

	r_postinit = true;

	if( !applicationName ) {
		applicationName = "Qfusion";
	}
	if( !screenshotPrefix ) {
		screenshotPrefix = "";
	}

	R_Register( screenshotPrefix );

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

	return rserr_ok;
}

static rserr_t R_PostInit( void ) {
	if( QGL_PostInit() != qgl_initerr_ok ) {
		return rserr_unknown;
	}

	glConfig.hwGamma = GLimp_GetGammaRamp( GAMMARAMP_STRIDE, &glConfig.gammaRampSize, glConfig.originalGammaRamp );
	if( glConfig.hwGamma ) {
		r_gamma->modified = true;
	}

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

	if( r_verbose ) {
		R_PrintInfo();
	}

	// load and compile GLSL programs
	RP_Init();

	R_InitVBO();

	TextureCache::init();

	TextureCache::instance()->applyFilter( wsw::StringView( r_texturefilter->string ), r_anisolevel->integer );

	MaterialCache::init();

	R_InitModels();

	wsw::ref::Frontend::init();

	R_ClearSkeletalCache();

	R_InitVolatileAssets();

	// TODO:......
	const GLenum glerr = qglGetError();
	if( glerr != GL_NO_ERROR ) {
		Com_Printf( "glGetError() = 0x%x\n", glerr );
	}

	return rserr_ok;
}

rserr_t R_TrySettingMode( int x, int y, int width, int height, int displayFrequency, const VidModeOptions &options ) {
	// If the fullscreen flag is the single difference, choose the lightweight path
	if( glConfig.width == width && glConfig.height == height ) {
		if( glConfig.fullScreen != options.fullscreen ) {
			return GLimp_SetFullscreenMode( displayFrequency, options.fullscreen );
		}
	}

	if( TextureCache *instance = TextureCache::maybeInstance() ) {
		instance->releasePrimaryRenderTargetAttachments();
	}

	RB_Shutdown();

	rserr_t err = GLimp_SetMode( x, y, width, height, displayFrequency, options );
	if( err != rserr_ok ) {
		rError() << "Could not GLimp_SetMode()";
	} else if( r_postinit ) {
		err = R_PostInit();
		r_postinit = false;
	}

	if( err != rserr_ok ) {
		return err;
	}

	RB_Init();

	TextureCache::instance()->createPrimaryRenderTargetAttachments();

	//
	// TODO
	//
	// R_BindFrameBufferObject( 0 );
	//

	return rserr_ok;
}

static void R_InitVolatileAssets() {
	// init volatile data
	R_InitSkeletalCache();
	R_InitCustomColors();

	wsw::ref::Frontend::instance()->initVolatileAssets();

	auto *materialCache = MaterialCache::instance();
	rsh.envShader = materialCache->loadDefaultMaterial( wsw::StringView( "$environment" ), SHADER_TYPE_OPAQUE_ENV );
	rsh.whiteShader = materialCache->loadDefaultMaterial( wsw::StringView( "$whiteimage" ), SHADER_TYPE_2D );
	rsh.emptyFogShader = materialCache->loadDefaultMaterial( wsw::StringView( "$emptyfog" ), SHADER_TYPE_FOG );

	if( !rsh.nullVBO ) {
		rsh.nullVBO = R_InitNullModelVBO();
	} else {
		R_TouchMeshVBO( rsh.nullVBO );
	}

	if( !rsh.postProcessingVBO ) {
		rsh.postProcessingVBO = R_InitPostProcessingVBO();
	} else {
		R_TouchMeshVBO( rsh.postProcessingVBO );
	}
}

static void R_DestroyVolatileAssets() {
	wsw::ref::Frontend::instance()->destroyVolatileAssets();

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

	R_DeferDataSync();

	R_DataSync();
}

void R_EndRegistration_( void ) {
	if( rsh.registrationOpen ) {
		rsh.registrationOpen = false;

		R_FreeUnusedModels();
		R_FreeUnusedVBOs();

		MaterialCache::instance()->freeUnusedObjects();

		TextureCache::instance()->freeAllUnusedTextures();

		R_DeferDataSync();

		R_DataSync();
	}
}

void R_Shutdown_( bool verbose ) {
	R_DestroyVolatileAssets();

	wsw::ref::Frontend::shutdown();

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