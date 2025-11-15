/*
Copyright (C) 2011 Victor Luchits

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
#include "program.h"
#include "backendactiontape.h"
#include "backendlocal.h"
#include "glstateproxy.h"

#include <common/helpers/noise.h>

#define RB_IsAlphaBlending( blendsrc,blenddst ) \
	( ( blendsrc ) == GLSTATE_SRCBLEND_SRC_ALPHA || ( blenddst ) == GLSTATE_DSTBLEND_SRC_ALPHA ) || \
	( ( blendsrc ) == GLSTATE_SRCBLEND_ONE_MINUS_SRC_ALPHA || ( blenddst ) == GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA )

static const shaderpass_t kBuiltinFogPass {
	.flags        = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA,
	.rgbgen       = { .type = RGB_GEN_FOG },
	.alphagen     = { .type = ALPHA_GEN_IDENTITY },
	.tcgen        = TC_GEN_FOG,
	.program_type = GLSL_PROGRAM_TYPE_FOG,
};

static const shaderpass_t kBuiltinShadowmapPass {
	.flags        = GLSTATE_DEPTHFUNC_EQ | GLSTATE_SRCBLEND_ZERO | GLSTATE_DSTBLEND_SRC_COLOR,
	.rgbgen       = { .type = RGB_GEN_IDENTITY },
	.alphagen     = { .type = ALPHA_GEN_IDENTITY },
	.tcgen        = TC_GEN_NONE,
	.program_type = GLSL_PROGRAM_TYPE_SHADOWMAP,
};

static const shaderpass_t kBuiltinOutlinePass {
	.flags        = GLSTATE_DEPTHWRITE,
	.rgbgen       = { .type = RGB_GEN_OUTLINE },
	.alphagen     = { .type = ALPHA_GEN_OUTLINE },
	.tcgen        = TC_GEN_NONE,
	.program_type = GLSL_PROGRAM_TYPE_OUTLINE,
};

static const shaderpass_t kBuiltinSkyboxPass {
	.rgbgen       = { .type = RGB_GEN_IDENTITY },
	.alphagen     = { .type = ALPHA_GEN_IDENTITY },
	.tcgen        = TC_GEN_BASE,
	.program_type = GLSL_PROGRAM_TYPE_Q3A_SHADER,
};

static void RB_SetShaderpassState( BackendState *backendState, unsigned passStateFlags );

static void RB_DrawMeshVerts( BackendState *backendState, const DrawMeshVertSpan *vertSpan, int primitive ) {
	// TODO: What's the purpose of v_drawElements
	if( !( v_drawElements.get() || backendState->material.currentEntity == &backendState->global.nullEnt ) ) [[unlikely]] {
		return;
	}

	backendState->gl.applyScissor();

	if( const auto *mdSpan = std::get_if<MultiDrawElemSpan>( vertSpan ) ) {
		backendState->gl.multiDrawElements( primitive, mdSpan->counts, GL_UNSIGNED_SHORT, mdSpan->indices, mdSpan->numDraws );
	} else if( const auto *vertElemSpan = std::get_if<VertElemSpan>( vertSpan ) ) {
		const unsigned numVerts  = vertElemSpan->numVerts;
		const unsigned numElems  = vertElemSpan->numElems;
		const unsigned firstVert = vertElemSpan->firstVert;
		const unsigned firstElem = vertElemSpan->firstElem;

		backendState->gl.drawRangeElements( primitive, firstVert, firstVert + numVerts - 1, (int)numElems,
											GL_UNSIGNED_SHORT, (GLvoid *)( firstElem * sizeof( elem_t ) ) );
	} else {
		assert( false );
	}
}

static void RB_BindExistingProgram( BackendState *backendState, int program ) {
	assert( program > 0 );
	if( backendState->program.boundProgram != program ) {
		backendState->draw.dirtyUniformState = true;
		backendState->program.boundProgram = program;
		backendState->actionTape->bindProgram( program );
	}
}

static void RB_SetupProgram( BackendState *backendState, int type, const shader_s *materialToGetDeforms, uint64_t features ) {
	// Deforms bypass BackendState program cache
	// (TODO: Why, were comparisons that expensive?)
	if( materialToGetDeforms->numdeforms ) [[unlikely]] {
		// Try to find an existing program in the global program cache
		const int program = RP_FindProgram( type, materialToGetDeforms, features );
		// TODO: Make a distinction between programs that failed to be created and programs that haven't been created yet
		if( program ) {
			RB_BindExistingProgram( backendState, program );
		} else {
			backendState->draw.dirtyUniformState = true;
			// Invalidate bound program in the simulated backend state
			backendState->program.boundProgram = -1;
			// Create the program during the tape execution
			backendState->actionTape->createAndBindProgram( type, materialToGetDeforms, features );
		}
	} else {
		// Perform a fast lookup by type and features in the first-level cache
		if( backendState->program.cachedFastLookupProgramType == type && backendState->program.cachedFastLookupProgramFeatures == features ) {
			assert( backendState->program.cachedFastLookupProgram > 0 );
			RB_BindExistingProgram( backendState, backendState->program.cachedFastLookupProgram );
		} else {
			// Try to find an existing prorgam in the global program cache
			const int program = RP_FindProgram( type, materialToGetDeforms, features );
			// TODO: Make a distinction between programs that failed to be created and programs that haven't been created yet
			if( program ) {
				RB_BindExistingProgram( backendState, program );
				// Save it in the first-level cache
				backendState->program.cachedFastLookupProgram         = program;
				backendState->program.cachedFastLookupProgramType     = type;
				backendState->program.cachedFastLookupProgramFeatures = features;
			} else {
				// Force the state invalidation as we don't know the exact program that is getting used
				// TODO: Do we need additional flags, like "can reset dirty uniform state?" - looks like we can check boundProgram < 0
				backendState->draw.dirtyUniformState = true;
				// Invalidate lookup cache in the simulated backend state (TODO: Is it needed?)
				backendState->program.cachedFastLookupProgram        = -1;
				backendState->program.cachedFastLookupProgramType    = -1;
				backendState->program.cachedFastLookupProgramFeatures = 0;
				// Invalidate bound program in the simulated backend state
				backendState->program.boundProgram = -1;
				// Create the program during the tape execution
				backendState->actionTape->createAndBindProgram( type, materialToGetDeforms, features );
			}
		}
	}
}

static inline float RB_FastSin( float t ) {
	return std::sin( t * (float)M_TWOPI );
}

[[nodiscard]]
static auto calcFuncValue( unsigned func, float arg ) -> float {
	const float modArg  = std::fmod( arg, 1.0f );
	const float argFrac = modArg + ( modArg >= 0.0f ? 0.0f : 1.0f );
	assert( argFrac >= 0.0f && argFrac <= 1.0f );
	switch( func ) {
		case SHADER_FUNC_SIN:
			return RB_FastSin( argFrac );
		case SHADER_FUNC_TRIANGLE: {
			if( argFrac < 0.25f ) {
				return argFrac * 4.0f;
			} else if( argFrac < 0.75f ) {
				return 2.0f - 4.0f * argFrac;
			} else {
				return ( argFrac - 0.75f ) * 4.0f - 1.0f;
			}
		}
		case SHADER_FUNC_SQUARE:
			return argFrac < 0.5f ? +1.0f : -1.0f;
		case SHADER_FUNC_SAWTOOTH:
			return argFrac;
		case SHADER_FUNC_INVERSESAWTOOTH:
			return 1.0f - argFrac;
		default:
			return RB_FastSin( argFrac );
	}
}

static float RB_TransformFogPlanes( const BackendState *backendState, const mfog_t *fog, vec3_t fogNormal,
									vec_t *fogDist, vec3_t vpnNormal, vec_t *vpnDist ) {
	const entity_t *e = backendState->material.currentEntity;

	assert( fog );
	assert( fogNormal && fogDist );
	assert( vpnNormal && vpnDist );

	const cplane_t *fogPlane = fog->visibleplane;
	const shader_s *fogShader = fog->shader;

	// distance to fog
	const float dist = PlaneDiff( backendState->global.cameraOrigin, fog->visibleplane );
	const float scale = e->scale;

	vec3_t viewtofog;
	if( e->rtype == RT_MODEL ) {
		VectorCopy( e->origin, viewtofog );
	} else {
		VectorClear( viewtofog );
	}

	// some math tricks to take entity's rotation matrix into account
	// for fog texture coordinates calculations:
	// M is rotation matrix, v is vertex, t is transform vector
	// n is plane's normal, d is plane's dist, r is view origin
	// (M*v + t)*n - d = (M*n)*v - ((d - t*n))
	// (M*v + t - r)*n = (M*n)*v - ((r - t)*n)
	Matrix3_TransformVector( e->axis, fogPlane->normal, fogNormal );
	VectorScale( fogNormal, scale, fogNormal );
	*fogDist = ( fogPlane->dist - DotProduct( viewtofog, fogPlane->normal ) );

	Matrix3_TransformVector( e->axis, backendState->global.cameraAxis, vpnNormal );
	VectorScale( vpnNormal, scale, vpnNormal );
	*vpnDist = ( ( backendState->global.cameraOrigin[0] - viewtofog[0] ) * backendState->global.cameraAxis[AXIS_FORWARD + 0] +
				 ( backendState->global.cameraOrigin[1] - viewtofog[1] ) * backendState->global.cameraAxis[AXIS_FORWARD + 1] +
				 ( backendState->global.cameraOrigin[2] - viewtofog[2] ) * backendState->global.cameraAxis[AXIS_FORWARD + 2] ) +
			   fogShader->fog_clearDist;

	return dist;
}

static void RB_VertexTCCelshadeMatrix( const BackendState *backendState, mat4_t matrix ) {
	const entity_t *e = backendState->material.currentEntity;
	if( e->model && !( backendState->global.renderFlags & RF_SHADOWMAPVIEW ) ) {
		vec3_t dir;
		R_LightForOrigin( e->lightingOrigin, dir, nullptr, nullptr, e->model->radius * e->scale, backendState->global.noWorldLight );

		mat4_t m;
		Matrix4_Identity( m );

		// rotate direction
		Matrix3_TransformVector( e->axis, dir, &m[0] );
		VectorNormalize( &m[0] );

		MakeNormalVectors( &m[0], &m[4], &m[8] );
		Matrix4_Transpose( m, matrix );
	}
}

static void RB_ApplyTCMods( const BackendState *backendState, const shaderpass_t *pass, mat4_t result ) {
	const double materialTime = backendState->material.currentShaderTime;
	for( unsigned tcmodNum = 0; tcmodNum < pass->numtcmods; ++tcmodNum ) {
		const tcmod_t *const tcmod = pass->tcmods + tcmodNum;
		switch( tcmod->type ) {
			case TC_MOD_ROTATE: {
				const float arg  = tcmod->args[0] * materialTime;
				const float sint = RB_FastSin( arg );
				const float cost = RB_FastSin( arg + 0.25f );
				mat4_t m1, m2;
				m2[0] =  cost, m2[1] = sint, m2[12] =  0.5f * ( sint - cost + 1 );
				m2[4] = -sint, m2[5] = cost, m2[13] = -0.5f * ( sint + cost - 1 );
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
			} break;
			case TC_MOD_SCALE: {
				Matrix4_Scale2D( result, tcmod->args[0], tcmod->args[1] );
			} break;
			case TC_MOD_TURB: {
				const float t1 = 0.25f;
				const float t2 = tcmod->args[2] + materialTime * tcmod->args[3];
				Matrix4_Scale2D( result,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 ) + tcmod->args[0] ) * t1,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 + 0.25 ) + tcmod->args[0] ) * t1 );
			} break;
			case TC_MOD_STRETCH: {
				float t2 = tcmod->args[3] + materialTime * tcmod->args[4];
				float t1 = calcFuncValue( (unsigned)tcmod->args[0], t2 ) * tcmod->args[2] + tcmod->args[1];
				t1 = t1 ? 1.0f / t1 : 1.0f;
				t2 = 0.5f - 0.5f * t1;
				Matrix4_Stretch2D( result, t1, t2 );
			} break;
			case TC_MOD_SCROLL: {
				float t1 = tcmod->args[0] * materialTime;
				float t2 = tcmod->args[1] * materialTime;
				if( pass->program_type != GLSL_PROGRAM_TYPE_DISTORTION ) { // HACK HACK HACK
					t1 = t1 - std::floor( t1 );
					t2 = t2 - std::floor( t2 );
				}
				Matrix4_Translate2D( result, t1, t2 );
			} break;
			case TC_MOD_TRANSFORM: {
				mat4_t m1, m2;
				m2[0] = tcmod->args[0], m2[1] = tcmod->args[2], m2[12] = tcmod->args[4],
				m2[5] = tcmod->args[1], m2[4] = tcmod->args[3], m2[13] = tcmod->args[5];
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
			} break;
			default:
				break;
		}
	}
}

[[nodiscard]]
static auto evaluateFunc( const shaderfunc_t *func, double materialTime ) -> float {
	float value;
	if( func->type == SHADER_FUNC_NOISE ) {
		// TODO: Not sure if this is suitable. There's no sufficient data to test it.
		// Vanilla material scripts do not use the noise function. Looks like we shouldn't care.
		const auto arg = (float)( ( materialTime + func->args[2] ) * func->args[3] );
		value = calcSimplexNoise2D( 0.0f, arg );
	} else {
		const auto arg = (float)( func->args[2] + materialTime * func->args[3] );
		value = calcFuncValue( func->type, arg );
	}
	return value * func->args[1] + func->args[0];
}

static float RB_GetShaderpassRgb( const BackendState *backendState, const shaderpass_t *pass, int *rgb ) {
	float a = 0.0f;
	double temp = 0.0f;
	vec3_t v { 0.0f, 0.0f, 0.0f };
	float colorMod = 1.0f;

	VectorSet( rgb, 255, 255, 255 );

	const shaderfunc_t *rgbgenfunc = &pass->rgbgen.func;
	switch( pass->rgbgen.type ) {
		case RGB_GEN_IDENTITY:
			break;
		case RGB_GEN_CONST:
			rgb[0] = ( int )( pass->rgbgen.args[0] * 255.0f );
			rgb[1] = ( int )( pass->rgbgen.args[1] * 255.0f );
			rgb[2] = ( int )( pass->rgbgen.args[2] * 255.0f );
			break;
		case RGB_GEN_ENTITYWAVE:
		case RGB_GEN_WAVE:
		case RGB_GEN_CUSTOMWAVE:
			if( rgbgenfunc->type == SHADER_FUNC_NONE ) {
				temp = 1;
			} else if( rgbgenfunc->type == SHADER_FUNC_RAMP ) {
				break;
			} else if( rgbgenfunc->args[1] == 0 ) {
				temp = rgbgenfunc->args[0];
			} else {
				temp = evaluateFunc( rgbgenfunc, backendState->material.currentShaderTime );
			}

			if( pass->rgbgen.type == RGB_GEN_ENTITYWAVE ) {
				VectorSet( v,
						   backendState->material.entityColor[0] * ( 1.0 / 255.0 ),
						   backendState->material.entityColor[1] * ( 1.0 / 255.0 ),
						   backendState->material.entityColor[2] * ( 1.0 / 255.0 ) );
			} else if( pass->rgbgen.type == RGB_GEN_CUSTOMWAVE ) {
				int c = R_GetCustomColor( (int)pass->rgbgen.args[0] );
				VectorSet( v,
						   COLOR_R( c ) * ( 1.0 / 255.0 ),
						   COLOR_G( c ) * ( 1.0 / 255.0 ),
						   COLOR_B( c ) * ( 1.0 / 255.0 ) );
			} else {
				VectorCopy( pass->rgbgen.args, v );
			}

			a = v[0]; rgb[0] = ( int )( a * 255.0f );
			a = v[1]; rgb[1] = ( int )( a * 255.0f );
			a = v[2]; rgb[2] = ( int )( a * 255.0f );
			colorMod = (float)temp;
			break;
		case RGB_GEN_OUTLINE:
			rgb[0] = backendState->material.entityOutlineColor[0];
			rgb[1] = backendState->material.entityOutlineColor[1];
			rgb[2] = backendState->material.entityOutlineColor[2];
			break;
		case RGB_GEN_ONE_MINUS_ENTITY:
			rgb[0] = 255 - backendState->material.entityColor[0];
			rgb[1] = 255 - backendState->material.entityColor[1];
			rgb[2] = 255 - backendState->material.entityColor[2];
			break;
		case RGB_GEN_FOG:
			rgb[0] = backendState->material.texFog->shader->fog_color[0];
			rgb[1] = backendState->material.texFog->shader->fog_color[1];
			rgb[2] = backendState->material.texFog->shader->fog_color[2];
			break;
		case RGB_GEN_ENVIRONMENT:
			rgb[0] = mapConfig.environmentColor[0];
			rgb[1] = mapConfig.environmentColor[1];
			rgb[2] = mapConfig.environmentColor[2];
			break;
		default:
			break;
	}

	return colorMod;
}

static int RB_GetShaderpassAlpha( const BackendState *backendState, const shaderpass_t *pass ) {
	int result = 255;
	float a = 0.0f;

	const shaderfunc_t *alphagenfunc = &pass->alphagen.func;
	switch( pass->alphagen.type ) {
		case ALPHA_GEN_IDENTITY:
			break;
		case ALPHA_GEN_CONST:
			result = ( int )( pass->alphagen.args[0] * 255.0f );
			break;
		case ALPHA_GEN_WAVE:
			if( !alphagenfunc || alphagenfunc->type == SHADER_FUNC_NONE ) {
				a = 1;
			} else if( alphagenfunc->type == SHADER_FUNC_RAMP ) {
				break;
			} else {
				a = evaluateFunc( alphagenfunc, backendState->material.currentShaderTime );
			}

			result = ( int )( a * 255.0f );
			break;
		case ALPHA_GEN_ENTITY:
			result = backendState->material.entityColor[3];
			break;
		case ALPHA_GEN_OUTLINE:
			result = backendState->material.entityOutlineColor[3];
		default:
			break;
	}

	return result;
}

static float RB_GetShaderpassColor( const BackendState *backendState, const shaderpass_t *pass, byte_vec4_t rgba_ ) {
	int rgba[4];

	const float colorMod = RB_GetShaderpassRgb( backendState, pass, &rgba[0] );
	rgba[3] = RB_GetShaderpassAlpha( backendState, pass );

	for( int i = 0; i < 4; i++ ) {
		rgba_[i] = wsw::clamp( rgba[i], 0, 255 );
	}

	return colorMod;
}

static Texture *RB_ShaderpassTex( const BackendState *backendState, const shaderpass_t *pass ) {
	if( pass->anim_numframes ) {
		Texture *res;
		if( pass->anim_fps > 0.0f ) {
			res = pass->images[(int)( pass->anim_fps * backendState->material.currentShaderTime ) % pass->anim_numframes];
		} else {
			assert( backendState->material.currentShaderFrac >= 0.0f && backendState->material.currentShaderFrac <= 1.0f );
			assert( pass->timelineFracs[0] == 0.0f );
			res = pass->images[0];
			for( unsigned i = 1; i < pass->anim_numframes; ++i ) {
				assert( pass->timelineFracs[i] >= 0.0f && pass->timelineFracs[i] < 1.0f );
				if( pass->timelineFracs[i] < backendState->material.currentShaderFrac ) {
					res = pass->images[i];
				} else {
					break;
				}
			}
		}
		assert( res );
		return res;
	}

	auto *const textureCache = TextureCache::instance();
	if( pass->flags & SHADERPASS_PORTALMAP ) {
		if( const auto *surface = backendState->material.currentPortalSurface ) {
			if( surface->texures[0] ) {
				return surface->texures[0];
			} else {
				return textureCache->blackTexture();
			}
		} else {
			return textureCache->blackTexture();
		}
	}

	Texture *const tex = pass->images[0];
	return tex ? tex : textureCache->noTexture();
}

static uint64_t RB_RGBAlphaGenToProgramFeatures( const BackendState *backendState, const colorgen_t *rgbgen, const colorgen_t *alphagen ) {
	uint64_t programFeatures = 0;
	int identity             = 0;

	switch( rgbgen->type ) {
		case RGB_GEN_VERTEX:
		case RGB_GEN_EXACT_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_VERTEX;
			break;
		case RGB_GEN_ONE_MINUS_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX;
			break;
		case RGB_GEN_WAVE:
		case RGB_GEN_CUSTOMWAVE:
		case RGB_GEN_ENTITYWAVE:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_CONST;
			if( rgbgen->func.type == SHADER_FUNC_RAMP ) {
				programFeatures |= GLSL_SHADER_COMMON_RGB_DISTANCERAMP;
			}
			break;
		case RGB_GEN_IDENTITY:
			identity++;
		default:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_CONST;
			break;
	}

	switch( alphagen->type ) {
		case ALPHA_GEN_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX;
			break;
		case ALPHA_GEN_ONE_MINUS_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX;
			break;
		case ALPHA_GEN_WAVE:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_CONST;
			if( alphagen->func.type == SHADER_FUNC_RAMP ) {
				programFeatures |= GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP;
			}
			break;
		case ALPHA_GEN_IDENTITY:
			identity++;
		default:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_CONST;
			break;
	}

	if( identity == 2 && !backendState->material.alphaHack ) {
		return 0;
	}

	return programFeatures;
}

static uint64_t RB_BonesTransformsToProgramFeatures( const BackendState *backendState ) {
	// check whether the current model is actually sketetal
	if( backendState->material.currentModelType != mod_skeletal ) {
		return 0;
	}
	// base pose sketetal models aren't animated and rendered as-is
	if( !backendState->draw.bonesData.numBones ) {
		return 0;
	}
	return backendState->draw.bonesData.maxWeights * GLSL_SHADER_COMMON_BONE_TRANSFORMS1;
}

static uint64_t RB_DlightbitsToProgramFeatures( unsigned dlightBits ) {
	assert( dlightBits );

	int numDlights     = Q_bitcount( dlightBits );
	const int varValue = v_lighting_maxGlslDlights.get();
	// TODO: This should be some kind of OptIntVar
	if( varValue && numDlights > varValue ) {
		numDlights = varValue;
	}

	if( numDlights <= 4 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_4;
	}
	if( numDlights <= 8 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_8;
	}
	if( numDlights <= 12 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_12;
	}
	return GLSL_SHADER_COMMON_DLIGHTS_16;
}

static uint64_t RB_AutospriteProgramFeatures( const BackendState *backendState ) {
	uint64_t programFeatures = 0;
	if( ( backendState->draw.currentVAttribs & VATTRIB_AUTOSPRITE2_BIT ) == VATTRIB_AUTOSPRITE2_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE2;
	} else if( ( backendState->draw.currentVAttribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE;
	}
	return programFeatures;
}

static uint64_t RB_InstancedArraysProgramFeatures( const BackendState *backendState ) {
	uint64_t programFeatures = 0;
	if( ( backendState->draw.currentVAttribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS;
	} else if( /*rb.drawMeshVertSpan.numInstances*/ false ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS;
	}
	return programFeatures;
}

static uint64_t RB_FogProgramFeatures( const BackendState *backendState, const shaderpass_t *pass, const mfog_t *fog ) {
	uint64_t programFeatures = 0;
	if( fog ) {
		programFeatures |= GLSL_SHADER_COMMON_FOG;
		if( fog == backendState->material.colorFog ) {
			programFeatures |= GLSL_SHADER_COMMON_FOG_RGB;
		}
	}
	return programFeatures;
}

static uint64_t RB_AlphatestProgramFeatures( const shaderpass_t *pass ) {
	switch( pass->flags & SHADERPASS_ALPHAFUNC ) {
		case SHADERPASS_AFUNC_GT0:
			return GLSL_SHADER_COMMON_AFUNC_GT0;
		case SHADERPASS_AFUNC_LT128:
			return GLSL_SHADER_COMMON_AFUNC_LT128;
		case SHADERPASS_AFUNC_GE128:
			return GLSL_SHADER_COMMON_AFUNC_GE128;
	}
	return 0;
}

static uint64_t RB_TcModsProgramFeatures( const shaderpass_t *pass ) {
	if( pass->numtcmods ) {
		return GLSL_SHADER_COMMON_TC_MOD;
	}
	return 0;
}

static uint64_t RB_sRGBProgramFeatures( const shaderpass_t *pass ) {
	// don't do srgb<->linear conversions at all, used for blitting framebuffers
	if( pass->flags & SHADERPASS_NOSRGB ) {
		return 0;
	}

	uint64_t programFeatures = 0;
	if( glConfig.sSRGB ) {
		programFeatures |= GLSL_SHADER_COMMON_SRGB2LINEAR;

		// ok, so we're getting sRGB linear input while rendering to
		// default framebuffer, so we need to go back from linear to sRGB
		programFeatures |= GLSL_SHADER_COMMON_LINEAR2SRB;
	}

	return programFeatures;
}

static void RB_UpdateCommonUniforms( BackendState *backendState, const shaderpass_t *pass, mat4_t texMatrix ) {
	const entity_t *e = backendState->material.currentEntity;

	// the logic here should match R_TransformForEntity
	vec3_t entDist, entOrigin;
	if( e->rtype != RT_MODEL ) {
		VectorClear( entOrigin );
		VectorCopy( backendState->global.cameraOrigin, entDist );
	} else {
		VectorCopy( e->origin, entOrigin );
		vec3_t tmp;
		VectorSubtract( backendState->global.cameraOrigin, e->origin, tmp );
		Matrix3_TransformVector( e->axis, tmp, entDist );
	}

	// calculate constant color
	byte_vec4_t constColor;
	const float colorMod = RB_GetShaderpassColor( backendState, pass, constColor );

	// apply modifications to texture coordinates
	if( pass->numtcmods ) {
		RB_ApplyTCMods( backendState, pass, texMatrix );
	}

	const float mirrorSide = ( backendState->global.renderFlags & RF_MIRRORVIEW ) ? -1 : +1;
	RP_UpdateViewUniforms( backendState, backendState->global.modelviewMatrix,
						   backendState->global.modelviewProjectionMatrix,
						   backendState->global.cameraOrigin, backendState->global.cameraAxis, mirrorSide,
						   backendState->gl.getViewport(), backendState->global.zNear, backendState->global.zFar );

	const auto glState = backendState->gl.getState();
	vec2_t blendMix = { 0, 0 };
	if( RB_IsAlphaBlending( glState & GLSTATE_SRCBLEND_MASK, glState & GLSTATE_DSTBLEND_MASK ) ) {
		blendMix[1] = 1;
		if( backendState->material.alphaHack ) {
			constColor[3] *= backendState->material.hackedAlpha;
		}
	} else {
		blendMix[0] = 1;
		if( backendState->material.alphaHack ) {
			constColor[0] *= backendState->material.hackedAlpha, constColor[1] *= backendState->material.hackedAlpha, constColor[2] *= backendState->material.hackedAlpha;
		}
	}

	RP_UpdateShaderUniforms( backendState, backendState->material.currentShaderTime,
							 entOrigin, entDist, backendState->material.entityColor,
							 constColor,
							 pass->rgbgen.func.type != SHADER_FUNC_NONE ? pass->rgbgen.func.args : pass->rgbgen.args,
							 pass->alphagen.func.type != SHADER_FUNC_NONE ? pass->alphagen.func.args : pass->alphagen.args,
							 texMatrix, colorMod );

	RP_UpdateDeformBuiltinUniforms( backendState, backendState->material.currentShaderTime, backendState->global.cameraOrigin,
									backendState->global.cameraAxis, entOrigin, mirrorSide );

	RP_UpdateBlendMixUniform( backendState, blendMix );

	RP_UpdateSoftParticlesUniforms( backendState, v_softParticles_scale.get() );
}

static void RB_UpdateFogUniforms( BackendState *backendState, const mfog_t *fog ) {
	assert( fog );

	cplane_t fogPlane, vpnPlane;
	const float dist = RB_TransformFogPlanes( backendState, fog, fogPlane.normal, &fogPlane.dist, vpnPlane.normal, &vpnPlane.dist );

	RP_UpdateFogUniforms( backendState, fog->shader->fog_color, fog->shader->fog_clearDist,
						  fog->shader->fog_dist, &fogPlane, &vpnPlane, dist );
}

static uint64_t RB_TcGenToProgramFeatures( const BackendState *backendState, int tcgen, vec_t *tcgenVec, mat4_t texMatrix, mat4_t genVectors ) {
	uint64_t programFeatures = 0;

	Matrix4_Identity( texMatrix );

	switch( tcgen ) {
		case TC_GEN_ENVIRONMENT:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_ENV;
			break;
		case TC_GEN_VECTOR:
			Matrix4_Identity( genVectors );
			Vector4Copy( &tcgenVec[0], &genVectors[0] );
			Vector4Copy( &tcgenVec[4], &genVectors[4] );
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_VECTOR;
			break;
		case TC_GEN_PROJECTION:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_PROJECTION;
			break;
		case TC_GEN_REFLECTION_CELSHADE:
			RB_VertexTCCelshadeMatrix( backendState, texMatrix );
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_CELSHADE;
			break;
		case TC_GEN_REFLECTION:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_REFLECTION;
			break;
		case TC_GEN_SURROUND:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_SURROUND;
			break;
		default:
			break;
	}

	return programFeatures;
}

static void RB_RenderMeshGLSL_Q3AShader( BackendState *backendState, const FrontendToBackendShared *fsh,
										 const DrawMeshVertSpan *vertSpan, int primitive,
										 const shaderpass_t *pass, uint64_t programFeatures ) {
	const bool isWorldSurface = backendState->material.currentModelType == mod_brush;
	const unsigned rgbgen     = pass->rgbgen.type;
	const entity_t *e         = backendState->material.currentEntity;

	bool isLightmapped = false;
	const superLightStyle_t *lightStyle = nullptr;
	// lightmapped surface pass
	if( isWorldSurface ) {
		const superLightStyle_t *drawStateLightStyle = backendState->draw.superLightStyle;
		if( drawStateLightStyle && drawStateLightStyle->lightmapNum[0] >= 0 ) {
			if( backendState->material.currentShader->flags & SHADER_LIGHTMAP ) {
				if( ( pass->flags & GLSTATE_BLEND_ADD ) != GLSTATE_BLEND_ADD ) {
					if( ( pass->flags & ( GLSTATE_SRCBLEND_SRC_ALPHA ) ) != GLSTATE_SRCBLEND_SRC_ALPHA ) {
						// TODO: Replace multiple || by the bitmask trick
						if( rgbgen == RGB_GEN_IDENTITY || rgbgen == RGB_GEN_CONST || rgbgen == RGB_GEN_WAVE
							|| rgbgen == RGB_GEN_CUSTOMWAVE || rgbgen == RGB_GEN_VERTEX
							|| rgbgen == RGB_GEN_ONE_MINUS_VERTEX || rgbgen == RGB_GEN_EXACT_VERTEX ) {
							lightStyle = drawStateLightStyle;
							isLightmapped = true;
						}
					}
				}
			}
		}
	}

	bool isWorldVertexLight = false;
	// vertex-lit world surface
	if( isWorldSurface ) {
		if ( rgbgen == RGB_GEN_VERTEX || rgbgen == RGB_GEN_EXACT_VERTEX ) {
			if( backendState->draw.superLightStyle ) {
				isWorldVertexLight = true;
			}
		}
	}

	const mfog_t *fog = backendState->material.fog;
	// possibly apply the fog inline
	if( fog == backendState->material.texFog ) {
		if( backendState->draw.currentShadowBits ) {
			fog = nullptr;
		} else if( backendState->material.currentShader->numpasses == 1 || ( isLightmapped && backendState->material.currentShader->numpasses == 2 ) ) {
			// TODO: Modifying the material state!
			backendState->material.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}
	programFeatures |= RB_FogProgramFeatures( backendState, pass, fog );

	vec3_t lightDir;
	vec4_t lightAmbient, lightDiffuse;
	// diffuse lighting for entities
	if( !isWorldSurface && rgbgen == RGB_GEN_LIGHTING_DIFFUSE && !( e->flags & RF_FULLBRIGHT ) ) {
		vec3_t temp = { 0.1f, 0.2f, 0.7f };
		float radius = 1;

		if( e->number != kWorldEntNumber && e->model != nullptr ) {
			radius = e->model->radius;
		}

		// get weighted incoming direction of world and dynamic lights
		R_LightForOrigin( e->lightingOrigin, temp, lightAmbient, lightDiffuse, radius * e->scale, backendState->global.noWorldLight );

		if( e->flags & RF_MINLIGHT ) {
			if( lightAmbient[0] <= 0.1f || lightAmbient[1] <= 0.1f || lightAmbient[2] <= 0.1f ) {
				VectorSet( lightAmbient, 0.1f, 0.1f, 0.1f );
			}
		}

		// rotate direction
		Matrix3_TransformVector( e->axis, temp, lightDir );
	} else {
		VectorSet( lightDir, 0, 0, 0 );
		Vector4Set( lightAmbient, 1, 1, 1, 1 );
		Vector4Set( lightDiffuse, 1, 1, 1, 1 );
	}

	const Texture *texture = RB_ShaderpassTex( backendState, pass );
	if( isLightmapped || isWorldVertexLight ) {
		// add dynamic lights
		if( backendState->draw.currentDlightBits ) {
			programFeatures |= RB_DlightbitsToProgramFeatures( backendState->draw.currentDlightBits );
		}
		if( DRAWFLAT( backendState ) ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT;
		}
		if( backendState->global.renderFlags & RF_LIGHTMAP ) {
			texture = TextureCache::instance()->whiteTexture();
		}
	}

	if( texture->flags & IT_ALPHAMASK ) {
		programFeatures |= GLSL_SHADER_Q3_ALPHA_MASK;
	}

	backendState->gl.bindTexture( 0, texture );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( backendState, &pass->rgbgen, &pass->alphagen );

	mat4_t texMatrix, genVectors;
	programFeatures |= RB_TcGenToProgramFeatures( backendState, pass->tcgen, pass->tcgenVec, texMatrix, genVectors );

	// set shaderpass state (blending, depthwrite, etc)
	unsigned passStateFlags = pass->flags;

	// possibly force depthwrite and give up blending when doing a lightmapped pass
	if( ( isLightmapped || isWorldVertexLight ) && !backendState->draw.doneDepthPass ) {
		if( !( passStateFlags & GLSTATE_DEPTHWRITE ) && ( backendState->material.currentShader->flags & SHADER_DEPTHWRITE ) ) {
			if( !( pass->flags & SHADERPASS_ALPHAFUNC ) ) {
				passStateFlags &= ~GLSTATE_BLEND_MASK;
			}
			passStateFlags |= GLSTATE_DEPTHWRITE;
		}
	}

	RB_SetShaderpassState( backendState, passStateFlags );

	/*
	if( programFeatures & GLSL_SHADER_COMMON_SOFT_PARTICLE ) {
		rb.glState->bindTexture( 3, rb.st.screenDepthTexCopy );
	}*/

	if( isLightmapped ) {
		int i = 0;
		// bind lightmap textures and set program's features for lightstyles
		while( i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255 ) {
			backendState->gl.bindTexture( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] );
			++i;
		}
		programFeatures |= ( i * GLSL_SHADER_Q3_LIGHTSTYLE0 );
		if( mapConfig.lightmapArrays ) {
			programFeatures |= GLSL_SHADER_Q3_LIGHTMAP_ARRAYS;
		}
	}

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_Q3A_SHADER, backendState->material.currentShader, programFeatures );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );

	RP_UpdateTexGenUniforms( backendState, texMatrix, genVectors );

	if( isWorldSurface || rgbgen == RGB_GEN_LIGHTING_DIFFUSE ) {
		RP_UpdateDiffuseLightUniforms( backendState, lightDir, lightAmbient, lightDiffuse );
	}

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		RB_UpdateFogUniforms( backendState, fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( backendState, backendState->draw.bonesData.numBones, backendState->draw.bonesData.dualQuats );
	}

	// dynamic lights
	if( isLightmapped || isWorldVertexLight ) {
		RP_UpdateDynamicLightsUniforms( backendState, fsh, lightStyle, e->origin, e->axis, backendState->draw.currentDlightBits );
	}

	// r_drawflat
	if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
		if( backendState->global.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
			RP_UpdateDrawFlatUniforms( backendState, colorWhite, colorWhite );
		} else {
			RP_UpdateDrawFlatUniforms( backendState, rsh.wallColor, rsh.floorColor );
		}
	}

	/*
	if( programFeatures & GLSL_SHADER_COMMON_SOFT_PARTICLE ) {
		RP_UpdateTextureUniforms( program, rb.st.screenDepthTex->width, rb.st.screenDepthTex->height );
	}*/

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_Material( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	if( ( backendState->material.currentModelType == mod_brush && !mapConfig.deluxeMappingEnabled )
		/*|| ( normalmap == rsh.blankBumpTexture && !glossmap && !decalmap && !entdecalmap )*/ ) {
		// render as plain Q3A shader, which is less computation-intensive
		RB_RenderMeshGLSL_Q3AShader( backendState, fsh, vertSpan, primitive, pass, programFeatures );
		return;
	}

	TextureCache *const textureCache = TextureCache::instance();

	const Texture *const baseTexture           = RB_ShaderpassTex( backendState, pass );
	const Texture *normalMapTexture            = pass->images[1] ? pass->images[1] : textureCache->blankNormalmap();
	const Texture *const glossMapTexture       = pass->images[2];
	const Texture *decalMapTexture             = pass->images[3];
	const Texture *const entityDecalMapTexture = pass->images[4];

	// use blank image if the normalmap is too tiny due to high picmip value
	if( !normalMapTexture || ( normalMapTexture->width < 2 || normalMapTexture->height < 2 ) ) {
		normalMapTexture = textureCache->blankNormalmap();
	}

	float offsetmappingScale = 0.0f;
	if( normalMapTexture->samples == 4 ) {
		offsetmappingScale = v_offsetMapping_scale.get() * backendState->material.currentShader->offsetmappingScale;
	}

	float glossIntensity = backendState->material.currentShader->glossIntensity;
	if( glossIntensity <= 0.0f ) {
		glossIntensity = v_lighting_glossIntensity.get();
	}
	float glossExponent = backendState->material.currentShader->glossExponent;
	if( glossExponent <= 0.0f ) {
		glossExponent = v_lighting_glossExponent.get();
	}

	const bool applyDecal = decalMapTexture != nullptr;

	const mfog_t *fog = backendState->material.fog;
	// possibly apply the "texture" fog inline
	if( fog == backendState->material.texFog ) {
		if( ( backendState->material.currentShader->numpasses == 1 ) && !backendState->draw.currentShadowBits ) {
			// TODO: Modifying the material state!
			backendState->material.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}
	programFeatures |= RB_FogProgramFeatures( backendState, pass, fog );

	if( backendState->material.currentModelType == mod_brush ) {
		// brush models
		if( !( v_offsetMapping.get() & 1 ) ) {
			offsetmappingScale = 0;
		}
		if( backendState->global.renderFlags & RF_LIGHTMAP ) {
			programFeatures |= GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
		if( DRAWFLAT( backendState ) ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT | GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
	} else if( backendState->material.currentModelType == mod_bad ) {
		// polys
		if( !( v_offsetMapping.get() & 2 ) ) {
			offsetmappingScale = 0;
		}
	} else {
		// regular models
		if( !( v_offsetMapping.get() & 4 ) ) {
			offsetmappingScale = 0;
		}
	#ifdef CELSHADEDMATERIAL
		programFeatures |= GLSL_SHADER_MATERIAL_CELSHADING;
	#endif
	#ifdef HALFLAMBERTLIGHTING
		programFeatures |= GLSL_SHADER_MATERIAL_HALFLAMBERT;
	#endif
	}

	// add dynamic lights
	if( backendState->draw.currentDlightBits ) {
		programFeatures |= RB_DlightbitsToProgramFeatures( backendState->draw.currentDlightBits );
	}

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	backendState->gl.bindTexture( 0, baseTexture );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( backendState, &pass->rgbgen, &pass->alphagen );

	RB_SetShaderpassState( backendState, pass->flags );

	// we only send S-vectors to GPU and recalc T-vectors as cross product
	// in vertex shader
	backendState->gl.bindTexture( 1, normalMapTexture );

	if( glossMapTexture && glossIntensity > 0.0f ) {
		programFeatures |= GLSL_SHADER_MATERIAL_SPECULAR;
		backendState->gl.bindTexture( 2, glossMapTexture );
	}

	if( applyDecal ) {
		programFeatures |= GLSL_SHADER_MATERIAL_DECAL;

		if( backendState->global.renderFlags & RF_LIGHTMAP ) {
			decalMapTexture = textureCache->blackTexture();
			programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
		} else {
			// if no alpha, use additive blending
			if( decalMapTexture->samples & 1 ) {
				programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
			}
		}

		backendState->gl.bindTexture( 3, decalMapTexture );
	}

	if( entityDecalMapTexture ) {
		programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL;

		// if no alpha, use additive blending
		if( entityDecalMapTexture->samples & 1 ) {
			programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL_ADD;
		}

		backendState->gl.bindTexture( 4, entityDecalMapTexture );
	}

	if( offsetmappingScale > 0 ) {
		programFeatures |= v_offsetMapping_reliefMapping.get() ?
						   GLSL_SHADER_MATERIAL_RELIEFMAPPING : GLSL_SHADER_MATERIAL_OFFSETMAPPING;
	}

	vec3_t lightDir { 0.0f, 0.0f, 0.0f };
	vec4_t ambient { 0.0f, 0.0f, 0.0f, 0.0f };
	vec4_t diffuse { 0.0f, 0.0f, 0.0f, 0.0f };
	const superLightStyle_t *lightStyle = nullptr;
	if( backendState->material.currentModelType == mod_brush ) {
		// world surface
		if( backendState->draw.superLightStyle && backendState->draw.superLightStyle->lightmapNum[0] >= 0 ) {
			lightStyle = backendState->draw.superLightStyle;

			int i = 0;
			// bind lightmap textures and set program's features for lightstyles
			while( i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255 ) {
				backendState->gl.bindTexture( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] );
				++i;
			}

			programFeatures |= ( i * GLSL_SHADER_MATERIAL_LIGHTSTYLE0 );

			if( mapConfig.lightmapArrays ) {
				programFeatures |= GLSL_SHADER_MATERIAL_LIGHTMAP_ARRAYS;
			}

			if( i == 1 ) {
				vec_t *rgb = lightStyles[lightStyle->lightmapStyles[0]].rgb;

				// GLSL_SHADER_MATERIAL_FB_LIGHTMAP indicates that there's no need to renormalize
				// the lighting vector for specular (saves 3 adds, 3 muls and 1 normalize per pixel)
				if( rgb[0] == 1 && rgb[1] == 1 && rgb[2] == 1 ) {
					programFeatures |= GLSL_SHADER_MATERIAL_FB_LIGHTMAP;
				}
			}

			if( !VectorCompare( mapConfig.ambient, vec3_origin ) ) {
				VectorCopy( mapConfig.ambient, ambient );
				programFeatures |= GLSL_SHADER_MATERIAL_AMBIENT_COMPENSATION;
			}
		} else {
			// vertex lighting
			VectorSet( lightDir, 0.1f, 0.2f, 0.7f );
			VectorSet( ambient, backendState->global.minLight, backendState->global.minLight, backendState->global.minLight );
			VectorSet( diffuse, backendState->global.minLight, backendState->global.minLight, backendState->global.minLight );

			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT | GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_MIX;
		}
	} else {
		programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT;

		if( backendState->material.currentModelType == mod_bad ) {
			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_FROM_NORMAL;

			VectorSet( lightDir, 0, 0, 0 );
			Vector4Set( ambient, 0, 0, 0, 0 );
			Vector4Set( diffuse, 1, 1, 1, 1 );
		} else {
			const entity_t *e = backendState->material.currentEntity;

			if( e->flags & RF_FULLBRIGHT ) {
				Vector4Set( ambient, 1, 1, 1, 1 );
				Vector4Set( diffuse, 1, 1, 1, 1 );
			} else {
				vec3_t temp;
				if( e->model && e->number != kWorldEntNumber ) {
					// get weighted incoming direction of world and dynamic lights
					R_LightForOrigin( e->lightingOrigin, temp, ambient, diffuse,
									  e->model->radius * e->scale, backendState->global.noWorldLight );
				} else {
					VectorSet( temp, 0.1f, 0.2f, 0.7f );
				}

				if( e->flags & RF_MINLIGHT ) {
					float minLight = backendState->global.minLight;
					float ambientL = VectorLength( ambient );

					if( ambientL < minLight ) {
						if( ambientL < 0.001 ) {
							VectorSet( ambient, 1, 1, 1 );
						}
						VectorNormalize( ambient );
						VectorScale( ambient, minLight, ambient );
					}
				}

				// rotate direction
				Matrix3_TransformVector( e->axis, temp, lightDir );
			}
		}
	}

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_MATERIAL, backendState->material.currentShader, programFeatures );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateMaterialUniforms( backendState, offsetmappingScale, glossIntensity, glossExponent );
	RP_UpdateDiffuseLightUniforms( backendState, lightDir, ambient, diffuse );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		RB_UpdateFogUniforms( backendState, fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( backendState, backendState->draw.bonesData.numBones, backendState->draw.bonesData.dualQuats );
	}

	// dynamic lights
	RP_UpdateDynamicLightsUniforms( backendState, fsh, lightStyle, backendState->material.currentEntity->origin,
									backendState->material.currentEntity->axis, backendState->draw.currentDlightBits );

	// r_drawflat
	if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
		if( backendState->global.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
			RP_UpdateDrawFlatUniforms( backendState, colorWhite, colorWhite );
		} else {
			RP_UpdateDrawFlatUniforms( backendState, rsh.wallColor, rsh.floorColor );
		}
	}

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_Distortion( BackendState *backendState, const FrontendToBackendShared *,
										  const DrawMeshVertSpan *vertSpan, int primitive,
										  const shaderpass_t *pass, uint64_t programFeatures ) {
	const portalSurface_s *const portalSurface = backendState->material.currentPortalSurface;
	// TODO: Is this condition reachable?
	if( !portalSurface ) {
		return;
	}

	TextureCache *const textureCache     = TextureCache::instance();
	Texture *const blankTexture          = textureCache->greyTexture();
	Texture *const blankNormalMapTexture = textureCache->blankNormalmap();

	const Texture *portalTextures[2];
	int width = 1, height = 1;
	for( int i = 0; i < 2; i++ ) {
		portalTextures[i] = portalSurface->texures[i];
		if( !portalTextures[i] ) {
			portalTextures[i] = blankTexture;
		} else {
			width  = portalTextures[i]->width;
			height = portalTextures[i]->height;
		}
	}

	const Texture *const dudvMapTexture   = pass->images[0] ? pass->images[0] : blankNormalMapTexture;
	const Texture *const normalMapTexture = pass->images[1] ? pass->images[1] : blankNormalMapTexture;
	if( dudvMapTexture != blankNormalMapTexture ) {
		programFeatures |= GLSL_SHADER_DISTORTION_DUDV;
	}

	if( portalTextures[0] == blankTexture && portalTextures[1] == blankTexture ) {
		// Let it be actually drawn
		programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
	} else {
		if( portalTextures[0] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
		}
		if( portalTextures[1] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFRACTION;
		}
	}

	const bool frontPlane = PlaneDiff( backendState->global.cameraOrigin, &portalSurface->untransformed_plane ) > 0;
	if( frontPlane ) {
		if( pass->alphagen.type != ALPHA_GEN_IDENTITY ) {
			programFeatures |= GLSL_SHADER_DISTORTION_DISTORTION_ALPHA;
		}
	}

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	backendState->gl.bindTexture( 0, dudvMapTexture );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( backendState, &pass->rgbgen, &pass->alphagen );
	programFeatures |= RB_FogProgramFeatures( backendState, pass, backendState->material.fog );

	RB_SetShaderpassState( backendState, pass->flags );

	if( normalMapTexture != blankNormalMapTexture ) {
		// eyeDot
		programFeatures |= GLSL_SHADER_DISTORTION_EYEDOT;
		backendState->gl.bindTexture( 1, normalMapTexture );
	}

	backendState->gl.bindTexture( 2, portalTextures[0] );           // reflection
	backendState->gl.bindTexture( 3, portalTextures[1] );           // refraction

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_DISTORTION, backendState->material.currentShader, programFeatures );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateDistortionUniforms( backendState, frontPlane );
	RP_UpdateTextureUniforms( backendState, width, height );

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_Outline( BackendState *backendState, const FrontendToBackendShared *,
									   const DrawMeshVertSpan *vertSpan, int primitive,
									   const shaderpass_t *pass, uint64_t programFeatures ) {
	if( backendState->material.currentModelType == mod_brush ) {
		programFeatures |= GLSL_SHADER_OUTLINE_OUTLINES_CUTOFF;
	}

	programFeatures |= RB_RGBAlphaGenToProgramFeatures( backendState, &pass->rgbgen, &pass->alphagen );
	programFeatures |= RB_FogProgramFeatures( backendState, pass, backendState->material.fog );

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_OUTLINE, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	const auto faceCull = backendState->gl.getCull();
	backendState->gl.setCull( GL_BACK );

	RB_SetShaderpassState( backendState, pass->flags );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateOutlineUniforms( backendState, backendState->material.currentEntity->outlineHeight * v_outlinesScale.get() );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		RB_UpdateFogUniforms( backendState, backendState->material.fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( backendState, backendState->draw.bonesData.numBones, backendState->draw.bonesData.dualQuats );
	}

	RB_DrawMeshVerts( backendState, vertSpan, primitive );

	backendState->gl.setCull( faceCull );
}

static uint64_t RB_BindCelshadeTexture( BackendState *backendState, int tmu, const Texture *texture,
										uint64_t feature, bool canAdd, const Texture *replacement ) {
	const Texture *textureToUse = nullptr;
	uint64_t resultFeatures     = 0;

	// Note: Preserving the orignal logic here
	if( texture ) {
		if( !( backendState->global.renderFlags & RF_SHADOWMAPVIEW ) ) [[likely]] {
			textureToUse = texture;
			resultFeatures |= feature;
			if( canAdd && ( textureToUse->samples & 1 ) ) {
				resultFeatures |= ( feature << 1 );
			}
		} else {
			if( texture->flags & IT_CUBEMAP ) {
				textureToUse = TextureCache::instance()->whiteCubemapTexture();
			} else {
				textureToUse = TextureCache::instance()->whiteTexture();
			}
		}
	}

	if( textureToUse ) {
		backendState->gl.bindTexture( tmu, textureToUse );
	}

	return resultFeatures;
}

static void RB_RenderMeshGLSL_Celshade( BackendState *backendState, const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	TextureCache *const textureCache = TextureCache::instance();

	const Texture *const baseTexture        = pass->images[0];
	const Texture *const shadeTexture       = pass->images[1];
	const Texture *const diffuseTexture     = pass->images[2];
	const Texture *const decalTexture       = pass->images[3];
	const Texture *const entityDecalTexture = pass->images[4];
	const Texture *const stripesTexture     = pass->images[5];
	const Texture *const lightTexture       = pass->images[6];

	backendState->gl.bindTexture( 0, baseTexture );

	const mfog_t *fog = backendState->material.fog;
	// possibly apply the "texture" fog inline
	if( fog == backendState->material.texFog ) {
		if( ( backendState->material.currentShader->numpasses == 1 ) && !backendState->draw.currentShadowBits ) {
			// TODO: Modifying the material state
			backendState->material.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}
	programFeatures |= RB_FogProgramFeatures( backendState, pass, fog );

	programFeatures |= RB_RGBAlphaGenToProgramFeatures( backendState, &pass->rgbgen, &pass->alphagen );

	RB_SetShaderpassState( backendState, pass->flags );

	Texture *const whiteTexture = textureCache->whiteTexture();
	assert( whiteTexture && whiteTexture->target == GL_TEXTURE_2D );
	Texture *const whiteCubemapTexture = textureCache->whiteCubemapTexture();
	assert( whiteCubemapTexture && whiteCubemapTexture->target == GL_TEXTURE_CUBE_MAP );

	programFeatures |= RB_BindCelshadeTexture( backendState, 1, shadeTexture, 0, false, whiteCubemapTexture );
	programFeatures |= RB_BindCelshadeTexture( backendState, 2, diffuseTexture, GLSL_SHADER_CELSHADE_DIFFUSE, false, nullptr );
	programFeatures |= RB_BindCelshadeTexture( backendState, 3, decalTexture, GLSL_SHADER_CELSHADE_DECAL, true, nullptr );
	programFeatures |= RB_BindCelshadeTexture( backendState, 4, entityDecalTexture, GLSL_SHADER_CELSHADE_ENTITY_DECAL, true, whiteTexture );
	programFeatures |= RB_BindCelshadeTexture( backendState, 5, stripesTexture, GLSL_SHADER_CELSHADE_STRIPES, true, nullptr );
	programFeatures |= RB_BindCelshadeTexture( backendState, 6, lightTexture, GLSL_SHADER_CELSHADE_CEL_LIGHT, true, nullptr );

	// update uniforms
	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_CELSHADE, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	mat4_t reflectionMatrix;
	RB_VertexTCCelshadeMatrix( backendState, reflectionMatrix );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateTexGenUniforms( backendState, reflectionMatrix, texMatrix );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		RB_UpdateFogUniforms( backendState, fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( backendState, backendState->draw.bonesData.numBones, backendState->draw.bonesData.dualQuats );
	}

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_Fog( BackendState *backendState, const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	const mfog_t *fog = backendState->material.fog;

	programFeatures |= GLSL_SHADER_COMMON_FOG;

	RB_SetShaderpassState( backendState, pass->flags );

	// update uniforms
	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_FOG, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix = { 0 };
	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RB_UpdateFogUniforms( backendState, fog );

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( backendState, backendState->draw.bonesData.numBones, backendState->draw.bonesData.dualQuats );
	}

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_FXAA( BackendState *backendState, const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	RB_SetShaderpassState( backendState, pass->flags );

	const Texture *image = pass->images[0];
	backendState->gl.bindTexture( 0, image );

	if( glConfig.ext.gpu_shader5 ) {
		programFeatures |= GLSL_SHADER_FXAA_FXAA3;
	}

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_FXAA, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateTextureUniforms( backendState, image->width, image->height );

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_YUV( BackendState *backendState, const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	RB_SetShaderpassState( backendState, pass->flags );

	backendState->gl.bindTexture( 0, pass->images[0] );
	backendState->gl.bindTexture( 1, pass->images[1] );
	backendState->gl.bindTexture( 2, pass->images[2] );

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_YUV, backendState->material.currentShader, programFeatures );

	// TODO: Should we set it to identity?
	mat4_t texMatrix = { 0 };
	RB_UpdateCommonUniforms( backendState, pass, texMatrix );

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_ColorCorrection( BackendState *backendState, const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	programFeatures &= ~GLSL_SHADER_COMMON_SRGB2LINEAR;
	if( pass->images[1] ) { // lut
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_LUT;
	}
	if( pass->images[2] ) { // output bloom
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_OVERBRIGHT;
	}
	if( pass->images[3] ) { // apply bloom
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_BLOOM;
	}

	if( pass->images[0]->flags & IT_FLOAT ) {
		if( glConfig.sSRGB ) {
			programFeatures |= GLSL_SHADER_COMMON_SRGB2LINEAR;
		}
		if( v_hdr.get() ) {
			programFeatures |= GLSL_SHADER_COLOR_CORRECTION_HDR;
		}
	}

	RB_SetShaderpassState( backendState, pass->flags );

	backendState->gl.bindTexture( 0, pass->images[0] );
	if( pass->images[1] ) {
		backendState->gl.bindTexture( 1, pass->images[1] );
	}
	for( int i = 0; i < NUM_BLOOM_LODS; i++ ) {
		if( pass->images[3 + i] ) {
			backendState->gl.bindTexture( 2 + i, pass->images[3 + i] );
		}
	}

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_COLOR_CORRECTION, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateColorCorrectionUniforms( backendState, v_hdrGamma.get(), backendState->global.hdrExposure );

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSL_KawaseBlur( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, uint64_t programFeatures ) {
	RB_SetShaderpassState( backendState, pass->flags );

	backendState->gl.bindTexture( 0, pass->images[0] );

	RB_SetupProgram( backendState, GLSL_PROGRAM_TYPE_KAWASE_BLUR, backendState->material.currentShader, programFeatures );

	mat4_t texMatrix = { 0 };
	Matrix4_Identity( texMatrix );

	RB_UpdateCommonUniforms( backendState, pass, texMatrix );
	RP_UpdateKawaseUniforms( backendState, pass->images[0]->width, pass->images[0]->height, pass->anim_numframes );

	RB_DrawMeshVerts( backendState, vertSpan, primitive );
}

static void RB_RenderMeshGLSLProgrammed( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass, unsigned programType ) {
	uint64_t features = 0;

	if( backendState->material.greyscale || pass->flags & SHADERPASS_GREYSCALE ) {
		features |= GLSL_SHADER_COMMON_GREYSCALE;
	}

	features |= RB_BonesTransformsToProgramFeatures( backendState );
	features |= RB_AutospriteProgramFeatures( backendState );
	features |= RB_InstancedArraysProgramFeatures( backendState );
	features |= RB_AlphatestProgramFeatures( pass );
	features |= RB_TcModsProgramFeatures( pass );
	features |= RB_sRGBProgramFeatures( pass );

	/*
	if( ( rb.currentShader->flags & SHADER_SOFT_PARTICLE )
		&& rb.st.screenDepthTexCopy
		&& ( rb.renderFlags & RF_SOFT_PARTICLES ) ) {
		features |= GLSL_SHADER_COMMON_SOFT_PARTICLE;
	}*/

	switch( programType ) {
		case GLSL_PROGRAM_TYPE_MATERIAL:
			return RB_RenderMeshGLSL_Material( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_DISTORTION:
			return RB_RenderMeshGLSL_Distortion( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_RGB_SHADOW:
			[[fallthrough]];
		case GLSL_PROGRAM_TYPE_SHADOWMAP:
			return;
		case GLSL_PROGRAM_TYPE_OUTLINE:
			return RB_RenderMeshGLSL_Outline( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_Q3A_SHADER:
			return RB_RenderMeshGLSL_Q3AShader( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_CELSHADE:
			return RB_RenderMeshGLSL_Celshade( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_FOG:
			return RB_RenderMeshGLSL_Fog( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_FXAA:
			return RB_RenderMeshGLSL_FXAA( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_YUV:
			return RB_RenderMeshGLSL_YUV( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_COLOR_CORRECTION:
			return RB_RenderMeshGLSL_ColorCorrection( backendState, fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_KAWASE_BLUR:
			return RB_RenderMeshGLSL_KawaseBlur( backendState, fsh, vertSpan, primitive, pass, features );
		default:
			wsw::failWithLogicError( "Unreachable" );
	}
}

static void RB_RenderPass( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass ) {
	// for depth texture we render light's view to, ignore passes that do not write into depth buffer
	if( ( backendState->global.renderFlags & RF_SHADOWMAPVIEW ) && !( pass->flags & GLSTATE_DEPTHWRITE ) ) [[unlikely]] {
		return;
	}

	const unsigned programType = pass->program_type ? pass->program_type : GLSL_PROGRAM_TYPE_Q3A_SHADER;
	RB_RenderMeshGLSLProgrammed( backendState, fsh, vertSpan, primitive, pass, programType );

	if( backendState->draw.dirtyUniformState ) {
		backendState->draw.donePassesTotal = 0;
		backendState->draw.dirtyUniformState = false;
	}

	if( backendState->gl.getState() & GLSTATE_DEPTHWRITE ) {
		backendState->draw.doneDepthPass = true;
	}

	backendState->draw.donePassesTotal++;
}

static void RB_UpdateCurrentShaderState( BackendState *backendState ) {
	const unsigned shaderFlags = backendState->material.currentShader->flags;

	// Face culling
	if( backendState->material.currentEntity->rtype == RT_SPRITE ) {
		backendState->gl.setCull( 0 );
	} else if( shaderFlags & SHADER_CULL_FRONT ) {
		backendState->gl.setCull( GL_FRONT );
	} else if( shaderFlags & SHADER_CULL_BACK ) {
		backendState->gl.setCull( GL_BACK );
	} else {
		backendState->gl.setCull( 0 );
	}

	unsigned stateFlags = 0;

	if( shaderFlags & SHADER_POLYGONOFFSET ) {
		stateFlags |= GLSTATE_OFFSET_FILL;
	}
	if( shaderFlags & SHADER_STENCILTEST ) {
		stateFlags |= GLSTATE_STENCIL_TEST;
	}

	if( backendState->material.noDepthTest ) {
		stateFlags |= GLSTATE_NO_DEPTH_TEST;
	}

	backendState->draw.currentShaderState = ( stateFlags & backendState->global.shaderStateANDmask ) | backendState->global.shaderStateORmask;
}

static void RB_SetShaderpassState( BackendState *backendState, unsigned passStateFlags ) {
	passStateFlags |= backendState->draw.currentShaderState;
	if( backendState->material.alphaHack ) {
		if( !( passStateFlags & GLSTATE_BLEND_MASK ) ) {
			// force alpha blending
			passStateFlags = ( passStateFlags & ~GLSTATE_DEPTHWRITE ) | GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
		}
	}
	if( backendState->material.noColorWrite ) {
		passStateFlags |= GLSTATE_NO_COLORWRITE;
	}
	if( backendState->material.depthEqual && ( passStateFlags & GLSTATE_DEPTHWRITE ) ) {
		passStateFlags |= GLSTATE_DEPTHFUNC_EQ;
	}

	backendState->gl.setState( passStateFlags );
}

static bool RB_TryExecutingSinglePassReusingBoundState( BackendState *backendState, const DrawMeshVertSpan *vertSpan, int primitive ) {
	// reuse current GLSL state (same program bound, same uniform values)
	if( !backendState->draw.dirtyUniformState && backendState->draw.donePassesTotal == 1 && backendState->program.boundProgram >= 0 ) {
		RB_DrawMeshVerts( backendState, vertSpan, primitive );
		return true;
	}
	return false;
}

static inline const vec_t *RB_TriangleLinesColor( const BackendState *backendState ) {
	if( v_showTris.get() != 2 ) {
		return colorWhite;
	}
	if( backendState->material.currentModelType == mod_brush ) {
		return colorBlack;
	}
	if( backendState->material.currentModelType != mod_bad ) {
		return colorRed;
	}
	if( backendState->material.currentEntity && backendState->material.currentEntity->number != kWorldEntNumber ) {
		return colorBlue;
	}
	return colorGreen;
}

void RB_DrawWireframeMesh( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive ) {
	if( RB_TryExecutingSinglePassReusingBoundState( backendState, vertSpan, primitive ) ) {
		return;
	}

	const shaderpass_t *referencePass;
	if( !backendState->material.currentShader->numpasses ) {
		// happens on fog volumes
		referencePass = &kBuiltinFogPass;
	} else {
		referencePass = &backendState->material.currentShader->passes[0];
	}

	shaderpass_t wireframePass = *referencePass;

	const float *const wireColor = RB_TriangleLinesColor( backendState );
	wireframePass.rgbgen.type = RGB_GEN_CONST;
	VectorCopy( wireColor, wireframePass.rgbgen.args );
	wireframePass.alphagen.type = ALPHA_GEN_CONST;
	VectorSet( wireframePass.alphagen.args, wireColor[3], wireColor[3], wireColor[3] );

	wireframePass.flags = 0;
	wireframePass.images[0] = TextureCache::instance()->whiteTexture();
	wireframePass.anim_fps = 0;
	wireframePass.anim_numframes = 0;
	wireframePass.program_type = GLSL_PROGRAM_TYPE_Q3A_SHADER;

	backendState->draw.currentShadowBits = 0;
	backendState->draw.currentDlightBits = 0;

	// TODO: Modifying the material state
	backendState->material.colorFog    = nullptr;
	backendState->material.texFog      = nullptr;
	backendState->draw.superLightStyle = nullptr;

	RB_UpdateCurrentShaderState( backendState );

	RB_RenderPass( backendState, fsh, vertSpan, primitive, &wireframePass );
}

// TODO: Show outlines in mirrors
#define ENTITY_OUTLINE( ent ) ( ( ( ( ent )->renderfx & RF_VIEWERMODEL ) ) ? 0 : ( ent )->outlineHeight )

void RB_DrawShadedMesh( BackendState *backendState, const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive ) {
	if( RB_TryExecutingSinglePassReusingBoundState( backendState, vertSpan, primitive ) ) {
		return;
	}

	const unsigned sort      = backendState->material.currentShader->sort;
	const unsigned numPasses = backendState->material.currentShader->numpasses;

	bool addGLSLOutline = false;
	if( ENTITY_OUTLINE( backendState->material.currentEntity ) ) {
		if( !( backendState->global.renderFlags & ( RF_CLIPPLANE | RF_SHADOWMAPVIEW ) ) ) [[likely]] {
			if( sort == SHADER_SORT_OPAQUE && ( backendState->material.currentShader->flags & SHADER_CULL_FRONT ) ) {
				addGLSLOutline = true;
			}
		}
	}

	RB_UpdateCurrentShaderState( backendState );

	for( unsigned passNum = 0; passNum < numPasses; ++passNum ) {
		const shaderpass_t *pass = backendState->material.currentShader->passes + passNum;
		if( !( pass->flags & SHADERPASS_DETAIL ) || v_detailTextures.get() ) {
			if( !( pass->flags & SHADERPASS_LIGHTMAP ) ) [[likely]] {
				RB_RenderPass( backendState, fsh, vertSpan, primitive, pass );
			}
		}
	}

	if( backendState->draw.currentShadowBits && sort >= SHADER_SORT_OPAQUE && sort <= SHADER_SORT_ALPHATEST ) {
		RB_RenderPass( backendState, fsh, vertSpan, primitive, &kBuiltinShadowmapPass );
	}

	if( addGLSLOutline ) {
		RB_RenderPass( backendState, fsh, vertSpan, primitive, &kBuiltinOutlinePass );
	}

	if( backendState->material.texFog && backendState->material.texFog->shader ) {
		shaderpass_t fogPass = kBuiltinFogPass;

		fogPass.images[0] = TextureCache::instance()->whiteTexture();
		if( !numPasses || backendState->material.currentShader->fog_dist != 0.0f ) {
			fogPass.flags &= ~GLSTATE_DEPTHFUNC_EQ;
		} else {
			fogPass.flags |= GLSTATE_DEPTHFUNC_EQ;
		}

		RB_RenderPass( backendState, fsh, vertSpan, primitive, &fogPass );
	}
}
