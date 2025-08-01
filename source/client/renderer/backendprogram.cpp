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
#include "backendlocal.h"

#define FTABLE_SIZE_POW 12
#define FTABLE_SIZE ( 1 << FTABLE_SIZE_POW )
#define FTABLE_CLAMP( x ) ( ( (int)( ( x ) * FTABLE_SIZE ) & ( FTABLE_SIZE - 1 ) ) )
#define FTABLE_EVALUATE( table, x ) ( ( table )[FTABLE_CLAMP( fmod( x, 1.0 ) )] )

#define DRAWFLAT() ( ( rb.currentModelType == mod_brush ) && ( rb.renderFlags & ( RF_DRAWFLAT | RF_DRAWBRIGHT ) ) && !( rb.currentShader->flags & SHADER_NODRAWFLAT ) )

enum {
	BUILTIN_GLSLPASS_FOG,
	BUILTIN_GLSLPASS_SHADOWMAP,
	BUILTIN_GLSLPASS_OUTLINE,
	BUILTIN_GLSLPASS_SKYBOX,
	MAX_BUILTIN_GLSLPASSES
};

static float rb_sintable[FTABLE_SIZE];
static float rb_triangletable[FTABLE_SIZE];
static float rb_squaretable[FTABLE_SIZE];
static float rb_sawtoothtable[FTABLE_SIZE];
static float rb_inversesawtoothtable[FTABLE_SIZE];

#define NOISE_SIZE  256
#define NOISE_VAL( a )    rb_noiseperm[( a ) & ( NOISE_SIZE - 1 )]
#define NOISE_INDEX( x, y, z, t ) NOISE_VAL( x + NOISE_VAL( y + NOISE_VAL( z + NOISE_VAL( t ) ) ) )
#define NOISE_LERP( a, b, w ) ( a * ( 1.0f - w ) + b * w )

static float rb_noisetable[NOISE_SIZE];
static int rb_noiseperm[NOISE_SIZE];

static shaderpass_t r_GLSLpasses[MAX_BUILTIN_GLSLPASSES];

static void RB_SetShaderpassState( int state );

static void RB_RenderMeshGLSL_Material( const FrontendToBackendShared *fsh, const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_Distortion( const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_Outline( const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_Q3AShader( const FrontendToBackendShared *fsh, const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_Celshade( const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_Fog( const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_FXAA( const shaderpass_t *pass, uint64_t programFeatures );
static void RB_RenderMeshGLSL_YUV( const shaderpass_t *pass, uint64_t programFeatures );

int RP_GetProgramObject( int elem );

void RP_UpdateShaderUniforms( int elem,
							  float shaderTime,
							  const vec3_t entOrigin, const vec3_t entDist, const uint8_t *entityColor,
							  const uint8_t *constColor, const float *rgbGenFuncArgs, const float *alphaGenFuncArgs,
							  const mat4_t texMatrix, float colorMod );

void RP_UpdateViewUniforms( int elem,
							const mat4_t modelviewMatrix, const mat4_t modelviewProjectionMatrix,
							const vec3_t viewOrigin, const mat3_t viewAxis,
							const float mirrorSide,
							int viewport[4],
							float zNear, float zFar );

void RP_UpdateBlendMixUniform( int elem, vec2_t blendMask );

void RP_UpdateSoftParticlesUniforms( int elem, float scale );

void RP_UpdateMaterialUniforms( int elem,
								float offsetmappingScale, float glossIntensity, float glossExponent );

void RP_UpdateDistortionUniforms( int elem, bool frontPlane );

void RP_UpdateTextureUniforms( int elem, int TexWidth, int TexHeight );

void RP_UpdateOutlineUniforms( int elem, float projDistance );

void RP_UpdateDiffuseLightUniforms( int elem,
									const vec3_t lightDir, const vec4_t lightAmbient, const vec4_t lightDiffuse );

void RP_UpdateDynamicLightsUniforms( const FrontendToBackendShared *fsh,
									 int elem, const superLightStyle_t *superLightStyle,
									 const vec3_t entOrigin, const mat3_t entAxis, unsigned int dlightbits );

void RP_UpdateFogUniforms( int elem, byte_vec4_t color, float clearDist, float opaqueDist,
						   cplane_t *fogPlane, cplane_t *eyePlane, float eyeFogDist );

void RP_UpdateTexGenUniforms( int elem, const mat4_t reflectionMatrix, const mat4_t vectorMatrix );

void RP_UpdateBonesUniforms( int elem, unsigned int numBones, dualquat_t *animDualQuat );

void RP_UpdateInstancesUniforms( int elem, unsigned int numInstances, instancePoint_t *instances );

void RP_UpdateDrawFlatUniforms( int elem, const vec3_t wallColor, const vec3_t floorColor );

void RP_UpdateColorCorrectionUniforms( int elem, float hdrGamme, float hdrExposure );

void RP_UpdateKawaseUniforms( int elem, int TexWidth, int TexHeight, int iteration );

/*
* RB_InitBuiltinPasses
*/
static void RB_InitBuiltinPasses( void ) {
	shaderpass_t *pass;

	// init optional GLSL program passes
	memset( r_GLSLpasses, 0, sizeof( r_GLSLpasses ) );

	// init fog pass
	pass = &r_GLSLpasses[BUILTIN_GLSLPASS_FOG];
	pass->program_type = GLSL_PROGRAM_TYPE_Q3A_SHADER;
	pass->tcgen = TC_GEN_FOG;
	pass->rgbgen.type = RGB_GEN_FOG;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->flags = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
	pass->program_type = GLSL_PROGRAM_TYPE_FOG;

	// shadowmap
	pass = &r_GLSLpasses[BUILTIN_GLSLPASS_SHADOWMAP];
	pass->flags = GLSTATE_DEPTHFUNC_EQ /*|GLSTATE_OFFSET_FILL*/ | GLSTATE_SRCBLEND_ZERO | GLSTATE_DSTBLEND_SRC_COLOR;
	pass->tcgen = TC_GEN_NONE;
	pass->rgbgen.type = RGB_GEN_IDENTITY;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->program_type = GLSL_PROGRAM_TYPE_SHADOWMAP;

	// outlines
	pass = &r_GLSLpasses[BUILTIN_GLSLPASS_OUTLINE];
	pass->flags = GLSTATE_DEPTHWRITE;
	pass->rgbgen.type = RGB_GEN_OUTLINE;
	pass->alphagen.type = ALPHA_GEN_OUTLINE;
	pass->tcgen = TC_GEN_NONE;
	pass->program_type = GLSL_PROGRAM_TYPE_OUTLINE;

	// skybox
	pass = &r_GLSLpasses[BUILTIN_GLSLPASS_SKYBOX];
	pass->program_type = GLSL_PROGRAM_TYPE_Q3A_SHADER;
	pass->tcgen = TC_GEN_BASE;
	pass->rgbgen.type = RGB_GEN_IDENTITY;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
}

/*
* RB_InitShading
*/
void RB_InitShading( void ) {
	int i;
	float t;

	// build lookup tables
	for( i = 0; i < FTABLE_SIZE; i++ ) {
		t = (float)i / (float)FTABLE_SIZE;

		rb_sintable[i] = sin( t * M_TWOPI );

		if( t < 0.25 ) {
			rb_triangletable[i] = t * 4.0;
		} else if( t < 0.75 ) {
			rb_triangletable[i] = 2 - 4.0 * t;
		} else {
			rb_triangletable[i] = ( t - 0.75 ) * 4.0 - 1.0;
		}

		if( t < 0.5 ) {
			rb_squaretable[i] = 1.0f;
		} else {
			rb_squaretable[i] = -1.0f;
		}

		rb_sawtoothtable[i] = t;
		rb_inversesawtoothtable[i] = 1.0 - t;
	}

	// init the noise table
	srand( 1001 );

	for( i = 0; i < NOISE_SIZE; i++ ) {
		rb_noisetable[i] = (float)( ( ( rand() / (float)RAND_MAX ) * 2.0 - 1.0 ) );
		rb_noiseperm[i] = (unsigned char)( rand() / (float)RAND_MAX * 255 );
	}

	RB_InitBuiltinPasses();
}

/*
* RB_FastSin
*/
static inline float RB_FastSin( float t ) {
	return FTABLE_EVALUATE( rb_sintable, t );
}

/*
* RB_TableForFunc
*/
static float *RB_TableForFunc( unsigned int func ) {
	switch( func ) {
		case SHADER_FUNC_SIN:
			return rb_sintable;
		case SHADER_FUNC_TRIANGLE:
			return rb_triangletable;
		case SHADER_FUNC_SQUARE:
			return rb_squaretable;
		case SHADER_FUNC_SAWTOOTH:
			return rb_sawtoothtable;
		case SHADER_FUNC_INVERSESAWTOOTH:
			return rb_inversesawtoothtable;
		default:
			break;
	}

	return rb_sintable;  // default to sintable
}

/*
* RB_BackendGetNoiseValue
*/
static float RB_BackendGetNoiseValue( float x, float y, float z, float t ) {
	int i;
	int ix, iy, iz, it;
	float fx, fy, fz, ft;
	float front[4], back[4];
	float fvalue, bvalue, value[2], finalvalue;

	ix = ( int )floor( x );
	fx = x - ix;
	iy = ( int )floor( y );
	fy = y - iy;
	iz = ( int )floor( z );
	fz = z - iz;
	it = ( int )floor( t );
	ft = t - it;

	for( i = 0; i < 2; i++ ) {
		front[0] = rb_noisetable[NOISE_INDEX( ix, iy, iz, it + i )];
		front[1] = rb_noisetable[NOISE_INDEX( ix + 1, iy, iz, it + i )];
		front[2] = rb_noisetable[NOISE_INDEX( ix, iy + 1, iz, it + i )];
		front[3] = rb_noisetable[NOISE_INDEX( ix + 1, iy + 1, iz, it + i )];

		back[0] = rb_noisetable[NOISE_INDEX( ix, iy, iz + 1, it + i )];
		back[1] = rb_noisetable[NOISE_INDEX( ix + 1, iy, iz + 1, it + i )];
		back[2] = rb_noisetable[NOISE_INDEX( ix, iy + 1, iz + 1, it + i )];
		back[3] = rb_noisetable[NOISE_INDEX( ix + 1, iy + 1, iz + 1, it + i )];

		fvalue = NOISE_LERP( NOISE_LERP( front[0], front[1], fx ), NOISE_LERP( front[2], front[3], fx ), fy );
		bvalue = NOISE_LERP( NOISE_LERP( back[0], back[1], fx ), NOISE_LERP( back[2], back[3], fx ), fy );
		value[i] = NOISE_LERP( fvalue, bvalue, fz );
	}

	finalvalue = NOISE_LERP( value[0], value[1], ft );

	return finalvalue;
}

/*
* RB_TransformFogPlanes
*/
static float RB_TransformFogPlanes( const mfog_t *fog, vec3_t fogNormal,
									vec_t *fogDist, vec3_t vpnNormal, vec_t *vpnDist ) {
	cplane_t *fogPlane;
	shader_t *fogShader;
	vec3_t viewtofog;
	float dist, scale;
	const entity_t *e = rb.currentEntity;

	assert( fog );
	assert( fogNormal && fogDist );
	assert( vpnNormal && vpnDist );

	fogPlane = fog->visibleplane;
	fogShader = fog->shader;

	// distance to fog
	dist = PlaneDiff( rb.cameraOrigin, fog->visibleplane );
	scale = e->scale;

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

	Matrix3_TransformVector( e->axis, rb.cameraAxis, vpnNormal );
	VectorScale( vpnNormal, scale, vpnNormal );
	*vpnDist = ( ( rb.cameraOrigin[0] - viewtofog[0] ) * rb.cameraAxis[AXIS_FORWARD + 0] +
				 ( rb.cameraOrigin[1] - viewtofog[1] ) * rb.cameraAxis[AXIS_FORWARD + 1] +
				 ( rb.cameraOrigin[2] - viewtofog[2] ) * rb.cameraAxis[AXIS_FORWARD + 2] ) +
			   fogShader->fog_clearDist;

	return dist;
}

/*
* RB_VertexTCCelshadeMatrix
*/
void RB_VertexTCCelshadeMatrix( mat4_t matrix ) {
	vec3_t dir;
	mat4_t m;
	const entity_t *e = rb.currentEntity;

	if( e->model != NULL && !( rb.renderFlags & RF_SHADOWMAPVIEW ) ) {
		R_LightForOrigin( e->lightingOrigin, dir, NULL, NULL, e->model->radius * e->scale, rb.noWorldLight );

		Matrix4_Identity( m );

		// rotate direction
		Matrix3_TransformVector( e->axis, dir, &m[0] );
		VectorNormalize( &m[0] );

		MakeNormalVectors( &m[0], &m[4], &m[8] );
		Matrix4_Transpose( m, matrix );
	}
}

/*
* RB_ApplyTCMods
*/
void RB_ApplyTCMods( const shaderpass_t *pass, mat4_t result ) {
	unsigned i;
	const float *table;
	double t1, t2, sint, cost;
	mat4_t m1, m2;
	const tcmod_t *tcmod;

	for( i = 0, tcmod = pass->tcmods; i < pass->numtcmods; i++, tcmod++ ) {
		switch( tcmod->type ) {
			case TC_MOD_ROTATE:
				cost = tcmod->args[0] * rb.currentShaderTime;
				sint = RB_FastSin( cost );
				cost = RB_FastSin( cost + 0.25 );
				m2[0] =  cost, m2[1] = sint, m2[12] =  0.5f * ( sint - cost + 1 );
				m2[4] = -sint, m2[5] = cost, m2[13] = -0.5f * ( sint + cost - 1 );
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
				break;
			case TC_MOD_SCALE:
				Matrix4_Scale2D( result, tcmod->args[0], tcmod->args[1] );
				break;
			case TC_MOD_TURB:
				t1 = ( 1.0 / 4.0 );
				t2 = tcmod->args[2] + rb.currentShaderTime * tcmod->args[3];
				Matrix4_Scale2D( result,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 ) + tcmod->args[0] ) * t1,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 + 0.25 ) + tcmod->args[0] ) * t1 );
				break;
			case TC_MOD_STRETCH:
				table = RB_TableForFunc( tcmod->args[0] );
				t2 = tcmod->args[3] + rb.currentShaderTime * tcmod->args[4];
				t1 = FTABLE_EVALUATE( table, t2 ) * tcmod->args[2] + tcmod->args[1];
				t1 = t1 ? 1.0f / t1 : 1.0f;
				t2 = 0.5f - 0.5f * t1;
				Matrix4_Stretch2D( result, t1, t2 );
				break;
			case TC_MOD_SCROLL:
				t1 = tcmod->args[0] * rb.currentShaderTime;
				t2 = tcmod->args[1] * rb.currentShaderTime;
				if( pass->program_type != GLSL_PROGRAM_TYPE_DISTORTION ) { // HACK HACK HACK
					t1 = t1 - floor( t1 );
					t2 = t2 - floor( t2 );
				}
				Matrix4_Translate2D( result, t1, t2 );
				break;
			case TC_MOD_TRANSFORM:
				m2[0] = tcmod->args[0], m2[1] = tcmod->args[2], m2[12] = tcmod->args[4],
				m2[5] = tcmod->args[1], m2[4] = tcmod->args[3], m2[13] = tcmod->args[5];
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
				break;
			default:
				break;
		}
	}
}

/*
* RB_GetShaderpassColor
*/
void RB_GetShaderpassColor( const shaderpass_t *pass, byte_vec4_t rgba_, float *colorMod ) {
	int c;
	int rgba[4];
	double temp;
	float *table, a;
	vec3_t v;
	const shaderfunc_t *rgbgenfunc = &pass->rgbgen.func;
	const shaderfunc_t *alphagenfunc = &pass->alphagen.func;

	Vector4Set( rgba, 255, 255, 255, 255 );

	switch( pass->rgbgen.type ) {
		case RGB_GEN_IDENTITY:
			break;
		case RGB_GEN_CONST:
			rgba[0] = ( int )( pass->rgbgen.args[0] * 255.0f );
			rgba[1] = ( int )( pass->rgbgen.args[1] * 255.0f );
			rgba[2] = ( int )( pass->rgbgen.args[2] * 255.0f );
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
				if( rgbgenfunc->type == SHADER_FUNC_NOISE ) {
					temp = RB_BackendGetNoiseValue( 0, 0, 0, ( rb.currentShaderTime + rgbgenfunc->args[2] ) * rgbgenfunc->args[3] );
				} else {
					table = RB_TableForFunc( rgbgenfunc->type );
					temp = rb.currentShaderTime * rgbgenfunc->args[3] + rgbgenfunc->args[2];
					temp = FTABLE_EVALUATE( table, temp ) * rgbgenfunc->args[1] + rgbgenfunc->args[0];
				}
				temp = temp * rgbgenfunc->args[1] + rgbgenfunc->args[0];
			}

			if( pass->rgbgen.type == RGB_GEN_ENTITYWAVE ) {
				VectorSet( v,
						   rb.entityColor[0] * ( 1.0 / 255.0 ),
						   rb.entityColor[1] * ( 1.0 / 255.0 ),
						   rb.entityColor[2] * ( 1.0 / 255.0 ) );
			} else if( pass->rgbgen.type == RGB_GEN_CUSTOMWAVE ) {
				c = R_GetCustomColor( (int)pass->rgbgen.args[0] );
				VectorSet( v,
						   COLOR_R( c ) * ( 1.0 / 255.0 ),
						   COLOR_G( c ) * ( 1.0 / 255.0 ),
						   COLOR_B( c ) * ( 1.0 / 255.0 ) );
			} else {
				VectorCopy( pass->rgbgen.args, v );
			}

			a = v[0]; rgba[0] = ( int )( a * 255.0f );
			a = v[1]; rgba[1] = ( int )( a * 255.0f );
			a = v[2]; rgba[2] = ( int )( a * 255.0f );
			*colorMod = (float)temp;
			break;
		case RGB_GEN_OUTLINE:
			rgba[0] = rb.entityOutlineColor[0];
			rgba[1] = rb.entityOutlineColor[1];
			rgba[2] = rb.entityOutlineColor[2];
			break;
		case RGB_GEN_ONE_MINUS_ENTITY:
			rgba[0] = 255 - rb.entityColor[0];
			rgba[1] = 255 - rb.entityColor[1];
			rgba[2] = 255 - rb.entityColor[2];
			break;
		case RGB_GEN_FOG:
			rgba[0] = rb.texFog->shader->fog_color[0];
			rgba[1] = rb.texFog->shader->fog_color[1];
			rgba[2] = rb.texFog->shader->fog_color[2];
			break;
		case RGB_GEN_ENVIRONMENT:
			rgba[0] = mapConfig.environmentColor[0];
			rgba[1] = mapConfig.environmentColor[1];
			rgba[2] = mapConfig.environmentColor[2];
			break;
		default:
			break;
	}

	switch( pass->alphagen.type ) {
		case ALPHA_GEN_IDENTITY:
			break;
		case ALPHA_GEN_CONST:
			rgba[3] = ( int )( pass->alphagen.args[0] * 255.0f );
			break;
		case ALPHA_GEN_WAVE:
			if( !alphagenfunc || alphagenfunc->type == SHADER_FUNC_NONE ) {
				a = 1;
			} else if( alphagenfunc->type == SHADER_FUNC_RAMP ) {
				break;
			} else {
				if( alphagenfunc->type == SHADER_FUNC_NOISE ) {
					a = RB_BackendGetNoiseValue( 0, 0, 0, ( rb.currentShaderTime + alphagenfunc->args[2] ) * alphagenfunc->args[3] );
				} else {
					table = RB_TableForFunc( alphagenfunc->type );
					a = alphagenfunc->args[2] + rb.currentShaderTime * alphagenfunc->args[3];
					a = FTABLE_EVALUATE( table, a );
				}

				a = a * alphagenfunc->args[1] + alphagenfunc->args[0];
			}

			rgba[3] = ( int )( a * 255.0f );
			break;
		case ALPHA_GEN_ENTITY:
			rgba[3] = rb.entityColor[3];
			break;
		case ALPHA_GEN_OUTLINE:
			rgba[3] = rb.entityOutlineColor[3];
		default:
			break;
	}

	for( c = 0; c < 4; c++ ) {
		rgba_[c] = bound( 0, rgba[c], 255 );
	}
}

/*
* RB_ShaderpassTex
*/
static inline Texture *RB_ShaderpassTex( const shaderpass_t *pass ) {
	if( pass->anim_numframes ) {
		Texture *res;
		if( pass->anim_fps > 0.0f ) {
			res = pass->images[(int)( pass->anim_fps * rb.currentShaderTime ) % pass->anim_numframes];
		} else {
			assert( rb.currentShaderFrac >= 0.0f && rb.currentShaderFrac <= 1.0f );
			assert( pass->timelineFracs[0] == 0.0f );
			res = pass->images[0];
			for( unsigned i = 1; i < pass->anim_numframes; ++i ) {
				assert( pass->timelineFracs[i] >= 0.0f && pass->timelineFracs[i] < 1.0f );
				if( pass->timelineFracs[i] < rb.currentShaderFrac ) {
					res = pass->images[i];
				} else {
					break;
				}
			}
		}
		return res;
	}

	auto *const textureCache = TextureCache::instance();
	if( pass->flags & SHADERPASS_PORTALMAP ) {
		return rb.currentPortalSurface && rb.currentPortalSurface->texures[0] ?
			   rb.currentPortalSurface->texures[0] : textureCache->blackTexture();
	}

	Texture *const tex = pass->images[0];
	return tex ? tex : textureCache->noTexture();
}

//==================================================================================

/*
* RB_RGBAlphaGenToProgramFeatures
*/
static int RB_RGBAlphaGenToProgramFeatures( const colorgen_t *rgbgen, const colorgen_t *alphagen ) {
	uint64_t programFeatures;
	int identity;

	identity = 0;
	programFeatures = 0;

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

	if( identity == 2 && !rb.alphaHack ) {
		return 0;
	}

	return programFeatures;
}

/*
* RB_BonesTransformsToProgramFeatures
*/
static uint64_t RB_BonesTransformsToProgramFeatures( void ) {
	// check whether the current model is actually sketetal
	if( rb.currentModelType != mod_skeletal ) {
		return 0;
	}
	// base pose sketetal models aren't animated and rendered as-is
	if( !rb.bonesData.numBones ) {
		return 0;
	}
	return rb.bonesData.maxWeights * GLSL_SHADER_COMMON_BONE_TRANSFORMS1;
}

/*
* RB_DlightbitsToProgramFeatures
*/
static uint64_t RB_DlightbitsToProgramFeatures( unsigned int dlightBits ) {
	int numDlights;

	if( !dlightBits ) {
		return 0;
	}

	numDlights = Q_bitcount( dlightBits );
	if( r_lighting_maxglsldlights->integer && numDlights > r_lighting_maxglsldlights->integer ) {
		numDlights = r_lighting_maxglsldlights->integer;
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

/*
* RB_AutospriteProgramFeatures
*/
static uint64_t RB_AutospriteProgramFeatures( void ) {
	uint64_t programFeatures = 0;
	if( ( rb.currentVAttribs & VATTRIB_AUTOSPRITE2_BIT ) == VATTRIB_AUTOSPRITE2_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE2;
	} else if( ( rb.currentVAttribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE;
	}
	return programFeatures;
}

/*
* RB_InstancedArraysProgramFeatures
*/
static uint64_t RB_InstancedArraysProgramFeatures( void ) {
	uint64_t programFeatures = 0;
	if( ( rb.currentVAttribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS;
	} else if( /*rb.drawCallData.numInstances*/ false ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS;
	}
	return programFeatures;
}

/*
* RB_FogProgramFeatures
*/
static uint64_t RB_FogProgramFeatures( const shaderpass_t *pass, const mfog_t *fog ) {
	uint64_t programFeatures = 0;
	if( fog ) {
		programFeatures |= GLSL_SHADER_COMMON_FOG;
		if( fog == rb.colorFog ) {
			programFeatures |= GLSL_SHADER_COMMON_FOG_RGB;
		}
	}
	return programFeatures;
}

/*
* RB_AlphatestProgramFeatures
*/
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

/*
* RB_TcModsProgramFeatures
*/
static uint64_t RB_TcModsProgramFeatures( const shaderpass_t *pass ) {
	if( pass->numtcmods ) {
		return GLSL_SHADER_COMMON_TC_MOD;
	}
	return 0;
}

/*
* RB_sRGBProgramFeatures
*
* The main framebuffer is always srgb, otherwise assume linear
*/
static uint64_t RB_sRGBProgramFeatures( const shaderpass_t *pass ) {
	uint64_t f = 0;

	if( pass->flags & SHADERPASS_NOSRGB ) {
		return 0; // don't do srgb<->linear conversions at all, used for blitting framebuffers

	}
	if( glConfig.sSRGB ) {
		f |= GLSL_SHADER_COMMON_SRGB2LINEAR;

		// ok, so we're getting sRGB linear input while rendering to
		// default framebuffer, so we need to go back from linear to sRGB
		f |= GLSL_SHADER_COMMON_LINEAR2SRB;
	}

	return f;
}

/*
* RB_UpdateCommonUniforms
*/
static void RB_UpdateCommonUniforms( int program, const shaderpass_t *pass, mat4_t texMatrix ) {
	vec3_t entDist, entOrigin;
	byte_vec4_t constColor;
	float colorMod = 1.0f;
	const entity_t *e = rb.currentEntity;
	vec3_t tmp;
	vec2_t blendMix = { 0, 0 };

	// the logic here should match R_TransformForEntity
	if( e->rtype != RT_MODEL ) {
		VectorClear( entOrigin );
		VectorCopy( rb.cameraOrigin, entDist );
	} else {
		VectorCopy( e->origin, entOrigin );
		VectorSubtract( rb.cameraOrigin, e->origin, tmp );
		Matrix3_TransformVector( e->axis, tmp, entDist );
	}

	// calculate constant color
	RB_GetShaderpassColor( pass, constColor, &colorMod );

	// apply modifications to texture coordinates
	if( pass->numtcmods ) {
		RB_ApplyTCMods( pass, texMatrix );
	}

	RP_UpdateViewUniforms( program,
						   rb.modelviewMatrix, rb.modelviewProjectionMatrix,
						   rb.cameraOrigin, rb.cameraAxis,
						   rb.renderFlags & RF_MIRRORVIEW ? -1 : 1,
						   rb.gl.viewport,
						   rb.zNear, rb.zFar
						   );

	if( RB_IsAlphaBlending( rb.gl.state & GLSTATE_SRCBLEND_MASK, rb.gl.state & GLSTATE_DSTBLEND_MASK ) ) {
		blendMix[1] = 1;
		if( rb.alphaHack ) {
			constColor[3] *= rb.hackedAlpha;
		}
	} else {
		blendMix[0] = 1;
		if( rb.alphaHack ) {
			constColor[0] *= rb.hackedAlpha, constColor[1] *= rb.hackedAlpha, constColor[2] *= rb.hackedAlpha;
		}
	}

	RP_UpdateShaderUniforms( program,
							 rb.currentShaderTime,
							 entOrigin, entDist, rb.entityColor,
							 constColor,
							 pass->rgbgen.func.type != SHADER_FUNC_NONE ? pass->rgbgen.func.args : pass->rgbgen.args,
							 pass->alphagen.func.type != SHADER_FUNC_NONE ? pass->alphagen.func.args : pass->alphagen.args,
							 texMatrix, colorMod );

	RP_UpdateBlendMixUniform( program, blendMix );

	RP_UpdateSoftParticlesUniforms( program, r_soft_particles_scale->value );
}

/*
* RB_UpdateFogUniforms
*/
static void RB_UpdateFogUniforms( int program, const mfog_t *fog ) {
	float dist;
	cplane_t fogPlane, vpnPlane;

	assert( fog != NULL );
	if( !fog ) {
		return;
	}

	dist = RB_TransformFogPlanes( fog, fogPlane.normal, &fogPlane.dist, vpnPlane.normal, &vpnPlane.dist );

	RP_UpdateFogUniforms( program, fog->shader->fog_color, fog->shader->fog_clearDist,
						  fog->shader->fog_dist, &fogPlane, &vpnPlane, dist );
}

static void RB_RenderMeshGLSL_Material( const FrontendToBackendShared *fsh, const shaderpass_t *pass, uint64_t programFeatures ) {
	int i;
	int program;
	vec3_t lightDir = { 0.0f, 0.0f, 0.0f };
	vec4_t ambient = { 0.0f, 0.0f, 0.0f, 0.0f }, diffuse = { 0.0f, 0.0f, 0.0f, 0.0f };
	float offsetmappingScale, glossIntensity, glossExponent;
	const superLightStyle_t *lightStyle = NULL;
	const mfog_t *fog = rb.fog;
	bool applyDecal;
	mat4_t texMatrix;

	auto *const textureCache = TextureCache::instance();

	// handy pointers
	Texture *base = RB_ShaderpassTex( pass );
	Texture *normalmap = pass->images[1] ? pass->images[1] : textureCache->blankNormalmap();
	Texture *glossmap = pass->images[2];
	Texture *decalmap = pass->images[3];
	Texture *entdecalmap = pass->images[4];

	// use blank image if the normalmap is too tiny due to high picmip value
	if( !normalmap || ( normalmap->width < 2 || normalmap->height < 2 ) ) {
		normalmap = textureCache->blankNormalmap();
	}

	if( ( rb.currentModelType == mod_brush && !mapConfig.deluxeMappingEnabled )
	    /*|| ( normalmap == rsh.blankBumpTexture && !glossmap && !decalmap && !entdecalmap )*/ ) {
		// render as plain Q3A shader, which is less computation-intensive
		RB_RenderMeshGLSL_Q3AShader( fsh, pass, programFeatures );
		return;
	}

	if( normalmap->samples == 4 ) {
		offsetmappingScale = r_offsetmapping_scale->value * rb.currentShader->offsetmappingScale;
	} else { // no alpha in normalmap, don't bother with offset mapping
		offsetmappingScale = 0;
	}

	glossIntensity = rb.currentShader->glossIntensity ? rb.currentShader->glossIntensity : r_lighting_glossintensity->value;
	glossExponent = rb.currentShader->glossExponent ? rb.currentShader->glossExponent : r_lighting_glossexponent->value;

	applyDecal = decalmap != NULL;

	// possibly apply the "texture" fog inline
	if( fog == rb.texFog ) {
		if( ( rb.currentShader->numpasses == 1 ) && !rb.currentShadowBits ) {
			rb.texFog = NULL;
		} else {
			fog = NULL;
		}
	}
	programFeatures |= RB_FogProgramFeatures( pass, fog );

	if( rb.currentModelType == mod_brush ) {
		// brush models
		if( !( r_offsetmapping->integer & 1 ) ) {
			offsetmappingScale = 0;
		}
		if( rb.renderFlags & RF_LIGHTMAP ) {
			programFeatures |= GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
		if( DRAWFLAT() ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT | GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
	} else if( rb.currentModelType == mod_bad ) {
		// polys
		if( !( r_offsetmapping->integer & 2 ) ) {
			offsetmappingScale = 0;
		}
	} else {
		// regular models
		if( !( r_offsetmapping->integer & 4 ) ) {
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
	if( rb.currentDlightBits ) {
		programFeatures |= RB_DlightbitsToProgramFeatures( rb.currentDlightBits );
	}

	Matrix4_Identity( texMatrix );

	RB_BindImage( 0, base );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( &pass->rgbgen, &pass->alphagen );

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	// we only send S-vectors to GPU and recalc T-vectors as cross product
	// in vertex shader
	RB_BindImage( 1, normalmap );         // normalmap

	if( glossmap && glossIntensity ) {
		programFeatures |= GLSL_SHADER_MATERIAL_SPECULAR;
		RB_BindImage( 2, glossmap ); // gloss
	}

	if( applyDecal ) {
		programFeatures |= GLSL_SHADER_MATERIAL_DECAL;

		if( rb.renderFlags & RF_LIGHTMAP ) {
			decalmap = textureCache->blackTexture();
			programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
		} else {
			// if no alpha, use additive blending
			if( decalmap->samples & 1 ) {
				programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
			}
		}

		RB_BindImage( 3, decalmap ); // decal
	}

	if( entdecalmap ) {
		programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL;

		// if no alpha, use additive blending
		if( entdecalmap->samples & 1 ) {
			programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL_ADD;
		}

		RB_BindImage( 4, entdecalmap ); // decal
	}

	if( offsetmappingScale > 0 ) {
		programFeatures |= r_offsetmapping_reliefmapping->integer ?
						   GLSL_SHADER_MATERIAL_RELIEFMAPPING : GLSL_SHADER_MATERIAL_OFFSETMAPPING;
	}

	if( rb.currentModelType == mod_brush ) {
		// world surface
		if( rb.superLightStyle && rb.superLightStyle->lightmapNum[0] >= 0 ) {
			lightStyle = rb.superLightStyle;

			// bind lightmap textures and set program's features for lightstyles
			for( i = 0; i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255; i++ )
				RB_BindImage( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] );

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
			VectorSet( ambient, rb.minLight, rb.minLight, rb.minLight );
			VectorSet( diffuse, rb.minLight, rb.minLight, rb.minLight );

			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT | GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_MIX;
		}
	} else {
		vec3_t temp;

		programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT;

		if( rb.currentModelType == mod_bad ) {
			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_FROM_NORMAL;

			VectorSet( lightDir, 0, 0, 0 );
			Vector4Set( ambient, 0, 0, 0, 0 );
			Vector4Set( diffuse, 1, 1, 1, 1 );
		} else {
			const entity_t *e = rb.currentEntity;

			if( e->flags & RF_FULLBRIGHT ) {
				Vector4Set( ambient, 1, 1, 1, 1 );
				Vector4Set( diffuse, 1, 1, 1, 1 );
			} else {
				if( e->model && e->number != kWorldEntNumber ) {
					// get weighted incoming direction of world and dynamic lights
					R_LightForOrigin( e->lightingOrigin, temp, ambient, diffuse,
									  e->model->radius * e->scale, rb.noWorldLight );
				} else {
					VectorSet( temp, 0.1f, 0.2f, 0.7f );
				}

				if( e->flags & RF_MINLIGHT ) {
					float minLight = rb.minLight;
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

	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_MATERIAL, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		// update uniforms

		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateMaterialUniforms( program, offsetmappingScale, glossIntensity, glossExponent );

		RP_UpdateDiffuseLightUniforms( program, lightDir, ambient, diffuse );

		if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
			RB_UpdateFogUniforms( program, fog );
		}

		// submit animation data
		if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
			RP_UpdateBonesUniforms( program, rb.bonesData.numBones, rb.bonesData.dualQuats );
		}

		// dynamic lights
		RP_UpdateDynamicLightsUniforms( fsh, program, lightStyle, rb.currentEntity->origin, rb.currentEntity->axis,
										rb.currentDlightBits );

		// r_drawflat
		if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
			if( rb.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
				RP_UpdateDrawFlatUniforms( program, colorWhite, colorWhite );
			} else {
				RP_UpdateDrawFlatUniforms( program, rsh.wallColor, rsh.floorColor );
			}
		}

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_Distortion
*/
static void RB_RenderMeshGLSL_Distortion( const shaderpass_t *pass, uint64_t programFeatures ) {
	int i;
	int width = 1, height = 1;
	int program;
	Texture *portaltexture[2];
	bool frontPlane;
	mat4_t texMatrix;

	if( !rb.currentPortalSurface ) {
		return;
	}

	auto *const textureCache = TextureCache::instance();
	auto *const blankTexture = textureCache->greyTexture();
	auto *const blankNormalmap = textureCache->blankNormalmap();
	for( i = 0; i < 2; i++ ) {
		portaltexture[i] = rb.currentPortalSurface->texures[i];
		if( !portaltexture[i] ) {
			portaltexture[i] = blankTexture;
		} else {
			width = portaltexture[i]->width;
			height = portaltexture[i]->height;
		}
	}

	Texture *const dudvmap = pass->images[0] ? pass->images[0] : blankNormalmap;
	Texture *const normalmap = pass->images[1] ? pass->images[1] : blankNormalmap;
	if( dudvmap != blankNormalmap ) {
		programFeatures |= GLSL_SHADER_DISTORTION_DUDV;
	}

	if( portaltexture[0] == blankTexture && portaltexture[1] == blankTexture ) {
		// Let it be actually drawn
		programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
	} else {
		if( portaltexture[0] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
		}
		if( portaltexture[1] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFRACTION;
		}
	}

	frontPlane = ( PlaneDiff( rb.cameraOrigin, &rb.currentPortalSurface->untransformed_plane ) > 0 ? true : false );

	if( frontPlane ) {
		if( pass->alphagen.type != ALPHA_GEN_IDENTITY ) {
			programFeatures |= GLSL_SHADER_DISTORTION_DISTORTION_ALPHA;
		}
	}

	Matrix4_Identity( texMatrix );

	RB_BindImage( 0, dudvmap );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( &pass->rgbgen, &pass->alphagen );

	programFeatures |= RB_FogProgramFeatures( pass, rb.fog );

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	if( normalmap != blankNormalmap ) {
		// eyeDot
		programFeatures |= GLSL_SHADER_DISTORTION_EYEDOT;

		RB_BindImage( 1, normalmap );
	}

	RB_BindImage( 2, portaltexture[0] );           // reflection
	RB_BindImage( 3, portaltexture[1] );           // refraction

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_DISTORTION, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateDistortionUniforms( program, frontPlane );

		RP_UpdateTextureUniforms( program, width, height );

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_Outline
*/
static void RB_RenderMeshGLSL_Outline( const shaderpass_t *pass, uint64_t programFeatures ) {
	int faceCull;
	int program;
	mat4_t texMatrix;

	if( rb.currentModelType == mod_brush ) {
		programFeatures |= GLSL_SHADER_OUTLINE_OUTLINES_CUTOFF;
	}

	programFeatures |= RB_RGBAlphaGenToProgramFeatures( &pass->rgbgen, &pass->alphagen );

	programFeatures |= RB_FogProgramFeatures( pass, rb.fog );

	// update uniforcms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_OUTLINE, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( !RB_BindProgram( program ) ) {
		return;
	}

	Matrix4_Identity( texMatrix );

	faceCull = rb.gl.faceCull;
	RB_Cull( GL_BACK );

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	RB_UpdateCommonUniforms( program, pass, texMatrix );

	RP_UpdateOutlineUniforms( program, rb.currentEntity->outlineHeight * r_outlines_scale->value );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		RB_UpdateFogUniforms( program, rb.fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( program, rb.bonesData.numBones, rb.bonesData.dualQuats );
	}

	RB_DrawElementsReal( rb.drawCallData );

	RB_Cull( faceCull );
}

/*
* RB_TcGenToProgramFeatures
*/
uint64_t RB_TcGenToProgramFeatures( int tcgen, vec_t *tcgenVec, mat4_t texMatrix, mat4_t genVectors ) {
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
			RB_VertexTCCelshadeMatrix( texMatrix );
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

static void RB_RenderMeshGLSL_Q3AShader( const FrontendToBackendShared *fsh, const shaderpass_t *pass, uint64_t programFeatures ) {
	int state;
	int program;
	int rgbgen = pass->rgbgen.type;
	const Texture *image;
	const mfog_t *fog = rb.fog;
	bool isWorldSurface = rb.currentModelType == mod_brush ? true : false;
	const superLightStyle_t *lightStyle = NULL;
	const entity_t *e = rb.currentEntity;
	bool isLightmapped = false, isWorldVertexLight = false;
	vec3_t lightDir;
	vec4_t lightAmbient, lightDiffuse;
	mat4_t texMatrix, genVectors;

	// lightmapped surface pass
	if( isWorldSurface &&
		rb.superLightStyle &&
		rb.superLightStyle->lightmapNum[0] >= 0 &&
		( rgbgen == RGB_GEN_IDENTITY
		  || rgbgen == RGB_GEN_CONST
		  || rgbgen == RGB_GEN_WAVE
		  || rgbgen == RGB_GEN_CUSTOMWAVE
		  || rgbgen == RGB_GEN_VERTEX
		  || rgbgen == RGB_GEN_ONE_MINUS_VERTEX
		  || rgbgen == RGB_GEN_EXACT_VERTEX ) &&
		( rb.currentShader->flags & SHADER_LIGHTMAP ) &&
		( pass->flags & GLSTATE_BLEND_ADD ) != GLSTATE_BLEND_ADD &&
		( pass->flags & ( GLSTATE_SRCBLEND_SRC_ALPHA ) ) != GLSTATE_SRCBLEND_SRC_ALPHA ) {
		lightStyle = rb.superLightStyle;
		isLightmapped = true;
	}

	// vertex-lit world surface
	if( isWorldSurface
		&& ( rgbgen == RGB_GEN_VERTEX || rgbgen == RGB_GEN_EXACT_VERTEX )
		&& ( rb.superLightStyle != NULL ) ) {
		isWorldVertexLight = true;
	} else {
		isWorldVertexLight = false;
	}

	// possibly apply the fog inline
	if( fog == rb.texFog ) {
		if( rb.currentShadowBits ) {
			fog = NULL;
		} else if( rb.currentShader->numpasses == 1 || ( isLightmapped && rb.currentShader->numpasses == 2 ) ) {
			rb.texFog = NULL;
		} else {
			fog = NULL;
		}
	}
	programFeatures |= RB_FogProgramFeatures( pass, fog );

	// diffuse lighting for entities
	if( !isWorldSurface && rgbgen == RGB_GEN_LIGHTING_DIFFUSE && !( e->flags & RF_FULLBRIGHT ) ) {
		vec3_t temp = { 0.1f, 0.2f, 0.7f };
		float radius = 1;

		if( e->number != kWorldEntNumber && e->model != NULL ) {
			radius = e->model->radius;
		}

		// get weighted incoming direction of world and dynamic lights
		R_LightForOrigin( e->lightingOrigin, temp, lightAmbient, lightDiffuse, radius * e->scale, rb.noWorldLight );

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

	image = RB_ShaderpassTex( pass );
	if( isLightmapped || isWorldVertexLight ) {
		// add dynamic lights
		if( rb.currentDlightBits ) {
			programFeatures |= RB_DlightbitsToProgramFeatures( rb.currentDlightBits );
		}
		if( DRAWFLAT() ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT;
		}
		if( rb.renderFlags & RF_LIGHTMAP ) {
			image = TextureCache::instance()->whiteTexture();
		}
	}

	if( image->flags & IT_ALPHAMASK ) {
		programFeatures |= GLSL_SHADER_Q3_ALPHA_MASK;
	}

	RB_BindImage( 0, image );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( &pass->rgbgen, &pass->alphagen );

	programFeatures |= RB_TcGenToProgramFeatures( pass->tcgen, pass->tcgenVec, texMatrix, genVectors );

	// set shaderpass state (blending, depthwrite, etc)
	state = pass->flags;

	// possibly force depthwrite and give up blending when doing a lightmapped pass
	if( ( isLightmapped || isWorldVertexLight ) &&
		!rb.doneDepthPass &&
		!( state & GLSTATE_DEPTHWRITE ) &&
		( rb.currentShader->flags & SHADER_DEPTHWRITE ) ) {
		if( !( pass->flags & SHADERPASS_ALPHAFUNC ) ) {
			state &= ~GLSTATE_BLEND_MASK;
		}
		state |= GLSTATE_DEPTHWRITE;
	}

	RB_SetShaderpassState( state );

	/*
	if( programFeatures & GLSL_SHADER_COMMON_SOFT_PARTICLE ) {
		RB_BindImage( 3, rb.st.screenDepthTexCopy );
	}*/

	if( isLightmapped ) {
		int i;

		// bind lightmap textures and set program's features for lightstyles
		for( i = 0; i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255; i++ )
			RB_BindImage( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] ); // lightmap
		programFeatures |= ( i * GLSL_SHADER_Q3_LIGHTSTYLE0 );
		if( mapConfig.lightmapArrays ) {
			programFeatures |= GLSL_SHADER_Q3_LIGHTMAP_ARRAYS;
		}
	}

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_Q3A_SHADER, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateTexGenUniforms( program, texMatrix, genVectors );

		if( isWorldSurface || rgbgen == RGB_GEN_LIGHTING_DIFFUSE ) {
			RP_UpdateDiffuseLightUniforms( program, lightDir, lightAmbient, lightDiffuse );
		}

		if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
			RB_UpdateFogUniforms( program, fog );
		}

		// submit animation data
		if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
			RP_UpdateBonesUniforms( program, rb.bonesData.numBones, rb.bonesData.dualQuats );
		}

		// dynamic lights
		if( isLightmapped || isWorldVertexLight ) {
			RP_UpdateDynamicLightsUniforms( fsh, program, lightStyle, e->origin, e->axis, rb.currentDlightBits );
		}

		// r_drawflat
		if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
			if( rb.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
				RP_UpdateDrawFlatUniforms( program, colorWhite, colorWhite );
			} else {
				RP_UpdateDrawFlatUniforms( program, rsh.wallColor, rsh.floorColor );
			}
		}

		/*
		if( programFeatures & GLSL_SHADER_COMMON_SOFT_PARTICLE ) {
			RP_UpdateTextureUniforms( program, rb.st.screenDepthTex->width, rb.st.screenDepthTex->height );
		}*/

		RB_DrawElementsReal( rb.drawCallData );
	}
}

static void RB_RenderMeshGLSL_Celshade( const shaderpass_t *pass, uint64_t programFeatures ) {
	const mfog_t *fog = rb.fog;

	Texture *const base = pass->images[0];
	Texture *const shade = pass->images[1];
	Texture *const diffuse = pass->images[2];
	Texture *const decal = pass->images[3];
	Texture *const entdecal = pass->images[4];
	Texture *const stripes = pass->images[5];
	Texture *const light = pass->images[6];

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	RB_BindImage( 0, base );

	mat4_t reflectionMatrix;
	RB_VertexTCCelshadeMatrix( reflectionMatrix );

	// possibly apply the "texture" fog inline
	if( fog == rb.texFog ) {
		if( ( rb.currentShader->numpasses == 1 ) && !rb.currentShadowBits ) {
			rb.texFog = NULL;
		} else {
			fog = NULL;
		}
	}
	programFeatures |= RB_FogProgramFeatures( pass, fog );

	// convert rgbgen and alphagen to GLSL feature defines
	programFeatures |= RB_RGBAlphaGenToProgramFeatures( &pass->rgbgen, &pass->alphagen );

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	auto *const textureCache = TextureCache::instance();
	Texture *const whiteTexture = textureCache->whiteTexture();
	assert( whiteTexture && whiteTexture->target == GL_TEXTURE_2D );
	Texture *const whiteCubemapTexture = textureCache->whiteCubemapTexture();
	assert( whiteCubemapTexture && whiteCubemapTexture->target == GL_TEXTURE_CUBE_MAP );

	// replacement images are there to ensure that the entity is still
	// properly colored despite real images still being loaded in a separate thread
#define CELSHADE_BIND( tmu,tex,feature,canAdd,replacement ) \
	if( tex ) { \
		Texture *btex = tex; \
		if( rb.renderFlags & RF_SHADOWMAPVIEW ) { \
			btex = tex->flags & IT_CUBEMAP ? whiteCubemapTexture : whiteTexture; \
		} else { \
			btex = tex ? tex : replacement; \
			if( btex ) { \
				programFeatures |= feature; \
				if( canAdd && ( btex->samples & 1 ) ) { \
					programFeatures |= ( ( feature ) << 1 );} \
			} \
		} \
		if( btex ) { \
			RB_BindImage( tmu, btex ); \
		} \
	}

	CELSHADE_BIND( 1, shade, 0, false, whiteCubemapTexture );
	CELSHADE_BIND( 2, diffuse, GLSL_SHADER_CELSHADE_DIFFUSE, false, NULL );
	CELSHADE_BIND( 3, decal, GLSL_SHADER_CELSHADE_DECAL, true, NULL );
	CELSHADE_BIND( 4, entdecal, GLSL_SHADER_CELSHADE_ENTITY_DECAL, true, whiteTexture );
	CELSHADE_BIND( 5, stripes, GLSL_SHADER_CELSHADE_STRIPES, true, NULL );
	CELSHADE_BIND( 6, light, GLSL_SHADER_CELSHADE_CEL_LIGHT, true, NULL );

#undef CELSHADE_BIND

	// update uniforms
	int program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_CELSHADE, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateTexGenUniforms( program, reflectionMatrix, texMatrix );

		if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
			RB_UpdateFogUniforms( program, fog );
		}

		// submit animation data
		if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
			RP_UpdateBonesUniforms( program, rb.bonesData.numBones, rb.bonesData.dualQuats );
		}

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_Fog
*/
static void RB_RenderMeshGLSL_Fog( const shaderpass_t *pass, uint64_t programFeatures ) {
	int program;
	const mfog_t *fog = rb.fog;
	mat4_t texMatrix = { 0 };

	programFeatures |= GLSL_SHADER_COMMON_FOG;

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_FOG, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RB_UpdateFogUniforms( program, fog );

		// submit animation data
		if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
			RP_UpdateBonesUniforms( program, rb.bonesData.numBones, rb.bonesData.dualQuats );
		}

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_FXAA
*/
static void RB_RenderMeshGLSL_FXAA( const shaderpass_t *pass, uint64_t programFeatures ) {
	bool fxaa3 = false;
	int program;
	const Texture *image = pass->images[0];
	mat4_t texMatrix;

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	Matrix4_Identity( texMatrix );

	RB_BindImage( 0, image );

	if( glConfig.ext.gpu_shader5 ) {
		fxaa3 = true;
	}
	if( fxaa3 ) {
		programFeatures |= GLSL_SHADER_FXAA_FXAA3;
	}

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_FXAA, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms,
								  rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateTextureUniforms( program, image->width, image->height );

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_YUV
*/
static void RB_RenderMeshGLSL_YUV( const shaderpass_t *pass, uint64_t programFeatures ) {
	int program;
	mat4_t texMatrix = { 0 };

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	RB_BindImage( 0, pass->images[0] );
	RB_BindImage( 1, pass->images[1] );
	RB_BindImage( 2, pass->images[2] );

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_YUV, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_ColorCorrection
*/
static void RB_RenderMeshGLSL_ColorCorrection( const shaderpass_t *pass, uint64_t programFeatures ) {
	int i;
	int program;
	mat4_t texMatrix;

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
		if( r_hdr->integer ) {
			programFeatures |= GLSL_SHADER_COLOR_CORRECTION_HDR;
		}
	}

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	Matrix4_Identity( texMatrix );

	RB_BindImage( 0, pass->images[0] );
	if( pass->images[1] ) {
		RB_BindImage( 1, pass->images[1] );
	}
	for( i = 0; i < NUM_BLOOM_LODS; i++ ) {
		if( pass->images[3 + i] ) {
			RB_BindImage( 2 + i, pass->images[3 + i] );
		}
	}

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_COLOR_CORRECTION, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateColorCorrectionUniforms( program, r_hdr_gamma->value, rb.hdrExposure );

		RB_DrawElementsReal( rb.drawCallData );
	}
}

/*
* RB_RenderMeshGLSL_KawaseBlur
*/
static void RB_RenderMeshGLSL_KawaseBlur( const shaderpass_t *pass, uint64_t programFeatures ) {
	int program;
	mat4_t texMatrix = { 0 };

	// set shaderpass state (blending, depthwrite, etc)
	RB_SetShaderpassState( pass->flags );

	RB_BindImage( 0, pass->images[0] );

	Matrix4_Identity( texMatrix );

	// update uniforms
	program = RB_RegisterProgram( GLSL_PROGRAM_TYPE_KAWASE_BLUR, NULL,
								  rb.currentShader->deformSig, rb.currentShader->deforms, rb.currentShader->numdeforms, programFeatures );
	if( RB_BindProgram( program ) ) {
		RB_UpdateCommonUniforms( program, pass, texMatrix );

		RP_UpdateKawaseUniforms( program, pass->images[0]->width, pass->images[0]->height, pass->anim_numframes );

		RB_DrawElementsReal( rb.drawCallData );
	}
}

void RB_RenderMeshGLSLProgrammed( const FrontendToBackendShared *fsh, const shaderpass_t *pass, int programType ) {
	uint64_t features = 0;

	if( rb.greyscale || pass->flags & SHADERPASS_GREYSCALE ) {
		features |= GLSL_SHADER_COMMON_GREYSCALE;
	}

	features |= RB_BonesTransformsToProgramFeatures();
	features |= RB_AutospriteProgramFeatures();
	features |= RB_InstancedArraysProgramFeatures();
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
			RB_RenderMeshGLSL_Material( fsh, pass, features );
			break;
		case GLSL_PROGRAM_TYPE_DISTORTION:
			RB_RenderMeshGLSL_Distortion( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_RGB_SHADOW:
			break;
		case GLSL_PROGRAM_TYPE_SHADOWMAP:
			break;
		case GLSL_PROGRAM_TYPE_OUTLINE:
			RB_RenderMeshGLSL_Outline( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_Q3A_SHADER:
			RB_RenderMeshGLSL_Q3AShader( fsh, pass, features );
			break;
		case GLSL_PROGRAM_TYPE_CELSHADE:
			RB_RenderMeshGLSL_Celshade( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_FOG:
			RB_RenderMeshGLSL_Fog( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_FXAA:
			RB_RenderMeshGLSL_FXAA( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_YUV:
			RB_RenderMeshGLSL_YUV( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_COLOR_CORRECTION:
			RB_RenderMeshGLSL_ColorCorrection( pass, features );
			break;
		case GLSL_PROGRAM_TYPE_KAWASE_BLUR:
			RB_RenderMeshGLSL_KawaseBlur( pass, features );
			break;
		default:
			Com_DPrintf( S_COLOR_YELLOW "WARNING: Unknown GLSL program type %i\n", programType );
			return;
	}
}

//==================================================================================

/*
* RB_UpdateVertexAttribs
*/
static void RB_UpdateVertexAttribs( void ) {
	vattribmask_t vattribs = rb.currentShader->vattribs;
	if( rb.superLightStyle ) {
		vattribs |= rb.superLightStyle->vattribs;
	}
	if( rb.bonesData.numBones ) {
		vattribs |= VATTRIB_BONES_BITS;
	}
	if( rb.currentEntity->outlineHeight ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( DRAWFLAT() ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( rb.currentShadowBits && ( rb.currentModelType == mod_brush ) ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	rb.currentVAttribs = vattribs;
}

/*
* RB_BindShader
*/
void RB_BindShader( const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
					const shader_t *shader, const mfog_t *fog ) {
	rb.currentShader = shader;
	rb.fog = fog;
	rb.texFog = rb.colorFog = NULL;

	rb.doneDepthPass = false;
	rb.dirtyUniformState = true;

	rb.currentEntity = e ? e : &rb.nullEnt;
	rb.currentModelType = rb.currentEntity->rtype == RT_MODEL && rb.currentEntity->model ? rb.currentEntity->model->type : mod_bad;
	rb.currentDlightBits = 0;
	rb.currentShadowBits = 0;
	rb.superLightStyle = NULL;

	rb.bonesData.numBones = 0;
	rb.bonesData.maxWeights = 0;

	rb.currentPortalSurface = NULL;

	if( !e ) {
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			rb.currentShaderTime = 1e-3 * (double)materialParams->shaderTime;
			rb.currentShaderFrac = materialParams->shaderFrac;
		} else {
			rb.currentShaderTime = 1e-3 * (double)rb.nullEnt.shaderTime;
			rb.currentShaderFrac = 0.0f;
		}
		rb.alphaHack = false;
		rb.greyscale = false;
		rb.noDepthTest = false;
		rb.noColorWrite =  false;
		rb.depthEqual = false;
	} else {
		Vector4Copy( rb.currentEntity->shaderRGBA, rb.entityColor );
		Vector4Copy( rb.currentEntity->outlineColor, rb.entityOutlineColor );
		int64_t givenShaderTime;
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			givenShaderTime      = materialParams->shaderTime;
			rb.currentShaderFrac = materialParams->shaderFrac;
		} else {
			givenShaderTime      = rb.currentEntity->shaderTime;
			rb.currentShaderFrac = 0.0f;
		}
		if( givenShaderTime > rb.time ) {
			rb.currentShaderTime = 0;
		} else {
			rb.currentShaderTime = 1e-3 * (double)( rb.time - givenShaderTime );
		}
		rb.alphaHack = e->renderfx & RF_ALPHAHACK ? true : false;
		rb.hackedAlpha = e->shaderRGBA[3] / 255.0;
		rb.greyscale = e->renderfx & RF_GREYSCALE ? true : false;
		rb.noDepthTest = e->renderfx & RF_NODEPTHTEST && e->rtype == RT_SPRITE ? true : false;
		rb.noColorWrite = e->renderfx & RF_NOCOLORWRITE ? true : false;
		rb.depthEqual = rb.alphaHack && ( e->renderfx & RF_WEAPONMODEL );
	}

	if( fog && fog->shader && !rb.noColorWrite ) {
		// should we fog the geometry with alpha texture or scale colors?
		if( !rb.alphaHack && Shader_UseTextureFog( shader ) ) {
			rb.texFog = fog;
		} else {
			// use scaling of colors
			rb.colorFog = fog;
		}
	}

	RB_UpdateVertexAttribs();
}

/*
* RB_SetLightstyle
*/
void RB_SetLightstyle( const superLightStyle_t *lightStyle ) {
	assert( rb.currentShader != NULL );
	rb.superLightStyle = lightStyle;
	rb.dirtyUniformState = true;

	RB_UpdateVertexAttribs();
}

/*
* RB_SetDlightBits
*/
void RB_SetDlightBits( unsigned int dlightBits ) {
	assert( rb.currentShader != NULL );
	rb.currentDlightBits = dlightBits;
	rb.dirtyUniformState = true;
}

/*
* RB_SetShadowBits
*/
void RB_SetShadowBits( unsigned int shadowBits ) {
	assert( rb.currentShader != NULL );
	rb.currentShadowBits = shadowBits;
	rb.dirtyUniformState = true;
}

/*
* RB_SetAnimData
*/
void RB_SetBonesData( int numBones, dualquat_t *dualQuats, int maxWeights ) {
	assert( rb.currentShader != NULL );

	if( numBones > MAX_GLSL_UNIFORM_BONES ) {
		numBones = MAX_GLSL_UNIFORM_BONES;
	}
	if( maxWeights > 4 ) {
		maxWeights = 4;
	}

	rb.bonesData.numBones = numBones;
	memcpy( rb.bonesData.dualQuats, dualQuats, numBones * sizeof( *dualQuats ) );
	rb.bonesData.maxWeights = maxWeights;

	rb.dirtyUniformState = true;

	RB_UpdateVertexAttribs();
}

/*
* RB_SetPortalSurface
*/
void RB_SetPortalSurface( const portalSurface_t *portalSurface ) {
	assert( rb.currentShader != NULL );
	rb.currentPortalSurface = portalSurface;
	rb.dirtyUniformState = true;
}

/*
* RB_SetInstanceData
*
* Internal backend function, only used by RB_DrawElementsReal to upload
* instance data
*/
void RB_SetInstanceData( int numInstances, instancePoint_t *instances ) {
	if( !rb.currentProgram ) {
		return;
	}
	RP_UpdateInstancesUniforms( rb.currentProgram, numInstances, instances );
}

/*
* RB_SetZClip
*/
void RB_SetZClip( float zNear, float zFar ) {
	rb.zNear = zNear;
	rb.zFar = zFar;
}

/*
* RB_SetLightParams
*/
void RB_SetLightParams( float minLight, bool noWorldLight, float hdrExposure ) {
	rb.minLight = minLight;
	rb.noWorldLight = noWorldLight;
	rb.hdrExposure = hdrExposure;
}

/*
* RB_RegisterProgram
*/
int RB_RegisterProgram( int type, const char *name, const DeformSig &deformSig,
						const deformv_t *deforms, int numDeforms, uint64_t features ) {
	int program;
	bool noDeforms = !numDeforms;

	if( rb.currentRegProgramType == type && noDeforms && rb.currentRegProgramFeatures == features ) {
		return rb.currentRegProgram;
	}

	program = RP_RegisterProgram( type, name, deformSig, deforms, numDeforms, features );
	if( noDeforms ) {
		rb.currentRegProgram = program;
		rb.currentRegProgramType = type;
		rb.currentRegProgramFeatures = features;
	}

	return program;
}

/*
* RB_BindProgram
*/
int RB_BindProgram( int program ) {
	int object;

	if( program == rb.currentProgram ) {
		return rb.currentProgramObject;
	}

	rb.currentProgram = program;
	if( !program ) {
		rb.currentProgramObject = 0;
		qglUseProgram( 0 );
		return 0;
	}

	object = RP_GetProgramObject( program );
	if( object ) {
		qglUseProgram( object );
	}
	rb.currentProgramObject = object;
	rb.dirtyUniformState = true;
	return object;
}

static void RB_RenderPass( const FrontendToBackendShared *fsh, const shaderpass_t *pass ) {
	// for depth texture we render light's view to, ignore passes that do not write into depth buffer
	if( ( rb.renderFlags & RF_SHADOWMAPVIEW ) && !( pass->flags & GLSTATE_DEPTHWRITE ) ) {
		return;
	}

	if( pass->program_type ) {
		RB_RenderMeshGLSLProgrammed( fsh, pass, pass->program_type );
	} else {
		RB_RenderMeshGLSLProgrammed( fsh, pass, GLSL_PROGRAM_TYPE_Q3A_SHADER );
	}

	if( rb.dirtyUniformState ) {
		rb.donePassesTotal = 0;
		rb.dirtyUniformState = false;
	}

	if( rb.gl.state & GLSTATE_DEPTHWRITE ) {
		rb.doneDepthPass = true;
	}

	rb.donePassesTotal++;
}

/*
* RB_SetShaderStateMask
*/
void RB_SetShaderStateMask( int ANDmask, int ORmask ) {
	rb.shaderStateANDmask = ANDmask;
	rb.shaderStateORmask = ORmask;
}

/*
* RB_SetShaderState
*/
static void RB_SetShaderState( void ) {
	int state;
	int shaderFlags = rb.currentShader->flags;

	// Face culling
	if( !gl_cull->integer || rb.currentEntity->rtype == RT_SPRITE ) {
		RB_Cull( 0 );
	} else if( shaderFlags & SHADER_CULL_FRONT ) {
		RB_Cull( GL_FRONT );
	} else if( shaderFlags & SHADER_CULL_BACK ) {
		RB_Cull( GL_BACK );
	} else {
		RB_Cull( 0 );
	}

	state = 0;

	if( shaderFlags & SHADER_POLYGONOFFSET ) {
		state |= GLSTATE_OFFSET_FILL;
	}
	if( shaderFlags & SHADER_STENCILTEST ) {
		state |= GLSTATE_STENCIL_TEST;
	}

	if( rb.noDepthTest ) {
		state |= GLSTATE_NO_DEPTH_TEST;
	}

	rb.currentShaderState = ( state & rb.shaderStateANDmask ) | rb.shaderStateORmask;
}

/*
* RB_SetShaderpassState
*/
static void RB_SetShaderpassState( int state ) {
	state |= rb.currentShaderState;
	if( rb.alphaHack ) {
		if( !( state & GLSTATE_BLEND_MASK ) ) {
			// force alpha blending
			state = ( state & ~GLSTATE_DEPTHWRITE ) | GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
		}
	}
	if( rb.noColorWrite ) {
		state |= GLSTATE_NO_COLORWRITE;
	}
	if( rb.depthEqual && ( state & GLSTATE_DEPTHWRITE ) ) {
		state |= GLSTATE_DEPTHFUNC_EQ;
	}
	RB_SetState( state );
}

/*
* RB_CleanSinglePass
*
* Attempts to reuse current GLSL state: since the dirty flag
* is not set and there have been no uniform updates, we can simply
* call glDrawElements with fresh vertex data
*/
static bool RB_CleanSinglePass( void ) {
	// reuse current GLSL state (same program bound, same uniform values)
	if( !rb.dirtyUniformState && rb.donePassesTotal == 1 ) {
		RB_DrawElementsReal( rb.drawCallData );
		return true;
	}
	return false;
}

/*
* RB_TriangleLinesColor
*/
static inline const vec_t *RB_TriangleLinesColor( void ) {
	if( r_showtris->integer != 2 ) {
		return colorWhite;
	}

	if( rb.currentModelType == mod_brush ) {
		return colorBlack;
	}
	if( rb.currentModelType != mod_bad ) {
		return colorRed;
	}
	if( rb.currentEntity && rb.currentEntity->number != kWorldEntNumber ) {
		return colorBlue;
	}
	return colorGreen;
}

void RB_DrawWireframeElements( const FrontendToBackendShared *fsh ) {
	static shaderpass_t r_triLinesPass;
	static vec4_t r_triLinesColor;
	shaderpass_t *pass;

	if( RB_CleanSinglePass() ) {
		return;
	}

	Vector4Copy( RB_TriangleLinesColor(), r_triLinesColor );

	if( !rb.currentShader->numpasses ) {
		// happens on fog volumes
		pass = &r_GLSLpasses[BUILTIN_GLSLPASS_FOG];
	} else {
		pass = &rb.currentShader->passes[0];
	}

	// set some flags
	rb.currentShadowBits = 0;
	rb.currentDlightBits = 0;
	rb.colorFog = rb.texFog = NULL;
	rb.superLightStyle = NULL;

	// copy and override
	r_triLinesPass = *pass;
	r_triLinesPass.rgbgen.type = RGB_GEN_CONST;
	VectorCopy( r_triLinesColor + 0, r_triLinesPass.rgbgen.args );
	r_triLinesPass.alphagen.type = ALPHA_GEN_CONST;
	VectorSet( r_triLinesPass.alphagen.args, r_triLinesColor[3], r_triLinesColor[3], r_triLinesColor[3] );
	r_triLinesPass.flags = 0;
	r_triLinesPass.images[0] = TextureCache::instance()->whiteTexture();
	r_triLinesPass.anim_fps = 0;
	r_triLinesPass.anim_numframes = 0;
	r_triLinesPass.program_type = GLSL_PROGRAM_TYPE_Q3A_SHADER;

	RB_SetShaderState();

	RB_RenderPass( fsh, &r_triLinesPass );
}

// TODO: Show outlines in mirrors
#define ENTITY_OUTLINE( ent ) ( ( ( ( ent )->renderfx & RF_VIEWERMODEL ) ) ? 0 : ( ent )->outlineHeight )

void RB_DrawShadedElements( const FrontendToBackendShared *fsh ) {
	unsigned i;
	bool addGLSLOutline = false;
	shaderpass_t *pass;

	if( RB_CleanSinglePass() ) {
		return;
	}

	if( ENTITY_OUTLINE( rb.currentEntity ) && !( rb.renderFlags & RF_CLIPPLANE )
		&& ( rb.currentShader->sort == SHADER_SORT_OPAQUE ) && ( rb.currentShader->flags & SHADER_CULL_FRONT )
		&& !( rb.renderFlags & RF_SHADOWMAPVIEW ) ) {
		addGLSLOutline = true;
	}

	RB_SetShaderState();

	for( i = 0, pass = rb.currentShader->passes; i < rb.currentShader->numpasses; i++, pass++ ) {
		if( ( pass->flags & SHADERPASS_DETAIL ) && !r_detailtextures->integer ) {
			continue;
		}
		if( pass->flags & SHADERPASS_LIGHTMAP ) {
			continue;
		}
		//Com_Printf( "Rendering shader %s\n", rb.currentShader->name.data() );
		RB_RenderPass( fsh, pass );
	}

	// shadow map
	if( rb.currentShadowBits && ( rb.currentShader->sort >= SHADER_SORT_OPAQUE )
		&& ( rb.currentShader->sort <= SHADER_SORT_ALPHATEST ) ) {
		RB_RenderPass( fsh, &r_GLSLpasses[BUILTIN_GLSLPASS_SHADOWMAP] );
	}

	// outlines
	if( addGLSLOutline ) {
		RB_RenderPass( fsh, &r_GLSLpasses[BUILTIN_GLSLPASS_OUTLINE] );
	}

	// fog
	if( rb.texFog && rb.texFog->shader ) {
		shaderpass_t *fogPass = &r_GLSLpasses[BUILTIN_GLSLPASS_FOG];

		fogPass->images[0] = TextureCache::instance()->whiteTexture();
		if( !rb.currentShader->numpasses || rb.currentShader->fog_dist ) {
			fogPass->flags &= ~GLSTATE_DEPTHFUNC_EQ;
		} else {
			fogPass->flags |= GLSTATE_DEPTHFUNC_EQ;
		}
		RB_RenderPass( fsh, fogPass );
	}
}
