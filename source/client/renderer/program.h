/*
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
#ifndef R_PROGRAM_H
#define R_PROGRAM_H

#define GLSL_BIT( x )                         ( 1ULL << ( x ) )
#define GLSL_BITS_VERSION                   16

#define DEFAULT_GLSL_MATERIAL_PROGRAM           "defaultMaterial"
#define DEFAULT_GLSL_DISTORTION_PROGRAM         "defaultDistortion"
#define DEFAULT_GLSL_RGB_SHADOW_PROGRAM         "defaultRGBShadow"
#define DEFAULT_GLSL_SHADOWMAP_PROGRAM          "defaultShadowmap"
#define DEFAULT_GLSL_OUTLINE_PROGRAM            "defaultOutline"
#define DEFAULT_GLSL_DYNAMIC_LIGHTS_PROGRAM     "defaultDynamicLights"
#define DEFAULT_GLSL_Q3A_SHADER_PROGRAM         "defaultQ3AShader"
#define DEFAULT_GLSL_CELSHADE_PROGRAM           "defaultCelshade"
#define DEFAULT_GLSL_FOG_PROGRAM                "defaultFog"
#define DEFAULT_GLSL_FXAA_PROGRAM               "defaultFXAA"
#define DEFAULT_GLSL_YUV_PROGRAM                "defaultYUV"
#define DEFAULT_GLSL_COLORCORRECTION_PROGRAM    "defaultColorCorrection"
#define DEFAULT_GLSL_KAWASE_BLUR_PROGRAM        "defaultKawaseBlur"

// program types
enum {
	GLSL_PROGRAM_TYPE_NONE,
	GLSL_PROGRAM_TYPE_MATERIAL,
	GLSL_PROGRAM_TYPE_DISTORTION,
	GLSL_PROGRAM_TYPE_RGB_SHADOW,
	GLSL_PROGRAM_TYPE_SHADOWMAP,
	GLSL_PROGRAM_TYPE_OUTLINE,
	GLSL_PROGRAM_TYPE_UNUSED,
	GLSL_PROGRAM_TYPE_Q3A_SHADER,
	GLSL_PROGRAM_TYPE_CELSHADE,
	GLSL_PROGRAM_TYPE_FOG,
	GLSL_PROGRAM_TYPE_FXAA,
	GLSL_PROGRAM_TYPE_YUV,
	GLSL_PROGRAM_TYPE_COLOR_CORRECTION,
	GLSL_PROGRAM_TYPE_KAWASE_BLUR,

	GLSL_PROGRAM_TYPE_MAXTYPE
};

// features common for all program types
#define GLSL_SHADER_COMMON_GREYSCALE            GLSL_BIT( 0 )
#define GLSL_SHADER_COMMON_FOG                  GLSL_BIT( 1 )
#define GLSL_SHADER_COMMON_FOG_RGB              GLSL_BIT( 2 )

#define GLSL_SHADER_COMMON_RGB_GEN_CONST        GLSL_BIT( 4 )
#define GLSL_SHADER_COMMON_RGB_GEN_VERTEX       GLSL_BIT( 5 )
#define GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX ( GLSL_SHADER_COMMON_RGB_GEN_CONST | GLSL_SHADER_COMMON_RGB_GEN_VERTEX )
#define GLSL_SHADER_COMMON_RGB_DISTANCERAMP      GLSL_BIT( 6 )

#define GLSL_SHADER_COMMON_SRGB2LINEAR          GLSL_BIT( 7 )

#define GLSL_SHADER_COMMON_ALPHA_GEN_CONST      GLSL_BIT( 8 )
#define GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX     GLSL_BIT( 9 )
#define GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX ( GLSL_SHADER_COMMON_ALPHA_GEN_CONST | \
														GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX )
#define GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP    GLSL_BIT( 10 )

#define GLSL_SHADER_COMMON_BONE_TRANSFORMS1     GLSL_BIT( 11 )
#define GLSL_SHADER_COMMON_BONE_TRANSFORMS2     GLSL_BIT( 12 )
#define GLSL_SHADER_COMMON_BONE_TRANSFORMS3     ( GLSL_SHADER_COMMON_BONE_TRANSFORMS1 | GLSL_SHADER_COMMON_BONE_TRANSFORMS2 )
#define GLSL_SHADER_COMMON_BONE_TRANSFORMS4     GLSL_BIT( 13 )
#define GLSL_SHADER_COMMON_BONE_TRANSFORMS      ( GLSL_SHADER_COMMON_BONE_TRANSFORMS1 | GLSL_SHADER_COMMON_BONE_TRANSFORMS2 \
												  | GLSL_SHADER_COMMON_BONE_TRANSFORMS3 | GLSL_SHADER_COMMON_BONE_TRANSFORMS4 )

#define GLSL_SHADER_COMMON_DLIGHTS_4            GLSL_BIT( 14 ) // 4
#define GLSL_SHADER_COMMON_DLIGHTS_8            GLSL_BIT( 15 ) // 8
#define GLSL_SHADER_COMMON_DLIGHTS_12           ( GLSL_SHADER_COMMON_DLIGHTS_4 | GLSL_SHADER_COMMON_DLIGHTS_8 ) // 12
#define GLSL_SHADER_COMMON_DLIGHTS_16           GLSL_BIT( 16 ) // 16
#define GLSL_SHADER_COMMON_DLIGHTS              ( GLSL_SHADER_COMMON_DLIGHTS_4 | GLSL_SHADER_COMMON_DLIGHTS_8 \
												  | GLSL_SHADER_COMMON_DLIGHTS_12 | GLSL_SHADER_COMMON_DLIGHTS_16 )

#define GLSL_SHADER_COMMON_DRAWFLAT             GLSL_BIT( 17 )

#define GLSL_SHADER_COMMON_AUTOSPRITE           GLSL_BIT( 18 )
#define GLSL_SHADER_COMMON_AUTOSPRITE2          GLSL_BIT( 19 )
#define GLSL_SHADER_COMMON_AUTOPARTICLE         GLSL_BIT( 20 )

#define GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS GLSL_BIT( 21 )
#define GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS  GLSL_BIT( 22 )

#define GLSL_SHADER_COMMON_SOFT_PARTICLE        GLSL_BIT( 23 )

#define GLSL_SHADER_COMMON_AFUNC_GT0            GLSL_BIT( 24 )
#define GLSL_SHADER_COMMON_AFUNC_LT128          GLSL_BIT( 25 )
#define GLSL_SHADER_COMMON_AFUNC_GE128          ( GLSL_SHADER_COMMON_AFUNC_GT0 | GLSL_SHADER_COMMON_AFUNC_LT128 )

#define GLSL_SHADER_COMMON_TC_MOD               GLSL_BIT( 27 )

#define GLSL_SHADER_COMMON_LINEAR2SRB           GLSL_BIT( 28 )

// material program type features
#define GLSL_SHADER_MATERIAL_LIGHTSTYLE0        GLSL_BIT( 32 )
#define GLSL_SHADER_MATERIAL_LIGHTSTYLE1        GLSL_BIT( 33 )
#define GLSL_SHADER_MATERIAL_LIGHTSTYLE2        ( GLSL_SHADER_MATERIAL_LIGHTSTYLE0 | GLSL_SHADER_MATERIAL_LIGHTSTYLE1 )
#define GLSL_SHADER_MATERIAL_LIGHTSTYLE3        GLSL_BIT( 34 )
#define GLSL_SHADER_MATERIAL_LIGHTSTYLE             ( ( GLSL_SHADER_MATERIAL_LIGHTSTYLE0 | GLSL_SHADER_MATERIAL_LIGHTSTYLE1 \
														| GLSL_SHADER_MATERIAL_LIGHTSTYLE2 | GLSL_SHADER_MATERIAL_LIGHTSTYLE3 ) )
#define GLSL_SHADER_MATERIAL_SPECULAR           GLSL_BIT( 35 )
#define GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT  GLSL_BIT( 36 )
#define GLSL_SHADER_MATERIAL_FB_LIGHTMAP        GLSL_BIT( 37 )
#define GLSL_SHADER_MATERIAL_OFFSETMAPPING      GLSL_BIT( 38 )
#define GLSL_SHADER_MATERIAL_RELIEFMAPPING      GLSL_BIT( 39 )
#define GLSL_SHADER_MATERIAL_AMBIENT_COMPENSATION GLSL_BIT( 40 )
#define GLSL_SHADER_MATERIAL_DECAL              GLSL_BIT( 41 )
#define GLSL_SHADER_MATERIAL_DECAL_ADD          GLSL_BIT( 42 )
#define GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY GLSL_BIT( 43 )
#define GLSL_SHADER_MATERIAL_CELSHADING         GLSL_BIT( 44 )
#define GLSL_SHADER_MATERIAL_HALFLAMBERT        GLSL_BIT( 45 )
#define GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_MIX GLSL_BIT( 46 )
#define GLSL_SHADER_MATERIAL_ENTITY_DECAL       GLSL_BIT( 47 )
#define GLSL_SHADER_MATERIAL_ENTITY_DECAL_ADD   GLSL_BIT( 48 )
#define GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_FROM_NORMAL  GLSL_BIT( 49 )
#define GLSL_SHADER_MATERIAL_LIGHTMAP_ARRAYS    GLSL_BIT( 50 )

// q3a shader features
#define GLSL_SHADER_Q3_TC_GEN_ENV               GLSL_BIT( 32 )
#define GLSL_SHADER_Q3_TC_GEN_VECTOR            GLSL_BIT( 33 )
#define GLSL_SHADER_Q3_TC_GEN_REFLECTION        ( GLSL_SHADER_Q3_TC_GEN_ENV | GLSL_SHADER_Q3_TC_GEN_VECTOR )
#define GLSL_SHADER_Q3_TC_GEN_CELSHADE          GLSL_BIT( 34 )
#define GLSL_SHADER_Q3_TC_GEN_PROJECTION        GLSL_BIT( 35 )
#define GLSL_SHADER_Q3_TC_GEN_SURROUND          GLSL_BIT( 36 )
#define GLSL_SHADER_Q3_COLOR_FOG                GLSL_BIT( 37 )
#define GLSL_SHADER_Q3_LIGHTSTYLE0              GLSL_BIT( 38 )
#define GLSL_SHADER_Q3_LIGHTSTYLE1              GLSL_BIT( 39 )
#define GLSL_SHADER_Q3_LIGHTSTYLE2              ( GLSL_SHADER_Q3_LIGHTSTYLE0 | GLSL_SHADER_Q3_LIGHTSTYLE1 )
#define GLSL_SHADER_Q3_LIGHTSTYLE3              GLSL_BIT( 40 )
#define GLSL_SHADER_Q3_LIGHTSTYLE               ( ( GLSL_SHADER_Q3_LIGHTSTYLE0 | GLSL_SHADER_Q3_LIGHTSTYLE1 \
													| GLSL_SHADER_Q3_LIGHTSTYLE2 | GLSL_SHADER_Q3_LIGHTSTYLE3 ) )
#define GLSL_SHADER_Q3_LIGHTMAP_ARRAYS          GLSL_BIT( 41 )
#define GLSL_SHADER_Q3_ALPHA_MASK               GLSL_BIT( 42 )

// distortions
#define GLSL_SHADER_DISTORTION_DUDV             GLSL_BIT( 32 )
#define GLSL_SHADER_DISTORTION_EYEDOT           GLSL_BIT( 33 )
#define GLSL_SHADER_DISTORTION_DISTORTION_ALPHA GLSL_BIT( 34 )
#define GLSL_SHADER_DISTORTION_REFLECTION       GLSL_BIT( 35 )
#define GLSL_SHADER_DISTORTION_REFRACTION       GLSL_BIT( 36 )

// rgb shadows
#define GLSL_SHADER_RGBSHADOW_24BIT             GLSL_BIT( 32 )

// shadowmaps
#define GLSL_SHADOWMAP_LIMIT                    4 // shadowmaps per program limit
// outlines
#define GLSL_SHADER_OUTLINE_OUTLINES_CUTOFF     GLSL_BIT( 32 )

// cellshading program features
#define GLSL_SHADER_CELSHADE_DIFFUSE            GLSL_BIT( 32 )
#define GLSL_SHADER_CELSHADE_DECAL              GLSL_BIT( 33 )
#define GLSL_SHADER_CELSHADE_DECAL_ADD          GLSL_BIT( 34 )
#define GLSL_SHADER_CELSHADE_ENTITY_DECAL       GLSL_BIT( 35 )
#define GLSL_SHADER_CELSHADE_ENTITY_DECAL_ADD   GLSL_BIT( 36 )
#define GLSL_SHADER_CELSHADE_STRIPES            GLSL_BIT( 37 )
#define GLSL_SHADER_CELSHADE_STRIPES_ADD        GLSL_BIT( 38 )
#define GLSL_SHADER_CELSHADE_CEL_LIGHT          GLSL_BIT( 39 )
#define GLSL_SHADER_CELSHADE_CEL_LIGHT_ADD      GLSL_BIT( 40 )

// fxaa
#define GLSL_SHADER_FXAA_FXAA3                  GLSL_BIT( 32 )

// hdr/bloom/tone-mapping/color-correction
#define GLSL_SHADER_COLOR_CORRECTION_LUT        GLSL_BIT( 32 )
#define GLSL_SHADER_COLOR_CORRECTION_HDR        GLSL_BIT( 33 )
#define GLSL_SHADER_COLOR_CORRECTION_OVERBRIGHT GLSL_BIT( 34 )
#define GLSL_SHADER_COLOR_CORRECTION_BLOOM      GLSL_BIT( 35 )

typedef struct {
	uint64_t featureBit;
	const char      *define;
	const char      *suffix;
} glsl_feature_t;

struct ShaderProgram {
	ShaderProgram *nextInHashBin { nullptr };
	ShaderProgram *nextInList { nullptr };
	uint64_t features { 0 };
	wsw::StringView name;

	int type { 0 };
	unsigned index { 0 };

	DeformSig deformSig;

	GLuint programId { 0 };
	GLuint vertexShaderId { 0 };
	GLuint fragmentShaderId { 0 };

	// Always set, may belong to the names allocator.
	void *nameDataToFree { nullptr };
	// Optional, points to the default heap if set.
	void *deformSigDataToFree { nullptr };

	struct loc_s {
		int ModelViewMatrix,
			ModelViewProjectionMatrix,

			ZRange,

			ViewOrigin,
			ViewAxis,

			MirrorSide,

			Viewport,

			LightDir,
			LightAmbient,
			LightDiffuse,
			LightingIntensity,

			TextureMatrix,

			GlossFactors,

			OffsetMappingScale,
			OutlineHeight,
			OutlineCutOff,

			FrontPlane,
			TextureParams,

			EntityDist,
			EntityOrigin,
			EntityColor,
			ConstColor,
			RGBGenFuncArgs,
			AlphaGenFuncArgs;

		struct {
			int Plane,
				Color,
				ScaleAndEyeDist,
				EyePlane;
		} Fog;

		int ShaderTime,

			ReflectionTexMatrix,
			VectorTexMatrix,

			DeluxemapOffset,
			LightstyleColor[MAX_LIGHTMAPS],

			DynamicLightsPosition[MAX_DLIGHTS],
			DynamicLightsDiffuseAndInvRadius[MAX_DLIGHTS >> 2],
			NumDynamicLights,

			DualQuats,

			InstancePoints,

			WallColor,
			FloorColor,

			BlendMix,
			ColorMod,

			SoftParticlesScale;

		int hdrGamma,
			hdrExposure;

		// builtin uniforms
		struct {
			int ShaderTime,
				ViewOrigin,
				ViewAxis,
				MirrorSide,
				EntityOrigin;
		} builtin;
	} loc;
};

void RP_Init();
void RP_Shutdown();

int RP_RegisterProgram( int type, const char *name, const DeformSig &deformSig,
						const deformv_t *deforms, int numDeforms, uint64_t features );

#endif // R_PROGRAM_H
