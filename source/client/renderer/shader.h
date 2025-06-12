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
#ifndef R_SHADER_H
#define R_SHADER_H

#define MAX_SHADERS                 2048
#define MAX_SHADER_PASSES           8
#define MAX_SHADER_DEFORMVS         8
#define MAX_SHADER_IMAGES           16
#define MAX_SHADER_TCMODS           8

// shader types (by superlightstyle)
typedef enum {
	SHADER_TYPE_DELUXEMAP       = 0,
	SHADER_TYPE_VERTEX          = 1,
	SHADER_TYPE_BSP_MIN         = SHADER_TYPE_DELUXEMAP,
	SHADER_TYPE_BSP_MAX         = SHADER_TYPE_VERTEX,
	SHADER_TYPE_DIFFUSE         = 2,
	SHADER_TYPE_2D              = 3,
	SHADER_TYPE_2D_RAW          = 4,
	SHADER_TYPE_CORONA          = 5,
	SHADER_TYPE_OPAQUE_ENV      = 6,
	SHADER_TYPE_VIDEO           = 7,
	SHADER_TYPE_FOG             = 9,
	SHADER_TYPE_2D_LINEAR       = 10,
} shaderType_e;

#define NUM_SHADER_TYPES_BSP ( SHADER_TYPE_BSP_MAX - SHADER_TYPE_BSP_MIN + 1 )

// shader flags
enum {
	SHADER_DEPTHWRITE               = 1 << 0,
	SHADER_SKY                      = 1 << 1,
	SHADER_CULL_FRONT               = 1 << 2,
	SHADER_CULL_BACK                = 1 << 3,
	SHADER_POLYGONOFFSET            = 1 << 4,
	SHADER_ENTITY_MERGABLE          = 1 << 5,
	SHADER_AUTOSPRITE               = 1 << 6,
	SHADER_LIGHTMAP                 = 1 << 7,
	SHADER_PORTAL                   = 1 << 8,
	SHADER_PORTAL_CAPTURE           = 1 << 9,
	SHADER_PORTAL_CAPTURE2          = 1 << 10,
	SHADER_NO_TEX_FILTERING         = 1 << 11,
	SHADER_ALLDETAIL                = 1 << 12,
	SHADER_NODRAWFLAT               = 1 << 13,
	SHADER_SOFT_PARTICLE            = 1 << 14,
	SHADER_FORCE_OUTLINE_WORLD      = 1 << 15,
	SHADER_STENCILTEST              = 1 << 16,
	SHADER_ANIM_FRAC                = 1 << 17,
};

// sorting
enum {
	SHADER_SORT_NONE                = 0,
	SHADER_SORT_PORTAL              = 1,
	SHADER_SORT_SKY                 = 2,
	SHADER_SORT_OPAQUE              = 3,
	SHADER_SORT_DECAL               = 4,
	SHADER_SORT_ALPHATEST           = 5,
	SHADER_SORT_BANNER              = 6,
	SHADER_SORT_FOG                 = 7,
	SHADER_SORT_UNDERWATER          = 8,
	SHADER_SORT_ADDITIVE            = 9,
	SHADER_SORT_NEAREST             = 14,
	SHADER_SORT_WEAPON              = 15, // optional phase: depth write but no color write
	SHADER_SORT_WEAPON2             = 16,
};

// shaderpass flags
#define SHADERPASS_MARK_BEGIN       0x20000 // same as GLSTATE_MARK_END
enum {
	SHADERPASS_LIGHTMAP             = SHADERPASS_MARK_BEGIN,
	SHADERPASS_DETAIL               = SHADERPASS_MARK_BEGIN << 1,
	SHADERPASS_PORTALMAP            = SHADERPASS_MARK_BEGIN << 2,
	SHADERPASS_GREYSCALE            = SHADERPASS_MARK_BEGIN << 3,
	SHADERPASS_SKYBOXSIDE           = SHADERPASS_MARK_BEGIN << 4,

	SHADERPASS_AFUNC_GT0            = SHADERPASS_MARK_BEGIN << 5,
	SHADERPASS_AFUNC_LT128          = SHADERPASS_MARK_BEGIN << 6,
	SHADERPASS_AFUNC_GE128          = SHADERPASS_AFUNC_GT0 | SHADERPASS_AFUNC_LT128,
	SHADERPASS_NOSRGB               = SHADERPASS_MARK_BEGIN << 7,
};

#define SHADERPASS_ALPHAFUNC ( SHADERPASS_AFUNC_GT0 | SHADERPASS_AFUNC_LT128 | SHADERPASS_AFUNC_GE128 )

// transform functions
enum {
	SHADER_FUNC_NONE,
	SHADER_FUNC_SIN,
	SHADER_FUNC_TRIANGLE,
	SHADER_FUNC_SQUARE,
	SHADER_FUNC_SAWTOOTH,
	SHADER_FUNC_INVERSESAWTOOTH,
	SHADER_FUNC_NOISE,
	SHADER_FUNC_CONSTANT,
	SHADER_FUNC_RAMP,

	MAX_SHADER_FUNCS
};

// RGB colors generation
enum {
	RGB_GEN_UNKNOWN,
	RGB_GEN_IDENTITY,
	RGB_GEN_CONST,
	RGB_GEN_WAVE,
	RGB_GEN_ENTITYWAVE,
	RGB_GEN_ONE_MINUS_ENTITY,
	RGB_GEN_VERTEX,
	RGB_GEN_ONE_MINUS_VERTEX,
	RGB_GEN_LIGHTING_DIFFUSE,
	RGB_GEN_EXACT_VERTEX,
	RGB_GEN_FOG,
	RGB_GEN_CUSTOMWAVE,
	RGB_GEN_OUTLINE,
	RGB_GEN_ENVIRONMENT
};

// alpha channel generation
enum {
	ALPHA_GEN_UNKNOWN,
	ALPHA_GEN_IDENTITY,
	ALPHA_GEN_CONST,
	ALPHA_GEN_VERTEX,
	ALPHA_GEN_ONE_MINUS_VERTEX,
	ALPHA_GEN_ENTITY,
	ALPHA_GEN_WAVE,
	ALPHA_GEN_OUTLINE
};

// texture coordinates generation
enum {
	TC_GEN_NONE,
	TC_GEN_BASE,
	TC_GEN_LIGHTMAP,
	TC_GEN_ENVIRONMENT,
	TC_GEN_VECTOR,
	TC_GEN_REFLECTION,
	TC_GEN_FOG,
	TC_GEN_REFLECTION_CELSHADE,
	TC_GEN_SVECTORS,
	TC_GEN_PROJECTION,
	TC_GEN_SURROUND
};

// tcmod functions
enum {
	TC_MOD_NONE,
	TC_MOD_SCALE,
	TC_MOD_SCROLL,
	TC_MOD_ROTATE,
	TC_MOD_TRANSFORM,
	TC_MOD_TURB,
	TC_MOD_STRETCH
};

// vertices deformation
enum {
	DEFORMV_NONE,
	DEFORMV_WAVE,
	DEFORMV_BULGE,
	DEFORMV_MOVE,
	DEFORMV_AUTOSPRITE,
	DEFORMV_AUTOSPRITE2,
	DEFORMV_AUTOPARTICLE,
	DEFORMV_OUTLINE
};

typedef struct shaderfunc_s {
	unsigned int type;                  // SHADER_FUNC enum
	float args[4];                      // offset, amplitude, phase_offset, rate
} shaderfunc_t;

typedef struct {
	unsigned int type;
	float args[6];
} tcmod_t;

typedef struct {
	unsigned int type;
	float args[3];
	shaderfunc_t func;
} colorgen_t;

typedef struct {
	unsigned int type;
	float args[4];
	shaderfunc_t func;
} deformv_t;

class Texture;

// Per-pass rendering state information
typedef struct shaderpass_s {
	unsigned int flags;

	colorgen_t rgbgen;
	colorgen_t alphagen;

	unsigned int tcgen;
	float               *tcgenVec;

	unsigned int numtcmods;
	tcmod_t             *tcmods;

	unsigned int program_type;

	Texture *images[MAX_SHADER_IMAGES];
	float timelineFracs[MAX_SHADER_IMAGES];

	// If zero but anim frames are present, timeline-based anim should be used
	float anim_fps;
	unsigned int anim_numframes;

	[[nodiscard]]
	int getNumRgbGenElems() const {
		auto type = rgbgen.type;
		if( type == RGB_GEN_WAVE || type == RGB_GEN_CONST ) {
			return 4;
		}
		return type == RGB_GEN_CUSTOMWAVE ? 2 : 0;
	}

	[[nodiscard]]
	int getNumAlphaGenElems() const {
		return alphagen.type == ALPHA_GEN_CONST ? 2 : 0;
	}

	[[nodiscard]]
	int getNumTCGenElems() const {
		return tcgen == TC_GEN_VECTOR ? 8 : 0;
	}
} shaderpass_t;

struct DeformSig {
	const int *data { nullptr };
	uint32_t hash { 0 };
	uint32_t len { 0 };

	bool operator==( const DeformSig &that ) const {
		return hash == that.hash && len == that.len && !std::memcmp( data, that.data, len * sizeof( int ) );
	}
};

// Shader information
typedef struct alignas( 8 ) shader_s {
	enum { ListLinks, BinLinks };

	shader_s *prev[2] { nullptr, nullptr };
	shader_s *next[2] { nullptr, nullptr };

	wsw::HashedStringView name;

	unsigned id { 0 };
	unsigned binIndex { ~0U };
	int registrationSequence { 0 };
	shaderType_e type { (shaderType_e)0 };

	unsigned int flags { 0 };
	vattribmask_t vattribs { 0 };
	unsigned int sort { 0 };
	int imagetags { 0 };                                  // usage tags of the images - currently only depend
	                                                // on type, but if one shader can be requesed with
	                                                // different tags, functions like R_TouchShader
	                                                // should merge the existing and the requested tags
	unsigned numpasses { 0 };
	shaderpass_t *passes { nullptr };

	unsigned numdeforms { 0 };
	deformv_t *deforms { nullptr };

	DeformSig deformSig;

	uint8_t fog_color[4] { 0, 0, 0, 0 };
	float fog_dist { 0.0f };
	float fog_clearDist { 0.0f };

	uint8_t skyColor[4] { 0, 0, 0, 0 };

	float glossIntensity { 0.0f };
	float glossExponent { 0.0f };
	float offsetmappingScale { 0.0f };

	float portalDistance { 0.0f };
} shader_t;

#define     Shader_UseTextureFog( s ) ( ( ( s )->sort <= SHADER_SORT_FOG && \
										  ( ( s )->flags & SHADER_DEPTHWRITE ) ) || ( s )->fog_dist || ( s )->type == SHADER_TYPE_FOG )

#define     Shader_ReadDepth( s ) ( ( s )->flags & SHADER_SOFT_PARTICLE )

unsigned    R_PackShaderOrder( const shader_t *shader );

void        R_TouchShader( shader_t *s );

struct ShaderParamsTable;

// Currently just for overriding entity_t parameters, kept private for now.
// entity_t must be eventually fully replaced by this data type.
struct ShaderParams {
	// Parameters of a material script
	struct Material {
		int64_t shaderTime { 0 };
		float shaderFrac { 0.0f };
	};

	// TODO: Add other components
	int materialComponentIndex { -1 };

	[[nodiscard]]
	static auto getMaterialParams( const ShaderParams *params, const ShaderParamsTable *table ) -> const Material *;
};

struct ShaderParamsTable {
	ShaderParams::Material *material { nullptr };
};

inline auto ShaderParams::getMaterialParams( const ShaderParams *params, const ShaderParamsTable *table ) -> const Material * {
	if( params && params->materialComponentIndex >= 0 ) {
		return &table->material[params->materialComponentIndex];
	}
	return nullptr;
}

#endif // R_SHADER_H
