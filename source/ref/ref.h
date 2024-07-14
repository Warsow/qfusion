/*
Copyright (C) 1997-2001 Id Software, Inc.

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

#ifndef WSW_ac367414_9635_4452_b2fc_9477f5254db5_H
#define WSW_ac367414_9635_4452_b2fc_9477f5254db5_H

#include "../common/wswstaticvector.h"
#include "../common/q_math.h"
#include "../client/animatedvalues.h"
#include "../common/freelistallocator.h"

#include <optional>
#include <span>
#include <variant>

// FIXME: move these to r_local.h?
#define MAX_DLIGHTS             32
#define MAX_ENTITIES            2048
#define MAX_POLY_VERTS          3000
#define MAX_QUAD_POLYS          256
#define MAX_COMPLEX_POLYS       64

// entity_state_t->renderfx flags
#define RF_MINLIGHT             0x1       // always have some light (viewmodel)
#define RF_FULLBRIGHT           0x2       // always draw full intensity
#define RF_FRAMELERP            0x4
#define RF_NOSHADOW             0x8

#define RF_VIEWERMODEL          0x10     // don't draw through eyes, only mirrors
#define RF_WEAPONMODEL          0x20     // only draw through eyes and depth hack
#define RF_CULLHACK             0x40
#define RF_FORCENOLOD           0x80
#define RF_NOPORTALENTS         0x100
#define RF_ALPHAHACK            0x200   // force alpha blending on opaque passes, read alpha from entity
#define RF_GREYSCALE            0x400
#define RF_NODEPTHTEST          0x800
#define RF_NOCOLORWRITE         0x1000

// refdef flags
#define RDF_UNDERWATER              0x1     // warp the screen as apropriate
#define RDF_NOWORLDMODEL            0x2     // used for player configuration screen
#define RDF_OLDAREABITS             0x4     // forces R_MarkLeaves if not set
#define RDF_PORTALINVIEW            0x8     // cull entities using vis too because pvs\areabits are merged serverside
#define RDF_SKYPORTALINVIEW         0x10    // draw skyportal instead of regular sky
#define RDF_WORLDOUTLINES           0x40    // draw cell outlines for world surfaces
#define RDF_CROSSINGWATER           0x80    // potentially crossing water surface
#define RDF_USEORTHO                0x100   // use orthographic projection
#define RDF_NOBSPOCCLUSIONCULLING   0x200
#define RDF_DRAWBRIGHT              0x400

// skm flags
#define SKM_ATTACHMENT_BONE     1

typedef struct orientation_s {
	mat3_t axis;
	vec3_t origin;
} orientation_t;

typedef struct bonepose_s {
	dualquat_t dualquat;
} bonepose_t;

typedef struct fragment_s {
	int firstvert;
	int numverts;                       // can't exceed MAX_POLY_VERTS
	int fognum;                         // -1 - no fog
	                                    //  0 - determine fog in R_AddPolyToScene
	                                    // >0 - valid fog volume number returned by R_GetClippedFragments
	vec3_t normal;
} fragment_t;

typedef struct {
	float rgb[3];                       // 0.0 - 2.0
} lightstyle_t;

typedef struct {
	float fov;
	float scale;
	vec3_t vieworg;
	vec3_t viewanglesOffset;
	bool noEnts;
} skyportal_t;

typedef enum {
	RT_MODEL,
	RT_SPRITE,
	RT_PORTALSURFACE,

	NUM_RTYPES
} refEntityType_t;

struct TransformParamsProvider {
	vec3_t origin {};
	mat3_t axis {};
	// TODO: Does lighting origin belong here?
	vec3_t lightingOrigin {};
	float scale { 1.0f };
	float radius { 1.0f };
	float rotation { 0.0f };
};

struct OutlineParamsProvider {
	uint8_t outlineColor[4];
	float outlineHeight;
};

struct AnimParamsProvider {
	int oldframe;
	float backlerp;
};

struct SkeletalParamsProvider {
	bonepose_t *boneposes { nullptr };
	bonepose_t *oldboneposes { nullptr };
};

struct MaterialParamsProvider {
	int64_t shaderTime;
	uint8_t shaderRGBA[4];
	struct Skin *customSkin { nullptr };
	struct shader_s *customShader { nullptr };
};

struct CommonParamsProvider {
	refEntityType_t rtype;
	unsigned number;
	int flags;
	int renderfx;
};

struct ShaderParamsProvider {
public:
	enum ComponentBits : unsigned {
		Common    = 1 << 0,
		Transform = 1 << 1,
		Material  = 1 << 2,
		Outline   = 1 << 3,
		Anim      = 1 << 4,
		Skeletal  = 1 << 5,
	};

	CommonParamsProvider *commonParams { nullptr };
	TransformParamsProvider *transformParams { nullptr };
	MaterialParamsProvider *materialParams { nullptr };
	OutlineParamsProvider *outlineParams { nullptr };
	AnimParamsProvider *animParams { nullptr };
	// TODO: Should it just extend anim params provider
	SkeletalParamsProvider *skeletalParams { nullptr };

	// TODO: Wrap in params/flatten with params
	// TODO: Do we really need bits for models/geometry?
	struct model_s *model { nullptr };

	unsigned presentComponentMask { 0 };
};

// These names should be final, not touching the existing partial rewrite of the codebase
using ShaderParams = ShaderParamsProvider;
using ShaderCommonParams = CommonParamsProvider;
using ShaderTransformParams = TransformParamsProvider;
using ShaderMaterialParams = MaterialParamsProvider;
using ShaderOutlineParams = OutlineParamsProvider;
using ShaderAnimParams = AnimParamsProvider;
using ShaderSkeletalParams = SkeletalParamsProvider;

typedef struct entity_s {
	refEntityType_t rtype;
	unsigned number;

	union {
		int flags;
		int renderfx;
	};

	struct model_s *model;              // opaque type outside refresh

	/*
	** most recent data
	*/
	mat3_t axis;
	vec3_t origin, origin2;
	vec3_t lightingOrigin;
	int frame;
	bonepose_t *boneposes;              // pretransformed boneposes for current frame

	/*
	** previous data for lerping
	*/
	int oldframe;
	bonepose_t *oldboneposes;           // pretransformed boneposes for old frame
	float backlerp;                     // 0.0 = current, 1.0 = old

	/*
	** texturing
	*/
	struct Skin *customSkin;      // registered .skin file
	struct shader_s *customShader;      // NULL for inline skin

	/*
	** misc
	*/
	int64_t shaderTime;
	union {
		byte_vec4_t color;
		uint8_t shaderRGBA[4];
	};

	float scale;
	float radius;                       // used as RT_SPRITE's radius
	float rotation;

	float outlineHeight;
	union {
		byte_vec4_t outlineColor;
		uint8_t outlineRGBA[4];
	};
} entity_t;

class RenderTargetComponents;

typedef struct refdef_s {
	RenderTargetComponents *renderTarget;
	int x, y, width, height;            // viewport, in virtual screen coordinates
	int scissor_x, scissor_y, scissor_width, scissor_height;
	int ortho_x, ortho_y;
	float fov_x, fov_y;
	vec3_t vieworg;
	mat3_t viewaxis;
	float blend[4];                     // rgba 0-1 full screen blend
	int64_t time;                       // time is used for timing offsets
	int rdflags;                        // RDF_UNDERWATER, etc
	skyportal_t skyportal;
	uint8_t *areabits;                  // if not NULL, only areas with set bits will be drawn
	float weaponAlpha;
	float minLight;                     // minimum value of ambient lighting applied to RF_MINLIGHT entities
	struct shader_s *colorCorrection;   // post processing color correction lookup table to apply
} refdef_t;

struct alignas( 16 ) Particle {
	enum SizeBehaviour : uint8_t {
		SizeNotChanging,
		Expanding,
		Shrinking,
		ExpandingAndShrinking,
		Thickening,
		Thinning,
		ThickeningAndThinning,
	};

	// Few notes on these value ranges:
	// They are ad-hoc structs for now, as we don't need more complicated stuff at this stage.
	// Distributions are assumed to be uniform in range (mean - spread, mean + spread) for historical reasons.
	// Negative values are clamped to zero.

	struct SpriteRules {
		// TODO: Why do we have to specify the {} initializer explicitly?
		struct { float mean { 1.0f }, spread { 0.0f }; } radius {};
		SizeBehaviour sizeBehaviour { SizeNotChanging };
	};

	struct SparkRules {
		struct { float mean { 1.0f }, spread { 0.0f }; } length {};
		struct { float mean { 1.0f }, spread { 0.0f }; } width {};
		float viewDirPartScale { 0.67f };
		SizeBehaviour sizeBehaviour { SizeNotChanging };
	};

	struct FlareProps {
		std::span<const LightLifespan> lightProps;
		float alphaScale { 1.0f };
		float radiusScale { 1.0f };
		// Flares are very cheap to process, even if the current implementation is not optimal.
		// These options are primarily for purposes of fine-tuning appearance.
		uint16_t flockFrameAffinityIndex { 0 };
		uint16_t flockFrameAffinityModulo { 0 };
		// For skipping individual particles
		uint16_t particleFrameAffinityModulo { 0 };
	};

	// Common for flocks/aggregates.
	// The name "rules" seems to be more appropriate than "params" for these stateless/shared objects.
	struct AppearanceRules {
		// Points to an external buffer with a greater lifetime.
		shader_s **materials;

		// Points to external buffers with a greater lifetime
		std::span<const RgbaLifespan> colors;

		// Unfortunately, std::span can't be used for materials due to value type restrictions.
		// TODO: Use our custom span type
		uint8_t numMaterials { 1 };

		// Program light properties.
		// Note that only a single particle (which is cycled) in a flock may be a light emitter during scene submission.
		// This span points to external buffers with a greater lifetime.
		// Empty if no light.
		// 1 element if the light props are the same for the entire flock.
		// Matches length of colors if it should be addressed by Particle::instanceColorIndex.
		// Other length options are invalid.
		std::span<const LightLifespan> lightProps;

		// These flares are similar to corona lights but are submitted as particle aggregates and not scene lights
		// (even corona lights contribute something else to the scene, e.g. they could affect vertex-lit surfaces).
		// Span values have the same meaning as lightProps ones.
		// In the current codebase state, we should not try drawing flares
		// during submission of spark particles due to material mismatch which leads to switching textures
		// (this could be solved by using a texture atlas, which is not available yet (?)).

		std::optional<FlareProps> flareProps;

		std::variant<SpriteRules, SparkRules> geometryRules;

		uint16_t lightFrameAffinityIndex { 0 };
		uint16_t lightFrameAffinityModulo { 0 };

		bool applyVertexDynLight { false };

	};

	float origin[4];
	float oldOrigin[4];
	// Kept/accumulated between frames
	float dynamicsVelocity[4];
	// Calculated in immediate mode for a given origin
    float artificialVelocity[4];
	float accel[4];
	float rotationAngle;
	float angularVelocity;

	int64_t spawnTime;
	// Gets updated every simulation frame prior to submission for rendering
	float lifetimeFrac;

	uint16_t lifetime;
	uint16_t activationDelay;

	uint8_t bounceCount;
	// Is not guaranteed to be correct/meaningful in all cases, check the actual usage
	uint8_t originalIndex;

	// TODO: All of this ties Particle to fixed AppearanceRules, allow supplying different instance data

	static constexpr float kByteSpreadNormalizer = 1.0f / 128.0f;

	// Should be set once upon spawn. Fractions are stored in a compact representation,
	// floating-point values should be reconstructed by multiplying by kByteParamNormalizer
	// The real parameter value is a multiple of this fraction by AppearanceRules:: parameter spread
	int8_t instanceLengthSpreadFraction;
	int8_t instanceWidthSpreadFraction;
	int8_t instanceRadiusSpreadFraction;

	static constexpr uint8_t kUnitExtraScaleAsByte = 16;
	static constexpr float kScaleOfByteExtraScale  = 1.0f / kUnitExtraScaleAsByte;

	uint8_t instanceLengthExtraScale;
	uint8_t instanceWidthExtraScale;
	uint8_t instanceRadiusExtraScale;

	// Keeps an index of an instance material in the AppearanceRules span
	uint8_t instanceMaterialIndex;
	// Keeps an index of instance color parameters in AppearanceRules color-related spans
	uint8_t instanceColorIndex;

	uint8_t rotationAxisIndex;
};

[[nodiscard]]
wsw_forceinline float calcSizeFracForLifetimeFrac( float lifetimeFrac, Particle::SizeBehaviour sizeBehaviour )  {
	assert( lifetimeFrac >= 0.0f && lifetimeFrac <= 1.0f );
	// Disallowed intentionally to avoid extra branching while testing the final particle dimensions for feasibility
	assert( sizeBehaviour != Particle::SizeNotChanging );

	float result;
	if( sizeBehaviour == Particle::Expanding || sizeBehaviour == Particle::Thickening ) {
		// Grow faster than the linear growth
		result = Q_Sqrt( lifetimeFrac );
	} else if( sizeBehaviour == Particle::Shrinking || sizeBehaviour == Particle::Thinning ) {
		// Shrink faster than the linear growth
		result = ( 1.0f - lifetimeFrac );
		result *= result;
	} else {
		assert( sizeBehaviour == Particle::ExpandingAndShrinking || sizeBehaviour == Particle::ThickeningAndThinning );
		if( lifetimeFrac < 0.5f ) {
			result = 2.0f * lifetimeFrac;
		} else {
			result = 2.0f * ( 1.0f - lifetimeFrac );
		}
	}
	assert( result >= 0.0f && result <= 1.0f );
	return result;
};

struct VisualTrace {
	shader_s *shader;
	const char *name;
	float endpos[3];
	cplane_t plane;
	int surfFlags;
	float fraction;
};

namespace wsw::ref {

void traceAgainstBspWorld( VisualTrace *tr, const float *start, const float *end, int skipSurfMask = 0 );
void traceAgainstBrushModel( VisualTrace *tr, const model_s *model, const float *origin,
							 const float *axis, const float *start, const float *end, int skipSurfMask = 0 );

}

namespace wsw::ref { class Frontend; }

struct QuadPoly;
struct DynamicMesh;

class Scene {
	friend class wsw::ref::Frontend;
public:
	struct DynamicLight {
		float origin[3];
		float programRadius;
		float coronaRadius;
		float maxRadius;
		float color[3];
		float mins[4];
		float maxs[4];
		bool hasProgramLight;
		bool hasCoronaLight;
	};

	// A flock of particles or just a bunch of particles with enclosing bounds
	struct ParticlesAggregate {
		const Particle *particles;
		Particle::AppearanceRules appearanceRules;
		float mins[4], maxs[4];
		unsigned numParticles { 0 };
	};

	struct CompoundDynamicMesh {
		static constexpr unsigned kMaxParts = 16;
		float cullMins[4], cullMaxs[4];
		const DynamicMesh **parts;
		// If this field is present, the baking array size must match the number of parts
		// Values are interpreted as follows:
		// Parts with negative values are drawn with respect of the order of values
		// (values with greater absolute value get drawn first) prior to all other parts.
		// Parts with zero values are drawn in order that is defined by their centers, on top of parts with negative values
		// Parts with positive values are draw with respect of the order of values
		// (values with lesser absolute value get drawn first) after all other parts.
		const float *meshOrderDesignators { nullptr };
		unsigned numParts;
	};
protected:
	Scene();

	wsw::StaticVector<DynamicLight, 1024> m_dynamicLights;

	entity_t *m_worldent;
	entity_t *m_polyent;

	wsw::StaticVector<entity_t *, MAX_ENTITIES + 48> m_entities;
	wsw::StaticVector<entity_t, 2> m_localEntities;
	// TODO: Use different subtypes so the storage is more compact
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_nullModelEntities;
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_aliasModelEntities;
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_skeletalModelEntities;
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_brushModelEntities;
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_spriteEntities;
	wsw::StaticVector<entity_t, MAX_ENTITIES> m_portalSurfaceEntities;

	// These polys are externally owned with a lifetime greater than the frame
	wsw::StaticVector<QuadPoly *, MAX_QUAD_POLYS> m_quadPolys;

	static constexpr unsigned kMaxParticlesInAggregate = 256;
	static constexpr unsigned kMaxParticleAggregates = 1024;

	static constexpr unsigned kMaxPartsInCompoundMesh = 8;
	static constexpr unsigned kMaxCompoundDynamicMeshes = 64;
	static constexpr unsigned kMaxDynamicMeshes = 128;

	wsw::StaticVector<ParticlesAggregate, kMaxParticleAggregates> m_particles;
	wsw::StaticVector<const DynamicMesh *, kMaxDynamicMeshes> m_dynamicMeshes;
	wsw::StaticVector<CompoundDynamicMesh, kMaxCompoundDynamicMeshes> m_compoundDynamicMeshes;
};

// TODO: Aggregate Scene as a member?
class DrawSceneRequest : public Scene {
	friend class wsw::ref::Frontend;

public:
	void addLight( const float *origin, float programRadius, float coronaRadius, float r, float g, float b );
	void addLight( const float *origin, float programRadius, float coronaRadius, const float *color ) {
		addLight( origin, programRadius, coronaRadius, color[0], color[1], color[2] );
	}

	// TODO: Allow adding multiple particle aggregates at once
	void addParticles( const float *mins, const float *maxs,
					   const Particle::AppearanceRules &appearanceRules,
					   const Particle *particles, unsigned numParticles );

	void addCompoundDynamicMesh( const float *mins, const float *maxs,
								 const DynamicMesh **parts, unsigned numParts,
								 const float *meshOrderDesignators = nullptr );

	void addDynamicMesh( const DynamicMesh *mesh );

	void addEntity( const entity_t *ent );

	// No copying is being performed
	void addPoly( QuadPoly *poly ) {
		if( !m_quadPolys.full() ) [[likely]] {
			m_quadPolys.push_back( poly );
		}
	}

	template <unsigned ComponentMask>
	[[nodiscard]]
	auto allocShaderParams() -> ShaderParams * {
		// Make sure only valid bits get supplied (zero is perfectly valid as well)
		static_assert( !( ComponentMask & ~( ShaderParams::Common | ShaderParams::Transform |
			ShaderParams::Material | ShaderParams::Anim | ShaderParams::Skeletal | ShaderParams::Outline ) ) );
		// Casting raw mem like this is perfectly valid as it's trivially constructible
		if( auto *componentsMem = (FrequentlyUsedComponents *)m_frequentlyUsedComponentsAllocator.allocOrNull() ) [[likely]] {
			auto *enclosingParams = new( componentsMem->aggregateStorage )ShaderParams;
			if constexpr( ComponentMask & ShaderParams::Common ) {
				enclosingParams->commonParams = new( componentsMem->commonStorage )ShaderCommonParams;
			}
			if constexpr( ComponentMask & ShaderParams::Transform ) {
				enclosingParams->transformParams = new( componentsMem->transformStorage )ShaderTransformParams;
			}
			if constexpr( ComponentMask & ShaderParams::Material ) {
				enclosingParams->materialParams = new( componentsMem->materialStorage )ShaderMaterialParams;
			}
			if constexpr( ComponentMask & ShaderParams::Anim ) {
				// Always succeeds, assuming we have successfully allocated enclosing params
				enclosingParams->animParams = new( m_animParamsAllocator.allocOrNull() )ShaderAnimParams;
			}
			if constexpr( ComponentMask & ShaderParams::Skeletal ) {
				// Same
				enclosingParams->skeletalParams = new( m_skeletalParamsAllocator.allocOrNull() )ShaderSkeletalParams;
			}
			if constexpr( ComponentMask & ShaderParams::Outline ) {
				// Same
				enclosingParams->outlineParams = new( m_outlineParamsAllocator.allocOrNull() )ShaderOutlineParams;
			}
			enclosingParams->presentComponentMask = ComponentMask;
			return enclosingParams;
		}
		return nullptr;
	}

	explicit DrawSceneRequest( const refdef_t &refdef ) : m_refdef( refdef ) {}

	~DrawSceneRequest() {
		// Prevent triggering assertions (TODO: We don't really need these allocators)
		m_frequentlyUsedComponentsAllocator.clear();
		m_animParamsAllocator.clear();
		m_skeletalParamsAllocator.clear();
		m_outlineParamsAllocator.clear();
	}
private:
	// TODO: Get rid of "refdef_t"
	refdef_t m_refdef;

	// This struct does not actually have much use besides simplifying size calculations
	// TODO: Add ASAN-poisoned fences between fields?
	struct FrequentlyUsedComponents {
		alignas( alignof( ShaderParams ) ) uint8_t aggregateStorage[sizeof( ShaderParams )];
		alignas( alignof( ShaderCommonParams ) ) uint8_t commonStorage[sizeof( ShaderCommonParams )];
		alignas( alignof( ShaderTransformParams ) ) uint8_t transformStorage[sizeof( ShaderTransformParams )];
		alignas( alignof( ShaderMaterialParams ) ) uint8_t materialStorage[sizeof( ShaderMaterialParams )];
	};

	static constexpr unsigned kMaxParamBundles = 1024;

	// Frequently used components get stored together in memory

	// TODO: We don't need freelists, we can use a bitmap-based allocator
	// and raw byte arrays instead of secondary allocators (assuming an index is managed by the bitmap allocator)
	wsw::MemberBasedFreelistAllocator<sizeof( FrequentlyUsedComponents ), kMaxParamBundles> m_frequentlyUsedComponentsAllocator;
	wsw::MemberBasedFreelistAllocator<sizeof( ShaderAnimParams ), kMaxParamBundles> m_animParamsAllocator;
	wsw::MemberBasedFreelistAllocator<sizeof( ShaderSkeletalParams ), kMaxParamBundles> m_skeletalParamsAllocator;
	wsw::MemberBasedFreelistAllocator<sizeof( ShaderOutlineParams ), kMaxParamBundles> m_outlineParamsAllocator;
};

struct QuadPoly {
	struct ViewAlignedSpriteRules {
		float color[4] { 1.0f, 1.0f, 1.0f, 1.0f };
	};

	struct ViewAlignedBeamRules {
		float dir[3];
		float width;
		float tileLength { 0.0f };
		float fromColor[4] { 1.0f, 1.0f, 1.0f, 1.0f };
		float toColor[4] { 1.0f, 1.0f, 1.0f, 1.0f };
	};

	struct OrientedSpriteRules {
		float axis[9];
		float color[4] { 1.0f, 1.0f, 1.0f, 1.0f };
	};

	using AppearanceRules = std::variant<ViewAlignedSpriteRules, ViewAlignedBeamRules, OrientedSpriteRules>;

	struct shader_s *material;
	float origin[3];
	float halfExtent;

	AppearanceRules appearanceRules { ViewAlignedSpriteRules {} };
};

struct DynamicMesh {
	struct shader_s *material;
	float cullMins[4], cullMaxs[4];

	bool applyVertexDynLight { false };

	virtual ~DynamicMesh() = default;

	// The scratchpad could be used for storing important temporaries for reusing during fillMeshBuffers call
	// without complicating state management of implementations.
	// Check the calling code to get the actually available scratchpad size.
	// The cameraId allows caching some properties for different dymamic meshes (which share something)
	// during drawing from the same camera (note that a single viewport may still perfectly involve
	// using multiple cameras during drawing it, e.g while drawing portals).
	// In this case, management of associated storage is a burden of a dynamic mesh implementation.
	[[nodiscard]]
	virtual auto getStorageRequirements( const float *__restrict viewOrigin,
										 const float *__restrict viewAxis,
										 float cameraViewTangent,
										 unsigned cameraId,
										 void *__restrict scratchpad ) const
		-> std::optional<std::pair<unsigned, unsigned>> = 0;

	// The scratchpad points to the same chunk of memory as during getStorageRequirements() call.
	[[nodiscard]]
	virtual auto fillMeshBuffers( const float *__restrict viewOrigin,
								  const float *__restrict viewAxis,
								  float cameraViewTangent,
								  unsigned cameraId,
								  const Scene::DynamicLight *dynamicLights,
								  std::span<const uint16_t> affectingLightIndices,
								  void *__restrict scratchpad,
								  vec4_t *__restrict destPositions,
								  vec4_t *__restrict destNormals,
								  vec2_t *__restrict destTexCoords,
								  byte_vec4_t *__restrict destColors,
								  uint16_t *__restrict destIndices ) const -> std::pair<unsigned, unsigned> = 0;
};

void BeginDrawingScenes();
DrawSceneRequest *CreateDrawSceneRequest( const refdef_t &refdef );
void SubmitDrawSceneRequest( DrawSceneRequest *request );
void EndDrawingScenes();

class Texture;

void        R_DrawStretchPic( int x, int y, int w, int h, float s1, float t1, float s2, float t2,
							  const vec4_t color, const shader_s *shader );
void        R_DrawRotatedStretchPic( int x, int y, int w, int h, float s1, float t1, float s2, float t2,
									 float angle, const vec4_t color, const shader_s *shader );

shader_s *R_WrapMenuTextureHandleInMaterial( unsigned externalTexNum );
shader_s *R_WrapHudTextureHandleInMaterial( unsigned externalTexNum );

struct model_s;

void        R_ModelBounds( const model_s *model, vec3_t mins, vec3_t maxs );
void        R_ModelFrameBounds( const model_s *model, int frame, vec3_t mins, vec3_t maxs );
void        R_RegisterWorldModel( const char *model );
model_s *R_RegisterModel( const char *name );

int         R_SkeletalGetBoneInfo( const model_s *mod, int bonenum, char *name, size_t name_size, int *flags );
void        R_SkeletalGetBonePose( const model_s *mod, int bonenum, int frame, bonepose_t *bonepose );
int         R_SkeletalGetNumBones( const model_s *mod, int *numFrames );

shader_s    *R_RegisterShader( const char *name, int type );
shader_s    *R_RegisterPic( const char *name );
shader_s    *R_RegisterRawAlphaMask( const char *name, int width, int height, const uint8_t *data );
shader_s    *R_RegisterSkin( const char *name );
shader_s    *R_RegisterLinearPic( const char *name );

struct ImageOptions {
	std::optional<std::pair<unsigned, unsigned>> desiredSize;
	unsigned borderWidth { 0 };
	bool fitSizeForCrispness { false };
	bool useOutlineEffect { false };

	template <typename T>
	void setDesiredSize( T width, T height ) {
		assert( width > 0 && height > 0 && width < (T)( 1 << 16 ) && height < (T)( 1 << 16 ) );
		desiredSize = std::make_pair( (unsigned)width, (unsigned)height );
	}
};

shader_s *R_CreateExplicitlyManaged2DMaterial();
void R_ReleaseExplicitlyManaged2DMaterial( shader_s *material );
bool R_UpdateExplicitlyManaged2DMaterialImage( shader_s *material, const char *name, const ImageOptions &options );

[[nodiscard]]
auto R_GetShaderDimensions( const shader_s *shader ) -> std::optional<std::pair<unsigned, unsigned>>;

void        R_ReplaceRawSubPic( shader_s *shader, int x, int y, int width, int height, const uint8_t *data );

struct Skin;
Skin *R_RegisterSkinFile( const char *name );
shader_s *R_FindShaderForSkinFile( const Skin *skin, const char *meshname );

bool RF_TransformVectorToViewport( const refdef_t *rd, const vec3_t in, vec2_t out );
bool RF_LerpTag( orientation_t *orient, const model_s *mod, int oldframe, int frame, float lerpfrac, const char *name );

void        R_SetCustomColor( int num, int r, int g, int b );

struct VidModeOptions {
	bool fullscreen { false };
	bool borderless { false };
};

rserr_t R_TrySettingMode( int x, int y, int width, int height, int displayFrequency, const VidModeOptions &options );

void RF_BeginRegistration();
void RF_EndRegistration();

void RF_AppActivate( bool active, bool minimize, bool destroy );
void RF_Shutdown( bool verbose );

void RF_BeginFrame( bool forceClear, bool forceVsync, bool uncappedFPS );
void RF_EndFrame();

void R_Set2DMode( bool );
void RF_Set2DScissor( int x, int y, int w, int h );

const char *RF_GetSpeedsMessage( char *out, size_t size );

int RF_GetAverageFrametime();

void R_Finish();

rserr_t     R_Init( const char *applicationName, const char *screenshotPrefix, int startupColor,
					int iconResource, const int *iconXPM,
					void *hinstance, void *wndproc, void *parenthWnd,
					bool verbose );

#endif // __REF_H
