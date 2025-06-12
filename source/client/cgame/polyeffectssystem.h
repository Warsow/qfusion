/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2022 Chasseur de bots

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

#ifndef WSW_424d6447_3887_491d_8150_c80cf50fd066_H
#define WSW_424d6447_3887_491d_8150_c80cf50fd066_H

#include <client/renderer/ref.h>
#include <common/helpers/randomgenerator.h>
#include <common/helpers/freelistallocator.h>
#include <common/facilities/q_comref.h>

class DrawSceneRequest;

struct CMShapeList;

struct CurvedPolyTrailProps {
	float minDistanceBetweenNodes { 8.0f };
	unsigned maxNodeLifetime { 250 };
	unsigned lingeringLimit { 200 };
	// This value is not that permissive due to long segments for some trails.
	float maxLength { 250.0f };
	float width { 4.0f };
	float fromColor[4] { 1.0f, 1.0f, 1.0f, 0.0f };
	float toColor[4] { 1.0f, 1.0f, 1.0f, 0.2f };
};

class PolyEffectsSystem {
public:
	// Opaque handles TODO optimize layout?
	struct CurvedBeam {};
	struct StraightBeam {};

	// Tile the image each tileLength units
	struct UvModeTile { float tileLength; };
	// Assign UV's as the beam is drawn using its full length
	struct UvModeClamp {};
	// Fit the image within the actual beam length
	struct UvModeFit {};

	using CurvedBeamUVMode = std::variant<UvModeTile, UvModeClamp, UvModeFit>;

    PolyEffectsSystem();
	~PolyEffectsSystem();

	void clear();

	[[nodiscard]]
	auto createCurvedBeamEffect( shader_s *material, unsigned povPlayerMask ) -> CurvedBeam *;
	void updateCurvedBeamEffect( CurvedBeam *, const float *fromColor, const float *toColor,
								 float width, const CurvedBeamUVMode &uvMode, std::span<const vec3_t> points );
	void destroyCurvedBeamEffect( CurvedBeam * );

	[[nodiscard]]
	auto createStraightBeamEffect( shader_s *material, unsigned povPlayerMask ) -> StraightBeam *;
	void updateStraightBeamEffect( StraightBeam *, const float *fromColor, const float *toColor,
								   float width, float tileLength, const float *from, const float *to );
	void destroyStraightBeamEffect( StraightBeam * );

	struct TransientBeamParams {
		shader_s *material { nullptr };
		RgbaLifespan beamColorLifespan;
		std::optional<std::pair<unsigned, LightLifespan>> lightProps;
		float width { 0.0f };
		float tileLength { 0.0f };
		unsigned timeout { 500 };
		unsigned numPlanes { 1 };
	};

	void spawnTransientBeamEffect( const float *from, const float *to, TransientBeamParams &&params );

	struct TracerParams {
		struct AlignForPovParams {
			vec3_t offsetProjectionsOntoViewAxis;
			unsigned povNum;
		};

		shader_s *material { nullptr };
		std::optional<AlignForPovParams> alignForPovParams;
		unsigned duration { 100 };
		float prestepDistance { 0.0f };
		float smoothEdgeDistance { 0.0f };
		float width { 0.0f };
		float minLength { 0.0f };
		float distancePercentage { 0.1f };
		float color[4] { 1.0f, 1.0f, 1.0f, 1.0f };
		float programLightRadius { 0.0f };
		float coronaLightRadius { 0.0f };
		float lightColor[3] { 1.0f, 1.0f, 1.0f };
		uint8_t lightFrameAffinityModulo { 0 };
		uint8_t lightFrameAffinityIndex { 0 };
	};

	// Returns an estimated hit time on success
	[[nodiscard]]
	auto spawnTracerEffect( const float *from, const float *to, unsigned povPlayerMask, TracerParams &&params ) -> std::optional<unsigned>;

	struct ImpactRosetteParams {
		shader_s *spikeMaterial { nullptr };
		shader_s *flareMaterial { nullptr };
		float origin[3];
		float offset[3];
		float dir[3];
		float innerConeAngle { 18.0f };
		float outerConeAngle { 30.0f };
		float spawnRingRadius { 1.0f };
		struct { float mean; float spread; } length;
		struct { float mean; float spread; } width;
		struct { unsigned min; unsigned max; } timeout;
		struct { unsigned min; unsigned max; } count;
		RgbaLifespan startColorLifespan;
		RgbaLifespan endColorLifespan;
		RgbaLifespan flareColorLifespan;
		std::optional<LightLifespan> lightLifespan;
		// TODO: Make fields to be of uint16_t type
		unsigned elementFlareFrameAffinityModulo { 0 };
		unsigned effectFlareFrameAffinityModulo { 0 };
		unsigned effectFlareFrameAffinityIndex { 0 };
		unsigned lightFrameAffinityModulo { 0 };
		unsigned lightFrameAffinityIndex { 0 };
	};

	void spawnImpactRosette( ImpactRosetteParams &&params );

	struct SimulatedRingParams {
		float origin[3];
		float offset[3];
		float axisDir[3];
		ValueLifespan alphaLifespan;
		struct { float mean; float spread; } innerSpeed;
		struct { float mean; float spread; } outerSpeed;
		unsigned lifetime { 100 };
		unsigned simulationDelay { 0 };
		unsigned movementDuration { ~0u };
		// 0.0 maps the inner ring to a texture center.
		// 0.5 maps the inner ring to a circle (half as small as a (maximal) inscribed circle) with center matching texture center.
		float innerTexCoordFrac { 0.0f };
		// 1.0 maps the outer ring to a (maximal) inscribed circle with center matching texture center.
		float outerTexCoordFrac { 1.0f };
		unsigned numClipMoveSmoothSteps { 0 };
		unsigned numClipAlphaSmoothSteps { 0 };
		bool softenOnContact { false };
		shader_s *material { nullptr };
	};

	// TODO: Extract it to the outer scope?
	struct CurvedBeamPoly : public DynamicMesh {
		// Points to an externally owned buffer
		std::span<const vec3_t> points;
		float fromColor[4];
		float toColor[4];
		float width;
		CurvedBeamUVMode uvMode;

		[[nodiscard]]
		auto getStorageRequirements( const float *, const float *, float, float, unsigned, void *, unsigned ) const
			-> std::optional<std::pair<unsigned, unsigned>> override;

		[[nodiscard]]
		auto fillMeshBuffers( const float *__restrict viewOrigin,
							  const float *__restrict viewAxis,
							  const Scene::DynamicLight *,
							  std::span<const uint16_t>,
							  void *__restrict,
							  vec4_t *__restrict positions,
							  vec4_t *__restrict normals,
							  vec2_t *__restrict texCoords,
							  byte_vec4_t *__restrict colors,
							  uint16_t *__restrict indices ) const -> std::pair<unsigned, unsigned> override;
	};

	void spawnSimulatedRing( SimulatedRingParams &&params );

	void simulateFrame( int64_t currTime );
	void submitToScene( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask );
private:
	struct StraightBeamEffect : public StraightBeam {
		StraightBeamEffect *prev { nullptr }, *next { nullptr };
		QuadPoly poly;
		unsigned povPlayerMask { ~0u };
	};

	struct CurvedBeamEffect : public CurvedBeam {
		CurvedBeamEffect *prev { nullptr }, *next { nullptr };
		CurvedBeamPoly poly;
		unsigned povPlayerMask { ~0u };
	};

	struct TransientBeamEffect {
		TransientBeamEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime;
		RgbaLifespan colorLifespan;
		unsigned timeout;
		// Contrary to tracked laser beams, the light position is computed in an "immediate" mode.
		// As these beams are multifunctional, the light appearance is more configurable.
		// This field is a-maybe pair of light timeout and light lifespan
		std::optional<std::pair<unsigned, LightLifespan>> lightProps;
		QuadPoly poly;
	};

	struct TracerEffect {
		TracerEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime;
		int64_t selectedForSubmissionAt;
		unsigned timeout;
		unsigned povPlayerMask { ~0u };
		std::optional<TracerParams::AlignForPovParams> alignForPovParams;
		vec3_t from;
		vec3_t to;
		vec3_t dir;
		float speed { 0.0f };
		float prestepDistance { 0.0f };
		float totalDistance { 0.0f };
		float distanceSoFar { 0.0f };
		float initialColorAlpha { 0.0f };
		float smoothEdgeDistance { 0.0f };
		float lightFadeInDistance { 0.0f };
		float lightFadeOutDistance { 0.0f };
		QuadPoly poly;
		float programLightRadius { 0.0f };
		float coronaLightRadius { 0.0f };
		float lightColor[3] { 1.0f, 1.0f, 1.0f };
		uint16_t lightFrameAffinityModulo { 0 };
		uint16_t lightFrameAffinityIndex { 0 };
    };

	struct ImpactRosetteEffect;

	struct ImpactRosetteElement {
		vec3_t from;
		vec3_t dir;
		float desiredLength;
		float lengthLimit;
		float width;
		float lifetimeFrac;
		unsigned lifetime;
	};

	static constexpr unsigned kMaxImpactRosetteElements = 8;

	struct ImpactRosetteSpikesPoly : public DynamicMesh {
		ImpactRosetteEffect *parentEffect;

		[[nodiscard]]
		auto getStorageRequirements( const float *, const float *, float, float, unsigned, void *, unsigned ) const
			-> std::optional<std::pair<unsigned, unsigned>> override;

		[[nodiscard]]
		auto fillMeshBuffers( const float *__restrict viewOrigin,
							  const float *__restrict viewAxis,
							  const Scene::DynamicLight *,
							  std::span<const uint16_t>,
							  void *__restrict,
							  vec4_t *__restrict positions,
							  vec4_t *__restrict normals,
							  vec2_t *__restrict texCoords,
							  byte_vec4_t *__restrict colors,
							  uint16_t *__restrict indices ) const -> std::pair<unsigned, unsigned> override;
	};

	struct ImpactRosetteFlarePoly : public DynamicMesh {
		ImpactRosetteEffect *parentEffect;

		[[nodiscard]]
		auto getStorageRequirements( const float *, const float *, float, float, unsigned, void *, unsigned ) const
			-> std::optional<std::pair<unsigned, unsigned>> override;

		[[nodiscard]]
		auto fillMeshBuffers( const float *__restrict viewOrigin,
							  const float *__restrict viewAxis,
							  const Scene::DynamicLight *,
							  std::span<const uint16_t>,
							  void *__restrict,
							  vec4_t *__restrict positions,
							  vec4_t *__restrict normals,
							  vec2_t *__restrict texCoords,
							  byte_vec4_t *__restrict colors,
							  uint16_t *__restrict indices ) const -> std::pair<unsigned, unsigned> override;
	};

	struct ImpactRosetteEffect {
		ImpactRosetteEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime;
		unsigned lifetime;
		RgbaLifespan startColorLifespan;
		RgbaLifespan endColorLifespan;
		RgbaLifespan flareColorLifespan;
		std::optional<LightLifespan> lightLifespan;
		unsigned elementFlareFrameAffinityModulo;
		unsigned effectFlareFrameAffinityModulo;
		unsigned effectFlareFrameAffinityIndex;
		unsigned lightFrameAffinityModulo;
		unsigned lightFrameAffinityIndex;
		unsigned lastLightEmitterElementIndex { 0 };
		ImpactRosetteElement elements[kMaxImpactRosetteElements];
		unsigned numElements { 0 };
		unsigned numFlareElementsThisFrame { 0 };
		uint8_t flareElementIndices[kMaxImpactRosetteElements];
		ImpactRosetteSpikesPoly spikesPoly;
		ImpactRosetteFlarePoly flarePoly;
	};

	struct RibbonEffect;

	struct RibbonPoly : public DynamicMesh {
		RibbonEffect *parentEffect { nullptr };
		float lifetimeFrac { 0.0f };

		[[nodiscard]]
		auto getStorageRequirements( const float *, const float *, float, float, unsigned, void *, unsigned ) const
			-> std::optional<std::pair<unsigned, unsigned>> override;

		[[nodiscard]]
		auto fillMeshBuffers( const float *__restrict viewOrigin,
							  const float *__restrict viewAxis,
							  const Scene::DynamicLight *,
							  std::span<const uint16_t>,
							  void *__restrict,
							  vec4_t *__restrict positions,
							  vec4_t *__restrict normals,
							  vec2_t *__restrict texCoords,
							  byte_vec4_t *__restrict colors,
							  uint16_t *__restrict indices ) const -> std::pair<unsigned, unsigned> override;
	};

	// TODO: Differentiate for different kinds of ribbons
	static constexpr unsigned kMaxRibbonEdges = 32;

	struct RibbonEffect {
		RibbonEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		unsigned lifetime { 0 };
		unsigned numEdges { 0 };
		unsigned movementDuration { 0 };
		unsigned simulationDelay { 0 };
		unsigned numClipMoveSmoothSteps { 0 };
		unsigned numClipAlphaSmoothSteps { 0 };
		// TODO: Experiment with an AOS memory layout
		vec3_t innerPositions[kMaxRibbonEdges];
		vec3_t outerPositions[kMaxRibbonEdges];
		vec3_t innerVelocities[kMaxRibbonEdges];
		vec3_t outerVelocities[kMaxRibbonEdges];
		vec2_t innerTexCoords[kMaxRibbonEdges];
		vec2_t outerTexCoords[kMaxRibbonEdges];
		// TODO: Unused for non-softened-on-contact ribbons, use a subtype?
		float innerContactAlpha[kMaxRibbonEdges];
		float outerContactAlpha[kMaxRibbonEdges];
		ValueLifespan alphaLifespan;
		RibbonPoly poly;
		bool isLooped { false };
		bool softenOnContact { false };
	};

	void destroyTransientBeamEffect( TransientBeamEffect *effect );
	void destroyTracerEffect( TracerEffect *effect );
	void destroyImpactRosetteEffect( ImpactRosetteEffect *effect );
	void destroyRibbonEffect( RibbonEffect *effect );

	void simulateBeams( int64_t currTime, float timeDeltaSeconds );
	void simulateTracers( int64_t currTime, float timeDeltaSeconds );
	void simulateRosettes( int64_t currTime, float timeDeltaSeconds );
	void simulateRibbons( int64_t currTime, float timeDeltaSeconds );

	void submitBeams( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask );
	void submitTracers( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask );
	void submitRosettes( int64_t currTime, DrawSceneRequest *request );
	void submitRibbons( int64_t currTime, DrawSceneRequest *request );

	void simulateRibbon( RibbonEffect *ribbon, float timeDeltaSeconds );

	struct SmoothRibbonParams {
		unsigned numEdges;
		unsigned numSteps;
		bool isLooped;
	};

	[[nodiscard]]
	static auto smoothRibbonFractions( float *original, float *pingPongBuffer1,
									   float *pingPongBuffer2, SmoothRibbonParams &&params ) -> float *;

	wsw::HeapBasedFreelistAllocator m_straightLaserBeamsAllocator { sizeof( StraightBeamEffect ), MAX_CLIENTS * 4 };
	wsw::HeapBasedFreelistAllocator m_curvedLaserBeamsAllocator { sizeof( CurvedBeamEffect ), MAX_CLIENTS * 4 };
	wsw::HeapBasedFreelistAllocator m_transientBeamsAllocator { sizeof( TransientBeamEffect ), MAX_CLIENTS * 2 };
    wsw::HeapBasedFreelistAllocator m_tracerEffectsAllocator { sizeof( TracerEffect ), MAX_CLIENTS * 8 };
    wsw::HeapBasedFreelistAllocator m_impactRosetteEffectsAllocator { sizeof( ImpactRosetteEffect ), 64 };
	wsw::HeapBasedFreelistAllocator m_ribbonEffectsAllocator { sizeof( RibbonEffect ), 32 };

	StraightBeamEffect *m_straightLaserBeamsHead { nullptr };
	CurvedBeamEffect *m_curvedLaserBeamsHead { nullptr };
	TransientBeamEffect *m_transientBeamsHead { nullptr };
	TracerEffect *m_tracerEffectsHead { nullptr };
	ImpactRosetteEffect *m_impactRosetteEffectsHead { nullptr };
	RibbonEffect *m_ribbonEffectsHead { nullptr };

	// TODO: Use the shared instance for the entire client codebase
	wsw::RandomGenerator m_rng;
	// TODO: Use some shared instance for the entire client codebase
	CMShapeList *m_tmpShapeList { nullptr };

	int64_t m_lastTime { 0 };
};

#endif
