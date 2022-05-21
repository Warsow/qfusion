#ifndef WSW_25e6c863_70a4_4d36_9597_133f93b31f91_H
#define WSW_25e6c863_70a4_4d36_9597_133f93b31f91_H

#include <span>

#include "../ref/ref.h"
#include "../qcommon/randomgenerator.h"
#include "../qcommon/freelistallocator.h"

struct CMShapeList;

class SimulatedHullsSystem {
	friend class TransientEffectsSystem;
	friend class TrackedEffectsSystem;
public:
	struct ColorChangeTimelineNode {
		std::span<const byte_vec4_t> replacementPalette;
		// Specifying it as a fraction is more flexible than absolute offsets
		float nodeActivationLifetimeFraction { 1.0f };
		unsigned colorChangeInterval { 15 };
		float dropChance { 0.0f };
		float replacementChance { 0.0f };
	};

	struct HullLayerParams {
		const float speed;
		const float finalOffset;
		const float speedSpikeChance;
		const float minSpeedSpike, maxSpeedSpike;
		const float biasAlongChosenDir;
		const float baseInitialColor[4];
		const float bulgeInitialColor[4];

		std::span<const ColorChangeTimelineNode> colorChangeTimeline;
	};

	SimulatedHullsSystem();
	~SimulatedHullsSystem();

	void simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *request );
private:
	static constexpr unsigned kNumVerticesForSubdivLevel[5] { 12, 42, 162, 642, 2562 };

	struct ColorChangeState {
		int64_t lastColorChangeAt { 0 };
		unsigned lastNodeIndex { 0 };
	};
	struct BaseRegularSimulatedHull {
		CMShapeList *shapeList { nullptr };
		int64_t spawnTime { 0 };

		vec4_t mins, maxs;
		vec3_t origin;

		unsigned lifetime { 0 };
		// Archimedes/xy expansion activation offset
		int64_t expansionStartAt { std::numeric_limits<int64_t>::max() };

		// Old/current
		vec4_t *vertexPositions[2];

		// Velocities of an initial burst, decelerate with time
		vec3_t *vertexBurstVelocities;
		// Velocities produced by external forces during simulation
		vec3_t *vertexForceVelocities;

		byte_vec4_t *vertexColors;

		std::span<const ColorChangeTimelineNode> colorChangeTimeline;
		ColorChangeState colorChangeState;

		float archimedesTopAccel { 0.0f }, archimedesBottomAccel { 0.0f };
		float xyExpansionTopAccel { 0.0f }, xyExpansionBottomAccel { 0.0f };
		float minZLastFrame { std::numeric_limits<float>::max() }, maxZLastFrame { std::numeric_limits<float>::min() };
		float avgXLastFrame { 0.0f }, avgYLastFrame { 0.0f };

		// The renderer assumes external lifetime of the submitted spans. Keep the buffer within the hull.
		ExternalMesh meshSubmissionBuffer[1];

		float lodCurrLevelTangentRatio { 0.25f };
		bool tesselateClosestLod { false };
		bool leprNextLevelColors { false };

		uint8_t positionsFrame { 0 };
		uint8_t subdivLevel { 0 };

		void simulate( int64_t currTime, float timeDeltaSeconds, wsw::RandomGenerator *__restrict rng );
	};

	template <unsigned SubdivLevel>
	struct RegularSimulatedHull : public BaseRegularSimulatedHull {
		static constexpr auto kNumVertices = kNumVerticesForSubdivLevel[SubdivLevel];

		RegularSimulatedHull<SubdivLevel> *prev { nullptr }, *next {nullptr };

		vec4_t storageOfPositions[2][kNumVertices];
		vec3_t storageOfBurstVelocities[kNumVertices];
		vec3_t storageOfForceVelocities[kNumVertices];
		byte_vec4_t storageOfColors[kNumVertices];

		RegularSimulatedHull() {
			this->vertexPositions[0]    = storageOfPositions[0];
			this->vertexPositions[1]    = storageOfPositions[1];
			this->vertexBurstVelocities = storageOfBurstVelocities;
			this->vertexForceVelocities = storageOfForceVelocities;
			this->vertexColors          = storageOfColors;
			this->subdivLevel           = SubdivLevel;
		}
	};

	struct BaseConcentricSimulatedHull {
		// Externally managed, should point to the unit mesh data
		const vec4_t *vertexMoveDirections;
		// Distances to the nearest obstacle (or the maximum growth radius in case of no obstacles)
		float *limitsAtDirections;
		int64_t spawnTime { 0 };

		struct Layer {
			vec4_t mins, maxs;
			vec4_t *vertexPositions;
			// Contains pairs (speed, distance from origin along the direction)
			vec2_t *vertexSpeedsAndDistances;
			byte_vec4_t *vertexColors;
			ExternalMesh *submittedMesh;

			// Subtracted from limitsAtDirections for this layer, must be non-negative.
			// This offset is supposed to prevent hulls from ending at the same distance in the end position.
			float finalOffset { 0 };

			std::span<const ColorChangeTimelineNode> colorChangeTimeline;
			ColorChangeState colorChangeState;

			bool useDrawOnTopHack { false };
		};

		Layer *layers { nullptr };

		vec4_t mins, maxs;
		vec3_t origin;

		unsigned numLayers { 0 };
		unsigned lifetime { 0 };

		uint8_t subdivLevel { 0 };

		void simulate( int64_t currTime, float timeDeltaSeconds, wsw::RandomGenerator *__restrict rng );
	};

	template <unsigned SubdivLevel, unsigned NumLayers>
	struct ConcentricSimulatedHull : public BaseConcentricSimulatedHull {
		static constexpr auto kNumVertices = kNumVerticesForSubdivLevel[SubdivLevel];

		ConcentricSimulatedHull<SubdivLevel, NumLayers> *prev { nullptr }, *next { nullptr };

		Layer storageOfLayers[NumLayers];
		float storageOfLimits[kNumVertices];
		vec4_t storageOfPositions[kNumVertices * NumLayers];
		vec2_t storageOfSpeedsAndDistances[kNumVertices * NumLayers];
		byte_vec4_t storageOfColors[kNumVertices * NumLayers];
		ExternalMesh storageOfMeshes[NumLayers];

		ConcentricSimulatedHull() {
			this->numLayers = NumLayers;
			this->subdivLevel = SubdivLevel;
			this->layers = &storageOfLayers[0];
			this->limitsAtDirections = &storageOfLimits[0];
			for( unsigned i = 0; i < NumLayers; ++i ) {
				Layer *const layer              = &layers[i];
				layer->vertexPositions          = &storageOfPositions[i * kNumVertices];
				layer->vertexSpeedsAndDistances = &storageOfSpeedsAndDistances[i * kNumVertices];
				layer->vertexColors             = &storageOfColors[i * kNumVertices];
				layer->submittedMesh            = &storageOfMeshes[i];
			}
		}
	};

	using FireHull  = ConcentricSimulatedHull<3, 5>;
	using BlastHull = ConcentricSimulatedHull<3, 3>;
	using SmokeHull = RegularSimulatedHull<2>;
	using WaveHull  = RegularSimulatedHull<2>;
	using AreaHull  = RegularSimulatedHull<3>;

	void unlinkAndFreeFireHull( FireHull *hull );
	void unlinkAndFreeBlastHull( BlastHull *hull );
	void unlinkAndFreeSmokeHull( SmokeHull *hull );
	void unlinkAndFreeWaveHull( WaveHull *hull );
	void unlinkAndFreeAreaHull( AreaHull *hull );

	template <typename Hull, bool HasShapeLists>
	[[nodiscard]]
	auto allocHull( Hull **head, wsw::FreelistAllocator *allocator, int64_t currTime, unsigned lifetime ) -> Hull *;

	// TODO: Having these specialized methods while the actual setup is performed by the caller feels wrong...

	[[nodiscard]]
	auto allocFireHull( int64_t currTime, unsigned lifetime ) -> FireHull *;
	[[nodiscard]]
	auto allocBlastHull( int64_t currTime, unsigned lifetime ) -> BlastHull *;
	[[nodiscard]]
	auto allocSmokeHull( int64_t currTime, unsigned lifetime ) -> SmokeHull *;
	[[nodiscard]]
	auto allocWaveHull( int64_t currTime, unsigned lifetime ) -> WaveHull *;
	[[nodiscard]]
	auto allocAreaHull( int64_t currTime, unsigned lifetime ) -> AreaHull *;

	void setupHullVertices( BaseRegularSimulatedHull *hull, const float *origin, const float *color,
							float speed, float speedSpread );

	void setupHullVertices( BaseConcentricSimulatedHull *hull, const float *origin,
							float scale, std::span<const HullLayerParams> paramsOfLayers );

	static void processColorChange( int64_t currTime, int64_t spawnTime, unsigned effectDuration,
									std::span<const ColorChangeTimelineNode> timeline,
									std::span<byte_vec4_t> colors,
									ColorChangeState *__restrict state,
									wsw::RandomGenerator *__restrict rng );

	[[nodiscard]]
	static auto computeCurrTimelineNodeIndex( unsigned startFromIndex, int64_t currTime,
											  int64_t spawnTime, unsigned effectDuration,
											  std::span<const ColorChangeTimelineNode> timeline ) -> unsigned;

	struct LodSetupParams {
		unsigned currSubdivLevel;
		unsigned minSubdivLevel;
		float currLevelTangentRatio;
		bool tesselateClosestLod;
		bool lerpNextLevelColors;
	};

	[[nodiscard]]
	static auto setupLods( ExternalMesh::LodProps *lods, LodSetupParams &&params ) -> unsigned;

	FireHull *m_fireHullsHead { nullptr };
	BlastHull *m_blastHullsHead { nullptr };
	SmokeHull *m_smokeHullsHead { nullptr };
	WaveHull *m_waveHullsHead { nullptr };
	AreaHull *m_areaHullsHead { nullptr };

	static constexpr unsigned kMaxFireHulls  = 32;
	static constexpr unsigned kMaxBlastHulls = 32;
	static constexpr unsigned kMaxSmokeHulls = kMaxFireHulls * 2;
	static constexpr unsigned kMaxWaveHulls  = kMaxFireHulls;
	static constexpr unsigned kMaxAreaHulls  = 32;

	wsw::StaticVector<CMShapeList *, kMaxSmokeHulls + kMaxWaveHulls + kMaxAreaHulls> m_freeShapeLists;
	CMShapeList *m_tmpShapeList { nullptr };

	wsw::HeapBasedFreelistAllocator m_fireHullsAllocator { sizeof( FireHull ), kMaxFireHulls };
	wsw::HeapBasedFreelistAllocator m_blastHullsAllocator { sizeof( BlastHull ), kMaxBlastHulls };
	wsw::HeapBasedFreelistAllocator m_smokeHullsAllocator { sizeof( SmokeHull ), kMaxSmokeHulls };
	wsw::HeapBasedFreelistAllocator m_waveHullsAllocator { sizeof( WaveHull ), kMaxWaveHulls };
	wsw::HeapBasedFreelistAllocator m_areaHullsAllocator { sizeof( AreaHull ), kMaxAreaHulls };

	wsw::RandomGenerator m_rng;
	int64_t m_lastTime { 0 };
};

#endif