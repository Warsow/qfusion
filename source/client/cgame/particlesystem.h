#ifndef WSW_d66e64a4_3cf7_44a9_a3f3_cc95b2beaa6a_H
#define WSW_d66e64a4_3cf7_44a9_a3f3_cc95b2beaa6a_H

struct CMShapeList;

template <typename> class SingletonHolder;

#include <common/freelistallocator.h>
#include <common/randomgenerator.h>
#include <client/renderer/ref.h>
#include <common/podbufferholder.h>
// TODO: Lift it to the top level
#include <server/game/ai/vec3.h>
#include "polyeffectssystem.h"

#include <span>

// Few notes on these value ranges:
// They are ad-hoc structs for now, as we don't need more complicated stuff at this stage.
// Distributions are obviously assumed to be uniform.
// It should not matter whether a bound is inclusive or exclusive due to a coarse nature of the corresponding value,
// unless it's explicitly indicated via a field name.

// Mutability of fields makes adjusting parameters in a loop more convenient
struct EllipsoidalFlockParams {
	float origin[3] { 0.0f, 0.0f, 0.0f };
	float offset[3] { 0.0f, 0.0f, 0.0f };
	float shiftDir[3] { 0.0f, 0.0f, 1.0f };
	// So far, only stretching/flattening the sphere along the single dir seems to be useful
	float stretchDir[3] { 0.0f, 0.0f, 1.0f };
	float stretchScale { 1.0f };
	float gravity { 600 };
	float drag { 0.0f };
	// Degrees per second for points which are close to the origin
	float vorticityAngularSpeed { 0.0f };
	float vorticityAxis[3] { 0.0f, 0.0f, 1.0f };
	// Units per second for points which are close to the origin
	float outflowSpeed { 0.0f };
	// Axis of the outflow, should be a unit vector
	float outflowAxis[3] { 0.0f, 0.0f, 1.0f };
	float turbulenceSpeed { 0.0f };
	float turbulenceScale { 1.0f };
	float restitution { 0.75f };
	struct { unsigned minInclusive { 1 }, maxInclusive { 1 }; } bounceCount;
	struct { float min { 300 }, max { 300 }; } speed;
	struct { float min { 0.0f }, max { 0.0f }; } shiftSpeed;
	struct { float min { 0.0f }, max { 0.0f }; } randomInitialRotation;
	struct { float min { 0.0f }, max { 0.0f }; } angularVelocity;
	struct { float min { 0.0f }, max { 1.0f }; } percentage;
	struct { unsigned min { 300u }, max { 700u }; } timeout;
	struct { unsigned min { 0 }, max { 0 }; } activationDelay;
	unsigned startBounceCounterDelay { 0 };
};

// Mutability of fields makes adjusting parameters in a loop more convenient
struct ConicalFlockParams {
	float origin[3] { 0.0f, 0.0f, 0.0f };
	float offset[3] { 0.0f, 0.0f, 0.0f };
	float dir[3] { 0.0f, 0.0f, 1.0f };
	float shiftDir[3] { 0.0f, 0.0f, 1.0f };
	float gravity { 600 };
	float drag { 0.0f };
	// Degrees per second for points which are close to the origin
	float vorticityAngularSpeed { 0.0f };
	float vorticityAxis[3] { 0.0f, 0.0f, 1.0f };
	// Units per second for points which are close to the origin
	float outflowSpeed { 0.0f };
	// Axis of the outflow, should be a unit vector
	float outflowAxis[3] { 0.0f, 0.0f, 1.0f };
	float turbulenceSpeed { 0.0f };
	float turbulenceScale { 1.0f };
	float restitution { 0.75f };
	float angle { 45.0f };
	float innerAngle { 0.0f };
	struct { unsigned minInclusive { 1 }, maxInclusive { 1 }; } bounceCount;
	struct { float min { 300 }, max { 300 }; } speed;
	struct { float min { 0.0f }, max { 0.0f }; } shiftSpeed;
	struct { float min { 0.0f }, max { 0.0f }; } randomInitialRotation;
	struct { float min { 0.0f }, max { 0.0f }; } angularVelocity;
	struct { float min { 0.0f }, max { 1.0f }; } percentage;
	struct { unsigned min { 300u }, max { 700u }; } timeout;
	struct { unsigned min { 0 }, max { 0 }; } activationDelay;
	unsigned startBounceCounterDelay { 0 };
};

struct FillFlockResult {
	int64_t resultTimeout;
	unsigned numParticles;
};

[[nodiscard]]
auto fillParticleFlock( const EllipsoidalFlockParams *__restrict params,
				        Particle *__restrict particles,
				        unsigned maxParticles,
				        const Particle::AppearanceRules *__restrict appearanceRules,
				        wsw::RandomGenerator *__restrict rng,
				        int64_t currTime, signed signedStride = 1, float extraScale = 1.0f ) -> FillFlockResult;

[[nodiscard]]
auto fillParticleFlock( const ConicalFlockParams *__restrict params,
				        Particle *__restrict particles,
				        unsigned maxParticles,
						const Particle::AppearanceRules *__restrict appearanceRules,
				  		wsw::RandomGenerator *__restrict rng,
						int64_t currTime, signed signedStride = 1, float extraScale = 1.0f ) -> FillFlockResult;

struct ParticleTrailUpdateParams {
	unsigned maxParticlesPerDrop { 1 };
	float dropDistance { 8.0f };
	float particleSizeMultiplier { 1.0f };
};

struct ParamsOfParticleTrailOfParticles {
	const Particle::AppearanceRules appearanceRules;
	ConicalFlockParams flockParamsTemplate;
	const ParticleTrailUpdateParams updateParams;
	const bool modulateByParentSize {false};
};

struct ParamsOfPolyTrailOfParticles {
	CurvedPolyTrailProps props;
	shader_s *material { nullptr };
};

struct PolyTrailOfParticles;

struct alignas( 16 ) ParticleFlock {
	Particle::AppearanceRules appearanceRules;
	float drag { 0.0f };

	// note that the accumulated error due simulation timestep size leads to travelling in the radial direction
	// this leads to larger spirals at lower framerates (or at lag spikes)
	// also note induced velocities by vorticity and outflow are proportional to 1/r
	// for vorticity the distance to vorticityOrigin and for outflow the distance to the line formed by the origin and vector

	// Radians/second for points on a reference distance
	float vorticityAngularSpeedRadians { 0.0f };
	// Units per second for points on a reference distance
	float outflowSpeed { 0.0f };
	// Units per second
	float turbulenceSpeed { 0.0f };
	float restitution { 0.75f };
	Particle *particles;
	int64_t timeoutAt;
	unsigned numActivatedParticles { 0 };
	unsigned numDelayedParticles { 0 };
	// Delayed particles are kept in the same memory chunk after the spawned ones.
	// delayedParticlesOffset should be >= numActivatedParticles.
	unsigned delayedParticlesOffset { 0 };
	unsigned minBounceCount { 0 }, maxBounceCount { 0 };
	unsigned startBounceCounterDelay { 0 };
	float keepOnImpactProbability { 1.0f };
	// TODO: Make links work with "m_"
	ParticleFlock *prev { nullptr }, *next { nullptr };
	// Set if the flock is a trail flock of particles
	ConicalFlockParams *flockParamsTemplate;
	// Set if the flock is a trail flock of particles
	const ParticleTrailUpdateParams *particleTrailUpdateParams;
	// May be set if the flock is a regular flock
	ParticleFlock *trailFlockOfParticles { nullptr };
	// Set if the flock is a trail flock of particles
	vec3_t *lastParticleTrailDropOrigins { nullptr };
	// May be set if the flock is a regular flock
	PolyTrailOfParticles *polyTrailOfParticles { nullptr };
	float mins[4];
	float maxs[4];
	unsigned lastLightEmitterParticleIndex { 0 };
	bool hasRotatingParticles { false };
	bool needsClipping { false };
	uint8_t globalBinIndex { 255 };
	uint8_t groupBinIndex { 255 };
	uint8_t underlyingStorageCapacity { 0 };
	// Put these fields last as they are rarely used
	float turbulenceCoordinateScale { 1.0f };
	// The origin of the vorticity effect
	float vorticityOrigin[3];
	float vorticityAxis[3];
	// Point used to define the line of the outflow
	float outflowOrigin[3]; //
	// Axis of the outflow, should be a unit vector
	float outflowAxis[3] { 0.0f, 0.0f, 1.0f };
	bool modulateByParentSize;
};

void updateParticleTrail( ParticleFlock *flock, ConicalFlockParams *flockParamsTemplate,
						  const float *actualOrigin, float *lastDropOrigin,
						  wsw::RandomGenerator *rng, int64_t currTime,
						  const ParticleTrailUpdateParams &updateParams );

class ParticleSystem {
public:
	static constexpr unsigned kMaxClippedTrailFlockSize = 64;
	static constexpr unsigned kMaxNonClippedTrailFlockSize = 96;

	static constexpr unsigned kClippedTrailFlocksBin = 3;
	static constexpr unsigned kNonClippedTrailFlocksBin = 4;
private:
	template<typename> friend class SingletonHolder;

	// Note: Externally managed entity trails also are put in this kind of bins
	struct RegularFlocksBin {
		wsw::HeapBasedFreelistAllocator allocator;
		ParticleFlock *head { nullptr };
		unsigned indexOfTrailBin { ~0u };
		unsigned maxParticlesPerFlock { 0 };
		bool needsClipping { false };

		RegularFlocksBin( unsigned maxParticlesPerFlock, unsigned maxFlocks );
	};

	// Particle trails of individual porticles are put in this kind of bin
	// TODO: The name can be confusing (see the RegularFlocksBin remark)
	struct ParticleTrailBin {
		wsw::HeapBasedFreelistAllocator allocator;
		const uint16_t trailFlockParamsOffset { 0 };
		const uint16_t trailUpdateParamsOffset { 0 };
		const uint16_t originsDataOffset { 0 };
		const uint16_t particleDataOffset { 0 };
		unsigned maxParticlesPerFlock { 0 };
		ParticleFlock *lingeringFlocksHead { nullptr };

		struct SizeSpec {
			unsigned elementSize;
			uint16_t flockParamsOffset;
			uint16_t updateParamsOffset;
			uint16_t originsOffset;
			uint16_t particleDataOffset;
		};

		[[nodiscard]] static auto calcSizeOfFlockData( unsigned maxParticlesPerFlock ) -> SizeSpec;
		ParticleTrailBin( unsigned maxParticlesPerFlock, unsigned maxFlocks );
	};

	struct PolyTrailBin {
		wsw::HeapBasedFreelistAllocator allocator;
		PolyTrailOfParticles *activeTrailsHead { nullptr };
		PolyTrailOfParticles *lingeringTrailsHead { nullptr };

		PolyTrailBin( unsigned maxParticlesPerFlock, unsigned maxFlocks );
	};

	static constexpr unsigned kMaxSmallFlocks  = 64;
	static constexpr unsigned kMaxMediumFlocks = 48;
	static constexpr unsigned kMaxLargeFlocks  = 24;

	static constexpr unsigned kMaxClippedTrailFlocks    = 32;
	static constexpr unsigned kMaxNonClippedTrailFlocks = 48;

	static constexpr unsigned kMaxSmallFlockSize  = 8;
	static constexpr unsigned kMaxMediumFlockSize = 48;
	static constexpr unsigned kMaxLargeFlockSize  = 144;

	// TODO: Vary by required trail length
	static constexpr unsigned kMaxSmallTrailFlockSize  = 128;
	// A ParticleAggregate cannot be larger than 256 elements,
	// and also we store the growth limit for a flock in an uint8_t variable, so let's limit it by 255.
	static constexpr unsigned kMaxMediumTrailFlockSize = 255;
	static constexpr unsigned kMaxLargeTrailFlockSize  = 255;

	wsw::StaticVector<RegularFlocksBin, 5> m_regularFlockBins;
	wsw::StaticVector<ParticleTrailBin, 3> m_trailsOfParticlesBins;
	wsw::StaticVector<PolyTrailBin, 3> m_polyTrailBins;
	int64_t m_lastTime { 0 };

	CMShapeList *m_tmpShapeList { nullptr };

	wsw::RandomGenerator m_rng;

	// TODO: Heap-allocate (we do not want to include heavyweight std headers and there's no alternative for now).
	// Caution: Once-added data must preserve its address during frame, so relocatable vectors are not an option.
	wsw::StaticVector<Particle::AppearanceRules, 36> m_frameFlareAppearanceRules;
	wsw::StaticVector<RgbaLifespan, 256> m_frameFlareColorLifespans;
	wsw::StaticVector<Particle, 256> m_frameFlareParticles;

	void unlinkAndFree( ParticleFlock *flock );
	void unlinkAndFree( PolyTrailOfParticles *trail );

	[[nodiscard]]
	auto createFlock( unsigned regularBinIndex, const Particle::AppearanceRules &rules,
					  const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
					  const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr ) -> ParticleFlock *;

	template <typename FlockParams>
	void addParticleFlockImpl( const Particle::AppearanceRules &appearanceRules,
							   const FlockParams &flockParams, unsigned binIndex, unsigned maxParticles,
							   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
							   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail );

	template <typename FlockParams>
	void setupFlockFieldsFromParams( ParticleFlock *__restrict flock, const FlockParams &flockParams );

	static void runStepKinematics( ParticleFlock *__restrict flock, float deltaSeconds, vec3_t resultBounds[2] );

	[[nodiscard]]
	static auto activateDelayedParticles( ParticleFlock *flock, int64_t currTime ) -> std::optional<int64_t>;

	void simulate( ParticleFlock *flock, wsw::RandomGenerator *rng, int64_t currTime, float deltaSeconds );
	void simulateWithoutClipping( ParticleFlock *__restrict flock, int64_t currTime, float deltaSeconds );

	[[nodiscard]]
	auto updateLifetimeOfActiveParticlesWithoutClipping( ParticleFlock *__restrict flock, int64_t currTime ) -> int64_t;

	void simulateParticleTrailOfParticles( ParticleFlock *baseFlock, wsw::RandomGenerator *rng, int64_t currTime, float deltaSeconds );
	void simulatePolyTrailOfParticles( ParticleFlock *baseFlock, PolyTrailOfParticles *trail, int64_t currTime );

	void submitFlock( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest );
	void submitPolyTrail( PolyTrailOfParticles *trail, DrawSceneRequest *drawSceneRequest );

	void tryAddingLight( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest );
	void tryAddingFlares( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest );
public:
	ParticleSystem();
	~ParticleSystem();

	void clear();

	// Use this non-templated interface to reduce call site code bloat
	// TODO: Get rid of user-visible distinction of bins!!!!!

	void addSmallParticleFlock( const Particle::AppearanceRules &rules, const EllipsoidalFlockParams &flockParams,
								const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );
	void addSmallParticleFlock( const Particle::AppearanceRules &rules, const ConicalFlockParams &flockParams,
								const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );

	void addMediumParticleFlock( const Particle::AppearanceRules &rules, const EllipsoidalFlockParams &flockParams,
								 const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								 const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );
	void addMediumParticleFlock( const Particle::AppearanceRules &rules, const ConicalFlockParams &flockParams,
								 const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								 const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );

	void addLargeParticleFlock( const Particle::AppearanceRules &rules, const EllipsoidalFlockParams &flockParams,
								const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								const ParamsOfPolyTrailOfParticles *paramsOfTrails = nullptr );
	void addLargeParticleFlock( const Particle::AppearanceRules &rules, const ConicalFlockParams &flockParams,
								const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								const ParamsOfPolyTrailOfParticles *paramsOfTrails = nullptr );

	[[nodiscard]]
	auto createTrailFlock( const Particle::AppearanceRules &appearanceRules,
						   const ConicalFlockParams &initialFlockParams, unsigned binIndex ) -> ParticleFlock *;

	void destroyTrailFlock( ParticleFlock *flock ) { unlinkAndFree( flock ); }

	void simulateFrame( int64_t currTime );
	void submitToScene( int64_t currTime, DrawSceneRequest *drawSceneRequest );
};

#endif
