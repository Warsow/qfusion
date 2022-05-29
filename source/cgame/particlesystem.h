#ifndef WSW_d66e64a4_3cf7_44a9_a3f3_cc95b2beaa6a_H
#define WSW_d66e64a4_3cf7_44a9_a3f3_cc95b2beaa6a_H

struct CMShapeList;

template <typename> class SingletonHolder;

#include "../qcommon/freelistallocator.h"
#include "../qcommon/randomgenerator.h"
#include "../ref/ref.h"

#include <span>

// Mutability of fields makes adjusting parameters in a loop more convenient
struct UniformFlockParams {
	float origin[3] { 1.0f / 0.0f, 1.0f / 0.0f, 1.0f / 0.0f };
	float offset[3] { 0.0f, 0.0f, 0.0f };
	float shiftDir[3] { 0.0f, 0.0f, 1.0f };
	float gravity { 600 };
	float drag { 0.0f };
	unsigned bounceCount { 3 };
	float minSpeed { 300 };
	float maxSpeed { 300 };
	float minShiftSpeed { 0.0f };
	float maxShiftSpeed { 0.0f };
	float minPercentage { 0.0f };
	float maxPercentage { 1.0f };
	unsigned minTimeout { 300u };
	unsigned maxTimeout { 700u };
};

// Mutability of fields makes adjusting parameters in a loop more convenient
struct ConeFlockParams {
	float origin[3];
	float offset[3] { 0.0f, 0.0f, 0.0f };
	float dir[3] { 0.0f, 0.0f, 1.0f };
	float shiftDir[3] { 0.0f, 0.0f, 1.0f };
	float gravity { 600 };
	float drag { 0.0f };
	float angle { 45 };
	unsigned bounceCount { 3 };
	float minSpeed { 300 };
	float maxSpeed { 300 };
	float minShiftSpeed { 0.0f };
	float maxShiftSpeed { 0.0f };
	float minPercentage { 0.0f };
	float maxPercentage { 1.0f };
	unsigned minTimeout { 300u };
	unsigned maxTimeout { 700u };
};

[[nodiscard]]
auto fillParticleFlock( const UniformFlockParams *__restrict params,
				        Particle *__restrict particles,
				        unsigned maxParticles,
				        const Particle::AppearanceRules *__restrict appearanceRules,
				        wsw::RandomGenerator *__restrict rng,
				        int64_t currTime ) -> std::pair<int64_t, unsigned>;

[[nodiscard]]
auto fillParticleFlock( const ConeFlockParams *__restrict params,
				        Particle *__restrict particles,
				        unsigned maxParticles,
						const Particle::AppearanceRules *__restrict appearanceRules,
				  		wsw::RandomGenerator *__restrict rng,
						int64_t currTime ) -> std::pair<int64_t, unsigned>;

struct alignas( 16 ) ParticleFlock {
	Particle::AppearanceRules appearanceRules;
	// Caution: No drag simulation is currently performed for non-clipped flocks
	float drag { 0.0f };
	Particle *particles;
	int64_t timeoutAt;
	unsigned numParticlesLeft;
	unsigned binIndex;
	CMShapeList *shapeList;
	// TODO: Make links work with "m_"
	ParticleFlock *prev { nullptr }, *next { nullptr };
	float mins[4];
	float maxs[4];
	unsigned lastLitParticleIndex;
};

class ParticleSystem {
public:
	static constexpr unsigned kMaxClippedTrailFlockSize = 64;
	static constexpr unsigned kMaxNonClippedTrailFlockSize = 96;

	static constexpr unsigned kClippedTrailFlocksBin = 3;
	static constexpr unsigned kNonClippedTrailFlocksBin = 4;
private:
	template<typename> friend class SingletonHolder;

	// TODO: Just align manually in the combined memory chunk
	static_assert( sizeof( ParticleFlock ) % 16 == 0 );

	struct FlocksBin {
		ParticleFlock *head { nullptr };
		wsw::HeapBasedFreelistAllocator allocator;
		const unsigned maxParticlesPerFlock;
		bool needsShapeLists { true };

		FlocksBin( unsigned maxParticlesPerFlock, unsigned maxFlocks )
			: allocator( sizeof( ParticleFlock ) + sizeof( Particle ) * maxParticlesPerFlock, maxFlocks )
			, maxParticlesPerFlock( maxParticlesPerFlock ) {}
	};

	static constexpr unsigned kMaxSmallFlocks  = 128;
	static constexpr unsigned kMaxMediumFlocks = 64;
	static constexpr unsigned kMaxLargeFlocks  = 20;

	static constexpr unsigned kMaxClippedTrailFlocks    = 32;
	static constexpr unsigned kMaxNonClippedTrailFlocks = 16;

	static constexpr unsigned kMaxSmallFlockSize  = 8;
	static constexpr unsigned kMaxMediumFlockSize = 48;
	static constexpr unsigned kMaxLargeFlockSize  = 144;

	static constexpr unsigned kMaxNumberOfClippedFlocks = kMaxClippedTrailFlocks +
		kMaxSmallFlocks + kMaxMediumFlocks + kMaxLargeFlocks;

	wsw::StaticVector<CMShapeList *, kMaxNumberOfClippedFlocks> m_freeShapeLists;

	wsw::StaticVector<FlocksBin, 5> m_bins;
	int64_t m_lastTime { 0 };

	wsw::RandomGenerator m_rng;

	void unlinkAndFree( ParticleFlock *flock );

	[[nodiscard]]
	auto createFlock( unsigned binIndex, int64_t currTime ) -> ParticleFlock *;

	template <typename FlockParams>
	void addParticleFlockImpl( const Particle::AppearanceRules &appearanceRules,
							   const FlockParams &flockParams,
							   unsigned binIndex, unsigned maxParticles );

	static void runStepKinematics( ParticleFlock *__restrict flock, float deltaSeconds, vec3_t resultBounds[2] );

	static void simulate( ParticleFlock *__restrict flock, int64_t currTime, float deltaSeconds );
	static void simulateWithoutClipping( ParticleFlock *__restrict flock, int64_t currTime, float deltaSeconds );
public:
	ParticleSystem();
	~ParticleSystem();

	// Use this non-templated interface to reduce call site code bloat

	void addSmallParticleFlock( const Particle::AppearanceRules &rules, const UniformFlockParams &flockParams );
	void addSmallParticleFlock( const Particle::AppearanceRules &rules, const ConeFlockParams &flockParams );

	void addMediumParticleFlock( const Particle::AppearanceRules &rules, const UniformFlockParams &flockParams );
	void addMediumParticleFlock( const Particle::AppearanceRules &rules, const ConeFlockParams &flockParams );

	void addLargeParticleFlock( const Particle::AppearanceRules &rules, const UniformFlockParams &flockParams );
	void addLargeParticleFlock( const Particle::AppearanceRules &rules, const ConeFlockParams &flockParams );

	[[nodiscard]]
	auto createTrailFlock( const Particle::AppearanceRules &appearanceRules, unsigned binIndex ) -> ParticleFlock *;

	void destroyTrailFlock( ParticleFlock *flock ) { unlinkAndFree( flock ); }

	void runFrame( int64_t currTime, DrawSceneRequest *drawSceneRequest );

	void tryAddingLight( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest );
};

#endif