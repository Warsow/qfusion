#include "particlesystem.h"
#include <common/helpers/links.h>
#include <client/client.h>
#include <common/helpers/noise.h>
#include "cg_local.h"
#include <common/facilities/configvars.h>
#include <common/facilities/profilerscope.h>

using wsw::operator""_asView;

FloatConfigVar v_debugImpact("debugImpact"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );

struct PolyTrailOfParticles {
	int64_t spawnedAt { 0 };
	// Global for the entire associated flock
	int64_t detachedAt { 0 };
	PolyTrailOfParticles *prev { nullptr }, *next { nullptr };
	CurvedPolyTrailProps props;

	struct ParticleEntry {
		PolyEffectsSystem::CurvedBeamPoly poly;
		// For the related particle
		float lingeringFrac { 0.0f };
		// For the related particle
		int64_t detachedAt { 0 };
		// We waste 8 bytes on alignment anyway
		// TODO: Optimize the memory layout
		int64_t touchedAt { 0 };
		// TODO: Allow varying size of the buffer
		// TODO: Use a circular buffer
		wsw::StaticVector<Vec3, 32> points;
		wsw::StaticVector<int64_t, 32> timestamps;
	};

	ParticleEntry *particleEntries { nullptr };
	unsigned numEntries { 0 };
	unsigned binIndex { 0 };

	~PolyTrailOfParticles() {
		for( unsigned i = 0; i < numEntries; ++i ) {
			particleEntries[i].~ParticleEntry();
		}
	}
};

auto ParticleSystem::ParticleTrailBin::calcSizeOfFlockData( unsigned maxParticlesPerFlock ) -> SizeSpec {
	// TODO: Use MemSpecBuilder?
	unsigned sizeSoFar                  = sizeof( ParticleFlock );
	const unsigned flockParamsOffset  = sizeSoFar;
	assert( !( flockParamsOffset % alignof( ConicalFlockParams ) ) );
	sizeSoFar += sizeof( ConicalFlockParams );
	const unsigned updateParamsOffset = sizeSoFar;
	assert( !( updateParamsOffset % alignof( ParticleTrailUpdateParams ) ) );
	sizeSoFar += sizeof( ParticleTrailUpdateParams );
	const unsigned originsOffset = sizeSoFar;
	assert( !( originsOffset % alignof( vec3_t ) ) );
	sizeSoFar += sizeof( vec3_t ) * maxParticlesPerFlock;
	assert( sizeSoFar < std::numeric_limits<uint16_t>::max() );
	// TODO: Align using generic facilities
	if( const auto rem = sizeSoFar % 16 ) {
		sizeSoFar += 16 - rem;
	}
	const unsigned particleDataOffset = sizeSoFar;
	sizeSoFar += sizeof( Particle ) * maxParticlesPerFlock;
	return SizeSpec {
		.elementSize = sizeSoFar,
		.flockParamsOffset = (uint16_t)flockParamsOffset, .updateParamsOffset = (uint16_t)updateParamsOffset,
		.originsOffset = (uint16_t)originsOffset, .particleDataOffset = (uint16_t)particleDataOffset,
	};
}

ParticleSystem::RegularFlocksBin::RegularFlocksBin( unsigned maxParticlesPerFlock, unsigned maxFlocks )
	: allocator( sizeof( ParticleFlock ) + maxParticlesPerFlock * sizeof( Particle ), maxFlocks )
	, maxParticlesPerFlock( maxParticlesPerFlock ) {}

ParticleSystem::ParticleTrailBin::ParticleTrailBin( unsigned maxParticlesPerFlock, unsigned maxFlocks )
	: allocator( calcSizeOfFlockData( maxParticlesPerFlock ).elementSize, maxFlocks )
	, trailFlockParamsOffset( calcSizeOfFlockData( maxParticlesPerFlock ).flockParamsOffset )
	, trailUpdateParamsOffset( calcSizeOfFlockData( maxParticlesPerFlock ).updateParamsOffset )
	, originsDataOffset( calcSizeOfFlockData( maxParticlesPerFlock ).originsOffset )
	, particleDataOffset( calcSizeOfFlockData( maxParticlesPerFlock ).particleDataOffset )
	, maxParticlesPerFlock( maxParticlesPerFlock ) {}

ParticleSystem::PolyTrailBin::PolyTrailBin( unsigned maxParticlesPerFlock, unsigned maxFlocks )
	: allocator( sizeof( PolyTrailOfParticles ) + maxParticlesPerFlock * sizeof( PolyTrailOfParticles::ParticleEntry ), maxFlocks ) {}

ParticleSystem::ParticleSystem() {
	// TODO: All of this asks for exception-safety
	m_tmpShapeList = CM_AllocShapeList( cl.cms );
	if( !m_tmpShapeList ) [[unlikely]] {
		wsw::failWithBadAlloc();
	}

	constexpr std::pair<unsigned, unsigned> regularBinProps[5] {
		{ kMaxSmallFlockSize, kMaxSmallFlocks }, { kMaxMediumFlockSize, kMaxMediumFlocks },
		{ kMaxLargeFlockSize, kMaxLargeFlocks }, { kMaxClippedTrailFlockSize, kMaxClippedTrailFlocks },
		{ kMaxNonClippedTrailFlockSize, kMaxNonClippedTrailFlocks },
	};

	for( unsigned i = 0; i < 5; ++i ) {
		const auto [flockSize, maxFlocks] = regularBinProps[i];
		assert( flockSize <= std::numeric_limits<uint8_t>::max() );
		auto *const bin      = new( m_regularFlockBins.unsafe_grow_back() )RegularFlocksBin( flockSize, maxFlocks );
		bin->indexOfTrailBin = i < 3 ? i : ~0u;
		bin->needsClipping   = i < 4;
	}

	constexpr std::pair<unsigned, unsigned> trailsOfParticlesProps[3] {
		{ kMaxSmallTrailFlockSize, kMaxSmallFlocks }, { kMaxMediumTrailFlockSize, kMaxMediumFlocks },
		{ kMaxLargeTrailFlockSize, kMaxLargeFlocks },
	};

	for( const auto &[flockSize, maxFlocks] : trailsOfParticlesProps ) {
		assert( flockSize <= std::numeric_limits<uint8_t>::max() );
		// Allocate few extra slots for lingering trails
		new( m_trailsOfParticlesBins.unsafe_grow_back() )ParticleTrailBin( flockSize, maxFlocks + 8 );
		new( m_polyTrailBins.unsafe_grow_back() )PolyTrailBin( flockSize, maxFlocks + 8 );
	}
}

ParticleSystem::~ParticleSystem() {
	clear();

	CM_FreeShapeList( cl.cms, m_tmpShapeList );
}

void ParticleSystem::clear() {
	// Dependent trail bins get cleared automatically
	for( RegularFlocksBin &bin: m_regularFlockBins ) {
		for( ParticleFlock *flock = bin.head, *next; flock; flock = next ) { next = flock->next;
			unlinkAndFree( flock );
		}
		assert( !bin.head );
	}
	for( ParticleTrailBin &bin: m_trailsOfParticlesBins ) {
		for( ParticleFlock *flock = bin.lingeringFlocksHead, *next; flock; flock = next ) { next = flock->next;
			unlinkAndFree( flock );
		}
		assert( !bin.lingeringFlocksHead );
	}
	for( PolyTrailBin &bin: m_polyTrailBins ) {
		assert( !bin.activeTrailsHead );
		for( PolyTrailOfParticles *trail = bin.lingeringTrailsHead, *next; trail; trail = next ) { next = trail->next;
			unlinkAndFree( trail );
		}
		assert( !bin.lingeringTrailsHead );
	}
	m_lastTime = 0;
}

void ParticleSystem::unlinkAndFree( ParticleFlock *flock ) {
	if( flock->globalBinIndex < m_regularFlockBins.size() ) {
		assert( flock->groupBinIndex < m_regularFlockBins.size() );
		RegularFlocksBin &primaryBin = m_regularFlockBins[flock->groupBinIndex];
		wsw::unlink( flock, &primaryBin.head );
		if( ParticleFlock *trailFlock = flock->trailFlockOfParticles ) {
			assert( trailFlock->globalBinIndex >= m_regularFlockBins.size() );
			assert( trailFlock->groupBinIndex < m_trailsOfParticlesBins.size() );
			assert( trailFlock->groupBinIndex == primaryBin.indexOfTrailBin );
			ParticleTrailBin &trailBin = m_trailsOfParticlesBins[trailFlock->groupBinIndex];
			wsw::link( trailFlock, &trailBin.lingeringFlocksHead );
		}
		if( PolyTrailOfParticles *polyTrail = flock->polyTrailOfParticles ) {
			assert( polyTrail->binIndex == primaryBin.indexOfTrailBin );
			PolyTrailBin &trailBin = m_polyTrailBins[primaryBin.indexOfTrailBin];
			wsw::unlink( polyTrail, &trailBin.activeTrailsHead );
			wsw::link( polyTrail, &trailBin.lingeringTrailsHead );
			polyTrail->detachedAt = cg.time;
		}
		flock->~ParticleFlock();
		primaryBin.allocator.free( flock );
	} else {
		assert( !flock->trailFlockOfParticles );
		assert( flock->groupBinIndex < m_trailsOfParticlesBins.size() );
		ParticleTrailBin &trailBin = m_trailsOfParticlesBins[flock->groupBinIndex];
		wsw::unlink( flock, &trailBin.lingeringFlocksHead );
		flock->~ParticleFlock();
		trailBin.allocator.free( flock );
	}
}

void ParticleSystem::unlinkAndFree( PolyTrailOfParticles *trail ) {
	assert( trail->binIndex < m_polyTrailBins.size() );
	PolyTrailBin &trailBin = m_polyTrailBins[trail->binIndex];
	// Trails always become lingering prior to the unlinkAndFree() call
	wsw::unlink( trail, &trailBin.lingeringTrailsHead );
	trail->~PolyTrailOfParticles();
	trailBin.allocator.free( trail );
}

auto ParticleSystem::createFlock( unsigned regularBinIndex, const Particle::AppearanceRules &appearanceRules,
								  const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
								  const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) -> ParticleFlock * {
	assert( regularBinIndex < std::size( m_regularFlockBins ) );
	RegularFlocksBin &primaryBin = m_regularFlockBins[regularBinIndex];

	[[maybe_unused]]
	uint8_t *primaryMem = primaryBin.allocator.allocOrNull();
	if( !primaryMem ) [[unlikely]] {
		ParticleFlock *oldestFlock = nullptr;
		auto leastTimeout = std::numeric_limits<int64_t>::max();
		for( ParticleFlock *flock = primaryBin.head; flock; flock = flock->next ) {
			if( flock->timeoutAt < leastTimeout ) {
				leastTimeout = flock->timeoutAt;
				oldestFlock  = flock;
			}
		}
		assert( oldestFlock );
		wsw::unlink( oldestFlock, &primaryBin.head );
		oldestFlock->~ParticleFlock();
		primaryMem = (uint8_t *)oldestFlock;
	}

	assert( primaryMem );

	auto *const particles = (Particle *)( primaryMem + sizeof( ParticleFlock ) );
	assert( ( (uintptr_t)particles % 16 ) == 0 );

	auto *const primaryFlock = new( primaryMem )ParticleFlock {
		.appearanceRules           = appearanceRules,
		.particles                 = particles,
		.needsClipping             = primaryBin.needsClipping,
		.globalBinIndex            = (uint8_t)regularBinIndex,
		.groupBinIndex             = (uint8_t)regularBinIndex,
		.underlyingStorageCapacity = (uint8_t)primaryBin.maxParticlesPerFlock,
	};

	if( paramsOfParticleTrail ) {
		assert( primaryBin.indexOfTrailBin < m_trailsOfParticlesBins.size() );
		ParticleTrailBin &trailsBin = m_trailsOfParticlesBins[primaryBin.indexOfTrailBin];
		uint8_t *trailMem           = trailsBin.allocator.allocOrNull();
		if( !trailMem ) [[unlikely]] {
			ParticleFlock *oldestFlock = nullptr;
			auto leastTimeout          = std::numeric_limits<int64_t>::max();
			for( ParticleFlock *flock = trailsBin.lingeringFlocksHead; flock; flock = flock->next ) {
				if( flock->timeoutAt < leastTimeout ) {
					leastTimeout = flock->timeoutAt;
					oldestFlock  = flock;
				}
			}
			assert( oldestFlock );
			wsw::unlink( oldestFlock, &trailsBin.lingeringFlocksHead );
			oldestFlock->~ParticleFlock();
			trailMem = (uint8_t *)oldestFlock;
		}

		assert( !( (uintptr_t)trailMem % 16 ) );

		auto *const trailFlock = new( trailMem )ParticleFlock {
			.appearanceRules           = paramsOfParticleTrail->appearanceRules,
			.particles                 = (Particle *)( trailMem + trailsBin.particleDataOffset ),
			.needsClipping             = false,
			.globalBinIndex            = (uint8_t)( m_regularFlockBins.size() + primaryBin.indexOfTrailBin ),
			.groupBinIndex             = (uint8_t)primaryBin.indexOfTrailBin,
			.underlyingStorageCapacity = (uint8_t)trailsBin.maxParticlesPerFlock,
		};

		trailFlock->lastParticleTrailDropOrigins = (vec3_t *)( trailMem + trailsBin.originsDataOffset );
		trailFlock->flockParamsTemplate          = new( trailMem + trailsBin.trailFlockParamsOffset )
			ConicalFlockParams( paramsOfParticleTrail->flockParamsTemplate );
		trailFlock->particleTrailUpdateParams    = new( trailMem + trailsBin.trailUpdateParamsOffset )
			ParticleTrailUpdateParams( paramsOfParticleTrail->updateParams );

		primaryFlock->trailFlockOfParticles = trailFlock;
	}

	if( paramsOfPolyTrail ) {
		assert( primaryBin.indexOfTrailBin < m_polyTrailBins.size() );
		PolyTrailBin &trailBin = m_polyTrailBins[primaryBin.indexOfTrailBin];
		uint8_t *trailMem      = trailBin.allocator.allocOrNull();
		if( !trailMem ) [[unlikely]] {
			// TODO: Take percentage of remaining trails in account?
			PolyTrailOfParticles *oldestTrail = nullptr;
			auto oldestDetachTime             = std::numeric_limits<int64_t>::max();
			for( PolyTrailOfParticles *trail = trailBin.lingeringTrailsHead; trail; trail = trail->next ) {
				assert( trail->detachedAt > 0 && trail->detachedAt < std::numeric_limits<int64_t>::max() );
				if( trail->detachedAt < oldestDetachTime ) {
					oldestDetachTime = trail->detachedAt;
					oldestTrail      = trail;
				}
			}
			if( oldestTrail ) [[likely]] {
				wsw::unlink( oldestTrail, &trailBin.lingeringTrailsHead );
			} else {
				assert( !trailBin.lingeringTrailsHead );
				auto oldestSpawnTime = std::numeric_limits<int64_t>::max();
				for( PolyTrailOfParticles *trail = trailBin.activeTrailsHead; trail; trail = trail->next ) {
					assert( trail->spawnedAt < std::numeric_limits<int64_t>::max() );
					if( trail->spawnedAt < oldestSpawnTime ) {
						oldestSpawnTime = trail->spawnedAt;
						oldestTrail     = trail;
					}
				}
				assert( oldestTrail );
				wsw::unlink( oldestTrail, &trailBin.activeTrailsHead );
			}
			assert( oldestTrail );
			oldestTrail->~PolyTrailOfParticles();
			trailMem = (uint8_t *)oldestTrail;
		}

		assert( !( (uintptr_t)trailMem % 16 ) );
		auto *const polyTrail  = new( trailMem )PolyTrailOfParticles {
			.props    = paramsOfPolyTrail->props,
			.binIndex = primaryBin.indexOfTrailBin,
		};

		auto *const entriesMem = (uint8_t *)( polyTrail + 1 );
		assert( !( ( uintptr_t )entriesMem % 8 ) );
		polyTrail->particleEntries = (PolyTrailOfParticles::ParticleEntry *)entriesMem;
		assert( !polyTrail->numEntries );

		wsw::link( polyTrail, &trailBin.activeTrailsHead );
		primaryFlock->polyTrailOfParticles = polyTrail;
	}

	wsw::link( primaryFlock, &primaryBin.head );
	return primaryFlock;
}

template <typename FlockParams>
void ParticleSystem::addParticleFlockImpl( const Particle::AppearanceRules &appearanceRules,
										   const FlockParams &flockParams, unsigned binIndex, unsigned maxParticles,
										   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
										   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	ParticleFlock *const __restrict flock = createFlock( binIndex, appearanceRules, paramsOfParticleTrail, paramsOfPolyTrail );

	signed fillStride;
	unsigned initialOffset, activatedCountMultiplier, delayedCountMultiplier;
	if( flockParams.activationDelay.max == 0 ) {
		fillStride               = 1;
		initialOffset            = 0;
		activatedCountMultiplier = 1;
		delayedCountMultiplier   = 0;
	} else {
		fillStride               = -1;
		initialOffset            = maxParticles - 1;
		activatedCountMultiplier = 0;
		delayedCountMultiplier   = 1;
	}

	const FillFlockResult fillResult = fillParticleFlock( std::addressof( flockParams ),
														  flock->particles + initialOffset,
														  maxParticles, std::addressof( appearanceRules ),
														  &m_rng, cg.time, fillStride );

	flock->timeoutAt              = fillResult.resultTimeout;
	flock->numActivatedParticles  = fillResult.numParticles * activatedCountMultiplier;
	flock->numDelayedParticles    = fillResult.numParticles * delayedCountMultiplier;
	flock->delayedParticlesOffset = ( initialOffset + 1 ) - fillResult.numParticles * delayedCountMultiplier;
	setupFlockFieldsFromParams( flock, flockParams );

	[[maybe_unused]] vec3_t dropOriginInitializer;
	if( paramsOfPolyTrail || paramsOfParticleTrail ) {
		VectorAdd( flockParams.origin, flockParams.offset, dropOriginInitializer );
	}

	if( paramsOfParticleTrail ) {
		ParticleFlock *const __restrict trailFlock = flock->trailFlockOfParticles;
		assert( trailFlock && trailFlock->flockParamsTemplate && trailFlock->particleTrailUpdateParams );
		vec3_t *const __restrict dropOrigins = trailFlock->lastParticleTrailDropOrigins;
		for( unsigned i = 0; i < flock->numActivatedParticles; ++i ) {
			assert( flock->particles[i].originalIndex == i );
			VectorCopy( dropOriginInitializer, dropOrigins[i] );
		}
		assert( flock->numDelayedParticles <= maxParticles - 1 );
		for( unsigned i = 0; i < flock->numDelayedParticles; ++i ) {
			const Particle *__restrict p = &flock->particles[maxParticles - 1 - i];
			VectorCopy( dropOriginInitializer, dropOrigins[p->originalIndex] );
		}

		assert( !trailFlock->numActivatedParticles && !trailFlock->numDelayedParticles );
		trailFlock->timeoutAt = std::numeric_limits<int64_t>::max();
		setupFlockFieldsFromParams( trailFlock, *trailFlock->flockParamsTemplate );

		trailFlock->modulateByParentSize = paramsOfParticleTrail->modulateByParentSize;
	}

	if( paramsOfPolyTrail ) {
		PolyTrailOfParticles *const __restrict polyTrail = flock->polyTrailOfParticles;
		assert( !polyTrail->numEntries );

		assert( fillResult.numParticles == flock->numActivatedParticles + flock->numDelayedParticles );
		// Construct entry objects as needed (they are non-trivial)
		for( unsigned i = 0; i < fillResult.numParticles; ++i ) {
			auto *const entry    = new( &polyTrail->particleEntries[i] )PolyTrailOfParticles::ParticleEntry;
			// Set up properties that are kept the same during the lifetime
			entry->poly.width    = paramsOfPolyTrail->props.width;
			entry->poly.uvMode   = PolyEffectsSystem::UvModeFit {};
			entry->poly.material = paramsOfPolyTrail->material ? paramsOfPolyTrail->material : cgs.shaderWhite;
		}

		polyTrail->spawnedAt  = cg.time;
		polyTrail->numEntries = fillResult.numParticles;
	}
}

template <typename FlockParams>
void ParticleSystem::setupFlockFieldsFromParams( ParticleFlock *__restrict flock, const FlockParams &params ) {

	flock->drag                         = params.drag;
	flock->vorticityAngularSpeedRadians = DEG2RAD( params.vorticityAngularSpeed );
	VectorSet( flock->vorticityOrigin, params.origin[0], params.origin[1], params.origin[2] );
	VectorSet( flock->vorticityAxis, params.vorticityAxis[0], params.vorticityAxis[1], params.vorticityAxis[2] );
	flock->outflowSpeed                 = params.outflowSpeed;
	VectorSet( flock->outflowOrigin, params.origin[0], params.origin[1], params.origin[2] );
	VectorSet( flock->outflowAxis, params.outflowAxis[0], params.outflowAxis[1], params.outflowAxis[2] );
	flock->turbulenceSpeed              = params.turbulenceSpeed;
	flock->turbulenceCoordinateScale    = Q_Rcp( params.turbulenceScale );
	flock->restitution                  = params.restitution;
	flock->hasRotatingParticles         = params.angularVelocity.min != 0.0f || params.angularVelocity.max != 0.0f;
	flock->minBounceCount               = params.bounceCount.minInclusive;
	flock->maxBounceCount               = params.bounceCount.maxInclusive;
	flock->startBounceCounterDelay      = params.startBounceCounterDelay;

	if( flock->minBounceCount < flock->maxBounceCount ) {
		// Assume that probability of dropping the particle for varyingCount + 1 impacts is finalDropProbability
		// Hence that probability of not dropping it for varyingCount + 1 impacts is 1.0f - finalDropProbability
		// (We assume that almost all particles must be dropped the next step after the max one, hence the +1)
		// The probability of not dropping it every step is pow( finalKeepProbability, 1.0f / ( varyingCount + 1 ) )
		constexpr float finalDropProbability = 0.95f;
		constexpr float finalKeepProbability = 1.00f - finalDropProbability;
		const unsigned varyingCount          = flock->maxBounceCount - flock->minBounceCount;
		flock->keepOnImpactProbability       = std::pow( finalKeepProbability, Q_Rcp( (float)( varyingCount + 1 ) ) );
	}
}

void ParticleSystem::addSmallParticleFlock( const Particle::AppearanceRules &rules,
											const EllipsoidalFlockParams &flockParams,
											const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<EllipsoidalFlockParams>( rules, flockParams, 0, kMaxSmallFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

void ParticleSystem::addSmallParticleFlock( const Particle::AppearanceRules &rules,
											const ConicalFlockParams &flockParams,
											const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<ConicalFlockParams>( rules, flockParams, 0, kMaxSmallFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

void ParticleSystem::addMediumParticleFlock( const Particle::AppearanceRules &rules,
											 const EllipsoidalFlockParams &flockParams,
											 const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											 const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<EllipsoidalFlockParams>( rules, flockParams, 1, kMaxMediumFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

void ParticleSystem::addMediumParticleFlock( const Particle::AppearanceRules &rules,
											 const ConicalFlockParams &flockParams,
											 const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											 const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<ConicalFlockParams>( rules, flockParams, 1, kMaxMediumFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

void ParticleSystem::addLargeParticleFlock( const Particle::AppearanceRules &rules,
											const EllipsoidalFlockParams &flockParams,
											const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<EllipsoidalFlockParams>( rules, flockParams, 2, kMaxLargeFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

void ParticleSystem::addLargeParticleFlock( const Particle::AppearanceRules &rules,
											const ConicalFlockParams &flockParams,
											const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
											const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	addParticleFlockImpl<ConicalFlockParams>( rules, flockParams, 2, kMaxLargeFlockSize, paramsOfParticleTrail, paramsOfPolyTrail );
}

auto ParticleSystem::createTrailFlock( const Particle::AppearanceRules &rules,
									   const ConicalFlockParams &initialFlockParams,
									   unsigned binIndex ) -> ParticleFlock * {
	assert( binIndex == kClippedTrailFlocksBin || binIndex == kNonClippedTrailFlocksBin );
	// Don't let it evict anything TODO???
	ParticleFlock *flock = createFlock( binIndex, rules, nullptr );
	// Externally managed
	flock->timeoutAt = std::numeric_limits<int64_t>::max();
	// This is important for drag/turbulence/vorticity/outflow
	setupFlockFieldsFromParams( flock, initialFlockParams );
	assert( flock->numActivatedParticles + flock->numDelayedParticles == 0 );
	return flock;
}

auto fillParticleFlock( const EllipsoidalFlockParams *__restrict params,
						Particle *__restrict particles,
						unsigned maxParticles,
						const Particle::AppearanceRules *__restrict appearanceRules,
						wsw::RandomGenerator *__restrict rng,
						int64_t currTime, signed signedStride, float extraScale )
	-> FillFlockResult {
	const vec3_t initialOrigin {
		params->origin[0] + params->offset[0],
		params->origin[1] + params->offset[1],
		params->origin[2] + params->offset[2]
	};

	unsigned numParticles = maxParticles;
	// We do not specify the exact bounds but a percentage
	// so we can use the same filler for different bin flocks.
	if( params->percentage.min != 1.0f && params->percentage.max != 1.0f ) {
		assert( params->percentage.min >= 0.0f && params->percentage.min <= 1.0f );
		assert( params->percentage.max >= 0.0f && params->percentage.min <= 1.0f );
		const float percentage = rng->nextFloat( params->percentage.min, params->percentage.max );
		numParticles = (unsigned)( (float)maxParticles * percentage );
		numParticles = wsw::clamp( numParticles, 1u, maxParticles );
	}

	assert( params->speed.min >= 0.0f && params->speed.min <= 1000.0f );
	assert( params->speed.max >= 0.0f && params->speed.max <= 1000.0f );
	assert( params->speed.min <= params->speed.max );

	// Negative shift speed could be feasible
	assert( params->shiftSpeed.min <= params->shiftSpeed.max );
	assert( std::fabs( VectorLength( params->shiftDir ) - 1.0f ) < 1e-3f );

	assert( params->angularVelocity.min <= params->angularVelocity.max );

	assert( params->activationDelay.min <= params->activationDelay.max );
	assert( params->activationDelay.max <= 10'000 );

	assert( params->vorticityAngularSpeed == 0.0f || std::fabs( VectorLengthFast( params->vorticityAxis ) - 1.0f ) < 1e-3f );
	assert( std::fabs( params->vorticityAngularSpeed ) <= 10.0f * 360.0f );
	assert( params->outflowSpeed == 0.0f || std::fabs( VectorLengthFast( params->outflowAxis ) - 1.0f ) < 1e-3f );
	assert( std::fabs( params->outflowSpeed ) <= 1000.0f );
	assert( params->turbulenceSpeed >= 0.0f && params->turbulenceSpeed <= 1000.0f );
	assert( params->turbulenceScale >= 1.0f && params->turbulenceScale <= 1000.0f );

	const vec3_t *__restrict dirs = ::kPredefinedDirs;

	assert( params->timeout.min && params->timeout.min <= params->timeout.max && params->timeout.max < 3000 );
	const unsigned timeoutSpread = params->timeout.max - params->timeout.min;
	auto resultTimeout = std::numeric_limits<int64_t>::min();

	const bool hasMultipleMaterials       = appearanceRules->numMaterials > 1;
	const bool hasMultipleColors          = appearanceRules->colors.size() > 1;
	const bool hasSpeedShift              = params->shiftSpeed.min != 0.0f || params->shiftSpeed.max != 0.0f;
	const bool isSpherical                = params->stretchScale == 1.0f;
	const bool hasRandomInitialRotation   = params->randomInitialRotation.min != 0.0f || params->randomInitialRotation.max != 0.0f;
	const bool hasVariableAngularVelocity = params->angularVelocity.min < params->angularVelocity.max;
	const bool hasVariableDelay           = params->activationDelay.min < params->activationDelay.max;

	unsigned colorsIndexMask = 0, materialsIndexMask = 0;
	if( hasMultipleColors ) {
		if( const unsigned maybeMask = appearanceRules->colors.size() - 1; ( maybeMask & ( maybeMask + 1 ) ) == 0 ) {
			colorsIndexMask = maybeMask;
		}
	}
	if( hasMultipleMaterials ) {
		if( const unsigned maybeMask = appearanceRules->numMaterials - 1; ( maybeMask & ( maybeMask + 1 ) ) == 0 ) {
			materialsIndexMask = maybeMask;
		}
	}

	assert( std::fabs( VectorLength( params->stretchDir ) - 1.0f ) < 1e-3f );

	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceWidthExtraScale )>, uint8_t> );
	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceLengthExtraScale )>, uint8_t> );
	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceRadiusExtraScale )>, uint8_t> );
	const auto extraScaleAsByte = wsw::clamp<uint8_t>( (uint8_t)( extraScale * Particle::kUnitExtraScaleAsByte ), 0, 255 );

	for( unsigned i = 0; i < numParticles; ++i ) {
		Particle *const __restrict p = particles + signedStride * (signed)i;

		Vector4Set( p->oldOrigin, initialOrigin[0], initialOrigin[1], initialOrigin[2], 0.0f );
		Vector4Set( p->accel, 0, 0, -params->gravity, 0 );

		const float *__restrict randomDir = dirs[rng->nextBounded( NUMVERTEXNORMALS )];
		const float speed = rng->nextFloat( params->speed.min, params->speed.max );

		// We try relying on branch prediction facilities
		// TODO: Add template/if constexpr specializations
		if( isSpherical ) {
			VectorScale( randomDir, speed, p->dynamicsVelocity );
		} else {
			const float stretchScale = params->stretchScale;
			const float stretchDot   = DotProduct( randomDir, params->stretchDir );
			vec3_t alignedPart, perpPart;
			VectorScale( params->stretchDir, stretchDot, alignedPart );
			VectorSubtract( randomDir, alignedPart, perpPart );
			// Compute using the prior knowledge instead of a late normalization to break dependency chains
			const float newAlignedSquareLen = ( stretchDot * stretchScale ) * ( stretchDot * stretchScale );
			const float perpSquareLen       = VectorLengthSquared( perpPart );
			// TODO: Supply hints that the arg is non-zero
			const float rcpNewDirLen  = Q_RSqrt( newAlignedSquareLen + perpSquareLen );
			const float velocityScale = speed * rcpNewDirLen;
			// Combine the perpendicular part with the aligned part using the strech scale
			VectorMA( perpPart, stretchScale, alignedPart, p->dynamicsVelocity );
			// Normalize and scale by the speed
			VectorScale( p->dynamicsVelocity, velocityScale, p->dynamicsVelocity );
		}

		if( hasSpeedShift ) {
			const float shift = rng->nextFloat( params->shiftSpeed.min, params->shiftSpeed.max );
			VectorMA( p->dynamicsVelocity, shift, params->shiftDir, p->dynamicsVelocity );
		}

		p->dynamicsVelocity[3] = 0.0f;

		p->rotationAngle = 0.0f;
		if( hasRandomInitialRotation ) {
			p->rotationAngle += AngleNormalize360( rng->nextFloat( params->randomInitialRotation.min, params->randomInitialRotation.max ) );
		}
		if( hasVariableAngularVelocity ) {
			p->rotationAxisIndex = rng->nextBoundedFast( std::size( kPredefinedDirs ) );
			p->angularVelocity   = rng->nextFloat( params->angularVelocity.min, params->angularVelocity.max );
		} else {
			p->rotationAxisIndex = 0;
			p->angularVelocity   = params->angularVelocity.min;
		}

		p->spawnTime     = currTime;
		p->lifetime      = params->timeout.min + rng->nextBoundedFast( timeoutSpread );
		p->originalIndex = (uint8_t)i;
		p->bounceCount   = 0;

		p->activationDelay = params->activationDelay.min;
		if( hasVariableDelay ) [[unlikely]] {
			p->activationDelay += rng->nextBoundedFast( params->activationDelay.max - params->activationDelay.min );
		}

		// TODO: Branchless?
		resultTimeout = wsw::max( p->spawnTime + p->lifetime, resultTimeout );

		const uint32_t randomDword = rng->next();
		p->instanceWidthSpreadFraction   = (int8_t)( ( randomDword >> 0 ) & 0xFF );
		p->instanceLengthSpreadFraction  = (int8_t)( ( randomDword >> 8 ) & 0xFF );
		p->instanceRadiusSpreadFraction  = (int8_t)( ( randomDword >> 16 ) & 0xFF );

		p->instanceWidthExtraScale = p->instanceLengthExtraScale = p->instanceRadiusExtraScale = extraScaleAsByte;

		if( hasMultipleMaterials ) {
			if( materialsIndexMask ) {
				p->instanceMaterialIndex = rng->next() & materialsIndexMask;
			} else {
				p->instanceMaterialIndex = (uint8_t)rng->nextBounded( appearanceRules->numMaterials );
			}
		} else {
			p->instanceMaterialIndex = 0;
		}
		if( hasMultipleColors ) {
			if( colorsIndexMask ) {
				p->instanceColorIndex = rng->next() & colorsIndexMask;
			} else {
				p->instanceColorIndex = (uint8_t)rng->nextBounded( appearanceRules->colors.size() );
			}
		} else {
			p->instanceColorIndex = 0;
		}
	}

	return FillFlockResult { .resultTimeout = resultTimeout, .numParticles = numParticles };
}

auto fillParticleFlock( const ConicalFlockParams *__restrict params,
						Particle *__restrict particles,
						unsigned maxParticles,
						const Particle::AppearanceRules *__restrict appearanceRules,
						wsw::RandomGenerator *__restrict rng,
						int64_t currTime, signed signedStride, float extraScale )
	-> FillFlockResult {
	const vec3_t initialOrigin {
		params->origin[0] + params->offset[0],
		params->origin[1] + params->offset[1],
		params->origin[2] + params->offset[2]
	};

	unsigned numParticles = maxParticles;
	if( params->percentage.min != 1.0f && params->percentage.max != 1.0f ) {
		assert( params->percentage.min >= 0.0f && params->percentage.min <= 1.0f );
		assert( params->percentage.max >= 0.0f && params->percentage.min <= 1.0f );
		// We do not specify the exact bounds but a percentage
		// so we can use the same filler for different bin flocks.
		const float percentage = rng->nextFloat( params->percentage.min, params->percentage.max );
		numParticles = (unsigned)( (float)maxParticles * percentage );
		numParticles = wsw::clamp( numParticles, 1u, maxParticles );
	}

	assert( params->speed.min >= 0.0f && params->speed.min <= 1000.0f );
	assert( params->speed.max >= 0.0f && params->speed.max <= 1000.0f );
	assert( params->speed.min <= params->speed.max );
	assert( std::fabs( VectorLength( params->dir ) - 1.0f ) < 1e-3f );

	// Negative shift speed could be feasible
	assert( params->shiftSpeed.min <= params->shiftSpeed.max );
	assert( std::fabs( VectorLength( params->shiftDir ) - 1.0f ) < 1e-3f );

	assert( params->angle >= 0.0f && params->angle <= 180.0f );
	assert( params->innerAngle >= 0.0f && params->innerAngle <= 180.0f );
	assert( params->innerAngle < params->angle );

	assert( params->angularVelocity.min <= params->angularVelocity.max );

	assert( params->activationDelay.min <= params->activationDelay.max );
	assert( params->activationDelay.max <= 10'000 );

	assert( params->vorticityAngularSpeed == 0.0f || std::fabs( VectorLengthFast( params->vorticityAxis ) - 1.0f ) < 1e-3f );
	assert( std::fabs( params->vorticityAngularSpeed ) <= 10.0f * 360.0f );
	assert( params->outflowSpeed == 0.0f || std::fabs( VectorLengthFast( params->outflowAxis ) - 1.0f ) < 1e-3f );
	assert( std::fabs( params->outflowSpeed ) <= 1000.0f );
	assert( params->turbulenceSpeed >= 0.0f && params->turbulenceSpeed <= 1000.0f );
	assert( params->turbulenceScale >= 1.0f && params->turbulenceScale <= 1000.0f );

	// TODO: Supply cosine values as parameters?

	float maxZ = 1.0f;
	if( params->innerAngle > 0.0f ) [[unlikely]] {
		maxZ = std::cos( (float) DEG2RAD( params->innerAngle ) );
	}

	const float minZ = std::cos( (float)DEG2RAD( params->angle ) );

	assert( params->timeout.min && params->timeout.min <= params->timeout.max && params->timeout.max < 3000 );
	const unsigned timeoutSpread = params->timeout.max - params->timeout.min;
	auto resultTimeout = std::numeric_limits<int64_t>::min();

	mat3_t transformMatrix;
	Matrix3_ForRotationOfDirs( &axis_identity[AXIS_UP], params->dir, transformMatrix );

	const bool hasMultipleMaterials       = appearanceRules->numMaterials > 1;
	const bool hasMultipleColors          = appearanceRules->colors.size() > 1;
	const bool hasSpeedShift              = params->shiftSpeed.min != 0.0f || params->shiftSpeed.max != 0.0f;
	const bool hasRandomInitialRotation   = params->randomInitialRotation.min != 0.0f || params->randomInitialRotation.max != 0.0f;
	const bool hasVariableAngularVelocity = params->angularVelocity.min < params->angularVelocity.max;
	const bool hasVariableDelay           = params->activationDelay.min != params->activationDelay.max;

	unsigned colorsIndexMask = 0, materialsIndexMask = 0;
	if( hasMultipleColors ) {
		if( const unsigned maybeMask = appearanceRules->colors.size() - 1; ( maybeMask & ( maybeMask + 1 ) ) == 0 ) {
			colorsIndexMask = maybeMask;
		}
	}
	if( hasMultipleMaterials ) {
		if( const unsigned maybeMask = appearanceRules->numMaterials - 1; ( maybeMask & ( maybeMask + 1 ) ) == 0 ) {
			materialsIndexMask = maybeMask;
		}
	}

	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceWidthExtraScale )>, uint8_t> );
	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceLengthExtraScale )>, uint8_t> );
	static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceRadiusExtraScale )>, uint8_t> );
	const auto extraScaleAsByte = wsw::clamp<uint8_t>( (uint8_t)( extraScale * Particle::kUnitExtraScaleAsByte ), 0, 255 );

	// TODO: Make cached conical samples for various angles?
	for( unsigned i = 0; i < numParticles; ++i ) {
		Particle *const __restrict p = particles + signedStride * (signed)i;

		Vector4Set( p->oldOrigin, initialOrigin[0], initialOrigin[1], initialOrigin[2], 0.0f );
		Vector4Set( p->accel, 0, 0, -params->gravity, 0 );

		// https://math.stackexchange.com/a/205589
		const float z   = rng->nextFloat( minZ, maxZ );
		const float r   = Q_Sqrt( 1.0f - z * z );
		const float phi = rng->nextFloat( 0.0f, 2.0f * (float)M_PI );

		const float speed = rng->nextFloat( params->speed.min, params->speed.max );
		const vec3_t untransformed { speed * r * std::cos( phi ), speed * r * std::sin( phi ), speed * z };
		Matrix3_TransformVector( transformMatrix, untransformed, p->dynamicsVelocity );

		// We try relying on branch prediction facilities
		// TODO: Add template/if constexpr specializations
		if( hasSpeedShift ) {
			const float shift = rng->nextFloat( params->shiftSpeed.min, params->shiftSpeed.max );
			VectorMA( p->dynamicsVelocity, shift, params->shiftDir, p->dynamicsVelocity );
		}

		p->dynamicsVelocity[3] = 0.0f;

		p->rotationAngle = 0.0f;
		if( hasRandomInitialRotation ) {
			p->rotationAngle += rng->nextFloat( params->randomInitialRotation.min, params->randomInitialRotation.max );
		}
		if( hasVariableAngularVelocity ) {
			p->rotationAxisIndex = rng->nextBoundedFast( std::size( kPredefinedDirs ) );
			p->angularVelocity   = rng->nextFloat( params->angularVelocity.min, params->angularVelocity.max );
		} else {
			p->rotationAxisIndex = 0;
			p->angularVelocity   = params->angularVelocity.min;
		}

		p->spawnTime     = currTime;
		p->lifetime      = params->timeout.min + rng->nextBoundedFast( timeoutSpread );
		p->originalIndex = (uint8_t)i;
		p->bounceCount   = 0;

		p->activationDelay = params->activationDelay.min;
		if( hasVariableDelay ) [[unlikely]] {
			p->activationDelay += rng->nextBoundedFast( params->activationDelay.max - params->activationDelay.min );
		}

		// TODO: Branchless?
		resultTimeout = wsw::max( p->spawnTime + p->lifetime, resultTimeout );

		const uint32_t randomDword = rng->next();
		p->instanceWidthSpreadFraction   = (int8_t)( ( randomDword >> 0 ) & 0xFF );
		p->instanceLengthSpreadFraction  = (int8_t)( ( randomDword >> 8 ) & 0xFF );
		p->instanceRadiusSpreadFraction  = (int8_t)( ( randomDword >> 16 ) & 0xFF );

		p->instanceWidthExtraScale = p->instanceLengthExtraScale = p->instanceRadiusExtraScale = extraScaleAsByte;

		if( hasMultipleMaterials ) {
			if( materialsIndexMask ) {
				p->instanceMaterialIndex = rng->next() & materialsIndexMask;
			} else {
				p->instanceMaterialIndex = (uint8_t)rng->nextBounded( appearanceRules->numMaterials );
			}
		} else {
			p->instanceMaterialIndex = 0;
		}
		if( hasMultipleColors ) {
			if( colorsIndexMask ) {
				p->instanceColorIndex = rng->next() & colorsIndexMask;
			} else {
				p->instanceColorIndex = (uint8_t)rng->nextBounded( appearanceRules->colors.size() );
			}
		} else {
			p->instanceColorIndex = 0;
		}
	}

	return FillFlockResult { .resultTimeout = resultTimeout, .numParticles = numParticles };
}

void updateParticleTrail( ParticleFlock *__restrict flock,
						  ConicalFlockParams *__restrict flockParamsTemplate,
						  const float *__restrict actualOrigin,
						  float *__restrict lastDropOrigin,
						  wsw::RandomGenerator *__restrict rng,
						  int64_t currTime,
						  const ParticleTrailUpdateParams &__restrict updateParams ) {
	const unsigned maxParticlesInFlock = flock->underlyingStorageCapacity;
	if( flock->numActivatedParticles + flock->numDelayedParticles < maxParticlesInFlock ) {
		const float squareDistance = DistanceSquared( lastDropOrigin, actualOrigin );
		if( squareDistance >= wsw::square( updateParams.dropDistance ) ) {
			vec3_t dir, stepVec;
			VectorSubtract( lastDropOrigin, actualOrigin, dir );

			const float rcpDistance = Q_RSqrt( squareDistance );
			const float distance    = squareDistance * rcpDistance;
			// The dir is directed towards the old position
			VectorScale( dir, rcpDistance, dir );
			// Make steps of dropDistance units towards the new position
			VectorScale( dir, -updateParams.dropDistance, stepVec );

			VectorCopy( lastDropOrigin, flockParamsTemplate->origin );
			VectorCopy( dir, flockParamsTemplate->dir );

			assert( ( flockParamsTemplate->vorticityAngularSpeed != 0.0f ) == ( flock->vorticityAngularSpeedRadians != 0.0f ) );
			if( flockParamsTemplate->vorticityAngularSpeed != 0.0f ) {
				// Note: The template vorticity axis is unused, update the flock directly
				VectorCopy( lastDropOrigin, flock->vorticityOrigin );
				VectorCopy( dir, flock->vorticityAxis );
			}
			assert( flockParamsTemplate->outflowSpeed == flock->outflowSpeed );
			if( flockParamsTemplate->outflowSpeed != 0.0f ) {
				// Note: The template outflow axis is unused, update the flock directly
				VectorCopy( lastDropOrigin, flock->outflowOrigin );
				VectorCopy( dir, flock->outflowAxis );
			}

			unsigned stepNum    = 0;
			const auto numSteps = (unsigned)wsw::max( 1.0f, distance * Q_Rcp( updateParams.dropDistance ) );
			do {
				const unsigned numParticlesSoFar = flock->numActivatedParticles + flock->numDelayedParticles;
				if( numParticlesSoFar >= flock->underlyingStorageCapacity ) [[unlikely]] {
					break;
				}

				signed fillStride;
				unsigned initialOffset;
				if( flockParamsTemplate->activationDelay.max == 0 ) {
					// Delayed particles must not be spawned in this case
					assert( !flock->numDelayedParticles );
					fillStride    = +1;
					initialOffset = flock->numActivatedParticles;
				} else {
					fillStride = -1;
					if( flock->delayedParticlesOffset ) {
						initialOffset = flock->delayedParticlesOffset - 1;
					} else {
						initialOffset = maxParticlesInFlock - 1;
					}
				}

				const FillFlockResult fillResult = fillParticleFlock( flockParamsTemplate,
																	  flock->particles + initialOffset,
																	  updateParams.maxParticlesPerDrop,
																	  std::addressof( flock->appearanceRules ),
																	  rng, currTime, fillStride,
																	  updateParams.particleSizeMultiplier );
				assert( fillResult.numParticles && fillResult.numParticles <= updateParams.maxParticlesPerDrop );

				if( flockParamsTemplate->activationDelay.max == 0 ) {
					flock->numActivatedParticles += fillResult.numParticles;
				} else {
					flock->numDelayedParticles += fillResult.numParticles;
					if( flock->delayedParticlesOffset ) {
						assert( flock->delayedParticlesOffset >= fillResult.numParticles );
						flock->delayedParticlesOffset -= fillResult.numParticles;
					} else {
						assert( maxParticlesInFlock >= fillResult.numParticles );
						flock->delayedParticlesOffset = maxParticlesInFlock - fillResult.numParticles;
					}
					assert( flock->delayedParticlesOffset + flock->numDelayedParticles <= maxParticlesInFlock );
					assert( flock->numActivatedParticles <= flock->delayedParticlesOffset );
				}

				assert( flock->numDelayedParticles + flock->numActivatedParticles <= maxParticlesInFlock );

				VectorAdd( flockParamsTemplate->origin, stepVec, flockParamsTemplate->origin );
			} while( ++stepNum < numSteps );

			VectorCopy( flockParamsTemplate->origin, lastDropOrigin );
		}
	}
}

[[nodiscard]]
static inline bool canShowForCurrentCgFrame( unsigned affinityIndex, unsigned affinityModulo ) {
	if( affinityModulo > 1 ) {
		assert( affinityIndex < affinityModulo );
		const auto moduloAsCountType = ( decltype( cg.frameCount ) )affinityModulo;
		const auto indexAsCountType  = ( decltype( cg.frameCount ) )affinityIndex;
		return indexAsCountType == cg.frameCount % moduloAsCountType;
	}
	return true;
}

void ParticleSystem::simulateFrame( int64_t currTime ) {
	WSW_PROFILER_SCOPE();

	if( currTime != m_lastTime ) {
		assert( currTime > m_lastTime );

		// Limit delta by sane bounds
		const float deltaSeconds = 1e-3f * (float)wsw::clamp( (int)( currTime - m_lastTime ), 1, 33 );

		// We split simulation/rendering loops for a better instructions cache utilization
		for( RegularFlocksBin &bin: m_regularFlockBins ) {
			for( ParticleFlock *flock = bin.head, *next; flock; flock = next ) { next = flock->next;
				if( currTime < flock->timeoutAt ) [[likely]] {
					// Otherwise, the flock could be awaiting filling externally, don't modify its timeout
					if( flock->numActivatedParticles + flock->numDelayedParticles > 0 ) [[likely]] {
						if( flock->needsClipping ) {
							simulate( flock, &m_rng, currTime, deltaSeconds );
						} else {
							simulateWithoutClipping( flock, currTime, deltaSeconds );
						}
					}
					if( flock->trailFlockOfParticles ) {
						simulateParticleTrailOfParticles( flock, &m_rng, currTime, deltaSeconds );
					}
					if( flock->polyTrailOfParticles ) {
						simulatePolyTrailOfParticles( flock, flock->polyTrailOfParticles, currTime );
					}
				} else {
					unlinkAndFree( flock );
				}
			}
		}

		for( ParticleTrailBin &bin: m_trailsOfParticlesBins ) {
			for( ParticleFlock *flock = bin.lingeringFlocksHead, *next; flock; flock = next ) { next = flock->next;
				if( currTime < flock->timeoutAt ) [[likely]] {
					if( flock->numActivatedParticles + flock->numDelayedParticles ) {
						simulateWithoutClipping( flock, currTime, deltaSeconds );
					} else {
						unlinkAndFree( flock );
					}
				} else {
					unlinkAndFree( flock );
				}
			}
		}

		for( PolyTrailBin &bin: m_polyTrailBins ) {
			for( PolyTrailOfParticles *trail = bin.lingeringTrailsHead, *next; trail; trail = next ) { next = trail->next;
				// Lingering of trails of individual particles is independent,
				// and could even happen prior to detaching a trail,
				// but if we reach this condition, we are sure all individual trails have finished lingering.
				assert( trail->props.lingeringLimit > 0 && trail->props.lingeringLimit < 1000 );
				if( currTime < trail->detachedAt + trail->props.lingeringLimit ) {
					simulatePolyTrailOfParticles( nullptr, trail, currTime );
				} else {
					unlinkAndFree( trail );
				}
			}
		}

		m_lastTime = currTime;
	}

	m_frameFlareParticles.clear();
	m_frameFlareColorLifespans.clear();
	m_frameFlareAppearanceRules.clear();
}

void ParticleSystem::submitToScene( [[maybe_unused]] int64_t currTime, DrawSceneRequest *request ) {
	assert( currTime == m_lastTime );

	for( RegularFlocksBin &bin: m_regularFlockBins ) {
		for( ParticleFlock *flock = bin.head; flock; flock = flock->next ) {
			if( flock->numActivatedParticles ) [[likely]] {
				submitFlock( flock, request );
			}
			if( ParticleFlock *const trailFlock = flock->trailFlockOfParticles ) [[unlikely]] {
				if( trailFlock->numActivatedParticles ) [[likely]] {
					submitFlock( trailFlock, request );
				}
			}
		}
	}

	for( ParticleTrailBin &bin: m_trailsOfParticlesBins ) {
		for( ParticleFlock *flock = bin.lingeringFlocksHead; flock; flock = flock->next ) {
			if( flock->numActivatedParticles ) [[likely]] {
				submitFlock( flock, request );
			}
		}
	}

	for( PolyTrailBin &bin: m_polyTrailBins ) {
		for( PolyTrailOfParticles *listHead : { bin.activeTrailsHead, bin.lingeringTrailsHead } ) {
			for( PolyTrailOfParticles *trail = listHead; trail; trail = trail->next ) {
				submitPolyTrail( trail, request );
			}
		}
	}
}

void ParticleSystem::submitFlock( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest ) {
	assert( flock->numActivatedParticles > 0 );
	drawSceneRequest->addParticles( flock->mins, flock->maxs, flock->appearanceRules, flock->particles, flock->numActivatedParticles );
	const Particle::AppearanceRules &rules = flock->appearanceRules;
	if( !rules.lightProps.empty() ) [[unlikely]] {
		// If the light display is tied to certain frames (e.g., every 3rd one, starting from 2nd absolute)
		if( canShowForCurrentCgFrame( rules.lightFrameAffinityIndex, rules.lightFrameAffinityModulo ) ) {
			tryAddingLight( flock, drawSceneRequest );
		}
	}
	if( rules.flareProps ) [[unlikely]] {
		const Particle::FlareProps &props = *rules.flareProps;
		if( canShowForCurrentCgFrame( props.flockFrameAffinityIndex, props.flockFrameAffinityModulo ) ) {
			tryAddingFlares( flock, drawSceneRequest );
		}
	}
}

void ParticleSystem::tryAddingLight( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest ) {
	const Particle::AppearanceRules &rules = flock->appearanceRules;
	assert( flock->numActivatedParticles );
	assert( rules.lightProps.size() == 1 || rules.lightProps.size() == rules.colors.size() );

	flock->lastLightEmitterParticleIndex = ( flock->lastLightEmitterParticleIndex + 1 ) % flock->numActivatedParticles;
	const Particle &particle = flock->particles[flock->lastLightEmitterParticleIndex];
	assert( particle.lifetimeFrac >= 0.0f && particle.lifetimeFrac <= 1.0f );

	const LightLifespan *lightLifespan;
	if( rules.lightProps.size() == 1 ) {
		lightLifespan = rules.lightProps.data();
	} else {
		lightLifespan = rules.lightProps.data() + particle.instanceColorIndex;
	}

	float lightRadius, lightColor[3];
	lightLifespan->getRadiusAndColorForLifetimeFrac( particle.lifetimeFrac, &lightRadius, lightColor );
	if( lightRadius >= 1.0f ) {
		drawSceneRequest->addLight( particle.origin, lightRadius, 0.0f, lightColor );
	}
}

void ParticleSystem::tryAddingFlares( ParticleFlock *flock, DrawSceneRequest *drawSceneRequest ) {
	assert( m_frameFlareParticles.size() == m_frameFlareColorLifespans.size() );

	if( m_frameFlareParticles.full() ) [[unlikely]] {
		return;
	}
	if( m_frameFlareAppearanceRules.full() ) [[unlikely]] {
		return;
	}

	assert( flock->numActivatedParticles );
	assert( flock->appearanceRules.flareProps );
	const Particle::FlareProps &flareProps = *flock->appearanceRules.flareProps;
	assert( flareProps.lightProps.size() == 1 || flareProps.lightProps.size() == flock->appearanceRules.colors.size() );

	const unsigned oldNumFrameFlareParticles = m_frameFlareParticles.size();

	unsigned frameCountByModulo = 0;
	if( unsigned modulo = flareProps.particleFrameAffinityModulo; modulo > 1 ) {
		frameCountByModulo = (unsigned)( cg.frameCount % (decltype( cg.frameCount ) )modulo );
	}

	BoundsBuilder boundsBuilder;
	unsigned numAddedParticles  = 0;
	unsigned flockParticleIndex = 0;

	do {
		if( flareProps.particleFrameAffinityModulo > 1 ) {
			if( ( flockParticleIndex % flareProps.particleFrameAffinityModulo ) != frameCountByModulo ) {
				continue;
			}
		}

		const Particle &baseParticle = flock->particles[flockParticleIndex];
		assert( baseParticle.lifetimeFrac >= 0.0f && baseParticle.lifetimeFrac <= 1.0f );

		// TODO: Lift this condition out of the loop?
		const LightLifespan *lightLifespan;
		if( flareProps.lightProps.size() == 1 ) {
			lightLifespan = flareProps.lightProps.data();
		} else {
			lightLifespan = flareProps.lightProps.data() + baseParticle.instanceColorIndex;
		}

		float lightRadius, lightColor[3];
		lightLifespan->getRadiusAndColorForLifetimeFrac( baseParticle.lifetimeFrac, &lightRadius, lightColor );

		assert( flareProps.radiusScale > 0.0f );
		lightRadius *= flareProps.radiusScale;

		static_assert( std::is_same_v<std::remove_cvref_t<decltype( Particle::instanceRadiusExtraScale )>, uint8_t> );
		const auto lightRadiusAsByte = wsw::clamp<uint8_t>( (uint8_t)( lightRadius * Particle::kUnitExtraScaleAsByte ), 0, 255 );
		if( lightRadiusAsByte > 0 ) {
			auto *const addedParticle = new( m_frameFlareParticles.unsafe_grow_back() )Particle( baseParticle );

			addedParticle->lifetimeFrac          = 0.0f;
			addedParticle->instanceColorIndex    = numAddedParticles;
			addedParticle->instanceMaterialIndex = 0;

			// Keep radius in the appearance rules the same (the thing we have to do), modify instance radius scale
			addedParticle->instanceRadiusExtraScale = lightRadiusAsByte;

			// TODO: This kind of sucks, can't we just supply inline colors?
			m_frameFlareColorLifespans.push_back( RgbaLifespan {
				.initial = { lightColor[0], lightColor[1], lightColor[2], flareProps.alphaScale },
			});

			// TODO: Load 4 components explicitly
			boundsBuilder.addPoint( addedParticle->origin );
			numAddedParticles++;

			if( m_frameFlareParticles.full() ) [[unlikely]] {
				break;
			}
		}
	} while( ++flockParticleIndex < flock->numActivatedParticles );

	assert( m_frameFlareParticles.size() == m_frameFlareColorLifespans.size() );

	if( numAddedParticles ) {
		const Particle *const addedParticles  = m_frameFlareParticles.data() + oldNumFrameFlareParticles;
		const RgbaLifespan *const addedColors = m_frameFlareColorLifespans.data() + oldNumFrameFlareParticles;
		m_frameFlareAppearanceRules.emplace_back( Particle::AppearanceRules {
			.materials     = cgs.media.shaderParticleFlare.getAddressOfHandle(),
			.colors        = { addedColors, numAddedParticles },
			.geometryRules = Particle::SpriteRules { .radius = { .mean = 1.0f } },
		});

		vec4_t mins, maxs;
		boundsBuilder.storeTo( mins, maxs );
		mins[3] = 0.0f, maxs[3] = 1.0f;

		drawSceneRequest->addParticles( mins, maxs, m_frameFlareAppearanceRules.back(), addedParticles, numAddedParticles );
	}
}

void ParticleSystem::submitPolyTrail( PolyTrailOfParticles *__restrict trail, DrawSceneRequest *__restrict request ) {
	for( unsigned i = 0; i < trail->numEntries; ++i ) {
		PolyTrailOfParticles::ParticleEntry &__restrict entry = trail->particleEntries[i];
		assert( entry.lingeringFrac >= 0.0f && entry.lingeringFrac <= 1.0f );
		if( entry.poly.points.size() > 1 && entry.lingeringFrac < 1.0f ) {
			VectorCopy( trail->props.fromColor, entry.poly.fromColor );
			VectorCopy( trail->props.toColor, entry.poly.toColor );
			const float alphaFrac   = 1.0f - entry.lingeringFrac;
			entry.poly.fromColor[3] = alphaFrac * trail->props.fromColor[3];
			entry.poly.toColor[3]   = alphaFrac * trail->props.toColor[3];
			request->addDynamicMesh( &entry.poly );
		}
	}
}

void ParticleSystem::runStepKinematics( ParticleFlock *__restrict flock, float deltaSeconds, vec3_t resultBounds[2] ) {
	assert( flock->numActivatedParticles );

	BoundsBuilder boundsBuilder;

	for( unsigned i = 0; i < flock->numActivatedParticles; ++i ) {
		Particle *const __restrict particle = flock->particles + i;

		vec4_t accel;
		Vector4Copy( particle->accel, accel );
        Vector4Set( particle->artificialVelocity, 0.0f, 0.0f, 0.0f, 0.0f );

		if( flock->drag > 0.0f ) {
			if( const float speed = VectorLengthFast( particle->dynamicsVelocity ); speed > 1.0f ) [[likely]] {
				const float drag = flock->drag * speed;
				VectorMA( accel, -drag, particle->dynamicsVelocity, accel );
			}
		}

		// Negative values lead to rotation in the opposite direction
		if( flock->vorticityAngularSpeedRadians != 0.0f ) {
			// it is important the axis is a unit vector
			assert( std::fabs( VectorLengthFast( flock->vorticityAxis ) - 1.0f ) < 1e-1f );

			/*
			to create a rotating motion around the axis, we express the coordinate of the particle into 3 new orthogonal
			base vectors: 1) the axis  2) vector from particle to axis r 3) the crossproduct of (1) and (2) n. Then the
			particle position after the timestep can be calculated as:
			projection of particle on the axis + cos(strength/radius * deltaT) * r + sin(strength/radius * deltaT) * n
			using a third order taylor series approximation, when k=strength/radius:
			projection of particle on the axis + (1-(k*deltaT)^2) * r + (k*deltaT-(k*deltaT)^3/6) * n
			where the projection plus the radius is simply the initial position, thus the change in position is:
			- (k*deltaT)^2 * r + (k*deltaT-(k*deltaT)^3/6) * n
			then the velocity is change in position over time (deltaT) thus it is:
			- k^2*deltaT * r + (k - k^3*deltaT^2/6) * n
			*/

			vec3_t offsetFromVorticityOrigin;
			VectorSubtract( particle->origin, flock->vorticityOrigin, offsetFromVorticityOrigin );
			// compute the base vectors:
			vec3_t inwardBaseVector; // the vector from the particle to the vorticity axis
			const float offsetAlongAxis = DotProduct( offsetFromVorticityOrigin, flock->vorticityAxis );
			VectorMA( offsetFromVorticityOrigin, -offsetAlongAxis, flock->vorticityAxis, inwardBaseVector );

			vec3_t outwardBaseVector;
			CrossProduct( offsetFromVorticityOrigin, flock->vorticityAxis, outwardBaseVector );

			// get the radius from the vorticity origin and compute strength/radius
			const float squaredRadiusToVorticityOrigin = VectorLengthSquared( offsetFromVorticityOrigin );
			const float rcpRadiusToVorticityOrigin     = Q_RSqrt( squaredRadiusToVorticityOrigin );
			const float effectiveStrength              = flock->vorticityAngularSpeedRadians * rcpRadiusToVorticityOrigin;

			// compute the induced velocity with the taylor series approximation
			vec3_t inducedVelocity = { 0.0f, 0.0f, 0.0f };
			const float higherOrderTerm = effectiveStrength * deltaSeconds;

			const float inwardComponent = -0.5f * effectiveStrength * higherOrderTerm;
			VectorMA( inducedVelocity, inwardComponent, inwardBaseVector, inducedVelocity );

			constexpr float oneSixth = 1.0f / 6.0f;
			const float outwardComponent = effectiveStrength * ( 1.0f - higherOrderTerm * higherOrderTerm * oneSixth );
			VectorMA( inducedVelocity, outwardComponent, outwardBaseVector, inducedVelocity );

			// in principle it could be added instantly to the particle's velocity instead of using separate vector
			// induced velocity, if that is critical to performance.
			VectorAdd( particle->artificialVelocity, inducedVelocity, particle->artificialVelocity );
		}

		// Negative values lead to inflow
		if( flock->outflowSpeed != 0.0f ) {
			vec4_t particleOffsetFromOrigin;
			VectorSubtract( particle->origin, flock->outflowOrigin, particleOffsetFromOrigin );
			const float lengthAlongAxis = DotProduct( particleOffsetFromOrigin, flock->outflowAxis );
			vec4_t perpendicularOffset;
			// It is important that the axis is a unit vector for this step
			VectorMA( particleOffsetFromOrigin, -lengthAlongAxis, flock->outflowAxis, perpendicularOffset );
			if( const float radiusSquared = VectorLengthSquared( perpendicularOffset ); radiusSquared > 1e-2f ) [[likely]] {
				constexpr float referenceDistance    = 2.0f;
				constexpr float rcpReferenceDistance = 1.0f / referenceDistance;
				vec4_t outflowVec;
				// Make the maximum the value at a radius of referenceDistance units
				const float rcpRadiusSquared = wsw::min( Q_Rcp( radiusSquared ), wsw::square( rcpReferenceDistance ) );
				// Make dependency 1/r
				VectorScale( perpendicularOffset, rcpRadiusSquared, outflowVec );
				VectorMA( particle->artificialVelocity, flock->outflowSpeed, outflowVec, particle->artificialVelocity );
			}
		}

		if( flock->turbulenceSpeed > 0.0f && particle->lifetimeFrac > 0.1f ) {
			vec3_t scaledOrigin;
			VectorScale( particle->origin, flock->turbulenceCoordinateScale, scaledOrigin );
			const Vec3 turbulence = calcSimplexNoiseCurl( scaledOrigin[0], scaledOrigin[1], scaledOrigin[2] );
			VectorMA( particle->artificialVelocity, flock->turbulenceSpeed, turbulence.data(), particle->artificialVelocity );
		}

		vec3_t effectiveVelocity;
		VectorMA( particle->dynamicsVelocity, deltaSeconds, accel, particle->dynamicsVelocity );
        VectorAdd( particle->dynamicsVelocity, particle->artificialVelocity, effectiveVelocity );
		VectorMA( particle->oldOrigin, deltaSeconds, effectiveVelocity, particle->origin );

		// TODO: Supply this 4-component vector explicitly
		boundsBuilder.addPoint( particle->origin );

		if( flock->hasRotatingParticles ) {
			particle->rotationAngle += particle->angularVelocity * deltaSeconds;
			particle->rotationAngle = AngleNormalize360( particle->rotationAngle );
		}
	}

	// TODO: Store 4 components directly
	boundsBuilder.storeToWithAddedEpsilon( resultBounds[0], resultBounds[1] );
}

[[nodiscard]]
static inline auto computeParticleLifetimeFrac( int64_t currTime, const Particle &__restrict particle ) -> float {
	const auto offset                 = (int)particle.activationDelay;
	const auto correctedDuration      = wsw::max( 1, particle.lifetime - offset );
	const auto lifetimeSoFar          = (int)( currTime - particle.spawnTime );
	const auto correctedLifetimeSoFar = wsw::max( 0, lifetimeSoFar - offset );
	const auto result                 = (float)correctedLifetimeSoFar * Q_Rcp( (float)correctedDuration );
	assert( result >= 0.0f && result <= 1.0f );
	return result;
}

void ParticleSystem::simulate( ParticleFlock *__restrict flock, wsw::RandomGenerator *__restrict rng,
							   int64_t currTime, float deltaSeconds ) {
	assert( flock->numActivatedParticles + flock->numDelayedParticles > 0 );

	auto timeoutOfParticlesLeft = std::numeric_limits<int64_t>::min();
	if( const auto maybeTimeoutOfDelayedParticles = activateDelayedParticles( flock, currTime ) ) {
		timeoutOfParticlesLeft = *maybeTimeoutOfDelayedParticles;
	}

	// Skip the further simulation if we do not have activated particles and did not manage to activate some.
	if( flock->numActivatedParticles ) [[likely]] {
		vec3_t possibleBounds[2];
		runStepKinematics( flock, deltaSeconds, possibleBounds );

		// TODO: Add a fused call
		CM_BuildShapeList( cl.cms, m_tmpShapeList, possibleBounds[0], possibleBounds[1], MASK_SOLID );
		CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, possibleBounds[0], possibleBounds[1] );

		// TODO: Let the BoundsBuilder store 4-component vectors
		VectorCopy( possibleBounds[0], flock->mins );
		VectorCopy( possibleBounds[1], flock->maxs );
		flock->mins[3] = 0.0f, flock->maxs[3] = 1.0f;

		// Skip collision calls if the built shape list is empty
		if( CM_GetNumShapesInShapeList( m_tmpShapeList ) == 0 ) {
			const int64_t timeoutOfActiveParticles = updateLifetimeOfActiveParticlesWithoutClipping( flock, currTime );
			timeoutOfParticlesLeft                 = std::max( timeoutOfParticlesLeft, timeoutOfActiveParticles );
		} else {
			assert( flock->restitution > 0.0f && flock->restitution <= 1.0f );
			const float debugBeamScale = v_debugImpact.get();

			trace_t trace;
			unsigned particleIndex = 0;
			do {
				Particle *const __restrict p = flock->particles + particleIndex;
				assert( p->spawnTime + p->activationDelay <= currTime );
				const int64_t particleTimeoutAt = p->spawnTime + p->lifetime;

				bool keepTheParticleInGeneral = false;
				if( particleTimeoutAt > currTime ) [[likely]] {
					CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, p->oldOrigin, p->origin, vec3_origin, vec3_origin, MASK_SOLID );

					if( trace.fraction == 1.0f ) [[likely]] {
						// Save the current origin as the old origin
						VectorCopy( p->origin, p->oldOrigin );
						// Keeping the particle in general, no impact to process
						keepTheParticleInGeneral = true;
					} else {
						bool keepTheParticleByImpactRules = false;
						if( !( trace.allsolid | trace.startsolid ) && !( trace.contents & CONTENTS_WATER ) ) [[likely]] {
							keepTheParticleByImpactRules = true;
							// Skip checking/updating the bounce counter during startBounceCounterDelay from spawn.
							// This helps with particles that should not normally bounce but happen to touch surfaces at start.
							// This condition always holds for a zero skipBounceCounterDelay.
							if ( p->spawnTime + flock->startBounceCounterDelay <= currTime ) [[likely]] {
								p->bounceCount++;
								if ( p->bounceCount > flock->maxBounceCount ) {
									keepTheParticleByImpactRules = false;
								} else if ( flock->keepOnImpactProbability != 1.0f && p->bounceCount > flock->minBounceCount ) {
									keepTheParticleByImpactRules = rng->tryWithChance( flock->keepOnImpactProbability );
								}
							}
						}

						if( keepTheParticleByImpactRules ) {
							// Reflect the dynamics velocity
							// Note: The artificial velocity is not updated
							const float oldSquareSpeed    = VectorLengthSquared( p->dynamicsVelocity );
							const float oldSpeedThreshold = 1.0f;
							const float newSpeedThreshold = oldSpeedThreshold * Q_Rcp( flock->restitution );
							if( oldSquareSpeed >= wsw::square( newSpeedThreshold ) ) [[likely]] {
								const float rcpOldSpeed = Q_RSqrt( oldSquareSpeed );
								vec3_t oldVelocityDir { p->dynamicsVelocity[0], p->dynamicsVelocity[1], p->dynamicsVelocity[2] };
								VectorScale( oldVelocityDir, rcpOldSpeed, oldVelocityDir );

								vec3_t reflectedVelocityDir;
								VectorReflect( oldVelocityDir, trace.plane.normal, 0, reflectedVelocityDir );

								if( p->lifetime > 32 ) [[likely]] {
									addRandomRotationToDir( reflectedVelocityDir, rng, 0.70f, 0.97f );
								}

								const float newSpeed = flock->restitution * ( oldSquareSpeed * rcpOldSpeed );
								// Save the reflected velocity
								VectorScale( reflectedVelocityDir, newSpeed, p->dynamicsVelocity );

								// Save the trace endpos with a slight offset as an origin for the next step.
								// This is not really correct but is OK.
								VectorAdd( trace.endpos, reflectedVelocityDir, p->oldOrigin );

								if( debugBeamScale > 0.0f ) {
									vec3_t to;
									VectorMA( p->oldOrigin, debugBeamScale, p->dynamicsVelocity, to );
									vec3_t color { 0.0f, 1.0f, 0.0f };
									cg.effectsSystem.spawnGameDebugBeam( p->oldOrigin, to, color, 0 );
								}

								keepTheParticleInGeneral = true;
							}
						}
					}
				}

				if( keepTheParticleInGeneral ) [[likely]] {
					p->lifetimeFrac        = computeParticleLifetimeFrac( currTime, *p );
					timeoutOfParticlesLeft = wsw::max( particleTimeoutAt, timeoutOfParticlesLeft );
					++particleIndex;
				} else {
					// Replace by the last particle
					flock->particles[particleIndex] = flock->particles[--flock->numActivatedParticles];
				}
			} while( particleIndex < flock->numActivatedParticles );
		}
	}

	// This also schedules disposal in case if all delayed particles
	// got timed out prior to activation during the activation attempt.
	flock->timeoutAt = timeoutOfParticlesLeft;
}

void ParticleSystem::simulateWithoutClipping( ParticleFlock *__restrict flock, int64_t currTime, float deltaSeconds ) {
	assert( flock->numActivatedParticles + flock->numDelayedParticles > 0 );

	auto timeoutOfParticlesLeft = std::numeric_limits<int64_t>::min();
	if( const auto maybeTimeoutOfDelayedParticles = activateDelayedParticles( flock, currTime ) ) {
		timeoutOfParticlesLeft = *maybeTimeoutOfDelayedParticles;
	}

	// Skip the further simulation if we do not have activated particles and did not manage to activate some.
	if( flock->numActivatedParticles ) [[likely]] {
		vec3_t resultingBounds[2];
		runStepKinematics( flock, deltaSeconds, resultingBounds );

		const int64_t timeoutOfActiveParticles = updateLifetimeOfActiveParticlesWithoutClipping( flock, currTime );
		timeoutOfParticlesLeft                 = std::max( timeoutOfParticlesLeft, timeoutOfActiveParticles );

		if( flock->numActivatedParticles ) [[likely]] {
			VectorCopy( resultingBounds[0], flock->mins );
			VectorCopy( resultingBounds[1], flock->maxs );
			flock->mins[3] = 0.0f, flock->maxs[3] = 1.0f;
		}
	}

	// This also schedules disposal in case if all delayed particles
	// got timed out prior to activation during the activation attempt.
	flock->timeoutAt = timeoutOfParticlesLeft;
}

auto ParticleSystem::updateLifetimeOfActiveParticlesWithoutClipping( ParticleFlock *__restrict flock, int64_t currTime ) -> int64_t {
	int64_t timeoutOfActiveParticles = std::numeric_limits<int64_t>::min();

	unsigned particleIndex = 0;
	do {
		Particle *const __restrict p = flock->particles + particleIndex;
		assert( p->spawnTime + p->activationDelay <= currTime );
		const int64_t particleTimeoutAt = p->spawnTime + p->lifetime;

		if( particleTimeoutAt > currTime ) [[likely]] {
			// Save the current origin as the old origin
			VectorCopy( p->origin, p->oldOrigin );
			p->lifetimeFrac = computeParticleLifetimeFrac( currTime, *p );
			timeoutOfActiveParticles = wsw::max( particleTimeoutAt, timeoutOfActiveParticles );
			++particleIndex;
		} else {
			// Replace by the last particle
			flock->particles[particleIndex] = flock->particles[--flock->numActivatedParticles];
		}
	} while( particleIndex < flock->numActivatedParticles );

	return timeoutOfActiveParticles;
}

auto ParticleSystem::activateDelayedParticles( ParticleFlock *flock, int64_t currTime ) -> std::optional<int64_t> {
	int64_t timeoutOfDelayedParticles = 0;

	// We must keep the delayed particles chunk "right-aligned" within the particles buffer,
	// so the data of delayed particles does not overlap with the data of activated ones.

	// Check the overlap invariant
	assert( !flock->numDelayedParticles || flock->numActivatedParticles <= flock->delayedParticlesOffset );

	const unsigned endIndexInBuffer = flock->delayedParticlesOffset + flock->numDelayedParticles;
	for( unsigned indexInBuffer = flock->delayedParticlesOffset; indexInBuffer < endIndexInBuffer; ++indexInBuffer ) {
		Particle *__restrict p = flock->particles + indexInBuffer;
		bool disposeParticle = true;
		if( const int64_t timeoutAt = p->spawnTime + p->lifetime; timeoutAt > currTime ) [[likely]] {
			if( const int64_t activationAt = p->spawnTime + p->activationDelay; activationAt <= currTime ) {
				//Com_Printf( "Activating: Copying delayed particle %d to %d\n", indexInBuffer, flock->numActivatedParticles );
				// Append it to the array of activated particles
				flock->particles[flock->numActivatedParticles++] = *p;
			} else {
				// The particle is kept delayed.
				// Calculate the result timeout, so it affects timeout of the entire flock.
				// (Newly activated particles contribute to the flock timeout in their simulation subroutines).
				timeoutOfDelayedParticles = wsw::max( timeoutAt, timeoutOfDelayedParticles );
				disposeParticle = false;
			}
		}
		if( disposeParticle ) {
			// Overwrite the particle data at the current index by the left-most one
			// This condition also prevents overwriting the back of the activated particles span
			// (that is currently harmless, but it's better to keep it correct)
			if( indexInBuffer != flock->delayedParticlesOffset ) {
				flock->particles[indexInBuffer] = flock->particles[flock->delayedParticlesOffset];
			}
			// Advance the left boundary
			flock->delayedParticlesOffset++;
			flock->numDelayedParticles--;
		}
	}

	assert( !flock->numDelayedParticles || flock->numActivatedParticles <= flock->delayedParticlesOffset );

	return timeoutOfDelayedParticles ? std::optional( timeoutOfDelayedParticles ) : std::nullopt;
}

void ParticleSystem::simulateParticleTrailOfParticles( ParticleFlock *baseFlock, wsw::RandomGenerator *rng,
													   int64_t currTime, float deltaSeconds ) {
	ParticleFlock *const __restrict trailFlock               = baseFlock->trailFlockOfParticles;
	ConicalFlockParams *const __restrict flockParamsTemplate = trailFlock->flockParamsTemplate;
	const ParticleTrailUpdateParams &__restrict updateParams = *trailFlock->particleTrailUpdateParams;

	// First, activate delayed particles
	// TODO: We do not need the return result
	(void)activateDelayedParticles( trailFlock, currTime );

	// Second, dispose timed out particles prior to spawing next
	for( unsigned i = 0; i < trailFlock->numActivatedParticles; ) {
		Particle *__restrict p = trailFlock->particles + i;
		if( const int64_t timeoutAt = p->spawnTime + p->lifetime; timeoutAt > currTime ) [[likely]] {
			++i;
		} else {
			trailFlock->numActivatedParticles--;
			trailFlock->particles[i] = trailFlock->particles[trailFlock->numActivatedParticles];
		}
	}

	Particle::SpriteRules *spriteRules = std::get_if<Particle::SpriteRules>( &baseFlock->appearanceRules.geometryRules );
	Particle::SparkRules *sparkRules   = std::get_if<Particle::SparkRules>( &baseFlock->appearanceRules.geometryRules );

	const Particle::SizeBehaviour sizeBehaviour = spriteRules ? spriteRules->sizeBehaviour : sparkRules->sizeBehaviour;

	const bool modulateSize = trailFlock->modulateByParentSize;

	// Second, spawn particles if needed
	for( unsigned i = 0; i < baseFlock->numActivatedParticles; ++i ) {
		const Particle *const p     = &baseFlock->particles[i];
		float *const lastDropOrigin = trailFlock->lastParticleTrailDropOrigins[p->originalIndex];
		const float sizeFrac        = modulateSize ? calcSizeFracForLifetimeFrac( p->lifetimeFrac, sizeBehaviour ) : 1.0f;

		const ParticleTrailUpdateParams instanceUpdateParams {
			.maxParticlesPerDrop    = updateParams.maxParticlesPerDrop,
			.dropDistance           = updateParams.dropDistance,
			.particleSizeMultiplier = updateParams.particleSizeMultiplier * sizeFrac
		};
		updateParticleTrail( trailFlock, flockParamsTemplate, p->origin, lastDropOrigin, rng, currTime, instanceUpdateParams );
	}

	if( trailFlock->numActivatedParticles ) [[likely]] {
		simulateWithoutClipping( trailFlock, currTime, deltaSeconds );
	} else {
		constexpr float minVal = std::numeric_limits<float>::max(), maxVal = std::numeric_limits<float>::lowest();
		Vector4Set( trailFlock->mins, minVal, minVal, minVal, minVal );
		Vector4Set( trailFlock->maxs, maxVal, maxVal, maxVal, maxVal );
	}

	trailFlock->timeoutAt = std::numeric_limits<int64_t>::max();
}

void ParticleSystem::simulatePolyTrailOfParticles( ParticleFlock *baseFlock, PolyTrailOfParticles *trail, int64_t currTime ) {
	// TODO: Lift it out of the class scope
	const auto updateTrailFn = TrackedEffectsSystem::updateCurvedPolyTrail;

	if( baseFlock ) {
		for( unsigned particleNum = 0; particleNum < baseFlock->numActivatedParticles; ++particleNum ) {
			const Particle *const particle                   = &baseFlock->particles[particleNum];
			PolyTrailOfParticles::ParticleEntry *const entry = &trail->particleEntries[particle->originalIndex];

			assert( !entry->detachedAt );
			entry->touchedAt = currTime;

			updateTrailFn( trail->props, particle->origin, currTime, &entry->points, &entry->timestamps );
			assert( entry->points.size() == entry->timestamps.size() );
		}
	}

	assert( currTime > 0 );
	assert( trail->props.lingeringLimit > 0 && trail->props.lingeringLimit < 1000 );
	const float rcpLingeringLimit = Q_Rcp( (float)trail->props.lingeringLimit );
	assert( trail->numEntries < 256 );
	for( unsigned entryNum = 0; entryNum < trail->numEntries; ++entryNum ) {
		PolyTrailOfParticles::ParticleEntry *const __restrict entry = &trail->particleEntries[entryNum];
		if( entry->detachedAt <= 0 && entry->touchedAt != currTime ) {
			entry->detachedAt = currTime;
		}
		if( entry->detachedAt > 0 ) {
			const int64_t lingeringTime = currTime - entry->detachedAt;
			if( lingeringTime < trail->props.lingeringLimit && entry->points.size() > 1 ) {
				entry->lingeringFrac = (float)lingeringTime * rcpLingeringLimit;
				assert( entry->lingeringFrac >= 0.0f && entry->lingeringFrac <= 1.0f );
				// Update the lingering trail as usual.
				// Submit the last known position as the current one.
				// This allows trails to shrink naturally.
				updateTrailFn( trail->props, entry->points.back().data(), currTime, &entry->points, &entry->timestamps );
			} else {
				entry->points.clear();
				entry->timestamps.clear();
			}
		}
		if( const unsigned numNodes = entry->points.size(); numNodes > 1 ) [[likely]] {
			BoundsBuilder boundsBuilder;
			unsigned nodeNum = 0;
			do {
				boundsBuilder.addPoint( entry->points[nodeNum].data() );
			} while( ++nodeNum < numNodes );
			boundsBuilder.storeToWithAddedEpsilon( entry->poly.cullMins, entry->poly.cullMaxs );
			entry->poly.cullMins[3] = 0.0f, entry->poly.cullMaxs[3] = 1.0f;
			entry->poly.points = std::span<const vec3_t>( (const vec3_t *)entry->points.data(), numNodes );
		} else {
			entry->poly.points = std::span<const vec3_t>();
		}
	}
}