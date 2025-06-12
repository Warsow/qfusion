#include "snd_effect_sampler.h"
#include "snd_leaf_props_cache.h"
#include "snd_effects_allocator.h"
#include "snd_propagation.h"
#include "efxpresetsregistry.h"

#include <common/types/staticvector.h>
#include <common/helpers/stringsplitter.h>
#include <common/facilities/cvar.h>

#include <limits>
#include <random>

static UnderwaterFlangerEffectSampler underwaterFlangerEffectSampler;

static ReverbEffectSampler reverbEffectSampler;

Effect *EffectSamplers::TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) {
	Effect *effect;
	if( ( effect = ::underwaterFlangerEffectSampler.TryApply( listenerProps, src, tryReusePropsSrc ) ) ) {
		return effect;
	}
	if( ( effect = ::reverbEffectSampler.TryApply( listenerProps, src, tryReusePropsSrc ) ) ) {
		return effect;
	}

	Com_Error( ERR_FATAL, "EffectSamplers::TryApply(): Can't find an applicable effect sampler\n" );
}

// We want sampling results to be reproducible especially for leaf sampling and thus use this local implementation
static std::minstd_rand0 samplingRandom;

float EffectSamplers::SamplingRandom() {
	typedef decltype( samplingRandom ) R;
	return ( samplingRandom() - R::min() ) / (float)( R::max() - R::min() );
}

Effect *UnderwaterFlangerEffectSampler::TryApply( const ListenerProps &listenerProps, src_t *src, const src_t * ) {
	if( !listenerProps.isInLiquid && !src->envUpdateState.isInLiquid ) {
		return nullptr;
	}

	float directObstruction = 0.9f;
	if( src->envUpdateState.isInLiquid && listenerProps.isInLiquid ) {
		directObstruction = ComputeDirectObstruction( listenerProps, src );
	}

	auto *effect = EffectsAllocator::Instance()->NewFlangerEffect( src );
	effect->directObstruction = directObstruction;
	effect->hasMediumTransition = src->envUpdateState.isInLiquid ^ listenerProps.isInLiquid;
	return effect;
}

static bool ENV_TryReuseSourceReverbProps( src_t *src, const src_t *tryReusePropsSrc, EaxReverbEffect *newEffect ) {
	if( !tryReusePropsSrc ) {
		return false;
	}

	auto *reuseEffect = Effect::Cast<const EaxReverbEffect *>( tryReusePropsSrc->envUpdateState.effect );
	if( !reuseEffect ) {
		return false;
	}

	// We are already sure that both sources are in the same contents kind (non-liquid).
	// Check distance between sources.
	const float squareDistance = DistanceSquared( tryReusePropsSrc->origin, src->origin );
	// If they are way too far for reusing
	if( squareDistance > 96 * 96 ) {
		return false;
	}

	// If they are very close, feel free to just copy props
	if( squareDistance > 4.0f * 4.0f ) {
		// Do a coarse raycast test between these two sources
		vec3_t start, end, dir;
		VectorSubtract( tryReusePropsSrc->origin, src->origin, dir );
		const float invDistance = 1.0f / sqrtf( squareDistance );
		VectorScale( dir, invDistance, dir );
		// Offset start and end by a dir unit.
		// Ensure start and end are in "air" and not on a brush plane
		VectorAdd( src->origin, dir, start );
		VectorSubtract( tryReusePropsSrc->origin, dir, end );

		trace_t trace;
		S_Trace( &trace, start, end, vec3_origin, vec3_origin, MASK_SOLID );
		if( trace.fraction != 1.0f ) {
			return false;
		}
	}

	newEffect->directObstruction        = reuseEffect->directObstruction;
	newEffect->secondaryRaysObstruction = reuseEffect->secondaryRaysObstruction;
	newEffect->reverbProps              = reuseEffect->reverbProps;
	return true;
}

void ObstructedEffectSampler::SetupDirectObstructionSamplingProps( src_t *src, unsigned minSamples, unsigned maxSamples ) {
	float quality = s_environment_sampling_quality->value;
	samplingProps_t *props = &src->envUpdateState.directObstructionSamplingProps;

	// If the quality is valid and has not been modified since the pattern has been set
	if( props->quality == quality ) {
		return;
	}

	unsigned numSamples = GetNumSamplesForCurrentQuality( minSamples, maxSamples );

	props->quality = quality;
	props->numSamples = numSamples;
	props->valueIndex = (uint16_t)( EffectSamplers::SamplingRandom() * std::numeric_limits<uint16_t>::max() );
}

struct DirectObstructionOffsetsHolder {
	static constexpr unsigned NUM_VALUES = 256;
	vec3_t offsets[NUM_VALUES];
	static constexpr float MAX_OFFSET = 20.0f;

	DirectObstructionOffsetsHolder() {
		for( float *v: offsets ) {
			for( int i = 0; i < 3; ++i ) {
				v[i] = -MAX_OFFSET + 2 * MAX_OFFSET * EffectSamplers::SamplingRandom();
			}
		}
	}
};

static DirectObstructionOffsetsHolder directObstructionOffsetsHolder;

float ObstructedEffectSampler::ComputeDirectObstruction( const ListenerProps &listenerProps, src_t *src ) {
	trace_t trace;
	envUpdateState_t *updateState;
	float *originOffset;
	vec3_t testedListenerOrigin;
	vec3_t testedSourceOrigin;
	float squareDistance;
	unsigned numTestedRays, numPassedRays;
	unsigned valueIndex;

	updateState = &src->envUpdateState;

	VectorCopy( listenerProps.origin, testedListenerOrigin );
	// TODO: We assume standard view height
	testedListenerOrigin[2] += 18.0f;

	squareDistance = DistanceSquared( testedListenerOrigin, src->origin );
	// Shortcut for sounds relative to the player
	if( squareDistance < 32.0f * 32.0f ) {
		return 0.0f;
	}

	if( !S_LeafsInPVS( listenerProps.GetLeafNum(), S_PointLeafNum( src->origin ) ) ) {
		return 1.0f;
	}

	vec3_t hintBounds[2];
	ClearBounds( hintBounds[0], hintBounds[1] );
	AddPointToBounds( testedListenerOrigin, hintBounds[0], hintBounds[1] );
	AddPointToBounds( src->origin, hintBounds[0], hintBounds[1] );
	// Account for obstruction sampling offsets
	// as we are going to compute the top node hint once
	for( int i = 0; i < 3; ++i ) {
		hintBounds[0][i] -= DirectObstructionOffsetsHolder::MAX_OFFSET;
		hintBounds[1][i] += DirectObstructionOffsetsHolder::MAX_OFFSET;
	}

	const int topNodeHint = S_FindTopNodeForBox( hintBounds[0], hintBounds[1] );
	S_Trace( &trace, testedListenerOrigin, src->origin, vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
	if( trace.fraction == 1.0f && !trace.startsolid ) {
		// Consider zero obstruction in this case
		return 0.0f;
	}

	SetupDirectObstructionSamplingProps( src, 3, MAX_DIRECT_OBSTRUCTION_SAMPLES );

	numPassedRays = 0;
	numTestedRays = updateState->directObstructionSamplingProps.numSamples;
	valueIndex = updateState->directObstructionSamplingProps.valueIndex;
	for( unsigned i = 0; i < numTestedRays; i++ ) {
		valueIndex = ( valueIndex + 1 ) % DirectObstructionOffsetsHolder::NUM_VALUES;
		originOffset = directObstructionOffsetsHolder.offsets[ valueIndex ];

		VectorAdd( src->origin, originOffset, testedSourceOrigin );
		S_Trace( &trace, testedListenerOrigin, testedSourceOrigin, vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
		if( trace.fraction == 1.0f && !trace.startsolid ) {
			numPassedRays++;
		}
	}

	return 1.0f - 0.9f * ( numPassedRays / (float)numTestedRays );
}

Effect *ReverbEffectSampler::TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) {
	EaxReverbEffect *effect = EffectsAllocator::Instance()->NewReverbEffect( src );
	effect->directObstruction = ComputeDirectObstruction( listenerProps, src );
	// We try reuse props only for reverberation effects
	// since reverberation effects sampling is extremely expensive.
	// Moreover, direct obstruction reuse is just not valid,
	// since even a small origin difference completely changes it.
	if( ENV_TryReuseSourceReverbProps( src, tryReusePropsSrc, effect ) ) {
		src->envUpdateState.needsInterpolation = false;
	} else {
		ComputeReverberation( listenerProps, src, effect );
	}
	return effect;
}

float ReverbEffectSampler::GetEmissionRadius() const {
	// Do not even bother casting rays 999999 units ahead for very attenuated sources.
	// However, clamp/normalize the hit distance using the same defined threshold
	float attenuation = src->attenuation;

	if( attenuation <= 1.0f ) {
		return 999999.9f;
	}

	clamp_high( attenuation, 10.0f );
	float distance = 4.0f * REVERB_ENV_DISTANCE_THRESHOLD;
	distance -= 3.5f * Q_Sqrt( attenuation / 10.0f ) * REVERB_ENV_DISTANCE_THRESHOLD;
	return distance;
}

void ReverbEffectSampler::ResetMutableState( const ListenerProps &listenerProps_, src_t *src_, EaxReverbEffect *effect_ ) {
	this->listenerProps = &listenerProps_;
	this->src = src_;
	this->effect = effect_;

	GenericRaycastSampler::ResetMutableState( primaryRayDirs, reflectionPoints, primaryHitDistances, src->origin );

	VectorCopy( listenerProps_.origin, testedListenerOrigin );
	testedListenerOrigin[2] += 18.0f;
}

class CachedPresetTracker {
public:
	CachedPresetTracker( const char *varName, const char *defaultValue ) noexcept
		: m_varName( varName ), m_defaultValue( defaultValue ) {}

	[[nodiscard]]
	auto getPreset() const -> const EfxReverbProps * {
		if( !m_var ) {
			m_var = Cvar_Get( m_varName, m_defaultValue, CVAR_CHEAT | CVAR_DEVELOPER );
			m_var->modified = true;
		}
		if( m_var->modified ) {
			m_preset = getByString( wsw::StringView( m_var->string ) );
			if( !m_preset ) {
				Com_Printf( S_COLOR_YELLOW "Failed to find a preset by string \"%s\" for %s. Using the default value \"%s\"\n",
							m_var->string, m_var->name, m_defaultValue );
				Cvar_ForceSet( m_var->name, m_defaultValue );
				m_preset = getByString( wsw::StringView( m_var->string ) );
				assert( m_preset );
			}
			m_var->modified = false;
		}
		return m_preset;
	}
private:
	[[nodiscard]]
	auto getByString( const wsw::StringView &string ) const -> const EfxReverbProps * {
		if( string.indexOf( ' ' ) == std::nullopt ) {
			return EfxPresetsRegistry::s_instance.findByName( string );
		} else {
			wsw::StringSplitter splitter( string );
			wsw::StaticVector<const EfxReverbProps *, 4> parts;
			while( const auto maybeToken = splitter.getNext( ' ' ) ) {
				if( !parts.full() ) {
					if( const auto *preset = EfxPresetsRegistry::s_instance.findByName( *maybeToken )) {
						parts.push_back( preset );
					} else {
						return nullptr;
					}
				} else {
					return nullptr;
				}
			}
			if( !parts.empty() ) {
				mixReverbProps( parts.begin(), parts.end(), &m_mixStorage );
				return &m_mixStorage;
			} else {
				return nullptr;
			}
		}
	}

	const char *const m_varName;
	const char *const m_defaultValue;
	mutable cvar_t *m_var { nullptr };
	mutable const EfxReverbProps *m_preset { nullptr };
	mutable EfxReverbProps m_mixStorage {};
};

static CachedPresetTracker g_tinyOpenRoomPreset { "s_tinyOpenRoomPreset", "quarry quarry plain" };
static CachedPresetTracker g_largeOpenRoomPreset { "g_largeOpenRoomPreset", "outdoors_rollingplains plain" };
static CachedPresetTracker g_hugeOpenRoomPreset { "s_hugeOpenRoomPreset", "outdoors_rollingplains plain mountains" };

static CachedPresetTracker g_tinyAbsorptiveRoomPreset { "s_tinyAbsorptiveRoomPreset", "wooden_smallroom" };
static CachedPresetTracker g_largeAbsorptiveRoomPreset { "s_largeAbsorptiveRoomPreset", "wooden_largeroom wooden_mediumroom" };
static CachedPresetTracker g_hugeAbsorptiveRoomPreset { "s_hugeAbsorptiveRoomPreset", "wooden_hall wooden_hall hangar" };

static CachedPresetTracker g_tinyNeutralRoomPreset { "s_tinyNeutralRoomPreset", "castle_smallroom wooden_smallroom" };
static CachedPresetTracker g_largeNeutralRoomPreset { "s_largeNeutralRoomPreset",
													  "castle_largeroom wooden_largeroom castle_mediumroom" };
static CachedPresetTracker g_hugeNeutralRoomPreset { "s_hugeNeutralRoomPreset", "castle_hall wooden_hall hangar" };

static CachedPresetTracker g_tinyReflectiveRoomPreset { "s_tinyReflectiveRoomPreset",
														"castle_smallroom spacestation_smallroom" };
static CachedPresetTracker g_largeReflectiveRoomPreset { "s_largeReflectiveRoomPreset",
														 "castle_largeroom spacestation_largeroom spacestation_mediumroom" };
static CachedPresetTracker g_hugeReflectiveRoomPreset { "s_hugeReflectiveRoomPreset",
														"castle_hall spacestation_hall hangar" };

static CachedPresetTracker g_tinyMetallicRoomPreset { "s_tinyMetallicRoomPreset", "factory_smallroom" };
static CachedPresetTracker g_largeMetallicRoomPreset { "s_largeMetallicRoomPreset", "factory_largeroom factory_mediumroom" };
static CachedPresetTracker g_hugeMetallicRoomPreset { "s_hugeMetallicRoomPreset", "factory_hall factory_hall hangar" };

void ReverbEffectSampler::ComputeReverberation( const ListenerProps &listenerProps_,
												src_t *src_,
												EaxReverbEffect *effect_ ) {
	ResetMutableState( listenerProps_, src_, effect_ );

	numPrimaryRays = GetNumSamplesForCurrentQuality( 16, MAX_REVERB_PRIMARY_RAY_SAMPLES );

	SetupPrimaryRayDirs();

	EmitPrimaryRays();

	if( !numPrimaryHits ) {
		// Keep existing values (they are valid by default now)
		return;
	}

	// Instead of trying to compute these factors every sampling call,
	// reuse pre-computed properties of CM map leafs that briefly resemble rooms/convex volumes.
	assert( src->envUpdateState.leafNum >= 0 );

	const auto *const leafPropsCache = LeafPropsCache::Instance();
	const LeafProps &leafProps = leafPropsCache->GetPropsForLeaf( src->envUpdateState.leafNum );

	EfxReverbProps openProps { EfxReverbProps::NoInit };
	EfxReverbProps closedMetallicProps { EfxReverbProps::NoInit };
	EfxReverbProps closedNonMetallicProps { EfxReverbProps::NoInit };

	if( const float roomSizeFactor = leafProps.getRoomSizeFactor(); roomSizeFactor <= 0.5f ) {
		const float sizeFrac = 2.0f * roomSizeFactor;
		assert( sizeFrac >= 0.0f && sizeFrac <= 1.0f );

		EfxReverbProps tinyProps { EfxReverbProps::NoInit };
		EfxReverbProps largeProps { EfxReverbProps::NoInit };

		if( const float smoothnessFactor = leafProps.getSmoothnessFactor(); smoothnessFactor <= 0.5f ) {
			const float smoothnessFrac = 2.0f * smoothnessFactor;
			assert( smoothnessFrac >= 0.0f && smoothnessFrac <= 1.0f );

			interpolateReverbProps( g_tinyAbsorptiveRoomPreset.getPreset(), smoothnessFrac,
									g_tinyNeutralRoomPreset.getPreset(), &tinyProps );
			interpolateReverbProps( g_largeAbsorptiveRoomPreset.getPreset(), smoothnessFrac,
									g_largeNeutralRoomPreset.getPreset(), &largeProps );
		} else {
			const float smoothnessFrac = 2.0f * ( smoothnessFactor - 0.5f );
			assert( smoothnessFrac >= 0.0f && smoothnessFrac <= 1.0f );

			interpolateReverbProps( g_tinyNeutralRoomPreset.getPreset(), smoothnessFrac,
									g_tinyReflectiveRoomPreset.getPreset(), &tinyProps );
			interpolateReverbProps( g_largeNeutralRoomPreset.getPreset(), smoothnessFrac,
									g_largeReflectiveRoomPreset.getPreset(), &largeProps );
		}

		interpolateReverbProps( &tinyProps, sizeFrac, &largeProps, &closedNonMetallicProps );
		interpolateReverbProps( g_tinyOpenRoomPreset.getPreset(), sizeFrac, g_largeOpenRoomPreset.getPreset(), &openProps );
		interpolateReverbProps( g_tinyMetallicRoomPreset.getPreset(), sizeFrac,
						 g_largeMetallicRoomPreset.getPreset(), &closedMetallicProps );
	} else {
		const float sizeFrac = 2.0f * ( roomSizeFactor - 0.5f );
		assert( sizeFrac >= 0.0f && sizeFrac <= 1.0f );

		EfxReverbProps largeProps { EfxReverbProps::NoInit };
		EfxReverbProps hugeProps { EfxReverbProps::NoInit };

		if( const float smoothnessFactor = leafProps.getSmoothnessFactor(); smoothnessFactor <= 0.5f ) {
			const float smoothnessFrac = 2.0f * smoothnessFactor;
			assert( smoothnessFrac >= 0.0f && smoothnessFrac <= 1.0f );

			interpolateReverbProps( g_largeAbsorptiveRoomPreset.getPreset(), smoothnessFrac,
									g_largeNeutralRoomPreset.getPreset(), &largeProps );
			interpolateReverbProps( g_hugeAbsorptiveRoomPreset.getPreset(), smoothnessFrac,
									g_hugeNeutralRoomPreset.getPreset(), &hugeProps );
		} else {
			const float smoothnessFrac = 2.0f * ( smoothnessFactor - 0.5f );
			assert( smoothnessFrac >= 0.0f && smoothnessFrac <= 1.0f );

			interpolateReverbProps( g_largeNeutralRoomPreset.getPreset(), smoothnessFrac,
									g_largeReflectiveRoomPreset.getPreset(), &largeProps );
			interpolateReverbProps( g_hugeNeutralRoomPreset.getPreset(), smoothnessFrac,
									g_hugeReflectiveRoomPreset.getPreset(), &hugeProps );
		}

		interpolateReverbProps( &largeProps, sizeFrac, &hugeProps, &closedNonMetallicProps );
		interpolateReverbProps( g_largeOpenRoomPreset.getPreset(), sizeFrac, g_hugeOpenRoomPreset.getPreset(), &openProps );
		interpolateReverbProps( g_largeMetallicRoomPreset.getPreset(), sizeFrac,
								g_hugeMetallicRoomPreset.getPreset(), &closedMetallicProps );
	}

	EfxReverbProps closedProps { EfxReverbProps::NoInit };
	interpolateReverbProps( &closedNonMetallicProps, leafProps.getMetallnessFactor(), &closedMetallicProps, &closedProps );

	interpolateReverbProps( &closedProps, leafProps.getSkyFactor(), &openProps, &effect->reverbProps );

	EmitSecondaryRays();
}

void ReverbEffectSampler::SetupPrimaryRayDirs() {
	assert( numPrimaryRays );

	SetupSamplingRayDirs( primaryRayDirs, numPrimaryRays );
}

void ReverbEffectSampler::EmitSecondaryRays() {
	int listenerLeafNum = listenerProps->GetLeafNum();

	auto *const panningUpdateState = &src->panningUpdateState;
	panningUpdateState->numPrimaryRays = numPrimaryRays;

	trace_t trace;

	unsigned numPassedSecondaryRays = 0;
	panningUpdateState->numPassedSecondaryRays = 0;
	for( unsigned i = 0; i < numPrimaryHits; i++ ) {
		// Cut off by PVS system early, we are not interested in actual ray hit points contrary to the primary emission.
		if( !S_LeafsInPVS( listenerLeafNum, S_PointLeafNum( reflectionPoints[i] ) ) ) {
			continue;
		}

		S_Trace( &trace, reflectionPoints[i], testedListenerOrigin, vec3_origin, vec3_origin, MASK_SOLID );
		if( trace.fraction == 1.0f && !trace.startsolid ) {
			numPassedSecondaryRays++;
			float *savedPoint = panningUpdateState->reflectionPoints[panningUpdateState->numPassedSecondaryRays++];
			VectorCopy( reflectionPoints[i], savedPoint );
		}
	}

	if( numPrimaryHits ) {
		float frac = numPassedSecondaryRays / (float)numPrimaryHits;
		// The secondary rays obstruction is complement to the `frac`
		effect->secondaryRaysObstruction = 1.0f - frac;
	} else {
		// Set minimal feasible values
		effect->secondaryRaysObstruction = 1.0f;
	}
}