#include "environmentupdates.h"
#include "snd_leaf_props_cache.h"
#include "snd_env_effects.h"
#include "snd_raycast_sampler.h"
#include "snd_effects_allocator.h"
#include "snd_propagation.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/stringsplitter.h>
#include <common/facilities/cvar.h>
#include <common/facilities/gs_public.h>
#include <common/facilities/protocol.h>
#include <common/facilities/sysclock.h>
#include <limits>
#include <random>

static ListenerProps g_listenerProps;

constexpr const auto MAX_DIRECT_OBSTRUCTION_SAMPLES = 8;
// Almost doubled for "realistic obstruction" (we need more secondary rays)
constexpr const auto MAX_REVERB_PRIMARY_RAY_SAMPLES = 80;

static_assert( PanningUpdateState::MAX_POINTS == MAX_REVERB_PRIMARY_RAY_SAMPLES, "" );

// We want sampling results to be reproducible especially for leaf sampling and thus use this local implementation
static std::minstd_rand0 samplingRandom;

float EffectSamplers::SamplingRandom() {
	typedef decltype( samplingRandom ) R;
	return ( samplingRandom() - R::min() ) / (float)( R::max() - R::min() );
}

int ListenerProps::GetLeafNum() const {
	if( leafNum < 0 ) {
		leafNum = S_PointLeafNum( origin );
	}
	return leafNum;
}

static void ENV_ShutdownGlobalInstances() {
	LeafPropsCache::Shutdown();
	CachedLeafsGraph::Shutdown();
	PropagationTable::Shutdown();

	EffectsAllocator::Shutdown();
}

static void ENV_DispatchEnsureValidCall() {
	LeafPropsCache::Instance()->EnsureValid();
	CachedLeafsGraph::Instance()->EnsureValid();
	PropagationTable::Instance()->EnsureValid();
}

static void ENV_InitGlobalInstances() {
	LeafPropsCache::Init();
	CachedLeafsGraph::Init();
	PropagationTable::Init();

	ENV_DispatchEnsureValidCall();

	EffectsAllocator::Init();
}

void ENV_Init() {
	if( !s_environment_effects->integer ) {
		return;
	}

	g_listenerProps.InvalidateCachedUpdateState();

	ENV_InitGlobalInstances();
}

void ENV_Shutdown() {
	if( !s_environment_effects->integer ) {
		return;
	}

	ENV_ShutdownGlobalInstances();

	g_listenerProps.InvalidateCachedUpdateState();
}

void ENV_EndRegistration() {
	if( !s_environment_effects->integer ) {
		return;
	}

	ENV_DispatchEnsureValidCall();
}

void ENV_RegisterSource( src_t *src ) {
	// Invalidate last update when reusing the source
	// (otherwise it might be misused for props interpolation)
	src->envUpdateState.lastEnvUpdateAt = 0;
	// Force an immediate update
	src->envUpdateState.nextEnvUpdateAt = 0;
	// Reset sampling patterns by setting an illegal quality value
	src->envUpdateState.directObstructionSamplingProps.quality = -1.0f;
}

void ENV_UnregisterSource( src_t *src ) {
	if( !s_environment_effects->integer ) {
		return;
	}

	// Prevent later occasional updates
	src->envUpdateState.nextEnvUpdateAt = std::numeric_limits<int64_t>::max();

	if( src->envUpdateState.effect || src->envUpdateState.oldEffect ) {
		auto *const effectsAllocator = EffectsAllocator::Instance();
		effectsAllocator->DeleteEffect( src->envUpdateState.oldEffect );
		src->envUpdateState.oldEffect = nullptr;
		effectsAllocator->DeleteEffect( src->envUpdateState.effect );
		src->envUpdateState.effect = nullptr;
	}

	// Detach the slot from the source
	alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, AL_FILTER_NULL );
	// Detach the effect from the slot
	alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL );
	// Detach the direct filter
	alSourcei( src->source, AL_DIRECT_FILTER, AL_FILTER_NULL );
	// Restore the original source gain
	if( src->volumeVar ) {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * src->volumeVar->value ) );
	} else {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * s_volume->value ) );
	}
}

class SourcesUpdatePriorityQueue {
	struct ComparableSource {
		src_t *src;
		ComparableSource(): src( nullptr ) {}
		ComparableSource( src_t *src_ ): src( src_ ) {}

		bool operator<( const ComparableSource &that ) const {
			// We use a max-heap, so the natural comparison order for priorities is the right one
			return src->envUpdateState.priorityInQueue < that.src->envUpdateState.priorityInQueue;
		}
	};

	ComparableSource heap[MAX_SRC];
	int numSourcesInHeap;
public:
	SourcesUpdatePriorityQueue() {
		Clear();
	}

	void Clear() { numSourcesInHeap = 0; }

	void AddSource( src_t *src, float urgencyScale );
	src_t *PopSource();
};

static void ENV_UpdateSourceEnvironment( src_t *src, const src_t *tryReusePropsSrc,
										 const ListenerProps &listenerProps, int64_t millisNow );

static void ENV_CollectForcedEnvironmentUpdates( SourcesUpdatePriorityQueue *priorityQueue ) {
	for( src_t *src = srclist, *end = srclist + src_count; src != end; ++src ) {
		if( !src->isActive ) {
			continue;
		}
		// Music? TODO: It should not share sources with regular sounds...
		if( !src->sfx ) {
			continue;
		}

		if( src->priority != SRCPRI_LOCAL ) {
			priorityQueue->AddSource( src, 1.0f );
			continue;
		}

		if( !src->envUpdateState.nextEnvUpdateAt ) {
			priorityQueue->AddSource( src, 1.0f );
			continue;
		}
	}
}

static void ENV_CollectRegularEnvironmentUpdates( SourcesUpdatePriorityQueue *priorityQueue ) {
	src_t *src, *end;
	envUpdateState_t *updateState;
	int64_t millisNow;
	int contents;

	millisNow = Sys_Milliseconds();

	for( src = srclist, end = srclist + src_count; src != end; ++src ) {
		if( !src->isActive ) {
			continue;
		}
		// Music? TODO: It should not share sources with regular sounds...
		if( !src->sfx ) {
			continue;
		}

		updateState = &src->envUpdateState;
		if( src->priority == SRCPRI_LOCAL ) {
			// If this source has never been updated, add it to the queue, otherwise skip further updates.
			if( !updateState->nextEnvUpdateAt ) {
				priorityQueue->AddSource( src, 5.0f );
			}
			continue;
		}

		contents = S_PointContents( src->origin );
		bool wasInLiquid = updateState->isInLiquid;
		updateState->isInLiquid = ( contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER ) ) != 0;
		if( updateState->isInLiquid ^ wasInLiquid ) {
			priorityQueue->AddSource( src, 2.0f );
			continue;
		}

		// Don't update lingering sources environment
		if( src->isLingering ) {
			continue;
		}

		if( updateState->nextEnvUpdateAt <= millisNow ) {
			// If the playback has been just added
			if( !updateState->nextEnvUpdateAt ) {
				priorityQueue->AddSource( src, 5.0f );
			} else {
				priorityQueue->AddSource( src, 1.0f );
			}
			continue;
		}

		// If the sound is not fixed
		if( updateState->entNum >= 0 ) {
			// If the sound origin has been significantly modified
			if( DistanceSquared( src->origin, updateState->lastUpdateOrigin ) > 128 * 128 ) {
				// Hack! Prevent fast-moving entities (that are very likely PG projectiles)
				// to consume the entire updates throughput
				if( VectorLengthSquared( src->velocity ) < 700 * 700 ) {
					priorityQueue->AddSource( src, 1.5f );
				}
				continue;
			}

			// If the entity velocity has been significantly modified
			if( DistanceSquared( src->velocity, updateState->lastUpdateVelocity ) > 200 * 200 ) {
				priorityQueue->AddSource( src, 1.5f );
				continue;
			}
		}
	}
}

void SourcesUpdatePriorityQueue::AddSource( src_t *src, float urgencyScale ) {
	float attenuationScale;

	assert( urgencyScale >= 0.0f );

	attenuationScale = src->attenuation / 20.0f;
	clamp_high( attenuationScale, 1.0f );
	attenuationScale = sqrtf( attenuationScale );
	assert( attenuationScale >= 0.0f && attenuationScale <= 1.0f );

	src->envUpdateState.priorityInQueue = urgencyScale;
	src->envUpdateState.priorityInQueue *= 1.0f - 0.7f * attenuationScale;

	// Construct a ComparableSource at the end of the heap array
	void *mem = heap + numSourcesInHeap++;
	new( mem )ComparableSource( src );
	// Update the heap
	wsw::push_heap( heap, heap + numSourcesInHeap );
}

src_t *SourcesUpdatePriorityQueue::PopSource() {
	if( !numSourcesInHeap ) {
		return nullptr;
	}

	// Pop the max element from the heap
	wsw::pop_heap( heap, heap + numSourcesInHeap );
	// Chop last heap array element (it does not belong to the heap anymore)
	numSourcesInHeap--;
	// Return the just truncated element
	return heap[numSourcesInHeap].src;
}

static void ENV_ProcessUpdatesPriorityQueue( ListenerProps *listenerProps, SourcesUpdatePriorityQueue *priorityQueue ) {
	const uint64_t micros = Sys_Microseconds();
	const int64_t millis = (int64_t)( micros / 1000 );
	src_t *src;

	listenerProps->InvalidateCachedUpdateState();

	const SoundSet *lastProcessedSfx = nullptr;
	const src_t *lastProcessedSrc = nullptr;
	float lastProcessedPriority = std::numeric_limits<float>::max();
	// Always do at least a single update
	for( ;; ) {
		if( !( src = priorityQueue->PopSource() ) ) {
			break;
		}

		const src_t *tryReusePropsSrc = nullptr;
		if( src->sfx == lastProcessedSfx ) {
			tryReusePropsSrc = lastProcessedSrc;
		}

		assert( lastProcessedPriority >= src->envUpdateState.priorityInQueue );
		lastProcessedPriority = src->envUpdateState.priorityInQueue;
		lastProcessedSfx = src->sfx;
		lastProcessedSrc = src;

		ENV_UpdateSourceEnvironment( src, tryReusePropsSrc, *listenerProps, millis );
		// Stop updates if the time quota has been exceeded immediately.
		// Do not block the commands queue processing.
		// The priority queue will be rebuilt next ENV_UpdateListenerCall().
		if( Sys_Microseconds() - micros > 2000 && lastProcessedPriority < 1.0f ) {
			break;
		}
	}
}

void ENV_UpdateRelativeSoundsSpatialization( const vec3_t origin, const vec3_t velocity ) {
	src_t *src, *end;

	for( src = srclist, end = srclist + src_count; src != end; ++src ) {
		if( !src->isActive ) {
			continue;
		}
		if( src->attenuation ) {
			continue;
		}
		VectorCopy( origin, src->origin );
		VectorCopy( velocity, src->velocity );
	}
}

static void ENV_UpdatePanning( int64_t millisNow, int listenerEntNum, const vec3_t origin, const mat3_t axes ) {
	for( src_t *src = srclist, *end = srclist + src_count; src != end; ++src ) {
		if( src->isActive ) {
			if( EaxReverbEffect *effect = src->envUpdateState.effect ) {
				effect->UpdatePanning( src, listenerEntNum, origin, axes );
			}
		}
	}
}

void ENV_UpdateListener( int listenerEntNum, const vec3_t origin, const vec3_t velocity, const mat3_t axes ) {
	vec3_t testedOrigin;
	bool needsForcedUpdate = false;
	bool isListenerInLiquid;

	if( !s_environment_effects->integer ) {
		return;
	}

	ENV_UpdateRelativeSoundsSpatialization( origin, velocity );

	// Check whether we have teleported or entered/left a liquid.
	// Run a forced major update in this case.

	if( DistanceSquared( origin, g_listenerProps.origin ) > 100.0f * 100.0f ) {
		needsForcedUpdate = true;
	} else if( DistanceSquared( velocity, g_listenerProps.velocity ) > 200.0f * 200.0f ) {
		needsForcedUpdate = true;
	}

	// Check the "head" contents. We assume the regular player viewheight.
	VectorCopy( origin, testedOrigin );
	testedOrigin[2] += 18;
	int contents = S_PointContents( testedOrigin );

	isListenerInLiquid = ( contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER ) ) != 0;
	if( g_listenerProps.isInLiquid != isListenerInLiquid ) {
		needsForcedUpdate = true;
	}

	VectorCopy( origin, g_listenerProps.origin );
	VectorCopy( velocity, g_listenerProps.velocity );
	g_listenerProps.entNum     = listenerEntNum;
	g_listenerProps.isInLiquid = isListenerInLiquid;

	// Sanitize the possibly modified cvar before the environment update
	if( s_environment_sampling_quality->value < 0.0f || s_environment_sampling_quality->value > 1.0f ) {
		Cvar_ForceSet( s_environment_sampling_quality->name, "0.5" );
	}

	// Caution! This code relies on an assumtion that the priority queue does not perform heap allocation.
	// Otherwise, cache the queue instance;
	SourcesUpdatePriorityQueue priorityQueue;

	if( needsForcedUpdate ) {
		ENV_CollectForcedEnvironmentUpdates( &priorityQueue );
	} else {
		ENV_CollectRegularEnvironmentUpdates( &priorityQueue );
	}

	ENV_ProcessUpdatesPriorityQueue( &g_listenerProps, &priorityQueue );

	// Panning info is dependent of environment one, make sure it is executed last
	ENV_UpdatePanning( Sys_Milliseconds(), listenerEntNum, testedOrigin, axes );
}

class ReverbRaycastSampler : public GenericRaycastSampler {
public:
	ReverbRaycastSampler( vec3_t *primaryRayDirs_, vec3_t *primaryHitPoints_, float *primaryHitDistances_,
						  const vec3_t emissionOrigin_, float emissionRadius_, unsigned numPrimaryRays_ )
		: GenericRaycastSampler( primaryRayDirs_, primaryHitPoints_, primaryHitDistances_,
								 emissionOrigin_, emissionRadius_, numPrimaryRays_ ) {}
};

class ReverbEffectComputer {
private:
	static void SetupDirectObstructionSamplingProps( src_t *src, unsigned minSamples, unsigned maxSamples );

	static float ComputeDirectObstruction( const ListenerProps &listenerProps, src_t *src );

	static unsigned GetNumSamplesForCurrentQuality( unsigned minSamples, unsigned maxSamples );

	static void ComputeReverberation( const ListenerProps &listenerProps_, src_t *src, EaxReverbEffect *effect );

	static float CalcEmissionRadius( const src_t *src );

	static void EmitSecondaryRays( const ReverbRaycastSampler &sampler, const ListenerProps &listenerProps, src_t *src, EaxReverbEffect *effect );
public:
	static EaxReverbEffect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc );
};

static void ENV_InterpolateEnvironmentProps( src_t *src, int64_t millisNow ) {
	auto *updateState = &src->envUpdateState;
	if( !updateState->effect ) {
		return;
	}

	int timeDelta = (int)( millisNow - updateState->lastEnvUpdateAt );
	updateState->effect->InterpolateProps( updateState->oldEffect, timeDelta );
	updateState->lastEnvUpdateAt = millisNow;
}

static void ENV_UpdateSourceEnvironment( src_t *src, const src_t *tryReusePropsSrc,
										 const ListenerProps &listenerProps, int64_t millisNow ) {
	envUpdateState_t *updateState = &src->envUpdateState;

	if( src->priority == SRCPRI_LOCAL ) {
		// Check whether the source has never been updated for this local sound.
		assert( !updateState->nextEnvUpdateAt );
		ENV_UnregisterSource( src );
		return;
	}

	if( src->isLooping ) {
		updateState->nextEnvUpdateAt = (int64_t)( (double)millisNow + 250 + 50 * random() );
	} else {
		// Don't bother updating it after the initial update.
		// This helps to prevent unpleasant effect property transitions, and also acts as a performance optimization.
		if( src->sfx->buffers[src->bufferIndex]->durationMillis < 1000 ) {
			updateState->nextEnvUpdateAt = std::numeric_limits<int64_t>::max();
		} else {
			updateState->nextEnvUpdateAt = (int64_t)( (double)millisNow + 400 + 100 * random() );
		}
	}

	VectorCopy( src->origin, updateState->lastUpdateOrigin );
	VectorCopy( src->velocity, updateState->lastUpdateVelocity );

	updateState->oldEffect = updateState->effect;
	updateState->needsInterpolation = true;

	// Get the leaf num before the update as it is important for all present tests
	updateState->leafNum = S_PointLeafNum( src->origin );

	updateState->effect = ReverbEffectComputer::TryApply( listenerProps, src, tryReusePropsSrc );

	updateState->effect->distanceAtLastUpdate = sqrtf( DistanceSquared( src->origin, listenerProps.origin ) );
	updateState->effect->lastUpdateAt = millisNow;

	if( updateState->needsInterpolation ) {
		ENV_InterpolateEnvironmentProps( src, millisNow );
	}

	// Recycle the old effect
	EffectsAllocator::Instance()->DeleteEffect( updateState->oldEffect );
	updateState->oldEffect = nullptr;

	updateState->effect->BindOrUpdate( src, listenerProps );

	// Prevent reusing an outdated leaf num
	updateState->leafNum = -1;
}

static bool ENV_TryReuseSourceReverbProps( src_t *src, const src_t *tryReusePropsSrc, EaxReverbEffect *newEffect ) {
	if( !tryReusePropsSrc ) {
		return false;
	}

	auto *reuseEffect = tryReusePropsSrc->envUpdateState.effect;
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

void ReverbEffectComputer::SetupDirectObstructionSamplingProps( src_t *src, unsigned minSamples, unsigned maxSamples ) {
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

unsigned ReverbEffectComputer::GetNumSamplesForCurrentQuality( unsigned minSamples, unsigned maxSamples ) {
	float quality = s_environment_sampling_quality->value;

	assert( quality >= 0.0f && quality <= 1.0f );
	assert( minSamples < maxSamples );

	auto numSamples = (unsigned)( minSamples + ( maxSamples - minSamples ) * quality );
	assert( numSamples && numSamples <= maxSamples );
	return numSamples;
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

float ReverbEffectComputer::ComputeDirectObstruction( const ListenerProps &listenerProps, src_t *src ) {
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

EaxReverbEffect *ReverbEffectComputer::TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) {
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

float ReverbEffectComputer::CalcEmissionRadius( const src_t *src ) {
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

void ReverbEffectComputer::ComputeReverberation( const ListenerProps &listenerProps,
												 src_t *src, EaxReverbEffect *effect ) {
	const unsigned numPrimaryRays = GetNumSamplesForCurrentQuality( 16, MAX_REVERB_PRIMARY_RAY_SAMPLES );

	vec3_t primaryRayDirs[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	vec3_t reflectionPoints[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	float primaryHitDistances[MAX_REVERB_PRIMARY_RAY_SAMPLES];

	GenericRaycastSampler::SetupSamplingRayDirs( primaryRayDirs, numPrimaryRays );

	ReverbRaycastSampler sampler( primaryRayDirs, reflectionPoints, primaryHitDistances, src->origin,
								  CalcEmissionRadius( src ), numPrimaryRays );

	sampler.EmitPrimaryRays();

	if( !sampler.numPrimaryHits ) {
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

	EmitSecondaryRays( sampler, listenerProps, src, effect );
}

void ReverbEffectComputer::EmitSecondaryRays( const ReverbRaycastSampler &sampler, const ListenerProps &listenerProps,
											  src_t *src, EaxReverbEffect *effect ) {
	int listenerLeafNum = listenerProps.GetLeafNum();

	vec3_t testedListenerOrigin;
	VectorCopy( listenerProps.origin, testedListenerOrigin );
	// TODO: Use attachment offsets
	testedListenerOrigin[2] += 18.0f;

	auto *const panningUpdateState = &src->panningUpdateState;
	panningUpdateState->numPrimaryRays = sampler.numPrimaryRays;

	trace_t trace;

	unsigned numPassedSecondaryRays = 0;
	panningUpdateState->numPassedSecondaryRays = 0;
	for( unsigned i = 0; i < sampler.numPrimaryHits; i++ ) {
		// Cut off by PVS system early, we are not interested in actual ray hit points contrary to the primary emission.
		if( !S_LeafsInPVS( listenerLeafNum, S_PointLeafNum( sampler.primaryHitPoints[i] ) ) ) {
			continue;
		}

		S_Trace( &trace, sampler.primaryHitPoints[i], testedListenerOrigin, vec3_origin, vec3_origin, MASK_SOLID );
		if( trace.fraction == 1.0f && !trace.startsolid ) {
			numPassedSecondaryRays++;
			float *savedPoint = panningUpdateState->reflectionPoints[panningUpdateState->numPassedSecondaryRays++];
			VectorCopy( sampler.primaryHitPoints[i], savedPoint );
		}
	}

	if( sampler.numPrimaryHits ) {
		float frac = numPassedSecondaryRays / (float)sampler.numPrimaryHits;
		// The secondary rays obstruction is complement to the `frac`
		effect->secondaryRaysObstruction = 1.0f - frac;
	} else {
		// Set minimal feasible values
		effect->secondaryRaysObstruction = 1.0f;
	}
}

void ENV_CalculateSourcePan( const vec3_t listenerOrigin, const mat3_t listenerAxes,
							 const PanningUpdateState *updateState, vec3_t earlyPan, vec3_t latePan ) {
	float earlyPanDir[3] { 0.0f, 0.0f, 0.0f };
	float latePanDir[3] { 0.0f, 0.0f, 0.0f };

	unsigned numAccountedDirs = 0;
	for( unsigned i = 0; i < updateState->numPassedSecondaryRays; ++i ) {
		float dir[3];
		VectorSubtract( listenerOrigin, updateState->reflectionPoints[i], dir );

		float squareDistance = VectorLengthSquared( dir );
		// Do not even take into account directions that have very short segments
		if( squareDistance < 48.0f * 48.0f ) {
			continue;
		}

		numAccountedDirs++;

		const float rcpDistance = Q_RSqrt( squareDistance );
		VectorScale( dir, rcpDistance, dir );

		const float distance         = squareDistance * rcpDistance;
		constexpr float rcpThreshold = 1.0f / REVERB_ENV_DISTANCE_THRESHOLD;
		const float distanceFrac     = wsw::min( 1.0f, distance * rcpThreshold );

		// Give far reflections a priority. Disallow zero values to guarantee that the normalization would succeed.
		const float lateFrac = 0.3f + 0.7f * distanceFrac;
		VectorMA( latePanDir, lateFrac, dir, latePanDir );

		// Give near reflections a priority
		const float earlyFrac = 1.0f - 0.7f * distanceFrac;
		VectorMA( earlyPanDir, earlyFrac, dir, earlyPanDir );
	}

	VectorSet( earlyPan, 0.0f, 0.0f, 0.0f );
	VectorSet( latePan, 0.0f, 0.0f, 0.0f );

	if( numAccountedDirs ) {
		VectorNormalizeFast( earlyPanDir );
		VectorNormalizeFast( latePanDir );

		// Convert to "speakers" coordinate system
		earlyPan[0] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_RIGHT] );
		latePan[0]  = -DotProduct( latePanDir, &listenerAxes[AXIS_RIGHT] );

		// Not sure about "minus" sign in this case...
		// We need something like 9.1 sound system (that has channels distinction in height) to test that
		earlyPan[1] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_UP] );
		latePan[1]  = -DotProduct( latePanDir, &listenerAxes[AXIS_UP] );

		earlyPan[2] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_FORWARD] );
		latePan[2]  = -DotProduct( latePanDir, &listenerAxes[AXIS_FORWARD] );

		// We should be more confident regarding the direction if most of primary rays did yield results
		const float panningStrengthScale = 0.3f * (float)numAccountedDirs * Q_Rcp( (float)updateState->numPrimaryRays );

		VectorScale( earlyPan, panningStrengthScale, earlyPan );
		VectorScale( latePan, panningStrengthScale, latePan );
	}
}

void ENV_CalculatePropagationOrigin( const vec3_t listenerOrigin, const vec3_t realSourceOrigin,
									 float *sourcePitchScale, vec3_t sourceOriginToUse ) {
	// Should be already set to a feasible value
	assert( *sourcePitchScale > 0.0f && *sourcePitchScale <= 1.0f );
	assert( VectorCompare( realSourceOrigin, sourceOriginToUse ) );

	// Provide a fake origin for the source that is at the same distance
	// as the real origin and is aligned to the sound propagation "window"
	// TODO: Precache at least the listener leaf for this sound backend update frame
	if( const int listenerLeaf = S_PointLeafNum( listenerOrigin ) ) {
		if( const int srcLeaf = S_PointLeafNum( listenerOrigin ) ) {
			vec3_t dir;
			float distance;
			if( PropagationTable::Instance()->GetIndirectPathProps( srcLeaf, listenerLeaf, dir, &distance ) ) {
				// The table stores distance using this granularity, so it might be zero
				// for very close leaves. Adding an extra distance won't harm
				// (even if the indirect path length is already larger than the straight euclidean distance).
				distance += 256.0f;
				// Feels better with this multiplier
				distance *= 1.15f;
				// Negate the vector scale multiplier as the dir is an sound influx dir to the listener
				// and we want to shift the origin along the line of the dir but from the listener
				VectorScale( dir, -distance, sourceOriginToUse );
				// Shift the listener origin in `dir` direction for `distance` units
				VectorAdd( listenerOrigin, sourceOriginToUse, sourceOriginToUse );
				const float gainLike = calcSoundGainForDistanceAndAttenuation( distance, ATTN_NORM );
				*sourcePitchScale += ( 1.0f - *sourcePitchScale ) * gainLike;
				// Suppress pitch modification if the difference is small
				if( std::fabs( *sourcePitchScale - 1.0f ) < 0.005f ) {
					*sourcePitchScale = 1.0f;
				}
			}
		}
	}
}