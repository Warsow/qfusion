#include "snd_env_sampler.h"
#include "snd_leaf_props_cache.h"
#include "snd_env_effects.h"
#include "snd_env_sampler.h"
#include "snd_effect_sampler.h"
#include "snd_effects_allocator.h"
#include "snd_propagation.h"
#include <common/helpers/algorithm.h>

#include <limits>

ListenerProps listenerProps;

static_assert( PanningUpdateState::MAX_POINTS == MAX_REVERB_PRIMARY_RAY_SAMPLES, "" );

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

	listenerProps.InvalidateCachedUpdateState();

	ENV_InitGlobalInstances();
}

void ENV_Shutdown() {
	if( !s_environment_effects->integer ) {
		return;
	}

	ENV_ShutdownGlobalInstances();

	listenerProps.InvalidateCachedUpdateState();
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
		alSourcef( src->source, AL_GAIN, clampSourceGain( src->fvol * src->volumeVar->value ) );
	} else {
		alSourcef( src->source, AL_GAIN, clampSourceGain( src->fvol * s_volume->value ) );
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

static SourcesUpdatePriorityQueue sourcesUpdatePriorityQueue;

static void ENV_ProcessUpdatesPriorityQueue();

static void ENV_UpdateSourceEnvironment( src_t *src, int64_t millisNow, const src_t *tryReusePropsSrc );

static inline void ENV_CollectForcedEnvironmentUpdates() {
	src_t *src, *end;

	auto &priorityQueue = ::sourcesUpdatePriorityQueue;

	for( src = srclist, end = srclist + src_count; src != end; ++src ) {
		if( !src->isActive ) {
			continue;
		}
		// Music? TODO: It should not share sources with regular sounds...
		if( !src->sfx ) {
			continue;
		}

		if( src->priority != SRCPRI_LOCAL ) {
			priorityQueue.AddSource( src, 1.0f );
			continue;
		}

		if( !src->envUpdateState.nextEnvUpdateAt ) {
			priorityQueue.AddSource( src, 1.0f );
			continue;
		}
	}
}

static void ENV_CollectRegularEnvironmentUpdates() {
	src_t *src, *end;
	envUpdateState_t *updateState;
	int64_t millisNow;
	int contents;

	millisNow = Sys_Milliseconds();

	auto &priorityQueue = ::sourcesUpdatePriorityQueue;

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
				priorityQueue.AddSource( src, 5.0f );
			}
			continue;
		}

		contents = S_PointContents( src->origin );
		bool wasInLiquid = updateState->isInLiquid;
		updateState->isInLiquid = ( contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER ) ) != 0;
		if( updateState->isInLiquid ^ wasInLiquid ) {
			priorityQueue.AddSource( src, 2.0f );
			continue;
		}

		// Don't update lingering sources environment
		if( src->isLingering ) {
			continue;
		}

		if( updateState->nextEnvUpdateAt <= millisNow ) {
			// If the playback has been just added
			if( !updateState->nextEnvUpdateAt ) {
				priorityQueue.AddSource( src, 5.0f );
			} else {
				priorityQueue.AddSource( src, 1.0f );
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
					priorityQueue.AddSource( src, 1.5f );
				}
				continue;
			}

			// If the entity velocity has been significantly modified
			if( DistanceSquared( src->velocity, updateState->lastUpdateVelocity ) > 200 * 200 ) {
				priorityQueue.AddSource( src, 1.5f );
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

static void ENV_ProcessUpdatesPriorityQueue() {
	const uint64_t micros = Sys_Microseconds();
	const int64_t millis = (int64_t)( micros / 1000 );
	src_t *src;

	listenerProps.InvalidateCachedUpdateState();

	const SoundSet *lastProcessedSfx = nullptr;
	const src_t *lastProcessedSrc = nullptr;
	float lastProcessedPriority = std::numeric_limits<float>::max();
	// Always do at least a single update
	for( ;; ) {
		if( !( src = sourcesUpdatePriorityQueue.PopSource() ) ) {
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

		ENV_UpdateSourceEnvironment( src, millis, tryReusePropsSrc );
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
			if( Effect *effect = src->envUpdateState.effect ) {
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

	if( DistanceSquared( origin, listenerProps.origin ) > 100.0f * 100.0f ) {
		needsForcedUpdate = true;
	} else if( DistanceSquared( velocity, listenerProps.velocity ) > 200.0f * 200.0f ) {
		needsForcedUpdate = true;
	}

	// Check the "head" contents. We assume the regular player viewheight.
	VectorCopy( origin, testedOrigin );
	testedOrigin[2] += 18;
	int contents = S_PointContents( testedOrigin );

	isListenerInLiquid = ( contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER ) ) != 0;
	if( listenerProps.isInLiquid != isListenerInLiquid ) {
		needsForcedUpdate = true;
	}

	VectorCopy( origin, listenerProps.origin );
	VectorCopy( velocity, listenerProps.velocity );
	listenerProps.entNum     = listenerEntNum;
	listenerProps.isInLiquid = isListenerInLiquid;

	// Sanitize the possibly modified cvar before the environment update
	if( s_environment_sampling_quality->value < 0.0f || s_environment_sampling_quality->value > 1.0f ) {
		Cvar_ForceSet( s_environment_sampling_quality->name, "0.5" );
	}

	sourcesUpdatePriorityQueue.Clear();

	if( needsForcedUpdate ) {
		ENV_CollectForcedEnvironmentUpdates();
	} else {
		ENV_CollectRegularEnvironmentUpdates();
	}

	ENV_ProcessUpdatesPriorityQueue();

	// Panning info is dependent of environment one, make sure it is executed last
	ENV_UpdatePanning( Sys_Milliseconds(), listenerEntNum, testedOrigin, axes );
}

static void ENV_InterpolateEnvironmentProps( src_t *src, int64_t millisNow ) {
	auto *updateState = &src->envUpdateState;
	if( !updateState->effect ) {
		return;
	}

	int timeDelta = (int)( millisNow - updateState->lastEnvUpdateAt );
	updateState->effect->InterpolateProps( updateState->oldEffect, timeDelta );
	updateState->lastEnvUpdateAt = millisNow;
}

static void ENV_UpdateSourceEnvironment( src_t *src, int64_t millisNow, const src_t *tryReusePropsSrc ) {
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
		if( src->sfx->bufferDurationMillis[src->bufferIndex] < 1000 ) {
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

	updateState->effect = EffectSamplers::TryApply( listenerProps, src, tryReusePropsSrc );

	updateState->effect->distanceAtLastUpdate = sqrtf( DistanceSquared( src->origin, listenerProps.origin ) );
	updateState->effect->lastUpdateAt = millisNow;

	if( updateState->needsInterpolation ) {
		ENV_InterpolateEnvironmentProps( src, millisNow );
	}

	// Recycle the old effect
	EffectsAllocator::Instance()->DeleteEffect( updateState->oldEffect );
	updateState->oldEffect = nullptr;

	updateState->effect->BindOrUpdate( src );

	// Prevent reusing an outdated leaf num
	updateState->leafNum = -1;
}

