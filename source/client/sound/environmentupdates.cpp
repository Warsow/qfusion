/*
Copyright (C) 2017-2026 vvk2212, Chasseur de bots

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

#include "environmentupdates.h"
#include "sourcemanager.h"
#include "snd_leaf_props_cache.h"
#include "snd_raycast_sampler.h"
#include "snd_propagation.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/stringsplitter.h>
#include <common/helpers/randomgenerator.h>
#include <common/facilities/cvar.h>
#include <common/facilities/gs_public.h>
#include <common/facilities/protocol.h>
#include <common/facilities/sysclock.h>
#include <limits>

struct ListenerProps {
	vec3_t origin;
	vec3_t velocity;
	int entNum { -1 };
	mutable int leafNum { -1 };
	bool isInLiquid { false };

	[[nodiscard]]
	auto getLeafNum() const -> int {
		if( leafNum < 0 ) {
			leafNum = S_PointLeafNum( origin );
		}
		return leafNum;
	}

	void invalidateCachedUpdateState() {
		leafNum = -1;
	}
};

static ListenerProps g_listenerProps;
static wsw::RandomGenerator g_choiceRng;

constexpr const auto MAX_DIRECT_OBSTRUCTION_SAMPLES = 8;
// Almost doubled for "realistic obstruction" (we need more secondary rays)
constexpr const auto MAX_REVERB_PRIMARY_RAY_SAMPLES = 80;

static_assert( PanningUpdateState::MAX_POINTS == MAX_REVERB_PRIMARY_RAY_SAMPLES, "" );

class SourceUpdatePriorityQueue {
public:
	void add( Source *src, float urgencyScale ) {
		assert( urgencyScale >= 0.0f );

		float attenuationScale = src->attenuation * ( 1.0f / 20.0f );
		clamp_high( attenuationScale, 1.0f );
		attenuationScale = Q_Sqrt( attenuationScale );
		assert( attenuationScale >= 0.0f && attenuationScale <= 1.0f );

		const float priorityInQueue = urgencyScale * ( 1.0f - 0.7f * attenuationScale );

		m_heap.emplace_back( { src, priorityInQueue } );
		wsw::push_heap( m_heap.begin(), m_heap.end(), kCmp );
	}
	[[nodiscard]]
	bool empty() const { return m_heap.empty(); }
	[[nodiscard]]
	auto pop() -> std::pair<Source *, float> {
		assert( !empty() );
		wsw::pop_heap( m_heap.begin(), m_heap.end(), kCmp );
		auto result = m_heap.back();
		m_heap.pop_back();
		return result;
	}
private:
	static constexpr auto kCmp = []( const std::pair<Source *, float> &lhs, const std::pair<Source *, float> &rhs ) -> bool {
		return lhs.second < rhs.second;
	};
	wsw::StaticVector<std::pair<Source *, float>, MAX_SRC> m_heap;
};

static void updateSourceEnv( Source *src, const Source *tryReusePropsSrc, const ListenerProps &listenerProps, int64_t millisNow );

static void collectForcedEnvUpdates( Source *srcListHead, SourceUpdatePriorityQueue *priorityQueue, int64_t ) {
	for( Source *src = srcListHead; src; src = src->next ) {
		if( src->priority == SRCPRI_LOCAL ) {
			if( !src->envUpdateState.lastUpdateAt ) {
				priorityQueue->add( src, 1.0f );
			}
		} else {
			priorityQueue->add( src, 1.0f );
		}
	}
}

static void collectRegularEnvUpdates( Source *srcListHead, SourceUpdatePriorityQueue *priorityQueue, int64_t millisNow ) {
	for( Source *src = srcListHead; src; src = src->next ) {
		const EnvUpdateState &updateState = src->envUpdateState;
		if( src->priority == SRCPRI_LOCAL ) {
			// If this source has never been updated, add it to the queue, otherwise skip further updates.
			if( !updateState.lastUpdateAt ) {
				priorityQueue->add( src, 5.0f );
			}
		} else if( !src->isLingering ) {
			if( updateState.nextUpdateAt <= millisNow ) {
				// If the playback has been just added
				if( !updateState.lastUpdateAt ) {
					priorityQueue->add( src, 5.0f );
				} else {
					priorityQueue->add( src, 1.0f );
				}
			} else {
				// If the sound is attached to some (moveable) entity
				// TODO: Is zero ent num the default value?
				if( src->entNum >= 0 ) {
					bool added = false;
					// If the sound origin has been significantly modified
					if( DistanceSquared( src->origin, updateState.lastUpdateOrigin ) > wsw::square( 128.0f ) ) {
						// Hack! Prevent fast-moving entities (that are very likely PG projectiles)
						// to consume the entire updates throughput
						if( VectorLengthSquared( src->velocity ) < wsw::square( 700.0f ) ) {
							priorityQueue->add( src, 1.5f );
							added = true;
						}
					}
					if( !added ) {
						// If the entity velocity has been significantly modified
						if( DistanceSquared( src->velocity, updateState.lastUpdateVelocity ) > wsw::square( 200.0f ) ) {
							priorityQueue->add( src, 1.5f );
						}
					}
				}
			}
		}
	}
}

static void processUpdatePriorityQueue( ListenerProps *listenerProps, SourceUpdatePriorityQueue *priorityQueue, int64_t millisAtUpdateStart ) {
	listenerProps->invalidateCachedUpdateState();

	const SoundSet *lastProcessedSfx = nullptr;
	const Source *lastProcessedSrc    = nullptr;
	float lastProcessedPriority      = std::numeric_limits<float>::max();
	unsigned numPerformedUpdates     = 0;
	bool hasReachedSkipLimit         = false;

	while( !priorityQueue->empty() ) {
		auto [src, priorityInQueue] = priorityQueue->pop();
		if( !hasReachedSkipLimit ) {
			// Suppress expensive time syscalls by checking the counter first
			// (we assume that this number of updates is OK to perform regardless of timings)
			if( numPerformedUpdates > 8 ) {
				hasReachedSkipLimit = Sys_Milliseconds() - millisAtUpdateStart > 2;
			}
		}
		// If we don't care of limits yet or if the source is a high-priority source or it has never been updated
		if( !hasReachedSkipLimit || priorityInQueue >= 1.0f || src->envUpdateState.lastUpdateAt <= 0 ) {
			const Source *tryReusePropsSrc = nullptr;
			if( src->sfx == lastProcessedSfx ) {
				tryReusePropsSrc = lastProcessedSrc;
			}

			assert( lastProcessedPriority >= priorityInQueue );
			lastProcessedPriority = priorityInQueue;
			lastProcessedSfx      = src->sfx;
			lastProcessedSrc      = src;

			updateSourceEnv( src, tryReusePropsSrc, *listenerProps, millisAtUpdateStart );
			numPerformedUpdates++;
		}
	}
}

// This subroutine copies listener origin to origin of relative sounds so we can update their enviroment
// TODO: Respect attachments !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
void updateRelativeSoundsSpatialization( Source *srcListHead, const vec3_t origin, const vec3_t velocity ) {
	for( Source *src = srcListHead; src; src = src->next ) {
		if( src->attenuation == 0.0f ) {
			VectorCopy( origin, src->origin );
			VectorCopy( velocity, src->velocity );
		}
	}
}

static void updatePanning( Source *srcListHead, int listenerEntNum, const vec3_t origin, const mat3_t axes ) {
	for( Source *src = srcListHead; src; src = src->next ) {
		if ( IsEffectActive( src ) ) {
			UpdateSourceEffectPanning( src, listenerEntNum, origin, axes );
		}
	}
}

void updateEnvOfListenerAndSources( Source *srcListHead, int64_t millisNow, int listenerEntNum,
									const vec3_t origin, const vec3_t velocity, const mat3_t axes ) {
	assert( s_environment_effects->integer );

	bool needsForcedUpdate = false;

	updateRelativeSoundsSpatialization( srcListHead, origin, velocity );

	// Check whether we have teleported or entered/left a liquid.
	// Run a forced major update in this case.

	if( DistanceSquared( origin, g_listenerProps.origin ) > 100.0f * 100.0f ) {
		needsForcedUpdate = true;
	} else if( DistanceSquared( velocity, g_listenerProps.velocity ) > 200.0f * 200.0f ) {
		needsForcedUpdate = true;
	}

	// Check the "head" contents. We assume the regular player viewheight.
	vec3_t testedOrigin;
	VectorCopy( origin, testedOrigin );
	testedOrigin[2] += 18;
	int contents = S_PointContents( testedOrigin );

	const bool isListenerInLiquid = ( contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER ) ) != 0;
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
	// Otherwise, cache the queue instance.
	SourceUpdatePriorityQueue priorityQueue;

	if( needsForcedUpdate ) {
		collectForcedEnvUpdates( srcListHead, &priorityQueue, millisNow );
	} else {
		collectRegularEnvUpdates( srcListHead, &priorityQueue, millisNow );
	}

	processUpdatePriorityQueue( &g_listenerProps, &priorityQueue, millisNow );

	// Panning info is dependent of environment one, make sure it is executed last
	updatePanning( srcListHead, listenerEntNum, testedOrigin, axes );
}

[[nodiscard]]
static bool tryReuseSourceReverbProps( Source *src, const Source *tryReusePropsSrc, ReverbEffectProps *effectProps ) {
	if( !tryReusePropsSrc ) {
		return false;
	}

	// If it has been disabled (due to quota limitations)
	if( !IsEffectActive( tryReusePropsSrc ) ) {
		return false;
	}

	const ReverbEffectProps &reuseProps = tryReusePropsSrc->envUpdateState.effectProps;

	// We are already sure that both sources are in the same contents kind (non-liquid).
	// Check distance between sources.
	const float squareDistance = DistanceSquared( tryReusePropsSrc->origin, src->origin );
	// If they are way too far for reusing
	if( squareDistance > wsw::square( 96.0f ) ) {
		return false;
	}

	// If they are very close, feel free to just copy props
	if( squareDistance > wsw::square( 8.0f ) ) {
		// Do a coarse raycast test between these two sources
		vec3_t start, end, dir;
		VectorSubtract( tryReusePropsSrc->origin, src->origin, dir );
		const float rcpDistance = Q_RSqrt( squareDistance );
		VectorScale( dir, rcpDistance, dir );
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

	effectProps->secondaryRaysObstruction = reuseProps.secondaryRaysObstruction;
	effectProps->reverbProps              = reuseProps.reverbProps;
	return true;
}

[[nodiscard]]
static auto computeDirectObstruction( const ListenerProps &listenerProps, wsw::RandomGenerator *choiceRng, Source *src ) -> float;
static void computeReverberation( const ListenerProps &listenerProps_, Source *src, int srcLeafNum, ReverbEffectProps *effectProps );

static void updateSourceEnv( Source *src, const Source *tryReusePropsSrc, const ListenerProps &listenerProps, int64_t millisNow ) {
	EnvUpdateState *const updateState = &src->envUpdateState;

	if( src->priority == SRCPRI_LOCAL ) {
		// Check whether the source has never been updated for this local sound.
		assert( !updateState->nextUpdateAt );
		assert( !updateState->nextUpdateAt );
		disableSourceEffectsAndEnvUpdates( src );
		// Ensure that we won't attempt to update it.
		// Note: We don't exclude SRCPRI_LOCAL sounds at the higher level
		// as we might think of adding effects to such sounds too
		// TODO: It seems it's better to exclude
		assert( updateState->lastUpdateAt == std::numeric_limits<int64_t>::max() );
		assert( updateState->nextUpdateAt == std::numeric_limits<int64_t>::max() );
	} else {
		ReverbEffectProps tmpStorageOfProps;
		const ReverbEffectProps *oldProps = nullptr;
		bool shouldInterpolate            = false;
		if( updateState->lastUpdateAt > 0 ) {
			tmpStorageOfProps  = updateState->effectProps;
			oldProps           = &tmpStorageOfProps;
			shouldInterpolate  = true;
		}

		if( src->isLooping ) {
			updateState->nextUpdateAt = millisNow + 250 + g_choiceRng.nextBoundedFast( 50 );
		} else {
			// Don't bother updating it after the initial update.
			// This helps to prevent unpleasant effect property transitions, and also acts as a performance optimization.
			if( src->sfx->buffers[src->bufferIndex]->durationMillis < 1000 ) {
				updateState->nextUpdateAt = std::numeric_limits<int64_t>::max();
			} else {
				updateState->nextUpdateAt = millisNow + 400 + g_choiceRng.nextBoundedFast( 100 );
			}
		}

		VectorCopy( src->origin, updateState->lastUpdateOrigin );
		VectorCopy( src->velocity, updateState->lastUpdateVelocity );

		updateState->effectProps                = ReverbEffectProps {};
		ReverbEffectProps *const newEffectProps = &updateState->effectProps;

		// Note: direst obstruction should not be reused, as it's extremely position-dependent
		newEffectProps->directObstruction = computeDirectObstruction( listenerProps, &g_choiceRng, src );
		if( tryReuseSourceReverbProps( src, tryReusePropsSrc, &updateState->effectProps ) ) {
			shouldInterpolate = false;
		} else {
			computeReverberation( listenerProps, src, S_PointLeafNum( src->origin ), &updateState->effectProps );
		}

		updateState->distanceAtLastUpdate = sqrtf( DistanceSquared( src->origin, listenerProps.origin ) );

		if( shouldInterpolate ) {
			const int timeDelta = (int)( millisNow - updateState->lastUpdateAt );
			assert( timeDelta > 0 );
			if( const int limit = 350; timeDelta < limit ) {
				const float lerpFrac = Q_Sqrt( (float)timeDelta * Q_Rcp( (float)limit ) );

				newEffectProps->directObstruction = std::lerp( oldProps->directObstruction,
															   newEffectProps->directObstruction, lerpFrac );
				newEffectProps->secondaryRaysObstruction = std::lerp( oldProps->secondaryRaysObstruction,
																	  newEffectProps->secondaryRaysObstruction, lerpFrac );

				interpolateReverbProps( &oldProps->reverbProps, lerpFrac,
										&newEffectProps->reverbProps, &newEffectProps->reverbProps );
			}
		}

		updateState->lastUpdateAt = millisNow;

		UpdateSourceEffectProps( src, updateState->effectProps, listenerProps.origin );
	}
}

[[nodiscard]]
static auto getNumSamplesForCurrentQuality( unsigned minSamples, unsigned maxSamples ) -> unsigned {
	const float quality = s_environment_sampling_quality->value;

	assert( quality >= 0.0f && quality <= 1.0f );
	assert( minSamples < maxSamples );

	auto numSamples = (unsigned)( minSamples + ( maxSamples - minSamples ) * quality );
	assert( numSamples && numSamples <= maxSamples );
	return numSamples;
}

static void setupDirectObstructionSamplingProps( Source *src, wsw::RandomGenerator *choiceRng, unsigned minSamples, unsigned maxSamples ) {
	const float quality        = s_environment_sampling_quality->value;
	SamplingProps *const props = &src->envUpdateState.directObstructionSamplingProps;

	// If the quality is not valid or has been modified since the pattern has been set
	if( props->quality != quality ) {
		const unsigned numSamples = getNumSamplesForCurrentQuality( minSamples, maxSamples );
		props->quality            = quality;
		props->numSamples         = numSamples;
		props->valueIndex         = choiceRng->next();
	}
}

struct DirectObstructionOffsetsHolder {
	static constexpr unsigned NUM_VALUES = 256;
	vec3_t offsets[NUM_VALUES];
	static constexpr float MAX_OFFSET = 20.0f;

	DirectObstructionOffsetsHolder() noexcept {
		wsw::RandomGenerator rng;
		for( float *v: offsets ) {
			for( int i = 0; i < 3; ++i ) {
				v[i] = rng.nextFloat( -MAX_OFFSET, +MAX_OFFSET );
			}
		}
	}
};

static const DirectObstructionOffsetsHolder g_directObstructionOffsetsHolder;

static auto computeDirectObstruction( const ListenerProps &listenerProps, wsw::RandomGenerator *choiceRng, Source *src ) -> float {
	vec3_t testedListenerOrigin;
	VectorCopy( listenerProps.origin, testedListenerOrigin );
	// TODO: We assume standard view height
	testedListenerOrigin[2] += 18.0f;

	const float squareDistance = DistanceSquared( testedListenerOrigin, src->origin );
	// Shortcut for sounds relative to the player
	if( squareDistance < wsw::square( 32.0f ) ) {
		return 0.0f;
	}

	if( !S_LeafsInPVS( listenerProps.getLeafNum(), S_PointLeafNum( src->origin ) ) ) {
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
	trace_t trace;
	S_Trace( &trace, testedListenerOrigin, src->origin, vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
	if( trace.fraction == 1.0f && !trace.startsolid ) {
		// Consider zero obstruction in this case
		return 0.0f;
	}

	setupDirectObstructionSamplingProps( src, &g_choiceRng, 3, MAX_DIRECT_OBSTRUCTION_SAMPLES );

	unsigned numPassedRays       = 0;
	unsigned valueIndex          = src->envUpdateState.directObstructionSamplingProps.valueIndex;
	const unsigned numTestedRays = src->envUpdateState.directObstructionSamplingProps.numSamples;
	for( unsigned rayNum = 0; rayNum < numTestedRays; rayNum++ ) {
		valueIndex = ( valueIndex + 1 ) % DirectObstructionOffsetsHolder::NUM_VALUES;

		vec3_t testedSourceOrigin;
		const float *originOffset = g_directObstructionOffsetsHolder.offsets[ valueIndex ];
		VectorAdd( src->origin, originOffset, testedSourceOrigin );
		S_Trace( &trace, testedListenerOrigin, testedSourceOrigin, vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
		if( trace.fraction == 1.0f && !trace.startsolid ) {
			numPassedRays++;
		}
	}

	return 1.0f - 0.9f * ( (float)numPassedRays / (float)numTestedRays );
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

[[nodiscard]]
static auto calcEmissionRadius( const Source *src ) -> float {
	// Do not even bother casting rays 99999 units ahead for very attenuated sources.
	// However, clamp/normalize the hit distance using the same defined threshold
	float attenuation = src->attenuation;
	if( attenuation <= 1.0f ) {
		return 99999.9f;
	}

	constexpr float attenuationLimit = 10.0f;
	attenuation = wsw::min( attenuation, attenuationLimit );

	const float scale = 4.0f - 3.5f * Q_Sqrt( attenuation * ( 1.0f / attenuationLimit ) );
	assert( scale > 0.0f && scale < 4.0f );
	return scale * REVERB_ENV_DISTANCE_THRESHOLD;
}

static void computeReverberation( const ListenerProps &listenerProps, Source *src, int srcLeafNum, ReverbEffectProps *effectProps ) {
	const unsigned numPrimaryRays = getNumSamplesForCurrentQuality( 16, MAX_REVERB_PRIMARY_RAY_SAMPLES );

	vec3_t primaryRayDirs[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	vec3_t reflectionPoints[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	float primaryHitDistances[MAX_REVERB_PRIMARY_RAY_SAMPLES];

	GenericRaycastSampler::SetupSamplingRayDirs( primaryRayDirs, numPrimaryRays );
	GenericRaycastSampler sampler( primaryRayDirs, reflectionPoints, primaryHitDistances, src->origin,
								   calcEmissionRadius( src ), numPrimaryRays );

	sampler.EmitPrimaryRays();

	if( !sampler.numPrimaryHits ) {
		// Keep existing values (they are valid by default now)
		return;
	}

	// Instead of trying to compute these factors every sampling call,
	// reuse pre-computed properties of CM map leafs that briefly resemble rooms/convex volumes.
	assert( srcLeafNum >= 0 );

	const auto *const leafPropsCache = LeafPropsCache::Instance();
	const LeafProps &leafProps = leafPropsCache->GetPropsForLeaf( srcLeafNum );

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

	interpolateReverbProps( &closedProps, leafProps.getSkyFactor(), &openProps, &effectProps->reverbProps );

	const int listenerLeafNum = listenerProps.getLeafNum();

	vec3_t testedListenerOrigin;
	VectorCopy( listenerProps.origin, testedListenerOrigin );
	// TODO: Use attachment offsets
	testedListenerOrigin[2] += 18.0f;

	auto *const panningUpdateState = &src->panningUpdateState;
	panningUpdateState->numPrimaryRays = sampler.numPrimaryRays;

	unsigned numPassedSecondaryRays = 0;
	panningUpdateState->numPassedSecondaryRays = 0;
	for( unsigned i = 0; i < sampler.numPrimaryHits; i++ ) {
		// Cut off by PVS system early, we are not interested in actual ray hit points contrary to the primary emission.
		if( S_LeafsInPVS( listenerLeafNum, S_PointLeafNum( sampler.primaryHitPoints[i] ) ) ) {
			trace_t trace;
			S_Trace( &trace, sampler.primaryHitPoints[i], testedListenerOrigin, vec3_origin, vec3_origin, MASK_SOLID );
			if( trace.fraction == 1.0f && !trace.startsolid ) {
				numPassedSecondaryRays++;
				float *savedPoint = panningUpdateState->reflectionPoints[panningUpdateState->numPassedSecondaryRays++];
				VectorCopy( sampler.primaryHitPoints[i], savedPoint );
			}
		}
	}

	if( sampler.numPrimaryHits ) {
		const float frac = (float)numPassedSecondaryRays / (float)sampler.numPrimaryHits;
		// The secondary rays obstruction is complement to the `frac`
		effectProps->secondaryRaysObstruction = 1.0f - frac;
	} else {
		// Set minimal feasible values
		effectProps->secondaryRaysObstruction = 1.0f;
	}
}

void calcReverbPan( const vec3_t listenerOrigin, const mat3_t listenerAxes,
					const PanningUpdateState *updateState, vec3_t earlyPan, vec3_t latePan ) {
	float earlyPanDir[3] { 0.0f, 0.0f, 0.0f };
	float latePanDir[3] { 0.0f, 0.0f, 0.0f };

	unsigned numAccountedDirs = 0;
	for( unsigned i = 0; i < updateState->numPassedSecondaryRays; ++i ) {
		float dir[3];
		VectorSubtract( listenerOrigin, updateState->reflectionPoints[i], dir );

		float squareDistance = VectorLengthSquared( dir );
		// Do not even take into account directions that have very short segments
		if( squareDistance > wsw::square( 48.0f ) ) {
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

void calcPropagationOrigin( const vec3_t listenerOrigin, const vec3_t realSourceOrigin,
							float *sourcePitchScale, vec3_t sourceOriginToUse ) {
	// Should be already set to a feasible value
	assert( *sourcePitchScale > 0.0f && *sourcePitchScale <= 1.0f );
	assert( VectorCompare( realSourceOrigin, sourceOriginToUse ) );

	// Provide a fake origin for the source that is at the same distance
	// as the real origin and is aligned to the sound propagation "window"
	// TODO: Precache at least the listener leaf for this sound backend update frame
	if( const int listenerLeaf = S_PointLeafNum( listenerOrigin ) ) {
		if( const int srcLeaf = S_PointLeafNum( realSourceOrigin ) ) {
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