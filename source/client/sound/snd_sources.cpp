/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

#include "snd_local.h"
#include "snd_env_effects.h"
#include "snd_env_sampler.h"
#include <common/helpers/algorithm.h>
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/protocol.h>
#include <common/facilities/sysclock.h>

src_t srclist[MAX_SRC];
int src_count = 0;
static bool src_inited = false;

typedef struct sentity_s {
	vec3_t origin;
	vec3_t velocity;
	mat3_t axis;
} sentity_t;
static sentity_t *entlist = NULL; //[MAX_EDICTS];
static int max_ents;

static void S_AdjustGain( src_t *src ) {
	if( auto *effect = src->envUpdateState.effect ) {
		effect->AdjustGain( src );
		return;
	}

	if( src->volumeVar ) {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * src->volumeVar->value ) );
	} else {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * s_volume->value ) );
	}
}

/*
* source_setup
*/
static void source_setup( src_t *src, const SoundSet *sfx, std::pair<ALuint, unsigned> chosenBufferAndIndex, float chosenPitch,
						  int priority, int entNum, int channel, SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation ) {
	clamp_low( attenuation, 0.0f );

	src->lastUse = Sys_Milliseconds();
	src->sfx = sfx;
	src->bufferIndex = chosenBufferAndIndex.second;
	src->priority = priority;
	src->entNum = entNum;
	src->channel = channel;
	src->attachmentTag = attachmentTag;
	src->fvol = fvol;
	src->chosenPitch = chosenPitch;
	src->attenuation = attenuation;
	src->isActive = true;
	src->isLocked = false;
	src->isLooping = false;
	src->isTracking = false;
	src->isLingering = false;
	src->touchedThisFrame = false;
	src->volumeVar = s_volume;
	VectorClear( src->origin );
	VectorClear( src->velocity );

	alSourcefv( src->source, AL_POSITION, vec3_origin );
	alSourcefv( src->source, AL_VELOCITY, vec3_origin );
	alSourcef( src->source, AL_GAIN, checkSourceGain( fvol * s_volume->value ) );
	alSourcef( src->source, AL_PITCH, chosenPitch );
	alSourcei( src->source, AL_SOURCE_RELATIVE, AL_FALSE );
	alSourcei( src->source, AL_LOOPING, AL_FALSE );
	alSourcei( src->source, AL_BUFFER, (ALint)chosenBufferAndIndex.first );

	alSourcef( src->source, AL_REFERENCE_DISTANCE, kSoundAttenuationRefDistance );
	alSourcef( src->source, AL_MAX_DISTANCE, kSoundAttenuationMaxDistance );
	alSourcef( src->source, AL_ROLLOFF_FACTOR, attenuation );

	ENV_RegisterSource( src );
}

/*
* source_kill
*/
static void source_kill( src_t *src ) {
	int numbufs;
	ALuint source = src->source;
	ALuint buffer;

	if( src->isLocked ) {
		return;
	}

	if( src->isActive ) {
		alSourceStop( source );
	} else {
		// Un-queue all queued buffers
		alGetSourcei( source, AL_BUFFERS_QUEUED, &numbufs );
		while( numbufs-- ) {
			alSourceUnqueueBuffers( source, 1, &buffer );
		}
	}

	// Un-queue all processed buffers
	alGetSourcei( source, AL_BUFFERS_PROCESSED, &numbufs );
	while( numbufs-- ) {
		alSourceUnqueueBuffers( source, 1, &buffer );
	}

	alSourcei( src->source, AL_BUFFER, AL_NONE );

	src->sfx = 0;
	src->lastUse = 0;
	src->priority = 0;
	src->entNum = -1;
	src->channel = -1;
	src->fvol = 1;
	src->isActive = false;
	src->isLocked = false;
	src->isLooping = false;
	src->isTracking = false;
	src->touchedThisFrame = false;
	src->loopIdentifyingToken = 0;

	src->isLingering = false;
	src->lingeringTimeoutAt = 0;

	ENV_UnregisterSource( src );
}

/*
* source_spatialize
*/
static void source_spatialize( src_t *src ) {
	if( src->attenuation == 0.0f ) {
		alSourcei( src->source, AL_SOURCE_RELATIVE, AL_TRUE );
		// this was set at source_setup, no need to redo every frame
		//alSourcefv( src->source, AL_POSITION, vec3_origin );
		//alSourcefv( src->source, AL_VELOCITY, vec3_origin );
		return;
	}

	if( src->isTracking ) {
		assert( src->entNum >= 0 && src->entNum < MAX_EDICTS );
		VectorCopy( entlist[src->entNum].origin, src->origin );
		VectorCopy( entlist[src->entNum].velocity, src->velocity );
		// TODO: Using hardcoded values here, transmit offsets for each frame prior to running updates
		// We have to limit offset to safe values within the regular player box
		switch( src->attachmentTag ) {
			case SoundSystem::OriginAttachment:
				break;
			case SoundSystem::WeaponAttachment:
				VectorMA( src->origin, 15.0f, &entlist[src->entNum].axis[AXIS_FORWARD], src->origin );
				break;
			case SoundSystem::HeadAttachment:
				VectorMA( src->origin, 8.0f, &entlist[src->entNum].axis[AXIS_FORWARD], src->origin );
				src->origin[2] += 18.0f;
				break;
			case SoundSystem::FeetAttachment:
				src->origin[2] -= 15.0f;
				break;
		}
	}

	// Delegate setting source origin to the effect in this case
	if( Effect::Cast<const EaxReverbEffect *>( src->envUpdateState.effect ) ) {
		return;
	}

	// TODO: Track last submitted values, don't rely on AL wrt. reducing switching of states?

	alSourcei( src->source, AL_SOURCE_RELATIVE, AL_FALSE );
	alSourcefv( src->source, AL_POSITION, src->origin );
	alSourcefv( src->source, AL_VELOCITY, src->velocity );
	alSourcef( src->source, AL_PITCH, src->chosenPitch );
}

static void source_loop( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entNum,
						 uintptr_t identifyingToken, SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation ) {
	assert( identifyingToken );

	if( !sfx ) {
		return;
	}

	// TODO: Allow player-local and explicitly positioned global sounds (which are not attached to entities)
	if( entNum < 0 || entNum >= max_ents ) {
		return;
	}

	src_t *existing = nullptr;
	for( int i = 0; i < src_count; ++i ) {
		src_t *const src = &srclist[i];
		if( src->isActive && src->loopIdentifyingToken == identifyingToken ) {
			existing = src;
			break;
		}
	}

	src_t *chosenSrc = existing;
	if( !chosenSrc ) {
		chosenSrc = S_AllocSource( SRCPRI_LOOP, entNum, 0 );
		if( !chosenSrc ) {
			return;
		}
		source_setup( chosenSrc, sfx, bufferAndIndex, pitch, SRCPRI_LOOP, entNum, -1, attachmentTag, fvol, attenuation );
		alSourcei( chosenSrc->source, AL_LOOPING, AL_TRUE );
		chosenSrc->loopIdentifyingToken = identifyingToken;
		chosenSrc->isLooping = true;
	}

	S_AdjustGain( chosenSrc );

	alSourcef( chosenSrc->source, AL_REFERENCE_DISTANCE, kSoundAttenuationRefDistance );
	alSourcef( chosenSrc->source, AL_MAX_DISTANCE, kSoundAttenuationMaxDistance );
	alSourcef( chosenSrc->source, AL_ROLLOFF_FACTOR, attenuation );

	if( !existing ) {
		if( chosenSrc->attenuation > 0.0f ) {
			chosenSrc->isTracking = true;
		}

		source_spatialize( chosenSrc );

		alSourcePlay( chosenSrc->source );
	}

	chosenSrc->touchedThisFrame = true;
}

static void S_ShutdownSourceEFX( src_t *src ) {
	if( src->directFilter ) {
		// Detach the filter from the source
		alSourcei( src->source, AL_DIRECT_FILTER, AL_FILTER_NULL );
		alDeleteFilters( 1, &src->directFilter );
		src->directFilter = 0;
	}

	// TODO: Check whether it is correct in all cases

	if( src->effect && src->effectSlot ) {
		// Detach the effect from the source
		alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, 0 );
		// Detach the effect from the slot
		alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL );
	}

	if( src->effect ) {
		alDeleteEffects( 1, &src->effect );
		src->effect = 0;
	}

	if( src->effectSlot ) {
		alDeleteAuxiliaryEffectSlots( 1, &src->effectSlot );
		src->effectSlot = 0;
	}

	// Suppress errors if any
	(void)alGetError();
}

static bool S_InitSourceEFX( src_t *src ) {
	src->directFilter = 0;
	src->effect       = 0;
	src->effectSlot   = 0;

	(void)alGetError();

	bool succeeded = false;
	do {
		alGenFilters( 1, &src->directFilter );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}

		alFilteri( src->directFilter, AL_FILTER_TYPE, AL_FILTER_LOWPASS );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}

		// Set default filter values (no actual attenuation)
		alFilterf( src->directFilter, AL_LOWPASS_GAIN, 1.0f );
		alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 1.0f );

		// Attach the filter to the source
		alSourcei( src->source, AL_DIRECT_FILTER, src->directFilter );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		alGenEffects( 1, &src->effect );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		alEffecti( src->effect, AL_EFFECT_TYPE, AL_EFFECT_REVERB );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		// Actually disable the reverb effect
		alEffectf( src->effect, AL_REVERB_GAIN, 0.0f );
		alGenAuxiliaryEffectSlots( 1, &src->effectSlot );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		// Attach the effect to the slot
		alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, src->effect );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		// Feed the slot from the source
		alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, src->effectSlot, 0, AL_FILTER_NULL );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}
		succeeded = true;
	} while( false );

	if( !succeeded ) {
		S_ShutdownSourceEFX( src );
	}
	return succeeded;
}

/*
* S_InitSources
*/
bool S_InitSources( int maxEntities, bool verbose ) {
	int i, j, maxSrc = MAX_SRC;
	bool useEfx = s_environment_effects->integer != 0;

	// Although we handle the failure of too many sources/effects allocation,
	// the AL library still prints an error message to stdout and it might be confusing.
	// Limit the number of sources (and attached effects) to this value a-priori.
	// This code also relies on recent versions on the library.
	// There still is a failure if a user tries to load a dated library.
	if ( useEfx && !strcmp( alGetString( AL_VENDOR ), "OpenAL Community" ) ) {
		maxSrc = 64;
	}

	memset( srclist, 0, sizeof( srclist ) );
	src_count = 0;

	// Allocate as many sources as possible
	for( i = 0; i < maxSrc; i++ ) {
		alGenSources( 1, &srclist[i].source );
		if( alGetError() != AL_NO_ERROR ) {
			break;
		}

		if( useEfx ) {
			if( !S_InitSourceEFX( &srclist[i] ) ) {
				if( src_count >= 16 ) {
					// We have created a minimally acceptable sources/effects set.
					// Just delete an orphan source without corresponding effects and stop sources creation.
					alDeleteSources( 1, &srclist[i].source );
					break;
				}

				Com_Printf( S_COLOR_YELLOW "Warning: Cannot create enough sound effects.\n" );
				Com_Printf( S_COLOR_YELLOW "Environment sound effects will be unavailable.\n" );
				Com_Printf( S_COLOR_YELLOW "Make sure you are using the recent OpenAL runtime.\n" );
				Cvar_ForceSet( s_environment_effects->name, "0" );

				// Cleanup already created effects while keeping sources
				for( j = 0; j < src_count; ++j ) {
					S_ShutdownSourceEFX( &srclist[j] );
				}

				// Continue creating sources, now without corresponding effects
				useEfx = false;
			}
		}

		src_count++;
	}

	if( !src_count ) {
		return false;
	}

	if( verbose ) {
		Com_Printf( "allocated %d sources\n", src_count );
	}

	if( maxEntities < 1 ) {
		return false;
	}

	entlist = ( sentity_t * )Q_malloc( sizeof( sentity_t ) * maxEntities );
	max_ents = maxEntities;

	src_inited = true;
	return true;
}

/*
* S_ShutdownSources
*/
void S_ShutdownSources( void ) {
	int i;

	if( !src_inited ) {
		return;
	}

	// Destroy all the sources
	for( i = 0; i < src_count; i++ ) {
		// This call expects that the AL source is valid
		S_ShutdownSourceEFX( &srclist[i] );
		alSourceStop( srclist[i].source );
		alDeleteSources( 1, &srclist[i].source );
	}

	memset( srclist, 0, sizeof( srclist ) );

	Q_free( entlist );
	entlist = NULL;

	src_inited = false;
}

/*
* S_SetEntitySpatialization
*/
void S_SetEntitySpatialization( int entnum, const vec3_t origin, const vec3_t velocity, const mat3_t axis ) {
	sentity_t *sent;

	if( entnum < 0 || entnum > max_ents ) {
		return;
	}

	sent = entlist + entnum;
	VectorCopy( origin, sent->origin );
	VectorCopy( velocity, sent->velocity );
	Matrix3_Copy( axis, sent->axis );
}

/**
* A zombie is a source that still has an "active" flag but AL reports that it is stopped (is completed).
*/
static void S_ProcessZombieSources( src_t **zombieSources, int numZombieSources, int numActiveEffects, int64_t millisNow );

/*
* S_UpdateSources
*/
void S_UpdateSources( void ) {
	ALint state;

	const int64_t millisNow = Sys_Milliseconds();

	src_t *zombieSources[MAX_SRC];
	int numZombieSources = 0;
	int numActiveEffects = 0;

	for( int i = 0; i < src_count; i++ ) {
		src_t *src = &srclist[i];
		if( !src->isActive ) {
			continue;
		}

		if( src->envUpdateState.effect ) {
			numActiveEffects++;
		}

		if( src->isLocked ) {
			continue;
		}

		if( src->volumeVar->modified ) {
			S_AdjustGain( &srclist[i] );
		}

		alGetSourcei( src->source, AL_SOURCE_STATE, &state );
		if( state == AL_STOPPED ) {
			// Do not even bother adding the source to the list of zombie sources in these cases:
			// 1) There's no effect attached
			// 2) There's no sfx attached
			if( !src->envUpdateState.effect || !src->sfx ) {
				source_kill( src );
			} else {
				zombieSources[numZombieSources++] = src;
			}
			src->entNum = -1;
			continue;
		}

		if( src->isLooping ) {
			// If a looping effect hasn't been touched this frame, kill it
			// Note: lingering produces bad results in this case
			if( !src->touchedThisFrame ) {
				// Don't even bother adding this source to a list of zombie sources...
				source_kill( &srclist[i] );
				// Do not misinform zombies processing logic
				if( src->envUpdateState.effect ) {
					numActiveEffects--;
				}
			} else {
				src->touchedThisFrame = false;
			}
		}

		source_spatialize( src );
	}

	S_ProcessZombieSources( zombieSources, numZombieSources, numActiveEffects, millisNow );
}

/**
* A zombie is a source that still has an "active" flag but AL reports that it is stopped (is completed).
*/
static void S_ProcessZombieSources( src_t **zombieSources, int numZombieSources, int numActiveEffects, int64_t millisNow ) {
	// First, kill all sources with expired lingering timeout
	for( int i = 0; i < numZombieSources; ) {
		src_t *const src = zombieSources[i];
		// Adding a source to "zombies" list makes sense only for sources with attached effects
		assert( src->envUpdateState.effect );

		// If the source is not lingering, set the lingering state
		if( !src->isLingering ) {
			src->isLingering = true;
			src->lingeringTimeoutAt = millisNow + src->envUpdateState.effect->GetLingeringTimeout();
			i++;
			continue;
		}

		// If the source lingering timeout has not expired
		if( src->lingeringTimeoutAt > millisNow ) {
			i++;
			continue;
		}

		source_kill( src );
		// Replace the current array cell by the last one, and repeat testing this cell next iteration
		zombieSources[i] = zombieSources[numZombieSources - 1];
		numZombieSources--;
		numActiveEffects--;
	}

	// Now we know an actual number of zombie sources and active effects left.
	// Aside from that, all zombie sources left in list are lingering.

	int effectsNumberThreshold = s_effects_number_threshold->integer;
	if( effectsNumberThreshold < 8 ) {
		effectsNumberThreshold = 8;
		Cvar_ForceSet( s_effects_number_threshold->name, "8" );
	} else if( effectsNumberThreshold > 32 ) {
		effectsNumberThreshold = 32;
		Cvar_ForceSet( s_effects_number_threshold->name, "32" );
	}

	if( numActiveEffects <= effectsNumberThreshold ) {
		return;
	}

	auto zombieSourceComparator = [=]( const src_t *lhs, const src_t *rhs ) {
		// Let sounds that have a lower quality hint be evicted first from the max heap
		// (The natural comparison order uses the opposite sign).
		return lhs->sfx->props.processingQualityHint > rhs->sfx->props.processingQualityHint;
	};

	wsw::make_heap( zombieSources, zombieSources + numZombieSources, zombieSourceComparator );

	// Cache results of Effect::ShouldKeepLingering() calls
	bool keepEffectLingering[MAX_SRC];
	memset( keepEffectLingering, 0, sizeof( keepEffectLingering ) );

	// Prefer copying globals to locals if they're accessed in loop
	// (an access to globals is indirect and is performed via "global relocation table")
	src_t *const srcBegin = srclist;

	for(;; ) {
		if( numActiveEffects <= effectsNumberThreshold ) {
			return;
		}
		if( numZombieSources <= 0 ) {
			break;
		}

		wsw::pop_heap( zombieSources, zombieSources + numZombieSources, zombieSourceComparator );
		src_t *const src = zombieSources[numZombieSources - 1];
		numZombieSources--;

		if( src->envUpdateState.effect->ShouldKeepLingering( src->sfx->props.processingQualityHint, millisNow ) ) {
			keepEffectLingering[src - srcBegin] = true;
			continue;
		}

		source_kill( src );
		numActiveEffects--;
	}

	// Start disabling effects completely.
	// This is fairly slow path but having excessive active effects count is much worse.
	// Note that effects status might have been changed.

	vec3_t listenerOrigin;
	alGetListener3f( AL_POSITION, listenerOrigin + 0, listenerOrigin + 1, listenerOrigin + 2 );

	src_t *disableEffectCandidates[MAX_SRC];
	float sourceScores[MAX_SRC];
	int numDisableEffectCandidates = 0;

	for( src_t *src = srcBegin; src != srcBegin + MAX_SRC; ++src ) {
		if( !src->isActive ) {
			continue;
		}
		if( !src->envUpdateState.effect ) {
			continue;
		}
		// If it was considered to keep effect lingering, do not touch the effect even if we want to disable some
		if( keepEffectLingering[src - srcBegin] ) {
			continue;
		}

		float squareDistance = DistanceSquared( listenerOrigin, src->origin );
		if( squareDistance < 72 * 72 ) {
			continue;
		}

		disableEffectCandidates[numDisableEffectCandidates++] = src;
		float evictionScore = sqrtf( squareDistance );
		evictionScore *= 1.0f / ( 0.5f + src->sfx->props.processingQualityHint );
		// Give looping sources higher priority, otherwise it might sound weird
		// if most of sources become inactive but the looping sound does not have an effect.
		evictionScore *= src->isLooping ? 0.5f : 1.0f;
		sourceScores[src - srcBegin] = evictionScore;
	}

	// Use capture by reference, MSVC tries to capture an array by value and consequently fails
	auto disableEffectComparator = [&]( const src_t *lhs, const src_t *rhs ) {
		// Keep the natural order, a value with greater eviction score should be evicted first
		return sourceScores[lhs - srcBegin] > sourceScores[rhs - srcBegin];
	};

	wsw::make_heap( disableEffectCandidates, disableEffectCandidates + numDisableEffectCandidates, disableEffectComparator );

	for(;; ) {
		if( numActiveEffects <= effectsNumberThreshold ) {
			break;
		}
		if( numDisableEffectCandidates <= 0 ) {
			break;
		}

		wsw::pop_heap( disableEffectCandidates, disableEffectCandidates + numDisableEffectCandidates, disableEffectComparator );
		src_t *src = disableEffectCandidates[numDisableEffectCandidates - 1];
		numDisableEffectCandidates--;

		ENV_UnregisterSource( src );
		numActiveEffects--;
	}
}


/*
* S_AllocSource
*/
src_t *S_AllocSource( int priority, int entNum, int channel ) {
	int i;
	int empty = -1;
	int weakest = -1;
	int64_t weakest_time = Sys_Milliseconds();
	int weakest_priority = priority;

	for( i = 0; i < src_count; i++ ) {
		if( srclist[i].isLocked ) {
			continue;
		}

		if( !srclist[i].isActive && ( empty == -1 ) ) {
			empty = i;
		}

		if( srclist[i].priority < weakest_priority ||
			( srclist[i].priority == weakest_priority && srclist[i].lastUse < weakest_time ) ) {
			weakest_priority = srclist[i].priority;
			weakest_time = srclist[i].lastUse;
			weakest = i;
		}

		// Is it an exact match, and not on channel 0?
		if( ( srclist[i].entNum == entNum ) && ( srclist[i].channel == channel ) && ( channel != 0 ) ) {
			source_kill( &srclist[i] );
			return &srclist[i];
		}
	}

	if( empty != -1 ) {
		return &srclist[empty];
	}

	if( weakest != -1 ) {
		source_kill( &srclist[weakest] );
		return &srclist[weakest];
	}

	return NULL;
}

/*
* S_GetALSource
*/
ALuint S_GetALSource( const src_t *src ) {
	return src->source;
}

/*
* S_StartLocalSound
*/
void S_StartLocalSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, float fvol ) {
	src_t *src = S_AllocSource( SRCPRI_LOCAL, -1, 0 );
	if( !src ) {
		return;
	}

	source_setup( src, sfx, bufferAndIndex, pitch, SRCPRI_LOCAL, -1, 0, SoundSystem::OriginAttachment, fvol, ATTN_NONE );
	alSourcei( src->source, AL_SOURCE_RELATIVE, AL_TRUE );

	alSourcePlay( src->source );
}

/*
* S_StartSound
*/
static void S_StartSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch,
						  const vec3_t origin, int entNum, int channel, SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation ) {
	src_t *src = S_AllocSource( SRCPRI_ONESHOT, entNum, channel );
	if( !src ) {
		return;
	}

	source_setup( src, sfx, bufferAndIndex, pitch, SRCPRI_ONESHOT, entNum, channel, attachmentTag, fvol, attenuation );

	if( src->attenuation ) {
		if( origin ) {
			VectorCopy( origin, src->origin );
		} else {
			src->isTracking = true;
		}
	}

	source_spatialize( src );

	alSourcePlay( src->source );
}

/*
* S_StartFixedSound
*/
void S_StartFixedSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, const vec3_t origin, int channel, float fvol, float attenuation ) {
	S_StartSound( sfx, bufferAndIndex, pitch, origin, 0, channel, SoundSystem::OriginAttachment, fvol, attenuation );
}

/*
* S_StartRelativeSound
*/
void S_StartRelativeSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entnum, int channel, float fvol, float attenuation ) {
	S_StartSound( sfx, bufferAndIndex, pitch, nullptr, entnum, channel, attachmentTag, fvol, attenuation );
}

/*
* S_AddLoopSound
*/
void S_AddLoopSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entnum, uintptr_t identifyingToken, float fvol, float attenuation ) {
	source_loop( sfx, bufferAndIndex, pitch, entnum, identifyingToken, attachmentTag, fvol, attenuation );
}

/*
* S_AllocRawSource
*/
src_t *S_AllocRawSource( int entNum, float fvol, float attenuation, cvar_t *volumeVar ) {
	src_t *src;

	if( !volumeVar ) {
		volumeVar = s_volume;
	}

	src = S_AllocSource( SRCPRI_STREAM, entNum, 0 );
	if( !src ) {
		return NULL;
	}

	source_setup( src, NULL, { 0, 0 }, 1.0f, SRCPRI_STREAM, entNum, 0, SoundSystem::OriginAttachment, fvol, attenuation );

	if( src->attenuation && entNum > 0 ) {
		src->isTracking = true;
	}

	src->volumeVar = volumeVar;
	S_AdjustGain( src );

	source_spatialize( src );
	return src;
}

/*
* S_StopAllSources
*/
void S_StopAllSources( bool retainLocal ) {
	for( int i = 0; i < src_count; i++ ) {
		// TODO: Perform a better distinction of local sounds
		if( !retainLocal || srclist[i].attenuation != ATTN_NONE ) {
			source_kill( &srclist[i] );
		}
	}
}
