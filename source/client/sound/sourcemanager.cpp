/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)
Copyright (C) 2017-2026 Chasseur de bots

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

#include "sourcemanager.h"
#include "environmentupdates.h"
#include "snd_local.h"

#include <common/helpers/algorithm.h>
#include <common/helpers/links.h>
#include <common/helpers/scopeexitaction.h>
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/protocol.h>
#include <common/facilities/sysclock.h>

SourceManager::SourceManager() {
	unsigned maxSources;
	if( s_environment_effects->integer ) {
		// Don't even try allocating that much
		maxSources = kMaxSources / 2;
	} else {
		maxSources = kMaxSources;
	}
	while( m_cachedHandles.size() != maxSources ) {
		CachedHandles handles;
		if( createHandles( &handles ) ) {
			m_cachedHandles.push_back( handles );
		} else {
			break;
		}
	}
	while( !m_cachedStreamHandles.full() ) {
		CachedStreamHandles handles;
		if( createStreamHandles( &handles ) ) {
			m_cachedStreamHandles.push_back( handles );
		} else {
			break;
		}
	}
}

SourceManager::~SourceManager() {
	for( Source *source = m_activeSourcesHead, *next; source; source = next ) { next = source->next;
		killSource( source );
	}
	for( StreamSource *source = m_streamSourcesHead, *next; source; source = next ) { next = source->next;
		killStreamSource( source );
	}
	for( CachedHandles &handles: m_cachedHandles ) {
		destroyHandles( &handles );
	}
	for( CachedStreamHandles &handles: m_cachedStreamHandles ) {
		destroyStreamHandles( &handles );
	}
}

void setupSourceEffectsAndEnvUpdates( Source *src ) {
	assert( s_environment_effects->integer );

	src->envUpdateState.lastUpdateAt = 0;
	// Force an immediate update
	src->envUpdateState.nextUpdateAt = 0;
	// Reset sampling patterns by setting an illegal quality value
	src->envUpdateState.directObstructionSamplingProps.quality = -1.0f;

	src->effectActive = true;
}

void disableSourceEffectsAndEnvUpdates( Source *src ) {
	assert( s_environment_effects->integer );

	// Prevent later occasional updates
	src->envUpdateState.lastUpdateAt = std::numeric_limits<int64_t>::max();
	src->envUpdateState.nextUpdateAt = std::numeric_limits<int64_t>::max();

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

	src->effectActive = false;
}

static void adjustGain( Source *src ) {
	if( src->volumeVar ) {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * src->volumeVar->value ) );
	} else {
		alSourcef( src->source, AL_GAIN, checkSourceGain( src->fvol * s_volume->value ) );
	}
}

void SourceManager::setupSource( Source *src, const SoundSet *sfx, std::pair<ALuint, unsigned> chosenBufferAndIndex,
								 float chosenPitch, int priority, int entNum, int channel,
								 SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation ) {
	attenuation = wsw::max( 0.0f, attenuation );

	src->spawnedAt     = Sys_Milliseconds();
	src->sfx           = sfx;
	src->bufferIndex   = chosenBufferAndIndex.second;
	src->priority      = priority;
	src->entNum        = entNum;
	src->channel       = channel;
	src->attachmentTag = attachmentTag;
	src->fvol          = fvol;
	src->chosenPitch   = chosenPitch;
	src->attenuation   = attenuation;
	src->volumeVar     = s_volume;

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

	if( s_environment_effects->integer ) {
		setupSourceEffectsAndEnvUpdates( src );
	}

	src->hasPendingPlayCall = true;
}

void SourceManager::killSource( Source *src ) {
	ALuint buffer = 0;
	ALint numbufs = 0;

	alSourceStop( src->source );

	// Un-queue all processed buffers
	alGetSourcei( src->source, AL_BUFFERS_PROCESSED, &numbufs );
	assert( numbufs >= 0 );
	while( numbufs ) {
		alSourceUnqueueBuffers( src->source, 1, &buffer );
		numbufs--;
	}

	alSourcei( src->source, AL_BUFFER, AL_NONE );

	if( s_environment_effects->integer ) {
		disableSourceEffectsAndEnvUpdates( src );
	}

	m_cachedHandles.push_back( CachedHandles {
		.source       = src->source,
		.directFilter = src->directFilter,
		.effect       = src->effect,
		.effectSlot   = src->effectSlot,
	});

	wsw::unlink( src, &m_activeSourcesHead );
	src->~Source();
	m_sourceAllocator.free( src );
}

void SourceManager::killStreamSource( StreamSource *src ) {
	stopStreamSource( src );

	m_cachedStreamHandles.push_back( CachedStreamHandles { .source = src->source } );

	wsw::unlink( src, &m_streamSourcesHead );
	src->~StreamSource();
	m_streamSourceAllocator.free( src );
}

void SourceManager::updateSpatialParams( Source *src ) {
	if( src->attenuation == 0.0f ) {
		// TODO: Do this only on the initial launch
		alSourcei( src->source, AL_SOURCE_RELATIVE, AL_TRUE );
		// this was set at source_setup, no need to redo every frame
		//alSourcefv( src->source, AL_POSITION, vec3_origin );
		//alSourcefv( src->source, AL_VELOCITY, vec3_origin );
		return;
	}

	if( src->isTracking ) {
		assert( src->entNum >= 0 && src->entNum < MAX_EDICTS );
		VectorCopy( m_entitySpatialParams[src->entNum].origin, src->origin );
		VectorCopy( m_entitySpatialParams[src->entNum].velocity, src->velocity );
		// TODO: Using hardcoded values here, transmit offsets for each frame prior to running updates
		// We have to limit offset to safe values within the regular player box
		switch( src->attachmentTag ) {
			case SoundSystem::OriginAttachment:
				break;
			case SoundSystem::WeaponAttachment:
				VectorMA( src->origin, 15.0f, &m_entitySpatialParams[src->entNum].axis[AXIS_FORWARD], src->origin );
				break;
			case SoundSystem::HeadAttachment:
				VectorMA( src->origin, 8.0f, &m_entitySpatialParams[src->entNum].axis[AXIS_FORWARD], src->origin );
				src->origin[2] += 18.0f;
				break;
			case SoundSystem::FeetAttachment:
				src->origin[2] -= 15.0f;
				break;
		}
	}

	// Delegate setting source origin to the effect in this case
	if( !IsEffectActive( src ) ) {
		// TODO: Track last submitted values, don't rely on AL wrt. reducing switching of states?
		alSourcei( src->source, AL_SOURCE_RELATIVE, AL_FALSE );
		alSourcefv( src->source, AL_POSITION, src->origin );
		alSourcefv( src->source, AL_VELOCITY, src->velocity );
		alSourcef( src->source, AL_PITCH, src->chosenPitch );
	}
}

void SourceManager::touchLoopSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag,
									std::pair<ALuint, unsigned> bufferAndIndex,
									float pitch, int entNum, uintptr_t identifyingToken, float fvol, float attenuation ) {
	assert( sfx );
	assert( identifyingToken );

	// TODO: Allow player-local and explicitly positioned global sounds (which are not attached to entities)
	if( entNum < 0 || entNum >= MAX_EDICTS ) {
		return;
	}

	Source *existing = nullptr;
	for( Source *src = m_activeSourcesHead; src; src = src->next ) {
		if( src->loopIdentifyingToken == identifyingToken ) {
			existing = src;
			break;
		}
	}

	Source *chosenSrc = existing;
	if( !chosenSrc ) {
		const int priority = SRCPRI_LOOP;
		chosenSrc = allocSource( priority, entNum, 0 );
		if( !chosenSrc ) {
			return;
		}
		setupSource( chosenSrc, sfx, bufferAndIndex, pitch, priority, entNum, -1, attachmentTag, fvol, attenuation );
		chosenSrc->loopIdentifyingToken = identifyingToken;
		chosenSrc->isLooping            = true;
		alSourcei( chosenSrc->source, AL_LOOPING, AL_TRUE );
	}

	adjustGain( chosenSrc );

	alSourcef( chosenSrc->source, AL_REFERENCE_DISTANCE, kSoundAttenuationRefDistance );
	alSourcef( chosenSrc->source, AL_MAX_DISTANCE, kSoundAttenuationMaxDistance );
	alSourcef( chosenSrc->source, AL_ROLLOFF_FACTOR, attenuation );

	if( !existing ) {
		if( chosenSrc->attenuation > 0.0f ) {
			chosenSrc->isTracking = true;
		}

		// TODO: When do we update params of existing sounds?
		updateSpatialParams( chosenSrc );
	}

	chosenSrc->touchedThisFrame = true;
}

void SourceManager::destroyHandles( CachedHandles *handles ) {
	if( handles->directFilter ) {
		// Detach the filter from the source
		alSourcei( handles->source, AL_DIRECT_FILTER, AL_FILTER_NULL );
		alDeleteFilters( 1, &handles->directFilter );
		handles->directFilter = 0;
	}

	// TODO: Check whether it is correct in all cases

	if( handles->effect && handles->effectSlot ) {
		// Detach the effect from the source
		alSource3i( handles->source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, 0 );
		// Detach the effect from the slot
		alAuxiliaryEffectSloti( handles->effectSlot, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL );
	}

	if( handles->effect ) {
		alDeleteEffects( 1, &handles->effect );
		handles->effect = 0;
	}

	if( handles->effectSlot ) {
		alDeleteAuxiliaryEffectSlots( 1, &handles->effectSlot );
		handles->effectSlot = 0;
	}

	if( handles->source ) {
		alDeleteSources( 1, &handles->source );
		handles->source = 0;
	}

	// Suppress errors if any
	(void)alGetError();
}

bool SourceManager::createHandles( CachedHandles *handles ) {
	*handles = CachedHandles {};
	assert( !handles->source && !handles->directFilter && !handles->effect && !handles->effectSlot );
	
	(void)alGetError();

	bool succeeded = false;
	do {
		alGenSources( 1, &handles->source );
		if( alGetError() != AL_NO_ERROR ) { break; }
		
		if( !s_environment_effects->integer ) {
			succeeded = true;
			break;
		}
		
		alGenFilters( 1, &handles->directFilter );
		if( alGetError() != AL_NO_ERROR ) { break; }

		alFilteri( handles->directFilter, AL_FILTER_TYPE, AL_FILTER_LOWPASS );
		if( alGetError() != AL_NO_ERROR ) { break; }

		// Set default filter values (no actual attenuation)
		alFilterf( handles->directFilter, AL_LOWPASS_GAIN, 1.0f );
		alFilterf( handles->directFilter, AL_LOWPASS_GAINHF, 1.0f );

		// Attach the filter to the source
		alSourcei( handles->source, AL_DIRECT_FILTER, handles->directFilter );
		if( alGetError() != AL_NO_ERROR ) { break; }

		alGenEffects( 1, &handles->effect );
		if( alGetError() != AL_NO_ERROR ) { break; }
		alEffecti( handles->effect, AL_EFFECT_TYPE, AL_EFFECT_EAXREVERB );
		if( alGetError() != AL_NO_ERROR ) { break; }

		// Actually disable the reverb effect
		alEffectf( handles->effect, AL_REVERB_GAIN, 0.0f );
		alGenAuxiliaryEffectSlots( 1, &handles->effectSlot );
		if( alGetError() != AL_NO_ERROR ) { break; }

		// Attach the effect to the slot
		alAuxiliaryEffectSloti( handles->effectSlot, AL_EFFECTSLOT_EFFECT, handles->effect );
		if( alGetError() != AL_NO_ERROR ) { break; }

		// Feed the slot from the source
		alSource3i( handles->source, AL_AUXILIARY_SEND_FILTER, handles->effectSlot, 0, AL_FILTER_NULL );
		if( alGetError() != AL_NO_ERROR ) { break; }

		succeeded = true;
	} while( false );

	if( !succeeded ) {
		destroyHandles( handles );
	}
	return succeeded;
}

bool SourceManager::createStreamHandles( CachedStreamHandles *handles ) {
	*handles = CachedStreamHandles {};

	(void)alGetError();

	ALuint source = 0;
	alGenSources( 1, &source );

	if( alGetError() != AL_NO_ERROR ) {
		return false;
	}

	handles->source = source;
	return true;
}

void SourceManager::destroyStreamHandles( CachedStreamHandles *handles ) {
	alDeleteSources( 1, &handles->source );
	*handles = CachedStreamHandles {};
}

void SourceManager::updateRegularSources( int64_t millisNow, const float *listenerOrigin ) {
	// A zombie is a source that still has an "active" flag but AL reports that it is stopped (is completed).

	Source *zombieSources[kMaxSources];
	unsigned numZombieSources = 0;
	unsigned numActiveEffects = 0;

	for( Source *src = m_activeSourcesHead, *next; src; src = next ) { next = src->next;
		if( IsEffectActive( src ) ) {
			numActiveEffects++;
		}

		if( src->volumeVar->modified ) {
			adjustGain( src );
		}

		if( src->hasPendingPlayCall ) {
			updateSpatialParams( src );
		} else {
			ALint state;
			alGetSourcei( src->source, AL_SOURCE_STATE, &state );
			if( state == AL_STOPPED ) {
				// Do not even bother adding the source to the list of zombie sources in these cases:
				// 1) There's no effect attached
				// 2) There's no sfx attached
				if( !IsEffectActive( src ) || !src->sfx ) {
					killSource( src );
				} else {
					zombieSources[numZombieSources++] = src;
				}
				// Note: It turns out that it's better to keep it attached to the source
				// (it's very important for POV-attached sounds which are not necessarily AL_SOURCE_RELATIVE)
				// src->entNum = -1;
			} else {
				bool isKept = true;
				if( src->isLooping ) {
					// If a looping effect hasn't been touched this frame, kill it
					// Note: lingering produces bad results in this case
					if( !src->touchedThisFrame ) {
						const bool wasEffectActive = IsEffectActive( src );
						// Don't even bother adding this source to a list of zombie sources...
						killSource( src );
						// Do not misinform zombies processing logic
						if( wasEffectActive ) {
							numActiveEffects--;
						}
						isKept = false;
					} else {
						src->touchedThisFrame = false;
					}
				}
				if( isKept ) {
					updateSpatialParams( src );
				}
			}
		}
	}

	processZombieSources( millisNow, listenerOrigin, zombieSources, numZombieSources, numActiveEffects );
}

void SourceManager::startPendingRegularSources() {
	for( Source *src = m_activeSourcesHead; src; src = src->next ) {
		if( src->hasPendingPlayCall ) {
			src->hasPendingPlayCall = false;
			alSourcePlay( src->source );
		}
	}
}

void SourceManager::processZombieSources( int64_t millisNow, const float *listenerOrigin, Source **zombieSources,
										  unsigned numZombieSources, unsigned numActiveEffects ) {
	// First, kill all sources with expired lingering timeout
	for( unsigned i = 0; i < numZombieSources; ) {
		Source *const src = zombieSources[i];
		// Adding a source to "zombies" list makes sense only for sources with attached effects
		assert( IsEffectActive( src ) );

		// If the source is not lingering, set the lingering state
		if( !src->isLingering ) {
			src->isLingering        = true;
			src->lingeringTimeoutAt = millisNow + (int)( src->envUpdateState.effectProps.reverbProps.decayTime * 1000 ) + 50;
			i++;
		} else {
			// If the source lingering timeout has not expired
			if( src->lingeringTimeoutAt > millisNow ) {
				i++;
			} else {
				killSource( src );
				// Replace the current array cell by the last one, and repeat testing this cell next iteration
				zombieSources[i] = zombieSources[numZombieSources - 1];
				numZombieSources--;
				numActiveEffects--;
			}
		}
	}

	// Now we know an actual number of zombie sources and active effects left.
	// Aside from that, all zombie sources left in list are lingering.

	// TODO: Use declared var wrappers
	auto effectNumberThreshold = (unsigned)s_effects_number_threshold->integer;
	if( effectNumberThreshold < 8 ) {
		effectNumberThreshold = 8;
		Cvar_ForceSet( s_effects_number_threshold->name, "8" );
	} else if( effectNumberThreshold > 32 ) {
		effectNumberThreshold = 32;
		Cvar_ForceSet( s_effects_number_threshold->name, "32" );
	}

	if( numActiveEffects > effectNumberThreshold ) {
		numActiveEffects = killZombieSources( zombieSources, numZombieSources, numActiveEffects, effectNumberThreshold );
		if( numActiveEffects > effectNumberThreshold ) {
			// Start disabling effects completely.
			// This is fairly slow path but having excessive active effects count is much worse.
			// Note that effects status might have been changed.
			disableExcessiveEffects( listenerOrigin, numActiveEffects, effectNumberThreshold );
		}
	}
}

[[nodiscard]]
static bool shouldKeepLingering( const EnvUpdateState &updateState, float sourceQualityHint ) {
	if( sourceQualityHint <= 0 ) {
		return false;
	}
	const ReverbEffectProps &effectProps = updateState.effectProps;
	clamp_high( sourceQualityHint, 1.0f );
	float factor = 0.5f * sourceQualityHint;
	factor += 0.25f * ( ( 1.0f - effectProps.directObstruction ) + ( 1.0f - effectProps.secondaryRaysObstruction ) );
	assert( factor >= 0.0f && factor <= 1.0f );
	// TODO: Calc gain for attenuation?
	return updateState.distanceAtLastUpdate < 192.0f + 768.0f * factor;
}

auto SourceManager::killZombieSources( Source **zombieSources, unsigned numZombieSources,
									   unsigned numActiveEffects, unsigned effectNumberThreshold ) -> unsigned {
	assert( numActiveEffects > effectNumberThreshold );

	const auto cmpZombieSources = [=]( const Source *lhs, const Source *rhs ) {
		// Let sounds that have a lower quality hint be evicted first from the max heap
		// (The natural comparison order uses the opposite sign).
		return lhs->sfx->props.processingQualityHint > rhs->sfx->props.processingQualityHint;
	};

	wsw::make_heap( zombieSources, zombieSources + numZombieSources, cmpZombieSources );

	while( numActiveEffects > effectNumberThreshold && numZombieSources > 0 ) {
		wsw::pop_heap( zombieSources, zombieSources + numZombieSources, cmpZombieSources );
		Source *const src = zombieSources[numZombieSources - 1];
		numZombieSources--;

		// TODO: Use a better signature
		if( !shouldKeepLingering( src->envUpdateState, src->sfx->props.processingQualityHint ) ) {
			killSource( src );
			numActiveEffects--;
		}
	}

	return numActiveEffects;
}

void SourceManager::disableExcessiveEffects( const float *listenerOrigin, unsigned numActiveEffects,
											 unsigned effectNumberThreshold ) {
	assert( numActiveEffects > effectNumberThreshold );

	Source *disableEffectCandidates[kMaxSources];
	float sourceScores[kMaxSources];
	int numDisableEffectCandidates = 0;

	for( Source *src = m_activeSourcesHead; src; src = src->next ) {
		if( IsEffectActive( src ) ) {
			const float squareDistance = DistanceSquared( listenerOrigin, src->origin );
			if( squareDistance > wsw::square( 72.0f ) ) {
				disableEffectCandidates[numDisableEffectCandidates++] = src;
				float evictionScore = Q_Sqrt( squareDistance );
				evictionScore *= Q_Rcp( 0.5f + src->sfx->props.processingQualityHint );
				// Give looping sources higher priority, otherwise it might sound weird
				// if most of sources become inactive but the looping sound does not have an effect.
				evictionScore *= src->isLooping ? 0.5f : 1.0f;
				sourceScores[src->index] = evictionScore;
			}
		}
	}

	// Use capture by reference, MSVC tries to capture an array by value and therefore fails
	const auto cmpCandidate = [&]( const Source *lhs, const Source *rhs ) {
		// Keep the natural order, a value with greater eviction score should be evicted first
		return sourceScores[lhs->index] > sourceScores[rhs->index];
	};

	wsw::make_heap( disableEffectCandidates, disableEffectCandidates + numDisableEffectCandidates, cmpCandidate );

	while( numActiveEffects > effectNumberThreshold && numDisableEffectCandidates > 0 ) {
		wsw::pop_heap( disableEffectCandidates, disableEffectCandidates + numDisableEffectCandidates, cmpCandidate );
		Source *src = disableEffectCandidates[numDisableEffectCandidates - 1];
		numDisableEffectCandidates--;

		disableSourceEffectsAndEnvUpdates( src );
		numActiveEffects--;
	}
}

auto SourceManager::getStreamSource( uintptr_t tag ) -> StreamSource * {
	for( StreamSource *source = m_streamSourcesHead; source; source = source->next ) {
		if( source->tag == tag ) {
			return source;
		}
	}

	return allocStreamSource( tag );
}

void SourceManager::updateStreamSources() {
	// Note: Contrary to regular sources, we don't destroy stopped stream sources
	for( StreamSource *source = m_streamSourcesHead; source; source = source->next ) {
		const unsigned processedMsec = drainProcessedSamples( source );
		if( source->queuedSamplesMsec >= processedMsec ) {
			source->queuedSamplesMsec -= processedMsec;
		} else {
			source->queuedSamplesMsec = 0;
		}
	}
}

void SourceManager::stopStreamSources() {
	for( StreamSource *source = m_streamSourcesHead; source; source = source->next ) {
		stopStreamSource( source );
	}
}

void SourceManager::stopStreamSource( StreamSource *src ) {
	alSourceStop( src->source );
	(void)drainProcessedSamples( src );
	alSourcei( src->source, AL_BUFFER, AL_NONE );
	src->queuedSamplesMsec = 0;
}

auto SourceManager::drainProcessedSamples( StreamSource *src ) const -> unsigned {
	ALint numBuffers      = 0;
	unsigned resultMillis = 0;
	alGetSourcei( src->source, AL_BUFFERS_PROCESSED, &numBuffers );
	assert( numBuffers >= 0 );
	while( numBuffers ) {
		ALuint buffer = 0;
		alSourceUnqueueBuffers( src->source, 1, &buffer );
		resultMillis += S_GetBufferLength( buffer );
		alDeleteBuffers( 1, &buffer );
		numBuffers--;
	}
	return resultMillis;
}

void SourceManager::pushStreamSamples( StreamSource *src, unsigned samples, unsigned rate, unsigned width, unsigned channels, const uint8_t *data, float volume ) {
	ALuint buffer = 0;
	[[maybe_unused]] wsw::ScopeExitAction destroyBuffer( [&] { alDeleteBuffers( 1, &buffer ); });

	alGenBuffers( 1, &buffer );
	ALenum error = 0;
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		return;
	}

	const ALuint format = S_SoundFormat( width, channels );

	alBufferData( buffer, format, data, ( samples * width * channels ), rate );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		return;
	}

	alSourceQueueBuffers( src->source, 1, &buffer );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		return;
	}

	src->queuedSamplesMsec += (ALuint)( (ALfloat)samples * 1000.0 / rate + 0.5f );

	alSourcef( src->source, AL_GAIN, checkSourceGain( volume ) );

	ALint state = 0;
	alGetSourcei( src->source, AL_SOURCE_STATE, &state );
	if( state != AL_PLAYING ) {
		alSourcePlay( src->source );
	}

	destroyBuffer.cancel();
}

auto SourceManager::allocSource( int priority, int entNum, int channel ) -> Source * {
	const int64_t millisNow = Sys_Milliseconds();

	const auto doAllocSource = [&,this]() -> Source * {
		assert( !m_cachedHandles.empty() );
		assert( !m_sourceAllocator.isFull() );

		CachedHandles handles = m_cachedHandles.back();
		m_cachedHandles.pop_back();

		unsigned index = 0;
		void *const mem = m_sourceAllocator.allocOrNull( &index );

		auto *source         = new( mem )Source;
		source->index        = index;
		source->spawnedAt    = millisNow;
		source->source       = handles.source;
		source->effect       = handles.effect;
		source->effectSlot   = handles.effectSlot;
		source->directFilter = handles.directFilter;

		wsw::link( source, &m_activeSourcesHead );

		return source;
	};

	// If the channel is specified, try finding a matching source for these entity number and channel, and overwrite it
	if( channel != 0 ) {
		for( Source *source = m_activeSourcesHead; source; source = source->next ) {
			if( source->entNum == entNum && source->channel == channel ) {
				killSource( source );
				// We are sure that we can allocate a new source
				// (note that it's address does not necessarily match the found one)
				return doAllocSource();
			}
		}
	}

	if( !m_cachedHandles.empty() ) {
		return doAllocSource();
	}

	int64_t worstTime   = millisNow;
	Source *worstSource = nullptr;
	int worstPriority   = priority;

	for( Source *source = m_activeSourcesHead; source; source = source->next ) {
		if( source->priority < worstPriority || ( source->priority == worstPriority && source->spawnedAt < worstTime ) ) {
			worstSource   = source;
			worstPriority = source->priority;
			worstTime     = source->spawnedAt;
		}
	}

	if( worstSource ) {
		killSource( worstSource );
		assert( !m_cachedHandles.empty() );
		assert( !m_sourceAllocator.isFull() );
		return doAllocSource();
	}

	return nullptr;
}

auto SourceManager::allocStreamSource( uintptr_t tag ) -> StreamSource * {
	if( !m_cachedStreamHandles.empty() ) {
		assert( !m_streamSourceAllocator.isFull() );

		CachedStreamHandles handles = m_cachedStreamHandles.back();
		m_cachedStreamHandles.pop_back();

		auto *src   = new( m_streamSourceAllocator.allocOrNull() )StreamSource;
		src->tag    = tag;
		src->source = handles.source;

		wsw::link( src, &m_streamSourcesHead );
		return src;
	}

	return nullptr;
}

void SourceManager::startLocalSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, float fvol ) {
	const int priority = SRCPRI_LOCAL;
	const int entNum   = -1;
	const int channel  = 0;

	if( Source *const src = allocSource( priority, entNum, channel ) ) {
		setupSource( src, sfx, bufferAndIndex, pitch, priority, entNum, channel, SoundSystem::OriginAttachment, fvol, ATTN_NONE );

		alSourcei( src->source, AL_SOURCE_RELATIVE, AL_TRUE );
	}
}

void SourceManager::startOneshotSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch,
									   const float *origin, int entNum, int channel,
									   SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation ) {
	const int priority = SRCPRI_ONESHOT;
	if( Source *const src = allocSource( priority, entNum, channel ) ) {
		setupSource( src, sfx, bufferAndIndex, pitch, priority, entNum, channel, attachmentTag, fvol, attenuation );

		if( attenuation > 0.0f ) {
			if( origin ) {
				VectorCopy( origin, src->origin );
			} else {
				src->isTracking = true;
			}
		}

		updateSpatialParams( src );
	}
}

void SourceManager::startFixedSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex,
									 float pitch, const vec3_t origin, int channel, float fvol, float attenuation ) {
	startOneshotSound( sfx, bufferAndIndex, pitch, origin, 0, channel, SoundSystem::OriginAttachment, fvol, attenuation );
}

void SourceManager::startRelativeSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag,
										std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entnum,
										int channel, float fvol, float attenuation ) {
	startOneshotSound( sfx, bufferAndIndex, pitch, nullptr, entnum, channel, attachmentTag, fvol, attenuation );
}

void SourceManager::stopAllRegularSources( bool retainLocal ) {
	for( Source *src = m_activeSourcesHead, *next; src; src = next ) { next = src->next;
		// TODO: Identify local sounds in an explicit fashion
		if( !retainLocal || src->attenuation != ATTN_NONE ) {
			killSource( src );
		}
	}
}

bool IsEffectActive( const Source *src ) {
	return src->effectActive;
}

[[maybe_unused]]
static void printReverbProps( const EfxReverbProps &props ) {
	sNotice() << "====================== :" << Sys_Milliseconds();
	sNotice() << "Density                :" << props.density;
	sNotice() << "Diffusion              :" << props.diffusion;
	sNotice() << "Decay time             :" << props.decayTime;
	sNotice() << "Decay HF Ratio         :" << props.decayHfRatio;
	sNotice() << "Decay LF Ratio         :" << props.decayLfRatio;
	sNotice() << "Gain                   :" << props.gain;
	sNotice() << "Gain HF                :" << props.gainHf;
	sNotice() << "Gain LF                :" << props.gainLf;
	sNotice() << "Reflections gain       :" << props.reflectionsGain;
	sNotice() << "Reflections delay      :" << props.reflectionsDelay;
	sNotice() << "Late reverb gain       :" << props.lateReverbGain;
	sNotice() << "Late reverb delay      :" << props.lateReverbDelay;
	sNotice() << "Echo time              :" << props.echoTime;
	sNotice() << "Echo depth             :" << props.echoDepth;
	sNotice() << "Modulation time        :" << props.modulationTime;
	sNotice() << "Modulation depth       :" << props.modulationDepth;
	sNotice() << "Air absorption gain HF :" << props.airAbsorptionGainHf;
	sNotice() << "HF reference           :" << props.hfReference;
	sNotice() << "LF reference           :" << props.lfReference;
}

void UpdateSourceEffectProps( Source *src, const ReverbEffectProps &effectProps, const vec3_t listenerOrigin ) {
	[[maybe_unused]] ALint effectType = 0;
	alGetEffecti( src->effect, AL_EFFECT_TYPE, &effectType );
	assert( AL_EFFECT_EAXREVERB == effectType );

	//PrintReverbProps( effectProps.reverbProps );

	alEffectf( src->effect, AL_EAXREVERB_DENSITY, effectProps.reverbProps.density );
	alEffectf( src->effect, AL_EAXREVERB_DIFFUSION, effectProps.reverbProps.diffusion );

	alEffectf( src->effect, AL_EAXREVERB_DECAY_TIME, effectProps.reverbProps.decayTime );
	alEffectf( src->effect, AL_EAXREVERB_DECAY_HFRATIO, effectProps.reverbProps.decayHfRatio );
	alEffectf( src->effect, AL_EAXREVERB_DECAY_LFRATIO, effectProps.reverbProps.decayLfRatio );

	const float distance         = DistanceFast( src->origin, listenerOrigin );
	const float distanceGainFrac = calcSoundGainForDistanceAndAttenuation( distance, src->attenuation );
	assert( distanceGainFrac >= 0.0f && distanceGainFrac <= 1.0f );

	// Make the effect less pronounced on close distance
	const float effectGain = effectProps.reverbProps.gain * ( 1.0f - 0.1f * distanceGainFrac );

	alEffectf( src->effect, AL_EAXREVERB_GAIN, effectGain );
	alEffectf( src->effect, AL_EAXREVERB_GAINHF, effectProps.reverbProps.gainHf * ( 1.0f - 0.5f * effectProps.secondaryRaysObstruction ) );
	alEffectf( src->effect, AL_EAXREVERB_GAINLF, effectProps.reverbProps.gainLf );

	alEffectf( src->effect, AL_EAXREVERB_REFLECTIONS_GAIN, effectProps.reverbProps.reflectionsGain );
	alEffectf( src->effect, AL_EAXREVERB_REFLECTIONS_DELAY, effectProps.reverbProps.reflectionsDelay );

	alEffectf( src->effect, AL_EAXREVERB_LATE_REVERB_GAIN, effectProps.reverbProps.lateReverbGain );
	alEffectf( src->effect, AL_EAXREVERB_LATE_REVERB_DELAY, effectProps.reverbProps.lateReverbDelay );

	alEffectf( src->effect, AL_EAXREVERB_ECHO_TIME, effectProps.reverbProps.echoTime );
	alEffectf( src->effect, AL_EAXREVERB_ECHO_DEPTH, effectProps.reverbProps.echoDepth );

	alEffectf( src->effect, AL_EAXREVERB_MODULATION_TIME, effectProps.reverbProps.modulationTime );
	alEffectf( src->effect, AL_EAXREVERB_MODULATION_DEPTH, effectProps.reverbProps.modulationDepth );

	alEffectf( src->effect, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, effectProps.reverbProps.airAbsorptionGainHf );

	alEffectf( src->effect, AL_EAXREVERB_LFREFERENCE, effectProps.reverbProps.lfReference );
	alEffectf( src->effect, AL_EAXREVERB_HFREFERENCE, effectProps.reverbProps.hfReference );

	alEffecti( src->effect, AL_EAXREVERB_DECAY_HFLIMIT, effectProps.reverbProps.decayHfLimit );

	// Configure the direct send filter parameters

	assert( effectProps.directObstruction >= 0.0f && effectProps.directObstruction <= 1.0f );
	assert( effectProps.secondaryRaysObstruction >= 0.0f && effectProps.secondaryRaysObstruction <= 1.0f );

	// Both partial obstruction factors are within [0, 1] range, so we can get a weighted average
	const float obstructionFrac = 0.3f * effectProps.directObstruction + 0.7f * effectProps.secondaryRaysObstruction;
	assert( obstructionFrac >= 0.0f && obstructionFrac <= 1.0f );

	// Strongly suppress the dry path on obstruction.
	// Note: we do not touch the entire source gain.
	alFilterf( src->directFilter, AL_LOWPASS_GAIN, 1.0f - 0.7f * obstructionFrac );

	// There's nothing special with looping sources, their current sfx/sounds happen to benefit from that
	if( src->isLooping ) {
		alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 1.0f - obstructionFrac );
	} else {
		alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 1.0f - 0.5f * obstructionFrac );
	}

	// Attach the filter to the source
	alSourcei( src->source, AL_DIRECT_FILTER, src->directFilter );
	// Attach the effect to the slot
	alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, src->effect );
	// Feed the slot from the source
	alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, src->effectSlot, 0, AL_FILTER_NULL );
}

static void UpdateDelegatedSpatialization( Source *src, int listenerEntNum, const vec3_t listenerOrigin ) {
	if( src->attenuation == ATTN_NONE ) {
		// It MUST already be a relative sound
#ifndef PUBLIC_BUILD
		ALint value;
		alGetSourcei( src->source, AL_SOURCE_RELATIVE, &value );
		assert( value == AL_TRUE );
#endif
		return;
	}

	alSourcei( src->source, AL_SOURCE_RELATIVE, AL_FALSE );

	float sourcePitchScale = 1.0f;
	vec3_t sourceOriginToUse;
	VectorCopy( src->origin, sourceOriginToUse );

	// Don't do that for own sounds
	if( listenerEntNum <= 0 || listenerEntNum != src->entNum ) {
		// Setting effect panning vectors is not sufficient for "realistic" obstruction,
		// as the dry path is still propagates like if there were no obstacles and walls.
		// We try modifying the source origin as well to simulate sound propagation.
		// These conditions must be met:
		// 1) the direct path is fully obstructed
		// 2) there is a definite propagation path
		if( src->envUpdateState.effectProps.directObstruction == 1.0f ) {
			sourcePitchScale = 0.96f;
			calcPropagationOrigin( listenerOrigin, src->origin, &sourcePitchScale, sourceOriginToUse );
		}
	}

	assert( sourcePitchScale > 0.0f && sourcePitchScale <= 1.0f );
	alSourcef( src->source, AL_PITCH, src->chosenPitch * sourcePitchScale );

	alSourcefv( src->source, AL_POSITION, sourceOriginToUse );
	// The velocity is kept untouched for now.
	alSourcefv( src->source, AL_VELOCITY, src->velocity );
}

void UpdateSourceEffectPanning( Source *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes ) {
	// "If there is an active EaxReverbEffect, setting source origin/velocity is delegated to it".
	UpdateDelegatedSpatialization( src, listenerEntNum, listenerOrigin );

	vec3_t earlyPan { 0.0f, 0.0f, 0.0f }, latePan { 0.0f, 0.0f, 0.0f };

	// Limit panning to listener-related sounds
	if( listenerEntNum > 0 && src->entNum == listenerEntNum ) {
		calcReverbPan( listenerOrigin, listenerAxes, &src->panningUpdateState, earlyPan, latePan );
	}

	alEffectfv( src->effect, AL_EAXREVERB_REFLECTIONS_PAN, earlyPan );
	alEffectfv( src->effect, AL_EAXREVERB_LATE_REVERB_PAN, latePan );
}