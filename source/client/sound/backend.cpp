/*
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)

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

#include "snd_local.h"
#include "environmentupdates.h"
#include "alsystemfacade.h"
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/sysclock.h>
#include <atomic>
#include <span>

typedef struct qbufPipe_s sndCmdPipe_t;

#define UPDATE_MSEC 10
static int64_t s_last_update_time;

static bool S_Init( bool verbose ) {
	s_last_update_time = 0;

	alDopplerFactor( s_doppler->value );
	// Defer s_sound_velocity application to S_Update() in order to avoid code duplication
	s_sound_velocity->modified = true;
	s_doppler->modified = false;

	S_LockBackgroundTrack( false );

	if( !S_InitDecoders( verbose ) ) {
		Com_Printf( "Failed to init decoders\n" );
		return false;
	}

	return true;
}

static void destroyContextAndDevice( ALCcontext *context, ALCdevice *device ) {
	if( context ) {
		alcMakeContextCurrent( nullptr );
		alcDestroyContext( context );
	}
	if( device ) {
		alcCloseDevice( device );
	}
}

[[nodiscard]]
static auto createDevice( bool verbose ) -> ALCdevice * {
	if( const char *defaultDevice = alcGetString( nullptr, ALC_DEFAULT_DEVICE_SPECIFIER ) ) {
		if( verbose ) {
			sNotice() << "Default OpenAL device" << wsw::StringView( defaultDevice );
		}
		if( const char *devices = alcGetString( nullptr, ALC_DEVICE_SPECIFIER ) ) {
			cvar_t *const var  = Cvar_Get( "s_openAL_device", defaultDevice, CVAR_ARCHIVE | CVAR_LATCH_SOUND );
			bool hasResetToDefault = false;
			if( !*var->string ) {
				sWarning() << "The specified OpenAL device name is empty, resetting to default";
				hasResetToDefault = true;
			}
			bool foundMatching = false;
			if( verbose ) {
				sNotice() << "Available OpenAL devices:";
			}
			for(; *devices; devices += std::strlen( devices ) + 1 ) {
				// TODO: Reuse strlen result for making a view
				sNotice() << " " << wsw::StringView( devices );
				if( strcmp( devices, var->string ) == 0 ) {
					foundMatching = true;
				}
			}
			if( !foundMatching && !hasResetToDefault ) {
				sWarning() << "Failed to find the specified device by name, resetting to default";
				Cvar_ForceSet( var->name, defaultDevice );
			}
			if( ALCdevice *device = alcOpenDevice( var->string ) ) {
				return device;
			} else {
				sError() << "Failed to open OpenAL device" << wsw::StringView( var->string );
			}
		} else {
			sError() << "Faield to retrieve the list of available OpenAL devices";
		}
	} else {
		sError() << "Failed to retrieve the default OpenAL device";
	}

	return nullptr;
}

[[nodiscard]]
static auto createContext( ALCdevice *device ) -> ALCcontext * {
	int attrList[6];
	int *attrPtr = &attrList[0];

	if( s_environment_effects->integer ) {
		// We limit each source to a single "auxiliary send" for optimization purposes.
		// This means each source has a single auxiliary output that feeds an effect aside from a direct output.
		*attrPtr++ = ALC_MAX_AUXILIARY_SENDS;
		*attrPtr++ = 1;
	}

	*attrPtr++ = ALC_HRTF_SOFT;
	*attrPtr++ = s_hrtf->integer ? 1 : 0;

	// Terminate the attributes pairs list
	*attrPtr++ = 0;
	*attrPtr++ = 0;

	if( ALCcontext *context = alcCreateContext( device, attrList ) ) {
		// TODO: Check?
		alcMakeContextCurrent( context );
		return context;
	} else {
		sError()<< "Failed to create OpenAL context";
	}

	return nullptr;
}

[[nodiscard]]
static auto createContextAndDevice( bool verbose ) -> std::optional<std::pair<ALCcontext *, ALCdevice *>> {
	if( ALCdevice *const device = createDevice( verbose ) ) {
		if( ALCcontext *const context = createContext( device ) ) {
			if( verbose ) {
				sNotice() << "OpenAL initialized";
				sNotice() << "  Device:     " << wsw::StringView( alcGetString( device, ALC_DEVICE_SPECIFIER ) );
				sNotice() << "  Vendor:     " << wsw::StringView( alGetString( AL_VENDOR ) );
				sNotice() << "  Version:    " << wsw::StringView( alGetString( AL_VERSION ) );
				sNotice() << "  Renderer:   " << wsw::StringView( alGetString( AL_RENDERER ) );
				sNotice() << "  Extensions: " << wsw::StringView( alGetString( AL_EXTENSIONS ) );
			}
			return std::make_pair( context, device );
		} else {
			destroyContextAndDevice( context, device );
		}
	} else {
		destroyContextAndDevice( nullptr, device );
	}

	return std::nullopt;
}

namespace wsw::snd {

// TODO: Get rid of init()/shutdown() calls, call free functions over pipe?
void Backend::init( bool verbose ) {
	if( auto maybeContextAndDevice = createContextAndDevice( verbose ) ) {
		if( S_Init( verbose ) ) {
			m_context       = maybeContextAndDevice->first;
			m_device        = maybeContextAndDevice->second;
			m_soundSetCache = new SoundSetCache;
			m_sourceManager = new SourceManager;
			m_initialized   = true;
		} else {
			destroyContextAndDevice( maybeContextAndDevice->first, maybeContextAndDevice->second );
		}
	}
}

void Backend::shutdown( bool verbose ) {
	m_sourceManager->stopStreamSources();

	S_LockBackgroundTrack( false );
	S_StopBackgroundTrack( m_sourceManager );

	delete m_sourceManager;
	delete m_soundSetCache;

	S_ShutdownDecoders( verbose );

	destroyContextAndDevice( m_context, m_device );

	// Note: this is followed by a separate "terminate pipe" call
}

void Backend::stopSounds( unsigned flags ) {
	// TODO: This line actually stops music playback unconditionally (but looks like we always supply StopMusic flag)
	m_sourceManager->stopStreamSources();
	m_sourceManager->stopAllRegularSources( ( flags & SoundSystem::RetainLocal ) != 0 );
	if( flags & SoundSystem::StopMusic ) {
		S_StopBackgroundTrack( m_sourceManager );
	}
}

void Backend::processFrameUpdates( const EntitySpatialParams &listenerSpatialParams ) {
	const float *origin   = listenerSpatialParams.origin;
	const float *axis     = listenerSpatialParams.axis;
	const float *velocity = listenerSpatialParams.velocity;
	const int entNum      = listenerSpatialParams.entNum;

	float orientation[6];

	orientation[0] = axis[AXIS_FORWARD + 0];
	orientation[1] = axis[AXIS_FORWARD + 1];
	orientation[2] = axis[AXIS_FORWARD + 2];
	orientation[3] = axis[AXIS_UP + 0];
	orientation[4] = axis[AXIS_UP + 1];
	orientation[5] = axis[AXIS_UP + 2];

	alListenerfv( AL_POSITION, origin );
	alListenerfv( AL_VELOCITY, velocity );
	alListenerfv( AL_ORIENTATION, orientation );

	const int64_t millisNow = Sys_Milliseconds();

	m_sourceManager->updateRegularSources( millisNow, listenerSpatialParams.origin );

	if( s_environment_effects->integer ) {
		updateEnvOfListenerAndSources( m_sourceManager->getActiveSourcesHead(), millisNow, entNum, origin, velocity, axis );
	}

	// We can start sources as their origins have been properly set up by the previous calls
	m_sourceManager->startPendingRegularSources();
}

auto Backend::loadSound( const SoundSetProps &props ) -> const SoundSet * {
	return m_soundSetCache->loadSound( m_registrationSequence, props );
}

auto Backend::findSoundSet( const SoundSetProps &props ) -> const SoundSet * {
	return m_soundSetCache->findSoundSet( props );
}

void Backend::beginRegistration() {
	m_registrationSequence++;
	if( !m_registrationSequence ) [[unlikely]] {
		m_registrationSequence = 1;
	}
}

void Backend::endRegistration() {
	m_soundSetCache->freeUnusedSoundSets( m_registrationSequence );
}

void Backend::setEntitySpatialParams( const EntitySpatialParamsBatch &batch ) {
	for( unsigned i = 0; i < batch.count; ++i ) {
		m_sourceManager->setEntitySpatialParams( batch.params[i] );
	}
}

void Backend::startLocalSound( const SoundSet *sound, float volume ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = m_soundSetCache->getBufferForPlayback( sound, true ) ) {
		const float pitch = m_soundSetCache->getPitchForPlayback( sound );
		m_sourceManager->startLocalSound( sound, *bufferAndIndex, pitch, volume );
	}
}

void Backend::startLocalSoundByName( const wsw::PodVector<char> &name, float volume ) {
	const SoundSetProps soundSetProps { .name = SoundSetProps::Exact( wsw::StringView( name.data(), name.size() ) ) };
	if( const SoundSet *soundSet = m_soundSetCache->loadSound( m_registrationSequence, soundSetProps ) ) {
		startLocalSound( soundSet, volume );
	}
}

void Backend::startFixedSound( const SoundSet *sound, const Vec3 &origin, int channel, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = m_soundSetCache->getBufferForPlayback( sound ) ) {
		const float pitch = m_soundSetCache->getPitchForPlayback( sound );
		m_sourceManager->startFixedSound( sound, *bufferAndIndex, pitch, origin.data(), channel, volume, attenuation );
	}
}

void Backend::startRelativeSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag,
								  int entNum, int channel, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = m_soundSetCache->getBufferForPlayback( sound ) ) {
		const float pitch = m_soundSetCache->getPitchForPlayback( sound );
		m_sourceManager->startRelativeSound( sound, attachmentTag, *bufferAndIndex, pitch, entNum,
											 channel, volume, attenuation );
	}
}

void Backend::addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag,
							int entNum, uintptr_t identifyingToken, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = m_soundSetCache->getBufferForPlayback( sound ) ) {
		// TODO: How is pitch managed for looping sounds
		const float pitch = m_soundSetCache->getPitchForPlayback( sound );
		m_sourceManager->touchLoopSound( sound, attachmentTag, *bufferAndIndex, pitch, entNum,
										 identifyingToken, volume, attenuation );
	}
}

void Backend::startBackgroundTrack( const wsw::PodVector<char> &intro, const wsw::PodVector<char> &loop, int mode ) {
	S_StartBackgroundTrack( m_sourceManager, intro.data(), loop.data(), mode );
}

void Backend::stopBackgroundTrack() {
	S_StopBackgroundTrack( m_sourceManager );
}

void Backend::lockBackgroundTrack( bool lock ) {
	S_LockBackgroundTrack( lock );
}

void Backend::advanceBackgroundTrack( int value ) {
	if( value < 0 ) {
		S_PrevBackgroundTrack();
	} else if( value > 0 ) {
		S_NextBackgroundTrack();
	} else {
		S_PauseBackgroundTrack();
	}
}

void Backend::activate( bool active ) {
	S_LockBackgroundTrack( !active );

	// TODO: Actually stop playing sounds while not active?
	if( active ) {
		alListenerf( AL_GAIN, 1 );
	} else {
		alListenerf( AL_GAIN, 0 );
	}
}

}

static void S_Update( std::atomic_bool *const isSoundSystemInitialized ) {
	if( *isSoundSystemInitialized ) {
		auto *soundSystem            = (wsw::snd::ALSoundSystem *)SoundSystem::instance();
		SourceManager *sourceManager = soundSystem->getBackend()->getSourceManager();

		S_UpdateMusic( sourceManager );
		sourceManager->updateStreamSources();
	}

	if( s_doppler->modified ) {
		if( s_doppler->value > 0.0f ) {
			alDopplerFactor( s_doppler->value );
		} else {
			alDopplerFactor( 0.0f );
		}
		s_doppler->modified = false;
	}

	if( s_sound_velocity->modified ) {
		// If environment effects are supported, we can set units to meters ratio.
		// In this case, we have to scale the hardcoded s_sound_velocity value.
		// (The engine used to assume that units to meters ratio is 1).
		float appliedVelocity = s_sound_velocity->value;
		if( appliedVelocity <= 0.0f ) {
			appliedVelocity = 0.0f;
		}
		alDopplerVelocity( appliedVelocity );
		alSpeedOfSound( appliedVelocity );
		s_sound_velocity->modified = false;
	}
}

static int S_EnqueuedCmdsWaiter( sndCmdPipe_t *queue, void *arg, bool timeout ) {
	const int read = QBufPipe_ReadCmds( queue );
	if( read < 0 ) {
		// shutdown
		return read;
	}

	const int64_t now = Sys_Milliseconds();
	if( timeout || now >= s_last_update_time + UPDATE_MSEC ) {
		s_last_update_time = now;
		S_Update( (std::atomic_bool *)arg );
	}

	return read;
}

void *S_BackgroundUpdateProc( void *param ) {
	using namespace wsw::snd;

	auto *arg                      = ( ALSoundSystem::ThreadProcArg *)param;
	qbufPipe_s *pipe               = arg->pipe;
	void *isSoundSystemInitialized = arg->isSoundSystemInitialized;

	// Don't hold the arg heap memory forever
	Q_free( arg );

	QBufPipe_Wait( pipe, S_EnqueuedCmdsWaiter, isSoundSystemInitialized, UPDATE_MSEC );

	return NULL;
}
