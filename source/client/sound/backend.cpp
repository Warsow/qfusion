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
#include "snd_env_sampler.h"
#include "alsystemfacade.h"
#include <common/helpers/links.h>
#include <common/helpers/algorithm.h>
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/sysclock.h>
#include <span>

extern int s_registration_sequence;

typedef struct qbufPipe_s sndCmdPipe_t;

static ALCdevice *alDevice;
static ALCcontext *alContext;

#define UPDATE_MSEC 10
static int64_t s_last_update_time;

static bool S_Init( void *hwnd, int maxEntities, bool verbose ) {
	int numDevices;
	int userDeviceNum = -1;
	char *devices, *defaultDevice;
	cvar_t *s_openAL_device;
	int attrList[6];
	int *attrPtr;

	alDevice = NULL;
	alContext = NULL;

	s_last_update_time = 0;

	// get system default device identifier
	defaultDevice = ( char * )alcGetString( NULL, ALC_DEFAULT_DEVICE_SPECIFIER );
	if( !defaultDevice ) {
		Com_Printf( "Failed to get openAL default device\n" );
		return false;
	}

	s_openAL_device = Cvar_Get( "s_openAL_device", defaultDevice, CVAR_ARCHIVE | CVAR_LATCH_SOUND );

	devices = ( char * )alcGetString( NULL, ALC_DEVICE_SPECIFIER );
	for( numDevices = 0; *devices; devices += strlen( devices ) + 1, numDevices++ ) {
		if( !Q_stricmp( s_openAL_device->string, devices ) ) {
			userDeviceNum = numDevices;

			// force case sensitive
			if( strcmp( s_openAL_device->string, devices ) ) {
				Cvar_ForceSet( "s_openAL_device", devices );
			}
		}
	}

	if( !numDevices ) {
		Com_Printf( "Failed to get openAL devices\n" );
		return false;
	}

	// the device assigned by the user is not available
	if( userDeviceNum == -1 ) {
		Com_Printf( "'s_openAL_device': incorrect device name, reseting to default\n" );

		Cvar_ForceSet( "s_openAL_device", defaultDevice );

		devices = ( char * )alcGetString( NULL, ALC_DEVICE_SPECIFIER );
		for( numDevices = 0; *devices; devices += strlen( devices ) + 1, numDevices++ ) {
			if( !Q_stricmp( s_openAL_device->string, devices ) ) {
				userDeviceNum = numDevices;
			}
		}

		if( userDeviceNum == -1 ) {
			Cvar_ForceSet( "s_openAL_device", defaultDevice );
		}
	}

	alDevice = alcOpenDevice( (const ALchar *)s_openAL_device->string );
	if( !alDevice ) {
		Com_Printf( "Failed to open device\n" );
		return false;
	}

	attrPtr = &attrList[0];
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

	// Create context
	alContext = alcCreateContext( alDevice, attrList );
	if( !alContext ) {
		Com_Printf( "Failed to create context\n" );
		return false;
	}

	alcMakeContextCurrent( alContext );

	if( verbose ) {
		sNotice() << "OpenAL initialized";

		if( numDevices ) {
			int i;

			Com_Printf( "  Devices:    " );

			devices = ( char * )alcGetString( NULL, ALC_DEVICE_SPECIFIER );
			for( i = 0; *devices; devices += strlen( devices ) + 1, i++ )
				Com_Printf( "%s%s", devices, ( i < numDevices - 1 ) ? ", " : "" );
			Com_Printf( "\n" );

			if( defaultDevice && *defaultDevice ) {
				Com_Printf( "  Default system device: %s\n", defaultDevice );
			}

			Com_Printf( "\n" );
		}

		sNotice() << "  Device:     " << wsw::StringView( alcGetString( alDevice, ALC_DEVICE_SPECIFIER ) );
		sNotice() << "  Vendor:     " << wsw::StringView( alGetString( AL_VENDOR ) );
		sNotice() << "  Version:    " << wsw::StringView( alGetString( AL_VERSION ) );
		sNotice() << "  Renderer:   " << wsw::StringView( alGetString( AL_RENDERER ) );
		sNotice() << "  Extensions: " << wsw::StringView( alGetString( AL_EXTENSIONS ) );
	}

	alDopplerFactor( s_doppler->value );
	// Defer s_sound_velocity application to S_Update() in order to avoid code duplication
	s_sound_velocity->modified = true;
	s_doppler->modified = false;

	S_LockBackgroundTrack( false );

	if( !S_InitDecoders( verbose ) ) {
		Com_Printf( "Failed to init decoders\n" );
		return false;
	}
	if( !S_InitSources( maxEntities, verbose ) ) {
		Com_Printf( "Failed to init sources\n" );
		return false;
	}

	return true;
}

static void S_SetListener( int entNum, const vec3_t origin, const vec3_t velocity, const mat3_t axis ) {
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

	ENV_UpdateListener( entNum, origin, velocity, axis );
}

namespace wsw::snd {

void Backend::init( bool verbose ) {
	if( S_Init( nullptr, MAX_EDICTS, verbose ) ) {
		m_initialized = true;
	}
}

void Backend::shutdown( bool verbose ) {
	S_StopStreams();
	S_LockBackgroundTrack( false );
	S_StopBackgroundTrack();

	for( SoundSet *soundSet = m_registeredSoundSetsHead, *next; soundSet; soundSet = next ) { next = soundSet->next;
		unlinkAndFree( soundSet );
	}

	S_ShutdownSources();
	S_ShutdownDecoders( verbose );

	if( alContext ) {
		alcMakeContextCurrent( NULL );
		alcDestroyContext( alContext );
		alContext = NULL;
	}

	if( alDevice ) {
		alcCloseDevice( alDevice );
		alDevice = NULL;
	}

	// Note: this is followed by a separate "terminate pipe" call
}

void Backend::stopSounds( unsigned flags ) {
	S_StopStreams();
	S_StopAllSources( ( flags & SoundSystem::RetainLocal ) != 0 );
	if( flags & SoundSystem::StopMusic ) {
		S_StopBackgroundTrack();
	}
}

void Backend::processFrameUpdates() {
	S_UpdateSources();
}

[[nodiscard]]
static auto getSoundSetName( const SoundSetProps &props ) -> wsw::StringView {
	if( const auto *exact = std::get_if<SoundSetProps::Exact>( &props.name ) ) {
		return exact->value;
	}
	if( const auto *pattern = std::get_if<SoundSetProps::Pattern>( &props.name ) ) {
		return pattern->pattern;
	}
	wsw::failWithLogicError( "Unreachable" );
}

[[nodiscard]]
static bool matchesByName( const SoundSetProps &lhs, const SoundSetProps &rhs ) {
	if( lhs.name.index() == rhs.name.index() ) {
		if( const auto *leftExact = std::get_if<SoundSetProps::Exact>( &lhs.name ) ) {
			const auto *rightExact = std::get_if<SoundSetProps::Exact>( &rhs.name );
			return leftExact->value.equalsIgnoreCase( rightExact->value );
		}
		if( const auto *leftPattern = std::get_if<SoundSetProps::Pattern>( &lhs.name ) ) {
			const auto *rightPattern = std::get_if<SoundSetProps::Pattern>( &rhs.name );
			return leftPattern->pattern.equalsIgnoreCase( rightPattern->pattern );
		}
		wsw::failWithLogicError( "Unreachable" );
	}
	return false;
}

[[nodiscard]]
static auto sanitizePitchVariations( const wsw::StringView &soundSetName, float *targetValues,
									 std::span<const float> givenValues ) -> unsigned {
	unsigned numSanitizedValues = 0;

	bool hasIllegalValues = false;
	bool hasTooManyValues = false;
	for( const float &value: givenValues ) {
		if( value <= 0.0f ) {
			hasIllegalValues = true;
		} else {
			if( numSanitizedValues == SoundSet::kMaxPitchVariations ) {
				hasTooManyValues = true;
			} else {
				targetValues[numSanitizedValues++] = value;
			}
		}
	}

	if( hasIllegalValues ) {
		sWarning() << "Pitch variations for sound set" << soundSetName << "have illegal values";
	}
	if( hasTooManyValues ) {
		sWarning() << "Too many pitch variations for sound set" << soundSetName;
	}

	assert( numSanitizedValues <= SoundSet::kMaxPitchVariations );
	return numSanitizedValues;
}

auto Backend::loadSound( const SoundSetProps &props ) -> const SoundSet * {
	[[maybe_unused]] const wsw::StringView name( getSoundSetName( props ) );

	float pitchVariations[SoundSet::kMaxPitchVariations];
	const unsigned numPitchVariations = sanitizePitchVariations( name, pitchVariations, props.pitchVariations );

	for( SoundSet *soundSet = m_registeredSoundSetsHead; soundSet; soundSet = soundSet->next ) {
		if( matchesByName( props, soundSet->props ) ) {
			bool paramsDiffer = false;
			// Note: We can't just assign values if we load different buffers for different variations
			if( soundSet->numPitchVariations != numPitchVariations ||
				!std::equal( pitchVariations, pitchVariations + numPitchVariations, soundSet->pitchVariations ) ) {
				paramsDiffer = true;
			}
			if( soundSet->props.processingQualityHint != props.processingQualityHint ) {
				paramsDiffer = true;
			}
			bool useThisSound = false;
			if( paramsDiffer ) {
				if( props.paramsOverrideMode == SoundSetProps::OverrideExistingParams ) {
					sWarning() << "Overwriting properties for already registered sound" << name;
					std::copy( pitchVariations, pitchVariations + numPitchVariations, soundSet->pitchVariations );
					soundSet->numPitchVariations          = numPitchVariations;
					soundSet->props.processingQualityHint = props.processingQualityHint;
					useThisSound = true;
				}
			} else {
				useThisSound = true;
			}
			if( useThisSound ) {
				soundSet->registrationSequence = s_registration_sequence;
				const bool hasToLoad = !props.lazyLoading && ( !soundSet->isLoaded && !soundSet->hasFailedLoading );
				if( hasToLoad ) {
					forceLoading( soundSet );
				}
				return soundSet;
			}
			// Otherwise, continue the loop and to find another sound set with exactly matching params.
		}
	}

	uint8_t *const mem = m_soundSetsAllocator.allocOrNull();
	if( !mem ) {
		sError() << "Failed to allocate a sound set for" << name << "(too many sound sets)";
		return nullptr;
	}

	auto *const newSoundSet = new( mem )SoundSet { .props = props, .registrationSequence = s_registration_sequence };

	// Save a deep copy of the name
	auto *const nameMem = (char *)( mem + sizeof( SoundSet ) );
	if( const auto *exactName = std::get_if<SoundSetProps::Exact>( &props.name ) ) {
		exactName->value.copyTo( nameMem, MAX_QPATH + 1 );
		newSoundSet->props.name = SoundSetProps::Exact { wsw::StringView( nameMem, exactName->value.size() ) };
	} else if( const auto *namePattern = std::get_if<SoundSetProps::Pattern>( &props.name ) ) {
		namePattern->pattern.copyTo( nameMem, MAX_QPATH + 1 );
		newSoundSet->props.name = SoundSetProps::Pattern { wsw::StringView( nameMem, namePattern->pattern.size() ) };
	} else {
		wsw::failWithLogicError( "Unreachable" );
	}

	std::copy( pitchVariations, pitchVariations + numPitchVariations, newSoundSet->pitchVariations );
	newSoundSet->numPitchVariations = numPitchVariations;

	wsw::link( newSoundSet, &m_registeredSoundSetsHead );

	if( !props.lazyLoading ) {
		forceLoading( newSoundSet );
	}

	return newSoundSet;
}

auto Backend::findSoundSet( const SoundSetProps &props ) -> const SoundSet * {
	for( SoundSet *soundSet = m_registeredSoundSetsHead; soundSet; soundSet = soundSet->next ) {
		if( matchesByName( props, soundSet->props ) ) {
			return soundSet;
		}
	}
	return nullptr;
}

void Backend::forceLoading( SoundSet *soundSet ) {
	assert( !soundSet->isLoaded && !soundSet->hasFailedLoading && soundSet->numBuffers == 0 );

	bool succeeded = false;
	if( const auto *exactName = std::get_if<SoundSetProps::Exact>( &soundSet->props.name ) ) {
		const wsw::PodVector<char> filePathData = SoundSystem::getPathForName( exactName->value );
		const wsw::StringView filePath( filePathData.data(), filePathData.size() - 1, wsw::StringView::ZeroTerminated );
		if( !filePath.empty() ) {
			if( const FileDataBuffer *fileDataBuffer = m_fileDataBufferCache.get( filePath ) ) {
				soundSet->buffers[0] = fileDataBuffer;
				soundSet->numBuffers = 1;
				succeeded            = true;
			} else {
				sError() << "Failed to load AL buffers for" << filePath;
			}
		} else {
			sError() << "Failed to find a path for exact name" << exactName->value;
		}
	} else if( const auto *namePattern = std::get_if<SoundSetProps::Pattern>( &soundSet->props.name ) ) {
		if( SoundSystem::getPathListForPattern( namePattern->pattern, &m_tmpPathListStorage ) ) {
			const size_t maxBuffers = std::size( soundSet->buffers );
			for( const wsw::StringView &filePath: m_tmpPathListStorage ) {
				if( soundSet->numBuffers < maxBuffers ) {
					if( const FileDataBuffer *buffer = m_fileDataBufferCache.get( filePath ) ) {
						soundSet->buffers[soundSet->numBuffers++] = buffer;
					} else {
						sError() << "Failed to load AL buffers for" << filePath;
						releaseFileDataBuffers( soundSet );
					}
				} else {
					sWarning() << "Too many files matching" << namePattern->pattern;
					break;
				}
			}
			// TODO: Allow specifying "all-or-nothing" success policy?
			succeeded = soundSet->numBuffers > 0;
		} else {
			sError() << "Failed to get path list for pattern" << namePattern->pattern;
		}
	} else {
		wsw::failWithLogicError( "Unreachable" );
	}

	soundSet->isLoaded         = succeeded;
	soundSet->hasFailedLoading = !succeeded;
}

void Backend::unlinkAndFree( SoundSet *soundSet ) {
	releaseFileDataBuffers( soundSet );
	wsw::unlink( soundSet, &m_registeredSoundSetsHead );
	soundSet->~SoundSet();
	m_soundSetsAllocator.free( soundSet );
}

void Backend::releaseFileDataBuffers( SoundSet *soundSet ) {
	for( unsigned i = 0; i < soundSet->numBuffers; ++i ) {
		m_fileDataBufferCache.release( soundSet->buffers[i] );
		soundSet->buffers[i] = nullptr;
	}
	soundSet->numBuffers = 0;
}

[[nodiscard]]
static auto choseIndex( unsigned limit, unsigned lastChosenIndex, RandomGenerator *rng ) -> unsigned {
	assert( limit < std::numeric_limits<uint8_t>::max() );
	if( limit < 2 ) [[likely]] {
		return 0;
	} else if( limit > 2 ) [[likely]] {
		for( unsigned attempt = 0; attempt < 20; ++attempt ) {
			if( const unsigned index = rng->nextBounded( limit ); index != lastChosenIndex ) {
				return index;
			}
		}
		wsw::failWithRuntimeError( "RNG bug" );
	} else {
		assert( limit == 2 && lastChosenIndex < 2 );
		// Allow chosing the last chosen item, but reduce the chance
		const unsigned altIndex = ( lastChosenIndex + 1 ) % 2;
		const unsigned values[3] { altIndex, lastChosenIndex, altIndex };
		return values[rng->nextBounded( std::size( values ) )];
	}
}

auto Backend::getBufferForPlayback( const SoundSet *soundSet, bool preferStereo ) -> std::optional<std::pair<ALuint, unsigned>> {
	if( soundSet ) {
		if( !soundSet->isLoaded ) {
			if( soundSet->hasFailedLoading ) {
				return std::nullopt;
			}
			// TODO? forceLoading( const SoundSet *) looks awkward as well
			forceLoading( const_cast<SoundSet *>( soundSet ) );
			if( soundSet->hasFailedLoading ) {
				return std::nullopt;
			}
			assert( soundSet->isLoaded );
		}
		const unsigned numBuffers = soundSet->numBuffers;
		assert( numBuffers > 0 );
		const unsigned index = choseIndex( numBuffers, soundSet->lastChosenBufferIndex, &m_rng );
		ALuint chosenBuffer;
		if( preferStereo ) {
			chosenBuffer = soundSet->buffers[index]->stereoBuffer;
			// Looks like it is originally a mono sound
			if( !chosenBuffer ) {
				chosenBuffer = soundSet->buffers[index]->buffer;
			}
		} else {
			chosenBuffer = soundSet->buffers[index]->buffer;
		}
		assert( alIsBuffer( chosenBuffer ) );
		soundSet->lastChosenBufferIndex = (uint8_t)index;
		return std::make_pair( chosenBuffer, index );
	}
	return std::nullopt;
}

// TODO: We don't need to disallow using previously chosen pitch
// if we guarantee choosing a different buffer (if the number of buffers > 2)
auto Backend::getPitchForPlayback( const SoundSet *soundSet ) -> float {
	if( soundSet ) {
		if( const auto numPitchVariations = soundSet->numPitchVariations; numPitchVariations > 0 ) {
			const auto index  = choseIndex( numPitchVariations, soundSet->lastChosenPitchIndex, &m_rng );
			const float pitch = soundSet->pitchVariations[index];
			assert( pitch > 0.0f );
			soundSet->lastChosenPitchIndex = (uint8_t)index;
			return pitch;
		}
	}
	return 1.0f;
}

void Backend::endRegistration() {
	for( SoundSet *soundSet = m_registeredSoundSetsHead, *next; soundSet; soundSet = next ) { next = soundSet->next;
		if( soundSet->registrationSequence != s_registration_sequence ) {
			unlinkAndFree( soundSet );
		}
	}
}

void Backend::setEntitySpatialParams( const EntitySpatialParamsBatch &batch ) {
	for( unsigned i = 0; i < batch.count; ++i ) {
		S_SetEntitySpatialization( batch.entNums[i], batch.origins[i], batch.velocities[i], batch.axes[i] );
	}
}

void Backend::setListener( int entNum, const Vec3 &origin, const Vec3 &velocity, const std::array<Vec3, 3> &axis ) {
	S_SetListener( entNum, origin.data(), velocity.data(), (const float *)axis.data() );
}

void Backend::startLocalSound( const SoundSet *sound, float volume ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = getBufferForPlayback( sound, true ) ) {
		S_StartLocalSound( sound, *bufferAndIndex, getPitchForPlayback( sound ), volume );
	}
}

void Backend::startLocalSoundByName( const wsw::PodVector<char> &name, float volume ) {
	const SoundSetProps soundSetProps { .name = SoundSetProps::Exact( wsw::StringView( name.data(), name.size() ) ) };
	if( const SoundSet *soundSet = this->loadSound( soundSetProps ) ) {
		startLocalSound( soundSet, volume );
	}
}

void Backend::startFixedSound( const SoundSet *sound, const Vec3 &origin, int channel, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = getBufferForPlayback( sound ) ) {
		S_StartFixedSound( sound, *bufferAndIndex, getPitchForPlayback( sound ), origin.data(), channel, volume, attenuation );
	}
}

void Backend::startRelativeSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, int channel, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = getBufferForPlayback( sound ) ) {
		S_StartRelativeSound( sound, attachmentTag, *bufferAndIndex, getPitchForPlayback( sound ), entNum, channel, volume, attenuation );
	}
}

void Backend::addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float volume, float attenuation ) {
	if( const std::optional<std::pair<ALuint, unsigned>> bufferAndIndex = getBufferForPlayback( sound ) ) {
		S_AddLoopSound( sound, attachmentTag, *bufferAndIndex, getPitchForPlayback( sound ), entNum, identifyingToken, volume, attenuation );
	}
}

void Backend::startBackgroundTrack( const wsw::PodVector<char> &intro, const wsw::PodVector<char> &loop, int mode ) {
	S_StartBackgroundTrack( intro.data(), loop.data(), mode );
}

void Backend::stopBackgroundTrack() {
	S_StopBackgroundTrack();
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

void S_Clear() {
}

static void S_Update( void ) {
	S_UpdateMusic();

	S_UpdateStreams();

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

static int S_EnqueuedCmdsWaiter( sndCmdPipe_t *queue, bool timeout ) {
	const int read = QBufPipe_ReadCmds( queue );
	if( read < 0 ) {
		// shutdown
		return read;
	}

	const int64_t now = Sys_Milliseconds();
	if( timeout || now >= s_last_update_time + UPDATE_MSEC ) {
		s_last_update_time = now;
		S_Update();
	}

	return read;
}

void *S_BackgroundUpdateProc( void *param ) {
	using namespace wsw::snd;

	auto *arg        = ( ALSoundSystem::ThreadProcArg *)param;
	qbufPipe_s *pipe = arg->pipe;

	// Don't hold the arg heap memory forever
	Q_free( arg );

	QBufPipe_Wait( pipe, S_EnqueuedCmdsWaiter, UPDATE_MSEC );

	return NULL;
}
