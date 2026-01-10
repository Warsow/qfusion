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

struct ALBufferHolder {
	ALuint buffer { 0 };
	ALBufferHolder() = default;
	explicit ALBufferHolder( ALuint buffer_ ): buffer( buffer_ ) {}
	~ALBufferHolder() {
		if( buffer ) {
			alDeleteBuffers( 1, &buffer );
		}
	}
	[[nodiscard]]
	auto releaseOwnership() -> ALuint {
		ALuint result = buffer;
		buffer = 0;
		return result;
	}
	operator bool() const {
		return buffer != 0;
	}
	ALBufferHolder( const ALBufferHolder &that ) = delete;
	ALBufferHolder &operator=( const ALBufferHolder &that ) = delete;
	ALBufferHolder( ALBufferHolder &&that ) = delete;
	ALBufferHolder &operator=( ALBufferHolder &&that ) = delete;
};

namespace std { void swap( ALBufferHolder &a, ALBufferHolder &b ) { std::swap( a.buffer, b.buffer ); } }

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
static bool assignPitchVariations( const wsw::StringView &soundSetName, SoundSet *soundSet, std::span<const float> values ) {
	float sanitizedValues[16];
	assert( std::size( sanitizedValues ) == std::size( soundSet->pitchVariations ) );
	unsigned numSantizedValues = 0;

	bool hasIllegalValues = false;
	bool hasTooManyValues = false;
	for( const float &value: values ) {
		if( value <= 0.0f ) {
			hasIllegalValues = true;
		} else {
			if( numSantizedValues == std::size( sanitizedValues ) ) {
				hasTooManyValues = true;
				break;
			} else {
				sanitizedValues[numSantizedValues++] = value;
			}
		}
	}

	if( hasIllegalValues ) {
		sWarning() << "Pitch variations for sound set" << soundSetName << "have illegal values";
	}
	if( hasTooManyValues ) {
		sWarning() << "Too many pitch variations for sound set" << soundSetName;
	}

	bool hasUpdates = false;
	if( soundSet->numPitchVariations != numSantizedValues ) {
		soundSet->numPitchVariations = numSantizedValues;
		hasUpdates = true;
	} else if( !std::equal( sanitizedValues, sanitizedValues + numSantizedValues, soundSet->pitchVariations ) ) {
		hasUpdates = true;
	}

	if( hasUpdates ) {
		std::copy( sanitizedValues, sanitizedValues + numSantizedValues, soundSet->pitchVariations );
	}
	return hasUpdates;
}

auto Backend::loadSound( const SoundSetProps &props ) -> const SoundSet * {
	[[maybe_unused]] const wsw::StringView name( getSoundSetName( props ) );

	for( SoundSet *soundSet = m_registeredSoundSetsHead; soundSet; soundSet = soundSet->next ) {
		if( matchesByName( props, soundSet->props ) ) {
			bool hasUpdates = false;
			bool hasToLoad  = false;
			// Note: We can't just assign values if we load different buffers for different variations
			if( assignPitchVariations( name, soundSet, props.pitchVariations ) ) {
				hasUpdates = true;
			}
			if( soundSet->props.processingQualityHint != props.processingQualityHint ) {
				soundSet->props.processingQualityHint = props.processingQualityHint;
				hasUpdates = true;
			}
			if( soundSet->props.lazyLoading != props.lazyLoading ) {
				soundSet->props.lazyLoading = props.lazyLoading;
				hasUpdates = true;
				hasToLoad  = !props.lazyLoading && ( !soundSet->isLoaded && !soundSet->hasFailedLoading );
			}
			if( hasUpdates ) {
				sWarning() << "Overwriting properties for already registered sound" << name;
			}
			soundSet->registrationSequence = s_registration_sequence;
			if( hasToLoad ) {
				forceLoading( soundSet );
			}
			return soundSet;
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

	(void)assignPitchVariations( name, newSoundSet, props.pitchVariations );

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
			ALuint buffer = 0, stereoBuffer = 0;
			unsigned durationMillis = 0;
			if( loadBuffersFromFile( filePath, &buffer, &stereoBuffer, &durationMillis ) ) {
				soundSet->buffers[0]              = buffer;
				soundSet->stereoBuffers[0]        = stereoBuffer;
				soundSet->bufferDurationMillis[0] = durationMillis;
				soundSet->numBuffers              = 1;
				succeeded                         = true;
			} else {
				sError() << "Failed to load AL buffers for" << filePath;
			}
		} else {
			sError() << "Failed to find a path for exact name" << exactName->value;
		}
	} else if( const auto *namePattern = std::get_if<SoundSetProps::Pattern>( &soundSet->props.name ) ) {
		if( SoundSystem::getPathListForPattern( namePattern->pattern, &m_tmpPathListStorage ) ) {
			assert( std::size( soundSet->buffers ) == std::size( soundSet->stereoBuffers ) );
			const size_t maxBuffers = std::size( soundSet->buffers );
			for( const wsw::StringView &filePath: m_tmpPathListStorage ) {
				if( soundSet->numBuffers < maxBuffers ) {
					ALuint buffer = 0, stereoBuffer = 0;
					unsigned durationMillis = 0;
					if( loadBuffersFromFile( filePath, &buffer, &stereoBuffer, &durationMillis ) ) {
						soundSet->buffers[soundSet->numBuffers]              = buffer;
						soundSet->stereoBuffers[soundSet->numBuffers]        = stereoBuffer;
						soundSet->bufferDurationMillis[soundSet->numBuffers] = durationMillis;
						soundSet->numBuffers++;
					} else {
						sError() << "Failed to load AL buffers for" << filePath;
						if( soundSet->numBuffers ) {
							alDeleteBuffers( (ALsizei)soundSet->numBuffers, soundSet->buffers );
							alDeleteBuffers( (ALsizei)soundSet->numBuffers, soundSet->stereoBuffers );
							soundSet->numBuffers = 0;
						}
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

// TODO: Do we really need bias?
template <typename T>
static void runResamplingLoop( const T *__restrict in, T *__restrict out, size_t numSteps, int bias ) {
	size_t stepNum = 0;
	if( bias == 0 ) {
		// Mix channels
		do {
			*out = (short)( ( in[0] + in[1] ) / 2 );
			out++;
			in += 2;
		} while( ++stepNum < numSteps );
	} else {
		const int channelIndex = bias < 0 ? 0 : 1;
		// Copy the channel
		do {
			*out = in[channelIndex];
			out++;
			in += 2;
		} while( ++stepNum < numSteps );
	}
}

bool Backend::loadBuffersFromFile( const wsw::StringView &filePath, ALuint *buffer, ALuint *stereoBuffer, unsigned *durationMillis ) {
	sDebug() << "Loading buffers for" << filePath;

	wsw::PodVector<char> ztFilePath( filePath );
	ztFilePath.append( '\0' );

	snd_info_t fileInfo;
	if( !S_LoadSound( ztFilePath.data(), &m_fileDataBuffer, &fileInfo ) ) {
		// It produces verbose output on its own
		return false;
	}

	const void *monoData;
	snd_info_t monoInfo;
	ALBufferHolder stereoBufferHolder;
	if( fileInfo.numChannels == 1 ) {
		monoData = m_fileDataBuffer.get();
		monoInfo = fileInfo;
	} else {
		const void *stereoData      = m_fileDataBuffer.get();
		const snd_info_t stereoInfo = fileInfo;

		ALBufferHolder tmpBufferHolder( uploadBufferData( filePath, stereoInfo, stereoData ) );
		if( !tmpBufferHolder ) {
			sError() << "Failed to upload stereo buffer data";
			return false;
		}

		// TODO: Check whether info parameters are really handled prpoperly

		const size_t monoSizeInBytes = stereoInfo.samplesPerChannel * stereoInfo.bytesPerSample;
		if( !m_resamplingBuffer.tryReserving( monoSizeInBytes ) ) {
			sError() << "Failed to reserve resampling buffer data";
			return false;
		}

		monoData = m_resamplingBuffer.get();
		if( stereoInfo.bytesPerSample == 2 ) {
			runResamplingLoop<int16_t>( (int16_t *)stereoData, (int16_t *)monoData, stereoInfo.samplesPerChannel, s_stereo2mono->integer );
		} else {
			runResamplingLoop<int8_t>( (int8_t *)stereoData, (int8_t *)monoData, stereoInfo.samplesPerChannel, s_stereo2mono->integer );
		}

		monoInfo             = stereoInfo;
		monoInfo.numChannels = 1;
		monoInfo.sizeInBytes = (int)monoSizeInBytes;

		std::swap( tmpBufferHolder, stereoBufferHolder );
	}

	ALBufferHolder monoBufferHolder( uploadBufferData( filePath, monoInfo, monoData ) );
	if( !monoBufferHolder ) {
		if( fileInfo.numChannels == 1 ) {
			sError() << "Failed to upload the buffer data";
		} else {
			sError() << "Failed to upload the mono buffer data";
		}
		return false;
	}

	*buffer = monoBufferHolder.releaseOwnership();
	assert( alIsBuffer( *buffer ) );
	*stereoBuffer = stereoBufferHolder.releaseOwnership();
	assert( !*stereoBuffer || alIsBuffer( *stereoBuffer ) );
	*durationMillis = (unsigned)( ( 1000 * (int64_t)fileInfo.samplesPerChannel ) / fileInfo.sampleRate );
	return true;
}

auto Backend::uploadBufferData( const wsw::StringView &logFilePath, const snd_info_t &info, const void *data ) -> ALuint {
	ALenum error = alGetError();
	if( error != AL_NO_ERROR ) {
		sWarning() << "Had an error" << error << "prior to loading of data for" << logFilePath;
	}

	ALuint rawBuffer = 0;
	alGenBuffers( 1, &rawBuffer );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		sWarning() << "Failed to create buffer:" << wsw::StringView( S_ErrorMessage( error ) ) << "while loading data for" << logFilePath;
		return 0;
	}

	ALBufferHolder bufferHolder( rawBuffer );
	rawBuffer = 0;

	const ALenum format = S_SoundFormat( info.bytesPerSample, info.numChannels );
	alBufferData( bufferHolder.buffer, format, data, info.sizeInBytes, info.sampleRate );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		sWarning() << "Failed to set buffer data" << wsw::StringView( S_ErrorMessage( error ) ) << "while loading data for" << logFilePath;
		return 0;
	}

	// Note: We have decided to drop forceful unloading of other buffers in case of AL_OUT_OF_MEMORY as its utility is questionable

	return bufferHolder.releaseOwnership();
}

void Backend::unlinkAndFree( SoundSet *soundSet ) {
	if( soundSet->numBuffers ) {
		assert( soundSet->isLoaded && !soundSet->hasFailedLoading );
		alDeleteBuffers( (ALsizei)soundSet->numBuffers, soundSet->buffers );
		// "Deleting buffer name 0 is a legal NOP"
		alDeleteBuffers( (ALsizei)soundSet->numBuffers, soundSet->stereoBuffers );
	}

	wsw::unlink( soundSet, &m_registeredSoundSetsHead );
	soundSet->~SoundSet();
	m_soundSetsAllocator.free( soundSet );
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
			chosenBuffer = soundSet->stereoBuffers[index];
			// Looks like it is originally a mono sound
			if( !chosenBuffer ) {
				[[maybe_unused]] const ALuint *const end = soundSet->stereoBuffers + soundSet->numBuffers;
				assert( !wsw::any_of( soundSet->stereoBuffers, end, []( ALuint b ) { return b != 0; } ) );
				chosenBuffer = soundSet->buffers[index];
			}
		} else {
			chosenBuffer = soundSet->buffers[index];
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
