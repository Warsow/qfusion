/*
Copyright (C) 1997-2001 Id Software, Inc.

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
#include <client/client.h>
#include <common/singletonholder.h>
#include <common/cmdargs.h>
#include <common/cmdcompat.h>
#include "alsystemfacade.h"

using wsw::operator""_asView;

class NullSoundSystem : public SoundSystem {
public:
	explicit NullSoundSystem( client_state_s *client_ ) : SoundSystem( client_ ) {}

	void deleteSelf( bool ) override;

	void postInit() override {}

	void beginRegistration() override {}
	void endRegistration() override {}

	void stopSounds( unsigned ) override {}

	void updateListener( int, const float *, const float *, const mat3_t ) override {}
	void activate( bool ) override {}

	void processFrameUpdates() override {}

	void setEntitySpatialParams( int, const float *, const float *, const float * ) override {};

	[[nodiscard]]
	auto registerSound( const SoundSetProps & ) -> const SoundSet * override { return nullptr; }

	void startFixedSound( const SoundSet *, const float *, int, float, float ) override {}
	void startRelativeSound( const SoundSet *, AttachmentTag attachmentTag, int, int, float, float ) override {}
	void startLocalSound( const char *, float ) override {}
	void startLocalSound( const SoundSet *, float ) override {}
	void addLoopSound( const SoundSet *, AttachmentTag attachmentTag, int, uintptr_t, float, float ) override {}

	void startBackgroundTrack( const char *, const char *, int ) override {}
	void stopBackgroundTrack() override {}
	void nextBackgroundTrack() override {}
	void prevBackgroundTrack() override {}
	void pauseBackgroundTrack() override {}
};

static SingletonHolder<NullSoundSystem> nullSoundSystemHolder;

void NullSoundSystem::deleteSelf( bool ) {
	::nullSoundSystemHolder.shutdown();
}

cvar_t *s_volume;
cvar_t *s_musicvolume;

cvar_t *s_doppler;
cvar_t *s_sound_velocity;
cvar_t *s_environment_effects;
cvar_t *s_environment_sampling_quality;
cvar_t *s_effects_number_threshold;
cvar_t *s_hrtf;
cvar_t *s_stereo2mono;
cvar_t *s_globalfocus;

static void SF_Music_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() == 2 ) {
		SoundSystem::instance()->startBackgroundTrack( Cmd_Argv( 1 ), Cmd_Argv( 1 ), 0 );
	} else if( Cmd_Argc() == 3 ) {
		SoundSystem::instance()->startBackgroundTrack( Cmd_Argv( 1 ), Cmd_Argv( 2 ), 0 );
	} else {
		sNotice() << "music <intro|playlist> [loop|shuffle]";
		return;
	}
}

static void SF_StopBackgroundTrack( const CmdArgs & ) {
	SoundSystem::instance()->stopBackgroundTrack();
}

static void SF_PrevBackgroundTrack( const CmdArgs & ) {
	SoundSystem::instance()->prevBackgroundTrack();
}

static void SF_NextBackgroundTrack( const CmdArgs & ) {
	SoundSystem::instance()->nextBackgroundTrack();
}

static void SF_PauseBackgroundTrack( const CmdArgs & ) {
	SoundSystem::instance()->pauseBackgroundTrack();
}

bool SoundSystem::init( client_state_t *client, const InitOptions &options ) {
	s_volume         = Cvar_Get( "s_volume", "0.8", CVAR_ARCHIVE );
	s_musicvolume    = Cvar_Get( "s_musicvolume", "0.05", CVAR_ARCHIVE );
	s_doppler        = Cvar_Get( "s_doppler", "1.0", CVAR_ARCHIVE );
	s_sound_velocity = Cvar_Get( "s_sound_velocity", "8500", CVAR_DEVELOPER );
	s_stereo2mono    = Cvar_Get( "s_stereo2mono", "0", CVAR_ARCHIVE );
	s_globalfocus    = Cvar_Get( "s_globalfocus", "0", CVAR_ARCHIVE );

	s_environment_effects          = Cvar_Get( "s_environment_effects", "1", CVAR_ARCHIVE | CVAR_LATCH_SOUND );
	s_environment_sampling_quality = Cvar_Get( "s_environment_sampling_quality", "0.5", CVAR_ARCHIVE );
	s_effects_number_threshold     = Cvar_Get( "s_effects_number_threshold", "15", CVAR_ARCHIVE );
	s_hrtf                         = Cvar_Get( "s_hrtf", "1", CVAR_ARCHIVE | CVAR_LATCH_SOUND );

	CL_Cmd_Register( "music"_asView, SF_Music_f );
	CL_Cmd_Register( "stopmusic"_asView, SF_StopBackgroundTrack );
	CL_Cmd_Register( "prevmusic"_asView, SF_PrevBackgroundTrack );
	CL_Cmd_Register( "nextmusic"_asView, SF_NextBackgroundTrack );
	CL_Cmd_Register( "pausemusic"_asView, SF_PauseBackgroundTrack );

	if( !options.useNullSystem ) {
		s_instance = wsw::snd::ALSoundSystem::tryCreate( client, options.verbose );
		if( s_instance ) {
			s_instance->postInit();
			return true;
		}
	}

	::nullSoundSystemHolder.init( client );
	s_instance = nullSoundSystemHolder.instance();
	s_instance->postInit();
	return options.useNullSystem;
}

void SoundSystem::shutdown( bool verbose ) {
	CL_Cmd_Unregister( "music"_asView );
	CL_Cmd_Unregister( "stopmusic"_asView );
	CL_Cmd_Unregister( "prevmusic"_asView );
	CL_Cmd_Unregister( "nextmusic"_asView );
	CL_Cmd_Unregister( "pausemusic"_asView );

	if( s_instance ) {
		s_instance->deleteSelf( verbose );
		s_instance = nullptr;
	}
}

auto SoundSystem::getExactName( const SoundSet *sound ) -> std::optional<wsw::StringView> {
	if( sound ) {
		if( const auto *exact = std::get_if<SoundSetProps::Exact>( &sound->props.name ) ) {
			return exact->value;
		}
	}
	return std::nullopt;
}

ALenum S_SoundFormat( int width, int channels ) {
	if( width == 1 ) {
		if( channels == 1 ) {
			return AL_FORMAT_MONO8;
		} else if( channels == 2 ) {
			return AL_FORMAT_STEREO8;
		}
	} else if( width == 2 ) {
		if( channels == 1 ) {
			return AL_FORMAT_MONO16;
		} else if( channels == 2 ) {
			return AL_FORMAT_STEREO16;
		}
	}

	Com_Printf( "Unknown sound format: %i channels, %i bits.\n", channels, width * 8 );
	return AL_FORMAT_MONO16;
}

/*
* S_GetBufferLength
*
* Returns buffer length expressed in milliseconds
*/
ALuint S_GetBufferLength( ALuint buffer ) {
	ALint size, bits, channels, freq;

	alGetBufferi( buffer, AL_SIZE, &size );
	alGetBufferi( buffer, AL_BITS, &bits );
	alGetBufferi( buffer, AL_FREQUENCY, &freq );
	alGetBufferi( buffer, AL_CHANNELS, &channels );

	if( alGetError() != AL_NO_ERROR ) {
		return 0;
	}
	return (ALuint)( (ALfloat)( size / ( bits / 8 ) / channels ) * 1000.0 / freq + 0.5f );
}

const char *S_ErrorMessage( ALenum error ) {
	switch( error ) {
		case AL_NO_ERROR:
			return "No error";
		case AL_INVALID_NAME:
			return "Invalid name";
		case AL_INVALID_ENUM:
			return "Invalid enumerator";
		case AL_INVALID_VALUE:
			return "Invalid value";
		case AL_INVALID_OPERATION:
			return "Invalid operation";
		case AL_OUT_OF_MEMORY:
			return "Out of memory";
		default:
			return "Unknown error";
	}
}

void S_Trace( trace_t *tr, const vec3_t start,
			  const vec3_t end, const vec3_t mins,
			  const vec3_t maxs, int mask, int topNodeHint ) {
	if( cl.cms ) {
		CM_TransformedBoxTrace( cl.cms, tr, start, end, mins, maxs, nullptr, mask, nullptr, nullptr, topNodeHint );
		return;
	}

	::memset( tr, 0, sizeof( trace_t ) );
	tr->fraction = 1.0f;
}

int S_PointContents( const float *p, int topNodeHint ) {
	if( cl.cms ) {
		return CM_TransformedPointContents( cl.cms, p, nullptr, nullptr, nullptr, topNodeHint );
	}
	return 0;
}

int S_PointLeafNum( const vec3_t p, int topNodeHint ) {
	if( cl.cms ) {
		return CM_PointLeafnum( cl.cms, p, topNodeHint );
	}
	return 0;
}

int S_NumLeafs() {
	if( cl.cms ) {
		return CM_NumLeafs( cl.cms );
	}
	return 0;
}

const vec3_t *S_GetLeafBounds( int leafnum ) {
	if( cl.cms ) {
		return CM_GetLeafBounds( cl.cms, leafnum );
	}
	return nullptr;
}

bool S_LeafsInPVS( int leafNum1, int leafNum2 ) {
	if( cl.cms ) {
		return ( leafNum1 == leafNum2 ) || CM_LeafsInPVS( cl.cms, leafNum1, leafNum2 );
	}
	return true;
}

int S_FindTopNodeForBox( const vec3_t mins, const vec3_t maxs ) {
	if( cl.cms ) {
		return CM_FindTopNodeForBox( cl.cms, mins, maxs );
	}
	return 0;
}

int S_FindTopNodeForSphere( const vec3_t center, float radius ) {
	if( cl.cms ) {
		return CM_FindTopNodeForSphere( cl.cms, center, radius );
	}
	return 0;
}

const char *S_GetConfigString( int index ) {
	return cl.configStrings.get( index ).value_or( wsw::StringView() ).data();
}

unsigned S_SuggestNumExtraThreadsForComputations() {
	if( const auto maybeNumberOfProcessors = Sys_GetNumberOfProcessors() ) {
		const auto [numPhysicalProcessors, numLogicalProcessors] = *maybeNumberOfProcessors;
		const unsigned chosenNumProcessors = developer->integer ? numLogicalProcessors : numPhysicalProcessors;
		if( chosenNumProcessors ) {
			// Take the current thread (which also acts as a worker thread for the task system) into account.
			return chosenNumProcessors - 1;
		}
	}
	// Use only the current thread.
	return 0;
}