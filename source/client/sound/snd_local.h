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
// snd_local.h -- private OpenAL sound functions

#ifndef SND_OPENAL_LOCAL_H
#define SND_OPENAL_LOCAL_H

//#define VORBISLIB_RUNTIME // enable this define for dynamic linked vorbis libraries

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/q_collision.h>
#include <common/facilities/cvar.h>
#include <common/facilities/messagestreams.h>
#include <common/types/podbuffer.h>
#include <client/snd_public.h>

#define AL_ALEXT_PROTOTYPES
#define AL_LIBTYPE_STATIC

#include "filedatabuffercache.h"

#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alext.h>
#include <AL/efx.h>

#include "efxpresetsregistry.h"

#define MAX_SRC 128

struct SoundSet {
	SoundSet *prev { nullptr }, *next { nullptr };
	SoundSetProps props;

	mutable int registrationSequence { 0 };

	static constexpr unsigned kMaxBuffers { 16 };
	const FileDataBuffer *buffers[kMaxBuffers];
	unsigned numBuffers { 0 };

	static constexpr unsigned kMaxPitchVariations { 16 };
	float pitchVariations[kMaxPitchVariations] {};
	unsigned numPitchVariations { 0 };

	bool hasFailedLoading { false };
	bool isLoaded { false };
	mutable uint8_t lastChosenBufferIndex { 0 };
	mutable uint8_t lastChosenPitchIndex { 0 };
};

extern cvar_t *s_volume;
extern cvar_t *s_musicvolume;
extern cvar_t *s_sources;
extern cvar_t *s_stereo2mono;

extern cvar_t *s_doppler;
extern cvar_t *s_sound_velocity;
extern cvar_t *s_environment_effects;
extern cvar_t *s_environment_sampling_quality;
extern cvar_t *s_effects_number_threshold;
extern cvar_t *s_hrtf;

#define SRCPRI_AMBIENT  0   // Ambient sound effects
#define SRCPRI_LOOP 1   // Looping (not ambient) sound effects
#define SRCPRI_ONESHOT  2   // One-shot sounds
#define SRCPRI_LOCAL    3   // Local sounds
#define SRCPRI_STREAM   4   // Streams (music, cutscenes)

[[nodiscard]]
static inline auto checkSourceGain( float givenVolume ) -> float {
	assert( givenVolume >= 0.0f && givenVolume < 1000.0f );
	return givenVolume;
}

class SourceManager;

// music
void S_StartBackgroundTrack( SourceManager *, const char *intro, const char *loop, int mode );
void S_StopBackgroundTrack( SourceManager * );
void S_PrevBackgroundTrack();
void S_NextBackgroundTrack( void );
void S_PauseBackgroundTrack( void );
void S_LockBackgroundTrack( bool lock );

/*
* Util (snd_al.c)
*/
ALenum S_SoundFormat( int width, int channels );
const char *S_ErrorMessage( ALenum error );
ALuint S_GetBufferLength( ALuint buffer );
void *S_BackgroundUpdateProc( void *param );

static constexpr float REVERB_ENV_DISTANCE_THRESHOLD = 4096.0f;

struct Source;
struct ReverbEffectProps;

bool IsEffectActive( const Source *src );
void UpdateSourceEffectProps( Source *src, const ReverbEffectProps &effectProps, const vec3_t listenerOrigin );
void UpdateSourceEffectPanning( Source *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes );

/*
* Music
*/
void S_UpdateMusic( SourceManager * );

/*
* Decoder
*/
typedef struct snd_info_s {
	// TODO: Some fields are redundant
	int sampleRate;
	int bytesPerSample;
	int numChannels;
	// Length of the data in samples per channel (i.e. stereo data contains 2x samplesPerChannel samples)
	int samplesPerChannel;
	int sizeInBytes;
} snd_info_t;

typedef struct snd_decoder_s snd_decoder_t;
typedef struct snd_stream_s {
	snd_decoder_t *decoder;
	bool isUrl;
	snd_info_t info; // TODO: Change to AL_FORMAT?
	void *ptr; // decoder specific stuff
} snd_stream_t;

typedef struct bgTrack_s {
	char *filename;
	bool ignore;
	bool loop;
	bool muteOnPause;
	snd_stream_t *stream;

	struct bgTrack_s *next; // the next track to be played, the looping part aways points to itself
	struct bgTrack_s *prev; // previous track in the playlist
	struct bgTrack_s *anext; // allocation linked list
} bgTrack_t;

bool S_InitDecoders( bool verbose );
void S_ShutdownDecoders( bool verbose );
bool S_LoadSound( const char *filename, PodBuffer<uint8_t> *dataBuffer, snd_info_t *info );
snd_stream_t *S_OpenStream( const char *filename, bool *delay );
bool S_ContOpenStream( snd_stream_t *stream );
int S_ReadStream( snd_stream_t *stream, int bytes, void *buffer );
void S_CloseStream( snd_stream_t *stream );
bool S_ResetStream( snd_stream_t *stream );
bool S_EoStream( snd_stream_t *stream );
int S_SeekSteam( snd_stream_t *stream, int ofs, int whence );

unsigned S_SuggestNumExtraThreadsForComputations();

// This stuff is used by the sound system implementation and is defined in the client code

void S_Trace( trace_s *tr, const float *start, const float *end, const float *mins,
			  const float *maxs, int mask, int topNodeHint = 0 );

int S_PointContents( const float *p, int topNodeHint = 0 );
int S_PointLeafNum( const float *p, int topNodeHint = 0 );

int S_NumLeafs();

const vec3_t *S_GetLeafBounds( int leafnum );
bool S_LeafsInPVS( int leafNum1, int leafNum2 );

int S_FindTopNodeForBox( const float *mins, const float *maxs );
int S_FindTopNodeForSphere( const float *center, float radius );

const char *S_GetConfigString( int index );

void S_AddDebugLine( const float *from, const float *to, int color );

#define sDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Debug ) ).getWriter()
#define sNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Notice ) ).getWriter()
#define sWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Warning ) ).getWriter()
#define sError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Error ) ).getWriter()

#endif