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

#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alext.h>
#include <AL/efx.h>

#define MAX_SRC 128

struct SoundSet {
	SoundSet *prev { nullptr }, *next { nullptr };
	SoundSetProps props;

	mutable int registrationSequence { 0 };

	ALuint buffers[16] {};
	ALuint stereoBuffers[16] {};
	unsigned bufferDurationMillis[16] {};
	float pitchVariations[16] {};
	unsigned numBuffers { 0 };
	unsigned numPitchVariations { 0 };

	bool hasFailedLoading { false };
	bool isLoaded { false };
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

// playing

void S_StartFixedSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, const vec3_t origin, int channel, float fvol, float attenuation );
void S_StartRelativeSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entnum, int channel, float fvol, float attenuation );

void S_StartLocalSound( const SoundSet *sound, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, float fvol );

void S_AddLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, int entnum, uintptr_t identifyingToken, float fvol, float attenuation );

void S_RawSamples2( unsigned int samples, unsigned int rate,
					unsigned short width, unsigned short channels, const uint8_t *data, bool music, float fvol );

// music
void S_StartBackgroundTrack( const char *intro, const char *loop, int mode );
void S_StopBackgroundTrack( void );
void S_PrevBackgroundTrack( void );
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

typedef struct {
	float quality;
	unsigned numSamples;
	uint16_t valueIndex;
} samplingProps_t;

struct PanningUpdateState {
	static constexpr auto MAX_POINTS = 80;
	vec3_t reflectionPoints[MAX_POINTS];
	unsigned numPrimaryRays;
	unsigned numPassedSecondaryRays;
};

class Effect;

typedef struct envUpdateState_s {
	const SoundSet *parent;

	int64_t nextEnvUpdateAt;
	int64_t lastEnvUpdateAt;

	Effect *oldEffect;
	Effect *effect;

	samplingProps_t directObstructionSamplingProps;

	vec3_t lastUpdateOrigin;
	vec3_t lastUpdateVelocity;

	int leafNum;

	int entNum;
	float attenuation;

	float priorityInQueue;

	bool isInLiquid;
	bool needsInterpolation;
} envUpdateState_t;

/*
* Source management
*/
typedef struct src_s {
	ALuint source;

	ALuint directFilter;
	ALuint effect;
	ALuint effectSlot;

	const SoundSet *sfx;
	unsigned bufferIndex;

	cvar_t *volumeVar;

	int64_t lastUse;    // Last time used

	uintptr_t loopIdentifyingToken;

	int priority;
	int entNum;
	int channel;
	SoundSystem::AttachmentTag attachmentTag;
	float chosenPitch;

	float fvol; // volume modifier, for s_volume updating
	float attenuation;

	bool isActive;
	bool isLocked;
	bool isLooping;
	bool isTracking;
	bool isLingering;
	bool touchedThisFrame;

	int64_t lingeringTimeoutAt;

	envUpdateState_t envUpdateState;
	PanningUpdateState panningUpdateState;

	vec3_t origin, velocity; // for local culling
} src_t;

extern src_t srclist[MAX_SRC];
extern int src_count;

bool S_InitSources( int maxEntities, bool verbose );
void S_ShutdownSources( void );
void S_UpdateSources( void );
src_t *S_AllocSource( int priority, int entnum, int channel );
src_t *S_FindSource( int entnum, int channel );
void S_LockSource( src_t *src );
void S_UnlockSource( src_t *src );
void S_StopAllSources( bool retainLocal );
ALuint S_GetALSource( const src_t *src );
src_t *S_AllocRawSource( int entNum, float fvol, float attenuation, cvar_t *volumeVar );
void S_SetEntitySpatialization( int entnum, const vec3_t origin, const vec3_t velocity, const mat3_t axis );

/*
* Music
*/
void S_UpdateMusic( void );

/*
* Stream
*/
void S_UpdateStreams( void );
void S_StopStreams( void );
void S_StopRawSamples( void );

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

unsigned S_GetRawSamplesLength( void );

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


#define sDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Debug ) ).getWriter()
#define sNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Notice ) ).getWriter()
#define sWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Warning ) ).getWriter()
#define sError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Sound, wsw::MessageCategory::Error ) ).getWriter()

#endif