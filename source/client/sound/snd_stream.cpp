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

typedef struct {
	src_t *src;
	ALuint source;
	int entNum;
	ALuint samples_length;
} rawsrc_t;

#define RAW_SOUND_ENTNUM    -9999
#define MAX_RAW_SOUNDS      16

static rawsrc_t raw_sounds[MAX_RAW_SOUNDS];

static rawsrc_t *find_rawsound( int entNum ) {
	int i;
	rawsrc_t *rs, *free;

	free = NULL;
	for( i = 0; i < MAX_RAW_SOUNDS; i++ ) {
		rs = &raw_sounds[i];

		if( !free && !rs->src ) {
			free = rs;
		} else if( rs->src && rs->entNum == entNum ) {
			return rs;
		}
	}

	return free;
}

static ALuint unqueue_buffers( rawsrc_t *rs ) {
	ALuint buffer;
	int processed = 0;
	ALuint processed_length;

	if( !rs ) {
		return 0;
	}

	processed = 0;
	processed_length = 0;

	// Un-queue any processed buffers, and delete them
	alGetSourcei( rs->source, AL_BUFFERS_PROCESSED, &processed );
	while( processed-- ) {
		alSourceUnqueueBuffers( rs->source, 1, &buffer );
		processed_length += S_GetBufferLength( buffer );
		alDeleteBuffers( 1, &buffer );
	}

	return processed_length;
}

static void update_rawsound( rawsrc_t *rs ) {
	ALuint processed_length;

	if( !rs->src ) {
		return;
	}

	// Un-queue any processed buffers, and delete them
	processed_length = unqueue_buffers( rs );
	if( rs->samples_length < processed_length ) {
		rs->samples_length = 0;
	} else {
		rs->samples_length -= processed_length;
	}
}

static void stop_rawsound( rawsrc_t *rs ) {
	if( !rs->src ) {
		return;
	}
	alSourceStop( rs->source );
	unqueue_buffers( rs );
	memset( rs, 0, sizeof( *rs ) );
}

/*
* Sound system wide functions (snd_local.h)
*/
void S_UpdateStreams( void ) {
	int i;
	rawsrc_t *rs;

	for( i = 0; i < MAX_RAW_SOUNDS; i++ ) {
		rs = &raw_sounds[i];
		if( !rs->src ) {
			continue;
		}

		update_rawsound( rs );

		if( !rs->src->isActive ) {
			memset( rs, 0, sizeof( *rs ) );
		}
	}
}

void S_StopStreams( void ) {
	int i;

	for( i = 0; i < MAX_RAW_SOUNDS; i++ )
		stop_rawsound( &raw_sounds[i] );
}

/*
* S_StopRawSamples
*/
void S_StopRawSamples( void ) {
	rawsrc_t *rs;

	rs = find_rawsound( RAW_SOUND_ENTNUM );
	if( rs ) {
		stop_rawsound( rs );
	}
}

static void S_RawSamples_( int entNum, float fvol, float attenuation,
						   unsigned int samples, unsigned int rate, unsigned short width,
						   unsigned short channels, const uint8_t *data, cvar_t *volumeVar ) {
	ALuint buffer;
	ALuint format;
	ALint state;
	ALenum error;
	rawsrc_t *rs;

	rs = find_rawsound( entNum );
	if( !rs ) {
		Com_Printf( "Couldn't allocate raw sound\n" );
		return;
	}

	// Create the source if necessary
	if( !rs->src || !rs->src->isActive ) {
		rs->src = S_AllocRawSource( entNum, fvol, attenuation, volumeVar );
		if( !rs->src ) {
			Com_Printf( "Couldn't allocate streaming source\n" );
			return;
		}
		rs->samples_length = 0;
		rs->source = S_GetALSource( rs->src );
		rs->entNum = entNum;
	}

	alGenBuffers( 1, &buffer );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		Com_Printf( "Couldn't create a sound buffer (%s)\n", S_ErrorMessage( error ) );
		return;
	}

	format = S_SoundFormat( width, channels );

	alBufferData( buffer, format, data, ( samples * width * channels ), rate );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		Com_Printf( "Couldn't fill sound buffer (%s)\n", S_ErrorMessage( error ) );
		return;
	}

	alSourceQueueBuffers( rs->source, 1, &buffer );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		Com_Printf( "Couldn't queue sound buffer (%s)\n", S_ErrorMessage( error ) );
		return;
	}

	rs->samples_length += (ALuint)( (ALfloat)samples * 1000.0 / rate + 0.5f );

	rs->src->fvol = fvol;
	alSourcef( rs->source, AL_GAIN, clampSourceGain( rs->src->fvol * rs->src->volumeVar->value ) );

	alGetSourcei( rs->source, AL_SOURCE_STATE, &state );
	if( state != AL_PLAYING ) {
		alSourcePlay( rs->source );
	}
}

/*
* S_RawSamples2
*/
void S_RawSamples2( unsigned int samples, unsigned int rate, unsigned short width,
					unsigned short channels, const uint8_t *data, bool music, float fvol ) {
	S_RawSamples_( RAW_SOUND_ENTNUM, fvol, ATTN_NONE, samples, rate, width,
				   channels, data, music ? s_musicvolume : s_volume );
}

/*
* S_GetRawSamplesLength
*/
unsigned S_GetRawSamplesLength( void ) {
	rawsrc_t *rs;

	rs = find_rawsound( RAW_SOUND_ENTNUM );
	if( rs && rs->src ) {
		return rs->samples_length;
	}
	return 0;
}