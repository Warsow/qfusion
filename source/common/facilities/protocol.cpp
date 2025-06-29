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

#include <common/version.h>
#include <common/facilities/msg.h>
#include <common/facilities/protocol.h>
#include <common/facilities/configstringstorage.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/messagestreams.h>

#include <cctype>

using wsw::operator""_asView;

#define DEMO_SAFEWRITE( demofile,msg,force ) \
	if( force || ( msg )->cursize > ( msg )->maxsize / 2 ) \
	{ \
		SNAP_RecordDemoMessage( demofile, msg, 0 ); \
		MSG_Clear( msg ); \
	}

static char dummy_meta_data[SNAP_MAX_DEMO_META_DATA_SIZE];

/*
* SNAP_RecordDemoMessage
*
* Writes given message to demofile
*/
void SNAP_RecordDemoMessage( int demofile, msg_t *msg, int offset ) {
	int len;

	if( !demofile ) {
		return;
	}

	// now write the entire message to the file, prefixed by length
	len = LittleLong( msg->cursize ) - offset;
	if( len <= 0 ) {
		return;
	}

	FS_Write( &len, 4, demofile );
	FS_Write( msg->data + offset, len, demofile );
}

/*
* SNAP_ReadDemoMessage
*/
int SNAP_ReadDemoMessage( int demofile, msg_t *msg ) {
	int read = 0, msglen = -1;

	read += FS_Read( &msglen, 4, demofile );

	msglen = LittleLong( msglen );
	if( msglen == -1 ) {
		return -1;
	}

	if( msglen > MAX_MSGLEN ) {
		Com_Error( ERR_DROP, "Error reading demo file: msglen > MAX_MSGLEN" );
	}
	if( (size_t )msglen > msg->maxsize ) {
		Com_Error( ERR_DROP, "Error reading demo file: msglen > msg->maxsize" );
	}

	read = FS_Read( msg->data, msglen, demofile );
	if( read != msglen ) {
		Com_Error( ERR_DROP, "Error reading demo file: End of file" );
	}

	msg->cursize = msglen;
	msg->readcount = 0;

	return read;
}

/*
* SNAP_DemoMetaDataMessage
*/
static void SNAP_DemoMetaDataMessage( msg_t *msg, const char *meta_data, size_t meta_data_realsize ) {
	int demoinfo_len, demoinfo_len_pos, demoinfo_end;
	int meta_data_ofs, meta_data_ofs_pos;

	// demoinfo message
	MSG_WriteUint8( msg, svc_demoinfo );

	demoinfo_len_pos = msg->cursize;
	MSG_WriteInt32( msg, 0 );    // svc_demoinfo length
	demoinfo_len = msg->cursize;

	meta_data_ofs_pos = msg->cursize;
	MSG_WriteInt32( msg, 0 );    // meta data start offset
	meta_data_ofs = msg->cursize;

	if( meta_data_realsize > SNAP_MAX_DEMO_META_DATA_SIZE ) {
		meta_data_realsize = SNAP_MAX_DEMO_META_DATA_SIZE;
	}

	meta_data_ofs = msg->cursize - meta_data_ofs;
	MSG_WriteInt32( msg, meta_data_realsize );       // real size
	MSG_WriteInt32( msg, SNAP_MAX_DEMO_META_DATA_SIZE ); // max size
	MSG_WriteData( msg, meta_data, meta_data_realsize );
	MSG_WriteData( msg, dummy_meta_data, SNAP_MAX_DEMO_META_DATA_SIZE - meta_data_realsize );

	demoinfo_end = msg->cursize;
	demoinfo_len = msg->cursize - demoinfo_len;

	msg->cursize = demoinfo_len_pos;
	MSG_WriteInt32( msg, demoinfo_len ); // svc_demoinfo length
	msg->cursize = meta_data_ofs_pos;
	MSG_WriteInt32( msg, meta_data_ofs );    // meta data start offset

	msg->cursize = demoinfo_end;
}

/*
* SNAP_RecordDemoMetaDataMessage
*/
static void SNAP_RecordDemoMetaDataMessage( int demofile, msg_t *msg ) {
	int complevel;

	FS_Flush( demofile );

	complevel = FS_GetCompressionLevel( demofile );
	FS_SetCompressionLevel( demofile, 0 );

	DEMO_SAFEWRITE( demofile, msg, true );

	FS_SetCompressionLevel( demofile, complevel );

	FS_Flush( demofile );
}

/*
* SNAP_BeginDemoRecording
*/
void SNAP_BeginDemoRecording( int demofile, unsigned int spawncount, unsigned int snapFrameTime,
							  const char *sv_name, unsigned int sv_bitflags, purelist_t *purelist,
							  const wsw::ConfigStringStorage &configStrings, entity_state_t *baselines ) {
	unsigned int i;
	msg_t msg;
	uint8_t msg_buffer[MAX_MSGLEN];
	purelist_t *purefile;
	entity_state_t nullstate;
	entity_state_t *base;

	MSG_Init( &msg, msg_buffer, sizeof( msg_buffer ) );

	SNAP_DemoMetaDataMessage( &msg, "", 0 );

	SNAP_RecordDemoMetaDataMessage( demofile, &msg );

	// serverdata message
	MSG_WriteUint8( &msg, svc_serverdata );
	MSG_WriteInt32( &msg, APP_DEMO_PROTOCOL_VERSION );
	MSG_WriteInt32( &msg, spawncount );
	MSG_WriteInt16( &msg, (unsigned short)snapFrameTime );
	MSG_WriteInt16( &msg, -1 ); // playernum
	MSG_WriteString( &msg, sv_name ); // level name
	MSG_WriteUint8( &msg, sv_bitflags & ~SV_BITFLAGS_HTTP ); // sv_bitflags

	// pure files
	i = Com_CountPureListFiles( purelist );
	if( i > (short)0x7fff ) {
		Com_Error( ERR_DROP, "Error: Too many pure files." );
	}

	MSG_WriteInt16( &msg, i );

	purefile = purelist;
	while( purefile ) {
		MSG_WriteString( &msg, purefile->filename );
		MSG_WriteInt32( &msg, purefile->checksum );
		purefile = purefile->next;

		DEMO_SAFEWRITE( demofile, &msg, false );
	}

	// config strings
	for( i = 0; i < MAX_CONFIGSTRINGS; i++ ) {
		if( auto maybeConfigString = configStrings.get( i ) ) {
			MSG_WriteUint8( &msg, svc_servercs );
			MSG_WriteString( &msg, va( "cs %i \"%s\"", i, maybeConfigString->data() ) );

			DEMO_SAFEWRITE( demofile, &msg, false );
		}
	}

	// baselines
	memset( &nullstate, 0, sizeof( nullstate ) );

	for( i = 0; i < MAX_EDICTS; i++ ) {
		base = &baselines[i];
		if( base->modelindex || base->sound || base->effects ) {
			MSG_WriteUint8( &msg, svc_spawnbaseline );
			MSG_WriteDeltaEntity( &msg, &nullstate, base, true );

			DEMO_SAFEWRITE( demofile, &msg, false );
		}
	}

	// client expects the server data to be in a separate packet
	DEMO_SAFEWRITE( demofile, &msg, true );

	MSG_WriteUint8( &msg, svc_servercs );
	MSG_WriteString( &msg, "precache" );

	DEMO_SAFEWRITE( demofile, &msg, true );
}

/*
* SNAP_BeginDemoRecording
*/
void SNAP_BeginDemoRecording( int demofile, unsigned int spawncount, unsigned int snapFrameTime,
							  const char *sv_name, unsigned int sv_bitflags, purelist_t *purelist, char *configstrings,
							  entity_state_t *baselines ) {
	unsigned int i;
	msg_t msg;
	uint8_t msg_buffer[MAX_MSGLEN];
	purelist_t *purefile;
	entity_state_t nullstate;
	entity_state_t *base;

	MSG_Init( &msg, msg_buffer, sizeof( msg_buffer ) );

	SNAP_DemoMetaDataMessage( &msg, "", 0 );

	SNAP_RecordDemoMetaDataMessage( demofile, &msg );

	// serverdata message
	MSG_WriteUint8( &msg, svc_serverdata );
	MSG_WriteInt32( &msg, APP_DEMO_PROTOCOL_VERSION );
	MSG_WriteInt32( &msg, spawncount );
	MSG_WriteInt16( &msg, (unsigned short)snapFrameTime );
	MSG_WriteInt16( &msg, -1 ); // playernum
	MSG_WriteString( &msg, sv_name ); // level name
	MSG_WriteUint8( &msg, sv_bitflags & ~SV_BITFLAGS_HTTP ); // sv_bitflags

	// pure files
	i = Com_CountPureListFiles( purelist );
	if( i > (short)0x7fff ) {
		Com_Error( ERR_DROP, "Error: Too many pure files." );
	}

	MSG_WriteInt16( &msg, i );

	purefile = purelist;
	while( purefile ) {
		MSG_WriteString( &msg, purefile->filename );
		MSG_WriteInt32( &msg, purefile->checksum );
		purefile = purefile->next;

		DEMO_SAFEWRITE( demofile, &msg, false );
	}

	// config strings
	for( i = 0; i < MAX_CONFIGSTRINGS; i++ ) {
		const char *configstring = configstrings + i * MAX_CONFIGSTRING_CHARS;
		if( configstring[0] ) {
			MSG_WriteUint8( &msg, svc_servercs );
			MSG_WriteString( &msg, va( "cs %i \"%s\"", i, configstring ) );

			DEMO_SAFEWRITE( demofile, &msg, false );
		}
	}

	// baselines
	memset( &nullstate, 0, sizeof( nullstate ) );

	for( i = 0; i < MAX_EDICTS; i++ ) {
		base = &baselines[i];
		if( base->modelindex || base->sound || base->effects ) {
			MSG_WriteUint8( &msg, svc_spawnbaseline );
			MSG_WriteDeltaEntity( &msg, &nullstate, base, true );

			DEMO_SAFEWRITE( demofile, &msg, false );
		}
	}

	// client expects the server data to be in a separate packet
	DEMO_SAFEWRITE( demofile, &msg, true );

	MSG_WriteUint8( &msg, svc_servercs );
	MSG_WriteString( &msg, "precache" );

	DEMO_SAFEWRITE( demofile, &msg, true );
}

/*
* SNAP_ClearDemoMeta
*/
size_t SNAP_ClearDemoMeta( char *meta_data, size_t meta_data_max_size ) {
	memset( meta_data, 0, meta_data_max_size );
	return 0;
}

/*
* SNAP_StopDemoRecording
*/
void SNAP_StopDemoRecording( int demofile ) {
	int i;

	// finishup
	i = LittleLong( -1 );
	FS_Write( &i, 4, demofile );
}

/*
* SNAP_WriteDemoMetaData
*/
void SNAP_WriteDemoMetaData( const char *filename, const char *meta_data, size_t meta_data_realsize ) {
	unsigned i;
	unsigned v;
	char tmpn[256];
	int filenum, filelen;
	msg_t msg;
	uint8_t msg_buffer[MAX_MSGLEN];
	void *compressed_msg;

	MSG_Init( &msg, msg_buffer, sizeof( msg_buffer ) );

	// write to a temp file
	v = 0;
	for( i = 0; filename[i]; i++ ) {
		v = ( v + i ) * 37 + tolower( filename[i] ); // case insensitivity
	}
	Q_snprintfz( tmpn, sizeof( tmpn ), "%u.tmp", v );

	if( FS_FOpenFile( tmpn, &filenum, FS_WRITE | SNAP_DEMO_GZ ) == -1 ) {
		return;
	}

	SNAP_DemoMetaDataMessage( &msg, meta_data, meta_data_realsize );
	SNAP_RecordDemoMetaDataMessage( filenum, &msg );

	// now open the original file in update mode and overwrite metadata

	// important note: we need to the load the temp file before closing it
	// because in the case of gz compression, closing the file may actually
	// write some data we don't want to copy
	filelen = FS_LoadFile( tmpn, &compressed_msg, NULL, 0 );

	if( compressed_msg ) {
		int origfile;

		if( FS_FOpenFile( filename, &origfile, FS_READ | FS_UPDATE ) != -1 ) {
			FS_Write( compressed_msg, filelen, origfile );
			FS_FCloseFile( origfile );
		}
		FS_FreeFile( compressed_msg );
	}

	FS_FCloseFile( filenum );

	FS_RemoveFile( tmpn );
}

/*
* SNAP_ReadDemoMetaData
*
* Reads null-terminated meta information from a demo file into a string
*/
size_t SNAP_ReadDemoMetaData( int demofile, char *meta_data, size_t meta_data_size ) {
	char demoinfo;
	int meta_data_ofs;
	unsigned int meta_data_realsize, meta_data_fullsize;

	if( !meta_data || !meta_data_size ) {
		return 0;
	}

	// fseek to zero byte, skipping initial msg length
	if( FS_Seek( demofile, 0 + sizeof( int ), FS_SEEK_SET ) < 0 ) {
		return 0;
	}

	// read svc_demoinfo
	FS_Read( &demoinfo, 1, demofile );
	if( demoinfo != svc_demoinfo ) {
		return 0;
	}

	// skip demoinfo length
	FS_Seek( demofile, sizeof( int ), FS_SEEK_CUR );

	// read meta data offset
	FS_Read( ( void * )&meta_data_ofs, sizeof( int ), demofile );
	meta_data_ofs = LittleLong( meta_data_ofs );

	if( FS_Seek( demofile, meta_data_ofs, FS_SEEK_CUR ) < 0 ) {
		return 0;
	}

	FS_Read( ( void * )&meta_data_realsize, sizeof( int ), demofile );
	FS_Read( ( void * )&meta_data_fullsize, sizeof( int ), demofile );

	meta_data_realsize = LittleLong( meta_data_realsize );
	meta_data_fullsize = LittleLong( meta_data_fullsize );

	FS_Read( ( void * )meta_data, wsw::min( meta_data_size, (size_t)meta_data_realsize ), demofile );
	meta_data[wsw::min( (size_t)meta_data_realsize, meta_data_size - 1 )] = '\0'; // termination \0

	return meta_data_realsize;
}

auto calcSoundGainForDistanceAndAttenuation( float distance, float attenuation ) -> float {
	constexpr float refDistance = kSoundAttenuationRefDistance;
	constexpr float maxDistance = kSoundAttenuationMaxDistance;

	distance = wsw::min( wsw::max( distance, refDistance ), maxDistance );

	// AL_INVERSE_DISTANCE_CLAMPED
	// gain = AL_REFERENCE_DISTANCE / (AL_REFERENCE_DISTANCE + AL_ROLLOFF_FACTOR * (distance - AL_REFERENCE_DISTANCE));

	return refDistance * Q_Rcp( refDistance + attenuation * ( distance - refDistance ) );
}