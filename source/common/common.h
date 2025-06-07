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

// common.h -- definitions common between client and server, but not game.dll

#ifndef __QCOMMON_H
#define __QCOMMON_H

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/q_shared.h>
#include <common/facilities/q_cvar.h>
#include <common/facilities/q_collision.h>

#include <common/facilities/qfiles.h>
#include "version.h"
#include <common/facilities/q_comref.h>
#include <common/facilities/bsp.h>
#include <common/facilities/cmodel.h>

//#define	PARANOID			// speed sapping error checking

//============================================================================

struct mempool_s;

struct snapshot_s;

struct ginfo_s;
struct client_s;
struct cmodel_state_s;
struct client_entities_s;
struct fatvis_s;

//============================================================================

typedef struct msg_s {
	uint8_t *data;
	size_t maxsize;
	size_t cursize;
	size_t readcount;
	bool compressed;
} msg_t;

typedef struct msg_field_s {
	int offset;
	int bits;
	int count;
	wireType_t encoding;
} msg_field_t;

// msg.c
void MSG_Init( msg_t *buf, uint8_t *data, size_t length );
void MSG_Clear( msg_t *buf );
void *MSG_GetSpace( msg_t *buf, size_t length );
void MSG_WriteData( msg_t *msg, const void *data, size_t length );
void MSG_CopyData( msg_t *buf, const void *data, size_t length );
int MSG_SkipData( msg_t *sb, size_t length );

//============================================================================

struct usercmd_s;
struct entity_state_s;

void MSG_WriteInt8( msg_t *sb, int c );
void MSG_WriteUint8( msg_t *sb, int c );
void MSG_WriteInt16( msg_t *sb, int c );
void MSG_WriteUint16( msg_t *sb, unsigned c );
void MSG_WriteInt32( msg_t *sb, int c );
void MSG_WriteInt64( msg_t *sb, int64_t c );
void MSG_WriteUintBase128( msg_t *msg, uint64_t c );
void MSG_WriteIntBase128( msg_t *msg, int64_t c );
void MSG_WriteFloat( msg_t *sb, float f );
void MSG_WriteHalfFloat( msg_t *sb, float f );
void MSG_WriteString( msg_t *sb, const char *s );
#define MSG_WriteAngle16( sb, f ) ( MSG_WriteInt16( ( sb ), ANGLE2SHORT( ( f ) ) ) )
void MSG_WriteDeltaUsercmd( msg_t *sb, const struct usercmd_s *from, struct usercmd_s *cmd );
void MSG_WriteDeltaEntity( msg_t *msg, const struct entity_state_s *from, const struct entity_state_s *to, bool force );
void MSG_WriteDeltaPlayerState( msg_t *msg, const player_state_t *ops, const player_state_t *ps );
void MSG_WriteDeltaGameState( msg_t *msg, const game_state_t *from, const game_state_t *to );
void MSG_WriteDeltaScoreboardData( msg_t *msg, const ReplicatedScoreboardData *from, const ReplicatedScoreboardData *to );
void MSG_WriteDir( msg_t *sb, vec3_t vector );
void MSG_WriteDeltaStruct( msg_t *msg, const void *from, const void *to, const msg_field_t *fields, size_t numFields );

void MSG_BeginReading( msg_t *sb );
int MSG_ReadInt8( msg_t *msg );
int MSG_ReadUint8( msg_t *msg );
int16_t MSG_ReadInt16( msg_t *sb );
uint16_t MSG_ReadUint16( msg_t *sb );
int MSG_ReadInt32( msg_t *sb );
int64_t MSG_ReadInt64( msg_t *sb );
uint64_t MSG_ReadUintBase128( msg_t *msg );
int64_t MSG_ReadIntBase128( msg_t *msg );
float MSG_ReadFloat( msg_t *sb );
float MSG_ReadHalfFloat( msg_t *sb );
char *MSG_ReadString( msg_t *sb );
char *MSG_ReadStringLine( msg_t *sb );
#define MSG_ReadAngle16( sb ) ( SHORT2ANGLE( MSG_ReadInt16( ( sb ) ) ) )
void MSG_ReadDeltaUsercmd( msg_t *sb, const struct usercmd_s *from, struct usercmd_s *cmd );
int MSG_ReadEntityNumber( msg_t *msg, bool *remove, unsigned *byteMask );
void MSG_ReadDeltaEntity( msg_t *msg, const entity_state_t *from, entity_state_t *to, int number, unsigned byteMask );
void MSG_ReadDeltaPlayerState( msg_t *msg, const player_state_t *ops, player_state_t *ps );
void MSG_ReadDeltaGameState( msg_t *msg, const game_state_t *from, game_state_t *to );
void MSG_ReadDeltaScoreboardData( msg_t *msg, const ReplicatedScoreboardData *from, ReplicatedScoreboardData *to );
void MSG_ReadDir( msg_t *sb, vec3_t vector );
void MSG_ReadData( msg_t *sb, void *buffer, size_t length );
void MSG_ReadDeltaStruct( msg_t *msg, const void *from, void *to, size_t size, const msg_field_t *fields, size_t numFields );
int MSG_BytesLeft( const msg_t *msg );
//============================================================================

typedef struct purelist_s {
	char *filename;
	unsigned checksum;
	struct purelist_s *next;
} purelist_t;

void Com_AddPakToPureList( purelist_t **purelist, const char *pakname, const unsigned checksum );
unsigned Com_CountPureListFiles( purelist_t *purelist );
purelist_t *Com_FindPakInPureList( purelist_t *purelist, const char *pakname );
void Com_FreePureList( purelist_t **purelist );

//============================================================================

#define SNAP_MAX_DEMO_META_DATA_SIZE    16 * 1024

// define this 0 to disable compression of demo files
#define SNAP_DEMO_GZ                    FS_GZ

void SNAP_ParseBaseline( struct msg_s *msg, entity_state_t *baselines );
void SNAP_SkipFrame( struct msg_s *msg, struct snapshot_s *header );
struct snapshot_s *SNAP_ParseFrame( struct msg_s *msg, struct snapshot_s *lastFrame, struct snapshot_s *backup, entity_state_t *baselines, int showNet );

void SNAP_WriteFrameSnapToClient( const struct ginfo_s *gi, struct client_s *client, struct msg_s *msg,
								  int64_t frameNum, int64_t gameTime,
								  const entity_state_t *baselines, const struct client_entities_s *client_entities,
								  int numcmds, const gcommand_t *commands, const char *commandsData );

void SNAP_BuildClientFrameSnap( struct cmodel_state_s *cms, struct ginfo_s *gi, int64_t frameNum, int64_t timeStamp,
								const float *skyPortalPovOrigin, struct client_s *client, const game_state_t *gameState,
								const ReplicatedScoreboardData *scoreboardData, struct client_entities_s *client_entities );

void SNAP_FreeClientFrames( struct client_s *client );

void SNAP_RecordDemoMessage( int demofile, struct msg_s *msg, int offset );
int SNAP_ReadDemoMessage( int demofile, struct msg_s *msg );

void SNAP_BeginDemoRecording( int demofile, unsigned int spawncount, unsigned int snapFrameTime,
							  const char *sv_name, unsigned int sv_bitflags, struct purelist_s *purelist,
							  char *configstrings, entity_state_t *baselines );

namespace wsw { class ConfigStringStorage; }

void SNAP_BeginDemoRecording( int demofile, unsigned int spawncount, unsigned int snapFrameTime,
							  const char *sv_name, unsigned int sv_bitflags, struct purelist_s *purelist,
							  const wsw::ConfigStringStorage &configStrings, entity_state_t *baselines );

void SNAP_StopDemoRecording( int demofile );
void SNAP_WriteDemoMetaData( const char *filename, const char *meta_data, size_t meta_data_realsize );
size_t SNAP_ClearDemoMeta( char *meta_data, size_t meta_data_max_size );
size_t SNAP_ReadDemoMetaData( int demofile, char *meta_data, size_t meta_data_size );

//============================================================================

int Com_GlobMatch( const char *pattern, const char *text, const bool casecmp );

void Info_Print( char *s );

//============================================================================

/* crc.h */
void CRC_Init( unsigned short *crcvalue );
void CRC_ProcessByte( unsigned short *crcvalue, uint8_t data );
unsigned short CRC_Value( unsigned short crcvalue );
unsigned short CRC_Block( uint8_t *start, int count );

/*
==============================================================

PROTOCOL

==============================================================
*/

// protocol.h -- communications protocols

//=========================================

#define PORT_INFO_SERVER    27950
#define PORT_SERVER         44400
#define PORT_HTTP_SERVER    44444
#define NUM_BROADCAST_PORTS 5

//=========================================

#define UPDATE_BACKUP   32  // copies of entity_state_t to keep buffered
// must be power of two

#define UPDATE_MASK ( UPDATE_BACKUP - 1 )

//==================
// the svc_strings[] array in snapshot.c should mirror this
//==================
extern const char * const svc_strings[256];
void _SHOWNET( msg_t *msg, const char *s, int shownet );

//
// server to client
//
enum svc_ops_e {
	svc_bad,

	// the rest are private to the client and server
	svc_nop,
	svc_servercmd,          // [string] string
	svc_serverdata,         // [int] protocol ...
	svc_spawnbaseline,
	svc_download,           // [short] size [size bytes]
	svc_playerinfo,         // variable
	svc_packetentities,     // [...]
	svc_gamecommands,
	svc_match,
	svc_scoreboard,
	svc_clcack,
	svc_servercs,           //tmp jalfixme : send reliable commands as unreliable
	svc_frame,
	svc_demoinfo,
	svc_extension           // for future expansion
};

//==============================================

//
// client to server
//
enum clc_ops_e {
	clc_bad,
	clc_nop,
	clc_move,               // [[usercmd_t]
	clc_svcack,
	clc_clientcommand,      // [string] message
	clc_extension
};

//==============================================

// serverdata flags
#define SV_BITFLAGS_PURE            ( 1 << 0 )
#define SV_BITFLAGS_RELIABLE        ( 1 << 1 )
#define SV_BITFLAGS_HTTP            ( 1 << 3 )
#define SV_BITFLAGS_HTTP_BASEURL    ( 1 << 4 )

// framesnap flags
#define FRAMESNAP_FLAG_DELTA        ( 1 << 0 )
#define FRAMESNAP_FLAG_ALLENTITIES  ( 1 << 1 )
#define FRAMESNAP_FLAG_MULTIPOV     ( 1 << 2 )

/*
==============================================================

Library

Dynamic library loading

==============================================================
*/

#ifdef __cplusplus
#define EXTERN_API_FUNC    extern "C"
#else
#define EXTERN_API_FUNC    extern
#endif

// common/library.c
typedef struct dllfunc_s { const char *name; void **funcPointer; } dllfunc_t;

void Com_UnloadLibrary( void **lib );
void *Com_LoadLibrary( const char *name, dllfunc_t *funcs ); // NULL-terminated array of functions
void *Com_LoadSysLibrary( const char *name, dllfunc_t *funcs ); // NULL-terminated array of functions
void *Com_LibraryProcAddress( void *lib, const char *name );

/*
==============================================================

CMD

Command text buffering and command execution

==============================================================
*/

/*

Any number of commands can be added in a frame, from several different sources.
Most commands come from either keybindings or console line input, but remote
servers can also send across commands and entire text files can be execed.

The + command line options are also added to the command buffer.
*/

//===========================================================================

/*

Command execution takes a null terminated string, breaks it into tokens,
then searches for a command or variable that matches the first token.

*/



/*
==============================================================

CVAR

==============================================================
*/

#include <common/facilities/cvar.h>
#include <common/facilities/net.h>

//============================================================================

typedef struct {
	const socket_t *socket;

	int dropped;                // between last packet and previous

	netadr_t remoteAddress;
	int game_port;              // game port value to write when transmitting

	// sequencing variables
	int incomingSequence;
	int incoming_acknowledged;
	int outgoingSequence;

	// incoming fragment assembly buffer
	int fragmentSequence;
	size_t fragmentLength;
	uint8_t fragmentBuffer[MAX_MSGLEN];

	// outgoing fragment buffer
	// we need to space out the sending of large fragmented messages
	bool unsentFragments;
	size_t unsentFragmentStart;
	size_t unsentLength;
	uint8_t unsentBuffer[MAX_MSGLEN];
	bool unsentIsCompressed;

	bool fatal_error;
} netchan_t;

void Netchan_Init( void );
void Netchan_Shutdown( void );
void Netchan_Setup( netchan_t *chan, const socket_t *socket, const netadr_t *address, int qport );
bool Netchan_Process( netchan_t *chan, msg_t *msg );
bool Netchan_Transmit( netchan_t *chan, msg_t *msg );
bool Netchan_PushAllFragments( netchan_t *chan );
bool Netchan_TransmitNextFragment( netchan_t *chan );
int Netchan_CompressMessage( msg_t *msg, uint8_t tmpBuffer[MAX_MSGLEN] );
int Netchan_DecompressMessage( msg_t *msg, uint8_t tmpBuffer[MAX_MSGLEN] );
void Netchan_OutOfBand( const socket_t *socket, const netadr_t *address, size_t length, const uint8_t *data );

#ifndef _MSC_VER
void Netchan_OutOfBandPrint( const socket_t *socket, const netadr_t *address, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
void Netchan_OutOfBandPrint( const socket_t *socket, const netadr_t *address, _Printf_format_string_ const char *format, ... );
#endif

int Netchan_GamePort( void );

/*
==============================================================

FILESYSTEM

==============================================================
*/

#define FS_NOTIFY_NEWPAKS   0x01

void        FS_Init( void );
int         FS_Rescan( void );
void        FS_Frame( void );
void        FS_Shutdown( void );

#include <common/types/stringview.h>

static inline const wsw::StringView kDataDirectory( "basewsw" );

int         FS_GetExplicitPurePakList( char ***paknames );
bool        FS_IsExplicitPurePak( const char *pakname, bool *wrongver );

// handling of absolute filenames
// only to be used if necessary (library not supporting custom file handling functions etc.)
const char *FS_WriteDirectory( void );
const char *FS_CacheDirectory( void );
const char *FS_DownloadsDirectory( void );
const char *FS_RuntimeDirectory( void );
void        FS_CreateAbsolutePath( const char *path );
const char *FS_AbsoluteNameForFile( const char *filename );
const char *FS_AbsoluteNameForBaseFile( const char *filename );

// // game and base files
// file streaming
int     FS_FOpenFile( const char *filename, int *filenum, int mode );
int     FS_FOpenBaseFile( const char *filename, int *filenum, int mode );
int     FS_FOpenAbsoluteFile( const char *filename, int *filenum, int mode );
void    FS_FCloseFile( int file );

int     FS_Read( void *buffer, size_t len, int file );
int     FS_Print( int file, const char *msg );

#ifndef _MSC_VER
int FS_Printf( int file, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
int FS_Printf( int file, _Printf_format_string_ const char *format, ... );
#endif

int     FS_Write( const void *buffer, size_t len, int file );
int     FS_Tell( int file );
int     FS_Seek( int file, int offset, int whence );
int     FS_Eof( int file );
int     FS_Flush( int file );
int     FS_FileNo( int file, size_t *offset );

void    FS_SetCompressionLevel( int file, int level );
int     FS_GetCompressionLevel( int file );

// file loading
int     FS_LoadFileExt( const char *path, int flags, void **buffer, void *stack, size_t stackSize, const char *filename, int fileline );
int     FS_LoadBaseFileExt( const char *path, int flags, void **buffer, void *stack, size_t stackSize, const char *filename, int fileline );
void    FS_FreeFile( void *buffer );
void    FS_FreeBaseFile( void *buffer );
#define FS_LoadFile( path,buffer,stack,stacksize ) FS_LoadFileExt( path,0,buffer,stack,stacksize,__FILE__,__LINE__ )
#define FS_LoadBaseFile( path,buffer,stack,stacksize ) FS_LoadBaseFileExt( path,0,buffer,stack,stacksize,__FILE__,__LINE__ )
#define FS_LoadCacheFile( path,buffer,stack,stacksize ) FS_LoadFileExt( path,FS_CACHE,buffer,stack,stacksize,__FILE__,__LINE__ )

/**
* Maps an existing file on disk for reading.
* Does *not* work for compressed virtual files.
*
* @return mapped pointer to data on disk or NULL if mapping failed or passed size is 0.
*/
void    *FS_MMapBaseFile( int file, size_t size, size_t offset );
void    FS_UnMMapBaseFile( int file, void *data );

int     FS_GetNotifications( void );
int     FS_RemoveNotifications( int bitmask );

// util functions
bool    FS_CopyFile( const char *src, const char *dst );
bool    FS_CopyBaseFile( const char *src, const char *dst );
bool    FS_ExtractFile( const char *src, const char *dst );
bool    FS_MoveFile( const char *src, const char *dst );
bool    FS_MoveBaseFile( const char *src, const char *dst );
bool    FS_MoveCacheFile( const char *src, const char *dst );
bool    FS_RemoveFile( const char *filename );
bool    FS_RemoveBaseFile( const char *filename );
bool    FS_RemoveAbsoluteFile( const char *filename );
bool    FS_RemoveDirectory( const char *dirname );
bool    FS_RemoveBaseDirectory( const char *dirname );
bool    FS_RemoveAbsoluteDirectory( const char *dirname );
unsigned    FS_ChecksumAbsoluteFile( const char *filename );
unsigned    FS_ChecksumBaseFile( const char *filename, bool ignorePakChecksum );
bool    FS_CheckPakExtension( const char *filename );
bool    FS_PakFileExists( const char *packfilename );

time_t      FS_FileMTime( const char *filename );
time_t      FS_BaseFileMTime( const char *filename );

// // only for game files
const char *FS_FirstExtension( const char *filename, const char *extensions[], int num_extensions );
const char *FS_PakNameForFile( const char *filename );
bool    FS_IsPureFile( const char *pakname );
const char *FS_FileManifest( const char *filename );
const char *FS_BaseNameForFile( const char *filename );

int         FS_GetFileList( const char *dir, const char *extension, char *buf, size_t bufsize, int start, int end );
int         FS_GetFileListExt( const char *dir, const char *extension, char *buf, size_t *bufsize, int start, int end );

// // only for base files
bool    FS_IsPakValid( const char *filename, unsigned *checksum );
bool    FS_AddPurePak( unsigned checksum );
void    FS_RemovePurePaks( void );

/*
==============================================================

MISC

==============================================================
*/

#define MAX_PRINTMSG    3072

void        Com_BeginRedirect( int target, char *buffer, int buffersize,
							   void ( *flush )( int, const char*, const void* ), const void *extra );
void        Com_EndRedirect( void );
void        Com_DeferConsoleLogReopen( void );
void        Com_ReopenConsoleLog();
void        Com_CloseConsoleLog( bool lock, bool shutdown );

#ifndef _MSC_VER
void Com_Printf( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
void Com_DPrintf( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
void Com_Error( com_error_code_t code, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) ) __attribute__( ( noreturn ) );
void Com_Quit( const CmdArgs & ) __attribute__( ( noreturn ) );
#else
void Com_Printf( _Printf_format_string_ const char *format, ... );
void Com_DPrintf( _Printf_format_string_ const char *format, ... );
__declspec( noreturn ) void Com_Error( com_error_code_t code, _Printf_format_string_ const char *format, ... );
__declspec( noreturn ) void Com_Quit( const CmdArgs & );
#endif

void        Com_DeferQuit( void );

int         Com_ClientState( void );        // this should have just been a cvar...
void        Com_SetClientState( int state );

bool		Com_DemoPlaying( void );
void        Com_SetDemoPlaying( bool state );

int         Com_ServerState( void );        // this should have just been a cvar...
void        Com_SetServerState( int state );

extern cvar_t *developer;
extern cvar_t *dedicated;
extern cvar_t *versioncvar;

void *Q_malloc( size_t size );
void *Q_realloc( void *buf, size_t newsize );
void Q_free( void *buf );
char *Q_strdup( const char *str );

void Qcommon_Init( int argc, char **argv );
void Qcommon_Frame( unsigned realMsec, unsigned *gameMsec, float *extraTime );
void Qcommon_Shutdown( void );

/*
==============================================================

NON-PORTABLE SYSTEM SERVICES

==============================================================
*/

// directory searching
#define SFF_ARCH    0x01
#define SFF_HIDDEN  0x02
#define SFF_RDONLY  0x04
#define SFF_SUBDIR  0x08
#define SFF_SYSTEM  0x10

void    Sys_Init( int, char ** );

void    Sys_AppActivate( void );

int64_t    Sys_Milliseconds( void );
uint64_t        Sys_Microseconds( void );
void        Sys_Sleep( unsigned int millis );

char    *Sys_ConsoleInput( void );
void    Sys_ConsoleOutput( char *string );
void    Sys_SendKeyEvents( void );

#ifndef _MSC_VER
void Sys_Error( const char *error, ... ) __attribute__( ( format( printf, 1, 2 ) ) ) __attribute__( ( noreturn ) );
void Sys_Quit( void ) __attribute__( ( noreturn ) );
#else
__declspec( noreturn ) void Sys_Error( _Printf_format_string_ const char *error, ... );
__declspec( noreturn ) void Sys_Quit( void );
#endif

char    *Sys_GetClipboardData( void );
bool Sys_SetClipboardData( const char *data );
void    Sys_FreeClipboardData( char *data );

// TODO: Use a Java-like iterator
char    **Sys_GetEnvironmentVariables();
void    Sys_DeleteEnvironmentVariable( const char *name );

bool    Sys_IsBrowserAvailable( void );
void    Sys_OpenURLInBrowser( const char *url );

int     Sys_GetCurrentProcessId( void );

int Sys_GetPixelRatio();

/*
==============================================================

CPU FEATURES

==============================================================
*/

// Query only features that seem to have some utility for the code base

#define Q_CPU_FEATURE_SSE2    ( 0x1u )
#define Q_CPU_FEATURE_SSE41   ( 0x2u )
#define Q_CPU_FEATURE_SSE42   ( 0x4u )
#define Q_CPU_FEATURE_AVX     ( 0x8u )

[[nodiscard]]
unsigned Sys_GetProcessorFeatures();
[[nodiscard]]
auto Sys_GetNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>>;

/*
==============================================================

CLIENT / SERVER SYSTEMS

==============================================================
*/

void CL_Init( void );
void CL_Disconnect( const char *message, bool isCalledByBuiltinServer = false );
void CL_Shutdown( void );
void CL_Frame( int realMsec, int gameMsec );
void CL_ParseServerMessage( msg_t *msg );
void CL_Netchan_Transmit( msg_t *msg );
void Con_Print( const char *text );
void SCR_BeginLoadingPlaque( void );

void SV_Init( void );
void SV_Shutdown( const char *finalmsg );
void SV_ShutdownGame( const char *finalmsg, bool reconnect );
void SV_Frame( unsigned realMsec, unsigned gameMsec );
bool SV_SendMessageToClient( struct client_s *client, msg_t *msg );
void SV_ParseClientMessage( struct client_s *client, msg_t *msg );

/*
==============================================================

MULTITHREADING

==============================================================
*/
#include <common/helpers/qthreads.h>

#endif // __QCOMMON_H
