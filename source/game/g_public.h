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

// g_public.h -- game dll information visible to server

#define GAME_API_VERSION    91

//===============================================================

#define MAX_ENT_CLUSTERS    16

typedef struct edict_s edict_t;
typedef struct Client Client;

struct ServersideClientBase {
	int m_ping;
	int m_health;
	int m_frags;
	bool m_multiview;
	player_state_t ps;

	ServersideClientBase() {
		resetShared();
	}

	void resetShared() {
		m_ping = m_health = m_frags = 0;
		m_multiview = false;
		memset( &ps, 0, sizeof( ps ) );
	}
};

typedef struct {
	Client *client;
	bool inuse;

	int num_clusters;           // if -1, use headnode instead
	int clusternums[MAX_ENT_CLUSTERS];
	int leafnums[MAX_ENT_CLUSTERS];
	int headnode;               // unused if num_clusters != -1
	int areanum, areanum2;

	//================================

	unsigned int svflags;                // SVF_NOCLIENT, SVF_MONSTER, etc
	vec3_t mins, maxs;
	vec3_t absmin, absmax, size;
	solid_t solid;
	int clipmask;
	edict_t *owner;
} entity_shared_t;

//===============================================================

namespace wsw {
	class OutputMessageStream;
	enum class MessageDomain : uint8_t;
	enum class MessageCategory : uint8_t;
}

struct CMShapeList;

struct CmdArgs;

class DeclaredConfigVar;

#include "../common/maplist.h"

//
// functions provided by the main engine
//
typedef struct {
	// special messages
	void ( *Print )( const char *msg );

	// aborts server with a game error
#ifndef _MSC_VER
	void ( *Error )( const char *msg ) __attribute__( ( noreturn ) );
#else
	void ( *Error )( const char *msg );
#endif

	// server commands sent to clients
	// Note: These commands are unreliable (the order is guaranteed to be preserved
	// but commands may be legitimately dropped due to server time adjustments on clients)
	void ( *GameCmd )( const edict_t *ent, const char *cmd );
	// reliable server commands sent to clients
	void ( *ServerCmd )( const edict_t *ent, const char *cmd );

	// config strings hold all the index strings,
	// and misc data like audio track and gridsize.
	// All of the current configstrings are sent to clients when
	// they connect, and changes are sent to all connected clients.
	void ( *ConfigString )( int num, const char *string );
	const char *( *GetConfigString )( int num );
	void ( *PureSound )( const char *name );
	void ( *PureModel )( const char *name );

	// the *index functions create configstrings and some internal server state
	int ( *ModelIndex )( const char *name );
	int ( *SoundIndex )( const char *name );
	int ( *ImageIndex )( const char *name );
	int ( *SkinIndex )( const char *name );

	int64_t ( *Milliseconds )( void );

	bool ( *inPVS )( const vec3_t p1, const vec3_t p2 );

	int ( *CM_NumInlineModels )( void );
	struct cmodel_s *( *CM_InlineModel )( int num );
	int ( *CM_TransformedPointContents )( const vec3_t p, const struct cmodel_s *cmodel, const vec3_t origin, const vec3_t angles, int topNodeHint );
	void ( *CM_TransformedBoxTrace )( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins, const vec3_t maxs, const struct cmodel_s *cmodel, int brushmask, const vec3_t origin, const vec3_t angles, int topNodeHint );
	void ( *CM_InlineModelBounds )( const struct cmodel_s *cmodel, vec3_t mins, vec3_t maxs );
	struct cmodel_s *( *CM_ModelForBBox )( const vec3_t mins, const vec3_t maxs );
	struct cmodel_s *( *CM_OctagonModelForBBox )( const vec3_t mins, const vec3_t maxs );
	void ( *CM_SetAreaPortalState )( int area, int otherarea, bool open );
	bool ( *CM_AreasConnected )( int area1, int area2 );
	int ( *CM_BoxLeafnums )( const vec3_t mins, const vec3_t maxs, int *list, int listsize, int *topnode, int topNodeHint );
	int ( *CM_LeafCluster )( int leafnum );
	int ( *CM_LeafArea )( int leafnum );
	int ( *CM_LeafsInPVS )( int leafnum1, int leafnum2 );
	int ( *CM_FindTopNodeForBox )( const vec3_t mins, const vec3_t maxs, unsigned maxValue );
	int ( *CM_FindTopNodeForSphere )( const vec3_t center, float radius, unsigned maxValue );
	CMShapeList *( *CM_AllocShapeList )();
	void ( *CM_FreeShapeList )( CMShapeList *list );
	int ( *CM_PossibleShapeListContents )( const CMShapeList *list );
	CMShapeList *( *CM_BuildShapeList )( CMShapeList *, const float *mins, const float *maxs, int clipMask );
	void ( *CM_ClipShapeList )( CMShapeList *, const CMShapeList *, const float *mins, const float *maxs );
	void ( *CM_ClipToShapeList )( const CMShapeList *list, trace_t *tr, const float *start,
		                          const float *end, const float *mins, const float *maxs, int clipMask );

	// managed memory allocation
	void *( *Mem_Alloc )( size_t size, const char *filename, int fileline );
	void ( *Mem_Free )( void *data, const char *filename, int fileline );

	// console variable interaction
	cvar_t *( *Cvar_Get )( const char *name, const char *value, int flags, class DeclaredConfigVar *controller );
	cvar_t *( *Cvar_Set )( const char *name, const char *value );
	void ( *Cvar_SetValue )( const char *name, float value );
	cvar_t *( *Cvar_ForceSet )( const char *name, const char *value );  // will return 0 0 if not found
	float ( *Cvar_Value )( const char *name );
	const char *( *Cvar_String )( const char *name );

	void ( *Cmd_AddCommand )( const char *name, void ( *cmd )( const CmdArgs & ) );
	void ( *Cmd_RemoveCommand )( const char *cmd_name );

	// files will be memory mapped read only
	// the returned buffer may be part of a larger pak file,
	// or a discrete file from anywhere in the quake search path
	// a -1 return means the file does not exist
	// NULL can be passed for buf to just determine existance
	int ( *FS_FOpenFile )( const char *filename, int *filenum, int mode );
	int ( *FS_Read )( void *buffer, size_t len, int file );
	int ( *FS_Write )( const void *buffer, size_t len, int file );
	int ( *FS_Print )( int file, const char *msg );
	int ( *FS_Tell )( int file );
	int ( *FS_Seek )( int file, int offset, int whence );
	int ( *FS_Eof )( int file );
	int ( *FS_Flush )( int file );
	void ( *FS_FCloseFile )( int file );
	bool ( *FS_RemoveFile )( const char *filename );
	int ( *FS_GetFileList )( const char *dir, const char *extension, char *buf, size_t bufsize, int start, int end );
	const char *( *FS_FirstExtension )( const char *filename, const char *extensions[], int num_extensions );
	bool ( *FS_MoveFile )( const char *src, const char *dst );
	time_t ( *FS_FileMTime )( const char *filename );
	bool ( *FS_RemoveDirectory )( const char *dirname );

	bool ( *ML_Update )( void );
	size_t ( *ML_GetListSize )();
	std::optional<MapNamesPair> ( *ML_GetMapByNum )( int num );
	bool ( *ML_FilenameExists )( const char *filename );
	const char *( *ML_GetFullname )( const char *filename );

	bool ( *Compress )( void *dst, size_t *dstSize, const void *src, size_t srcSize );

	// add commands to the server console as if they were typed in for map changing, etc
	void ( *Cmd_ExecuteText )( int exec_when, const char *text );
	void ( *Cbuf_Execute )( void );

	// a fake client connection, ClientConnect is called afterwords
	// with fakeClient set to true
	int ( *FakeClientConnect )( const char *fakeUserinfo, const char *fakeSocketType, const char *fakeIP );
	void ( *DropClient )( struct edict_s *ent, ReconnectBehaviour reconnectBehaviour, const char *message );
	int ( *GetClientState )( int numClient );
	void ( *ExecuteClientThinks )( int clientNum );

	// The edict array is allocated in the game dll so it
	// can vary in size from one game to another.
	void ( *LocateEntities )( struct edict_s *edicts, int edict_size, int num_edicts, int max_edicts );

	wsw::OutputMessageStream *( *createMessageStream )( wsw::MessageDomain, wsw::MessageCategory );
	void ( *submitMessageStream )( wsw::OutputMessageStream * );
} game_import_t;

//
// functions exported by the game subsystem
//
typedef struct {
	// if API is different, the dll cannot be used
	int ( *API )( void );

	// the init function will only be called when a game starts,
	// not each time a level is loaded.  Persistant data for clients
	// and the server can be allocated in init
	void ( *Init )( unsigned int seed, unsigned int framemsec, int protocol, const char *demoExtension );
	void ( *Shutdown )( void );

	// each new level entered will cause a call to SpawnEntities
	void ( *InitLevel )( char *mapname, char *entities, int entstrlen, int64_t levelTime, int64_t serverTime, int64_t realTime );

	bool ( *ClientConnect )( edict_t *ent, char *userinfo, bool fakeClient );
	void ( *ClientBegin )( edict_t *ent );
	void ( *ClientUserinfoChanged )( edict_t *ent, char *userinfo );
	void ( *ClientDisconnect )( edict_t *ent, const char *reason );
	void ( *ClientCommand )( edict_t *ent, uint64_t clientCommandNum, const CmdArgs & );
	void ( *ClientThink )( edict_t *ent, usercmd_t *cmd, int timeDelta );

	void ( *RunFrame )( unsigned int msec, int64_t serverTime );
	void ( *SnapFrame )( void );
	void ( *ClearSnap )( void );

	const game_state_t *( *GetGameState )();
	const ReplicatedScoreboardData *( *GetScoreboardDataForClient )( unsigned clientNum );
	const ReplicatedScoreboardData *( *GetScoreboardDataForDemo )();

	bool ( *AllowDownload )( edict_t *ent, const char *requestname, const char *uploadname );
} game_export_t;
