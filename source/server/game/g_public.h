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

//===============================================================

#include <common/cmdsystem.h>

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
	vec3_t absmin, absmax, size;  // Caution: These bounds are for broadphase collision. They are overly large for rotated stuff.
	solid_t solid;
	int clipmask;
	edict_t *owner;
} entity_shared_t;

struct CMShapeList;

struct CmdArgs;

class DeclaredConfigVar;

//
// Game exports
//

void G_Init( unsigned seed, unsigned framemsec, int protocol, const char *demoExtension );
void G_Shutdown();
void G_InitLevel( char *mapname, char *entities, int entstrlen, int64_t levelTime, int64_t serverTime, int64_t realTime );

void G_RunFrame( unsigned msec, int64_t serverTime );
void G_SnapFrame();
void G_ClearSnap();

const game_state_t *G_GetGameState();
const ReplicatedScoreboardData *G_GetScoreboardDataForClient( unsigned clientNum );
const ReplicatedScoreboardData *G_GetScoreboardDataForDemo();

bool G_ClientConnect( edict_t *ent, char *userinfo, bool fakeClient );
void G_ClientBegin( edict_t *ent );
void G_ClientUserinfoChanged( edict_t *ent, char *userinfo );
void G_ClientDisconnect( edict_t *ent, const char *reason );
void G_ClientCommand( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs );
void G_ClientThink( edict_t *ent, usercmd_t *cmd, int timeDelta );

// Game imports

int SVC_FakeConnect( const char *fakeUserinfo, const char *fakeSocketType, const char *fakeIP );
void SV_LocateEntities( struct edict_s *edicts, int edict_size, int num_edicts, int max_edicts );

void SV_Cmd_Register( const char *name, CmdFunc cmdFunc, CompletionQueryFunc completionFunc = nullptr );
void SV_Cmd_Unregister( const char *name );
void SV_Cmd_ExecuteText( int when, const char *text );
void SV_Cbuf_ExecutePendingCommands();

void SV_ExecuteClientThinks( int clientNum );

// Can't figure out better names rather than prefixing by G_
void G_DropClient( struct edict_s *ent, ReconnectBehaviour reconnectBehaviour, const char *message = nullptr );
int G_GetClientState( int numClient );

bool SV_CompressBytes( void *dst, size_t *dstSize, const void *src, size_t srcSize );

void SV_DispatchGameCmd( const struct edict_s *ent, const char *cmd );
void SV_DispatchServerCmd( const struct edict_s *ent, const char *cmd );

void SV_SetConfigString( int num, const char *string );
const char *SV_GetConfigString( int num );

void SV_PureSound( const char *name );
void SV_PureModel( const char *name );

int SV_ModelIndex( const char *name );
int SV_SoundIndex( const char *name );
int SV_ImageIndex( const char *name );
int SV_SkinIndex( const char *name );

bool SV_InPVS( const vec3_t p1, const vec3_t p2 );
int SV_TransformedPointContents( const vec3_t p, const struct cmodel_s *cmodel,	const vec3_t origin, const vec3_t angles, int topNodeHint = 0 );

void SV_TransformedBoxTrace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins,
							 const vec3_t maxs, const struct cmodel_s *cmodel, int brushmask,
							 const vec3_t origin, const vec3_t angles, int topNodeHint = 0 );

int SV_NumInlineModels();
struct cmodel_s *SV_InlineModel( int num );
void SV_InlineModelBounds( const struct cmodel_s *cmodel, vec3_t mins, vec3_t maxs );
struct cmodel_s *SV_ModelForBBox( const vec3_t mins, const vec3_t maxs );
struct cmodel_s *SV_OctagonModelForBBox( const vec3_t mins, const vec3_t maxs );
void SV_SetAreaPortalState( int area, int otherarea, bool open );

int SV_BoxLeafnums( const vec3_t mins, const vec3_t maxs, int *list, int listsize, int *topnode, int topNodeHint = 0 );

int SV_LeafCluster( int leafnum );
int SV_LeafArea( int leafnum );
int SV_LeafsInPVS( int leafnum1, int leafnum2 );
int SV_FindTopNodeForBox( const vec3_t mins, const vec3_t maxs, unsigned maxValue = ~( 0u ) );
int SV_FindTopNodeForSphere( const vec3_t center, float radius, unsigned maxValue = ~( 0u ) );

CMShapeList *SV_AllocShapeList();
void SV_FreeShapeList( CMShapeList *list );
int SV_PossibleShapeListContents( const CMShapeList *list );
CMShapeList *SV_BuildShapeList( CMShapeList *list, const float *mins, const float *maxs, int clipMask );
void SV_ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs );
void SV_ClipToShapeList( const CMShapeList *list, trace_t *tr, const float *start, const float *end, const float *mins, const float *maxs, int clipMask );