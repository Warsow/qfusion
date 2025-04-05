/*
Copyright (C) 2002-2003 Victor Luchits

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

#ifndef __CG_PUBLIC_H__
#define __CG_PUBLIC_H__

struct orientation_s;
struct bonepose_s;
struct shader_s;
struct fragment_s;
struct entity_s;
struct refdef_s;
struct poly_s;
struct model_s;
struct cmodel_s;
struct qfontface_s;

typedef size_t ( *cg_async_stream_read_cb_t )( const void *buf, size_t numb, float percentage,
											 int status, const char *contentType, void *privatep );
typedef void ( *cg_async_stream_done_cb_t )( int status, const char *contentType, void *privatep );

typedef void ( *cg_raw_samples_cb_t )( void *, unsigned int, unsigned int, unsigned short, unsigned short, const uint8_t * );
typedef unsigned int ( *cg_get_raw_samples_cb_t )( void* );

typedef void ( *cg_fdrawchar_t )( int x, int y, int w, int h, float s1, float t1, float s2, float t2, const vec4_t color, const struct shader_s *shader );

#define CGAME_HARD_LINKED

//
// structs and variables shared with the main engine
//

#define MAX_PARSE_ENTITIES  1024
typedef struct snapshot_s {
	bool valid;             // cleared if delta parsing was invalid
	int64_t serverFrame;
	int64_t serverTime;    // time in the server when frame was created
	int64_t ucmdExecuted;
	bool delta;
	bool allentities;
	bool multipov;
	int64_t deltaFrameNum;
	size_t areabytes;
	uint8_t *areabits;             // portalarea visibility bits
	int numplayers;
	player_state_t playerStates[MAX_CLIENTS];
	ReplicatedScoreboardData scoreboardData;
	int numEntities;
	entity_state_t parsedEntities[MAX_PARSE_ENTITIES];
	game_state_t gameState;
	int numgamecommands;
	gcommand_t gamecommands[MAX_PARSE_GAMECOMMANDS];
	char gamecommandsData[( MAX_STRING_CHARS / 16 ) * MAX_PARSE_GAMECOMMANDS];
	size_t gamecommandsDataHead;
} snapshot_t;

//===============================================================

void CG_InitPersistentState();

void CG_Init( const char *serverName, unsigned int playerNum,
			  int vidWidth, int vidHeight, int pixelRatio,
			  bool demoplaying, const char *demoName, bool pure, unsigned snapFrameTime,
			  int protocol, const char *demoExtension, int sharedSeed, bool gameStart );
void CG_Shutdown();
void CG_Reset();
void CG_ClearEffects();

namespace wsw { class StringView; }

void CG_ConfigString( int i, const wsw::StringView &string );

struct ViewState;

void CG_GameCommand( ViewState *viewState, const wsw::StringView &fullText );
void CG_GetEntitySpatilization( int entNum, float *origin, float *velocity, float *axis );
float CG_GetSensitivityScale( float sens, float zoomSens );
bool CG_NewFrameSnap( snapshot_t *frame, snapshot_t *lerpframe );

// Less namespace-polluting than enums
struct CGRenderViewResult {
	bool hasBlittedTheMenu { false };
	bool hasBlittedTheHud { false };
	bool hasRenderedUIInternally { false };
	bool hasRunGCIfNeeded { false };
};

CGRenderViewResult CG_RenderView( int frameTime, int realFrameTime, int64_t realTime, int64_t serverTime, unsigned extrapolationTime );

void CG_InputFrame( int64_t inputTimestamp, int keyboardDeltaMillis, float mouseDeltaMillis );
void CG_ClearInputState();
bool CG_GrabsMouseMovement();
unsigned CG_GetButtonBits();
void CG_AddViewAngles( vec3_t viewAngles );
void CG_AddMovement( vec3_t movement );
void CG_MouseMove( int mx, int my );

struct CmdArgs;
void CG_OptionsStatus( const CmdArgs &cmdArgs );
void CG_ReloadOptions( const CmdArgs &cmdArgs );
void CG_ReloadCommands( const CmdArgs &cmdArgs );

#endif
