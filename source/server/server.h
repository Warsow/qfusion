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
// server.h

#ifndef QFUSION_SERVER_H
#define QFUSION_SERVER_H

#include <common/common.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/configstringstorage.h>
#include <common/types/staticstring.h>
#include <common/helpers/mmuuid.h>
#include <common/facilities/outputmessages.h>
#include "game/g_public.h"

#include <cstdlib>
#include <cmath>
#include <new>
#include <utility>

//=============================================================================

#define MAX_INFO_SERVERS                     16 // max recipients for heartbeat packets
#define HEARTBEAT_SECONDS               300
#define TTL_INFO_SERVERS                24 * 60 * 60

#define USERINFO_UPDATE_COOLDOWN_MSEC   2000

typedef enum {
	ss_dead,        // no map loaded
	ss_loading,     // spawning level edicts
	ss_game         // actively running
} server_state_t;

// some commands are only valid before the server has finished
// initializing (precache commands, static sounds / objects, etc)

typedef struct ginfo_s {
	struct edict_s *edicts;
	struct client_s *clients;

	int edict_size;
	int num_edicts;         // current number, <= max_edicts
	int max_edicts;
	int max_clients;        // <= sv_maxclients, <= max_edicts
} ginfo_t;

struct Client : public ServersideClientBase {};

struct edict_s {
	entity_state_t s;   // communicated by server to clients
	entity_shared_t r;  // shared by both the server system and game
};

#define EDICT_NUM( n ) ( (edict_t *)( (uint8_t *)sv.gi.edicts + sv.gi.edict_size * ( n ) ) )
#define NUM_FOR_EDICT( e ) ( ( (uint8_t *)( e ) - (uint8_t *)sv.gi.edicts ) / sv.gi.edict_size )

typedef struct {
	bool allentities;
	bool multipov;
	int clientarea;
	int numareas;
	int areabytes;
	uint8_t *areabits;                  // portalarea visibility bits
	int numplayers;
	int ps_size;
	player_state_t *ps;                 // [numplayers]
	int num_entities;
	int first_entity;                   // into the circular sv.client_entities[]
	int64_t sentTimeStamp;         // time at what this frame snap was sent to the clients
	unsigned int UcmdExecuted;
	game_state_t gameState;
	ReplicatedScoreboardData scoreboardData;
} client_snapshot_t;

typedef struct {
	char *name;
	int file;
	int size;               // total bytes (can't use EOF because of paks)
	int64_t timeout;   // so we can free the file being downloaded
	                        // if client omits sending success or failure message
} client_download_t;

typedef struct {
	int64_t framenum;
	char command[MAX_STRING_CHARS];
} game_command_t;

#define LATENCY_COUNTS  16

#define HTTP_CLIENT_SESSION_SIZE 16

typedef struct client_s {
	sv_client_state_t state;

	char userinfo[MAX_INFO_STRING];         // name, etc
	char userinfoLatched[MAX_INFO_STRING];  // flood prevention - actual userinfo updates are delayed
	int64_t userinfoLatchTimeout;

	bool reliable;                  // no need for acks, connection is reliable

	wsw::StaticString<MAX_STRING_CHARS> reliableCommands[MAX_RELIABLE_COMMANDS];
	int64_t reliableSequence;      // last added reliable message, not necesarily sent or acknowledged yet
	int64_t reliableAcknowledge;   // last acknowledged reliable message
	int64_t reliableSent;          // last sent reliable message, not necesarily acknowledged yet

	game_command_t gameCommands[MAX_RELIABLE_COMMANDS];
	int64_t gameCommandCurrent;             // position in the gameCommands table

	int64_t clientCommandExecuted; // last client-command we received

	int64_t UcmdTime;
	int64_t UcmdExecuted;          // last client-command we executed
	int64_t UcmdReceived;          // last client-command we received
	usercmd_t ucmds[CMD_BACKUP];        // each message will send several old cmds

	int64_t lastPacketSentTime;    // time when we sent the last message to this client
	int64_t lastPacketReceivedTime; // time when we received the last message from this client
	int64_t lastconnect;

	int64_t lastframe;                  // used for delta compression etc.
	bool nodelta;               // send one non delta compressed frame trough
	int64_t nodelta_frame;              // when we get confirmation of this frame, the non-delta frame is through
	int64_t lastSentFrameNum;  // for knowing which was last frame we sent

	int frame_latency[LATENCY_COUNTS];
	edict_t *edict;                 // EDICT_NUM(clientnum+1)
	char name[MAX_INFO_VALUE];      // extracted from userinfo, high bits masked
	char session[HTTP_CLIENT_SESSION_SIZE];  // session id for HTTP requests

	MovingAverage<31, int64_t> packetPings;

	client_snapshot_t snapShots[UPDATE_BACKUP]; // updates can be delta'd from here

	client_download_t download;

	int challenge;                  // challenge of this user, randomly generated

	netchan_t netchan;

	mm_uuid_t mm_session;
	mm_uuid_t mm_ticket;
	char mm_login[MAX_INFO_VALUE];

	[[nodiscard]] bool isAFakeClient() const {
		return edict && ( edict->r.svflags & SVF_FAKECLIENT );
	}
	[[nodiscard]] bool isAHiddenClient() const {
		return !edict || ( edict->r.svflags & SVF_NOCLIENT );
	}
} client_t;

// a client can leave the server in one of four ways:
// dropping properly by quiting or disconnecting
// timing out if no valid messages are received for timeout.value seconds
// getting kicked off by the server operator
// a program error, like an overflowed reliable buffer

//=============================================================================

// MAX_CHALLENGES is made large to prevent a denial
// of service attack that could cycle all of them
// out before legitimate users connected
#define MAX_CHALLENGES  1024

// MAX_SNAP_ENTITIES is the guess of what we consider maximum amount of entities
// to be sent to a client into a snap. It's used for finding size of the backup storage
#define MAX_SNAP_ENTITIES 64

typedef struct client_entities_s {
	unsigned num_entities;              // maxclients->integer*UPDATE_BACKUP*MAX_PACKET_ENTITIES
	unsigned next_entities;             // next client_entity to use
	entity_state_t *entities;           // [num_entities]
} client_entities_t;

extern cvar_t *sv_ip;
extern cvar_t *sv_port;

extern cvar_t *sv_ip6;
extern cvar_t *sv_port6;

#ifdef HTTP_SUPPORT
extern cvar_t *sv_http;
extern cvar_t *sv_http_ip;
extern cvar_t *sv_http_ipv6;
extern cvar_t *sv_http_port;
extern cvar_t *sv_http_upstream_baseurl;
extern cvar_t *sv_http_upstream_ip;
extern cvar_t *sv_http_upstream_realip_header;
#endif

extern cvar_t *sv_maxclients;

extern cvar_t *sv_uploads_http;
extern cvar_t *sv_uploads_baseurl;
extern cvar_t *sv_uploads_demos;
extern cvar_t *sv_uploads_demos_baseurl;

void SV_InitOperatorCommands( void );
void SV_ShutdownOperatorCommands( void );

void SV_UserinfoChanged( client_t *cl );

void SV_InfoServerHeartbeat( void );
void SV_InfoServerSendQuit( void );

void SV_UpdateActivity( void );

void SV_ConnectionlessPacket( const socket_t *socket, const netadr_t *address, msg_t *msg );
void SV_InitInfoServers( void );
void SV_UpdateInfoServers( void );

void SV_InitGame( void );
void SV_Map( const char *level, bool devmap );
void SV_SetServerConfigStrings( void );

void SV_AddPureFile( const wsw::StringView &fileName );
void SV_PureList_f( const CmdArgs & );

void SV_Cmd_Register( const wsw::StringView &name, CmdFunc cmdFunc, CompletionQueryFunc completionFunc = nullptr );
void SV_Cmd_Unregister( const wsw::StringView &name );

void SV_Cmd_ExecuteNow( const char *text );
void SV_Cmd_ExecuteNow2( const wsw::PodVector<char> &text );

void SV_Cbuf_AppendCommand( const char *text );

bool SV_Netchan_Transmit( netchan_t *netchan, msg_t *msg );
void SV_SendServerCommand( client_t *cl, const char *format, ... );
void SV_AddGameCommand( client_t *client, const char *cmd );
void SV_AddReliableCommandsToMessage( client_t *client, msg_t *msg );
bool SV_SendClientsFragments( void );
void SV_InitClientMessage( client_t *client, msg_t *msg, uint8_t *data, size_t size );
bool SV_SendMessageToClient( client_t *client, msg_t *msg );
void SV_ResetClientFrameCounters( void );
void SV_AddServerCommand( client_t *client, const wsw::StringView &cmd );
void SV_SendConfigString( client_t *cl, int index, const wsw::StringView &string );

typedef struct {
	const socket_t *socket;
	const netadr_t *address;
} flush_params_t;

void SV_FlushRedirect( int sv_redirected, const char *outputbuf, const void *extra );
void SV_SendClientMessages( void );

#ifndef _MSC_VER
void SV_BroadcastCommand( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
#else
void SV_BroadcastCommand( _Printf_format_string_ const char *format, ... );
#endif

void SV_ParseClientMessage( client_t *client, msg_t *msg );
bool SV_ClientConnect( const socket_t *socket, const netadr_t *address,
					   client_t *client, char *userinfo,
					   int game_port, int challenge, bool fakeClient,
					   mm_uuid_t ticket_id, mm_uuid_t session_id );

#ifndef _MSC_VER
void SV_DropClient( client_t *drop, ReconnectBehaviour reconnectBehaviour, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
void SV_DropClient( client_t *drop, ReconnectBehaviour reconnectBehaviour, _Printf_format_string_ const char *format, ... );
#endif

void SV_ClientResetCommandBuffers( client_t *client );
void SV_ClientCloseDownload( client_t *client );

void SV_WriteFrameSnapToClient( client_t *client, msg_t *msg );
void SV_BuildClientFrameSnap( client_t *client );

void SV_InitGameProgs( void );
void SV_ShutdownGameProgs( void );

void SV_Demo_WriteSnap( void );
void SV_Demo_Start_f( const CmdArgs & );
void SV_Demo_Stop_f( const CmdArgs & );
void SV_Demo_Cancel_f( const CmdArgs & );
void SV_Demo_Purge_f( const CmdArgs & );

void HandleClientCommand_Demolist( client_t *client, const CmdArgs &cmdArgs );
void HandleClientCommand_Demoget( client_t *client, const CmdArgs &cmdArgs );
void HandleClientCommand_Motd( client_t *client, const CmdArgs &cmdArgs );

bool SV_IsDemoDownloadRequest( const char *request );

void SV_MOTD_Update( void );

void SV_Web_Init( void );
void SV_Web_Shutdown( void );
bool SV_Web_Running( void );
const char *SV_Web_UpstreamBaseUrl( void );
bool SV_Web_AddGameClient( const char *session, int clientNum, const netadr_t *netAdr );
void SV_Web_RemoveGameClient( const char *session );

void SV_NotifyClientOfStartedBuiltinServer();
void SV_NotifyBuiltinServerOfShutdownGameRequest();

#define svDebug()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Server, wsw::MessageCategory::Debug ) ).getWriter()
#define svNotice()  wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Server, wsw::MessageCategory::Notice ) ).getWriter()
#define svWarning() wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Server, wsw::MessageCategory::Warning ) ).getWriter()
#define svError()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Server, wsw::MessageCategory::Error ) ).getWriter()

#endif