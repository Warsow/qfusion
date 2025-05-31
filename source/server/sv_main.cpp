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

#include "server.h"
#include <common/cmdsystem.h>
#include <common/maplist.h>
#include <common/singletonholder.h>
#include <common/pipeutils.h>
#include <common/compression.h>
#include <common/demometadata.h>
#include <common/profilerscope.h>
#include <common/wswtonum.h>
#include <common/wswfs.h>
#include <common/wswalgorithm.h>
#include <common/wswstringsplitter.h>
#include <common/gs_public.h>

#include <variant>
#include <string>
#include <span>

using wsw::operator""_asView;

static bool sv_initialized = false;

static struct {
	int64_t nextHeartbeat;
	int64_t lastActivity;
	unsigned int snapFrameTime;     // msecs between server packets
	unsigned int gameFrameTime;     // msecs between game code executions
	bool autostarted;
	int64_t lastInfoServerResolve;
	unsigned int autoUpdateMinute;  // the minute number we should run the autoupdate check, in the range 0 to 59
} svc; // constant server info (trully persistant since sv_init)

typedef struct {
	netadr_t adr;
	int challenge;
	int64_t time;
} challenge_t;

typedef struct {
	int file;
	char *filename;
	char *tempname;
	time_t localtime;
	int64_t basetime, duration;
	client_t client;                // special client for writing the messages
} server_static_demo_t;

static struct {
	bool initialized;               // sv_init has completed
	int64_t realtime;               // real world time - always increasing, no clamping, etc
	int64_t gametime;               // game world time - always increasing, no clamping, etc

	socket_t socket_udp;
	socket_t socket_udp6;
	socket_t socket_loopback;

	char mapcmd[MAX_TOKEN_CHARS];       // ie: *intro.cin+base

	int spawncount;                     // incremented each server start
	// used to check late spawns

	client_t *clients;                  // [sv_maxclients->integer];
	client_entities_t client_entities;

	challenge_t challenges[MAX_CHALLENGES]; // to prevent invalid IPs from connecting

	server_static_demo_t demo;

	purelist_t *purelist;               // pure file support

	cmodel_state_t *cms;                // passed to CM-functions

	char *motd;
} svs;

static struct {
	server_state_t state;       // precache commands are only valid during load

	int64_t nextSnapTime;              // always sv.framenum * svc.snapFrameTime msec
	int64_t framenum;

	char mapname[MAX_QPATH];               // map name

	wsw::ConfigStringStorage configStrings;

	entity_state_t baselines[MAX_EDICTS];

	//
	// global variables shared between game and server
	//
	ginfo_t gi;

	void clear() {
		memset( &state, 0, sizeof( state ) );
		nextSnapTime = 0;
		framenum = 0;
		mapname[0] = '\0';

		configStrings.clear();

		memset( &baselines, 0, sizeof( baselines ) );
		memset( &gi, 0, sizeof( gi ) );
	}
} sv;

class IteratorOverClients {
public:
	struct Params {
		int minAcceptableState { CS_CONNECTING };
		bool includeFakeClients { false };
		// Note: Hidden clients are not even getting put in the array of all clients (TODO Really?)
	};

	explicit IteratorOverClients( Params &&params ) : m_params( params ) {}

	[[nodiscard]]
	auto getNextWithIndex() -> std::optional<std::pair<client_t *, int>> {
		std::optional<std::pair<client_t *, int>> result;
		for( int index = m_index; index < sv_maxclients->integer; ++index ) {
			if( client_t *client = svs.clients + index; isAcceptable( client ) ) {
				m_index = index + 1;
				result  = std::make_pair( client, index );
				break;
			}
		}
		return result;
	}

	[[nodiscard]]
	auto getNext() -> client_t * {
		client_t *result = nullptr;
		for( int index = m_index; index < sv_maxclients->integer; ++index ) {
			if( client_t *client = svs.clients + index; isAcceptable( client ) ) {
				m_index = index + 1;
				result  = client;
				break;
			}
		}
		return result;
	}

	void rewind() { m_index = 0; }
private:
	[[nodiscard]]
	bool isAcceptable( const client_t *client ) const {
		if( client->state >= m_params.minAcceptableState ) {
			if( m_params.includeFakeClients || !client->isAFakeClient() ) {
				return true;
			}
		}
		return false;
	}

	int m_index { 0 };
	const Params m_params;
};

// IPv4
cvar_t *sv_ip;
cvar_t *sv_port;

// IPv6
cvar_t *sv_ip6;
cvar_t *sv_port6;

static cvar_t *sv_timeout;            // seconds without any message
static cvar_t *sv_zombietime;         // seconds to sink messages after disconnect

static cvar_t *rcon_password;         // password for remote server commands

cvar_t *sv_uploads_http;
cvar_t *sv_uploads_baseurl;
cvar_t *sv_uploads_demos;
cvar_t *sv_uploads_demos_baseurl;

static cvar_t *sv_pure;

cvar_t *sv_maxclients;

#ifdef HTTP_SUPPORT
cvar_t *sv_http;
cvar_t *sv_http_ip;
cvar_t *sv_http_ipv6;
cvar_t *sv_http_port;
cvar_t *sv_http_upstream_baseurl;
cvar_t *sv_http_upstream_ip;
cvar_t *sv_http_upstream_realip_header;
#endif

static cvar_t *sv_showRcon;
static cvar_t *sv_showChallenge;
static cvar_t *sv_showInfoQueries;

static cvar_t *sv_hostname;
static cvar_t *sv_public;         // should heartbeats be sent
static cvar_t *sv_defaultmap;

static cvar_t *sv_iplimit;

static cvar_t *sv_reconnectlimit; // minimum seconds between connect messages

// wsw : jal

static cvar_t *sv_compresspackets;
static cvar_t *sv_infoservers;
static cvar_t *sv_skilllevel;

// wsw : debug netcode
static cvar_t *sv_debug_serverCmd;

static cvar_t *sv_MOTD;
static cvar_t *sv_MOTDFile;
static cvar_t *sv_MOTDString;

static cvar_t *sv_demodir;

static void *ge;

typedef enum { RD_NONE, RD_PACKET } redirect_t;

#define SV_OUTPUTBUF_LENGTH ( MAX_MSGLEN - 16 )
char sv_outputbuf[SV_OUTPUTBUF_LENGTH];

// shared message buffer to be used for occasional messages
static msg_t tmpMessage;
static uint8_t tmpMessageData[MAX_MSGLEN];

static uint8_t g_netchanCompressionBuffer[MAX_MSGLEN];
static netchan_t g_netchanInstanceBackup;

typedef struct sv_infoserver_s {
	netadr_t address;
	bool steam;
} sv_infoserver_t;

static sv_infoserver_t sv_infoServers[MAX_INFO_SERVERS];

static int64_t accTime = 0;

void G_DropClient( edict_t *ent, ReconnectBehaviour reconnectBehaviour, const char *message ) {
	if( ent ) {
		const int entNum = NUM_FOR_EDICT( ent );
		if( entNum >= 1 && entNum <= sv_maxclients->integer ) {
			client_t *drop = svs.clients + ( entNum - 1 );
			if( message ) {
				SV_DropClient( drop, reconnectBehaviour, "%s", message );
			} else {
				SV_DropClient( drop, reconnectBehaviour, NULL );
			}
		}
	}
}

/*
* PF_GetClientState
*
* Game code asks for the state of this client
*/
int G_GetClientState( int clientNum ) {
	if( clientNum >= 0 && clientNum < sv_maxclients->integer ) {
		return svs.clients[clientNum].state;
	}
	return -1;
}

/*
* PF_GameCmd
*
* Sends the server command to clients.
* If ent is NULL the command will be sent to all connected clients
* Note: These commands are unreliable (the order is guaranteed to be preserved
* but commands may be legitimately dropped due to server time adjustments on clients)
*/
void SV_DispatchGameCmd( const edict_t *ent, const char *cmd ) {
	if( cmd && cmd[0] ) {
		if( !ent ) {
			for( int clientNum = 0; clientNum < sv_maxclients->integer; ++clientNum ) {
				if( client_t *client = svs.clients + clientNum; client->state >= CS_SPAWNED ) {
					SV_AddGameCommand( client, cmd );
				}
			}
		} else {
			const int entNum = NUM_FOR_EDICT( ent );
			if( entNum >= 1 && entNum <= sv_maxclients->integer ) {
				if( client_t *client = svs.clients + ( entNum - 1 ); client->state >= CS_SPAWNED ) {
					SV_AddGameCommand( client, cmd );
				}
			}
		}
	}
}

void SV_DispatchServerCmd( const edict_t *ent, const char *cmd ) {
	if( cmd && cmd[0] ) {
		static CmdArgsSplitter splitter;
		const CmdArgs args           = splitter.exec( wsw::StringView( cmd ) );
		bool hasPassedValidation     = false;
		constexpr unsigned maxLength = MAX_STRING_CHARS;
		bool isAllowedCommand        = false;
		// TODO: Use explicit separate calls instead of putting the filter in this narrow interface?
		// TODO: There should be better/generic facilities to notify of gametype change/restart
		for( const wsw::StringView &view: { "optionsstatus"_asView, "reloadoptions"_asView, "reloadcommands"_asView } ) {
			if( view.equalsIgnoreCase( args[0] ) ) {
				isAllowedCommand = true;
				break;
			}
		}
		if( isAllowedCommand ) {
			hasPassedValidation = true;
			// join back
			size_t totalLength = 0;
			for( unsigned i = 0; i < args.size(); ++i ) {
				totalLength += args[0].length() + 3;
				if( totalLength >= maxLength ) {
					hasPassedValidation = false;
					break;
				}
			}
		}
		if( hasPassedValidation ) {
			wsw::StaticString<MAX_STRING_CHARS> buffer;
			for( unsigned i = 0; i < args.size(); ++i ) {
				if( i == 0 ) {
					buffer << args[i];
				} else {
					buffer << '"' << args[i] << '"';
				}
				if( i + 1 != args.size() ) {
					buffer << ' ';
				}
			}
			if( !ent ) {
				for( int clientNum = 0; clientNum < sv_maxclients->integer; ++clientNum ) {
					if( client_t *client = svs.clients + clientNum; client->state >= CS_SPAWNED ) {
						SV_AddServerCommand( client, buffer.asView() );
					}
				}
			} else {
				const int entNum = NUM_FOR_EDICT( ent );
				if( entNum >= 1 && entNum <= sv_maxclients->integer ) {
					if( client_t *client = svs.clients + ( entNum - 1 ); client->state >= CS_SPAWNED ) {
						SV_AddServerCommand( client, buffer.asView() );
					}
				}
			}
		} else {
			Com_Error( ERR_DROP, "Failed to validate game-sent server command %s", cmd );
		}
	}
}

void SV_SetConfigString( int index, const char *val ) {
	if( val ) {
		if( index < 0 || index >= MAX_CONFIGSTRINGS ) {
			Com_Error( ERR_DROP, "configstring: bad index %i", index );
		} else {
			if( index < SERVER_PROTECTED_CONFIGSTRINGS ) {
				Com_Printf( "WARNING: 'SV_SetConfigstring', configstring %i is server protected\n", index );
			} else {
				if( !COM_ValidateConfigstring( val ) ) {
					Com_Printf( "WARNING: 'SV_SetConfigstring' invalid configstring %i: %s\n", index, val );
				} else {
					const wsw::StringView stringView( val );
					wsw::ConfigStringStorage &storage = sv.configStrings;

					bool hasTheSameContent = false;
					if( const std::optional<wsw::StringView> maybeExistingString = storage.get( index ) ) {
						if( maybeExistingString->equals( stringView ) ) {
							hasTheSameContent = true;
						}
					}

					if( !hasTheSameContent ) {
						storage.set( index, stringView );
						if( sv.state != ss_loading ) {
							SV_SendServerCommand( nullptr, "cs %i \"%s\"", index, val );
						}
					}
				}
			}
		}
	}
}

const char *SV_GetConfigString( int index ) {
	if( index >= 0 && index < MAX_CONFIGSTRINGS ) {
		return sv.configStrings.get( index ).value_or( wsw::StringView() ).data();
	}
	return nullptr;
}

void SV_PureSound( const char *name ) {
	if( sv.state != ss_loading ) {
		return;
	}

	if( !name || !name[0] || strlen( name ) >= MAX_QPATH ) {
		return;
	}

	char tempname[MAX_QPATH];
	Q_strncpyz( tempname, name, sizeof( tempname ) );

	bool hasExtension = COM_FileExtension( tempname ) != nullptr;
	if( !hasExtension ) {
		if( const char *extension = FS_FirstExtension( tempname, SOUND_EXTENSIONS, NUM_SOUND_EXTENSIONS ) ) {
			COM_ReplaceExtension( tempname, extension, sizeof( tempname ) );
			hasExtension = true;
		}
	}

	if( hasExtension ) {
		SV_AddPureFile( wsw::StringView( tempname ) );
	}
}

/*
* SV_AddPureShader
*
* FIXME: For now we don't parse shaders, but simply assume that it uses the same name .tga or .jpg
*/
static void SV_AddPureShader( const char *name ) {
	if( !name || !name[0] ) {
		return;
	}

	assert( name && name[0] && strlen( name ) < MAX_QPATH );
	if( !Q_strnicmp( name, "textures/common/", strlen( "textures/common/" ) ) ) {
		return;
	}

	char tempname[MAX_QPATH];
	Q_strncpyz( tempname, name, sizeof( tempname ) );

	bool hasExtension = COM_FileExtension( tempname );
	if( !hasExtension ) {
		if( const char *extension = FS_FirstExtension( tempname, IMAGE_EXTENSIONS, NUM_IMAGE_EXTENSIONS ) ) {
			COM_ReplaceExtension( tempname, extension, sizeof( tempname ) );
			hasExtension = true;
		}
	}

	if( hasExtension ) {
		SV_AddPureFile( wsw::StringView( tempname ) );
	}
}

static void SV_AddPureBSP() {
	SV_AddPureFile( sv.configStrings.getWorldModel().value() );

	int ref = 0;
	for(;; ) {
		if( const char *shader = CM_ShaderrefName( svs.cms, ref ) ) {
			SV_AddPureShader( shader );
			++ref;
		} else {
			break;
		}
	}
}

void SV_PureModel( const char *name ) {
	if( sv.state != ss_loading ) {
		return;
	}
	if( !name || !name[0] || strlen( name ) >= MAX_QPATH ) {
		return;
	}

	if( name[0] == '*' ) {  // inline model
		if( !strcmp( name, "*0" ) ) {
			SV_AddPureBSP(); // world
		}
	} else {
		SV_AddPureFile( wsw::StringView( name ) );
	}
}

bool SV_CompressBytes( void *dst, size_t *const dstSize, const void *src, size_t srcSize ) {
	unsigned long compressedSize = *dstSize;
	if( qzcompress( (Bytef *)dst, &compressedSize, (unsigned char*)src, srcSize ) == Z_OK ) {
		*dstSize = compressedSize;
		return true;
	}
	return false;
}

void SV_ShutdownGameProgs() {
	if( ge ) {
		G_Shutdown();
		ge = nullptr;
	};
}

void SV_LocateEntities( struct edict_s *edicts, int edict_size, int num_edicts, int max_edicts ) {
	if( !edicts || edict_size < (int)sizeof( entity_shared_t ) ) {
		Com_Error( ERR_DROP, "SV_LocateEntities: bad edicts" );
	}

	sv.gi.edicts      = edicts;
	sv.gi.clients     = svs.clients;
	sv.gi.edict_size  = edict_size;
	sv.gi.num_edicts  = num_edicts;
	sv.gi.max_edicts  = max_edicts;
	sv.gi.max_clients = wsw::min( num_edicts, sv_maxclients->integer );
}

static int SV_FindIndex( const char *name, int start, int max, bool createIfMissing ) {
	int resultIndex = 0;

	if( name && name[0] ) {
		const wsw::StringView nameView( name );
		if( nameView.length() >= MAX_CONFIGSTRING_CHARS ) {
			Com_Error( ERR_DROP, "Configstring too long: %s\n", name );
		}

		int testedIndex    = 1;
		bool foundMatching = false;
		// TODO: Check whether we should start from `start`, not 1
		for(; testedIndex < max; testedIndex++ ) {
			if( const std::optional<wsw::StringView> &maybeConfigString = sv.configStrings.get( start + testedIndex ) ) {
				if( maybeConfigString->equals( nameView ) ) {
					foundMatching = true;
					break;
				}
			} else {
				break;
			}
		}

		if( foundMatching ) {
			resultIndex = testedIndex;
		} else if( createIfMissing ) {
			if( testedIndex == max ) {
				Com_Error( ERR_DROP, "*Index: overflow" );
			}

			sv.configStrings.set( start + testedIndex, nameView );

			// send the update to everyone
			if( sv.state != ss_loading ) {
				SV_SendServerCommand( nullptr, "cs %i \"%s\"", start + testedIndex, name );
			}

			resultIndex = testedIndex;
		}
	}

	return resultIndex;
}

int SV_ModelIndex( const char *name ) {
	return SV_FindIndex( name, CS_MODELS, MAX_MODELS, true );
}

int SV_SoundIndex( const char *name ) {
	return SV_FindIndex( name, CS_SOUNDS, MAX_SOUNDS, true );
}

int SV_ImageIndex( const char *name ) {
	return SV_FindIndex( name, CS_IMAGES, MAX_IMAGES, true );
}

int SV_SkinIndex( const char *name ) {
	return SV_FindIndex( name, CS_SKINFILES, MAX_SKINFILES, true );
}

// TODO: Let the game access svs.cms directly once it's refactored

bool SV_InPVS( const float *p1, const float *p2 ) {
	return CM_InPVS( svs.cms, p1, p2 );
}

int SV_TransformedPointContents( const vec3_t p, const struct cmodel_s *cmodel, const vec3_t origin, const vec3_t angles, int topNodeHint ) {
	return CM_TransformedPointContents( svs.cms, p, cmodel, origin, angles, topNodeHint );
}

void SV_TransformedBoxTrace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins, const vec3_t maxs,
							 const struct cmodel_s *cmodel, int brushmask, const vec3_t origin, const vec3_t angles, int topNodeHint ) {
	CM_TransformedBoxTrace( svs.cms, tr, start, end, mins, maxs, cmodel, brushmask, origin, angles, topNodeHint );
}

int SV_NumInlineModels() {
	return CM_NumInlineModels( svs.cms );
}

struct cmodel_s *SV_InlineModel( int num ) {
	return CM_InlineModel( svs.cms, num );
}

void SV_InlineModelBounds( const struct cmodel_s *cmodel, vec3_t mins, vec3_t maxs ) {
	CM_InlineModelBounds( svs.cms, cmodel, mins, maxs );
}

struct cmodel_s *SV_ModelForBBox( const vec3_t mins, const vec3_t maxs ) {
	return CM_ModelForBBox( svs.cms, mins, maxs );
};

struct cmodel_s *SV_OctagonModelForBBox( const vec3_t mins, const vec3_t maxs ) {
	return CM_OctagonModelForBBox( svs.cms, mins, maxs );
}

bool SV_AreasConnected( int area1, int area2 ) {
	return CM_AreasConnected( svs.cms, area1, area2 );
}

void SV_SetAreaPortalState( int area, int otherarea, bool open ) {
	CM_SetAreaPortalState( svs.cms, area, otherarea, open );
}

int SV_BoxLeafnums( const vec3_t mins, const vec3_t maxs, int *list, int listsize, int *topnode, int topNodeHint ) {
	return CM_BoxLeafnums( svs.cms, mins, maxs, list, listsize, topnode, topNodeHint );
}

int SV_LeafCluster( int leafnum ) {
	return CM_LeafCluster( svs.cms, leafnum );
}

int SV_LeafArea( int leafnum ) {
	return CM_LeafArea( svs.cms, leafnum );
}

int SV_LeafsInPVS( int leafnum1, int leafnum2 ) {
	return CM_LeafsInPVS( svs.cms, leafnum1, leafnum2 );
}

int SV_FindTopNodeForBox( const vec3_t mins, const vec3_t maxs, unsigned maxValue ) {
	return CM_FindTopNodeForBox( svs.cms, mins, maxs, maxValue );
}

int SV_FindTopNodeForSphere( const vec3_t center, float radius, unsigned maxValue ) {
	return CM_FindTopNodeForSphere( svs.cms, center, radius, maxValue );
}

CMShapeList *SV_AllocShapeList() {
	return CM_AllocShapeList( svs.cms );
}

void SV_FreeShapeList( CMShapeList *list ) {
	CM_FreeShapeList( svs.cms, list );
}

int SV_PossibleShapeListContents( const CMShapeList *list ) {
	return CM_PossibleShapeListContents( list );
}

CMShapeList *SV_BuildShapeList( CMShapeList *list, const float *mins, const float *maxs, int clipMask ) {
	return CM_BuildShapeList( svs.cms, list, mins, maxs, clipMask );
}

void SV_ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs ) {
	CM_ClipShapeList( svs.cms, list, baseList, mins, maxs );
}

void SV_ClipToShapeList( const CMShapeList *list, trace_t *tr, const float *start,
						 const float *end, const float *mins, const float *maxs, int clipMask ) {
	CM_ClipToShapeList( svs.cms, list, tr, start, end, mins, maxs, clipMask );
};

void SV_InitGameProgs() {
	// unload anything we have now
	if( ge ) {
		SV_ShutdownGameProgs();
	}

	ge = (void *)1;

	SV_SetServerConfigStrings();

	G_Init( time( nullptr ), svc.snapFrameTime, APP_PROTOCOL_VERSION, APP_DEMO_EXTENSION_STR );
}

static void SV_CalcPings() {
	IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_SPAWNED, .includeFakeClients = false } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		if( client->packetPings.hasSufficentNumberOfSamples() ) [[likely]] {
			const auto newPingValue = (int)wsw::clamp<int64_t>( client->packetPings.avg(), 1, 999 );
			if( client->edict->r.client->m_ping > 0 ) {
				// Hide tiny fluctuations
				if( std::abs( client->edict->r.client->m_ping - newPingValue ) > 1 ) {
					client->edict->r.client->m_ping = newPingValue;
				}
			} else {
				client->edict->r.client->m_ping = newPingValue;
			}
		} else {
			client->edict->r.client->m_ping = 999;
		}
	}
}

static bool SV_ProcessPacket( netchan_t *netchan, msg_t *msg ) {
	// TODO: Do something more sophisticated
	g_netchanInstanceBackup = *netchan;
	if( !Netchan_Process( netchan, msg ) ) {
		return false; // wasn't accepted for some reason
	}

	// now if compressed, expand it
	MSG_BeginReading( msg );
	MSG_ReadInt32( msg ); // sequence
	MSG_ReadInt32( msg ); // sequence_ack
	MSG_ReadInt16( msg ); // game_port
	if( msg->compressed ) {
		if( const int zerror = Netchan_DecompressMessage( msg, g_netchanCompressionBuffer ); zerror < 0 ) {
			// compression error. Drop the packet
			Com_DPrintf( "SV_ProcessPacket: Compression error %i. Dropping packet\n", zerror );
			*netchan = g_netchanInstanceBackup;
			return false;
		}
	}

	return true;
}

static void SV_ReadPackets() {
	WSW_PROFILER_SCOPE();

	msg_t msg;
	uint8_t msgData[MAX_MSGLEN];

	MSG_Init( &msg, msgData, sizeof( msgData ) );

	for( socket_t *socket: { &svs.socket_loopback, &svs.socket_udp, &svs.socket_udp6 } ) {
		if( socket->open ) {
			netadr_t address;
			int ret = 0;
			while( ( ret = NET_GetPacket( socket, &address, &msg ) ) != 0 ) {
				if( ret == -1 ) {
					Com_Printf( "NET_GetPacket: Error: %s\n", NET_ErrorString() );
				} else {
					// check for connectionless packet (0xffffffff) first
					if( *(int *)msg.data == -1 ) {
						SV_ConnectionlessPacket( socket, &address, &msg );
					} else {
						// read the game port out of the message so we can fix up
						// stupid address translating routers
						MSG_BeginReading( &msg );
						MSG_ReadInt32( &msg ); // sequence number
						MSG_ReadInt32( &msg ); // sequence number
						const int game_port = MSG_ReadInt16( &msg ) & 0xffff;
						// data follows

						client_t *matchingClient = nullptr;
						IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_CONNECTING } );
						while( client_t *const client = iteratorOverClients.getNext() ) {
							if( NET_CompareBaseAddress( &address, &client->netchan.remoteAddress ) ) {
								if( client->netchan.game_port == game_port ) {
									matchingClient = client;
									break;
								}
							}
						}

						if( matchingClient ) {
							const unsigned short addr_port = NET_GetAddressPort( &address );
							if( NET_GetAddressPort( &matchingClient->netchan.remoteAddress ) != addr_port ) {
								svNotice() << "SV_ReadPackets: fixing up a translated port";
								NET_SetAddressPort( &matchingClient->netchan.remoteAddress, addr_port );
							}
							// This is a valid, sequenced packet, so process it
							if( SV_ProcessPacket( &matchingClient->netchan, &msg ) ) {
								matchingClient->lastPacketReceivedTime = svs.realtime;
								SV_ParseClientMessage( matchingClient, &msg );
							}
						}
					}
				}
			}
		}
	}
}

/*
* SV_CheckTimeouts
*
* If a packet has not been received from a client for timeout->value
* seconds, drop the conneciton.  Server frames are used instead of
* realtime to avoid dropping the local client while debugging.
*
* When a client is normally dropped, the client_t goes into a zombie state
* for a few seconds to make sure any final reliable message gets resent
* if necessary
*/
static void SV_CheckTimeouts() {
	for( int clientNum = 0; clientNum < sv_maxclients->integer; ++clientNum ) {
		client_t *const cl = svs.clients + clientNum;
		// Fake clients do not timeout
		if( cl->edict && ( cl->edict->r.svflags & SVF_FAKECLIENT ) ) {
			cl->lastPacketReceivedTime = svs.realtime;
		} else if( cl->lastPacketReceivedTime > svs.realtime ) {
			// Message times may be wrong across a changelevel
			cl->lastPacketReceivedTime = svs.realtime;
		}

		if( cl->state == CS_ZOMBIE ) {
			if( cl->lastPacketReceivedTime + 1000 * sv_zombietime->value < svs.realtime ) {
				// Can now be reused
				cl->state = CS_FREE;
			}
		} else if( cl->state != CS_FREE ) {
			if( cl->lastPacketReceivedTime + 1000 * sv_timeout->value < svs.realtime ) {
				// Don't even bother with transition to zombie state
				SV_DropClient( cl, ReconnectBehaviour::OfUserChoice, "%s", "Error: Connection timed out" );
				cl->state = CS_FREE;
			} else {
				// timeout downloads left open
				if( cl->download.name && cl->download.timeout < svs.realtime ) {
					Com_Printf( "Download of %s to %s%s timed out\n", cl->download.name, cl->name, S_COLOR_WHITE );
					SV_ClientCloseDownload( cl );
				}
			}
		}
	}
}

/*
* SV_CheckLatchedUserinfoChanges
*
* To prevent flooding other players, consecutive userinfo updates are delayed,
* and only the last one is applied.
* Applies latched userinfo updates if the timeout is over.
*/
static void SV_CheckLatchedUserinfoChanges() {
	const int64_t time = Sys_Milliseconds();
	IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_CONNECTING, .includeFakeClients = true } );
	while( client_t *client = iteratorOverClients.getNext() ) {
		if( client->userinfoLatched[0] && client->userinfoLatchTimeout <= time ) {
			Q_strncpyz( client->userinfo, client->userinfoLatched, sizeof( client->userinfo ) );
			client->userinfoLatched[0] = '\0';
			SV_UserinfoChanged( client );
		}
	}
}

#define WORLDFRAMETIME 16 // 62.5fps

static bool SV_RunGameFrame( int msec ) {
	WSW_PROFILER_SCOPE();

	accTime += msec;

	bool refreshSnapshot   = false;
	bool refreshGameModule = false;

	const bool sentFragments = SV_SendClientsFragments();

	// see if it's time to run a new game frame
	if( accTime >= WORLDFRAMETIME ) {
		refreshGameModule = true;
	}

	// see if it's time for a new snapshot
	if( !sentFragments && svs.gametime >= sv.nextSnapTime ) {
		refreshSnapshot   = true;
		refreshGameModule = true;
	}

	// if there aren't pending packets to be sent, we can sleep
	if( dedicated->integer && !sentFragments && !refreshSnapshot ) {
		const int sleeptime = wsw::min( (int)( WORLDFRAMETIME - ( accTime + 1 ) ), (int)( sv.nextSnapTime - ( svs.gametime + 1 ) ) );

		if( sleeptime > 0 ) {
			socket_t *sockets [] = { &svs.socket_udp, &svs.socket_udp6 };
			socket_t *opened_sockets [sizeof( sockets ) / sizeof( sockets[0] ) + 1 ];
			size_t sock_ind, open_ind;

			// Pass only the opened sockets to the sleep function
			open_ind = 0;
			for( sock_ind = 0; sock_ind < sizeof( sockets ) / sizeof( sockets[0] ); sock_ind++ ) {
				socket_t *sock = sockets[sock_ind];
				if( sock->open ) {
					opened_sockets[open_ind] = sock;
					open_ind++;
				}
			}
			opened_sockets[open_ind] = nullptr;

			NET_Sleep( sleeptime, opened_sockets );
		}
	}

	if( refreshGameModule ) {
		// update ping based on the last known frame from all clients
		SV_CalcPings();

		int64_t moduleTime;
		if( accTime >= WORLDFRAMETIME ) {
			moduleTime = WORLDFRAMETIME;
			accTime -= WORLDFRAMETIME;
			if( accTime >= WORLDFRAMETIME ) { // don't let it accumulate more than 1 frame
				accTime = WORLDFRAMETIME - 1;
			}
		} else {
			moduleTime = accTime;
			accTime = 0;
		}

		G_RunFrame( moduleTime, svs.gametime );
	}

	// if we don't have to send a snapshot we are done here
	if( refreshSnapshot ) {
		// set up for sending a snapshot
		sv.framenum++;
		G_SnapFrame();

		// set time for next snapshot
		int extraSnapTime = (int)( svs.gametime - sv.nextSnapTime );
		if( extraSnapTime > svc.snapFrameTime * 0.5 ) { // don't let too much time be accumulated
			extraSnapTime = svc.snapFrameTime * 0.5;
		}

		sv.nextSnapTime = svs.gametime + ( svc.snapFrameTime - extraSnapTime );

		return true;
	}

	return false;
}

void SV_UpdateActivity( void ) {
	svc.lastActivity = Sys_Milliseconds();
	//Com_Printf( "Server activity\n" );
}

void SV_Frame( unsigned realmsec, unsigned gamemsec ) {
	WSW_PROFILER_SCOPE();

	// if server is not active, do nothing
	if( !svs.initialized ) {
		if( !svc.autostarted ) {
			svc.autostarted = true;
			if( dedicated->integer ) {
				if( ( sv.state == ss_dead ) && sv_defaultmap && strlen( sv_defaultmap->string ) && !strlen( sv.mapname ) ) {
					SV_Cbuf_AppendCommand( va( "map %s\n", sv_defaultmap->string ) );
				}
			}
		}
	} else {
		svs.realtime += realmsec;
		svs.gametime += gamemsec;

		// check timeouts
		SV_CheckTimeouts();

		// get packets from clients
		SV_ReadPackets();

		// apply latched userinfo changes
		SV_CheckLatchedUserinfoChanges();

		// let everything in the world think and move
		if( SV_RunGameFrame( gamemsec ) ) {
			// send messages back to the clients that had packets read this frame
			SV_SendClientMessages();

			// write snap to server demo file
			SV_Demo_WriteSnap();

			// send a heartbeat to info servers if needed
			SV_InfoServerHeartbeat();

			// clear teleport flags, etc for next frame
			G_ClearSnap();
		}
	}
}

/*
* SV_CreateBaseline
*
* Entity baselines are used to compress the update messages
* to the clients -- only the fields that differ from the
* baseline will be transmitted
*/
static void SV_CreateBaseline() {
	for( int entNum = 1; entNum < sv.gi.num_edicts; entNum++ ) {
		if( edict_t *svent = EDICT_NUM( entNum ); svent->r.inuse ) {
			svent->s.number = entNum;
			// take current state as baseline
			sv.baselines[entNum] = svent->s;
		}
	}
}

void SV_PureList_f( const CmdArgs & ) {
	Com_Printf( "Pure files:\n" );
	for( purelist_t *pureFile = svs.purelist; pureFile; pureFile = pureFile->next ) {
		Com_Printf( "- %s (%u)\n", pureFile->filename, pureFile->checksum );
	}
}

static void SV_AddPurePak( const char *pakname ) {
	if( !Com_FindPakInPureList( svs.purelist, pakname ) ) {
		Com_AddPakToPureList( &svs.purelist, pakname, FS_ChecksumBaseFile( pakname, false ) );
	}
}

void SV_AddPureFile( const wsw::StringView &fileName ) {
	assert( fileName.isZeroTerminated() );

	if( !fileName.empty() ) {
		if( const char *pakName = FS_PakNameForFile( fileName.data() ) ) {
			Com_DPrintf( "Pure file: %s (%s)\n", pakName, fileName.data() );
			SV_AddPurePak( pakName );
		}
	}
}

static void SV_ReloadPureList() {
	Com_FreePureList( &svs.purelist );

	// *pure.(pk3|pak)
	char **paks = nullptr;
	if( int numpaks = FS_GetExplicitPurePakList( &paks ) ) {
		for( int i = 0; i < numpaks; i++ ) {
			SV_AddPurePak( paks[i] );
			Q_free( paks[i] );
		}
		Q_free( paks );
	}
}

void SV_SetServerConfigStrings() {
	wsw::StaticString<16> tmp;

	(void)tmp.assignf( "%d\n", sv_maxclients->integer );
	sv.configStrings.setMaxClients( tmp.asView() );

	sv.configStrings.setHostName( wsw::StringView( Cvar_String( "sv_hostname" ) ) );
	// Set a zero UUID at server spawn so no attempt to fetch a match UUID is performed
	// until the game module actually requests doing this by clearing the config string.
	sv.configStrings.setMatchUuid( wsw::StringView( "00000000-0000-0000-0000-000000000000" ) );
}

/*
* SV_SpawnServer
* Change the server to a new map, taking all connected clients along with it.
*/
static void SV_SpawnServer( const char *server, bool devmap ) {
	if( devmap ) {
		Cvar_ForceSet( "sv_cheats", "1" );
	}

	Cvar_FixCheatVars();

	svNotice() << "----- Server Initialization -----";
	svNotice() << "SpawnServer:" << wsw::StringView( server );

	svs.spawncount++;   // any partially connected client will be restarted

	Com_SetServerState( ss_dead );

	// wipe the entire per-level structure
	sv.clear();
	SV_ResetClientFrameCounters();
	svs.realtime = 0;
	svs.gametime = 0;
	SV_UpdateActivity();

	Q_strncpyz( sv.mapname, server, sizeof( sv.mapname ) );

	SV_SetServerConfigStrings();

	sv.nextSnapTime = 1000;

	wsw::StaticString<1024> tmp;
	(void)tmp.assignf( "maps/%s.bsp", server );
	sv.configStrings.setWorldModel( tmp.asView() );

	unsigned checksum = 0;
	CM_LoadMap( svs.cms, tmp.data(), false, &checksum );

	(void)tmp.assignf( "%d", checksum );
	sv.configStrings.setMapCheckSum( tmp.asView() );

	// reserve the first modelIndexes for inline models
	for( int i = 1; i < CM_NumInlineModels( svs.cms ); i++ ) {
		(void)tmp.assignf( "*%d", i );
		sv.configStrings.setModel( tmp.asView(), i );
	}

	// set serverinfo variable
	Cvar_FullSet( "mapname", sv.mapname, CVAR_SERVERINFO | CVAR_READONLY, true );

	//
	// spawn the rest of the entities on the map
	//

	// precache and static commands can be issued during
	// map initialization
	sv.state = ss_loading;
	Com_SetServerState( sv.state );

	// set purelist
	SV_ReloadPureList();

	// load and spawn all other entities
	G_InitLevel( sv.mapname, CM_EntityString( svs.cms ), CM_EntityStringLen( svs.cms ), 0, svs.gametime, svs.realtime );

	// run two frames to allow everything to settle
	G_RunFrame( svc.snapFrameTime, svs.gametime );
	G_RunFrame( svc.snapFrameTime, svs.gametime );

	SV_CreateBaseline(); // create a baseline for more efficient communications

	// all precaches are complete
	sv.state = ss_game;
	Com_SetServerState( sv.state );

	svNotice() << "-------------------------------------";
}

/*
* SV_InitGame
* A brand new game has been started
*/
void SV_InitGame() {
	SV_NotifyClientOfStartedBuiltinServer();

	if( svs.initialized ) {
		// cause any connected clients to reconnect
		SV_ShutdownGame( "Server restarted", true );

		// SV_ShutdownGame will also call Cvar_GetLatchedVars
	} else {
		// get any latched variable changes (sv_maxclients, etc)
		Cvar_GetLatchedVars( CVAR_LATCH );
	}

	svs.initialized = true;

	if( sv_skilllevel->integer > 2 ) {
		Cvar_ForceSet( "sv_skilllevel", "2" );
	}
	if( sv_skilllevel->integer < 0 ) {
		Cvar_ForceSet( "sv_skilllevel", "0" );
	}

	// init clients
	if( sv_maxclients->integer < 1 ) {
		Cvar_FullSet( "sv_maxclients", "8", CVAR_SERVERINFO | CVAR_LATCH, true );
	} else if( sv_maxclients->integer > MAX_CLIENTS ) {
		Cvar_FullSet( "sv_maxclients", va( "%i", MAX_CLIENTS ), CVAR_SERVERINFO | CVAR_LATCH, true );
	}

	svs.spawncount = ::rand();
	svs.clients = (client_t *)Q_malloc( sizeof( client_t ) * sv_maxclients->integer );
	svs.client_entities.num_entities = sv_maxclients->integer * UPDATE_BACKUP * MAX_SNAP_ENTITIES;
	svs.client_entities.entities = (entity_state_t *)Q_malloc( sizeof( entity_state_t ) * svs.client_entities.num_entities );

	// init network stuff

	netadr_t address {}, ipv6_address {};
	address.type = NA_NOTRANSMIT;
	ipv6_address.type = NA_NOTRANSMIT;

	if( !dedicated->integer ) {
		NET_InitAddress( &address, NA_LOOPBACK );
		if( !NET_OpenSocket( &svs.socket_loopback, SOCKET_LOOPBACK, &address, true ) ) {
			Com_Error( ERR_FATAL, "Couldn't open loopback socket: %s\n", NET_ErrorString() );
		}
	}

	bool socket_opened = false;
	if( dedicated->integer || sv_maxclients->integer > 1 ) {
		// IPv4
		NET_StringToAddress( sv_ip->string, &address );
		NET_SetAddressPort( &address, sv_port->integer );
		if( !NET_OpenSocket( &svs.socket_udp, SOCKET_UDP, &address, true ) ) {
			Com_Printf( "Error: Couldn't open UDP socket: %s\n", NET_ErrorString() );
		} else {
			socket_opened = true;
		}

		// IPv6
		NET_StringToAddress( sv_ip6->string, &ipv6_address );
		if( ipv6_address.type == NA_IP6 ) {
			NET_SetAddressPort( &ipv6_address, sv_port6->integer );
			if( !NET_OpenSocket( &svs.socket_udp6, SOCKET_UDP, &ipv6_address, true ) ) {
				Com_Printf( "Error: Couldn't open UDP6 socket: %s\n", NET_ErrorString() );
			} else {
				socket_opened = true;
			}
		} else {
			Com_Printf( "Error: invalid IPv6 address: %s\n", sv_ip6->string );
		}
	}

	if( dedicated->integer && !socket_opened ) {
		Com_Error( ERR_FATAL, "Couldn't open any socket\n" );
	}

	// init mm
	// SV_MM_Init();

	// init game
	SV_InitGameProgs();

	for( int i = 0; i < sv_maxclients->integer; i++ ) {
		edict_t *ent = EDICT_NUM( i + 1 );
		ent->s.number = i + 1;
		svs.clients[i].edict = ent;
	}

	// load the map
	assert( !svs.cms );
	svs.cms = CM_New();
	CM_AddReference( svs.cms );
}

/*
* SV_FinalMessage
*
* Used by SV_ShutdownGame to send a final message to all
* connected clients before the server goes down.  The messages are sent immediately,
* not just stuck on the outgoing message list, because the server is going
* to totally exit after returning from this function.
*/
static void SV_FinalMessage( const char *message, bool reconnect ) {
	IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_CONNECTING, .includeFakeClients = false } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		if( reconnect ) {
			SV_SendServerCommand( client, "forcereconnect \"%s\"", message );
		} else {
			SV_SendServerCommand( client, "disconnect %u \"%s\" %u", (unsigned)ReconnectBehaviour::DontReconnect,
								  message, (unsigned)ConnectionDropStage::TerminatedByServer );
		}

		SV_InitClientMessage( client, &tmpMessage, nullptr, 0 );
		SV_AddReliableCommandsToMessage( client, &tmpMessage );

		// send it twice
		SV_SendMessageToClient( client, &tmpMessage );
		SV_SendMessageToClient( client, &tmpMessage );
	}
}

/*
* SV_ShutdownGame
*
* Called when each game quits
*/
void SV_ShutdownGame( const char *finalmsg, bool reconnect ) {
	if( !svs.initialized ) {
		return;
	}

	if( svs.demo.file ) {
		SV_Demo_Stop_f( CmdArgs {} );
	}

	if( svs.clients ) {
		SV_FinalMessage( finalmsg, reconnect );
	}

	SV_ShutdownGameProgs();

	// SV_MM_Shutdown();

	SV_InfoServerSendQuit();

	NET_CloseSocket( &svs.socket_loopback );
	NET_CloseSocket( &svs.socket_udp );
	NET_CloseSocket( &svs.socket_udp6 );

	// get any latched variable changes (sv_maxclients, etc)
	Cvar_GetLatchedVars( CVAR_LATCH );

	if( svs.clients ) {
		Q_free( svs.clients );
		svs.clients = NULL;
	}

	if( svs.client_entities.entities ) {
		Q_free( svs.client_entities.entities );
		memset( &svs.client_entities, 0, sizeof( svs.client_entities ) );
	}

	if( svs.cms ) {
		// CM_ReleaseReference will take care of freeing up the memory
		// if there are no other modules referencing the collision model
		CM_ReleaseReference( svs.cms );
		svs.cms = NULL;
	}

	sv.clear();
	Com_SetServerState( sv.state );

	Com_FreePureList( &svs.purelist );

	if( svs.motd ) {
		Q_free( svs.motd );
		svs.motd = NULL;
	}

	memset( &svs, 0, sizeof( svs ) );

	svs.initialized = false;
}

/*
* SV_Map
* command from the console or progs.
*/
void SV_Map( const char *level, bool devmap ) {
	if( svs.demo.file ) {
		SV_Demo_Stop_f( CmdArgs {} );
	}

	// skip the end-of-unit flag if necessary
	if( level[0] == '*' ) {
		level++;
	}

	if( sv.state == ss_dead ) {
		// The game is just starting
		SV_InitGame();
	}

	// remove all bots before changing map
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		if( client->isAFakeClient() ) {
			SV_DropClient( client, ReconnectBehaviour::DontReconnect, nullptr );
		}
	}

	// wsw : Medar : this used to be at SV_SpawnServer, but we need to do it before sending changing
	// so we don't send frames after sending changing command
	// leave slots at start for clients only
	for( int i = 0; i < sv_maxclients->integer; i++ ) {
		// TODO: Clean up iteration here as well
		// needs to reconnect
		if( svs.clients[i].state > CS_CONNECTING ) {
			svs.clients[i].state = CS_CONNECTING;
		}

		svs.clients[i].lastframe = -1;
		memset( svs.clients[i].gameCommands, 0, sizeof( svs.clients[i].gameCommands ) );
	}

	SV_MOTD_Update();

	// TODO:??? SCR_BeginLoadingPlaque()

	SV_BroadcastCommand( "changing\n" );
	SV_SendClientMessages();
	SV_SpawnServer( level, devmap );
	SV_BroadcastCommand( "reconnect\n" );
}

/*
* SV_UserinfoChanged
*
* Pull specific info from a newly changed userinfo string
* into a more C friendly form.
*/
void SV_UserinfoChanged( client_t *client ) {
	assert( client );
	assert( Info_Validate( client->userinfo ) );

	if( !client->isAFakeClient() ) {
		// force the IP key/value pair so the game can filter based on ip
		if( !Info_SetValueForKey( client->userinfo, "socket", NET_SocketTypeToString( client->netchan.socket->type ) ) ) {
			SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Couldn't set userinfo (socket)\n" );
			return;
		}
		if( !Info_SetValueForKey( client->userinfo, "ip", NET_AddressToString( &client->netchan.remoteAddress ) ) ) {
			SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Couldn't set userinfo (ip)\n" );
			return;
		}
	}

	// mm session
	mm_uuid_t uuid = Uuid_ZeroUuid();
	const char *sessionValue = Info_ValueForKey( client->userinfo, "cl_mm_session" );
	if( sessionValue ) {
		Uuid_FromString( sessionValue, &uuid );
	}
	if( !sessionValue || !Uuid_Compare( uuid, client->mm_session ) ) {
		char uuid_buffer[UUID_BUFFER_SIZE];
		Info_SetValueForKey( client->userinfo, "cl_mm_session", Uuid_ToString( uuid_buffer, client->mm_session ) );
	}

	// mm login
	if( client->mm_login[0] != '\0' ) {
		Info_SetValueForKey( client->userinfo, "cl_mm_login", client->mm_login );
	} else {
		Info_RemoveKey( client->userinfo, "cl_mm_login" );
	}

	// call prog code to allow overrides
	G_ClientUserinfoChanged( client->edict, client->userinfo );

	if( !Info_Validate( client->userinfo ) ) {
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Invalid userinfo (after game)" );
		return;
	}

	// we assume that game module deals with setting a correct name
	const char *nameValue = Info_ValueForKey( client->userinfo, "name" );
	if( !nameValue || !nameValue[0] ) {
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: No name set" );
		return;
	}

	Q_strncpyz( client->name, nameValue, sizeof( client->name ) );
}

/*
* SV_Init
*
* Only called at plat.exe startup, not for each game
*/
void SV_Init() {
	assert( !sv_initialized );

	memset( &svc, 0, sizeof( svc ) );

	SV_InitOperatorCommands();

	Cvar_Get( "sv_cheats", "0", CVAR_SERVERINFO | CVAR_LATCH );
	Cvar_Get( "protocol", va( "%i", APP_PROTOCOL_VERSION ), CVAR_SERVERINFO | CVAR_NOSET );

	sv_ip =             Cvar_Get( "sv_ip", "", CVAR_ARCHIVE | CVAR_LATCH );
	sv_port =           Cvar_Get( "sv_port", va( "%i", PORT_SERVER ), CVAR_ARCHIVE | CVAR_LATCH );

	sv_ip6 =            Cvar_Get( "sv_ip6", "::", CVAR_ARCHIVE | CVAR_LATCH );
	sv_port6 =          Cvar_Get( "sv_port6", va( "%i", PORT_SERVER ), CVAR_ARCHIVE | CVAR_LATCH );

#ifdef HTTP_SUPPORT
	sv_http =           Cvar_Get( "sv_http", "1", CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_LATCH );
	sv_http_port =      Cvar_Get( "sv_http_port", va( "%i", PORT_HTTP_SERVER ), CVAR_ARCHIVE | CVAR_LATCH );
	sv_http_ip =        Cvar_Get( "sv_http_ip", "", CVAR_ARCHIVE | CVAR_LATCH );
	sv_http_ipv6 =      Cvar_Get( "sv_http_ipv6", "", CVAR_ARCHIVE | CVAR_LATCH );
	sv_http_upstream_baseurl =  Cvar_Get( "sv_http_upstream_baseurl", "", CVAR_ARCHIVE | CVAR_LATCH );
	sv_http_upstream_realip_header = Cvar_Get( "sv_http_upstream_realip_header", "", CVAR_ARCHIVE );
	sv_http_upstream_ip = Cvar_Get( "sv_http_upstream_ip", "", CVAR_ARCHIVE );
#endif

	rcon_password =         Cvar_Get( "rcon_password", "", 0 );
	sv_hostname =           Cvar_Get( "sv_hostname", APPLICATION " server", CVAR_SERVERINFO | CVAR_ARCHIVE );
	sv_timeout =            Cvar_Get( "sv_timeout", "125", 0 );
	sv_zombietime =         Cvar_Get( "sv_zombietime", "2", 0 );
	sv_showRcon =           Cvar_Get( "sv_showRcon", "1", 0 );
	sv_showChallenge =      Cvar_Get( "sv_showChallenge", "0", 0 );
	sv_showInfoQueries =    Cvar_Get( "sv_showInfoQueries", "0", 0 );

	sv_uploads_http =       Cvar_Get( "sv_uploads_http", "1", CVAR_READONLY );
	sv_uploads_baseurl =    Cvar_Get( "sv_uploads_baseurl", "", CVAR_ARCHIVE );
	sv_uploads_demos =      Cvar_Get( "sv_uploads_demos", "1", CVAR_ARCHIVE );
	sv_uploads_demos_baseurl =  Cvar_Get( "sv_uploads_demos_baseurl", "", CVAR_ARCHIVE );
	if( dedicated->integer ) {
		sv_pure =       Cvar_Get( "sv_pure", "1", CVAR_ARCHIVE | CVAR_LATCH | CVAR_SERVERINFO );

#ifdef PUBLIC_BUILD
		sv_public =     Cvar_Get( "sv_public", "1", CVAR_ARCHIVE | CVAR_LATCH );
#else
		sv_public =     Cvar_Get( "sv_public", "0", CVAR_ARCHIVE | CVAR_LATCH );
#endif
	} else {
		sv_pure =       Cvar_Get( "sv_pure", "0", CVAR_ARCHIVE | CVAR_LATCH | CVAR_SERVERINFO );
		sv_public =     Cvar_Get( "sv_public", "0", CVAR_ARCHIVE );
	}

	sv_iplimit = Cvar_Get( "sv_iplimit", "3", CVAR_ARCHIVE );

	sv_defaultmap =         Cvar_Get( "sv_defaultmap", "wdm1", CVAR_ARCHIVE );
	sv_reconnectlimit =     Cvar_Get( "sv_reconnectlimit", "3", CVAR_ARCHIVE );
	sv_maxclients =         Cvar_Get( "sv_maxclients", "16", CVAR_ARCHIVE | CVAR_SERVERINFO | CVAR_LATCH );

	// fix invalid sv_maxclients values
	if( sv_maxclients->integer < 1 ) {
		Cvar_FullSet( "sv_maxclients", "1", CVAR_ARCHIVE | CVAR_SERVERINFO | CVAR_LATCH, true );
	} else if( sv_maxclients->integer > MAX_CLIENTS ) {
		Cvar_FullSet( "sv_maxclients", va( "%i", MAX_CLIENTS ), CVAR_ARCHIVE | CVAR_SERVERINFO | CVAR_LATCH, true );
	}

	sv_demodir = Cvar_Get( "sv_demodir", "", CVAR_NOSET );
	if( sv_demodir->string[0] && Com_GlobMatch( "*[^0-9a-zA-Z_@]*", sv_demodir->string, false ) ) {
		Com_Printf( "Invalid demo prefix string: %s\n", sv_demodir->string );
		Cvar_ForceSet( "sv_demodir", "" );
	}

	sv_compresspackets = Cvar_Get( "sv_compresspackets", "1", CVAR_DEVELOPER );
	sv_skilllevel =      Cvar_Get( "sv_skilllevel", "2", CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_LATCH );

	if( sv_skilllevel->integer > 2 ) {
		Cvar_ForceSet( "sv_skilllevel", "2" );
	}
	if( sv_skilllevel->integer < 0 ) {
		Cvar_ForceSet( "sv_skilllevel", "0" );
	}

	sv_infoservers =          Cvar_Get( "infoservers", DEFAULT_INFO_SERVERS_IPS, CVAR_LATCH );

	sv_debug_serverCmd =        Cvar_Get( "sv_debug_serverCmd", "0", CVAR_ARCHIVE );

	sv_MOTD = Cvar_Get( "sv_MOTD", "0", CVAR_ARCHIVE );
	sv_MOTDFile = Cvar_Get( "sv_MOTDFile", "", CVAR_ARCHIVE );
	sv_MOTDString = Cvar_Get( "sv_MOTDString", "", CVAR_ARCHIVE );
	SV_MOTD_Update();

	// this is a message holder for shared use
	MSG_Init( &tmpMessage, tmpMessageData, sizeof( tmpMessageData ) );

	// init server updates ratio
	cvar_t *sv_pps;
	if( dedicated->integer ) {
		sv_pps = Cvar_Get( "sv_pps", "20", CVAR_SERVERINFO | CVAR_NOSET );
	} else {
		sv_pps = Cvar_Get( "sv_pps", "20", CVAR_SERVERINFO );
	}
	svc.snapFrameTime = (int)( 1000 / sv_pps->value );
	if( svc.snapFrameTime > 200 ) { // too slow, also, netcode uses a byte
		Cvar_ForceSet( "sv_pps", "5" );
		svc.snapFrameTime = 200;
	} else if( svc.snapFrameTime < 10 ) {   // abusive
		Cvar_ForceSet( "sv_pps", "100" );
		svc.snapFrameTime = 10;
	}

	cvar_t *sv_fps = Cvar_Get( "sv_fps", "62", CVAR_NOSET );
	svc.gameFrameTime = (int)( 1000 / sv_fps->value );
	if( svc.gameFrameTime > svc.snapFrameTime ) { // gamecode can never be slower than snaps
		svc.gameFrameTime = svc.snapFrameTime;
		Cvar_ForceSet( "sv_fps", sv_pps->dvalue );
	}

	svc.autoUpdateMinute = ::rand() % 60;

	Com_Printf( "Game running at %i fps. Server transmit at %i pps\n", sv_fps->integer, sv_pps->integer );

	SV_InitInfoServers();

	ML_Init();

	SV_Web_Init();

	sv_initialized = true;
}

/*
* SV_Shutdown
*
* Called once when the program is shutting down
*/
void SV_Shutdown( const char *finalmsg ) {
	if( sv_initialized ) {
		sv_initialized = false;

		SV_Web_Shutdown();
		ML_Shutdown();

		SV_ShutdownGame( finalmsg, false );

		SV_ShutdownOperatorCommands();
	}
}

void SV_ClientResetCommandBuffers( client_t *client ) {
	// reset the reliable commands buffer
	client->clientCommandExecuted = 0;
	client->reliableAcknowledge = 0;
	client->reliableSequence = 0;
	client->reliableSent = 0;
	for( auto &cmd: client->reliableCommands ) {
		cmd.clear();
	}

	// reset the usercommands buffer(clc_move)
	client->UcmdTime = 0;
	client->UcmdExecuted = 0;
	client->UcmdReceived = 0;
	memset( client->ucmds, 0, sizeof( client->ucmds ) );

	// reset snapshots delta-compression
	client->lastframe = -1;
	client->lastSentFrameNum = 0;
}

void SV_ClientCloseDownload( client_t *client ) {
	if( client->download.file ) {
		FS_FCloseFile( client->download.file );
	}
	if( client->download.name ) {
		Q_free( client->download.name );
	}
	memset( &client->download, 0, sizeof( client->download ) );
}

static void SV_GenerateWebSession( client_t *client ) {
	const char *chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
	// A session has log(64) x 16 = 6 x 16 = 96 bits of information.
	// A UUID has 128 bits of information so using a single UUID is fine.
	static_assert( sizeof( svs.clients[0].session ) == 16 );
	const mm_uuid_t uuid = mm_uuid_t::Random();
	char *p = client->session;

	uint64_t bits = uuid.hiPart;
	for( int i = 0; i < 8; ++i ) {
		*p++ = chars[bits & 63];
		bits >>= 6;
	}
	bits = uuid.loPart;
	for( int i = 0; i < 8; ++i ) {
		*p++ = chars[bits & 63];
		bits >>= 6;
	}

	assert( p - client->session == 16 );
	p[-1] = '\0';
}

/*
* SV_ClientConnect
* accept the new client
* this is the only place a client_t is ever initialized
*/
bool SV_ClientConnect( const socket_t *socket, const netadr_t *address,
					   client_t *client, char *userinfo,
					   int game_port, int challenge, bool fakeClient,
					   mm_uuid_t ticket_id, mm_uuid_t session_id ) {
	const int clientNum = (int)( client - svs.clients );
	edict_t *const ent  = EDICT_NUM( clientNum + 1 );

	// make sure the client state is reset before an mm connection callback gets called
	memset( client, 0, sizeof( *client ) );

	// Try starting validation of a client session and ticket
	// session_id = SVStatsowFacade::Instance()->OnClientConnected( client, address, userinfo, ticket_id, session_id );
	session_id = Uuid_FFFsUuid();
	// rej* user info strings would be set properly in this case
	//if( session_id.IsZero() ) {
	//	return false;
	//}

	// Make sure the session id has been set
	// assert( ::strlen( Info_ValueForKey( userinfo, "cl_mm_session" ) ) == UUID_DATA_LENGTH );

	// get the game a chance to reject this connection or modify the userinfo
	if( G_ClientConnect( ent, userinfo, fakeClient ) ) {
		// the connection is accepted, set up the client slot
		client->edict     = ent;
		client->challenge = challenge; // save challenge for checksumming

		client->mm_session = session_id;
		client->mm_ticket  = ticket_id;

		if( socket ) {
			switch( socket->type ) {
				case SOCKET_UDP:
				case SOCKET_LOOPBACK:
					client->reliable = false;
					break;
				default:
					assert( false );
			}
		} else {
			assert( fakeClient );
			client->reliable = false;
		}

		SV_ClientResetCommandBuffers( client );

		// reset timeouts
		client->lastPacketReceivedTime = svs.realtime;
		client->lastconnect = Sys_Milliseconds();

		// init the connection
		client->state = CS_CONNECTING;

		if( fakeClient ) {
			client->netchan.remoteAddress.type = NA_NOTRANSMIT; // fake-clients can't transmit
		} else {
			Netchan_Setup( &client->netchan, socket, address, game_port );
		}

		// parse some info from the info strings
		client->userinfoLatchTimeout = Sys_Milliseconds() + USERINFO_UPDATE_COOLDOWN_MSEC;
		Q_strncpyz( client->userinfo, userinfo, sizeof( client->userinfo ) );
		SV_UserinfoChanged( client );

		SV_GenerateWebSession( client );
		SV_Web_AddGameClient( client->session, clientNum, &client->netchan.remoteAddress );

		return true;
	}

	return false;
}

/*
* SV_DropClient
*
* Called when the player is totally leaving the server, either willingly
* or unwillingly.  This is NOT called if the entire server is quiting
* or crashing.
*/
void SV_DropClient( client_t *drop, ReconnectBehaviour reconnectBehaviour, const char *format, ... ) {
	char *reason;
	char string[1024];

	if( format ) {
		va_list argptr;
		va_start( argptr, format );
		Q_vsnprintfz( string, sizeof( string ), format, argptr );
		va_end( argptr );
		reason = string;
	} else {
		Q_strncpyz( string, "User disconnected", sizeof( string ) );
		reason = nullptr;
	}

	// remove the rating of the client
	if( drop->edict ) {
		// ge->RemoveRating( drop->edict );
	}

	if( drop->isAFakeClient() ) {
		G_ClientDisconnect( drop->edict, reason );
		SV_ClientResetCommandBuffers( drop ); // make sure everything is clean
	} else {
		SV_InitClientMessage( drop, &tmpMessage, NULL, 0 );
		SV_SendServerCommand( drop, "disconnect %u \"%s\"", (unsigned)reconnectBehaviour, string );
		SV_AddReliableCommandsToMessage( drop, &tmpMessage );

		SV_SendMessageToClient( drop, &tmpMessage );
		Netchan_PushAllFragments( &drop->netchan );

		if( drop->state >= CS_CONNECTED ) {
			// call the prog function for removing a client
			// this will remove the body, among other things
			G_ClientDisconnect( drop->edict, reason );
		} else if( drop->name[0] ) {
			Com_Printf( "Connecting client %s%s disconnected (%s%s)\n", drop->name, S_COLOR_WHITE, reason, S_COLOR_WHITE );
		}
	}

	// SVStatsowFacade::Instance()->OnClientDisconnected( drop );

	SNAP_FreeClientFrames( drop );

	SV_Web_RemoveGameClient( drop->session );

	if( drop->download.name ) {
		SV_ClientCloseDownload( drop );
	}

	drop->state = CS_ZOMBIE;    // become free in a few seconds
	drop->name[0] = 0;
}

/*
* SV_New_f
*
* Sends the first message from the server to a connected client.
* This will be sent on the initial connection and upon each server load.
*/
static void HandleClientCommand_New( client_t *client, const CmdArgs & ) {
	Com_DPrintf( "New() from %s\n", client->name );

	// if in CS_AWAITING we have sent the response packet the new once already,
	// but client might have not got it so we send it again
	if( client->state >= CS_SPAWNED ) {
		Com_Printf( "New not valid -- already spawned\n" );
		return;
	}

	//
	// serverdata needs to go over for all types of servers
	// to make sure the protocol is right, and to set the gamedir
	//
	SV_InitClientMessage( client, &tmpMessage, NULL, 0 );

	// send the serverdata
	MSG_WriteUint8( &tmpMessage, svc_serverdata );
	MSG_WriteInt32( &tmpMessage, APP_PROTOCOL_VERSION );
	MSG_WriteInt32( &tmpMessage, svs.spawncount );
	MSG_WriteInt16( &tmpMessage, (unsigned short)svc.snapFrameTime );

	const int playernum = client - svs.clients;
	MSG_WriteInt16( &tmpMessage, playernum );

	// send full levelname
	MSG_WriteString( &tmpMessage, sv.mapname );

	//
	// game server
	//
	int sv_bitflags = 0;
	if( sv.state == ss_game ) {
		// set up the entity for the client
		edict_t *ent = EDICT_NUM( playernum + 1 );
		ent->s.number = playernum + 1;
		client->edict = ent;

		if( sv_pure->integer ) {
			sv_bitflags |= SV_BITFLAGS_PURE;
		}
		if( client->reliable ) {
			sv_bitflags |= SV_BITFLAGS_RELIABLE;
		}
		if( SV_Web_Running() ) {
			const char *baseurl = SV_Web_UpstreamBaseUrl();
			sv_bitflags |= SV_BITFLAGS_HTTP;
			if( baseurl[0] ) {
				sv_bitflags |= SV_BITFLAGS_HTTP_BASEURL;
			}
		}
		MSG_WriteUint8( &tmpMessage, sv_bitflags );
	}

	if( sv_bitflags & SV_BITFLAGS_HTTP ) {
		if( sv_bitflags & SV_BITFLAGS_HTTP_BASEURL ) {
			MSG_WriteString( &tmpMessage, sv_http_upstream_baseurl->string );
		} else {
			MSG_WriteInt16( &tmpMessage, sv_http_port->integer ); // HTTP port number
		}
	}

	// always write purelist
	const unsigned numpure = Com_CountPureListFiles( svs.purelist );
	if( numpure > (short)0x7fff ) {
		Com_Error( ERR_DROP, "Error: Too many pure files." );
	}

	MSG_WriteInt16( &tmpMessage, numpure );

	for( purelist_t *pureFile = svs.purelist; pureFile; pureFile = pureFile->next ) {
		MSG_WriteString( &tmpMessage, pureFile->filename );
		MSG_WriteInt32( &tmpMessage, pureFile->checksum );
	}

	SV_ClientResetCommandBuffers( client );

	SV_SendMessageToClient( client, &tmpMessage );
	Netchan_PushAllFragments( &client->netchan );

	// don't let it send reliable commands until we get the first configstring request
	client->state = CS_CONNECTING;
}

static void HandleClientCommand_Configstrings( client_t *client, const CmdArgs &cmdArgs ) {
	if( client->state == CS_CONNECTING ) {
		svDebug() << "Start Configstrings() from" << wsw::StringView( client->name );
		client->state = CS_CONNECTED;
	} else {
		svDebug() << "Configstrings() from" << wsw::StringView( client->name );
	}

	if( client->state != CS_CONNECTED ) {
		svWarning() << "configstrings not valid -- already spawned";
	} else {
		// handle the case of a level changing while a client was connecting
		if( atoi( Cmd_Argv( 1 ) ) != svs.spawncount ) {
			svWarning() << "HandleClientCommand_Configstrings from different level";
			SV_SendServerCommand( client, "reconnect" );
		} else {
			int start = atoi( Cmd_Argv( 2 ) );
			if( start < 0 ) {
				start = 0;
			}

			for(;; ) {
				if( start >= MAX_CONFIGSTRINGS ) {
					break;
				}
				if( client->reliableSequence - client->reliableAcknowledge >= MAX_RELIABLE_COMMANDS - 8 ) {
					break;
				}
				if( const auto maybeConfigString = sv.configStrings.get( start ) ) {
					SV_SendConfigString( client, start, *maybeConfigString );
				}
				start++;
			}

			// send next command
			if( start == MAX_CONFIGSTRINGS ) {
				SV_SendServerCommand( client, "cmd baselines %i 0", svs.spawncount );
			} else {
				SV_SendServerCommand( client, "cmd configstrings %i %i", svs.spawncount, start );
			}
		}
	}
}

static void HandleClientCommand_Baselines( client_t *client, const CmdArgs &cmdArgs ) {
	svDebug() << "Baselines() from" << wsw::StringView( client->name );

	if( client->state != CS_CONNECTED ) {
		svWarning() << "baselines not valid -- already spawned";
	} else {
		// handle the case of a level changing while a client was connecting
		if( atoi( Cmd_Argv( 1 ) ) != svs.spawncount ) {
			svWarning() << "HandleClientCommand_Baselines from different level";
			HandleClientCommand_New( client, cmdArgs );
		} else {
			int start = atoi( Cmd_Argv( 2 ) );
			if( start < 0 ) {
				start = 0;
			}

			entity_state_t nullstate;
			memset( &nullstate, 0, sizeof( nullstate ) );

			// write a packet full of data
			SV_InitClientMessage( client, &tmpMessage, NULL, 0 );

			while( tmpMessage.cursize < FRAGMENT_SIZE * 3 && start < MAX_EDICTS ) {
				if( const entity_state_t *base = &sv.baselines[start]; base->number ) {
					MSG_WriteUint8( &tmpMessage, svc_spawnbaseline );
					MSG_WriteDeltaEntity( &tmpMessage, &nullstate, base, true );
				}
				start++;
			}

			// send next command
			if( start == MAX_EDICTS ) {
				SV_SendServerCommand( client, "precache %i", svs.spawncount );
			} else {
				SV_SendServerCommand( client, "cmd baselines %i %i", svs.spawncount, start );
			}

			SV_AddReliableCommandsToMessage( client, &tmpMessage );
			SV_SendMessageToClient( client, &tmpMessage );
		}
	}
}

static void HandleClientCommand_Begin( client_t *client, const CmdArgs &cmdArgs ) {
	svDebug() << "Begin() from" << wsw::StringView( client->name );

	if( client->state != CS_CONNECTED ) {
		if( dedicated->integer ) {
			svWarning() << "HandleClientCommand_Begin: 'Begin' from already spawned client" << wsw::StringView( client->name );
		}
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "Error: Begin while connected" );
	} else {
		// handle the case of a level changing while a client was connecting
		if( atoi( Cmd_Argv( 1 ) ) != svs.spawncount ) {
			svWarning() << "HandleClientCommand_Begin from different level";
			SV_SendServerCommand( client, "changing" );
			SV_SendServerCommand( client, "reconnect" );
		} else {
			client->state = CS_SPAWNED;
			// call the game begin function
			G_ClientBegin( client->edict );
		}
	}
}

/*
* SV_NextDownload_f
*
* Responds to reliable nextdl packet with unreliable download packet
* If nextdl packet's offet information is negative, download will be stopped
*/
static void HandleClientCommand_NextDownload( client_t *client, const CmdArgs &cmdArgs ) {
	if( !client->download.name ) {
		Com_Printf( "nextdl message for client with no download active, from: %s\n", client->name );
	} else if( Q_stricmp( client->download.name, Cmd_Argv( 1 ) ) ) {
		Com_Printf( "nextdl message for wrong filename, from: %s\n", client->name );
	} else {
		const int offset = atoi( Cmd_Argv( 2 ) );

		if( offset > client->download.size ) {
			Com_Printf( "nextdl message with too big offset, from: %s\n", client->name );
		} else if( offset < 0 ) {
			if( offset == -1 ) {
				Com_Printf( "Upload of %s to %s%s completed\n", client->download.name, client->name, S_COLOR_WHITE );
			} else {
				Com_Printf( "Upload of %s to %s%s failed\n", client->download.name, client->name, S_COLOR_WHITE );
			}
			SV_ClientCloseDownload( client );
		} else {
			bool isFileValid = false;
			if( !client->download.file ) {
				Com_Printf( "Starting server upload of %s to %s\n", client->download.name, client->name );

				client->download.size = FS_FOpenBaseFile( client->download.name, &client->download.file, FS_READ );
				if( !client->download.file || client->download.size < 0 ) {
					Com_Printf( "Error opening %s for uploading\n", client->download.name );
					SV_ClientCloseDownload( client );
				} else {
					isFileValid = true;
				}
			}

			if( isFileValid ) {
				SV_InitClientMessage( client, &tmpMessage, NULL, 0 );
				SV_AddReliableCommandsToMessage( client, &tmpMessage );

				uint8_t data[FRAGMENT_SIZE * 2];

				int blocksize = client->download.size - offset;
				// jalfixme: adapt download to user rate setting and sv_maxrate setting.
				if( blocksize > (int)sizeof( data ) ) {
					blocksize = (int)sizeof( data );
				}
				if( offset + blocksize > client->download.size ) {
					blocksize = client->download.size - offset;
				}
				if( blocksize < 0 ) {
					blocksize = 0;
				}

				if( blocksize > 0 ) {
					FS_Seek( client->download.file, offset, FS_SEEK_SET );
					blocksize = FS_Read( data, blocksize, client->download.file );
				}

				MSG_WriteUint8( &tmpMessage, svc_download );
				MSG_WriteString( &tmpMessage, client->download.name );
				MSG_WriteInt32( &tmpMessage, offset );
				MSG_WriteInt32( &tmpMessage, blocksize );
				if( blocksize > 0 ) {
					MSG_CopyData( &tmpMessage, data, blocksize );
				}
				SV_SendMessageToClient( client, &tmpMessage );

				client->download.timeout = svs.realtime + 10000;
			}
		}
	}
}

/*
* SV_GameAllowDownload
* Asks game function whether to allow downloading of a file
*/
static bool SV_GameAllowDownload( client_t *client, const char *requestname, const char *uploadname ) {
	if( client->state < CS_SPAWNED ) {
		return false;
	}

	// allow downloading demos
	if( SV_IsDemoDownloadRequest( requestname ) ) {
		return sv_uploads_demos->integer != 0;
	}

	return false;
}

/*
* SV_DenyDownload
* Helper function for generating initdownload packets for denying download
*/
static void SV_DenyDownload( client_t *client, const char *reason ) {
	// size -1 is used to signal that it's refused
	// URL field is used for deny reason
	SV_InitClientMessage( client, &tmpMessage, NULL, 0 );
	SV_SendServerCommand( client, "initdownload \"%s\" %i %u %i \"%s\"", "", -1, 0, false, reason ? reason : "" );
	SV_AddReliableCommandsToMessage( client, &tmpMessage );
	SV_SendMessageToClient( client, &tmpMessage );
}

static bool SV_FilenameForDownloadRequest( const char *requestname, bool requestpak,
										   const char **uploadname, const char **errormsg ) {
	if( FS_CheckPakExtension( requestname ) ) {
		if( !requestpak ) {
			*errormsg = "Pak file requested as a non pak file";
			return false;
		}
		if( FS_FOpenBaseFile( requestname, NULL, FS_READ ) == -1 ) {
			*errormsg = "File not found";
			return false;
		}

		*uploadname = requestname;
	} else {
		if( FS_FOpenFile( requestname, NULL, FS_READ ) == -1 ) {
			*errormsg = "File not found";
			return false;
		}

		// check if file is inside a PAK
		if( requestpak ) {
			*uploadname = FS_PakNameForFile( requestname );
			if( !*uploadname ) {
				*errormsg = "File not available in pack";
				return false;
			}
		} else {
			*uploadname = FS_BaseNameForFile( requestname );
			if( !*uploadname ) {
				*errormsg = "File only available in pack";
				return false;
			}
		}
	}
	return true;
}

/*
* SV_BeginDownload_f
* Responds to reliable download packet with reliable initdownload packet
*/
static void HandleClientCommand_BeginDownload( client_t *client, const CmdArgs &cmdArgs ) {
	const char *const requestname = Cmd_Argv( 2 );
	const char *denyErrorMessage  = nullptr;
	const char *uploadname        = nullptr;

	if( !requestname[0] || !COM_ValidateRelativeFilename( requestname ) ) {
		denyErrorMessage = "Invalid filename";
	} else {
		const bool requestpak = atoi( Cmd_Argv( 1 ) ) == 1;

		if( !SV_FilenameForDownloadRequest( requestname, requestpak, &uploadname, &denyErrorMessage ) ) {
			assert( denyErrorMessage );
		} else {
			if( FS_CheckPakExtension( uploadname ) ) {
				bool foundPure = false;

				// allow downloading paks from the pure list, if not spawned
				if( client->state < CS_SPAWNED ) {
					for( purelist_t *purefile = svs.purelist; purefile; purefile = purefile->next ) {
						if( !strcmp( uploadname, purefile->filename ) ) {
							foundPure = true;
							break;
						}
					}
				}

				// game module has a change to allow extra downloads
				if( !foundPure && !SV_GameAllowDownload( client, requestname, uploadname ) ) {
					denyErrorMessage = "Downloading of this file is not allowed";
				}
			} else {
				if( !SV_GameAllowDownload( client, requestname, uploadname ) ) {
					denyErrorMessage = "Downloading of this file is not allowed";
				}
			}

			if( !denyErrorMessage ) {
				// we will just overwrite old download, if any
				if( client->download.name ) {
					SV_ClientCloseDownload( client );
				}
				client->download.size = FS_LoadBaseFile( uploadname, nullptr, nullptr, 0 );
				if( client->download.size == -1 ) {
					Com_Printf( "Error getting size of %s for uploading\n", uploadname );
					client->download.size = 0;
					denyErrorMessage = "Error getting file size";
				}
			}
		}
	}

	if( denyErrorMessage ) {
		SV_DenyDownload( client, denyErrorMessage );
	} else {
		const bool local_http   = SV_Web_Running() && sv_uploads_http->integer != 0;
		const unsigned checksum = FS_ChecksumBaseFile( uploadname, false );

		client->download.timeout = svs.realtime + 1000 * 60 * 60; // this is web download timeout

		const size_t name_size = sizeof( char ) * ( strlen( uploadname ) + 1 );
		client->download.name = (char *)Q_malloc( name_size );
		Q_strncpyz( client->download.name, uploadname, name_size );

		Com_Printf( "Offering %s to %s\n", client->download.name, client->name );

		wsw::PodVector<char> url;
		if( FS_CheckPakExtension( uploadname ) && ( local_http || sv_uploads_baseurl->string[0] != 0 ) ) {
			// .pk3 and .pak download from the web
			if( local_http ) {
				url.append( "files/"_asView );
				url.resize( url.size() + strlen( uploadname ) * 3 );
				Q_urlencode_unsafechars( uploadname, url.data() + 6, url.size() + 1 - 6 );
			} else {
				url.append( wsw::StringView( sv_uploads_baseurl->string ) );
				url.push_back( '/' );
			}
		} else if( SV_IsDemoDownloadRequest( requestname ) && ( local_http || sv_uploads_demos_baseurl->string[0] != 0 ) ) {
			// demo file download from the web
			if( local_http ) {
				url.append( "files/"_asView );
				url.resize( url.size() + strlen( uploadname ) * 3 );
				Q_urlencode_unsafechars( uploadname, url.data() + 6, url.size() + 1 - 6 );
			} else {
				url.append( wsw::StringView( sv_uploads_demos_baseurl->string ) );
				url.push_back( '/' );
			}
		}

		url.append( '\0' );

		SV_InitClientMessage( client, &tmpMessage, NULL, 0 );
		SV_SendServerCommand( client, "initdownload \"%s\" %i %u %i \"%s\"", client->download.name,
							  client->download.size, checksum, local_http ? 1 : 0, url.data() );
		SV_AddReliableCommandsToMessage( client, &tmpMessage );
		SV_SendMessageToClient( client, &tmpMessage );
	}
}

/*
* SV_Disconnect_f
* The client is going to disconnect, so remove the connection immediately
*/
static void HandleClientCommand_Disconnect( client_t *client, const CmdArgs & ) {
	SV_DropClient( client, ReconnectBehaviour::DontReconnect, NULL );
}

static void HandleClientCommand_Info( client_t *client, const CmdArgs & ) {
	Info_Print( Cvar_Serverinfo() );
}

static void HandleClientCommand_Userinfo( client_t *client, const CmdArgs &cmdArgs ) {
	const char *info = Cmd_Argv( 1 );
	if( !Info_Validate( info ) ) {
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Invalid userinfo" );
	} else {
		const int64_t time = Sys_Milliseconds();
		if( client->userinfoLatchTimeout > time ) {
			Q_strncpyz( client->userinfoLatched, info, sizeof( client->userinfo ) );
		} else {
			Q_strncpyz( client->userinfo, info, sizeof( client->userinfo ) );

			client->userinfoLatched[0] = '\0';
			client->userinfoLatchTimeout = time + USERINFO_UPDATE_COOLDOWN_MSEC;

			SV_UserinfoChanged( client );
		}
	}
}

static void HandleClientCommand_NoDelta( client_t *client, const CmdArgs & ) {
	client->nodelta       = true;
	client->nodelta_frame = 0;
	client->lastframe     = -1; // jal : I'm not sure about this. Seems like it's missing but...
}

typedef struct {
	const char *name;
	void ( *func )( client_t *client, const CmdArgs &cmdArgs );
} ucmd_t;

static ucmd_t ucmds[] =
	{
		// auto issued
		{ "new",           HandleClientCommand_New },
		{ "configstrings", HandleClientCommand_Configstrings },
		{ "baselines",  HandleClientCommand_Baselines },
		{ "begin",      HandleClientCommand_Begin },
		{ "disconnect", HandleClientCommand_Disconnect },
		{ "usri",      HandleClientCommand_Userinfo },

		{ "nodelta",   HandleClientCommand_NoDelta },

		// issued by hand at client consoles
		{ "info",     HandleClientCommand_Info },

		{ "download", HandleClientCommand_BeginDownload },
		{ "nextdl",   HandleClientCommand_NextDownload },

		// server demo downloads
		{ "demolist", HandleClientCommand_Demolist },
		{ "demoget",  HandleClientCommand_Demoget },

		{ "svmotd",   HandleClientCommand_Motd },

		{ NULL, NULL }
	};

static void SV_ExecuteUserCommand( client_t *client, uint64_t clientCommandNum, const char *s ) {
	static CmdArgsSplitter argsSplitter;
	const CmdArgs &cmdArgs = argsSplitter.exec( wsw::StringView( s ) );

	ucmd_t *u;
	for( u = ucmds; u->name; u++ ) {
		if( !strcmp( cmdArgs[0].data(), u->name ) ) {
			u->func( client, cmdArgs );
			break;
		}
	}

	if( client->state >= CS_SPAWNED && !u->name && sv.state == ss_game ) {
		G_ClientCommand( client->edict, clientCommandNum, cmdArgs );
	}
}

/*
* SV_FindNextUserCommand - Returns the next valid usercmd_t in execution list
*/
usercmd_t *SV_FindNextUserCommand( client_t *client ) {
	// Ucmds can never have a higher timestamp than server time, unless cheating
	int64_t bestFittingTime = svs.gametime;
	usercmd_t *ucmd         = nullptr;

	if( client ) {
		for( unsigned ucmdNum = client->UcmdExecuted + 1; ucmdNum <= client->UcmdReceived; ucmdNum++ ) {
			usercmd_t *const ucmdForNum = &client->ucmds[ucmdNum & CMD_MASK];
			// skip backups if already executed
			if( client->UcmdTime < ucmdForNum->serverTimeStamp ) {
				if( !ucmd || ucmdForNum->serverTimeStamp < bestFittingTime ) {
					bestFittingTime = ucmdForNum->serverTimeStamp;
					ucmd            = ucmdForNum;
				}
			}
		}
	}

	return ucmd;
}

/*
* SV_ExecuteClientThinks - Execute all pending usercmd_t
*/
void SV_ExecuteClientThinks( int clientNum ) {
	// TODO: These guards should be assertions
	if( clientNum >= sv_maxclients->integer || clientNum < 0 ) {
		return;
	}

	client_t *const client = svs.clients + clientNum;
	if( client->state < CS_SPAWNED ) {
		return;
	}

	if( client->isAFakeClient() ) {
		return;
	}

	// don't let client command time delay too far away in the past
	int64_t minUcmdTime = ( svs.gametime > 999 ) ? ( svs.gametime - 999 ) : 0;
	if( client->UcmdTime < minUcmdTime ) {
		client->UcmdTime = minUcmdTime;
	}

	usercmd_t *ucmd = nullptr;
	while( ( ucmd = SV_FindNextUserCommand( client ) ) != nullptr ) {
		unsigned msec = ucmd->serverTimeStamp - client->UcmdTime;
		Q_clamp( msec, 1, 200 );
		ucmd->msec = msec;
		int timeDelta = 0;
		if( client->lastframe > 0 ) {
			timeDelta = -(int)( svs.gametime - ucmd->serverTimeStamp );
		}

		G_ClientThink( client->edict, ucmd, timeDelta );

		client->UcmdTime = ucmd->serverTimeStamp;
	}

	// we did the entire update
	client->UcmdExecuted = client->UcmdReceived;
}

/*
* SV_ParseMoveCommand
*/
static void SV_ParseMoveCommand( client_t *client, msg_t *msg ) {
	const int lastframe = MSG_ReadInt32( msg );
	// read the id of the first ucmd we will receive
	const auto ucmdHead = (unsigned)MSG_ReadInt32( msg );
	// read the number of ucmds we will receive
	const auto ucmdCount = (unsigned)MSG_ReadUint8( msg );

	if( ucmdCount > CMD_MASK ) {
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Ucmd overflow" );
	} else {
		const unsigned ucmdFirst = ucmdHead > ucmdCount ? ucmdHead - ucmdCount : 0;
		client->UcmdReceived = ucmdHead < 1 ? 0 : ucmdHead - 1;

		// read the user commands
		for( unsigned i = ucmdFirst; i < ucmdHead; i++ ) {
			if( i == ucmdFirst ) {
				// first one isn't delta compressed
				usercmd_t nullcmd;
				memset( &nullcmd, 0, sizeof( nullcmd ) );
				// jalfixme: check for too old overflood
				MSG_ReadDeltaUsercmd( msg, &nullcmd, &client->ucmds[i & CMD_MASK] );
			} else {
				MSG_ReadDeltaUsercmd( msg, &client->ucmds[( i - 1 ) & CMD_MASK], &client->ucmds[i & CMD_MASK] );
			}
		}

		if( client->state != CS_SPAWNED ) {
			client->lastframe = -1;
		} else {
			if( lastframe != client->lastframe ) {
				client->lastframe = lastframe;
				if( client->lastframe > 0 ) {
					//client->frame_latency[client->lastframe&(LATENCY_COUNTS-1)] = svs.gametime - (client->frames[client->lastframe & UPDATE_MASK].sentTimeStamp;
					// this is more accurate. A little bit hackish, but more accurate
					client->frame_latency[client->lastframe & ( LATENCY_COUNTS - 1 )] =
						svs.gametime - ( client->ucmds[client->UcmdReceived & CMD_MASK].serverTimeStamp + svc.snapFrameTime );
				}
			}
		}
	}
}

/*
* SV_ParseClientMessage
* The current message is parsed for the given client
*/
void SV_ParseClientMessage( client_t *client, msg_t *msg ) {
	// TODO: Should be an assertion
	if( !msg ) {
		return;
	}

	SV_UpdateActivity();

	char *s;
	bool move_issued = false;
	int64_t cmdNum;

	// only allow one move command
	move_issued = false;
	while( msg->readcount < msg->cursize ) {
		switch( const int c = MSG_ReadUint8( msg ); c ) {
			default: {
				Com_Printf( "SV_ParseClientMessage: unknown command char: %d\n", c );
				SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Unknown command char" );
				return;
			} break;
			case clc_nop: {
			} break;
			case clc_move: {
				if( move_issued ) {
					return; // someone is trying to cheat...
				}
				move_issued = true;
				SV_ParseMoveCommand( client, msg );
			} break;
			case clc_svcack: {
				if( client->reliable ) {
					Com_Printf( "SV_ParseClientMessage: svack from reliable client\n" );
					SV_DropClient( client, ReconnectBehaviour::DontReconnect, "%s", "Error: svack from reliable client" );
					return;
				}
				cmdNum = MSG_ReadIntBase128( msg );
				if( cmdNum < client->reliableAcknowledge || cmdNum > client->reliableSent ) {
					//SV_DropClient( client, DROP_TYPE_GENERAL, "%s", "Error: bad server command acknowledged" );
					return;
				}
				client->reliableAcknowledge = cmdNum;
				client->packetPings.add( svs.realtime - MSG_ReadIntBase128( msg ) - svc.snapFrameTime / 2 );
			} break;
			case clc_clientcommand: {
				cmdNum = 0;
				if( !client->reliable ) {
					cmdNum = MSG_ReadIntBase128( msg );
					if( cmdNum <= client->clientCommandExecuted ) {
						s = MSG_ReadString( msg ); // read but ignore
						continue;
					}
					client->clientCommandExecuted = cmdNum;
				}
				s = MSG_ReadString( msg );
				SV_ExecuteUserCommand( client, cmdNum, s );
				if( client->state == CS_ZOMBIE ) {
					return; // disconnect command
				}
			} break;
			case clc_extension: {
				[[maybe_unused]] int ext = MSG_ReadUint8( msg );  // extension id
				MSG_ReadUint8( msg );        // version number
				[[maybe_unused]] int len = MSG_ReadInt16( msg ); // command length

				// unsupported
				MSG_SkipData( msg, len );
				break;
			} break;
		}
	}

	if( msg->readcount > msg->cursize ) {
		Com_Printf( "SV_ParseClientMessage: badread\n" );
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Bad message" );
		return;
	}
}

void SV_FlushRedirect( int sv_redirected, const char *outputbuf, const void *extra ) {
	const flush_params_t *params = ( const flush_params_t * )extra;

	if( sv_redirected == RD_PACKET ) {
		Netchan_OutOfBandPrint( params->socket, params->address, "print\n%s", outputbuf );
	}
}

void SV_AddGameCommand( client_t *client, const char *cmd ) {
	// TODO: Should be an assertion
	if( !client ) {
		return;
	}

	client->gameCommandCurrent++;
	const int index = client->gameCommandCurrent & ( MAX_RELIABLE_COMMANDS - 1 );
	Q_strncpyz( client->gameCommands[index].command, cmd, sizeof( client->gameCommands[index].command ) );
	if( client->lastSentFrameNum ) {
		client->gameCommands[index].framenum = client->lastSentFrameNum + 1;
	} else {
		client->gameCommands[index].framenum = sv.framenum;
	}
}

/*
* SV_AddServerCommand
*
* The given command will be transmitted to the client, and is guaranteed to
* not have future snapshot_t executed before it is executed
*/
void SV_AddServerCommand( client_t *client, const wsw::StringView &cmd ) {
	// TODO: Should be an assertion
	assert( client );
	if( !client ) {
		return;
	}

	// TODO: Don't call for fake clients?
	if( client->isAFakeClient() ) {
		return;
	}

	// TODO: Don't call with such params
	const auto len = cmd.length();
	if( !len ) {
		return;
	}

	if( len + 1 > client->reliableCommands[0].capacity() ) {
		// This is an server error, not the client one.
		Com_Error( ERR_DROP, "A server command %s is too long\n", cmd.data() );
	}

	// ch : To avoid overflow of messages from excessive amount of configstrings
	// we batch them here. On incoming "cs" command, we'll trackback the queue
	// to find a pending "cs" command that has space in it. If we'll find one,
	// we'll batch this there, if not, we'll create a new one.
	const wsw::StringView csPrefix( "cs ", 3 );
	if( cmd.startsWith( csPrefix ) ) {
		for( auto i = client->reliableSequence; i > client->reliableSent; i-- ) {
			auto &otherCmd = client->reliableCommands[i & ( MAX_RELIABLE_COMMANDS - 1 )];
			if( otherCmd.startsWith( csPrefix ) ) {
				if( ( otherCmd.length() + len ) < otherCmd.capacity() ) {
					otherCmd.append( cmd.drop( 2 ) );
					return;
				}
			}
		}
	}

	client->reliableSequence++;
	// if we would be losing an old command that hasn't been acknowledged, we must drop the connection
	// we check == instead of >= so a broadcast print added by SV_DropClient() doesn't cause a recursive drop client
	if( client->reliableSequence - client->reliableAcknowledge == MAX_RELIABLE_COMMANDS + 1 ) {
		SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "%s", "Error: Too many pending reliable server commands" );
	} else {
		const auto index = client->reliableSequence & ( MAX_RELIABLE_COMMANDS - 1 );
		client->reliableCommands[index].assign( cmd );
	}
}

/*
* SV_SendServerCommand
*
* Sends a reliable command string to be interpreted by
* the client: "cs", "changing", "disconnect", etc
* A NULL client will broadcast to all clients
*/
void SV_SendServerCommand( client_t *cl, const char *format, ... ) {
	char buffer[MAX_MSGLEN];

	va_list argptr;
	va_start( argptr, format );
	int charsWritten = Q_vsnprintfz( buffer, sizeof( buffer ), format, argptr );
	va_end( argptr );

	if( (size_t)charsWritten >= sizeof( buffer ) ) {
		Com_Error( ERR_DROP, "Server command overflow" );
	}

	const wsw::StringView cmd( buffer, (size_t)charsWritten );
	if( cl ) {
		if( cl->state >= CS_CONNECTING ) {
			SV_AddServerCommand( cl, cmd );
		}
	} else {
		IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
		while( client_t *client = iteratorOverClients.getNext() ) {
			SV_AddServerCommand( client, cmd );
		}
		// add to demo
		if( svs.demo.file ) {
			SV_AddServerCommand( &svs.demo.client, cmd );
		}
	}
}

static_assert( kMaxNonFragmentedConfigStringLen < MAX_STRING_CHARS );
static_assert( kMaxNonFragmentedConfigStringLen > kMaxConfigStringFragmentLen );

static void SV_AddFragmentedConfigString( client_t *cl, int index, const wsw::StringView &string ) {
	wsw::StaticString<MAX_STRING_CHARS> buffer;

	const size_t len = string.length();
	// Don't use for transmission of shorter config strings
	assert( len >= kMaxNonFragmentedConfigStringLen );

	const size_t numFragments = len / kMaxConfigStringFragmentLen + ( len % kMaxConfigStringFragmentLen ? 1 : 0 );
	assert( numFragments >= 2 );

	wsw::StringView view( string );
	for( size_t i = 0; i < numFragments; ++i ) {
		wsw::StringView fragment = view.take( kMaxConfigStringFragmentLen );
		assert( ( i + 1 != numFragments && fragment.length() == kMaxConfigStringFragmentLen ) || !fragment.empty() );

		buffer.clear();
		buffer << wsw::StringView( "csf ", 4 );
		buffer << index << ' ' << i << ' ' << numFragments << ' ' << fragment.length() << ' ';
		buffer << '"' << fragment << '"';
		SV_AddServerCommand( cl, buffer.asView() );

		view = view.drop( fragment.length() );
		assert( !view.empty() || i + 1 == numFragments );
	}
}

void SV_SendConfigString( client_t *cl, int index, const wsw::StringView &string ) {
	if( string.length() + 16 > MAX_MSGLEN ) {
		Com_Error( ERR_DROP, "Configstring overflow: #%d len=%d\n", index, (int)string.length() );
	}

	if( string.length() < kMaxNonFragmentedConfigStringLen ) {
		assert( string.isZeroTerminated() );
		SV_SendServerCommand( cl, "cs %i \"%s\"", index, string.data() );
	} else {
		if( cl ) {
			if( cl->state >= CS_CONNECTING ) {
				SV_AddFragmentedConfigString( cl, index, string );
			}
		} else {
			IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
			while( client_t *client = iteratorOverClients.getNext() ) {
				SV_AddFragmentedConfigString( client, index, string );
			}
			if( svs.demo.file ) {
				SV_AddFragmentedConfigString( &svs.demo.client, index, string );
			}
		}
	}
}

/*
* SV_AddReliableCommandsToMessage
*
* (re)send all server commands the client hasn't acknowledged yet
*/
void SV_AddReliableCommandsToMessage( client_t *client, msg_t *msg ) {
	// TODO: Don't call for fake clients
	if( client->isAFakeClient() ) {
		return;
	}

	if( sv_debug_serverCmd->integer ) {
		Com_Printf( "sv_cl->reliableAcknowledge: %" PRIi64 " sv_cl->reliableSequence:%" PRIi64 "\n",
					client->reliableAcknowledge, client->reliableSequence );
	}

	// write any unacknowledged serverCommands
	for( unsigned i = client->reliableAcknowledge + 1; i <= client->reliableSequence; i++ ) {
		const auto &cmd = client->reliableCommands[i & ( MAX_RELIABLE_COMMANDS - 1 )];
		if( !cmd.empty() ) {
			MSG_WriteUint8( msg, svc_servercmd );
			if( !client->reliable ) {
				MSG_WriteInt32( msg, i );
			}
			MSG_WriteString( msg, cmd.data() );
			if( sv_debug_serverCmd->integer ) {
				Com_Printf( "SV_AddServerCommandsToMessage(%i):%s\n", i, cmd.data() );
			}
		}
	}

	client->reliableSent = client->reliableSequence;
	if( client->reliable ) {
		client->reliableAcknowledge = client->reliableSent;
	}
}

/*
* SV_BroadcastCommand
*
* Sends a command to all connected clients. Ignores client->state < CS_SPAWNED check
*/
void SV_BroadcastCommand( const char *format, ... ) {
	// TODO: Should be an assertion
	if( !sv.state ) {
		return;
	}

	va_list argptr;
	char string[1024];

	va_start( argptr, format );
	Q_vsnprintfz( string, sizeof( string ), format, argptr );
	va_end( argptr );

	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *client = iteratorOverClients.getNext() ) {
		SV_SendServerCommand( client, string );
	}
}

bool SV_SendClientsFragments() {
	bool sent = false;

	// send a message to each connected client
	IteratorOverClients iteratorOverClients( { .includeFakeClients = false } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		if( client->netchan.unsentFragments ) {
			if( !Netchan_TransmitNextFragment( &client->netchan ) ) {
				const char *errorString   = NET_ErrorString();
				const char *addressString = NET_AddressToString( &client->netchan.remoteAddress );
				Com_Printf( "Error sending fragment to %s: %s\n", addressString, errorString );
				if( client->reliable ) {
					SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "Error sending fragment: %s\n", errorString );
				}
			} else {
				sent = true;
			}
		}
	}

	return sent;
}

bool SV_Netchan_Transmit( netchan_t *netchan, msg_t *msg ) {
	// if we got here with unsent fragments, fire them all now
	if( !Netchan_PushAllFragments( netchan ) ) {
		return false;
	}

	if( sv_compresspackets->integer ) {
		// it's compression error, just send uncompressed
		if( const int zerror = Netchan_CompressMessage( msg, g_netchanCompressionBuffer ); zerror < 0 ) {
			Com_DPrintf( "SV_Netchan_Transmit (ignoring compression): Compression error %i\n", zerror );
		}
	}

	return Netchan_Transmit( netchan, msg );
}

void SV_InitClientMessage( client_t *client, msg_t *msg, uint8_t *data, size_t size ) {
	// TODO: Should not call for fake clients
	if( client->isAFakeClient() ) {
		return;
	}

	if( data && size ) {
		MSG_Init( msg, data, size );
	}
	MSG_Clear( msg );

	// write the last client-command we received so it's acknowledged
	if( !client->reliable ) {
		MSG_WriteUint8( msg, svc_clcack );
		MSG_WriteUintBase128( msg, client->clientCommandExecuted );
		MSG_WriteUintBase128( msg, client->UcmdReceived ); // acknowledge the last ucmd
		MSG_WriteIntBase128( msg, svs.realtime );
	}
}

bool SV_SendMessageToClient( client_t *client, msg_t *msg ) {
	assert( client );
	// TODO: Do not call for fake clients
	if( client->isAFakeClient() ) {
		return true;
	}

	// transmit the message data
	client->lastPacketSentTime = svs.realtime;
	return SV_Netchan_Transmit( &client->netchan, msg );
}

/*
* SV_ResetClientFrameCounters
* This is used for a temporary sanity check I'm doing.
*/
void SV_ResetClientFrameCounters() {
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		client->lastSentFrameNum = 0;
	}
}

void SV_WriteFrameSnapToClient( client_t *client, msg_t *msg ) {
	SNAP_WriteFrameSnapToClient( &sv.gi, client, msg, sv.framenum, svs.gametime, sv.baselines,
								 &svs.client_entities, 0, NULL, NULL );
}

void SV_BuildClientFrameSnap( client_t *client ) {
	vec_t *skyorg = NULL, origin[3];

	if( auto maybeSkyBoxString = sv.configStrings.getSkyBox() ) {
		int noents = 0;
		float f1 = 0, f2 = 0;

		if( sscanf( maybeSkyBoxString->data(), "%f %f %f %f %f %i", &origin[0], &origin[1], &origin[2], &f1, &f2, &noents ) >= 3 ) {
			if( !noents ) {
				skyorg = origin;
			}
		}
	}

	const ReplicatedScoreboardData *scoreboardData;
	if( client->edict ) {
		scoreboardData = G_GetScoreboardDataForClient( client->edict->s.number - 1 );
	} else {
		scoreboardData = G_GetScoreboardDataForDemo();
	}

	SNAP_BuildClientFrameSnap( svs.cms, &sv.gi, sv.framenum, svs.gametime, skyorg,
							   client, G_GetGameState(), scoreboardData, &svs.client_entities );
}

static bool SV_SendClientDatagram( client_t *client ) {
	assert( !client->isAFakeClient() );

	SV_InitClientMessage( client, &tmpMessage, NULL, 0 );

	SV_AddReliableCommandsToMessage( client, &tmpMessage );

	// send over all the relevant entity_state_t
	// and the player_state_t
	SV_BuildClientFrameSnap( client );

	SV_WriteFrameSnapToClient( client, &tmpMessage );

	return SV_SendMessageToClient( client, &tmpMessage );
}

void SV_SendClientMessages() {
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *const client = iteratorOverClients.getNext() ) {
		if( client->isAFakeClient() ) {
			client->lastSentFrameNum = sv.framenum;
		} else {
			SV_UpdateActivity();

			if( client->state == CS_SPAWNED ) {
				if( !SV_SendClientDatagram( client ) ) {
					Com_Printf( "Error sending message to %s: %s\n", client->name, NET_ErrorString() );
					if( client->reliable ) {
						SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "Error sending message: %s\n", NET_ErrorString() );
					}
				}
			} else {
				// send pending reliable commands, or send heartbeats for not timing out
				if( client->reliableSequence > client->reliableAcknowledge || svs.realtime - client->lastPacketSentTime > 1000 ) {
					SV_InitClientMessage( client, &tmpMessage, nullptr, 0 );
					SV_AddReliableCommandsToMessage( client, &tmpMessage );
					if( !SV_SendMessageToClient( client, &tmpMessage ) ) {
						Com_Printf( "Error sending message to %s: %s\n", client->name, NET_ErrorString() );
						if( client->reliable ) {
							SV_DropClient( client, ReconnectBehaviour::OfUserChoice, "Error sending message: %s\n", NET_ErrorString() );
						}
					}
				}
			}
		}
	}
}

static inline void SNAP_WriteDeltaEntity( msg_t *msg, const entity_state_t *from, const entity_state_t *to,
										  const client_snapshot_t *frame, bool force ) {
	MSG_WriteDeltaEntity( msg, from, to, force );
}

/*
* SNAP_EmitPacketEntities
*
* Writes a delta update of an entity_state_t list to the message.
*/
static void SNAP_EmitPacketEntities( const client_snapshot_t *from, const client_snapshot_t *to,
									 msg_t *msg, const entity_state_t *baselines,
									 const entity_state_t *client_entities, int num_client_entities ) {
	MSG_WriteUint8( msg, svc_packetentities );

	const int from_num_entities = !from ? 0 : from->num_entities;

	int newindex = 0;
	int oldindex = 0;
	while( newindex < to->num_entities || oldindex < from_num_entities ) {
		int newnum = 9999;
		const entity_state_t *newent = nullptr;
		if( newindex < to->num_entities ) {
			newent = &client_entities[( to->first_entity + newindex ) % num_client_entities];
			newnum = newent->number;
		}

		int oldnum = 9999;
		const entity_state_t *oldent = nullptr;
		if( oldindex < from_num_entities ) {
			oldent = &client_entities[( from->first_entity + oldindex ) % num_client_entities];
			oldnum = oldent->number;
		}

		if( newnum == oldnum ) {
			// delta update from old position
			// because the force parm is false, this will not result
			// in any bytes being emited if the entity has not changed at all
			// note that players are always 'newentities', this updates their oldorigin always
			// and prevents warping ( wsw : jal : I removed it from the players )
			SNAP_WriteDeltaEntity( msg, oldent, newent, to, false );
			oldindex++;
			newindex++;
			continue;
		}

		if( newnum < oldnum ) {
			// this is a new entity, send it from the baseline
			SNAP_WriteDeltaEntity( msg, &baselines[newnum], newent, to, true );
			newindex++;
			continue;
		}

		if( newnum > oldnum ) {
			// the old entity isn't present in the new message
			SNAP_WriteDeltaEntity( msg, oldent, nullptr, to, false );
			oldindex++;
			continue;
		}
	}

	MSG_WriteInt16( msg, 0 ); // end of packetentities
}

static void SNAP_WriteDeltaGameStateToClient( const client_snapshot_t *from, const client_snapshot_t *to, msg_t *msg ) {
	MSG_WriteUint8( msg, svc_match );
	MSG_WriteDeltaGameState( msg, from ? &from->gameState : nullptr, &to->gameState );
}

static void SNAP_WriteDeltaScoreboardDataToClient( const client_snapshot_t *from, const client_snapshot_t *to, msg_t *msg ) {
	MSG_WriteUint8( msg, svc_scoreboard );
	MSG_WriteDeltaScoreboardData( msg, from ? &from->scoreboardData : nullptr, &to->scoreboardData );
}

static void SNAP_WritePlayerstateToClient( msg_t *msg, const player_state_t *ops, const player_state_t *ps, const client_snapshot_t *frame ) {
	MSG_WriteUint8( msg, svc_playerinfo );
	MSG_WriteDeltaPlayerState( msg, ops, ps );
}

[[nodiscard]]
static bool isValidPendingCommand( const client_t *snapTargetClient, const game_command_t *cmd, int64_t frameNum ) {
	if( cmd->command[0] && cmd->framenum + 256 >= frameNum && cmd->framenum <= frameNum ) {
		if( snapTargetClient->lastframe >= 0 && cmd->framenum > snapTargetClient->lastframe ) {
			return true;
		}
	}
	return false;
}

static void SNAP_WriteMultiPOVCommands( const ginfo_t *gi, const client_t *snapTargetClient, msg_t *msg, int64_t frameNum ) {
	int64_t positions[MAX_CLIENTS];
	// how many clients in snapshot
	int maxnumtargets = 0;

	// find the first command to send from every client
	IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_SPAWNED, .includeFakeClients = true } );
	while( auto maybeClientAndNum = iteratorOverClients.getNextWithIndex() ) {
		auto [client, i] = *maybeClientAndNum;
		if( !client->isAHiddenClient() || ( client == snapTargetClient ) ) {
			positions[i] = client->gameCommandCurrent - MAX_RELIABLE_COMMANDS + 1;
			for(; positions[i] <= client->gameCommandCurrent; positions[i]++ ) {
				const auto *cmd = &client->gameCommands[positions[i] & ( MAX_RELIABLE_COMMANDS - 1 )];
				// we need to check for too new commands too, because gamecommands for the next snap are generated
				// all the time, and we might want to create a server demo frame or something in between snaps
				if( isValidPendingCommand( snapTargetClient, cmd, frameNum ) ) {
					break;
				}
			}
			maxnumtargets++;
		}
	}

	for(;; ) {
		int numtargets = 0, maxtarget = 0;
		int64_t pickedCmdFrameNum = 0;
		// TODO: This can be a single dword
		uint8_t targets[MAX_CLIENTS / 8];

		const char *pickedCmdText = nullptr;
		memset( targets, 0, sizeof( targets ) );

		// we find the message with the earliest framenum, and collect all recipients for that
		iteratorOverClients.rewind();
		while( auto maybeClientAndNum = iteratorOverClients.getNextWithIndex() ) {
			auto [client, i] = *maybeClientAndNum;
			if( !client->isAHiddenClient() || ( client == snapTargetClient ) ) {
				if( positions[i] <= client->gameCommandCurrent ) {
					const auto &cmd = client->gameCommands[positions[i] & ( MAX_RELIABLE_COMMANDS - 1 )];

					// If it's the same command in the same frame
					if( pickedCmdText && !strcmp( cmd.command, pickedCmdText ) && pickedCmdFrameNum == cmd.framenum ) {
						targets[i >> 3] |= 1 << ( i & 7 );
						maxtarget = i + 1;
						numtargets++;
					} else if( !pickedCmdText || cmd.framenum < pickedCmdFrameNum ) {
						// Either we're picking a new command or switching to a new command
						// TODO: If we switch to a new command, is it guaranteed that the previous command is already written?
						pickedCmdText = cmd.command;
						pickedCmdFrameNum = cmd.framenum;
						// Reset targets for a single recipient so far
						memset( targets, 0, sizeof( targets ) );
						targets[i >> 3] |= 1 << ( i & 7 );
						maxtarget = i + 1;
						numtargets = 1;
					}

					// We have collected all clients for this command
					if( numtargets == maxnumtargets ) {
						break;
					}
				}
			}
		}

		if( !pickedCmdText ) {
			// We haven't managed to find a command to send to at least a single client, stop at this.
			break;
		}

		// send it
		// never write a command if it's of a higher framenum
		if( frameNum >= pickedCmdFrameNum ) {
			assert( frameNum - pickedCmdFrameNum < std::numeric_limits<int16_t>::max() );
			// do not allow the message buffer to overflow (can happen on flood updates)
			// TODO: Should it be a break?
			if( msg->cursize + strlen( pickedCmdText ) + 512 <= msg->maxsize ) {
				MSG_WriteInt16( msg, (int16_t)( frameNum - pickedCmdFrameNum ) );
				MSG_WriteString( msg, pickedCmdText );

				// TODO: Just write a dword
				// 0 means everyone
				if( numtargets == maxnumtargets ) {
					MSG_WriteUint8( msg, 0 );
				} else {
					int bytes = ( maxtarget + 7 ) / 8;
					MSG_WriteUint8( msg, bytes );
					MSG_WriteData( msg, targets, bytes );
				}
			}
		}

		// Advance positions for targets included for this command
		for( int i = 0; i < maxtarget; i++ ) {
			if( targets[i >> 3] & ( 1 << ( i & 7 ) ) ) {
				positions[i]++;
			}
		}

		pickedCmdText = nullptr;
	};
}

void SNAP_WriteFrameSnapToClient( const ginfo_t *gi, client_t *client, msg_t *msg, int64_t frameNum, int64_t gameTime,
								  const entity_state_t *baselines, const client_entities_t *client_entities,
								  int numcmds, const gcommand_t *commands, const char *commandsData ) {
	// this is the frame we are creating
	const client_snapshot_t *frame = &client->snapShots[frameNum & UPDATE_MASK];

	// for non-reliable clients we need to send nodelta frame until the client responds
	if( client->nodelta && !client->reliable ) {
		if( !client->nodelta_frame ) {
			client->nodelta_frame = frameNum;
		} else if( client->lastframe >= client->nodelta_frame ) {
			client->nodelta = false;
		}
	}

	const client_snapshot_t *oldframe;
	if( client->lastframe <= 0 || client->lastframe > frameNum || client->nodelta ) {
		// client is asking for a not compressed retransmit
		oldframe = nullptr;
	} else if( frameNum >= client->lastframe + UPDATE_MASK ) {
		// client hasn't gotten a good message through in a long time
		oldframe = nullptr;
	} else {
		// we have a valid message to delta from
		oldframe = &client->snapShots[client->lastframe & UPDATE_MASK];
		if( oldframe->multipov != frame->multipov ) {
			// don't delta compress a frame of different POV type
			oldframe = nullptr;
		}
	}

	if( client->nodelta && client->reliable ) {
		client->nodelta = false;
	}

	MSG_WriteUint8( msg, svc_frame );

	const int pos = msg->cursize;
	MSG_WriteInt16( msg, 0 );       // we will write length here

	MSG_WriteIntBase128( msg, gameTime ); // serverTimeStamp
	MSG_WriteUintBase128( msg, frameNum );
	MSG_WriteUintBase128( msg, client->lastframe );
	MSG_WriteUintBase128( msg, frame->UcmdExecuted );

	int flags = 0;
	if( oldframe ) {
		flags |= FRAMESNAP_FLAG_DELTA;
	}
	if( frame->allentities ) {
		flags |= FRAMESNAP_FLAG_ALLENTITIES;
	}
	if( frame->multipov ) {
		flags |= FRAMESNAP_FLAG_MULTIPOV;
	}
	MSG_WriteUint8( msg, flags );

	MSG_WriteUint8( msg, 0 );   // rate dropped packets - untracked for now

	// add game comands
	MSG_WriteUint8( msg, svc_gamecommands );
	if( frame->multipov ) {
		SNAP_WriteMultiPOVCommands( gi, client, msg, frameNum );
	} else {
		for( int64_t cmdNum = client->gameCommandCurrent - MAX_RELIABLE_COMMANDS + 1; cmdNum <= client->gameCommandCurrent; cmdNum++ ) {
			const auto *cmd = &client->gameCommands[cmdNum & ( MAX_RELIABLE_COMMANDS - 1 )];
			if( isValidPendingCommand( client, cmd, frameNum ) ) {
				// TODO: Should this really be a break?
				// do not allow the message buffer to overflow (can happen on flood updates)
				if( msg->cursize + strlen( cmd->command ) + 512 <= msg->maxsize ) {
					// send it
					assert( frameNum - cmd->framenum < std::numeric_limits<int16_t>::max() );
					MSG_WriteInt16( msg, (int16_t)( frameNum - cmd->framenum ) );
					MSG_WriteString( msg, cmd->command );
				}
			}
		}
	}
	MSG_WriteInt16( msg, -1 );

	// send over the areabits
	MSG_WriteUint8( msg, frame->areabytes );
	MSG_WriteData( msg, frame->areabits, frame->areabytes );

	SNAP_WriteDeltaGameStateToClient( oldframe, frame, msg );
	SNAP_WriteDeltaScoreboardDataToClient( oldframe, frame, msg );

	// delta encode the playerstate
	for( int i = 0; i < frame->numplayers; i++ ) {
		if( oldframe && oldframe->numplayers > i ) {
			SNAP_WritePlayerstateToClient( msg, &oldframe->ps[i], &frame->ps[i], frame );
		} else {
			SNAP_WritePlayerstateToClient( msg, nullptr, &frame->ps[i], frame );
		}
	}
	MSG_WriteUint8( msg, 0 );

	// delta encode the entities
	const entity_state_t *entityStates = client_entities ? client_entities->entities : nullptr;
	const int numEntities = client_entities ? client_entities->num_entities : 0;
	SNAP_EmitPacketEntities( oldframe, frame, msg, baselines, entityStates, numEntities );

	// write length into reserved space
	const int length = msg->cursize - pos - 2;
	msg->cursize = pos;
	MSG_WriteInt16( msg, length );
	msg->cursize += length;

	client->lastSentFrameNum = frameNum;
}

static bool SNAP_BitsCullEntity( const cmodel_state_t *cms, const edict_t *ent, const uint8_t *bits, int max_clusters ) {
	// too many leafs for individual check, go by headnode
	if( ent->r.num_clusters == -1 ) {
		if( !CM_HeadnodeVisible( cms, ent->r.headnode, bits ) ) {
			return true;
		}
		return false;
	}

	// check individual leafs
	for( int i = 0; i < max_clusters; i++ ) {
		const int l = ent->r.clusternums[i];
		if( bits[l >> 3] & ( 1 << ( l & 7 ) ) ) {
			return false;
		}
	}

	return true;    // not visible/audible
}

class SnapEntNumsList {
	mutable int m_nums[MAX_EDICTS];
	bool m_added[MAX_EDICTS];
	mutable unsigned m_numEnts { 0 };
	int m_maxNumSoFar { 0 };
	mutable bool m_isSorted { false };
public:
	SnapEntNumsList() {
		std::memset( m_added, 0, sizeof( m_added ) );
	}

	void addEntNum( int entNum ) {
		m_added[entNum] = true;
		m_maxNumSoFar   = wsw::max( entNum, m_maxNumSoFar );
	}

	[[nodiscard]]
	auto sortedNums() const -> std::span<const int> {
		if( !m_isSorted ) {
			m_isSorted = true;
			assert( m_numEnts == 0 );
			// avoid adding world to the list by all costs
			for( int num = 1; num <= m_maxNumSoFar; num++ ) {
				if( m_added[num] ) {
					m_nums[m_numEnts++] = num;
				}
			}
			assert( std::is_sorted( m_nums, m_nums + m_numEnts ) );
		}
		return { m_nums, m_nums + m_numEnts };
	}
};

static bool SNAP_SnapCullSoundEntity( const cmodel_state_t *cms, const edict_t *ent, const vec3_t listener_origin, float attenuation ) {
	if( attenuation == 0.0f ) {
		return false;
	}

	// extend the influence sphere cause the player could be moving
	const float dist = DistanceFast( ent->s.origin, listener_origin ) - 128;
	const float gain = calcSoundGainForDistanceAndAttenuation( wsw::max( 0.0f, dist ), attenuation );
	// curved attenuations can keep barely audible sounds for long distances
	return gain <= 0.05f;
}

static inline bool SNAP_IsSoundCullOnlyEntity( const edict_t *ent ) {
	// If it is set explicitly
	if( ent->r.svflags & SVF_SOUNDCULL ) {
		return true;
	}

	// If there is no sound
	if( !ent->s.sound ) {
		return false;
	}

	// Check whether there is nothing else to transmit
	return !ent->s.modelindex && !ent->s.events[0] && !ent->s.light && !ent->s.effects;
}

enum class CullResult {
	DefinitelyCulled,
	ShouldCheckBits,
	DefinitelyKept,
};

static CullResult SNAP_SnapCullEntity( const cmodel_state_t *cms, const edict_t *ent, const edict_t *clent, const vec3_t vieworg ) {
	// filters: this entity has been disabled for comunication
	if( ent->r.svflags & SVF_NOCLIENT ) {
		return CullResult::DefinitelyCulled;
	}

	// we have decided to transmit (almost) everything for spectators
	if( clent->r.client->ps.stats[STAT_REALTEAM] == TEAM_SPECTATOR ) {
		return CullResult::DefinitelyKept;
	}

	// filters: transmit only to clients in the same team as this entity
	// broadcasting is less important than team specifics
	if( ( ent->r.svflags & SVF_ONLYTEAM ) && ( clent && ent->s.team != clent->s.team ) ) {
		return CullResult::DefinitelyCulled;
	}

	// send only to owner
	if( ( ent->r.svflags & SVF_ONLYOWNER ) && ( clent && ent->s.ownerNum != clent->s.number ) ) {
		return CullResult::DefinitelyCulled;
	}

	if( ent->r.svflags & SVF_BROADCAST ) { // send to everyone
		return CullResult::DefinitelyKept;
	}

	if( ( ent->r.svflags & SVF_FORCETEAM ) && ( clent && ent->s.team == clent->s.team ) ) {
		return CullResult::DefinitelyKept;
	}

	if( ent->r.areanum < 0 ) {
		return CullResult::DefinitelyCulled;
	}

	if( SNAP_IsSoundCullOnlyEntity( ent ) ) {
		if( SNAP_SnapCullSoundEntity( cms, ent, vieworg, ent->s.attenuation ) ) {
			return CullResult::DefinitelyCulled;
		}
		return CullResult::DefinitelyKept;
	}

	// If an entity has active sounds/events
	if( ent->s.events[0] || ent->s.sound ) {
		// If the sound is audible, always keep it
		if( !SNAP_SnapCullSoundEntity( cms, ent, vieworg, ent->s.attenuation ) ) {
			return CullResult::DefinitelyKept;
		}
		// Otherwise, fall back to visibility checks
		// (e.g. if we can't hear sounds of a player anyway, check whether the player is visible)
		return CullResult::ShouldCheckBits;
	}

	return CullResult::ShouldCheckBits;
}

static bool SNAP_SnapCullEntityForMultiPovs( const cmodel_state_t *cms, const edict_t *ent,
											 const edict_t **povEntities, const vec3_t *viewOrigins,
											 unsigned numPovEntities, const uint8_t *fatpvs ) {
	assert( numPovEntities > 0 && numPovEntities < MAX_CLIENTS );
	CullResult bestCullResult = CullResult::DefinitelyCulled;
	static_assert( CullResult::DefinitelyCulled < CullResult::ShouldCheckBits );
	static_assert( CullResult::ShouldCheckBits < CullResult::DefinitelyKept );
	for( unsigned povNum = 0; povNum < numPovEntities; ++povNum ) {
		const CullResult cullResult = SNAP_SnapCullEntity( cms, ent, povEntities[povNum], viewOrigins[povNum] );
		if( bestCullResult < cullResult ) {
			bestCullResult = cullResult;
			if( cullResult == CullResult::DefinitelyKept ) {
				break;
			}
		}
	}
	bool isCulled = true;
	if( bestCullResult == CullResult::DefinitelyKept ) {
		isCulled = false;
	} else if( bestCullResult == CullResult::ShouldCheckBits ) {
		if( !SNAP_BitsCullEntity( cms, ent, fatpvs, ent->r.num_clusters ) ) {
			isCulled = false;
		}
	}
	return isCulled;
}

static void SNAP_BuildSnapEntitiesList( cmodel_state_t *cms, ginfo_t *gi,
										const edict_t **povEntities, unsigned numPovEntities,
										const float *skyPortalPovOrigin,
										client_snapshot_t *frame, SnapEntNumsList &list ) {
	// TODO: Get rid of areas?
	frame->areabytes  = CM_AreaRowSize( cms ) * CM_NumAreas( cms );
	frame->clientarea = -1;
	std::memset( frame->areabits, 255, frame->areabytes );

	// TODO: Should we allocate on stack THAT much?
	[[maybe_unused]] vec3_t viewOrigins[MAX_CLIENTS];
	[[maybe_unused]] uint8_t fatpvs[MAX_MAP_LEAFS / 8];
	[[maybe_unused]] int8_t earlyCullingResults[MAX_EDICTS];

	if( !frame->allentities ) {
		assert( numPovEntities > 0 );
		std::memset( earlyCullingResults, 0, gi->num_edicts );
		for( unsigned povNum = 0; povNum < numPovEntities; ++povNum ) {
			const edict_t *povEnt = povEntities[povNum];
			VectorCopy( povEnt->s.origin, viewOrigins[povNum] );
			viewOrigins[povNum][2] += povEnt->r.client->ps.viewheight;
			if( povNum == 0 ) {
				std::memset( fatpvs, 0, CM_ClusterRowSize( cms ) );
			}
			CM_MergePVS( cms, viewOrigins[povNum], fatpvs );
		}
		if( skyPortalPovOrigin ) {
			CM_MergePVS( cms, skyPortalPovOrigin, fatpvs );
		}
		for( int entNum = 1; entNum < gi->num_edicts; entNum++ ) {
			edict_t *const ent = EDICT_NUM( entNum );
			// If it's a portal with a different target origin
			if( ( ent->r.svflags & SVF_PORTAL ) && !VectorCompare( ent->s.origin, ent->s.origin2 ) ) {
				const bool isCulled = SNAP_SnapCullEntityForMultiPovs( cms, ent, povEntities, viewOrigins, numPovEntities, fatpvs );
				earlyCullingResults[entNum] = isCulled ? 1 : -1;
				if( !isCulled ) {
					CM_MergePVS( cms, ent->s.origin2, fatpvs );
				}
			}
		}
	}

	// add the entities to the list
	for( int entNum = 1; entNum < gi->num_edicts; entNum++ ) {
		edict_t *const ent = EDICT_NUM( entNum );

		// fix number if broken
		if( ent->s.number != entNum ) {
			Com_Printf( "FIXING ENT->S.NUMBER: %i %i!!!\n", ent->s.number, entNum );
			ent->s.number = entNum;
		}

		bool shouldAdd = true;
		if( !frame->allentities ) {
			// If it's not in the list of povs
			if( !wsw::contains( povEntities, povEntities + numPovEntities, ent ) ) {
				if( earlyCullingResults[entNum] == 0 ) [[likely]] {
					if( SNAP_SnapCullEntityForMultiPovs( cms, ent, povEntities, viewOrigins, numPovEntities, fatpvs ) ) {
						shouldAdd = false;
					}
				} else if( earlyCullingResults[entNum] > 0 ) {
					shouldAdd = false;
				}
			}
		}

		if( shouldAdd ) {
			list.addEntNum( entNum );

			if( ent->r.svflags & SVF_FORCEOWNER ) {
				// make sure owner number is valid too
				if( ent->s.ownerNum > 0 && ent->s.ownerNum < gi->num_edicts ) {
					list.addEntNum( ent->s.ownerNum );
				} else {
					Com_Printf( "FIXING ENT->S.OWNERNUM: %i %i!!!\n", ent->s.type, ent->s.ownerNum );
					ent->s.ownerNum = 0;
				}
			}
		}
	}
}

/*
* SNAP_BuildClientFrameSnap
*
* Decides which entities are going to be visible to the client, and
* copies off the playerstat and areabits.
*/
void SNAP_BuildClientFrameSnap( cmodel_state_t *cms, ginfo_t *gi, int64_t frameNum,
								int64_t timeStamp, const float *skyPortalPovOrigin, client_t *client,
								const game_state_t *gameState,
								const ReplicatedScoreboardData *scoreboardData,
								client_entities_t *client_entities ) {
	assert( gameState );
	assert( scoreboardData );

	edict_t *clent = client->edict;
	// TODO: Should it happen?
	if( clent && !clent->r.client ) {   // allow nullptr ent for server record
		return;     // not in game yet
	}

	// this is the frame we are creating
	client_snapshot_t *frame = &client->snapShots[frameNum & UPDATE_MASK];
	frame->sentTimeStamp = timeStamp;
	frame->UcmdExecuted = client->UcmdExecuted;

	if( clent ) {
		frame->allentities = false;
		frame->multipov    = clent->r.client->m_multiview;
	} else {
		frame->multipov    = true;
		frame->allentities = true;
	}

	const edict_t *povEntities[MAX_CLIENTS];
	unsigned numPovEntities = 0;

	if( frame->multipov ) {
		for( int clientNum = 0; clientNum < gi->max_clients; clientNum++ ) {
			const edict_t *ent = EDICT_NUM( clientNum + 1 );
			bool isAcceptable  = false;
			if( clent == ent ) {
				isAcceptable = true;
			} else {
				bool couldBeAcceptable = false;
				if( clent ) {
					if( clent->s.team == TEAM_SPECTATOR ) {
						// Try writing everybody in-game (non-specs) to specs
						couldBeAcceptable = ent->s.team != TEAM_SPECTATOR;
					} else if( clent->s.team != TEAM_PLAYERS ) {
						if( clent->s.team == ent->s.team ) {
							// Try writing teammates to team players
							couldBeAcceptable = ent->s.team != TEAM_SPECTATOR;
						}
					}
				} else {
					// Try writing everybody connected including specs to demo
					couldBeAcceptable = true;
				}
				if( couldBeAcceptable ) {
					if( ent->r.inuse && ent->r.client && !( ent->r.svflags & SVF_NOCLIENT ) ) {
						isAcceptable = true;
					}
				}
			}
			if( isAcceptable ) {
				povEntities[numPovEntities] = ent;
				numPovEntities++;
			}
		}
	} else {
		povEntities[0] = clent;
		numPovEntities = 1;
	}

	// areaportals matrix
	int numareas = CM_NumAreas( cms );
	if( frame->numareas < numareas ) {
		frame->numareas = numareas;

		numareas *= CM_AreaRowSize( cms );
		if( frame->areabits ) {
			Q_free( frame->areabits );
			frame->areabits = nullptr;
		}
		frame->areabits = (uint8_t*)Q_malloc( numareas );
	}

	if( frame->ps_size < numPovEntities ) {
		if( frame->ps ) {
			Q_free( frame->ps );
			frame->ps = nullptr;
		}

		frame->ps = ( player_state_t* )Q_malloc( sizeof( player_state_t ) * numPovEntities );
		frame->ps_size = (int)numPovEntities;
	}

	if( frame->multipov ) {
		frame->numplayers = 0;
		for( unsigned povIndex = 0; povIndex < numPovEntities; ++povIndex ) {
			const edict_t *ent = povEntities[povIndex];
			frame->ps[frame->numplayers] = ent->r.client->ps;
			frame->ps[frame->numplayers].playerNum = ent->s.number - 1;
			frame->numplayers++;
		}
	} else {
		frame->ps[0]           = clent->r.client->ps;
		frame->ps[0].playerNum = NUM_FOR_EDICT( clent ) - 1;
		frame->numplayers      = 1;
	}

	// build up the list of visible entities
	SnapEntNumsList entNumsList;
	SNAP_BuildSnapEntitiesList( cms, gi, povEntities, numPovEntities, skyPortalPovOrigin, frame, entNumsList );

	// store current match state information
	frame->gameState = *gameState;
	// TODO: We can write here
	frame->scoreboardData = *scoreboardData;

	if( clent ) {
		const int povTeam = clent->s.team;
		if( povTeam != TEAM_SPECTATOR ) {
			const unsigned povPlayerNum = clent->s.number - 1;
			auto *const transmittedData = &frame->scoreboardData;
			if( povTeam == TEAM_PLAYERS ) {
				for( unsigned playerIndex = 0; playerIndex < (unsigned)MAX_CLIENTS; ++playerIndex ) {
					if( transmittedData->getPlayerTeam( playerIndex ) > TEAM_SPECTATOR ) {
						if( transmittedData->getPlayerNum( playerIndex ) != povPlayerNum ) {
							transmittedData->shadowPrivateData( playerIndex );
						}
					}
				}
			} else {
				for( unsigned playerIndex = 0; playerIndex < MAX_CLIENTS; ++playerIndex ) {
					const int team = frame->scoreboardData.getPlayerTeam( playerIndex );
					if( team > TEAM_SPECTATOR && team != povTeam ) {
						if( transmittedData->getPlayerNum( playerIndex ) != povPlayerNum ) {
							transmittedData->shadowPrivateData( playerIndex );
						}
					}
				}
			}
		}
	}

	// dump the entities list
	unsigned nextEntities = client_entities->next_entities;
	frame->num_entities = 0;
	frame->first_entity = (int)nextEntities;

	for( const int entNum : entNumsList.sortedNums() ) {
		// add it to the circular client_entities array
		const edict_t *ent    = EDICT_NUM( entNum );
		entity_state_t *state = &client_entities->entities[nextEntities % client_entities->num_entities];

		*state = ent->s;
		state->svflags = ent->r.svflags;

		// don't mark *any* missiles as solid
		if( ent->r.svflags & SVF_PROJECTILE ) {
			state->solid = 0;
		}

		frame->num_entities++;
		nextEntities++;
	}

	client_entities->next_entities = nextEntities;
}

static void SNAP_FreeClientFrame( client_snapshot_t *frame ) {
	Q_free( frame->areabits );
	frame->areabits = nullptr;
	frame->numareas = 0;
	Q_free( frame->ps );
	frame->ps      = nullptr;
	frame->ps_size = 0;
}

void SNAP_FreeClientFrames( client_t *client ) {
	for( client_snapshot_t &frame: client->snapShots ) {
		SNAP_FreeClientFrame( &frame );
	}
}

static void SV_AddInfoServer_f( const char *addressString, bool steam ) {
	// TODO: Don't call for such arguments
	if( !addressString || !addressString[0] ) {
		return;
	}

	netadr_t address {};
	if( !NET_StringToAddress( addressString, &address ) ) {
		Com_Printf( "'SV_AddInfoServer_f' Bad info server address: %s\n", addressString );
		return;
	}

	if( !sv_public->integer ) {
		Com_Printf( "'SV_AddInfoServer_f' Only public servers use info servers.\n" );
		return;
	}

	// TODO: Don't call in this state
	// never go public when not acting as a game server
	if( sv.state > ss_game ) {
		return;
	}

	std::optional<int> freeSlot;
	for( int slotNum = 0; slotNum < MAX_INFO_SERVERS; slotNum++ ) {
		auto *const server = &sv_infoServers[slotNum];
		if( server->address.type == NA_NOTRANSMIT ) {
			freeSlot = slotNum;
			break;
		}
	}

	if( freeSlot != std::nullopt ) {
		if( NET_GetAddressPort( &address ) == 0 ) {
			NET_SetAddressPort( &address, PORT_INFO_SERVER );
		}
		sv_infoServers[*freeSlot].address = address;
		sv_infoServers[*freeSlot].steam   = steam;
		Com_Printf( "Added new info server #%i at %s\n", *freeSlot, NET_AddressToString( &address ) );
	} else {
		Com_Printf( "'SV_AddInfoServer_f' List of info servers is already full\n" );
	}
}

static void SV_ResolveInfoServers() {
	// wsw : jal : initialize info servers list
	memset( sv_infoServers, 0, sizeof( sv_infoServers ) );

	//never go public when not acting as a game server
	if( sv.state > ss_game ) {
		return;
	}

	if( !sv_public->integer ) {
		return;
	}

	wsw::StringSplitter stringSplitter( wsw::StringView( sv_infoservers->string ) );
	while( const std::optional<wsw::StringView> address = stringSplitter.getNext() ) {
		wsw::PodVector<char> ztAddress( address->data(), address->size() );
		ztAddress.append( '\0' );
		SV_AddInfoServer_f( ztAddress.data(), false );
	}

	svc.lastInfoServerResolve = Sys_Milliseconds();
}

void SV_InitInfoServers() {
	SV_ResolveInfoServers();

	svc.nextHeartbeat = Sys_Milliseconds() + HEARTBEAT_SECONDS * 1000; // wait a while before sending first heartbeat
}

void SV_UpdateInfoServers() {
	if( svc.lastInfoServerResolve + TTL_INFO_SERVERS < Sys_Milliseconds() ) {
		SV_ResolveInfoServers();
	}
}

void SV_InfoServerHeartbeat() {
	const int64_t time = Sys_Milliseconds();
	if( svc.nextHeartbeat > time ) {
		return;
	}

	svc.nextHeartbeat = time + HEARTBEAT_SECONDS * 1000;

	if( !sv_public->integer || ( sv_maxclients->integer == 1 ) ) {
		return;
	}

	// never go public when not acting as a game server
	if( sv.state > ss_game ) {
		return;
	}

	for( const auto &server : sv_infoServers ) {
		if( server.address.type != NA_NOTRANSMIT ) {
			if( dedicated && dedicated->integer ) {
				Com_Printf( "Sending heartbeat to %s\n", NET_AddressToString( &server.address ) );
			}

			socket_t *const socket = ( server.address.type == NA_IP6 ? &svs.socket_udp6 : &svs.socket_udp );
			if( server.steam ) {
				uint8_t steamHeartbeat = 'q';
				NET_SendPacket( socket, &steamHeartbeat, sizeof( steamHeartbeat ), &server.address );
			} else {
				// warning: "DarkPlaces" is a protocol name here, not a game name. Do not replace it.
				Netchan_OutOfBandPrint( socket, &server.address, "heartbeat DarkPlaces\n" );
			}
		}
	}
}

void SV_InfoServerSendQuit() {
	if( !sv_public->integer || ( sv_maxclients->integer == 1 ) ) {
		return;
	}

	// never go public when not acting as a game server
	if( sv.state > ss_game ) {
		return;
	}

	for( auto &server : sv_infoServers ) {
		if( server.steam && ( server.address.type != NA_NOTRANSMIT ) ) {
			socket_t *socket = ( server.address.type == NA_IP6 ? &svs.socket_udp6 : &svs.socket_udp );

			if( dedicated && dedicated->integer ) {
				Com_Printf( "Sending quit to %s\n", NET_AddressToString( &server.address ) );
			}

			const char quitMessage[] = "b\n";
			NET_SendPacket( socket, ( const uint8_t * )quitMessage, sizeof( quitMessage ), &server.address );
		}
	}
}

static char *SV_LongInfoString( bool fullStatus ) {
	static char status[MAX_MSGLEN - 16];

	Q_strncpyz( status, Cvar_Serverinfo(), sizeof( status ) );

	// convert "g_gametype" to "gametype"
	if( const char *gametype = Info_ValueForKey( status, "g_gametype" ) ) {
		Info_RemoveKey( status, "g_gametype" );
		Info_SetValueForKey( status, "gametype", gametype );
	}

	size_t statusLength = strlen( status );

	int bots = 0;
	int count = 0;
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( const client_t *client = iteratorOverClients.getNext() ) {
		if( client->isAFakeClient() ) {
			bots++;
		}
		count++;
	}

	char tempstr[1024] = { 0 };
	if( bots ) {
		Q_snprintfz( tempstr, sizeof( tempstr ), "\\bots\\%i", bots );
	}

	Q_snprintfz( tempstr + strlen( tempstr ), sizeof( tempstr ) - strlen( tempstr ), "\\clients\\%i%s", count, fullStatus ? "\n" : "" );
	size_t tempstrLength = strlen( tempstr );
	if( statusLength + tempstrLength >= sizeof( status ) ) {
		return status; // can't hold any more
	}

	Q_strncpyz( status + statusLength, tempstr, sizeof( status ) - statusLength );
	statusLength += tempstrLength;

	if( fullStatus ) {
		iteratorOverClients.rewind();
		while( const client_t *client = iteratorOverClients.getNext() ) {
			Q_snprintfz( tempstr, sizeof( tempstr ), "%i %i \"%s\" %i\n",
						 client->edict->r.client->m_frags, client->edict->r.client->m_ping, client->name, client->edict->s.team );
			tempstrLength = strlen( tempstr );
			if( statusLength + tempstrLength >= sizeof( status ) ) {
				break; // can't hold any more
			}
			Q_strncpyz( status + statusLength, tempstr, sizeof( status ) - statusLength );
			statusLength += tempstrLength;
		}
	}

	return status;
}

/*
* SV_ShortInfoString
* Generates a short info string for broadcast scan replies
*/
static char *SV_ShortInfoString( void ) {
#define MAX_STRING_SVCINFOSTRING 180
#define MAX_SVCINFOSTRING_LEN ( MAX_STRING_SVCINFOSTRING - 4 )
	static char string[MAX_STRING_SVCINFOSTRING];

	int bots = 0;
	int count = 0;
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( const client_t *client = iteratorOverClients.getNext() ) {
		if( client->isAFakeClient() ) {
			bots++;
		} else {
			count++;
		}
	}

	const int maxcount = sv_maxclients->integer - bots;

	//format:
	//" \377\377\377\377info\\n\\server_name\\m\\map name\\u\\clients/maxclients\\g\\gametype\\s\\skill\\EOT "

	char entry[20];
	char hostname[64];

	Q_strncpyz( hostname, sv_hostname->string, sizeof( hostname ) );
	Q_snprintfz( string, sizeof( string ),
				 "\\\\n\\\\%s\\\\m\\\\%8s\\\\u\\\\%2i/%2i\\\\",
				 hostname,
				 sv.mapname,
				 count > 99 ? 99 : count,
				 maxcount > 99 ? 99 : maxcount
	);

	size_t len = strlen( string );
	Q_snprintfz( entry, sizeof( entry ), "g\\\\%6s\\\\", Cvar_String( "g_gametype" ) );
	if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
		Q_strncatz( string, entry, sizeof( string ) );
		len = strlen( string );
	}

	if( Cvar_Value( "g_instagib" ) ) {
		Q_snprintfz( entry, sizeof( entry ), "ig\\\\1\\\\" );
		if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
			Q_strncatz( string, entry, sizeof( string ) );
			len = strlen( string );
		}
	}


	Q_snprintfz( entry, sizeof( entry ), "s\\\\%1d\\\\", sv_skilllevel->integer );
	if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
		Q_strncatz( string, entry, sizeof( string ) );
		len = strlen( string );
	}

	if( Cvar_String( "password" )[0] != '\0' ) {
		Q_snprintfz( entry, sizeof( entry ), "p\\\\1\\\\" );
		if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
			Q_strncatz( string, entry, sizeof( string ) );
			len = strlen( string );
		}
	}

	if( bots ) {
		Q_snprintfz( entry, sizeof( entry ), "b\\\\%2i\\\\", bots > 99 ? 99 : bots );
		if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
			Q_strncatz( string, entry, sizeof( string ) );
			len = strlen( string );
		}
	}

	if( Cvar_Value( "g_race_gametype" ) ) {
		Q_snprintfz( entry, sizeof( entry ), "r\\\\1\\\\" );
		if( MAX_SVCINFOSTRING_LEN - len > strlen( entry ) ) {
			Q_strncatz( string, entry, sizeof( string ) );
			len = strlen( string );
		}
	}

	// finish it
	Q_strncatz( string, "EOT", sizeof( string ) );
	return string;
}

static void HandleOobCommand_Ack( const socket_t *socket, const netadr_t *address, const CmdArgs & ) {
	Com_Printf( "Ping acknowledge from %s\n", NET_AddressToString( address ) );
}

static void HandleOobCommand_Ping( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	// send any arguments back with ack
	Netchan_OutOfBandPrint( socket, address, "ack %s", Cmd_Args() );
}

static void HandleOobCommand_Info( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	if( sv_showInfoQueries->integer ) {
		Com_Printf( "Info Packet %s\n", NET_AddressToString( address ) );
	}

	// KoFFiE: When not public and coming from a LAN address
	//         assume broadcast and respond anyway, otherwise ignore
	if( ( ( !sv_public->integer ) && ( !NET_IsLANAddress( address ) ) ) || ( sv_maxclients->integer == 1 ) ) {
		return;
	}

	// ignore when in invalid server state
	if( sv.state < ss_loading || sv.state > ss_game ) {
		return;
	}

	// don't reply when we are locked for mm
	// if( SV_MM_IsLocked() )
	//	return;

	// different protocol version
	if( atoi( Cmd_Argv( 1 ) ) != APP_PROTOCOL_VERSION ) {
		return;
	}

	// check for full/empty filtered states
	bool allow_empty = false, allow_full = false;
	for( int i = 0; i < Cmd_Argc(); i++ ) {
		if( !Q_stricmp( Cmd_Argv( i ), "full" ) ) {
			allow_full = true;
		} else if( !Q_stricmp( Cmd_Argv( i ), "empty" ) ) {
			allow_empty = true;
		}
	}

	int count = 0;
	for( int i = 0; i < sv_maxclients->integer; i++ ) {
		if( svs.clients[i].state >= CS_CONNECTED ) {
			count++;
		}
	}

	if( ( count == sv_maxclients->integer ) && !allow_full ) {
		return;
	}

	if( ( count == 0 ) && !allow_empty ) {
		return;
	}

	if( const char *string = SV_ShortInfoString() ) {
		Netchan_OutOfBandPrint( socket, address, "info\n%s", string );
	}
}

static void SVC_SendInfoString( const socket_t *socket, const netadr_t *address, const char *requestType, const char *responseType, bool fullStatus, const CmdArgs &cmdArgs ) {
	if( sv_showInfoQueries->integer ) {
		Com_Printf( "%s Packet %s\n", requestType, NET_AddressToString( address ) );
	}

	// KoFFiE: When not public and coming from a LAN address
	//         assume broadcast and respond anyway, otherwise ignore
	if( ( ( !sv_public->integer ) && ( !NET_IsLANAddress( address ) ) ) || ( sv_maxclients->integer == 1 ) ) {
		return;
	}

	// ignore when in invalid server state
	if( sv.state < ss_loading || sv.state > ss_game ) {
		return;
	}

	// don't reply when we are locked for mm
	// if( SV_MM_IsLocked() )
	//	return;

	// send the same string that we would give for a status OOB command
	if( const char *string = SV_LongInfoString( fullStatus ) ) {
		Netchan_OutOfBandPrint( socket, address, "%s\n\\challenge\\%s%s", responseType, Cmd_Argv( 1 ), string );
	}
}

static void HandleOobCommand_GetInfo( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	SVC_SendInfoString( socket, address, "GetInfo", "infoResponse", false, cmdArgs );
}

static void HandleOobCommand_GetStatus( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	SVC_SendInfoString( socket, address, "GetStatus", "statusResponse", true, cmdArgs );
}


/*
* SVC_GetChallenge
*
* Returns a challenge number that can be used
* in a subsequent client_connect command.
* We do this to prevent denial of service attacks that
* flood the server with invalid connection IPs.  With a
* challenge, they must give a valid IP address.
*/
static void HandleOobCommand_GetChallenge( const socket_t *socket, const netadr_t *address, const CmdArgs & ) {
	if( sv_showChallenge->integer ) {
		Com_Printf( "Challenge Packet %s\n", NET_AddressToString( address ) );
	}

	int index          = 0;
	int oldestIndex    = 0;
	int64_t oldestTime = std::numeric_limits<int64_t>::max();

	// see if we already have a challenge for this ip
	for( index = 0; index < MAX_CHALLENGES; index++ ) {
		if( NET_CompareBaseAddress( address, &svs.challenges[index].adr ) ) {
			break;
		} else {
			if( svs.challenges[index].time < oldestTime ) {
				oldestTime = svs.challenges[index].time;
				oldestIndex = index;
			}
		}
	}

	if( index == MAX_CHALLENGES ) {
		// overwrite the oldest
		svs.challenges[oldestIndex].challenge = rand() & 0x7fff;
		svs.challenges[oldestIndex].adr       = *address;
		svs.challenges[oldestIndex].time      = Sys_Milliseconds();
		index = oldestIndex;
	}

	Netchan_OutOfBandPrint( socket, address, "challenge %i", svs.challenges[index].challenge );
}

static const char *ValidateChallenge( const netadr_t *address, int challenge ) {
	for( auto &entry : svs.challenges ) {
		if( NET_CompareBaseAddress( address, &entry.adr ) ) {
			if( challenge == entry.challenge ) {
				// wsw : r1q2 : reset challenge
				entry.challenge = 0;
				entry.time      = 0;
				NET_InitAddress( &entry.adr, NA_NOTRANSMIT );
				return nullptr;
			}
			return "Bad challenge";
		}
	}
	return "No challenge for address";
}

static bool CheckIPConnectionLimit( const netadr_t *address ) {
	//r1: limit connections from a single IP
	if( sv_iplimit->integer ) {
		int previousclients = 0;
		IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_ZOMBIE, .includeFakeClients = false } );
		while( const client_t *cl = iteratorOverClients.getNext() ) {
			if( NET_CompareBaseAddress( address, &cl->netchan.remoteAddress ) ) {
				//r1: zombies are less dangerous
				if( cl->state == CS_ZOMBIE ) {
					previousclients++;
				} else {
					previousclients += 2;
				}
			}
		}

		if( previousclients >= sv_iplimit->integer * 2 ) {
			return false;
		}
	}
	// Check passed
	return true;
}

struct NoMatchingSlot {};
struct TooSoon {};

static std::variant<NoMatchingSlot, TooSoon, int> FindExistingClientSlotForConnection( const netadr_t *address, int game_port ) {
	const int64_t time = Sys_Milliseconds();
	IteratorOverClients iteratorOverClients( { .minAcceptableState = CS_ZOMBIE, .includeFakeClients = false } );
	while( auto maybeClientAndNum = iteratorOverClients.getNextWithIndex() ) {
		auto [cl, i] = *maybeClientAndNum;
		bool matchesAddress = false;
		if( NET_CompareAddress( address, &cl->netchan.remoteAddress ) ) {
			matchesAddress = true;
		} else if( NET_CompareBaseAddress( address, &cl->netchan.remoteAddress ) && cl->netchan.game_port == game_port ) {
			matchesAddress = true;
		}
		if( matchesAddress ) {
			if( !NET_IsLocalAddress( address ) && ( time - cl->lastconnect ) < (unsigned)( sv_reconnectlimit->integer * 1000 ) ) {
				return TooSoon {};
			}
			return i;
		}
	}
	return NoMatchingSlot {};
}

static client_t *FindClientToAssignNewConnection() {
	client_t *newcl = nullptr;
	for( int i = 0; i < sv_maxclients->integer; i++ ) {
		client_t *cl = svs.clients + i;
		if( cl->state == CS_FREE ) {
			newcl = cl;
			break;
		}
	}
	if( !newcl ) {
		for( int i = 0; i < sv_maxclients->integer; i++ ) {
			client_t *cl = svs.clients + i;
			// overwrite fakeclient if no free spots found
			// TODO: The game module should decide what bot to kick
			if( cl->state && cl->edict && ( cl->edict->r.svflags & SVF_FAKECLIENT ) ) {
				newcl = cl;
			}
		}
		if( newcl ) {
			if( newcl->state && newcl->edict && ( newcl->edict->r.svflags & SVF_FAKECLIENT ) ) {
				SV_DropClient( newcl, ReconnectBehaviour::DontReconnect, "%s", "Need room for a real player" );
			}
		}
	}
	return newcl;
}

static void SendRejectPacket( const socket_t *socket, const netadr_t *address, const char *message, ReconnectBehaviour reconnectBehaviour ) {
	// The two initial numeric values are for compatibility with old (pre-2.6) clients
	Netchan_OutOfBandPrint( socket, address, "reject\n0\n0\n%s\n%u\n%u\n%u\n", ( message ? message : "" ),
							(unsigned)APP_PROTOCOL_VERSION, (unsigned)ConnectionDropStage::EstablishingFailed, (unsigned)reconnectBehaviour );
}

static void HandleOobCommand_Connect( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	Com_DPrintf( "HandleOobCommand_Connect(%s)\n", Cmd_Args() );

	if( const int version = atoi( Cmd_Argv( 1 ) ); version != APP_PROTOCOL_VERSION ) {
		if( version <= 6 ) { // before reject packet was added
			Netchan_OutOfBandPrint( socket, address, "print\nServer is version %4.2f. Protocol %3i\n",
									APP_VERSION, APP_PROTOCOL_VERSION );
		} else {
			wsw::StaticString<128> buffer;
			buffer << wsw::StringView( "Server and client don't have the same version: expected=" );
			buffer << APP_PROTOCOL_VERSION << wsw::StringView( ", got" ) << version;
			SendRejectPacket( socket, address, buffer.data(), ReconnectBehaviour::DontReconnect );
		}
		Com_DPrintf( "    rejected connect from protocol %i\n", version );
	} else {
		if( !Info_Validate( Cmd_Argv( 4 ) ) ) {
			SendRejectPacket( socket, address, "Invalid userinfo string", ReconnectBehaviour::DontReconnect );
			Com_DPrintf( "Connection from %s refused: invalid userinfo string\n", NET_AddressToString( address ) );
		} else {
			char userinfo[MAX_INFO_STRING];
			Q_strncpyz( userinfo, Cmd_Argv( 4 ), sizeof( userinfo ) );

			// force the IP key/value pair so the game can filter based on ip
			if( !Info_SetValueForKey( userinfo, "socket", NET_SocketTypeToString( socket->type ) ) ) {
				SendRejectPacket( socket, address, "Couldn't set userinfo (socket)", ReconnectBehaviour::OfUserChoice );
				Com_DPrintf( "Connection from %s refused: couldn't set userinfo (socket)\n", NET_AddressToString( address ) );
			} else if( !Info_SetValueForKey( userinfo, "ip", NET_AddressToString( address ) ) ) {
				SendRejectPacket( socket, address, "Couldn't set userinfo (ip)", ReconnectBehaviour::OfUserChoice );
				Com_DPrintf( "Connection from %s refused: couldn't set userinfo (ip)\n", NET_AddressToString( address ) );
			} else {
				mm_uuid_t session_id, ticket_id;
				if( Cmd_Argc() >= 7 ) {
					// we have extended information, ticket-id and session-id
					Com_Printf( "Extended information %s\n", Cmd_Argv( 6 ) );
					if( !Uuid_FromString( Cmd_Argv( 6 ), &ticket_id ) ) {
						ticket_id = session_id = Uuid_ZeroUuid();
					} else {
						const char *session_id_str = Info_ValueForKey( userinfo, "cl_mm_session" );
						if( !Uuid_FromString( session_id_str, &session_id ) ) {
							ticket_id = session_id = Uuid_ZeroUuid();
						}
					}
				} else {
					ticket_id = session_id = Uuid_ZeroUuid();
				}

				const int challenge = atoi( Cmd_Argv( 3 ) );
				if( const char *challengeErrorString = ValidateChallenge( address, challenge ) ) {
					SendRejectPacket( socket, address, challengeErrorString, ReconnectBehaviour::OfUserChoice );
				} else {
					if( !CheckIPConnectionLimit( address ) ) {
						SendRejectPacket( socket, address, "Too many connections from your host", ReconnectBehaviour::DontReconnect );
						Com_DPrintf( "%s:connect rejected : too many connections\n", NET_AddressToString( address ) );
					} else {
						const int game_port = atoi( Cmd_Argv( 2 ) );
						// if there is already a slot for this ip, reuse it
						std::variant<NoMatchingSlot, TooSoon, int> findResult = FindExistingClientSlotForConnection( address, game_port );
						if( std::holds_alternative<TooSoon>( findResult ) ) {
							Com_DPrintf( "%s:reconnect rejected : too soon\n", NET_AddressToString( address ) );
						} else {
							client_t *newcl     = nullptr;
							if( const int *slot = std::get_if<int>( &findResult ) ) {
								Com_Printf( "%s:reconnect\n", NET_AddressToString( address ) );
								newcl = svs.clients + *slot;
							} else {
								newcl = FindClientToAssignNewConnection();
							}
							if( !newcl ) {
								SendRejectPacket( socket, address, "The server is full", ReconnectBehaviour::OfUserChoice );
								Com_DPrintf( "Server is full. Rejected a connection.\n" );
							} else {
								// get the game a chance to reject this connection or modify the userinfo
								if( !SV_ClientConnect( socket, address, newcl, userinfo, game_port, challenge, false, ticket_id, session_id ) ) {
									std::optional<ReconnectBehaviour> reconnectBehavior;
									if( const auto maybeRawBehavior = wsw::toNum<unsigned>( Info_ValueForKey( userinfo, "rejbehavior" ) ) ) {
										for( const ReconnectBehaviour behaviourValue : kReconnectBehaviourValues ) {
											if( (unsigned)behaviourValue == *maybeRawBehavior ) {
												reconnectBehavior = behaviourValue;
												break;
											}
										}
									}

									const char *rejmsg = Info_ValueForKey( userinfo, "rejmsg" );
									if( !rejmsg ) {
										rejmsg = "Game module rejected connection";
									}

									SendRejectPacket( socket, address, rejmsg, reconnectBehavior.value() );
									Com_DPrintf( "Game rejected a connection.\n" );
								} else {
									// send the connect packet to the client
									Netchan_OutOfBandPrint( socket, address, "client_connect\n%s", newcl->session );
								}
							}
						}
					}
				}
			}
		}
	}
}

/*
* SVC_FakeConnect
* (Not a real out of band command)
* A connection request that came from the game module
*/
int SVC_FakeConnect( const char *fakeUserinfo, const char *fakeSocketType, const char *fakeIP ) {
	Com_DPrintf( "SVC_FakeConnect ()\n" );

	if( !fakeUserinfo ) {
		fakeUserinfo = "";
	}
	if( !fakeIP ) {
		fakeIP = "127.0.0.1";
	}
	if( !fakeSocketType ) {
		fakeIP = "loopback";
	}

	char userinfo[MAX_INFO_STRING];
	Q_strncpyz( userinfo, fakeUserinfo, sizeof( userinfo ) );

	// force the IP key/value pair so the game can filter based on ip
	if( Info_SetValueForKey( userinfo, "socket", fakeSocketType ) && Info_SetValueForKey( userinfo, "ip", fakeIP ) ) {
		// find a client slot
		client_t *newcl = nullptr;
		for( int i = 0; i < sv_maxclients->integer; i++ ) {
			client_t *cl = svs.clients + i;
			if( cl->state == CS_FREE ) {
				newcl = cl;
				break;
			}
		}

		if( !newcl ) {
			Com_DPrintf( "Rejected a connection.\n" );
		} else {
			netadr_t address;
			NET_InitAddress( &address, NA_NOTRANSMIT );
			// get the game a chance to reject this connection or modify the userinfo
			mm_uuid_t session_id = Uuid_ZeroUuid();
			mm_uuid_t ticket_id  = Uuid_ZeroUuid();
			if( !SV_ClientConnect( nullptr, &address, newcl, userinfo, -1, -1, true, session_id, ticket_id ) ) {
				Com_DPrintf( "Game rejected a connection.\n" );
			} else {
				// directly call the game begin function
				newcl->state = CS_SPAWNED;
				G_ClientBegin( newcl->edict );

				return NUM_FOR_EDICT( newcl->edict );
			}
		}
	}

	return -1;
}

static int Rcon_Auth( const CmdArgs &cmdArgs ) {
	if( !strlen( rcon_password->string ) ) {
		return 0;
	}

	if( strcmp( Cmd_Argv( 1 ), rcon_password->string ) ) {
		return 0;
	}

	return 1;
}

/*
* SVC_RemoteCommand
*
* A client issued an rcon command.
* Shift down the remaining args
* Redirect all printfs
*/
static void HandleOobCommand_Rcon( const socket_t *socket, const netadr_t *address, const CmdArgs &cmdArgs ) {
	if( !Rcon_Auth( cmdArgs ) ) {
		Com_Printf( "Bad rcon from %s:\n%s\n", NET_AddressToString( address ), Cmd_Args() );
	} else {
		Com_Printf( "Rcon from %s:\n%s\n", NET_AddressToString( address ), Cmd_Args() );
	}

	flush_params_t extra;
	extra.socket = socket;
	extra.address = address;
	Com_BeginRedirect( RD_PACKET, sv_outputbuf, SV_OUTPUTBUF_LENGTH, SV_FlushRedirect, ( const void * )&extra );

	if( sv_showRcon->integer ) {
		Com_Printf( "Rcon Packet %s\n", NET_AddressToString( address ) );
	}

	if( !Rcon_Auth( cmdArgs ) ) {
		Com_Printf( "Bad rcon_password.\n" );
	} else {
		char remaining[1024];
		remaining[0] = 0;
		for( int i = 2; i < Cmd_Argc(); i++ ) {
			Q_strncatz( remaining, "\"", sizeof( remaining ) );
			Q_strncatz( remaining, Cmd_Argv( i ), sizeof( remaining ) );
			Q_strncatz( remaining, "\" ", sizeof( remaining ) );
		}

		SV_Cmd_ExecuteNow( remaining );
	}

	Com_EndRedirect();
}

typedef struct {
	const char *name;
	void ( *func )( const socket_t *socket, const netadr_t *address, const CmdArgs & );
} connectionless_cmd_t;

static connectionless_cmd_t connectionless_cmds[] =
	{
		{ "ping", HandleOobCommand_Ping },
		{ "ack", HandleOobCommand_Ack },
		{ "info", HandleOobCommand_Info },
		{ "getinfo", HandleOobCommand_GetInfo },
		{ "getstatus", HandleOobCommand_GetStatus },
		{ "getchallenge", HandleOobCommand_GetChallenge },
		{ "connect", HandleOobCommand_Connect },
		{ "rcon", HandleOobCommand_Rcon },

		{ NULL, NULL }
	};

/*
* SV_ConnectionlessPacket
*
* A connectionless packet has four leading 0xff
* characters to distinguish it from a game channel.
* Clients that are in the game can still send
* connectionless packets.
*/
void SV_ConnectionlessPacket( const socket_t *socket, const netadr_t *address, msg_t *msg ) {

	MSG_BeginReading( msg );
	MSG_ReadInt32( msg );    // skip the -1 marker

	static CmdArgsSplitter argsSplitter;
	const CmdArgs &cmdArgs = argsSplitter.exec( wsw::StringView( MSG_ReadStringLine( msg ) ) );

	Com_DPrintf( "Packet %s : %s\n", NET_AddressToString( address ), cmdArgs[0].data() );

	for( const connectionless_cmd_t *cmd = connectionless_cmds; cmd->name; cmd++ ) {
		if( !strcmp( cmdArgs[0].data(), cmd->name ) ) {
			cmd->func( socket, address, cmdArgs );
			return;
		}
	}

	Com_DPrintf( "Bad connectionless packet from %s:\n%s\n", NET_AddressToString( address ), cmdArgs[0].data() );
}

#define SV_DEMO_DIR va( "demos/server%s%s", sv_demodir->string[0] ? "/" : "", sv_demodir->string[0] ? sv_demodir->string : "" )

static void SV_Demo_WriteMessage( msg_t *msg ) {
	assert( svs.demo.file );
	if( svs.demo.file ) {
		SNAP_RecordDemoMessage( svs.demo.file, msg, 0 );
	}
}

static void SV_Demo_WriteStartMessages() {
	SNAP_BeginDemoRecording( svs.demo.file, svs.spawncount, svc.snapFrameTime, sv.mapname, SV_BITFLAGS_RELIABLE,
							 svs.purelist, sv.configStrings, sv.baselines );
}

void SV_Demo_WriteSnap() {
	if( !svs.demo.file ) {
		return;
	}

	bool hasActivePlayers = false;
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *client = iteratorOverClients.getNext() ) {
		// TODO: Should this condition be an iterator parameter?
		if( !client->isAHiddenClient() ) {
			hasActivePlayers = true;
			break;
		}
	}
	if( !hasActivePlayers ) {
		Com_Printf( "No players left, stopping server side demo recording\n" );
		SV_Demo_Stop_f( CmdArgs {} );
	} else {
		msg_t msg;
		uint8_t msg_buffer[MAX_MSGLEN];
		MSG_Init( &msg, msg_buffer, sizeof( msg_buffer ) );

		SV_BuildClientFrameSnap( &svs.demo.client );

		SV_WriteFrameSnapToClient( &svs.demo.client, &msg );

		SV_AddReliableCommandsToMessage( &svs.demo.client, &msg );

		SV_Demo_WriteMessage( &msg );

		svs.demo.duration = svs.gametime - svs.demo.basetime;
		svs.demo.client.lastframe = sv.framenum; // FIXME: is this needed?
	}
}

static void SV_Demo_InitClient() {
	memset( &svs.demo.client, 0, sizeof( svs.demo.client ) );

	svs.demo.client.reliable = true;

	svs.demo.client.reliableAcknowledge = 0;
	svs.demo.client.reliableSequence    = 0;
	svs.demo.client.reliableSent        = 0;

	for( auto &cmd: svs.demo.client.reliableCommands ) {
		cmd.clear();
	}

	svs.demo.client.lastframe = sv.framenum - 1;
	svs.demo.client.nodelta   = false;
}

void SV_Demo_Start_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		Com_Printf( "Usage: serverrecord <demoname>\n" );
		return;
	}

	if( svs.demo.file ) {
		Com_Printf( "Already recording\n" );
		return;
	}

	if( sv.state != ss_game ) {
		Com_Printf( "Must be in a level to record\n" );
		return;
	}

	bool hasActivePlayers = false;
	IteratorOverClients iteratorOverClients( { .includeFakeClients = true } );
	while( client_t *client = iteratorOverClients.getNext() ) {
		// TODO: Should this condition be an iterator parameter?
		if( !client->isAHiddenClient() ) {
			hasActivePlayers = true;
			break;
		}
	}
	if( !hasActivePlayers ) {
		Com_Printf( "No players in game, can't record a demo\n" );
	} else {
		//
		// open the demo file
		//

		// real name
		size_t demofilename_size = sizeof( char ) * ( strlen( SV_DEMO_DIR ) + 1 + strlen( Cmd_Args() ) + strlen( APP_DEMO_EXTENSION_STR ) + 1 );
		svs.demo.filename = (char *)Q_malloc( demofilename_size );

		Q_snprintfz( svs.demo.filename, demofilename_size, "%s/%s", SV_DEMO_DIR, Cmd_Args() );

		COM_SanitizeFilePath( svs.demo.filename );

		if( !COM_ValidateRelativeFilename( svs.demo.filename ) ) {
			Q_free( svs.demo.filename );
			svs.demo.filename = nullptr;
			Com_Printf( "Invalid filename.\n" );
		} else {
			COM_DefaultExtension( svs.demo.filename, APP_DEMO_EXTENSION_STR, demofilename_size );

			// temp name
			demofilename_size = sizeof( char ) * ( strlen( svs.demo.filename ) + strlen( ".rec" ) + 1 );
			svs.demo.tempname = (char *)Q_malloc( demofilename_size );
			Q_snprintfz( svs.demo.tempname, demofilename_size, "%s.rec", svs.demo.filename );

			// open it
			if( FS_FOpenFile( svs.demo.tempname, &svs.demo.file, FS_WRITE | SNAP_DEMO_GZ ) == -1 ) {
				Com_Printf( "Error: Couldn't open file: %s\n", svs.demo.tempname );
				Q_free( svs.demo.filename );
				svs.demo.filename = nullptr;
				Q_free( svs.demo.tempname );
				svs.demo.tempname = nullptr;
			} else {
				Com_Printf( "Recording server demo: %s\n", svs.demo.filename );

				SV_Demo_InitClient();

				// write serverdata, configstrings and baselines
				svs.demo.duration = 0;
				svs.demo.basetime = svs.gametime;
				svs.demo.localtime = time( nullptr );
				SV_Demo_WriteStartMessages();

				// write one nodelta frame
				svs.demo.client.nodelta = true;
				SV_Demo_WriteSnap();
				svs.demo.client.nodelta = false;
			}
		}
	}
}

static void SV_Demo_Stop( bool cancel, bool silent ) {
	if( !svs.demo.file ) {
		if( !silent ) {
			Com_Printf( "No server demo recording in progress\n" );
		}
		return;
	}

	if( cancel ) {
		Com_Printf( "Canceled server demo recording: %s\n", svs.demo.filename );
	} else {
		SNAP_StopDemoRecording( svs.demo.file );

		Com_Printf( "Stopped server demo recording: %s\n", svs.demo.filename );
	}

	FS_FCloseFile( svs.demo.file );
	svs.demo.file = 0;

	if( cancel ) {
		if( !FS_RemoveFile( svs.demo.tempname ) ) {
			Com_Printf( "Error: Failed to delete the temporary server demo file\n" );
		}
	} else {
		using namespace wsw;

		char metadata[SNAP_MAX_DEMO_META_DATA_SIZE];
		DemoMetadataWriter writer( metadata );

		writer.writePair( kDemoKeyServerName, sv.configStrings.getHostName().value() );
		writer.writePair( kDemoKeyTimestamp, wsw::StringView( va( "%" PRIu64, (uint64_t)svs.demo.localtime ) ) );
		writer.writePair( kDemoKeyDuration, wsw::StringView( va( "%u", (int)ceil( svs.demo.duration / 1000.0f ) ) ) );
		writer.writePair( kDemoKeyMapName, sv.configStrings.getMapName().value() );
		writer.writePair( kDemoKeyMapChecksum, sv.configStrings.getMapCheckSum().value() );
		writer.writePair( kDemoKeyGametype, sv.configStrings.getGametypeName().value() );

		writer.writeTag( kDemoTagMultiPov );

		const auto [metadataSize, wasComplete] = writer.markCurrentResult();
		if( !wasComplete ) {
			Com_Printf( S_COLOR_YELLOW "The demo metadata was truncated\n" );
		}

		SNAP_WriteDemoMetaData( svs.demo.tempname, metadata, metadataSize );

		if( !FS_MoveFile( svs.demo.tempname, svs.demo.filename ) ) {
			Com_Printf( "Error: Failed to rename the server demo file\n" );
		}
	}

	svs.demo.localtime = 0;
	svs.demo.basetime = svs.demo.duration = 0;

	SNAP_FreeClientFrames( &svs.demo.client );

	Q_free( svs.demo.filename );
	svs.demo.filename = nullptr;
	Q_free( svs.demo.tempname );
	svs.demo.tempname = nullptr;
}

void SV_Demo_Stop_f( const CmdArgs &cmdArgs ) {
	SV_Demo_Stop( false, atoi( Cmd_Argv( 1 ) ) != 0 );
}

void SV_Demo_Cancel_f( const CmdArgs &cmdArgs ) {
	SV_Demo_Stop( true, atoi( Cmd_Argv( 1 ) ) != 0 );
}

[[nodiscard]]
static bool isAutoRecordDemoName( const wsw::StringView &fileName, const wsw::StringView &extension ) {
	assert( fileName.endsWith( extension ) );
	constexpr wsw::StringView suffixTemplate( "_auto0000" );
	if( fileName.size() < extension.size() + suffixTemplate.size() ) {
		return false;
	}
	if( const auto maybeAutoSuffix = fileName.dropRight( extension.size() ).takeRightExact( suffixTemplate.size() ) ) {
		constexpr wsw::StringView autoPrefix( "_auto"_asView );
		if( maybeAutoSuffix->startsWith( autoPrefix ) ) {
			const wsw::StringView numSuffix = maybeAutoSuffix->drop( autoPrefix.size() );
			if( const std::optional<unsigned> maybeNum = wsw::toNum<unsigned>( numSuffix ) ) {
				if( *maybeNum >= 0 && *maybeNum < 9999 ) {
					// TODO: Is this check really needed?
					if( wsw::StaticString<32> chars( "%04u", *maybeNum ); chars.asView() == numSuffix ) {
						return true;
					}
				}
			}
		}
	}
	return false;
}

void SV_Demo_Purge_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() > 2 ) {
		Com_Printf( "Usage: serverrecordpurge [maxautodemos]\n" );
		return;
	}

	unsigned maxautodemos = 0;
	if( Cmd_Argc() == 2 ) {
		maxautodemos = (unsigned)atoi( Cmd_Argv( 1 ) );
	}

	const wsw::StringView demoDir( SV_DEMO_DIR );
	const wsw::StringView extension( APP_DEMO_EXTENSION_STR );
	wsw::PodVector<char> path;

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeSearchResult = searchResultHolder.findDirFiles( demoDir, extension ) ) {
		unsigned numAutoDemos = 0;
		for( const wsw::StringView &fileName: *maybeSearchResult ) {
			if( isAutoRecordDemoName( fileName, extension ) ) {
				numAutoDemos++;
			}
		}
		if( numAutoDemos > maxautodemos ) {
			// TODO: In the current state we don't guarantee stability of results, so we have to redo the checks
			for( const wsw::StringView &fileName: *maybeSearchResult ) {
				if( isAutoRecordDemoName( fileName, extension ) ) {
					path.clear();
					path.append( demoDir.data(), demoDir.size() );
					path.push_back( '/' );
					path.append( fileName.data(), fileName.size() );
					svNotice() << "Removing old autorecord demo:" << path;
					path.append( '\0' );
					if( !FS_RemoveFile( path.data() ) ) {
						svNotice() << "Couldn't remove file\n" << path;
					} else {
						numAutoDemos--;
						if( !numAutoDemos ) {
							break;
						}
					}
				}
			}
		}
	}
}

#define DEMOS_PER_VIEW  30

void HandleClientCommand_Demolist( client_t *client, const CmdArgs &cmdArgs ) {
	if( client->state < CS_SPAWNED ) {
		return;
	}

	if( Cmd_Argc() > 2 ) {
		SV_AddGameCommand( client, "pr \"Usage: demolist [from-position]\n\"" );
		return;
	}

	int from = -1;
	if( Cmd_Argc() == 2 ) {
		from = atoi(Cmd_Argv( 1 ) ) - 1;
		if( from < 0 ) {
			SV_AddGameCommand( client, "pr \"Usage: demolist [from-position]\n\"" );
			return;
		}
	}

	wsw::PodVector<char> message;
	message.append( "pr \"Available demos:\n----------------\n"_asView );

	const wsw::StringView demoDir( SV_DEMO_DIR );
	const wsw::StringView extension( APP_DEMO_EXTENSION_STR );

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeSearchResult = searchResultHolder.findDirFiles( demoDir, extension ) ) {
		// Could be zero for a successful call as well (a success means there were no FS error)
		if( const auto numDemos = (int)maybeSearchResult->getNumFiles() ) {
			if( from < 0 ) {
				from = wsw::max( 0, numDemos - DEMOS_PER_VIEW );
			} else if( from > numDemos - 1 ) {
				from = numDemos - 1;
			}

			int skippedCount = 0;
			int writtenCount = 0;
			for( const wsw::StringView fileName: *maybeSearchResult ) {
				// TODO: Can something better be used?
				if( skippedCount < from ) {
					skippedCount++;
				} else if( writtenCount == DEMOS_PER_VIEW ) {
					message.append( "...\n"_asView );
					break;
				} else {
					// Should not happen?
					assert( !fileName.contains( '"' ) );
					// TODO: Don't append extension
					if( fileName.length() < MAX_STRING_CHARS ) {
						if( fileName.length() + message.size() >= MAX_STRING_CHARS ) {
							message.push_back( '"' );
							SV_AddGameCommand( client, message.data() );
							message.append( "pr \""_asView );
						}
						const wsw::StringView withoutExtension = fileName.dropRight( extension.length() );
						message.append( std::to_string( from + writtenCount + 1 ) );
						message.append( ": "_asView );
						message.append( withoutExtension.data(), withoutExtension.length() );
						message.push_back( '\n' );
						writtenCount++;
					} else {
						svWarning() << "The name of demo" << fileName << "is too long";
					}
				}
			}
		} else {
			message.append( "None\n"_asView );
		}
	} else {
		message.append( "Server demo retrieval error\n"_asView );
	}

	if( !message.empty() ) {
		message.push_back( '"' );
		message.push_back( '\0' );
		SV_AddGameCommand( client, message.data() );
	}
}

void HandleClientCommand_Demoget( client_t *client, const CmdArgs &cmdArgs ) {
	if( client->state < CS_SPAWNED ) {
		return;
	}
	if( Cmd_Argc() != 2 ) {
		return;
	}

	const wsw::StringView demoDir( SV_DEMO_DIR );
	const wsw::StringView extension( APP_DEMO_EXTENSION_STR );

	wsw::PodVector<char> message;
	message.append( wsw::StringView( "demoget \"" ) );
	message.append( demoDir.data(), demoDir.size() );
	message.push_back( '/' );

	bool found = false;

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeSearchResult = searchResultHolder.findDirFiles( demoDir, extension ) ) {
		if( const auto numDemos = (int)maybeSearchResult->getNumFiles() ) {
			const int num = wsw::clamp<int>( atoi( Cmd_Argv( 1 ) ) - 1, 0, numDemos - 1 );

			int skippedCount = 0;
			// TODO: Can we do something better
			for( const wsw::StringView &fileName: *maybeSearchResult ) {
				if( skippedCount < num ) {
					skippedCount++;
				} else {
					message.append( fileName.data(), fileName.size() );
					found = true;
					break;
				}
			}
		}
	}

	if( found ) {
		message.push_back( '"' );
		message.push_back( '\0' );
		SV_AddGameCommand( client, message.data() );
	}
}

bool SV_IsDemoDownloadRequest( const char *request ) {
	if( !request ) {
		return false;
	}

	const char *demoDir     = SV_DEMO_DIR;
	const size_t demoDirLen = strlen( demoDir );

	if( strlen( request ) <= demoDirLen + 1 + strlen( APP_DEMO_EXTENSION_STR ) ) {
		// should at least contain demo dir name and demo file extension
		return false;
	}

	if( Q_strnicmp( request, demoDir, demoDirLen ) || request[demoDirLen] != '/' ) {
		// nah, wrong dir
		return false;
	}

	const char *ext = COM_FileExtension( request );
	if( !ext || Q_stricmp( ext, APP_DEMO_EXTENSION_STR ) ) {
		// wrong extension
		return false;
	}

	return true;
}

static client_t *SV_FindPlayer( const char *s ) {
	client_t *player = nullptr;
	if( s ) {
		// Check for a number match
		if( s[0] >= '0' && s[0] <= '9' ) {
			if( const int slot = atoi( s ); slot >= 0 && slot < sv_maxclients->integer ) {
				player = &svs.clients[slot];
			}
		} else {
			// Check for a name match
			for( int i = 0; i < sv_maxclients->integer; i++ ) {
				client_t *const cl = svs.clients + i;
				if( cl->state ) {
					if( !Q_stricmp( cl->name, s ) ) {
						player = cl;
						break;
					}
				}
			}
		}
	}
	if( player ) {
		if( player->state && player->edict ) {
			return player;
		}
		Com_Printf( "Client %s is not active\n", s );
		return nullptr;
	} else {
		Com_Printf( "Failed to find player %s\n", s );
		return nullptr;
	}
}

/*
* SV_Map_f
*
* User command to change the map
* map: restart game, and start map
* devmap: restart game, enable cheats, and start map
* gamemap: just start the map
*/
static void SV_Map_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		Com_Printf( "Usage: %s <map>\n", Cmd_Argv( 0 ) );
		return;
	}

	const char *map;
	// if map "<map>" is used Cmd_Args() will return the "" as well.
	if( Cmd_Argc() == 2 ) {
		map = Cmd_Argv( 1 );
	} else {
		map = Cmd_Args();
	}

	Com_DPrintf( "SV_GameMap(%s)\n", map );

	// applies to fullnames and filenames (whereas + strlen( "maps/" ) wouldnt)
	if( strlen( map ) >= MAX_QPATH ) {
		Com_Printf( "Map name too long.\n" );
		return;
	}

	bool found = false;
	char mapname[MAX_QPATH];
	Q_strncpyz( mapname, map, sizeof( mapname ) );
	if( ML_ValidateFilename( mapname ) ) {
		COM_StripExtension( mapname );
		if( ML_FilenameExists( mapname ) ) {
			found = true;
		} else {
			ML_Update();
			if( ML_FilenameExists( mapname ) ) {
				found = true;
			}
		}
	}

	if( !found ) {
		if( ML_ValidateFullname( map ) ) {
			Q_strncpyz( mapname, ML_GetFilename( map ), sizeof( mapname ) );
			if( *mapname ) {
				found = true;
			}
		}
	}

	if( !found ) {
		Com_Printf( "Couldn't find map: %s\n", map );
	} else {
		if( FS_GetNotifications() & FS_NOTIFY_NEWPAKS ) {
			FS_RemoveNotifications( FS_NOTIFY_NEWPAKS );
			sv.state = ss_dead; // don't save current level when changing
		} else if( !Q_stricmp( Cmd_Argv( 0 ), "map" ) || !Q_stricmp( Cmd_Argv( 0 ), "devmap" ) ) {
			sv.state = ss_dead; // don't save current level when changing
		}

		SV_UpdateInfoServers();

		// start up the next map
		SV_Map( mapname, !Q_stricmp( Cmd_Argv( 0 ), "devmap" ) );

		// archive server state
		Q_strncpyz( svs.mapcmd, mapname, sizeof( svs.mapcmd ) );
	}
}

static void SV_Heartbeat_f( const CmdArgs & ) {
	svc.nextHeartbeat = Sys_Milliseconds();
}

static void SV_Serverinfo_f( const CmdArgs & ) {
	Com_Printf( "Server info settings:\n" );
	Info_Print( Cvar_Serverinfo() );
}

static void SV_DumpUser_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() != 2 ) {
		Com_Printf( "Usage: info <userid>\n" );
	} else {
		if( client_t *client = SV_FindPlayer( Cmd_Argv( 1 ) ) ) {
			Com_Printf( "userinfo\n" );
			Com_Printf( "--------\n" );
			Info_Print( client->userinfo );
		}
	}
}

static void SV_KillServer_f( const CmdArgs & ) {
	if( svs.initialized ) {
		SV_ShutdownGame( "Server was killed", false );
	}
}

static void SV_CvarCheck_f( const CmdArgs &cmdArgs ) {
	if( !svs.initialized ) {
		return;
	}

	if( Cmd_Argc() != 3 ) {
		Com_Printf( "Usage: cvarcheck <userid> <cvar name>\n" );
		return;
	}

	if( !Q_stricmp( Cmd_Argv( 1 ), "all" ) ) {
		IteratorOverClients iteratorOverClients( { .includeFakeClients = false } );
		while( client_t *const client = iteratorOverClients.getNext() ) {
			SV_SendServerCommand( client, "cvarinfo \"%s\"", Cmd_Argv( 2 ) );
		}
	} else {
		if( client_t *const client = SV_FindPlayer( Cmd_Argv( 1 ) ) ) {
			SV_SendServerCommand( client, "cvarinfo \"%s\"", Cmd_Argv( 2 ) );
		} else {
			Com_Printf( "%s is not valid client id\n", Cmd_Argv( 1 ) );
		}
	}
}

void SV_InitOperatorCommands() {
	SV_Cmd_Register( "heartbeat"_asView, SV_Heartbeat_f );
	SV_Cmd_Register( "serverinfo"_asView, SV_Serverinfo_f );
	SV_Cmd_Register( "dumpuser"_asView, SV_DumpUser_f );

	SV_Cmd_Register( "map"_asView, SV_Map_f, ML_CompleteBuildList );
	SV_Cmd_Register( "devmap"_asView, SV_Map_f, ML_CompleteBuildList );
	SV_Cmd_Register( "gamemap"_asView, SV_Map_f, ML_CompleteBuildList );
	SV_Cmd_Register( "killserver"_asView, SV_KillServer_f );

	SV_Cmd_Register( "serverrecord"_asView, SV_Demo_Start_f );
	SV_Cmd_Register( "serverrecordstop"_asView, SV_Demo_Stop_f );
	SV_Cmd_Register( "serverrecordcancel"_asView, SV_Demo_Cancel_f );
	SV_Cmd_Register( "serverrecordpurge"_asView, SV_Demo_Purge_f );

	SV_Cmd_Register( "purelist"_asView, SV_PureList_f );

	SV_Cmd_Register( "cvarcheck"_asView, SV_CvarCheck_f );
}

void SV_ShutdownOperatorCommands() {
	SV_Cmd_Unregister( "heartbeat"_asView );
	SV_Cmd_Unregister( "serverinfo"_asView );
	SV_Cmd_Unregister( "dumpuser"_asView );

	SV_Cmd_Unregister( "map"_asView );
	SV_Cmd_Unregister( "devmap"_asView );
	SV_Cmd_Unregister( "gamemap"_asView );
	SV_Cmd_Unregister( "killserver"_asView );

	SV_Cmd_Unregister( "serverrecord"_asView );
	SV_Cmd_Unregister( "serverrecordstop"_asView );
	SV_Cmd_Unregister( "serverrecordcancel"_asView );
	SV_Cmd_Unregister( "serverrecordpurge"_asView );

	SV_Cmd_Unregister( "purelist"_asView );

	SV_Cmd_Unregister( "cvarcheck"_asView );
}

void SV_MOTD_SetMOTD( const char *motd ) {
	const size_t l = wsw::min( strlen( motd ), (size_t)1024 );
	if( l == 0 ) {
		if( svs.motd ) {
			Q_free( svs.motd );
		}
		svs.motd = NULL;
	} else {
		if( svs.motd ) {
			Q_free( svs.motd );
		}

		svs.motd = (char *)Q_malloc( ( l + 1 ) * sizeof( char ) );
		Q_strncpyz( svs.motd, motd, l + 1 );

		char *pos;
		// MOTD may come from a CRLF file so strip off \r's (we waste a few bytes here)
		while( ( pos = strchr( svs.motd, '\r' ) ) != NULL ) {
			memmove( pos, pos + 1, strlen( pos + 1 ) + 1 );
		}
	}
}

void SV_MOTD_LoadFromFile() {
	char *f = nullptr;

	FS_LoadFile( sv_MOTDFile->string, (void **)&f, nullptr, 0 );
	if( !f ) {
		Com_Printf( "Couldn't load MOTD file: %s\n", sv_MOTDFile->string );
		Cvar_ForceSet( "sv_MOTDFile", "" );
		SV_MOTD_SetMOTD( "" );
	} else {
		if( strchr( f, '"' ) ) { // FIXME: others?
			Com_Printf( "Warning: MOTD file contains illegal characters.\n" );
			Cvar_ForceSet( "sv_MOTDFile", "" );
			SV_MOTD_SetMOTD( "" );
		} else {
			SV_MOTD_SetMOTD( f );
		}

		FS_FreeFile( f );
	}
}

void SV_MOTD_Update() {
	if( sv_MOTD->integer ) {
		if( sv_MOTDString->string[0] ) {
			SV_MOTD_SetMOTD( sv_MOTDString->string );
		} else if( sv_MOTDFile->string[0] ) {
			SV_MOTD_LoadFromFile();
		} else {
			SV_MOTD_SetMOTD( "" );
		}
	}
}

void HandleClientCommand_Motd( client_t *client, const CmdArgs &cmdArgs ) {
	if( sv_MOTD->integer && svs.motd && svs.motd[0] ) {
		const int flag = ( Cmd_Argc() > 1 ? 1 : 0 );
		SV_AddGameCommand( client, va( "motd %d \"%s\n\"", flag, svs.motd ) );
	}
}

class SVCmdSystem: public CmdSystem {
	void registerSystemCommands() override {
		checkCallingThread();
		registerCommand( "exec"_asView, handlerOfExec );
		registerCommand( "echo"_asView, handlerOfEcho );
		registerCommand( "alias"_asView, handlerOfAlias );
		registerCommand( "aliasa"_asView, handlerOfAliasa );
		registerCommand( "unalias"_asView, handlerOfUnalias );
		registerCommand( "unaliasall"_asView, handlerOfUnaliasall );
		registerCommand( "wait"_asView, handlerOfWait );
		registerCommand( "vstr"_asView, handlerOfVstr );
	}

	static void handlerOfExec( const CmdArgs & );
	static void handlerOfEcho( const CmdArgs & );
	static void handlerOfAlias( const CmdArgs & );
	static void handlerOfAliasa( const CmdArgs & );
	static void handlerOfUnalias( const CmdArgs & );
	static void handlerOfUnaliasall( const CmdArgs & );
	static void handlerOfWait( const CmdArgs & );
	static void handlerOfVstr( const CmdArgs & );
};

static SingletonHolder<SVCmdSystem> g_svCmdSystemHolder;

void SVCmdSystem::handlerOfExec( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfExec( cmdArgs );
}

void SVCmdSystem::handlerOfEcho( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfEcho( cmdArgs );
}

void SVCmdSystem::handlerOfAlias( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfAlias( false, cmdArgs );
}

void SVCmdSystem::handlerOfAliasa( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfAlias( true, cmdArgs );
}

void SVCmdSystem::handlerOfUnalias( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfUnalias( cmdArgs );
}

void SVCmdSystem::handlerOfUnaliasall( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfUnaliasall( cmdArgs );
}

void SVCmdSystem::handlerOfWait( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfWait( cmdArgs );
}

void SVCmdSystem::handlerOfVstr( const CmdArgs &cmdArgs ) {
	g_svCmdSystemHolder.instance()->helperForHandlerOfVstr( cmdArgs );
}

void SV_InitCmdSystem() {
	g_svCmdSystemHolder.init();
}

CmdSystem *SV_GetCmdSystem() {
	return g_svCmdSystemHolder.instance();
}

void SV_ShutdownCmdSystem() {
	g_svCmdSystemHolder.shutdown();
}

void SV_Cmd_ExecuteText( int when, const char *text ) {
	switch( when ) {
		case EXEC_NOW:
			g_svCmdSystemHolder.instance()->executeNow( wsw::StringView( text ) );
			break;
		case EXEC_APPEND:
			g_svCmdSystemHolder.instance()->appendCommand( wsw::StringView( text ) );
			break;
		case EXEC_INSERT:
			g_svCmdSystemHolder.instance()->prependCommand( wsw::StringView( text ) );
			break;
		default:
			Sys_Error( "Illegal EXEC_WHEN code" );
	}
}

void SV_Cmd_ExecuteNow( const char *text ) {
	g_svCmdSystemHolder.instance()->executeNow( wsw::StringView( text ) );
}

void SV_Cmd_ExecuteNow2( const wsw::PodVector<char> &text ) {
	g_svCmdSystemHolder.instance()->executeNow( wsw::StringView( text.data(), text.size() ) );
}

void SV_Cbuf_AppendCommand( const char *text ) {
	g_svCmdSystemHolder.instance()->appendCommand( wsw::StringView( text ) );
}

void SV_Cbuf_ExecutePendingCommands() {
	g_svCmdSystemHolder.instance()->executeBufferCommands();
}