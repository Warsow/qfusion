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

#include "serverlist.h"
#include <common/links.h>
#include <common/singletonholder.h>
#include "client.h"

#include <atomic>
#include <cinttypes>
#include <cstdlib>
#include <limits>

uint32_t NET_AddressHash( const netadr_t & );

static qmutex_t *resolverMutex;
// An additional helper for the resolver thread
static std::atomic<bool> initialized;
static std::atomic<int> numActiveResolvers;
static SingletonHolder<ServerList> serverListHolder;

void ServerList::init() {
	serverListHolder.init();
	initialized = true;

	const char *infoServersStr = Cvar_String( "infoservers" );
	if( !*infoServersStr ) {
		return;
	}

	int numInfoServers = 0;
	for( const char *ptr = infoServersStr; ptr; ) {
		char *infoAddress = COM_Parse( &ptr );
		if( !*infoAddress ) {
			break;
		}
		numInfoServers++;
	}

	if( !( resolverMutex = QMutex_Create() ) ) {
		return;
	}

	// Set this first as some resolvers may return almost immediately
	::numActiveResolvers = wsw::min( (int)kMaxInfoServers, numInfoServers );

	int numSpawnedResolvers = 0;
	for( const char *ptr = infoServersStr; ptr; ) {
		if( numSpawnedResolvers == kMaxInfoServers ) {
			break;
		}
		char *addressString = COM_Parse( &ptr );
		if( !*addressString ) {
			break;
		}
		size_t len = ::strlen( addressString );
		char *s = new char[len + 1];
		memcpy( s, addressString, len );
		s[len] = '\0';
		QThread_Create( &ServerList::resolverThreadFunc, s );
		numSpawnedResolvers++;
	}
}

void ServerList::shutdown() {
	initialized = false;
	serverListHolder.shutdown();
	// The mutex is not disposed intentionally
}

auto ServerList::instance() -> ServerList * {
	return serverListHolder.instance();
}

void *ServerList::resolverThreadFunc( void *param ) {
	const char *string = (char *)param;

	bool resolved = false;
	netadr_t address;
	NET_StringToAddress( string, &address );
	if( address.type == NA_IP || address.type == NA_IP6 ) {
		resolved = true;
		if( NET_GetAddressPort( &address ) == 0 ) {
			NET_SetAddressPort( &address, PORT_INFO_SERVER );
		}
	}

	// Decrement number of active resolvers regardless of resolution status
	int numResolversLeft = 9999;
	QMutex_Lock( resolverMutex );
	if( initialized ) {
		if( resolved ) {
			instance()->addInfoServer( address );
		}
		numResolversLeft = numActiveResolvers.fetch_sub( 1 );
	}
	QMutex_Unlock( resolverMutex );

	// Destroy the mutex in this case regardless of resolution status (as we no longer need it)
	if( !numResolversLeft ) {
		QMutex_Destroy( &resolverMutex );
	}

	// We held the string for printing it in this case
	if( !resolved ) {
		clWarning() << "Failed to resolve info server address" << wsw::StringView( string );
	}

	delete[] string;
	return nullptr;
}

void ServerList::parseGetServersResponse( const socket_t *socket, const netadr_t &address, msg_t *msg ) {
	if( !m_listener ) {
		return;
	}

	// TODO: Check whether the packet came from an actual info server server
	// TODO: Is it possible at all? (We're talking about UDP packets).

	MSG_BeginReading( msg );
	(void)MSG_ReadInt32( msg );

	static const auto prefixLen = sizeof( "getserversResponse" ) - 1;
	if( !MSG_SkipData( msg, prefixLen ) ) {
		return;
	}

	for(;; ) {
		if( !MSG_BytesLeft( msg ) ) {
			return;
		}

		netadr_t readAddress;
		int numAddressBytes;
		netadrtype_t addressType;
		uint8_t *destBytes;
		uint16_t *destPort;

		const char startPrefix = (char)MSG_ReadInt8( msg );
		if( startPrefix == '\\' ) {
			numAddressBytes = 4;
			addressType = NA_IP;
			destBytes = readAddress.address.ipv4.ip;
			destPort = &readAddress.address.ipv4.port;
		} else if( startPrefix == '/' ) {
			numAddressBytes = 16;
			addressType = NA_IP6;
			destBytes = readAddress.address.ipv6.ip;
			destPort = &readAddress.address.ipv6.port;
		} else {
			clWarning() << "Illegal address prefix" << startPrefix;
			return;
		}

		if( MSG_BytesLeft( msg ) < numAddressBytes + 2 ) {
			clWarning() << "Too few bytes in message for an address";
			return;
		}

		const uint8_t *addressBytes = msg->data + msg->readcount;
		const uint8_t *portBytes = addressBytes + numAddressBytes;

		// Stop parsing on a zero port. Its weird but that's what actual engine sources do.
		// Note: the comment in the old code says "both endians need this swapped"
		const uint16_t port = ( (uint16_t)portBytes[1] << 8 ) | portBytes[0];
		if( !( portBytes[0] | portBytes[1] ) ) {
			return;
		}

		NET_InitAddress( &readAddress, addressType );
		::memcpy( destBytes, addressBytes, numAddressBytes );
		*destPort = port;
		onServerAddressReceived( readAddress );
		MSG_SkipData( msg, numAddressBytes + 2 );
	}
}

void ServerList::parseGetInfoResponse( const socket_t *socket, const netadr_t &address, msg_t *msg ) {
	if( !m_listener ) {
		return;
	}

	PolledGameServer *const server = findServerByAddress( address );
	if( !server ) {
		// Be silent in this case, it can legally occur if a server times out and a packet arrives then
		return;
	}

	ServerInfo *const parsedServerInfo = parseServerInfo( msg, server );
	if( !parsedServerInfo ) {
		return;
	}

	if( MSG_BytesLeft( msg ) > 0 ) {
		clWarning() << "Extra bytes in getinfo response, dropping the info";
		delete parsedServerInfo;
		return;
	}

	onNewServerInfo( server, parsedServerInfo );
}

auto ServerList::parseServerInfo( msg_t *msg, PolledGameServer *server ) -> ServerInfo * {
	auto *const info = new ServerInfo;
	if( m_serverInfoParser->parse( msg, info, server->m_lastAcknowledgedChallenge ) ) {
		server->m_lastAcknowledgedChallenge = m_serverInfoParser->getParsedChallenge();
		return info;
	}

	delete info;
	return nullptr;
}

void ServerList::parseGetStatusResponse( const socket_t *socket, const netadr_t &address, msg_t *msg ) {
	if( !m_listener ) {
		return;
	}

	PolledGameServer *const server = findServerByAddress( address );
	if( !server ) {
		return;
	}

	ServerInfo *const parsedServerInfo = parseServerInfo( msg, server );
	if( !parsedServerInfo ) {
		return;
	}

	// ParsePlayerInfo() returns a null pointer if there is no clients.
	// Avoid qualifying this case as a parsing failure, do an actual parsing only if there are clients.
	if( parsedServerInfo->numClients ) {
		if( !( parsePlayerInfo( msg, parsedServerInfo ) ) ) {
			delete parsedServerInfo;
			return;
		}
	}

	onNewServerInfo( server, parsedServerInfo );
}

// TODO: Generalize and lift to Links.h
static auto LinkToTail( PlayerInfo *item, PlayerInfo **listTailRef ) -> PlayerInfo * {
	if( *listTailRef ) {
		( *listTailRef )->next = item;
	}
	item->next = nullptr;
	item->prev = *listTailRef;
	*listTailRef = item;
	return item;
}

bool ServerList::parsePlayerInfo( msg_t *msg_, ServerInfo *serverInfo ) {
	const char *chars = (const char *)( msg_->data + msg_->readcount );

	const unsigned currSize = msg_->cursize;
	const unsigned readCount = msg_->readcount;
	assert( currSize >= readCount );
	unsigned bytesLeft = currSize - readCount;
	const char *s = chars;

	PlayerInfo *listTail[4] = { nullptr, nullptr, nullptr, nullptr };

	// Skip '\n' at the beginning (if any)
	if( *s == '\n' ) {
		s++;
	}

	int score = 0, ping = 0, team = 0;
	char *endptr = nullptr;

	for(;; ) {
		if( s - chars >= bytesLeft ) {
			break;
		}

		if( *s == '\n' ) {
			break;
		}

		if( !scanInt( s, &endptr, &score ) ) {
			return false;
		}
		s = endptr + 1;

		if( s - chars >= bytesLeft ) {
			return false;
		}

		if( !scanInt( s, &endptr, &ping ) ) {
			return false;
		}
		s = endptr + 1;

		if( s - chars >= bytesLeft ) {
			return false;
		}

		if( *s != '"' ) {
			return false;
		}
		s++;

		const auto nameStart = (unsigned)( s - chars );
		unsigned nameLength = 0;
		for( ;; ) {
			if( s - chars >= bytesLeft ) {
				return false;
			}

			if( *s == '"' ) {
				nameLength = (unsigned)( s - chars ) - nameStart;
				break;
			}
			s++;
		}

		static_assert( sizeof( PlayerInfo::name ) < std::numeric_limits<uint8_t>::max() );

		if( nameLength >= sizeof( PlayerInfo::name ) ) {
			return false;
		}
		s++;

		if( s - chars >= bytesLeft ) {
			return false;
		}

		if( !scanInt( s, &endptr, &team ) ) {
			return false;
		}

		if( (unsigned)team > 3u ) {
			return false;
		}

		s = endptr;
		if( *s != '\n' ) {
			return false;
		}

		auto *playerInfo = new PlayerInfo;
		playerInfo->score = score;
		playerInfo->name.assign( chars + nameStart, nameLength );
		playerInfo->ping = ping;
		playerInfo->team = team;

		if( !serverInfo->teamInfoHeads[team] ) {
			serverInfo->teamInfoHeads[team] = playerInfo;
			listTail[team] = playerInfo;
		} else {
			::LinkToTail( playerInfo, &listTail[team] );
		}

		serverInfo->numTeamPlayers[team]++;
		s++;
	}

	return true;
}

auto ServerList::findServerByAddress( const netadr_t &address ) -> PolledGameServer * {
	return findServerByAddress( address, ::NET_AddressHash( address ) % kNumHashBins );
}

auto ServerList::findServerByAddress( const netadr_t &address, unsigned binIndex ) -> PolledGameServer * {
	for( PolledGameServer *server = m_serversHashBins[binIndex]; server; server = server->nextInBin() ) {
		if( NET_CompareAddress( &server->m_networkAddress, &address ) ) {
			return server;
		}
	}
	return nullptr;
}

void ServerList::onServerAddressReceived( const netadr_t &address ) {
	const uint32_t hash = NET_AddressHash( address );
	const auto binIndex = hash % kNumHashBins;
	if( findServerByAddress( address, binIndex ) ) {
		// TODO: Touch the found server?
		return;
	}

	auto *const server = new PolledGameServer;
	server->m_networkAddress = address;
	wsw::link( server, &m_serversHead, PolledGameServer::LIST_LINKS );
	server->m_addressHash = hash;
	server->m_hashBinIndex = binIndex;
	wsw::link( server, &m_serversHashBins[binIndex], PolledGameServer::BIN_LINKS );
}

ServerList::ServerList() {
	memset( m_serversHashBins, 0, sizeof( m_serversHashBins ) );
	this->m_serverInfoParser = new ServerInfoParser;
}

ServerList::~ServerList() {
	clearState();
	delete m_serverInfoParser;
}

void ServerList::clearState() {
	m_lastInfoServersPollAt = 0;
	m_lastInfoServerIndex = 0;

	PolledGameServer *nextServer;
	for( PolledGameServer *server = m_serversHead; server; server = nextServer ) {
		nextServer = server->nextInList();
		const auto binIndex = NET_AddressHash( server->getAddress() ) % kNumHashBins;
		wsw::unlink( server, &m_serversHashBins[binIndex], PolledGameServer::BIN_LINKS );
		delete server;
	}

	std::fill( std::begin( m_serversHashBins ), std::end( m_serversHashBins ), nullptr );
	m_serversHead = nullptr;
}

void ServerList::frame() {
	if( !m_listener ) {
		return;
	}

	dropTimedOutServers();

	emitPollInfoServersPackets();
	emitPollGameServersPackets();
}

void ServerList::startPushingUpdates( ServerListListener *listener, bool showEmptyServers, bool showFullServers ) {
	assert( listener );
	clearState();

	this->m_listener = listener;
	this->m_showEmptyServers = showEmptyServers;
	this->m_showFullServers = showFullServers;
}

void ServerList::stopPushingUpdates() {
	clearState();

	this->m_listener = nullptr;
}

void ServerList::emitPollInfoServersPackets() {
	const auto millisNow = Sys_Milliseconds();

	if( millisNow - m_lastInfoServersPollAt < 1500 ) {
		return;
	}

	// Make the warning affected by the timer too (do not spam in console way too often), do not return prematurely
	if( m_numInfoServers ) {
		m_lastInfoServerIndex = ( m_lastInfoServerIndex + 1 ) % m_numInfoServers;
		sendPollInfoServerPacket( m_infoServers[m_lastInfoServerIndex] );
	}

	m_lastInfoServersPollAt = millisNow;
}

void ServerList::emitPollGameServersPackets() {
	const auto millisNow = Sys_Milliseconds();
	for( PolledGameServer *server = m_serversHead; server; server = server->nextInList() ) {
		if( millisNow - server->m_lastInfoRequestSentAt < 750 ) {
			continue;
		}
		sendPollGameServerPacket( server );
		server->m_lastInfoRequestSentAt = millisNow;
	}
}

void ServerList::dropTimedOutServers() {
	const auto millisNow = Sys_Milliseconds();
	PolledGameServer *nextServer;
	for( PolledGameServer *server = m_serversHead; server; server = nextServer ) {
		nextServer = server->nextInList();
		if( millisNow - server->m_lastInfoRequestSentAt < 1000 ) {
			// Wait for the first info received...
			if( server->m_lastInfoReceivedAt && millisNow - server->m_lastInfoReceivedAt > 5000 ) {
				dropServer( server );
			}
		}
	}
}

void ServerList::dropServer( PolledGameServer *server ) {
	m_listener->onServerRemoved( server );
	wsw::unlink( server, &m_serversHead, PolledGameServer::LIST_LINKS );
	wsw::unlink( server, &m_serversHashBins[server->m_hashBinIndex], PolledGameServer::BIN_LINKS );
	delete server;
}

void ServerList::sendPollInfoServerPacket( const netadr_t &address ) {
	socket_t *const socket = ( address.type == NA_IP ) ? &cls.socket_udp : &cls.socket_udp6;
	const char *const full = m_showFullServers ? "full" : "";
	const char *const empty = m_showEmptyServers ? "empty" : "";
	Netchan_OutOfBandPrint( socket, &address, "getservers Warsow %d %s %s", 22, full, empty );
}

void ServerList::sendPollGameServerPacket( PolledGameServer *server ) {
	uint64_t challenge = Sys_Milliseconds();
	socket_t *socket = ( server->m_networkAddress.type == NA_IP ) ? &cls.socket_udp : &cls.socket_udp6;
	if( m_showPlayerInfo ) {
		Netchan_OutOfBandPrint( socket, &server->m_networkAddress, "getstatus %" PRIu64, challenge );
	} else {
		Netchan_OutOfBandPrint( socket, &server->m_networkAddress, "getinfo %" PRIu64, challenge );
	}
}

void ServerList::onNewServerInfo( PolledGameServer *server, ServerInfo *newServerInfo ) {
	if( server->m_oldInfo ) {
		assert( server->m_currInfo && "The current info must be present if the old one is" );
		delete server->m_oldInfo;
	}

	server->m_oldInfo = server->m_currInfo;
	server->m_currInfo = newServerInfo;
	server->m_lastInfoReceivedAt = Sys_Milliseconds();

	if( !newServerInfo->matchesOld( server->m_oldInfo ) ) {
		if( server->m_oldInfo ) {
			m_listener->onServerUpdated( server );
		} else {
			// Defer server addition until a first info arrives.
			// Otherwise there is just nothing to show in a server browser.
			// If there is no old info, the listener has not been notified about a new server yet.
			m_listener->onServerAdded( server );
		}
	}
}

void MatchTime::clear() {
	memset( this, 0, sizeof( MatchTime ) );
}

bool MatchTime::operator==( const MatchTime &that ) const {
	return !memcmp( this, &that, sizeof( MatchTime ) );
}

void MatchScore::clear() {
	scores[0].clear();
	scores[1].clear();
}

bool MatchScore::operator==( const MatchScore &that ) const {
	// Its better to do integer comparisons first, thats why there are no individual TeamScore::Equals() methods
	for( int i = 0; i < 2; ++i ) {
		if( this->scores[i].score != that.scores[i].score ) {
			return false;
		}
	}

	for( int i = 0; i < 2; ++i ) {
		if( this->scores[i].name != that.scores[i].name ) {
			return false;
		}
	}
	return true;
}

bool PlayerInfo::operator==( const PlayerInfo &that ) const {
	// Do these cheap comparisons first
	if( this->score != that.score || this->ping != that.ping || this->team != that.team ) {
		return false;
	}
	return this->name == that.name;
}

void ServerInfo::clearPlayerInfo( PlayerInfo *infoHead ) {
	PlayerInfo *nextInfo;
	for( PlayerInfo *info = infoHead; info; info = nextInfo ) {
		nextInfo = info->next;
		delete info;
	}
}

ServerInfo::~ServerInfo() {
	for( auto *infoHead: teamInfoHeads ) {
		clearPlayerInfo( infoHead );
	}
}

bool ServerInfo::comparePlayersList( const PlayerInfo *list1, const PlayerInfo *list2 ) {
	for(;; ) {
		if( !list1 ) {
			return !list2;
		}
		if( !list2 ) {
			return false;
		}
		if( *list1 != *list2 ) {
			return false;
		}

		list1 = list1->next;
		list2 = list2->next;
	}
}

bool ServerInfo::matchesOld( ServerInfo *oldInfo ) {
	if( !oldInfo ) {
		return false;
	}

	// Test fields that are likely to change often first

	if( this->time != oldInfo->time ) {
		return false;
	}

	if( this->numClients != oldInfo->numClients ) {
		return false;
	}

	for( int i = 0; i < 4; ++i ) {
		if( this->numTeamPlayers[i] != oldInfo->numTeamPlayers[i] ) {
			return false;
		}
		if( !comparePlayersList( teamInfoHeads[i], oldInfo->teamInfoHeads[i] ) ) {
			return false;
		}
	}

	if( this->score != oldInfo->score ) {
		return false;
	}

	if( mapname != oldInfo->mapname ) {
		return false;
	}

	if( gametype != oldInfo->gametype ) {
		return false;
	}

	if( this->numBots != oldInfo->numBots ) {
		return false;
	}

	// Never changes until server restart

	if( serverName != oldInfo->serverName ) {
		return false;
	}

	return this->maxClients == oldInfo->maxClients && this->needPassword == oldInfo->needPassword;
}