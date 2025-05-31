#ifndef WSW_SERVERLIST_H
#define WSW_SERVERLIST_H

#include <common/common.h>
#include <common/wswstaticstring.h>
#include <common/wswstaticvector.h>
#include "serverinfoparser.h"

class PlayerInfo {
public:
	wsw::StaticString<32> name;
	PlayerInfo *prev { nullptr };
	PlayerInfo *next { nullptr };
	int score { 0 };
	int ping { 0 };
	int team { 0 };

	bool operator==( const PlayerInfo &that ) const;
	bool operator!=( const PlayerInfo &that ) const {
		return !( *this == that );
	}

	[[nodiscard]]
	auto getName() const -> wsw::StringView { return name.asView(); }
	[[nodiscard]]
	auto getPing() const -> int { return ping; }
	[[nodiscard]]
	auto getScore() const -> int { return score; }
};

struct MatchTime {
	int timeMinutes;
	int limitMinutes;
	int timeSeconds;
	int limitSeconds;
	bool isWarmup : 1;
	bool isCountdown : 1;
	bool isFinished : 1;
	bool isOvertime : 1;
	bool isSuddenDeath : 1;
	bool isTimeout : 1;

	MatchTime() {
		clear();
	}

	void clear();
	bool operator==( const MatchTime &that ) const;
	bool operator!=( const MatchTime &that ) const {
		return !( *this == that );
	}
};

struct MatchScore {
	struct TeamScore {
		int score { 0 };
		wsw::StaticString<32> name;

		void clear() {
			score = 0;
			name.clear();
		}
	};

	TeamScore scores[2];

	[[nodiscard]]
	const TeamScore &getAlphaScore() const { return scores[0]; }

	[[nodiscard]]
	const TeamScore &getBetaScore() const { return scores[1]; }

	void clear();
	bool operator==( const MatchScore &that ) const;
	bool operator!=( const MatchScore &that ) const {
		return ( *this == that );
	}
};

class ServerInfo {
	static void clearPlayerInfo( PlayerInfo *infoHead );
	[[nodiscard]]
	static bool comparePlayersList( const PlayerInfo *list1, const PlayerInfo *list2 );
public:
	wsw::StaticString<64> serverName;
	wsw::StaticString<32> gametype;
	wsw::StaticString<32> mapname;

	~ServerInfo();

	PlayerInfo *teamInfoHeads[4] { nullptr, nullptr, nullptr, nullptr };
	int numTeamPlayers[4] { 0, 0, 0, 0 };

	MatchTime time;
	MatchScore score;

	uint8_t maxClients { 0 };
	uint8_t numClients { 0 };
	uint8_t numBots { 0 };

	bool needPassword { false };

	bool matchesOld( ServerInfo *oldInfo );

	[[nodiscard]]
	auto getPlayersListForTeam( int team ) const -> std::pair<const PlayerInfo *, int> {
		assert( (unsigned)team <= 3u );
		return std::make_pair( teamInfoHeads[team], numTeamPlayers[team] );
	}
};

namespace wsw {
template <typename T> auto link( T *, T **, int ) -> T *;
template <typename T> auto unlink( T *, T **, int ) -> T *;
}

class PolledGameServer {
	friend class ServerList;
	template <typename T> friend auto wsw::link( T *, T **, int ) -> T *;
	template <typename T> friend auto wsw::unlink( T *, T **, int ) -> T *;

	enum { LIST_LINKS, BIN_LINKS };
	PolledGameServer *prev[2] { nullptr, nullptr };
	PolledGameServer *next[2] { nullptr, nullptr };

	PolledGameServer *nextInBin() { return next[BIN_LINKS]; }
	PolledGameServer *nextInList() { return next[LIST_LINKS]; }

	uint32_t m_addressHash { 0 };
	unsigned m_hashBinIndex { 0 };
	netadr_t m_networkAddress {};

	ServerInfo *m_currInfo { nullptr };
	ServerInfo *m_oldInfo { nullptr };

	int64_t m_lastInfoRequestSentAt { 0 };
	int64_t m_lastInfoReceivedAt { 0 };

	uint64_t m_lastAcknowledgedChallenge { 0 };

	unsigned m_instanceId { 0 };

	[[nodiscard]]
	auto getCheckedInfo() const -> const ServerInfo * {
		assert( m_currInfo );
		return m_currInfo;
	}

public:
	~PolledGameServer() {
		delete m_currInfo;
		delete m_oldInfo;
	}

	[[nodiscard]]
	auto getInstanceId() const -> unsigned { return m_instanceId; }

	[[nodiscard]]
	auto getAddress() const -> const netadr_t & { return m_networkAddress; }

	[[nodiscard]]
	auto getServerName() const -> wsw::StringView { return getCheckedInfo()->serverName.asView(); }

	[[nodiscard]]
	auto getGametype() const -> wsw::StringView { return getCheckedInfo()->gametype.asView(); }

	[[nodiscard]]
	auto getMapName() const -> wsw::StringView { return getCheckedInfo()->mapname.asView(); }

	[[nodiscard]]
	auto getTime() const -> const MatchTime & { return getCheckedInfo()->time; }

	[[nodiscard]]
	auto getAlphaName() const -> wsw::StringView { return getCheckedInfo()->score.getAlphaScore().name.asView(); }

	[[nodiscard]]
	auto getBetaName() const -> wsw::StringView { return getCheckedInfo()->score.getBetaScore().name.asView(); }

	[[nodiscard]]
	auto getAlphaScore() const -> int { return getCheckedInfo()->score.getAlphaScore().score; }

	[[nodiscard]]
	auto getBetaScore() const -> int { return getCheckedInfo()->score.getBetaScore().score; }

	[[nodiscard]]
	auto getMaxClients() const -> int { return getCheckedInfo()->maxClients; }
	[[nodiscard]]
	auto getNumClients() const -> int { return getCheckedInfo()->numClients; }
	[[nodiscard]]
	auto getNumBots() const -> int { return getCheckedInfo()->numBots; }

	[[nodiscard]]
	bool needPassword() const { return getCheckedInfo()->needPassword; }

	[[nodiscard]]
	auto getSpectators() const -> std::pair<const PlayerInfo *, int> {
		return getCheckedInfo()->getPlayersListForTeam( 0 );
	}
	[[nodiscard]]
	auto getPlayersTeam() const -> std::pair<const PlayerInfo *, int> {
		return getCheckedInfo()->getPlayersListForTeam( 1 );
	}
	[[nodiscard]]
	auto getAlphaTeam() const -> std::pair<const PlayerInfo *, int> {
		return getCheckedInfo()->getPlayersListForTeam( 2 );
	}
	[[nodiscard]]
	auto getBetaTeam() const -> std::pair<const PlayerInfo *, int> {
		return getCheckedInfo()->getPlayersListForTeam( 3 );
	}
};

class ServerListListener {
public:
	virtual ~ServerListListener() = default;

	virtual void onServerAdded( const PolledGameServer *server ) = 0;
	virtual void onServerRemoved( const PolledGameServer *server ) = 0;
	virtual void onServerUpdated( const PolledGameServer *server ) = 0;
};

class ServerInfoParser;

class ServerList {
	template <typename> friend class SingletonHolder;

	ServerListListener *m_listener { nullptr };

	PolledGameServer *m_serversHead { nullptr };

	static constexpr unsigned kNumHashBins = 97;
	PolledGameServer *m_serversHashBins[kNumHashBins];

	static constexpr unsigned kMaxInfoServers = 4;
	netadr_t m_infoServers[kMaxInfoServers];

	unsigned m_numInfoServers { 0 };

	int64_t m_lastInfoServersPollAt { 0 };
	unsigned m_lastInfoServerIndex { 0 };

	bool m_showEmptyServers { false };
	bool m_showFullServers { true };
	bool m_showPlayerInfo { true };

	void onNewServerInfo( PolledGameServer *server, ServerInfo *parsedServerInfo );

	ServerInfoParser *m_serverInfoParser;

	[[nodiscard]]
	auto parseServerInfo( msg_t *msg, PolledGameServer *server ) -> ServerInfo *;
	[[nodiscard]]
	bool parsePlayerInfo( msg_t *msg, ServerInfo *serverInfo );

	[[nodiscard]]
	auto findServerByAddress( const netadr_t &address ) -> PolledGameServer *;
	[[nodiscard]]
	auto findServerByAddress( const netadr_t &address, unsigned binIndex ) -> PolledGameServer *;

	void emitPollInfoServersPackets();
	void sendPollInfoServerPacket( const netadr_t &address );
	void emitPollGameServersPackets();
	void sendPollGameServerPacket( PolledGameServer *server );

	void dropTimedOutServers();
	void dropServer( PolledGameServer *server );

	ServerList();
	~ServerList();

	void clearState();

	static void *resolverThreadFunc( void * );

	void addInfoServer( const netadr_t &address ) {
		assert( m_numInfoServers < kMaxInfoServers );
		m_infoServers[m_numInfoServers++] = address;
	}

	// TODO: Should not be called directly by global context
	// (ingoing connectionless packets should be tested and parsed by an instance of this ServerList entirely)
	void onServerAddressReceived( const netadr_t &address );
public:
	static void init();
	static void shutdown();
	static auto instance() -> ServerList *;

	void startPushingUpdates( ServerListListener *listener_, bool showEmptyServers, bool showFullServers );
	void stopPushingUpdates();

	void frame();

	void parseGetInfoResponse( const socket_t *socket, const netadr_t &address, msg_t *msg );
	void parseGetStatusResponse( const socket_t *socket, const netadr_t &address, msg_t *msg );
	void parseGetServersResponse( const socket_t *socket, const netadr_t &address, msg_t *msg );
};

#endif
