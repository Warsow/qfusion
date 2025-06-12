#ifndef WSW_e85f918d_1654_4d38_a893_87bdd3af22ad_H
#define WSW_e85f918d_1654_4d38_a893_87bdd3af22ad_H

#include <common/helpers/q_math.h>
#include <common/facilities/q_comref.h>
#include <common/types/stringview.h>

#include <cstdarg>

class ChatHandlersChain;
struct edict_s;
struct RespectStats;

/// @brief A helper that aids printing chat messages without most
/// redundant formatting operations in a naive implementation.
class ChatPrintHelper {
	enum { kTextOffset = 32 };
	enum { kMaxTextLength = 1024 };

	const edict_s *const m_sender;
	const uint64_t m_sendCommandNum;
	// Add an extra space for a terminating quote and the zero
	char m_buffer[kTextOffset + kMaxTextLength + 2];
	int m_senderNum { 0 };
	int m_messageLength { 0 };
	bool m_hasPrintedToServerConsole { false };
	bool m_skipServerConsole { false };

	void formatTextFromVarargs( const char *format, va_list va );
	void finishSetup();

	void printToServerConsole( bool teamOnly );

	// Returns an address of the whole combined buffer that is ready to be supplied as a command
	[[nodiscard]]
	auto setupPrefixForOthers( bool teamOnly ) -> const char *;
	[[nodiscard]]
	auto setupPrefixForSender( bool teamOnly ) -> const char *;

	void printTo( const edict_s *target, bool teamOnly );
	void dispatchWithFilter( const ChatHandlersChain *filter, bool teamOnly );
public:
	ChatPrintHelper &operator=( ChatPrintHelper && ) = delete;
	ChatPrintHelper( ChatPrintHelper && ) = delete;

#ifndef _MSC_VER
	ChatPrintHelper( const edict_s *sender, uint64_t sendCommandNum, const char *format, ... )
		__attribute__( ( format( printf, 4, 5 ) ) );
	explicit ChatPrintHelper( const char *format, ... )
		__attribute__( ( format( printf, 2, 3 ) ) );
#else
	ChatPrintHelper( const edict_s *sender, uint64_t sendCommandNum, _Printf_format_string_ const char *format, ... );
	explicit ChatPrintHelper( _Printf_format_string_ const char *format, ... );
#endif

	ChatPrintHelper( const edict_s *sender, uint64_t sendCommandNum, const wsw::StringView &message );

	void skipServerConsole() { m_skipServerConsole = true; }

	void printToChatOf( const edict_s *target ) { printTo( target, false ); }
	void printToTeamChatOf( const edict_s *target ) { printTo( target, true ); }
	void printToTeam( const ChatHandlersChain *filter = nullptr ) { dispatchWithFilter( filter, true ); }
	void printToEverybody( const ChatHandlersChain *filter = nullptr ) { dispatchWithFilter( filter, false ); }
};

struct CmdArgs;

class IgnoreFilter {
	struct ClientEntry {
		static_assert( MAX_CLIENTS <= 64, "" );
		uint64_t ignoredClientsMask;
		bool ignoresEverybody;
		bool ignoresNotTeammates;

		ClientEntry() {
			Reset();
		}

		void Reset() {
			ignoredClientsMask = 0;
			ignoresEverybody = false;
			ignoresNotTeammates = false;
		}

		void SetClientBit( int clientNum, bool ignore ) {
			assert( (unsigned)clientNum < 64u );
			uint64_t bit = ( ( (uint64_t)1 ) << clientNum );
			if( ignore ) {
				ignoredClientsMask |= bit;
			} else {
				ignoredClientsMask &= ~bit;
			}
		}

		bool GetClientBit( int clientNum ) const {
			assert( (unsigned)clientNum < 64u );
			return ( ignoredClientsMask & ( ( (uint64_t)1 ) << clientNum ) ) != 0;
		}
	};

	ClientEntry m_entries[MAX_CLIENTS];

	void sendChangeFilterVarCommand( const edict_s *ent );
	void printIgnoreCommandUsage( const edict_s *ent, bool ignore );
public:
	void handleIgnoreCommand( const edict_s *ent, bool ignore, const CmdArgs &cmdArgs );
	void handleIgnoreListCommand( const edict_s *ent, const CmdArgs &cmdArgs );

	void reset();

	void resetForClient( int clientNum ) { m_entries[clientNum].Reset(); };

	[[nodiscard]]
	bool ignores( const edict_s *target, const edict_s *source ) const;
	void notifyOfIgnoredMessage( const edict_s *target, const edict_s *source ) const;
	void onUserInfoChanged( const edict_s *user );
};

struct ChatMessage {
	const uint64_t clientCommandNum;
	const wsw::StringView text;
	const unsigned clientNum;
};

/**
 * A common supertype for chat message handlers.
 * Could pass a message to another filter, reject it or print it on its own.
 */
class ChatHandler {
protected:
	virtual ~ChatHandler() = default;

	/**
	 * Should reset the held internal state for a new match
	 */
	virtual void reset() = 0;

	/**
	 * Should reset the held internal state for a client
	 */
	virtual void resetForClient( int clientNum ) = 0;

	/**
	 * Should "handle" the message appropriately.
	 */
	[[nodiscard]]
	virtual auto handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> = 0;
};

/**
 * A {@code ChatHandler} that allows to mute/unmute public messages of a player during match time.
 */
class MuteFilter final : public ChatHandler {
	friend class ChatHandlersChain;

	bool m_muted[MAX_CLIENTS] {};

	MuteFilter() { reset(); }

	void mute( const edict_s *ent );
	void unmute( const edict_s *ent );

	void resetForClient( int clientNum ) override {
		m_muted[clientNum] = false;
	}

	void reset() override {
		std::memset( m_muted, 0, sizeof( m_muted ) );
	}

	[[nodiscard]]
	auto handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> override;
};

/**
 * A {@code ChatHandler} that allows to throttle down flux of messages from a player.
 * This filter could serve for throttling team messages of a player as well.
 */
class FloodFilter final : public ChatHandler {
	friend class ChatHandlersChain;

	FloodFilter() { reset(); }

	void resetForClient( int clientNum ) override {}
	void reset() override {}

public:
	enum Flags : unsigned { DontPrint = 0x1, TeamOnly = 0x2 };
private:
	[[nodiscard]]
	auto detectFlood( const edict_s *ent, unsigned flags = 0 ) -> std::optional<uint16_t>;
	[[nodiscard]]
	auto detectFlood( const ChatMessage &message, unsigned flags = 0 ) -> std::optional<uint16_t>;

	[[nodiscard]]
	auto handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> override {
		if( const auto millisLeft = detectFlood( message, DontPrint ) ) {
			return MessageFault { message.clientCommandNum, MessageFault::Flood, *millisLeft };
		}
		return std::nullopt;
	}
};

/**
 * A {@code ChatHandler} that analyzes chat messages of a player.
 * Messages are never rejected by this handler.
 * Some information related to the honor of Respect and Sportsmanship Codex
 * is extracted from messages and transmitted to the {@code StatsowFacade}.
 */
class RespectHandler final : public ChatHandler {
	friend class ChatHandlersChain;
	friend class StatsowFacade;
	friend class RespectTokensRegistry;

	enum { kNumTokens = 10 };

	struct ClientEntry {
		int64_t m_warnedAt { 0 };
		int64_t m_joinedMidGameAt { 0 };
		int64_t m_lastSaidAt[kNumTokens] {};
		const edict_s *m_ent { nullptr };
		unsigned m_numSaidTokens[kNumTokens] {0 };
		bool m_hasCompletedMatchStartAction { false };
		bool m_hasCompletedMatchEndAction { false };
		bool m_hasTakenCountdownHint { false };
		bool m_hasTakenStartHint { false };
		bool m_hasTakenSecondStartHint { false };
		bool m_hasTakenFinalHint { false };
		bool m_hasIgnoredCodex { false };
		bool m_hasViolatedCodex { false };

		void reset();

		[[nodiscard]]
		bool handleMessage( const ChatMessage &message );

		[[nodiscard]]
		bool processMessageAsRespectTokensOnlyMessage( const wsw::StringView &message );

		/**
		 * Checks actions necessary to honor Respect and Sportsmanship Codex for a client every frame
		 */
		void checkBehaviour( int64_t matchStartTime );

		void requestClientRespectAction( unsigned tokenNum );

		void announceMisconductBehaviour( const char *action );

		/**
		 * Adds accumulated data to reported stats.
		 * Handles respect violation states appropriately.
		 * @param reportedStats respect stats that are finally reported to Statsow
		 */
		void addToReportStats( RespectStats *reportedStats );

		void onClientDisconnected();
		void onClientJoinedTeam( int newTeam );
	};

	ClientEntry m_entries[MAX_CLIENTS];

	int64_t m_matchStartedAt { -1 };
	int64_t m_lastFrameMatchState { -1 };

	RespectHandler();

	void resetForClient( int clientNum ) override { m_entries[clientNum].reset(); }

	void reset() override;

	[[nodiscard]]
	bool skipStatsForClient( const edict_s *ent ) const;

	[[nodiscard]]
	auto handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> override;

	void frame();

	void addToReportStats( const edict_s *ent, RespectStats *reportedStats );

	void onClientDisconnected( const edict_s *ent );
	void onClientJoinedTeam( const edict_s *ent, int newTeam );
};

/**
 * A container for all registered {@code ChatHandler} instances
 * that has a {@code ChatHandler} interface itself and acts as a facade
 * for the chat filtering subsystem applying filters in desired order.
 */
class ChatHandlersChain final : public ChatHandler {
	template <typename T> friend class SingletonHolder;

	friend class StatsowFacade;

	MuteFilter m_muteFilter;
	FloodFilter m_floodFilter;
	RespectHandler m_respectHandler;
	IgnoreFilter m_ignoreFilter;

	ChatHandlersChain() { reset(); }

	void reset() override;
public:
	[[nodiscard]]
	auto handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> override;

	void resetForClient( int clientNum ) override;

	static void init();
	static void shutdown();
	static ChatHandlersChain *instance();

	void mute( const edict_s *ent ) { m_muteFilter.mute( ent ); }
	void unmute( const edict_s *ent ) { m_muteFilter.unmute( ent ); }

	[[nodiscard]]
	auto detectFlood( const edict_s *ent, unsigned floodFilterFlags = 0 ) -> std::optional<uint16_t> {
		return m_floodFilter.detectFlood( ent, floodFilterFlags );
	}
	[[nodiscard]]
	bool skipStatsForClient( const edict_s *ent ) const {
		return m_respectHandler.skipStatsForClient( ent );
	}

	void onClientDisconnected( const edict_s *ent );

	void onClientJoinedTeam( const edict_s *ent, int newTeam ) {
		m_respectHandler.onClientJoinedTeam( ent, newTeam );
	}

	void addToReportStats( const edict_s *ent, RespectStats *reportedStats ) {
		m_respectHandler.addToReportStats( ent, reportedStats );
	}

	[[nodiscard]]
	bool ignores( const edict_s *target, const edict_s *source ) const {
		return m_ignoreFilter.ignores( target, source );
	}

	void notifyOfIgnoredMessage( const edict_s *target, const edict_s *source ) const {
		m_ignoreFilter.notifyOfIgnoredMessage( target, source );
	}

	static void handleIgnoreCommand( edict_s *ent, const CmdArgs &cmdArgs ) {
		instance()->m_ignoreFilter.handleIgnoreCommand( ent, true, cmdArgs );
	}

	static void handleUnignoreCommand( edict_s *ent, const CmdArgs &cmdArgs ) {
		instance()->m_ignoreFilter.handleIgnoreCommand( ent, false, cmdArgs );
	}

	static void handleIgnoreListCommand( edict_s *ent, const CmdArgs &cmdArgs ) {
		instance()->m_ignoreFilter.handleIgnoreListCommand( ent, cmdArgs );
	}

	void onUserInfoChanged( const edict_s *ent ) {
		m_ignoreFilter.onUserInfoChanged( ent );
	}

	void frame();
};



#endif