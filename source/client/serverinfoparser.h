#ifndef WSW_SERVERINFOPARSER_H
#define WSW_SERVERINFOPARSER_H

#include <common/types/staticstring.h>
#include <common/types/staticvector.h>

class ServerInfo;
struct msg_s;

class ServerInfoParser {
	// These fields are used to pass info during parsing
	ServerInfo *m_info { nullptr };
	uint64_t m_lastAcknowledgedChallenge { 0 };

	// This field is parsed along with info KV pairs
	uint64_t m_parsedChallenge { 0 };

	wsw::HashedStringView m_keyView;
	wsw::StringView m_valueView;

	const char *m_chars { nullptr };
	unsigned m_index { 0 };
	unsigned m_bytesLeft { 0 };

	typedef bool ( ServerInfoParser::*HandlerMethod )( const wsw::StringView & );

	struct TokenHandler {
		const wsw::HashedStringView m_key;
		TokenHandler *m_nextInHashBin { nullptr };
		const HandlerMethod m_method;

		TokenHandler( const char *key, HandlerMethod handler )
			: m_key( key ), m_method( handler ) {}

		[[nodiscard]]
		bool canHandle( const wsw::HashedStringView &key ) const {
			return m_key.equalsIgnoreCase( key );
		}

		[[nodiscard]]
		bool handle( ServerInfoParser *parser, const wsw::StringView &value ) const {
			return ( parser->*m_method )( value );
		}
	};

	wsw::StaticVector<TokenHandler, 16> m_handlers;

	static constexpr auto kNumHashBins = 17;
	TokenHandler *m_handlersHashMap[kNumHashBins];

	void addHandler( const char *command, HandlerMethod handler );
	void linkHandlerEntry( TokenHandler *handlerEntry );

	bool handleChallenge( const wsw::StringView &value );
	bool handleHostname( const wsw::StringView & );
	bool handleMaxClients( const wsw::StringView & );
	bool handleMapname( const wsw::StringView & );
	bool handleMatchTime( const wsw::StringView & );
	bool handleMatchScore( const wsw::StringView & );
	bool handleGametype( const wsw::StringView & );
	bool handleNumBots( const wsw::StringView & );
	bool handleNumClients( const wsw::StringView & );
	bool handleNeedPass( const wsw::StringView & );

	template<typename T>
	[[nodiscard]]
	bool handleInteger( const wsw::StringView &, T *result ) const;

	template<unsigned N>
	[[nodiscard]]
	bool handleString( const wsw::StringView &, wsw::StaticString<N> *result ) const;

	[[nodiscard]]
	bool scanForKey();
	[[nodiscard]]
	bool scanForValue();
public:
	ServerInfoParser();

	[[nodiscard]]
	bool parse( msg_s *msg_, ServerInfo *info_, uint64_t lastAcknowledgedChallenge_ );
	[[nodiscard]]
	bool handleKVPair();

	[[nodiscard]]
	auto getParsedChallenge() const -> uint64_t { return m_parsedChallenge; }
};

static inline bool scanInt( const char *s, char **endptr, int *result ) {
	long maybeResult = strtol( s, endptr, 10 );

	if( maybeResult == std::numeric_limits<long>::min() || maybeResult == std::numeric_limits<long>::max() ) {
		if( errno == ERANGE ) {
			return false;
		}
	}
	*result = (int)maybeResult;
	return true;
}

#endif
