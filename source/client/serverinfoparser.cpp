#include <common/helpers/hash.h>
#include <common/common.h>
#include <common/helpers/tonum.h>
#include "serverinfoparser.h"
#include "serverlist.h"
#include "client.h"

ServerInfoParser::ServerInfoParser() {
	std::fill( std::begin( m_handlersHashMap ), std::end( m_handlersHashMap ), nullptr );

	addHandler( "challenge", &ServerInfoParser::handleChallenge );
	addHandler( "sv_hostname", &ServerInfoParser::handleHostname );
	addHandler( "sv_maxclients", &ServerInfoParser::handleMaxClients );
	addHandler( "mapname", &ServerInfoParser::handleMapname );
	addHandler( "g_match_time", &ServerInfoParser::handleMatchTime );
	addHandler( "g_match_score", &ServerInfoParser::handleMatchScore );
	addHandler( "gametype", &ServerInfoParser::handleGametype );
	addHandler( "bots", &ServerInfoParser::handleNumBots );
	addHandler( "clients", &ServerInfoParser::handleNumClients );
	addHandler( "g_needpass", &ServerInfoParser::handleNeedPass );
}

bool ServerInfoParser::scanForKey() {
	uint32_t hash = 0;
	unsigned start = m_index;
	while( m_index < m_bytesLeft && m_chars[m_index] != '\\' ) {
		hash = wsw::nextHashStep( hash, m_chars[m_index] );
		m_index++;
	}

	// If no '\\' has been found before end of data
	if( m_index >= m_bytesLeft ) {
		return false;
	}

	// Otherwise we have met a '\\'
	m_keyView = wsw::HashedStringView( m_chars + start, m_index - start, hash );
	m_index++;
	return true;
}

bool ServerInfoParser::scanForValue() {
	unsigned start = m_index;
	while( m_index < m_bytesLeft && m_chars[m_index] != '\\' && m_chars[m_index] != '\n' ) {
		m_index++;
	}

	// If we have ran out of range without stopping at termination characters
	if( m_index >= m_bytesLeft ) {
		return false;
	}

	m_valueView = wsw::StringView( m_chars + start, m_index - start );
	return true;
}

bool ServerInfoParser::parse( msg_t *msg_, ServerInfo *info_, uint64_t lastAcknowledgedChallenge_ ) {
	this->m_info = info_;
	this->m_lastAcknowledgedChallenge = lastAcknowledgedChallenge_;
	this->m_parsedChallenge = 0;
	this->m_index = 0;
	this->m_chars = (const char *)( msg_->data + msg_->readcount );
	this->m_bytesLeft = MSG_BytesLeft( msg_ );

	constexpr const char *missingChallenge = "Warning: ServerList::ServerInfoParser::Parse(): missing a challenge\n";

	for(;; ) {
		if( m_index >= m_bytesLeft ) {
			msg_->readcount += m_index;
			if( !m_parsedChallenge ) {
				Com_DPrintf( missingChallenge );
				return false;
			}
			return true;
		}

		// Expect new '\\'
		if( m_chars[m_index] != '\\' ) {
			return false;
		}
		m_index++;

		// Expect a key
		if( !scanForKey() ) {
			return false;
		}

		// Expect a value
		if( !scanForValue() ) {
			return false;
		}

		// Now try handling the pair matched in the character input
		if( !handleKVPair() ) {
			return false;
		}

		// If we have stopped at \n while scanning for value
		if( m_chars[m_index] == '\n' ) {
			msg_->readcount += m_index;
			if( !m_parsedChallenge ) {
				Com_DPrintf( missingChallenge );
				return false;
			}
			return true;
		}
	}
}

void ServerInfoParser::addHandler( const char *command, HandlerMethod handler ) {
	if( m_handlers.full() ) {
		Com_Error( ERR_FATAL, "ServerList::ServerInfoParser::AddHandler(): too many handlers\n" );
	}

	void *mem = m_handlers.unsafe_grow_back();
	linkHandlerEntry( new( mem )TokenHandler( command, handler ) );
}

void ServerInfoParser::linkHandlerEntry( TokenHandler *handlerEntry ) {
	unsigned hashBinIndex = handlerEntry->m_key.getHash() % kNumHashBins;

	handlerEntry->m_nextInHashBin = m_handlersHashMap[hashBinIndex];
	m_handlersHashMap[hashBinIndex] = handlerEntry;
}

bool ServerInfoParser::handleKVPair() {
	unsigned hashBinIndex = this->m_keyView.getHash() % kNumHashBins;
	for( TokenHandler *entry = m_handlersHashMap[hashBinIndex]; entry; entry = entry->m_nextInHashBin ) {
		if( entry->canHandle( this->m_keyView ) ) {
			return entry->handle( this, this->m_valueView );
		}
	}

	// If the key is unknown, return with success.
	// Only parsing errors for known keys should terminate parsing.
	return true;
}

template <typename T>
bool ServerInfoParser::handleInteger( const wsw::StringView &value, T *result ) const {
	if( auto maybeResult = wsw::toNum<T>( value ) ) {
		*result = *maybeResult;
		return true;
	}
	return false;
}

template<unsigned N>
bool ServerInfoParser::handleString( const wsw::StringView &value, wsw::StaticString<N> *result ) const {
	if( value.size() > std::numeric_limits<uint8_t>::max() ) {
		clWarning() << "The server info value" << value << "exceeds result size limits";
		return false;
	}

	if( value.size() >= result->capacity() ) {
		clWarning() << "The server info value" << value << "exceeds the result capacity" << result->capacity();
		return false;
	}

	result->assign( value );
	return true;
}

bool ServerInfoParser::handleChallenge( const wsw::StringView &value ) {
	if( !handleInteger( value, &m_parsedChallenge ) ) {
		return false;
	}
	return m_parsedChallenge > m_lastAcknowledgedChallenge;
}

bool ServerInfoParser::handleHostname( const wsw::StringView &value ) {
	return handleString( value, &m_info->serverName );
}

bool ServerInfoParser::handleMaxClients( const wsw::StringView &value ) {
	return handleInteger( value, &m_info->maxClients );
}

bool ServerInfoParser::handleMapname( const wsw::StringView &value ) {
	return handleString( value, &m_info->mapname );
}

static inline bool scanMinutesAndSeconds( const char *s, char **endptr, int *minutes, int *seconds ) {
	int minutesValue, secondsValue;

	if( !scanInt( s, endptr, &minutesValue ) ) {
		return false;
	}

	s = *endptr;

	if( *s != ':' ) {
		return false;
	}
	s++;

	if( !scanInt( s, endptr, &secondsValue ) ) {
		return false;
	}

	if( minutesValue < 0 ) {
		return false;
	}

	if( secondsValue < 0 || secondsValue > 60 ) {
		return false;
	}
	*minutes = minutesValue;
	*seconds = secondsValue;
	return true;
}

#define DECLARE_MATCH_FUNC( funcName, flagString )                \
	static inline bool funcName( const char *s, char **endptr ) { \
		static const size_t length = strlen( flagString );        \
		if( !strncmp( s, flagString, length ) ) {                 \
			*endptr = const_cast<char *>( s + length );           \
			return true;                                          \
		}                                                         \
		return false;                                             \
	}

DECLARE_MATCH_FUNC( matchOvertime, "overtime" )
DECLARE_MATCH_FUNC( matchSuddenDeath, "suddendeath" )
DECLARE_MATCH_FUNC( matchInTimeout, "(in timeout)" )

static const wsw::StringView kTimeWarmup( "Warmup" );
static const wsw::StringView kTimeFinished( "Finished" );
static const wsw::StringView kTimeCountdown( "Countdown" );

bool ServerInfoParser::handleMatchTime( const wsw::StringView &value ) {
	if( kTimeWarmup.equalsIgnoreCase( value ) ) {
		m_info->time.isWarmup = true;
		return true;
	}

	if( kTimeFinished.equalsIgnoreCase( value ) ) {
		m_info->time.isFinished = true;
		return true;
	}

	if( kTimeCountdown.equalsIgnoreCase( value ) ) {
		m_info->time.isCountdown = true;
		return true;
	}

	char *ptr;
	if( !scanMinutesAndSeconds( value.data(), &ptr, &m_info->time.timeMinutes, &m_info->time.timeSeconds ) ) {
		return false;
	}

	if( (size_t)( ptr - value.data() ) == value.size() ) {
		return true;
	}

	if( *ptr != ' ' ) {
		return false;
	}
	ptr++;

	if( *ptr == '/' ) {
		ptr++;

		if( *ptr != ' ' ) {
			return false;
		}
		ptr++;

		if( !scanMinutesAndSeconds( value.data(), &ptr, &m_info->time.limitMinutes, &m_info->time.limitSeconds ) ) {
			return false;
		}

		if( !*ptr ) {
			return true;
		}

		if( *ptr == ' ' ) {
			ptr++;
		}
	}

	for(;; ) {
		if( *ptr == 'o' && matchOvertime( ptr, &ptr ) ) {
			m_info->time.isOvertime = true;
			continue;
		}

		if( *ptr == 's' && matchSuddenDeath( ptr, &ptr ) ) {
			m_info->time.isSuddenDeath = true;
			continue;
		}

		if( *ptr == '(' && matchInTimeout( ptr, &ptr ) ) {
			m_info->time.isTimeout = true;
			continue;
		}

		if( *ptr == ' ' ) {
			ptr++;
			continue;
		}

		if( *ptr == '/' || *ptr == '\n' ) {
			return true;
		}

		if( (size_t)( ptr - value.data() ) >= value.size() ) {
			return false;
		}
	}
}

bool ServerInfoParser::handleMatchScore( const wsw::StringView &value ) {
	m_info->score.clear();

	const auto valueLength = value.size();
	if( !valueLength ) {
		return true;
	}

	int scores[2] = { 0, 0 };
	unsigned offsets[2] = { 0, 0 };
	unsigned lengths[2] = { 0, 0 };
	const char *const valueData = value.data();
	const char *s = valueData;
	for( int i = 0; i < 2; ++i ) {
		while( *s == ' ' && (size_t)( s - valueData ) < valueLength ) {
			s++;
		}
		offsets[i] = (unsigned)( s - valueData );
		// Should not use strchr here (there is no zero terminator at the end of the value)
		while( *s != ':' && (size_t)( s - valueData ) < valueLength ) {
			s++;
		}

		if( (size_t)( s - valueData ) >= valueLength ) {
			return false;
		}
		lengths[i] = (unsigned)( s - valueData ) - offsets[i];

		if( lengths[i] >= m_info->score.scores[0].name.capacity() ) {
			return false;
		}
		s++;

		if( *s != ' ' ) {
			return false;
		}
		s++;

		char *endptr;
		if( !scanInt( s, &endptr, &scores[i] ) ) {
			return false;
		}
		s = endptr;
	}

	for( int i = 0; i < 2; ++i ) {
		auto *teamScore = &m_info->score.scores[i];
		teamScore->score = scores[i];
		teamScore->name.assign( valueData + offsets[i], lengths[i] );
	}

	return true;
}

bool ServerInfoParser::handleGametype( const wsw::StringView &value ) {
	return handleString( value, &m_info->gametype );
}

bool ServerInfoParser::handleNumBots( const wsw::StringView &value ) {
	return handleInteger( value, &m_info->numBots );
}

bool ServerInfoParser::handleNumClients( const wsw::StringView &value ) {
	return handleInteger( value, &m_info->numClients );
}

bool ServerInfoParser::handleNeedPass( const wsw::StringView &value ) {
	return handleInteger( value, &m_info->needPassword );
}