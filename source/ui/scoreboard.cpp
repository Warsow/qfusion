#include "scoreboard.h"

#include "../qcommon/wswstringsplitter.h"
#include "../qcommon/wswtonum.h"
#include "../client/client.h"

using wsw::operator""_asView;

// TODO: This should be shared...
static const wsw::StringView kPlaceholder( "-"_asView );

namespace wsw::ui {

void Scoreboard::clearSchema() {
	m_columnKinds.clear();
	m_columnTitlesStorage.clear();
	m_columnAssetsStorage.clear();
}

void Scoreboard::reload() {
	clearSchema();

	if( const auto maybeError = doReload() ) {
		assert( maybeError->isZeroTerminated() );
		Com_Printf( S_COLOR_RED "ScoreboardModelProxy: %s\n", maybeError->data() );
		clearSchema();
	}

	static_assert( std::is_pod_v<decltype( m_oldRawData )> );
	std::memset( &m_oldRawData, 0, sizeof( ReplicatedScoreboardData ) );
}

auto Scoreboard::doReload() -> std::optional<wsw::StringView> {
	assert( m_columnKinds.empty() && m_columnTitlesStorage.empty() && m_columnAssetsStorage.empty() );

	if( const auto maybeAssetsString = ::cl.configStrings.get( CS_SCOREBOARD_ASSETS ) ) {
		if( !parseAssets( *maybeAssetsString ) ) {
			return "Failed to parse the assets config string"_asView;
		}
	}

	// Protect against bogus server configstring data
	if( const auto maybeLayoutString = ::cl.configStrings.get( CS_SCOREBOARD_SCHEMA ) ) {
		if( !parseLayout( *maybeLayoutString ) ) {
			return "Failed to parse the layout config string"_asView;
		}
	} else {
		return "The layout config string is missing"_asView;
	}

	assert( !m_columnTitlesStorage.empty() && m_columnTitlesStorage.size() == m_columnKinds.size() );
	return std::nullopt;
}

bool Scoreboard::parseLayoutTitle( const wsw::StringView &token ) {
	if( token.length() > 16 ) {
		return false;
	}
	wsw::StringView title( token );
	if( token.equalsIgnoreCase( kPlaceholder ) ) {
		title = wsw::StringView();
	}
	if( !m_columnTitlesStorage.canAdd( title ) ) {
		return false;
	}
	wsw::StaticString<16> tmp;
	if( title.contains( '_' ) ) {
		for( char ch : title ) {
			tmp.push_back( ( ch != '_' ) ? ch : ' ' );
		}
		title = tmp.asView();
	}
	m_columnTitlesStorage.add( title );
	return true;
}

bool Scoreboard::parseLayoutKind( const wsw::StringView &token ) {
	const std::array<ColumnKind, 4> uniqueFields { Nickname, Clan, Ping, Score };

	const auto maybeValue = wsw::toNum<unsigned>( token );
	// TODO: Use magic_enum for obtaining the max value
	if( !maybeValue || *maybeValue > Icon ) {
		return false;
	}
	const auto kind = (ColumnKind)*maybeValue;
	if( std::find( uniqueFields.begin(), uniqueFields.end(), kind ) != uniqueFields.end() ) {
		// Disallow multiple columns of these kinds
		if( std::find( m_columnKinds.begin(), m_columnKinds.end(), kind ) != m_columnKinds.end() ) {
			return false;
		}
		// Disallow empty titles for these kinds
		if( m_columnTitlesStorage.back().empty() ) {
			return false;
		}
	} else {
		if( m_columnTitlesStorage.back().empty() ) {
			if( kind != Icon ) {
				return false;
			}
		}
	}
	m_columnKinds.push_back( kind );
	return true;
}

bool Scoreboard::parseLayoutSlot( const wsw::StringView &token ) {
	assert( m_columnKinds.size() == m_columnSlots.size() + 1 );

	const ColumnKind kind = m_columnKinds.back();
	if( isSeparateSlotSpaceKind( kind ) ) {
		if( !token.equalsIgnoreCase( kPlaceholder ) ) {
			return false;
		}
		// Push an illegal number so we crash on attempt of using it.
		// TODO: Use std::optional instead?
		m_columnSlots.push_back( ~0u );
		return true;
	}

	if( token.equalsIgnoreCase( kPlaceholder ) ) {
		if( kind != Ping ) {
			return false;
		}
	}

	const auto maybeSlot = wsw::toNum<unsigned>( token );
	if( !maybeSlot ) {
		return false;
	}

	const unsigned slot = *maybeSlot;
	if( slot >= kMaxShortSlots ) {
		return false;
	}

	for( unsigned existingSlot: m_columnSlots ) {
		if( existingSlot == slot ) {
			return false;
		}
	}

	m_columnSlots.push_back( slot );
	assert( m_columnSlots.size() == m_columnKinds.size() );
	return true;
}

bool Scoreboard::parseLayout( const wsw::StringView &string ) {
	assert( m_columnKinds.empty() );

	wsw::StaticVector<ColumnKind, 4> metFields;

	wsw::StringSplitter splitter( string );
	while( const auto maybeTokenAndNum = splitter.getNextWithNum() ) {
		if( m_columnKinds.size() == m_columnKinds.capacity() ) {
			return false;
		}
		bool res;
		const auto &[token, num] = *maybeTokenAndNum;
		switch( num % 3 ) {
			case 0: res = parseLayoutTitle( token ); break;
			case 1: res = parseLayoutKind( token ); break;
			case 2: res = parseLayoutSlot( token ); break;
		}
		if( !res ) {
			Com_Printf( S_COLOR_CYAN "%d\n", (int)( num % 3 ) );
			return false;
		}
	}
	return m_columnKinds.size() > 2 && m_columnTitlesStorage.size() == m_columnKinds.size();
}

bool Scoreboard::parseAssets( const wsw::StringView &string ) {
	assert( m_columnAssetsStorage.empty() );

	wsw::StringSplitter splitter( string );
	while( const auto maybeToken = splitter.getNext() ) {
		const auto token( *maybeToken );
		if( !m_columnAssetsStorage.canAdd( token ) ) {
			return false;
		}
		for( const wsw::StringView &existing: m_columnAssetsStorage ) {
			if( token.equalsIgnoreCase( existing ) ) {
				return false;
			}
		}
		// TODO: Check whether it's a valid asset?
		m_columnAssetsStorage.add( token );
	}

	return true;
}

bool Scoreboard::checkUpdates( const RawData &currData, PlayerUpdatesList &playerUpdates, TeamUpdatesList &teamUpdates ) {
	playerUpdates.clear();
	teamUpdates.clear();

	// TODO: Check team score updates

	// TODO: Limit iteration by gs.maxclients
	for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
		if( auto maybePlayerUpdates = checkPlayerDataUpdates( m_oldRawData, currData, i ) ) {
			playerUpdates.push_back( *maybePlayerUpdates );
		}
	}

	const bool result = !( teamUpdates.empty() && playerUpdates.empty() );
	if( result ) {
		m_oldRawData = currData;
	}
	return result;
}

void Scoreboard::handleConfigString( unsigned int configStringIndex, const wsw::StringView &string ) {
	const auto playerNum = (unsigned)( configStringIndex - CS_PLAYERINFOS );
	assert( playerNum < (unsigned)MAX_CLIENTS );
	// Consider this as a full update currently
	m_pendingPlayerUpdates[playerNum] = (PendingPlayerUpdates)( PendingClanUpdate | PendingNameUpdate );
}

auto Scoreboard::checkPlayerDataUpdates( const RawData &oldOne, const RawData &newOne, unsigned playerNum )
	 -> std::optional<PlayerUpdates> {
	const bool nickname = m_pendingPlayerUpdates[playerNum] & PendingNameUpdate;
	const bool clan = m_pendingPlayerUpdates[playerNum] & PendingClanUpdate;
	m_pendingPlayerUpdates[playerNum] = NoPendingUpdates;

	const bool score = newOne.getPlayerScore( playerNum ) != oldOne.getPlayerScore( playerNum );

	uint8_t mask = 0;
	static_assert( 1u << kMaxShortSlots <= std::numeric_limits<uint8_t>::max() );
	for( unsigned slot = 0; slot < kMaxShortSlots; ++slot ) {
		if( newOne.getPlayerShort( playerNum, slot ) != oldOne.getPlayerShort( playerNum, slot ) ) {
			mask |= ( 1u << slot );
		}
	}

	if( ( (unsigned)nickname | (unsigned)clan | (unsigned)score | (unsigned)mask ) ) {
		return PlayerUpdates { mask, nickname, clan, score };
	}

	return std::nullopt;
}

}