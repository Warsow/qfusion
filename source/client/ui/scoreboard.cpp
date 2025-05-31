#include "scoreboard.h"

#include "local.h"
#include <common/q_shared.h>
#include <common/gs_public.h>
#include <common/wswalgorithm.h>
#include <common/wswstringsplitter.h>
#include <common/wswtonum.h>
#include <client/client.h>

using wsw::operator""_asView;

// TODO: This should be shared...
static const wsw::StringView kPlaceholder( "-"_asView );

namespace wsw::ui {

void Scoreboard::clearSchema() {
	m_columnKinds.clear();
	m_columnSlots.clear();
	m_titleColumnSpans.clear();
	m_pingSlot = std::nullopt;
	m_nameColumn = std::nullopt;
	m_columnTitlesStorage.clear();
	m_columnAssetsStorage.clear();
	m_titleSpanColumnsLeft = 0;
}

void Scoreboard::reload() {
	clearSchema();

	if( const auto maybeError = doReload() ) {
		assert( maybeError->isZeroTerminated() );
		uiError() << "Failed to reload scoreboard schema" << *maybeError;
		clearSchema();
	}

	static_assert( std::is_standard_layout_v<decltype( m_oldRawData )> );
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

bool Scoreboard::parseLayoutTitleColumnSpan( const wsw::StringView &token ) {
	assert( m_columnKinds.size() == m_titleColumnSpans.size() + 1 );
	if( const auto maybeNum = wsw::toNum<unsigned>( token ) ) {
		if( const auto num = *maybeNum; num <= 3 ) {
			if( m_titleSpanColumnsLeft ) {
				if( num || m_columnKinds.back() != Icon ) {
					return false;
				}
				m_titleSpanColumnsLeft--;
			} else {
				if( num != 1 && m_columnKinds.back() != Icon ) {
					return false;
				}
				m_titleSpanColumnsLeft = num - 1;
			}
			m_titleColumnSpans.push_back( num );
			return true;
		}
	}
	return false;
}

bool Scoreboard::parseLayoutKind( const wsw::StringView &token ) {
	const std::array<ColumnKind, 5> uniqueFields { Nickname, Clan, Ping, Score, Status };

	const auto maybeValue = wsw::toNum<unsigned>( token );
	// TODO: Use magic_enum for obtaining the max value
	if( !maybeValue || *maybeValue > Icon ) {
		return false;
	}
	const auto kind = (ColumnKind)*maybeValue;
	if( wsw::contains( uniqueFields, kind ) ) {
		// Disallow multiple columns of these kinds
		if( wsw::contains( m_columnKinds, kind ) ) {
			return false;
		}
		// Disallow empty titles for these kinds
		if( m_columnTitlesStorage.back().empty() ) {
			if( kind != Status ) {
				return false;
			}
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
	if( kind == Nickname || kind == Clan || kind == Score ) {
		if( !token.equalsIgnoreCase( kPlaceholder ) ) {
			return false;
		}
		// Push an illegal number so we crash on attempt of using it.
		// TODO: Use std::optional instead?
		m_columnSlots.push_back( ~0u );
		return true;
	}

	if( token.equalsIgnoreCase( kPlaceholder ) ) {
		return false;
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

	wsw::StringSplitter splitter( string );
	while( const auto maybeTokenAndNum = splitter.getNextWithNum() ) {
		if( m_columnKinds.full() ) {
			return false;
		}
		bool res = false;
		const auto &[token, num] = *maybeTokenAndNum;
		switch( num % 4 ) {
			case 0: res = parseLayoutTitle( token ); break;
			case 1: res = parseLayoutKind( token ); break;
			case 2: res = parseLayoutSlot( token ); break;
			case 3: res = parseLayoutTitleColumnSpan( token ); break;
		}
		if( !res ) {
			return false;
		}
	}

	if( m_columnKinds.size() < 2 || m_columnTitlesStorage.size() != m_columnKinds.size() ) {
		return false;
	}

	if( m_titleSpanColumnsLeft ) {
		return false;
	}

	assert( m_columnKinds.size() == m_titleColumnSpans.size() );

	assert( m_pingSlot == std::nullopt );
	assert( m_nameColumn == std::nullopt );
	// TODO: Share with the game module implementation? Supply the slot within the replicated data?
	unsigned slotCounter = 0;
	for( unsigned column = 0; column < m_columnKinds.size(); ++column ) {
		const auto kind = m_columnKinds[column];
		if( kind == Nickname ) {
			m_nameColumn = column;
		} else if( kind != Clan && kind != Score ) {
			if( kind == Ping ) {
				m_pingSlot = slotCounter;
			}
			slotCounter++;
		}
	}

	// The ping column could be omitted but the name is always present
	assert( m_nameColumn.has_value() );
	return true;
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

auto Scoreboard::checkAndGetUpdates( const RawData &currData, PlayerUpdatesList &playerUpdates,
									 TeamUpdatesList &teamUpdates ) -> std::optional<UpdateFlags> {
	playerUpdates.clear();
	teamUpdates.clear();

	unsigned updatesMask = 0x0;
	bool isTeamUpdateNeeded[4] { false, false, false, false };

	// This element-wise comparison is 100% correct for this field, as it's padded by zeroes
	if( std::memcmp( currData.challengersQueue, m_oldRawData.challengersQueue, std::size( currData.challengersQueue ) ) != 0 ) {
		updatesMask |= (unsigned)UpdateFlags::Challengers;
	}

	for( unsigned playerIndex = 0; playerIndex < kMaxPlayers; ++playerIndex ) {
		const auto oldTeam = m_oldRawData.getPlayerTeam( playerIndex );
		const auto newTeam = currData.getPlayerTeam( playerIndex );
		if( oldTeam != newTeam ) {
			isTeamUpdateNeeded[oldTeam] = isTeamUpdateNeeded[newTeam] = true;
		}
		if( addPlayerUpdates( m_oldRawData, currData, playerIndex, playerUpdates ) ) {
			const unsigned playerNum = currData.getPlayerNum( playerIndex );
			static_assert( kMaxPlayers <= sizeof( unsigned ) * 8 );
			assert( playerNum >= 0 && playerNum < sizeof( unsigned ) * 8 );
			if( !( updatesMask & (unsigned)UpdateFlags::Challengers ) ) {
				const uint8_t *const queueBegin = std::begin( currData.challengersQueue );
				const uint8_t *const queueEnd   = std::end( currData.challengersQueue );
				if( wsw::contains( queueBegin, queueEnd, playerNum + 1 ) ) {
					updatesMask |= (unsigned)UpdateFlags::Challengers;
				}
			}
			if( currData.getPlayerTeam( playerIndex ) == TEAM_SPECTATOR ) {
				updatesMask |= (unsigned)UpdateFlags::Spectators;
			}
		}
	}

	if( !playerUpdates.empty() ) {
		updatesMask |= (unsigned)UpdateFlags::Players;
	}

	for( unsigned i = 0; i < 4; ++i ) {
		if( isTeamUpdateNeeded[i] ) {
			teamUpdates.push_back( TeamUpdates { .team = (uint8_t)i, .players = true } );
			updatesMask |= (unsigned)UpdateFlags::Teams;
		}
	}

	// This condition saves an (implicit) memcpy call.
	if( updatesMask ) {
		m_oldRawData = currData;
		return (UpdateFlags)updatesMask;
	}

	return std::nullopt;
}

auto Scoreboard::getImageAssetPath( unsigned asset ) const -> std::optional<wsw::StringView> {
	if( asset && asset < m_columnAssetsStorage.size() + 1 ) {
		return m_columnAssetsStorage[asset - 1];
	}
	return std::nullopt;
}

auto Scoreboard::getPlayerPing( unsigned playerIndex ) const -> int {
	assert( m_pingSlot.has_value() );
	assert( playerIndex < (unsigned)kMaxPlayers );
	return m_oldRawData.getPlayerShort( playerIndex, *m_pingSlot );
}

auto Scoreboard::getPlayerName( unsigned playerIndex ) const -> wsw::StringView {
	assert( m_nameColumn.has_value() );
	assert( playerIndex < (unsigned)kMaxPlayers );
	return CG_PlayerName( m_oldRawData.getPlayerNum( playerIndex ) );
}

auto Scoreboard::getPlayerNameForColumn( unsigned playerIndex, unsigned column ) const -> wsw::StringView {
	assert( m_columnKinds[column] == Nickname );
	assert( playerIndex < (unsigned)kMaxPlayers );
	return CG_PlayerName( m_oldRawData.getPlayerNum( playerIndex ) );
}

auto Scoreboard::getPlayerClanForColumn( unsigned playerIndex, unsigned column ) const -> wsw::StringView {
	assert( m_columnKinds[column] == Clan );
	assert( playerIndex < (unsigned)kMaxPlayers );
	return CG_PlayerClan( m_oldRawData.getPlayerNum( playerIndex ) );
}

bool Scoreboard::addPlayerUpdates( const RawData &oldOne, const RawData &newOne, unsigned playerIndex, PlayerUpdatesList &dest ) {
	const auto oldPlayerNum = oldOne.getPlayerNum( playerIndex );
	const auto newPlayerNum = newOne.getPlayerNum( playerIndex );

	bool nickname = false, clan = false;
	if( oldPlayerNum != newPlayerNum ) {
		// Consider doing a full update in this case
		nickname = clan = true;
		// Keep update counters as-is.
		// What to do in this case is non-obvious and having a small pending extra update is harmless.
	} else {
		const auto playerNum = newPlayerNum;
		const auto *const tracker = wsw::ui::NameChangesTracker::instance();
		const unsigned nameCounter = tracker->getLastNicknameUpdateCounter( playerNum );
		const unsigned clanCounter = tracker->getLastClanUpdateCounter( playerNum );
		if( nameCounter != m_lastNameUpdateCounters[playerNum] ) {
			m_lastNameUpdateCounters[playerNum] = nameCounter;
			nickname = true;
		}
		if( clanCounter != m_lastClanUpdateCounters[playerNum] ) {
			m_lastClanUpdateCounters[playerNum] = clanCounter;
			clan = true;
		}
	}

	const bool score = newOne.getPlayerScore( playerIndex ) != oldOne.getPlayerScore( playerIndex );
	const bool ghosting = newOne.isPlayerGhosting( playerIndex ) != oldOne.isPlayerGhosting( playerIndex );

	uint8_t mask = 0;
	static_assert( 1u << kMaxShortSlots <= std::numeric_limits<uint8_t>::max() );
	for( unsigned slot = 0; slot < kMaxShortSlots; ++slot ) {
		if( newOne.getPlayerShort( playerIndex, slot ) != oldOne.getPlayerShort( playerIndex, slot ) ) {
			mask |= ( 1u << slot );
		}
	}

	if( (unsigned)nickname | (unsigned)clan | (unsigned)score | (unsigned)mask | (unsigned)ghosting ) {
		new( dest.unsafe_grow_back() )PlayerUpdates { (uint8_t)playerIndex, mask, nickname, clan, score, ghosting };
		return true;
	}

	return false;
}

}