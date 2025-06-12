#ifndef WSW_955f6ab1_1bd9_4796_8984_0f1b743c660c_H
#define WSW_955f6ab1_1bd9_4796_8984_0f1b743c660c_H

#include <common/facilities/q_comref.h>
#include <common/types/staticstring.h>
#include <common/types/staticvector.h>
#include <common/types/stringspanstorage.h>

#include <array>

namespace wsw::ui {

// TODO: Shouldn't it belong to the CG subsystem?
class Scoreboard : wsw::ScoreboardShared {
	wsw::StaticVector<ColumnKind, kMaxColumns> m_columnKinds;
	wsw::StaticVector<unsigned, kMaxColumns> m_titleColumnSpans;
	wsw::StaticVector<unsigned, kMaxAssets> m_columnSlots;

	std::optional<unsigned> m_nameColumn;
	std::optional<unsigned> m_pingSlot;

	unsigned m_titleSpanColumnsLeft { 0 };

	wsw::StringSpanStaticStorage<uint8_t, uint8_t, kMaxColumns, kTitleDataLimit> m_columnTitlesStorage;
	wsw::StringSpanStaticStorage<uint8_t, uint8_t, kMaxAssets, kAssetDataLimit> m_columnAssetsStorage;

	using RawData = ReplicatedScoreboardData;

	RawData m_oldRawData {};

	std::array<unsigned, MAX_CLIENTS> m_lastNameUpdateCounters;
	std::array<unsigned, MAX_CLIENTS> m_lastClanUpdateCounters;

	[[nodiscard]]
	bool parseLayout( const wsw::StringView &string );

	[[nodiscard]]
	bool parseLayoutTitle( const wsw::StringView &token );

	[[nodiscard]]
	bool parseLayoutTitleColumnSpan( const wsw::StringView &token );

	[[nodiscard]]
	bool parseLayoutKind( const wsw::StringView &token );

	[[nodiscard]]
	bool parseLayoutSlot( const wsw::StringView &token );

	[[nodiscard]]
	bool parseAssets( const wsw::StringView &string );

	void clearSchema();

	[[nodiscard]]
	auto doReload() -> std::optional<wsw::StringView>;
public:
	struct PlayerUpdates {
		uint8_t playerIndex;
		uint8_t shortSlotsMask;
		bool nickname: 1;
		bool clan: 1;
		bool score: 1;
		bool ghosting: 1;
	};

	struct TeamUpdates {
		uint8_t team;
		bool score : 1;
		bool name : 1;
		bool players : 1;
	};

	using PlayerUpdatesList = wsw::StaticVector<PlayerUpdates, MAX_CLIENTS>;
	using TeamUpdatesList = wsw::StaticVector<TeamUpdates, 4>;
private:
	[[nodiscard]]
	bool addPlayerUpdates( const RawData &oldOne, const RawData &newOne, unsigned playerIndex, PlayerUpdatesList &dest );
public:
	Scoreboard() {
		m_lastNameUpdateCounters.fill( 0 );
		m_lastClanUpdateCounters.fill( 0 );
	}

	void reload();

	[[nodiscard]]
	auto getImageAssetPath( unsigned asset ) const -> std::optional<wsw::StringView>;

	[[nodiscard]]
	auto getColumnsCount() const -> unsigned {
		return m_columnKinds.size();
	}
	[[nodiscard]]
	auto getColumnKind( unsigned column ) const -> ColumnKind {
		return m_columnKinds[column];
	}
	[[nodiscard]]
	auto getColumnTitle( unsigned column ) const -> wsw::StringView {
		return m_columnTitlesStorage[column];
	}
	[[nodiscard]]
	auto getColumnSlot( unsigned column ) const -> unsigned {
		return m_columnSlots[column];
	}
	[[nodiscard]]
	auto getTitleColumnSpan( unsigned column ) const -> unsigned {
		return m_titleColumnSpans[column];
	}

	[[nodiscard]]
	auto getPlayerNum( unsigned playerIndex ) const -> unsigned {
		return m_oldRawData.getPlayerNum( playerIndex );
	}
	[[nodiscard]]
	auto getPlayerTeam( unsigned playerIndex ) const -> unsigned {
		return m_oldRawData.getPlayerTeam( playerIndex );
	}
	[[nodiscard]]
	bool isPlayerConnected( unsigned playerIndex ) const {
		return m_oldRawData.isPlayerConnected( playerIndex );
	}
	[[nodiscard]]
	bool isPlayerGhosting( unsigned playerIndex ) const {
		return m_oldRawData.isPlayerGhosting( playerIndex );
	}

	[[nodiscard]]
	auto getClientNumOfChallenger( unsigned indexInQueue ) const -> std::optional<unsigned> {
		return m_oldRawData.getClientNumOfChallenger( indexInQueue );
	}

	[[nodiscard]]
	auto getPlayerIconForColumn( unsigned playerIndex, unsigned column ) const -> unsigned {
		assert( m_columnKinds[column] == Icon );
		return m_oldRawData.getPlayerShort( playerIndex, m_columnSlots[column] );
	}
	[[nodiscard]]
	auto getPlayerNumberForColumn( unsigned playerIndex, unsigned column ) const -> int {
		assert( m_columnKinds[column] == Number );
		return m_oldRawData.getPlayerShort( playerIndex, m_columnSlots[column] );
	}
	[[nodiscard]]
	auto getPlayerGlyphForColumn( unsigned playerIndex, unsigned column ) const -> int {
		assert( m_columnKinds[column] == Glyph );
		return m_oldRawData.getPlayerShort( playerIndex, m_columnSlots[column] );
	}
	[[nodiscard]]
	auto getPlayerPingForColumn( unsigned playerIndex, unsigned column ) const -> int {
		assert( m_columnKinds[column] == Ping );
		return m_oldRawData.getPlayerShort( playerIndex, m_columnSlots[column] );
	}
	[[nodiscard]]
	auto getPlayerScoreForColumn( unsigned playerIndex, unsigned column ) const -> int {
		assert( m_columnKinds[column] == Score );
		return m_oldRawData.getPlayerScore( playerIndex );
	}
	[[nodiscard]]
	auto getPlayerStatusForColumn( unsigned playerIndex, unsigned column ) const -> int {
		assert( m_columnKinds[column] == Status );
		return m_oldRawData.getPlayerShort( playerIndex, m_columnSlots[column] );
	}

	[[nodiscard]]
	bool hasPing() const { return m_pingSlot.has_value(); }
	[[nodiscard]]
	auto getPlayerPing( unsigned playerIndex ) const -> int;
	[[nodiscard]]
	auto getPlayerName( unsigned playerIndex ) const -> wsw::StringView;

	// Getters for table-based models that track columns. The column is redundant but is supplied for extra validation.

	[[nodiscard]]
	auto getPlayerNameForColumn( unsigned playerIndex, unsigned column ) const -> wsw::StringView;
	[[nodiscard]]
	auto getPlayerClanForColumn( unsigned playerIndex, unsigned column ) const -> wsw::StringView;

	enum class UpdateFlags : unsigned {
		Players      = 1 << 0,  // Properties of players were updated
		Spectators   = 1 << 1,  // Properties of spectators were updated
		Teams        = 1 << 2,  // Teams (lists of players in team) were updated
		Challengers  = 1 << 3,  // Challengers or properties of challengers were updated
	};

	[[nodiscard]]
	auto checkAndGetUpdates( const RawData &currData, PlayerUpdatesList &playerUpdates,
						     TeamUpdatesList &teamUpdates ) -> std::optional<UpdateFlags>;
};

}

#endif