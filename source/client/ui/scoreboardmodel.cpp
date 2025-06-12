#include "scoreboardmodel.h"
#include <common/helpers/enumtokenmatcher.h>
#include <common/facilities/configvars.h>
#include <common/facilities/cvar.h>
#include "uisystem.h"
#include "local.h"

#include <QJsonObject>
#include <QMetaEnum>

#include <array>

using wsw::operator""_asView;

namespace wsw::ui {

// TODO: Can't we just reuse QMetaEnum instead of declaring these matches manually?
// For now, we can limit themselves to existing solutions.
// The mentioned option should be considered if future additions of such vars are planned.

class LayoutMatcher : public wsw::EnumTokenMatcher<ScoreboardModelProxy::Layout, LayoutMatcher> {
public:
	LayoutMatcher() : wsw::EnumTokenMatcher<ScoreboardModelProxy::Layout, LayoutMatcher>( {
		{ "SideBySide"_asView, ScoreboardModelProxy::SideBySide },
		{ "ColumnWise"_asView, ScoreboardModelProxy::ColumnWise },
		{ "Mixed"_asView, ScoreboardModelProxy::Mixed },
	}) {}
};

class TableStyleMatcher : public wsw::EnumTokenMatcher<ScoreboardModelProxy::TableStyle, TableStyleMatcher> {
public:
	TableStyleMatcher() : wsw::EnumTokenMatcher<ScoreboardModelProxy::TableStyle, TableStyleMatcher>( {
		{ "Checkerboard"_asView, ScoreboardModelProxy::Checkerboard },
		{ "RowStripes"_asView, ScoreboardModelProxy::RowStripes },
		{ "ColumnStripes"_asView, ScoreboardModelProxy::ColumnStripes },
		{ "Flat"_asView, ScoreboardModelProxy::Flat },
	}) {}
};

static ::EnumValueConfigVar<ScoreboardModelProxy::Layout, LayoutMatcher> v_layout { "scb_layout"_asView, {
	.byDefault = ScoreboardModelProxy::SideBySide, .flags = CVAR_ARCHIVE },
};
static ::EnumValueConfigVar<ScoreboardModelProxy::TableStyle, TableStyleMatcher> v_tableStyle { "scb_tableStyle"_asView, {
	.byDefault = ScoreboardModelProxy::Checkerboard, .flags = CVAR_ARCHIVE },
};

[[nodiscard]]
static inline auto formatGlyph( int codePoint ) -> QChar {
	// Only the Unicode BMP is supported as we limit transmitted values to short
	assert( (unsigned)codePoint < (unsigned)std::numeric_limits<uint16_t>::max() );
	QChar ch( (uint16_t)codePoint );
	return ch.isPrint() ? ch : QChar();
}

[[nodiscard]]
static inline auto formatStatus( int value ) -> QVariant {
	if( value < 32 ) {
		return value;
	}
	return formatGlyph( value );
}

auto ScoreboardTeamModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_proxy->m_playerIndicesForLists[m_teamListIndex].size();
}

auto ScoreboardTeamModel::columnCount( const QModelIndex & ) const -> int {
	return (int)m_proxy->m_scoreboard.getColumnsCount();
}

auto ScoreboardTeamModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Kind, "kind" }, { Value, "value" }, { IsCompact, "isCompact" }, { IsGhosting, "isGhosting" } };
}

auto ScoreboardTeamModel::data( const QModelIndex &modelIndex, int role ) const -> QVariant {
	if( !modelIndex.isValid() ) {
		return QVariant();
	}
	const auto &scb = m_proxy->m_scoreboard;
	const auto &indices = m_proxy->m_playerIndicesForLists;
	const auto column = (unsigned)modelIndex.column();
	if( column >= scb.getColumnsCount() ) {
		return QVariant();
	}
	const auto row = (unsigned)modelIndex.row();
	if( row >= indices[m_teamListIndex].size() ) {
		return QVariant();
	}
	if( role == Kind ) {
		return scb.getColumnKind( column );
	}
	const auto playerIndex = indices[m_teamListIndex][row];
	if( role == IsGhosting ) {
		return scb.isPlayerGhosting( playerIndex );
	}
	if( role == IsCompact ) {
		unsigned columnsLeftInCurrentSpan = 0;
		for( unsigned testedColumn = 0; testedColumn <= column; testedColumn++ ) {
			if( testedColumn == column ) {
				return columnsLeftInCurrentSpan || scb.getTitleColumnSpan( testedColumn ) > 1;
			} else {
				if( const unsigned span = scb.getTitleColumnSpan( testedColumn ); span > 1 ) {
					columnsLeftInCurrentSpan = span;
				}
				if( columnsLeftInCurrentSpan ) {
					columnsLeftInCurrentSpan--;
				}
			}
		}
	}
	if( role != Value ) {
		return QVariant();
	}
	// TODO: This is awkward a bit
	switch( scb.getColumnKind( column ) ) {
		case Nickname: return toStyledText( scb.getPlayerNameForColumn( playerIndex, column ) );
		case Clan: return toStyledText( scb.getPlayerClanForColumn( playerIndex, column ) );
		case Score: return scb.getPlayerScoreForColumn( playerIndex, column );
		case Status: return formatStatus( scb.getPlayerStatusForColumn( playerIndex, column ) );
		case Ping: return formatPing( scb.getPlayerPingForColumn( playerIndex, column ) );
		case Number: return scb.getPlayerNumberForColumn( playerIndex, column );
		case Glyph: return formatGlyph( scb.getPlayerGlyphForColumn( playerIndex, column ) );
		case Icon: return scb.getPlayerIconForColumn( playerIndex, column );
	}
	wsw::failWithLogicError( "Unreachable" );
}

auto ScoreboardSpecsModelData::asQmlArray() const -> QJsonArray {
	if( m_isMarkedAsUpdated ) {
		m_isMarkedAsUpdated = false;
		m_cachedArrayData = QJsonArray();
		const auto &scoreboard = m_proxy->m_scoreboard;
		if( scoreboard.hasPing() ) {
			// TODO: allow specifying formatPing() result type
			for( unsigned playerIndex: *m_indices ) {
				m_cachedArrayData.append( QJsonObject {
					{ "name", toStyledText( scoreboard.getPlayerName( playerIndex ) ) },
					{ "ping", QString::fromLocal8Bit( formatPing( scoreboard.getPlayerPing( playerIndex ) ) ) }
				});
			}
		} else {
			for( unsigned playerIndex: *m_indices ) {
				const QString zeroPingString( QString::fromLocal8Bit( formatPing( 0 ) ) );
				m_cachedArrayData.append( QJsonObject {
					{ "name", toStyledText( scoreboard.getPlayerName( playerIndex ) ) },
					{ "ping", zeroPingString }
				});
			}
		}
	}
	return m_cachedArrayData;
}

void ScoreboardAccuracyData::update( const AccuracyRows &accuracyRows ) {
	wsw::StaticVector<Entry, kNumAccuracySlots> newData;
	for( unsigned i = 0; i < kNumAccuracySlots; ++i ) {
		const uint8_t strong = accuracyRows.strong[i], weak = accuracyRows.weak[i];
		if( strong + weak ) {
			newData.emplace_back( Entry { .weapon = (uint8_t)( i + WEAP_GUNBLADE ), .weak = weak, .strong = strong } );
		}
	}
	m_isMarkedAsUpdated = false;
	if( m_trackedData.size() != newData.size() ) {
		m_isMarkedAsUpdated = true;
	} else {
		if( !std::equal( m_trackedData.cbegin(), m_trackedData.cend(), newData.cbegin() ) ) {
			m_isMarkedAsUpdated = true;
		}
	}
	if( m_isMarkedAsUpdated ) {
		m_trackedData.clear();
		m_trackedData.insert( m_trackedData.end(), newData.begin(), newData.end() );
	}
}

auto ScoreboardAccuracyData::asQmlArray() const -> QJsonArray {
	QJsonArray result;
	for( const Entry &entry: m_trackedData ) {
		result.append( QJsonObject {
			{ "weapon", entry.weapon }, { "weak", entry.weak }, { "strong", entry.strong }
		});
	}
	return result;
}

void ScoreboardModelProxy::reload() {
	m_scoreboard.reload();
	Q_EMIT schemaChanged();
}

auto ScoreboardModelProxy::getColumnKind( int column ) const -> int {
	return (int)m_scoreboard.getColumnKind( (unsigned)column );
}

auto ScoreboardModelProxy::getTitleColumnSpan( int column ) const -> int {
	return (int)m_scoreboard.getTitleColumnSpan( (unsigned)column );
}

auto ScoreboardModelProxy::getColumnTitle( int column ) const -> QByteArray {
	const wsw::StringView title( m_scoreboard.getColumnTitle( (unsigned)column ) );
	return !title.empty() ? QByteArray( title.data(), title.size() ) : QByteArray();
}

auto ScoreboardModelProxy::getImageAssetPath( int asset ) const -> QByteArray {
	if( auto maybePath = m_scoreboard.getImageAssetPath( (unsigned)asset ) ) {
		return QByteArray( "image://wsw/" ) + QByteArray( maybePath->data(), maybePath->size() );
	}
	return QByteArray();
}

bool ScoreboardModelProxy::isMixedListRowAlpha( int row ) const {
	const auto &nums = std::end( m_playerIndicesForLists )[-1];
	assert( (unsigned)row < nums.size() );
	return m_scoreboard.getPlayerTeam( nums[row] ) == TEAM_ALPHA;
}

ScoreboardModelProxy::ScoreboardModelProxy()
	: m_layoutModificationTracker( &v_layout ), m_tableStyleModificationTracker( &v_tableStyle ) {
	new( m_teamModelsHolder.unsafe_grow_back() )ScoreboardTeamModel( this, TEAM_PLAYERS );
	new( m_teamModelsHolder.unsafe_grow_back() )ScoreboardTeamModel( this, TEAM_ALPHA );
	new( m_teamModelsHolder.unsafe_grow_back() )ScoreboardTeamModel( this, TEAM_BETA );
	new( m_teamModelsHolder.unsafe_grow_back() )ScoreboardTeamModel( this, TEAM_BETA + 1 );

	checkVars();
}

void ScoreboardModelProxy::dispatchPlayerRowUpdates( const PlayerUpdates &updates, int team,
													 int rowInTeam, std::optional<int> rowInMixedList ) {
	assert( ( team != TEAM_PLAYERS ) == ( rowInMixedList != std::nullopt ) );
	assert( team >= TEAM_PLAYERS && team <= TEAM_BETA );
	QAbstractTableModel *const teamModel = &m_teamModelsHolder[team - 1];
	QAbstractTableModel *const mixedModel = ( team != TEAM_PLAYERS ) ? m_teamModelsHolder.end() - 1 : nullptr;
	if( updates.ghosting ) {
		const QVector<int> *changedRoles = &ScoreboardTeamModel::kGhostingRoleAsVector;
		if( updates.nickname | updates.clan | updates.score | updates.shortSlotsMask ) {
			changedRoles = &ScoreboardTeamModel::kValueAndGhostingRolesAsVector;
		}

		// We have to force redrawing of each cell upon ghosting status change
		for( unsigned i = 0; i < m_scoreboard.getColumnsCount(); ++i ) {
			QModelIndex teamModelIndex( teamModel->index( rowInTeam, (int)i ) );
			teamModel->dataChanged( teamModelIndex, teamModelIndex, *changedRoles );
			if( mixedModel ) {
				QModelIndex mixedModelIndex( mixedModel->index( *rowInMixedList, (int)i ) );
				mixedModel->dataChanged( mixedModelIndex, mixedModelIndex, *changedRoles );
			}
		}
		return;
	}

	assert( !updates.ghosting );
	const QVector<int> &changedRoles = ScoreboardTeamModel::kValueRoleAsVector;
	for( unsigned i = 0; i < m_scoreboard.getColumnsCount(); ++i ) {
		// Check whether the table cell really needs updating
		const auto kind = m_scoreboard.getColumnKind( i );
		if( kind >= Status ) {
			assert( kind == Status || kind == Ping || kind == Number || kind == Glyph || kind == Icon );
			const unsigned slotBit = 1u << m_scoreboard.getColumnSlot( i );
			if( !( slotBit & (unsigned)updates.shortSlotsMask ) ) {
				continue;
			}
		} else {
			if( kind == Nickname ) {
				if( !updates.nickname ) {
					continue;
				}
			} else if( kind == Clan ) {
				if( !updates.clan ) {
					continue;
				}
			} else if( kind == Score ) {
				if( !updates.score ) {
					continue;
				}
			} else {
				assert( 0 && "Unreachable" );
			}
		}

		QModelIndex teamModelIndex( teamModel->index( rowInTeam, (int)i ) );
		teamModel->dataChanged( teamModelIndex, teamModelIndex, changedRoles );
		if( mixedModel ) {
			QModelIndex mixedModelIndex( mixedModel->index( *rowInMixedList, (int)i ) );
			mixedModel->dataChanged( mixedModelIndex, mixedModelIndex, changedRoles );
		}
	}
}

void ScoreboardModelProxy::checkVars() {
	if( m_layoutModificationTracker.checkAndReset() ) {
		if( const Layout newLayout = v_layout.get(); newLayout != m_layout ) {
			m_layout = newLayout;
			Q_EMIT layoutChanged( newLayout );
		}
	}
	if( m_tableStyleModificationTracker.checkAndReset() ) {
		if( const TableStyle newTableStyle = v_tableStyle.get(); newTableStyle != m_tableStyle ) {
			m_tableStyle = newTableStyle;
			Q_EMIT tableStyleChanged( m_tableStyle );
		}
	}
}

void ScoreboardModelProxy::update( const ReplicatedScoreboardData &currData, const AccuracyRows &accuracyRows ) {
	checkVars();

	m_accuracyModel.update( accuracyRows );
	if( m_accuracyModel.isMarkedAsUpdated() ) {
		Q_EMIT accuracyModelChanged();
	}

	Scoreboard::PlayerUpdatesList playerUpdates;
	Scoreboard::TeamUpdatesList teamUpdates;
	const auto maybeUpdateFlags = m_scoreboard.checkAndGetUpdates( currData, playerUpdates, teamUpdates );
	if( !maybeUpdateFlags ) {
		return;
	}

	for( auto &indices: m_playerIndicesForLists ) {
		indices.clear();
	}

	// We should update player nums first so fully reset models get a correct data from the very beginning

	static_assert( TEAM_SPECTATOR == 0 && TEAM_PLAYERS == 1 && TEAM_ALPHA == 2 && TEAM_BETA == 3 );
	using TableOfRowsInTeam = std::array<uint8_t, kMaxPlayers>;
	alignas( 16 ) TableOfRowsInTeam tablesOfRowsInTeams[5];

	for( unsigned playerIndex = 0; playerIndex < kMaxPlayers; ++playerIndex ) {
		if( m_scoreboard.isPlayerConnected( playerIndex ) ) {
			const auto teamNum                         = m_scoreboard.getPlayerTeam( playerIndex );
			auto &indicesForTeam                       = m_playerIndicesForLists[teamNum];
			auto &playerIndicesToRowsInTeamList        = tablesOfRowsInTeams[teamNum];
			playerIndicesToRowsInTeamList[playerIndex] = (uint8_t)indicesForTeam.size();
			indicesForTeam.push_back( playerIndex );
			if( teamNum == TEAM_ALPHA || teamNum == TEAM_BETA ) {
				auto &indicesForMixedList                   = m_playerIndicesForLists[TEAM_BETA + 1];
				auto &playerIndicesToRowsInMixedList        = tablesOfRowsInTeams[TEAM_BETA + 1];
				playerIndicesToRowsInMixedList[playerIndex] = (uint8_t)indicesForMixedList.size();
				indicesForMixedList.push_back( playerIndex );
			}
		}
	}

	const bool mustResetChallengers = (unsigned)*maybeUpdateFlags & (unsigned)Scoreboard::UpdateFlags::Challengers;
	const bool mustResetSpectators  = (unsigned)*maybeUpdateFlags & (unsigned)Scoreboard::UpdateFlags::Spectators;
	if( mustResetChallengers ) {
		alignas( 16 ) unsigned clientIndices[kMaxPlayers];
		std::fill( std::begin( clientIndices ), std::end( clientIndices ), ~0u );
		for( unsigned playerIndex = 0; playerIndex < kMaxPlayers; ++playerIndex ) {
			if( m_scoreboard.isPlayerConnected( playerIndex ) ) {
				const unsigned clientNum = m_scoreboard.getPlayerNum( playerIndex );
				clientIndices[clientNum] = playerIndex;
			}
		}
		if( mustResetChallengers ) {
			m_challengers.clear();
			for( unsigned i = 0; i < (unsigned)MAX_CLIENTS; ++i ) {
				if( const auto maybeClientNum = m_scoreboard.getClientNumOfChallenger( i ) ) {
					m_challengers.push_back( clientIndices[*maybeClientNum] );
				} else {
					break;
				}
			}
			m_challengersModel.markAsUpdated();
		}
	}

	if( mustResetSpectators ) {
		m_specsModel.markAsUpdated();
	}

	bool wasTeamReset[4] { false, false, false, false };

	for( const Scoreboard::TeamUpdates &teamUpdate: teamUpdates ) {
		// TODO: Handle other team updates (not only player changes) as well
		if( teamUpdate.players ) {
			assert( teamUpdate.team >= TEAM_SPECTATOR && teamUpdate.team <= TEAM_BETA );
			wasTeamReset[teamUpdate.team] = true;
			if( teamUpdate.team == TEAM_SPECTATOR ) {
				m_specsModel.markAsUpdated();
			} else {
				auto &model = m_teamModelsHolder[teamUpdate.team - 1];
				// Forcing a full reset is the easiest approach.
				model.beginResetModel();
				model.endResetModel();
			}
		}
	}

	const bool wasMixedListReset = wasTeamReset[TEAM_ALPHA] | wasTeamReset[TEAM_BETA];
	if( wasMixedListReset ) {
		auto &model = std::end( m_teamModelsHolder )[-1];
		model.beginResetModel();
		model.endResetModel();
	}

	for( const Scoreboard::PlayerUpdates &playerUpdate: playerUpdates ) {
		const unsigned playerIndex = playerUpdate.playerIndex;
		if( m_scoreboard.isPlayerConnected( playerIndex ) ) {
			const auto teamNum = (int)m_scoreboard.getPlayerTeam( playerIndex );
			if( !wasTeamReset[teamNum] ) {
				// Spectators use the simplified model along with challengers
				if( teamNum == TEAM_SPECTATOR ) {
					m_specsModel.markAsUpdated();
				} else {
					const auto &playerIndexToRowInTeamList = tablesOfRowsInTeams[teamNum];
					const auto rowInTeamList               = (int)playerIndexToRowInTeamList[playerIndex];
					assert( (unsigned)rowInTeamList < (unsigned)m_playerIndicesForLists[teamNum].size() );
					std::optional<int> rowInMixedList;
					if( teamNum == TEAM_ALPHA || teamNum == TEAM_BETA ) {
						const auto &playerIndexToRowInMixedList = tablesOfRowsInTeams[TEAM_BETA + 1];
						rowInMixedList                          = (int)playerIndexToRowInMixedList[playerIndex];
						assert( (unsigned)*rowInMixedList < (unsigned)m_playerIndicesForLists[TEAM_BETA + 1].size() );
					}
					dispatchPlayerRowUpdates( playerUpdate, teamNum, rowInTeamList, rowInMixedList );
				}
			}
		}
	}

	static_assert( std::size( wasTeamReset ) == 4 );
	for( int i = 0; i < 4; ++i ) {
		if( wasTeamReset[i] ) {
			if( i != TEAM_SPECTATOR ) {
				Q_EMIT teamReset( i );
			}
		}
	}
	if( wasMixedListReset ) {
		Q_EMIT teamReset( 4 );
	}

	if( m_specsModel.isMarkedAsUpdated() ) {
		Q_EMIT specsModelChanged();
	}
	if( m_challengersModel.isMarkedAsUpdated() ) {
		Q_EMIT challengersModelChanged();
	}
}

}
