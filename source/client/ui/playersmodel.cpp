#include "playersmodel.h"
#include "local.h"
#include <common/wswalgorithm.h>

namespace wsw::ui {

auto PlayersModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Number, "number" },
		{ Nickname, "nickname" }
	};
}

auto PlayersModel::rowCount( const QModelIndex & ) const -> int {
	return m_players.size();
}

auto PlayersModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		const auto row = index.row();
		if( (unsigned)row < (unsigned)m_players.size() ) {
			switch( role ) {
				case Number: return m_players[row].playerNum;
				case Nickname: {
					const auto [off, len] = m_players[row].nameSpan;
					return toStyledText( m_oldNameData.asView().drop( off ).take( len ) );
				}
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

void PlayersModel::update( const ReplicatedScoreboardData &scoreboardData ) {
	wsw::StaticVector<unsigned, MAX_CLIENTS> currPlayerNums;
	for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
		if( scoreboardData.isPlayerConnected( i ) ) {
			currPlayerNums.push_back( scoreboardData.getPlayerNum( i ) );
		}
	}

	wsw::sortPodNonSpeedCritical( currPlayerNums.begin(), currPlayerNums.end() );

	wsw::StaticVector<unsigned, MAX_CLIENTS> modifiedRows;
	const bool fullReset = currPlayerNums.size() != m_players.size();
	if( !fullReset ) {
		for( unsigned i = 0; i < currPlayerNums.size(); ++i ) {
			const auto [oldNameSpan, oldPlayerNum] = m_players[i];
			const auto currPlayerNum = currPlayerNums[i];
			if( oldPlayerNum != currPlayerNum ) {
				modifiedRows.push_back( i );
				continue;
			}
			const auto [off, len] = oldNameSpan;
			const wsw::StringView oldName( m_oldNameData.asView().drop( off ).take( len ) );
			const wsw::StringView currName( CG_PlayerName( currPlayerNum ) );
			if( !oldName.equals( currName ) ) {
				modifiedRows.push_back( i );
			}
		}
	}

	// Update the data before dispatching signals
	m_players.clear();
	m_oldNameData.clear();
	for( unsigned playerNum: currPlayerNums ) {
		const wsw::StringView name( CG_PlayerName( playerNum ) );
		const auto off = (unsigned)m_oldNameData.size();
		m_oldNameData.append( name );
		m_players.push_back( { { off, name.length() }, playerNum } );
	}

	if( fullReset ) {
		beginResetModel();
		endResetModel();
	} else {
		for( const unsigned row: modifiedRows ) {
			QModelIndex modelIndex( index( (int)row ) );
			Q_EMIT dataChanged( modelIndex, modelIndex );
		}
	}
}

}