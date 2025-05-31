#include "serverlistmodel.h"
#include "local.h"
#include <QJsonObject>
#include <QJsonArray>

namespace wsw::ui {

auto ServerListModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ ServerName, "serverName" },
		{ MapName, "mapName" },
		{ Gametype, "gametype" },
		{ Address, "address" },
		{ NumPlayers, "numPlayers" },
		{ MaxPlayers, "maxPlayers" },
		{ TimeMinutes, "timeMinutes" },
		{ TimeSeconds, "timeSeconds" },
		{ TimeFlags, "timeFlags" },
		{ AlphaTeamName, "alphaTeamName" },
		{ BetaTeamName, "betaTeamName" },
		{ AlphaTeamScore, "alphaTeamScore" },
		{ BetaTeamScore, "betaTeamScore" },
		{ AlphaTeamList, "alphaTeamList" },
		{ BetaTeamList, "betaTeamList" },
		{ PlayersTeamList, "playersTeamList" },
		{ SpectatorsList, "spectatorsList" }
	};
}

auto ServerListModel::rowCount( const QModelIndex & ) const -> int {
	int size = m_servers.size();
	if( size % 2 ) {
		size += 1;
	}
	return size / 2;
}

auto ServerListModel::columnCount( const QModelIndex & ) const -> int {
	return 2;
}

void ServerListModel::clear() {
	beginResetModel();
	m_servers.clear();
	endResetModel();
	Q_EMIT wasReset();
}

auto ServerListModel::getServerAtIndex( int index ) const -> const PolledGameServer * {
	if( (unsigned)index < m_servers.size() ) {
		return m_servers[index];
	}
	return nullptr;
}

auto ServerListModel::findIndexOfServer( const PolledGameServer *server ) const -> std::optional<unsigned> {
	for( unsigned i = 0; i < m_servers.size(); ++i ) {
		if( m_servers[i] == server ) {
			return i;
		}
	}
	return std::nullopt;
}

auto ServerListModel::data( const QModelIndex &modelIndex, int role ) const -> QVariant {
	const int arrayIndex = modelIndex.row() * 2 + modelIndex.column();
	const auto *const server = getServerAtIndex( arrayIndex );
	if( !server ) {
		if( arrayIndex != (int)m_servers.size() ) {
			wsw::failWithLogicError( "Attempt to retrieve a data out of bounds" );
		}
		// Allow accessing the bottom-right empty cell
		return QVariant();
	}

	switch( role ) {
		case ServerName: return toStyledText( server->getServerName() );
		case MapName: return toStyledText( server->getMapName() );
		case Gametype: return toStyledText( server->getGametype() );
		case Address: return QVariant( QString::fromLatin1( NET_AddressToString( &server->getAddress() ) ) );
		case Ping: return QVariant();
		case NumPlayers: return server->getNumClients();
		case MaxPlayers: return server->getMaxClients();
		case TimeMinutes: return server->getTime().timeMinutes;
		case TimeSeconds: return server->getTime().timeSeconds;
		case TimeFlags: return toMatchTimeFlags( server->getTime() );
		case AlphaTeamName: return toStyledText( server->getAlphaName() );
		case BetaTeamName: return toStyledText( server->getBetaName() );
		case AlphaTeamScore: return server->getAlphaScore();
		case BetaTeamScore: return server->getBetaScore();
		case AlphaTeamList: return toQmlTeamList( server->getAlphaTeam().first );
		case BetaTeamList: return toQmlTeamList( server->getBetaTeam().first );
		case PlayersTeamList: return toQmlTeamList( server->getPlayersTeam().first );
		case SpectatorsList: return toQmlTeamList( server->getSpectators().first );
		default: return QVariant();
	}
}

void ServerListModel::onServerAdded( const PolledGameServer *server ) {
	if( findIndexOfServer( server ) ) {
		wsw::failWithLogicError( "The server is already present" );
	}

	const auto serversCount = (int)m_servers.size();
	if( serversCount % 2 ) {
		m_servers.push_back( server );
		const QModelIndex modelIndex( index( rowCount( QModelIndex() ), 1 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex );
		return;
	}

	const auto newRowCount = ( serversCount + 1 ) / 2;
	beginInsertRows( QModelIndex(), newRowCount, newRowCount );
	m_servers.push_back( server );
	endInsertRows();
}

void ServerListModel::onServerRemoved( const PolledGameServer *server ) {
	const auto maybeServerIndex = findIndexOfServer( server );
	if( !maybeServerIndex ) {
		wsw::failWithLogicError( "Failed to find the server for removal" );
	}

	// Swap with the last row.
	// The QSortFilterProxy model that we're going to use
	// should help to preserve the desired order regardless of that.
	std::swap( m_servers[*maybeServerIndex], m_servers.back() );

	// TODO: Optimize dispatching updates (specify the rectangle in a QModelIndex() constructor)
	assert( !m_servers.empty() );
	for( unsigned i = *maybeServerIndex; i < m_servers.size() - 1u; ++i ) {
		const auto row = (int)i / 2;
		const auto column = (int)i % 2;
		const QModelIndex modelIndex( index( row, column ) );
		Q_EMIT dataChanged( modelIndex, modelIndex );
	}

	const auto oldLastRow = (int)m_servers.size() / 2;
	// If there's going to be a row removal
	if( m_servers.size() % 2 ) {
		beginRemoveRows( QModelIndex(), oldLastRow, oldLastRow );
		m_servers.pop_back();
		endRemoveRows();
	} else {
		m_servers.pop_back();
		// Make the bottom-right cell reflect it's now-empty status
		const QModelIndex modelIndex( index( oldLastRow, 1 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex );
	}
}

void ServerListModel::onServerUpdated( const PolledGameServer *server ) {
	const auto maybeServerIndex = findIndexOfServer( server );
	if( !maybeServerIndex ) {
		wsw::failWithLogicError( "Failed to find the server for update" );
	}

	const auto row = (int)*maybeServerIndex / 2;
	const auto column = (int)*maybeServerIndex % 2;

	const QModelIndex modelIndex( index( row, column ) );
	Q_EMIT dataChanged( modelIndex, modelIndex );
}

auto ServerListModel::toQmlTeamList( const PlayerInfo *playerInfoHead ) -> QVariant {
	if( !playerInfoHead ) {
		return QVariant();
	}

	QJsonArray result;
	for( const auto *info = playerInfoHead; info; info = info->next ) {
		QJsonObject obj {
			{ "name", toStyledText( info->getName() ) },
			{ "ping", info->getPing() },
			{ "score", info->getScore() }
		};
		result.append( obj );
	}

	return result;
}

auto ServerListModel::toMatchTimeFlags( const MatchTime &time ) -> int {
	int flags = 0;
	// TODO: Parse match time flags as an enum?
	flags |= time.isWarmup ? Warmup : 0;
	flags |= time.isCountdown ? Countdown : 0;
	flags |= time.isFinished ? Finished : 0;
	flags |= time.isOvertime ? Overtime: 0;
	flags |= time.isSuddenDeath ? SuddenDeath : 0;
	flags |= time.isTimeout ? Timeout : 0;
	return flags;
}

}