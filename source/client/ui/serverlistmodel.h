#ifndef WSW_b8181ad3_2c0a_441c_86e9_6c3241ea0535_H
#define WSW_b8181ad3_2c0a_441c_86e9_6c3241ea0535_H

#include <client/serverlist.h>
#include <common/types/podvector.h>
#include <QAbstractTableModel>

namespace wsw::ui {

class ServerListModel : public QAbstractTableModel, public ServerListListener {
	Q_OBJECT

	enum Role {
		ServerName = Qt::UserRole + 1,
		MapName,
		Gametype,
		Address,
		Ping,
		NumPlayers,
		MaxPlayers,
		TimeMinutes,
		TimeSeconds,
		TimeFlags,
		AlphaTeamName,
		BetaTeamName,
		AlphaTeamScore,
		BetaTeamScore,
		AlphaTeamList,
		BetaTeamList,
		PlayersTeamList,
		SpectatorsList
	};

	wsw::PodVector<const PolledGameServer *> m_servers;

	[[nodiscard]]
	auto getServerAtIndex( int index ) const -> const PolledGameServer *;

	[[nodiscard]]
	auto findIndexOfServer( const PolledGameServer *server ) const -> std::optional<unsigned>;

	[[nodiscard]]
	static auto toQmlTeamList( const PlayerInfo *playerInfoHead ) -> QVariant;
	[[nodiscard]]
	static auto toMatchTimeFlags( const MatchTime &time ) -> int;
public:
	enum MatchTimeFlags {
		Warmup          = 1 << 0,
		Countdown       = 1 << 1,
		Finished        = 1 << 2,
		Overtime        = 1 << 3,
		SuddenDeath     = 1 << 4,
		Timeout         = 1 << 5
	};
	Q_ENUM( MatchTimeFlags );

	Q_SIGNAL void wasReset();

 	[[nodiscard]]
 	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto columnCount( const QModelIndex & ) const -> int override;

	void clear();

	void onServerAdded( const PolledGameServer *server ) override;
	void onServerUpdated( const PolledGameServer *server ) override;
	void onServerRemoved( const PolledGameServer *server ) override;
};

}

#endif
