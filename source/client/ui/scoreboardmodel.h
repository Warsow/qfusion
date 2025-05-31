#ifndef WSW_95e07a0d_69fa_4239_9546_1fa5a5580f55_H
#define WSW_95e07a0d_69fa_4239_9546_1fa5a5580f55_H

#include "scoreboard.h"
#include <common/configvars.h>
#include <common/gs_public.h>

#include <QAbstractListModel>
#include <QJsonArray>
#include <QObject>

#include <span>

struct ReplicatedScoreboardData;
struct AccuracyRows;

namespace wsw::ui {

class ScoreboardModelProxy;

class ScoreboardTeamModel : public QAbstractTableModel, ScoreboardShared {
	Q_OBJECT

	friend class ScoreboardModelProxy;

	ScoreboardModelProxy *const m_proxy;
	const int m_teamListIndex;

	enum Role {
		Kind = Qt::UserRole + 1,
		Value,
		IsCompact,
		IsGhosting
	};

	ScoreboardTeamModel( ScoreboardModelProxy *proxy, int teamListIndex )
		: m_proxy( proxy ), m_teamListIndex( teamListIndex ) {}

	static inline QVector<int> kValueRoleAsVector { Value };
	static inline QVector<int> kGhostingRoleAsVector { IsGhosting };
	static inline QVector<int> kValueAndGhostingRolesAsVector { Value, IsGhosting };

	Q_PROPERTY( int teamTag MEMBER m_teamListIndex CONSTANT )

	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto columnCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto data( const QModelIndex &modelIndex, int role ) const -> QVariant override;
};

class ScoreboardSpecsModelData {
	mutable QJsonArray m_cachedArrayData;
	ScoreboardModelProxy *const m_proxy;
	StaticVector<unsigned, MAX_CLIENTS> *m_indices;
	mutable bool m_isMarkedAsUpdated { false };
public:
	ScoreboardSpecsModelData( ScoreboardModelProxy *proxy, StaticVector<unsigned, MAX_CLIENTS> *indices )
		: m_proxy( proxy ), m_indices( indices ) {}

	void markAsUpdated() { m_isMarkedAsUpdated = true; }
	[[nodiscard]]
	bool isMarkedAsUpdated() const { return m_isMarkedAsUpdated; }
	[[nodiscard]]
	auto asQmlArray() const -> QJsonArray;
};

class ScoreboardAccuracyData {
	mutable bool m_isMarkedAsUpdated { false };

	struct Entry {
		uint8_t weapon, weak, strong;
		[[nodiscard]]
		bool operator==( const Entry &that ) const {
			return weapon == that.weapon && weak == that.weak && strong == that.strong;
		}
	};

	StaticVector<Entry, kNumAccuracySlots> m_trackedData;

public:
	void update( const AccuracyRows &accuracyRows );
	[[nodiscard]]
	bool isMarkedAsUpdated() const { return m_isMarkedAsUpdated; }
	[[nodiscard]]
	auto asQmlArray() const -> QJsonArray;
};

class ScoreboardModelProxy : public QObject, ScoreboardShared {
	Q_OBJECT

	friend class ScoreboardTeamModel;
	friend class ScoreboardSpecsModelData;
public:
	enum Layout {
		SideBySide,
		ColumnWise,
		Mixed
	};
	Q_ENUM( Layout );
	enum TableStyle {
		Checkerboard,
		RowStripes,
		ColumnStripes,
		Flat
	};
	Q_ENUM( TableStyle );

	Scoreboard m_scoreboard;

	// Can't declare a plain array due to the type being noncopyable and we don't want to use a dynamic allocation.
	StaticVector<ScoreboardTeamModel, 4> m_teamModelsHolder;
	ScoreboardSpecsModelData m_specsModel { this, &m_playerIndicesForLists[TEAM_SPECTATOR] };
	ScoreboardSpecsModelData m_challengersModel { this, &m_challengers };

	ScoreboardAccuracyData m_accuracyModel;

	Layout m_layout { SideBySide };
	TableStyle m_tableStyle { Checkerboard };

	VarModificationTracker m_layoutModificationTracker;
	VarModificationTracker m_tableStyleModificationTracker;

	void checkVars();

	wsw::StaticVector<unsigned, MAX_CLIENTS> m_playerIndicesForLists[5];
	wsw::StaticVector<unsigned, MAX_CLIENTS> m_challengers;

	using PlayerUpdates = Scoreboard::PlayerUpdates;

	void dispatchPlayerRowUpdates( const PlayerUpdates &updates, int team, int rowInTeam, std::optional<int> rowInMixedList );
public:
	enum class QmlColumnKind {
		Nickname,
		Clan,
		Score,
		Status,
		Ping,
		Number,
		Glyph,
		Icon
	};
	Q_ENUM( QmlColumnKind );
	static_assert( (int)QmlColumnKind::Nickname == (int)Nickname );
	static_assert( (int)QmlColumnKind::Clan == (int)Clan );
	static_assert( (int)QmlColumnKind::Score == (int)Score );
	static_assert( (int)QmlColumnKind::Status == (int)Status );
	static_assert( (int)QmlColumnKind::Ping == (int)Ping );
	static_assert( (int)QmlColumnKind::Number == (int)Number );
	static_assert( (int)QmlColumnKind::Glyph == (int)Glyph );
	static_assert( (int)QmlColumnKind::Icon == (int)Icon );

	ScoreboardModelProxy();

	Q_SIGNAL void teamReset( int resetTeamTag );

	[[nodiscard]]
	Q_INVOKABLE int getColumnKind( int column ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getColumnTitle( int column ) const;
	[[nodiscard]]
	Q_INVOKABLE int getTitleColumnSpan( int column ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getImageAssetPath( int asset ) const;
	[[nodiscard]]
	Q_INVOKABLE bool isMixedListRowAlpha( int row ) const;
	[[nodiscard]]
	Q_INVOKABLE int getColumnsCount() const { return m_scoreboard.getColumnsCount(); }

	Q_SIGNAL void schemaChanged();

	Q_SIGNAL void layoutChanged( Layout layout );
	Q_PROPERTY( Layout layout MEMBER m_layout NOTIFY layoutChanged );
	Q_SIGNAL void tableStyleChanged( TableStyle tableStyle );
	Q_PROPERTY( TableStyle tableStyle MEMBER m_tableStyle NOTIFY tableStyleChanged );

	Q_SIGNAL void specsModelChanged();
	Q_PROPERTY( QJsonArray specsModel READ getSpecsModel NOTIFY specsModelChanged );
	Q_SIGNAL void challengersModelChanged();
	Q_PROPERTY( QJsonArray challengersModel READ getChallengersModel NOTIFY challengersModelChanged );

	Q_SIGNAL void accuracyModelChanged();
	Q_PROPERTY( QJsonArray accuracyModel READ getAccuracyModel NOTIFY accuracyModelChanged );

	[[nodiscard]]
	auto getSpecsModel() -> QJsonArray { return m_specsModel.asQmlArray(); }
	[[nodiscard]]
	auto getChallengersModel() -> QJsonArray { return m_challengersModel.asQmlArray(); }

	[[nodiscard]]
	auto getAccuracyModel() -> QJsonArray { return m_accuracyModel.asQmlArray(); }

	[[nodiscard]]
	auto getPlayersModel() -> ScoreboardTeamModel * { return &m_teamModelsHolder[0]; }
	[[nodiscard]]
	auto getAlphaModel() -> ScoreboardTeamModel * { return &m_teamModelsHolder[1]; }
	[[nodiscard]]
	auto getBetaModel() -> ScoreboardTeamModel * { return &m_teamModelsHolder[2]; }
	[[nodiscard]]
	auto getMixedModel() -> ScoreboardTeamModel * { return &m_teamModelsHolder[3]; }

	void reload();
	void update( const ReplicatedScoreboardData &currData, const AccuracyRows &accuracyRows );
};

}

#endif
