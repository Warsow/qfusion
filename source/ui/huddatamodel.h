#ifndef WSW_113c126b_3afd_470e_8b61_492a157d3d3d_H
#define WSW_113c126b_3afd_470e_8b61_492a157d3d3d_H

#include <QAbstractListModel>
#include <QColor>

#include <array>

#include "../common/configvars.h"
#include "../common/wswstaticvector.h"
#include "../common/wswstaticstring.h"
#include "hudlayoutmodel.h"
#include "cgameimports.h"

struct ReplicatedScoreboardData;
class StringConfigVar;

namespace wsw::ui {

class HudCommonDataModel;
class HudPovDataModel;

class InventoryModel : public QAbstractListModel {
	friend class HudPovDataModel;

	Q_OBJECT
public:
	InventoryModel();

	Q_PROPERTY( int numInventoryItems MEMBER kNumInventoryItems CONSTANT );
private:
	enum Role {
		Displayed = Qt::UserRole + 1,
		HasWeapon,
		Active,
		IconPath,
		Color,
		WeakAmmoCount,
		StrongAmmoCount
	};

	struct Entry {
		int weaponNum, weakCount, strongCount;
		bool displayed, hasWeapon, active;
	};

	static constexpr unsigned kNumInventoryItems { 10 };
	wsw::StaticVector<Entry, kNumInventoryItems> m_entries;
	QVector<int> m_changedRolesStorage;

	void checkPropertyChanges( unsigned viewStateIndex );

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

class TeamListModel : public QAbstractListModel {
	friend class HudPovDataModel;

	enum Role {
		Health,
		Armor,
		WeaponIconPath,
		Nickname,
		Powerups
	};

	struct Entry {
		unsigned playerNum;
		unsigned health { 0 }, armor { 0 };
		unsigned weapon { 0 };
		unsigned powerups { 0 };
	};

	using EntriesVector = wsw::StaticVector<Entry, 32>;

	EntriesVector m_oldEntries;
	EntriesVector m_entries;

	static inline const QVector<int> kHealthAsRole { Health };
	static inline const QVector<int> kArmorAsRole { Armor };
	static inline const QVector<int> kHealthAndArmorAsRole { Armor };
	static inline const QVector<int> kWeaponIconPathAsRole { WeaponIconPath };

	unsigned m_povPlayerNum { ~0u };
	int m_team { -1 };

	std::array<unsigned, MAX_CLIENTS> m_nicknameUpdateCounters;

	TeamListModel() {
		m_nicknameUpdateCounters.fill( 0 );
	}

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	void fillEntries( const ReplicatedScoreboardData &scoreboardData, EntriesVector &entries );
	void resetWithScoreboardData( const ReplicatedScoreboardData &scoreboardData );
	void saveNicknameUpdateCounters( const EntriesVector &entries );

	void update( const ReplicatedScoreboardData &scoreboardData, unsigned povPlayerNum );
};

class FragsFeedModel : public QAbstractListModel {
	friend class HudCommonDataModel;

	enum Role { Victim = Qt::UserRole + 1, Attacker, IconPath };

	struct Entry {
		int64_t timestamp;
		wsw::StaticString<32> victimName, attackerName;
		unsigned meansOfDeath { 0 };
		std::optional<int> victimTeamColor, attackerTeamColor;
	};

	HudCommonDataModel *const m_hudDataModel;

	explicit FragsFeedModel( HudCommonDataModel *hudDataModel ) : m_hudDataModel( hudDataModel ) {}

	// TODO: Use a circular buffer / StaticDeque (check whether it's really functional)?
	wsw::StaticVector<Entry, 4> m_entries;

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	[[nodiscard]]
	static auto toDisplayedName( const wsw::StringView &rawName, const std::optional<int> &teamColor ) -> QString;

	void addFrag( const std::pair<wsw::StringView, int> &victimAndTeam,
				  int64_t timestamp, unsigned meansOfDeath,
				  const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam );

	void reset();
	void update( int64_t currTime );
};

class MessageFeedModel : public QAbstractListModel {
	friend class HudPovDataModel;

	enum Role { Message = Qt::UserRole + 1 };

	struct Entry {
		int64_t timestamp;
		wsw::StaticString<1024> message;
	};

	static constexpr unsigned kMaxEntries = 4;

	Entry m_pendingEntries[kMaxEntries];
	unsigned m_numPendingEntries { 0 };
	unsigned m_pendingEntriesHead { 0 };
	unsigned m_pendingEntriesTail { 0 };

	bool m_isFadingOut { false };

	wsw::StaticVector<Entry, kMaxEntries> m_entries;

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	void addMessage( const wsw::StringView &message, int64_t timestamp );

	void update( int64_t currTime );
	void reset();

	[[nodiscard]]
	bool isFadingOut() const { return m_isFadingOut; }
};

class AwardsModel : public QAbstractListModel {
	friend class HudPovDataModel;

	// TODO: Add icons?
	enum Role { Message = Qt::UserRole + 1 };

	struct Entry {
		int64_t timestamp;
		wsw::StaticString<64> message;
	};

	wsw::StaticVector<Entry, 4> m_entries;
	wsw::PodVector<Entry> m_pendingEntries;

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &modelIndex, int role ) const -> QVariant override;

	void addAward( const wsw::StringView &award, int64_t timestamp );

	void update( int64_t currTime );
	void reset();
};

struct ObjectiveIndicatorState {
	Q_GADGET

public:
	BasicObjectiveIndicatorState m_underlying;

	ObjectiveIndicatorState() = default;
	explicit ObjectiveIndicatorState( const BasicObjectiveIndicatorState &underlying )  : m_underlying( underlying ) {}

	Q_PROPERTY( QColor color READ getColor );
	Q_PROPERTY( int anim READ getAnim );
	Q_PROPERTY( int progress READ getProgress );
	Q_PROPERTY( int iconNum READ getIconNum );
	Q_PROPERTY( int stringNum READ getStringNum );
	Q_PROPERTY( bool enabled READ getEnabled );

	[[nodiscard]]
	auto getColor() const -> QColor {
		return QColor::fromRgb( m_underlying.color[0], m_underlying.color[1], m_underlying.color[2] );
	}

	[[nodiscard]] auto getAnim() const -> int { return m_underlying.anim; }
	[[nodiscard]] auto getProgress() const -> int { return m_underlying.progress; }
	[[nodiscard]] auto getIconNum() const -> int { return m_underlying.iconNum; }
	[[nodiscard]] auto getStringNum() const -> int { return m_underlying.stringNum; }
	[[nodiscard]] bool getEnabled() const { return m_underlying.enabled; }

	[[nodiscard]]
	bool operator!=( const ObjectiveIndicatorState &that ) const { return m_underlying != that.m_underlying; }
};

class PerfDataRowModel : public QObject {
	Q_OBJECT
public:
	PerfDataRowModel();

	Q_SIGNAL void displayedPeakMinChanged( qreal peakMin );
	Q_PROPERTY( qreal displayedPeakMin MEMBER m_displayedPeakMin NOTIFY displayedPeakMinChanged );

	Q_SIGNAL void displayedPeakMaxChanged( qreal peakMax );
	Q_PROPERTY( qreal displayedPeakMax MEMBER m_displayedPeakMax NOTIFY displayedPeakMaxChanged );

	Q_SIGNAL void averageChanged( qreal average );
	Q_PROPERTY( qreal average MEMBER m_average NOTIFY averageChanged );

	Q_SIGNAL void sampleDataChanged();

	// All these properties are not notifiable and are expected to be read once
	// in a sampleDataChanged() handler to avoid excessive individual updates.
	Q_PROPERTY( qreal actualMin MEMBER m_actualMin );
	Q_PROPERTY( qreal actualMax MEMBER m_actualMax );

	Q_INVOKABLE int getSampleCount() { return m_samples.size(); }
	Q_INVOKABLE qreal sampleAt( int index ) { return m_samples[index]; }

	void update( int64_t timestamp, float valueToAdd );
private:
	qreal m_actualMin { 0.0 };
	qreal m_actualMax { 0.0 };
	qreal m_displayedPeakMin { 0.0 };
	qreal m_displayedPeakMax { 0.0 };
	qreal m_average { 0.0 };
	QVector<qreal> m_samples;

	int64_t m_peakMinTimestamp { 0 }, m_peakMaxTimestamp { 0 };
	QVector<qreal> m_prevSamples;
};

// Just an namespace for the enum
class HudDataModel : public QObject {
	Q_OBJECT
public:
	enum Team {
		TeamSpectators = 1,
		TeamPlayers,
		TeamAlpha,
		TeamBeta,
	};
	Q_ENUM( Team );
};

class HudPovDataModel : public HudDataModel {
	Q_OBJECT

public:
	Q_SIGNAL void activeWeaponIconChanged( const QByteArray &activeWeaponIcon );
	Q_PROPERTY( QByteArray activeWeaponIcon READ getActiveWeaponIcon NOTIFY activeWeaponIconChanged );
	Q_SIGNAL void activeWeaponNameChanged( const QByteArray &activeWeaponName );
	Q_PROPERTY( QByteArray activeWeaponName READ getActiveWeaponName NOTIFY activeWeaponNameChanged );
	Q_SIGNAL void activeWeaponWeakAmmoChanged( int activeWeaponWeaponAmmo );
	Q_PROPERTY( int activeWeaponWeakAmmo MEMBER m_activeWeaponWeakAmmo NOTIFY activeWeaponWeakAmmoChanged );
	Q_SIGNAL void activeWeaponStrongAmmoChanged( int activeWeaponStrongAmmo );
	Q_PROPERTY( int activeWeaponStrongAmmo MEMBER m_activeWeaponStrongAmmo NOTIFY activeWeaponStrongAmmoChanged );
	Q_SIGNAL void activeWeaponColorChanged( const QColor &activeWeaponColor );
	Q_PROPERTY( QColor activeWeaponColor READ getActiveWeaponColor NOTIFY activeWeaponColorChanged );

	Q_SIGNAL void healthChanged( int health );
	Q_PROPERTY( int health MEMBER m_health NOTIFY healthChanged );
	Q_SIGNAL void armorChanged( int armor );
	Q_PROPERTY( int armor MEMBER m_armor NOTIFY armorChanged );

	Q_SIGNAL void isMessageFeedFadingOutChanged( bool isMessageFeedFadingOut );
	Q_PROPERTY( bool isMessageFeedFadingOut READ getIsMessageFeedFadingOut NOTIFY isMessageFeedFadingOutChanged );

	Q_SIGNAL void statusMessageChanged( const QString &statusMessage );
	Q_PROPERTY( QString statusMessage MEMBER m_formattedStatusMessage NOTIFY statusMessageChanged );

	Q_SIGNAL void nicknameChanged( const QString &nickname );
	Q_PROPERTY( QString nickname MEMBER m_formattedNickname NOTIFY nicknameChanged );

	Q_SIGNAL void hasActivePovChanged( bool hasActivePov );
	Q_PROPERTY( bool hasActivePov MEMBER m_hasActivePov NOTIFY hasActivePovChanged );
	Q_SIGNAL void isUsingChasePovChanged( bool isUsingChasePov );
	Q_PROPERTY( bool isUsingChasePov MEMBER m_isUsingChasePov NOTIFY isUsingChasePovChanged );
	Q_SIGNAL void isPovAliveChanged( bool isPovAlive );
	Q_PROPERTY( bool isPovAlive MEMBER m_isPovAlive NOTIFY isPovAliveChanged );

	[[nodiscard]]
	Q_INVOKABLE QObject *getInventoryModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getTeamListModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getMessageFeedModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getAwardsModel();

	void addToMessageFeed( const wsw::StringView &message, int64_t timestamp ) {
		m_messageFeedModel.addMessage( message, timestamp );
	}

	void addAward( const wsw::StringView &award, int64_t timestamp ) {
		m_awardsModel.addAward( award, timestamp );
	}

	void resetHudFeed();

	[[nodiscard]]
	bool getIsMessageFeedFadingOut() const { return m_messageFeedModel.isFadingOut(); }

	void addStatusMessage( const wsw::StringView &message, int64_t timestamp );

	void checkPropertyChanges( int64_t currTime );
	void updateScoreboardData( const ReplicatedScoreboardData &scoreboardData );

	void setViewStateIndex( unsigned viewStateIndex );
	[[nodiscard]]
	auto getViewStateIndex() const -> unsigned;
	// Note: We don't return std::optional<unsigned> as models with unset view state index
	// should not be accessed. hasValidViewStateIndex() is only for debug checks.
	// Contrary to that, retrieval of unset player number by getPlayerNum() is perfectly valid in the current codebase.
	[[nodiscard]]
	bool hasValidViewStateIndex() const;
	void clearViewStateIndex();

	void setPlayerNum( unsigned playerNum );
	[[nodiscard]]
	auto getPlayerNum() const -> std::optional<unsigned>;
	[[nodiscard]]
	bool isPovAlive() const { return m_isPovAlive; }
	[[nodiscard]]
	bool isUsingChasePov() const { return m_isUsingChasePov; }
	void clearPlayerNum();
private:
	[[nodiscard]]
	auto getActiveWeaponIcon() const -> QByteArray;
	[[nodiscard]]
	auto getActiveWeaponName() const -> QByteArray;
	[[nodiscard]]
	auto getActiveWeaponColor() const -> QColor;

	InventoryModel m_inventoryModel;
	TeamListModel m_teamListModel;
	MessageFeedModel m_messageFeedModel;
	AwardsModel m_awardsModel;

	unsigned m_playerNum { ~0u };
	unsigned m_viewStateIndex { ~0u };
	// Valid up-to-date counters are non-zero
	unsigned m_nicknameUpdateCounter { 0 };

	int64_t m_lastStatusMessageTimestamp { 0 };
	wsw::StaticString<96> m_originalStatusMessage;
	// TODO make toStyledText() work with arbitrary types
	QString m_formattedStatusMessage;

	QString m_formattedNickname;

	int m_activeWeapon { 0 };
	int m_activeWeaponWeakAmmo { 0 };
	int m_activeWeaponStrongAmmo { 0 };

	int m_health { 0 }, m_armor { 0 };

	bool m_hasActivePov { false };
	bool m_isUsingChasePov { false };
	bool m_isPovAlive { false };

	bool m_hasSetInventoryModelOwnership { false };
	bool m_hasSetTeamListModelOwnership { false };
	bool m_hasSetMessageFeedModelOwnership { false };
	bool m_hasSetAwardsModelOwnership { false };
};

class HudCommonDataModel : public HudDataModel {
	Q_OBJECT

	friend class FragsFeedModel;

public:
	Q_PROPERTY( QObject *regularLayoutModel READ getRegularLayoutModel CONSTANT );
	Q_PROPERTY( QObject *miniviewLayoutModel READ getMiniviewLayoutModel CONSTANT );

	Q_SIGNAL void alphaNameChanged( const QByteArray &alphaName );
	Q_PROPERTY( const QByteArray alphaName MEMBER m_styledAlphaName NOTIFY alphaNameChanged );
	Q_SIGNAL void betaNameChanged( const QByteArray &betaName );
	Q_PROPERTY( const QByteArray betaName MEMBER m_styledBetaName NOTIFY betaNameChanged );
	Q_SIGNAL void alphaClanChanged( const QByteArray &alphaClan );
	Q_PROPERTY( const QByteArray alphaClan MEMBER m_styledAlphaClan NOTIFY alphaClanChanged );
	Q_SIGNAL void betaClanChanged( const QByteArray &betaClan );
	Q_PROPERTY( const QByteArray betaClan MEMBER m_styledBetaClan NOTIFY betaClanChanged );
	Q_SIGNAL void alphaColorChanged( const QColor &alphaColor );
	Q_PROPERTY( QColor alphaColor MEMBER m_alphaColor NOTIFY alphaColorChanged );
	Q_SIGNAL void betaColorChanged( const QColor &betaColor );
	Q_PROPERTY( QColor betaColor MEMBER m_betaColor NOTIFY betaColorChanged );
	Q_SIGNAL void alphaScoreChanged( int alphaScore );
	Q_PROPERTY( int alphaScore MEMBER m_alphaScore NOTIFY alphaScoreChanged );
	Q_SIGNAL void betaScoreChanged( int betaScore );
	Q_PROPERTY( int betaScore MEMBER m_betaScore NOTIFY betaScoreChanged );
	Q_SIGNAL void alphaPlayersStatusChanged( const QByteArray &alphaPlayersStatus );
	Q_PROPERTY( const QByteArray alphaPlayersStatus MEMBER m_alphaPlayersStatus NOTIFY alphaPlayersStatusChanged );
	Q_SIGNAL void betaPlayersStatusChanged( const QByteArray &betaPlayersStatus );
	Q_PROPERTY( const QByteArray betaPlayersStatus MEMBER m_betaPlayersStatus NOTIFY betaPlayersStatusChanged );

	Q_SIGNAL void indicator1StateChanged( const QVariant &indicator1State );
	Q_PROPERTY( QVariant indicator1State READ getIndicator1State NOTIFY indicator1StateChanged );
	Q_SIGNAL void indicator2StateChanged( const QVariant &indicator2State );
	Q_PROPERTY( QVariant indicator2State READ getIndicator2State NOTIFY indicator2StateChanged );
	Q_SIGNAL void indicator3StateChanged( const QVariant &indicator3State );
	Q_PROPERTY( QVariant indicator3State READ getIndicator3State NOTIFY indicator3StateChanged );

	Q_SIGNAL void hasTwoTeamsChanged( bool hasTwoTeams );
	Q_PROPERTY( bool hasTwoTeams MEMBER m_hasTwoTeams NOTIFY hasTwoTeamsChanged );

	Q_SIGNAL void realClientTeamChanged( Team realClientTeam );
	Q_PROPERTY( Team realClientTeam MEMBER m_realClientTeam NOTIFY realClientTeamChanged );

	Q_SIGNAL void activeItemsMaskChanged( int activeItemsMask );
	Q_PROPERTY( int activeItemsMask MEMBER m_activeItemsMask NOTIFY activeItemsMaskChanged );

	Q_SIGNAL void matchTimeSecondsChanged( const QByteArray &seconds );
	Q_PROPERTY( const QByteArray matchTimeSeconds MEMBER m_formattedSeconds NOTIFY matchTimeSecondsChanged );
	Q_SIGNAL void matchTimeMinutesChanged( const QByteArray &minutes );
	Q_PROPERTY( const QByteArray matchTimeMinutes MEMBER m_formattedMinutes NOTIFY matchTimeMinutesChanged );
	Q_SIGNAL void matchStateStringChanged( const QByteArray &matchStateString );
	Q_PROPERTY( const QByteArray matchStateString MEMBER m_matchStateString NOTIFY matchStateStringChanged );

	Q_SIGNAL void isInWarmupStateChanged( bool isInWarmupState );
	Q_PROPERTY( bool isInWarmupState MEMBER m_isInWarmupState NOTIFY isInWarmupStateChanged );

	Q_SIGNAL void isInPostmatchStateChanged( bool isInPostmatchState );
	Q_PROPERTY( bool isInPostmatchState MEMBER m_isInPostmatchState NOTIFY isInPostmatchStateChanged );

	Q_SIGNAL void highlightedMiniviewIndexChanged( int highlightedMiniviewIndex );
	Q_PROPERTY( int highlightedMiniviewIndex MEMBER m_highlightedMiniviewIndex NOTIFY highlightedMiniviewIndexChanged );

	// For wtf gametype
	static constexpr unsigned kMaxMiniviews = 12;

	Q_SIGNAL void hasTiledMiniviewsChanged( bool hasTiledMiniviews );
	Q_PROPERTY( bool hasTiledMiniviews MEMBER m_hasTiledMiniviews NOTIFY hasTiledMiniviewsChanged );

	Q_SIGNAL void miniviewLayoutChangedPass1();
	Q_SIGNAL void miniviewLayoutChangedPass2();
	Q_INVOKABLE QObject *getMiniviewModelForIndex( int index );
	Q_INVOKABLE QVariant getMiniviewPlayerNumForIndex( int index );
	Q_INVOKABLE QVariant getFixedMiniviewPositionForIndex( int indexOfModel ) const;
	Q_INVOKABLE QVariant getFixedPositionMiniviewIndices() const;
	Q_INVOKABLE QVariant getMiniviewIndicesForPane( int paneNum ) const;

	Q_INVOKABLE int getAllowedNumRowsForMiniviewPane( int paneNum ) const;
	Q_INVOKABLE int getAllowedNumColumnsForMiniviewPane( int paneNum ) const;
	Q_INVOKABLE int getPreferredNumRowsForMiniviewPane( int paneNum ) const;
	Q_INVOKABLE int getPreferredNumColumnsForMiniviewPane( int paneNum ) const;

	[[nodiscard]]
	auto getViewStateIndexForMiniviewModelIndex( int miniviewModelIndex ) const -> unsigned;

	enum Powerup { Quad  = 0x1, Shell = 0x2, Regen = 0x4 };
	Q_ENUM( Powerup );

	[[nodiscard]]
	Q_INVOKABLE QObject *getFragsFeedModel();

	[[nodiscard]]
	Q_INVOKABLE QByteArray getWeaponFullName( int weapon ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getWeaponShortName( int weapon ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getWeaponIconPath( int weapon ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getWeaponModelPath( int weapon ) const;
	[[nodiscard]]
	Q_INVOKABLE QColor getWeaponColor( int weapon ) const;

	[[nodiscard]]
	Q_INVOKABLE QStringList getAvailableRegularCrosshairs() const;
	[[nodiscard]]
	Q_INVOKABLE QStringList getAvailableStrongCrosshairs() const;

	[[nodiscard]]
	Q_INVOKABLE QByteArray getRegularCrosshairFilePath( const QByteArray &fileName ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getStrongCrosshairFilePath( const QByteArray &fileName ) const;

	[[nodiscard]]
	Q_INVOKABLE QVariant getIndicator1State() { return QVariant::fromValue( m_indicatorStates[0] ); }
	[[nodiscard]]
	Q_INVOKABLE QVariant getIndicator2State() { return QVariant::fromValue( m_indicatorStates[1] ); }
	[[nodiscard]]
	Q_INVOKABLE QVariant getIndicator3State() { return QVariant::fromValue( m_indicatorStates[2] ); }

	enum IndicatorAnim { NoAnim, AlertAnim, ActionAnim };
	Q_ENUM( IndicatorAnim );

	[[nodiscard]]
	Q_INVOKABLE QByteArray getIndicatorIconPath( int iconNum ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getIndicatorStatusString( int stringNum ) const;

	[[nodiscard]]
	Q_INVOKABLE QObject *getFrametimeDataRowModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getPingDataRowModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getPacketlossDataRowModel();

	Q_SLOT void onHudUpdated( const QByteArray &name, HudLayoutModel::Flavor flavor );

	void resetHudFeed();

	void addFragEvent( const std::pair<wsw::StringView, int> &victimAndTeam,
					   int64_t timestamp, unsigned meansOfDeath,
					   const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam );

	// For miniview models
	void addToMessageFeed( unsigned playerNum, const wsw::StringView &message, int64_t timestamp );
	void addAward( unsigned playerNum, const wsw::StringView &award, int64_t timestamp );
	void addStatusMessage( unsigned playerNum, const wsw::StringView &message, int64_t timestamp );

	void addToFrametimeTimeline( int64_t timestamp, float frametime );
	void addToPingTimeline( int64_t timestamp, float ping );
	void addToPacketlossTimeline( int64_t timestamp, bool hadPacketloss );

	void checkPropertyChanges( int64_t currTime );
	void updateScoreboardData( const ReplicatedScoreboardData &scoreboardData );

	explicit HudCommonDataModel( int pixelsPerLogicalUnit );
private:
	[[nodiscard]]
	auto getRegularLayoutModel() -> QObject *;
	[[nodiscard]]
	auto getMiniviewLayoutModel() -> QObject *;

	[[nodiscard]]
	auto findPovModelByPlayerNum( unsigned playerNum ) -> HudPovDataModel *;

	[[nodiscard]]
	static auto toQColor( int color ) -> QColor {
		return QColor::fromRgb( COLOR_R( color ), COLOR_G( color ), COLOR_B( color ) );
	}

	static void setFormattedTime( QByteArray *dest, int value );
	static void setStyledName( QByteArray *dest, const wsw::StringView &name );

	[[nodiscard]]
	static auto getCrosshairFilePath( const wsw::StringView &prefix, const QByteArray &fileName ) -> QByteArray;

	[[nodiscard]]
	auto getStatusForNumberOfPlayers( int numPlayers ) const -> QByteArray;
	void updateTeamPlayerStatuses( const ReplicatedScoreboardData &scoreboardData );

	using HudNameString = wsw::StaticString<HudLayoutModel::kMaxHudNameLength>;
	void handleVarChanges( StringConfigVar *var, InGameHudLayoutModel *model, HudNameString *currName );

	void updateMiniviewData( int64_t currTime );

	const int m_pixelsPerLogicalUnit;

	FragsFeedModel m_fragsFeedModel { this };

	VarModificationTracker m_regularHudChangesTracker;
	VarModificationTracker m_miniviewHudChangesTracker;
	InGameHudLayoutModel m_regularLayoutModel { HudLayoutModel::Regular };
	InGameHudLayoutModel m_miniviewLayoutModel { HudLayoutModel::Miniview };
	HudNameString m_regularHudName, m_miniviewHudName;

	wsw::StaticString<32> m_alphaName, m_betaName;
	QByteArray m_styledAlphaName, m_styledBetaName;
	QByteArray m_styledAlphaClan, m_styledBetaClan;

	QByteArray m_alphaPlayersStatus, m_betaPlayersStatus;
	int m_numAliveAlphaPlayers { 0 }, m_numAliveBetaPlayers { 0 };
	int m_pendingNumAliveAlphaPlayers { 0 }, m_pendingNumAliveBetaPlayers { 0 };
	unsigned m_lastIndividualAlphaClanCounter { 0 }, m_lastIndividualBetaClanCounter { 0 };
	std::optional<int> m_pendingIndividualAlphaPlayerNum, m_pendingIndividualBetaPlayerNum;

	ObjectiveIndicatorState m_indicatorStates[3];

	PerfDataRowModel m_frametimeDataRowModel;
	PerfDataRowModel m_pingDataRowModel;
	PerfDataRowModel m_packetlossDataRowModel;
	bool m_hasSetFrametimeModelOwnership { false };
	bool m_hasSetPingModelOwnership { false };
	bool m_hasSetPacketlossModelOwnership { false };

	int m_rawAlphaColor { 0 }, m_rawBetaColor { 0 };
	QColor m_alphaColor { toQColor( m_rawAlphaColor ) };
	QColor m_betaColor { toQColor( m_rawBetaColor ) };
	int m_alphaScore { 0 }, m_pendingAlphaScore { 0 };
	int m_betaScore { 0 }, m_pendingBetaScore { 0 };
	bool m_hasTwoTeams { false };

	int m_activeItemsMask { 0 };
	Team m_realClientTeam { TeamSpectators };

	QByteArray m_formattedSeconds;
	QByteArray m_formattedMinutes;
	QByteArray m_matchStateString;
	int m_matchTimeSeconds { 0 };
	int m_matchTimeMinutes { 0 };

	struct FixedPositionMinivewEntry {
		int indexOfModel;
		unsigned viewStateIndex;
		Rect position;
	};

	struct HudControlledMiniviewEntry {
		int indexOfModel;
		int paneNumber;
	};

	wsw::StaticVector<FixedPositionMinivewEntry, kMaxMiniviews> m_fixedPositionMinviews;
	wsw::StaticVector<HudControlledMiniviewEntry, kMaxMiniviews> m_hudControlledMinviewsForPane[2];

	HudPovDataModel m_miniviewDataModels[kMaxMiniviews];

	int m_highlightedMiniviewIndex { -1 };

	bool m_hasTiledMiniviews { false };

	bool m_isInWarmupState { false };
	bool m_isInPostmatchState { false };

	bool m_hasSetFragsFeedModelOwnership { false };
	bool m_hasSetRegularLayoutModelOwnership { false };
	bool m_hasSetMinivewLayoutModelOwnership { false };
};

}

Q_DECLARE_METATYPE( wsw::ui::ObjectiveIndicatorState )

#endif