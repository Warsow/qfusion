#include "uisystem.h"
#include "../common/configvars.h"
#include "../common/links.h"
#include "../common/profilerscope.h"
#include "../common/singletonholder.h"
#include "../common/wswstaticvector.h"
#include "../common/common.h"
#include "../client/client.h"
#include "actionrequestmodel.h"
#include "callvotesmodel.h"
#include "chatmodel.h"
#include "demos.h"
#include "gametypeoptionsmodel.h"
#include "gametypesmodel.h"
#include "hudlayoutmodel.h"
#include "huddatamodel.h"
#include "nativelydrawnitems.h"
#include "playersmodel.h"
#include "serverlistmodel.h"
#include "keysandbindingsmodel.h"
#include "scoreboardmodel.h"
#include "videoplaybacksystem.h"
#include "wswimageprovider.h"

#include <QGuiApplication>
#include <QOpenGLContext>
#include <QOpenGLFramebufferObject>
#include <QQuickRenderControl>
#include <QQuickWindow>
#include <QOffscreenSurface>
#include <QOpenGLFunctions>
#include <QOpenGLExtraFunctions>
#include <QQmlEngine>
#include <QQmlComponent>
#include <QQmlContext>
#include <QQuickItem>
#include <QUrl>
#include <QScopedPointer>
#include <QFontDatabase>
#include <QQmlProperty>
#include <QtPlugin>

#include <clocale>
#include <span>
#include "../common/common.h"
#include "../common/wswalgorithm.h"

#ifdef _WIN32
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin);
#else
Q_IMPORT_PLUGIN(QXcbIntegrationPlugin)
Q_IMPORT_PLUGIN(QXcbGlxIntegrationPlugin)
#endif
#if QT_VERSION > QT_VERSION_CHECK(5, 13, 2)
Q_IMPORT_PLUGIN(QtQmlPlugin);
#endif
Q_IMPORT_PLUGIN(QtQuick2Plugin);
Q_IMPORT_PLUGIN(QtQuick2WindowPlugin);
Q_IMPORT_PLUGIN(QtQuickLayoutsPlugin);
Q_IMPORT_PLUGIN(QtGraphicalEffectsPlugin);
Q_IMPORT_PLUGIN(QtGraphicalEffectsPrivatePlugin);
Q_IMPORT_PLUGIN(QtQuickControls2Plugin);
Q_IMPORT_PLUGIN(QtQuickControls2MaterialStylePlugin);
Q_IMPORT_PLUGIN(QtQuickTemplates2Plugin);
Q_IMPORT_PLUGIN(QMultimediaDeclarativeModule);
Q_IMPORT_PLUGIN(QmlShapesPlugin);
Q_IMPORT_PLUGIN(QSvgPlugin);
Q_IMPORT_PLUGIN(QTgaPlugin);

using wsw::operator""_asView;

QVariant VID_GetMainContextHandle();

bool GLimp_BeginUIRenderingHacks();
bool GLimp_EndUIRenderingHacks();

extern BoolConfigVar v_showFps;
extern BoolConfigVar v_showNet;

// See the respective Qt patch
extern size_t qt_wswHeapSizeGCThreshold;

static FloatConfigVar v_mouseSensitivity { "ui_mouseSensitivity"_asView, {
	.byDefault = 1.0f, .min = inclusive( 0.5f ), .max = inclusive( 5.0f ), .flags = CVAR_ARCHIVE, }
};
static FloatConfigVar v_mouseAccel { "ui_mouseAccel"_asView, {
	.byDefault = 0.2f, .min = exclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE, }
};
static BoolConfigVar v_debugNativelyDrawnItems { "ui_debugNativelyDrawnItems"_asView, {
	.byDefault = false, .flags = CVAR_DEVELOPER, }
};

// Let the upper bound be exclusive, so the limit fits 32 bits
static UnsignedConfigVar v_heapSizeGCThresholdInGame { "ui_heapSizeGCThresholdInGame"_asView, {
	.byDefault = 512u, .min = inclusive( 4u ), .max = exclusive( 4096u ), .flags = CVAR_ARCHIVE },
};
static UnsignedConfigVar v_heapSizeGCThresholdMenu { "ui_heapSizeGCThresholdMenu"_asView, {
	.byDefault = 4u, .min = inclusive( 4u ), .max = exclusive( 32u ), .flags = CVAR_ARCHIVE },
};

static BoolConfigVar v_runGCOnTeleportEvent { "ui_runGCOnTeleportEvent"_asView, {
	.byDefault = true, .flags = CVAR_ARCHIVE, },
};

namespace wsw::ui {

class CustomQuickRenderControl : public QQuickRenderControl {
public:
	// See comments in the site where it actually gets modified
	QWindow *m_windowForDensityRetrieval { nullptr };
	auto renderWindow( QPoint *pt ) -> QWindow * override {
		return m_windowForDensityRetrieval;
	}
};

class QmlSandbox : public QObject {
	friend class QtUISystem;

	Q_OBJECT
public:
	~QmlSandbox() override;

	Q_SLOT void onSceneGraphInitialized();
	Q_SLOT void onRenderRequested();
	Q_SLOT void onSceneChanged();
	Q_SLOT void onComponentStatusChanged( QQmlComponent::Status status );

	[[nodiscard]]
	bool requestsRendering() const { return m_hasPendingRedraw || m_hasPendingSceneChange; }
private:
	explicit QmlSandbox( QtUISystem *uiSystem ) : m_uiSystem( uiSystem ) {};

	std::unique_ptr<QOpenGLContext> m_controlContext;
	std::unique_ptr<QOpenGLFramebufferObject> m_framebufferObject;
	std::unique_ptr<QOffscreenSurface> m_surface;
	std::unique_ptr<QQuickWindow> m_window;
	std::unique_ptr<CustomQuickRenderControl> m_control;
	std::unique_ptr<QQmlEngine> m_engine;
	std::unique_ptr<QQmlComponent> m_component;
	QObject *m_rootObject { nullptr };
	QtUISystem *const m_uiSystem;

	bool m_hasPendingSceneChange { false };
	bool m_hasPendingRedraw { false };
	bool m_hasSucceededLoading { false };
	bool m_hasValidFboContent { false };
};

class QtUISystem : public QObject, public UISystem {
	Q_OBJECT

	// The implementation is borrowed from https://github.com/RSATom/QuickLayer

	template <typename> friend class ::SingletonHolder;
	friend class NativelyDrawnImage;
	friend class NativelyDrawnModel;
	friend class QmlSandbox;
public:
	void refreshProperties() override;
	void renderInternally() override;

	void drawBackgroundMapIfNeeded() override;
	void drawMenuPartInMainContext() override;
	void drawHudPartInMainContext() override;
	void drawCursorInMainContext() override;

	void runGCIfNeeded() override;
	void handleGCSafepoint( GCSafepointKind kind ) override;

	void beginRegistration() override;
	void endRegistration() override;

	[[nodiscard]]
	bool grabsKeyboardAndMouseButtons() const override;
	[[nodiscard]]
	bool grabsMouseMovement() const override;
	[[nodiscard]]
	bool handleKeyEvent( int quakeKey, bool keyDown ) override;
	[[nodiscard]]
	bool handleCharEvent( int ch ) override;
	[[nodiscard]]
	bool handleMouseMovement( float frameTimeMillis, int dx, int dy ) override;

	void handleEscapeKey() override;

	void addToChat( const wsw::cl::ChatMessage &message ) override;
	void addToTeamChat( const wsw::cl::ChatMessage &message ) override;

	void handleMessageFault( const MessageFault &messageFault ) override;

	void handleConfigString( unsigned configStringNum, const wsw::StringView &configString ) override;

	void handleClientInfoChanges( unsigned clientNum ) override;

	void updateScoreboard( const ReplicatedScoreboardData &scoreboardData, const AccuracyRows &accuracyRows ) override;

	[[nodiscard]]
	bool isShowingScoreboard() const override;
	void setScoreboardShown( bool shown ) override;

	[[nodiscard]]
	bool isShowingModalMenu() const override;

	[[nodiscard]]
	bool suggestsUsingVSync() const override;

	void toggleChatPopup() override;
	void toggleTeamChatPopup() override;

	void touchActionRequest( const wsw::StringView &tag, unsigned timeout,
						  	 const wsw::StringView &title, const wsw::StringView &actionDesc,
						  	 const std::pair<wsw::StringView, int> *actionsBegin,
						  	 const std::pair<wsw::StringView, int> *actionsEnd ) override;

	void handleOptionsStatusCommand( const wsw::StringView &status ) override;

	void reloadOptions() override;

	void resetHudFeed() override;

	void addFragEvent( const std::pair<wsw::StringView, int> &victimAndTeam,
					   unsigned meansOfDeath,
					   const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam ) override;

	void addToMessageFeed( unsigned playerNum, const wsw::StringView &message ) override;

	void addAward( unsigned playerNum, const wsw::StringView &award ) override;

	void addStatusMessage( unsigned playerNum, const wsw::StringView &message ) override;

	void addToFrametimeTimeline( int64_t timestamp, float frametime ) override;
	void addToPingTimeline( int64_t timestamp, float ping ) override;
	void addToPacketlossTimeline( int64_t timestamp, bool hadPacketloss ) override;

	void notifyOfDroppedConnection( const wsw::StringView &message, ReconnectBehaviour rectonnectBehaviour, ConnectionDropStage dropStage );

	void notifyOfUpdateAvailable( const wsw::StringView &version ) override;
	void notifyOfUpdateNotFound() override;

	Q_INVOKABLE void stopReactingToDroppedConnection() { m_pendingReconnectBehaviour = std::nullopt; }

	void dispatchShuttingDown() override { Q_EMIT shuttingDown(); }
	auto retrieveNumberOfHudMiniviewPanes() -> unsigned override;
	auto retrieveLimitOfMiniviews() -> unsigned override { return m_hudCommonDataModel.kMaxMiniviews; }
	auto retrieveHudControlledMiniviews( Rect positions[MAX_CLIENTS], unsigned viewStateNums[MAX_CLIENTS] ) -> unsigned override;

	[[nodiscard]]
	auto getFrameTimestamp() const -> int64_t { return ::cls.realtime; }

	[[nodiscard]]
	bool isInUIRenderingMode() { return m_isInUIRenderingMode; }
	void enterUIRenderingMode();
	void leaveUIRenderingMode();

	Q_PROPERTY( bool isShowingMainMenu READ isShowingMainMenu NOTIFY isShowingMainMenuChanged );
	Q_PROPERTY( bool isShowingConnectionScreen READ isShowingConnectionScreen NOTIFY isShowingConnectionScreenChanged );
	Q_PROPERTY( bool isShowingInGameMenu READ isShowingInGameMenu NOTIFY isShowingInGameMenuChanged );
	Q_PROPERTY( bool isShowingDemoPlaybackMenu READ isShowingDemoPlaybackMenu NOTIFY isShowingDemoPlaybackMenuChanged );
	Q_PROPERTY( bool isShowingIntroScreen READ isShowingIntroScreen NOTIFY isShowingIntroScreenChanged );
	Q_PROPERTY( bool isShowingUpdateScreen READ isShowingUpdateScreen NOTIFY isShowingUpdateScreenChanged );

	Q_PROPERTY( bool isDebuggingNativelyDrawnItems READ isDebuggingNativelyDrawnItems NOTIFY isDebuggingNativelyDrawnItemsChanged );

	Q_PROPERTY( bool isShowingScoreboard READ isShowingScoreboard NOTIFY isShowingScoreboardChanged );

	Q_PROPERTY( bool isConsoleOpen MEMBER m_isConsoleOpen NOTIFY isConsoleOpenChanged );
	Q_PROPERTY( bool isClientDisconnected MEMBER m_isClientDisconnected NOTIFY isClientDisconnectedChanged );

	Q_PROPERTY( bool isShowingChatPopup MEMBER m_isShowingChatPopup NOTIFY isShowingChatPopupChanged );
	Q_PROPERTY( bool isShowingTeamChatPopup MEMBER m_isShowingTeamChatPopup NOTIFY isShowingTeamChatPopupChanged );
	Q_PROPERTY( bool hasTeamChat MEMBER m_hasTeamChat NOTIFY hasTeamChatChanged );

	Q_SIGNAL void isShowingHudChanged( bool isShowingHud );
	Q_PROPERTY( bool isShowingHud MEMBER m_isShowingHud NOTIFY isShowingHudChanged );

	Q_PROPERTY( bool isShowingActionRequests READ isShowingActionRequests NOTIFY isShowingActionRequestsChanged );

	Q_SIGNAL void availableUpdateVersionChanged( const QString &version );
	Q_PROPERTY( QString availableUpdateVersion MEMBER m_availableUpdateVersion NOTIFY availableUpdateVersionChanged );

	// Asks Qml
	Q_SIGNAL void displayedHudItemsRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyDisplayedHudItemAndMargin( QQuickItem *item, qreal margin );

	Q_SIGNAL void hudOccludersChanged();
	// Asks Qml
	Q_SIGNAL void hudOccludersRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyHudOccluder( QQuickItem *item );

	[[nodiscard]]
	Q_INVOKABLE bool isHudItemOccluded( QQuickItem *item );

	// Asks Qml
	Q_SIGNAL void nativelyDrawnItemsOccludersRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyNativelyDrawnItemsOccluder( QQuickItem *item );

	// Asks Qml
	Q_SIGNAL void nativelyDrawnItemsRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyNativelyDrawnItem( QQuickItem *item );

	// Asks Qml
	Q_SIGNAL void hudControlledMiniviewItemsRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyHudControlledMiniviewItemAndModelIndex( QQuickItem *item, int modelIndex );

	// Asks Qml
	Q_SIGNAL void hudMiniviewPanesRetrievalRequested();
	// Qml should call this method in reply
	Q_INVOKABLE void supplyHudMiniviewPane( int number );

	Q_INVOKABLE QVariant getCVarValue( const QString &name ) const;
	Q_INVOKABLE void setCVarValue( const QString &name, const QVariant &value );

	Q_PROPERTY( bool hasPendingCVarChanges READ hasPendingCVarChanges NOTIFY hasPendingCVarChangesChanged );

	Q_INVOKABLE void commitPendingCVarChanges();
	Q_INVOKABLE void rollbackPendingCVarChanges();
	Q_INVOKABLE void reportPendingCVarChanges( const QString &name, const QVariant &value );

	Q_SIGNAL void pendingCVarChangesCommitted();
	Q_SIGNAL void checkingCVarChangesRequested();
	Q_SIGNAL void reportingPendingCVarChangesRequested();
	Q_SIGNAL void rollingPendingCVarChangesBackRequested();

	Q_INVOKABLE void ensureObjectDestruction( QObject *object );

	Q_INVOKABLE void showMainMenu();
	Q_INVOKABLE void returnFromInGameMenu();
	Q_INVOKABLE void returnFromMainMenu();
	Q_INVOKABLE void finishIntro();
	Q_INVOKABLE void hideUpdates();
	Q_INVOKABLE void downloadUpdates();

	Q_INVOKABLE void closeChatPopup();

	Q_INVOKABLE void connectToAddress( const QByteArray &address );
	Q_INVOKABLE void reconnectWithPassword( const QByteArray &password );
	Q_INVOKABLE void reconnect();

	enum LocalServerFlags {
		LocalServerInsta  = 0x1,
		LocalServerPublic = 0x2
	};

	Q_ENUM( LocalServerFlags );

	Q_INVOKABLE void launchLocalServer( const QByteArray &gametype, const QByteArray &map, int flags, int numBots, int skillLevel );

	Q_INVOKABLE void quit();
	Q_INVOKABLE void disconnect();

	Q_INVOKABLE void setReady();
	Q_INVOKABLE void setNotReady();
	Q_INVOKABLE void enterChallengersQueue();
	Q_INVOKABLE void leaveChallengersQueue();
	Q_INVOKABLE void spectate();
	Q_INVOKABLE void join();
	Q_INVOKABLE void joinAlpha();
	Q_INVOKABLE void joinBeta();

	Q_INVOKABLE void switchToPlayerNum( int playerNum );

	Q_SIGNAL void canSpectateChanged( bool canSpectate );
	Q_PROPERTY( bool canSpectate MEMBER m_canSpectate NOTIFY canSpectateChanged );

	Q_SIGNAL void canJoinChanged( bool canJoin );
	Q_PROPERTY( bool canJoin MEMBER m_canJoin NOTIFY canJoinChanged );

	Q_SIGNAL void canJoinAlphaChanged( bool canJoinAlpha );
	Q_PROPERTY( bool canJoinAlpha MEMBER m_canJoinAlpha NOTIFY canJoinAlphaChanged );

	Q_SIGNAL void canJoinBetaChanged( bool canJoinBeta );
	Q_PROPERTY( bool canJoinBeta MEMBER m_canJoinBeta NOTIFY canJoinBetaChanged );

	Q_SIGNAL void canBeReadyChanged( bool canBeReady );
	Q_PROPERTY( bool canBeReady MEMBER m_canBeReady NOTIFY canBeReadyChanged );

	Q_SIGNAL void isReadyChanged( bool isReady );
	Q_PROPERTY( bool isReady MEMBER m_isReady NOTIFY isReadyChanged );

	Q_SIGNAL void canToggleChallengerStatusChanged( bool canToggleChallengerStatus );
	Q_PROPERTY( bool canToggleChallengerStatus MEMBER m_canToggleChallengerStatus NOTIFY canToggleChallengerStatusChanged );

	Q_SIGNAL void isInChallengersQueueChanged( bool isInChallengersQueue );
	Q_PROPERTY( bool isInChallengersQueue MEMBER m_isInChallengersQueue NOTIFY isInChallengersQueueChanged );

	Q_INVOKABLE QVariant colorFromRgbString( const QString &string ) const;

	Q_INVOKABLE QMatrix4x4 makeSkewXMatrix( qreal height, qreal degrees ) const;
	Q_INVOKABLE QMatrix4x4 makeTranslateMatrix( qreal translateX, qreal translateY ) const;

	Q_INVOKABLE QByteArray formatPing( int ping ) const;

	Q_INVOKABLE QColor colorWithAlpha( const QColor &color, qreal alpha );

	Q_INVOKABLE void playHoverSound();
	Q_INVOKABLE void playSwitchSound();
	Q_INVOKABLE void playForwardSound() override;
	Q_INVOKABLE void playBackSound();

	enum ServerListFlags {
		ShowEmptyServers = 0x1,
		ShowFullServers  = 0x2
	};
	Q_ENUM( ServerListFlags );

	Q_INVOKABLE void startServerListUpdates( int flags );
	Q_INVOKABLE void stopServerListUpdates();

	Q_INVOKABLE void callVote( const QByteArray &name, const QByteArray &value, bool isOperatorCall );

	Q_SIGNAL void isOperatorChanged( bool isOperator );
	Q_PROPERTY( bool isOperator MEMBER m_isOperator NOTIFY isOperatorChanged );

	enum ReconnectBehaviour_ {
		DontReconnect   = (unsigned)ReconnectBehaviour::DontReconnect,
		OfUserChoice    = (unsigned)ReconnectBehaviour::OfUserChoice,
		RequestPassword = (unsigned)ReconnectBehaviour::RequestPassword,
	};
	Q_ENUM( ReconnectBehaviour_ );

	Q_SIGNAL bool isReactingToDroppedConnectionChanged( bool isReactingToDroppedConnection );
	Q_PROPERTY( bool isReactingToDroppedConnection READ isReactingToDroppedConnection NOTIFY isReactingToDroppedConnectionChanged );

	Q_SIGNAL void reconnectBehaviourChanged( QVariant reconnectBehaviour );
	Q_PROPERTY( QVariant reconnectBehaviour READ getReconnectBehaviour NOTIFY reconnectBehaviourChanged );

	Q_SIGNAL void droppedConnectionTitleChanged( const QString &droppedConnectionTitle );
	Q_PROPERTY( QString droppedConnectionTitle READ getDroppedConnectionTitle NOTIFY droppedConnectionTitleChanged );

	Q_SIGNAL void droppedConnectionMessageChanged( const QString &droppedConnectionMessage );
	Q_PROPERTY( QString droppedConnectionMessage READ getDroppedConnectionMessage NOTIFY droppedConnectionMessageChanged );

	Q_PROPERTY( QColor black MEMBER m_colorBlack CONSTANT );
	Q_PROPERTY( QColor red MEMBER m_colorRed CONSTANT );
	Q_PROPERTY( QColor green MEMBER m_colorGreen CONSTANT );
	Q_PROPERTY( QColor yellow MEMBER m_colorYellow CONSTANT );
	Q_PROPERTY( QColor blue MEMBER m_colorBlue CONSTANT );
	Q_PROPERTY( QColor cyan MEMBER m_colorCyan CONSTANT );
	Q_PROPERTY( QColor magenta MEMBER m_colorMagenta CONSTANT );
	Q_PROPERTY( QColor white MEMBER m_colorWhite CONSTANT );
	Q_PROPERTY( QColor orange MEMBER m_colorOrange CONSTANT );
	Q_PROPERTY( QColor grey MEMBER m_colorGrey CONSTANT );
	Q_PROPERTY( QVariantList consoleColors MEMBER m_consoleColors CONSTANT );
	Q_PROPERTY( QStringList playerModels MEMBER m_playerModels CONSTANT );
	Q_PROPERTY( QString defaultPlayerModel MEMBER m_defaultPlayerModel CONSTANT );
	Q_PROPERTY( QString defaultTeamAlphaModel MEMBER m_defaultTeamAlphaModel CONSTANT );
	Q_PROPERTY( QString defaultTeamBetaModel MEMBER m_defaultTeamBetaModel CONSTANT );
	Q_PROPERTY( QString defaultTeamPlayersModel MEMBER m_defaultTeamPlayersModel CONSTANT );
	Q_PROPERTY( QJsonArray videoModeHeadingsList MEMBER s_videoModeHeadingsList CONSTANT );
	Q_PROPERTY( QJsonArray videoModeWidthValuesList MEMBER s_videoModeWidthValuesList CONSTANT );
	Q_PROPERTY( QJsonArray videoModeHeightValuesList MEMBER s_videoModeHeightValuesList CONSTANT );
	Q_PROPERTY( qreal minRegularCrosshairSize MEMBER s_minRegularCrosshairSize CONSTANT );
	Q_PROPERTY( qreal maxRegularCrosshairSize MEMBER s_maxRegularCrosshairSize CONSTANT );
	Q_PROPERTY( qreal minStrongCrosshairSize MEMBER s_minStrongCrosshairSize CONSTANT );
	Q_PROPERTY( qreal maxStrongCrosshairSize MEMBER s_maxStrongCrosshairSize CONSTANT );
	Q_PROPERTY( qreal crosshairSizeStep MEMBER s_crosshairSizeStep CONSTANT );
	Q_PROPERTY( QString regularFontFamily MEMBER s_regularFontFamily CONSTANT );
	Q_PROPERTY( QString headingFontFamily MEMBER s_headingFontFamily CONSTANT );
	Q_PROPERTY( QString numbersFontFamily MEMBER s_numbersFontFamily CONSTANT );
	Q_PROPERTY( QString symbolsFontFamily MEMBER s_symbolsFontFamily CONSTANT );
	Q_PROPERTY( QString emojiFontFamily MEMBER s_emojiFontFamily CONSTANT );
signals:
	Q_SIGNAL void isShowingScoreboardChanged( bool isShowingScoreboard );
	Q_SIGNAL void isShowingChatPopupChanged( bool isShowingChatPopup );
	Q_SIGNAL void isShowingTeamChatPopupChanged( bool isShowingTeamChatPopup );
	Q_SIGNAL void hasTeamChatChanged( bool hasTeamChat );

	Q_SIGNAL void isShowingActionRequestsChanged( bool isShowingActionRequests );

	Q_SIGNAL void isConsoleOpenChanged( bool isConsoleOpen );
	Q_SIGNAL void isClientDisconnectedChanged( bool isClientDisconnected );

	Q_SIGNAL void isShowingMainMenuChanged( bool isShowingMainMenu );
	Q_SIGNAL void isShowingConnectionScreenChanged( bool isShowingConnectionScreen );
	Q_SIGNAL void isShowingInGameMenuChanged( bool isShowingInGameMenu );
	Q_SIGNAL void isShowingIntroScreenChanged( bool isShowingIntroScreen );
	Q_SIGNAL void isShowingUpdateScreenChanged( bool isShowingUpdateScreen );

	Q_SIGNAL void isShowingDemoPlaybackMenuChanged( bool isShowingDemoMenu );
	Q_SIGNAL void isDebuggingNativelyDrawnItemsChanged( bool isDebuggingNativelyDrawnItems );
	Q_SIGNAL void hasPendingCVarChangesChanged( bool hasPendingCVarChanges );

	Q_SIGNAL void shuttingDown();
private:
	static inline QGuiApplication *s_application { nullptr };
	static inline int s_fakeArgc { 0 };
	static inline char **s_fakeArgv { nullptr };
	static inline QString s_charStrings[128];

	static inline const qreal s_minRegularCrosshairSize { kRegularCrosshairSizeProps.minSize };
	static inline const qreal s_maxRegularCrosshairSize { kRegularCrosshairSizeProps.maxSize };
	static inline const qreal s_minStrongCrosshairSize { kStrongCrosshairSizeProps.minSize };
	static inline const qreal s_maxStrongCrosshairSize { kStrongCrosshairSizeProps.maxSize };
	static inline const qreal s_crosshairSizeStep { 1.0 };

	static inline const QString s_regularFontFamily { "Ubuntu" };
	static inline const QString s_headingFontFamily { "IBM Plex Sans" };
	static inline const QString s_numbersFontFamily { s_headingFontFamily };
	static inline const QString s_symbolsFontFamily { "Noto Sans Symbols2" };

	// Windows system facilities cannot handle Noto Emoji.
	// So far we need 3 glyphs to serve as icon replacements.
	// Try relying on the system font on Windows platform.
	// This should eventually be fixed.
#ifndef _WIN32
	static inline const QString s_emojiFontFamily { "Noto Color Emoji" };
#else
	static inline const QString s_emojiFontFamily { "Segoe UI Emoji" };
#endif

	// &'static so the value is preserved upon restarts
	static inline bool s_hasFinishedIntro { false };
	static inline bool s_hasDisplayedUpdates { false };

	bool m_pendingFinishIntro { false };
	bool m_hasCheckedFirstFrame { false };
	bool m_pendingHideUpdates { false };

	int64_t m_lastDrawMenuPartTimestamp { 0 };

	// Shared for sandbox instances (it is a shared context as well)
	std::unique_ptr<QOpenGLContext> m_externalContext;

	// For tracking changes
	wsw::PodVector<QRectF> m_oldHudOccluders;
	// Values are in logical units
	wsw::PodVector<QRectF> m_hudOccluders;

	wsw::PodVector<NativelyDrawn *> m_nativelyDrawnItems;
	wsw::PodVector<NativelyDrawn *> m_nativelyDrawnUnderlayHeap;
	wsw::PodVector<NativelyDrawn *> m_nativelyDrawnOverlayHeap;
	// Values are in pixels
	wsw::PodVector<QRectF> m_occludersOfNativelyDrawnItems;

	// Values are in pixels
	wsw::PodVector<QPair<QRectF, qreal>> m_boundsOfDrawnHudItems;
	// Avoid using std::vector<bool>
	wsw::PodVector<uint8_t> m_drawnCellsMaskOfHudImage;
	wsw::PodVector<QPair<unsigned, unsigned>> m_columnRangesOfCellGridRows;

	wsw::Vector<QPair<QString, QVariant>> m_pendingCVarChanges;

	Rect *m_miniviewItemPositions { nullptr };
	unsigned *m_miniviewViewStateIndices { nullptr };
	unsigned m_numRetrievedMiniviews { 0 };
	bool m_hasMiniviewPane1 { false };
	bool m_hasMiniviewPane2 { false };

	std::unique_ptr<QmlSandbox> m_menuSandbox;
	std::unique_ptr<QmlSandbox> m_hudSandbox;

	bool m_isInUIRenderingMode { false };

	ServerListModel m_serverListModel;
	GametypesModel m_gametypesModel;
	KeysAndBindingsModel m_keysAndBindingsModel;

	ChatProxy m_chatProxy { ChatProxy::Chat };
	ChatProxy m_teamChatProxy { ChatProxy::TeamChat };

	CallvotesModelProxy m_callvotesModel;

	ScoreboardModelProxy m_scoreboardModel;

	DemosResolver m_demosResolver;
	DemosModel m_demosModel { &m_demosResolver };
	DemoPlayer m_demoPlayer { this };

	GametypeOptionsModel m_gametypeOptionsModel;

	PlayersModel m_playersModel;

	ActionRequestsModel m_actionRequestsModel;

	HudEditorModel m_regularHudEditorModel { HudLayoutModel::Regular };
	HudEditorModel m_miniviewHudEditorModel { HudLayoutModel::Miniview };
	HudCommonDataModel m_hudCommonDataModel;
	HudPovDataModel m_hudPovDataModel;

	QString m_availableUpdateVersion;

	QString m_pendingDroppedConnectionTitle;
	QString m_droppedConnectionTitle;
	QString m_pendingDroppedConnectionMessage;
	QString m_droppedConnectionMessage;
	std::optional<ReconnectBehaviour_> m_pendingReconnectBehaviour;
	std::optional<ReconnectBehaviour_> m_reconnectBehaviour;

	connstate_t m_clientState { CA_UNINITIALIZED };
	bool m_isPlayingADemo { false };
	bool m_isOperator { false };

	enum ActiveMenuMask : unsigned {
		MainMenu             = 1 << 0,
		ConnectionScreen     = 1 << 1,
		InGameMenu           = 1 << 2,
		DemoPlaybackMenu     = 1 << 3,
		IntroScreen          = 1 << 4,
		UpdateScreen         = 1 << 5,
	};

	unsigned m_activeMenuMask { 0 };

	bool m_isRequestedToShowScoreboard { false };
	bool m_isShowingScoreboard { false };

	bool m_isRequestedToShowChatPopup { false };
	bool m_isRequestedToShowTeamChatPopup { false };
	bool m_isShowingChatPopup { false };
	bool m_isShowingTeamChatPopup { false };

	bool m_isShowingActionRequests { false };

	bool m_hasTeamChat { false };
	bool m_isShowingHud { false };

	bool m_isConsoleOpen { false };
	bool m_isClientDisconnected { true };

	bool m_canBeReady { false };
	bool m_isReady { false };

	bool m_canJoin { false };
	bool m_canSpectate { false };
	bool m_canJoinAlpha { false };
	bool m_canJoinBeta { false };

	bool m_canToggleChallengerStatus { false };
	bool m_isInChallengersQueue { false };

	bool m_hasStartedBackgroundMapLoading { false };
	bool m_hasSucceededBackgroundMapLoading { false };

	const int m_widthInPixels;
	const int m_heightInPixels;
	const int m_pixelsPerLogicalUnit { 0 };

	qreal m_mouseXY[2] { 0.0, 0.0 };

	int64_t m_oldSoundPlaybackTimestamp { 0 };
	uintptr_t m_oldSoundPlaybackTag { 0 };

	VarModificationTracker m_debugNativelyDrawnItemsChangesTracker { &v_debugNativelyDrawnItems };

	bool m_hasStartedSandboxGCSequence { false };
	bool m_hasPendingAdviseGCRequest { false };
	bool m_hasPendingForceGCRequest { false };
	int64_t m_lastGCTimestamp { 0 };

	static void initPersistentPart( int logicalUnitsToPixelRatio );
	static void registerFonts();
	static void registerFontFlavorsFromDirectory( const wsw::StringView &shortDirectoryName );
	static void registerFont( const wsw::StringView &path );
	static void registerCustomQmlTypes();
	static void retrieveVideoModes();
	[[maybe_unused]]
	bool updateGCThreshold();

	enum SandboxKind { MenuSandbox, HudSandbox };
	void registerContextProperties( QQmlContext *context, SandboxKind sandboxKind );

	[[nodiscard]]
	auto createQmlSandbox( int logicalWidth, int logicalHeight, SandboxKind kind ) -> std::unique_ptr<QmlSandbox>;

	[[nodiscard]]
	static auto colorForNum( int num ) -> QColor {
		const auto *v = color_table[num];
		return QColor::fromRgbF( v[0], v[1], v[2] );
	}

	const QColor m_colorBlack { colorForNum( 0 ) };
	const QColor m_colorRed { colorForNum( 1 ) };
	const QColor m_colorGreen { colorForNum( 2 ) };
	const QColor m_colorYellow { colorForNum( 3 ) };
	const QColor m_colorBlue { colorForNum( 4 ) };
	const QColor m_colorCyan { colorForNum( 5 ) };
	const QColor m_colorMagenta { colorForNum( 6 ) };
	const QColor m_colorWhite { colorForNum( 7 ) };
	const QColor m_colorOrange { colorForNum( 8 ) };
	const QColor m_colorGrey { colorForNum( 9 ) };

	const QVariantList m_consoleColors {
		m_colorBlack, m_colorRed, m_colorGreen, m_colorYellow, m_colorBlue,
		m_colorCyan, m_colorMagenta, m_colorWhite, m_colorOrange, m_colorGrey
	};

	const QStringList m_playerModels { "viciious", "bobot", "monada", "bigvic", "padpork", "silverclaw" };
	const QString m_defaultPlayerModel { DEFAULT_PLAYERMODEL };
	const QString m_defaultTeamPlayersModel { DEFAULT_TEAMPLAYERS_MODEL };
	const QString m_defaultTeamAlphaModel { DEFAULT_TEAMALPHA_MODEL };
	const QString m_defaultTeamBetaModel { DEFAULT_TEAMBETA_MODEL };

	static inline QJsonArray s_videoModeHeadingsList;
	static inline QJsonArray s_videoModeWidthValuesList;
	static inline QJsonArray s_videoModeHeightValuesList;

	void playSoundUsingLimiter( /* &'static */ const char *path );

	[[nodiscard]]
	bool isShowingDemoPlaybackMenu() const { return ( m_activeMenuMask & DemoPlaybackMenu ) != 0; }
	[[nodiscard]]
	bool isShowingMainMenu() const { return ( m_activeMenuMask & MainMenu ) != 0; }
	[[nodiscard]]
	bool isShowingConnectionScreen() const { return ( m_activeMenuMask & ConnectionScreen ) != 0; }
	[[nodiscard]]
	bool isShowingInGameMenu() const { return ( m_activeMenuMask & InGameMenu ) != 0; }
	[[nodiscard]]
	bool isShowingIntroScreen() const { return ( m_activeMenuMask & IntroScreen ) != 0; }
	[[nodiscard]]
	bool isShowingUpdateScreen() const { return ( m_activeMenuMask & UpdateScreen ) != 0; }

	[[nodiscard]]
	bool isShowingActionRequests() const { return m_isShowingActionRequests; }

	[[nodiscard]]
	bool isDebuggingNativelyDrawnItems() const;

	[[nodiscard]]
	bool hasPendingCVarChanges() const { return !m_pendingCVarChanges.empty(); }

	[[nodiscard]]
	bool isReactingToDroppedConnection() const { return m_reconnectBehaviour != std::nullopt; }
	[[nodiscard]]
	auto getReconnectBehaviour() const -> QVariant {
		return m_reconnectBehaviour ? QVariant( *m_reconnectBehaviour ) : QVariant();
	}
	[[nodiscard]]
	auto getDroppedConnectionTitle() const -> const QString & { return m_droppedConnectionTitle; }
	[[nodiscard]]
	auto getDroppedConnectionMessage() const -> const QString & { return m_droppedConnectionMessage; }

	explicit QtUISystem( int widthInPixels, int heightInPixels, int pixelsPerLogicalUnit );
	~QtUISystem() override;

	template <typename Value>
	void appendSetCVarCommand( const wsw::StringView &name, const Value &value );

	[[nodiscard]]
	auto findCVarOrThrow( const QByteArray &name ) const -> cvar_t *;

	void updateCVarAwareControls();
	void updateHudOccluders();

	auto mapLogicalRectToPixels( const QRectF &logicalRect ) const -> QRectF;

	void setActiveMenuMask( unsigned activeMask );

	void renderQml( QmlSandbox *sandbox );

	[[nodiscard]]
	auto getPressedMouseButtons() const -> Qt::MouseButtons;
	[[nodiscard]]
	auto getPressedKeyboardModifiers() const -> Qt::KeyboardModifiers;

	[[nodiscard]]
	auto getTargetWindowsForKeyboardInput( QQuickWindow *targetWindows[2] ) -> unsigned;

	[[nodiscard]]
	auto convertQuakeKeyToQtKey( int quakeKey ) const -> std::optional<Qt::Key>;
};

bool QtUISystem::updateGCThreshold() {
	unsigned varValue;
	// Note: Don't switch to the "menu" GC behavior while just using scoreboard
	if( ( m_activeMenuMask & ~DemoPlaybackMenu ) || m_isShowingChatPopup || m_isShowingTeamChatPopup ) {
		varValue = v_heapSizeGCThresholdMenu.get();
	} else {
		varValue = v_heapSizeGCThresholdInGame.get();
	}

	const auto oldGCTreshold  = qt_wswHeapSizeGCThreshold;
	qt_wswHeapSizeGCThreshold = (size_t)( 1024 * 1024 ) * (size_t)varValue;
	// Don't make GC advisory if the new actual threshold is greater than old one
	if( oldGCTreshold <= qt_wswHeapSizeGCThreshold ) {
		return false;
	}
	// Don't make immediate GC advisory if it looks like we are switching from gameplay to the menu.
	// Let the first following allocation trigger it naturally.
	// This idea is experimental.
	if( oldGCTreshold == v_heapSizeGCThresholdInGame.get() && qt_wswHeapSizeGCThreshold == v_heapSizeGCThresholdMenu.get() ) {
		return false;
	}
	return true;
}

[[nodiscard]]
static bool isAPrintableChar( int ch ) {
	// See https://en.cppreference.com/w/cpp/string/byte/isprint
	return ch >= 0 &&  ch <= 127 && std::isprint( (unsigned char)ch );
}

void QtUISystem::initPersistentPart( int pixelsPerLogicalUnit ) {
	if( !s_application ) {
		QCoreApplication::setAttribute( Qt::AA_EnableHighDpiScaling );

		// TODO: Sanitize the entire environment
		qputenv( "QT_SCALE_FACTOR", QByteArray::number( pixelsPerLogicalUnit ) );
		qputenv( "QT_AUTO_SCREEN_SCALE_FACTOR", "1" );

		s_application = new QGuiApplication( s_fakeArgc, s_fakeArgv );
		// Fix the overwritten locale, if any
		(void)std::setlocale( LC_ALL, "C" );

		// Force using the core profile inside Qt guts
		// https://bugreports.qt.io/browse/QTBUG-84099
		QSurfaceFormat format;
		format.setVersion( 3, 3 );
		format.setProfile( QSurfaceFormat::CoreProfile );
		QSurfaceFormat::setDefaultFormat( format );

		registerFonts();
		registerCustomQmlTypes();

		// Initialize the table of textual strings corresponding to characters
		for( const QString &s: s_charStrings ) {
			const auto offset = (int)( std::addressof( s ) - s_charStrings );
			if( isAPrintableChar( offset ) ) {
				s_charStrings[offset] = QString::asprintf( "%c", (char)offset );
			}
		}

		retrieveVideoModes();

		// Set some reasonable default prior to initialization of Qml engines
		qt_wswHeapSizeGCThreshold = v_heapSizeGCThresholdMenu.get();
	}
}

void QtUISystem::registerCustomQmlTypes() {
	const QString reason( "This type is a native code bridge and cannot be instantiated" );
	const char *const uri = "net.warsow";
	qmlRegisterUncreatableType<QtUISystem>( uri, 2, 6, "UISystem", reason );
	qmlRegisterUncreatableType<ChatProxy>( uri, 2, 6, "ChatProxy", reason );
	qmlRegisterUncreatableType<CallvotesListModel>( uri, 2, 6, "CallvotesModel", reason );
	qmlRegisterUncreatableType<GametypeDef>( uri, 2, 6, "GametypeDef", reason );
	qmlRegisterUncreatableType<GametypesModel>( uri, 2, 6, "GametypesModel", reason );
	qmlRegisterUncreatableType<ScoreboardModelProxy>( uri, 2, 6, "Scoreboard", reason );
	qmlRegisterUncreatableType<ScoreboardTeamModel>( uri, 2, 6, "ScoreboardTeamModel", reason );
	qmlRegisterUncreatableType<KeysAndBindingsModel>( uri, 2, 6, "KeysAndBindings", reason );
	qmlRegisterUncreatableType<ServerListModel>( uri, 2, 6, "ServerListModel", reason );
	qmlRegisterUncreatableType<DemosResolver>( uri, 2, 6, "DemosResolver", reason );
	qmlRegisterUncreatableType<DemoPlayer>( uri, 2, 6, "DemoPlayer", reason );
	qmlRegisterUncreatableType<GametypeOptionsModel>( uri, 2, 6, "GametypeOptionsModel", reason );
	qmlRegisterUncreatableType<HudLayoutModel>( uri, 2, 6, "HudLayoutModel", reason );
	qmlRegisterUncreatableType<HudEditorModel>( uri, 2, 6, "HudEditorModel", reason );
	qmlRegisterUncreatableType<InGameHudLayoutModel>( uri, 2, 6, "InGameHudLayoutModel", reason );
	qmlRegisterUncreatableType<HudDataModel>( uri, 2, 6, "HudDataModel", reason );
	qmlRegisterUncreatableType<HudCommonDataModel>( uri, 2, 6, "HudCommonDataModel", reason );
	qmlRegisterUncreatableType<HudPovDataModel>( uri, 2, 6, "HudPovDataModel", reason );
	qmlRegisterType<NativelyDrawnImage>( uri, 2, 6, "NativelyDrawnImage_Native" );
	qmlRegisterType<NativelyDrawnModel>( uri, 2, 6, "NativelyDrawnModel_Native" );
	qmlRegisterType<VideoSource>( uri, 2, 6, "WswVideoSource" );
	// CAUTION! Everything that uses a singleton must import the uri, otherwise another instance gets created
	qmlRegisterSingletonType( QUrl( "qrc:///Hud.qml" ), uri, 2, 6, "Hud" );
	qmlRegisterSingletonType( QUrl( "qrc:///UI.qml" ), uri, 2, 6, "UI" );
}

void QtUISystem::registerFontFlavorsFromDirectory( const wsw::StringView &shortDirectoryName ) {
	uiDebug() << "Registering UI font flavors from" << shortDirectoryName << "directory";

	wsw::StaticString<MAX_QPATH> path;
	path << "fonts/ui/"_asView << shortDirectoryName;

	wsw::fs::SearchResultHolder searchResultHolder;
	if( auto maybeCallResult = searchResultHolder.findDirFiles( path.asView(), ".ttf"_asView ) ) {
		if( maybeCallResult->getNumFiles() > 0 ) {
			// Sanity check: make sure they have a common prefix of at least one character
			// TODO: Allow iterating over slices, like [1:], so we don't need optional for the initially chosen prefix
			std::optional<char> prevPrefix;
			for( const wsw::StringView &fileName: *maybeCallResult ) {
				assert( !fileName.empty() );
				if( prevPrefix ) {
					if( prevPrefix != std::optional( fileName.front() ) ) {
						uiWarning() << "*.ttf files in"_asView << path << "do not have a minimal common prefix";
						break;
					}
				} else {
					prevPrefix = fileName.front();
				}
			}

			path.push_back( '/' );
			const auto dirPathLength = path.length();
			// It must be iterable again
			for( const wsw::StringView &fileName: *maybeCallResult ) {
				path.erase( dirPathLength );
				path << fileName;
				registerFont( path.asView() );
			}
		} else {
			wsw::StaticString<256> message;
			message << "Failed to find *.ttf files in"_asView << path;
			wsw::failWithRuntimeError( message.data() );
		}
	} else {
		wsw::StaticString<256> message;
		message << "Failed to enumerate files in"_asView << path;
		wsw::failWithRuntimeError( message.data() );
	}
}

void QtUISystem::registerFonts() {
	QFontDatabase::removeAllApplicationFonts();

	registerFontFlavorsFromDirectory( "regular"_asView );
	registerFontFlavorsFromDirectory( "heading"_asView );

	registerFontFlavorsFromDirectory( "symbols"_asView );

	// See the related to s_emojiFontFamily remark
#ifndef _WIN32
	registerFontFlavorsFromDirectory( "emoji"_asView );
#endif

	QFont font( "Ubuntu", 12 );
	font.setWeight( QFont::Normal );
	font.setStyleStrategy( (QFont::StyleStrategy)( font.styleStrategy() | QFont::NoFontMerging ) );
	QGuiApplication::setFont( font );
}

void QtUISystem::registerFont( const wsw::StringView &path ) {
	uiDebug() << "Registering UI font" << path;

	if( auto handle = wsw::fs::openAsReadHandle( path ) ) {
		const size_t size = handle->getInitialFileSize();
		QByteArray data( (int)size, Qt::Uninitialized );
		if( handle->readExact( data.data(), size ) ) {
			if( QFontDatabase::addApplicationFontFromData( data ) >= 0 ) {
				return;
			}
		}
	}

	wsw::StaticString<256> message;
	message << "Failed to register "_asView << path;
	wsw::failWithRuntimeError( message.data() );
}

void QtUISystem::registerContextProperties( QQmlContext *context, SandboxKind sandboxKind ) {
	context->setContextProperty( "__ui", this );
	// TODO: Show hud popups in the menu sandbox/context?
	context->setContextProperty( "__chatProxy", &m_chatProxy );
	context->setContextProperty( "__teamChatProxy", &m_teamChatProxy );
	context->setContextProperty( "__hudCommonDataModel", &m_hudCommonDataModel );
	context->setContextProperty( "__hudPovDataModel", &m_hudPovDataModel );

	// This condition not only helps to avoid global namespace pollution,
	// but first and foremost is aimed to prevent keeping excessive GC roots.

	if( sandboxKind == HudSandbox ) {
		context->setContextProperty( "__actionRequestsModel", &m_actionRequestsModel );
	} else {
		context->setContextProperty( "__serverListModel", &m_serverListModel );
		context->setContextProperty( "__keysAndBindings", &m_keysAndBindingsModel );
		context->setContextProperty( "__gametypesModel", &m_gametypesModel );
		context->setContextProperty( "__gametypeOptionsModel", &m_gametypeOptionsModel );
		context->setContextProperty( "__regularCallvotesModel", m_callvotesModel.getRegularModel() );
		context->setContextProperty( "__operatorCallvotesModel", m_callvotesModel.getOperatorModel() );
		context->setContextProperty( "__scoreboard", &m_scoreboardModel );
		context->setContextProperty( "__scoreboardSpecsModel", m_scoreboardModel.getSpecsModel() );
		context->setContextProperty( "__scoreboardPlayersModel", m_scoreboardModel.getPlayersModel() );
		context->setContextProperty( "__scoreboardAlphaModel", m_scoreboardModel.getAlphaModel() );
		context->setContextProperty( "__scoreboardBetaModel", m_scoreboardModel.getBetaModel() );
		context->setContextProperty( "__scoreboardMixedModel", m_scoreboardModel.getMixedModel() );
		context->setContextProperty( "__scoreboardChallengersModel", m_scoreboardModel.getChallengersModel() );
		context->setContextProperty( "__demosModel", &m_demosModel );
		context->setContextProperty( "__demosResolver", &m_demosResolver );
		context->setContextProperty( "__demoPlayer", &m_demoPlayer );
		context->setContextProperty( "__playersModel", &m_playersModel );
		context->setContextProperty( "__regularHudEditorModel", &m_regularHudEditorModel );
		context->setContextProperty( "__miniviewHudEditorModel", &m_miniviewHudEditorModel );
	}
}

void QtUISystem::retrieveVideoModes() {
	int width, height;
	for( unsigned i = 0; VID_GetModeInfo( &width, &height, i ); ++i ) {
		s_videoModeWidthValuesList.append( width );
		s_videoModeHeightValuesList.append( height );
		s_videoModeHeadingsList.append( QString::asprintf( "%dx%d", width, height ) );
	}
}

QmlSandbox::~QmlSandbox() {
	// Try being explicit with regard to deinitialization order. This is mandatory for the GL context.
	if( m_rootObject ) {
		if( auto *const item = qobject_cast<QQuickItem *>( m_rootObject ) ) {
			item->setParentItem( nullptr );
			item->setParent( nullptr );
		}
		delete m_rootObject;
		m_rootObject = nullptr;
	}
	m_component.reset();
	if( m_controlContext ) {
		const bool wasInUIRenderingMode = m_uiSystem->isInUIRenderingMode();
		if( !wasInUIRenderingMode ) {
			m_uiSystem->enterUIRenderingMode();
		}
		if( m_controlContext->makeCurrent( m_surface.get() ) ) {
			if( m_framebufferObject ) {
				m_framebufferObject.reset( nullptr );
			}
			// https://bugreports.qt.io/browse/QTBUG-42213
			m_control.reset();
		}
		// TODO: What to do if we fail to make the context to be current
		m_controlContext.reset();
		if( !wasInUIRenderingMode ) {
			m_uiSystem->leaveUIRenderingMode();
		}
	} else {
		assert( !m_framebufferObject );
		assert( !m_control );
	}
	m_engine.reset();
	m_surface.reset();
	m_window.reset();
}

void QmlSandbox::onSceneGraphInitialized() {
	const QSize bufferSize( m_uiSystem->m_widthInPixels, m_uiSystem->m_heightInPixels );
	m_framebufferObject.reset( new QOpenGLFramebufferObject( bufferSize, QOpenGLFramebufferObject::CombinedDepthStencil ) );
	m_window->setRenderTarget( m_framebufferObject.get() );
}

void QmlSandbox::onRenderRequested() {
	m_hasPendingRedraw = true;
}

void QmlSandbox::onSceneChanged() {
	m_hasPendingSceneChange = true;
}

void QmlSandbox::onComponentStatusChanged( QQmlComponent::Status status ) {
	if ( QQmlComponent::Ready != status ) {
		if( status == QQmlComponent::Error ) {
			uiError() << "The root Qml component loading has failed:" << m_component->errorString();
		}
		return;
	}

	m_rootObject = m_component->create();
	if( !m_rootObject ) {
		uiError() << "Failed to finish the root Qml component creation";
		return;
	}

	auto *rootItem = qobject_cast<QQuickItem*>( m_rootObject );
	if( !rootItem ) {
		uiError() << "The root Qml component is not a QQuickItem";
		return;
	}

	QQuickItem *const parentItem = m_window->contentItem();
	const QSizeF size( m_window->width(), m_window->height() );
	parentItem->setSize( size );
	rootItem->setParentItem( parentItem );
	rootItem->setParent( parentItem );
	rootItem->setSize( size );

	m_hasSucceededLoading = true;
}

static SingletonHolder<QtUISystem> uiSystemInstanceHolder;

void UISystem::init( int widthInPixels, int heightInPixels, int logicalUnitsToPixelsScale ) {
	uiSystemInstanceHolder.init( widthInPixels, heightInPixels, logicalUnitsToPixelsScale );
	VideoPlaybackSystem::init();
}

void UISystem::shutdown() {
	uiSystemInstanceHolder.instance()->dispatchShuttingDown();
	uiSystemInstanceHolder.shutdown();
	VideoPlaybackSystem::shutdown();
}

auto UISystem::instance() -> UISystem * {
	return uiSystemInstanceHolder.instance();
}

void QtUISystem::renderInternally() {
	WSW_PROFILER_SCOPE();

	if( m_menuSandbox && m_menuSandbox->requestsRendering() ) {
		enterUIRenderingMode();
		renderQml( m_menuSandbox.get() );
		leaveUIRenderingMode();
	}
	if( m_hudSandbox && m_hudSandbox->requestsRendering() ) {
		enterUIRenderingMode();
		renderQml( m_hudSandbox.get() );
		leaveUIRenderingMode();
	}
}

QtUISystem::QtUISystem( int widthInPixels, int heightInPixels, int pixelsPerLogicalUnit )
	: m_hudCommonDataModel( pixelsPerLogicalUnit ), m_widthInPixels( widthInPixels )
	, m_heightInPixels( heightInPixels ), m_pixelsPerLogicalUnit( pixelsPerLogicalUnit ) {
	assert( widthInPixels > 0 && heightInPixels > 0 && pixelsPerLogicalUnit > 0 );

	initPersistentPart( pixelsPerLogicalUnit );

	m_externalContext.reset( new QOpenGLContext );
	m_externalContext->setNativeHandle( VID_GetMainContextHandle() );
	if( !m_externalContext->create() ) {
		uiError() << "Failed to create a Qt wrapper of the main rendering context";
		return;
	}

	const int logicalWidth  = widthInPixels / pixelsPerLogicalUnit;
	const int logicalHeight = heightInPixels / pixelsPerLogicalUnit;
	assert( logicalWidth > 0 && logicalHeight > 0 );

	m_menuSandbox = createQmlSandbox( logicalWidth, logicalHeight, MenuSandbox );
	m_hudSandbox  = createQmlSandbox( logicalWidth, logicalHeight, HudSandbox );

	connect( &m_regularHudEditorModel, &HudEditorModel::hudUpdated, &m_hudCommonDataModel, &HudCommonDataModel::onHudUpdated );
	connect( &m_miniviewHudEditorModel, &HudEditorModel::hudUpdated, &m_hudCommonDataModel, &HudCommonDataModel::onHudUpdated );
}

QtUISystem::~QtUISystem() {
	// Ensure that sandboxes are deleted with a priority over other fields
	m_menuSandbox.reset();
	m_hudSandbox.reset();

	assert( !m_isInUIRenderingMode );
	// Don't try recycling these resources upon restart in another context
	NativelyDrawn::recycleResourcesInMainContext();
}

auto QtUISystem::createQmlSandbox( int logicalWidth, int logicalHeight, SandboxKind kind ) -> std::unique_ptr<QmlSandbox> {
	std::unique_ptr<QmlSandbox> sandbox( new QmlSandbox( this ) );

	QSurfaceFormat format;
	format.setDepthBufferSize( 24 );
	format.setStencilBufferSize( 8 );
	format.setMajorVersion( 3 );
	format.setMinorVersion( 3 );
	format.setRenderableType( QSurfaceFormat::OpenGL );
	format.setProfile( QSurfaceFormat::CoreProfile );

	assert( m_externalContext );

	sandbox->m_controlContext.reset( new QOpenGLContext );
	sandbox->m_controlContext->setFormat( format );
	sandbox->m_controlContext->setShareContext( m_externalContext.get() );
	if( !sandbox->m_controlContext->create() ) {
		uiError() << "Failed to create a dedicated OpenGL context for a Qml sandbox";
		return nullptr;
	}

	sandbox->m_control.reset( new CustomQuickRenderControl );
	sandbox->m_window.reset( new QQuickWindow( sandbox->m_control.get() ) );
	sandbox->m_window->setGeometry( 0, 0, logicalWidth, logicalHeight );
	sandbox->m_window->setColor( Qt::transparent );

	connect( sandbox->m_window.get(), &QQuickWindow::sceneGraphInitialized, sandbox.get(), &QmlSandbox::onSceneGraphInitialized );
	connect( sandbox->m_control.get(), &QQuickRenderControl::renderRequested, sandbox.get(), &QmlSandbox::onRenderRequested );
	connect( sandbox->m_control.get(), &QQuickRenderControl::sceneChanged, sandbox.get(), &QmlSandbox::onSceneChanged );

	sandbox->m_surface.reset( new QOffscreenSurface );
	sandbox->m_surface->setFormat( sandbox->m_controlContext->format() );
	sandbox->m_surface->create();
	if( !sandbox->m_surface->isValid() ) {
		uiError() << "Failed to create a dedicated Qt OpenGL offscreen surface";
		return nullptr;
	}

	const bool wasInUIRenderingMode = isInUIRenderingMode();
	if( !wasInUIRenderingMode ) {
		enterUIRenderingMode();
	}

	bool hadErrors = true;
	if( sandbox->m_controlContext->makeCurrent( sandbox->m_surface.get() ) ) {
		sandbox->m_control->initialize( sandbox->m_controlContext.get() );
		hadErrors = sandbox->m_controlContext->functions()->glGetError() != GL_NO_ERROR;
		sandbox->m_controlContext->doneCurrent();
	} else {
		uiError() << "Failed to make the dedicated Qt OpenGL rendering context current";
	}

	if( !wasInUIRenderingMode ) {
		leaveUIRenderingMode();
	}

	if( hadErrors ) {
		uiError() << "Failed to initialize the Qt Quick render control from the given GL context";
		return nullptr;
	}

	sandbox->m_engine.reset( new QQmlEngine );
	sandbox->m_engine->addImageProvider( "wsw", new wsw::ui::WswImageProvider );

	registerContextProperties( sandbox->m_engine->rootContext(), kind );

	sandbox->m_component.reset( new QQmlComponent( sandbox->m_engine.get() ) );

	connect( sandbox->m_component.get(), &QQmlComponent::statusChanged, sandbox.get(), &QmlSandbox::onComponentStatusChanged );

	sandbox->m_component->loadUrl( QUrl( kind == MenuSandbox ? "qrc:///MenuRootItem.qml" : "qrc:///HudRootItem.qml" ) );

	// Check onComponentStatusChanged() results
	if( sandbox->m_hasSucceededLoading ) {
		return sandbox;
	}
	return nullptr;
}

void QtUISystem::renderQml( QmlSandbox *sandbox ) {
	WSW_PROFILER_SCOPE();

	assert( sandbox->m_hasPendingSceneChange || sandbox->m_hasPendingRedraw );

	if( !sandbox->m_controlContext->makeCurrent( sandbox->m_surface.get() ) ) {
		// Consider this a fatal error
		Com_Error( ERR_FATAL, "Failed to make the dedicated Qt OpenGL rendering context current\n" );
	}

	assert( !sandbox->m_control->m_windowForDensityRetrieval );
	// Just setting QT_SCALE_FACTOR is insufficient as the Qt backend uses 1.0 as density
	// if QQuickRenderControl::renderWindowFor() returns nullptr (it always does).
	// See void QQuickWindowPrivate::renderSceneGraph(const QSize &size);
	sandbox->m_control->m_windowForDensityRetrieval = sandbox->m_window.get();

	if( sandbox->m_hasPendingSceneChange ) {
		// This MUST be performed in the GL context of the control.
		// Namely, these calls may create VAO-handled geometry, and VAOs are not shareable among contexts.
		sandbox->m_control->polishItems();
		sandbox->m_control->sync();
	}

	sandbox->m_hasPendingSceneChange = sandbox->m_hasPendingRedraw = false;

	sandbox->m_control->render();

	sandbox->m_hasValidFboContent = true;

	// Note that setting it once at construction leads to malfunction of some other parts, hence we reset it asap.
	// This is a hack, but at least we can avoid fragile patching of Qt.
	sandbox->m_control->m_windowForDensityRetrieval = nullptr;

	sandbox->m_controlContext->doneCurrent();
}

void QtUISystem::enterUIRenderingMode() {
	WSW_PROFILER_SCOPE();

	assert( !m_isInUIRenderingMode );
	m_isInUIRenderingMode = true;

	if( !GLimp_BeginUIRenderingHacks() ) {
		Com_Error( ERR_FATAL, "Failed to enter the UI rendering mode\n" );
	}
}

void QtUISystem::leaveUIRenderingMode() {
	WSW_PROFILER_SCOPE();

	assert( m_isInUIRenderingMode );
	m_isInUIRenderingMode = false;

	if( !GLimp_EndUIRenderingHacks() ) {
		Com_Error( ERR_FATAL, "Failed to leave the UI rendering mode\n" );
	}
}

void QtUISystem::drawMenuPartInMainContext() {
	WSW_PROFILER_SCOPE();

	if( m_menuSandbox && m_menuSandbox->m_hasValidFboContent ) {
		// TODO: All of this ties natively drawn items to the menu part

		const int64_t timestamp = getFrameTimestamp();
		const int64_t delta = wsw::min( (int64_t)33, timestamp - m_lastDrawMenuPartTimestamp );
		m_lastDrawMenuPartTimestamp = timestamp;

		// Ask Qml subscribers for actual values

		m_nativelyDrawnItems.clear();
		m_occludersOfNativelyDrawnItems.clear();

		Q_EMIT nativelyDrawnItemsOccludersRetrievalRequested();
		Q_EMIT nativelyDrawnItemsRetrievalRequested();

		m_nativelyDrawnOverlayHeap.clear();
		m_nativelyDrawnUnderlayHeap.clear();

		// Make deeper items get evicted first from a max-heap
		const auto cmp = []( const NativelyDrawn *lhs, const NativelyDrawn *rhs ) {
			return lhs->m_nativeZ > rhs->m_nativeZ;
		};

		for( NativelyDrawn *nativelyDrawn : m_nativelyDrawnItems ) {
			const QQuickItem *item = nativelyDrawn->m_selfAsItem;
			assert( item );

			if( item->isVisible() ) {
				if( nativelyDrawn->m_nativeZ < 0 ) {
					m_nativelyDrawnUnderlayHeap.push_back( nativelyDrawn );
					std::push_heap( m_nativelyDrawnUnderlayHeap.begin(), m_nativelyDrawnUnderlayHeap.end(), cmp );
				} else {
					// Don't draw natively drawn items on top of occluders.
					// TODO: We either draw everything or draw nothing, a proper clipping/
					// fragmented drawing would be a correct solution.
					// Still, this the current approach produces acceptable results.
					bool occluded = false;
					const QRectF itemBounds( item->mapRectToScene( item->boundingRect() ) );
					for( const QRectF &bounds: m_occludersOfNativelyDrawnItems ) {
						if( bounds.intersects( itemBounds ) ) {
							occluded = true;
							break;
						}
					}
					if( !occluded ) {
						m_nativelyDrawnOverlayHeap.push_back( nativelyDrawn );
						std::push_heap( m_nativelyDrawnOverlayHeap.begin(), m_nativelyDrawnOverlayHeap.end(), cmp );
					}
				}
			}
		}

		// This is quite inefficient as we switch rendering modes for different kinds of items.
		// Unfortunately this is mandatory for maintaining the desired Z order.
		// Considering the low number of items of this kind the performance impact should be negligible.

		while( !m_nativelyDrawnUnderlayHeap.empty() ) {
			std::pop_heap( m_nativelyDrawnUnderlayHeap.begin(), m_nativelyDrawnUnderlayHeap.end(), cmp );
			// Note: each drawSelfNatively() call manages 2D/scene mode on its own
			m_nativelyDrawnUnderlayHeap.back()->drawSelfNatively( timestamp, delta, m_pixelsPerLogicalUnit );
			m_nativelyDrawnUnderlayHeap.pop_back();
		}

		NativelyDrawn::recycleResourcesInMainContext();

		// Don't blit initial FBO content
		if( m_activeMenuMask || m_isShowingScoreboard ) {
			shader_s *const material = R_WrapMenuTextureHandleInMaterial( m_menuSandbox->m_framebufferObject->texture() );
			R_Set2DMode( true );
			R_DrawStretchPic( 0, 0, m_widthInPixels, m_heightInPixels, 0.0f, 1.0f, 1.0f, 0.0f, colorWhite, material );
			R_Set2DMode( false );
		}

		while( !m_nativelyDrawnOverlayHeap.empty() ) {
			std::pop_heap( m_nativelyDrawnOverlayHeap.begin(), m_nativelyDrawnOverlayHeap.end(), cmp );
			// Note: each drawSelfNatively() call manages 2D/scene mode on its own
			m_nativelyDrawnOverlayHeap.back()->drawSelfNatively( timestamp, delta, m_pixelsPerLogicalUnit );
			m_nativelyDrawnOverlayHeap.pop_back();
		}
	}
}

void QtUISystem::drawHudPartInMainContext() {
	WSW_PROFILER_SCOPE();

	if( m_hudSandbox && m_hudSandbox->m_hasValidFboContent ) {
		if( m_isShowingHud || m_isShowingChatPopup || m_isShowingTeamChatPopup || m_isShowingActionRequests ) {
			shader_s *const material = R_WrapHudTextureHandleInMaterial( m_hudSandbox->m_framebufferObject->texture() );

			m_boundsOfDrawnHudItems.clear();
			Q_EMIT displayedHudItemsRetrievalRequested();

			const qreal windowWidth  = m_widthInPixels;
			const qreal windowHeight = m_heightInPixels;

			// We cannot just blit texture regions that correspond to HUD items
			// as items may ovelap and are alpha-blended.
			// Instead, we mark non-overlapping grid regions that are (partially or fully) occupied by HUD items.

			const unsigned cellWidth      = 64 * m_pixelsPerLogicalUnit;
			const unsigned cellHeight     = 64 * m_pixelsPerLogicalUnit;
			const unsigned numGridColumns = (unsigned)( std::ceil( windowWidth ) / cellWidth ) + 1;
			const unsigned numGridRows    = (unsigned)( std::ceil( windowHeight ) / cellHeight ) + 1;

			// Let items make their footprint on the grid.

			m_drawnCellsMaskOfHudImage.resize( numGridRows * numGridColumns );
			auto *const __restrict maskData = m_drawnCellsMaskOfHudImage.data();
			std::fill( maskData, maskData + m_drawnCellsMaskOfHudImage.size(), 0 );

			m_columnRangesOfCellGridRows.resize( numGridRows );
			for( unsigned row = 0; row < numGridRows; ++row ) {
				m_columnRangesOfCellGridRows[row] = { numGridColumns, 0u };
			}

			for( const auto &[itemRect, margin]: m_boundsOfDrawnHudItems ) {
				assert( margin >= 0.0 );
				const auto minX = (unsigned)wsw::max( std::round( itemRect.x() - margin ), 0.0 );
				const auto maxX = (unsigned)wsw::min( std::round( itemRect.x() + itemRect.width() + margin ), windowWidth - 1.0 );
				const auto minY = (unsigned)wsw::max( std::round( itemRect.y() - margin ), 0.0 );
				const auto maxY = (unsigned)wsw::min( std::round( itemRect.y() + itemRect.height() + margin ), windowHeight - 1.0 );
				// This accounts for degenerate/clipped out rectangles
				if( minX < maxX && minY < maxY ) [[likely]] {
					const unsigned minColumn = minX / cellWidth;
					const unsigned minRow    = minY / cellHeight;
					const unsigned maxColumn = maxX / cellWidth;
					const unsigned maxRow    = maxY / cellHeight;
					assert( minRow <= maxRow && maxRow < numGridRows );
					assert( minColumn <= maxColumn && maxColumn < numGridColumns );
					for( unsigned row = minRow; row <= maxRow; ++row ) {
						for( unsigned column = minColumn; column <= maxColumn; ++column ) {
							assert( row * numGridColumns + column < numGridRows * numGridColumns );
							maskData[row * numGridColumns + column] = 1;
						}
						// Track min/max columns in row to optimize the scanning pass
						auto &[minColumnInRow, maxColumnInRow] = m_columnRangesOfCellGridRows[row];
						minColumnInRow = wsw::min( minColumnInRow, minColumn );
						maxColumnInRow = wsw::max( maxColumnInRow, maxColumn );
					}
				}
			}

			// Draw grid cells which have been marked by items
			R_Set2DMode( true );

			const float rcpWindowWidth  = 1.0f / (float)windowWidth;
			const float rcpWindowHeight = 1.0f / (float)windowHeight;

#if 0
			int numCellsDrawn = 0, numStripesDrawn = 0;
			for( unsigned row = 0; row < numGridRows; ++row ) {
				const auto [minColumnInRow, maxColumnInRow] = m_columnRangesOfCellGridRows[row];
				unsigned column = minColumnInRow;
				const auto y    = (int)row * (int)cellHeight;
				for(;; ) {
					// Try detecting the start of the next stripe of marked cells, or stop attempts.
					// This also accounts for empty (non-marked at all) rows.
					while( column <= maxColumnInRow && !maskData[row * numGridColumns + column] ) {
						++column;
					}
					// TODO: We would like to use labeled breaks which can be activated from the preceding loop
					if( !( column <= maxColumnInRow ) ) {
						break;
					}
					const auto stripeStartColumn = column;
					++column;
					while( column <= maxColumnInRow && maskData[row * numGridColumns + column] ) {
						++column;
					}
					const auto x           = (int)stripeStartColumn * (int)cellWidth;
					const auto stripeWidth = (int)cellWidth * (int)( column - stripeStartColumn );
					const float s1         = 0.0f + (float)x * rcpWindowWidth;
					const float t1         = 1.0f - (float)y * rcpWindowHeight;
					const float s2         = 0.0f + (float)( x + stripeWidth ) * rcpWindowWidth;
					const float t2         = 1.0f - (float)( y + cellHeight ) * rcpWindowHeight;
					const float color[4] {
						(float)(numStripesDrawn % 2 ), 1.0f - (float)( numStripesDrawn % 2 ), 1.0f, 0.25f
					};
					R_DrawStretchPic( x, y, stripeWidth, cellHeight, s1, t1, s2, t2, color, cgs.shaderWhite );
					numCellsDrawn += (int)( column - stripeStartColumn );
					numStripesDrawn++;
				}
			}
			uiNotice() << "Drawn" << numCellsDrawn << "cells of" << numGridRows * numGridColumns
				<< "as" << numStripesDrawn << "stripes in" << numGridRows << "rows";
#endif

			for( unsigned row = 0; row < numGridRows; ++row ) {
				const auto [minColumnInRow, maxColumnInRow] = m_columnRangesOfCellGridRows[row];
				unsigned column = minColumnInRow;
				const auto y    = (int)row * (int)cellHeight;
				for(;; ) {
					// Try detecting the start of the next stripe of marked cells, or stop attempts.
					// This also accounts for empty (non-marked at all) rows.
					while( column <= maxColumnInRow && !maskData[row * numGridColumns + column] ) {
						++column;
					}
					// TODO: We would like to use labeled breaks which can be activated from the preceding loop
					if( !( column <= maxColumnInRow ) ) {
						break;
					}
					const auto stripeStartColumn = column;
					++column;
					while( column <= maxColumnInRow && maskData[row * numGridColumns + column] ) {
						++column;
					}
					const auto x           = (int)stripeStartColumn * (int)cellWidth;
					const auto stripeWidth = (int)cellWidth * (int)( column - stripeStartColumn );
					const float s1         = 0.0f + (float)x * rcpWindowWidth;
					const float t1         = 1.0f - (float)y * rcpWindowHeight;
					const float s2         = 0.0f + (float)( x + stripeWidth ) * rcpWindowWidth;
					const float t2         = 1.0f - (float)( y + cellHeight ) * rcpWindowHeight;
					R_DrawStretchPic( x, y, stripeWidth, cellHeight, s1, t1, s2, t2, colorWhite, material );
				}
			}

			R_Set2DMode( false );
		}
	}


}

void QtUISystem::drawCursorInMainContext() {
	if( m_activeMenuMask || CG_UsesTiledView() ) {
		R_Set2DMode( true );

		// TODO: Handle precaching of resources properly
		auto *cursorMaterial = R_RegisterPic( "gfx/ui/cursor.tga" );

		const auto x   = (int)m_mouseXY[0] * m_pixelsPerLogicalUnit;
		const auto y   = (int)m_mouseXY[1] * m_pixelsPerLogicalUnit;
		const int side = 32 * m_pixelsPerLogicalUnit;

		R_DrawStretchPic( x, y, side, side, 0.0f, 0.0f, 1.0f, 1.0f, colorWhite, cursorMaterial );
		R_Set2DMode( false );
	}
}

void QtUISystem::drawBackgroundMapIfNeeded() {
	if( m_clientState != CA_DISCONNECTED ) {
		m_hasStartedBackgroundMapLoading = false;
		m_hasSucceededBackgroundMapLoading = false;
		return;
	}

	if( !m_hasStartedBackgroundMapLoading ) {
		R_RegisterWorldModel( UI_BACKGROUND_MAP_PATH );
		m_hasStartedBackgroundMapLoading = true;
	} else if( !m_hasSucceededBackgroundMapLoading ) {
		if( R_RegisterModel( UI_BACKGROUND_MAP_PATH ) ) {
			m_hasSucceededBackgroundMapLoading = true;
		}
	}

	if( !m_hasSucceededBackgroundMapLoading ) {
		return;
	}

	if( !( m_activeMenuMask & ( MainMenu | IntroScreen | UpdateScreen ) ) ) {
		return;
	}

	refdef_t rdf;
	memset( &rdf, 0, sizeof( rdf ) );

	const auto widthAndHeight = std::make_pair( m_widthInPixels, m_heightInPixels );
	std::tie( rdf.x, rdf.y ) = std::make_pair( 0, 0 );
	std::tie( rdf.width, rdf.height ) = widthAndHeight;

	// This is a copy-paste from Warsow 2.1 map_ui.pk3 CSS
	const vec3_t origin { 302.0f, -490.0f, 120.0f };
	const vec3_t angles { 0, -240, 0 };

	VectorCopy( origin, rdf.vieworg );
	AnglesToAxis( angles, rdf.viewaxis );
	rdf.fov_x = 90.0f;
	rdf.fov_y = CalcFov( 90.0f, rdf.width, rdf.height );
	AdjustFov( &rdf.fov_x, &rdf.fov_y, rdf.width, rdf.height, false );
	rdf.time = 0;

	std::tie( rdf.scissor_x, rdf.scissor_y ) = std::make_pair( 0, 0 );
	std::tie( rdf.scissor_width, rdf.scissor_height ) = widthAndHeight;

	BeginDrawingScenes();
	ExecuteSingleDrawSceneRequestNonSpeedCritical( CreateDrawSceneRequest( rdf ) );
	EndDrawingScenes();
}

void QtUISystem::runGCIfNeeded() {
	WSW_PROFILER_SCOPE();

	[[maybe_unused]]
	const auto currTimestamp = getFrameTimestamp();

	// Update the threshold every frame
	const bool shouldStartGCDueToThresholdChange = updateGCThreshold();
	if( shouldStartGCDueToThresholdChange ) {
		// In this case, trying to distribute updates over frames does not make sense
		// (it's very likely that the updated threshold is going to trigger GC in both engines ASAP)
		m_menuSandbox->m_engine->collectGarbage();
		m_hudSandbox->m_engine->collectGarbage();
		m_lastGCTimestamp = currTimestamp;
	} else {
		bool shouldStartSandboxGCSequence = false;
		if( m_hasPendingForceGCRequest ) {
			shouldStartSandboxGCSequence = true;
		} else if( m_hasPendingAdviseGCRequest ) {
			// Apply cooldown if the request is advisory, not forceful
			if( m_lastGCTimestamp + 1000 < currTimestamp ) {
				shouldStartSandboxGCSequence = true;
			}
		}
		if( shouldStartSandboxGCSequence ) {
			m_menuSandbox->m_engine->collectGarbage();
			m_hasStartedSandboxGCSequence = true;
			m_lastGCTimestamp = currTimestamp;
		} else if( m_hasStartedSandboxGCSequence ) {
			m_hudSandbox->m_engine->collectGarbage();
			m_hasStartedSandboxGCSequence = false;
		}
	}

	m_hasPendingForceGCRequest = m_hasPendingAdviseGCRequest = false;
}

void QtUISystem::handleGCSafepoint( GCSafepointKind kind ) {
	switch( kind ) {
		case GCSafepointKind::Respawn: {
			m_hasPendingAdviseGCRequest = true;
		} break;
		case GCSafepointKind::Teleport: {
			if( v_runGCOnTeleportEvent.get() ) {
				m_hasPendingAdviseGCRequest = true;
			}
		} break;
	}
}

void QtUISystem::beginRegistration() {
}

void QtUISystem::endRegistration() {
}

void QtUISystem::showMainMenu() {
	setActiveMenuMask( MainMenu );
}

void QtUISystem::returnFromInGameMenu() {
	if( m_isPlayingADemo ) {
		setActiveMenuMask( DemoPlaybackMenu );
	} else {
		setActiveMenuMask( 0 );
	}
}

void QtUISystem::returnFromMainMenu() {
	if( m_clientState == CA_ACTIVE ) {
		setActiveMenuMask( InGameMenu );
	}
}

void QtUISystem::finishIntro() {
	m_pendingFinishIntro = true;
}

void QtUISystem::hideUpdates() {
	m_pendingHideUpdates = true;
}

void QtUISystem::downloadUpdates() {
	Sys_OpenURLInBrowser( APP_URL );
	quit();
}

void QtUISystem::closeChatPopup() {
	const bool wasShowingChatPopup = m_isShowingChatPopup;
	const bool wasShowingTeamChatPopup = m_isShowingTeamChatPopup;

	m_isShowingChatPopup = false;
	m_isShowingTeamChatPopup = false;
	m_isRequestedToShowChatPopup = false;
	m_isRequestedToShowTeamChatPopup = false;

	if( wasShowingChatPopup ) {
		Q_EMIT isShowingChatPopupChanged( false );
	}
	if( wasShowingTeamChatPopup ) {
		Q_EMIT isShowingTeamChatPopupChanged( false );
	}

	m_hasPendingAdviseGCRequest = true;
}

void QtUISystem::setActiveMenuMask( unsigned activeMask ) {
	if( m_activeMenuMask == activeMask ) {
		return;
	}

	const auto oldActiveMask = m_activeMenuMask;

	const bool wasShowingMainMenu = isShowingMainMenu();
	const bool wasShowingConnectionScreen = isShowingConnectionScreen();
	const bool wasShowingInGameMenu = isShowingInGameMenu();
	const bool wasShowingDemoPlaybackMenu = isShowingDemoPlaybackMenu();
	const bool wasShowingIntroScreen = isShowingIntroScreen();
	const bool wasShowingUpdateScreen = isShowingUpdateScreen();

	m_activeMenuMask = activeMask;

	const bool _isShowingMainMenu = isShowingMainMenu();
	const bool _isShowingConnectionScreen = isShowingConnectionScreen();
	const bool _isShowingInGameMenu = isShowingInGameMenu();
	const bool _isShowingDemoPlaybackMenu = isShowingDemoPlaybackMenu();
	const bool _isShowingIntroScreen = isShowingIntroScreen();
	const bool _isShowingUpdateScreen = isShowingUpdateScreen();

	if( wasShowingMainMenu != _isShowingMainMenu ) {
		Q_EMIT isShowingMainMenuChanged( _isShowingMainMenu );
	}
	if( wasShowingConnectionScreen != _isShowingConnectionScreen ) {
		Q_EMIT isShowingConnectionScreenChanged( _isShowingConnectionScreen );
	}
	if( wasShowingInGameMenu != _isShowingInGameMenu ) {
		Q_EMIT isShowingInGameMenuChanged( _isShowingInGameMenu );
	}
	if( wasShowingDemoPlaybackMenu != _isShowingDemoPlaybackMenu ) {
		Q_EMIT isShowingDemoPlaybackMenuChanged( _isShowingDemoPlaybackMenu );
	}
	if( wasShowingIntroScreen != _isShowingIntroScreen ) {
		Q_EMIT isShowingIntroScreenChanged( _isShowingIntroScreen );
	}
	if( wasShowingUpdateScreen != _isShowingUpdateScreen ) {
		Q_EMIT isShowingUpdateScreenChanged( _isShowingUpdateScreen );
	}

	if( m_activeMenuMask && !oldActiveMask ) {
		CL_ClearInputState();
	}

	// Force GC upon switching from the menu back to gameplay
	if( !( m_activeMenuMask & ~DemoPlaybackMenu ) && ( oldActiveMask & ~DemoPlaybackMenu ) ) {
		m_hasPendingForceGCRequest = true;
	}
}

void QtUISystem::refreshProperties() {
	WSW_PROFILER_SCOPE();

	const auto lastClientState = m_clientState;
	const auto actualClientState = cls.state;
	m_clientState = actualClientState;

	const bool wasPlayingADemo = m_isPlayingADemo;
	const bool isPlayingADemo = cls.demoPlayer.playing;
	m_isPlayingADemo = isPlayingADemo;

	if( !s_hasFinishedIntro || !s_hasDisplayedUpdates ) {
		if( actualClientState > CA_DISCONNECTED || m_pendingReconnectBehaviour != std::nullopt ) {
			m_pendingFinishIntro = true;
			m_pendingHideUpdates = true;
		}
	}

	bool checkMaskChanges = false;
	if( m_clientState != lastClientState ) {
		checkMaskChanges = true;
	} else if( isPlayingADemo != wasPlayingADemo ) {
		checkMaskChanges = true;
	} else if( m_pendingReconnectBehaviour != m_reconnectBehaviour ) {
		checkMaskChanges = true;
	}

	if( m_pendingFinishIntro ) {
		s_hasFinishedIntro   = true;
		m_pendingFinishIntro = false;
		checkMaskChanges     = true;
	}

	if( m_pendingHideUpdates ) {
		s_hasDisplayedUpdates = true;
		m_pendingHideUpdates  = false;
		checkMaskChanges      = true;
	}

	bool shouldStartMenuPlaylist = false;
	if( m_hasCheckedFirstFrame ) {
		m_hasCheckedFirstFrame = false;
		checkMaskChanges = true;
		shouldStartMenuPlaylist = true;
	}

	if( !shouldStartMenuPlaylist ) {
		if( actualClientState != lastClientState && actualClientState == CA_DISCONNECTED && !m_pendingReconnectBehaviour ) {
			shouldStartMenuPlaylist = true;
		}
	}
	if( m_pendingReconnectBehaviour == std::nullopt && m_reconnectBehaviour != std::nullopt ) {
		if( actualClientState == CA_DISCONNECTED ) {
			shouldStartMenuPlaylist = true;
		}
	}

	if( checkMaskChanges ) {
		const bool wasShowingScoreboard = m_isShowingScoreboard;
		const bool wasShowingChatPopup = m_isShowingChatPopup;
		const bool wasShowingTeamChatPopup = m_isShowingTeamChatPopup;
		if( !s_hasDisplayedUpdates ) {
			setActiveMenuMask( UpdateScreen );
		} else if( !s_hasFinishedIntro ) {
			setActiveMenuMask( IntroScreen );
		} else if( m_pendingReconnectBehaviour ) {
			m_reconnectBehaviour = m_pendingReconnectBehaviour;
			setActiveMenuMask( ConnectionScreen );
			m_droppedConnectionTitle   = m_pendingDroppedConnectionTitle;
			m_droppedConnectionMessage = m_pendingDroppedConnectionMessage;
			Q_EMIT droppedConnectionTitleChanged( getDroppedConnectionTitle() );
			Q_EMIT droppedConnectionMessageChanged( getDroppedConnectionMessage() );
			Q_EMIT reconnectBehaviourChanged( getReconnectBehaviour() );
			Q_EMIT isReactingToDroppedConnectionChanged( true );
		} else if( actualClientState == CA_DISCONNECTED ) {
			setActiveMenuMask( MainMenu );
			m_chatProxy.clear();
			m_teamChatProxy.clear();
		} else if( actualClientState == CA_ACTIVE ) {
			if( isPlayingADemo ) {
				setActiveMenuMask( DemoPlaybackMenu );
			} else {
				setActiveMenuMask( InGameMenu );
			}
			reloadOptions();
		} else if( actualClientState >= CA_GETTING_TICKET && actualClientState <= CA_LOADING ) {
			setActiveMenuMask( ConnectionScreen );
		}
		// Hide scoreboard upon state changes
		m_isShowingScoreboard = false;
		m_isRequestedToShowScoreboard = false;
		if( wasShowingScoreboard ) {
			Q_EMIT isShowingScoreboardChanged( false );
		}
		// Hide chat upon state changes
		m_isShowingChatPopup = false;
		m_isRequestedToShowChatPopup = false;
		m_isShowingTeamChatPopup = false;
		m_isRequestedToShowTeamChatPopup = false;
		if( wasShowingChatPopup ) {
			Q_EMIT isShowingChatPopupChanged( false );
		}
		if( wasShowingTeamChatPopup ) {
			Q_EMIT isShowingTeamChatPopupChanged( false );
		}

		// Reset the state related to the dropped connection for robustness
		if( actualClientState != CA_DISCONNECTED || m_pendingReconnectBehaviour == std::nullopt ) {
			const bool wasReactingToDroppedConnection = isReactingToDroppedConnection();
			m_pendingReconnectBehaviour               = std::nullopt;
			m_reconnectBehaviour                      = std::nullopt;
			m_pendingDroppedConnectionMessage.clear();
			m_pendingDroppedConnectionTitle.clear();
			m_droppedConnectionTitle.clear();
			m_droppedConnectionMessage.clear();
			if( wasReactingToDroppedConnection ) {
				assert( !isReactingToDroppedConnection() );
				Q_EMIT isReactingToDroppedConnectionChanged( false );
				Q_EMIT reconnectBehaviourChanged( QVariant() );
				Q_EMIT droppedConnectionTitleChanged( QString() );
				Q_EMIT droppedConnectionMessageChanged( QString() );
			}
		}
		m_hudPovDataModel.resetHudFeed();
		m_hudCommonDataModel.resetHudFeed();
	}

	const bool hadTeamChat = m_hasTeamChat;
	m_hasTeamChat = false;
	if( CL_Cmd_Exists( "say_team"_asView ) ) {
		m_hasTeamChat = ( CG_RealClientTeam() == TEAM_SPECTATOR ) || ( GS_TeamBasedGametype( *cggs ) && !GS_IndividualGametype( *cggs ) );
	}

	if( hadTeamChat != m_hasTeamChat ) {
		// Hide all popups forcefully in this case
		m_isRequestedToShowChatPopup = false;
		m_isRequestedToShowTeamChatPopup = false;
		Q_EMIT hasTeamChatChanged( m_hasTeamChat );
	}

	bool shouldShowChatPopup     = false;
	bool shouldShowTeamChatPopup = false;
	bool shouldShowScoreboard    = false;
	if( !( m_activeMenuMask & ~DemoPlaybackMenu ) ) {
		shouldShowChatPopup     = m_isRequestedToShowChatPopup;
		shouldShowTeamChatPopup = m_isRequestedToShowTeamChatPopup;
		shouldShowScoreboard    = m_isRequestedToShowScoreboard;
		if( !shouldShowScoreboard && actualClientState == CA_ACTIVE && GS_MatchState( *cggs ) > MATCH_STATE_PLAYTIME ) {
			shouldShowScoreboard = true;
		}
	}

	if( m_isShowingScoreboard != shouldShowScoreboard ) {
		m_isShowingScoreboard = shouldShowScoreboard;
		m_hasPendingAdviseGCRequest |= !shouldShowScoreboard;
		Q_EMIT isShowingScoreboardChanged( m_isShowingScoreboard );
	}
	// Note: This code is simular to the scoreboard handling code for consistency reasons,
	// but the actual work for closing popups is performed by closeChatPopup()
	if( m_isShowingChatPopup != shouldShowChatPopup ) {
		m_isShowingChatPopup = shouldShowChatPopup;
		m_hasPendingAdviseGCRequest |= !shouldShowChatPopup;
		Q_EMIT isShowingChatPopupChanged( m_isShowingChatPopup );
	}

	if( m_isShowingTeamChatPopup != shouldShowTeamChatPopup ) {
		m_isShowingTeamChatPopup = shouldShowTeamChatPopup;
		m_hasPendingAdviseGCRequest |= !shouldShowTeamChatPopup;
		Q_EMIT isShowingTeamChatPopupChanged( m_isShowingTeamChatPopup );
	}

	const bool wasShowingHud = m_isShowingHud;
	m_isShowingHud = actualClientState == CA_ACTIVE && !( m_activeMenuMask & MainMenu );
	if( m_isShowingHud != wasShowingHud ) {
		Q_EMIT isShowingHudChanged( m_isShowingHud );
	}

	const bool wasShowingActionRequests = m_isShowingActionRequests;
	m_isShowingActionRequests = !m_actionRequestsModel.empty() && !m_activeMenuMask;
	if( wasShowingActionRequests != m_isShowingActionRequests ) {
		Q_EMIT isShowingActionRequestsChanged( m_isShowingActionRequests );
	}

	const bool wasConsoleOpen = m_isConsoleOpen;
	m_isConsoleOpen = Con_HasKeyboardFocus();
	if( wasConsoleOpen != m_isConsoleOpen ) {
		Q_EMIT isConsoleOpenChanged( m_isConsoleOpen );
	}

	const bool wasClientDisconnected = m_isClientDisconnected;
	m_isClientDisconnected = actualClientState <= CA_DISCONNECTED;
	if( wasClientDisconnected != m_isClientDisconnected ) {
		Q_EMIT isClientDisconnectedChanged( m_isClientDisconnected );
	}

	if( m_debugNativelyDrawnItemsChangesTracker.checkAndReset() ) {
		Q_EMIT isDebuggingNativelyDrawnItemsChanged( v_debugNativelyDrawnItems.get() );
	}

	if( const bool oldCanBeReady = m_canBeReady; oldCanBeReady != ( m_canBeReady = CG_CanBeReady() ) ) {
		Q_EMIT canBeReadyChanged( m_canBeReady );
	}

	if( const bool wasReady = m_isReady; wasReady != ( m_isReady = CG_IsReady() ) ) {
		Q_EMIT isReadyChanged( m_isReady );
	}

	if( const bool wasOperator = m_isOperator; wasOperator != ( m_isOperator = CG_IsOperator() ) ) {
		Q_EMIT isOperatorChanged( m_isOperator );
	}

	const bool oldCanSpectate = m_canSpectate;
	const bool oldCanJoin = m_canJoin;
	const bool oldCanJoinAlpha = m_canJoinAlpha;
	const bool oldCanJoinBeta = m_canJoinBeta;
	const bool oldCanToggleChallengerStatus = m_canToggleChallengerStatus;
	const bool oldIsInChallengersQueue = m_isInChallengersQueue;

	// TODO: This is fine for now but something more sophisticated should be really used
	// TODO: Send these values via client stat flags
	m_canSpectate = m_canJoin = m_canJoinAlpha = m_canJoinBeta = false;
	m_isInChallengersQueue = m_canToggleChallengerStatus = false;
	if( actualClientState == CA_ACTIVE ) {
		const bool hasChallengersQueue = GS_HasChallengers( *cggs );
		const int team = CG_MyRealTeam();
		m_canSpectate = ( team != TEAM_SPECTATOR );
		m_isInChallengersQueue = CG_IsChallenger();
		if( GS_MatchState( *cggs ) <= MATCH_STATE_PLAYTIME ) {
			if( hasChallengersQueue ) {
				m_canToggleChallengerStatus = true;
			} else {
				m_canJoin = ( team == TEAM_SPECTATOR );
				if( CG_HasTwoTeams() ) {
					m_canJoinAlpha = ( team != TEAM_ALPHA );
					m_canJoinBeta = ( team != TEAM_BETA );
				}
			}
		}
	}

	if( oldCanSpectate != m_canSpectate ) {
		Q_EMIT canSpectateChanged( m_canSpectate );
	}
	if( oldCanJoin != m_canJoin ) {
		Q_EMIT canJoinChanged( m_canJoin );
	}
	if( oldCanJoinAlpha != m_canJoinAlpha ) {
		Q_EMIT canJoinAlphaChanged( m_canJoinAlpha );
	}
	if( oldCanJoinBeta != m_canJoinBeta ) {
		Q_EMIT canJoinBetaChanged( m_canJoinBeta );
	}
	if( oldCanToggleChallengerStatus != m_canToggleChallengerStatus ) {
		Q_EMIT canToggleChallengerStatusChanged( m_canToggleChallengerStatus );
	}
	if( oldIsInChallengersQueue != m_isInChallengersQueue ) {
		Q_EMIT isInChallengersQueueChanged( m_isInChallengersQueue );
	}

	m_keysAndBindingsModel.checkUpdates();
	m_demoPlayer.checkUpdates();
	m_actionRequestsModel.update();

	const auto timestamp = getFrameTimestamp();
	VideoPlaybackSystem::instance()->update( timestamp );
	m_hudCommonDataModel.checkPropertyChanges( timestamp );

	const auto oldPovPlayerNum  = m_hudPovDataModel.getPlayerNum();
	const bool wasPovAlive      = m_hudPovDataModel.isPovAlive();
	const bool wasUsingChasePov = m_hudPovDataModel.isUsingChasePov();

	if( CG_IsViewAttachedToPlayer() ) {
		const unsigned viewStateIndex = CG_GetPrimaryViewStateIndex();
		m_hudPovDataModel.setViewStateIndex( viewStateIndex );
		if( const std::optional<unsigned> playerNum = CG_GetPlayerNumForViewState( viewStateIndex ) ) {
			m_hudPovDataModel.setPlayerNum( *playerNum );
		} else {
			m_hudPovDataModel.clearPlayerNum();
		}
	} else {
		m_hudPovDataModel.setViewStateIndex( CG_GetOurClientViewStateIndex() );
		m_hudPovDataModel.clearPlayerNum();
	}

	m_hudPovDataModel.checkPropertyChanges( timestamp );

	if( m_hudPovDataModel.isUsingChasePov() != wasUsingChasePov ) {
		m_hasPendingAdviseGCRequest = true;
	} else if( m_hudPovDataModel.isUsingChasePov() ) {
		if( m_hudPovDataModel.getPlayerNum() != oldPovPlayerNum ) {
			m_hasPendingAdviseGCRequest = true;
		}
	} else {
		// Don't trigger GC unless we are really resuming gameplay.
		if( m_hudPovDataModel.isPovAlive() && !wasPovAlive ) {
			m_hasPendingForceGCRequest = true;
		}
	}

	updateCVarAwareControls();
	updateHudOccluders();

#ifndef _WIN32
	QGuiApplication::processEvents( QEventLoop::AllEvents );
#endif

	if( shouldStartMenuPlaylist ) {
		const char *const playlist = "sounds/music/menu.m3u";
		SoundSystem::instance()->startBackgroundTrack( playlist, playlist, 3 );
	}
}

bool QtUISystem::handleMouseMovement( float frameTime, int dx, int dy ) {
	if( !m_menuSandbox ) {
		return false;
	}

	if( !m_activeMenuMask && !CG_UsesTiledView() ) {
		return false;
	}

	if( !dx && !dy ) {
		return true;
	}

	const int bounds[2] = { m_menuSandbox->m_window->width(), m_menuSandbox->m_window->height() };
	const int deltas[2] = { dx, dy };

	float sensitivity = v_mouseSensitivity.get();
	if( frameTime > 0 ) {
		sensitivity += v_mouseAccel.get() * std::sqrt( dx * dx + dy * dy ) / (float)( frameTime );
	}

	for( int i = 0; i < 2; ++i ) {
		if( !deltas[i] ) {
			continue;
		}
		qreal scaledDelta = ( (qreal)deltas[i] * sensitivity );
		// Make sure we won't lose a mouse movement due to fractional part loss
		if( !scaledDelta ) {
			scaledDelta = Q_sign( deltas[i] );
		}
		m_mouseXY[i] += scaledDelta;
		Q_clamp( m_mouseXY[i], 0, bounds[i] );
	}

	wsw::StaticVector<QQuickWindow *, 2> targetWindows;
	targetWindows.push_back( m_menuSandbox->m_window.get() );
	if( m_hudSandbox ) {
		targetWindows.push_back( m_hudSandbox->m_window.get() );
	}

	const QPointF point( m_mouseXY[0], m_mouseXY[1] );
	const Qt::MouseButtons mouseButtons   = getPressedMouseButtons();
	const Qt::KeyboardModifiers modifiers = getPressedKeyboardModifiers();
	for( QQuickWindow *targetWindow : targetWindows ) {
		QMouseEvent event( QEvent::MouseMove, point, Qt::NoButton, mouseButtons, modifiers );
		QCoreApplication::sendEvent( targetWindow, &event );
	}

	return true;
}

bool QtUISystem::grabsKeyboardAndMouseButtons() const {
	if( m_activeMenuMask != 0 ) {
		return ( m_activeMenuMask & DemoPlaybackMenu ) == 0;
	}
	return m_isShowingChatPopup || m_isShowingTeamChatPopup;
}

bool QtUISystem::grabsMouseMovement() const {
	if( m_activeMenuMask != 0 ) {
		return ( m_activeMenuMask & DemoPlaybackMenu ) == 0;
	}
	return m_isShowingChatPopup || m_isShowingTeamChatPopup;
}

void QtUISystem::handleEscapeKey() {
	bool didSpecialHandling = false;

	if( m_clientState == CA_ACTIVE ) {
		const bool isPostmatch = GS_MatchState( *cggs ) > MATCH_STATE_PLAYTIME;

		// The scoreboard is always shown post-match so we can't handle the ket by toggling it
		if( !isPostmatch ) {
			if( !( m_activeMenuMask & ~DemoPlaybackMenu ) ) {
				if( m_isRequestedToShowScoreboard ) {
					m_isRequestedToShowScoreboard = false;
					didSpecialHandling            = true;
				}
			}
		}

		if( !didSpecialHandling ) {
			if( m_activeMenuMask == 0 ) {
				if( isPostmatch ) {
					setActiveMenuMask( InGameMenu );
					didSpecialHandling = true;
				} else {
					if( !m_isRequestedToShowChatPopup && !m_isRequestedToShowTeamChatPopup ) {
						setActiveMenuMask( InGameMenu );
						didSpecialHandling = true;
					}
				}
			}
			if( didSpecialHandling ) {
				playForwardSound();
			}
		}
	}

	if( !didSpecialHandling ) {
		QQuickWindow *targetWindows[2] { nullptr, nullptr };
		if( const unsigned numTargetWindows = getTargetWindowsForKeyboardInput( targetWindows ) ) {
			const Qt::KeyboardModifiers modifiers = getPressedKeyboardModifiers();
			for( unsigned i = 0; i < numTargetWindows; ++i ) {
				QKeyEvent keyEvent( QEvent::KeyPress, Qt::Key_Escape, modifiers );
				QCoreApplication::sendEvent( targetWindows[i], &keyEvent );
			}
		}
	}
}

bool QtUISystem::handleKeyEvent( int quakeKey, bool keyDown ) {
	QQuickWindow *targetWindows[2] { nullptr, nullptr };
	const unsigned numTargetWindows = getTargetWindowsForKeyboardInput( targetWindows );
	if( !numTargetWindows ) {
		if( keyDown ) {
			return m_actionRequestsModel.handleKeyEvent( quakeKey );
		}
		return false;
	}

	[[maybe_unused]]
	bool propagateToCGameIfNotAccepted = false;
	if( m_menuSandbox ) {
		if( QQuickWindow *window = m_menuSandbox->m_window.get(); window == targetWindows[0] || window == targetWindows[1] ) {
			if( m_activeMenuMask & DemoPlaybackMenu ) {
				propagateToCGameIfNotAccepted = true;
			}
		}
	}
	if( !propagateToCGameIfNotAccepted ) {
		if( m_hudSandbox ) {
			if( QQuickWindow *window = m_hudSandbox->m_window.get(); window == targetWindows[0] || window == targetWindows[1] ) {
				if( CG_UsesTiledView() ) {
					propagateToCGameIfNotAccepted = true;
				}
			}
		}
	}

	Qt::MouseButton mouseButton = Qt::NoButton;
	if( quakeKey == K_MOUSE1 ) {
		mouseButton = Qt::LeftButton;
	} else if( quakeKey == K_MOUSE2 ) {
		mouseButton = Qt::RightButton;
	} else if( quakeKey == K_MOUSE3 ) {
		mouseButton = Qt::MiddleButton;
	}

	if( mouseButton != Qt::NoButton ) {
		bool hasAccepted = false;
		const QPointF point( m_mouseXY[0], m_mouseXY[1] );
		const QEvent::Type eventType          = keyDown ? QEvent::MouseButtonPress : QEvent::MouseButtonRelease;
		const Qt::MouseButtons mouseButtons   = getPressedMouseButtons();
		const Qt::KeyboardModifiers modifiers = getPressedKeyboardModifiers();
		for( unsigned i = 0; i < numTargetWindows; ++i ) {
			QMouseEvent event( eventType, point, mouseButton, mouseButtons, modifiers );
			const bool sent = QCoreApplication::sendEvent( targetWindows[i], &event );
			// Allow propagation of events to the cgame view switching logic
			// if the mouse event was not handled by the player bar or the tiled view selector
			if( sent && propagateToCGameIfNotAccepted ) {
				hasAccepted |= event.isAccepted();
			}
		}
		if( propagateToCGameIfNotAccepted ) {
			return hasAccepted;
		}
		return true;
	}

	const std::optional<Qt::Key> maybeQtKey = convertQuakeKeyToQtKey( quakeKey );
	if( !maybeQtKey ) {
		// TODO: What should we do in this case
		return false;
	}

	bool hasAccepted = false;
	const Qt::KeyboardModifiers modifiers = getPressedKeyboardModifiers();
	for( unsigned i = 0; i < numTargetWindows; ++i ) {
		const QEvent::Type type = keyDown ? QEvent::KeyPress : QEvent::KeyRelease;
		QKeyEvent keyEvent( type, *maybeQtKey, modifiers );
		const bool sent = QCoreApplication::sendEvent( targetWindows[i], &keyEvent );
		// Hacks: If we set event.accepted to key events of input controls in Qml, we do not actually get any input.
		// Let us assume that events get accepted if the currenly focused item is some kind of a text input control.
		if( sent && propagateToCGameIfNotAccepted ) {
			if( const QQuickItem *activeFocusItem = targetWindows[i]->activeFocusItem() ) {
				const char *className = activeFocusItem->metaObject()->className();
				for( const char *knownName: { "TextInput", "TextEdit", "TextField", "TextArea" } ) {
					if( ::strstr( className, knownName ) ) {
						hasAccepted = true;
						break;
					}
				}
			}
		}
	}
	if( propagateToCGameIfNotAccepted ) {
		return hasAccepted;
	}
	return true;
}

bool QtUISystem::handleCharEvent( int ch ) {
	if( !isAPrintableChar( ch ) ) {
		return true;
	}

	QQuickWindow *targetWindows[2] { nullptr, nullptr };
	const unsigned numTargetWindows = getTargetWindowsForKeyboardInput( targetWindows );
	if( !numTargetWindows ) {
		return false;
	}

	const Qt::KeyboardModifiers modifiers = getPressedKeyboardModifiers();
	for( unsigned i = 0; i < numTargetWindows; ++i ) {
		// The plain cast of `ch` to Qt::Key seems to be correct in this case
		// (all printable characters seem to map 1-1 to Qt key codes)
		QKeyEvent pressEvent( QEvent::KeyPress, (Qt::Key)ch, modifiers, s_charStrings[ch] );
		QCoreApplication::sendEvent( targetWindows[i], &pressEvent );
		QKeyEvent releaseEvent( QEvent::KeyRelease, (Qt::Key)ch, modifiers );
		QCoreApplication::sendEvent( targetWindows[i], &releaseEvent );
	}
	return true;
}

auto QtUISystem::getPressedMouseButtons() const -> Qt::MouseButtons {
	const auto *const keyHandlingSystem = wsw::cl::KeyHandlingSystem::instance();

	auto result = Qt::MouseButtons();
	if( keyHandlingSystem->isKeyDown( K_MOUSE1 ) ) {
		result |= Qt::LeftButton;
	}
	if( keyHandlingSystem->isKeyDown( K_MOUSE2 ) ) {
		result |= Qt::RightButton;
	}
	if( keyHandlingSystem->isKeyDown( K_MOUSE3 ) ) {
		result |= Qt::MiddleButton;
	}
	return result;
}

auto QtUISystem::getPressedKeyboardModifiers() const -> Qt::KeyboardModifiers {
	const auto *const keyHandlingSystem = wsw::cl::KeyHandlingSystem::instance();

	auto result = Qt::KeyboardModifiers();
	if( keyHandlingSystem->isKeyDown( K_LCTRL ) || keyHandlingSystem->isKeyDown( K_RCTRL ) ) {
		result |= Qt::ControlModifier;
	}
	if( keyHandlingSystem->isKeyDown( K_LALT ) || keyHandlingSystem->isKeyDown( K_RALT ) ) {
		result |= Qt::AltModifier;
	}
	if( keyHandlingSystem->isKeyDown( K_LSHIFT ) || keyHandlingSystem->isKeyDown( K_RSHIFT ) ) {
		result |= Qt::ShiftModifier;
	}
	return result;
}

auto QtUISystem::getTargetWindowsForKeyboardInput( QQuickWindow *targetWindows[2] ) -> unsigned {
	unsigned result     = 0;
	bool addedHudWindow = false;
	if( m_activeMenuMask ) {
		if( m_menuSandbox ) {
			targetWindows[result++] = m_menuSandbox->m_window.get();
		}
	} else {
		if( m_hudSandbox ) {
			if( m_isShowingChatPopup || m_isShowingTeamChatPopup ) {
				targetWindows[result++] = m_hudSandbox->m_window.get();
				addedHudWindow = true;
			}
		}
	}
	if( m_hudSandbox && !addedHudWindow ) {
		if( CG_UsesTiledView() && ( !m_activeMenuMask || ( m_activeMenuMask & DemoPlaybackMenu ) ) ) {
			if( !addedHudWindow ) {
				targetWindows[result++] = m_hudSandbox->m_window.get();
			}
		}
	}
	return result;
}

auto QtUISystem::convertQuakeKeyToQtKey( int quakeKey ) const -> std::optional<Qt::Key> {
	if( quakeKey < 0 ) {
		return std::nullopt;
	}

	static_assert( K_BACKSPACE == 127 );
	if( quakeKey < 127 ) {
		if( quakeKey == K_TAB ) {
			return Qt::Key_Tab;
		}
		if( quakeKey == K_ENTER ) {
			return Qt::Key_Enter;
		}
		if( quakeKey == K_ESCAPE ) {
			return Qt::Key_Escape;
		}
		if( quakeKey == K_SPACE ) {
			return Qt::Key_Space;
		}
		if( std::isprint( quakeKey ) ) {
			return (Qt::Key)quakeKey;
		}
		return std::nullopt;
	}

	if( quakeKey >= K_F1 && quakeKey <= K_F15 ) {
		return (Qt::Key)( Qt::Key_F1 + ( quakeKey - K_F1 ) );
	}

	// Some other seemingly unuseful keys are ignored
	switch( quakeKey ) {
		case K_BACKSPACE: return Qt::Key_Backspace;

		case K_UPARROW: return Qt::Key_Up;
		case K_DOWNARROW: return Qt::Key_Down;
		case K_LEFTARROW: return Qt::Key_Left;
		case K_RIGHTARROW: return Qt::Key_Right;

		case K_LALT:
		case K_RALT:
			return Qt::Key_Alt;

		case K_LCTRL:
		case K_RCTRL:
			return Qt::Key_Control;

		case K_LSHIFT:
		case K_RSHIFT:
			return Qt::Key_Shift;

		case K_INS: return Qt::Key_Insert;
		case K_DEL: return Qt::Key_Delete;
		case K_PGDN: return Qt::Key_PageDown;
		case K_PGUP: return Qt::Key_PageUp;
		case K_HOME: return Qt::Key_Home;
		case K_END: return Qt::Key_End;

		default: return std::nullopt;
	}
}

bool QtUISystem::isDebuggingNativelyDrawnItems() const {
	return v_debugNativelyDrawnItems.get();
}

void QtUISystem::supplyNativelyDrawnItem( QQuickItem *item ) {
	auto *const nativelyDrawn = dynamic_cast<NativelyDrawn *>( item );
	if( !nativelyDrawn ) {
		wsw::failWithLogicError( "An item is not an instance of NativelyDrawn" );
	}
	assert( nativelyDrawn->m_selfAsItem == item );
	assert( !wsw::contains( m_nativelyDrawnItems, nativelyDrawn ) );
	m_nativelyDrawnItems.push_back( nativelyDrawn );
}

void QtUISystem::supplyNativelyDrawnItemsOccluder( QQuickItem *item ) {
	if( item->isVisible() ) {
		m_occludersOfNativelyDrawnItems.push_back( mapLogicalRectToPixels( item->mapRectToScene( item->boundingRect() ) ) );
	}
}

void QtUISystem::supplyHudOccluder( QQuickItem *item ) {
	if( item->isVisible() ) {
		// Keep it in logical units since it belongs to the UI internal machinery
		m_hudOccluders.emplace_back( item->mapRectToScene( item->boundingRect() ) );
	}
}

bool QtUISystem::isHudItemOccluded( QQuickItem *item ) {
	QRectF itemRect( item->mapRectToScene( item->boundingRect() ) );
	itemRect.setWidth( itemRect.width() + 10.0 );
	itemRect.setHeight( itemRect.height() + 10.0 );
	itemRect.moveTopLeft( QPointF( itemRect.x() - 5.0, itemRect.y() - 5.0 ) );
	for( const QRectF &occluderRect : m_hudOccluders ) {
		if( occluderRect.intersects( itemRect ) ) {
			return true;
		}
	}
	return false;
}

void QtUISystem::supplyDisplayedHudItemAndMargin( QQuickItem *item, qreal margin ) {
	assert( margin >= 0.0 );
	if( item->isVisible() ) {
		const QRectF &rectInPixels = mapLogicalRectToPixels( item->mapRectToScene( item->boundingRect() ) );
		m_boundsOfDrawnHudItems.emplace_back( qMakePair( rectInPixels, m_pixelsPerLogicalUnit * margin ) );
	}
}

auto QtUISystem::findCVarOrThrow( const QByteArray &name ) const -> cvar_t * {
	if( cvar_t *maybeVar = Cvar_Find( name.constData() ) ) {
		return maybeVar;
	}
	std::string message;
	message += "Failed to find a var \"";
	message += std::string_view( name.data(), (size_t)name.size() );
	message += "\" by name";
	wsw::failWithLogicError( message.c_str() );
}

QVariant QtUISystem::getCVarValue( const QString &name ) const {
	return QVariant( findCVarOrThrow( name.toLatin1() )->string );
}

void QtUISystem::setCVarValue( const QString &name, const QVariant &value ) {
	const QByteArray nameBytes( name.toLatin1() );
	auto *const cvar = findCVarOrThrow( nameBytes );

	// TODO: What to do with that?
	if( ( cvar->flags & CVAR_LATCH_VIDEO ) || ( cvar->flags & CVAR_LATCH_SOUND ) ) {
		uiWarning() << "Refusing to apply a video/sound-latched var" << name << "value immediately";
		return;
	}

	Cvar_ForceSet( nameBytes.constData(), value.toString().toLatin1().constData() );
}

void QtUISystem::commitPendingCVarChanges() {
	// Ask again for getting actual values
	m_pendingCVarChanges.clear();
	Q_EMIT reportingPendingCVarChangesRequested();

	if( hasPendingCVarChanges() ) {
		assert( !m_pendingCVarChanges.empty() );

		bool shouldRestartVideo = false;
		bool shouldRestartSound = false;

		for( const auto &[name, value] : qAsConst( m_pendingCVarChanges ) ) {
			const QByteArray nameBytes( name.toUtf8() );
			const QByteArray valueBytes( value.toString().toLatin1().constData() );

			const auto flags = Cvar_Flags( nameBytes.constData() );
			if( flags & CVAR_LATCH_VIDEO ) {
				shouldRestartVideo = true;
			}
			if( flags & CVAR_LATCH_SOUND ) {
				shouldRestartSound = true;
			}

			Cvar_ForceSet( nameBytes.constData(), valueBytes.constData() );
		}

		m_pendingCVarChanges.clear();
		assert( !hasPendingCVarChanges() );
		Q_EMIT hasPendingCVarChangesChanged( false );

		if( shouldRestartVideo ) {
			CL_Cbuf_AppendCommand( "vid_restart" );
		}
		if( shouldRestartSound ) {
			CL_Cbuf_AppendCommand( "s_restart" );
		}
	}

	Q_EMIT pendingCVarChangesCommitted();
}

void QtUISystem::rollbackPendingCVarChanges() {
	Q_EMIT rollingPendingCVarChangesBackRequested();
	m_pendingCVarChanges.clear();
	Q_EMIT hasPendingCVarChangesChanged( false );
}

void QtUISystem::reportPendingCVarChanges( const QString &name, const QVariant &value ) {
	assert( value.isValid() );
	m_pendingCVarChanges.emplace_back( qMakePair( name, value ) );
}

void QtUISystem::updateCVarAwareControls() {
	const bool hadPendingChanges = hasPendingCVarChanges();
	assert( hadPendingChanges == !m_pendingCVarChanges.empty() );
	m_pendingCVarChanges.clear();

	// Ask all connected controls
	Q_EMIT reportingPendingCVarChangesRequested();

	if( const bool hasChanges = hasPendingCVarChanges(); hasChanges != hadPendingChanges ) {
		Q_EMIT hasPendingCVarChangesChanged( hasChanges );
	}

	// TODO: Should we reorder these checks?
	Q_EMIT checkingCVarChangesRequested();
}

void QtUISystem::updateHudOccluders() {
	m_oldHudOccluders.clear();
	std::swap( m_oldHudOccluders, m_hudOccluders );
	assert( m_hudOccluders.empty() );

	Q_EMIT hudOccludersRetrievalRequested();

	if( !std::equal( m_oldHudOccluders.begin(), m_oldHudOccluders.end(), m_hudOccluders.begin(), m_hudOccluders.end() ) ) {
		// Force subscribers to update their visibility
		Q_EMIT hudOccludersChanged();
	}
}

auto QtUISystem::mapLogicalRectToPixels( const QRectF &logicalRect ) const -> QRectF {
	const auto x = m_pixelsPerLogicalUnit * logicalRect.x();
	const auto y = m_pixelsPerLogicalUnit * logicalRect.y();
	const auto w = m_pixelsPerLogicalUnit * logicalRect.width();
	const auto h = m_pixelsPerLogicalUnit * logicalRect.height();
	return QRectF( x, y, w, h );
}

void QtUISystem::ensureObjectDestruction( QObject *object ) {
	if( object ) [[likely]] {
		if( object->parent() ) [[unlikely]] {
			uiWarning() << "Attempt to ensure destruction of an object" << object << "with a present parent";
		} else {
			QQmlEngine::setObjectOwnership( object, QQmlEngine::JavaScriptOwnership );
		}
	}
}

void QtUISystem::quit() {
	CL_Cbuf_AppendCommand( "quit\n" );
}

void QtUISystem::disconnect() {
	CL_Cbuf_AppendCommand( "disconnect\n" );
}

void QtUISystem::spectate() {
	assert( m_canSpectate );
	CL_Cbuf_AppendCommand( "spec\n" );
}

void QtUISystem::join() {
	assert( m_canJoin );
	CL_Cbuf_AppendCommand( "join\n" );
}

void QtUISystem::joinAlpha() {
	assert( m_canJoinAlpha );
	CL_Cbuf_AppendCommand( "join alpha\n" );
}

void QtUISystem::joinBeta() {
	assert( m_canJoinBeta );
	CL_Cbuf_AppendCommand( "join beta\n" );
}

void QtUISystem::setReady() {
	assert( m_canBeReady );
	CL_Cbuf_AppendCommand( "ready\n" );
}

void QtUISystem::setNotReady() {
	assert( m_canBeReady );
	CL_Cbuf_AppendCommand( "notready\n" );
}

void QtUISystem::enterChallengersQueue() {
	assert( m_canToggleChallengerStatus );
	// TODO: Is there a separate command
	CL_Cbuf_AppendCommand( "join\n" );
}

void QtUISystem::leaveChallengersQueue() {
	assert( m_canToggleChallengerStatus );
	// TODO: Is there a separate command
	CL_Cbuf_AppendCommand( "spec\n" );
}

void QtUISystem::callVote( const QByteArray &name, const QByteArray &value, bool isOperatorCall ) {
	wsw::StaticString<1024> command;
	if( isOperatorCall ) {
		command << "opcall"_asView;
	} else {
		command << "callvote"_asView;
	}
	command << ' ' << wsw::StringView( name.data(), (unsigned)name.size() );
	command << ' ' << wsw::StringView( value.data(), (unsigned)value.size() );
	command << '\n';
	CL_Cbuf_AppendCommand( command.data() );
}

void QtUISystem::switchToPlayerNum( int playerNum ) {
	CG_SwitchToPlayerNum( (unsigned)playerNum );
}

auto QtUISystem::colorFromRgbString( const QString &string ) const -> QVariant {
	if( int color = COM_ReadColorRGBString( string.toUtf8().constData() ); color != -1 ) {
		return QColor::fromRgb( COLOR_R( color ), COLOR_G( color ), COLOR_B( color ), 255 );
	}
	return QVariant();
}

auto QtUISystem::makeSkewXMatrix( qreal height, qreal degrees ) const -> QMatrix4x4 {
	assert( degrees > -90.0 && degrees < +90.0 );
	const float angle = (float)DEG2RAD( degrees );
	const float sin = std::sin( angle );
	const float cos = std::cos( angle );
	const float tan = sin * Q_Rcp( cos );
	QMatrix4x4 result {
		+1.0, -sin, +0.0, +0.0,
		+0.0, +1.0, +0.0, +0.0,
		+0.0, +0.0, +1.0, +0.0,
		+0.0, +0.0, +0.0, +1.0
	};
	result.translate( tan * (float)height, 0.0f );
	return result;
}

auto QtUISystem::makeTranslateMatrix( qreal translateX, qreal translateY ) const -> QMatrix4x4 {
	QMatrix4x4 result;
	result.setToIdentity();
	result.translate( translateX, translateY );
	return result;
}

auto QtUISystem::formatPing( int ping ) const -> QByteArray {
	return wsw::ui::formatPing( ping );
}

auto QtUISystem::colorWithAlpha( const QColor &color, qreal alpha ) -> QColor {
	assert( alpha >= 0.0 && alpha <= 1.0 );
	return QColor::fromRgbF( color.redF(), color.greenF(), color.blueF(), alpha );
}

void QtUISystem::playHoverSound() {
	playSoundUsingLimiter( "sounds/menu/hover" );
}

void QtUISystem::playSwitchSound() {
	playSoundUsingLimiter( "sounds/menu/switch" );
}

void QtUISystem::playForwardSound() {
	playSoundUsingLimiter( "sounds/menu/forward" );
}

void QtUISystem::playBackSound() {
	playSoundUsingLimiter( "sounds/menu/back" );
}

void QtUISystem::playSoundUsingLimiter( /* &'static */ const char *path ) {
	const auto tag  = (uintptr_t)path;
	if( m_oldSoundPlaybackTag != tag || m_oldSoundPlaybackTimestamp + 50 < cls.realtime ) {
		m_oldSoundPlaybackTag       = tag;
		m_oldSoundPlaybackTimestamp = cls.realtime;
		SoundSystem::instance()->startLocalSound( path, 0.7f );
	}
}

void QtUISystem::startServerListUpdates( int flags ) {
	m_serverListModel.clear();
	const bool showEmptyServers = ( flags & ShowEmptyServers ) != 0;
	const bool showFullServers = ( flags & ShowFullServers ) != 0;
	ServerList::instance()->startPushingUpdates( &m_serverListModel, showEmptyServers, showFullServers );
}

void QtUISystem::stopServerListUpdates() {
	m_serverListModel.clear();
	ServerList::instance()->stopPushingUpdates();
}

void QtUISystem::addToChat( const wsw::cl::ChatMessage &message ) {
	m_chatProxy.addReceivedMessage( message, getFrameTimestamp() );
}

void QtUISystem::addToTeamChat( const wsw::cl::ChatMessage &message ) {
	m_teamChatProxy.addReceivedMessage( message, getFrameTimestamp() );
}

void QtUISystem::handleMessageFault( const MessageFault &messageFault ) {
	m_chatProxy.handleMessageFault( messageFault );
	m_teamChatProxy.handleMessageFault( messageFault );
}

void QtUISystem::handleConfigString( unsigned configStringIndex, const wsw::StringView &string ) {
	if( (unsigned)( configStringIndex - CS_CALLVOTEINFOS ) < (unsigned)MAX_CALLVOTEINFOS ) {
		m_callvotesModel.handleConfigString( configStringIndex, string );
	}
}

void QtUISystem::handleClientInfoChanges( unsigned clientNum ) {
	auto *const nameChangesTracker = NameChangesTracker::instance();
	nameChangesTracker->registerNicknameUpdate( clientNum );
	nameChangesTracker->registerClanUpdate( clientNum );
}

void QtUISystem::updateScoreboard( const ReplicatedScoreboardData &scoreboardData, const AccuracyRows &accuracyRows ) {
	m_scoreboardModel.update( scoreboardData, accuracyRows );
	m_playersModel.update( scoreboardData );
	m_hudCommonDataModel.updateScoreboardData( scoreboardData );
	m_hudPovDataModel.updateScoreboardData( scoreboardData );
}

bool QtUISystem::isShowingScoreboard() const {
	return m_isShowingScoreboard;
}

void QtUISystem::setScoreboardShown( bool shown ) {
	m_isRequestedToShowScoreboard = shown;
}

bool QtUISystem::suggestsUsingVSync() const {
	return ( m_activeMenuMask & ( MainMenu | InGameMenu | ConnectionScreen | IntroScreen | UpdateScreen ) ) != 0;
}

void QtUISystem::toggleChatPopup() {
	m_isRequestedToShowChatPopup = !m_isRequestedToShowChatPopup;
}

void QtUISystem::toggleTeamChatPopup() {
	if( m_hasTeamChat ) {
		m_isRequestedToShowTeamChatPopup = !m_isRequestedToShowTeamChatPopup;
	} else {
		m_isRequestedToShowTeamChatPopup = false;
		m_isRequestedToShowChatPopup     = !m_isRequestedToShowChatPopup;
	}
}

void QtUISystem::touchActionRequest( const wsw::StringView &tag, unsigned int timeout,
									 const wsw::StringView &title, const wsw::StringView &actionDesc,
									 const std::pair<wsw::StringView, int> *actionsBegin,
									 const std::pair<wsw::StringView, int> *actionsEnd ) {
	m_actionRequestsModel.touch( tag, timeout, title, actionDesc, actionsBegin, actionsEnd );
}

void QtUISystem::handleOptionsStatusCommand( const wsw::StringView &status ) {
	m_gametypeOptionsModel.handleOptionsStatusCommand( status );
}

void QtUISystem::reloadOptions() {
	// TODO: Make these calls non-destructive (don't reset models unless its really needeed)
	m_callvotesModel.reload();
	m_scoreboardModel.reload();
	m_gametypeOptionsModel.reload();
}

void QtUISystem::resetHudFeed() {
	m_hudCommonDataModel.resetHudFeed();
	m_hudPovDataModel.resetHudFeed();
}

void QtUISystem::addFragEvent( const std::pair<wsw::StringView, int> &victimAndTeam,
							   unsigned meansOfDeath,
							   const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam ) {
	m_hudCommonDataModel.addFragEvent( victimAndTeam, getFrameTimestamp(), meansOfDeath, attackerAndTeam );
}

void QtUISystem::addToMessageFeed( unsigned playerNum, const wsw::StringView &message ) {
	// TODO: Decouple pov-specific messages and messages to everybody
	if( m_hudPovDataModel.getPlayerNum() == std::optional( playerNum ) ) {
		m_hudPovDataModel.addToMessageFeed( message, getFrameTimestamp() );
	} else {
		m_hudCommonDataModel.addToMessageFeed( playerNum, message, getFrameTimestamp() );
	}
}

void QtUISystem::addAward( unsigned playerNum, const wsw::StringView &award ) {
	if( m_hudPovDataModel.getPlayerNum() == std::optional( playerNum ) ) {
		m_hudPovDataModel.addAward( award, getFrameTimestamp() );
	} else {
		m_hudCommonDataModel.addAward( playerNum, award, getFrameTimestamp() );
	}
}

void QtUISystem::addStatusMessage( unsigned playerNum, const wsw::StringView &message ) {
	if( m_hudPovDataModel.getPlayerNum() == std::optional( playerNum ) ) {
		m_hudPovDataModel.addStatusMessage( message, getFrameTimestamp() );
	} else {
		m_hudCommonDataModel.addStatusMessage( playerNum, message, getFrameTimestamp() );
	}
}

void QtUISystem::addToFrametimeTimeline( int64_t timestamp, float frametime ) {
	if( v_showFps.get() ) {
		WSW_PROFILER_SCOPE();
		m_hudCommonDataModel.addToFrametimeTimeline( timestamp, frametime );
	}
}

void QtUISystem::addToPingTimeline( int64_t timestamp, float ping ) {
	if( v_showNet.get() ) {
		WSW_PROFILER_SCOPE();
		m_hudCommonDataModel.addToPingTimeline( timestamp, ping );
	}
};

void QtUISystem::addToPacketlossTimeline( int64_t timestamp, bool hadPacketloss ) {
	if( v_showNet.get() ) {
		WSW_PROFILER_SCOPE();
		m_hudCommonDataModel.addToPacketlossTimeline( timestamp, hadPacketloss );
	}
}

static const QString kConnectionFailedTitle( "Connection failed" );
static const QString kConnectionErrorTitle( "Connection error" );
static const QString kConnectionTerminatedTitle( "Connection terminated" );

void QtUISystem::notifyOfDroppedConnection( const wsw::StringView &message, ReconnectBehaviour reconnectBehaviour, ConnectionDropStage dropStage ) {
	switch( dropStage ) {
		case ConnectionDropStage::EstablishingFailed: m_pendingDroppedConnectionTitle = kConnectionFailedTitle; break;
		case ConnectionDropStage::FunctioningError: m_pendingDroppedConnectionTitle = kConnectionErrorTitle; break;
		case ConnectionDropStage::TerminatedByServer: m_pendingDroppedConnectionTitle = kConnectionTerminatedTitle; break;
	}
	m_pendingDroppedConnectionMessage.clear();
	m_pendingDroppedConnectionMessage.append( QLatin1String( message.data(), (int)message.size() ) );
	if( reconnectBehaviour == ReconnectBehaviour::Autoreconnect ) {
		wsw::failWithLogicError( "The UI should not be notified of dropped connections with Autoreconnect behaviour" );
	}
	static_assert( (unsigned)ReconnectBehaviour_::DontReconnect == (unsigned)ReconnectBehaviour::DontReconnect );
	static_assert( (unsigned)ReconnectBehaviour_::OfUserChoice == (unsigned)ReconnectBehaviour::OfUserChoice );
	static_assert( (unsigned)ReconnectBehaviour_::RequestPassword == (unsigned)ReconnectBehaviour_::RequestPassword );
	m_pendingReconnectBehaviour = (ReconnectBehaviour_)reconnectBehaviour;
}

void QtUISystem::notifyOfUpdateAvailable( const wsw::StringView &version ) {
	const QLatin1String qtCompatVersion( version.data(), (int)version.size() );
	if( m_availableUpdateVersion.compare( qtCompatVersion ) != 0 ) {
		m_availableUpdateVersion = qtCompatVersion;
		Q_EMIT availableUpdateVersionChanged( m_availableUpdateVersion );
	}
}

void QtUISystem::notifyOfUpdateNotFound() {
	m_pendingHideUpdates = true;
}

template <typename Value>
void QtUISystem::appendSetCVarCommand( const wsw::StringView &name, const Value &value ) {
	wsw::StaticString<256> command;
	assert( !name.contains( ' ' ) && !name.contains( '\t' ) );
	command << "set "_asView << name << " \""_asView << value << "\";"_asView;
	CL_Cbuf_AppendCommand( command.data() );
}

void QtUISystem::connectToAddress( const QByteArray &address ) {
	wsw::StaticString<256> command;
	command << "connect "_asView << wsw::StringView( address.data(), (unsigned)address.size() );
	CL_Cbuf_AppendCommand( command.data() );
}

void QtUISystem::reconnectWithPassword( const QByteArray &password ) {
	wsw::StaticString<256> command;
	appendSetCVarCommand( "password"_asView, wsw::StringView( password.data(), password.size() ) );
	CL_Cbuf_AppendCommand( "reconnect" );
	m_pendingReconnectBehaviour = std::nullopt;
}

void QtUISystem::reconnect() {
	// TODO: Check whether we actually can reconnect
	CL_Cbuf_AppendCommand( "reconnect" );
	// Protect from sticking in this state
	m_pendingReconnectBehaviour = std::nullopt;
}

void QtUISystem::launchLocalServer( const QByteArray &gametype, const QByteArray &map, int flags, int numBots, int skillLevel ) {
	appendSetCVarCommand( "g_gametype"_asView, gametype );
	appendSetCVarCommand( "g_instagib"_asView, ( flags & LocalServerInsta ) ? 1 : 0 );
	appendSetCVarCommand( "g_numbots"_asView, numBots );
	appendSetCVarCommand( "sv_public"_asView, ( flags & LocalServerPublic ) ? 1 : 0 );
	appendSetCVarCommand( "sv_skillLevel"_asView, skillLevel );

	wsw::StaticString<256> command;
	command << "map "_asView << map;
	CL_Cbuf_AppendCommand( command.data() );
}

bool QtUISystem::isShowingModalMenu() const {
	if( m_menuSandbox ) {
		return ( m_activeMenuMask & ( InGameMenu | MainMenu | IntroScreen | UpdateScreen ) ) != 0;
	}
	return false;
}

auto QtUISystem::retrieveNumberOfHudMiniviewPanes() -> unsigned {
	m_hasMiniviewPane1 = false;
	m_hasMiniviewPane2 = false;

	Q_EMIT hudMiniviewPanesRetrievalRequested();

	return ( m_hasMiniviewPane1 ? 1 : 0 ) + ( m_hasMiniviewPane2 ? 1 : 0 );
}

auto QtUISystem::retrieveHudControlledMiniviews( Rect *positions, unsigned *viewStateIndices ) -> unsigned {
	m_miniviewItemPositions    = positions;
	m_miniviewViewStateIndices = viewStateIndices;
	m_numRetrievedMiniviews    = 0;

	Q_EMIT hudControlledMiniviewItemsRetrievalRequested();

	return m_numRetrievedMiniviews;
}

void QtUISystem::supplyHudMiniviewPane( int number ) {
	assert( number == 1 || number == 2 );
	if( number == 1 ) {
		m_hasMiniviewPane1 = true;
	} else if( number == 2 ) {
		m_hasMiniviewPane2 = true;
	}
}

void QtUISystem::supplyHudControlledMiniviewItemAndModelIndex( QQuickItem *item, int modelIndex ) {
	const QRectF &fRect         = item->boundingRect();
	const QPointF &fRealTopLeft = item->mapToGlobal( fRect.topLeft() );

	const QRect  iRect         = fRect.toRect();
	const QPoint iRealTopLeft = fRealTopLeft.toPoint();

	m_miniviewItemPositions[m_numRetrievedMiniviews] = Rect {
		.x      = m_pixelsPerLogicalUnit * iRealTopLeft.x(),
		.y      = m_pixelsPerLogicalUnit * iRealTopLeft.y(),
		.width  = m_pixelsPerLogicalUnit * iRect.width(),
		.height = m_pixelsPerLogicalUnit * iRect.height(),
	};

	const unsigned viewStateIndex = m_hudCommonDataModel.getViewStateIndexForMiniviewModelIndex( modelIndex );

	m_miniviewViewStateIndices[m_numRetrievedMiniviews] = viewStateIndex;
	m_numRetrievedMiniviews++;
}

}

#include "uisystem.moc"