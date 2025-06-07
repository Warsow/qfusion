#ifndef WSW_0646db93_a5f6_4b71_9267_800aa42abe4b_H
#define WSW_0646db93_a5f6_4b71_9267_800aa42abe4b_H

#include <cstdint>
#include <optional>
#include <span>

#include <common/helpers/q_math.h>
#include <common/facilities/q_shared.h>
#include <common/facilities/q_comref.h>
#include "cgameimports.h"

struct MessageFault;
struct ReplicatedScoreboardData;

struct AccuracyRows {
	using Span = std::span<const uint8_t, 10>;
	const Span weak;
	const Span strong;
};

namespace wsw { class StringView; }

namespace wsw::cl { struct ChatMessage; }

namespace wsw::ui {

class UISystem {
public:
	virtual ~UISystem() = default;

	static void init( int widthInPixels, int heightInPixels, int logicalUnitsToPixelsScale );
	static void shutdown();

	[[nodiscard]]
	static auto instance() -> UISystem *;

	virtual void refreshProperties() = 0;
	virtual void renderInternally() = 0;

	virtual void drawBackgroundMapIfNeeded() = 0;
	virtual void drawMenuPartInMainContext() = 0;
	virtual void drawHudPartInMainContext() = 0;
	virtual void drawCursorInMainContext() = 0;

	virtual void runGCIfNeeded() = 0;

	// These "safepoints" have little to no relation to JVM safepoints, but we like the name.
	// Consider game states when it is relatively safe to trigger GC without irritating users "safepoints".
	enum class GCSafepointKind { Respawn, Teleport };
	virtual void handleGCSafepoint( GCSafepointKind kind ) = 0;

	virtual void beginRegistration() = 0;
	virtual void endRegistration() = 0;

	[[nodiscard]]
	virtual bool grabsKeyboardAndMouseButtons() const = 0;
	[[nodiscard]]
	virtual bool grabsMouseMovement() const = 0;
	[[nodiscard]]
	virtual bool handleKeyEvent( int quakeKey, bool keyDown ) = 0;
	[[nodiscard]]
	virtual bool handleCharEvent( int ch ) = 0;
	[[nodiscard]]
	virtual bool handleMouseMovement( float frameTimeMillis, int dx, int dy ) = 0;

	virtual void handleEscapeKey() = 0;

	virtual void addToChat( const wsw::cl::ChatMessage &message ) = 0;
	virtual void addToTeamChat( const wsw::cl::ChatMessage &message ) = 0;

	virtual void handleMessageFault( const MessageFault &messageFault ) = 0;

	virtual void handleConfigString( unsigned configStringNum, const wsw::StringView &string ) = 0;

	virtual void handleClientInfoChanges( unsigned clientNum ) = 0;

	virtual void updateScoreboard( const ReplicatedScoreboardData &scoreboardData, const AccuracyRows &accuracyRows ) = 0;

	virtual void setScoreboardShown( bool shown ) = 0;
	[[nodiscard]]
	virtual bool isShowingScoreboard() const = 0;

	[[nodiscard]]
	virtual bool isShowingModalMenu() const = 0;

	[[nodiscard]]
	virtual bool suggestsUsingVSync() const = 0;

	virtual void toggleChatPopup() = 0;
	virtual void toggleTeamChatPopup() = 0;

	virtual void playForwardSound() = 0;

	// This is a workaround for the current lack of ranges support
	template <typename ActionsRange>
	void touchActionRequest( const wsw::StringView &tag, unsigned timeout,
						     const wsw::StringView &title, const wsw::StringView &desc,
						     const ActionsRange &actions ) {
		touchActionRequest( tag, timeout, title, desc, std::begin( actions ), std::end( actions ) );
	}

	virtual void touchActionRequest( const wsw::StringView &tag, unsigned timeout,
								     const wsw::StringView &title, const wsw::StringView &desc,
								  	 const std::pair<wsw::StringView, int> *actionsBegin,
								  	 const std::pair<wsw::StringView, int> *actionsEnd ) = 0;

	virtual void handleOptionsStatusCommand( const wsw::StringView &status ) = 0;

	virtual void reloadOptions() = 0;

	virtual void resetHudFeed() = 0;
	virtual void addFragEvent( const std::pair<wsw::StringView, int> &victimAndTeam,
							   unsigned meansOfDeath,
							   const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam ) = 0;

	virtual void addToMessageFeed( unsigned playerNum, const wsw::StringView &message ) = 0;

	virtual void addAward( unsigned playerNum, const wsw::StringView &award ) = 0;

	virtual void addStatusMessage( unsigned playerNum, const wsw::StringView &message ) = 0;

	// Accepts a monotonic timestamp purely for presentation purposes.
	// Currently it regulates whether old peak values are kept on screen upon these submissions.
	virtual void addToFrametimeTimeline( int64_t timestamp, float frametime ) = 0;
	virtual void addToPingTimeline( int64_t timestamp, float ping ) = 0;
	virtual void addToPacketlossTimeline( int64_t timestamp, bool hadPacketloss ) = 0;

	virtual void notifyOfDroppedConnection( const wsw::StringView &message, ReconnectBehaviour reconnectBehaviour, ConnectionDropStage dropStage ) = 0;

	virtual void notifyOfUpdateAvailable( const wsw::StringView &version ) = 0;
	virtual void notifyOfUpdateNotFound() = 0;

	virtual void dispatchShuttingDown() = 0;

	[[nodiscard]]
	virtual auto retrieveNumberOfHudMiniviewPanes() -> unsigned = 0;
	[[nodiscard]]
	virtual auto retrieveLimitOfMiniviews() -> unsigned = 0;
	[[nodiscard]]
	virtual auto retrieveHudControlledMiniviews( Rect positions[MAX_CLIENTS], unsigned viewStateNums[MAX_CLIENTS] ) -> unsigned = 0;
};

}

#endif
