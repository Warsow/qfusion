#ifndef WSW_804a8cb0_3bf5_49a3_978f_70dc00ba524a_H
#define WSW_804a8cb0_3bf5_49a3_978f_70dc00ba524a_H

// It would be better if we avoid inclusion of the implementation headers
// but we do not want to lose some performance on indirect access.
// Let offsets of all members be known statically.

#include "predictioncontext.h"

#include "bunnytostairsorrampexitaction.h"
#include "bunnyfollowingreachchainaction.h"
#include "bunnytestingnextreachdirsaction.h"
#include "bunnytestingmultipleturnsaction.h"
#include "bunnytobestvisiblereachaction.h"
#include "bunnytobestclusterpointaction.h"

#include "fallbackscriptsandactions.h"

class Bot;

// Roughly based on token buckets algorithm
class alignas( 4 )RateLimiter {
	Int64Align4 refilledAt;
	float refillRatePerMillis;
	unsigned intervalMillis;
	const int size;
	int value;

	int GetNewValue( int64_t millisNow ) const {
		int64_t diff = millisNow - refilledAt;
		auto tokensToAdd = (int)(diff * refillRatePerMillis);
		if( tokensToAdd <= 0 ) {
			return value;
		}

		int newValue = value;
		if( value <= 0 ) {
			newValue = tokensToAdd;
			if( newValue > size ) {
				newValue = size;
			}
		} else {
			newValue += tokensToAdd;
			if( newValue > size ) {
				newValue = 0;
			}
		}
		return newValue;
	}

	void Refill( int64_t millisNow ) {
		int newValue = GetNewValue( millisNow );
		if( value != newValue ) {
			value = newValue;
			refilledAt = millisNow - ( millisNow - refilledAt ) % intervalMillis;
		}
	}
public:
	explicit RateLimiter( int actionsPerSecond )
		: refilledAt( 0 )
		, refillRatePerMillis( actionsPerSecond / 1000.0f )
		, intervalMillis( (unsigned)( 1000.0f / actionsPerSecond ) )
		, size( actionsPerSecond )
		, value( 1 ) {
		// Note: initializing the value by 1 is important.
		// Otherwise the first TryAcquire() attempt fails.
		// This algorithm converges to theoretical values well according to tests.
	}

	bool TryAcquire( int64_t levelTime ) {
		Refill( levelTime );
		value -= 1;
		return value >= 0;
	}
};

class MovementSubsystem {
	friend class Bot;
	friend struct MovementState;
	friend class PredictionContext;
	friend class BaseAction;
	friend class MovementScript;
	friend class WalkToPointAction;
	friend class WalkToPointScript;
	friend class ElevatorScript;
	friend class JumppadScript;
	friend class LandToPreventFallingScript;
	friend class BunnyToBestFloorClusterPointAction;
	friend class PredictingAndCachingMovementScript;
public:
	explicit MovementSubsystem( Bot *bot_ );

	[[nodiscard]]
	auto getMovementState() -> const MovementState & { return m_movementState; }

	void onInterceptedPredictedEvent( int ev, int parm ) {
		assert( m_testedScript );
		static_cast<PredictingMovementScript *>( m_testedScript )->onInterceptedPredictedEvent( ev, parm );
	}

	void onInterceptedPMoveTouchTriggers( pmove_t *pm, const vec3_t previousOrigin ) {
		assert( m_testedScript );
		static_cast<PredictingMovementScript *>( m_testedScript )->onInterceptedPMoveTouchTriggers( pm, previousOrigin );
	}

	void setCampingSpot( const AiCampingSpot &campingSpot ) {
		//movementState.campingSpotState.Activate( campingSpot );
	}

	void resetCampingSpot() {
		//movementState.campingSpotState.Deactivate();
	}

	[[nodiscard]]
	bool hasActiveCampingSpot() const {
		return false;
		//return movementState.campingSpotState.IsActive();
	}

	void setPendingLookAtPoint( const AiPendingLookAtPoint &lookAtPoint, unsigned timeoutPeriod ) {
		m_pendingLookAtPointState.pendingLookAtPoint = lookAtPoint;
		m_pendingLookAtPointState.timeoutAt          = level.time + timeoutPeriod;
	}

	void resetPendingLookAtPoint() {
		m_pendingLookAtPointState.timeoutAt = 0;
	}

	[[nodiscard]]
	bool hasPendingLookAtPoint() const {
		return m_pendingLookAtPointState.timeoutAt > level.time;
	}

	void activateJumppadState( const edict_t *jumppadEnt );
	void activateElevatorState( const edict_t *triggerEnt );

	bool canChangeWeapons() const;

	void reset() {
		resetPendingLookAtPoint();
		m_activeScript                   = nullptr;
		m_prevActiveScript               = nullptr;
		m_lastNearbyElevatorReach.entNum = 0;
		m_lastNearbyJumppadReach.entNum  = 0;
	}

	[[nodiscard]]
	bool canInterruptMovement() const;

	void frame( BotInput *input );
	void applyInput( BotInput *input, PredictionContext *context = nullptr );

private:
	struct CachedLastNearbyTriggerReach;

	void applyPendingTurnToLookAtPoint( BotInput *input, PredictionContext *context = nullptr );

	void setActiveScript( MovementScript *script );

	[[nodiscard]]
	auto findFallbackScript( BotInput *input ) -> MovementScript *;

	[[nodiscard]]
	auto findLastResortGroundScript( BotInput *input ) -> MovementScript *;

	[[nodiscard]]
	bool produceBotInput( MovementScript *script, BotInput *input );

	[[nodiscard]]
	auto findNextReachNumForTravelType( int desiredTravelType, int hopLimit ) -> int;

	[[nodiscard]]
	auto findTriggerReachNumForScriptActivation( int triggerEntNum, int desiredTravelType,
												 const CachedLastNearbyTriggerReach &cached ) -> int;

	Bot *const m_bot;

	// Limits weapon jumps attempts per second
	// (consequential attempts are allowed but no more than several frames,
	// otherwise a bot might loop attempts forever)
	RateLimiter m_weaponJumpAttemptsRateLimiter;
	// Is not for rate limiting but for preventing instant weapon switch for shooting after a failed attempt
	Int64Align4 m_lastWeaponJumpTriggeringFailedAt { 0 };

	class BunnyHopScript : public PredictingAndCachingMovementScript {
	public:
		explicit BunnyHopScript( MovementSubsystem *movementSubsystem )
			: PredictingAndCachingMovementScript( movementSubsystem )
			, m_bunnyToStairsOrRampExitAction( movementSubsystem )
			, m_bunnyToBestFloorClusterPointAction( movementSubsystem )
			, m_bunnyFollowingReachChainAction( movementSubsystem )
			, m_bunnyTestingNextReachDirsAction( movementSubsystem )
			, m_bunnyToBestVisibleReachAction( movementSubsystem )
			, m_bunnyTestingMultipleTurnsAction( movementSubsystem ) {
			m_timeoutAt              = std::numeric_limits<int64_t>::max();
			m_storageOfActionPtrs[0] = &m_bunnyToStairsOrRampExitAction;
			m_storageOfActionPtrs[1] = &m_bunnyToBestFloorClusterPointAction;
			m_storageOfActionPtrs[2] = &m_bunnyFollowingReachChainAction;
			m_storageOfActionPtrs[3] = &m_bunnyTestingNextReachDirsAction;
			m_storageOfActionPtrs[4] = &m_bunnyToBestVisibleReachAction;
			m_storageOfActionPtrs[5] = &m_bunnyTestingMultipleTurnsAction;
			m_movementActions        = m_storageOfActionPtrs;
		}
	private:
		BunnyToStairsOrRampExitAction m_bunnyToStairsOrRampExitAction;
		BunnyToBestFloorClusterPointAction m_bunnyToBestFloorClusterPointAction;
		BunnyFollowingReachChainAction m_bunnyFollowingReachChainAction;
		BunnyTestingNextReachDirsAction m_bunnyTestingNextReachDirsAction;
		BunnyToBestVisibleReachAction m_bunnyToBestVisibleReachAction;
		BunnyTestingMultipleTurnsAction m_bunnyTestingMultipleTurnsAction;
		BaseAction *m_storageOfActionPtrs[6] {};
	};

	MovementState m_movementState;

	SameFloorClusterAreasCache m_sameFloorClusterAreasCache;
	NextFloorClusterAreasCache m_nextFloorClusterAreasCache;

	struct PendingLookAtPointState {
		AiPendingLookAtPoint pendingLookAtPoint;
		int64_t timeoutAt { 0 };
	} m_pendingLookAtPointState;

	MovementScript *m_activeScript { nullptr };
	MovementScript *m_testedScript { nullptr };
	MovementScript *m_prevActiveScript { nullptr };

	int64_t m_noScriptOnGroundSinceLevelTime { 0 };
	int64_t m_noScriptOnGroundSinceLevelFramenum { 0 };

	JumppadScript m_jumppadScript { this };
	ElevatorScript m_elevatorScript { this };
	BunnyHopScript m_bunnyHopScript { this };
	WalkToPointScript m_walkToPointScript { this };
	JumpToPointScript m_jumpToPointScript { this };
	LandToPreventFallingScript m_landToPreventFallingScript { this };
	WaitForLandingRelaxedScript m_waitForLandingRelaxedScript { this };
	TraverseJumpReachScript m_traverseJumpReachScript { this };
	TraverseBarrierJumpReachScript m_traverseBarrierJumpReachScript { this };
	TraverseWalkOffLedgeReachScript m_traverseWalkOffLedgeReachScript { this };
	SingleFrameSideStepScript m_singleFrameSideStepScript { this };

	struct CachedLastNearbyTriggerReach {
		int64_t touchedAt { 0 };
		int reachNum { 0 };
		int entNum { 0 };
	};

	CachedLastNearbyTriggerReach m_lastNearbyJumppadReach;
	CachedLastNearbyTriggerReach m_lastNearbyElevatorReach;
};

#endif
