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
	friend class BunnyToBestFloorClusterPointAction;
	friend class PredictingAndCachingMovementScript;

	Bot *const bot;

	static constexpr unsigned MAX_SAVED_AREAS = PredictionContext::MAX_SAVED_LANDING_AREAS;
	wsw::StaticVector<int, MAX_SAVED_AREAS> savedLandingAreas;

	// Limits weapon jumps attempts per second
	// (consequential attempts are allowed but no more than several frames,
	// otherwise a bot might loop attempts forever)
	RateLimiter weaponJumpAttemptsRateLimiter;
	// Is not for rate limiting but for preventing instant weapon switch for shooting after a failed attempt
	Int64Align4 lastWeaponJumpTriggeringFailedAt { 0 };

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

	MovementState movementState;

	SameFloorClusterAreasCache sameFloorClusterAreasCache;
	NextFloorClusterAreasCache nextFloorClusterAreasCache;

	void ApplyPendingTurnToLookAtPoint( BotInput *input, PredictionContext *context = nullptr );

	struct PendingLookAtPointState {
		AiPendingLookAtPoint pendingLookAtPoint;
		int64_t timeoutAt { 0 };
	} pendingLookAtPointState;

	MovementScript *activeScript { nullptr };
	MovementScript *testedScript { nullptr };

	BunnyHopScript bunnyHopScript { this };
	WalkToPointScript walkToPointScript { this };
	TraverseJumpReachScript traverseJumpReachScript { this };
	TraverseBarrierJumpReachScript traverseBarrierJumpReachScript { this };
	TraverseWalkOffLedgeReachScript traverseWalkOffLedgeReachScript { this };

	[[nodiscard]]
	auto findFallbackScript( BotInput *input ) -> MovementScript *;

	[[nodiscard]]
	bool produceBotInput( MovementScript *script, BotInput *input );
public:
	explicit MovementSubsystem( Bot *bot_ );

	auto getMovementState() -> const MovementState & { return movementState; }

	void OnInterceptedPredictedEvent( int ev, int parm ) {
		assert( testedScript );
		static_cast<PredictingMovementScript *>( testedScript )->onInterceptedPredictedEvent( ev, parm );
	}

	void OnInterceptedPMoveTouchTriggers( pmove_t *pm, const vec3_t previousOrigin ) {
		assert( testedScript );
		static_cast<PredictingMovementScript *>( testedScript )->onInterceptedPMoveTouchTriggers( pm, previousOrigin );
	}

	void SetCampingSpot( const AiCampingSpot &campingSpot ) {
		//movementState.campingSpotState.Activate( campingSpot );
	}

	void ResetCampingSpot() {
		//movementState.campingSpotState.Deactivate();
	}

	bool HasActiveCampingSpot() const {
		return false;
		//return movementState.campingSpotState.IsActive();
	}

	void SetPendingLookAtPoint( const AiPendingLookAtPoint &lookAtPoint, unsigned timeoutPeriod ) {
		pendingLookAtPointState.pendingLookAtPoint = lookAtPoint;
		pendingLookAtPointState.timeoutAt          = level.time + timeoutPeriod;
	}

	void ResetPendingLookAtPoint() {
		pendingLookAtPointState.timeoutAt = 0;
	}

	bool HasPendingLookAtPoint() const {
		return pendingLookAtPointState.timeoutAt > level.time;
	}

	void ActivateJumppadState( const edict_t *jumppadEnt ) {
		//movementState.jumppadMovementState.Activate( jumppadEnt );
	}

	bool CanChangeWeapons() const;

	void Reset() {
		ResetPendingLookAtPoint();
	}

	bool CanInterruptMovement() const;

	void Frame( BotInput *input );
	void ApplyInput( BotInput *input, PredictionContext *context = nullptr );
};

#endif
