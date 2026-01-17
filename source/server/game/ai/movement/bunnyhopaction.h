#ifndef WSW_16616c7d_398c_4acf_af00_726bd6ec9608_H
#define WSW_16616c7d_398c_4acf_af00_726bd6ec9608_H

#include "baseaction.h"

class BunnyHopAction : public BaseAction {
	friend class PredictionContext;
public:
	BunnyHopAction( MovementSubsystem *subsystem, const char *name, int debugColor )
		: BaseAction( subsystem, name, debugColor ) {
		// Do NOT stop prediction on this! We have to check where the bot is going to land!
		BaseAction::m_stopPredictionOnTouchingNavEntity = false;
	}

	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;
	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason reason,
									   unsigned stoppedAtFrameIndex ) override;
	void beforePlanning() override;

protected:
	int m_travelTimeAtSequenceStart { 0 };
	int m_reachAtSequenceStart { 0 };

	Vec3 m_latchedHopOrigin { 0, 0, 0 };

	// Best results so far achieved in the action application sequence
	int m_minTravelTimeToNavTargetSoFar { 0 };
	int m_minTravelTimeAreaNumSoFar { 0 };

	float m_distanceToReachAtStart { std::numeric_limits<float>::infinity() };
	float m_distanceInNavTargetAreaAtStart { std::numeric_limits<float>::infinity() };

	// A fraction of speed gain per frame time.
	// Might be negative, in this case it limits allowed speed loss
	float m_minDesiredSpeedGainPerSecond { 0.0f };
	unsigned m_currentSpeedLossSequentialMillis { 0 };
	unsigned m_tolerableSpeedLossSequentialMillis { 300 };

	// When bot bunnies over a gap, its target either becomes unreachable
	// or travel time is calculated from the bottom of the pit.
	// These timers allow to temporarily skip targer reachability/travel time tests.
	unsigned m_currentUnreachableTargetSequentialMillis { 0 };
	unsigned m_tolerableUnreachableTargetSequentialMillis { 700 };

	// Allow increased final travel time if the min travel time area is reachable by walking
	// from the final area and walking travel time is lower than this limit.
	// It allows to follow the reachability chain less strictly while still being close to it.
	unsigned m_tolerableWalkableIncreasedTravelTimeMillis { 3000 };

	// There is a mechanism for completely disabling an action for further planning by setting isDisabledForPlanning flag.
	// However we need a more flexible way of disabling an action after an failed application sequence.
	// A sequence started from different frame that the failed one might succeed.
	// An application sequence will not start at the frame indexed by this value.
	unsigned m_disabledForApplicationFrameIndex { std::numeric_limits<unsigned>::max() };

	// This should be set if we want to continue prediction
	// but give a path built by a current sequence an additional penalty
	// accounted by PredictionContext::CompleteOrSaveGoodEnoughPath()
	unsigned m_sequencePathPenalty { 0 };

	bool m_hasEnteredNavTargetArea { false };
	bool m_hasTouchedNavTarget { false };

	bool m_hasALatchedHop { false };
	bool m_didTheLatchedHop { false };

	bool m_hasCheckedForInputInversion { false };
	bool m_shouldUseInputInversion { false };

	unsigned m_hopCounter { 0 };

	// TODO: Mark as virtual in base class and mark as final here to avoid a warning about hiding parent member?
	[[nodiscard]]
	auto genericCheckIsActionEnabled( PredictionContext *context ) -> PredictionResult;
	[[nodiscard]]
	auto checkCommonBunnyHopPreconditions( PredictionContext *context ) -> PredictionResult;
	[[nodiscard]]
	bool setupBunnyHopping( const Vec3 &intendedLookDir, PredictionContext *context );
	[[nodiscard]]
	bool canFlyAboveGroundRelaxed( const PredictionContext *context ) const;
	[[nodiscard]]
	bool checkRiskyMovementAllowed( PredictionContext *context ) const;
	[[nodiscard]]
	bool canSetWalljump( PredictionContext *context, const Vec3 &velocity2DDir, const Vec3 &intended2DLookDir ) const;

	// Can be overridden for finer control over tests
	[[nodiscard]]
	virtual bool checkStepSpeedGainOrLoss( PredictionContext *context );

	[[nodiscard]]
	bool checkNavTargetAreaTransition( PredictionContext *context );

	[[nodiscard]]
	bool tryHandlingUnreachableTarget( PredictionContext *context );

	[[nodiscard]]
	bool tryHandlingWorseTravelTimeToTarget( PredictionContext *context,
		                                     int currTravelTimeToTarget,
		                                     int groundedAreaNum );

	[[nodiscard]]
	bool wasOnGroundThisFrame( const PredictionContext *context ) const;

	void ensurePathPenalty( unsigned penalty ) {
		assert( penalty < 30000 );
		m_sequencePathPenalty = wsw::max( m_sequencePathPenalty, penalty );
	}

	[[nodiscard]]
	bool checkDirectReachWalkingOrFallingShort( int fromAreaNum, int toAreaNum );

	[[nodiscard]]
	bool hasMadeAnAdvancementPriorToLanding( PredictionContext *context, int currTravelTimeToTarget );
};

#endif
