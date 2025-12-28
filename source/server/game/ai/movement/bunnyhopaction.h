#ifndef WSW_16616c7d_398c_4acf_af00_726bd6ec9608_H
#define WSW_16616c7d_398c_4acf_af00_726bd6ec9608_H

#include "baseaction.h"

class BunnyHopAction : public BaseAction {
	friend class PredictionContext;
protected:
	int travelTimeAtSequenceStart { 0 };
	int reachAtSequenceStart { 0 };

	Vec3 latchedHopOrigin { 0, 0, 0 };

	// Best results so far achieved in the action application sequence
	int minTravelTimeToNavTargetSoFar { 0 };
	int minTravelTimeAreaNumSoFar { 0 };

	float distanceToReachAtStart { std::numeric_limits<float>::infinity() };
	float distanceInNavTargetAreaAtStart { std::numeric_limits<float>::infinity() };

	// A fraction of speed gain per frame time.
	// Might be negative, in this case it limits allowed speed loss
	float minDesiredSpeedGainPerSecond { 0.0f };
	unsigned currentSpeedLossSequentialMillis { 0 };
	unsigned tolerableSpeedLossSequentialMillis { 300 };

	// When bot bunnies over a gap, its target either becomes unreachable
	// or travel time is calculated from the bottom of the pit.
	// These timers allow to temporarily skip targer reachability/travel time tests.
	unsigned currentUnreachableTargetSequentialMillis { 0 };
	unsigned tolerableUnreachableTargetSequentialMillis { 700 };

	// Allow increased final travel time if the min travel time area is reachable by walking
	// from the final area and walking travel time is lower than this limit.
	// It allows to follow the reachability chain less strictly while still being close to it.
	unsigned tolerableWalkableIncreasedTravelTimeMillis { 3000 };

	// There is a mechanism for completely disabling an action for further planning by setting isDisabledForPlanning flag.
	// However we need a more flexible way of disabling an action after an failed application sequence.
	// A sequence started from different frame that the failed one might succeed.
	// An application sequence will not start at the frame indexed by this value.
	unsigned disabledForApplicationFrameIndex { std::numeric_limits<unsigned>::max() };

	// This should be set if we want to continue prediction
	// but give a path built by a current sequence an additional penalty
	// accounted by PredictionContext::CompleteOrSaveGoodEnoughPath()
	unsigned sequencePathPenalty { 0 };

	bool hasEnteredNavTargetArea { false };
	bool hasTouchedNavTarget { false };

	bool hasALatchedHop { false };
	bool didTheLatchedHop { false };

	unsigned hopsCounter { 0 };

	void SetupCommonBunnyHopInput( PredictionContext *context );
	// TODO: Mark as virtual in base class and mark as final here to avoid a warning about hiding parent member?
	bool GenericCheckIsActionEnabled( PredictionContext *context );
	bool CheckCommonBunnyHopPreconditions( PredictionContext *context );
	bool SetupBunnyHopping( const Vec3 &intendedLookVec, PredictionContext *context );
	bool CanFlyAboveGroundRelaxed( const PredictionContext *context ) const;
	bool CanSetWalljump( PredictionContext *context,
						 const Vec3 &velocity2DDir,
						 const Vec3 &intended2DLookDir ) const;
	void TrySetWalljump( PredictionContext *context,
						 const Vec3 &velocity2DDir,
						 const Vec3 &intended2DLookDir );

	// Can be overridden for finer control over tests
	virtual bool CheckStepSpeedGainOrLoss( PredictionContext *context );

	bool CheckNavTargetAreaTransition( PredictionContext *context );

	bool TryHandlingUnreachableTarget( PredictionContext *context );

	bool TryHandlingWorseTravelTimeToTarget( PredictionContext *context,
		                                     int currTravelTimeToTarget,
		                                     int groundedAreaNum );

	bool WasOnGroundThisFrame( const PredictionContext *context ) const;

	void EnsurePathPenalty( unsigned penalty ) {
		assert( penalty < 30000 );
		sequencePathPenalty = wsw::max( sequencePathPenalty, penalty );
	}

	bool CheckDirectReachWalkingOrFallingShort( int fromAreaNum, int toAreaNum );

	bool HasMadeAnAdvancementPriorToLanding( PredictionContext *context, int currTravelTimeToTarget );
public:
	BunnyHopAction( MovementSubsystem *subsystem, const char *name_, int debugColor_ = 0 )
		: BaseAction( subsystem, name_, debugColor_ ) {
		// Do NOT stop prediction on this! We have to check where the bot is going to land!
		BaseAction::stopPredictionOnTouchingNavEntity = false;
	}

	void CheckPredictionStepResults( PredictionContext *context ) override;
	void OnApplicationSequenceStarted( PredictionContext *context ) override;
	void OnApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason reason,
									   unsigned stoppedAtFrameIndex ) override;
	void BeforePlanning() override;
};

#endif
