#ifndef WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H
#define WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H

class Bot;
class MovementSubsystem;

#include "predictioncontext.h"

class BaseAction : public MovementPredictionConstants {
	friend class PredictionContext;
	void RegisterSelf();

protected:
	// Must be set by RegisterSelf() call. We have to break a circular dependency.
	Bot *bot { nullptr };
	MovementSubsystem *const m_subsystem;
	const char *name;

	// An action could set this field in PlanPredictionStep()
	// to avoid further redundant list lookup in PredictionContext::NextMovementStep().
	// The latter method gets and resets this field.
	const CMShapeList *thisFrameCMShapeList { nullptr };

	int debugColor;

	// Used to establish a direct mapping between integers and actions.
	// It is very useful for algorithms that involve lookup tables addressed by this field.
	// Must be set by RegisterSelf() call.
	unsigned actionNum { std::numeric_limits<unsigned>::max() };

	Vec3 originAtSequenceStart { 0, 0, 0 };

	unsigned sequenceStartFrameIndex { std::numeric_limits<unsigned>::max() };
	unsigned sequenceEndFrameIndex { std::numeric_limits<unsigned>::max() };

	// Has the action been completely disabled in current planning session for further planning
	bool isDisabledForPlanning { false };
	// These flags are used by default CheckPredictionStepResults() implementation.
	// Set these flags in child class to tweak the mentioned method behaviour.
	bool stopPredictionOnTouchingJumppad { true };
	bool stopPredictionOnTouchingTeleporter { true };
	bool stopPredictionOnTouchingPlatform { true };
	bool stopPredictionOnTouchingNavEntity { true };
	bool stopPredictionOnEnteringWater { true };
	bool failPredictionOnEnteringHazardImpactZone { true };

	class FlyUntilLandingAction &FlyUntilLandingAction();
	class LandOnSavedAreasAction &LandOnSavedAreasAction();

	void Debug( const char *format, ... ) const;
	// We want to have a full control over movement code assertions, so use custom ones for this class
	void Assert( bool condition, const char *message = nullptr ) const;
	template <typename T>
	void Assert( T conditionLikeValue, const char *message = nullptr ) const {
		Assert( conditionLikeValue != 0, message );
	}

	bool GenericCheckIsActionEnabled( PredictionContext *context ) const;

	void CheckDisableOrSwitchPreconditions( PredictionContext *context, const char *methodTag );

	void DisableWithAlternative( PredictionContext *context );
	void SwitchOrStop( PredictionContext *context );
	void SwitchOrRollback( PredictionContext *context );

	bool HasTouchedNavEntityThisFrame( PredictionContext *context );
public:
	inline BaseAction( MovementSubsystem *subsystem, const char *name_, int debugColor_ = 0 )
		: m_subsystem( subsystem ), name( name_ ), debugColor( debugColor_ ) {
		RegisterSelf();
	}
	virtual void PlanPredictionStep( PredictionContext *context ) = 0;
	virtual void ExecActionRecord( const MovementActionRecord *record,
								   BotInput *inputWillBeUsed,
								   PredictionContext *context = nullptr );

	virtual void CheckPredictionStepResults( PredictionContext *context );

	virtual void BeforePlanning();
	virtual void AfterPlanning() {}

	// If an action has been applied consequently in N frames, these frames are called an application sequence.
	// Usually an action is valid and can be applied in all application sequence frames except these cases:
	// N = 1 and the first (and the last) action application is invalid
	// N > 1 and the last action application is invalid
	// The first callback is very useful for saving some initial state
	// related to the frame for further checks during the entire application sequence.
	// The second callback is provided for symmetry reasons
	// (e.g. any resources that are allocated in the first callback might need cleanup).
	virtual void OnApplicationSequenceStarted( PredictionContext *context );

	// Might be called in a next frame, thats what stoppedAtFrameIndex is.
	// If application sequence has failed, stoppedAtFrameIndex is ignored.
	virtual void OnApplicationSequenceStopped( PredictionContext *context,
											   SequenceStopReason reason,
											   unsigned stoppedAtFrameIndex );

	unsigned SequenceDuration( const PredictionContext *context ) const;

	const char *Name() const { return name; }
	int DebugColor() const { return debugColor; }
	unsigned ActionNum() const { return actionNum; }
	bool IsDisabledForPlanning() const { return isDisabledForPlanning; }
};

// Lets not create excessive headers for these dummy action declarations

class HandleTriggeredJumppadAction : public BaseAction {
public:
	explicit HandleTriggeredJumppadAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "HandleTriggeredJumppadAction", COLOR_RGB( 0, 128, 128 ) ) {}
	void PlanPredictionStep( PredictionContext *context ) override;
};

class SwimMovementAction : public BaseAction {
public:
	explicit SwimMovementAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "SwimMovementAction", COLOR_RGB( 0, 0, 255 ) ) {
		this->stopPredictionOnEnteringWater = false;
	}
	void PlanPredictionStep( PredictionContext *context ) override;
	void CheckPredictionStepResults( PredictionContext *context ) override;
};

class FlyUntilLandingAction : public BaseAction {
public:
	explicit FlyUntilLandingAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "FlyUntilLandingAction", COLOR_RGB( 0, 255, 0 ) ) {}
	void PlanPredictionStep( PredictionContext *context ) override;
};

#endif
