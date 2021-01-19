#ifndef WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H
#define WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H

class Bot;
class BotMovementModule;

class BaseScript2;

#include "predictioncontext.h"

class BaseMovementAction : public MovementPredictionConstants
{
	friend class MovementPredictionContext;
	void RegisterSelf();

protected:
	// Must be set by RegisterSelf() call. We have to break a circular dependency.
	Bot *bot { nullptr };
	BaseScript2 *const m_script;
	const char *name;

	// An action could set this field in PlanPredictionStep()
	// to avoid further redundant list lookup in MovementPredictionContext::NextMovementStep().
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

	void Debug( const char *format, ... ) const;
	// We want to have a full control over movement code assertions, so use custom ones for this class
	inline void Assert( bool condition, const char *message = nullptr ) const;
	template <typename T>
	inline void Assert( T conditionLikeValue, const char *message = nullptr ) const {
		Assert( conditionLikeValue != 0, message );
	}

	inline bool GenericCheckIsActionEnabled( MovementPredictionContext *context,
											 BaseMovementAction *suggestedAction = nullptr ) const;

	inline void CheckDisableOrSwitchPreconditions( MovementPredictionContext *context, const char *methodTag );

	inline void DisableWithAlternative( MovementPredictionContext *context, BaseMovementAction *suggestedAction );
	inline void SwitchOrStop( MovementPredictionContext *context, BaseMovementAction *suggestedAction );
	inline void SwitchOrRollback( MovementPredictionContext *context, BaseMovementAction *suggestedAction );

	bool HasTouchedNavEntityThisFrame( MovementPredictionContext *context );
public:
	inline BaseMovementAction( BaseScript2 *script_, const char *name_, int debugColor_ = 0 )
		: m_script( script_ ), name( name_ ), debugColor( debugColor_ ) {
		RegisterSelf();
	}
	virtual void PlanPredictionStep( MovementPredictionContext *context ) = 0;
	virtual void ExecActionRecord( const MovementActionRecord *record,
								   BotInput *inputWillBeUsed,
								   MovementPredictionContext *context = nullptr );

	virtual void CheckPredictionStepResults( MovementPredictionContext *context );

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
	virtual void OnApplicationSequenceStarted( MovementPredictionContext *context );

	// Might be called in a next frame, thats what stoppedAtFrameIndex is.
	// If application sequence has failed, stoppedAtFrameIndex is ignored.
	virtual void OnApplicationSequenceStopped( MovementPredictionContext *context,
											   SequenceStopReason reason,
											   unsigned stoppedAtFrameIndex );

	unsigned SequenceDuration( const MovementPredictionContext *context ) const;

	inline const char *Name() const { return name; }
	inline int DebugColor() const { return debugColor; }
	inline unsigned ActionNum() const { return actionNum; }
	inline bool IsDisabledForPlanning() const { return isDisabledForPlanning; }
};

#define DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( name, debugColor_ ) \
	name( BaseScript2 *script ) : BaseMovementAction( script, #name, debugColor_ )

// Lets not create excessive headers for these dummy action declarations
/*
class HandleTriggeredJumppadAction : public BaseMovementAction
{
public:
	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( HandleTriggeredJumppadAction, COLOR_RGB( 0, 128, 128 ) ) {}
	void PlanPredictionStep( MovementPredictionContext *context ) override;
};

class SwimMovementAction : public BaseMovementAction
{
public:
	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( SwimMovementAction, COLOR_RGB( 0, 0, 255 ) ) {
		this->stopPredictionOnEnteringWater = false;
	}
	void PlanPredictionStep( MovementPredictionContext *context ) override;
	void CheckPredictionStepResults( MovementPredictionContext *context ) override;
};

class FlyUntilLandingAction : public BaseMovementAction
{
public:
	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( FlyUntilLandingAction, COLOR_RGB( 0, 255, 0 ) ) {}
	void PlanPredictionStep( MovementPredictionContext *context ) override;
};*/

#endif
