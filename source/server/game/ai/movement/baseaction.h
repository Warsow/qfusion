#ifndef WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H
#define WSW_5cf0a1e2_2a76_4db2_829e_e062cb495eb2_H

class Bot;
class MovementSubsystem;

#include "predictioncontext.h"

class BaseAction : public MovementPredictionConstants {
	friend class PredictionContext;
public:
	BaseAction( MovementSubsystem *subsystem, const char *name, int debugColor )
		: m_subsystem( subsystem ), m_name( name ), m_debugColor( debugColor ) {}

	virtual auto planPredictionStep( PredictionContext *context ) -> PredictionResult = 0;

	virtual void execActionRecord( const MovementActionRecord *record,
								   BotInput *inputWillBeUsed,
								   PredictionContext *context = nullptr );

	[[nodiscard]]
	virtual auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult;

	virtual void beforePlanning();
	virtual void afterPlanning() {}

	// If an action has been applied consequently in N frames, these frames are called an application sequence.
	// Usually an action is valid and can be applied in all application sequence frames except these cases:
	// N = 1 and the first (and the last) action application is invalid
	// N > 1 and the last action application is invalid
	// The first callback is very useful for saving some initial state
	// related to the frame for further checks during the entire application sequence.
	// The second callback is provided for symmetry reasons
	// (e.g. any resources that are allocated in the first callback might need cleanup).
	virtual void onApplicationSequenceStarted( PredictionContext *context );

	// Might be called in a next frame, thats what stoppedAtFrameIndex is.
	// If application sequence has failed, stoppedAtFrameIndex is ignored.
	virtual void onApplicationSequenceStopped( PredictionContext *context,
											   SequenceStopReason reason,
											   unsigned stoppedAtFrameIndex );

	[[nodiscard]]
	auto getSequenceDuration( const PredictionContext *context ) const -> unsigned;

	[[nodiscard]]
	auto getName() const -> const char * { return m_name; }
	[[nodiscard]]
	auto getDebugColor() const -> int { return m_debugColor; }

protected:
	Bot *m_bot { nullptr };
	MovementSubsystem *const m_subsystem;
	const char *m_name;

	// An action could set this field in PlanPredictionStep()
	// to avoid further redundant list lookup in PredictionContext::NextMovementStep().
	// The latter method gets and resets this field.
	const CMShapeList *m_thisFrameCMShapeList { nullptr };

	const int m_debugColor;

	Vec3 m_originAtSequenceStart { 0, 0, 0 };

	unsigned m_sequenceStartFrameIndex { std::numeric_limits<unsigned>::max() };
	unsigned m_sequenceEndFrameIndex { std::numeric_limits<unsigned>::max() };

	// Has the action been completely disabled in current planning session for further planning
	bool m_isDisabledForPlanning { false };
	// These flags are used by default CheckPredictionStepResults() implementation.
	// Set these flags in child class to tweak the mentioned method behaviour.
	bool m_stopPredictionOnTouchingJumppad { true };
	bool m_stopPredictionOnTouchingTeleporter { true };
	bool m_stopPredictionOnTouchingPlatform { true };
	bool m_stopPredictionOnTouchingNavEntity { true };
	bool m_stopPredictionOnEnteringWater { true };
	bool m_failPredictionOnEnteringHazardImpactZone { true };

	void Debug( const char *format, ... ) const;
	// We want to have a full control over movement code assertions, so use custom ones for this class
	void Assert( bool condition, const char *message = nullptr ) const;
	template <typename T>
	void Assert( T conditionLikeValue, const char *message = nullptr ) const {
		Assert( conditionLikeValue != 0, message );
	}

	[[nodiscard]]
	auto genericCheckIsActionEnabled( PredictionContext *context ) const -> PredictionResult;

	[[nodiscard]]
	bool checkNoBumpingOrBouncing( PredictionContext *context );

	[[nodiscard]]
	bool hasTouchedNavEntityThisFrame( PredictionContext *context );
};

#endif
