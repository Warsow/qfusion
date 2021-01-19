#ifndef WSW_bbfd3c31_cdfd_4dd8_882f_b8854c95351e_H
#define WSW_bbfd3c31_cdfd_4dd8_882f_b8854c95351e_H

#include "basemovementaction.h"

/*
class RidePlatformAction : public BaseMovementAction
{
	friend class BotMovementModule;

public:
	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( RidePlatformAction, COLOR_RGB( 128, 128, 0 ) ) {
		// Shut an analyzer up
		currTestedAreaIndex = 0;
	}
	void PlanPredictionStep( MovementPredictionContext *context ) override;
	void CheckPredictionStepResults( MovementPredictionContext *context ) override;

	void BeforePlanning() override {
		BaseMovementAction::BeforePlanning();
		currTestedAreaIndex = 0;
	}

	void OnApplicationSequenceStopped( MovementPredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;

	static constexpr auto MAX_SAVED_AREAS = MovementPredictionContext::MAX_SAVED_LANDING_AREAS;
	using ExitAreasVector = wsw::StaticVector<int, MAX_SAVED_AREAS>;

private:
	ExitAreasVector tmpExitAreas;
	unsigned currTestedAreaIndex;

	const edict_t *GetPlatform( MovementPredictionContext *context ) const;
	// A context might be null!
	void TrySaveExitAreas( MovementPredictionContext *context, const edict_t *platform );
	const ExitAreasVector &SuggestExitAreas( MovementPredictionContext *context, const edict_t *platform );
	void FindExitAreas( MovementPredictionContext *context, const edict_t *platform, ExitAreasVector &exitAreas );

	void SetupIdleRidingPlatformMovement( MovementPredictionContext *context, const edict_t *platform );
	void SetupExitPlatformMovement( MovementPredictionContext *context, const edict_t *platform );
};*/

#endif
