#ifndef WSW_fe1d72af_a22c_48bd_b858_ef25e129ccb1_H
#define WSW_fe1d72af_a22c_48bd_b858_ef25e129ccb1_H

#include "baseaction.h"

class LandOnSavedAreasAction : public BaseAction {
	friend class HandleTriggeredJumppadAction;

	wsw::StaticVector<int, MAX_SAVED_LANDING_AREAS> savedLandingAreas;
	using FilteredAreas = wsw::StaticVector<AreaAndScore, MAX_SAVED_LANDING_AREAS * 2>;

	int currAreaIndex { 0 };
	unsigned totalTestedAreas { 0 };

	// Returns a Z level when the landing is expected to be started
	float SaveJumppadLandingAreas( const edict_t *jumppadEntity );
public:
	explicit LandOnSavedAreasAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "LandOnSavedAreasAction", COLOR_RGB( 255, 0, 255 ) ) {}

	bool TryLandingStepOnArea( int areaNum, PredictionContext *context );
	void PlanPredictionStep( PredictionContext *context ) override;
	void CheckPredictionStepResults( PredictionContext *context ) override;
	void BeforePlanning() override;
	void AfterPlanning() override;
};

#endif
