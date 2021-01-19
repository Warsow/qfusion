#ifndef WSW_3bf645b5_e1c5_48b5_b4f5_a2eb3bb75631_H
#define WSW_3bf645b5_e1c5_48b5_b4f5_a2eb3bb75631_H

#include "bunnyhopaction.h"

class BunnyToStairsOrRampExitAction: public BunnyHopAction {
	float *intendedLookDir { nullptr };
	Vec3 lookDirStorage { vec3_origin };
	int targetFloorCluster { 0 };

	bool TryFindAndSaveLookDir( MovementPredictionContext *context );
	void TrySaveExitFloorCluster( MovementPredictionContext *context, int exitAreaNum );
public:
	explicit BunnyToStairsOrRampExitAction( BaseScript2 *script ):
		BunnyHopAction( script, "BunnyToStairsOrRampExitAction", COLOR_RGB( 0, 255, 255 ) ) {}

	void BeforePlanning() override {
		BunnyHopAction::BeforePlanning();
		targetFloorCluster = 0;
		intendedLookDir = nullptr;
	}

	void PlanPredictionStep( MovementPredictionContext *context ) override;
	void CheckPredictionStepResults( MovementPredictionContext *context ) override;
};

#endif
