#ifndef WSW_3bf645b5_e1c5_48b5_b4f5_a2eb3bb75631_H
#define WSW_3bf645b5_e1c5_48b5_b4f5_a2eb3bb75631_H

#include "bunnyhopaction.h"

class BunnyToStairsOrRampExitAction: public BunnyHopAction {
	float *m_intendedLookDir { nullptr };
	Vec3 m_lookDirStorage { vec3_origin };
	int m_targetFloorCluster { 0 };

	bool tryFindingAndSavingLookDir( PredictionContext *context );
	void trySavingExitFloorCluster( PredictionContext *context, int exitAreaNum );
public:
	explicit BunnyToStairsOrRampExitAction( MovementSubsystem *subsystem ):
		BunnyHopAction( subsystem, "BunnyToStairsOrRampExitAction", COLOR_RGB( 0, 255, 255 ) ) {}

	void beforePlanning() override {
		BunnyHopAction::beforePlanning();
		m_targetFloorCluster = 0;
		m_intendedLookDir = nullptr;
	}

	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
	[[nodiscard]]
	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;
};

#endif
