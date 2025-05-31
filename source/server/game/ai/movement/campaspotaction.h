#ifndef WSW_06604b3c_dd68_43dd_b482_d92433ea23c6_H
#define WSW_06604b3c_dd68_43dd_b482_d92433ea23c6_H

#include "baseaction.h"

class CampASpotMovementAction : public BaseAction {
	unsigned disabledForApplicationFrameIndex { std::numeric_limits<unsigned>::max() };

	bool TryUpdateKeyMoveDirs( PredictionContext *context );
	Vec3 GetUpdatedPendingLookDir( PredictionContext *context );
public:
	explicit CampASpotMovementAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "CampASpotMovementAction", COLOR_RGB( 128, 0, 128 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;
	void CheckPredictionStepResults( PredictionContext *context ) override;
	void OnApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
	void BeforePlanning() override {
		BaseAction::BeforePlanning();
		disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	}
};

#endif
