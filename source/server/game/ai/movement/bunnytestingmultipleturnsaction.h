#ifndef WSW_9849a266_7efe_4445_8b70_994d3ad35223_H
#define WSW_9849a266_7efe_4445_8b70_994d3ad35223_H

#include "bunnyhopaction.h"

class BunnyTestingMultipleTurnsAction : public BunnyHopAction {
	Vec3 initialDir { 0, 0, 0 };
	int attemptNum { 0 };
	bool hasWalljumped { false };

	static constexpr const auto kMaxAngles = 4;
	static constexpr const auto kMaxAttempts = 2 * kMaxAngles;

	static const float kAngularSpeed[kMaxAngles];
public:
	explicit BunnyTestingMultipleTurnsAction( MovementSubsystem *subsystem )
		: BunnyHopAction( subsystem, "BunnyTestingMultipleTurnsAction", COLOR_RGB( 255, 0, 0 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;

	void BeforePlanning() override {
		BunnyHopAction::BeforePlanning();
		attemptNum = 0;
	}

	void OnApplicationSequenceStarted( PredictionContext *context ) override {
		BunnyHopAction::OnApplicationSequenceStarted( context );
		hasWalljumped = false;
	}

	void OnApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
};

#endif
