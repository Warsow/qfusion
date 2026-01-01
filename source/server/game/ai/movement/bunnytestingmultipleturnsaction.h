#ifndef WSW_9849a266_7efe_4445_8b70_994d3ad35223_H
#define WSW_9849a266_7efe_4445_8b70_994d3ad35223_H

#include "bunnyhopaction.h"

class BunnyTestingMultipleTurnsAction : public BunnyHopAction {
	Vec3 m_initialDir { 0, 0, 0 };
	int m_attemptNum { 0 };
	bool m_hasWalljumped { false };

	static constexpr const auto kMaxAngles = 4;
	static constexpr const auto kMaxAttempts = 2 * kMaxAngles;

	static const float kAngularSpeed[kMaxAngles];
public:
	explicit BunnyTestingMultipleTurnsAction( MovementSubsystem *subsystem )
		: BunnyHopAction( subsystem, "BunnyTestingMultipleTurnsAction", COLOR_RGB( 255, 0, 0 ) ) {}

	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;

	void beforePlanning() override {
		BunnyHopAction::beforePlanning();
		m_attemptNum = 0;
	}

	void onApplicationSequenceStarted( PredictionContext *context ) override {
		BunnyHopAction::onApplicationSequenceStarted( context );
		m_hasWalljumped = false;
	}

	void onApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
};

#endif
