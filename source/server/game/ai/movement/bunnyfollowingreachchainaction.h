#ifndef WSW_e82fabeb_dc12_4635_afa2_1bd0592bcc82_H
#define WSW_e82fabeb_dc12_4635_afa2_1bd0592bcc82_H

#include "bunnyhopaction.h"

class BunnyFollowingReachChainAction: public BunnyHopAction {
	int m_cachedCurrentAreaNum { -1 };
	int m_cachedDroppedAreaNum { -1 };
	int m_cachedReachNum { -1 };
	int m_cachedNextReachNum { -1 };
	bool m_cachedReachPointsToTrigger { false };
public:
	explicit BunnyFollowingReachChainAction( MovementSubsystem *subsystem )
		: BunnyHopAction( subsystem, "BunnyFollowingReachChainAction", COLOR_RGB( 192, 64, 108 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;

	void BeforePlanning() override {
		BunnyHopAction::BeforePlanning();
		m_cachedCurrentAreaNum = -1;
		m_cachedDroppedAreaNum = -1;
		m_cachedReachNum = -1;
		m_cachedNextReachNum = -1;
		m_cachedReachPointsToTrigger = false;
	}
};

#endif