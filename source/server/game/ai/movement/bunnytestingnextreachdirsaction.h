#ifndef WSW_070ae985_cb80_4626_bc36_42ebcb6f2716_H
#define WSW_070ae985_cb80_4626_bc36_42ebcb6f2716_H

#include "bunnytestingmultipledirsaction.h"

class BunnyTestingNextReachDirsAction final : public BunnyTestingSavedLookDirsAction {
	static constexpr const char *NAME = "BunnyTestingNextReachDirsAction";

	friend class BunnyToBestNavMeshPointAction;

	void SaveSuggestedLookDirs( PredictionContext *context ) override;
public:
	explicit BunnyTestingNextReachDirsAction( MovementSubsystem *subsystem );

	void BeforePlanning() override;
};

#endif
