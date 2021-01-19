#ifndef WSW_7950e788_bd5a_46ca_8d72_c27da961c713_H
#define WSW_7950e788_bd5a_46ca_8d72_c27da961c713_H

#include "bunnytestingmultipledirsaction.h"

class BunnyToBestFloorClusterPointAction final : public BunnyTestingMultipleLookDirsAction {
	using Super = BunnyTestingMultipleLookDirsAction;

	Vec3 localDirStorage { 0, 0, 0 };

	bool hasTestedSameCluster { false };
	bool hasTestedNextCluster { false };

	static constexpr const char *NAME = "BunnyToBestFloorClusterPointAction";

	void OnApplicationSequenceStarted( MovementPredictionContext *context ) override;

	void OnApplicationSequenceFailed( MovementPredictionContext *context, unsigned ) override;
public:
	explicit BunnyToBestFloorClusterPointAction( BaseScript2 *script );

	void BeforePlanning() override {
		Super::BeforePlanning();
		hasTestedSameCluster = false;
		hasTestedNextCluster = false;
	}
};

#endif
