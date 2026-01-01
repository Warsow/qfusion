#ifndef WSW_7950e788_bd5a_46ca_8d72_c27da961c713_H
#define WSW_7950e788_bd5a_46ca_8d72_c27da961c713_H

#include "bunnytestingmultipledirsaction.h"

class BunnyToBestFloorClusterPointAction final : public BunnyTestingMultipleLookDirsAction {
	using Super = BunnyTestingMultipleLookDirsAction;

	Vec3 m_localDirStorage { 0, 0, 0 };

	bool m_hasTestedSameCluster { false };
	bool m_hasTestedNextCluster { false };

	static constexpr const char *NAME = "BunnyToBestFloorClusterPointAction";

	void onApplicationSequenceStarted( PredictionContext *context ) override;

	void onApplicationSequenceFailed( PredictionContext *context, unsigned ) override;
public:
	explicit BunnyToBestFloorClusterPointAction( MovementSubsystem *subsystem );

	void beforePlanning() override {
		Super::beforePlanning();
		m_hasTestedSameCluster = false;
		m_hasTestedNextCluster = false;
	}
};

#endif
