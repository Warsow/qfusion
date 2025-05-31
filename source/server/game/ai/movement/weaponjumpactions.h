#ifndef WSW_f438caae_47fc_4ebb_ba29_7a7323ee5b33_H
#define WSW_f438caae_47fc_4ebb_ba29_7a7323ee5b33_H

#include "baseaction.h"

class ScheduleWeaponJumpAction: public BaseAction {
	friend class WeaponJumpWeaponsTester;

	enum { MAX_AREAS = 64 };

	bool TryJumpDirectlyToTarget( PredictionContext *context, const int *suitableWeapons, int numWeapons );
	// Gets raw nearby areas
	int GetCandidatesForJumpingToTarget( PredictionContext *context, int *areaNums );
	// Cuts off some raw areas using cheap tests
	// Modifies raw nearby areas buffer in-place
	int FilterRawCandidateAreas( PredictionContext *context, int *areaNums, int numRawAreas );
	// Filters out areas that are not (significantly) closer to the target
	// Modifies the supplied buffer in-place as well.
	// Writes travel times to target to the travel times buffer.
	int ReachTestNearbyTargetAreas( PredictionContext *context, int *areaNums, int *travelTimes, int numAreas );

	int GetCandidatesForReachChainShortcut( PredictionContext *context, int *areaNums );
	bool TryShortcutReachChain( PredictionContext *context, const int *suitableWeapons, int numWeapons );

	void PrepareJumpTargets( PredictionContext *context, const int *areaNums, vec3_t *targets, int numAreas );

	// Monotonically increasing dummy travel times (1, 2, ...).
	// Used for providing travel times for reach chain shortcut.
	// Areas in a reach chain are already ordered.
	// Using real travel times complicates interfaces in this case.
	static int dummyTravelTimes[MAX_AREAS];

	mutable bool hasTestedComputationQuota { false };
	mutable bool hasAcquiredComputationQuota { false };

	bool TryGetComputationQuota() const;

	/**
	 * Allows to get a rough estimate how expensive weapon jump tests are going to be
	 * (this depends of collision world complexity and AAS for the map)
	 * @return a value in [0, 1] range
	 */
	float EstimateMapComputationalComplexity() const;

	const int *GetTravelTimesForReachChainShortcut();

	void SaveLandingAreas( PredictionContext *context, int areaNum );
public:
	explicit ScheduleWeaponJumpAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "ScheduleWeaponJumpAction", COLOR_RGB( 0, 0, 0 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;

	void BeforePlanning() override {
		BaseAction::BeforePlanning();
		hasTestedComputationQuota = false;
		hasAcquiredComputationQuota = false;
	}
};

class TryTriggerWeaponJumpAction: public BaseAction {
public:
	explicit TryTriggerWeaponJumpAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "TryTriggerWeaponJumpAction", COLOR_RGB( 0, 0, 0 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;
};

class CorrectWeaponJumpAction: public BaseAction {
public:
	explicit CorrectWeaponJumpAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "CorrectWeaponJumpAction", COLOR_RGB( 0, 0, 0 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;
};

#endif
