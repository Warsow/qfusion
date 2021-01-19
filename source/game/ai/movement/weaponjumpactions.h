#ifndef WSW_f438caae_47fc_4ebb_ba29_7a7323ee5b33_H
#define WSW_f438caae_47fc_4ebb_ba29_7a7323ee5b33_H

#include "basemovementaction.h"

//class ScheduleWeaponJumpAction: public BaseMovementAction {
//	friend class WeaponJumpWeaponsTester;
//
//	enum { MAX_AREAS = 64 };
//
//	bool TryJumpDirectlyToTarget( MovementPredictionContext *context, const int *suitableWeapons, int numWeapons );
//	// Gets raw nearby areas
//	int GetCandidatesForJumpingToTarget( MovementPredictionContext *context, int *areaNums );
//	// Cuts off some raw areas using cheap tests
//	// Modifies raw nearby areas buffer in-place
//	int FilterRawCandidateAreas( MovementPredictionContext *context, int *areaNums, int numRawAreas );
//	// Filters out areas that are not (significantly) closer to the target
//	// Modifies the supplied buffer in-place as well.
//	// Writes travel times to target to the travel times buffer.
//	int ReachTestNearbyTargetAreas( MovementPredictionContext *context, int *areaNums, int *travelTimes, int numAreas );
//
//	int GetCandidatesForReachChainShortcut( MovementPredictionContext *context, int *areaNums );
//	bool TryShortcutReachChain( MovementPredictionContext *context, const int *suitableWeapons, int numWeapons );
//
//	void PrepareJumpTargets( MovementPredictionContext *context, const int *areaNums, vec3_t *targets, int numAreas );
//
//	// Monotonically increasing dummy travel times (1, 2, ...).
//	// Used for providing travel times for reach chain shortcut.
//	// Areas in a reach chain are already ordered.
//	// Using real travel times complicates interfaces in this case.
//	static int dummyTravelTimes[MAX_AREAS];
//
//	mutable bool hasTestedComputationQuota { false };
//	mutable bool hasAcquiredComputationQuota { false };
//
//	inline bool TryGetComputationQuota() const;
//
//	/**
//	 * Allows to get a rough estimate how expensive weapon jump tests are going to be
//	 * (this depends of collision world complexity and AAS for the map)
//	 * @return a value in [0, 1] range
//	 */
//	inline float EstimateMapComputationalComplexity() const;
//
//	inline const int *GetTravelTimesForReachChainShortcut();
//
//	void SaveLandingAreas( MovementPredictionContext *context, int areaNum );
//public:
//	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( ScheduleWeaponJumpAction, COLOR_RGB( 0, 0, 0 ) ) {}
//
//	void PlanPredictionStep( MovementPredictionContext *context ) override;
//
//	void BeforePlanning() override {
//		BaseMovementAction::BeforePlanning();
//		hasTestedComputationQuota = false;
//		hasAcquiredComputationQuota = false;
//	}
//};
//
//class TryTriggerWeaponJumpAction: public BaseMovementAction {
//public:
//	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( TryTriggerWeaponJumpAction, COLOR_RGB( 0, 0, 0 ) ) {}
//
//	void PlanPredictionStep( MovementPredictionContext *context ) override;
//};
//
//class CorrectWeaponJumpAction: public BaseMovementAction {
//public:
//	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( CorrectWeaponJumpAction, COLOR_RGB( 0, 0, 0 ) ) {}
//
//	void PlanPredictionStep( MovementPredictionContext *context ) override;
//};

#endif
