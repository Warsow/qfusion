#ifndef WSW_5509c612_0347_4d1b_bcae_5ec860bb84ec_H
#define WSW_5509c612_0347_4d1b_bcae_5ec860bb84ec_H

#include "baseaction.h"

class MovementScript;

namespace wsw::ai::movement { struct NearbyTriggersCache; }

class FallbackAction : public BaseAction {
	struct ClosestTriggerProblemParams {
	private:
		vec3_t origin;
		int fromAreaNums[2];
	public:
		const int numFromAreas;
		const int goalAreaNum;

		ClosestTriggerProblemParams( const vec3_t origin_, const int *fromAreaNums_, int numFromAreas_, int goalAreaNum_ )
			: numFromAreas( numFromAreas_ ), goalAreaNum( goalAreaNum_ ) {
			VectorCopy( origin_, this->origin );
			fromAreaNums[0] = numFromAreas_ > 0 ? fromAreaNums_[0] : 0;
			fromAreaNums[1] = numFromAreas_ > 1 ? fromAreaNums_[1] : 0;
		}

		const float *Origin() const { return origin; }
		const int *FromAreaNums() const { return fromAreaNums; }
	};

	const edict_t *FindClosestToTargetTrigger( PredictionContext *context );

	const edict_t *FindClosestToTargetTrigger( const ClosestTriggerProblemParams &problemParams,
											   const wsw::ai::movement::NearbyTriggersCache &triggersCache );

	const edict_t *FindClosestToTargetTrigger( const ClosestTriggerProblemParams &problemParams,
											   const uint16_t *triggerEntNums,
											   int numTriggerEnts, int *travelTime );

	void SetupNavTargetAreaMovement( PredictionContext *context );
	void SetupLostNavTargetMovement( PredictionContext *context );
	MovementScript *TryFindMovementFallback( PredictionContext *context );
	MovementScript *TryFindAasBasedFallback( PredictionContext *context );

	MovementScript *TryFindWalkReachFallback( PredictionContext *context,
												   const aas_reachability_t &nextReach );

	MovementScript *TryFindWalkOffLedgeReachFallback( PredictionContext *context,
														   const aas_reachability_t &nextReach );

	MovementScript *TryFindJumpLikeReachFallback( PredictionContext *context,
													   const aas_reachability_t &nextReach );

	MovementScript *TryFindStairsFallback( PredictionContext *context );
	MovementScript *TryFindRampFallback( PredictionContext *context, int rampAreaNum, int forbiddenAreaNum = 0 );
	MovementScript *TryFindLostNavTargetFallback( PredictionContext *context );
	MovementScript *TryFindNearbyRampAreasFallback( PredictionContext *context );
	MovementScript *TryFindWalkableTriggerFallback( PredictionContext *context );

	MovementScript *TryFindJumpFromLavaFallback( PredictionContext *context ) {
		return TryFindJumpToSpotFallback( context, false );
	}

	MovementScript *TryFindJumpAdvancingToTargetFallback( PredictionContext *context );

	MovementScript *TryFindJumpToSpotFallback( PredictionContext *context, bool testTravelTime );

	MovementScript *TryNodeBasedFallbacksLeft( PredictionContext *context );

	bool CanWaitForLanding( PredictionContext *context );
public:
	explicit FallbackAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "FallbackAction", COLOR_RGB( 0, 0, 0 ) ) {}

	void PlanPredictionStep( PredictionContext *context ) override;
	void CheckPredictionStepResults( PredictionContext *context ) override {
		AI_FailWith( __FUNCTION__, "This method should never get called (PlanMovmementStep() should stop planning)\n" );
	}
};

#endif
