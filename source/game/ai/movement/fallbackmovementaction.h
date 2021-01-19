#ifndef WSW_5509c612_0347_4d1b_bcae_5ec860bb84ec_H
#define WSW_5509c612_0347_4d1b_bcae_5ec860bb84ec_H

#include "basemovementaction.h"

/*
class MovementScript;

namespace wsw::ai::movement { struct NearbyTriggersCache; }

class FallbackMovementAction : public BaseMovementAction {
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

	const edict_t *FindClosestToTargetTrigger( MovementPredictionContext *context );

	const edict_t *FindClosestToTargetTrigger( const ClosestTriggerProblemParams &problemParams,
											   const wsw::ai::movement::NearbyTriggersCache &triggersCache );

	const edict_t *FindClosestToTargetTrigger( const ClosestTriggerProblemParams &problemParams,
											   const uint16_t *triggerEntNums,
											   int numTriggerEnts, int *travelTime );

	void SetupNavTargetAreaMovement( MovementPredictionContext *context );
	void SetupLostNavTargetMovement( MovementPredictionContext *context );
	MovementScript *TryFindMovementFallback( MovementPredictionContext *context );
	MovementScript *TryFindAasBasedFallback( MovementPredictionContext *context );

	MovementScript *TryFindWalkReachFallback( MovementPredictionContext *context,
												   const aas_reachability_t &nextReach );

	MovementScript *TryFindWalkOffLedgeReachFallback( MovementPredictionContext *context,
														   const aas_reachability_t &nextReach );

	MovementScript *TryFindJumpLikeReachFallback( MovementPredictionContext *context,
													   const aas_reachability_t &nextReach );

	MovementScript *TryFindStairsFallback( MovementPredictionContext *context );
	MovementScript *TryFindRampFallback( MovementPredictionContext *context, int rampAreaNum, int forbiddenAreaNum = 0 );
	MovementScript *TryFindLostNavTargetFallback( MovementPredictionContext *context );
	MovementScript *TryFindNearbyRampAreasFallback( MovementPredictionContext *context );
	MovementScript *TryFindWalkableTriggerFallback( MovementPredictionContext *context );

	MovementScript *TryFindJumpFromLavaFallback( MovementPredictionContext *context ) {
		return TryFindJumpToSpotFallback( context, false );
	}

	MovementScript *TryFindJumpAdvancingToTargetFallback( MovementPredictionContext *context );

	MovementScript *TryFindJumpToSpotFallback( MovementPredictionContext *context, bool testTravelTime );

	MovementScript *TryNodeBasedFallbacksLeft( MovementPredictionContext *context );

	MovementScript *TryShortcutOtherFallbackByJumping( MovementPredictionContext *context,
															const vec3_t initialTarget,
															int initialTargetAreaNum = 0 );

	MovementScript *TryShortcutOtherFallbackByJumping( MovementPredictionContext *context,
															int initialTargetAreaNum );
public:
	DECLARE_MOVEMENT_ACTION_CONSTRUCTOR( FallbackMovementAction, COLOR_RGB( 0, 0, 0 ) ) {}
	void PlanPredictionStep( MovementPredictionContext *context ) override;
	void CheckPredictionStepResults( MovementPredictionContext *context ) override {
		AI_FailWith( __FUNCTION__, "This method should never get called (PlanMovmementStep() should stop planning)\n" );
	}
};
*/

#endif
