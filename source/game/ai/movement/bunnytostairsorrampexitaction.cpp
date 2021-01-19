#include "movementlocal.h"
#include "bunnytostairsorrampexitaction.h"

void BunnyToStairsOrRampExitAction::PlanPredictionStep( MovementPredictionContext *context ) {
	// This action is the first applied action as it is specialized
	// and falls back to other bunnying actions if it cannot be applied.
	if( !GenericCheckIsActionEnabled( context ) ) {
		return;
	}

	if( !CheckCommonBunnyHopPreconditions( context ) ) {
		return;
	}

	if( !intendedLookDir ) {
		if( !TryFindAndSaveLookDir( context ) ) {
			this->isDisabledForPlanning = true;
			context->SetPendingRollback();
			return;
		}
	}

	if( !SetupBunnyHopping( Vec3( intendedLookDir ), context ) ) {
		return;
	}
}

bool BunnyToStairsOrRampExitAction::TryFindAndSaveLookDir( MovementPredictionContext *context ) {
	int groundedAreaNum = context->CurrGroundedAasAreaNum();
	if( !groundedAreaNum ) {
		Debug( "A current grounded area num is not defined\n" );
		return false;
	}

	const auto *aasWorld = AiAasWorld::Instance();
	if( aasWorld->AreaSettings()[ groundedAreaNum ].areaflags & AREA_INCLINED_FLOOR ) {
		const int *exitAreaNum = TryFindBestInclinedFloorExitArea( context, groundedAreaNum, groundedAreaNum );
		if( !exitAreaNum ) {
			Debug( "Can't find an exit area of the current grouned inclined floor area\n" );
			return false;
		}

		Debug( "Found a best exit area of an inclined floor area\n" );
		lookDirStorage.Set( aasWorld->Areas()[*exitAreaNum].center );
		lookDirStorage -= context->movementState->entityPhysicsState.Origin();
		lookDirStorage.Normalize();
		intendedLookDir = lookDirStorage.Data();

		TrySaveExitFloorCluster( context, *exitAreaNum );
		return true;
	}

	const int stairsClusterNum = aasWorld->StairsClusterNum( groundedAreaNum );
	if( !stairsClusterNum ) {
		Debug( "The current grounded area is neither an inclined floor area, nor a stairs cluster area\n" );
		return false;
	}

	const auto *exitAreaNum = TryFindBestStairsExitArea( context, stairsClusterNum );
	if( !exitAreaNum ) {
		Debug( "Can't find an exit area of the current stairs cluster\n" );
		return false;
	}

	Debug( "Found a best exit area of an stairs cluster\n" );
	lookDirStorage.Set( aasWorld->Areas()[*exitAreaNum].center );
	lookDirStorage -= context->movementState->entityPhysicsState.Origin();
	lookDirStorage.Normalize();
	intendedLookDir = lookDirStorage.Data();

	// Try find an area that is a boundary area of the exit area and is in a floor cluster
	TrySaveExitFloorCluster( context, *exitAreaNum );
	return true;
}

void BunnyToStairsOrRampExitAction::TrySaveExitFloorCluster( MovementPredictionContext *context, int exitAreaNum ) {
	const auto *const aasWorld = AiAasWorld::Instance();
	const auto *const aasReach = aasWorld->Reachabilities();
	const auto *const aasFloorClusterNums = aasWorld->AreaFloorClusterNums();
	const auto *const routeCache = context->RouteCache();

	// Check whether exit area is already in cluster
	targetFloorCluster = aasFloorClusterNums[exitAreaNum];
	if( targetFloorCluster ) {
		return;
	}

	const int targetAreaNum = context->NavTargetAasAreaNum();

	int areaNum = exitAreaNum;
	while( areaNum != targetAreaNum ) {
		int reachNum;
		if( !routeCache->PreferredRouteToGoalArea( areaNum, targetAreaNum, &reachNum ) ) {
			break;
		}
		const auto &reach = aasReach[reachNum];
		const int travelType = reach.traveltype & TRAVELTYPE_MASK;
		if( travelType != TRAVEL_WALK ) {
			break;
		}
		const int nextAreaNum = reach.areanum;
		targetFloorCluster = aasFloorClusterNums[nextAreaNum];
		if( targetFloorCluster ) {
			break;
		}
		areaNum = nextAreaNum;
	}
}

void BunnyToStairsOrRampExitAction::CheckPredictionStepResults( MovementPredictionContext *context ) {
	// We skip the direct superclass method call!
	// Much more lenient checks are used for this specialized action.
	// Only generic checks for all movement actions should be performed in addition.
	BaseMovementAction::CheckPredictionStepResults( context );
	if( context->cannotApplyAction || context->isCompleted ) {
		return;
	}

	// There is no target floor cluster saved
	if( !targetFloorCluster ) {
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	// Make sure we don't stop prediction at start.
	// The distance threshold is low due to troublesome movement in these kinds of areas.
	if( originAtSequenceStart.SquareDistance2DTo( entityPhysicsState.Origin() ) < SQUARE( 20 ) ) {
		return;
	}

	// If the bot has not touched a ground this frame
	if( !entityPhysicsState.GroundEntity() && !context->frameEvents.hasJumped ) {
		return;
	}

	if( AiAasWorld::Instance()->FloorClusterNum( context->CurrGroundedAasAreaNum() ) != targetFloorCluster ) {
		return;
	}

	Debug( "The prediction step has lead to touching a ground in the target floor cluster" );
	context->isCompleted = true;
}
