#include "movementlocal.h"
#include "bunnytostairsorrampexitaction.h"

void BunnyToStairsOrRampExitAction::PlanPredictionStep( PredictionContext *context ) {
	// This action is the first applied action as it is specialized
	// and falls back to other bunnying actions if it cannot be applied.
	if( !GenericCheckIsActionEnabled( context, &m_subsystem->bunnyFollowingReachChainAction ) ) {
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

bool BunnyToStairsOrRampExitAction::TryFindAndSaveLookDir( PredictionContext *context ) {
	int groundedAreaNum = context->CurrGroundedAasAreaNum();
	if( !groundedAreaNum ) {
		Debug( "A current grounded area num is not defined\n" );
		return false;
	}

	const auto *aasWorld = AiAasWorld::instance();
	if( aasWorld->getAreaSettings()[ groundedAreaNum ].areaflags & AREA_INCLINED_FLOOR ) {
		const int *exitAreaNum = TryFindBestInclinedFloorExitArea( context, groundedAreaNum, groundedAreaNum );
		if( !exitAreaNum ) {
			Debug( "Can't find an exit area of the current grouned inclined floor area\n" );
			return false;
		}

		Debug( "Found a best exit area of an inclined floor area\n" );
		lookDirStorage.Set( aasWorld->getAreas()[*exitAreaNum].center );
		lookDirStorage -= context->movementState->entityPhysicsState.Origin();
		if( !lookDirStorage.normalize() ) {
			return false;
		}

		intendedLookDir = lookDirStorage.Data();

		TrySaveExitFloorCluster( context, *exitAreaNum );
		return true;
	}

	const int stairsClusterNum = aasWorld->stairsClusterNum( groundedAreaNum );
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
	lookDirStorage.Set( aasWorld->getAreas()[*exitAreaNum].center );
	lookDirStorage -= context->movementState->entityPhysicsState.Origin();
	if( !lookDirStorage.normalize() ) {
		return false;
	}

	intendedLookDir = lookDirStorage.Data();

	// Try find an area that is a boundary area of the exit area and is in a floor cluster
	TrySaveExitFloorCluster( context, *exitAreaNum );
	return true;
}

void BunnyToStairsOrRampExitAction::TrySaveExitFloorCluster( PredictionContext *context, int exitAreaNum ) {
	const auto *const aasWorld = AiAasWorld::instance();
	const auto aasReach = aasWorld->getReaches();
	const auto aasFloorClusterNums = aasWorld->areaFloorClusterNums();
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
		if( !routeCache->FindRoute( areaNum, targetAreaNum, bot->TravelFlags(), &reachNum ) ) {
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

void BunnyToStairsOrRampExitAction::CheckPredictionStepResults( PredictionContext *context ) {
	// We skip the direct superclass method call!
	// Much more lenient checks are used for this specialized action.
	// Only generic checks for all movement actions should be performed in addition.
	BaseAction::CheckPredictionStepResults( context );
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
	if( originAtSequenceStart.SquareDistance2DTo( entityPhysicsState.Origin() ) < wsw::square( 20 ) ) {
		return;
	}

	// If the bot has not touched a ground this frame
	if( !entityPhysicsState.GroundEntity() && !context->frameEvents.hasJumped ) {
		return;
	}

	if( AiAasWorld::instance()->floorClusterNum( context->CurrGroundedAasAreaNum() ) != targetFloorCluster ) {
		return;
	}

	Debug( "The prediction step has lead to touching a ground in the target floor cluster" );
	context->isCompleted = true;
}
