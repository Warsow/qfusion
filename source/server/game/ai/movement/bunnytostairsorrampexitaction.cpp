#include "movementlocal.h"
#include "bunnytostairsorrampexitaction.h"

auto BunnyToStairsOrRampExitAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( const auto result = genericCheckIsActionEnabled( context ); result != PredictionResult::Continue ) {
		return result;
	}

	if( const auto result = checkCommonBunnyHopPreconditions( context ); result != PredictionResult::Continue ) {
		return result;
	}

	if( !m_intendedLookDir ) {
		if( !tryFindingAndSavingLookDir( context ) ) {
			return PredictionResult::Abort;
		}
	}

	if( !setupBunnyHopping( Vec3( m_intendedLookDir ), context ) ) {
		return PredictionResult::Abort;
	}

	return PredictionResult::Continue;
}

bool BunnyToStairsOrRampExitAction::tryFindingAndSavingLookDir( PredictionContext *context ) {
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
		m_lookDirStorage.set( aasWorld->getAreas()[*exitAreaNum].center );
		m_lookDirStorage -= context->movementState->entityPhysicsState.Origin();
		if( !m_lookDirStorage.normalize() ) {
			return false;
		}

		m_intendedLookDir = m_lookDirStorage.data();

		trySavingExitFloorCluster( context, *exitAreaNum );
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
	m_lookDirStorage.set( aasWorld->getAreas()[*exitAreaNum].center );
	m_lookDirStorage -= context->movementState->entityPhysicsState.Origin();
	if( !m_lookDirStorage.normalize() ) {
		return false;
	}

	m_intendedLookDir = m_lookDirStorage.data();

	// Try find an area that is a boundary area of the exit area and is in a floor cluster
	trySavingExitFloorCluster( context, *exitAreaNum );
	return true;
}

void BunnyToStairsOrRampExitAction::trySavingExitFloorCluster( PredictionContext *context, int exitAreaNum ) {
	const auto *const aasWorld = AiAasWorld::instance();
	const auto aasReach = aasWorld->getReaches();
	const auto aasFloorClusterNums = aasWorld->areaFloorClusterNums();
	const auto *const routeCache = context->RouteCache();

	// Check whether exit area is already in cluster
	m_targetFloorCluster = aasFloorClusterNums[exitAreaNum];
	if( m_targetFloorCluster ) {
		return;
	}

	const int targetAreaNum = context->NavTargetAasAreaNum();

	int areaNum = exitAreaNum;
	while( areaNum != targetAreaNum ) {
		int reachNum;
		if( !routeCache->FindRoute( areaNum, targetAreaNum, m_bot->TravelFlags(), &reachNum ) ) {
			break;
		}
		const auto &reach = aasReach[reachNum];
		const int travelType = reach.traveltype & TRAVELTYPE_MASK;
		if( travelType != TRAVEL_WALK ) {
			break;
		}
		const int nextAreaNum = reach.areanum;
		m_targetFloorCluster = aasFloorClusterNums[nextAreaNum];
		if( m_targetFloorCluster ) {
			break;
		}
		areaNum = nextAreaNum;
	}
}

auto BunnyToStairsOrRampExitAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	// We skip the direct superclass method call!
	// Much more lenient checks are used for this specialized action.
	// Only generic checks for all movement actions should be performed in addition.
	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		return result;
	}

	// There is no target floor cluster saved
	if( !m_targetFloorCluster ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	// Make sure we don't stop prediction at start.
	// The distance threshold is low due to troublesome movement in these kinds of areas.
	if( m_originAtSequenceStart.squareDistance2DTo( entityPhysicsState.Origin() ) < wsw::square( 20 ) ) {
		return PredictionResult::Continue;
	}

	// If the bot has not touched a ground this frame
	if( !entityPhysicsState.GroundEntity() && !context->frameEvents.hasJumped ) {
		return PredictionResult::Continue;
	}

	if( AiAasWorld::instance()->floorClusterNum( context->CurrGroundedAasAreaNum() ) != m_targetFloorCluster ) {
		return PredictionResult::Continue;
	}

	Debug( "The prediction step has lead to touching a ground in the target floor cluster" );
	return PredictionResult::Complete;
}
