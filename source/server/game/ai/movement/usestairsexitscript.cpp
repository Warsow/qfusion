#include "usestairsexitscript.h"
#include "movementlocal.h"

bool UseStairsExitScript::TryDeactivate( PredictionContext *context ) {
	// Call the superclass method first
	if( GenericGroundMovementScript::TryDeactivate( context ) ) {
		return true;
	}

	if( GenericGroundMovementScript::ShouldSkipTests( context ) ) {
		return false;
	}

	const auto aasAreaStairsClusterNums = AiAasWorld::instance()->areaStairsClusterNums();

	int areaNums[2] = { 0, 0 };
	int numBotAreas = GetCurrBotAreas( areaNums );
	for( int i = 0; i < numBotAreas; ++i ) {
		const int areaNum = areaNums[i];
		// The bot has entered the exit area (make sure this condition is first)
		if( areaNum == this->exitAreaNum ) {
			status = COMPLETED;
			return true;
		}
		// The bot is still in the same stairs cluster
		if( aasAreaStairsClusterNums[areaNum] == stairsClusterNum ) {
			assert( status == PENDING );
			return false;
		}
	}

	// The bot is neither in the same stairs cluster nor in the cluster exit area
	status = INVALID;
	return true;
}

const uint16_t *TryFindBestStairsExitArea( PredictionContext *context, int stairsClusterNum ) {
	const int toAreaNum = context->NavTargetAasAreaNum();
	if( !toAreaNum ) {
		return nullptr;
	}

	const int currTravelTimeToTarget = context->TravelTimeToNavTarget();
	if( !currTravelTimeToTarget ) {
		return nullptr;
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto *routeCache = context->RouteCache();

	const std::span<const uint16_t> stairsClusterAreaNums = aasWorld->stairsClusterData( stairsClusterNum );

	// TODO: Support curved stairs, here and from StairsClusterBuilder side

	// Determine whether highest or lowest area is closer to the nav target
	const uint16_t *stairsBoundaryAreas[2];
	stairsBoundaryAreas[0] = std::addressof( stairsClusterAreaNums.front() );
	stairsBoundaryAreas[1] = std::addressof( stairsClusterAreaNums.back() );

	int bestStairsAreaIndex = -1;
	int bestTravelTimeOfStairsAreas = std::numeric_limits<int>::max();
	for( int i = 0; i < 2; ++i ) {
		// TODO: Eliminate the intermediate bestAreaTravelTime variable (this is a result of unrelated refactoring)
		int bestAreaTravelTime = std::numeric_limits<int>::max();
		int travelTime = routeCache->FindRoute( *stairsBoundaryAreas[i], toAreaNum, context->TravelFlags() );
		if( travelTime && travelTime < bestAreaTravelTime ) {
			bestAreaTravelTime = travelTime;
		}
		// The stairs boundary area is not reachable
		if( bestAreaTravelTime == std::numeric_limits<int>::max() ) {
			return nullptr;
		}
		// Make sure a stairs area is closer to the nav target than the current one
		if( bestAreaTravelTime < currTravelTimeToTarget ) {
			if( bestAreaTravelTime < bestTravelTimeOfStairsAreas ) {
				bestTravelTimeOfStairsAreas = bestAreaTravelTime;
				bestStairsAreaIndex = i;
			}
		}
	}

	if( bestStairsAreaIndex < 0 ) {
		return nullptr;
	}

	// The value points to the cluster data that is persistent in memory
	// during the entire match, so returning this address is legal.
	return stairsBoundaryAreas[bestStairsAreaIndex];
}

MovementScript *FallbackAction::TryFindStairsFallback( PredictionContext *context ) {
	const auto *aasWorld = AiAasWorld::instance();

	int currAreaNums[2] = { 0, 0 };
	const int numCurrAreas = context->movementState->entityPhysicsState.PrepareRoutingStartAreas( currAreaNums );

	int stairsClusterNum = 0;
	for( int i = 0; i < numCurrAreas; ++i ) {
		if( ( stairsClusterNum = aasWorld->stairsClusterNum( currAreaNums[i] ) ) ) {
			break;
		}
	}

	if( !stairsClusterNum ) {
		return nullptr;
	}

	const auto *bestAreaNum = TryFindBestStairsExitArea( context, stairsClusterNum );
	if( !bestAreaNum ) {
		return nullptr;
	}

	// Note: Don't try to apply jumping shortcut, results are very poor.

	auto *script = &m_subsystem->useStairsExitScript;
	script->Activate( stairsClusterNum, *bestAreaNum );
	return script;
}