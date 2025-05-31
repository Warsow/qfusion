#include "userampexitscript.h"
#include "movementlocal.h"

bool UseRampExitScript::TryDeactivate( PredictionContext *context ) {
	// Call the superclass method first
	if( GenericGroundMovementScript::TryDeactivate( context ) ) {
		return true;
	}

	if( GenericGroundMovementScript::ShouldSkipTests( context ) ) {
		return false;
	}

	int areaNums[2] = { 0, 0 };
	const int numBotAreas = GetCurrBotAreas( areaNums, context );
	for( int i = 0; i < numBotAreas; ++i ) {
		const int areaNum = areaNums[i];
		// The bot has entered the exit area (make sure this condition is first)
		if( areaNum == exitAreaNum ) {
			status = COMPLETED;
			return true;
		}
		// The bot is still on the same ramp area
		if( areaNum == rampAreaNum ) {
			assert( status == PENDING );
			return false;
		}
	}

	// The bot is neither on the ramp nor in the ramp exit area
	status = INVALID;
	return true;
}

const int *TryFindBestInclinedFloorExitArea( PredictionContext *context, int rampAreaNum, int forbiddenAreaNum ) {
	const auto *aasWorld = AiAasWorld::instance();
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto aasReach = aasWorld->getReaches();

	// Find ramp start and end flat grounded areas

	int lowestAreaNum = 0;
	int lowestReachNum = 0;
	float lowestAreaHeight = std::numeric_limits<float>::max();
	int highestAreaNum = 0;
	int highestReachNum = 0;
	float highestAreaHeight = std::numeric_limits<float>::lowest();

	const auto &rampAreaSettings = aasAreaSettings[rampAreaNum];
	int reachNum = rampAreaSettings.firstreachablearea;
	const int endReachNum = reachNum + rampAreaSettings.numreachableareas;
	for(; reachNum != endReachNum; ++reachNum) {
		const auto &reach = aasReach[reachNum];
		if( reach.traveltype != TRAVEL_WALK ) {
			continue;
		}
		const int reachAreaNum = reach.areanum;
		if( reach.areanum == forbiddenAreaNum ) {
			continue;
		}

		const auto &reachAreaFlags = aasAreaSettings[reachAreaNum].areaflags;
		if( !( reachAreaFlags & AREA_GROUNDED ) ) {
			continue;
		}

		const auto &reachArea = aasAreas[reachAreaNum];
		if( reachArea.mins[2] < lowestAreaHeight ) {
			lowestAreaHeight = reachArea.mins[2];
			lowestAreaNum = reachAreaNum;
			lowestReachNum = reachNum;
		}
		if( reachArea.mins[2] > highestAreaHeight ) {
			highestAreaHeight = reachArea.mins[2];
			highestAreaNum = reachAreaNum;
			highestReachNum = reachNum;
		}
	}

	if( !lowestAreaNum || !highestAreaNum ) {
		return nullptr;
	}

	// Note: The comparison operator has been changed from >= to >
	// since adjacent ramp areas are likely to have the same bounding box height dimensions
	if( lowestAreaHeight > highestAreaHeight ) {
		return nullptr;
	}

	const int travelTimeToTarget = context->TravelTimeToNavTarget();
	if( !travelTimeToTarget ) {
		return nullptr;
	}

	// Find what area is closer to the nav target
	int fromAreaNums[2] = { lowestAreaNum, highestAreaNum };
	int fromReachNums[2] = { lowestReachNum, highestReachNum };
	int toAreaNum = context->NavTargetAasAreaNum();
	int bestIndex = -1;
	int bestTravelTime = std::numeric_limits<int>::max();
	const auto *routeCache = context->RouteCache();
	const int travelFlags = context->TravelFlags();
	for( int i = 0; i < 2; ++i ) {
		int travelTime = routeCache->FindRoute( fromAreaNums[i], toAreaNum, travelFlags );
		if( travelTime && travelTime < travelTimeToTarget && travelTime < bestTravelTime ) {
			bestIndex = i;
			bestTravelTime = travelTime;
		}
	}

	if( bestIndex < 0 ) {
		return nullptr;
	}

	// Return a pointer to a persistent during the match memory
	return &aasReach[fromReachNums[bestIndex]].areanum;
}

MovementScript *FallbackAction::TryFindRampFallback( PredictionContext *context, int rampAreaNum, int forbiddenAreaNum ) {
	const int *bestExitAreaNum = TryFindBestInclinedFloorExitArea( context, rampAreaNum, forbiddenAreaNum );
	if( !bestExitAreaNum ) {
		return nullptr;
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto &exitArea = aasWorld->getAreas()[*bestExitAreaNum];

	Vec3 areaPoint( exitArea.center );
	areaPoint.Z() = exitArea.mins[2] + 1.0f - playerbox_stand_mins[2];

	auto *script = &m_subsystem->useRampExitScript;
	script->Activate( rampAreaNum, *bestExitAreaNum );
	return script;
}