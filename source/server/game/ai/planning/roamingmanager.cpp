#include "roamingmanager.h"
#include "../combat/tacticalspotsregistry.h"

// Cannot be defined in the header
BotRoamingManager::BotRoamingManager( Bot *bot_ )
	: bot( bot_ )
	, tacticalSpotsRegistry( TacticalSpotsRegistry::Instance() )
	, aasWorld( AiAasWorld::instance() ) {
	visitedAt = (int64_t *)Q_malloc( sizeof( int64_t ) * tacticalSpotsRegistry->numSpots );
	ClearVisitedSpots();
}

const Vec3 &BotRoamingManager::GetCachedRoamingSpot() {
	if( spotSelectedAt != level.time ) {
		cachedSpotOrigin.Set( GetRoamingSpot() );
		spotSelectedAt = level.time;
	}
	return cachedSpotOrigin;
}

void BotRoamingManager::CheckSpotsProximity() {
	DisableSpotsInRadius( bot->Origin(), 96.0f );
}

void BotRoamingManager::ClearVisitedSpots() {
	if( !tacticalSpotsRegistry->numSpots ) {
		AI_FailWith( "BotRoamingManager::ClearVisitedSpots()", "There is no tactical spots\n" );
	}
	memset( visitedAt, 0, sizeof( int64_t ) * tacticalSpotsRegistry->numSpots );
}

const Vec3 &BotRoamingManager::GetRoamingSpot() {
	const auto *routeCache = bot->RouteCache();

	// If the bot is using a tactical spot as a roaming spot
	if( currTacticalSpotNum >= 0 ) {
		// Check whether is has been reached since the last GetRoamingSpot() call
		if( IsTemporarilyDisabled( (unsigned)currTacticalSpotNum ) ) {
			currTacticalSpotNum = -1;
		} else {
			// Check whether it is still reachable
			int fromAreaNums[2] { 0, 0 };
			const int spotAreaNum = tacticalSpotsRegistry->spots[currTacticalSpotNum].aasAreaNum;
			const int numFromAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( fromAreaNums );
			if( !routeCache->FindRoute( fromAreaNums, numFromAreas, spotAreaNum, bot->TravelFlags() ) ) {
				currTacticalSpotNum = -1;
			}
		}
	}

	if( currTacticalSpotNum >= 0 ) {
		// Keep it as-is
		return tmpSpotOrigin;
	}

	int spotNum = TrySuggestTacticalSpot();
	if( spotNum >= 0 ) {
		tmpSpotOrigin.Set( tacticalSpotsRegistry->spots[spotNum].origin );
		currTacticalSpotNum = spotNum;
		return tmpSpotOrigin;
	}

	if( int areaNum = TrySuggestRandomAasArea() ) {
		return SetTmpSpotFromArea( areaNum );
	}

	if( int areaNum = TrySuggestNearbyAasArea() ) {
		return SetTmpSpotFromArea( areaNum );
	}

	tmpSpotOrigin.Set( -99999, -99999, -99999 );
	return tmpSpotOrigin;
}

inline const Vec3 &BotRoamingManager::SetTmpSpotFromArea( int areaNum ) {
	const auto &area = aasWorld->getAreas()[areaNum];
	tmpSpotOrigin.Set( area.center );
	tmpSpotOrigin.Z() = area.mins[2] + 8.0f;
	return tmpSpotOrigin;
}

int BotRoamingManager::TrySuggestTacticalSpot() {
	// Limit the number of tested for reachability spots.
	// It may lead to performance spikes otherwise.
	Candidates candidateSpots;

	const int64_t levelTime = level.time;
	const unsigned numSpots = tacticalSpotsRegistry->numSpots;
	const auto *spots = tacticalSpotsRegistry->spots;
	for( unsigned i = 0; i < candidateSpots.capacity(); ++i ) {
		uint16_t spotNum = (uint16_t)( brandom( 0.0f, numSpots - 0.1f ) );
		if( IsTemporarilyDisabled( spotNum, levelTime ) ) {
			continue;
		}

		const auto &spot = spots[spotNum];
		if( DistanceSquared( bot->Origin(), spot.origin ) < 384 * 384 ) {
			continue;
		}

		candidateSpots.push_back( spotNum );
	}

	if( candidateSpots.size() < candidateSpots.capacity() / 2 ) {
		// If spot states get reset, some spot is likely to be selected on next GetRoamingSpot() call
		TryResetAllSpotsDisabledState();
	}

	int fromAreas[2] { 0, 0 };
	const int numFromAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( fromAreas );
	return TryFindReachableSpot( candidateSpots, fromAreas, numFromAreas );
}

int BotRoamingManager::TryFindReachableSpot( const Candidates &candidateSpots, const int *fromAreaNums, int numFromAreas ) {
	for( const int spotNum: candidateSpots ) {
		const int spotAreaNum = tacticalSpotsRegistry->spots[spotNum].aasAreaNum;
		if( bot->routeCache->FindRoute( fromAreaNums, numFromAreas, spotAreaNum, bot->TravelFlags() ) ) {
			return spotNum;
		}
	}

	return -1;
}

int BotRoamingManager::TryFindReachableArea( const Candidates &candidateAreas, const int *fromAreaNums, int numFromAreas ) {
	for( const int areaNum: candidateAreas ) {
		if( bot->routeCache->FindRoute( fromAreaNums, numFromAreas, areaNum, bot->TravelFlags() ) ) {
			return areaNum;
		}
	}

	return 0;
}

int BotRoamingManager::TrySuggestRandomAasArea() {
	const int currAreaNum = bot->EntityPhysicsState()->CurrAasAreaNum();
	const int groundedAreaNum = bot->EntityPhysicsState()->DroppedToFloorAasAreaNum();

	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto numAreas = (int)aasAreas.size();

	Candidates candidateAreas;

	for( unsigned i = 0; i < candidateAreas.capacity(); ++i ) {
		int areaNum = (int)( brandom( 1.0f, numAreas - 0.1f ) );
		if( areaNum == currAreaNum ) {
			continue;
		}

		if( !IsFeasibleArea( aasAreas[areaNum], aasAreaSettings[areaNum] ) ) {
			continue;
		}

		if( areaNum == groundedAreaNum ) {
			continue;
		}

		candidateAreas.push_back( areaNum );
	}

	int fromAreaNums[] = { currAreaNum, groundedAreaNum };
	int numFromAreas = fromAreaNums[0] == fromAreaNums[1] ? 2 : 1;
	return TryFindReachableArea( candidateAreas, fromAreaNums, numFromAreas );
}

int BotRoamingManager::TrySuggestNearbyAasArea() {
	const Vec3 mins( Vec3( -192, -192, -128 ) + bot->Origin() );
	const Vec3 maxs( Vec3( +192, +192, +128 ) + bot->Origin() );

	int boxAreaNumsBuffer[64];
	const auto boxAreaNums = aasWorld->findAreasInBox( mins, maxs, boxAreaNumsBuffer, 64 );

	const int currAreaNum = bot->EntityPhysicsState()->CurrAasAreaNum();
	const int groundedAreaNum = bot->EntityPhysicsState()->DroppedToFloorAasAreaNum();

	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	Candidates candidateAreas;

	if( (int)candidateAreas.capacity() >= boxAreaNums.size() ) {
		for( const int areaNum : boxAreaNums ) {
			if( currAreaNum == areaNum ) {
				continue;
			}

			if( IsFeasibleArea( aasAreas[areaNum], aasAreaSettings[areaNum] ) ) {
				continue;
			}

			// Test it last (its unlikely to fail)
			if( groundedAreaNum == areaNum ) {
				continue;
			}

			candidateAreas.push_back( areaNum );
		}
	} else {
		for( const int areaNum : boxAreaNums ) {
			if( currAreaNum == areaNum ) {
				continue;
			}

			if( IsFeasibleArea( aasAreas[areaNum], aasAreaSettings[areaNum] ) ) {
				continue;
			}

			if( groundedAreaNum == areaNum ) {
				continue;
			}

			candidateAreas.push_back( areaNum );
			if( candidateAreas.full() ) {
				break;
			}
		}
	}

	int fromAreaNums[] = { currAreaNum, groundedAreaNum };
	int numFromAreas = fromAreaNums[0] == fromAreaNums[1] ? 1 : 2;
	return TryFindReachableArea( candidateAreas, fromAreaNums, numFromAreas );
}

bool BotRoamingManager::IsFeasibleArea( const aas_area_t &area, const aas_areasettings_t &areaSettings ) {
	if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
		return false;
	}

	if( areaSettings.areaflags & ( AREA_JUNK | AREA_DISABLED ) ) {
		return false;
	}

	if( areaSettings.contents & AREACONTENTS_DONOTENTER ) {
		return false;
	}

	if( area.maxs[0] - area.mins[0] < 24.0f || area.maxs[1] - area.mins[1] < 24.0f ) {
		return false;
	}

	Vec3 areaPoint( area.center );
	areaPoint.Z() = area.mins[2] + 16.0f;
	if( areaPoint.SquareDistanceTo( bot->Origin() ) <= 144.0f ) {
		return false;
	}

	return true;
}

void BotRoamingManager::TryResetAllSpotsDisabledState() {
	const int64_t levelTime = level.time;
	// Check whether there are unvisited/expired spots
	unsigned numEnabledSpots = 0;
	for( unsigned i = 0, end = tacticalSpotsRegistry->numSpots; i < end; ++i ) {
		if( !IsTemporarilyDisabled( i, levelTime ) ) {
			numEnabledSpots++;
			if( numEnabledSpots > 3 ) {
				return;
			}
		}
	}

	// If (almost) all spots are visited (and have not expired yet), make all spots available to visit
	ClearVisitedSpots();
}

void BotRoamingManager::DisableSpotsInRadius( const vec3_t origin, float radius ) {
	TacticalSpotsRegistry::OriginParams originParams( origin, game.edicts + bot->EntNum(), radius, bot->RouteCache() );
	uint16_t insideSpotNum = std::numeric_limits<uint16_t>::max();
	const auto &spotsFromQuery = tacticalSpotsRegistry->FindSpotsInRadius( originParams, &insideSpotNum );
	const int64_t levelTime = level.time;

	// Do not count spots behind a wall / an obstacle as visited.
	trace_t trace;
	for( const auto spotNum: spotsFromQuery  ) {
		if( spotNum != insideSpotNum ) {
			const auto &spot = tacticalSpotsRegistry->spots[spotNum];
			// TODO: Use PVS/AASArea vis?
			SolidWorldTrace( &trace, bot->Origin(), spot.origin );
			if( trace.fraction != 1.0f ) {
				continue;
			}
		}
		visitedAt[spotNum] = levelTime;
	}
}
