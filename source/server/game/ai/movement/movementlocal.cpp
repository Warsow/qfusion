#include "movementlocal.h"
#include "../manager.h"
#include "../classifiedentitiescache.h"



static const float kTopNodeCacheAddToMins[] = { -56, -56, -24 };
static const float kTopNodeCacheAddToMaxs[] = { +56, +56, +24 };

CollisionTopNodeCache::CollisionTopNodeCache() noexcept
	: defaultBoundsCache( "TopNodeCache", kTopNodeCacheAddToMins, kTopNodeCacheAddToMaxs )
	, zeroStepBoundsCache( nullptr, kTopNodeCacheAddToMins, kTopNodeCacheAddToMaxs ) {}

int CollisionTopNodeCache::getTopNode( const float *absMins, const float *absMaxs, bool isZeroStep ) const {
	// Put the likely case first
	if( !isZeroStep ) {
		if( defaultBoundsCache.checkOrUpdateBounds( absMins, absMaxs ) ) {
			return *defaultCachedNode;
		}

		const auto [cachedMins, cachedMaxs] = defaultBoundsCache.getCachedBounds();
		defaultCachedNode = SV_FindTopNodeForBox( cachedMins, cachedMaxs );
		return *defaultCachedNode;
	}

	// Take switching to another bot into account. That's why the bounds test is primarily needed.
	if( !zeroStepBoundsCache.checkOrUpdateBounds( absMins, absMaxs ) ) {
		// zeroStepBoundsCache is updated first so mins/maxs are always valid
		const auto [cachedMins, cachedMaxs] = zeroStepBoundsCache.getCachedBounds();
		cachedZeroStepNode = SV_FindTopNodeForBox( cachedMins, cachedMaxs );
	}

	// Cached bounds initially are illegal so this value gets set on a first checkOnUpdateBounds() call.
	assert( cachedZeroStepNode.has_value() );
	defaultCachedNode = *cachedZeroStepNode;
	// Force an update of the primary bounds cache
	defaultBoundsCache.setFrom( zeroStepBoundsCache );
	return *cachedZeroStepNode;
}

CollisionTopNodeCache collisionTopNodeCache;

static const float kShapesListCacheAddToMins[] = { -64, -64, -32 };
static const float kShapesListCacheAddToMaxs[] = { +64, +64, +32 };

CollisionShapesListCache::CollisionShapesListCache() noexcept
	: defaultBoundsCache( "ShapesListCache", kShapesListCacheAddToMins, kShapesListCacheAddToMaxs )
	, zeroStepBoundsCache( nullptr, kShapesListCacheAddToMins, kShapesListCacheAddToMaxs ) {}

CollisionShapesListCache::~CollisionShapesListCache() {
	SV_FreeShapeList( defaultCachedList );
	SV_FreeShapeList( defaultClippedList );
	SV_FreeShapeList( zeroStepCachedList );
	SV_FreeShapeList( zeroStepClippedList );
}

CollisionShapesListCache shapesListCache;

constexpr auto kListClipMask = MASK_PLAYERSOLID | MASK_WATER | CONTENTS_TRIGGER | CONTENTS_JUMPPAD | CONTENTS_TELEPORTER;

const CMShapeList *CollisionShapesListCache::prepareList( const float *mins, const float *maxs, bool isZeroStep ) const {
	// Put the likely case first
	if( !isZeroStep ) {
		return defaultPrepareList( mins, maxs );
	}

	if( zeroStepBoundsCache.checkOrUpdateBounds( mins, maxs ) ) {
		activeCachedList = zeroStepCachedList;
		defaultBoundsCache.setFrom( zeroStepBoundsCache );
		return zeroStepClippedList;
	}

	if( !defaultCachedList ) {
		defaultCachedList = SV_AllocShapeList();
		defaultClippedList = SV_AllocShapeList();
		zeroStepCachedList = SV_AllocShapeList();
		zeroStepClippedList = SV_AllocShapeList();
	}

	const auto [cachedMins, cachedMaxs] = zeroStepBoundsCache.getCachedBounds();
	activeCachedList = SV_BuildShapeList( zeroStepCachedList, cachedMins, cachedMaxs, kListClipMask );
	SV_ClipShapeList( zeroStepClippedList, zeroStepCachedList, mins, maxs );

	defaultBoundsCache.setFrom( zeroStepBoundsCache );
	return zeroStepClippedList;
}

const CMShapeList *CollisionShapesListCache::defaultPrepareList( const float *mins, const float *maxs ) const {
	if( defaultBoundsCache.checkOrUpdateBounds( mins, maxs ) ) {
		assert( activeCachedList == defaultCachedList || activeCachedList == zeroStepCachedList );
		SV_ClipShapeList( defaultClippedList, activeCachedList, mins, maxs );
		return defaultClippedList;
	}

	const auto [cachedMins, cachedMaxs] = defaultBoundsCache.getCachedBounds();
	activeCachedList = SV_BuildShapeList( defaultCachedList, cachedMins, cachedMaxs, kListClipMask );
	SV_ClipShapeList( defaultClippedList, activeCachedList, mins, maxs );
	return defaultClippedList;
}

bool ReachChainWalker::Exec() {
	assert( targetAreaNum >= 0 );
	assert( numStartAreas >= 0 );

	assert( targetAreaNum >= 0 );
	assert( numStartAreas >= 0 );

	lastAreaNum    = 0;
	startAreaNum   = 0;
	lastReachNum   = 0;
	startReachNum  = 0;
	lastTravelTime = 0;

	// We have to handle the first reach. separately as we start from multiple alternative areas.
	for( int i = 0; i < numStartAreas; ++i ) {
		const int testedStartAreaNum = startAreaNums[i];
		int reachNum                 = 0;
		if( const int travelTime = routeCache->FindRoute( testedStartAreaNum, targetAreaNum, travelFlags, &reachNum ) ) {
			if( !lastTravelTime || travelTime < lastTravelTime ) {
				lastAreaNum    = testedStartAreaNum;
				startAreaNum   = testedStartAreaNum;
				lastReachNum   = reachNum;
				startReachNum  = reachNum;
				lastTravelTime = travelTime;
			}
		}
	}

	if( !lastTravelTime ) {
		return false;
	}

	const auto *const aasWorld = AiAasWorld::instance();
	const auto aasReach = aasWorld->getReaches();

	assert( (unsigned)lastReachNum < (unsigned)aasReach.size() );
	if( !Accept( lastReachNum, aasReach[lastReachNum], lastTravelTime ) ) {
		return true;
	}

	int areaNum = aasReach[lastReachNum].areanum;
	while( areaNum != targetAreaNum ) {
		lastTravelTime = routeCache->FindRoute( areaNum, targetAreaNum, travelFlags, &lastReachNum );
		if( !lastTravelTime ) {
			return false;
		}
		lastAreaNum = areaNum;
		assert( (unsigned)lastReachNum < (unsigned)aasReach.size() );
		const auto &reach = aasReach[lastReachNum];
		if( !Accept( lastReachNum, reach, lastTravelTime ) ) {
			return true;
		}
		areaNum = reach.areanum;
	}

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

bool TraceArcInSolidWorld( const vec3_t from, const vec3_t to ) {
	const auto brushMask = MASK_WATER | MASK_SOLID;
	trace_t trace;

	Vec3 midPoint( to );
	midPoint += from;
	midPoint *= 0.5f;

	// Lets figure out deltaZ making an assumption that all forward momentum is converted to the direction to the point
	// Note that we got rid of idea making these tests depending of a current AI entity physics state due to flicker issues.

	const float squareDistanceToMidPoint = wsw::square( from[0] - midPoint.x() ) + wsw::square( from[1] - midPoint.y() );
	if( squareDistanceToMidPoint < wsw::square( 32 ) ) {
		StaticWorldTrace( &trace, from, to, brushMask );
		return trace.fraction == 1.0f;
	}

	// Assume a default ground movement speed
	const float timeToMidPoint = Q_Sqrt( squareDistanceToMidPoint ) * Q_Rcp( GS_DefaultPlayerSpeed( *ggs ) );
	// Assume an almost default jump speed
	float deltaZ = ( 0.75f * DEFAULT_JUMPSPEED ) * timeToMidPoint;
	deltaZ -= 0.5f * level.gravity * ( timeToMidPoint * timeToMidPoint );

	// Does not worth making an arc
	// Note that we ignore negative deltaZ since the real trajectory differs anyway
	if( deltaZ < 2.0f ) {
		StaticWorldTrace( &trace, from, to, brushMask );
		return trace.fraction == 1.0f;
	}

	midPoint.z() += deltaZ;

	StaticWorldTrace( &trace, from, midPoint.data(), brushMask );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	StaticWorldTrace( &trace, midPoint.data(), to, brushMask );
	return trace.fraction == 1.0f;
}

void DirToKeyInput( const Vec3 &desiredDir, const vec3_t actualForwardDir, const vec3_t actualRightDir, BotInput *input ) {
	input->ClearMovementDirections();

	float dotForward = desiredDir.dot( actualForwardDir );
	if( dotForward > 0.3 ) {
		input->SetForwardMovement( 1 );
	} else if( dotForward < -0.3 ) {
		input->SetForwardMovement( -1 );
	}

	float dotRight = desiredDir.dot( actualRightDir );
	if( dotRight > 0.3 ) {
		input->SetRightMovement( 1 );
	} else if( dotRight < -0.3 ) {
		input->SetRightMovement( -1 );
	}

	// Prevent being blocked
	if( !input->ForwardMovement() && !input->RightMovement() ) {
		input->SetForwardMovement( 1 );
	}
}