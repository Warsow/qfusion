#include "floorclusterareascache.h"
#include "movementlocal.h"
#include "../combat/tacticalspotsregistry.h"
#include <common/helpers/algorithm.h>

bool FloorClusterAreasCache::AreaPassesCollisionTest( PredictionContext *context, int areaNum ) const {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	Vec3 start( entityPhysicsState.Origin() );
	if( entityPhysicsState.GroundEntity() ) {
		start.z() += 3.0f;
	}

	vec3_t mins, maxs;
	TacticalSpotsRegistry::GetSpotsWalkabilityTraceBounds( mins, maxs );
	return AreaPassesCollisionTest( start, areaNum, mins, maxs );
}

bool SameFloorClusterAreasCache::AreaPassesCollisionTest( const Vec3 &start,
														  int areaNum,
														  const vec3_t mins,
														  const vec3_t maxs ) const {
	const auto &area = aasWorld->getAreas()[areaNum];
	Vec3 areaPoint( area.center );
	areaPoint.z() = area.mins[2] + 1.0f + ( -playerbox_stand_mins[2] );

	// We deliberately have to check against entities, like the tank on wbomb1 A spot, and not only solid world
	trace_t trace;
	float *start_ = const_cast<float *>( start.data() );
	float *mins_ = const_cast<float *>( mins );
	float *maxs_ = const_cast<float *>( maxs );
	G_Trace( &trace, start_, mins_, maxs_, areaPoint.data(), game.edicts + bot->EntNum(), MASK_AISOLID );
	return trace.fraction == 1.0f;
}

bool NextFloorClusterAreasCache::AreaPassesCollisionTest( const Vec3 &start,
														  int areaNum,
														  const vec3_t mins,
														  const vec3_t maxs ) const {
	const auto &area = aasWorld->getAreas()[areaNum];
	Vec3 areaPoint( area.center );
	areaPoint.z() = area.mins[2] + 1.0f + ( -playerbox_stand_mins[2] );

	trace_t trace;
	// We cannot ensure walkability with the same degree of certainty as for the same floor cluster.
	// Use a cheaper collision test only against the solid world.
	SolidWorldTrace( &trace, start.data(), areaPoint.data(), mins, maxs );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	// Once again, there are no cheap ways to determine whether there is a gap in-between.
	// We are in different floor clusters.
	// This heuristic however should work well.
	// False positives are going to be cut off by movement prediction system.
	// There are no callers that use this cache without prediction.

	// Can't use the hack below if the tested area has greater elevation.
	if( start.z() + playerbox_stand_mins[2] < area.mins[2] ) {
		return true;
	}

	areaPoint.z() -= 96.0f;
	// Use a segment instead of a box but check for liquids
	StaticWorldTrace( &trace, start.data(), areaPoint.data(), CONTENTS_SOLID | CONTENTS_WATER );
	// If there is a gap in-between the trace is very likely to hit solid below the area floor.
	return trace.fraction != 1.0f && trace.endpos[2] >= area.mins[2];
}

bool SameFloorClusterAreasCache::NeedsToBeComputed( PredictionContext *context ) const {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const auto floorClusterNums = aasWorld->areaFloorClusterNums();

	if( !computedTargetAreaNum ) {
		return true;
	}

	if( floorClusterNums[context->CurrGroundedAasAreaNum()] != floorClusterNums[computedTargetAreaNum] ) {
		return true;
	}

	if( computedTargetAreaPoint.squareDistanceTo( entityPhysicsState.Origin() ) < wsw::square( REACHABILITY_RADIUS ) ) {
		return true;
	}

	// Walkability tests in cluster are cheap but sometimes produce false negatives,
	// so do not check for walkability in the first second to prevent choice jitter
	if( level.time - computedAt > 1000 ) {
		if( !aasWorld->isAreaWalkableInFloorCluster( context->CurrGroundedAasAreaNum(), computedTargetAreaNum ) ) {
			return true;
		}
	}

	return !FloorClusterAreasCache::AreaPassesCollisionTest( context, computedTargetAreaNum );
}

int FloorClusterAreasCache::GetClosestToTargetPoint( PredictionContext *context, float *resultPoint, int *resultAreaNum ) const {
	// We have switched to using a cached value as far as it is feasible
	// avoiding computing an actual point almost every frame
	// (it has proven to cause jitter/looping)

	// Check whether an old value is present and is feasible
	if( NeedsToBeComputed( context ) ) {
		computedTargetAreaNum = 0;
		computedTargetAreaPoint.set( 0, 0, 0 );
		if( ( computedTravelTime = FindClosestToTargetPoint( context, &computedTargetAreaNum ) ) ) {
			computedAt = level.time;
			const auto &area = aasWorld->getAreas()[computedTargetAreaNum];
			computedTargetAreaPoint.set( area.center );
			computedTargetAreaPoint.z() = area.mins[2] + ( -playerbox_stand_mins[2] );
		}
	}

	if( computedTravelTime ) {
		if( resultAreaNum ) {
			*resultAreaNum = computedTargetAreaNum;
		}
		if( resultPoint ) {
			computedTargetAreaPoint.copyTo( resultPoint );
		}
		return computedTravelTime;
	}

	return 0;
}

void FloorClusterAreasCache::TryReusingOldHeap( PredictionContext *context,
												int currGroundedAreaNum,
												int expectedClusterNum,
												CandidateAreasHeap &scratchpadHeap ) const {
	scratchpadHeap.clear();
	// If the cached results were computed for specified cluster
	if( aasWorld->areaFloorClusterNums()[computedTargetAreaNum] == expectedClusterNum ) {
		// If we're still in the same area and there are cached results
		if( currGroundedAreaNum == computedForAreaNum && !oldCandidatesHeap.empty() ) {
			// Populate the current heap (that is going to be modified) by backed heap values
			for( const auto &heapElem: oldCandidatesHeap ) {
				scratchpadHeap.push_back( heapElem );
			}
		}
		return;
	}

	oldCandidatesHeap.clear();

	computedForAreaNum = currGroundedAreaNum;
	const auto maxTravelTimeThreshold = context->TravelTimeToNavTarget();
	// Build new areas heap for the new flood start area
	const auto clusterAreaNums = aasWorld->floorClusterData( expectedClusterNum );
	// The number of areas in the cluster areas list prepends the first area num
	BuildCandidateAreasHeap( context, maxTravelTimeThreshold, clusterAreaNums, scratchpadHeap );

	// Save the heap
	for( const auto &heapElem: scratchpadHeap ) {
		oldCandidatesHeap.push_back( heapElem );
	}
}

int SameFloorClusterAreasCache::FindClosestToTargetPoint( PredictionContext *context, int *resultAreaNum ) const {
	const int currGroundedAreaNum = context->CurrGroundedAasAreaNum();
	if( !currGroundedAreaNum ) {
		return false;
	}

	CandidateAreasHeap candidateAreasHeap;
	const int currClusterNum = aasWorld->areaFloorClusterNums()[currGroundedAreaNum];
	this->TryReusingOldHeap( context, currGroundedAreaNum, currClusterNum, candidateAreasHeap );

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	vec3_t traceMins, traceMaxs;
	TacticalSpotsRegistry::GetSpotsWalkabilityTraceBounds( traceMins, traceMaxs );
	Vec3 start( entityPhysicsState.Origin() );
	if( entityPhysicsState.GroundEntity() ) {
		start.z() += 1.0f;
	}

	while( !candidateAreasHeap.empty() ) {
		wsw::pop_heap( candidateAreasHeap.begin(), candidateAreasHeap.end() );
		int areaNum = candidateAreasHeap.back().areaNum;
		int travelTime = (int)( -candidateAreasHeap.back().score );
		candidateAreasHeap.pop_back();

		if( !aasWorld->isAreaWalkableInFloorCluster( currGroundedAreaNum, areaNum ) ) {
			continue;
		}

		// We hope we have done all possible cutoffs at this moment of execution.
		// We still need this collision test since cutoffs are performed using thin rays.
		// This test is expensive that's why we try to defer it as far at it is possible.
		if( !AreaPassesCollisionTest( start, areaNum, traceMins, traceMaxs ) ) {
			continue;
		}

		// Stop on the first (and best since a heap is used) feasible area
		if( resultAreaNum ) {
			*resultAreaNum = areaNum;
		}
		return travelTime;
	}

	return 0;
}

void FloorClusterAreasCache::PrepareAreasForSmallCluster( PredictionContext *__restrict context,
														  const Hazard *__restrict hazardToEvade,
														  int maxTravelTimeThreshold,
														  std::span<const uint16_t> clusterAreaNums,
														  CandidateAreasHeap &__restrict result ) const {
	// Prevent misusing
	assert( result.empty() && result.capacity() >= clusterAreaNums.size() );

	const auto aasAreas = aasWorld->getAreas();
	const auto *__restrict routeCache = bot->RouteCache();
	const float *__restrict botOrigin = context->movementState->entityPhysicsState.Origin();
	const int toAreaNum = context->NavTargetAasAreaNum();
	const float zOffset = 1.0f - playerbox_stand_mins[2];

	const float squareNearThreshold = areaSelectionNearThreshold * areaSelectionNearThreshold;
	const float squareFarThreshold = areaSelectionFarThreshold * areaSelectionFarThreshold;

	for( const int areaNum : clusterAreaNums ) {
		const auto &area = aasAreas[areaNum];
		Vec3 areaPoint( area.center );
		areaPoint.z() = area.mins[2] + zOffset;

		const float squareDistance = areaPoint.squareDistanceTo( botOrigin );
		if( squareDistance < squareNearThreshold || squareDistance > squareFarThreshold ) {
			continue;
		}

		// Putting this in loop is not that expensive as it's remains the same and the branch gets predicted well
		if( hazardToEvade ) {
			if( hazardToEvade->HasImpactOnPoint( areaPoint ) ) {
				continue;
			}
		}

		const int areaTime = routeCache->FindRoute( areaNum, toAreaNum, bot->TravelFlags() );
		if( !areaTime || areaTime >= maxTravelTimeThreshold ) {
			continue;
		}

		assert( !result.full() );
		// Note: negate the travel time here so closest to target areas are evicted first from the max heap
		new( result.unsafe_grow_back() )AreaAndScore( areaNum, -areaTime );
	}
}

void FloorClusterAreasCache::PrepareAreasForLargeCluster( PredictionContext *__restrict context,
														  const Hazard *__restrict hazardToEvade,
														  int maxTravelTimeThreshold,
														  std::span<const uint16_t> clusterAreaNums,
														  CandidateAreasHeap &__restrict result ) const {
	// Prevent misusing
	assert( result.empty() && result.capacity() < clusterAreaNums.size() );

	const auto aasAreas = aasWorld->getAreas();
	const auto *__restrict routeCache = bot->RouteCache();
	const float *__restrict botOrigin = context->movementState->entityPhysicsState.Origin();
	const float zOffset = 1.0f - playerbox_stand_mins[2];

	const float squareNearThreshold = areaSelectionNearThreshold * areaSelectionNearThreshold;
	const float squareFarThreshold = areaSelectionFarThreshold * areaSelectionFarThreshold;

	// First cut off as many areas as possible.
	// AreAreasInPVS() call is not that cheap as we expected.
	CandidateAreasHeap distanceHeap;
	for( const int areaNum : clusterAreaNums ) {
		const auto &area = aasAreas[areaNum];
		Vec3 areaPoint( area.center );
		areaPoint.z() = area.mins[2] + zOffset;

		const float squareDistance = areaPoint.squareDistanceTo( botOrigin );
		if( squareDistance < squareNearThreshold || squareDistance > squareFarThreshold ) {
			continue;
		}

		// Putting this in loop is not that expensive as it's remains the same and the branch gets predicted well
		if( hazardToEvade ) {
			if( hazardToEvade->HasImpactOnPoint( areaPoint ) ) {
				continue;
			}
		}

		// Usually best areas are farther from the bot.
		// Unfortunately worst areas are too.
		// As we have to cut off areas, let closest areas retain in the max-heap.
		const float score = squareDistance;

		if( distanceHeap.full() ) {
			// Evict the farthest area
			wsw::pop_heap( distanceHeap.begin(), distanceHeap.end() );
			distanceHeap.pop_back();
		}

		new( distanceHeap.unsafe_grow_back() )AreaAndScore( areaNum, score );
		wsw::push_heap( distanceHeap.begin(), distanceHeap.end() );
	}

	const int toAreaNum = context->NavTargetAasAreaNum();
	for( const auto &areaAndScore: distanceHeap ) {
		const auto areaNum = areaAndScore.areaNum;
		const int areaTime = routeCache->FindRoute( areaNum, toAreaNum, bot->TravelFlags() );
		if( !areaTime || areaTime >= maxTravelTimeThreshold ) {
			continue;
		}

		assert( !result.full() );
		// Note: negate the travel time here so closest to target areas are evicted first from the max heap
		new( result.unsafe_grow_back() )AreaAndScore( areaNum, -areaTime );
	}
}

void FloorClusterAreasCache::BuildCandidateAreasHeap( PredictionContext *context,
													  int maxTravelTimeThreshold,
													  std::span<const uint16_t> clusterAreaNums,
													  CandidateAreasHeap &result ) const {
	result.clear();

	const auto *hazard = bot->PrimaryHazard();
	// Reduce branching in the loop below
	if( bot->ShouldRushHeadless() || ( hazard && !hazard->SupportsImpactTests() ) ) {
		hazard = nullptr;
	}
	if( clusterAreaNums.size() <= HEAP_SIZE ) {
		PrepareAreasForSmallCluster( context, hazard, maxTravelTimeThreshold, clusterAreaNums, result );
	} else {
		PrepareAreasForLargeCluster( context, hazard, maxTravelTimeThreshold, clusterAreaNums, result );
	}

	// Candidates written in `result` are not arranged in a heap (but must have feasible scores)
	wsw::make_heap( result.begin(), result.end() );
}

bool NextFloorClusterAreasCache::NeedsToBeComputed( PredictionContext *context ) const {
	const auto floorClusterNums = aasWorld->areaFloorClusterNums();

	// There were no data computed
	if( !computedTargetAreaNum ) {
		return true;
	}

	// We have reached the cluster of the result
	if( floorClusterNums[context->CurrGroundedAasAreaNum()] == floorClusterNums[computedTargetAreaNum] ) {
		return true;
	}

	return !FloorClusterAreasCache::AreaPassesCollisionTest( context, computedTargetAreaNum );
}

int NextFloorClusterAreasCache::FindClosestToTargetPoint( PredictionContext *context, int *resultAreaNum ) const {
	int currGroundedAreaNum = context->CurrGroundedAasAreaNum();
	if( !currGroundedAreaNum ) {
		return false;
	}

	const auto *const __restrict routeCache = bot->RouteCache();
	const auto aasReaches = aasWorld->getReaches();
	const auto floorClusterNums = aasWorld->areaFloorClusterNums();

	// We try to find a number of next floor cluster in reach chain
	int nextClusterNum = 0;
	// Could be zero if we are not currently in some floor cluster.
	// We are still interested in getting next floor cluster in this case.
	const int startClusterNum = floorClusterNums[currGroundedAreaNum];
	int pathAreaNum = currGroundedAreaNum;
	const int targetAreaNum = context->NavTargetAasAreaNum();
	while( pathAreaNum != targetAreaNum ) {
		int reachNum;
		if( !routeCache->FindRoute( pathAreaNum, targetAreaNum, bot->TravelFlags(), &reachNum ) ) {
			return false;
		}
		const auto &reach = aasReaches[reachNum];
		const auto travelType = reach.traveltype & TRAVELTYPE_MASK;
		// We have met non bhop-compatible travel type prior to entering next floor cluster
		if( travelType != TRAVEL_WALK && travelType != TRAVEL_WALKOFFLEDGE ) {
			return false;
		}
		pathAreaNum = reach.areanum;
		nextClusterNum = floorClusterNums[pathAreaNum];
		// We have found a next cluster
		if( nextClusterNum && nextClusterNum != startClusterNum ) {
			break;
		}
	}

	if( !nextClusterNum ) {
		return false;
	}

	// If we are currently in a floor cluster and its very likely that the next cluster is not visible
	if( startClusterNum && !aasWorld->areFloorClustersCertainlyVisible( startClusterNum, nextClusterNum ) ) {
		return false;
	}

	CandidateAreasHeap candidateAreasHeap;
	TryReusingOldHeap( context, currGroundedAreaNum, nextClusterNum, candidateAreasHeap );

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	vec3_t traceMins, traceMaxs;
	TacticalSpotsRegistry::GetSpotsWalkabilityTraceBounds( traceMins, traceMaxs );
	Vec3 start( entityPhysicsState.Origin() );
	if( entityPhysicsState.GroundEntity() ) {
		start.z() += 4.0f;
	}

	while( !candidateAreasHeap.empty() ) {
		wsw::pop_heap( candidateAreasHeap.begin(), candidateAreasHeap.end() );
		int areaNum = candidateAreasHeap.back().areaNum;
		int travelTime = (int)( -candidateAreasHeap.back().score );
		candidateAreasHeap.pop_back();

		// Unfortunately no cheaper cutoffs are possible for this
		// TODO: we can precompute floor cluster to floor cluster vis table
		// at start to reject the entire attempt of testing next cluster quickly
		if( !AreaPassesCollisionTest( start, areaNum, traceMins, traceMaxs ) ) {
			continue;
		}

		// Stop on the first (and best since a heap is used) feasible area
		if( resultAreaNum ) {
			*resultAreaNum = areaNum;
		}
		return travelTime;
	}

	return 0;
}