#include "coverproblemsolver.h"
#include "spotsproblemsolverslocal.h"
#include "../navigation/aaselementsmask.h"

CoverProblemSolver::CoverProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ )
	: TacticalSpotsProblemSolver( originParams_, problemParams_ ), problemParams( problemParams_ ) {
	// Strongest criteria must come last
	addSortCriterion( SpotSortCriterion::TravelTime, 3 );
	addSortCriterion( SpotSortCriterion::EnemyVisImpact, 3 );
}

int CoverProblemSolver::findMany( vec3_t *spots, int maxSpots ) {
	uint16_t insideSpotNum;
	const SpotsQueryVector &spotsFromQuery = tacticalSpotsRegistry->FindSpotsInRadius( originParams, &insideSpotNum );
	SpotsAndScoreVector &candidateSpots = tacticalSpotsRegistry->cleanAndGetSpotsAndScoreVector();
	selectCandidateSpots( spotsFromQuery, candidateSpots );
	// Use these cheap calls to cut off as many spots as possible before a first collision filter
	pruneByReachTables( candidateSpots );
	pruneByAreaVisTables( candidateSpots );

	// Return early in this case.
	// All expensive stuff starts below.
	if( candidateSpots.empty() ) {
		return 0;
	}

	wsw::StaticVector<int, MAX_EDICTS> entNums;
	const int topNode = findTopNodeAndEntNums( candidateSpots, entNums );

	// These calls rely on vis tables to some degree and thus should not be extremely expensive.
	// Make sure we select not less than 5 candidates if possible even if maxSpots is lesser.
	pruneByCoarseRayTests( candidateSpots, topNode, entNums );
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 5, maxSpots ) );

	// Make sure we select not less than 5 candidates if possible even if maxSpots is lesser.
	applyEnemiesInfluence( candidateSpots );
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 5, maxSpots ) );

	// Even "fine" collision checks are actually faster than pathfinding.
	selectCoverSpots( candidateSpots, topNode, entNums );

	// Make sure we select not less than 3 candidates if possible even if maxSpots is lesser.
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 3, maxSpots ) );

	checkSpotsReach( candidateSpots, maxSpots );

	// Sort spots before a final selection so best spots are first
	sort( candidateSpots );
	return makeResultsPruningByProximity( candidateSpots, spots, maxSpots );
}

void CoverProblemSolver::pruneByAreaVisTables( SpotsAndScoreVector &spotsAndScores ) {
	const auto *const aasWorld = AiAasWorld::instance();
	const auto aasAreas = aasWorld->getAreas();
	const int attackerAreaNum = aasWorld->findAreaNum( problemParams.attackerOrigin );

	// Check whether we may consider that an an area is fully visible for attacker if the table data indicates visibility.
	// Currently table data is very coarse and is computed by a raycast from an area center to another area center.
	// This might matter for this subtle cover problem that often is vital for a bot.
	const auto &attackerArea = aasAreas[attackerAreaNum];
	const float threshold = 64.0f + problemParams.harmfulRayThickness;
	// Check XY area dimensions
	for( int i = 0; i < 2; ++i ) {
		if( attackerArea.maxs[i] - attackerArea.mins[i] > threshold ) {
			// Can't do conclusions for the attacker area based on table data
			return;
		}
	}

	const auto *const spots = tacticalSpotsRegistry->spots;
	const bool *attackerVisRow = aasWorld->decompressAreaVis( attackerAreaNum, AasElementsMask::TmpAreasVisRow() );

	unsigned numFeasibleSpots = 0;
	for( const SpotAndScore &spotAndScore: spotsAndScores ) {
		const int spotAreaNum = spots[spotAndScore.spotNum].aasAreaNum;
		const auto &spotArea = aasAreas[spotAreaNum];
		// Check area XY dimensions
		if( spotArea.maxs[0] - spotArea.mins[0] > threshold || spotArea.maxs[1] - spotArea.mins[1] > threshold ) {
			// Can't do conclusions for the spot area based on table data
			continue;
		}
		// Given the fact we've checked dimensions this test should produce very few false negatives
		if( attackerVisRow[spotAreaNum] ) {
			continue;
		}
		spotsAndScores[numFeasibleSpots++] = spotAndScore;
	}

	spotsAndScores.truncate( numFeasibleSpots );
}

int CoverProblemSolver::findTopNodeAndEntNums( SpotsAndScoreVector &spotsAndScores, EntNumsVector &entNums ) {
	// Should not be called for these parameters
	assert( !spotsAndScores.empty() );
	assert( entNums.empty() );

	vec3_t bounds[2];
	ClearBounds( bounds[0], bounds[1] );

	// We're building a box for an attacker and all spots
	if( const auto *__restrict ent = problemParams.attackerEntity ) {
		AddPointToBounds( ent->r.absmin, bounds[0], bounds[1] );
		AddPointToBounds( ent->r.absmax, bounds[0], bounds[1] );
	} else {
		AddPointToBounds( problemParams.attackerOrigin, bounds[0], bounds[1] );
	}

	const auto *const spots = tacticalSpotsRegistry->spots;
	for( const SpotAndScore &spotAndScore: spotsAndScores ) {
		const auto &__restrict spot = spots[spotAndScore.spotNum];
		AddPointToBounds( spot.absMins, bounds[0], bounds[1] );
		AddPointToBounds( spot.absMaxs, bounds[0], bounds[1] );
	}

	const int topNode = SV_FindTopNodeForBox( bounds[0], bounds[1] );

	const auto numRawEnts = (unsigned)GClip_AreaEdicts( bounds[0], bounds[1], entNums.begin(), MAX_EDICTS, AREA_SOLID, 0 );
	assert( numRawEnts < entNums.capacity() );
	entNums.unsafe_set_size( numRawEnts );

	pruneRawEntNums( entNums );

	return topNode;
}

void CoverProblemSolver::pruneRawEntNums( EntNumsVector &entNums ) {
	unsigned numKeptEnts = 0;

	// Filter out entities that should be excluded from collision tests for raycasts from attacker origin to spots.
	// TODO: Use much more aggressive cutoffs

	const auto *const gameEdicts = game.edicts;
	// May be null but harmless in this case
	const auto *const originEntity = originParams.originEntity;
	const auto *const attackerEntity = problemParams.attackerEntity;
	if( !attackerEntity ) {
		const auto *const attackerOrigin = problemParams.attackerOrigin;
		for( int entNum: entNums ) {
			const auto *const ent = gameEdicts + entNum;
			// Skip the origin entity (that asks for cover).
			// We assume that the entity is going to flee from its current position.
			if( ent == originEntity ) {
				continue;
			}
			// Consider that only these entities can block attacker view
			if( ent->movetype != MOVETYPE_NONE ) {
				continue;
			}
			// TODO: Precache the attacker leaf num if the PVS test is kept
			if( !SV_InPVS( attackerOrigin, ent->s.origin ) ) {
				continue;
			}
			entNums[numKeptEnts++] = entNum;
		}

		entNums.truncate( numKeptEnts );
		return;
	}

	const auto *const pvsCache = EntitiesPvsCache::Instance();
	for( int entNum: entNums ) {
		const auto *const ent = gameEdicts + entNum;
		// Exclude the attacker entity from further collision tests.
		// Otherwise we're going to keep hitting it on every raycast.
		// Exclude the origin entity (if any) for reasons described above.
		if( ent == attackerEntity || ent == originEntity ) {
			continue;
		}
		if( ent->movetype != MOVETYPE_NONE ) {
			continue;
		}
		if( !pvsCache->AreInPvs( attackerEntity, ent ) ) {
			continue;
		}
		entNums[numKeptEnts++] = entNum;
	}

	entNums.truncate( numKeptEnts );
}

void CoverProblemSolver::pruneByCoarseRayTests( SpotsAndScoreVector &spotsAndScores,
												int collisionTopNodeHint,
												const EntNumsVector &entNums ) {
	const auto *const spots = tacticalSpotsRegistry->spots;

	unsigned numFeasibleSpots = 0;
	// Filter spots in-place
	for( const SpotAndScore &spotAndScore: spotsAndScores ) {
		const TacticalSpot &spot = spots[spotAndScore.spotNum];
		// Check whether spot is certainly visible
		if( castRay( problemParams.attackerOrigin, spot.origin, collisionTopNodeHint, entNums ) ) {
			continue;
		}
		spotsAndScores[numFeasibleSpots++] = spotAndScore;
	}

	spotsAndScores.truncate( numFeasibleSpots );
}

void CoverProblemSolver::selectCoverSpots( SpotsAndScoreVector &spotsAndScores,
										   int collisionTopNodeHint,
										   const EntNumsVector &entNums ) {
	const float harmfulRayThickness = problemParams.harmfulRayThickness;
	const auto *const spots = tacticalSpotsRegistry->spots;

	const vec3_t rayBounds[2] = {
		{ -harmfulRayThickness, -harmfulRayThickness, -harmfulRayThickness },
		{ +harmfulRayThickness, +harmfulRayThickness, +harmfulRayThickness }
	};

	unsigned numFilteredSpots = 0;
	// Filter spots in-place
	for( const SpotAndScore &spotAndScore: spotsAndScores ) {
		const TacticalSpot &spot = spots[spotAndScore.spotNum];
		if( !looksLikeACoverSpot( spot, rayBounds, collisionTopNodeHint, entNums ) ) {
			continue;
		}

		spotsAndScores[numFilteredSpots++] = spotAndScore;
	}

	spotsAndScores.truncate( numFilteredSpots );
}

bool CoverProblemSolver::looksLikeACoverSpot( const TacticalSpot &spot,
											  const vec3_t *rayBounds,
											  int collisionTopNodeHint,
											  const EntNumsVector &entNums ) {
	vec3_t bounds[2];
	// Convert bounds from relative to absolute
	VectorAdd( rayBounds[0], spot.absMins, bounds[0] );
	VectorAdd( rayBounds[1], spot.absMaxs, bounds[1] );

	// TODO: Reduce the number of tested corners by using a projection onto view plane for the attacker
	for( int i = 0; i < 8; ++i ) {
		const vec3_t testedPoint = {
			bounds[( i >> 2 ) & 1][0],
			bounds[( i >> 1 ) & 1][1],
			bounds[( i >> 0 ) & 1][2]
		};
		// If the corner is certainly visible
		if( castRay( problemParams.attackerOrigin, testedPoint, collisionTopNodeHint, entNums ) ) {
			return false;
		}
	}

	return true;
}

bool CoverProblemSolver::castRay( const float *from, const float *to, int topNode, const EntNumsVector &entNums ) {
	trace_t trace;

	// Test against blocking view entities first.
	// Either number of these entities is low or we can cut off expensive long raycasts in the static world.

	const auto *const gameEdicts = game.edicts;
	for( int entNum: entNums ) {
		const auto *ent = gameEdicts + entNum;

		// TODO: Optimize using AABB/line intersection
		const auto *model = SV_ModelForBBox( ent->r.mins, ent->r.maxs );
		SV_TransformedBoxTrace( &trace, from, to, vec3_origin, vec3_origin, model,
			                         MASK_SHOT, ent->s.origin, ent->s.angles, topNode );

		// A ray is blocked by some other solid entity
		if( trace.fraction != 1.0f ) {
			return false;
		}
	}

	StaticWorldTrace( &trace, from, to, MASK_SHOT, vec3_origin, vec3_origin, topNode );
	// Check whether a ray is blocked by a solid world
	return trace.fraction == 1.0f;
}
