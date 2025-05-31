#include "advantageproblemsolver.h"
#include "spotsproblemsolverslocal.h"
#include "../navigation/aaselementsmask.h"

AdvantageProblemSolver::AdvantageProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ )
	: TacticalSpotsProblemSolver( originParams_, problemParams_ ), problemParams( problemParams_ ) {
	// Strongest criteria must come last
	addSortCriterion( SpotSortCriterion::SpotVisImpact, 3 );
	addSortCriterion( SpotSortCriterion::OriginDistance, 3 );
	addSortCriterion( SpotSortCriterion::EntityDistance, 3 );
	addSortCriterion( SpotSortCriterion::HeightOverOrigin, 2 );
	addSortCriterion( SpotSortCriterion::HeightOverEntity, 2 );
	addSortCriterion( SpotSortCriterion::TravelTime, 5 );
	addSortCriterion( SpotSortCriterion::EnemyVisImpact, 3 );
}

int AdvantageProblemSolver::findMany( vec3_t *spots, int maxSpots ) {
	uint16_t insideSpotNum;
	SpotsQueryVector &spotsFromQuery = tacticalSpotsRegistry->FindSpotsInRadius( originParams, &insideSpotNum );
	// Cut off some raw spots from query by vis tables
	pruneByVisTables( spotsFromQuery );
	SpotsAndScoreVector &candidateSpots = tacticalSpotsRegistry->cleanAndGetSpotsAndScoreVector();
	// This should be cheap as well
	selectCandidateSpots( spotsFromQuery, candidateSpots );
	// Cut off expensive routing calls for spots that a-priori do not have a feasible travel time
	pruneByReachTables( candidateSpots );

	// Now cast rays in a collision world... it's actually cheaper than pathfinding.
	// Make sure we select not less than 5 candidates if possible even if maxSpots is lesser.
	checkOriginVisibility( candidateSpots );
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 5, maxSpots ) );

	// Apply enemy influence... this is not that expensive
	applyEnemiesInfluence( candidateSpots );
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 5, maxSpots ) );

	// Make sure we select not less than 3 candidates if possible even if maxSpots is lesser
	applyVisAndOtherFactors( candidateSpots );

	// Should be always sorted before the last selection call
	sortAndTakeNBestIfOptimizingAggressively( candidateSpots, wsw::max( 3, maxSpots ) );

	checkSpotsReach( candidateSpots, maxSpots );

	sort( candidateSpots );
	return makeResultsPruningByProximity( candidateSpots, spots, maxSpots );
}

void AdvantageProblemSolver::selectCandidateSpots( const SpotsQueryVector &spotsFromQuery, SpotsAndScoreVector &candidateSpots ) {
	const float minHeightAdvantageOverOrigin = problemParams.minHeightAdvantageOverOrigin;
	const float minHeightAdvantageOverEntity = problemParams.minHeightAdvantageOverEntity;
	const float minSquareDistanceToEntity = problemParams.minSpotDistanceToEntity * problemParams.minSpotDistanceToEntity;
	const float maxSquareDistanceToEntity = problemParams.maxSpotDistanceToEntity * problemParams.maxSpotDistanceToEntity;
	const float searchRadius = originParams.searchRadius;
	const float originZ = originParams.origin[2];
	const float entityZ = problemParams.keepVisibleOrigin[2];
	const float rangeOfAdvantageOverOrigin = problemParams.advantageOverOriginForMaxScore - minHeightAdvantageOverOrigin;
	const float rangeOfAdvantageOverEntity = problemParams.advantageOverEntityForMaxScore - minHeightAdvantageOverEntity;
	const float originAdvantageNormalizer = Q_Rcp( rangeOfAdvantageOverOrigin );
	const float entityAdvantageNormalizer = Q_Rcp( rangeOfAdvantageOverEntity );
	// Copy to stack for faster access
	Vec3 origin( originParams.origin );
	Vec3 entityOrigin( problemParams.keepVisibleOrigin );

	const auto *const spots = tacticalSpotsRegistry->spots;

	for( auto spotNum: spotsFromQuery ) {
		const TacticalSpot &spot = spots[spotNum];

		float heightOverOrigin = spot.absMins[2] - originZ;
		if( heightOverOrigin < minHeightAdvantageOverOrigin ) {
			continue;
		}

		float heightOverEntity = spot.absMins[2] - entityZ;
		if( heightOverEntity < minHeightAdvantageOverEntity ) {
			continue;
		}

		float squareDistanceToOrigin = DistanceSquared( origin.Data(), spot.origin );
		if( squareDistanceToOrigin > searchRadius * searchRadius ) {
			continue;
		}

		float squareDistanceToEntity = DistanceSquared( entityOrigin.Data(), spot.origin );
		if( squareDistanceToEntity < minSquareDistanceToEntity ) {
			continue;
		}
		if( squareDistanceToEntity > maxSquareDistanceToEntity ) {
			continue;
		}

		auto [criteriaScores, scoresIndex] = this->addNextScores();
		float originAdvantageScore = ( heightOverOrigin - minHeightAdvantageOverOrigin );
		originAdvantageScore = wsw::min( originAdvantageScore, rangeOfAdvantageOverOrigin );
		originAdvantageScore *= originAdvantageNormalizer;
		float entityAdvantageScore = ( heightOverEntity - minHeightAdvantageOverEntity );
		entityAdvantageScore = wsw::min( entityAdvantageScore, rangeOfAdvantageOverEntity );
		entityAdvantageScore *= entityAdvantageNormalizer;
		criteriaScores->set( SpotSortCriterion::HeightOverOrigin, originAdvantageScore );
		criteriaScores->set( SpotSortCriterion::HeightOverEntity, entityAdvantageScore );
		candidateSpots.emplace_back( SpotAndScore( spotNum, scoresIndex ) );
	}
}

void AdvantageProblemSolver::pruneByVisTables( SpotsQueryVector &spotsFromQuery ) {
	int keepVisibleAreaNum = 0;
	Vec3 keepVisibleOrigin( problemParams.keepVisibleOrigin );
	if( const auto *keepVisibleEntity = problemParams.keepVisibleEntity ) {
		keepVisibleOrigin.Z() += 0.66f * keepVisibleEntity->r.maxs[2];
		if( const Bot *bot = keepVisibleEntity->bot ) {
			int areaNums[2] { 0, 0 };
			bot->EntityPhysicsState()->PrepareRoutingStartAreas( areaNums );
			keepVisibleAreaNum = areaNums[0];
		}
	}

	const auto *const spots = tacticalSpotsRegistry->spots;
	const auto *const aasWorld = AiAasWorld::instance();
	if( !keepVisibleAreaNum ) {
		keepVisibleAreaNum = aasWorld->findAreaNum( keepVisibleOrigin );
	}

	// Decompress an AAS areas vis row for the area of the "keep visible origin/entity"
	const auto *keepVisEntRow = aasWorld->decompressAreaVis( keepVisibleAreaNum, AasElementsMask::TmpAreasVisRow() );

	unsigned numKeptSpots = 0;
	// Filter spots in-place
	for( auto spotNum: spotsFromQuery ) {
		const auto spotAreaNum = spots[spotNum].aasAreaNum;
		// Check whether the keep visible entity/origin is considered visible from the spot.
		// Generally speaking the visibility relation should not be symmetric
		// but the AAS area table vis computations are made only against a solid collision world.
		// Consider the entity non-visible from spot if the spot is not considered visible from the entity.
		if( !keepVisEntRow[spotAreaNum] ) {
			continue;
		}

		// Store spot num in-place
		spotsFromQuery[numKeptSpots++] = spotNum;
	}

	spotsFromQuery.truncate( numKeptSpots );
}

void AdvantageProblemSolver::checkOriginVisibility( SpotsAndScoreVector &candidateSpots ) {
	edict_t *passent = const_cast<edict_t*>( originParams.originEntity );
	edict_t *keepVisibleEntity = const_cast<edict_t *>( problemParams.keepVisibleEntity );
	Vec3 keepVisibleOrigin( problemParams.keepVisibleOrigin );
	if( keepVisibleEntity ) {
		// Its a good idea to add some offset from the ground
		keepVisibleOrigin.Z() += 0.66f * keepVisibleEntity->r.maxs[2];
	}

	trace_t trace;
	const edict_t *gameEdicts = game.edicts;
	const auto *const spots = tacticalSpotsRegistry->spots;
	const float spotZOffset = -playerbox_stand_mins[2] + playerbox_stand_viewheight;

	unsigned numKeptSpots = 0;
	for( const SpotAndScore &spotAndScore : candidateSpots ) {
		//.Spot origins are dropped to floor (only few units above)
		// Check whether we can hit standing on this spot (having the gun at viewheight)
		Vec3 from( spots[spotAndScore.spotNum].origin );
		from.Z() += spotZOffset;
		G_Trace( &trace, from.Data(), nullptr, nullptr, keepVisibleOrigin.Data(), passent, MASK_AISOLID );
		if( trace.fraction != 1.0f && gameEdicts + trace.ent != keepVisibleEntity ) {
			continue;
		}

		candidateSpots[numKeptSpots++] = spotAndScore;
	}

	candidateSpots.truncate( numKeptSpots );
}

void AdvantageProblemSolver::applyVisAndOtherFactors( SpotsAndScoreVector &candidateSpots ) {
	if( candidateSpots.empty() ) {
		return;
	}

	const Vec3 origin( originParams.origin );
	const Vec3 entityOrigin( problemParams.keepVisibleOrigin );
	const float searchRadius = originParams.searchRadius;
	const float originWeightFalloffDistanceRatio = problemParams.originWeightFalloffDistanceRatio;
	const float entityWeightFalloffDistanceRatio = problemParams.entityWeightFalloffDistanceRatio;
	const float minSpotDistanceToEntity = problemParams.minSpotDistanceToEntity;
	const float entityDistanceRange = problemParams.maxSpotDistanceToEntity - problemParams.minSpotDistanceToEntity;

	const auto *const spotVisibilityTable = tacticalSpotsRegistry->spotVisibilityTable;
	const auto *const spots = tacticalSpotsRegistry->spots;
	const auto numSpots = tacticalSpotsRegistry->numSpots;

	for( unsigned i = 0; i < candidateSpots.size(); ++i ) {
		const auto &spotAndScore = candidateSpots[i];
		unsigned testedSpotNum = spotAndScore.spotNum;
		// Get address of the visibility table row
		const uint8_t *spotVisForSpotNum = spotVisibilityTable + testedSpotNum * numSpots;

		unsigned visSum = 0;
		for( unsigned j = 0; j < i; ++j ) {
			visSum += spotVisForSpotNum[j];
		}

		// Skip i-th index

		for( unsigned j = i + 1; j < candidateSpots.size(); ++j ) {
			visSum += spotVisForSpotNum[j];
		}

		const TacticalSpot &testedSpot = spots[testedSpotNum];

		// The maximum possible visibility score for a pair of spots is 255
		float visFactor = Q_Sqrt( 0.001f + (float)visSum / ( candidateSpots.size() * 256.0f ) );

		float originDistance = Q_Sqrt( 0.001f + origin.SquareDistanceTo( testedSpot.origin ) );
		// TODO: Refactor/inline that too
		float originDistanceFactor = ComputeDistanceFactor( originDistance, originWeightFalloffDistanceRatio, searchRadius );

		float entityDistance = Q_Sqrt( 0.001f + DistanceSquared( testedSpot.origin, entityOrigin.Data() ) );
		entityDistance -= minSpotDistanceToEntity;
		// TODO: Refactor/inline that too
		float entityDistanceFactor = ComputeDistanceFactor( entityDistance,
															entityWeightFalloffDistanceRatio,
															entityDistanceRange );
		auto &criteriaScores = scores[spotAndScore.scoreIndex];
		criteriaScores.set( SpotSortCriterion::SpotVisImpact, visFactor );
		criteriaScores.set( SpotSortCriterion::EntityDistance, entityDistanceFactor );
		criteriaScores.set( SpotSortCriterion::OriginDistance, originDistanceFactor );
	}
}