#include "dodgehazardproblemsolver.h"
#include "spotsproblemsolverslocal.h"
#include "../navigation/aaselementsmask.h"

DodgeHazardProblemSolver::DodgeHazardProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ )
	: TacticalSpotsProblemSolver( originParams_, problemParams_ ), problemParams( problemParams_ ) {
	addSortCriterion( SpotSortCriterion::GenericScore, 5 );
}

int DodgeHazardProblemSolver::findMany( vec3_t *spotOrigins, int maxSpots ) {
	uint16_t insideSpotNum;
	const SpotsQueryVector &spotsFromQuery = tacticalSpotsRegistry->FindSpotsInRadius( originParams, &insideSpotNum );
	SpotsAndScoreVector &candidateSpots = tacticalSpotsRegistry->cleanAndGetSpotsAndScoreVector();
	selectCandidateSpots( spotsFromQuery, candidateSpots );

	auto maybeVelocityDir = getVelocityDirForConformanceTests();
	if( maybeVelocityDir ) {
		addSortCriterion( SpotSortCriterion::VelocityConformance, 3 );
		modifyScoreByVelocityConformance( candidateSpots, *maybeVelocityDir );
	}

	// Sort spots so best spots are first
	sort( candidateSpots );
	checkSpotsReach( candidateSpots, maxSpots );
	// TODO: Continue retrieval if numSpots < maxSpots and merge results
	if( int numSpots = makeResultsPruningByProximity( candidateSpots, spotOrigins, maxSpots ) ) {
		return numSpots;
	}

	OriginAndScoreVector &fallbackCandidates = tacticalSpotsRegistry->cleanAndGetOriginAndScoreVector();
	selectFallbackSpotLikeOrigins( spotsFromQuery, fallbackCandidates );
	if( maybeVelocityDir ) {
		modifyScoreByVelocityConformance( fallbackCandidates, *maybeVelocityDir );
	}

	// We consider fallback candidates a-priori reachable so no reach test is performed.
	// Actually this is due to way too many changes required to spots solver architecture.
	sort( fallbackCandidates );
	return makeResultsPruningByProximity( fallbackCandidates, spotOrigins, maxSpots );
}

std::optional<Vec3> DodgeHazardProblemSolver::getVelocityDirForConformanceTests() const {
	const edict_t *ent = originParams.originEntity;
	if( !ent ) {
		return std::nullopt;
	}

	const float *__restrict origin = ent->s.origin;
	// Make sure that the current entity params match problem params
	if( !VectorCompare( origin, originParams.origin ) ) {
		return std::nullopt;
	}

	Vec3 velocityDir( ent->velocity );
	const float squareSpeed = velocityDir.squareLength();
	const float runSpeed = GS_DefaultPlayerSpeed( *ggs );
	if( squareSpeed < runSpeed * runSpeed ) {
		return std::nullopt;
	}

	velocityDir *= Q_RSqrt( squareSpeed );
	return velocityDir;
}

template <typename V>
void DodgeHazardProblemSolver::modifyScoreByVelocityConformance( V &input, const Vec3 &velocityDir ) {
	const float *__restrict origin = originParams.origin;
	for( auto &spotAndScoreLike: input ) {
		Vec3 toSpotDir = Vec3( ::SpotOriginOf( spotAndScoreLike ) ) - origin;
		float velocityDotFactor;
		if( toSpotDir.normalizeFast() ) {
			velocityDotFactor = 0.5f * ( 1.0f + velocityDir.dot( toSpotDir ) );
		} else {
			velocityDotFactor = 1.0f;
		}
		scores[spotAndScoreLike.scoreIndex].set( SpotSortCriterion::VelocityConformance, velocityDotFactor );
	}
}

void DodgeHazardProblemSolver::selectCandidateSpots( const SpotsQueryVector &spotsFromQuery,
													 SpotsAndScoreVector &candidateSpots ) {
	auto [dodgeDir, mayNegateDodgeDir] = makeDodgeHazardDir();

	const float searchRadius = originParams.searchRadius;
	const float minHeightAdvantage = problemParams.minHeightAdvantageOverOrigin;
	const float originZ = originParams.origin[2];
	const float *__restrict origin = originParams.origin;
	const auto *aasWorld = AiAasWorld::instance();
	const int originAreaNum = originParams.originAreaNum;
	const int originFloorClusterNum = aasWorld->floorClusterNum( originAreaNum );
	const int topNodeHint = SV_FindTopNodeForSphere( originParams.origin, originParams.searchRadius );
	trace_t trace;

	const auto *const spots = tacticalSpotsRegistry->spots;
	for( auto spotNum: spotsFromQuery ) {
		const TacticalSpot &spot = spots[spotNum];

		float heightOverOrigin = spot.absMins[2] - originZ;
		if( heightOverOrigin < minHeightAdvantage ) {
			continue;
		}

		Vec3 toSpotDir = Vec3( spot.origin ) - origin;
		const float squareDistance = toSpotDir.squareLength();
		if( squareDistance < 1 ) {
			continue;
		}

		toSpotDir *= Q_RSqrt( squareDistance );
		const float dot = toSpotDir.dot( dodgeDir );
		const float absDot = std::fabs( dot );
		// We can do smarter tricks using std::signbit() & !mightNegateDodgeDir but this is not really a hot code path
		if( ( mayNegateDodgeDir ? absDot : dot ) < 0.2f ) {
			continue;
		}

		// Try rejecting candidates early if the spot is in the same floor cluster and does not seem to be walkable
		if( originFloorClusterNum && aasWorld->floorClusterNum( spot.aasAreaNum ) == originFloorClusterNum ) {
			if( !aasWorld->isAreaWalkableInFloorCluster( originAreaNum, spot.aasAreaNum )) {
				continue;
			}
		}

		StaticWorldTrace( &trace, origin, spot.origin, CONTENTS_SOLID, vec3_origin, vec3_origin, topNodeHint );
		if( trace.fraction != 1.0f ) {
			continue;
		}

		heightOverOrigin -= minHeightAdvantage;
		const float heightOverOriginFactor = BoundedFraction( heightOverOrigin, searchRadius - minHeightAdvantage );

		auto [criteriaScores, scoresIndex] = addNextScores();
		criteriaScores->set( SpotSortCriterion::GenericScore, absDot );
		criteriaScores->set( SpotSortCriterion::HeightOverOrigin, heightOverOriginFactor );
		candidateSpots.push_back( SpotAndScore( spotNum, scoresIndex ) );
	}
}

std::pair<Vec3, bool> DodgeHazardProblemSolver::makeDodgeHazardDir() const {
	if( problemParams.avoidSplashDamage ) {
		Vec3 result( 0, 0, 0 );
		Vec3 originToHitDir = problemParams.hazardHitPoint - originParams.origin;
		float degrees = originParams.originEntity ? -originParams.originEntity->s.angles[YAW] : -90;
		RotatePointAroundVector( result.data(), &axis_identity[AXIS_UP], originToHitDir.data(), degrees );
		if( !result.normalizeFast() ) {
			result = Vec3( 1, 0, 0 );
		}
		if( std::fabs( result.x() ) < 0.3 ) {
			result.x() = 0;
		}
		if( std::fabs( result.y() ) < 0.3 ) {
			result.y() = 0;
		}
		result.z() = 0;
		result.x() *= -1.0f;
		result.y() *= -1.0f;
		return std::make_pair( result, false );
	}

	Vec3 selfToHitPoint = problemParams.hazardHitPoint - originParams.origin;
	selfToHitPoint.z() = 0;
	// If bot is not hit in its center, try pick a direction that is opposite to a vector from bot center to hit point
	if( selfToHitPoint.normalizeFast( { .minAcceptableLength = 4.0f } ) ) {
		// Check whether this direction really helps to dodge the hazard
		// (the less is the abs. value of the dot product, the closer is the chosen direction to a perpendicular one)
		if( std::fabs( selfToHitPoint.dot( originParams.origin ) ) < 0.5f ) {
			if( std::fabs( selfToHitPoint.x() ) < 0.3f ) {
				selfToHitPoint.x() = 0;
			}
			if( std::fabs( selfToHitPoint.y() ) < 0.3f ) {
				selfToHitPoint.y() = 0;
			}
			return std::make_pair( -selfToHitPoint, false );
		}
	}

	// Otherwise just pick a direction that is perpendicular to the hazard direction
	float maxCrossSqLen = 0.0f;
	Vec3 result( 0, 0, 0 );
	for( int i = 0; i < 3; ++i ) {
		Vec3 cross = problemParams.hazardDirection.cross( &axis_identity[i * 3] );
		cross.z() = 0;
		float crossSqLen = cross.squareLength();
		if( crossSqLen <= maxCrossSqLen ) {
			continue;
		}
		maxCrossSqLen = crossSqLen;
		float invLen = Q_RSqrt( crossSqLen );
		result.x() = cross.x() * invLen;
		result.y() = cross.y() * invLen;
	}
	return std::make_pair( result, true );
}

void DodgeHazardProblemSolver::selectFallbackSpotLikeOrigins( const SpotsQueryVector &spotsFromQuery,
															  OriginAndScoreVector &result ) {
	const auto &aasWorld = AiAasWorld::instance();
	const auto aasAreas = aasWorld->getAreas();

	bool *const failedAtArea = AasElementsMask::TmpAreasVisRow();
	::memset( failedAtArea, 0, sizeof( bool ) * aasAreas.size() );
	const auto &spots = tacticalSpotsRegistry->spots;
	for( const auto &spotAndScore: spotsFromQuery ) {
		failedAtArea[spots[spotAndScore].aasAreaNum] = true;
	}

	const int originAreaNum = originParams.originAreaNum;
	const int stairsClusterNum = aasWorld->stairsClusterNum( originAreaNum );
	if( stairsClusterNum ) {
		const std::span<const uint16_t> stairsClusterAreaNums = aasWorld->stairsClusterData( stairsClusterNum );
		for( const int areaNum : { stairsClusterAreaNums.front(), stairsClusterAreaNums.back() } ) {
			if( !failedAtArea[areaNum] ) {
				result.emplace_back( OriginAndScore::ForArea( aasAreas, areaNum, addNextScores().second ) );
			}
		}
		return;
	}

	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto &originAreaSettings = aasAreaSettings[originAreaNum];
	if( originAreaSettings.areaflags & AREA_INCLINED_FLOOR ) {
		// TODO: Cache this at loading
		float minAreaHeight = +99999;
		float maxAreaHeight = -99999;
		int rampStartArea = 0, rampEndArea = 0;
		const auto &aasReach = aasWorld->getReaches();
		const int maxReachNum = originAreaSettings.firstreachablearea + originAreaSettings.numreachableareas;
		for( int reachNum = originAreaSettings.firstreachablearea; reachNum < maxReachNum; ++reachNum ) {
			const auto &reach = aasReach[reachNum];
			if( ( reach.traveltype & TRAVELTYPE_MASK ) != TRAVEL_WALK ) {
				continue;
			}
			const int reachAreaNum = aasReach[reachNum].areanum;
			const auto &reachArea = aasWorld->getAreas()[reachAreaNum];
			if( reachArea.mins[2] < minAreaHeight ) {
				rampStartArea = reachAreaNum;
				minAreaHeight = reachArea.mins[2];
			}
			if( reachArea.maxs[2] > maxAreaHeight ) {
				rampEndArea = reachAreaNum;
				maxAreaHeight = reachArea.mins[2];
			}
			// If we've found two areas (avoid an extra branch by using a multiplication)
			if( ( rampStartArea * rampEndArea ) != 0 ) {
				break;
			}
		}
		if( rampStartArea && !failedAtArea[rampStartArea] ) {
			result.emplace_back( OriginAndScore::ForArea( aasAreas, rampStartArea, addNextScores().second ) );
		}
		if( rampEndArea && !failedAtArea[rampEndArea] ) {
			result.emplace_back( OriginAndScore::ForArea( aasAreas, rampEndArea, addNextScores().second ) );
		}
		return;
	}

	const int floorClusterNum = aasWorld->areaFloorClusterNums()[originAreaNum];
	if( !floorClusterNum ) {
		return;
	}

	int skipAreaNum = originAreaNum;
	// We perform 2D distance comparisons in a floor cluster
	const float originX = originParams.origin[0];
	const float originY = originParams.origin[1];
	const float squareProximityThreshold = problemParams.spotProximityThreshold * problemParams.spotProximityThreshold;
	if( const float *center = aasAreas[skipAreaNum].center ) {
		const float dx = center[0] - originX;
		const float dy = center[1] - originY;
		if( dx * dx + dy * dy > squareProximityThreshold ) {
			skipAreaNum = 0;
		}
	}

	const float squareSearchRadius = originParams.searchRadius * originParams.searchRadius;
	const std::span<const uint16_t> floorClusterAreaNums = aasWorld->floorClusterData( floorClusterNum );
	for( const int areaNum: floorClusterAreaNums ) {
		if( areaNum == skipAreaNum ) {
			continue;
		}
		const auto &area = aasAreas[areaNum];
		const float dx = area.center[0] - originX;
		const float dy = area.center[1] - originY;
		const float squareDistance = dx * dx + dy * dy;
		if( squareDistance > squareSearchRadius ) {
			continue;
		}
		if( squareDistance < squareProximityThreshold ) {
			continue;
		}
		if( failedAtArea[areaNum] ) {
			continue;
		}
		if( !aasWorld->isAreaWalkableInFloorCluster( originAreaNum, areaNum ) ) {
			continue;
		}
		result.emplace_back( OriginAndScore::ForArea( aasAreas, areaNum, addNextScores().second ) );
	}
}