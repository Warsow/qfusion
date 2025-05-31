#ifndef WSW_7a962d83_96a7_469b_b7c4_66e4fc12e747_H
#define WSW_7a962d83_96a7_469b_b7c4_66e4fc12e747_H

#include "tacticalspotsproblemsolver.h"

class CoverProblemSolver: public TacticalSpotsProblemSolver {
public:
	class ProblemParams: public BaseProblemParams {
		friend class CoverProblemSolver;
		const edict_t *attackerEntity;
		vec3_t attackerOrigin;
		float harmfulRayThickness;
	public:
		ProblemParams( const edict_t *attackerEntity_, float harmfulRayThickness_ )
			: attackerEntity( attackerEntity_ ), harmfulRayThickness( harmfulRayThickness_ ) {
			VectorCopy( attackerEntity_->s.origin, this->attackerOrigin );
		}

		ProblemParams( const vec3_t attackerOrigin_, float harmfulRayThickness_ )
			: attackerEntity( nullptr ), harmfulRayThickness( harmfulRayThickness_ ) {
			VectorCopy( attackerOrigin_, this->attackerOrigin );
		}
	};
private:
	using EntNumsVector = wsw::StaticVector<int, MAX_EDICTS>;

	int findTopNodeAndEntNums( SpotsAndScoreVector &spotsAndScores, EntNumsVector &entNums );

	void pruneRawEntNums( EntNumsVector &entNums );

	void pruneByAreaVisTables( SpotsAndScoreVector &spotsAndScores );

	void pruneByCoarseRayTests( SpotsAndScoreVector &spotsAndScores, int topNode, const EntNumsVector &entNums );

	void selectCoverSpots( SpotsAndScoreVector &candidateSpots, int topNode, const EntNumsVector &entNums );

	bool looksLikeACoverSpot( const TacticalSpot &spot, const vec3_t *rayBounds, int topNode, const EntNumsVector &entNums );

	bool castRay( const float *from, const float *to, int topNode, const EntNumsVector &entNums );

	const ProblemParams &problemParams;
public:
	CoverProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ );

	int findMany( vec3_t *spots, int numSpots ) override;
};

#endif
