#ifndef WSW_9a2e79b8_fb1d_4959_83a8_0f00c80b18e4_H
#define WSW_9a2e79b8_fb1d_4959_83a8_0f00c80b18e4_H

#include "tacticalspotsproblemsolver.h"

class AdvantageProblemSolver: public TacticalSpotsProblemSolver {
public:
	class ProblemParams : public BaseProblemParams {
		friend class AdvantageProblemSolver;

		const edict_t *keepVisibleEntity;
		vec3_t keepVisibleOrigin;
		float minSpotDistanceToEntity { 0.0f };
		float maxSpotDistanceToEntity { 999999.0f };
		float originWeightFalloffDistanceRatio { 0.0f };
		float entityWeightFalloffDistanceRatio { 0.0f };
		float minHeightAdvantageOverEntity { -999999.0f };
		float advantageOverEntityForMaxScore { 128.0f };
	public:
		explicit ProblemParams( const edict_t *keepVisibleEntity_ )
			: keepVisibleEntity( keepVisibleEntity_ ) {
			VectorCopy( keepVisibleEntity->s.origin, this->keepVisibleOrigin );
		}

		explicit ProblemParams( const vec3_t keepVisibleOrigin_ )
			: keepVisibleEntity( nullptr ) {
			VectorCopy( keepVisibleOrigin_, this->keepVisibleOrigin );
		}

		void setMinSpotDistanceToEntity( float distance ) {
			assert( distance > 0 );
			minSpotDistanceToEntity = distance;
		}
		void setMaxSpotDistanceToEntity( float distance ) {
			assert( distance > 0 );
			maxSpotDistanceToEntity = distance;
		}

		void setEntityWeightFalloffDistanceRatio( float ratio ) {
			assert( ratio >= 0.0f && ratio <= 1.0f );
			entityWeightFalloffDistanceRatio = ratio;
		}

		void setOriginWeightFalloffDistanceRatio( float ratio ) {
			assert( ratio >= 0.0f && ratio <= 1.0f );
			originWeightFalloffDistanceRatio = ratio;
		}

		void setMinHeightAdvantageOverEntity( float height ) {
			minHeightAdvantageOverEntity = height;
		}

		void setAdvantageOverEntityForMaxScore( float threshold ) {
			assert( threshold > 0 );
			advantageOverEntityForMaxScore = threshold;
		}
	};
private:
	const ProblemParams &problemParams;

	void checkOriginVisibility( SpotsAndScoreVector &spotsAndScores );

	void applyVisAndOtherFactors( SpotsAndScoreVector &spotsAndScores );

	void pruneByVisTables( SpotsQueryVector &spotsFromQuery );

	void selectCandidateSpots( const SpotsQueryVector &spotsFromQuery, SpotsAndScoreVector &candidateSpots ) override;
public:
	AdvantageProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ );

	int findMany( vec3_t *spots, int maxSpots ) override;
};

#endif
