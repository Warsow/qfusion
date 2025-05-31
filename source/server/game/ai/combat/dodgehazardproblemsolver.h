#ifndef WSW_254ab4ef_c8f2_4be8_8a13_b96b5269eede_H
#define WSW_254ab4ef_c8f2_4be8_8a13_b96b5269eede_H

#include "tacticalspotsproblemsolver.h"

class DodgeHazardProblemSolver: public TacticalSpotsProblemSolver {
public:
	class ProblemParams : public BaseProblemParams {
		friend class DodgeHazardProblemSolver;
		const Vec3 &hazardHitPoint;
		const Vec3 &hazardDirection;
		const bool avoidSplashDamage;
	public:
		ProblemParams( const Vec3 &hazardHitPoint_, const Vec3 &hazardDirection_, bool avoidSplashDamage_ )
			: hazardHitPoint( hazardHitPoint_ )
			, hazardDirection( hazardDirection_ )
			, avoidSplashDamage( avoidSplashDamage_ ) {}
	};
private:
	const ProblemParams &problemParams;

	/**
	 * Makes a dodge hazard direction for given {@code ProblemParams}
	 * @return a pair of a direction and a flag indicating whether the direction is allowed to be negated.
	 */
	[[nodiscard]]
	std::pair<Vec3, bool> makeDodgeHazardDir() const;

	void selectCandidateSpots( const SpotsQueryVector &spotsFromQuery, SpotsAndScoreVector &candidateSpots ) override;
	void selectFallbackSpotLikeOrigins( const SpotsQueryVector &spotsFromQuery, OriginAndScoreVector &originsAndScores );

	std::optional<Vec3> getVelocityDirForConformanceTests() const;

	template <typename V>
	void modifyScoreByVelocityConformance( V &input, const Vec3 &velocityDir );
public:
	DodgeHazardProblemSolver( const OriginParams &originParams_, const ProblemParams &problemParams_ );

	int findMany( vec3_t *spots, int numSpots ) override;
};

#endif
