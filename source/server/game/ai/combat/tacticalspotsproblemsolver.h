#ifndef WSW_58f972a1_330b_4474_9e59_c6c2fe3825ac_H
#define WSW_58f972a1_330b_4474_9e59_c6c2fe3825ac_H

#include "../bot.h"
#include "tacticalspotsregistry.h"

class TacticalSpotsProblemSolver {
public:
	typedef TacticalSpotsRegistry::TacticalSpot TacticalSpot;
	typedef TacticalSpotsRegistry::OriginParams OriginParams;
	typedef TacticalSpotsRegistry::SpotAndScore SpotAndScore;
	typedef TacticalSpotsRegistry::OriginAndScore OriginAndScore;
	typedef TacticalSpotsRegistry::SpotsQueryVector SpotsQueryVector;
	typedef TacticalSpotsRegistry::SpotsAndScoreVector SpotsAndScoreVector;
	typedef TacticalSpotsRegistry::OriginAndScoreVector OriginAndScoreVector;

	static constexpr auto MAX_SPOTS = TacticalSpotsRegistry::MAX_SPOTS;


	class BaseProblemParams {
		friend class TacticalSpotsProblemSolver;
		friend class AdvantageProblemSolver;
		friend class DodgeHazardProblemSolver;
	protected:
		const TrackedEnemy *enemiesListHead { nullptr };
		const TrackedEnemy *ignoredEnemy { nullptr };
		unsigned lastSeenEnemyMillisThreshold { 5000 };

		float minHeightAdvantageOverOrigin { 0.0f };
		float advantageOverOriginForMaxScore { 128.0f };
		int maxFeasibleTravelTimeMillis { 5000 };
		float spotProximityThreshold { 64.0f };
		bool checkToAndBackReach { false };
		bool optimizeAggressively { false };
	public:
		void setCheckToAndBackReach( bool checkToAndBack ) {
			this->checkToAndBackReach = checkToAndBack;
		}

		void setMinHeightAdvantageOverOrigin( float minHeight ) {
			minHeightAdvantageOverOrigin = minHeight;
		}

		void setAdvantageOverOriginForMaxScore( float threshold ) {
			assert( threshold > 0 );
			advantageOverOriginForMaxScore = threshold;
		}

		void setMaxFeasibleTravelTimeMillis( int millis ) {
			assert( millis > 1 );
			maxFeasibleTravelTimeMillis = millis;
		}

		void setSpotProximityThreshold( float radius ) {
			assert( radius > 0 );
			spotProximityThreshold = radius;
		}

		void setImpactfulEnemies( const TrackedEnemy *listHead_,
			                      const TrackedEnemy *ignoredEnemy_,
							      unsigned lastSeenMillisThreshold_ = 3000u ) {
			this->enemiesListHead = listHead_;
			this->ignoredEnemy = ignoredEnemy_;
			this->lastSeenEnemyMillisThreshold = lastSeenMillisThreshold_;
		}

		void setOptimizeAggressively( bool aggressively ) {
			optimizeAggressively = aggressively;
		}
	};
protected:
	const OriginParams &originParams;
	TacticalSpotsRegistry *const tacticalSpotsRegistry;
	TacticalSpotsRegistry::CriteriaScoresVector &scores;

	std::pair<CriteriaScores *, unsigned> addNextScores() {
		auto *criteriaScores = new( scores.unsafe_grow_back() )CriteriaScores();
		return std::make_pair( criteriaScores, scores.size() - 1u );
	}

	virtual void selectCandidateSpots( const SpotsQueryVector &spotsFromQuery, SpotsAndScoreVector &spots );

	virtual void pruneByReachTablesFromOrigin( SpotsAndScoreVector &spots );

	virtual void checkSpotsReachFromOrigin( SpotsAndScoreVector &spots, unsigned maxSpots );

	virtual void pruneByReachTablesFromOriginAndBack( SpotsAndScoreVector &spots );

	virtual void checkSpotsReachFromOriginAndBack( SpotsAndScoreVector &spots, unsigned maxSpots );

	void pruneByReachTables( SpotsAndScoreVector &spots ) {
		if( problemParams.checkToAndBackReach ) {
			pruneByReachTablesFromOriginAndBack( spots );
		} else {
			pruneByReachTablesFromOrigin( spots );
		}
	}

	void checkSpotsReach( SpotsAndScoreVector &spotsAndScores, unsigned maxResultSpots ) {
		if( problemParams.checkToAndBackReach ) {
			checkSpotsReachFromOriginAndBack( spotsAndScores, maxResultSpots );
		} else {
			checkSpotsReachFromOrigin( spotsAndScores, maxResultSpots );
		}
	}

	virtual void applyEnemiesInfluence( SpotsAndScoreVector &spotsAndScores );

	int makeResultsPruningByProximity( const SpotsAndScoreVector &spotsAndScores, vec3_t *origins, unsigned maxSpots );
	int makeResultsPruningByProximity( const OriginAndScoreVector &originsAndScores, vec3_t *origins, unsigned maxSpots );

	// TODO: We don't need a non-sorting version?
	void sortAndTakeNBestIfOptimizingAggressively( SpotsAndScoreVector &spotsAndScores, unsigned limit ) {
		assert( limit <= MAX_SPOTS );
		if( !problemParams.optimizeAggressively ) {
			return;
		}
		if( spotsAndScores.size() <= limit ) {
			return;
		}
		sort( spotsAndScores );
		spotsAndScores.truncate( limit );
	}

	template <typename SpotLikeVector>
	void sortImpl( SpotLikeVector &v );

	void sort( SpotsAndScoreVector &v );
	void sort( OriginAndScoreVector &v );

	void addSortCriterion( SpotSortCriterion criterion, int distinctGroupsScale ) {
		assert( distinctGroupsScale > 1 );
		criteria.push_back( { criterion, distinctGroupsScale } );
		unsigned bit = 1u << (unsigned)criterion;
		assert( !( addedCriteriaMask & bit ) );
		addedCriteriaMask |= bit;
	}
private:
	const BaseProblemParams &problemParams;

	template <typename SpotsAndScores>
	int makeResultsPruningByProximityImpl( const SpotsAndScores &spotsAndScores, vec3_t *origins, unsigned maxSpots );

	// For debugging
	unsigned addedCriteriaMask { 0 };

	struct AddedCriterion {
		SpotSortCriterion criterion;
		int valueGroupsDistinctionScale;
	};

	// TODO: Use magic_enum
	wsw::StaticVector<AddedCriterion, 9> criteria;
public:
	TacticalSpotsProblemSolver( const OriginParams &originParams_, const BaseProblemParams &problemParams_ )
		: originParams( originParams_ )
		, tacticalSpotsRegistry( TacticalSpotsRegistry::instance )
		, scores( tacticalSpotsRegistry->cleanAndGetCriteriaScoresVector() )
		, problemParams( problemParams_ ) {}

	virtual ~TacticalSpotsProblemSolver() = default;

	virtual bool findSingle( vec3_t spot ) {
		// Assume an address of array of spot origins is the address of the first component of the single vec3_t param
		return findMany( (vec3_t *)&spot[0], 1 ) == 1;
	}

	virtual int findMany( vec3_t *spots, int maxSpots ) = 0;
};





#endif
