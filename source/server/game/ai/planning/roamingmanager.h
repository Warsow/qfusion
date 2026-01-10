#ifndef WSW_3b769ef7_93e4_4e4a_8593_27a2be5b7485_H
#define WSW_3b769ef7_93e4_4e4a_8593_27a2be5b7485_H

#include "../ailocal.h"
#include <common/types/staticvector.h>

class BotRoamingManager {
	// Note: very large values lead to all spots being considered as visited
	// during the first VISITED_SPOT_EXPIRATION_TIME millis of the level time.
	// Even if TryResetAllSpotsDisabledState() fills visitedAt by zero,
	// levelTime - visitedAt[spotNum] is still below this limit.
	static constexpr unsigned VISITED_SPOT_EXPIRATION_TIME = 10 * 1000;

	int64_t *visitedAt;
	Bot *const bot;
	Vec3 tmpSpotOrigin { 0, 0, 0 };
	Vec3 cachedSpotOrigin { 0, 0, 0 };
	int64_t spotSelectedAt { 0 };
	int currTacticalSpotNum { -1 };
	unsigned numVisitedSpots { 0 };
	const class TacticalSpotsRegistry *tacticalSpotsRegistry;
	const class AiAasWorld *aasWorld;

	using Candidates = wsw::StaticVector<int, 20>;

	const Vec3 &GetRoamingSpot();

	bool IsTemporarilyDisabled( unsigned spotNum ) const {
		return MillisSinceVisited( spotNum, level.time ) < VISITED_SPOT_EXPIRATION_TIME;
	}

	bool IsTemporarilyDisabled( unsigned spotNum, int64_t levelTime ) const {
		return MillisSinceVisited( spotNum, levelTime ) < VISITED_SPOT_EXPIRATION_TIME;
	}

	inline const Vec3 &SetTmpSpotFromArea( int areaNum );
	// Non-negative return values are feasible
	int TrySuggestTacticalSpot();
	// Non-negative return values are feasible
	int TryFindReachableSpot( const Candidates &candidateSpots, const int *fromAreaNums, int numFromAreas );
	// Positive return values are feasible
	int TryFindReachableArea( const Candidates &candidateAreas, const int *fromAreaNums, int numFromAreas );
	void TryResetAllSpotsDisabledState();
	// Positive return values are feasible
	int TrySuggestRandomAasArea();
	int TrySuggestNearbyAasArea();
	bool IsFeasibleArea( const aas_area_t &area, const aas_areasettings_t &areaSettings );

	void ClearVisitedSpots();
public:
	explicit BotRoamingManager( Bot *bot_ );

	~BotRoamingManager() {
		Q_free( visitedAt );
	}

	uint64_t MillisSinceVisited( unsigned spotNum ) const {
		assert( level.time >= visitedAt[spotNum] );
		return (uint64_t)( level.time - visitedAt[spotNum] );
	}

	uint64_t MillisSinceVisited( unsigned spotNum, int64_t levelTime ) const {
		assert( levelTime >= visitedAt[spotNum] );
		return (uint64_t)( levelTime - visitedAt[spotNum] );
	}

	void DisableSpotsInRadius( const Vec3 &origin, float radius ) {
		DisableSpotsInRadius( origin.data(), radius );
	}

	void DisableSpotsInRadius( const vec3_t origin, float radius );

	// All calls during a single frame are guaranteed to return the same result
	const Vec3 &GetCachedRoamingSpot();

	void CheckSpotsProximity();
};

#endif
