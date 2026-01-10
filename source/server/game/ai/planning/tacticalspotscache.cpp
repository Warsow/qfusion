#include "../bot.h"
#include "../combat/tacticalspotsregistry.h"
#include "../combat/advantageproblemsolver.h"
#include "../combat/coverproblemsolver.h"
#include "../combat/dodgehazardproblemsolver.h"

inline const AiAasRouteCache *BotTacticalSpotsCache::RouteCache() {
	return m_bot->RouteCache();
}

inline float BotTacticalSpotsCache::Skill() const {
	return m_bot->Skill();
}

bool BotTacticalSpotsCache::botHasAlmostSameOrigin( const Vec3 &unpackedOrigin ) const {
	return unpackedOrigin.squareDistanceTo( unpackedOrigin ) < 1.0f;
}

void BotTacticalSpotsCache::clear() {
	m_coverSpotsTacticalSpotsCache.clear();

	m_nearbyEntitiesCache.clear();

	m_runAwayTeleportOriginsCache.clear();
	m_runAwayJumppadOriginsCache.clear();
	m_runAwayElevatorOriginsCache.clear();

	m_dodgeHazardSpotsCache.clear();
}

template <typename ProblemParams>
inline bool BotTacticalSpotsCache::findForOrigin( const ProblemParams &problemParams,
												  const Vec3 &origin, float searchRadius, vec3_t result ) {
	if( botHasAlmostSameOrigin( origin ) ) {
		// Provide a bot entity to aid trace checks
		AdvantageProblemSolver::OriginParams originParams( game.edicts + m_bot->EntNum(), searchRadius, RouteCache() );
		return AdvantageProblemSolver( originParams, problemParams ).findSingle( result );
	}
	TacticalSpotsRegistry::OriginParams originParams( origin.data(), searchRadius, RouteCache() );
	return AdvantageProblemSolver( originParams, problemParams ).findSingle( result );
}

template <typename Result, typename Cache, typename Method, typename... Args>
auto BotTacticalSpotsCache::getThroughCache( Cache *cache, Method method, const Args &...args )
	-> std::optional<Result> {
	const typename Cache::CachedArg unpacked[] { args... };

	if( std::optional<Result> cached = cache->tryGettingCached( std::begin( unpacked ), std::end( unpacked ) ) ) {
		return cached;
	}

	if( cache->m_entries.full() ) {
		return std::nullopt;
	}

	void *const mem   = cache->m_entries.unsafe_grow_back();
	auto *const entry = new( mem )typename Cache::Entry;

	std::optional<Result> result = ( this->*method )( args... );
	entry->validForArgs.insert( entry->validForArgs.begin(), std::begin( unpacked ), std::end( unpacked ) );
	entry->payload = result;

	return result;
}

[[nodiscard]]
auto BotTacticalSpotsCache::getSingleOriginSpot( SingleOriginSpotsCache *cachedSpots, const Vec3 &botOrigin,
												 const Vec3 &enemyOrigin, FindSingleOriginMethod findMethod )
												 -> std::optional<Vec3> {
	return getThroughCache<Vec3, SingleOriginSpotsCache, FindSingleOriginMethod, Vec3, Vec3>(
		cachedSpots, findMethod, botOrigin, enemyOrigin );
}

[[nodiscard]]
auto BotTacticalSpotsCache::getDualOriginSpot( DualOriginSpotsCache *cachedSpots, const Vec3 &botOrigin,
											   const Vec3 &enemyOrigin, FindDualOriginMethod findMethod )
											   -> std::optional<DualOrigin> {
	return getThroughCache<DualOrigin, DualOriginSpotsCache, FindDualOriginMethod, Vec3, Vec3>(
		cachedSpots, findMethod, botOrigin, enemyOrigin );
}

[[nodiscard]]
auto BotTacticalSpotsCache::getDodgeHazardSpot( const Vec3 &botOrigin, const Vec3 &hazardHitPoint, const Vec3 &hazardDir,
												bool isHazardSplashLike ) -> std::optional<Vec3> {
	auto method = &BotTacticalSpotsCache::findDodgeHazardSpot;
	return getThroughCache<Vec3, DodgeHazardSpotsCache, decltype( method ), Vec3, Vec3, Vec3, bool>(
		&m_dodgeHazardSpotsCache, method, botOrigin, hazardHitPoint, hazardDir, isHazardSplashLike );
}

template <typename ProblemParams>
inline void BotTacticalSpotsCache::takeEnemiesIntoAccount( ProblemParams &problemParams ) {
	if( Skill() < 0.33f ) {
		return;
	}
	const std::optional<SelectedEnemy> &selectedEnemy = m_bot->GetSelectedEnemy();
	if( !selectedEnemy ) {
		return;
	}
	const TrackedEnemy *listHead = m_bot->TrackedEnemiesHead();
	const TrackedEnemy *ignored  = selectedEnemy->GetTrackedEnemy();
	problemParams.setImpactfulEnemies( listHead, ignored );
}

auto BotTacticalSpotsCache::findCoverSpot( const Vec3 &origin, const Vec3 &enemyOrigin ) -> std::optional<Vec3> {
	const float searchRadius = kLasergunRange;
	CoverProblemSolver::ProblemParams problemParams( enemyOrigin.data(), 32.0f );
	problemParams.setMinHeightAdvantageOverOrigin( -searchRadius );
	problemParams.setCheckToAndBackReach( false );
	problemParams.setMaxFeasibleTravelTimeMillis( 1250 );
	takeEnemiesIntoAccount( problemParams );

	vec3_t result;
	if( botHasAlmostSameOrigin( origin ) ) {
		TacticalSpotsRegistry::OriginParams originParams( game.edicts + m_bot->EntNum(), searchRadius, RouteCache() );
		if( CoverProblemSolver( originParams, problemParams ).findSingle( result ) ) {
			return Vec3( result );
		}
	}

	TacticalSpotsRegistry::OriginParams originParams( origin.data(), searchRadius, RouteCache() );
	if( CoverProblemSolver( originParams, problemParams ).findSingle( result ) ) {
		return Vec3( result );
	}

	return std::nullopt;
}

auto BotTacticalSpotsCache::findDodgeHazardSpot( const Vec3 &botOrigin, const Vec3 &hazardHitPoint, const Vec3 &hazardDir,
												 bool isHazardSplashLike ) -> std::optional<Vec3> {
	DodgeHazardProblemSolver::OriginParams originParams( game.edicts + m_bot->EntNum(), 256.0f, m_bot->RouteCache() );
	DodgeHazardProblemSolver::ProblemParams problemParams( hazardHitPoint, hazardDir, isHazardSplashLike );
	problemParams.setCheckToAndBackReach( false );
	problemParams.setMinHeightAdvantageOverOrigin( -64.0f );
	// Influence values are quite low because evade direction factor must be primary
	problemParams.setMaxFeasibleTravelTimeMillis( 2500 );

	vec3_t spotOrigin;
	if( DodgeHazardProblemSolver( originParams, problemParams ).findSingle( spotOrigin ) ) {
		return Vec3( spotOrigin );
	}

	return std::nullopt;
}

auto BotTacticalSpotsCache::findNearbyEntities( const Vec3 &origin, float radius ) -> std::span<const uint16_t> {
	if( const NearbyEntitiesCache::Entry *cacheEntry = m_nearbyEntitiesCache.tryGettingCached( origin, radius ) ) {
		return { cacheEntry->entNums, cacheEntry->numEntities };
	}

	NearbyEntitiesCache::Entry *const cacheEntry = m_nearbyEntitiesCache.tryAlloc( origin.data(), radius );
	if( !cacheEntry ) {
		return {};
	}

	// Find more entities in radius than we can store (most entities will usually be filtered out)

	int radiusEntNums[64];
	assert( std::size( cacheEntry->entNums ) < std::size( radiusEntNums ) );
	int numRadiusEntities = GClip_FindInRadius( origin.data(), radius, radiusEntNums, std::size( radiusEntNums ) );
	// Note that this value might be greater than the buffer capacity (an actual number of entities is returned)
	numRadiusEntities = wsw::min<int>( numRadiusEntities, std::size( radiusEntNums ) );

	assert( cacheEntry->numEntities == 0 );
	const auto *const gameEnts = game.edicts;
	for( int i = 0; i < numRadiusEntities; ++i ) {
		const auto *const ent = gameEnts + radiusEntNums[i];
		if( ent->r.inuse && ent->classname && ent->touch ) {
			if( !cacheEntry->tryAddingNext( radiusEntNums[i] ) ) [[unlikely]] {
				break;
			}
		}
	}

	return{ cacheEntry->entNums, cacheEntry->numEntities };
}

void BotTacticalSpotsCache::findReachableClassEntities( const Vec3 &origin, float radius, const char *classname,
														BotTacticalSpotsCache::ReachableEntities &result ) {
	const std::span<const uint16_t> entNums = findNearbyEntities( origin, radius );

	ReachableEntities candidateEntities;
	const auto *gameEnts = game.edicts;

	for( const int entNum: entNums ) {
		const auto *const ent = gameEnts + entNum;
		if( !Q_stricmp( ent->classname, classname ) ) {
			const float distance = DistanceFast( origin.data(), ent->s.origin );
			candidateEntities.push_back( EntAndScore( entNum, radius - distance ) );
			if( candidateEntities.full() ) [[unlikely]] {
				break;
			}
		}
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto *routeCache = RouteCache();

	int fromAreaNums[2] { 0, 0 };
	int numFromAreas;
	// If an origin matches actual bot origin
	if( botHasAlmostSameOrigin( origin ) ) {
		numFromAreas = m_bot->EntityPhysicsState()->PrepareRoutingStartAreas( fromAreaNums );
	} else {
		fromAreaNums[0] = aasWorld->findAreaNum( origin );
		numFromAreas = fromAreaNums[0] ? 1 : 0;
	}

	for( EntAndScore &candidate: candidateEntities ) {
		const auto *const ent = gameEnts + candidate.entNum;

		const int toAreaNum = findMostFeasibleEntityAasArea( ent, aasWorld );
		if( !toAreaNum ) {
			continue;
		}

		const int travelTime = routeCache->FindRoute( fromAreaNums, numFromAreas, toAreaNum, m_bot->TravelFlags() );
		if( !travelTime ) {
			continue;
		}

		// AAS travel time is in seconds^-2
		const float factor = Q_Sqrt( 1.01f - wsw::min( travelTime, 200 ) * Q_Rcp( 200 ) );
		result.push_back( EntAndScore( candidate.entNum, candidate.score * factor ) );
	}

	// Sort entities so best entities are first
	wsw::sortByFieldDescending( result.begin(), result.end(), &EntAndScore::score );
}

int BotTacticalSpotsCache::findMostFeasibleEntityAasArea( const edict_t *ent, const AiAasWorld *aasWorld ) const {
	int areaNumsBuffer[24];
	const Vec3 boxMins( Vec3( -20, -20, -12 ) + ent->r.absmin );
	const Vec3 boxMaxs( Vec3( +20, +20, +12 ) + ent->r.absmax );
	const auto boxAreaNums = aasWorld->findAreasInBox( boxMins.data(), boxMaxs.data(), areaNumsBuffer, 24 );

	const auto aasAreaSettings = aasWorld->getAreaSettings();
	for( const int areaNum : boxAreaNums ) {
		const int areaFlags = aasAreaSettings[areaNum].areaflags;
		if( !( areaFlags & AREA_GROUNDED ) ) {
			continue;
		}
		if( areaFlags & AREA_DISABLED ) {
			continue;
		}
		return areaNum;
	}
	return 0;
}

auto BotTacticalSpotsCache::findRunAwayTeleportOrigin( const Vec3 &origin, const Vec3 &enemyOrigin )
	-> std::optional<DualOrigin> {
	ReachableEntities reachableEntities;
	findReachableClassEntities( origin, kLasergunRange, "trigger_teleport", reachableEntities );

	const auto *const pvsCache = EntitiesPvsCache::Instance();
	const auto *const enemyEnt = m_bot->GetSelectedEnemy().value().Ent();
	const auto *const gameEnts = game.edicts;
	for( const auto &entAndScore: reachableEntities ) {
		const auto *const ent = gameEnts + entAndScore.entNum;
		if( !ent->target ) {
			continue;
		}
		const auto *const dest = G_Find( nullptr, FOFS( targetname ), ent->target );
		if( !dest ) {
			continue;
		}

		if( !pvsCache->AreInPvs( enemyEnt, dest ) ) {
			continue;
		}

		trace_t trace;
		G_Trace( &trace, enemyEnt->s.origin, nullptr, nullptr, dest->s.origin, enemyEnt, MASK_AISOLID );
		if( trace.fraction == 1.0f || trace.ent > 0 ) {
			continue;
		}

		const Vec3 movesFrom( ent->s.origin ), movesTo( dest->s.origin );
		return std::make_pair( movesFrom, movesTo );
	}

	return std::nullopt;
}

auto BotTacticalSpotsCache::findRunAwayJumppadOrigin( const Vec3 &origin, const Vec3 &enemyOrigin )
	-> std::optional<DualOrigin> {
	ReachableEntities reachableEntities;
	findReachableClassEntities( origin, kLasergunRange, "trigger_push", reachableEntities );

	const auto *const pvsCache = EntitiesPvsCache::Instance();
	const auto *const enemyEnt = m_bot->GetSelectedEnemy().value().Ent();
	const auto *const gameEnts = game.edicts;
	for( const auto &entAndScore: reachableEntities ) {
		const auto *const ent = gameEnts + entAndScore.entNum;
		if( !pvsCache->AreInPvs( enemyEnt, ent ) ) {
			continue;
		}

		trace_t trace;
		G_Trace( &trace, enemyEnt->s.origin, nullptr, nullptr, ent->target_ent->s.origin, enemyEnt, MASK_AISOLID );
		if( trace.fraction == 1.0f || trace.ent > 0 ) {
			continue;
		}

		const Vec3 movesFrom( ent->s.origin ), movesTo( ent->target_ent->s.origin );
		return std::make_pair( movesFrom, movesTo );
	}

	return std::nullopt;
}

auto BotTacticalSpotsCache::findRunAwayElevatorOrigin( const Vec3 &origin, const Vec3 &enemyOrigin )
	-> std::optional<DualOrigin> {
	ReachableEntities reachableEntities;
	findReachableClassEntities( origin, kLasergunRange, "func_plat", reachableEntities );

	const auto *const pvsCache = EntitiesPvsCache::Instance();
	const auto *const enemyEnt = m_bot->GetSelectedEnemy().value().Ent();
	const auto *const gameEnts = game.edicts;
	for( const auto &entAndScore: reachableEntities ) {
		const auto *const ent = gameEnts + entAndScore.entNum;
		// Can't run away via elevator if the elevator has been always activated
		if( ent->moveinfo.state != STATE_BOTTOM ) {
			continue;
		}

		if( !pvsCache->AreInPvs( enemyEnt, ent ) ) {
			continue;
		}

		trace_t trace;
		G_Trace( &trace, enemyEnt->s.origin, nullptr, nullptr, ent->moveinfo.end_origin, enemyEnt, MASK_AISOLID );
		if( trace.fraction == 1.0f || trace.ent > 0 ) {
			continue;
		}

		// Copy trigger origin
		Vec3 movesFrom( ent->s.origin );
		// Drop origin to the elevator bottom
		movesFrom.z() = ent->r.absmin[2] + 16;
		Vec3 movesTo( ent->moveinfo.end_origin );
		return std::make_pair( movesFrom, movesTo );
	}

	return std::nullopt;
}