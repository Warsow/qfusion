#ifndef WSW_65a80be2_b9c9_4018_9d25_db1f7e268aef_H
#define WSW_65a80be2_b9c9_4018_9d25_db1f7e268aef_H

#include "../ailocal.h"

#include <variant>

class BotTacticalSpotsCache {
public:
	explicit BotTacticalSpotsCache( Bot *bot ) : m_bot( bot ) {}

	void clear();

	[[nodiscard]]
	auto getCoverSpot( const Vec3 &botOrigin, const Vec3 &enemyOrigin ) -> std::optional<Vec3> {
		return getSingleOriginSpot( &m_coverSpotsTacticalSpotsCache, botOrigin, enemyOrigin,
									&BotTacticalSpotsCache::findCoverSpot );
	}

	[[nodiscard]]
	auto getDodgeHazardSpot( const Vec3 &botOrigin, const Vec3 &hazardHitPoint, const Vec3 &hazardDir,
							 bool isHazardSplashLike ) -> std::optional<Vec3>;

	using DualOrigin = std::pair<Vec3, Vec3>;

	[[nodiscard]]
	auto getRunAwayTeleportOrigin( const Vec3 &botOrigin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin> {
		return getDualOriginSpot( &m_runAwayTeleportOriginsCache, botOrigin, enemyOrigin,
								  &BotTacticalSpotsCache::findRunAwayTeleportOrigin );
	}

	[[nodiscard]]
	auto getRunAwayJumppadOrigin( const Vec3 &botOrigin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin> {
		return getDualOriginSpot( &m_runAwayJumppadOriginsCache, botOrigin, enemyOrigin,
								  &BotTacticalSpotsCache::findRunAwayJumppadOrigin );
	}

	[[nodiscard]]
	auto getRunAwayElevatorOrigin( const Vec3 &botOrigin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin> {
		return getDualOriginSpot( &m_runAwayElevatorOriginsCache, botOrigin, enemyOrigin,
								  &BotTacticalSpotsCache::findRunAwayElevatorOrigin );
	}
private:
	// Use just a plain array for caching spots.
	// High number of distinct (and thus searched for) spots will kill TacticalSpotsRegistry performance first.
	template <typename Payload, unsigned MaxCachedSpots, unsigned CapacityOfArgs = 2>
	struct SpotsCache {
		// Should be std::any, but it is very likely to allocate Vec3 copies on heap
		using CachedArg = std::variant<Vec3, unsigned, int, float, bool>;

		struct Entry {
			wsw::StaticVector<CachedArg, CapacityOfArgs> validForArgs;
			std::optional<Payload> payload;
		};

		wsw::StaticVector<Entry, MaxCachedSpots> m_entries;

		void clear() { m_entries.clear(); }

		[[nodiscard]]
		auto tryGettingCached( const CachedArg *argsBegin, const CachedArg *argsEnd ) const -> std::optional<Payload> {
			for( const Entry &entry: m_entries ) {
				assert( (size_t)( argsEnd - argsBegin ) == entry.validForArgs.size() );
				// TODO: Use a lightweight <algorithm> replacement
				bool matches = true;
				for( unsigned i = 0; i < entry.validForArgs.size(); ++i ) {
					if( entry.validForArgs[i] != *( argsBegin + i ) ) {
						matches = false;
						break;
					}
				}
				if( matches ) {
					return entry.payload;
				}
			}
			return std::nullopt;
		}
	};

	Bot *const m_bot;

	typedef SpotsCache<Vec3, 1> SingleOriginSpotsCache;
	SingleOriginSpotsCache m_coverSpotsTacticalSpotsCache;

	typedef SpotsCache<DualOrigin, 1> DualOriginSpotsCache;
	DualOriginSpotsCache m_runAwayTeleportOriginsCache;
	DualOriginSpotsCache m_runAwayJumppadOriginsCache;
	DualOriginSpotsCache m_runAwayElevatorOriginsCache;

	typedef SpotsCache<Vec3, 1, 4> DodgeHazardSpotsCache;
	DodgeHazardSpotsCache m_dodgeHazardSpotsCache;

	[[nodiscard]]
	auto findCoverSpot( const Vec3 &origin, const Vec3 &enemyOrigin ) -> std::optional<Vec3>;

	[[nodiscard]]
	auto findDodgeHazardSpot( const Vec3 &botOrigin, const Vec3 &hazardHitPoint, const Vec3 &hazardDir,
							  bool isHazardSplashLike ) -> std::optional<Vec3>;

	template <typename ProblemParams>
	inline void takeEnemiesIntoAccount( ProblemParams &problemParams );

	// We can't(?) refer to a nested class in a forward declaration, so declare the parameter as a template one
	template <typename ProblemParams>
	inline bool findForOrigin( const ProblemParams &problemParams, const Vec3 &origin, float searchRadius, vec3_t result );

	[[nodiscard]]
	auto findRunAwayTeleportOrigin( const Vec3 &origin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin>;
	[[nodiscard]]
	auto findRunAwayJumppadOrigin( const Vec3 &origin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin>;
	[[nodiscard]]
	auto findRunAwayElevatorOrigin( const Vec3 &origin, const Vec3 &enemyOrigin ) -> std::optional<DualOrigin>;

	using ReachableEntities = wsw::StaticVector<EntAndScore, 16>;
	void findReachableClassEntities( const Vec3 &origin, float radius, const char *classname, ReachableEntities &result );

	// AiAasWorld::findAreaNum() fails so often for teleports/elevators, etc, so we have to use this method.
	// AiAasWorld is provided as an argument to avoid an implicit retrieval of global instance in a loop.
	// TODO: Reuse trigger area nums cache!
	int findMostFeasibleEntityAasArea( const edict_t *ent, const AiAasWorld *aasWorld ) const;

	struct NearbyEntitiesCache {
		struct Entry {
			uint16_t entNums[32];
			unsigned numEntities { 0 };
			float radius { 0.0f };
			vec3_t botOrigin;

			bool tryAddingNext( int entNum ) {
				assert( entNum > 0 && entNum < MAX_EDICTS );
				if( numEntities != std::size( entNums ) ) {
					entNums[numEntities++] = (uint16_t)entNum;
					return true;
				}
				return false;
			}
		};

		void clear() { m_entries.clear(); }

		[[nodiscard]]
		auto tryAlloc( const float *botOrigin, float radius ) -> Entry * {
			if( !m_entries.full() ) {
				Entry *e       = m_entries.unsafe_grow_back();
				e->numEntities = 0;
				e->radius      = radius;
				VectorCopy( botOrigin, e->botOrigin );
				return e;
			}
			return nullptr;
		}

		[[nodiscard]]
		auto tryGettingCached( const Vec3 &origin, float radius ) const -> const Entry * {
			for( const Entry &entry: m_entries ) {
				if( VectorCompare( entry.botOrigin, origin.data() ) && entry.radius == radius ) {
					return std::addressof( entry );
				}
			}
			return nullptr;
		}

		wsw::StaticVector<Entry, 3> m_entries;
	};

	NearbyEntitiesCache m_nearbyEntitiesCache;

	[[nodiscard]]
	auto findNearbyEntities( const Vec3 &origin, float radius ) -> std::span<const uint16_t>;

	// These functions are extracted to be able to mock a bot entity
	// by a player entity easily for testing and tweaking the cache
	inline const class AiAasRouteCache *RouteCache();
	inline float Skill() const;

	[[nodiscard]]
	bool botHasAlmostSameOrigin( const Vec3 &origin ) const;

	template <typename Result, typename Cache, typename Method, typename... Args>
	[[nodiscard]]
	auto getThroughCache( Cache *cache, Method method, const Args &...args ) -> std::optional<Result>;

	typedef std::optional<Vec3> ( BotTacticalSpotsCache::*FindSingleOriginMethod )( const Vec3 &, const Vec3 & );

	[[nodiscard]]
	auto getSingleOriginSpot( SingleOriginSpotsCache *cachedSpots, const Vec3 &botOrigin,
							  const Vec3 &enemyOrigin, FindSingleOriginMethod findMethod ) -> std::optional<Vec3>;

	typedef std::optional<DualOrigin> ( BotTacticalSpotsCache::*FindDualOriginMethod )( const Vec3 &, const Vec3 & );

	[[nodiscard]]
	auto getDualOriginSpot( DualOriginSpotsCache *cachedSpots, const Vec3 &botOrigin,
							const Vec3 &enemyOrigin, FindDualOriginMethod findMethod ) -> std::optional<DualOrigin>;

};

#endif
