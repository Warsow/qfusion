#include "squadbasedteam.h"
#include "objectivebasedteam.h"
#include "teamplaylocal.h"
#include "../groundtracecache.h"
#include "../bot.h"
#include "../../../qcommon/links.h"

#include <limits>
#include <cmath>
#include <cstdlib>
#include <algorithm>

/**
 * While {@code AiAasRouteCache} is fairly efficient at retrieval of cached results,
 * we can use retrieval of a cached travel time at cost of a single branch for this specialized cache.
 * Moreover, travel times must be computed using routing caches of bot clients whether it's possible
 * for a correct handling of current preferred/allowed travel flags and blocked areas specific to a bot.
 */
class ClientToClientTable {
	mutable int aasTravelTimes[MAX_CLIENTS * MAX_CLIENTS];

	/**
	 * A common entry point for every accessor that wraps it.
	 * Actually tries to fetch a cached value.
	 */
	int GetTravelTime( int fromEntNum, int toEntNum ) const;

	/**
	 * Actually performs travel time computations.
	 * @note result of this method is not symmetrical in relation to arguments swapping.
	 */
	int FindTravelTime( int fromEntNum, int toEntNum ) const;

	/**
	 * Tries to find an entity area num, possibly dropping entity to floor.
	 * @param ent an entity to find an area for
	 * @param areaNums a buffer for result areas, must be capable to store 2 areas.
	 * @return a number of found areas (up to 2).
	 */
	int FindEntityAreas( const edict_t *ent, int *areaNums ) const;
public:
	ClientToClientTable() {
		std::fill( aasTravelTimes, aasTravelTimes + MAX_CLIENTS, -1 );
	}

	/**
	 * Must be called every game frame. Invalidates cached results.
	 */
	void Update() {
		// negative values mean that a value should be lazily computed on demand
		std::fill( aasTravelTimes, aasTravelTimes + MAX_CLIENTS * MAX_CLIENTS, -1 );
	}

	int GetTravelTime( const edict_t *fromEnt, const edict_t *toEnt ) const {
		return GetTravelTime( ENTNUM( fromEnt ), ENTNUM( toEnt ) );
	}
	int GetTravelTime( const Client *fromClient, const Client *toClient ) const {
		return GetTravelTime( (int)( fromClient - game.clients ) + 1, (int)( toClient - game.clients ) + 1 );
	}
	int GetTravelTime( const Bot *fromBot, const Bot *toBot ) const {
		return GetTravelTime( ENTNUM( fromBot->Self() ), ENTNUM( toBot->Self() ) );
	}
};

int ClientToClientTable::GetTravelTime( int fromEntNum, int toEntNum ) const {
#ifdef _DEBUG
	constexpr const char *tag = "ClientToClientTable::GetTravelTime()";
	if( fromEntNum < 1 || fromEntNum > gs.maxclients ) {
		AI_FailWith( tag, "`fromEntNum` #%d does not correspond to a client\n", fromEntNum );
	}
	if( toEntNum < 1 || toEntNum > gs.maxclients ) {
		AI_FailWith( tag, "`toEntNum` #%d does not correspond to a client\n", fromEntNum );
	}
#endif
	int *const valuePtr = &aasTravelTimes[( fromEntNum - 1 ) * MAX_CLIENTS + ( toEntNum - 1 )];
	if( *valuePtr < 0 ) {
		*valuePtr = FindTravelTime( fromEntNum, toEntNum );
	}
	return *valuePtr;
}

int ClientToClientTable::FindTravelTime( int fromEntNum, int toEntNum ) const {
	const AiAasRouteCache *routeCache;

	int fromAreaNums[2] = { 0, 0 };
	int numFromAreas;

	int toAreaNums[2] = { 0, 0 };
	int numToAreas;

	const edict_t *const fromEnt = game.edicts + fromEntNum;
	if( Bot *const bot = fromEnt->bot ) {
		routeCache = bot->RouteCache();
		numFromAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( fromAreaNums );
	} else {
		routeCache = AiAasRouteCache::Shared();
		numFromAreas = FindEntityAreas( fromEnt, fromAreaNums );
	}

	const edict_t *const toEnt = game.edicts + toEntNum;
	if( Bot *const bot = toEnt->bot ) {
		numToAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( toAreaNums );
	} else {
		numToAreas = FindEntityAreas( toEnt, toAreaNums );
	}

	// AAS routines return 0 on failure (1 is the minimal feasible travel time)
	int bestTravelTime = 0;
	for( int i = 0; i < numToAreas; ++i ) {
		if( const int travelTime = routeCache->PreferredRouteToGoalArea( fromAreaNums, numFromAreas, toAreaNums[i] ) ) {
			if( bestTravelTime && travelTime > bestTravelTime ) {
				continue;
			}
			bestTravelTime = travelTime;
		}
	}

	return bestTravelTime;
}

int ClientToClientTable::FindEntityAreas( const edict_t *ent, int *areaNums ) const {
	const auto *aasWorld = AiAasWorld::instance();
	int numResultAreas = 0;
	int areaNum = aasWorld->findAreaNum( ent );
	if( areaNum ) {
		areaNums[numResultAreas++] = areaNum;
		// If the first area already has ground
		if( aasWorld->isAreaGrounded( areaNum ) ) {
			return numResultAreas;
		}
	}

	vec3_t tmpOrigin;
	if( AiGroundTraceCache::Instance()->TryDropToFloor( ent, 64.0f, tmpOrigin ) ) {
		const int droppedAreaNum = aasWorld->findAreaNum( tmpOrigin );
		if( droppedAreaNum && droppedAreaNum != areaNum ) {
			areaNums[numResultAreas++] = droppedAreaNum;
		}
	}

	return numResultAreas;
}

static ClientToClientTable clientToClientTable;

AiSquad::AiSquad( AiSquadBasedTeam *parent_ )
	: parent( parent_ ) {
	std::fill_n( lastDroppedByBotTimestamps, MAX_SQUAD_SIZE, 0 );
	std::fill_n( lastDroppedForBotTimestamps, MAX_SQUAD_SIZE, 0 );
}

void AiSquad::DetachBots() {
	if( botsDetached ) {
		return;
	}
	Bot *nextBot;
	for( Bot *bot = botsListHead; bot; bot = nextBot ) {
		nextBot = bot->NextInSquad();
		bot->OnDetachedFromSquad( this );
		parent->AddToOrphansList( bot );
	}
	botsListHead = nullptr;
	botsDetached = true;
}

bool AiSquad::IsSupporter( const edict_t *bot ) const {
	if( !isValid ) {
		return false;
	}

	for( Bot *otherBot = botsListHead; otherBot; otherBot = otherBot->NextInSquad() ) {
		if( otherBot->Self() != bot ) {
			if( !otherBot->IsGhosting() && IsCarrier( otherBot->Self() ) ) {
				return !IsCarrier( bot );
			}
		}
	}
	return false;
}

/**
 * A squad connectivity should be restored in this limit of time, otherwise a squad should be invalidated
 */
constexpr unsigned CONNECTIVITY_TIMEOUT = 750;
/**
 * This value defines a distance limit for quick rejection of non-feasible bot pairs for new squads
 */
constexpr float CONNECTIVITY_PROXIMITY = 256.0f;
/**
 * This value defines summary AAS movement time limit from one bot to other
 */
constexpr int CONNECTIVITY_MOVE_CENTISECONDS = 100;

void AiSquad::Think() {
	if( !isValid ) {
		return;
	}

	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		if( bot->IsGhosting() ) {
			Invalidate();
			return;
		}
	}

	// Keep validating the bots list
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			if( bot == otherBot ) {
				AI_FailWith( "AiSquad::Think()", "The bots list is malformed (there are duplicates)" );
			}
		}
	}

	canMoveTogether = CheckCanMoveTogether();
	canFightTogether = CheckCanFightTogether();

	if( canMoveTogether || canFightTogether ) {
		brokenConnectivityTimeoutAt = level.time + CONNECTIVITY_TIMEOUT;
	} else if( brokenConnectivityTimeoutAt <= level.time ) {
		Invalidate();
	}

	if( !isValid ) {
		return;
	}

#if 0
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		const int colors[3] = { COLOR_RGB( 128, 0, 0 ), COLOR_RGB( 0, 128, 0 ), COLOR_RGB( 0, 0, 128 ) };
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			AITools_DrawColorLine( bot->Origin(), otherBot->Origin(), colors[Num() % 3], 0 );
		}
	}
#endif

	CheckMembersInventory();
}

bool AiSquad::CheckCanMoveTogether() const {
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			if( DistanceSquared( bot->Origin(), otherBot->Origin() ) > CONNECTIVITY_PROXIMITY * CONNECTIVITY_PROXIMITY ) {
				return false;
			}
		}
	}

	// Test for a possible cheap shortcut using floor cluster nums of the bots
	if( const auto firstClusterNum = GetBotFloorCluster( botsListHead ) ) {
		// Check whether all bots are in the same floor cluster
		Bot *nextBot = botsListHead->NextInSquad();
		for(; nextBot; nextBot = nextBot->NextInSquad() ) {
			if( firstClusterNum != GetBotFloorCluster( nextBot ) ) {
				break;
			}
		}

		// All bots are in the same floor cluster.
		// This does not really guarantee they are reachable to each other in CONNECTIVITY_MOVE_CENTISECONDS.
		// However interrupting at this produces satisfiable results.
		if( !nextBot ) {
			return true;
		}
	}

	// Check whether each bot is reachable for at least a single other bot
	// or may reach at least a single other bot
	// (some reachabilities such as teleports are not reversible)
	int aasTravelTime;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			// Check direct travel time (it's given in seconds^-2)
			aasTravelTime = ::clientToClientTable.GetTravelTime( bot, otherBot );
			// At least bot j is reachable from bot i, move to next bot
			if( aasTravelTime && aasTravelTime < CONNECTIVITY_MOVE_CENTISECONDS ) {
				continue;
			}
			// Bot j is not reachable from bot i, check travel time from j to i
			aasTravelTime = ::clientToClientTable.GetTravelTime( otherBot, bot );
			if( !aasTravelTime || aasTravelTime >= CONNECTIVITY_MOVE_CENTISECONDS ) {
				return false;
			}
		}
	}
	return true;
}

bool AiSquad::CheckCanFightTogether() const {
	// Just check that each bot is visible for each other one
	// TODO: share teammates visibility checks with /awareness stuff?

	trace_t trace;
	const auto *pvsCache = EntitiesPvsCache::Instance();
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			auto *const firstEnt = const_cast<edict_t*>( bot->Self() );
			auto *const secondEnt = const_cast<edict_t*>( otherBot->Self() );
			if( !pvsCache->AreInPvs( firstEnt, secondEnt ) ) {
				return false;
			}

			G_Trace( &trace, firstEnt->s.origin, nullptr, nullptr, secondEnt->s.origin, firstEnt, MASK_AISOLID );
			if( trace.fraction != 1.0f && trace.ent != ENTNUM( secondEnt ) ) {
				return false;
			}
		}
	}

	return true;
}

int AiSquad::GetBotFloorCluster( Bot *bot ) const {
	const std::span<const uint16_t> aasFloorClusters = AiAasWorld::instance()->areaFloorClusterNums();
	// Zero area nums are handled by the dummy zero cluster num for the zero area
	if( int clusterNum = aasFloorClusters[bot->EntityPhysicsState()->CurrAasAreaNum()] ) {
		return clusterNum;
	}
	if( int clusterNum = aasFloorClusters[bot->EntityPhysicsState()->DroppedToFloorAasAreaNum()] ) {
		return clusterNum;
	}
	return 0;
}

class ExtendedWeaponDefsCache {
	int tiersForWeapon[WEAP_TOTAL];
	int sortedByTierWeapons[WEAP_TOTAL];
	const gs_weapon_definition_t *weaponDefs[WEAP_TOTAL];

	mutable bool initialized { false };

	void EnsureInitialized() const {
		if( !initialized ) {
			const_cast<ExtendedWeaponDefsCache *>( this )->Initialize();
			initialized = true;
		}
	}

	void Initialize();
public:
	/**
	 * Gets cached weapon definitions (useful for future changes)
	 */
	const gs_weapon_definition_t *const *WeaponDefs() const {
		EnsureInitialized();
		return &weaponDefs[0];
	}

	/**
	 * Gets an array of tiers for every weapon (addressed by weapon number)
	 */
	const int *WeaponTiers() const {
		EnsureInitialized();
		return tiersForWeapon;
	}

	/**
	 * Gets an array of all known weapon numbers sorted by their tiers (worst first)
	 */
	const int *SortedByTierWeapons() const {
		EnsureInitialized();
		return sortedByTierWeapons;
	}
};

static ExtendedWeaponDefsCache extendedWeaponDefsCache;

void ExtendedWeaponDefsCache::Initialize() {
	struct WeaponAndTier {
		int weapon, tier;
		WeaponAndTier(): weapon( 0 ), tier( 0 ) {}
		WeaponAndTier( int weapon_, int tier_ ) : weapon( weapon_ ), tier( tier_ ) {}
		bool operator<( const WeaponAndTier &that ) const { return tier > that.tier; }
	};

	WeaponAndTier weaponTiers[WEAP_TOTAL];

	weaponTiers[WEAP_NONE]            = WeaponAndTier( WEAP_NONE, 0 );
	weaponTiers[WEAP_GUNBLADE]        = WeaponAndTier( WEAP_GUNBLADE, 0 );
	weaponTiers[WEAP_GRENADELAUNCHER] = WeaponAndTier( WEAP_GRENADELAUNCHER, 1 );
	weaponTiers[WEAP_RIOTGUN]         = WeaponAndTier( WEAP_RIOTGUN, 1 );
	weaponTiers[WEAP_MACHINEGUN]      = WeaponAndTier( WEAP_MACHINEGUN, 2 );
	weaponTiers[WEAP_PLASMAGUN]       = WeaponAndTier( WEAP_PLASMAGUN, 2 );
	weaponTiers[WEAP_LASERGUN]        = WeaponAndTier( WEAP_LASERGUN, 3 );
	weaponTiers[WEAP_ROCKETLAUNCHER]  = WeaponAndTier( WEAP_ROCKETLAUNCHER, 3 );
	weaponTiers[WEAP_ELECTROBOLT]     = WeaponAndTier( WEAP_ELECTROBOLT, 3 );
	weaponTiers[WEAP_INSTAGUN]        = WeaponAndTier( WEAP_INSTAGUN, 3 );

	static_assert( WEAP_NONE == 0, "This loop assumes zero lower bound" );
	for( int weapon = WEAP_NONE; weapon < WEAP_TOTAL; ++weapon ) {
		tiersForWeapon[weapon] = weaponTiers[weapon].tier;
	}

	std::sort( weaponTiers, weaponTiers + WEAP_TOTAL );
	for( int i = 0; i < WEAP_TOTAL; ++i ) {
		sortedByTierWeapons[i] = weaponTiers[i].weapon;
	}

	for( int weapon = WEAP_NONE; weapon < WEAP_TOTAL; ++weapon ) {
		weaponDefs[weapon] = GS_GetWeaponDef( weapon );
	}
}

int AiSquad::FindBotWeaponsTiers( int *maxBotWeaponTiers ) const {
	const auto *const weaponDefs = extendedWeaponDefsCache.WeaponDefs();
	const auto *const tiersForWeapon = extendedWeaponDefsCache.WeaponTiers();

	std::fill_n( maxBotWeaponTiers, MAX_CLIENTS, 0 );
	// Lowest best weapon tier among all squad bots
	int minBotWeaponTier = 3;

	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		auto *const botTierPtr = maxBotWeaponTiers + bot->ClientNum();
		for( int weaponNum = WEAP_GUNBLADE; weaponNum != WEAP_TOTAL; ++weaponNum ) {
			if( !bot->Inventory()[weaponNum] ) {
				continue;
			}
			const auto ammoTag = weaponDefs[weaponNum]->firedef.ammo_id;
			if( ammoTag == AMMO_NONE ) {
				continue;
			}
			if( bot->Inventory()[ammoTag] <= weaponDefs[weaponNum]->firedef.ammo_low ) {
				continue;
			}
			if( *botTierPtr < tiersForWeapon[weaponNum] ) {
				*botTierPtr = tiersForWeapon[weaponNum];
			}
		}
		if( minBotWeaponTier > *botTierPtr ) {
			minBotWeaponTier = *botTierPtr;
		}
	}

	return minBotWeaponTier;
}


int AiSquad::FindLowestBotHealth() const {
	int result = botsListHead->Health();
	for( Bot *bot = botsListHead->NextInSquad(); bot; bot = bot->NextInSquad() ) {
		if( bot->Health() < result ) {
			result = bot->Health();
		}
	}

	return result;
}

int AiSquad::FindLowestBotArmor() const {
	int result = botsListHead->Armor();
	for( Bot *bot = botsListHead->NextInSquad(); bot; bot = bot->NextInSquad() ) {
		if( bot->Armor() < result ) {
			result = bot->Armor();
		}
	}

	return result;
}

bool AiSquad::FindHealthSuppliers( bool wouldSupplyHealth[MAX_CLIENTS] ) const {
	bool result = false;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		result |= ( wouldSupplyHealth[bot->ClientNum()] = bot->CanAndWouldDropHealth() );
	}
	return result;
}

bool AiSquad::FindArmorSuppliers( bool wouldSupplyArmor[MAX_CLIENTS] ) const {
	bool result = false;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		result |= ( wouldSupplyArmor[bot->ClientNum()] = bot->CanAndWouldDropArmor() );
	}
	return result;
}

void AiSquad::CheckMembersInventory() {
	// In this function we try to do cheap test first to reject expensive calls early

	// i-th client has best weapon of tier maxBotWeaponTiers[i]
	int maxBotWeaponTiers[MAX_CLIENTS];
	bool canDropWeapons = false;
	bool shouldDropWeapons = false;
	if( level.gametype.dropableItemsMask & ( IT_WEAPON | IT_AMMO ) ) {
		int minBotWeaponTier = FindBotWeaponsTiers( maxBotWeaponTiers );
		shouldDropWeapons = minBotWeaponTier <= 2;
		canDropWeapons = true;
	}

	bool shouldDropHealth = FindLowestBotHealth() < 75;
	bool shouldDropArmor = FindLowestBotArmor() < 50;

	if( !shouldDropWeapons && !shouldDropHealth && !shouldDropArmor ) {
		return;
	}

	bool wouldBotDropHealth[MAX_CLIENTS];
	bool wouldDropHealth = false;
	if( shouldDropHealth && ( level.gametype.dropableItemsMask & IT_HEALTH ) ) {
		wouldDropHealth = FindHealthSuppliers( wouldBotDropHealth );
	}

	bool wouldBotDropArmor[MAX_CLIENTS];
	bool wouldDropArmor = false;
	if( shouldDropArmor && ( level.gametype.dropableItemsMask & IT_ARMOR ) ) {
		wouldDropArmor = FindArmorSuppliers( wouldBotDropArmor );
	}

	if( !shouldDropWeapons && !wouldDropHealth && !wouldDropArmor ) {
		return;
	}

	// This is a very expensive call
	if( ShouldNotDropItemsNow() ) {
		return;
	}

	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		const int botNum = bot->ClientNum();
		// Do not drop anything for a bot again during 3 seconds after last drop
		if( level.time - lastDroppedForBotTimestamps[botNum] < 3000 ) {
			continue;
		}

		// Check whether a bot is likely to already have a dropped item as a goal
		if( const std::optional<SelectedNavEntity> &selectedNavEntity = bot->GetSelectedNavEntity() ) {
			if( selectedNavEntity->navEntity->IsDroppedEntity() && selectedNavEntity->cost > 3.0f ) {
				continue;
			}
		}

		const bool needsWeapon = maxBotWeaponTiers[botNum] <= 2;
		// TODO: Check player class abilities
		const bool needsHealth = bot->Health() < 75;
		const bool needsArmor = bot->Armor() < 50;

		// Skip expensive FindSupplierCandidates() call by doing this cheap test first
		if( !needsWeapon && !needsHealth && !needsArmor ) {
			continue;
		}

		Bot *suppliersListHead = nullptr;
		FindSupplierCandidates( bot, &suppliersListHead );

		// Do not do simultaneous drops by different bots.
		// In theory dropping health, armor and weapon+ammo by a single bot is OK
		// but its unlikely that there are player classes capable of doing that.

		if( needsHealth && wouldDropHealth && RequestHealthDrop( bot, wouldBotDropHealth, suppliersListHead ) ) {
			continue;
		}

		if( needsWeapon && canDropWeapons && RequestWeaponAndAmmoDrop( bot, maxBotWeaponTiers, suppliersListHead ) ) {
			continue;
		}

		if( needsArmor && wouldDropArmor && RequestArmorDrop( bot, wouldBotDropArmor, suppliersListHead ) ) {
			continue;
		}
	}
}

bool AiSquad::ShouldNotDropItemsNow() const {
	// We have to merge enemies as we no longer have a shared enemies tracker
	wsw::StaticVector<const TrackedEnemy *, MAX_CLIENTS> mergedBotEnemies;
	for( const Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		for( const TrackedEnemy *enemy = bot->TrackedEnemiesHead(); enemy; enemy = enemy->NextInTrackedList() ) {
			// TODO: Disallow keeping invalid enemies in list
			if( enemy->IsValid() ) [[likely]] {
				const auto cmp = [&]( const TrackedEnemy *e ) {
					return e->m_ent == enemy->m_ent;
				};
				auto it = std::find_if( mergedBotEnemies.begin(), mergedBotEnemies.end(), cmp );
				if( it != mergedBotEnemies.end() ) {
					// Replace by a more actual record
					if( const TrackedEnemy *existing = *it; existing->LastSeenAt() < enemy->LastSeenAt() ) {
						*it = enemy;
					}
				} else {
					mergedBotEnemies.push_back( enemy );
					if( mergedBotEnemies.full() ) [[unlikely]] {
						break;
					}
				}
			}
		}
	}

	// First, compute squad AABB
	vec3_t mins, maxs;
	ClearBounds( mins, maxs );
	for( const Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		const float *origin = bot->Self()->s.origin;
		for( int i = 0; i < 3; ++i ) {
			if( maxs[i] < origin[i] ) {
				maxs[i] = origin[i];
			}
			if( mins[i] > origin[i] ) {
				mins[i] = origin[i];
			}
		}
	}

	Vec3 squadCenter( mins );
	squadCenter += maxs;
	squadCenter *= 0.5f;

	struct MaybeStealer {
		const TrackedEnemy *enemy;
		Vec3 extrapolatedOrigin;
		MaybeStealer( const TrackedEnemy *enemy_, const Vec3 &extrapolatedOrigin_ )
			: enemy( enemy_ ), extrapolatedOrigin( extrapolatedOrigin_ ) {}

		// Recently seen stealers should be first in a sorted list
		bool operator<( const MaybeStealer &that ) const {
			return enemy->LastSeenAt() > that.enemy->LastSeenAt();
		}
	};

	// First, reject enemies by distance
	wsw::StaticVector<MaybeStealer, MAX_CLIENTS> maybeStealers;
	for( const TrackedEnemy *enemy: mergedBotEnemies ) {
		assert( enemy->IsValid() );

		Vec3 enemyVelocityDir( enemy->LastSeenVelocity() );
		float squareEnemySpeed = enemyVelocityDir.SquaredLength();
		if( squareEnemySpeed < 1 ) {
			continue;
		}

		float invEnemySpeed = Q_RSqrt( squareEnemySpeed );
		float enemySpeed = 1.0f / invEnemySpeed;
		enemyVelocityDir *= invEnemySpeed;

		// Extrapolate last seen position but not more for 1 second
		// TODO: Test for collisions with the solid (it may be expensive)
		// If an extrapolated origin is inside solid, further trace test will treat an enemy as invisible
		float extrapolationSeconds = wsw::min( 1.0f, 0.001f * ( level.time - enemy->LastSeenAt() ) );
		Vec3 extrapolatedLastSeenPosition( enemy->LastSeenVelocity() );
		extrapolatedLastSeenPosition *= extrapolationSeconds;
		extrapolatedLastSeenPosition += enemy->LastSeenOrigin();

		Vec3 enemyToSquadCenterDir( squadCenter );
		enemyToSquadCenterDir -= extrapolatedLastSeenPosition;
		if( !enemyToSquadCenterDir.normalizeFast() ) {
			maybeStealers.push_back( MaybeStealer( enemy, extrapolatedLastSeenPosition ) );
			continue;
		}

		float directionFactor = enemyToSquadCenterDir.Dot( enemyVelocityDir );
		if( directionFactor < 0 ) {
			if( BoundsAndSphereIntersect( mins, maxs, extrapolatedLastSeenPosition.Data(), 192.0f ) ) {
				maybeStealers.push_back( MaybeStealer( enemy, extrapolatedLastSeenPosition ) );
			}
		} else {
			float radius = 192.0f + extrapolationSeconds * enemySpeed * directionFactor;
			if( BoundsAndSphereIntersect( mins, maxs, extrapolatedLastSeenPosition.Data(), radius ) ) {
				maybeStealers.push_back( MaybeStealer( enemy, extrapolatedLastSeenPosition ) );
			}
		}
	}

	// Sort all stealers based on last seen time (recently seen first)
	std::sort( maybeStealers.begin(), maybeStealers.end() );

	// Check not more than 4 most recently seen stealers.
	// Use trace instead of path travel time estimation because pathfinder may fail to find a path.
	trace_t trace;
	const auto *pvsCache = EntitiesPvsCache::Instance();
	for( unsigned i = 0, end = wsw::min( 4u, maybeStealers.size() ); i < end; ++i ) {
		MaybeStealer stealer = maybeStealers[i];
		for( const Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
			auto *const botEnt = const_cast<edict_t*>( bot->self );
			if( !pvsCache->AreInPvs( botEnt, stealer.enemy->m_ent ) ) {
				continue;
			}

			G_Trace( &trace, botEnt->s.origin, nullptr, nullptr, stealer.extrapolatedOrigin.Data(), botEnt, MASK_AISOLID );
			if( trace.fraction == 1.0f || game.edicts + trace.ent == stealer.enemy->m_ent ) {
				return true;
			}
		}
	}

	return false;
}

void AiSquad::FindSupplierCandidates( Bot *consumer, Bot **suppliersListHead ) const {
	Vec3 botVelocityDir( consumer->Velocity() );
	float squareBotSpeed = botVelocityDir.SquaredLength();

	// If a bot moves fast, modify score for mates depending of the bot velocity direction
	// (try to avoid stopping a fast-moving bot)
	bool applyDirectionFactor = false;
	if( squareBotSpeed > DEFAULT_PLAYERSPEED * DEFAULT_PLAYERSPEED ) {
		botVelocityDir *= Q_RSqrt( squareBotSpeed );
		applyDirectionFactor = true;
	}

	struct BotAndScore {
		Bot *bot;
		float score;
		BotAndScore( Bot *bot_, float score_ ) : bot( bot_ ), score( score_ ) {}
		bool operator<( const BotAndScore &that ) const { return score > that.score; }
	};

	wsw::StaticVector<BotAndScore, MAX_SQUAD_SIZE - 1> candidates;

	const auto thisBotNum = consumer->ClientNum();
	for( Bot *bot = botsListHead; bot; bot = bot->NextInSquad() ) {
		const auto thatBotNum = bot->ClientNum();
		if( thatBotNum == thisBotNum ) {
			continue;
		}
		// Wait a second for next drop
		if( level.time - lastDroppedByBotTimestamps[thatBotNum] < 1000 ) {
			continue;
		}

		int travelTime = ::clientToClientTable.GetTravelTime( consumer, bot );
		if( !travelTime ) {
			continue;
		}

		float score = 1.0f - BoundedFraction( travelTime, CONNECTIVITY_MOVE_CENTISECONDS );
		if( applyDirectionFactor ) {
			Vec3 botToThatBot( bot->Origin() );
			botToThatBot -= consumer->Origin();
			if( botToThatBot.normalizeFast() ) {
				score *= 0.5f + 0.5f * botToThatBot.Dot( botVelocityDir );
			}
		}
		candidates.push_back( BotAndScore( bot, score ) );
	}

	// Sort mates, most suitable item suppliers first
	std::sort( candidates.begin(), candidates.end() );

	*suppliersListHead = nullptr;
	for( BotAndScore &botAndScore: candidates ) {
		wsw::link( botAndScore.bot, suppliersListHead, Bot::TMP_LINKS );
	}
}

// See definition explanation why the code is weird
void AiSquad::SetDroppedEntityAsBotGoal( edict_t *ent ) {
	const char *tag = __FUNCTION__;
	if( !ent || !ent->r.inuse ) {
		AI_FailWith( tag, "ent is null or not in use" );
	}

	// The target ent should be set to a bot entity
	if( !ent->target_ent ) {
		AI_FailWith( tag, "target_ent is not set" );
	}

	// Allow other bots (and itself) to grab this item too
	// (But the suppliant has a priority since the goal has been set immediately)
	AI_AddNavEntity( ent, (ai_nav_entity_flags)( AI_NAV_REACH_AT_TOUCH | AI_NAV_DROPPED ) );

	// Check whether the bot has been removed
	// We might as well check for team change but it is extremely rare,
	// requires tracking the supplier team
	// and also picking an item dropped for a bot as a former teammate won't harm
	edict_t *botEnt = ent->target_ent;
	if( !botEnt->r.inuse || G_ISGHOSTING( botEnt ) || !botEnt->bot ) {
		return;
	}

	const NavEntity *navEntity = NavEntitiesRegistry::Instance()->NavEntityForEntity( ent );
	SelectedNavEntity selectedNavEntity {
		.timeoutAt = level.time + 5000, .navEntity = navEntity, .cost = 1.0f, .pickupGoalWeight = 5.0f
	};
	botEnt->bot->ForceSetNavEntity( selectedNavEntity );
	botEnt->bot->ForcePlanBuilding();
}

bool AiSquad::RequestWeaponAndAmmoDrop( Bot *consumer, const int *maxBotWeaponTiers, Bot *suppliersListHead ) {
	const auto *const weaponDefs = extendedWeaponDefsCache.WeaponDefs();
	const auto *const sortedByTierWeapons = extendedWeaponDefsCache.SortedByTierWeapons();

	// Should be set to a first chosen supplier's botNum
	// Further drop attempts should be made only for this bot.
	// (Items should be dropped from the same origin to be able to set a common movement goal)
	Bot *chosenSupplier = nullptr;
	// Not more than 3 items may be dropped on the same time (and by the same bot)
	int droppedItemsCount = 0;

	for( int i = 0; i < WEAP_TOTAL; ++i ) {
		const int currWeapon = sortedByTierWeapons[i];
		const auto &fireDef = weaponDefs[currWeapon]->firedef;

		edict_t *dropped = nullptr;

		// If the bot has this weapon, check whether he needs an ammo for it
		if( consumer->Inventory()[currWeapon] ) {
			// No ammo is required, go to the next weapon
			if( fireDef.ammo_id == AMMO_NONE ) {
				continue;
			}
			// Bot has enough ammo, go to the next weapon
			if( consumer->Inventory()[fireDef.ammo_id] > fireDef.ammo_low ) {
				continue;
			}

			// Find who may drop an ammo
			if( level.gametype.dropableItemsMask & IT_AMMO ) {
				if( chosenSupplier ) {
					dropped = TryDropAmmo( consumer, chosenSupplier, currWeapon );
				} else {
					for( Bot *supplier = suppliersListHead; supplier; supplier = supplier->NextInTmpList() ) {
						// We have checked this once during supplier candidates selection
						// mostly for suppliers selection algorithm optimization,
						// but this may have changed during weapon/health/armor drops in this frame.
						if( level.time - lastDroppedByBotTimestamps[supplier->ClientNum()] < 1000 ) {
							continue;
						}

						dropped = TryDropAmmo( consumer, supplier, currWeapon );
						if( !dropped ) {
							continue;
						}

						chosenSupplier = supplier;
						break;
					}
				}
			}
		}

		// Check who may drop a weapon
		if( !dropped ) {
			if( chosenSupplier ) {
				dropped = TryDropWeapon( consumer, chosenSupplier, currWeapon, maxBotWeaponTiers );
			} else {
				for( Bot *supplier = suppliersListHead; supplier; supplier = supplier->NextInTmpList() ) {
					if( level.time - lastDroppedByBotTimestamps[supplier->ClientNum()] < 1000 ) {
						continue;
					}

					dropped = TryDropWeapon( consumer, supplier, currWeapon, maxBotWeaponTiers );
					if( !dropped ) {
						continue;
					}

					chosenSupplier = supplier;
					break;
				}
			}
		}

		if( dropped ) {
			// If this is first dropped item, set is as a pending goal
			if( !droppedItemsCount ) {
				dropped->target_ent = consumer->Self();
				dropped->enemy = (edict_t *)this;
				dropped->stop = AiSquad::SetDroppedEntityAsBotGoal;
				// Register drop timestamp
				lastDroppedForBotTimestamps[consumer->ClientNum()] = level.time;
				lastDroppedByBotTimestamps[chosenSupplier->ClientNum()] = level.time;
			}
			droppedItemsCount++;
			if( droppedItemsCount == 3 ) {
				return true;
			}
		}
	}

	return droppedItemsCount > 0;
}

edict_t *AiSquad::TryDropAmmo( Bot *consumer, Bot *supplier, int weapon ) {
	const auto &fireDef = extendedWeaponDefsCache.WeaponDefs()[weapon]->firedef;
	// Min ammo quantity to be able to drop it
	float minAmmo = fireDef.ammo_pickup;
	// If mate has not only ammo but weapon, leave mate some ammo
	if( supplier->Inventory()[weapon] ) {
		minAmmo += fireDef.ammo_low;
	}

	if( supplier->Inventory()[fireDef.ammo_id] < minAmmo ) {
		return nullptr;
	}

	edict_t *dropped = G_DropItem( supplier->Self(), GS_FindItemByTag( fireDef.ammo_id ) );
	if( dropped ) {
		G_Say_Team( supplier->Self(), va( "Dropped %%d at %%D for %s", consumer->Nick() ) );
	}
	return dropped;
}

edict_t *AiSquad::TryDropWeapon( Bot *consumer, Bot *supplier, int weapon, const int *maxBotWeaponTiers ) {
	const auto *const weaponDefs = extendedWeaponDefsCache.WeaponDefs();
	const auto *const weaponTiers = extendedWeaponDefsCache.WeaponTiers();

	// The mate does not have this weapon
	if( !supplier->Inventory()[weapon] ) {
		return nullptr;
	}

	const auto &fireDef = weaponDefs[weapon]->firedef;
	// The mate does not have enough ammo for this weapon
	if( supplier->Inventory()[fireDef.ammo_id] <= fireDef.ammo_low ) {
		return nullptr;
	}

	// Compute a weapon tier of mate's inventory left after the possible drop
	int newMaxBestWeaponTier = 0;
	for( int otherWeapon = WEAP_GUNBLADE + 1; otherWeapon < WEAP_TOTAL; ++otherWeapon ) {
		if( otherWeapon == weapon ) {
			continue;
		}
		if( !supplier->Inventory()[otherWeapon] ) {
			continue;
		}
		const auto &otherFireDef = weaponDefs[otherWeapon]->firedef;
		if( supplier->Inventory()[otherFireDef.ammo_id] <= otherFireDef.ammo_low ) {
			continue;
		}

		newMaxBestWeaponTier = wsw::max( newMaxBestWeaponTier, weaponTiers[otherWeapon] );
	}

	// If the does not keep its top weapon tier after dropping a weapon
	if( newMaxBestWeaponTier < maxBotWeaponTiers[supplier->ClientNum()] ) {
		return nullptr;
	}

	// Try drop a weapon
	edict_t *dropped = G_DropItem( supplier->Self(), GS_FindItemByTag( weapon ) );
	if( dropped ) {
		G_Say_Team( supplier->Self(), va( "Dropped %%d at %%D for %s", consumer->Nick() ) );
	}
	return dropped;
}

bool AiSquad::RequestHealthDrop( Bot *consumer, bool wouldSupplyHealth[MAX_CLIENTS], Bot *suppliersListHead ) {
	return RequestDrop( consumer, wouldSupplyHealth, suppliersListHead, &Bot::DropHealth );
}

bool AiSquad::RequestArmorDrop( Bot *consumer, bool wouldSupplyArmor[MAX_CLIENTS], Bot *suppliersListHead ) {
	return RequestDrop( consumer, wouldSupplyArmor, suppliersListHead, &Bot::DropArmor );
}

bool AiSquad::RequestDrop( Bot *consumer, bool wouldSupply[MAX_CLIENTS], Bot *suppliers, void ( Bot::*dropFunc )() ) {
	const int botFloorClusterNum = GetBotFloorCluster( consumer );
	// Disallow dropping items outside of floor clusters
	if( !botFloorClusterNum ) {
		return false;
	}

	for( Bot *supplier = suppliers; supplier; supplier->NextInTmpList() ) {
		const auto supplierNum = supplier->ClientNum();
		if( !wouldSupply[supplierNum] ) {
			continue;
		}

		if( botFloorClusterNum != GetBotFloorCluster( supplier ) ) {
			continue;
		}

		// We have checked this once during supplier candidates selection
		// mostly for suppliers selection algorithm optimization,
		// but this may have changed during weapon/health/armor drops in this frame.
		if( level.time - lastDroppedByBotTimestamps[supplierNum] < 1000 ) {
			continue;
		}

		( supplier->*dropFunc )();
		lastDroppedByBotTimestamps[supplierNum] = level.time;
		lastDroppedForBotTimestamps[consumer->ClientNum()] = level.time;
		G_Say_Team( supplier->Self(), va( "Dropped %%d at %%D for %s", consumer->Nick() ) );
		return true;
	}
	return false;
}

void AiSquad::PrepareToAddBots() {
	isValid = true;
	canFightTogether = false;
	canMoveTogether = false;
	brokenConnectivityTimeoutAt = level.time + CONNECTIVITY_TIMEOUT;
	botsDetached = false;
	botsListHead = nullptr;
	numBots = 0;
}

void AiSquad::AddBot( Bot *bot ) {
	if( !isValid ) {
		AI_FailWith( "AiSquad::AddBot()", "Can't add a bot to a unused or invalid squad\n" );
	}

	for( Bot *presentBot = botsListHead; presentBot; presentBot = presentBot->NextInSquad() ) {
		if( presentBot == bot ) {
			AI_FailWith( "AiSquad::AddBot()", "Can't add a bot to the squad (it is already present)\n" );
		}
	}

	wsw::link( bot, &botsListHead, Bot::SQUAD_LINKS );
	numBots++;
	bot->OnAttachedToSquad( this );
}

float AiSquad::GetScoreForBotAttachment( const Bot *bot ) const {
	if( !isValid ) {
		return 0.0f;
	}
	if( numBots == MAX_SQUAD_SIZE ) {
		return 0.0f;
	}

	float score = 0.0f;
	const auto &table = ::clientToClientTable;
	const int botNavTargetAreaNum = bot->NavTargetAasAreaNum();
	for( Bot *presentBot = botsListHead; presentBot; presentBot = presentBot->NextInSquad() ) {
		// The bot should not be attached again
		if( bot == presentBot ) {
			return 0.0f;
		}
		constexpr float squaredDistanceLimit = CONNECTIVITY_PROXIMITY * CONNECTIVITY_PROXIMITY;
		if( DistanceSquared( bot->Origin(), presentBot->Origin() ) > squaredDistanceLimit ) {
			continue;
		}

		const int t1 = table.GetTravelTime( bot, presentBot );
		if( !t1 || t1 > CONNECTIVITY_MOVE_CENTISECONDS ) {
			continue;
		}
		const int t2 = table.GetTravelTime( presentBot, bot );
		if( !t2 || t2 > CONNECTIVITY_MOVE_CENTISECONDS ) {
			continue;
		}

		score += 1.0f;
		// Put a huge additional score if the bot has the same target as another bot does
		if( botNavTargetAreaNum && botNavTargetAreaNum == presentBot->NavTargetAasAreaNum() ) {
			score += 3.0f;
		}
	}

	return score;
}

AiSquadBasedTeam::AiSquadBasedTeam( int team_ )
	: AiBaseTeam( team_ ), assistanceTracker( this ) {
	// We have to construct squads explicitly since they do not have a default/trivial constructor
	// Note: avoid getting an address of next (non-existent yet) element that is harmless but triggers a debug assertion

	auto *squad = new( squads.unsafe_grow_back() )AiSquad( this );
	squad->prev = nullptr;
	squad->next = squads.begin() + 1;
	squad->squadNum = 0;

	for( int i = 1; i < MAX_CLIENTS - 1; ++i ) {
		squad = new( squads.unsafe_grow_back() )AiSquad( this );
		squad->prev = &squads[i - 1];
		squad->next = squads.begin() + i + 1;
		squad->squadNum = i;
	}

	squad = new( squads.unsafe_grow_back() )AiSquad( this );
	assert( squad == &squads[MAX_CLIENTS - 1] );
	squad->prev = &squads[MAX_CLIENTS - 2];
	squad->next = nullptr;
	squad->squadNum = MAX_CLIENTS - 1;

	freeSquadsHead = &squads[0];
}

AiSquadBasedTeam::~AiSquadBasedTeam() {
	// We must detach bots since this destructor may be called mid-game
	// (when a script requests a team logic with more features and this team is replaced by other one)
	// Otherwise bots are going to do use-after-free memory access and crash the game subsequently.
	for( AiSquad *squad = usedSquadsHead; squad; squad = squad->NextInList() ) {
		squad->DetachBots();
	}
}

void AiSquadBasedTeam::Frame() {
	// Call super method first, it may contain some logic
	AiBaseTeam::Frame();

	// Keep checking whether the orphans list is valid
	for( Bot *bot = orphanBotsHead; bot; bot = bot->NextInSquad() ) {
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			if( bot == otherBot ) {
				AI_FailWith( "SquadsBuilder::Exec()", "There are duplicates in the orphans list" );
			}
		}
	}

	// Try to unlink invalid squads (they might have invalidated themselves)
	AiSquad *nextSquad;
	for( AiSquad *squad = usedSquadsHead; squad; squad = nextSquad ) {
		nextSquad = squad->NextInList();
		if( squad->IsValid() ) {
			continue;
		}
		wsw::unlink( squad, &usedSquadsHead );
		wsw::link( squad, &freeSquadsHead );
	}

	// This should be called before AiSquad::Update() (since squads expect this to be valid)
	::clientToClientTable.Update();

	// Call squads Update() (and, thus, Frame() and, maybe, Think()) each frame as it is expected
	// even if all squad AI logic is performed only in AiSquad::Think()
	// to prevent further errors if we decide later to put some logic in Frame()
	for( AiSquad *squad = usedSquadsHead; squad; squad = squad->NextInList() ) {
		squad->Update();
	}
}

void AiSquadBasedTeam::OnBotAdded( Bot *bot ) {
	// Link to orphan bots and wait for squads clustering
	wsw::link( bot, &orphanBotsHead, Bot::SQUAD_LINKS );
}

void AiSquadBasedTeam::OnBotRemoved( Bot *bot ) {
	// Perform some additional checks unless this is a public release build.
	// These checks have turned to be very useful to catch bugs.

	// If the bot is in squad, we should invalidate the squad.
	// The AiSquad::Invalidate() links all squad bots to our orphan lists.
	bool wasInSquad = false;
	if( auto *const squad = bot->squad ) {
		wasInSquad = true;
		Bot *squadBot = nullptr;
		for( squadBot = squad->botsListHead; squadBot; squadBot = squadBot->NextInSquad() ) {
			if( squadBot == bot ) {
				// This call links the bot to the orphans list
				squad->Invalidate();
				break;
			}
		}
		// If the loop has not been interrupted, the bot has not been found
		if( !squadBot ) {
			FailWith( "OnBotRemoved(%p): the bot was not in squad %p", bot, squad );
		}
	}

	// Now the bot should be in orphans list regardless whether was it in squad or not.
	for( Bot *orphanBot = orphanBotsHead; orphanBot; orphanBot = orphanBot->NextInSquad() ) {
		if( orphanBot == bot ) {
			wsw::unlink( bot, &orphanBotsHead, Bot::SQUAD_LINKS );
			return;
		}
	}

	if( wasInSquad ) {
		FailWith( "OnBotRemoved(%p): the bot has not appeared in orphans list", bot );
	} else {
		FailWith( "OnBotRemoved(%p): the bot was not in orphans list", bot );
	}
}

void AiSquadBasedTeam::TransferStateFrom( AiBaseTeam *that ) {
	// The super method must always be called
	AiBaseTeam::TransferStateFrom( that );

	auto *thatSquadBasedTeam = dynamic_cast<AiSquadBasedTeam *>( that );
	if( !thatSquadBasedTeam ) {
		return;
	}

	// Force detachment of bots from squads so they end in "that" orphans list
	for( AiSquad *squad = thatSquadBasedTeam->usedSquadsHead; squad; squad = squad->NextInList() ) {
		squad->Invalidate();
	}

	// Transfer the orphan bots list
	this->orphanBotsHead = thatSquadBasedTeam->orphanBotsHead;
	thatSquadBasedTeam->orphanBotsHead = nullptr;
}

void AiSquadBasedTeam::Think() {
	// Call super method first, this call must not be omitted
	AiBaseTeam::Think();

	assistanceTracker.Think();

	// Try setting up squads for orphan bots
	if( orphanBotsHead ) {
		SetupSquads();
	}
}

struct alignas( 4 )CandidatePair {
	float score;
	uint16_t clientNum1;
	uint16_t clientNum2;

	CandidatePair( const Bot *bot1_, const Bot *bot2_, float score_ )
		: score( score_ ), clientNum1( bot1_->ClientNum() ), clientNum2( bot2_->ClientNum() ) {}

	bool operator<( const CandidatePair &that ) const { return score > that.score; }
};

void AiSquadBasedTeam::SetupSquads() {
	const auto &table = ::clientToClientTable;
	// Given N as the maximal number of clients, there could be up to N * (N - 1) pairs.
	// This will never be met in realistic use cases (the number of bots is limited and teams are almost always even).
	wsw::StaticVector<CandidatePair, 64> candidatePairs;
	for( Bot *bot = orphanBotsHead; bot; bot = bot->NextInSquad() ) {
		if( bot->IsGhosting() ) {
			continue;
		}
		const int botNavTargetAreaNum = bot->NavTargetAasAreaNum();
		for( Bot *otherBot = bot->NextInSquad(); otherBot; otherBot = otherBot->NextInSquad() ) {
			if( otherBot->IsGhosting() ) {
				continue;
			}
			if( DistanceSquared( bot->Origin(), otherBot->Origin() ) > CONNECTIVITY_PROXIMITY * CONNECTIVITY_PROXIMITY ) {
				continue;
			}
			const auto t1 = table.GetTravelTime( bot, otherBot );
			if( !t1 || t1 > CONNECTIVITY_MOVE_CENTISECONDS ) {
				continue;
			}
			const int t2 = table.GetTravelTime( otherBot, bot );
			if( !t2 || t2 > CONNECTIVITY_MOVE_CENTISECONDS ) {
				continue;
			}
			// Let the score be negative so closest pairs get greater score
			float score = -( t1 + t2 );
			// Lower the score magnitude (and thus make it greater) if they have the same nav target
			if( botNavTargetAreaNum && botNavTargetAreaNum == otherBot->NavTargetAasAreaNum() ) {
				score *= 0.33f;
			}
			candidatePairs.emplace_back( CandidatePair( bot, otherBot, score ) );
			// Add a protection against overflow that still is possible in theory
			if( candidatePairs.full() ) {
				goto sortPairs;
			}
		}
	}

sortPairs:
	// Sort pairs so closest pairs are first
	std::sort( candidatePairs.begin(), candidatePairs.end() );

	bool isClientAssigned[MAX_CLIENTS];
	std::fill( std::begin( isClientAssigned ), std::end( isClientAssigned ), false );

	auto *const ents = game.edicts;
	for( const auto &pair: candidatePairs ) {
		// Skip clients that already got assigned to squads
		if( isClientAssigned[pair.clientNum1] | isClientAssigned[pair.clientNum2] ) {
			continue;
		}
		AiSquad *const squad = AllocSquad();
		for( int clientNum : { pair.clientNum1, pair.clientNum2 } ) {
			isClientAssigned[clientNum] = true;
			Bot *bot = ents[clientNum + 1].bot;
			wsw::unlink( bot, &orphanBotsHead, Bot::SQUAD_LINKS );
			squad->AddBot( bot );
		}
	}

	// For each remaining orphan bot try attaching a bot to an existing squad.

	Bot *nextBot;
	for( Bot *bot = orphanBotsHead; bot; bot = nextBot ) {
		nextBot = bot->NextInSquad();

		AiSquad *bestSquad = nullptr;
		float bestScore = 0.0f;
		for( AiSquad *squad = usedSquadsHead; squad; squad = squad->NextInList() ) {
			float score = squad->GetScoreForBotAttachment( bot );
			if( score <= bestScore ) {
				continue;
			}

			bestScore = score;
			bestSquad = squad;
		}

		if( !bestSquad ) {
			continue;
		}

		// Unlink the bot from orphans list
		wsw::unlink( bot, &orphanBotsHead, Bot::SQUAD_LINKS );
		// Link the bot to the squad
		bestSquad->AddBot( bot );
	}

}

AiSquad *AiSquadBasedTeam::AllocSquad() {
	// We never exhaust squads capacity as MAX_CLIENTS is the maximum number of squads
	assert( freeSquadsHead );

	AiSquad *const squad = wsw::unlink( freeSquadsHead, &freeSquadsHead );
	wsw::link( squad, &usedSquadsHead );
	// This is very important action, otherwise the squad will not think
	squad->SetFrameAffinity( frameAffinityModulo, frameAffinityOffset );
	squad->PrepareToAddBots();
	return squad;
}

void AiSquadBasedTeam::AddToOrphansList( Bot *bot ) {
	for( Bot *presentBot = orphanBotsHead; presentBot; presentBot = presentBot->NextInSquad() ) {
		if( presentBot == bot ) {
			AI_FailWith( "AiSquadBasedTeam::AddToOrphansList()", "A bot is already in the list\n" );
		}
	}
	wsw::link( bot, &orphanBotsHead, Bot::SQUAD_LINKS );
}

AiSquadBasedTeam *AiSquadBasedTeam::InstantiateTeam( int teamNum ) {
	void *mem = Q_malloc( sizeof( AiSquadBasedTeam ) );
	return new( mem )AiSquadBasedTeam( teamNum );
}

AiSquadBasedTeam *AiSquadBasedTeam::InstantiateTeam( int teamNum, const std::type_info &desiredType ) {
	if( typeid( AiSquadBasedTeam ) == desiredType ) {
		return InstantiateTeam( teamNum );
	}

	void *mem = Q_malloc( sizeof( AiObjectiveBasedTeam ) );
	return new( mem )AiObjectiveBasedTeam( teamNum );
}

AiSquadBasedTeam::PlayerAssistanceTracker::PlayerAssistanceTracker( const AiSquadBasedTeam *parent_ )
	: parent( parent_ ) {
	influenceScores = (int8_t *)Q_malloc( MAX_CLIENTS * MAX_CLIENTS * sizeof( int8_t ) );
	std::fill( std::begin( assistanceMillisLeft ), std::end( assistanceMillisLeft ), 0 );
	std::fill( std::begin( assistedClientNum ), std::end( assistedClientNum ), -1 );
}

bool AiSquadBasedTeam::OverridesEntityWeights( const Bot *bot ) const {
	// Call the super method as it is expected
	if( AiBaseTeam::OverridesEntityWeights( bot ) ) {
		return true;
	}
	return assistanceTracker.OverridesEntityWeights( bot );
}

inline bool AiSquadBasedTeam::PlayerAssistanceTracker::OverridesEntityWeights( const Bot *bot ) const {
	return assistedClientNum[bot->ClientNum()] >= 0;
}

using WeightsPair = const std::pair<float, float>;

WeightsPair *AiSquadBasedTeam::GetEntityWeights( const Bot *bot, const NavEntity *navEntity ) const {
	WeightsPair *superWeights = AiBaseTeam::GetEntityWeights( bot, navEntity );
	WeightsPair *thisWeights = assistanceTracker.GetEntityWeights( bot, navEntity );
	return ChooseWeights( superWeights, thisWeights );
}

WeightsPair *AiSquadBasedTeam::PlayerAssistanceTracker::GetEntityWeights( const Bot *bot, const NavEntity *nav ) const {
	if( !nav->IsClient() ) {
		return nullptr;
	}

	// That's who is currently being assisted
	const int assistedNum = assistedClientNum[bot->ClientNum()];
	// That's the number of nav entity client
	const int entNum = nav->EntityId();
	const int entClientNum = entNum - 1;
	if( assistedNum != entClientNum ) {
		return nullptr;
	}

	const edict_t *ent = game.edicts + entNum;
	// Make sure everything is right even if some of these checks are redundant
	if( G_ISGHOSTING( ent ) || ent->s.team != parent->teamNum ) {
		return nullptr;
	}

	// Stop trying to hug the assisted player
	if( DistanceSquared( bot->Origin(), ent->s.origin ) < 72.0f * 72.0f ) {
		return nullptr;
	}

	// Give the client a high nav weight and an extremely high planning goal weight
	tmpWeights = std::make_pair( 9.0f, 999.0f );
	return &tmpWeights;
}


static constexpr int REFILL_SCORE = 2;

void AiSquadBasedTeam::PlayerAssistanceTracker::UpdateInfluence() {
	const auto &__restrict teamList = ::teamlist[parent->teamNum];
	const auto *const __restrict playerIndices = teamList.playerIndices;
	auto *const __restrict gameEdicts = game.edicts;
	const auto *const __restrict pvsCache = EntitiesPvsCache::Instance();
	auto *const __restrict scores_ = this->influenceScores;
	const auto levelTime = level.time;

	trace_t trace;
	// Iterate over all teammates in the outer loop to reuse some checks
	for( int i = 0; i < teamList.numplayers; ++i ) {
		const auto playerEntNum = playerIndices[i];
		edict_t *const __restrict ent = gameEdicts + playerEntNum;
		// Skip other AI beings
		if( ent->ai ) {
			continue;
		}
		if( G_ISGHOSTING( ent ) ) {
			continue;
		}

		const auto *__restrict client = ent->r.client;

		// Ignore clients that were not active for a half of a second or longer.
		// Otherwise getting assisted just by happening to be spawned in a group at round start is annoying.
		if( client->last_activity + 500 < levelTime ) {
			continue;
		}

		vec3_t mateForwardDir;
		AngleVectors( ent->s.angles, mateForwardDir, nullptr, nullptr );

		const bool isZooming = client->ps.pmove.stats[PM_STAT_ZOOMTIME] > 0;
		// Avoid excessive branching on `isZooming` in the loop below
		const float squareDistanceThreshold = isZooming ? std::numeric_limits<float>::max() : 1250.0f * 1250.0f;
		const float dotThreshold = isZooming ? 0.95f : 0.85f;

		// Pick faster if the mate is crouching. "slice" suggested this.
		const int refillScore = client->ps.pmove.stats[PM_STAT_CROUCHTIME] ? 2 * REFILL_SCORE : REFILL_SCORE;

		for( Bot *bot = parent->teamBotsHead; bot; bot = bot->NextInBotsTeam() ) {
			const auto botClientNum = bot->ClientNum();
			edict_t *const __restrict botEnt = gameEdicts + botClientNum + 1;
			if( G_ISGHOSTING( botEnt ) ) {
				continue;
			}

			// TODO: Start prefetching now &botScoresRow[playerClientNum]
			auto *const __restrict botScoresRow = scores_ + botClientNum * MAX_CLIENTS;

			Vec3 toBotVec( bot->Origin() );
			toBotVec -= ent->s.origin;
			const float squareDistance = toBotVec.SquaredLength();
			// Check whether the teammate is zooming
			// Skip far bots unless a teammate tries to highlight the bot while zooming.
			// (`squareDistanceThreshold` depends of zooming).
			if( squareDistance > squareDistanceThreshold ) {
				continue;
			}

			// Check whether the teammate is looking at the bot
			if( toBotVec.Dot( mateForwardDir ) * Q_RSqrt( squareDistance + 1.0f ) < dotThreshold ) {
				continue;
			}

			if( !pvsCache->AreInPvs( ent, botEnt ) ) {
				continue;
			}

			// TODO: Use raycast in a floor cluster for quick rejection?

			Vec3 traceStart( ent->s.origin );
			traceStart.Z() += ent->viewheight;
			Vec3 traceEnd( mateForwardDir );
			traceEnd *= 99999.0f;
			traceEnd += traceStart;

			G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), ent, MASK_PLAYERSOLID );
			if( gameEdicts + trace.ent != botEnt ) {
				continue;
			}

			assert( playerEntNum > 0 );
			const auto playerClientNum = playerEntNum - 1;
			botScoresRow[playerClientNum] += refillScore;
		}
	}
}

void AiSquadBasedTeam::PlayerAssistanceTracker::DrainAndPick() {
	auto *const __restrict gameEdicts = game.edicts;
	auto *const __restrict scores_ = this->influenceScores;
	auto *const __restrict assistanceMillisLeft_ = this->assistanceMillisLeft;
	auto *const __restrict assistedClientNum_ = this->assistedClientNum;

	// For every bot in this team
	for( Bot *bot = parent->teamBotsHead; bot; bot = bot->NextInBotsTeam() ) {
		const auto botClientNum = bot->ClientNum();
		edict_t *const __restrict botEnt = gameEdicts + botClientNum + 1;
		auto *const __restrict botScoresRow = scores_ + botClientNum * MAX_CLIENTS;
		// Check whether the previous assistance order has expired

		// TODO: Drop assumptions on default frame time
		// Note that we do not have to track the time exactly.
		// Just let the assistance expire in few seconds.
		constexpr auto thinkInterval = 16 * 4;
		// Assume that it requires tracking a bot perfectly for a half of a second
		constexpr int maxScore = ( REFILL_SCORE * 500 ) / thinkInterval;
		// Avoid score overflow
		static_assert( maxScore < std::numeric_limits<int8_t>::max() - REFILL_SCORE, "" );

		auto &__restrict millisLeft = assistanceMillisLeft_[botClientNum];
		auto &__restrict assistedNum = assistedClientNum_[botClientNum];
		millisLeft -= thinkInterval;
		if( millisLeft <= 0 ) {
			millisLeft = 0;
			assistedNum = -1;
		} else {
			// Check whether the assisted player is no longer valid
			const auto oldAssistedNum = assistedNum;
			const edict_t *assisted = gameEdicts + oldAssistedNum + 1;
			if( G_ISGHOSTING( assisted ) || assisted->s.team != parent->teamNum ) {
				millisLeft = 0;
				assistedNum = -1;
				// Reset tracking score as well
				botScoresRow[oldAssistedNum] = 0;
			}
		}

		// If still assisting somebody
		if( assistedNum >= 0 ) {
			// Just drain/clamp scores
			for( int i = 0; i < MAX_CLIENTS; ++i ) {
				static_assert( REFILL_SCORE > 1, "" );
				botScoresRow[i] -= ( REFILL_SCORE - 1 );
				Q_clamp( botScoresRow[i], 0, maxScore );
			}
			continue;
		}

		// Pick somebody to assist while draining/clamping scores
		int goodClientNum = -1;
		for( int i = 0; i < MAX_CLIENTS; ++i ) {
			static_assert( REFILL_SCORE > 1, "" );
			botScoresRow[i] -= ( REFILL_SCORE - 1 );
			Q_clamp( botScoresRow[i], 0, maxScore );
			// If the score is the maximal score after draining
			if( botScoresRow[i] >= maxScore - ( REFILL_SCORE - 1 ) ) {
				goodClientNum = i;
			}
		}

		if( goodClientNum < 0 ) {
			continue;
		}

		millisLeft = 7500;
		assistedNum = goodClientNum;

		const auto goodEntNum = goodClientNum + 1;
		const char *nickName = gameEdicts[goodEntNum].r.client->netname.data();
		G_Say_Team( botEnt, va( "Roger! I've got your back %s\n", nickName ) );
	}
}