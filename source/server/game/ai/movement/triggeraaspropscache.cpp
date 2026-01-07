#include "triggeraaspropscache.h"
#include "movementlocal.h"
#include "../classifiedentitiescache.h"
#include "../manager.h"

TriggerAasPropsCache triggerAasPropsCache;

void TriggerAasPropsCache::clear() {
	m_triggerAreaNumsForTeleportReach.clear();
	m_triggerAreaNumsForJumppadReach.clear();
	m_triggerAreaNumsForElevatorReach.clear();
}

[[nodiscard]]
static auto findContactingEntNumForReach( int reachNum, [[maybe_unused]] int travelType, float halfExtraExtent,
										  std::span<const uint16_t> classEntNums ) -> int {
	const auto *const aasWorld = AiAasWorld::instance();
	const auto *const gameEnts = game.edicts;
	const auto &reach          = aasWorld->getReaches()[reachNum];
	assert( reach.traveltype == travelType );
	assert( halfExtraExtent > 0.0f );

	const Vec3 contactMins( Vec3( -halfExtraExtent, -halfExtraExtent, -halfExtraExtent ) + reach.start );
	const Vec3 contactMaxs( Vec3( +halfExtraExtent, +halfExtraExtent, +halfExtraExtent ) + reach.start );

	for( const auto entNum: classEntNums ) {
		const auto *const ent = gameEnts + entNum;
		if( GClip_EntityContact( contactMins.Data(), contactMaxs.Data(), ent ) ) {
			return entNum;
		}
	}

	return 0;
}

[[nodiscard]]
static auto findTeleporterEntNumForReach( int reachNum ) -> int {
	return findContactingEntNumForReach( reachNum, TRAVEL_TELEPORT, 4.0f,
										 wsw::ai::ClassifiedEntitiesCache::instance()->getAllPersistentMapTeleporters() );
}

[[nodiscard]]
static auto findJumppadEntNumForReach( int reachNum ) -> int {
	return findContactingEntNumForReach( reachNum, TRAVEL_JUMPPAD, 16.0f,
										 wsw::ai::ClassifiedEntitiesCache::instance()->getAllPersistentMapJumppads() );
}

[[nodiscard]]
static auto findElevatorEntNumForReach( int reachNum ) -> int {
	const auto *const aasWorld = AiAasWorld::instance();
	const auto *const gameEnts = game.edicts;
	const auto &reach          = aasWorld->getReaches()[reachNum];
	assert( reach.traveltype == TRAVEL_ELEVATOR );

	for( const auto entNum: wsw::ai::ClassifiedEntitiesCache::instance()->getAllPersistentMapPlatformTriggers() ) {
		const auto *const trigger = gameEnts + entNum;
		if( const auto *platform = trigger->enemy ) [[likely]] {
			// BSPC be_aas_reach.c: "the facenum is the model number"
			if( (int)platform->s.modelindex == (int)reach.edgenum ) {
				return entNum;
			}
		}
	}

	return 0;
}

[[nodiscard]]
static auto getTriggerEntNumForReach( int reachNum, std::unordered_map<int, int> *cache, auto (*findFn)( int ) -> int )
	-> std::optional<int> {
	if( auto it = cache->find( reachNum ); it != cache->end() ) {
		return it->second ? std::optional( it->second ) : std::nullopt;
	}
	const int entNum = findFn( reachNum );
	cache->insert( { reachNum, entNum } );
	return entNum ? std::optional( entNum ) : std::nullopt;
}

auto TriggerAasPropsCache::getTriggerEntNumForTeleportReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerAreaNumsForTeleportReach,
									 &findTeleporterEntNumForReach );
}

auto TriggerAasPropsCache::getTriggerEntNumForJumppadReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerAreaNumsForJumppadReach,
									 &findJumppadEntNumForReach );
}

auto TriggerAasPropsCache::getTriggerEntNumForElevatorReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerAreaNumsForElevatorReach,
									 &findElevatorEntNumForReach );
}