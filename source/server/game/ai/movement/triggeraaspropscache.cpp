#include "triggeraaspropscache.h"
#include "movementlocal.h"
#include "../classifiedentitiescache.h"
#include "../manager.h"

TriggerAasPropsCache triggerAasPropsCache;

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
	return findContactingEntNumForReach( reachNum, TRAVEL_JUMPPAD, 32.0f,
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
static auto getTriggerEntNumForReach( int reachNum, std::unordered_map<int, int> *cache ) -> std::optional<int> {
	if( auto it = cache->find( reachNum ); it != cache->end() ) {
		return it->second;
	}
	return std::nullopt;
}

auto TriggerAasPropsCache::getTriggerEntNumForTeleportReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerEntNumsForTeleportReach );
}

auto TriggerAasPropsCache::getTriggerEntNumForJumppadReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerEntNumsForJumppadReach );
}

auto TriggerAasPropsCache::getTriggerEntNumForElevatorReach( int reachNum ) const -> std::optional<int> {
	return getTriggerEntNumForReach( reachNum, &m_triggerEntNumsForElevatorReach );
}

auto TriggerAasPropsCache::getJumppadTargetAreas( int entNum ) const -> std::span<const uint16_t> {
	if( auto it = m_jummpadTargetAreas.find( entNum ); it != m_jummpadTargetAreas.end() ) {
		return it->second;
	}
	return {};
}

void TriggerAasPropsCache::reload() {
	m_triggerEntNumsForTeleportReach.clear();
	m_triggerEntNumsForJumppadReach.clear();
	m_triggerEntNumsForElevatorReach.clear();

	const auto *const aasWorld = AiAasWorld::instance();
	// TODO: zipWithIndex
	for( int reachNum = 1; reachNum < (int)aasWorld->getReaches().size(); ++reachNum ) {
		const auto &reach = aasWorld->getReaches()[reachNum];
		if( reach.traveltype == TRAVEL_TELEPORT ) {
			if( const int entNum = findTeleporterEntNumForReach( reachNum ) ) {
				m_triggerEntNumsForTeleportReach.insert( { reachNum, entNum } );
			}
		} else if( reach.traveltype == TRAVEL_JUMPPAD ) {
			if( const int entNum = findJumppadEntNumForReach( reachNum ) ) {
				m_triggerEntNumsForJumppadReach.insert( { reachNum, entNum } );
				m_jummpadTargetAreas[entNum].append( reach.areanum );
			}
		} else if( reach.traveltype == TRAVEL_ELEVATOR ) {
			if( const int entNum = findElevatorEntNumForReach( reachNum ) ) {
				m_triggerEntNumsForElevatorReach.insert( { reachNum, entNum } );
			}
		}
	}

	for( const int entNum: wsw::ai::ClassifiedEntitiesCache::instance()->getAllPersistentMapJumppads() ) {
		const auto *ent = game.edicts + entNum;
		if( !m_jummpadTargetAreas.contains( entNum ) ) {
			aiWarning() << "Failed to find jumppad target areas for" << wsw::StringView( ent->classname ) << "at" <<
				"mins" << ent->r.absmin[0] << ent->r.absmin[1] << ent->r.absmin[2] <<
				"maxs" << ent->r.absmax[0] << ent->r.absmax[1] << ent->r.absmax[2];
		}
	}
}
