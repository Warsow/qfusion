#include "triggerareanumscache.h"
#include "movementlocal.h"
#include "../classifiedentitiescache.h"
#include "../manager.h"

TriggerAreaNumsCache triggerAreaNumsCache;

void TriggerAreaNumsCache::clear() {
	std::memset( m_areaNums, 0, sizeof( m_areaNums ) );
	m_triggersForArea.clear();
	m_jumppadTargetAreaNums.clear();
	m_testedTriggersForArea.reset();
	m_hasTriggersForArea.reset();
}

auto TriggerAreaNumsCache::getAreaNum( int entNum ) const -> int {
	int *const __restrict areaNumRef = &m_areaNums[entNum];
	// Put the likely case first
	if( *areaNumRef ) {
		return *areaNumRef;
	}

	// Find an area that has suitable flags matching the trigger type
	const auto *const __restrict aasWorld = AiAasWorld::instance();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto *const __restrict aiManager = AiManager::Instance();

	int desiredAreaContents = 0;
	bool isAPlatform        = false;
	const edict_t *__restrict ent = game.edicts + entNum;
	if( ent->classname ) {
		if( !Q_stricmp( ent->classname, "trigger_push" ) ) {
			desiredAreaContents = AREACONTENTS_JUMPPAD;
		} else if( !Q_stricmp( ent->classname, "trigger_teleport" ) ) {
			desiredAreaContents = AREACONTENTS_TELEPORTER;
		} else if( !Q_stricmp( ent->classname, "trigger_platform" ) ) {
			isAPlatform = true;
		}
	}

	Vec3 mins( ent->r.absmin );
	Vec3 maxs( ent->r.absmax );
	if( desiredAreaContents || isAPlatform ) {
		mins += Vec3( -64, -64, -64 );
		maxs += Vec3( +64, +64, +64 );
	}

	// TODO: Allow passing a growable container, or allow retrieval of the exact number of areas in box
	int boxAreaNumsBuffer[2048];
	const auto boxAreaNums = aasWorld->findAreasInBox( mins.Data(), maxs.Data(), boxAreaNumsBuffer, std::size( boxAreaNumsBuffer ) );
	for( const int areaNum: boxAreaNums ) {
		const auto contents = aasAreaSettings[areaNum].contents;
		if( contents & AREACONTENTS_DONOTENTER ) {
			continue;
		}
		const auto areaflags = aasAreaSettings[areaNum].areaflags;
		if( areaflags & AREA_DISABLED ) {
			continue;
		}
		if( desiredAreaContents ) {
			if( !( contents & desiredAreaContents ) ) {
				continue;
			}
		} else if( isAPlatform ) {
			const auto &areaSettings = aasAreaSettings[areaNum];
			int reachNum             = areaSettings.firstreachablearea;
			const int reachNumLimit  = areaSettings.firstreachablearea + areaSettings.numreachableareas;
			for(; reachNum < reachNumLimit; ++reachNum ) {
				if( aasWorld->getReaches()[reachNum].traveltype == TRAVEL_ELEVATOR ) {
					break;
				}
			}
			if( reachNum == reachNumLimit ) {
				continue;
			}
		}
		// Save this feasible area
		*areaNumRef = areaNum;
		// Break upon reaching a really "good" area
		if( !( areaflags & ( AREA_JUNK | AREA_LEDGE ) ) ) {
			if( aiManager->IsAreaReachableFromHubAreas( areaNum ) ) {
				break;
			}
		}
	}

	// TODO: There is an assumption that it always succeeds
	assert( *areaNumRef );
	return *areaNumRef;
}

auto TriggerAreaNumsCache::getTriggersForArea( int areaNum ) const -> const ClassTriggerNums * {
	const auto *const __restrict aasWorld = AiAasWorld::instance();

	if( m_testedTriggersForArea[areaNum] ) {
		if( m_hasTriggersForArea[areaNum] ) {
			const auto it = m_triggersForArea.find( areaNum );
			assert( it != m_triggersForArea.end() );
			return &it->second;
		}
		return nullptr;
	}

	m_testedTriggersForArea[areaNum] = true;

	unsigned numTeleporters = 0, numJumppads = 0, numPlatforms = 0;
	uint16_t teleporterNums[MAX_EDICTS], jumppadNums[MAX_EDICTS], platformNums[MAX_EDICTS];

	const auto &area = aasWorld->getAreas()[areaNum];

	const auto *const __restrict gameEnts = game.edicts;
	const auto *const __restrict entitiesCache = wsw::ai::ClassifiedEntitiesCache::instance();
	for( const uint16_t num : entitiesCache->getAllPersistentMapJumppads() ) {
		if( GClip_EntityContact( area.mins, area.maxs, gameEnts + num ) ) {
			jumppadNums[numJumppads++] = num;
		}
	}

	for( const uint16_t num : entitiesCache->getAllPersistentMapTeleporters() ) {
		if( GClip_EntityContact( area.mins, area.maxs, gameEnts + num ) ) {
			teleporterNums[numTeleporters++] = num;
		}
	}

	for( const uint16_t num : entitiesCache->getAllPersistentMapPlatformTriggers() ) {
		if( GClip_EntityContact( area.mins, area.maxs, gameEnts + num ) ) {
			platformNums[numPlatforms++] = num;
		}
	}

	m_testedTriggersForArea[areaNum] = true;
	if( const auto numOfNums = ( numTeleporters + numJumppads + numPlatforms ) ) {
		m_hasTriggersForArea[areaNum] = true;
		auto [it, _] = m_triggersForArea.insert( std::make_pair( areaNum, ClassTriggerNums {} ) );
		ClassTriggerNums *const storage = &it->second;
		storage->m_teleporterNumsSpan   = storage->addNums( teleporterNums, numTeleporters );
		storage->m_jumppadNumsSpan      = storage->addNums( jumppadNums, numJumppads );
		storage->m_platformNumsSpan     = storage->addNums( platformNums, numPlatforms );
		return storage;
	}
	return nullptr;
}

auto TriggerAreaNumsCache::getJumppadAreaNumAndTargetAreaNums( int entNum ) const -> std::pair<int, std::span<const int>> {
	const int jumppadAreaNum = getAreaNum( entNum );
	if( const auto it = m_jumppadTargetAreaNums.find( entNum ); it != m_jumppadTargetAreaNums.end() ) {
		return { jumppadAreaNum, it->second };
	}

	auto [it, _] = m_jumppadTargetAreaNums.emplace( std::make_pair( entNum, wsw::PodVector<int> {} ) );
	findJumppadTargetAreaNums( game.edicts + entNum, jumppadAreaNum, &it->second );
	return { jumppadAreaNum, it->second };
}

void TriggerAreaNumsCache::findJumppadTargetAreaNums( const edict_t *jumppadEntity, int jumppadAreaNum, wsw::PodVector<int> *targetAreaNums ) {
	targetAreaNums->clear();

	// CAUTION: jumppadEntity target_ent->r. bounds are invalid
	constexpr float minDeltaZ = 16.0f;
	const float deltaZ = jumppadEntity->target_ent->s.origin[2] - jumppadEntity->r.absmax[2];
	if( deltaZ < minDeltaZ ) {
		aiWarning() << "Weird jumppad target setup";
		return;
	}

	const auto *aasWorld        = AiAasWorld::instance();
	const auto *routeCache      = AiAasRouteCache::Shared();
	const auto *aasAreas        = aasWorld->getAreas().data();
	const auto *aasAreaSettings = aasWorld->getAreaSettings().data();

	int boxAreasBuffer[64];

	int stepNum            = 0;
	constexpr int maxSteps = 10;
	const float stepZ      = ( deltaZ - minDeltaZ ) / maxSteps;
	do {
		// Get areas around the jumppad destination
		Vec3 maxs( +144, +144, 0 );
		Vec3 mins( -144, -144, -( minDeltaZ + stepZ * (float)stepNum ) );
		maxs += jumppadEntity->target_ent->s.origin;
		mins += jumppadEntity->target_ent->s.origin;

		const auto boxAreaNums = aasWorld->findAreasInBox( mins, maxs, boxAreasBuffer, std::size( boxAreasBuffer ) );
		for( int areaNum: boxAreaNums ) {
			const auto &areaSettings = aasAreaSettings[areaNum];
			if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
				continue;
			}
			if( areaSettings.contents & AREACONTENTS_DONOTENTER ) {
				continue;
			}
			if( areaSettings.areaflags & AREA_DISABLED ) {
				continue;
			}
			if( stepNum + 1 < maxSteps ) {
				// Check reachability from jumppad to area and back
				const int fromJumppadTravelFlags = TFL_WALK | TFL_AIR | TFL_JUMPPAD;
				if( !routeCache->FindRoute( jumppadAreaNum, areaNum, fromJumppadTravelFlags ) ) {
					continue;
				}
				const int toJumppadTravelFlags = TFL_WALK | TFL_AIR | TFL_WALKOFFLEDGE;
				if( !routeCache->FindRoute( areaNum, jumppadAreaNum, toJumppadTravelFlags ) ) {
					continue;
				}
			} else {
				// The last resort, just compare by height
				const auto &rawArea = aasAreas[areaNum];
				if( rawArea.mins[2] < jumppadEntity->r.absmax[2] + minDeltaZ ) {
					continue;
				}
			}
			targetAreaNums->push_back( areaNum );
		}
	} while( targetAreaNums->empty() && ++stepNum < maxSteps );
}