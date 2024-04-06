#include "movementlocal.h"
#include "../manager.h"
#include "../classifiedentitiescache.h"

TriggerAreaNumsCache triggerAreaNumsCache;

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

	int desiredAreaContents = ~0;
	const edict_t *__restrict ent = game.edicts + entNum;
	if( ent->classname ) {
		if( !Q_stricmp( ent->classname, "trigger_push" ) ) {
			desiredAreaContents = AREACONTENTS_JUMPPAD;
		} else if( !Q_stricmp( ent->classname, "trigger_teleport" ) ) {
			desiredAreaContents = AREACONTENTS_TELEPORTER;
		}
	}

	*areaNumRef = 0;

	int boxAreaNumsBuffer[64];
	const auto boxAreaNums = aasWorld->findAreasInBox( ent->r.absmin, ent->r.absmax, boxAreaNumsBuffer, 64 );
	for( const int areaNum: boxAreaNums ) {
		if( !( aasAreaSettings[areaNum].contents & desiredAreaContents ) ) {
			continue;
		}
		if( !aiManager->IsAreaReachableFromHubAreas( areaNum ) ) {
			continue;
		}
		*areaNumRef = areaNum;
		break;
	}

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
	const float *__restrict areaMins = area.mins;
	const float *__restrict areaMaxs = area.maxs;

	const auto *const __restrict gameEnts = game.edicts;
	const auto *const __restrict entitiesCache = wsw::ai::ClassifiedEntitiesCache::instance();
	for( const uint16_t num : entitiesCache->getAllPersistentMapJumppads() ) {
		const auto *const __restrict ent = gameEnts + num;
		if( BoundsIntersect( ent->r.absmin, ent->r.absmax, areaMins, areaMaxs ) ) {
			jumppadNums[numJumppads++] = num;
		}
	}

	for( const uint16_t num : entitiesCache->getAllPersistentMapTeleporters() ) {
		const auto *const __restrict ent = gameEnts + num;
		if( BoundsIntersect( ent->r.absmin, ent->r.absmax, areaMins, areaMaxs ) ) {
			teleporterNums[numTeleporters++] = num;
		}
	}

	for( const uint16_t num : entitiesCache->getAllPersistentMapPlatformTriggers() ) {
		const auto *const __restrict ent = gameEnts + num;
		if( BoundsIntersect( ent->r.absmin, ent->r.absmax, areaMins, areaMaxs ) ) {
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

static const float kTopNodeCacheAddToMins[] = { -56, -56, -24 };
static const float kTopNodeCacheAddToMaxs[] = { +56, +56, +24 };

CollisionTopNodeCache::CollisionTopNodeCache() noexcept
	: defaultBoundsCache( "TopNodeCache", kTopNodeCacheAddToMins, kTopNodeCacheAddToMaxs )
	, zeroStepBoundsCache( nullptr, kTopNodeCacheAddToMins, kTopNodeCacheAddToMaxs ) {}

int CollisionTopNodeCache::getTopNode( const float *absMins, const float *absMaxs, bool isZeroStep ) const {
	// Put the likely case first
	if( !isZeroStep ) {
		if( defaultBoundsCache.checkOrUpdateBounds( absMins, absMaxs ) ) {
			return *defaultCachedNode;
		}

		const auto [cachedMins, cachedMaxs] = defaultBoundsCache.getCachedBounds();
		defaultCachedNode = trap_CM_FindTopNodeForBox( cachedMins, cachedMaxs );
		return *defaultCachedNode;
	}

	// Take switching to another bot into account. That's why the bounds test is primarily needed.
	if( !zeroStepBoundsCache.checkOrUpdateBounds( absMins, absMaxs ) ) {
		// zeroStepBoundsCache is updated first so mins/maxs are always valid
		const auto [cachedMins, cachedMaxs] = zeroStepBoundsCache.getCachedBounds();
		cachedZeroStepNode = trap_CM_FindTopNodeForBox( cachedMins, cachedMaxs );
	}

	// Cached bounds initially are illegal so this value gets set on a first checkOnUpdateBounds() call.
	assert( cachedZeroStepNode.has_value() );
	defaultCachedNode = *cachedZeroStepNode;
	// Force an update of the primary bounds cache
	defaultBoundsCache.setFrom( zeroStepBoundsCache );
	return *cachedZeroStepNode;
}

CollisionTopNodeCache collisionTopNodeCache;

static const float kShapesListCacheAddToMins[] = { -64, -64, -32 };
static const float kShapesListCacheAddToMaxs[] = { +64, +64, +32 };

CollisionShapesListCache::CollisionShapesListCache() noexcept
	: defaultBoundsCache( "ShapesListCache", kShapesListCacheAddToMins, kShapesListCacheAddToMaxs )
	, zeroStepBoundsCache( nullptr, kShapesListCacheAddToMins, kShapesListCacheAddToMaxs ) {}

CollisionShapesListCache::~CollisionShapesListCache() {
	GAME_IMPORT.CM_FreeShapeList( defaultCachedList );
	GAME_IMPORT.CM_FreeShapeList( defaultClippedList );
	GAME_IMPORT.CM_FreeShapeList( zeroStepCachedList );
	GAME_IMPORT.CM_FreeShapeList( zeroStepClippedList );
}

CollisionShapesListCache shapesListCache;

constexpr auto kListClipMask = MASK_PLAYERSOLID | MASK_WATER | CONTENTS_TRIGGER | CONTENTS_JUMPPAD | CONTENTS_TELEPORTER;

const CMShapeList *CollisionShapesListCache::prepareList( const float *mins, const float *maxs, bool isZeroStep ) const {
	// Put the likely case first
	if( !isZeroStep ) {
		return defaultPrepareList( mins, maxs );
	}

	if( zeroStepBoundsCache.checkOrUpdateBounds( mins, maxs ) ) {
		activeCachedList = zeroStepCachedList;
		defaultBoundsCache.setFrom( zeroStepBoundsCache );
		return zeroStepClippedList;
	}

	if( !defaultCachedList ) {
		defaultCachedList = GAME_IMPORT.CM_AllocShapeList();
		defaultClippedList = GAME_IMPORT.CM_AllocShapeList();
		zeroStepCachedList = GAME_IMPORT.CM_AllocShapeList();
		zeroStepClippedList = GAME_IMPORT.CM_AllocShapeList();
	}

	const auto [cachedMins, cachedMaxs] = zeroStepBoundsCache.getCachedBounds();
	activeCachedList = GAME_IMPORT.CM_BuildShapeList( zeroStepCachedList, cachedMins, cachedMaxs, kListClipMask );
	GAME_IMPORT.CM_ClipShapeList( zeroStepClippedList, zeroStepCachedList, mins, maxs );

	defaultBoundsCache.setFrom( zeroStepBoundsCache );
	return zeroStepClippedList;
}

const CMShapeList *CollisionShapesListCache::defaultPrepareList( const float *mins, const float *maxs ) const {
	if( defaultBoundsCache.checkOrUpdateBounds( mins, maxs ) ) {
		assert( activeCachedList == defaultCachedList || activeCachedList == zeroStepCachedList );
		GAME_IMPORT.CM_ClipShapeList( defaultClippedList, activeCachedList, mins, maxs );
		return defaultClippedList;
	}

	const auto [cachedMins, cachedMaxs] = defaultBoundsCache.getCachedBounds();
	activeCachedList = GAME_IMPORT.CM_BuildShapeList( defaultCachedList, cachedMins, cachedMaxs, kListClipMask );
	GAME_IMPORT.CM_ClipShapeList( defaultClippedList, activeCachedList, mins, maxs );
	return defaultClippedList;
}

bool ReachChainWalker::Exec() {
	assert( targetAreaNum >= 0 );
	assert( numStartAreas >= 0 );

	lastReachNum = 0;
	startAreaNum = 0;
	lastAreaNum = 0;

	// We have to handle the first reach. separately as we start from up to 2 alternative areas.
	// Also we have to inline PreferredRouteToGoalArea() here to save the actual lastAreaNum for the initial step
	for( int i = 0; i < numStartAreas; ++i ) {
		lastTravelTime = routeCache->PreferredRouteToGoalArea( startAreaNums[i], targetAreaNum, &lastReachNum );
		if( lastTravelTime ) {
			lastAreaNum = startAreaNum = startAreaNums[i];
			break;
		}
	}

	if( !lastTravelTime ) {
		return false;
	}

	const auto *const aasWorld = AiAasWorld::instance();
	const auto aasReach = aasWorld->getReaches();

	assert( (unsigned)lastReachNum < (unsigned)aasReach.size() );
	if( !Accept( lastReachNum, aasReach[lastReachNum], lastTravelTime ) ) {
		return true;
	}

	int areaNum = aasReach[lastReachNum].areanum;
	while( areaNum != targetAreaNum ) {
		lastTravelTime = routeCache->PreferredRouteToGoalArea( areaNum, targetAreaNum, &lastReachNum );
		if( !lastTravelTime ) {
			return false;
		}
		lastAreaNum = areaNum;
		assert( (unsigned)lastReachNum < (unsigned)aasReach.size() );
		const auto &reach = aasReach[lastReachNum];
		if( !Accept( lastReachNum, reach, lastTravelTime ) ) {
			return true;
		}
		areaNum = reach.areanum;
	}

	return true;
}

bool TraceArcInSolidWorld( const vec3_t from, const vec3_t to ) {
	const auto brushMask = MASK_WATER | MASK_SOLID;
	trace_t trace;

	Vec3 midPoint( to );
	midPoint += from;
	midPoint *= 0.5f;

	// Lets figure out deltaZ making an assumption that all forward momentum is converted to the direction to the point
	// Note that we got rid of idea making these tests depending of a current AI entity physics state due to flicker issues.

	const float squareDistanceToMidPoint = wsw::square( from[0] - midPoint.X() ) + wsw::square( from[1] - midPoint.Y() );
	if( squareDistanceToMidPoint < wsw::square( 32 ) ) {
		StaticWorldTrace( &trace, from, to, brushMask );
		return trace.fraction == 1.0f;
	}

	// Assume a default ground movement speed
	const float timeToMidPoint = Q_Sqrt( squareDistanceToMidPoint ) * Q_Rcp( DEFAULT_PLAYERSPEED );
	// Assume an almost default jump speed
	float deltaZ = ( 0.75f * DEFAULT_JUMPSPEED ) * timeToMidPoint;
	deltaZ -= 0.5f * level.gravity * ( timeToMidPoint * timeToMidPoint );

	// Does not worth making an arc
	// Note that we ignore negative deltaZ since the real trajectory differs anyway
	if( deltaZ < 2.0f ) {
		StaticWorldTrace( &trace, from, to, brushMask );
		return trace.fraction == 1.0f;
	}

	midPoint.Z() += deltaZ;

	StaticWorldTrace( &trace, from, midPoint.Data(), brushMask );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	StaticWorldTrace( &trace, midPoint.Data(), to, brushMask );
	return trace.fraction == 1.0f;
}

void DirToKeyInput( const Vec3 &desiredDir, const vec3_t actualForwardDir, const vec3_t actualRightDir, BotInput *input ) {
	input->ClearMovementDirections();

	float dotForward = desiredDir.Dot( actualForwardDir );
	if( dotForward > 0.3 ) {
		input->SetForwardMovement( 1 );
	} else if( dotForward < -0.3 ) {
		input->SetForwardMovement( -1 );
	}

	float dotRight = desiredDir.Dot( actualRightDir );
	if( dotRight > 0.3 ) {
		input->SetRightMovement( 1 );
	} else if( dotRight < -0.3 ) {
		input->SetRightMovement( -1 );
	}

	// Prevent being blocked
	if( !input->ForwardMovement() && !input->RightMovement() ) {
		input->SetForwardMovement( 1 );
	}
}