/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
This file is part of Quake III Arena source code.
Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.
Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

#include "aasroutecache.h"
#include "aaselementsmask.h"
#include "aasstaticroutetable.h"
#include "../ailocal.h"
#include "../bot.h"

#include <common/helpers/links.h>
#include <common/helpers/md5.h>
#include <common/helpers/algorithm.h>

template <typename T> inline T *CastCheckingAlignment( void *ptr ) {
	assert( !( ( (uintptr_t)ptr ) % alignof( T ) ) );
	return (T *)ptr;
}

static inline uint16_t ToUint16CheckingRange( int value ) {
	assert( (unsigned)value <= std::numeric_limits<uint16_t>::max() );
	return (uint16_t)value;
}

// Static member definition
AiAasRouteCache *AiAasRouteCache::shared = nullptr;
AiAasRouteCache *AiAasRouteCache::instancesHead = nullptr;
uint64_t AiAasRouteCache::defaultBlockedAreasDigest[2];

// TODO: We can and should eliminate access to this lookup table
// along with necessity to maintain it
// if AAS file representation is decoupled from the memory one
static int travelFlagForType[MAX_TRAVELTYPES];

void AiAasRouteCache::Init( const AiAasWorld &aasWorld ) {
	constexpr const char *tag = "AiAasRouteCache::Init()";
	if( shared ) {
		AI_FailWith( tag, "The shared instance is already present\n" );
	}
	if( instancesHead ) {
		AI_FailWith( tag, "The instances head is already set\n" );
	}

	// Prepare the lookup table
	InitTravelFlagFromType();

	// Prepare the default blocked areas digest
	InitDefaultBlockedAreasDigest( aasWorld );

	// AiAasRouteCache is quite large, so it should be allocated on heap
	shared = (AiAasRouteCache *)Q_malloc( sizeof( AiAasRouteCache ) );
	new( shared )AiAasRouteCache( *AiAasWorld::instance() );

	instancesHead = shared;
}

void AiAasRouteCache::Shutdown() {
	assert( ( instancesHead == nullptr ) == ( shared == nullptr ) );

	// This may be called on first map load when an instance has never been instantiated
	if( !shared ) {
		return;
	}

	shared->~AiAasRouteCache();
	Q_free( shared );
	// Allow the pointer to be reused, otherwise an assertion will fail on a next Init() call
	shared = nullptr;
	instancesHead = nullptr;
}

AiAasRouteCache *AiAasRouteCache::NewInstance() {
	auto *instance = new( Q_malloc( sizeof( AiAasRouteCache ) ) )AiAasRouteCache( Shared() );
	wsw::link( instance, &AiAasRouteCache::instancesHead );
	return instance;
}

void AiAasRouteCache::ReleaseInstance( AiAasRouteCache *instance ) {
	if( instance == Shared() ) {
		AI_FailWith( "AiAasRouteCache::ReleaseInstance()", "Attempt to release the shared instance\n" );
	}

	wsw::unlink( instance, &AiAasRouteCache::instancesHead );
	instance->~AiAasRouteCache();
	Q_free( instance );
}

AiAasRouteCache::AiAasRouteCache( const AiAasWorld &aasWorld_ )
	: aasWorld( aasWorld_ ) {
	InitCompactReachDataAreaDataAndHelpers();

	InitPathFindingNodes();

	CreateReversedReach();

	InitClusterAreaCache();
	InitPortalCache();

	CalculateAreaTravelTimes();
	InitPortalMaxTravelTimes();

	blockedAreasDigest[0] = AiAasRouteCache::defaultBlockedAreasDigest[0];
	blockedAreasDigest[1] = AiAasRouteCache::defaultBlockedAreasDigest[1];

	loaded = true;
}

AiAasRouteCache::AiAasRouteCache( AiAasRouteCache *parent )
	: aasWorld( parent->aasWorld ), loaded( true ) {
	InitPathFindingNodes();

	// A ref counter is shared for aasRevReach and aasRevLinks
	// as they are allocated within a single memory chunk
	// and aasRevReach is at the beginning of that chunk.
	aasRevReach = AddRef( parent->aasRevReach );
	// Just copy the address.
	aasRevLinks = parent->aasRevLinks;

	// Create a new mutable copy of data based on the parent's one
	areaPathFindingData = parent->CloneAreaPathFindingData();

	// These items share the buffer as well, refer to the explanation above
	reachPathFindingData = AddRef( parent->reachPathFindingData );

	InitClusterAreaCache();
	InitPortalCache();

	areaTravelTimes = AddRef( parent->areaTravelTimes );
	portalMaxTravelTimes = AddRef( parent->portalMaxTravelTimes );

	// Set the digest to the default one for the map
	blockedAreasDigest[0] = AiAasRouteCache::defaultBlockedAreasDigest[0];
	blockedAreasDigest[1] = AiAasRouteCache::defaultBlockedAreasDigest[1];
}

AiAasRouteCache::~AiAasRouteCache() {
	if( !loaded ) {
		return;
	}

	FreeAllClusterAreaCache();
	FreeAllPortalCache();

	FreeRefCountedMemory( areaTravelTimes );
	FreeRefCountedMemory( portalMaxTravelTimes );
	FreeRefCountedMemory( aasRevReach );

	FreeMemory( areaPathFindingNodes );
	FreeMemory( portalPathFindingNodes );

	FreeRefCountedMemory( reachPathFindingData );
	FreeMemory( areaPathFindingData );

	FreeAreaAndPortalMemoryPools();
}

/**
 * We have switched to passing AAS arrays explicitly as arguments to avoid hidden costs of pointer chasing
 */
inline int ClusterAreaNum( std::span<const aas_areasettings_t> aasAreaSettings,
						   std::span<const aas_portal_t> aasPortals,
						   int cluster, int areaNum ) {
	const auto &areaSettings = aasAreaSettings[areaNum];
	const int areaCluster = areaSettings.cluster;
	if( areaCluster > 0 ) {
		return areaSettings.clusterareanum;
	}

	const auto &portal = aasPortals[-areaCluster];
	const int side = portal.frontcluster != cluster;
	return portal.clusterareanum[side];
}

void AiAasRouteCache::InitTravelFlagFromType() {
	for( int &flag: travelFlagForType ) {
		flag = TFL_INVALID;
	}

	travelFlagForType[TRAVEL_INVALID] = TFL_INVALID;
	travelFlagForType[TRAVEL_WALK] = TFL_WALK;
	travelFlagForType[TRAVEL_CROUCH] = TFL_CROUCH;
	travelFlagForType[TRAVEL_BARRIERJUMP] = TFL_BARRIERJUMP;
	travelFlagForType[TRAVEL_JUMP] = TFL_JUMP;
	travelFlagForType[TRAVEL_LADDER] = TFL_LADDER;
	travelFlagForType[TRAVEL_WALKOFFLEDGE] = TFL_WALKOFFLEDGE;
	travelFlagForType[TRAVEL_SWIM] = TFL_SWIM;
	travelFlagForType[TRAVEL_WATERJUMP] = TFL_WATERJUMP;
	travelFlagForType[TRAVEL_TELEPORT] = TFL_TELEPORT;
	travelFlagForType[TRAVEL_ELEVATOR] = TFL_ELEVATOR;
	travelFlagForType[TRAVEL_ROCKETJUMP] = TFL_ROCKETJUMP;
	travelFlagForType[TRAVEL_BFGJUMP] = TFL_BFGJUMP;
	travelFlagForType[TRAVEL_GRAPPLEHOOK] = TFL_GRAPPLEHOOK;
	travelFlagForType[TRAVEL_DOUBLEJUMP] = TFL_DOUBLEJUMP;
	travelFlagForType[TRAVEL_RAMPJUMP] = TFL_RAMPJUMP;
	travelFlagForType[TRAVEL_STRAFEJUMP] = TFL_STRAFEJUMP;
	travelFlagForType[TRAVEL_JUMPPAD] = TFL_JUMPPAD;
	travelFlagForType[TRAVEL_FUNCBOB] = TFL_FUNCBOB;
}

void AiAasRouteCache::InitDefaultBlockedAreasDigest( const AiAasWorld &aasWorld ) {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	bool *const blockedAreasTable = AasElementsMask::BlockedAreasTable();
	for( size_t i = 0; i < aasAreaSettings.size(); ++i ) {
		blockedAreasTable[i] = (bool)( aasAreaSettings[i].areaflags & AREA_DISABLED );
	}

	const auto tableSizeInBytes = (int)( aasAreaSettings.size() * sizeof( bool ) );
	::md5_digest( blockedAreasTable, tableSizeInBytes, (uint8_t *)defaultBlockedAreasDigest );
}

void AiAasRouteCache::UnlinkCache( AreaOrPortalCacheTable *cache ) {
	if( cache->time_next ) {
		cache->time_next->time_prev = cache->time_prev;
	} else {
		newestCache = cache->time_prev;
	}
	if( cache->time_prev ) {
		cache->time_prev->time_next = cache->time_next;
	} else {
		oldestCache = cache->time_next;
	}
	cache->time_next = nullptr;
	cache->time_prev = nullptr;
}

void AiAasRouteCache::LinkCache( AreaOrPortalCacheTable *cache ) {
	if( newestCache ) {
		newestCache->time_next = cache;
		cache->time_prev = newestCache;
	} else {
		oldestCache = cache;
		cache->time_prev = nullptr;
	}
	cache->time_next = nullptr;
	newestCache = cache;
}

void AiAasRouteCache::FreeRoutingCache( AreaOrPortalCacheTable *cache ) {
	UnlinkCache( cache );
	FreeAreaAndPortalCacheMemory( cache );
}

void AiAasRouteCache::SetDisabledZones( DisableZoneRequest **requests, int numRequests ) {
	// Copy the reference to a local var for faster access
	AreaPathFindingData *const areaPathFindingData = this->areaPathFindingData;
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	assert( aasAreaSettings.size() == aasWorld.getAreas().size() );
	const auto numAreas = aasAreaSettings.size();

	// First, save old area statuses and set new ones as non-blocked
	for( size_t i = 0; i < numAreas; ++i ) {
		areaPathFindingData[i].disabledStatus.ShiftCurrToOldStatus();
	}

	auto *const blockedAreasTable = AasElementsMask::BlockedAreasTable();
	memset( blockedAreasTable, 0, numAreas * sizeof( bool ) );

	for( int i = 0; i < numRequests; ++i ) {
		requests[i]->FillBlockedAreasTable( blockedAreasTable );
	}

	// True if there were other blocked areas filled by requests
	bool metCustomBlockedAreas = false;
	// For each selected area mark area as disabled.
	// Put a global (static) disabled status of an area too.
	for( size_t i = 0; i < numAreas; ++i ) {
		// Check this before merging with global blocked status!
		metCustomBlockedAreas = metCustomBlockedAreas | blockedAreasTable[i];
		// Make sure we not only set disabled status but update blocked areas table as well
		// for globally-disabled areas so they are included in the digest of blocked areas.
		blockedAreasTable[i] = blockedAreasTable[i] | (bool)( aasAreaSettings[i].areaflags & AREA_DISABLED );
		if( blockedAreasTable[i] ) {
			areaPathFindingData[i].disabledStatus.SetCurrStatus( true );
		}
	}

	// For each area compare its old and new status
	bool shouldClearCache = false;
	for( size_t i = 0; i < numAreas; ++i ) {
		const auto &status = areaPathFindingData[i].disabledStatus;
		// TODO: We can test multiple statuses using SIMD
		if( status.OldStatus() != status.CurrStatus() ) {
			shouldClearCache = true;
			break;
		}
	}

	// Keep the digest the same in this case
	if( !shouldClearCache ) {
		return;
	}

	ResetAllClusterAreaCache();
	ResetAllPortalCache();

	newestCache = nullptr;
	oldestCache = nullptr;

	m_resultsCache.reset();

	// Reset to the default digest in this case
	if( !metCustomBlockedAreas ) {
		blockedAreasDigest[0] = defaultBlockedAreasDigest[0];
		blockedAreasDigest[1] = defaultBlockedAreasDigest[1];
		return;
	}

	// Save the digest for the new blocked areas vector.
	::md5_digest( blockedAreasTable, numAreas, (uint8_t *)blockedAreasDigest );
}

static int AreaContentsTravelFlags( const aas_areasettings_t &areaSettings ) {
	const int contents = areaSettings.contents;

	int result = 0;
	if( contents & AREACONTENTS_WATER ) {
		result |= TFL_WATER;
	} else if( contents & AREACONTENTS_SLIME ) {
		result |= TFL_SLIME;
	} else if( contents & AREACONTENTS_LAVA ) {
		result |= TFL_LAVA;
	} else {
		result |= TFL_AIR;
	}

	if( contents & AREACONTENTS_DONOTENTER ) {
		result |= TFL_DONOTENTER;
	}
	if( contents & AREACONTENTS_NOTTEAM1 ) {
		result |= TFL_NOTTEAM1;
	}
	if( contents & AREACONTENTS_NOTTEAM2 ) {
		result |= TFL_NOTTEAM2;
	}

	if( areaSettings.areaflags & AREA_BRIDGE ) {
		result |= TFL_BRIDGE;
	}

	return result;
}

void AiAasRouteCache::InitCompactReachDataAreaDataAndHelpers() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasPortals = aasWorld.getPortals();
	const auto aasReaches = aasWorld.getReaches();

	const size_t numAreas = aasAreaSettings.size();
	const size_t numReaches = aasReaches.size();

	static_assert( alignof( ReachPathFindingData ) < alignof( int ), "Alignment assumptions are broken" );
	// We should check this as long as AAS world code does not operate on types of guaranteed fixed size
	static_assert( sizeof( int ) == sizeof( int32_t ), "Assumptions on int being 32 bit are broken" );

	const size_t reachDataSize = numReaches * sizeof( ReachPathFindingData );
	reachPathFindingData = (ReachPathFindingData *)GetClearedRefCountedMemory( reachDataSize );

	const size_t areaDataSize = numAreas * sizeof( AreaPathFindingData );
	areaPathFindingData = (AreaPathFindingData *)GetClearedMemory( areaDataSize );

	for( size_t i = 0; i < numAreas; ++i ) {
		const auto &areaSettings = aasAreaSettings[i];
		auto *const areaData = &areaPathFindingData[i];
		areaData->firstReachNum = ToUint16CheckingRange( areaSettings.firstreachablearea );
		int clusterOrPortalNum = areaSettings.cluster;
		assert( clusterOrPortalNum >= std::numeric_limits<int8_t>::min() );
		assert( clusterOrPortalNum <= std::numeric_limits<int8_t>::max() );
		areaData->clusterOrPortalNum = (int8_t)clusterOrPortalNum;
		int clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, clusterOrPortalNum, i );
		areaData->clusterAreaNum = ToUint16CheckingRange( clusterAreaNum );
		areaData->disabledStatus = AreaDisabledStatus();
		// Set an initial disabled status in this case
		if( aasAreaSettings[i].areaflags & AREA_DISABLED ) {
			areaData->disabledStatus.SetCurrStatus( true );
		}
	}

	for( size_t i = 0; i < numReaches; ++i ) {
		const auto &reach = aasReaches[i];
		uint16_t travelTime = reach.traveltime;
		// Try to avoid ledge areas to prevent unintended falling
		// by increasing the travel time by some penalty value (3 seconds).
		// That's an idea from Doom 3 source code.
		// Apply penalty on areas that do not look like useful as well
		const auto nextAreaFlags = aasAreaSettings[reach.areanum].areaflags;
		if( nextAreaFlags & ( AREA_LEDGE | AREA_JUNK ) ) {
			if( nextAreaFlags & AREA_WALL ) {
				if( nextAreaFlags & AREA_LEDGE ) {
					// If this area has a wall, it usually cannot be avoided, so apply lesser penalty
					travelTime = ToUint16CheckingRange( travelTime + 50 );
				}
				if( nextAreaFlags & AREA_JUNK ) {
					travelTime = ToUint16CheckingRange( travelTime + 100 );
				}
			} else {
				if( nextAreaFlags & AREA_LEDGE ) {
					travelTime = ToUint16CheckingRange( travelTime + 100 );
				}
				if( nextAreaFlags & AREA_JUNK ) {
					travelTime = ToUint16CheckingRange( travelTime + 50 );
				}
			}
		}

		// Combine intrinsic reach travel flags and old "area contents travel flags" for the target area
		int travelFlags = ::travelFlagForType[reach.traveltype & TRAVELTYPE_MASK];
		travelFlags |= AreaContentsTravelFlags( aasAreaSettings[reach.areanum] );

		auto *const &reachData = &reachPathFindingData[i];
		reachData->travelFlags = travelFlags;
		reachData->travelTime = travelTime;
	}
}

AiAasRouteCache::AreaPathFindingData *AiAasRouteCache::CloneAreaPathFindingData() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto numAreas = aasAreaSettings.size();
	const size_t dataSize = numAreas * sizeof( AreaPathFindingData );
	auto *const newData = (AreaPathFindingData *)GetClearedMemory( dataSize );
	assert( this->areaPathFindingData );
	memcpy( newData, this->areaPathFindingData, dataSize );
	// Reset area routing status to a static one (that corresponds to the flags)
	for( size_t i = 0; i < numAreas; ++i ) {
		newData[i].disabledStatus.SetCurrStatus( ( aasAreaSettings[i].areaflags & AREA_DISABLED ) ? true : false );
	}
	return newData;
}

void AiAasRouteCache::CreateReversedReach() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasReaches = aasWorld.getReaches();
	const auto numAreas = aasAreaSettings.size();

	const size_t revReachSize = numAreas * sizeof( RevReach );
	const size_t revLinkSize = aasReaches.size() * sizeof( RevLink );

	auto *ptr = (uint8_t *)GetClearedRefCountedMemory( revReachSize + revLinkSize );

	this->aasRevReach = CastCheckingAlignment<RevReach>( ptr );
	ptr += revReachSize;
	this->aasRevLinks = CastCheckingAlignment<RevLink>( ptr );

	for( size_t i = 1; i < numAreas; ++i ) {
		const auto &areaSettings = aasAreaSettings[i];
		int numReachableAreas = areaSettings.numreachableareas;
		if( numReachableAreas >= 128 ) {
			G_Printf( S_COLOR_YELLOW "area %d has more than 128 reachabilities\n", (int)i );
			numReachableAreas = 128;
		}

		// Create reversed links for the reachabilities
		for( int n = 0; n < numReachableAreas; n++ ) {
			const auto *reach = &aasReaches[areaSettings.firstreachablearea + n];

			auto *revLink = CastCheckingAlignment<RevLink>( ptr );

			revLink->areaNum = ToUint16CheckingRange( i );
			revLink->linkNum = ToUint16CheckingRange( areaSettings.firstreachablearea + n );
			revLink->nextLink = aasRevReach[reach->areanum].firstRevLink;
			aasRevReach[reach->areanum].firstRevLink = ToUint16CheckingRange( (int)( revLink - aasRevLinks ) );
			aasRevReach[reach->areanum].numLinks++;

			ptr += sizeof( RevLink );
		}
	}
}

#define PAD( base, alignment ) ( ( ( base ) + ( alignment ) - 1 ) & ~( ( alignment ) - 1 ) )

void AiAasRouteCache::CalculateAreaTravelTimes() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasReaches = aasWorld.getReaches();
	const auto numAreas = aasAreaSettings.size();

	// "area travel times" is a 3-dimensional array (is viewed as a 3-dimensional array).
	// The outer index is addressed by area numbers.
	// The middle index is addressed by a relative number of a reachability
	// starting from first for the area settings for the area.
	// The inner index is addressed by a number of a link in reversed reachabilities chain for the reachability.
	size_t size = numAreas * sizeof( uint16_t ** );
	for( size_t i = 0; i < numAreas; i++ ) {
		const auto &revReach = aasRevReach[i];
		const int numReachAreas = aasAreaSettings[i].numreachableareas;
		size += numReachAreas * sizeof( uint16_t * );
		size += numReachAreas * PAD( revReach.numLinks, sizeof( void * ) ) * sizeof( uint16_t );
	}

	auto *ptr = (uint8_t *)GetClearedRefCountedMemory( size );
	areaTravelTimes = (uint16_t ***) ptr;

	ptr += numAreas * sizeof( uint16_t ** );

	for( size_t i = 0; i < numAreas; i++ ) {
		const auto &areaSettings = aasAreaSettings[i];

		// This array is addressed by a relative number of reachability
		uint16_t **const areaReachTravelTimes = areaTravelTimes[i] = CastCheckingAlignment<uint16_t *>( ptr );
		ptr += areaSettings.numreachableareas * sizeof( uint16_t * );

		// This is a head of reversed reachabilities chain for the area
		const auto &revReach = aasRevReach[i];
		for( int l = 0; l < areaSettings.numreachableareas; l++ ) {
			uint16_t *const linkTravelTimes = areaReachTravelTimes[l] = CastCheckingAlignment<uint16_t>( ptr );
			ptr += PAD( revReach.numLinks, sizeof( void * ) ) * sizeof( uint16_t );

			const auto &reach = aasReaches[areaSettings.firstreachablearea + l];
			int revLinkNum = revReach.firstRevLink;
			const RevLink *revLink;
			for( int n = 0; n < revReach.numLinks; revLinkNum = revLink->nextLink, n++ ) {
				revLink = aasRevLinks + revLinkNum;
				// This is an inlined and modified body of old AreaTravelTime() call
				// The old comment says:
				// "travel time in hundredths of a second = distance * 100 / speed"
				// The old code used to take a distance as a base and apply "distance factors"
				// for 3 different kinds of movement: walking, swimming and crouching.
				// Crouching detection was based on "area presence type"
				// which is completely wrong set by the AAS compiler
				// (every area that has a small height has other presence type than "normal"
				// even if no crouching is really required (there is an void area above).
				// This used to lead to producing incorrect time estimations.
				float distance = sqrtf( DistanceSquared( aasReaches[revLink->linkNum].end, reach.start ) );
				// Apply "distance factors" tweaked for actual average bot movement speed.
				if( !( areaSettings.areaflags & AREA_LIQUID ) ) {
					distance *= 0.23f;
				} else {
					distance *= 1.00f;
				}
				const auto intDistance = (int)distance;
				if( intDistance > 1 ) {
					linkTravelTimes[n] = ToUint16CheckingRange( intDistance );
					continue;
				}
				// Set to the minimal feasible AAS travel time
				linkTravelTimes[n] = 1;
			}
		}
	}
}

void AiAasRouteCache::InitPortalMaxTravelTimes() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasPortals = aasWorld.getPortals();

	const auto numPortals = (int)aasPortals.size();

	portalMaxTravelTimes = (int *)GetClearedRefCountedMemory( numPortals * sizeof( int ) );

	for( int portalNum = 0; portalNum < numPortals; portalNum++ ) {
		const auto &portal = aasPortals[portalNum];
		const auto portalAreaNum = portal.areanum;
		// Reversed reach. of this portal area
		const auto &revReach = &aasRevReach[portalAreaNum];
		const auto numReachAreas = aasAreaSettings[portalAreaNum].numreachableareas;

		int maxTime = 0;
		for( int l = 0; l < numReachAreas; l++ ) {
			const RevLink *revLink;
			int revLinkNum = revReach->firstRevLink;
			for( int n = 0; n < revReach->numLinks; revLinkNum = revLink->nextLink, n++ ) {
				revLink = aasRevLinks + revLinkNum;
				int t = areaTravelTimes[portalAreaNum][l][n];
				if( t > maxTime ) {
					maxTime = t;
				}
			}
		}

		portalMaxTravelTimes[portalNum] = maxTime;
	}
}

class FreelistPool {
public:
	/**
	 * A header that is put at a beginning of a block before actual user-visible data.
	 * Links are arrays so generic {@code ::link()/::unlink()} facilities can be used.
	 */
	struct BlockHeader {
		BlockHeader *prev[1];
		BlockHeader *next[1];
	};

	static_assert( !( sizeof( BlockHeader ) % 8 ), "BlockHeader size is assumed to be a multiple of 8" );
private:
	// Freelist head
	BlockHeader *freeBlock { nullptr };
	// Actual blocks data
	uint8_t *buffer;

	// An actual blocks data size and a maximal count of blocks.
	const size_t blockSize, maxBlocks;
	size_t blocksInUse { 0 };
public:
	FreelistPool( void *buffer_, size_t bufferSize, size_t blockSize_ );
	virtual ~FreelistPool() = default;

	void *Alloc( size_t size );
	void Free( void *ptr );

	inline bool MayOwn( const void *ptr ) const {
		const uint8_t *comparablePtr = (const uint8_t *)ptr;
		const uint8_t *bufferBoundary = buffer + ( blockSize + sizeof( BlockHeader ) ) * ( maxBlocks );
		return buffer <= comparablePtr && comparablePtr <= bufferBoundary;
	}

	inline size_t Size() const { return blocksInUse; }
	inline size_t Capacity() const { return maxBlocks; }
};

class AreaAndPortalCacheAllocatorBin {
	FreelistPool *freelistPool { nullptr };

	void *usedSingleBlock { nullptr };
	void *freeSingleBlock { nullptr };

	const unsigned numBlocks;
	const unsigned blockSize;

	// Sets the reference in the chunk to its owner so the block can destruct self
	inline void *SetSelfAsTag( void *block ) {
		// Check the block alignment
		// We have to always return 8-byte aligned blocks to follow the malloc contract
		// regardless of address size, so 8 bytes are wasted anyway
		uint64_t *u = (uint64_t *)block;
		u[0] = (uintptr_t)this;
		return u + 1;
	}

	// Don't call directly, use FreeTaggedBlock() instead.
	// A real raw block pointer is expected (that is legal to be fed to G_Free),
	// and Alloc() returns pointers shifted by 8 (a tag is prepended)
	void Free( void *ptr );
public:
	AreaAndPortalCacheAllocatorBin *next { nullptr };

	static unsigned SuggestNumberOfBlocks() {
		// ( 2^16 - 1 ) is the maximum supported number of reachabilities (we use short indices)
		auto frac = AiAasWorld::instance()->getReaches().size() / (float)std::numeric_limits<uint16_t>::max();
		// Requirements do not grow linear. This will be a better approximation.
		frac = std::sqrt( frac );
		return (unsigned)( 512.0f + 768.0f * frac );
	}

	AreaAndPortalCacheAllocatorBin( size_t blockSize_, size_t numBlocks_ = SuggestNumberOfBlocks() )
		: numBlocks( (unsigned)numBlocks_ ), blockSize( (unsigned)blockSize_ ) {}

	~AreaAndPortalCacheAllocatorBin() {
		if( usedSingleBlock ) {
			Q_free( usedSingleBlock );
		}
		if( freeSingleBlock ) {
			Q_free( freeSingleBlock );
		}
		if( freelistPool ) {
			freelistPool->~FreelistPool();
			Q_free( freelistPool );
		}
	}

	void *Alloc( size_t size );

	bool FitsSize( size_t size ) const {
		// Use a strict comparison and do not try to reuse a bin for blocks of different size.
		// There are usually very few size values.
		// Moreover bins for small sizes are addressed by the size.
		// If we try reusing the same bin for different sizes, the cache gets evicted way too often.
		// (these small bins usually correspond to cluster cache).
		return size == blockSize;
	}

	bool NeedsCleanup() const {
		if( !freelistPool ) {
			return false;
		}
		// Raising the threshold leads to pool exhaustion on some maps
		return freelistPool->Size() / (float)freelistPool->Capacity() > 0.66f;
	}

	static void FreeTaggedBlock( void *ptr ) {
		// Check alignment of the supplied pointer
		assert( !( (uintptr_t)ptr % 8 ) );
		// Real block starts 8 bytes below
		uint64_t *realBlock = ( (uint64_t *)ptr ) - 1;
		// Extract the bin stored as a tag
		auto *bin = (AreaAndPortalCacheAllocatorBin *)( (uintptr_t)realBlock[0] );
		// Free the real block registered by the bin
		bin->Free( realBlock );
	}
};

FreelistPool::FreelistPool( void *buffer_, size_t bufferSize, size_t blockSize_ )
	: buffer( (uint8_t *)buffer_ )
	, blockSize( blockSize_ )
	, maxBlocks( bufferSize / ( blockSize_ + sizeof( BlockHeader ) ) ) {
#ifndef PUBLIC_BUILD
	if( ( (uintptr_t)buffer ) & 7 ) {
		AI_FailWith( "FreelistPool::FreelistPool()", "Illegal buffer pointer (should be at least 8-byte-aligned)\n" );
	}
	if( blockSize & 7u ) {
		AI_FailWith( "FreelistPool::FreelistPool()", "Illegal block size (must be a multiple of 8)\n" );
	}
#endif

	freeBlock = nullptr;
	if( !maxBlocks ) {
		return;
	}

	auto *currBlock = (BlockHeader *)this->buffer;
	freeBlock = currBlock;
	if( maxBlocks == 1 ) {
		currBlock->prev[0] = nullptr;
		currBlock->next[0] = nullptr;
		return;
	}

	// We can't use array access on BlockHeader * pointer
	// since a real block size is not a sizeof(BlockHeader).
	// A next block has this offset in bytes from previous one:
	const size_t stride = blockSize + sizeof( BlockHeader );
	uint8_t *nextBlockPtr = this->buffer + stride;
	auto *nextBlock = (BlockHeader *)nextBlockPtr;
	// Setup links in free list for the first block
	currBlock->prev[0] = nullptr;
	currBlock->next[0] = nextBlock;

	auto *prevBlock = currBlock;
	currBlock = nextBlock;
	// Setup links for blocks [1, maxBlocks - 1)
	for( unsigned i = 1; i < maxBlocks - 1; ++i ) {
		nextBlockPtr += stride;
		nextBlock = (BlockHeader *)nextBlockPtr;
		assert( (uint8_t *)currBlock - (uint8_t *)prevBlock == (ptrdiff_t)stride );
		assert( (uint8_t *)nextBlock - (uint8_t *)currBlock == (ptrdiff_t)stride );
		currBlock->prev[0] = prevBlock;
		currBlock->next[0] = nextBlock;
		prevBlock = currBlock;
		currBlock = nextBlock;
	}

	// currBlock must point at the last block
	assert( (void *)currBlock == (void *)( this->buffer + stride * ( maxBlocks - 1 ) ) );
	assert( (void *)prevBlock == (void *)( this->buffer + stride * ( maxBlocks - 2 ) ) );
	currBlock->prev[0] = prevBlock;
	currBlock->next[0] = nullptr;
}

void *FreelistPool::Alloc( size_t size ) {
#ifndef PUBLIC_BUILD
	if( size > blockSize ) {
		constexpr const char *format = "Attempt to allocate more bytes %u than the block size %u\n";
		AI_FailWith( "FreelistPool::Alloc()", format, (unsigned)size, (unsigned)blockSize );
	}

	if( !freeBlock ) {
		AI_FailWith( "FreelistPool::Alloc()", "There are no free blocks left\n" );
	}
#endif

	BlockHeader *const block = wsw::unlink( freeBlock, &freeBlock, 0 );
	++blocksInUse;
	// Return a pointer to a datum after the header
	return block + 1;
}

void FreelistPool::Free( void *ptr ) {
	BlockHeader *const block = ( (BlockHeader *)ptr ) - 1;
	wsw::link( block, &freeBlock, 0 );
	--blocksInUse;
}

void *AreaAndPortalCacheAllocatorBin::Alloc( size_t size ) {
#ifndef PUBLIC_BUILD
	if( size > blockSize ) {
		const char *message = "Don't call Alloc() if the cache is a-priory incapable of allocating chunk of specified size";
		AI_FailWith("AreaAndPortalCacheAllocatorBin::Alloc()", "%s", message );
	}
#endif

	constexpr auto TAG_SIZE = 8;
	static_assert( !( TAG_SIZE % 8 ), "TAG_SIZE must be a multiple of 8" );

	// Once the freelist pool for many chunks has been initialized, it handles all allocation requests.
	if( freelistPool ) {
		// Allocate 8 extra bytes for the tag
		if( freelistPool->Size() != freelistPool->Capacity() ) {
			return SetSelfAsTag( freelistPool->Alloc( size + TAG_SIZE ) );
		} else {
			// The pool capacity has been exhausted. Fall back to using G_Malloc()
			// This is not a desired behavior but we should not crash in these extreme cases.
			return SetSelfAsTag( Q_malloc( (size_t)( size + TAG_SIZE ) ) );
		}
	}

	// Check if there is a free unpooled block saved for further allocations.
	// Check then whether no unpooled blocks were used at all, and allocate a new one.
	if( freeSingleBlock ) {
		assert( !usedSingleBlock );
		usedSingleBlock = freeSingleBlock;
		freeSingleBlock = nullptr;
		return SetSelfAsTag( usedSingleBlock );
	} else if( !usedSingleBlock ) {
		// Allocate 8 extra bytes for the tag
		usedSingleBlock = Q_malloc( (size_t)( size + TAG_SIZE ) );
		return SetSelfAsTag( usedSingleBlock );
	}

	// Too many blocks is going to be used.
	// Allocate a freelist pool in a single continuous memory block, and use it for further allocations.

	// The block size must be aligned itself, that is a FreelistPool() contract
	size_t alignedBlockSize = blockSize;
	if( alignedBlockSize % 8 ) {
		alignedBlockSize += 8 - blockSize % 8;
	}

	// Each block needs some space for the header and 8 extra bytes for the block tag
	size_t bufferSize = ( alignedBlockSize + sizeof( FreelistPool::BlockHeader ) + TAG_SIZE ) * numBlocks;
	size_t bufferAlignmentBytes = 0;
	if( sizeof( FreelistPool ) % 8 ) {
		bufferAlignmentBytes = 8 - sizeof( FreelistPool ) % 8;
	}

	size_t memSize = sizeof( FreelistPool ) + bufferAlignmentBytes + bufferSize;
	uint8_t *memBlock = (uint8_t *)Q_malloc( memSize );
	memset( memBlock, 0, memSize );
	uint8_t *poolBuffer = memBlock + sizeof( FreelistPool ) + bufferAlignmentBytes;
	// Note: It is important to tell the pool about the extra space occupied by block tags
	freelistPool = new( memBlock )FreelistPool( poolBuffer, bufferSize, alignedBlockSize + TAG_SIZE );
	return SetSelfAsTag( freelistPool->Alloc( size + TAG_SIZE ) );
}

void AreaAndPortalCacheAllocatorBin::Free( void *ptr ) {
	if( ptr != usedSingleBlock ) {
		// Check whether it has been allocated by the freelist pool
		// or has been allocated via G_Malloc() if the pool capacity has been exceeded.
		if( freelistPool->MayOwn( ptr ) ) {
			freelistPool->Free( ptr );
		} else {
			Q_free( ptr );
		}
		return;
	}

	assert( !freeSingleBlock );
	// If the freelist pool is not allocated yet at the moment of this Free() call,
	// its likely there is no further necessity in the pool.
	// Save the single block for further use.
	if( !freelistPool ) {
		freeSingleBlock = usedSingleBlock;
	} else {
		// Free the single block, as further allocations are handled by the freelist pool.
		Q_free( usedSingleBlock );
	}

	usedSingleBlock = nullptr;
}

void *AiAasRouteCache::GetClearedMemory( size_t size ) {
	void *mem = Q_malloc( size );
	::memset( mem, 0, size );
	return mem;
}

void AiAasRouteCache::FreeMemory( void *ptr ) {
	Q_free( ptr );
}

void *AiAasRouteCache::AllocAreaAndPortalCacheMemory( size_t size ) {
	// Check whether the corresponding bin chunk size is small enough
	// to allow the bin to be addressed directly by size.
	if( size < sizeof( areaAndPortalSmallBinsTable ) / sizeof( *areaAndPortalSmallBinsTable ) ) {
		if( auto *bin = areaAndPortalSmallBinsTable[size] ) {
			assert( bin->FitsSize( size ) );
			if( bin->NeedsCleanup() ) {
				FreeOldestCache();
			}
			return bin->Alloc( size );
		}

		// Create a new bin for the size
		void *mem = Q_malloc( sizeof( AreaAndPortalCacheAllocatorBin ) );
		memset( mem, 0, sizeof( AreaAndPortalCacheAllocatorBin ) );
		auto *newBin = new( mem )AreaAndPortalCacheAllocatorBin( size );
		areaAndPortalSmallBinsTable[size] = newBin;
		return newBin->Alloc( size );
	}

	// Check whether there are bins able to handle the request in the common bins list
	for( AreaAndPortalCacheAllocatorBin *bin = areaAndPortalCacheHead; bin; bin = bin->next ) {
		if( bin->FitsSize( size ) ) {
			if( bin->NeedsCleanup() ) {
				FreeOldestCache();
			}
			return bin->Alloc( size );
		}
	}

	// Create a new bin for the size
	void *mem = Q_malloc( sizeof( AreaAndPortalCacheAllocatorBin ) );
	memset( mem, 0, sizeof( AreaAndPortalCacheAllocatorBin ) );
	auto *newBin = new( mem )AreaAndPortalCacheAllocatorBin( size );

	// Link it to the bins list head
	newBin->next = areaAndPortalCacheHead;
	areaAndPortalCacheHead = newBin;

	return newBin->Alloc( size );
}

void AiAasRouteCache::FreeAreaAndPortalCacheMemory( void *ptr ) {
	// The chunk stores its owner as a tag
	AreaAndPortalCacheAllocatorBin::FreeTaggedBlock( ptr );
}

void AiAasRouteCache::FreeAreaAndPortalMemoryPools() {
	for( AreaAndPortalCacheAllocatorBin *bin = areaAndPortalCacheHead, *nextBin = nullptr; bin; bin = nextBin ) {
		nextBin = bin->next;
		bin->~AreaAndPortalCacheAllocatorBin();
		Q_free( bin );
	}

	int binsTableCapacity = sizeof( areaAndPortalSmallBinsTable ) / sizeof( areaAndPortalSmallBinsTable[0] );
	for( int i = 0; i < binsTableCapacity; ++i ) {
		if( areaAndPortalSmallBinsTable[i] ) {
			areaAndPortalSmallBinsTable[i]->~AreaAndPortalCacheAllocatorBin();
			Q_free( areaAndPortalSmallBinsTable[i] );
		}
	}
}

bool AiAasRouteCache::FreeOldestCache() {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasPortals = aasWorld.getPortals();
	if( auto *cache = oldestCache ) {
		if( cache->prev ) {
			cache->prev->next = cache->next;
		} else {
			// TODO: Area and portal caches must belong to different lists! Avoid this branching!
			if( cache->type == CACHETYPE_AREA ) {
				auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, cache->cluster, cache->areaNum );
				clusterAreaCache[cache->cluster][clusterAreaNum] = cache->next;
			} else {
				portalCache[cache->areaNum] = cache->next;
			}
		}
		if( cache->next ) {
			cache->next->prev = cache->prev;
		}

		FreeRoutingCache( cache );
		return true;
	}

	return false;
}

AiAasRouteCache::AreaOrPortalCacheTable *AiAasRouteCache::AllocRoutingCache( int numTravelTimes, bool zeroMemory ) {
	size_t size = sizeof( AreaOrPortalCacheTable );
	size += numTravelTimes * sizeof( uint16_t );
	size += numTravelTimes * sizeof( uint8_t );

	auto *const cache = (AreaOrPortalCacheTable *)AllocAreaAndPortalCacheMemory( size );
	if( zeroMemory ) {
		::memset( cache, 0, size );
	}

	return cache;
}

void AiAasRouteCache::FreeAllClusterAreaCache() {
	if( !clusterAreaCache ) {
		return;
	}

	ResetAllClusterAreaCache();
	FreeMemory( clusterAreaCache );
	clusterAreaCache = nullptr;
}

void AiAasRouteCache::ResetAllClusterAreaCache() {
	assert( clusterAreaCache );

	const auto aasClusters = aasWorld.getClusters();
	for( size_t i = 0, end = aasClusters.size(); i < end; i++ ) {
		const auto *const cluster = &aasClusters[i];
		for( int j = 0; j < cluster->numareas; j++ ) {
			AreaOrPortalCacheTable *nextCache;
			for( auto *cache = clusterAreaCache[i][j]; cache; cache = nextCache ) {
				nextCache = cache->next;
				FreeAreaAndPortalCacheMemory( cache );
			}
			clusterAreaCache[i][j] = nullptr;
		}
	}
}

void AiAasRouteCache::InitClusterAreaCache() {
	const auto aasClusters = aasWorld.getClusters();
	const auto numClusters = aasClusters.size();

	size_t totalNumAreas = 0;
	for( size_t i = 0; i < numClusters; i++ ) {
		totalNumAreas += aasClusters[i].numareas;
	}

	// The "cluster are cache" is a two dimensional array with pointers
	// for every cluster to routing cache for every area in that cluster
	size_t numBytes = numClusters * sizeof( AreaOrPortalCacheTable ** );
	numBytes += totalNumAreas * sizeof( AreaOrPortalCacheTable * );
	auto *ptr = (uint8_t *) GetClearedMemory( numBytes );

	clusterAreaCache = (AreaOrPortalCacheTable ***)ptr;

	ptr += numClusters * sizeof( AreaOrPortalCacheTable ** );
	for( size_t i = 0; i < numClusters; i++ ) {
		clusterAreaCache[i] = CastCheckingAlignment<AreaOrPortalCacheTable *>( ptr );
		ptr += aasClusters[i].numareas * sizeof( AreaOrPortalCacheTable * );
	}
}

void AiAasRouteCache::FreeAllPortalCache() {
	if( !portalCache ) {
		return;
	}

	ResetAllPortalCache();
	FreeMemory( portalCache );
	portalCache = nullptr;
}

void AiAasRouteCache::ResetAllPortalCache() {
	assert( portalCache );
	for( int i = 0, end = aasWorld.getAreas().size(); i < end; i++ ) {
		AreaOrPortalCacheTable *nextCache;
		for( auto *cache = portalCache[i]; cache; cache = nextCache ) {
			nextCache = cache->next;
			FreeAreaAndPortalCacheMemory( cache );
		}
		portalCache[i] = nullptr;
	}
}

void AiAasRouteCache::InitPortalCache() {
	portalCache = (AreaOrPortalCacheTable **)GetClearedMemory( aasWorld.getAreas().size() * sizeof( AreaOrPortalCacheTable * ) );
}

void AiAasRouteCache::InitPathFindingNodes() {
	maxReachAreas = 0;
	for( const auto &cluster: aasWorld.getClusters() ) {
		int numReachAreas = cluster.numreachabilityareas;
		if( numReachAreas > maxReachAreas ) {
			maxReachAreas = numReachAreas;
		}
	}

	areaPathFindingNodes = (PathFinderNode *)GetClearedMemory( maxReachAreas * sizeof( PathFinderNode ) );
	portalPathFindingNodes = (PathFinderNode *)GetClearedMemory( ( aasWorld.getPortals().size() + 1 ) * sizeof( PathFinderNode ) );

	oldestCache = nullptr;
	newestCache = nullptr;
}

/**
 * We force 4-byte alignment in hope a compiler will operate with this data type
 * as with a single 32-bit word and not as two smaller values
 */
struct alignas( 4 )RoutingUpdateRef {
	uint16_t index;
	uint16_t tmpTravelTime;

	RoutingUpdateRef( int index_, uint16_t tmpTravelTime_ )
		: index( ToUint16CheckingRange( index_ ) ), tmpTravelTime( tmpTravelTime_ ) {}

	bool operator<( const RoutingUpdateRef &that ) const {
		return tmpTravelTime > that.tmpTravelTime;
	}
};

static_assert( sizeof( RoutingUpdateRef ) == 4, "" );
static_assert( alignof( RoutingUpdateRef ) == 4, "" );

// Dijkstra's algorithm labels
constexpr const int8_t UNREACHED = 0;
constexpr const int8_t LABELED = -1;
constexpr const int8_t SCANNED = +1;

struct BucketNode {
	uint16_t areaNum { 0 };
	uint16_t nextNodeNum { 0 };
};

struct MonotonicIntegerHeap {
	BucketNode nodesStorage[(1u << 16) + 1];
	uint16_t bucketHeads[(1u << 16)];
	int numUsedNodes { 0 };

	void clear() {
		// Let zero refer to a null node
		// so we can use memset for fast preparation of buckets
		memset( bucketHeads, 0, sizeof( bucketHeads ) );
	}

	// We do not want to put additional methods to ensure there's no
	// hidden function call costs in the tight path-finding loop.
};

// Let it be global for saving memory bandwidth when switching from bot to bot.
// It's unlikely that routing is going performed from multiple threads simultaneously in foreseeable future.
static MonotonicIntegerHeap globalHeap;

void AiAasRouteCache::UpdateAreaRoutingCache( std::span<const aas_areasettings_t> aasAreaSettings,
											  std::span<const aas_portal_t> aasPortals,
											  AreaOrPortalCacheTable *areaCache ) const {
	//NOTE: not more than 128 reachabilities per area allowed
	uint16_t startAreaTravelTimes[128];

	//number of reachability areas within this cluster
	const int numReachAreas = aasWorld.getClusters()[areaCache->cluster].numreachabilityareas;
	const int badTravelFlags = ~areaCache->travelFlags;

	auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, areaCache->cluster, areaCache->areaNum );
	if( clusterAreaNum >= numReachAreas ) {
		return;
	}

	memset( startAreaTravelTimes, 0, sizeof( startAreaTravelTimes ) );

	// Precache all references to avoid pointer chasing in loop
	const auto *const aasRevReach = this->aasRevReach;
	const auto *const aasRevLinks = this->aasRevLinks;
	auto *const pathFindingNodes = this->areaPathFindingNodes;
	const auto *const areaPathFindingData = this->areaPathFindingData;
	const auto *const reachPathFindingData = this->reachPathFindingData;

	for( int i = 0, end = maxReachAreas; i < end; ++i ) {
		pathFindingNodes[i].dijkstraLabel = UNREACHED;
	}

	MonotonicIntegerHeap *const __restrict heap = &::globalHeap;
	heap->clear();

	PathFinderNode *currAreaNode = &pathFindingNodes[clusterAreaNum];
	currAreaNode->areaNum = areaCache->areaNum;
	currAreaNode->areaTravelTimes = startAreaTravelTimes;
	currAreaNode->tmpTravelTime = ToUint16CheckingRange( areaCache->startTravelTime );
	areaCache->travelTimes[clusterAreaNum] = ToUint16CheckingRange( areaCache->startTravelTime );
	currAreaNode->dijkstraLabel = LABELED;

	// Put the initial node after the first element
	// (0 is considered a "null" node index)
	heap->nodesStorage[1].areaNum = clusterAreaNum;
	heap->nodesStorage[1].nextNodeNum = 0;
	heap->bucketHeads[0] = 1;
	heap->numUsedNodes = 2;

	int lastBucketTime = 0;
	int maxTravelTimeSoFar = 0;

	//while there are updates in the current list
	for(; lastBucketTime <= maxTravelTimeSoFar; lastBucketTime++ ) {
		int heapNodeNum = heap->bucketHeads[lastBucketTime];
		for(; heapNodeNum; ) {
			const BucketNode *currHeapNode = &heap->nodesStorage[heapNodeNum];
			currAreaNode = &pathFindingNodes[currHeapNode->areaNum];
			currAreaNode->dijkstraLabel = SCANNED;
			heapNodeNum = currHeapNode->nextNodeNum;

			// Check all reversed reachability links
			const auto &revReach = aasRevReach[currAreaNode->areaNum];
			const int numLinks = revReach.numLinks;
			int revLinkNum = revReach.firstRevLink;
			const RevLink *revLink;
			int revLinkAreaIndex = 0;
			for(; revLinkAreaIndex < numLinks; revLinkNum = revLink->nextLink, revLinkAreaIndex++ ) {
				revLink = aasRevLinks + revLinkNum;
				const auto reachNum = revLink->linkNum;
				const auto &reachData = reachPathFindingData[reachNum];
				// If the reachability has an undesired travel type
				if( reachData.travelFlags & badTravelFlags ) {
					continue;
				}

				// Number of the area the reversed reachability leads to
				const auto nextAreaNum = revLink->areaNum;

				const auto &nextAreaData = areaPathFindingData[nextAreaNum];
				// If it is not allowed to enter the next area
				if( nextAreaData.disabledStatus.CurrStatus() ) {
					continue;
				}

				// Get the cluster number of the area
				const auto clusterOrPortalNum = nextAreaData.clusterOrPortalNum;
				// Don't leave the cluster
				if( clusterOrPortalNum > 0 && clusterOrPortalNum != areaCache->cluster ) {
					continue;
				}

				clusterAreaNum = nextAreaData.clusterAreaNum;
				if( clusterAreaNum >= numReachAreas ) {
					continue;
				}

				auto *const nextNode = &pathFindingNodes[clusterAreaNum];
				if( nextNode->dijkstraLabel != UNREACHED ) {
					continue;
				}

				// Time already travelled
				// plus the traveltime through the current area
				// plus the travel time from the reachability
				int relaxedTime = currAreaNode->tmpTravelTime;
				relaxedTime += currAreaNode->areaTravelTimes[revLinkAreaIndex];
				relaxedTime += reachData.travelTime;
				// We must check overflow, should never happen in production
				uint16_t t = ToUint16CheckingRange( relaxedTime );

				// Check whether we can "relax" the edge (in Dijkstra terms)
				auto *const timeToRelax = &areaCache->travelTimes[clusterAreaNum];
				if( *timeToRelax && *timeToRelax <= t ) {
					continue;
				}

				*timeToRelax = t;
				maxTravelTimeSoFar = wsw::max( relaxedTime, maxTravelTimeSoFar );

				const auto reachOffset = reachNum - nextAreaData.firstReachNum;
				assert( (unsigned) reachOffset < 255 );
				areaCache->reachOffsets[clusterAreaNum] = (uint8_t) reachOffset;

				nextNode->areaNum = ToUint16CheckingRange( nextAreaNum );
				nextNode->tmpTravelTime = t;
				nextNode->areaTravelTimes = areaTravelTimes[nextAreaNum][reachOffset];
				nextNode->dijkstraLabel = LABELED;

				// Allocate a new node for the `clusterAreaNum` area
				const auto newNodeNum = (uint16_t)heap->numUsedNodes++;
				BucketNode *const newNode = &heap->nodesStorage[newNodeNum];
				newNode->areaNum = clusterAreaNum;
				// Link the newly allocated node to the bucket for `relaxedTime`
				newNode->nextNodeNum = heap->bucketHeads[relaxedTime];
				heap->bucketHeads[relaxedTime] = newNodeNum;
			}
		}
	}
}

inline void AiAasRouteCache::AreaOrPortalCacheTable::FixVarLenDataRefs( int numTravelTimes ) {
	auto *base = ( uint8_t * )this;
	this->travelTimes = (uint16_t *)( base + sizeof( AreaOrPortalCacheTable ) );
	assert( ( (uintptr_t)this->travelTimes ) % sizeof( uint16_t ) == 0 );
	this->reachOffsets = base + sizeof( AreaOrPortalCacheTable ) + numTravelTimes * sizeof( uint16_t );
	this->size = sizeof( AreaOrPortalCacheTable ) + numTravelTimes * ( sizeof( uint16_t ) + sizeof( uint8_t ) );
}

inline void AiAasRouteCache::AreaOrPortalCacheTable::SetPathFindingProps( int cluster_, int areaNum_, int travelFlags_ ) {
	this->cluster = ToUint16CheckingRange( cluster_ );
	this->areaNum = ToUint16CheckingRange( areaNum_ );
	this->startTravelTime = 1;
	this->travelFlags = travelFlags_;
}

AiAasRouteCache::AreaOrPortalCacheTable *
AiAasRouteCache::GetAreaRoutingCache( std::span<const aas_areasettings_t> aasAreaSettings,
									  std::span<const aas_portal_t> aasPortals,
									  int clusterNum, int areaNum, int travelFlags ) {
	//number of the area in the cluster
	const auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, clusterNum, areaNum );
	//find the cache without undesired travel flags
	AreaOrPortalCacheTable *cache = clusterAreaCache[clusterNum][clusterAreaNum];
	for(; cache; cache = cache->next ) {
		//if there aren't used any undesired travel types for the cache
		if( cache->travelFlags == travelFlags ) {
			break;
		}
	}

	if( !cache ) {
		const int numTravelTimes = aasWorld.getClusters()[clusterNum].numreachabilityareas;
		// Try checking whether siblings have a cache for this area
		if( const auto *siblingCache = FindSiblingCache( clusterNum, clusterAreaNum, travelFlags ) ) {
			// Allocate a raw memory chunk as we're about to overwrite it
			cache = AllocRoutingCache( numTravelTimes, false );
			// Copy the data behind the header
			auto *const dest = (uint8_t *)cache + sizeof( AreaOrPortalCacheTable );
			const auto *src = (uint8_t *)siblingCache + sizeof( AreaOrPortalCacheTable );
			::memcpy( dest, src, siblingCache->size - sizeof( AreaOrPortalCacheTable ) );
			// Prevent occasionally sharing sibling mutable data (pointers)
			memset( cache, 0, sizeof( AreaOrPortalCacheTable ) );
			// Fix header fields
			cache->FixVarLenDataRefs( numTravelTimes );
			// Make sure the sibling cache was valid
			assert( cache->size == siblingCache->size );
			cache->SetPathFindingProps( clusterNum, areaNum, travelFlags );
		} else {
			// Allocate a clean memory chunk
			cache = AllocRoutingCache( numTravelTimes, true );
			cache->FixVarLenDataRefs( numTravelTimes );
			cache->SetPathFindingProps( clusterNum, areaNum, travelFlags );
			UpdateAreaRoutingCache( aasAreaSettings, aasPortals, cache );
		}

		auto *oldCacheHead = clusterAreaCache[clusterNum][clusterAreaNum];
		assert( cache->prev == nullptr );
		cache->next = oldCacheHead;
		if( oldCacheHead ) {
			oldCacheHead->prev = cache;
		}
		clusterAreaCache[clusterNum][clusterAreaNum] = cache;
	} else {
		UnlinkCache( cache );
	}

	cache->type = CACHETYPE_AREA;
	LinkCache( cache );
	return cache;
}

const AiAasRouteCache::AreaOrPortalCacheTable *
AiAasRouteCache::FindSiblingCache( int clusterNum, int clusterAreaNum, int travelFlags ) const {
	// We're not 100% confident yet whether the implementation is valid.
	// Add an option to override the sharing behaviour if sharing cache for some maps lead to troubles.
	if( !v_shareRoutingCache.get() ) {
		return nullptr;
	}

	for( const auto *that = AiAasRouteCache::instancesHead; that; that = that->next ) {
		// Make sure we're using the same digest
		// (it's very likely we have the same blocked areas vector in this case)
		if( !BlockedAreasDigestsMatch( that->blockedAreasDigest, this->blockedAreasDigest ) ) {
			continue;
		}

		const AreaOrPortalCacheTable *cache = that->clusterAreaCache[clusterNum][clusterAreaNum];
		for(; cache; cache = cache->next ) {
			if( cache->travelFlags == travelFlags ) {
				return cache;
			}
		}
	}

	return nullptr;
}

void AiAasRouteCache::UpdatePortalRoutingCache( AreaOrPortalCacheTable *portalCache ) {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasPortalIndex = aasWorld.getPortalIndex();
	const auto aasPortals = aasWorld.getPortals();
	const auto aasClusters = aasWorld.getClusters();
	auto *const portalMaxTravelTimes = this->portalMaxTravelTimes;
	auto *const pathFindingNodes = this->portalPathFindingNodes;

	const size_t numPortals = aasPortals.size();
	for( size_t i = 0; i < numPortals + 1; ++i ) {
		pathFindingNodes[i].dijkstraLabel = UNREACHED;
	}

	PathFinderNode *currNode = &pathFindingNodes[numPortals];
	currNode->cluster = portalCache->cluster;
	currNode->areaNum = portalCache->areaNum;
	currNode->tmpTravelTime = ToUint16CheckingRange( portalCache->startTravelTime );
	currNode->dijkstraLabel = LABELED;

	// If the start area is a cluster portal, store the travel time for that portal
	const auto clusterNum = aasAreaSettings[portalCache->areaNum].cluster;
	if( clusterNum < 0 ) {
		portalCache->travelTimes[-clusterNum] = ToUint16CheckingRange( portalCache->startTravelTime );
	}

	wsw::StaticVector<RoutingUpdateRef, 1024> updateHeap;
	updateHeap.push_back( RoutingUpdateRef( numPortals, ToUint16CheckingRange( portalCache->startTravelTime ) ) );

	//while there are updates in the current list
	while( !updateHeap.empty() ) {
		wsw::pop_heap( updateHeap.begin(), updateHeap.end() );
		currNode = &portalPathFindingNodes[updateHeap.back().index];
		currNode->dijkstraLabel = SCANNED;
		updateHeap.pop_back();

		// Fix invalid access to cluster 0
		if( !currNode->cluster ) {
			continue;
		}

		const auto *cluster = &aasClusters[currNode->cluster];
		const auto *cache = GetAreaRoutingCache( aasAreaSettings, aasPortals, currNode->cluster,
												 currNode->areaNum, portalCache->travelFlags );
		// Take all portals of the cluster
		for( int i = 0; i < cluster->numportals; i++ ) {
			const auto portalNum = aasPortalIndex[cluster->firstportal + i];
			const auto *portal = &aasPortals[portalNum];
			//if this is the portal of the current update continue
			if( portal->areanum == currNode->areaNum ) {
				continue;
			}
			auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, currNode->cluster, portal->areanum );
			if( clusterAreaNum >= cluster->numreachabilityareas ) {
				continue;
			}

			uint16_t t = cache->travelTimes[clusterAreaNum];
			if( !t ) {
				continue;
			}
			t = ToUint16CheckingRange( t + currNode->tmpTravelTime );

			// Check whether we can "relax" the edge (in Dijkstra term)
			if( portalCache->travelTimes[portalNum] && portalCache->travelTimes[portalNum] <= t ) {
				continue;
			}

			portalCache->travelTimes[portalNum] = t;
			auto *const nextNode = &pathFindingNodes[portalNum];
			if( portal->frontcluster == currNode->cluster ) {
				nextNode->cluster = ToUint16CheckingRange( portal->backcluster );
			} else {
				nextNode->cluster = ToUint16CheckingRange( portal->frontcluster );
			}
			nextNode->areaNum = ToUint16CheckingRange( portal->areanum );
			//add travel time through the actual portal area for the next update
			nextNode->tmpTravelTime = ToUint16CheckingRange( t + portalMaxTravelTimes[portalNum] );
			if( nextNode->dijkstraLabel != UNREACHED ) {
				continue;
			}

			nextNode->dijkstraLabel = LABELED;
			updateHeap.push_back( RoutingUpdateRef( portalNum, nextNode->tmpTravelTime ) );
			wsw::push_heap( updateHeap.begin(), updateHeap.end() );
		}
	}
}

AiAasRouteCache::AreaOrPortalCacheTable *
AiAasRouteCache::GetPortalRoutingCache( std::span<const aas_areasettings_t> aasAreaSettings,
										std::span<const aas_portal_t> aasPortals,
										int clusterNum, int areaNum, int travelFlags ) {
	AreaOrPortalCacheTable *cache;
	//find the cached portal routing if existing
	for( cache = portalCache[areaNum]; cache; cache = cache->next ) {
		if( cache->travelFlags == travelFlags ) {
			break;
		}
	}
	//if the portal routing isn't cached
	if( !cache ) {
		cache = AllocRoutingCache( aasWorld.getPortals().size() );
		cache->FixVarLenDataRefs( aasWorld.getPortals().size() );
		cache->SetPathFindingProps( clusterNum, areaNum, travelFlags );
		//add the cache to the cache list
		cache->prev = nullptr;
		// Warning! Do not precache this reference at the beginning!
		// AllocRoutingCache() calls might modify the member!
		auto *oldCacheHead = portalCache[areaNum];
		cache->next = oldCacheHead;
		if( oldCacheHead ) {
			oldCacheHead->prev = cache;
		}
		portalCache[areaNum] = cache;
		//update the cache
		UpdatePortalRoutingCache( cache );
	} else {
		UnlinkCache( cache );
	}
	//the cache has been accessed
	cache->type = CACHETYPE_PORTAL;
	LinkCache( cache );
	return cache;
}

int AiAasRouteCache::FindRoute( const int *fromAreaNums, int numFromAreas, int toAreaNum, int travelFlags, int *reachNum ) const {
	int bestTravelTime = std::numeric_limits<int>::max();
	int bestReachNum = 0;

	for( int j = 0; j < numFromAreas; ++j ) {
		RoutingResult routingResult;
		if( RoutingResultToGoalArea( fromAreaNums[j], toAreaNum, travelFlags, &routingResult ) ) {
			if( bestTravelTime > routingResult.travelTime ) {
				bestTravelTime = routingResult.travelTime;
				bestReachNum = routingResult.reachNum;
			}
		}
	}

	if( bestTravelTime == std::numeric_limits<int>::max() ) {
		return 0;
	}

	*reachNum = bestReachNum;
	return bestTravelTime;
}

bool AiAasRouteCache::RoutingResultToGoalArea( int fromAreaNum, int toAreaNum,
											   int travelFlags, RoutingResult *result ) const {
	if( fromAreaNum == toAreaNum ) {
		result->travelTime = 1;
		result->reachNum = 0;
		return true;
	}

	const auto aasAreas = aasWorld.getAreas();
	if( (unsigned)( fromAreaNum - 1 ) >= (unsigned)( aasAreas.size() - 1 ) ) {
		return false;
	}

	if( (unsigned)( toAreaNum - 1 ) >= (unsigned)( aasAreas.size() - 1 ) ) {
		return false;
	}

	if( aasWorld.isAreaADoNotEnterArea( fromAreaNum ) || aasWorld.isAreaADoNotEnterArea( toAreaNum ) ) {
		travelFlags |= TFL_DONOTENTER;
	}

	auto *const nonConstThis = const_cast<AiAasRouteCache *>( this );
#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
	// Bypass the results cache due to reentrancy problems during these checks
	if( AasStaticRouteTable::s_isCheckingMatchWithRouteCache ) [[unlikely]] {
		RoutingRequest request( fromAreaNum, toAreaNum, travelFlags );
		return nonConstThis->FindRoute( request, result );
	}
#endif

	const uint64_t key      = FastRoutingResultsCache::makeKey( fromAreaNum, toAreaNum, travelFlags );
	const uint16_t binIndex = FastRoutingResultsCache::calcBinIndexForKey( key );
	if( const FastRoutingResultsCache::Node *cacheNode = m_resultsCache.getCachedResultForKey( binIndex, key ) ) {
		result->reachNum   = cacheNode->reachability;
		result->travelTime = cacheNode->travelTime;
		return cacheNode->reachability != 0;
	}

	FastRoutingResultsCache::Node *const cacheNode = nonConstThis->m_resultsCache.allocAndRegisterForKey( binIndex, key );

	// Don't try reading from the table if it explicitly blocks that
	if( AasStaticRouteTable::s_isAccessibleForRouteCache ) [[likely]] {
		if( travelFlags == Bot::ALLOWED_TRAVEL_FLAGS ) {
			if( BlockedAreasDigestsMatch( blockedAreasDigest, defaultBlockedAreasDigest ) ) {
				std::optional<std::pair<int, uint16_t>> tableResult;
				// Caution: May call RoutingResultToGoalArea() in result match checking mode
				if( ( tableResult = AasStaticRouteTable::instance()->getAllowedRouteFromTo( fromAreaNum, toAreaNum ) ) ) {
					result->reachNum   = cacheNode->reachability = ToUint16CheckingRange( tableResult->first );
					result->travelTime = cacheNode->travelTime   = ToUint16CheckingRange( tableResult->second );
					return true;
				} else {
					cacheNode->reachability = 0;
					cacheNode->travelTime   = 0;
					return false;
				}
			}
		}
	}

	RoutingRequest request( fromAreaNum, toAreaNum, travelFlags );
	// TODO: It's non-obvious that FindRoute() modifies `result`
	if( nonConstThis->FindRoute( request, result ) ) {
		cacheNode->reachability = ToUint16CheckingRange( result->reachNum );
		cacheNode->travelTime = ToUint16CheckingRange( result->travelTime );
		return true;
	}

	cacheNode->reachability = 0;
	cacheNode->travelTime = 0;
	return false;
}

bool AiAasRouteCache::FindRoute( const RoutingRequest &request, RoutingResult *result ) {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto aasPortals = aasWorld.getPortals();

	auto clusterNum = aasAreaSettings[request.areaNum].cluster;
	auto goalClusterNum = aasAreaSettings[request.goalAreaNum].cluster;
	// Check if the area is a portal of the goal area cluster
	if( clusterNum < 0 && goalClusterNum > 0 ) {
		const auto *portal = &aasPortals[-clusterNum];
		if( portal->frontcluster == goalClusterNum || portal->backcluster == goalClusterNum ) {
			clusterNum = goalClusterNum;
		}
	}
	// Check if the goalarea is a portal of the area cluster
	else if( clusterNum > 0 && goalClusterNum < 0 ) {
		const aas_portal_t *portal = &aasPortals[-goalClusterNum];
		if( portal->frontcluster == clusterNum || portal->backcluster == clusterNum ) {
			goalClusterNum = clusterNum;
		}
	}
	// Fix invalid access to cluster 0
	else if( !clusterNum || !goalClusterNum ) {
		return false;
	}

	// If both areas are in the same cluster
	// NOTE: there might be a shorter route via another cluster!!! but we don't care
	if( clusterNum > 0 && goalClusterNum > 0 && clusterNum == goalClusterNum ) {
		const auto *areaCache = GetAreaRoutingCache( aasAreaSettings, aasPortals, clusterNum,
													 request.goalAreaNum, request.travelFlags );
		// The number of the area in the cluster
		const auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, clusterNum, request.areaNum );
		// The cluster the area is in
		const auto *cluster = &aasWorld.getClusters()[clusterNum];
		// If the area is NOT a reachability area
		if( clusterAreaNum >= cluster->numreachabilityareas ) {
			return false;
		}
		// If it is possible to travel to the goal area through this cluster
		if( areaCache->travelTimes[clusterAreaNum] != 0 ) {
			result->reachNum = aasAreaSettings[request.areaNum].firstreachablearea;
			result->reachNum += areaCache->reachOffsets[clusterAreaNum];
			result->travelTime = areaCache->travelTimes[clusterAreaNum];
			return true;
		}
	}

	goalClusterNum = aasAreaSettings[request.goalAreaNum].cluster;
	// If the goal area is a portal
	if( goalClusterNum < 0 ) {
		// Just assume the goal area is part of the front cluster
		goalClusterNum = aasPortals[-goalClusterNum].frontcluster;
	}

	auto *portalCache = GetPortalRoutingCache( aasAreaSettings, aasPortals, goalClusterNum,
											   request.goalAreaNum, request.travelFlags );
	return RouteToGoalPortal( request, portalCache, result );
}

bool AiAasRouteCache::RouteToGoalPortal( const RoutingRequest &request,
										 AreaOrPortalCacheTable *portalCache,
										 RoutingResult *result ) {
	const auto aasAreaSettings = aasWorld.getAreaSettings();
	const auto clusterNum = aasAreaSettings[request.areaNum].cluster;
	// If the area is a cluster portal, read directly from the portal cache
	if( clusterNum < 0 ) {
		result->travelTime = portalCache->travelTimes[-clusterNum];
		result->reachNum = aasAreaSettings[request.areaNum].firstreachablearea;
		result->reachNum += portalCache->reachOffsets[-clusterNum];
		return true;
	}

	const auto aasPortalIndex = aasWorld.getPortalIndex();
	const auto aasPortals = aasWorld.getPortals();
	// The cluster the area is in
	const auto *cluster = &aasWorld.getClusters()[clusterNum];

	int bestTime = 0;
	int bestReachNum = -1;
	// Find the portal of the area cluster leading towards the goal area
	for( int i = 0; i < cluster->numportals; i++ ) {
		const auto portalNum = aasPortalIndex[cluster->firstportal + i];
		// If the goal area isn't reachable from the portal
		const auto travelTimeFromPortalToGoal = portalCache->travelTimes[portalNum];
		if( !travelTimeFromPortalToGoal ) {
			continue;
		}

		const auto *portal = &aasPortals[portalNum];
		// Get the cache of the portal area
		const auto *areaCache = GetAreaRoutingCache( aasAreaSettings, aasPortals, clusterNum,
													 portal->areanum, request.travelFlags );
		// Current area inside the current cluster
		const auto clusterAreaNum = ClusterAreaNum( aasAreaSettings, aasPortals, clusterNum, request.areaNum );
		// If the area is NOT a reachability area
		if( clusterAreaNum >= cluster->numreachabilityareas ) {
			continue;
		}
		// If the portal is NOT reachable from this area
		const auto areaToPortalTravelTime = areaCache->travelTimes[clusterAreaNum];
		if( !areaToPortalTravelTime ) {
			continue;
		}

		// Total travel time is the travel time the portal area is from
		// the goal area plus the travel time towards the portal area
		uint16_t t = ToUint16CheckingRange( travelTimeFromPortalToGoal + areaToPortalTravelTime );
		//FIXME: add the exact travel time through the actual portal area
		//NOTE: for now we just add the largest travel time through the portal area
		//		because we can't directly calculate the exact travel time
		//		to be more specific we don't know which reachability was used to travel
		//		into the portal area
		t = ToUint16CheckingRange( t + portalMaxTravelTimes[portalNum] );
		// Check whether the time is better than the one already found
		if( bestTime && t >= bestTime ) {
			continue;
		}

		auto reachNum = aasAreaSettings[request.areaNum].firstreachablearea + areaCache->reachOffsets[clusterAreaNum];
		bestReachNum = reachNum;
		bestTime = t;
	}

	if( bestReachNum < 0 ) {
		return false;
	}

	result->reachNum = bestReachNum;
	result->travelTime = bestTime;
	return true;
}
