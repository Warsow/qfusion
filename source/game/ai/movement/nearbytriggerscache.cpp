#include "nearbytriggerscache.h"
#include "predictioncontext.h"
#include "../classifiedentitiescache.h"
#include "../ailocal.h"

namespace wsw::ai::movement {

NearbyTriggersCache::NearbyTriggersCache() {
	triggerTravelFlags[0] = TRAVEL_JUMPPAD;
	triggerTravelFlags[1] = TRAVEL_TELEPORT;
	triggerTravelFlags[2] = TRAVEL_ELEVATOR;

	triggerNumEnts[0] = &numJumppadEnts;
	triggerNumEnts[1] = &numTeleportEnts;
	triggerNumEnts[2] = &numPlatformTriggerEnts;

	triggerEntNums[0] = &jumppadEntNums[0];
	triggerEntNums[1] = &teleportEntNums[0];
	triggerEntNums[2] = &platformTriggerEntNums[0];
}

auto NearbyTriggersCache::clipToRegion( std::span<const uint16_t> entNums, uint16_t *dest, size_t destSize ) -> unsigned {
	const auto *__restrict regionMins = lastComputedForMins;
	const auto *__restrict regionMaxs = lastComputedForMaxs;
	const auto *__restrict gameEnts = game.edicts;

	size_t numDestEnts = 0;
	if( entNums.size() <= destSize ) {
		for( auto num: entNums ) {
			const auto *__restrict ent = gameEnts + num;
			if( BoundsIntersect( regionMins, regionMaxs, ent->r.absmin, ent->r.absmax ) ) {
				dest[numDestEnts++] = num;
			}
		}
	} else {
		for( auto num: entNums ) {
			const auto *__restrict ent = gameEnts + num;
			if( BoundsIntersect( regionMins, regionMaxs, ent->r.absmin, ent->r.absmax ) ) {
				dest[numDestEnts++] = num;
				if( numDestEnts == destSize ) {
					break;
				}
			}
		}
	}

	return numDestEnts;
}

void NearbyTriggersCache::ensureValidForBounds( const float *__restrict absMins,
												const float *__restrict absMaxs ) {
	// TODO: Simd ("withinBounds") this
	int i;
	for( i = 0; i < 3; ++i ) {
		if( lastComputedForMins[i] >= absMins[i] ) {
			break;
		}
		if( lastComputedForMaxs[i] <= absMaxs[i] ) {
			break;
		}
	}

	// If all coords have passed tests
	if( i == 3 ) {
		return;
	}

	VectorSet( lastComputedForMins, -192, -192, -192 );
	VectorSet( lastComputedForMaxs, +192, +192, +192 );
	VectorAdd( absMins, lastComputedForMins, lastComputedForMins );
	VectorAdd( absMaxs, lastComputedForMaxs, lastComputedForMaxs );

	numTeleportEnts = clipToRegion( context->m_teleporterEntNumsToUseDuringPrediction, teleportEntNums, kMaxClassEnts );
	numJumppadEnts  = clipToRegion( context->m_jumppadEntNumsToUseDuringPrediction, jumppadEntNums, kMaxClassEnts );
	numOtherEnts    = clipToRegion( context->m_otherTriggerEntNumsToUseDuringPrediction, otherEntNums, kMaxOtherEnts );

	numPlatformSolidEnts   = 0;
	numPlatformTriggerEnts = 0;

	// Note: This iterated span includes triggers that are outside the region if their solid body is within it
	for( const uint16_t triggerEntNum: context->m_platformTriggerEntNumsToUseDuringPrediction ) {
		const auto *__restrict trigger = game.edicts + triggerEntNum;
		if( numPlatformTriggerEnts < std::size( platformTriggerEntNums ) ) [[likely]] {
			if( BoundsIntersect( lastComputedForMins, lastComputedForMaxs, trigger->r.absmin, trigger->r.absmax ) ) {
				platformTriggerEntNums[numPlatformTriggerEnts++] = triggerEntNum;
			}
		}
		if( numPlatformSolidEnts < std::size( platformSolidEntNums ) ) [[likely]] {
			const auto *__restrict platform = trigger->enemy;
			assert( platform && platform->use == Use_Plat );
			if( BoundsIntersect( lastComputedForMins, lastComputedForMaxs, platform->r.absmin, platform->r.absmax ) ) {
				platformSolidEntNums[numPlatformSolidEnts++] = platform->s.number;
			}
		}
	}
}

}