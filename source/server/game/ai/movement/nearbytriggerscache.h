#ifndef WSW_0b4bb3fb_4c2f_47f9_afe7_68889268c9bd_H
#define WSW_0b4bb3fb_4c2f_47f9_afe7_68889268c9bd_H

#include "../navigation/aasworld.h"

class PredictionContext;

namespace wsw::ai::movement {

struct NearbyTriggersCache {
	PredictionContext *context;

	vec3_t lastComputedForMins { +99999, +99999, +99999 };
	vec3_t lastComputedForMaxs { -99999, -99999, -99999 };

	unsigned numJumppadEnts { 0 };
	unsigned numTeleportEnts { 0 };
	unsigned numElevatorTriggerEnts { 0 };
	unsigned numOtherEnts { 0 };

	static constexpr const size_t kMaxClassEnts { 8 };
	static constexpr const size_t kMaxOtherEnts { 16 };

	uint16_t jumppadEntNums[kMaxClassEnts];
	uint16_t teleportEntNums[kMaxClassEnts];
	uint16_t elevatorTriggerEntNums[kMaxClassEnts];
	uint16_t platformSolidEntNums[kMaxClassEnts];
	uint16_t otherEntNums[kMaxOtherEnts];

	int triggerTravelFlags[3];
	const unsigned *triggerNumEnts[3];
	const uint16_t *triggerEntNums[3];

	NearbyTriggersCache();

	void ensureValidForBounds( const float *__restrict absMins, const float *__restrict absMaxs );
private:
	[[nodiscard]]
	auto clipToRegion( std::span<const uint16_t> entNums, uint16_t *dest, size_t destSize ) -> unsigned;
};

}

#endif
