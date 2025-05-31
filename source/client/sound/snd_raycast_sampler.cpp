#include <common/q_collision.h>
#include "snd_raycast_sampler.h"
#include "snd_env_effects.h"

#include <algorithm>

class RayDirsHolder {
	// TODO: Keep it in sync/share this with LeafPropsSampler
	enum { MAX_RAYS = 1024 };
	vec3_t cachedDirs[MAX_RAYS];
	unsigned lastNumRays { 0 };

	void ComputeDirs( unsigned numRays );
public:
	void SetupDirs( vec3_t *rayDirs, unsigned numRays ) {
		if( lastNumRays != numRays ) {
			ComputeDirs( numRays );
			lastNumRays = numRays;
		}
		memcpy( rayDirs, cachedDirs, numRays * sizeof( *rayDirs ) );
	}
};

static RayDirsHolder rayDirsHolder;

void GenericRaycastSampler::SetupSamplingRayDirs( vec3_t *rayDirs, unsigned numRays ) {
	::rayDirsHolder.SetupDirs( rayDirs, numRays );
}

void RayDirsHolder::ComputeDirs( unsigned numRays ) {
	assert( numRays <= MAX_RAYS );
	// According to https://github.com/DenMSC
	// the algorithm source is https://stackoverflow.com/a/26127012
	for( unsigned i = 0; i < numRays; i++ ) {
		float offset = 2.0f / (float)numRays;
		float increment = ( (float)M_PI ) * ( 3.0f - sqrtf( 5.0f ) );

		float y = ( i * offset ) - 1 + ( offset / 2.0f );
		float r = sqrtf( 1 - y * y );
		float phi = i * increment;
		float x = cosf( phi ) * r;
		float z = sinf( phi ) * r;

		cachedDirs[i][0] = x;
		cachedDirs[i][1] = y;
		cachedDirs[i][2] = z;
	}
}

bool GenericRaycastSampler::CheckAndAddHitSurfaceProps( const trace_t &trace ) {
	return !( trace.surfFlags & ( SURF_SKY | SURF_NOIMPACT | SURF_NOMARKS | SURF_FLESH | SURF_NOSTEPS ) );
}

void GenericRaycastSampler::EmitPrimaryRays() {
	const float primaryEmissionRadius = GetEmissionRadius();
	// Using top node hints is quite beneficial for small emission radii.
	const int topNode = S_FindTopNodeForSphere( emissionOrigin, primaryEmissionRadius );

	// These values must be reset at this stage
	assert( !averageDistance );
	assert( !numPrimaryHits );
	assert( !numRaysHitAnySurface );

	// Check whether sampling params have been set
	assert( numPrimaryRays );
	assert( primaryRayDirs );
	assert( primaryHitDistances );

	trace_t trace;
	for( unsigned i = 0; i < numPrimaryRays; ++i ) {
		float *sampleDir, *hitPoint;
		sampleDir = primaryRayDirs[i];

		vec3_t testedRayPoint;
		VectorScale( sampleDir, primaryEmissionRadius, testedRayPoint );
		VectorAdd( testedRayPoint, emissionOrigin, testedRayPoint );
		S_Trace( &trace, emissionOrigin, testedRayPoint, vec3_origin, vec3_origin, MASK_SOLID | MASK_WATER, topNode );

		if( trace.startsolid || trace.allsolid ) {
			continue;
		}

		if( trace.fraction == 1.0f ) {
			continue;
		}

		numRaysHitAnySurface++;
		if( !CheckAndAddHitSurfaceProps( trace ) ) {
			continue;
		}

		if( DistanceSquared( emissionOrigin, trace.endpos ) < 2 * 2 ) {
			continue;
		}

		// Check whether recording hit points is requested (usually for secondary emission)
		if( primaryHitPoints ) {
			// Do not use the trace.endpos exactly as a source of a reflected wave.
			// (a following trace call would probably fail for this start origin).
			// Add -sampleDir offset to the trace.endpos
			hitPoint = primaryHitPoints[numPrimaryHits];
			VectorCopy( sampleDir, hitPoint );
			VectorNegate( hitPoint, hitPoint );
			VectorAdd( trace.endpos, hitPoint, hitPoint );
		}

		float squareDistance = DistanceSquared( emissionOrigin, trace.endpos );
		if( squareDistance < REVERB_ENV_DISTANCE_THRESHOLD * REVERB_ENV_DISTANCE_THRESHOLD ) {
			primaryHitDistances[numPrimaryHits] = sqrtf( squareDistance );
		} else {
			primaryHitDistances[numPrimaryHits] = REVERB_ENV_DISTANCE_THRESHOLD;
		}

		assert( primaryHitDistances[numPrimaryHits] >= 0.0f );
		assert( primaryHitDistances[numPrimaryHits] <= REVERB_ENV_DISTANCE_THRESHOLD );
		averageDistance += primaryHitDistances[numPrimaryHits];

		numPrimaryHits++;
	}

	if( numPrimaryHits ) {
		averageDistance /= (float)numPrimaryHits;
	}

	assert( numRaysHitAnySurface <= numPrimaryRays );
	assert( numRaysHitAnySurface >= numPrimaryHits );
}

float GenericRaycastSampler::ComputePrimaryHitDistanceStdDev() const {
	assert( numPrimaryHits );

	float variance = 0.0f;
	for( unsigned i = 0; i < numPrimaryHits; ++i ) {
		float delta = primaryHitDistances[i] - averageDistance;
		variance += delta * delta;
	}

	variance /= numPrimaryHits;
	return sqrtf( variance );
}

float GenericRaycastSampler::ComputeRoomSizeFactor() const {
	assert( numPrimaryHits );

	// Get the standard deviation of primary rays hit distances.
	const float sigma = ComputePrimaryHitDistanceStdDev();

	// Note: The hit distance distribution is not Gaussian and is not symmetrical!
	// It heavily depends of the real environment,
	// and might be close to Gaussian if a player is in a center of a huge box.
	// Getting the farthest point that is within 3 sigma does not feel good.

	// Select distances that >= the average distance
	float *contestedDistances = primaryHitDistances;
	int numContestedDistances = 0;
	for( unsigned i = 0; i < numPrimaryHits; ++i ) {
		if( primaryHitDistances[i] >= averageDistance ) {
			contestedDistances[numContestedDistances++] = primaryHitDistances[i];
		}
	}

	// We are sure there is at least 1 distance >= the average distance
	assert( numContestedDistances );

	// Sort hit distances so the nearest one is the first
	std::sort( contestedDistances, contestedDistances + numContestedDistances );

	const float connectivityDistance = wsw::max( 256.0f, sigma );

	float prevDistance = contestedDistances[0];
	// Stop on first distance that violates "connectivity".
	// This way distance threshold could propagate along a hallway or tube.
	for( int i = 1; i < numContestedDistances; ++i ) {
		float currDistance = contestedDistances[i];
		assert( currDistance >= prevDistance );
		if( currDistance - prevDistance > connectivityDistance ) {
			break;
		}
		prevDistance = currDistance;
	}

	// Prevent reusing primaryHitDistances as they were used as a scratchpad.
	// We do not complicate interface by adding another buffer to supply.
	primaryHitDistances = nullptr;

	return prevDistance / REVERB_ENV_DISTANCE_THRESHOLD;
}