#ifndef QFUSION_SND_RAYCAST_SAMPLER_H
#define QFUSION_SND_RAYCAST_SAMPLER_H

#include "snd_local.h"

class GenericRaycastSampler {
	friend class LeafPropsComputer;
public:
	virtual ~GenericRaycastSampler() = default;

	GenericRaycastSampler( vec3_t *primaryRayDirs_, vec3_t *primaryHitPoints_,
						   float *primaryHitDistances_, const vec3_t emissionOrigin_,
						   float emissionRadius_, unsigned numPrimaryRays_ )
		: numPrimaryRays( numPrimaryRays_ ), emissionRadius( emissionRadius_ ) {
		this->primaryRayDirs = primaryRayDirs_;
		this->primaryHitPoints = primaryHitPoints_;
		this->primaryHitDistances = primaryHitDistances_;

		VectorCopy( emissionOrigin_, this->emissionOrigin );
	}

	static void SetupSamplingRayDirs( vec3_t *rayDirs, unsigned numRays );
	const unsigned numPrimaryRays;
	// A number of ray hits that address recorded hit points/distances
	// that in turn could be used for secondary emission.
	// (TODO: Consider using a better name)
	unsigned numPrimaryHits { 0 };
	// A number of rays with non-1.0 trace fraction, this number is not less than numPrimaryHits
	unsigned numRaysHitAnySurface { 0 };
	float averageDistance { 0.0f };

	vec3_t emissionOrigin { 0.0f, 0.0f, 0.0f };
	const float emissionRadius;

	[[nodiscard]]
	virtual bool CheckAndAddHitSurfaceProps( const trace_t &trace );

	float ComputePrimaryHitDistanceStdDev() const;

	float ComputeRoomSizeFactor() const;

	void EmitPrimaryRays();

	vec3_t *primaryRayDirs;
	vec3_t *primaryHitPoints;
	mutable float *primaryHitDistances;
};

#endif
