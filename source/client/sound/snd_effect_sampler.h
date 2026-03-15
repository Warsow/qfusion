#ifndef QFUSION_SND_REVERB_SAMPLER_H
#define QFUSION_SND_REVERB_SAMPLER_H

#include "snd_raycast_sampler.h"
#include "snd_env_effects.h"

struct ListenerProps {
	vec3_t origin;
	vec3_t velocity;
	int entNum { -1 };
	mutable int leafNum { -1 };
	bool isInLiquid { false };

	int GetLeafNum() const {
		if( leafNum < 0 ) {
			leafNum = S_PointLeafNum( origin );
		}
		return leafNum;
	}

	void InvalidateCachedUpdateState() {
		leafNum = -1;
	}
};

class EffectSamplers {
public:
	static float SamplingRandom();
};

constexpr const auto MAX_DIRECT_OBSTRUCTION_SAMPLES = 8;
// Almost doubled for "realistic obstruction" (we need more secondary rays)
constexpr const auto MAX_REVERB_PRIMARY_RAY_SAMPLES = 80;

class ReverbRaycastSampler : public GenericRaycastSampler {
public:
	ReverbRaycastSampler( vec3_t *primaryRayDirs_, vec3_t *primaryHitPoints_, float *primaryHitDistances_,
						  const vec3_t emissionOrigin_, float emissionRadius_, unsigned numPrimaryRays_ )
		: GenericRaycastSampler( primaryRayDirs_, primaryHitPoints_, primaryHitDistances_,
								 emissionOrigin_, emissionRadius_, numPrimaryRays_ ) {}
};

class ReverbEffectComputer {
private:
	static void SetupDirectObstructionSamplingProps( src_t *src, unsigned minSamples, unsigned maxSamples );

	static float ComputeDirectObstruction( const ListenerProps &listenerProps, src_t *src );

	static unsigned GetNumSamplesForCurrentQuality( unsigned minSamples, unsigned maxSamples );

	static void ComputeReverberation( const ListenerProps &listenerProps_, src_t *src, EaxReverbEffect *effect );

	static float CalcEmissionRadius( const src_t *src );

	static void EmitSecondaryRays( const ReverbRaycastSampler &sampler, const ListenerProps &listenerProps, src_t *src, EaxReverbEffect *effect );
public:
	static EaxReverbEffect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc );
};

#endif
