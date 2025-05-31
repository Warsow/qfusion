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

class EffectSampler {
protected:
	static unsigned GetNumSamplesForCurrentQuality( unsigned minSamples, unsigned maxSamples ) {
		float quality = s_environment_sampling_quality->value;

		assert( quality >= 0.0f && quality <= 1.0f );
		assert( minSamples < maxSamples );

		auto numSamples = (unsigned)( minSamples + ( maxSamples - minSamples ) * quality );
		assert( numSamples && numSamples <= maxSamples );
		return numSamples;
	}

	virtual Effect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) = 0;
};

class EffectSamplers {
public:
	// Would be nice just have a method of the same signature in Effect scope but static.
	// Unfortunately there is no companion objects like in Scala/Kotlin.
	static Effect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc );
	static float SamplingRandom();
};

class ObstructedEffectSampler: public virtual EffectSampler {
protected:
	void SetupDirectObstructionSamplingProps( src_t *src, unsigned minSamples, unsigned maxSamples );

	float ComputeDirectObstruction( const ListenerProps &listenerProps, src_t *src );
};

class UnderwaterFlangerEffectSampler final: public ObstructedEffectSampler {
public:
	Effect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) override;
};

constexpr const auto MAX_DIRECT_OBSTRUCTION_SAMPLES = 8;
// Almost doubled for "realistic obstruction" (we need more secondary rays)
constexpr const auto MAX_REVERB_PRIMARY_RAY_SAMPLES = 80;

class ReverbEffectSampler final: public ObstructedEffectSampler, private GenericRaycastSampler {
private:
	vec3_t primaryRayDirs[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	vec3_t reflectionPoints[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	float primaryHitDistances[MAX_REVERB_PRIMARY_RAY_SAMPLES];
	vec3_t testedListenerOrigin;

	const ListenerProps *listenerProps;
	src_t * src;
	EaxReverbEffect * effect;

	void ComputeReverberation( const ListenerProps &listenerProps_, src_t *src_, EaxReverbEffect *effect_ );

	void ResetMutableState( const ListenerProps &listenerProps_, src_t *src_, EaxReverbEffect *effect_ );

	float GetEmissionRadius() const override;
	void SetupPrimaryRayDirs();
	void EmitSecondaryRays();

public:
	Effect *TryApply( const ListenerProps &listenerProps, src_t *src, const src_t *tryReusePropsSrc ) override;
};

#endif
