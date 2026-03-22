#ifndef QFUSION_SND_ENV_SAMPLER_H
#define QFUSION_SND_ENV_SAMPLER_H

#include <common/facilities/q_collision.h>

struct src_s;

struct ListenerProps {
	vec3_t origin;
	vec3_t velocity;
	int entNum { -1 };
	mutable int leafNum { -1 };
	bool isInLiquid { false };

	int GetLeafNum() const;

	void InvalidateCachedUpdateState() {
		leafNum = -1;
	}
};

struct PanningUpdateState;

class EffectSamplers {
public:
	static float SamplingRandom();
};

void ENV_Init();
void ENV_Shutdown();
void ENV_EndRegistration();

void ENV_UpdateListener( int entNum, const vec3_t origin, const vec3_t velocity, const mat3_t axes );

void ENV_CalculateSourcePan( const vec3_t listenerOrigin, const mat3_t listenerAxes,
							 const PanningUpdateState *updateState, vec3_t earlyPan, vec3_t latePan );

void ENV_CalculatePropagationOrigin( const vec3_t listenerOrigin, const vec3_t realSourceOrigin,
									 float *sourcePitchScale, vec3_t sourceOriginToUse );

void ENV_RegisterSource( struct src_s *src );

void ENV_UnregisterSource( struct src_s *src );

#endif
