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

class EffectSamplers {
public:
	static float SamplingRandom();
};

void ENV_Init();
void ENV_Shutdown();
void ENV_EndRegistration();

void ENV_UpdateListener( int entNum, const vec3_t origin, const vec3_t velocity, const mat3_t axes );

void ENV_RegisterSource( struct src_s *src );

void ENV_UnregisterSource( struct src_s *src );

#endif
