#ifndef QFUSION_SND_ENV_EFFECTS_H
#define QFUSION_SND_ENV_EFFECTS_H

#include "snd_local.h"
#include "efxpresetsregistry.h"

struct src_s;
struct ListenerProps;

static constexpr float REVERB_ENV_DISTANCE_THRESHOLD = 4096.0f;

struct EfxPresetEntry;

class EaxReverbEffect final {
	friend class ReverbEffectSampler;

public:

	void UpdateDelegatedSpatialization( struct src_s *src, int listenerEntNum, const vec3_t listenerOrigin );


	vec3_t tmpSourceOrigin { 0, 0, 0 };

	ALint type { AL_EFFECT_EAXREVERB };

	void AdjustGain( src_s *src ) const;

	// A timestamp of last props update
	int64_t lastUpdateAt { 0 };
	// An distance between emitter and listener at last props update
	float distanceAtLastUpdate { 0.0f };

	EaxReverbEffect() {
		reverbProps.gain = 0.0f;
	}

	EfxReverbProps reverbProps {};

	float directObstruction { 0.0f };
	// An intermediate of the reverb sampling algorithm, useful for gain adjustment
	float secondaryRaysObstruction { 0.0f };

	unsigned GetLingeringTimeout() const {
		return (unsigned)( reverbProps.decayTime * 1000 + 50 );
	}

	bool ShouldKeepLingering( float sourceQualityHint, int64_t millisNow ) const;

	void BindOrUpdate( src_s *src, const ListenerProps &listenerProps );
	void InterpolateProps( const EaxReverbEffect *oldOne, int timeDelta );

	void UpdatePanning( src_s *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes );

	void CheckCurrentlyBoundEffect( src_s *src );
	virtual void IntiallySetupEffect( src_s *src );

	virtual float GetSourceGain( src_s *src ) const;

	void AttachEffect( src_s *src );
};

#endif
