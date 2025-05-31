#ifndef QFUSION_SND_ENV_EFFECTS_H
#define QFUSION_SND_ENV_EFFECTS_H

#include "snd_local.h"
#include "efxpresetsregistry.h"

struct src_s;

static constexpr float REVERB_ENV_DISTANCE_THRESHOLD = 4096.0f;

class Effect {
public:
	virtual void BindOrUpdate( src_s *src ) = 0;

	virtual void InterpolateProps( const Effect *oldOne, int timeDelta ) {};

	virtual unsigned GetLingeringTimeout() const {
		return 500;
	};

	virtual bool ShouldKeepLingering( float sourceQualityHint, int64_t millisNow ) const {
		return sourceQualityHint > 0;
	};

	virtual void UpdatePanning( src_s *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes ) {}

	virtual ~Effect() = default;

	template <typename T> static const T Cast( const Effect *effect ) {
		// We might think of optimizing it in future, that's why the method is favourable over the direct cast
		return dynamic_cast<T>( const_cast<Effect *>( effect ) );
	}

	void AdjustGain( src_s *src ) const;

	// A timestamp of last props update
	int64_t lastUpdateAt { 0 };
	// An distance between emitter and listener at last props update
	float distanceAtLastUpdate { 0.0f };
protected:
	ALint type;

	explicit Effect( ALint type_ ): type( type_ ) {}

	void CheckCurrentlyBoundEffect( src_s *src );
	virtual void IntiallySetupEffect( src_s *src );

	virtual float GetSourceGain( src_s *src ) const;

	void AttachEffect( src_s *src );
};

class UnderwaterFlangerEffect final: public Effect {
	void IntiallySetupEffect( src_s *src ) override;
	float GetSourceGain( src_s *src ) const override;
public:
	UnderwaterFlangerEffect(): Effect( AL_EFFECT_FLANGER ) {}

	float directObstruction;
	bool hasMediumTransition;

	void BindOrUpdate( src_s *src ) override;
	void InterpolateProps( const Effect *oldOne, int timeDelta ) override;

	unsigned GetLingeringTimeout() const override {
		return 500;
	}

	bool ShouldKeepLingering( float sourceQualityHint, int64_t millisNow ) const override {
		return sourceQualityHint > 0;
	}
};

struct EfxPresetEntry;

class EaxReverbEffect final: public Effect {
	friend class ReverbEffectSampler;

	void UpdateDelegatedSpatialization( struct src_s *src, int listenerEntNum, const vec3_t listenerOrigin );

	vec3_t tmpSourceOrigin { 0, 0, 0 };
public:
	EaxReverbEffect(): Effect( AL_EFFECT_EAXREVERB ) {
		reverbProps.gain = 0.0f;
	}

	EfxReverbProps reverbProps {};

	float directObstruction { 0.0f };
	// An intermediate of the reverb sampling algorithm, useful for gain adjustment
	float secondaryRaysObstruction { 0.0f };

	unsigned GetLingeringTimeout() const override {
		return (unsigned)( reverbProps.decayTime * 1000 + 50 );
	}

	bool ShouldKeepLingering( float sourceQualityHint, int64_t millisNow ) const override;

	void BindOrUpdate( struct src_s *src ) override;
	void InterpolateProps( const Effect *oldOne, int timeDelta ) override;

	void UpdatePanning( src_s *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes ) override;
};

#endif
