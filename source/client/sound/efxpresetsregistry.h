#ifndef WSW_f7aab381_683d_4dce_ae3a_57a81e46af6b_H
#define WSW_f7aab381_683d_4dce_ae3a_57a81e46af6b_H

#define AL_LIBTYPE_STATIC
#include <AL/efx.h>

struct EfxReverbProps {
	float density;
	float diffusion;
	float gain;
	float gainHf;
	float gainLf;
	float decayTime;
	float decayHfRatio;
	float decayLfRatio;
	float reflectionsGain;
	float reflectionsDelay;
	float lateReverbGain;
	float lateReverbDelay;
	float echoTime;
	float echoDepth;
	float modulationTime;
	float modulationDepth;
	float airAbsorptionGainHf;
	float hfReference;
	float lfReference;
	int decayHfLimit;

	struct NoInitType {};
	static constexpr NoInitType NoInit {};

	explicit EfxReverbProps( NoInitType ) {}

	EfxReverbProps() {
		density             = AL_EAXREVERB_DEFAULT_DENSITY;
		diffusion           = AL_EAXREVERB_DEFAULT_DIFFUSION;
		gain                = AL_EAXREVERB_DEFAULT_GAIN;
		gainHf              = AL_EAXREVERB_DEFAULT_GAINHF;
		gainLf              = AL_EAXREVERB_DEFAULT_GAINLF;
		decayTime           = AL_EAXREVERB_DEFAULT_DECAY_TIME;
		decayHfRatio        = AL_EAXREVERB_DEFAULT_DECAY_HFRATIO;
		decayLfRatio        = AL_EAXREVERB_DEFAULT_DECAY_LFRATIO;
		reflectionsGain     = AL_EAXREVERB_DEFAULT_REFLECTIONS_GAIN;
		reflectionsDelay    = AL_EAXREVERB_DEFAULT_REFLECTIONS_DELAY;
		lateReverbGain      = AL_EAXREVERB_DEFAULT_LATE_REVERB_GAIN;
		lateReverbDelay     = AL_EAXREVERB_DEFAULT_LATE_REVERB_DELAY;
		echoTime            = AL_EAXREVERB_DEFAULT_ECHO_TIME;
		echoDepth           = AL_EAXREVERB_DEFAULT_ECHO_DEPTH;
		modulationTime      = AL_EAXREVERB_DEFAULT_MODULATION_TIME;
		modulationDepth     = AL_EAXREVERB_DEFAULT_MODULATION_DEPTH;
		airAbsorptionGainHf = AL_EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF;
		hfReference         = AL_EAXREVERB_DEFAULT_HFREFERENCE;
		lfReference         = AL_EAXREVERB_DEFAULT_LFREFERENCE;
		decayHfLimit        = AL_EAXREVERB_DEFAULT_DECAY_HFLIMIT;
	}

	static const EfxReverbProps kDefaultProps;
};

namespace wsw { class StringView; }
namespace wsw { class HashedStringView; }

class EfxPresetsRegistry {
	friend struct EfxPresetEntry;
public:
	static EfxPresetsRegistry s_instance;

	[[nodiscard]]
	auto findByName( const wsw::StringView &name ) const -> const EfxReverbProps *;
	[[nodiscard]]
	auto findByName( const wsw::HashedStringView &name ) const -> const EfxReverbProps *;
private:
	static constexpr auto kNumHashBins = 97;
	struct EfxPresetEntry *m_hashBins[kNumHashBins];
};

void interpolateReverbProps( const EfxReverbProps *from, float frac, const EfxReverbProps *to, EfxReverbProps *dest );
void mixReverbProps( const EfxReverbProps **begin, const EfxReverbProps **end, EfxReverbProps *dest );

#endif
