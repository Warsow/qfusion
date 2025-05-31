#include "efxpresetsregistry.h"
#include "snd_local.h"
#include <common/wswbasicmath.h>
#include <array>

// TODO: Switch to using OpenAL SOFT headers across the entire codebase?
#include "../../../third-party/openal-soft/include/AL/efx-presets.h"

EfxPresetsRegistry EfxPresetsRegistry::s_instance;

// This is a common supertype for all presets that are declared globally
// and link themselves to the global presets cache
// (so we do not have to maintain an array of presets and its length manually)
struct EfxPresetEntry {
	const wsw::HashedStringView name;
	EfxPresetEntry *nextInHashBin { nullptr };
	EfxReverbProps props { EfxReverbProps::NoInit };

	explicit EfxPresetEntry( const char *presetMacroName, EFXEAXREVERBPROPERTIES &&eaxProps ) noexcept
		: name( presetMacroName ) {
		const auto binIndex = name.getHash() % EfxPresetsRegistry::kNumHashBins;
		this->nextInHashBin = EfxPresetsRegistry::s_instance.m_hashBins[binIndex];
		EfxPresetsRegistry::s_instance.m_hashBins[binIndex] = this;

		this->props.density             = eaxProps.flDensity;
		this->props.diffusion           = eaxProps.flDiffusion;
		this->props.gain                = eaxProps.flGain;
		this->props.gainHf              = eaxProps.flGainHF;
		this->props.gainLf              = eaxProps.flGainLF;
		this->props.decayTime           = eaxProps.flDecayTime;
		this->props.decayHfRatio        = eaxProps.flDecayHFRatio;
		this->props.decayLfRatio        = eaxProps.flDecayLFRatio;
		this->props.reflectionsGain     = eaxProps.flReflectionsGain;
		this->props.reflectionsDelay    = eaxProps.flReflectionsDelay;
		this->props.lateReverbGain      = eaxProps.flLateReverbGain;
		this->props.lateReverbDelay     = eaxProps.flLateReverbDelay;
		this->props.echoTime            = eaxProps.flEchoTime;
		this->props.echoDepth           = eaxProps.flEchoDepth;
		this->props.modulationTime      = eaxProps.flModulationTime;
		this->props.modulationDepth     = eaxProps.flModulationDepth;
		this->props.airAbsorptionGainHf = eaxProps.flAirAbsorptionGainHF;
		this->props.hfReference         = eaxProps.flHFReference;
		this->props.lfReference         = eaxProps.flLFReference;
	}
};

auto EfxPresetsRegistry::findByName( const wsw::StringView &name ) const -> const EfxReverbProps * {
	return findByName( wsw::HashedStringView( name ) );
}

auto EfxPresetsRegistry::findByName( const wsw::HashedStringView &name ) const -> const EfxReverbProps * {
	const auto binIndex = name.getHash() % kNumHashBins;
	for( EfxPresetEntry *entry = m_hashBins[binIndex]; entry; entry = entry->nextInHashBin ) {
		if( entry->name.equalsIgnoreCase( name ) ) {
			return &entry->props;
		}
	}
	return nullptr;
}

#define DEFINE_PRESET( presetMacroName ) \
	static EfxPresetEntry presetMacroName##_presetEntry( #presetMacroName, EFX_REVERB_PRESET_##presetMacroName );

//$cat efx-presets.h | grep EFX_R | sed -e 's/EFX_REVERB_PRESET_//g' | awk '{print(sprintf("DEFINE_PRESET( %s );", $2));}'

DEFINE_PRESET( GENERIC );
DEFINE_PRESET( PADDEDCELL );
DEFINE_PRESET( ROOM );
DEFINE_PRESET( BATHROOM );
DEFINE_PRESET( LIVINGROOM );
DEFINE_PRESET( STONEROOM );
DEFINE_PRESET( AUDITORIUM );
DEFINE_PRESET( CONCERTHALL );
DEFINE_PRESET( CAVE );
DEFINE_PRESET( ARENA );
DEFINE_PRESET( HANGAR );
DEFINE_PRESET( CARPETEDHALLWAY );
DEFINE_PRESET( HALLWAY );
DEFINE_PRESET( STONECORRIDOR );
DEFINE_PRESET( ALLEY );
DEFINE_PRESET( FOREST );
DEFINE_PRESET( CITY );
DEFINE_PRESET( MOUNTAINS );
DEFINE_PRESET( QUARRY );
DEFINE_PRESET( PLAIN );
DEFINE_PRESET( PARKINGLOT );
DEFINE_PRESET( SEWERPIPE );
DEFINE_PRESET( UNDERWATER );
DEFINE_PRESET( DRUGGED );
DEFINE_PRESET( DIZZY );
DEFINE_PRESET( PSYCHOTIC );
DEFINE_PRESET( CASTLE_SMALLROOM );
DEFINE_PRESET( CASTLE_SHORTPASSAGE );
DEFINE_PRESET( CASTLE_MEDIUMROOM );
DEFINE_PRESET( CASTLE_LARGEROOM );
DEFINE_PRESET( CASTLE_LONGPASSAGE );
DEFINE_PRESET( CASTLE_HALL );
DEFINE_PRESET( CASTLE_CUPBOARD );
DEFINE_PRESET( CASTLE_COURTYARD );
DEFINE_PRESET( CASTLE_ALCOVE );
DEFINE_PRESET( FACTORY_SMALLROOM );
DEFINE_PRESET( FACTORY_SHORTPASSAGE );
DEFINE_PRESET( FACTORY_MEDIUMROOM );
DEFINE_PRESET( FACTORY_LARGEROOM );
DEFINE_PRESET( FACTORY_LONGPASSAGE );
DEFINE_PRESET( FACTORY_HALL );
DEFINE_PRESET( FACTORY_CUPBOARD );
DEFINE_PRESET( FACTORY_COURTYARD );
DEFINE_PRESET( FACTORY_ALCOVE );
DEFINE_PRESET( ICEPALACE_SMALLROOM );
DEFINE_PRESET( ICEPALACE_SHORTPASSAGE );
DEFINE_PRESET( ICEPALACE_MEDIUMROOM );
DEFINE_PRESET( ICEPALACE_LARGEROOM );
DEFINE_PRESET( ICEPALACE_LONGPASSAGE );
DEFINE_PRESET( ICEPALACE_HALL );
DEFINE_PRESET( ICEPALACE_CUPBOARD );
DEFINE_PRESET( ICEPALACE_COURTYARD );
DEFINE_PRESET( ICEPALACE_ALCOVE );
DEFINE_PRESET( SPACESTATION_SMALLROOM );
DEFINE_PRESET( SPACESTATION_SHORTPASSAGE );
DEFINE_PRESET( SPACESTATION_MEDIUMROOM );
DEFINE_PRESET( SPACESTATION_LARGEROOM );
DEFINE_PRESET( SPACESTATION_LONGPASSAGE );
DEFINE_PRESET( SPACESTATION_HALL );
DEFINE_PRESET( SPACESTATION_CUPBOARD );
DEFINE_PRESET( SPACESTATION_ALCOVE );
DEFINE_PRESET( WOODEN_SMALLROOM );
DEFINE_PRESET( WOODEN_SHORTPASSAGE );
DEFINE_PRESET( WOODEN_MEDIUMROOM );
DEFINE_PRESET( WOODEN_LARGEROOM );
DEFINE_PRESET( WOODEN_LONGPASSAGE );
DEFINE_PRESET( WOODEN_HALL );
DEFINE_PRESET( WOODEN_CUPBOARD );
DEFINE_PRESET( WOODEN_COURTYARD );
DEFINE_PRESET( WOODEN_ALCOVE );
DEFINE_PRESET( SPORT_EMPTYSTADIUM );
DEFINE_PRESET( SPORT_SQUASHCOURT );
DEFINE_PRESET( SPORT_SMALLSWIMMINGPOOL );
DEFINE_PRESET( SPORT_LARGESWIMMINGPOOL );
DEFINE_PRESET( SPORT_GYMNASIUM );
DEFINE_PRESET( SPORT_FULLSTADIUM );
DEFINE_PRESET( SPORT_STADIUMTANNOY );
DEFINE_PRESET( PREFAB_WORKSHOP );
DEFINE_PRESET( PREFAB_SCHOOLROOM );
DEFINE_PRESET( PREFAB_PRACTISEROOM );
DEFINE_PRESET( PREFAB_OUTHOUSE );
DEFINE_PRESET( PREFAB_CARAVAN );
DEFINE_PRESET( DOME_TOMB );
DEFINE_PRESET( PIPE_SMALL );
DEFINE_PRESET( DOME_SAINTPAULS );
DEFINE_PRESET( PIPE_LONGTHIN );
DEFINE_PRESET( PIPE_LARGE );
DEFINE_PRESET( PIPE_RESONANT );
DEFINE_PRESET( OUTDOORS_BACKYARD );
DEFINE_PRESET( OUTDOORS_ROLLINGPLAINS );
DEFINE_PRESET( OUTDOORS_DEEPCANYON );
DEFINE_PRESET( OUTDOORS_CREEK );
DEFINE_PRESET( OUTDOORS_VALLEY );
DEFINE_PRESET( MOOD_HEAVEN );
DEFINE_PRESET( MOOD_HELL );
DEFINE_PRESET( MOOD_MEMORY );
DEFINE_PRESET( DRIVING_COMMENTATOR );
DEFINE_PRESET( DRIVING_PITGARAGE );
DEFINE_PRESET( DRIVING_INCAR_RACER );
DEFINE_PRESET( DRIVING_INCAR_SPORTS );
DEFINE_PRESET( DRIVING_INCAR_LUXURY );
DEFINE_PRESET( DRIVING_FULLGRANDSTAND );
DEFINE_PRESET( DRIVING_EMPTYGRANDSTAND );
DEFINE_PRESET( DRIVING_TUNNEL );
DEFINE_PRESET( CITY_STREETS );
DEFINE_PRESET( CITY_SUBWAY );
DEFINE_PRESET( CITY_MUSEUM );
DEFINE_PRESET( CITY_LIBRARY );
DEFINE_PRESET( CITY_UNDERPASS );
DEFINE_PRESET( CITY_ABANDONED );
DEFINE_PRESET( DUSTYROOM );
DEFINE_PRESET( CHAPEL );
DEFINE_PRESET( SMALLWATERROOM );

using EfxReverbField = float ( EfxReverbProps::* );

static const EfxReverbField kEfxReverbLinearXerpFields[] {
	// Looks like these values should use linear interpolation
	&EfxReverbProps::diffusion, &EfxReverbProps::echoDepth, &EfxReverbProps::modulationDepth,
	// Not sure of this one
	&EfxReverbProps::density,
};

static const std::pair<EfxReverbField, std::pair<float, float>> kEfxReverbLogExpXerpFields[] {
	// Note: It seems that we have to convert these linear gain values to bels (logarithmic values) for interpolation
	// as we should interpolate perception strength which is logarithmic
	{ &EfxReverbProps::gain, { AL_EAXREVERB_MIN_GAIN, AL_EAXREVERB_MAX_GAIN } },
	{ &EfxReverbProps::gainHf, { AL_EAXREVERB_MIN_GAINHF, AL_EAXREVERB_MAX_GAINHF } },
	{ &EfxReverbProps::gainLf, { AL_EAXREVERB_MIN_GAINLF, AL_EAXREVERB_MAX_GAINLF } },
	{ &EfxReverbProps::airAbsorptionGainHf, { AL_EAXREVERB_MIN_AIR_ABSORPTION_GAINHF, AL_EAXREVERB_MAX_AIR_ABSORPTION_GAINHF } },
	{ &EfxReverbProps::reflectionsGain, { AL_EAXREVERB_MIN_REFLECTIONS_GAIN, AL_EAXREVERB_MAX_REFLECTIONS_GAIN } },
	{ &EfxReverbProps::lateReverbGain, { AL_EAXREVERB_MIN_LATE_REVERB_GAIN, AL_EAXREVERB_MAX_LATE_REVERB_GAIN } },

	// It could seem logical to interpolate duration-like values linearly,
	// but there seem to be some corellation with gain, so it feels better this way
	{ &EfxReverbProps::decayTime, { AL_EAXREVERB_MIN_DECAY_TIME, AL_EAXREVERB_MAX_DECAY_TIME } },
	{ &EfxReverbProps::decayHfRatio, { AL_EAXREVERB_MIN_DECAY_HFRATIO, AL_EAXREVERB_MAX_DECAY_HFRATIO } },
	{ &EfxReverbProps::decayLfRatio, { AL_EAXREVERB_MIN_DECAY_LFRATIO, AL_EAXREVERB_MAX_DECAY_LFRATIO } },
	{ &EfxReverbProps::reflectionsDelay, { AL_EAXREVERB_MIN_REFLECTIONS_DELAY, AL_EAXREVERB_MAX_REFLECTIONS_DELAY } },
	{ &EfxReverbProps::lateReverbDelay, { AL_EAXREVERB_MIN_LATE_REVERB_DELAY, AL_EAXREVERB_MAX_LATE_REVERB_DELAY } },
	{ &EfxReverbProps::echoTime, { AL_EAXREVERB_MIN_ECHO_TIME, AL_EAXREVERB_MAX_ECHO_TIME } },
	{ &EfxReverbProps::modulationTime, { AL_EAXREVERB_MIN_MODULATION_TIME, AL_EAXREVERB_MAX_MODULATION_TIME } },

	// Not sure of these ones
	{ &EfxReverbProps::hfReference, { AL_EAXREVERB_MIN_HFREFERENCE, AL_EAXREVERB_MAX_HFREFERENCE } },
	{ &EfxReverbProps::lfReference, { AL_EAXREVERB_MIN_LFREFERENCE, AL_EAXREVERB_MAX_LFREFERENCE } },
};

static_assert( std::size( kEfxReverbLinearXerpFields ) + std::size( kEfxReverbLogExpXerpFields ) ==
	( sizeof( EfxReverbProps ) - sizeof( EfxReverbProps::decayHfLimit ) ) / sizeof( float ) );

// Choose AL_FALSE over AL_TRUE for decayHfLimit
static_assert( AL_FALSE < AL_TRUE );

void interpolateReverbProps( const EfxReverbProps *from, float frac, const EfxReverbProps *to, EfxReverbProps *dest ) {
	for( const auto &fieldPtr : kEfxReverbLinearXerpFields ) {
		( dest->*fieldPtr ) = std::lerp( ( from->*fieldPtr ), ( to->*fieldPtr ), frac );
	}
	for( const auto &[fieldPtr, bounds] : kEfxReverbLogExpXerpFields ) {
		const auto &[minBound, maxBound] = bounds;
		assert( minBound >= 0.0f && minBound < maxBound );
		const float fromValue = from->*fieldPtr;
		const float toValue   = to->*fieldPtr;
		assert( fromValue >= minBound && fromValue <= maxBound );
		assert( toValue >= minBound && toValue <= maxBound );
		if( fromValue == toValue ) {
			( dest->*fieldPtr ) = toValue;
		} else {
			// Note that we cannot just lerp tiny values under some threshold as it cannot be implemented for mixReverbProps()
			const float safetyEpsilon   = minBound > 0.0f ? 0.0f : 1e-6f;
			const float linearFromValue = std::log( fromValue + safetyEpsilon );
			const float linearToValue   = std::log( toValue + safetyEpsilon );
			const float xerpValue       = std::exp( std::lerp( linearFromValue, linearToValue, frac ) );
			( dest->*fieldPtr )         = wsw::clamp( xerpValue, minBound, maxBound );
		}
	}

	dest->decayHfLimit = wsw::min( from->decayHfLimit, to->decayHfLimit );
}

void mixReverbProps( const EfxReverbProps **begin, const EfxReverbProps **end, EfxReverbProps *dest ) {
	assert( end > begin );
	const float rcpCountOfProps = 1.0f / (float)( end - begin );

	// Clear field accumulators
	for( const auto &fieldPtr : kEfxReverbLinearXerpFields ) {
		( dest->*fieldPtr ) = 0.0f;
	}
	for( const auto &[fieldPtr, _] : kEfxReverbLogExpXerpFields ) {
		( dest->*fieldPtr ) = 0.0f;
	}
	dest->decayHfLimit = AL_TRUE;

	// Add (linear) values to field accumulators
	for( const EfxReverbProps **ppProps = begin; ppProps != end; ++ppProps ) {
		const EfxReverbProps *const props = *ppProps;
		for( const auto &fieldPtr : kEfxReverbLinearXerpFields ) {
			( dest->*fieldPtr ) += ( props->*fieldPtr );
		}
		for( const auto &[fieldPtr, bounds] : kEfxReverbLogExpXerpFields ) {
			const auto &[minBound, maxBound] = bounds;
			const float value = props->*fieldPtr;
			assert( value >= minBound && value <= maxBound );
			const float linearValue = std::log( value + ( minBound > 0.0f ? 0.0f : 1e-6f ) );
			( dest->*fieldPtr ) += linearValue;
		}
		dest->decayHfLimit = wsw::min( dest->decayHfLimit, props->decayHfLimit );
	}

	// Apply weights/map back
	for( const auto &fieldPtr : kEfxReverbLinearXerpFields ) {
		( dest->*fieldPtr ) *= rcpCountOfProps;
	}
	for( const auto &[fieldPtr, bounds] : kEfxReverbLogExpXerpFields ) {
		const float value   = std::exp( ( ( dest->*fieldPtr ) * rcpCountOfProps ) );
		( dest->*fieldPtr ) = wsw::clamp( value, bounds.first, bounds.second );
	}
}
