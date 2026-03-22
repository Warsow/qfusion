#include "snd_env_effects.h"
#include "environmentupdates.h"
#include <common/facilities/gs_public.h>
#include <common/facilities/protocol.h>
#include <common/facilities/sysclock.h>

void EaxReverbEffect::CheckCurrentlyBoundEffect( src_t *src ) {
	ALint effectType;

	// We limit every source to have only a single effect.
	// This is required to comply with the runtime effects count restriction.
	// If the effect type has been changed, we have to delete an existing effect.
	alGetEffecti( src->effect, AL_EFFECT_TYPE, &effectType );
	if( this->type == effectType ) {
		return;
	}

	// Detach the slot from the source
	alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, AL_EFFECTSLOT_NULL, 0, AL_FILTER_NULL );
	// Detach the effect from the slot
	alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, AL_EFFECT_NULL );

	// TODO: Can we reuse the effect?
	alDeleteEffects( 1, &src->effect );

	IntiallySetupEffect( src );
}

void EaxReverbEffect::IntiallySetupEffect( src_t *src ) {
	alGenEffects( 1, &src->effect );
	alEffecti( src->effect, AL_EFFECT_TYPE, this->type );
}

float EaxReverbEffect::GetSourceGain( src_s *src ) const {
	return src->fvol * src->volumeVar->value;
}

void EaxReverbEffect::AdjustGain( src_t *src ) const {
	alSourcef( src->source, AL_GAIN, checkSourceGain( GetSourceGain( src ) ) );
}

void EaxReverbEffect::AttachEffect( src_t *src ) {
	// Set gain in any case (useful if the "attenuate on obstruction" flag has been turned off).
	AdjustGain( src );

	// Attach the filter to the source
	alSourcei( src->source, AL_DIRECT_FILTER, src->directFilter );
	// Attach the effect to the slot
	alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, src->effect );
	// Feed the slot from the source
	alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, src->effectSlot, 0, AL_FILTER_NULL );
}

[[maybe_unused]]
static void PrintReverbProps( const EfxReverbProps &props ) {
	Com_Printf( "====================== : %" PRId64 "\n", Sys_Milliseconds() );
	Com_Printf( "Density                : %f\n", props.density );
	Com_Printf( "Diffusion              : %f\n", props.diffusion );
	Com_Printf( "Decay time             : %f\n", props.decayTime );
	Com_Printf( "Decay HF Ratio         : %f\n", props.decayHfRatio );
	Com_Printf( "Decay LF Ratio         : %f\n", props.decayLfRatio );
	Com_Printf( "Gain                   : %f\n", props.gain );
	Com_Printf( "Gain HF                : %f\n", props.gainHf );
	Com_Printf( "Gain LF                : %f\n", props.gainLf );
	Com_Printf( "Reflections gain       : %f\n", props.reflectionsGain );
	Com_Printf( "Reflections delay      : %f\n", props.reflectionsDelay );
	Com_Printf( "Late reverb gain       : %f\n", props.lateReverbGain );
	Com_Printf( "Late reverb delay      : %f\n", props.lateReverbDelay );
	Com_Printf( "Echo time              : %f\n", props.echoTime );
	Com_Printf( "Echo depth             : %f\n", props.echoDepth );
	Com_Printf( "Modulation time        : %f\n", props.modulationTime );
	Com_Printf( "Modulation depth       : %f\n", props.modulationDepth );
	Com_Printf( "Air absorption gain HF : %f\n", props.airAbsorptionGainHf );
	Com_Printf( "HF reference           : %f\n", props.hfReference );
	Com_Printf( "LF reference           : %f\n", props.lfReference );
}

void EaxReverbEffect::BindOrUpdate( src_t *src, const ListenerProps &listenerProps ) {
	CheckCurrentlyBoundEffect( src );

	//PrintReverbProps( this->reverbProps );

	alEffectf( src->effect, AL_EAXREVERB_DENSITY, this->reverbProps.density );
	alEffectf( src->effect, AL_EAXREVERB_DIFFUSION, this->reverbProps.diffusion );

	alEffectf( src->effect, AL_EAXREVERB_DECAY_TIME, this->reverbProps.decayTime );
	alEffectf( src->effect, AL_EAXREVERB_DECAY_HFRATIO, this->reverbProps.decayHfRatio );
	alEffectf( src->effect, AL_EAXREVERB_DECAY_LFRATIO, this->reverbProps.decayLfRatio );

	const float distance         = DistanceFast( src->origin, listenerProps.origin );
	const float distanceGainFrac = calcSoundGainForDistanceAndAttenuation( distance, src->attenuation );
	assert( distanceGainFrac >= 0.0f && distanceGainFrac <= 1.0f );

	// Make the effect less pronounced on close distance
	const float effectGain = this->reverbProps.gain * ( 1.0f - 0.1f * distanceGainFrac );

	alEffectf( src->effect, AL_EAXREVERB_GAIN, effectGain );
	alEffectf( src->effect, AL_EAXREVERB_GAINHF, this->reverbProps.gainHf * ( 1.0f - 0.5f * secondaryRaysObstruction ) );
	alEffectf( src->effect, AL_EAXREVERB_GAINLF, this->reverbProps.gainLf );

	alEffectf( src->effect, AL_EAXREVERB_REFLECTIONS_GAIN, this->reverbProps.reflectionsGain );
	alEffectf( src->effect, AL_EAXREVERB_REFLECTIONS_DELAY, this->reverbProps.reflectionsDelay );

	alEffectf( src->effect, AL_EAXREVERB_LATE_REVERB_GAIN, this->reverbProps.lateReverbGain );
	alEffectf( src->effect, AL_EAXREVERB_LATE_REVERB_DELAY, this->reverbProps.lateReverbDelay );

	alEffectf( src->effect, AL_EAXREVERB_ECHO_TIME, this->reverbProps.echoTime );
	alEffectf( src->effect, AL_EAXREVERB_ECHO_DEPTH, this->reverbProps.echoDepth );

	alEffectf( src->effect, AL_EAXREVERB_MODULATION_TIME, this->reverbProps.modulationTime );
	alEffectf( src->effect, AL_EAXREVERB_MODULATION_DEPTH, this->reverbProps.modulationDepth );

	alEffectf( src->effect, AL_EAXREVERB_AIR_ABSORPTION_GAINHF, this->reverbProps.airAbsorptionGainHf );

	alEffectf( src->effect, AL_EAXREVERB_LFREFERENCE, this->reverbProps.lfReference );
	alEffectf( src->effect, AL_EAXREVERB_HFREFERENCE, this->reverbProps.hfReference );

	alEffecti( src->effect, AL_EAXREVERB_DECAY_HFLIMIT, this->reverbProps.decayHfLimit );

	// Configure the direct send filter parameters

	assert( directObstruction >= 0.0f && directObstruction <= 1.0f );
	assert( secondaryRaysObstruction >= 0.0f && secondaryRaysObstruction <= 1.0f );

	// Both partial obstruction factors are within [0, 1] range, so we can get a weighted average
	const float obstructionFrac = 0.3f * this->directObstruction + 0.7f * this->secondaryRaysObstruction;
	assert( obstructionFrac >= 0.0f && obstructionFrac <= 1.0f );

	// Strongly suppress the dry path on obstruction.
	// Note: we do not touch the entire source gain.
	alFilterf( src->directFilter, AL_LOWPASS_GAIN, 1.0f - 0.7f * obstructionFrac );

	// There's nothing special with looping sources, their current sfx/sounds happen to benefit from that
	if( src->isLooping ) {
		alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 1.0f - obstructionFrac );
	} else {
		alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 1.0f - 0.5f * obstructionFrac );
	}

	AttachEffect( src );
}

static float CalcLerpFracForTimeDelta( int timeDelta ) {
	if( float rawFrac = (float)timeDelta * ( 1.0f / 200.0f ); rawFrac < 1.0f ) {
		return 0.50f + 0.45f * rawFrac;
	}
	return 1.0f;
}

bool EaxReverbEffect::ShouldKeepLingering( float sourceQualityHint, int64_t millisNow ) const {
	if( sourceQualityHint <= 0 ) {
		return false;
	}
	if( millisNow - lastUpdateAt > 200 ) {
		return false;
	}
	clamp_high( sourceQualityHint, 1.0f );
	float factor = 0.5f * sourceQualityHint;
	factor += 0.25f * ( ( 1.0f - directObstruction ) + ( 1.0f - secondaryRaysObstruction ) );
	assert( factor >= 0.0f && factor <= 1.0f );
	return distanceAtLastUpdate < 192.0f + 768.0f * factor;
}

void EaxReverbEffect::InterpolateProps( const EaxReverbEffect *that, int timeDelta ) {
	if( !that ) {
		return;
	}

	const float lerpFrac = CalcLerpFracForTimeDelta( timeDelta );

	directObstruction        = std::lerp( directObstruction, that->directObstruction, lerpFrac );
	secondaryRaysObstruction = std::lerp( secondaryRaysObstruction, that->secondaryRaysObstruction, lerpFrac );

	interpolateReverbProps( &that->reverbProps, lerpFrac, &this->reverbProps, &this->reverbProps );
}



void EaxReverbEffect::UpdatePanning( src_s *src, int listenerEntNum, const vec3_t listenerOrigin, const mat3_t listenerAxes ) {
	// "If there is an active EaxReverbEffect, setting source origin/velocity is delegated to it".
	UpdateDelegatedSpatialization( src, listenerEntNum, listenerOrigin );

	if( src->attenuation != ATTN_NONE ) {
		return;
	}
	// Disable panning for listener sounds except for weapon sounds
	if( listenerEntNum > 0 && src->entNum == listenerEntNum && src->attachmentTag != SoundSystem::WeaponAttachment ) {
		return;
	}

	vec3_t earlyPan, latePan;
	ENV_CalculateSourcePan( listenerOrigin, listenerAxes, &src->panningUpdateState, earlyPan, latePan );

	alEffectfv( src->effect, AL_EAXREVERB_REFLECTIONS_PAN, earlyPan );
	alEffectfv( src->effect, AL_EAXREVERB_LATE_REVERB_PAN, latePan );
}

void EaxReverbEffect::UpdateDelegatedSpatialization( struct src_s *src, int listenerEntNum, const vec3_t listenerOrigin ) {
	if( src->attenuation == ATTN_NONE ) {
		// It MUST already be a relative sound
#ifndef PUBLIC_BUILD
		ALint value;
		alGetSourcei( src->source, AL_SOURCE_RELATIVE, &value );
		assert( value == AL_TRUE );
#endif
		return;
	}

	alSourcei( src->source, AL_SOURCE_RELATIVE, AL_FALSE );

	float sourcePitchScale = 1.0f;
	vec3_t sourceOriginToUse;
	VectorCopy( src->origin, sourceOriginToUse );

	// Don't do that for own sounds
	if( listenerEntNum <= 0 || listenerEntNum != src->entNum ) {
		// Setting effect panning vectors is not sufficient for "realistic" obstruction,
		// as the dry path is still propagates like if there were no obstacles and walls.
		// We try modifying the source origin as well to simulate sound propagation.
		// These conditions must be met:
		// 1) the direct path is fully obstructed
		// 2) there is a definite propagation path
		if( directObstruction == 1.0f ) {
			sourcePitchScale = 0.96f;
			ENV_CalculatePropagationOrigin( listenerOrigin, src->origin, &sourcePitchScale, sourceOriginToUse );
		}
	}

	assert( sourcePitchScale > 0.0f && sourcePitchScale <= 1.0f );
	alSourcef( src->source, AL_PITCH, src->chosenPitch * sourcePitchScale );

	alSourcefv( src->source, AL_POSITION, sourceOriginToUse );
	// The velocity is kept untouched for now.
	alSourcefv( src->source, AL_VELOCITY, src->velocity );
}