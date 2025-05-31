#include "snd_env_effects.h"
#include "snd_effect_sampler.h"
#include "snd_propagation.h"
#include <common/gs_public.h>

#include <limits>
#include <cmath>

// TODO
extern ListenerProps listenerProps;

void Effect::CheckCurrentlyBoundEffect( src_t *src ) {
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

void Effect::IntiallySetupEffect( src_t *src ) {
	alGenEffects( 1, &src->effect );
	alEffecti( src->effect, AL_EFFECT_TYPE, this->type );
}

float Effect::GetSourceGain( src_s *src ) const {
	return src->fvol * src->volumeVar->value;
}

void Effect::AdjustGain( src_t *src ) const {
	alSourcef( src->source, AL_GAIN, clampSourceGain( GetSourceGain( src ) ) );
}

void Effect::AttachEffect( src_t *src ) {
	// Set gain in any case (useful if the "attenuate on obstruction" flag has been turned off).
	AdjustGain( src );

	// Attach the filter to the source
	alSourcei( src->source, AL_DIRECT_FILTER, src->directFilter );
	// Attach the effect to the slot
	alAuxiliaryEffectSloti( src->effectSlot, AL_EFFECTSLOT_EFFECT, src->effect );
	// Feed the slot from the source
	alSource3i( src->source, AL_AUXILIARY_SEND_FILTER, src->effectSlot, 0, AL_FILTER_NULL );
}

void UnderwaterFlangerEffect::IntiallySetupEffect( src_t *src ) {
	Effect::IntiallySetupEffect( src );
	// This is the only place where the flanger gets tweaked
	alEffectf( src->effect, AL_FLANGER_DEPTH, 0.5f );
	alEffectf( src->effect, AL_FLANGER_FEEDBACK, -0.4f );
}

float UnderwaterFlangerEffect::GetSourceGain( src_t *src ) const {
	float gain = src->fvol * src->volumeVar->value;
	// Lower gain significantly if there is a medium transition
	// (if the listener is not in liquid and the source is, and vice versa)
	if( hasMediumTransition ) {
		gain *= 0.25f;
	}

	// Modify the gain by the direct obstruction factor
	// Lowering the gain by 1/3 on full obstruction is fairly sufficient (its not linearly perceived)
	gain *= 1.0f - 0.33f * directObstruction;
	assert( gain >= 0.0f && gain <= 1.0f );
	return gain;
}

void UnderwaterFlangerEffect::BindOrUpdate( src_t *src ) {
	CheckCurrentlyBoundEffect( src );

	alFilterf( src->directFilter, AL_LOWPASS_GAINHF, 0.0f );

	AttachEffect( src );
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

void EaxReverbEffect::BindOrUpdate( src_t *src ) {
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

void UnderwaterFlangerEffect::InterpolateProps( const Effect *oldOne, int timeDelta ) {
	const auto *that = Cast<UnderwaterFlangerEffect *>( oldOne );
	if( !that ) {
		return;
	}

	directObstruction = std::lerp( directObstruction, that->directObstruction, CalcLerpFracForTimeDelta( timeDelta ) );
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

void EaxReverbEffect::InterpolateProps( const Effect *oldOne, int timeDelta ) {
	const auto *that = Cast<EaxReverbEffect *>( oldOne );
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

	// Disable reverb panning for local sounds
	if( src->attenuation != ATTN_NONE ) {
		return;
	}
	// Disable panning for listener sounds except for weapon sounds
	if( listenerEntNum > 0 && src->entNum == listenerEntNum && src->attachmentTag != SoundSystem::WeaponAttachment ) {
		return;
	}

	const auto *updateState = &src->panningUpdateState;

	float earlyPanDir[3] { 0.0f, 0.0f, 0.0f };
	float latePanDir[3] { 0.0f, 0.0f, 0.0f };

	unsigned numAccountedDirs = 0;
	for( unsigned i = 0; i < updateState->numPassedSecondaryRays; ++i ) {
		float dir[3];
		VectorSubtract( listenerOrigin, src->panningUpdateState.reflectionPoints[i], dir );

		float squareDistance = VectorLengthSquared( dir );
		// Do not even take into account directions that have very short segments
		if( squareDistance < 48.0f * 48.0f ) {
			continue;
		}

		numAccountedDirs++;

		const float rcpDistance = Q_RSqrt( squareDistance );
		VectorScale( dir, rcpDistance, dir );

		const float distance         = squareDistance * rcpDistance;
		constexpr float rcpThreshold = 1.0f / REVERB_ENV_DISTANCE_THRESHOLD;
		const float distanceFrac     = wsw::min( 1.0f, distance * rcpThreshold );

		// Give far reflections a priority. Disallow zero values to guarantee that the normalization would succeed.
		const float lateFrac = 0.3f + 0.7f * distanceFrac;
		VectorMA( latePanDir, lateFrac, dir, latePanDir );

		// Give near reflections a priority
		const float earlyFrac = 1.0f - 0.7f * distanceFrac;
		VectorMA( earlyPanDir, earlyFrac, dir, earlyPanDir );
	}

	float earlyPan[3] { 0.0f, 0.0f, 0.0f };
	float latePan[3] { 0.0f, 0.0f, 0.0f };
	if( numAccountedDirs ) {
		VectorNormalizeFast( earlyPanDir );
		VectorNormalizeFast( latePanDir );

		// Convert to "speakers" coordinate system
		earlyPan[0] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_RIGHT] );
		latePan[0]  = -DotProduct( latePanDir, &listenerAxes[AXIS_RIGHT] );

		// Not sure about "minus" sign in this case...
		// We need something like 9.1 sound system (that has channels distinction in height) to test that
		earlyPan[1] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_UP] );
		latePan[1]  = -DotProduct( latePanDir, &listenerAxes[AXIS_UP] );

		earlyPan[2] = -DotProduct( earlyPanDir, &listenerAxes[AXIS_FORWARD] );
		latePan[2]  = -DotProduct( latePanDir, &listenerAxes[AXIS_FORWARD] );

		// We should be more confident regarding the direction if most of primary rays did yield results
		const float panningStrengthScale = 0.3f * (float)numAccountedDirs * Q_Rcp( (float)updateState->numPrimaryRays );

		VectorScale( earlyPan, panningStrengthScale, earlyPan );
		VectorScale( latePan, panningStrengthScale, latePan );
	}

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

	float sourcePitchScale    = 1.0f;
	const float *sourceOrigin = src->origin;
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
			// Provide a fake origin for the source that is at the same distance
			// as the real origin and is aligned to the sound propagation "window"
			// TODO: Precache at least the listener leaf for this sound backend update frame
			if( const int listenerLeaf = S_PointLeafNum( listenerOrigin ) ) {
				if( const int srcLeaf = S_PointLeafNum( src->origin ) ) {
					vec3_t dir;
					float distance;
					if( PropagationTable::Instance()->GetIndirectPathProps( srcLeaf, listenerLeaf, dir, &distance ) ) {
						// The table stores distance using this granularity, so it might be zero
						// for very close leaves. Adding an extra distance won't harm
						// (even if the indirect path length is already larger than the straight euclidean distance).
						distance += 256.0f;
						// Feels better with this multiplier
						distance *= 1.15f;
						// Negate the vector scale multiplier as the dir is an sound influx dir to the listener
						// and we want to shift the origin along the line of the dir but from the listener
						VectorScale( dir, -distance, tmpSourceOrigin );
						// Shift the listener origin in `dir` direction for `distance` units
						VectorAdd( listenerOrigin, tmpSourceOrigin, tmpSourceOrigin );
						// Use the shifted origin as a fake position in the world-space for the source
						sourceOrigin = tmpSourceOrigin;
						assert( sourcePitchScale > 0.0f && sourcePitchScale < 1.0f );
						const float gainLike = calcSoundGainForDistanceAndAttenuation( distance, ATTN_NORM );
						sourcePitchScale += ( 1.0f - sourcePitchScale ) * gainLike;
						if( std::fabs( sourcePitchScale - 1.0f ) < 0.005f ) {
							sourcePitchScale = 1.0f;
						}
					}
				}
			}
		}
	}

	alSourcef( src->source, AL_PITCH, src->chosenPitch * sourcePitchScale );
	alSourcefv( src->source, AL_POSITION, sourceOrigin );
	// The velocity is kept untouched for now.
	alSourcefv( src->source, AL_VELOCITY, src->velocity );
}