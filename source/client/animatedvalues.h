#ifndef WSW_9f0f029f_013f_4e67_9922_8f23e8184639_H
#define WSW_9f0f029f_013f_4e67_9922_8f23e8184639_H

#include <cassert>

#include <common/q_math.h>

struct RgbaLifespan {
	float initial[4] { 1.0f, 1.0f, 1.0f, 0.0f };
	float fadedIn[4] { 1.0f, 1.0f, 1.0f, 1.0f };
	float fadedOut[4] { 1.0f, 1.0f, 1.0f, 0.0f };

	float finishFadingInAtLifetimeFrac { 0.25f };
	float startFadingOutAtLifetimeFrac { 0.75f };

	void getColorForLifetimeFrac( float lifetimeFrac, float *colorRgba ) const __restrict {
		assert( lifetimeFrac >= 0.0f && lifetimeFrac <= 1.0f );

		assert( finishFadingInAtLifetimeFrac > 0.01f );
		assert( startFadingOutAtLifetimeFrac < 0.99f );
		assert( finishFadingInAtLifetimeFrac + 0.01f < startFadingOutAtLifetimeFrac );

		if( lifetimeFrac < finishFadingInAtLifetimeFrac ) [[unlikely]] {
			// Fade in
			float fadeInFrac = lifetimeFrac * Q_Rcp( finishFadingInAtLifetimeFrac );
			assert( fadeInFrac > -0.01f && fadeInFrac < 1.01f );
			fadeInFrac = wsw::clamp( fadeInFrac, 0.0f, 1.0f );
			Vector4Lerp( initial, fadeInFrac, fadedIn, colorRgba );
		} else {
			if( lifetimeFrac > startFadingOutAtLifetimeFrac ) [[unlikely]] {
				// Fade out
				float fadeOutFrac = lifetimeFrac - startFadingOutAtLifetimeFrac;
				fadeOutFrac *= Q_Rcp( 1.0f - startFadingOutAtLifetimeFrac );
				assert( fadeOutFrac > -0.01f && fadeOutFrac < 1.01f );
				fadeOutFrac = wsw::clamp( fadeOutFrac, 0.0f, 1.0f );
				Vector4Lerp( fadedIn, fadeOutFrac, fadedOut, colorRgba );
			} else {
				// Use the color of the "faded-in" state
				Vector4Copy( fadedIn, colorRgba );
			}
		}
	}
};

struct RgbLifespan {
	float initial[3] { 1.0f, 1.0f, 1.0f };
	float fadedIn[3] { 1.0f, 1.0f, 1.0f };
	float fadedOut[3] { 1.0f, 1.0f, 1.0f };

	float finishFadingInAtLifetimeFrac { 0.25f };
	float startFadingOutAtLifetimeFrac { 0.75f };

	void getColorForLifetimeFrac( float lifetimeFrac, float *colorRgb ) const __restrict {
		assert( lifetimeFrac >= 0.0f && lifetimeFrac <= 1.0f );

		assert( finishFadingInAtLifetimeFrac > 0.01f );
		assert( startFadingOutAtLifetimeFrac < 0.99f );
		assert( finishFadingInAtLifetimeFrac + 0.01f < startFadingOutAtLifetimeFrac );

		if( lifetimeFrac < finishFadingInAtLifetimeFrac ) [[unlikely]] {
			// Fade in
			float fadeInFrac = lifetimeFrac * Q_Rcp( finishFadingInAtLifetimeFrac );
			assert( fadeInFrac > -0.01f && fadeInFrac < 1.01f );
			fadeInFrac = wsw::clamp( fadeInFrac, 0.0f, 1.0f );
			VectorLerp( initial, fadeInFrac, fadedIn, colorRgb );
		} else {
			if( lifetimeFrac > startFadingOutAtLifetimeFrac ) [[unlikely]] {
				// Fade out
				float fadeOutFrac = lifetimeFrac - startFadingOutAtLifetimeFrac;
				fadeOutFrac *= Q_Rcp( 1.0f - startFadingOutAtLifetimeFrac );
				assert( fadeOutFrac > -0.01f && fadeOutFrac < 1.01f );
				fadeOutFrac = wsw::clamp( fadeOutFrac, 0.0f, 1.0f );
				VectorLerp( fadedIn, fadeOutFrac, fadedOut, colorRgb );
			} else {
				// Use the color of the "faded-in" state
				VectorCopy( fadedIn, colorRgb );
			}
		}
	}
};

struct ValueLifespan {
	float initial { 0.0f };
	float fadedIn { 1.0f };
	float fadedOut { 0.0f };

	float finishFadingInAtLifetimeFrac { 0.25f };
	float startFadingOutAtLifetimeFrac { 0.75f };

	[[nodiscard]]
	auto getValueForLifetimeFrac( float lifetimeFrac ) const __restrict -> float {
		assert( lifetimeFrac >= 0.0f && lifetimeFrac <= 1.0f );

		assert( finishFadingInAtLifetimeFrac > 0.01f );
		assert( startFadingOutAtLifetimeFrac < 0.99f );
		assert( finishFadingInAtLifetimeFrac + 0.01f < startFadingOutAtLifetimeFrac );

		if( lifetimeFrac < finishFadingInAtLifetimeFrac ) [[unlikely]] {
			float fadeInFrac = lifetimeFrac * Q_Rcp( finishFadingInAtLifetimeFrac );
			assert( fadeInFrac > -0.01f && fadeInFrac < 1.01f );
			fadeInFrac = wsw::clamp( fadeInFrac, 0.0f, 1.0f );
			return fadeInFrac * fadedIn + ( 1.0f - fadeInFrac ) * initial;
		} else {
			if( lifetimeFrac > startFadingOutAtLifetimeFrac ) [[unlikely]] {
				float fadeOutFrac = lifetimeFrac - startFadingOutAtLifetimeFrac;
				fadeOutFrac *= Q_Rcp( 1.0f - startFadingOutAtLifetimeFrac );
				assert( fadeOutFrac > -0.01f && fadeOutFrac < 1.01f );
				fadeOutFrac = wsw::clamp( fadeOutFrac, 0.0f, 1.0f );
				return fadeOutFrac * fadedOut + ( 1.0f - fadeOutFrac ) * fadedIn;
			} else {
				return fadedIn;
			}
		}
	}
};

struct LightLifespan {
	RgbLifespan colorLifespan;
	ValueLifespan radiusLifespan;

	void getRadiusAndColorForLifetimeFrac( float lifetimeFrac, float *radius, float *colorRgb ) const __restrict {
		colorLifespan.getColorForLifetimeFrac( lifetimeFrac, colorRgb );
		*radius = radiusLifespan.getValueForLifetimeFrac( lifetimeFrac );
	}
};

#endif