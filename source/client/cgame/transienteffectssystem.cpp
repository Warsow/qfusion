/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2022 Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include "transienteffectssystem.h"
#include "cg_local.h"
#include <client/client.h>
#include <common/links.h>
#include <common/configvars.h>
#include <common/noise.h>
#include <common/profilerscope.h>

#include <cstdlib>
#include <cstring>

extern BoolConfigVar v_explosionWave, v_explosionSmoke, v_explosionClusters;
extern BoolConfigVar v_cartoonHitEffect;

static constexpr unsigned kCachedSmokeBulgeSubdivLevel = 3;
static constexpr unsigned kCachedSmokeBulgeMaskSize    = 642;
static constexpr unsigned kNumCachedSmokeBulgeMasks    = 8;

TransientEffectsSystem::~TransientEffectsSystem() {
	clear();
}

void TransientEffectsSystem::clear() {
	for( EntityEffect *effect = m_entityEffectsHead, *next; effect; effect = next ) { next = effect->next;
		unlinkAndFreeEntityEffect( effect );
	}
	assert( !m_entityEffectsHead );
	for( PolyEffect *effect = m_polyEffectsHead, *next; effect; effect = next ) { next = effect->next;
		unlinkAndFreePolyEffect( effect );
	}
	assert( !m_polyEffectsHead );
	for( LightEffect *effect = m_lightEffectsHead, *next; effect; effect = next ) { next = effect->next;
		unlinkAndFreeLightEffect( effect );
	}
	assert( !m_lightEffectsHead );
	for( DelayedEffect *effect = m_delayedEffectsHead, *next; effect; effect = next ) { next = effect->next;
		unlinkAndFreeDelayedEffect( effect );
	}
	assert( !m_delayedEffectsHead );

	m_lastTime = 0;
}

#define asByteColor( r, g, b, a ) {  \
		(uint8_t)( ( r ) * 255.0f ), \
		(uint8_t)( ( g ) * 255.0f ), \
		(uint8_t)( ( b ) * 255.0f ), \
		(uint8_t)( ( a ) * 255.0f )  \
	}

static const byte_vec4_t kFireCoreReplacementPalette[] {
	asByteColor( 1.00f, 0.67f, 0.56f, 0.56f ),
	asByteColor( 1.00f, 0.67f, 0.50f, 0.67f ),
	asByteColor( 1.00f, 0.56f, 0.42f, 0.50f ),
	asByteColor( 1.00f, 0.56f, 0.42f, 0.28f ),
};

static const byte_vec4_t kFireReplacementPalette[] {
	asByteColor( 1.00f, 0.42f, 0.00f, 0.15f ),
	asByteColor( 1.00f, 0.28f, 0.00f, 0.15f ),
	asByteColor( 1.00f, 0.67f, 0.00f, 0.15f ),
	asByteColor( 1.00f, 0.50f, 0.00f, 0.15f ),
	asByteColor( 1.00f, 0.56f, 0.00f, 0.15f ),
};

static const byte_vec4_t kDarkFireDecayPalette[] {
	asByteColor( 0.3f, 0.2f, 0.2f, 0.3f ),
	asByteColor( 0.2f, 0.2f, 0.2f, 0.4f ),
	asByteColor( 0.3f, 0.1f, 0.1f, 0.3f ),
	asByteColor( 0.2f, 0.1f, 0.1f, 0.3f ),
	asByteColor( 0.3f, 0.2f, 0.2f, 0.3f ),
	asByteColor( 0.0f, 0.0f, 0.0f, 0.3f ),
};

static const byte_vec4_t kLightFireDecayPalette[] {
	asByteColor( 0.7f, 0.7f, 0.7f, 0.1f ),
	asByteColor( 0.3f, 0.3f, 0.3f, 0.2f ),
	asByteColor( 0.4f, 0.4f, 0.4f, 0.2f ),
	asByteColor( 0.5f, 0.5f, 0.5f, 0.3f ),
	asByteColor( 0.6f, 0.6f, 0.6f, 0.2f ),
	asByteColor( 0.7f, 0.7f, 0.7f, 0.1f ),
};

struct FireHullLayerParamsHolder {
	SimulatedHullsSystem::ColorChangeTimelineNode darkColorChangeTimeline[5][3] {
		{
			{ /* Layer 0 */ },
			{
				.activateAtLifetimeFraction = 0.35f, .replacementPalette = kFireCoreReplacementPalette,
				.sumOfDropChanceForThisSegment = 0.0f, .sumOfReplacementChanceForThisSegment = 0.1f,
			},
			{
				.activateAtLifetimeFraction = 0.6f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 3.5f, .sumOfReplacementChanceForThisSegment = 0.5f,
			}
		},
		{
			{ /* Layer 1 */ },
			{
				.activateAtLifetimeFraction = 0.35f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 0.0f, .sumOfReplacementChanceForThisSegment = 0.2f,
			},
			{
				.activateAtLifetimeFraction = 0.6f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 2.5f, .sumOfReplacementChanceForThisSegment = 3.5f,
			}
		},
		{
			{ /* Layer 2 */ },
			{
				.activateAtLifetimeFraction = 0.3f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 0.0f, .sumOfReplacementChanceForThisSegment = 0.3f,
			},
			{
				.activateAtLifetimeFraction = 0.6f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 2.0f, .sumOfReplacementChanceForThisSegment = 4.0f,
				.allowIncreasingOpacity = true
			}
		},
		{
			{ /* Layer 3 */ },
			{
				.activateAtLifetimeFraction = 0.3f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 0.0f, .sumOfReplacementChanceForThisSegment = 0.5f,
			},
			{
				.activateAtLifetimeFraction = 0.55f,
				.sumOfDropChanceForThisSegment = 2.0f, .sumOfReplacementChanceForThisSegment = 4.5f,
				.allowIncreasingOpacity = true
			}
		},
		{
			{ /* Layer 4 */ },
			{
				.activateAtLifetimeFraction = 0.1f, .replacementPalette = kFireReplacementPalette,
				.sumOfDropChanceForThisSegment = 0.0f, .sumOfReplacementChanceForThisSegment = 0.5f,
			},
			{
				.activateAtLifetimeFraction = 0.5f,
				.sumOfDropChanceForThisSegment = 2.0f, .sumOfReplacementChanceForThisSegment = 4.5f,
				.allowIncreasingOpacity = true
			}
		}
	};

	SimulatedHullsSystem::HullLayerParams darkHullLayerParams[5] {
		{
			.speed = 22.5f, .finalOffset = 8.0f,
			.speedSpikeChance = 0.05f, .minSpeedSpike = 10.0f, .maxSpeedSpike = 15.0f,
			.biasAlongChosenDir = 30.0f,
			.baseInitialColor = { 1.0f, 0.9f, 0.9f, 1.0f },
			.bulgeInitialColor = { 1.0f, 1.0f, 1.0f, 1.0f },
		},
		{
			.speed = 35.0f, .finalOffset = 6.0f,
			.speedSpikeChance = 0.05f, .minSpeedSpike = 7.5f, .maxSpeedSpike = 15.0f,
			.biasAlongChosenDir = 25.0f,
			.baseInitialColor = { 1.0f, 0.9f, 0.9f, 0.7f },
			.bulgeInitialColor = { 1.0f, 1.0f, 1.0f, 0.9f },
		},
		{
			.speed = 45.0f, .finalOffset = 4.0f,
			.speedSpikeChance = 0.05f, .minSpeedSpike = 7.5f, .maxSpeedSpike = 15.0f,
			.biasAlongChosenDir = 20.0f,
			.baseInitialColor = { 1.0f, 0.9f, 0.4f, 0.7f },
			.bulgeInitialColor = { 1.0f, 0.9f, 0.7f, 0.9f },
		},
		{
			.speed = 52.5f, .finalOffset = 2.0f,
			.speedSpikeChance = 0.05f, .minSpeedSpike = 7.5f, .maxSpeedSpike = 15.0f,
			.biasAlongChosenDir = 20.0f,
			.baseInitialColor = { 1.0f, 0.6f, 0.3f, 0.7f },
			.bulgeInitialColor = { 1.0f, 0.9f, 0.7f, 0.7f },
		},
		{
			.speed = 60.0f, .finalOffset = 0.0f,
			.speedSpikeChance = 0.05f, .minSpeedSpike = 7.5f, .maxSpeedSpike = 15.0f,
			.biasAlongChosenDir = 10.0f,
			.baseInitialColor = { 1.0f, 0.5f, 0.2f, 0.7f },
			.bulgeInitialColor = { 1.0f, 0.7f, 0.4f, 0.7f },
		},
	};

	SimulatedHullsSystem::HullLayerParams lightHullLayerParams[5];
	SimulatedHullsSystem::ColorChangeTimelineNode lightColorChangeTimeline[5][3];

	SimulatedHullsSystem::HullLayerParams darkClusterHullLayerParams[2];
	SimulatedHullsSystem::HullLayerParams lightClusterHullLayerParams[2];

	FireHullLayerParamsHolder() noexcept {
		for( size_t layerNum = 0; layerNum < std::size( darkHullLayerParams ); ++layerNum ) {
			// Set the timeline which is not set inline to reduce boilerplate
			darkHullLayerParams[layerNum].colorChangeTimeline = darkColorChangeTimeline[layerNum];

			std::memcpy( lightColorChangeTimeline[layerNum], darkColorChangeTimeline[layerNum],
						 sizeof( darkColorChangeTimeline[layerNum] ) );

			const size_t lastNodeIndex = std::size( lightColorChangeTimeline[layerNum] ) - 1;

			// Raise it for light hulls so they morph to smoke more aggressively
			lightColorChangeTimeline[layerNum][lastNodeIndex].sumOfReplacementChanceForThisSegment *= 1.5f;

			// Set replacement palettes that differ for these layers
			if( layerNum > 2 ) {
				assert( darkColorChangeTimeline[layerNum][lastNodeIndex].replacementPalette.empty() );
				darkColorChangeTimeline[layerNum][lastNodeIndex].replacementPalette  = kDarkFireDecayPalette;
				assert( lightColorChangeTimeline[layerNum][lastNodeIndex].replacementPalette.empty() );
				lightColorChangeTimeline[layerNum][lastNodeIndex].replacementPalette = kLightFireDecayPalette;
			}

			lightHullLayerParams[layerNum] = darkHullLayerParams[layerNum];
			lightHullLayerParams[layerNum].colorChangeTimeline = lightColorChangeTimeline[layerNum];
		}

		assert( std::size( darkClusterHullLayerParams ) == 2 && std::size( lightClusterHullLayerParams ) == 2 );
		darkClusterHullLayerParams[0]  = std::begin( darkHullLayerParams )[0];
		darkClusterHullLayerParams[1]  = std::end( darkHullLayerParams )[-1];
		lightClusterHullLayerParams[0] = std::begin( lightHullLayerParams )[0];
		lightClusterHullLayerParams[1] = std::end( lightHullLayerParams )[-1];

		for( SimulatedHullsSystem::HullLayerParams *layerParam: { &darkClusterHullLayerParams[0],
																  &darkClusterHullLayerParams[1],
																  &lightClusterHullLayerParams[0],
																  &lightClusterHullLayerParams[1] } ) {
			// Account for small lifetime relatively to the primary hull
			layerParam->minSpeedSpike *= 3.0f;
			layerParam->maxSpeedSpike *= 3.0f;
			layerParam->speed         *= 3.0f;
			// Make it more irregular as well
			layerParam->biasAlongChosenDir *= 1.5f;
		}
	}
};

static const FireHullLayerParamsHolder kFireHullParams;

struct ToonSmokeOffsetKeyframeHolder {
	static constexpr int kNumVertices           = 2562;
	static constexpr int kNumKeyframes          = 20;
	static constexpr unsigned kMinLifetime      = 850;
	static constexpr unsigned kNumShadingLayers = 3;
	// Looks like a variable and not a constant, even it's technicall is the latter thing
	static constexpr float maxOffset            = 1.0f;

	float vertexOffsetStorage[kNumKeyframes][kNumVertices];
	float fireVertexMaskStorage[kNumKeyframes][kNumVertices];
	float fadeVertexMaskStorage[kNumKeyframes][kNumVertices];
	float vertexOffsetsFromLimitsStorage[kNumKeyframes][kNumVertices];

	// ShadingLayer does not have a default constructor, here's the workaround
	wsw::StaticVector<SimulatedHullsSystem::ShadingLayer, kNumKeyframes * kNumShadingLayers> shadingLayersStorage;
	// This type needs careful initialization of fields, hence it's better to construct instances manually
	wsw::StaticVector<SimulatedHullsSystem::OffsetKeyframe, kNumKeyframes> setOfKeyframes;

	static constexpr byte_vec4_t kFireColors[3] {
		{ 25, 25, 25, 255 },   // Gray
		{ 255, 70, 30, 255 },  // Orange
		{ 255, 160, 45, 255 }, // Yellow
	};

	static constexpr byte_vec4_t kFadeColors[2] {
		{ 0, 0, 0, 255 },
		{ 100, 100, 100, 0 },
	};

	static constexpr byte_vec4_t kHighlightColors[2] {
		{ 55, 55, 55, 0 },
		{ 0, 0, 0, 0 },
	};

	ToonSmokeOffsetKeyframeHolder() noexcept {
		constexpr float scrollSpeed     = 1.43f;
		constexpr float scrollDistance  = scrollSpeed * ( (float)kMinLifetime * 1e-3f );

		constexpr float initialSize     = 0.1f;
		constexpr float fadeRange       = 0.12f;
		constexpr float fadeStartAtFrac = 0.1f;
		constexpr float zFadeInfluence  = 0.3f;

		const std::span<const vec4_t> verticesSpan = SimulatedHullsSystem::getUnitIcosphere( 4 );
		assert( verticesSpan.size() == kNumVertices );

		for( unsigned frameNum = 0; frameNum < kNumKeyframes; frameNum++ ) {
			const float keyframeFrac     = (float)(frameNum) / (float)( kNumKeyframes - 1 ); // -1 so the final value is 1.0f
			const float fireLifetime     = 0.54f; // 0.47
			const float fireStartFrac    = 0.9f;
			const float fireLifetimeFrac = wsw::min( 1.0f, keyframeFrac * ( 1.0f / fireLifetime ) );
			const float fireFrac         = fireLifetimeFrac * fireStartFrac + ( 1.0f - fireStartFrac );

			const float scrolledDistance = scrollDistance * keyframeFrac;

			float *const frameVertexOffsets     = vertexOffsetStorage[frameNum];
			float *const frameFireVertexMask    = fireVertexMaskStorage[frameNum];
			float *const frameFadeVertexMask    = fadeVertexMaskStorage[frameNum];
			float *const frameOffsetsFromLimits = vertexOffsetsFromLimitsStorage[frameNum];

			//const float expansion = (1-initialSize) * ( 1.f - (x-1.f)*(x-1.f) ) + initialSize;
			const float initialVelocity = 5.0f;
			const float expansion       = ( 1.0f - initialSize ) * ( 1.0f - std::exp( -initialVelocity * keyframeFrac ) ) + initialSize;

			const vec4_t *const vertices = verticesSpan.data();
			for( unsigned vertexNum = 0; vertexNum < kNumVertices; vertexNum++ ) {
				const float *const vertex = vertices[vertexNum];
				const float voronoiNoise  = calcVoronoiNoiseSquared( vertex[0], vertex[1], vertex[2] + scrolledDistance );
				const float offset        = expansion * ( 1.0f - 0.7f * voronoiNoise );

				frameVertexOffsets[vertexNum]  = offset;
				frameFireVertexMask[vertexNum] = voronoiNoise; // Values between 1 and 0 where 1 has the highest offset

				const float simplexNoise       = calcSimplexNoise3D( vertex[0], vertex[1], vertex[2] - scrolledDistance);
				const float fadeFrac           = ( keyframeFrac - fadeStartAtFrac ) / ( 1.0f - fadeStartAtFrac );
				const float zFade              = 0.5f * ( vertex[2] + 1.0f ) * zFadeInfluence;
				frameFadeVertexMask[vertexNum] = fadeFrac - simplexNoise * ( 1.0f - zFadeInfluence ) - zFade + fadeRange;
			}

			SimulatedHullsSystem::MaskedShadingLayer fireMaskedShadingLayer;
			fireMaskedShadingLayer.vertexMaskValues = frameFireVertexMask;
			fireMaskedShadingLayer.colors           = kFireColors;

			fireMaskedShadingLayer.colorRanges[0] = fireFrac * fireFrac;
			fireMaskedShadingLayer.colorRanges[1] = fireFrac;
			fireMaskedShadingLayer.colorRanges[2] = std::sqrt( fireFrac );
			fireMaskedShadingLayer.blendMode      = SimulatedHullsSystem::BlendMode::AlphaBlend;
			fireMaskedShadingLayer.alphaMode      = SimulatedHullsSystem::AlphaMode::Override;

			// 0.1f to 0.2f produced a neat outline along the hull
			SimulatedHullsSystem::DotShadingLayer highlightDotShadingLayer;
			highlightDotShadingLayer.colors         = kHighlightColors;
			highlightDotShadingLayer.colorRanges[0] = 0.4f;
			highlightDotShadingLayer.colorRanges[1] = 0.48f;
			highlightDotShadingLayer.blendMode      = SimulatedHullsSystem::BlendMode::Add;
			highlightDotShadingLayer.alphaMode      = SimulatedHullsSystem::AlphaMode::Add;

			SimulatedHullsSystem::MaskedShadingLayer fadeMaskedShadingLayer;
			fadeMaskedShadingLayer.vertexMaskValues = frameFadeVertexMask;
			fadeMaskedShadingLayer.colors           = kFadeColors;
			fadeMaskedShadingLayer.colorRanges[0]   = 0.0f;
			fadeMaskedShadingLayer.colorRanges[1]   = fadeRange;
			fadeMaskedShadingLayer.blendMode        = SimulatedHullsSystem::BlendMode::Add;
			fadeMaskedShadingLayer.alphaMode        = SimulatedHullsSystem::AlphaMode::Override;

			const auto *const frameLayersData = shadingLayersStorage.data() + shadingLayersStorage.size();
			static_assert( kNumShadingLayers == 3 );
			shadingLayersStorage.emplace_back( fireMaskedShadingLayer );
			shadingLayersStorage.emplace_back( highlightDotShadingLayer );
			shadingLayersStorage.emplace_back( fadeMaskedShadingLayer );

			std::fill( frameOffsetsFromLimits, frameOffsetsFromLimits + kNumVertices, 0.0f );

			setOfKeyframes.emplace_back( SimulatedHullsSystem::OffsetKeyframe {
				.lifetimeFraction = keyframeFrac,
				.offsets          = frameVertexOffsets,
				.offsetsFromLimit = frameOffsetsFromLimits,
				.shadingLayers    = { frameLayersData, kNumShadingLayers },
			});
		}
	}
};

static const ToonSmokeOffsetKeyframeHolder toonSmokeKeyframes;

// Caution: Specifying the full range of vertices would not allow the cloud to be rendered due to reaching the limit

static SimulatedHullsSystem::CloudMeshProps g_fireInnerCloudMeshProps {
	.alphaScaleLifespan              = { .initial = 0.0f, .fadedIn = 1.0f, .fadedOut = 0.0f },
	.radiusLifespan                  = { .initial = 0.0f, .fadedIn = 3.0f, .fadedOut = 1.0f },
	.tessLevelShiftForMinVertexIndex = -1,
	.tessLevelShiftForMaxVertexIndex = -0,
	.shiftFromDefaultLevelToHide     = -1,
};

static SimulatedHullsSystem::CloudMeshProps g_fireOuterCloudMeshProps {
	.alphaScaleLifespan              = { .initial = 0.0f, .fadedIn = 0.75f, .fadedOut = 0.75f },
	.radiusLifespan                  = { .initial = 0.0f, .fadedIn = 6.0f, .fadedOut = 3.0f },
	.tessLevelShiftForMinVertexIndex = -1,
	.tessLevelShiftForMaxVertexIndex = -0,
	.shiftFromDefaultLevelToHide     = -1,
};

static SimulatedHullsSystem::AppearanceRules g_fireInnerCloudAppearanceRules = SimulatedHullsSystem::SolidAppearanceRules {};
static SimulatedHullsSystem::AppearanceRules g_fireOuterCloudAppearanceRules = SimulatedHullsSystem::SolidAppearanceRules {};

void TransientEffectsSystem::spawnExplosionHulls( const float *origin, float radius ) {
	// 250 for radius of 64
	// TODO: Make radius affect hulls
	constexpr float lightRadiusScale = 1.0f / 64.0f;
	allocLightEffect( m_lastTime, origin, vec3_origin, 0.0f, 400u, LightLifespan {
		.colorLifespan = {
			.initial  = { 1.0f, 0.9f, 0.7f },
			.fadedIn  = { 1.0f, 0.8f, 0.5f },
			.fadedOut = { 1.0f, 0.5f, 0.0f },
		},
		.radiusLifespan = {
			.fadedIn = 250.0f * radius * lightRadiusScale,
			.finishFadingInAtLifetimeFrac = 0.15f,
			.startFadingOutAtLifetimeFrac = 0.75f,
		},
	});

	SimulatedHullsSystem *const hullsSystem = &cg.simulatedHullsSystem;

	float fireHullScale;
	unsigned fireHullTimeout;
	std::span<const SimulatedHullsSystem::HullLayerParams> fireHullLayerParams;
	if( v_explosionSmoke.get() ) {
		fireHullScale       = 1.6f;
		fireHullTimeout     = 450;
		fireHullLayerParams = kFireHullParams.lightHullLayerParams;
	} else {
		fireHullScale       = 1.5f;
		fireHullTimeout     = 550;
		fireHullLayerParams = kFireHullParams.darkHullLayerParams;
	}

	m_explosionCompoundMeshCounter++;
	if( !m_explosionCompoundMeshCounter ) [[unlikely]] {
		m_explosionCompoundMeshCounter++;
	}
	const unsigned compoundMeshKey = m_explosionCompoundMeshCounter;

	if( auto *const hull = hullsSystem->allocFireHull( m_lastTime, fireHullTimeout ) ) {
		hullsSystem->setupHullVertices( hull, origin, fireHullScale, fireHullLayerParams );
		hull->compoundMeshKey = compoundMeshKey;

		hull->vertexViewDotFade          = SimulatedHullsSystem::ViewDotFade::FadeOutContour;
		hull->layers[0].overrideHullFade = SimulatedHullsSystem::ViewDotFade::NoFade;
		hull->layers[1].overrideHullFade = SimulatedHullsSystem::ViewDotFade::NoFade;

		// We have to update material references due to invalidation upon restarts
		g_fireInnerCloudMeshProps.material = cgs.media.shaderFireHullParticle;
		g_fireOuterCloudMeshProps.material = cgs.media.shaderFireHullParticle;

		g_fireInnerCloudAppearanceRules = SimulatedHullsSystem::SolidAndCloudAppearanceRules {
			.cloudRules = SimulatedHullsSystem::CloudAppearanceRules {
				.spanOfMeshProps = { &g_fireInnerCloudMeshProps, 1 },
			}
		};

		g_fireOuterCloudAppearanceRules = SimulatedHullsSystem::SolidAndCloudAppearanceRules {
			.cloudRules = SimulatedHullsSystem::CloudAppearanceRules {
				.spanOfMeshProps = { &g_fireOuterCloudMeshProps, 1 },
			}
		};

		hull->layers[0].overrideAppearanceRules                   = &g_fireInnerCloudAppearanceRules;
		hull->layers[hull->numLayers - 1].overrideAppearanceRules = &g_fireOuterCloudAppearanceRules;
	}

	if( v_explosionWave.get() ) {
		const vec4_t waveColor { 1.0f, 1.0f, 1.0f, 0.05f };
		if( auto *const hull = hullsSystem->allocWaveHull( m_lastTime, 250 ) ) {
			hullsSystem->setupHullVertices( hull, origin, waveColor, 500.0f, 50.0f );
		}
	}

	if( v_explosionSmoke.get() ) {
		const std::span<const SimulatedHullsSystem::OffsetKeyframe> toonSmokeKeyframeSet = toonSmokeKeyframes.setOfKeyframes;

		const float randomFactor  = 0.4f;
		const float randomScaling = 1.0f + randomFactor * m_rng.nextFloat();

		const float toonSmokeScale   = 38.0f * randomScaling;
		const auto toonSmokeLifetime = (unsigned)( (float)toonSmokeKeyframes.kMinLifetime * randomScaling );
		if( auto *const hull = hullsSystem->allocToonSmokeHull( m_lastTime, toonSmokeLifetime ) ) {
			hullsSystem->setupHullVertices( hull, origin, toonSmokeScale,
											&toonSmokeKeyframeSet, toonSmokeKeyframes.maxOffset );
			hull->compoundMeshKey = compoundMeshKey;
		}
	}

	// Disallow clusters if smoke is enabled (they are barely noticeable and make the combined effect worse)
	if( v_explosionClusters.get() && !v_explosionSmoke.get() ) {
		unsigned numSpawnedClusters = 0;
		unsigned oldDirNum          = 0;
		while( numSpawnedClusters < 12 ) {
			const unsigned dirNum  = m_rng.nextBoundedFast( std::size( kPredefinedDirs ) );
			const float *randomDir = kPredefinedDirs[dirNum];
			// Just check against the last directory so this converges faster
			if( DotProduct( randomDir, kPredefinedDirs[oldDirNum] ) > 0.7f ) {
				continue;
			}

			oldDirNum = dirNum;
			numSpawnedClusters++;

			const auto spawnDelay = ( fireHullTimeout / 4 ) + m_rng.nextBoundedFast( fireHullTimeout / 4 );
			auto *const effect = allocDelayedEffect( m_lastTime, origin, spawnDelay, ConcentricHullSpawnRecord {
				.layerParams = ::kFireHullParams.darkClusterHullLayerParams,
				.scale       = m_rng.nextFloat( 0.25f, 0.37f ) * fireHullScale,
				.timeout     = fireHullTimeout / 3,
				.allocMethod = (ConcentricHullSpawnRecord::AllocMethod)&SimulatedHullsSystem::allocFireClusterHull,
				.vertexViewDotFade         = SimulatedHullsSystem::ViewDotFade::FadeOutContour,
				.overrideLayer0ViewDotFade = SimulatedHullsSystem::ViewDotFade::NoFade,
			});

			const float randomSpeed = m_rng.nextFloat( 160.0f, 175.0f );
			VectorScale( randomDir, randomSpeed, effect->velocity );
			effect->simulation = DelayedEffect::SimulateMovement;
		}
	}
}

void TransientEffectsSystem::spawnCartoonHitEffect( const float *origin, const float *dir, int damage, unsigned povPlayerMask ) {
	if( v_cartoonHitEffect.get() ) {
		float radius = 0.0f;
		shader_s *material = nullptr;
		if( damage > 64 ) {
			// OUCH!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit3, 24.0f );
		} else if( damage > 50 ) {
			// POW!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit, 19.0f );
		} else if( damage > 38 ) {
			// SPLITZOW!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit2, 15.0f );
		}

		if( material ) {
			// TODO:
			vec3_t localDir;
			if( !VectorLength( dir ) ) {
				// TODO: Why using view state, check how often does it happen
				VectorNegate( &getPrimaryViewState()->view.axis[AXIS_FORWARD], localDir );
			} else {
				VectorNormalize2( dir, localDir );
			}

			vec3_t spriteOrigin;
			// Move effect a bit up from player
			VectorCopy( origin, spriteOrigin );
			spriteOrigin[2] += ( playerbox_stand_maxs[2] - playerbox_stand_mins[2] ) + 1.0f;

			EntityEffect *effect = addSpriteEntityEffect( material, spriteOrigin, radius, 700u );
			effect->entity.rotation = 0.0f;
			// TODO: Add a correct sampling of random sphere points as a random generator method
			for( unsigned i = 0; i < 3; ++i ) {
				effect->velocity[i] = m_rng.nextFloat( -10.0f, +10.0f );
			}
			effect->povPlayerMask = povPlayerMask;
		}
	}
}

void TransientEffectsSystem::spawnBleedingVolumeEffect( const float *origin, const float *dir, unsigned damageLevel,
														const float *bloodColor, unsigned duration,
														float scale, unsigned povPlayerMask ) {
	if( auto *hull = cg.simulatedHullsSystem.allocWaveHull( m_lastTime, duration ) ) {
		vec3_t hullOrigin;
		constexpr float offset = -32.0f;
		VectorMA( origin, offset, dir, hullOrigin );

		float speed, speedSpreadFrac;
		bool tesselateClosestLod = true;
		SimulatedHullsSystem::ViewDotFade viewDotFade;
		// TODO: Lift the code to EffectsSystemFacade, get rid of the separate TransientEffectsSystem
		if( damageLevel == 1 ) {
			speed               = 50.0f;
			speedSpreadFrac     = 0.18f;
			tesselateClosestLod = false;
			viewDotFade         = SimulatedHullsSystem::ViewDotFade::FadeOutCenterLinear;
		} else if( damageLevel == 2 ) {
			speed           = 80.0f;
			speedSpreadFrac = 0.27f;
			viewDotFade     = SimulatedHullsSystem::ViewDotFade::FadeOutCenterLinear;
		} else if( damageLevel == 3 ) {
			speed           = 110.0f;
			speedSpreadFrac = 0.39f;
			viewDotFade     = SimulatedHullsSystem::ViewDotFade::FadeOutCenterQuadratic;
		} else {
			speed           = 140.0f;
			speedSpreadFrac = 0.50f;
			viewDotFade     = SimulatedHullsSystem::ViewDotFade::FadeOutCenterCubic;
		}

		const vec4_t hullColor { bloodColor[0], bloodColor[1], bloodColor[2], 0.5f };
		cg.simulatedHullsSystem.setupHullVertices( hull, hullOrigin, hullColor, scale * speed, scale * speedSpreadFrac * speed );
		hull->povPlayerMask       = povPlayerMask;
		hull->vertexViewDotFade   = viewDotFade;
		hull->tesselateClosestLod = tesselateClosestLod;
		hull->minFadedOutAlpha    = 0.1f;
	}
}

void TransientEffectsSystem::spawnElectroboltHitEffect( const float *origin, const float *dir, const float *decalColor,
														const float *energyColor, bool spawnDecal ) {
	spawnElectroboltLikeHitEffect( origin, dir, decalColor, energyColor, cgs.media.modElectroBoltWallHit, spawnDecal );
}

void TransientEffectsSystem::spawnInstagunHitEffect( const float *origin, const float *dir, const float *decalColor,
													 const float *energyColor, bool spawnDecal ) {
	spawnElectroboltLikeHitEffect( origin, dir, decalColor, energyColor, cgs.media.modInstagunWallHit, spawnDecal );
}

void TransientEffectsSystem::spawnElectroboltLikeHitEffect( const float *origin, const float *dir, 
															const float *decalColor, const float *energyColor, 
															model_s *model, bool spawnDecal ) {
	if( spawnDecal ) {
		EntityEffect *entityEffect = addModelEntityEffect( model, origin, dir, 600u );
		VectorScale( decalColor, 255.0f, entityEffect->entity.shaderRGBA );
	}

	allocLightEffect( m_lastTime, origin, dir, 4.0f, 250u, LightLifespan {
		.colorLifespan = {
			.initial  = { 1.0f, 1.0f, 1.0f },
			.fadedIn  = { energyColor[0], energyColor[1], energyColor[2] },
			.fadedOut = { energyColor[0], energyColor[1], energyColor[2] },
			.finishFadingInAtLifetimeFrac = 0.10f,
		},
		.radiusLifespan = {
			.fadedIn = 144.0f,
			.finishFadingInAtLifetimeFrac = 0.10f,
		},
	});

	if( v_explosionWave.get() ) {
		if( auto *hull = cg.simulatedHullsSystem.allocWaveHull( m_lastTime, 200 ) ) {
			const vec4_t hullColor { energyColor[0], energyColor[1], energyColor[2], 0.075f };
			cg.simulatedHullsSystem.setupHullVertices( hull, origin, hullColor, 750.0f, 100.0f );
		}
		if( auto *hull = cg.simulatedHullsSystem.allocWaveHull( m_lastTime, 200 ) ) {
			const vec4_t hullColor { energyColor[0], energyColor[1], energyColor[2], 0.1f };
			cg.simulatedHullsSystem.setupHullVertices( hull, origin, hullColor, 125.0f, 50.0f );
		}
	}
}

void TransientEffectsSystem::spawnPlasmaImpactEffect( const float *origin, const float *dir ) {
	EntityEffect *const entityEffect = addModelEntityEffect( cgs.media.modPlasmaExplosion, origin, dir, 300u );
	entityEffect->scaleLifespan = {
		.initial                      = 0.0f,
		.fadedIn                      = 2.5f,
		.fadedOut                     = 2.5f,
		.finishFadingInAtLifetimeFrac = 0.1f,
	};

	allocLightEffect( m_lastTime, origin, dir, 4.0f, 200, LightLifespan {
		.colorLifespan = {
			.initial  = { 1.0f, 1.0f, 1.0f },
			.fadedIn  = { 0.0f, 1.0f, 0.3f },
			.fadedOut = { 0.0f, 0.7f, 0.0f },
		},
		.radiusLifespan = { .fadedIn = 96.0f, },
	});

	if( v_explosionWave.get() ) {
		if( auto *hull = cg.simulatedHullsSystem.allocWaveHull( m_lastTime, 175 ) ) {
			const vec4_t hullColor { colorGreen[0], colorGreen[1], colorGreen[2], 0.05f };
			cg.simulatedHullsSystem.setupHullVertices( hull, origin, hullColor, 300.0f, 75.0f );
		}
	}
}

static const byte_vec4_t kBlastHullLayer0ReplacementPalette[] {
	asByteColor( 0.7f, 0.7f, 0.5f, 0.1f ),
	asByteColor( 0.7f, 0.7f, 0.4f, 0.1f ),
	asByteColor( 0.7f, 0.7f, 0.4f, 0.1f ),
};

static const byte_vec4_t kBlastHullLayer1ReplacementPalette[] {
	asByteColor( 0.7f, 0.7f, 0.5f, 0.1f ),
	asByteColor( 0.7f, 0.7f, 0.4f, 0.1f ),
	asByteColor( 0.7f, 0.6f, 0.4f, 0.1f ),
};

static const byte_vec4_t kBlastHullLayer2ReplacementPalette[] {
	asByteColor( 0.7f, 0.7f, 0.3f, 0.1f ),
	asByteColor( 0.7f, 0.7f, 0.4f, 0.1f ),
	asByteColor( 0.7f, 0.5f, 0.4f, 0.1f ),
};

static const byte_vec4_t kBlastHullDecayPalette[] {
	asByteColor( 0.7f, 0.7f, 0.4f, 0.05f ),
	asByteColor( 0.7f, 0.3f, 0.2f, 0.05f ),
	asByteColor( 0.7f, 0.4f, 0.1f, 0.05f )
};

static const SimulatedHullsSystem::ColorChangeTimelineNode kBlastHullLayer0ColorChangeTimeline[3] {
	{
		.sumOfDropChanceForThisSegment = 0.1f,
	},
	{
		.activateAtLifetimeFraction = 0.33f, .replacementPalette = kBlastHullLayer0ReplacementPalette,
		.sumOfDropChanceForThisSegment = 0.1f, .sumOfReplacementChanceForThisSegment = 0.3f,
	},
	{
		.activateAtLifetimeFraction = 0.67f, .replacementPalette = kBlastHullDecayPalette,
		.sumOfDropChanceForThisSegment = 1.5f, .sumOfReplacementChanceForThisSegment = 1.0f,
	}
};

static const SimulatedHullsSystem::ColorChangeTimelineNode kBlastHullLayer1ColorChangeTimeline[3] {
	{
	},
	{
		.activateAtLifetimeFraction = 0.33f, .replacementPalette = kBlastHullLayer1ReplacementPalette,
		.sumOfDropChanceForThisSegment = 0.1f, .sumOfReplacementChanceForThisSegment = 0.5f,
	},
	{
		.activateAtLifetimeFraction = 0.67f, .replacementPalette = kBlastHullDecayPalette,
		.sumOfDropChanceForThisSegment = 1.5f, .sumOfReplacementChanceForThisSegment = 1.0f,
	}
};

static const SimulatedHullsSystem::ColorChangeTimelineNode kBlastHullLayer2ColorChangeTimeline[3] {
	{
	},
	{
		.activateAtLifetimeFraction = 0.33f, .replacementPalette = kBlastHullLayer2ReplacementPalette,
		.sumOfDropChanceForThisSegment = 0.1f, .sumOfReplacementChanceForThisSegment = 0.7f,
	},
	{
		.activateAtLifetimeFraction = 0.67f, .replacementPalette = kBlastHullDecayPalette,
		.sumOfDropChanceForThisSegment = 1.5f, .sumOfReplacementChanceForThisSegment = 1.0f,
	}
};

static const SimulatedHullsSystem::HullLayerParams kBlastHullLayerParams[3] {
	{
		.speed = 30.0f, .finalOffset = 5.0f,
		.speedSpikeChance = 0.10f, .minSpeedSpike = 5.0f, .maxSpeedSpike = 20.0f,
		.biasAlongChosenDir = 15.0f,
		.baseInitialColor = { 0.9f, 1.0f, 0.6f, 1.0f },
		.bulgeInitialColor = { 0.9f, 1.0f, 1.0f, 1.0f },
		.colorChangeTimeline = kBlastHullLayer0ColorChangeTimeline
	},
	{
		.speed = 40.0f, .finalOffset = 2.5f,
		.speedSpikeChance = 0.08f, .minSpeedSpike = 5.0f, .maxSpeedSpike = 20.0f,
		.biasAlongChosenDir = 15.0f,
		.baseInitialColor = { 1.0f, 0.7f, 0.4f, 1.0f },
		.bulgeInitialColor = { 1.0f, 0.8f, 0.5f, 1.0f },
		.colorChangeTimeline = kBlastHullLayer1ColorChangeTimeline
	},
	{
		.speed = 50.0f, .finalOffset = 0.0f,
		.speedSpikeChance = 0.08f, .minSpeedSpike = 5.0f, .maxSpeedSpike = 20.0f,
		.biasAlongChosenDir = 15.0f,
		.baseInitialColor = { 0.7, 0.6f, 0.4f, 0.7f },
		.bulgeInitialColor = { 1.0f, 0.7f, 0.4f, 0.7f },
		.colorChangeTimeline = kBlastHullLayer2ColorChangeTimeline
	},
};

static SimulatedHullsSystem::CloudMeshProps g_blastHullCloudMeshProps {
	.alphaScaleLifespan          = { .initial = 0.0f, .fadedIn = 0.1f, .fadedOut = 0.1f },
	.radiusLifespan              = { .initial = 0.0f, .fadedIn = 4.0f, .fadedOut = 1.0f },
	.shiftFromDefaultLevelToHide = -1,
};

void TransientEffectsSystem::spawnGunbladeBlastImpactEffect( const float *origin, const float *dir ) {
	allocLightEffect( m_lastTime, origin, dir, 8.0f, 350u, LightLifespan {
		.colorLifespan = {
			.initial  = { 1.0f, 1.0f, 0.5f },
			.fadedIn  = { 1.0f, 0.8f, 0.3f },
			.fadedOut = { 0.5f, 0.7f, 0.3f },
		},
		.radiusLifespan = { .fadedIn = 144.0f, },
	});

	const vec3_t hullOrigin { origin[0] + 8.0f * dir[0], origin[1] + 8.0f * dir[1], origin[2] + 8.0f * dir[2] };

	if( auto *hull = cg.simulatedHullsSystem.allocBlastHull( m_lastTime, 450 ) ) {
		cg.simulatedHullsSystem.setupHullVertices( hull, hullOrigin, 1.25f, kBlastHullLayerParams );
		hull->vertexViewDotFade          = SimulatedHullsSystem::ViewDotFade::FadeOutContour;
		hull->layers[0].overrideHullFade = SimulatedHullsSystem::ViewDotFade::NoFade;

		g_blastHullCloudMeshProps.material = cgs.media.shaderBlastHullParticle;

		SimulatedHullsSystem::CloudAppearanceRules cloudRulesMsvcWorkaround {
			.spanOfMeshProps = { &g_blastHullCloudMeshProps, 1 },
		};

		hull->appearanceRules = SimulatedHullsSystem::SolidAndCloudAppearanceRules {
			.cloudRules = cloudRulesMsvcWorkaround,
		};
	}

	if( v_explosionWave.get() ) {
		if( auto *hull = cg.simulatedHullsSystem.allocWaveHull( m_lastTime, 200 ) ) {
			const vec4_t waveHullColor { 1.0f, 0.9f, 0.6f, 0.05f };
			cg.simulatedHullsSystem.setupHullVertices( hull, hullOrigin, waveHullColor, 300.0f, 30.0f );
		}
	}
}

void TransientEffectsSystem::spawnGunbladeBladeImpactEffect( const float *origin, const float *dir ) {
	(void)addModelEntityEffect( cgs.media.modBladeWallHit, origin, dir, 300u );
	// TODO: Add light when hitting metallic surfaces?
}

void TransientEffectsSystem::spawnBulletImpactModel( const float *origin, const float *dir ) {
	EntityEffect *effect = addModelEntityEffect( cgs.media.modBladeWallExplo, origin, dir, 33u );
	effect->scaleLifespan = {
		.initial                      = 0.0f,
		.fadedIn                      = 0.3f,
		.fadedOut                     = 0.3f,
		.finishFadingInAtLifetimeFrac = 0.1f,
		.startFadingOutAtLifetimeFrac = 0.3f,
	};
}

void TransientEffectsSystem::spawnPelletImpactModel( const float *origin, const float *dir ) {
	EntityEffect *effect = addModelEntityEffect( cgs.media.modBladeWallExplo, origin, dir, 108u );
	effect->scaleLifespan = {
		.initial                      = 0.0f,
		.fadedIn                      = 0.3f,
		.fadedOut                     = 0.2f,
		.finishFadingInAtLifetimeFrac = 0.05f,
		.startFadingOutAtLifetimeFrac = 0.35f,
	};
}

void TransientEffectsSystem::addDelayedParticleEffect( unsigned delay, ParticleFlockBin bin,
													   const ConicalFlockParams &flockParams,
													   const Particle::AppearanceRules &appearanceRules,
													   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
													   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	allocDelayedEffect( m_lastTime, flockParams.origin, delay, ConicalFlockSpawnRecord {
		.flockParams = flockParams, .appearanceRules = appearanceRules, .bin = bin,
		.paramsOfParticleTrail = paramsOfParticleTrail ? std::optional( *paramsOfParticleTrail ) : std::nullopt,
		.paramsOfPolyTrail     = paramsOfPolyTrail ? std::optional( *paramsOfPolyTrail ) : std::nullopt,
	});
}

void TransientEffectsSystem::addDelayedParticleEffect( unsigned delay, ParticleFlockBin bin,
													   const EllipsoidalFlockParams &flockParams,
													   const Particle::AppearanceRules &appearanceRules,
													   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail,
													   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail ) {
	allocDelayedEffect( m_lastTime, flockParams.origin, delay, EllipsoidalFlockSpawnRecord {
		.flockParams = flockParams, .appearanceRules = appearanceRules, .bin = bin,
		.paramsOfParticleTrail = paramsOfParticleTrail ? std::optional( *paramsOfParticleTrail ) : std::nullopt,
		.paramsOfPolyTrail     = paramsOfPolyTrail ? std::optional( *paramsOfPolyTrail ) : std::nullopt,
	});
}

void TransientEffectsSystem::addDelayedImpactRosetteEffect( unsigned delay,
															const PolyEffectsSystem::ImpactRosetteParams &params ) {
	allocDelayedEffect( m_lastTime, vec3_origin, delay, ImpactRosetteSpawnRecord { .params = params } );
}

void TransientEffectsSystem::spawnDustImpactEffect( const float *origin, const float *dir, float radius ) {
	vec3_t axis1, axis2;
	PerpendicularVector( axis2, dir );
	CrossProduct( dir, axis2, axis1 );

	VectorNormalize( axis1 ), VectorNormalize( axis2 );

	float angle = 0.0f;
	constexpr const int count = 12;
	const float speed = 0.67f * radius;
	const float angleStep = (float)M_TWOPI * Q_Rcp( (float)count );
	for( int i = 0; i < count; ++i ) {
		const float scale1 = std::sin( angle ), scale2 = std::cos( angle );
		angle += angleStep;

		vec3_t velocity { 0.0f, 0.0f, 0.0f };
		VectorMA( velocity, speed * scale1, axis1, velocity );
		VectorMA( velocity, speed * scale2, axis2, velocity );

		EntityEffect *effect  = addSpriteEntityEffect( cgs.media.shaderSmokePuff2, origin, 10.0f, 700u );
		effect->alphaLifespan = { .initial  = 0.25f, .fadedIn  = 0.25f, .fadedOut = 0.0f };
		effect->scaleLifespan = { .initial  = 0.0f, .fadedIn  = 0.33f, .fadedOut = 0.0f };
		VectorCopy( velocity, effect->velocity );
	}
}

void TransientEffectsSystem::spawnDashEffect( const float *origin, const float *dir ) {
	// Model orientation/streching hacks
	vec3_t angles;
	VecToAngles( dir, angles );
	angles[1] += 270.0f;
	EntityEffect *effect = addModelEntityEffect( cgs.media.modDash, origin, dir, 700u );
	AnglesToAxis( angles, effect->entity.axis );
	// Scale Z
	effect->entity.axis[2 * 3 + 2] *= 2.0f;
	effect->scaleLifespan = {
		.initial                      = 0.0f,
		.fadedIn                      = 0.15f,
		.fadedOut                     = 0.15f,
		.finishFadingInAtLifetimeFrac = 0.12f,
	};
}

auto TransientEffectsSystem::addModelEntityEffect( model_s *model, const float *origin, const float *dir,
												   unsigned duration ) -> EntityEffect * {
	EntityEffect *const effect = allocEntityEffect( m_lastTime, duration );

	std::memset( &effect->entity, 0, sizeof( entity_s ) );
	effect->entity.rtype = RT_MODEL;
	effect->entity.renderfx = RF_NOSHADOW;
	effect->entity.model = model;
	effect->entity.customShader = nullptr;
	effect->entity.shaderTime = m_lastTime;
	effect->entity.scale = 0.0f;
	effect->entity.rotation = (float)m_rng.nextBounded( 360 );

	VectorSet( effect->entity.shaderRGBA, 255, 255, 255 );

	NormalVectorToAxis( dir, &effect->entity.axis[0] );
	VectorCopy( origin, effect->entity.origin );

	return effect;
}

auto TransientEffectsSystem::addSpriteEntityEffect( shader_s *material, const float *origin, float radius,
													unsigned duration ) -> EntityEffect * {
	EntityEffect *const effect = allocEntityEffect( m_lastTime, duration );

	std::memset( &effect->entity, 0, sizeof( entity_s ) );
	effect->entity.rtype = RT_SPRITE;
	effect->entity.renderfx = RF_NOSHADOW;
	effect->entity.radius = radius;
	effect->entity.customShader = material;
	effect->entity.shaderTime = m_lastTime;
	effect->entity.scale = 0.0f;
	effect->entity.rotation = (float)m_rng.nextBounded( 360 );

	VectorSet( effect->entity.shaderRGBA, 255, 255, 255 );

	Matrix3_Identity( effect->entity.axis );
	VectorCopy( origin, effect->entity.origin );

	return effect;
}

auto TransientEffectsSystem::allocEntityEffect( int64_t currTime, unsigned duration ) -> EntityEffect * {
	void *mem = m_entityEffectsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		// TODO: Prioritize effects so unimportant ones get evicted first
		EntityEffect *oldestEffect = nullptr;
		// TODO: Choose by nearest timeout/lifetime fraction?
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( EntityEffect *effect = m_entityEffectsHead; effect; effect = effect->next ) {
			if( oldestSpawnTime > effect->spawnTime ) {
				oldestSpawnTime = effect->spawnTime;
				oldestEffect = effect;
			}
		}
		assert( oldestEffect );
		wsw::unlink( oldestEffect, &m_entityEffectsHead );
		oldestEffect->~EntityEffect();
		mem = oldestEffect;
	}

	assert( duration >= 16 && duration <= std::numeric_limits<uint16_t>::max() );

	auto *effect = new( mem )EntityEffect;
	effect->duration  = duration;
	effect->spawnTime = currTime;

	wsw::link( effect, &m_entityEffectsHead );
	return effect;
}

// TODO: Generalize!!!
auto TransientEffectsSystem::allocPolyEffect( int64_t currTime, unsigned duration ) -> PolyEffect * {
	void *mem = m_polyEffectsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		// TODO: Prioritize effects so unimportant ones get evicted first
		PolyEffect *oldestEffect = nullptr;
		// TODO: Choose by nearest timeout/lifetime fraction?
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( PolyEffect *effect = m_polyEffectsHead; effect; effect = effect->next ) {
			if( oldestSpawnTime > effect->spawnTime ) {
				oldestSpawnTime = effect->spawnTime;
				oldestEffect = effect;
			}
		}
		assert( oldestEffect );
		wsw::unlink( oldestEffect, &m_polyEffectsHead );
		oldestEffect->~PolyEffect();
		mem = oldestEffect;
	}

	auto *effect = new( mem )PolyEffect;
	effect->duration  = duration;
	effect->spawnTime = currTime;

	wsw::link( effect, &m_polyEffectsHead );
	return effect;
}

auto TransientEffectsSystem::allocLightEffect( int64_t currTime, const float *origin, const float *offset,
											   float offsetScale, unsigned duration,
											   LightLifespan &&lightLifespan ) -> LightEffect * {
	void *mem = m_lightEffectsAllocator.allocOrNull();
	// TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Generalize
	if( !mem ) [[unlikely]] {
		// TODO: Prioritize effects so unimportant ones get evicted first
		LightEffect *oldestEffect = nullptr;
		// TODO: Choose by nearest timeout/lifetime fraction?
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( LightEffect *effect = m_lightEffectsHead; effect; effect = effect->next ) {
			if( oldestSpawnTime > effect->spawnTime ) {
				oldestSpawnTime = effect->spawnTime;
				oldestEffect = effect;
			}
		}
		assert( oldestEffect );
		wsw::unlink( oldestEffect, &m_lightEffectsHead );
		oldestEffect->~LightEffect();
		mem = oldestEffect;
	}

	auto *effect          = new( mem )LightEffect;
	effect->duration      = duration;
	effect->spawnTime     = currTime;
	effect->lightLifespan = std::forward<LightLifespan>( lightLifespan );
	VectorMA( origin, offsetScale, offset, effect->origin );

	wsw::link( effect, &m_lightEffectsHead );
	return effect;
}

auto TransientEffectsSystem::allocDelayedEffect( int64_t currTime, const float *origin, unsigned delay,
												 DelayedEffect::SpawnRecord &&spawnRecord ) -> DelayedEffect * {
	void *mem = m_delayedEffectsAllocator.allocOrNull();
	// TODO!!!!!!!!!!!!
	if( !mem ) {
		DelayedEffect *oldestEffect = nullptr;
		// TODO: Choose by nearest timeout/lifetime fraction?
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( DelayedEffect *effect = m_delayedEffectsHead; effect; effect = effect->next ) {
			if( oldestSpawnTime > effect->spawnTime ) {
				oldestSpawnTime = effect->spawnTime;
				oldestEffect = effect;
			}
		}
		assert( oldestEffect );
		wsw::unlink( oldestEffect, &m_delayedEffectsHead );
		oldestEffect->~DelayedEffect();
		mem = oldestEffect;
	}

	auto *effect = new( mem )DelayedEffect { .spawnRecord = std::move_if_noexcept( spawnRecord ) };
	effect->spawnTime  = currTime;
	effect->spawnDelay = delay;
	VectorCopy( origin, effect->origin );

	wsw::link( effect, &m_delayedEffectsHead );
	return effect;
}

void TransientEffectsSystem::unlinkAndFreeEntityEffect( EntityEffect *effect ) {
	wsw::unlink( effect, &m_entityEffectsHead );
	effect->~EntityEffect();
	m_entityEffectsAllocator.free( effect );
}

void TransientEffectsSystem::unlinkAndFreePolyEffect( PolyEffect *effect ) {
	wsw::unlink( effect, &m_polyEffectsHead );
	effect->~PolyEffect();
	m_polyEffectsAllocator.free( effect );
}

void TransientEffectsSystem::unlinkAndFreeLightEffect( LightEffect *effect ) {
	wsw::unlink( effect, &m_lightEffectsHead );
	effect->~LightEffect();
	m_lightEffectsAllocator.free( effect );
}

void TransientEffectsSystem::unlinkAndFreeDelayedEffect( DelayedEffect *effect ) {
	wsw::unlink( effect, &m_delayedEffectsHead );
	effect->~DelayedEffect();
	m_delayedEffectsAllocator.free( effect );
}

void TransientEffectsSystem::simulateFrame( int64_t currTime ) {
	WSW_PROFILER_SCOPE();

	if( currTime != m_lastTime ) {
		assert( currTime > m_lastTime );

		// Can't be computed in a constructor due to initialization order issues
		// TODO: This should get changed once we get rid of most globals
		if( m_cachedSmokeBulgeMasksBuffer.empty() ) [[unlikely]] {
			float floatMaskBuffer[kCachedSmokeBulgeMaskSize];
			std::fill( floatMaskBuffer + kCachedSmokeBulgeMaskSize, floatMaskBuffer + kCachedSmokeBulgeMaskSize, 0.0f );

			m_cachedSmokeBulgeMasksBuffer.reserve( kCachedSmokeBulgeMaskSize * kNumCachedSmokeBulgeMasks );

			for ( unsigned i = 0; i < kNumCachedSmokeBulgeMasks; ++i ) {
				cg.simulatedHullsSystem.calcSmokeBulgeSpeedMask( floatMaskBuffer, kCachedSmokeBulgeSubdivLevel, 7 );
				for( unsigned vertexNum = 0; vertexNum < kCachedSmokeBulgeMaskSize; ++vertexNum ) {
					m_cachedSmokeBulgeMasksBuffer.push_back( (uint8_t)( 255.0f * floatMaskBuffer[vertexNum] ) );
				}
			}

			m_cachedSmokeBulgeMasksBuffer.shrink_to_fit();
		}

		// Limit the time step
		const float timeDeltaSeconds = 1e-3f * (float)wsw::min<int64_t>( 33, currTime - m_lastTime );

		simulateDelayedEffects( currTime, timeDeltaSeconds );
		simulateEntityEffects( currTime, timeDeltaSeconds );
		simulatePolyEffects( currTime, timeDeltaSeconds );
		simulateLightEffects( currTime, timeDeltaSeconds );

		m_lastTime = currTime;
	}
}

void TransientEffectsSystem::submitToScene( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask ) {
	submitEntityEffects( currTime, request, povPlayerMask );
	submitPolyEffects( currTime, request );
	submitLightEffects( currTime, request );
}

void TransientEffectsSystem::simulateEntityEffects( int64_t currTime, float timeDeltaSeconds ) {
	const model_s *const dashModel = cgs.media.modDash;

	for( EntityEffect *__restrict effect = m_entityEffectsHead, *next; effect; effect = next ) { next = effect->next;
		bool doneWithThisEffect = false;
		if( effect->spawnTime + effect->duration <= currTime ) [[unlikely]] {
			unlinkAndFreeEntityEffect( effect );
			doneWithThisEffect = true;
		}
		if( !doneWithThisEffect ) [[likely]] {
			// Dash model hacks
			if( effect->entity.model == dashModel ) [[unlikely]] {
				float *const zScale = effect->entity.axis + ( 2 * 3 ) + 2;
				*zScale -= 4.0f * timeDeltaSeconds;
				if( *zScale < 0.01f ) {
					unlinkAndFreeEntityEffect( effect );
					doneWithThisEffect = true;
				}
			}
			if( !doneWithThisEffect ) [[likely]] {
				vec3_t moveVec;
				VectorScale( effect->velocity, timeDeltaSeconds, moveVec );
				VectorAdd( effect->entity.origin, moveVec, effect->entity.origin );
			}
		}
	}
}

void TransientEffectsSystem::submitEntityEffects( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask ) {
	// TODO: Accept as an argument?
	const float backlerp = 1.0f - cg.lerpfrac;

	for( EntityEffect *__restrict effect = m_entityEffectsHead; effect; effect = effect->next ) {
		if( effect->povPlayerMask & povPlayerMask ) {
			const auto lifetimeMillis = (unsigned)( currTime - effect->spawnTime );
			assert( lifetimeMillis < effect->duration );
			const float lifetimeFrac = (float)lifetimeMillis * Q_Rcp( (float)effect->duration );

			effect->entity.backlerp      = backlerp;
			effect->entity.scale         = effect->scaleLifespan.getValueForLifetimeFrac( lifetimeFrac );
			effect->entity.shaderRGBA[3] = (uint8_t)( 255 * effect->alphaLifespan.getValueForLifetimeFrac( lifetimeFrac ) );
			effect->entity.shaderTime    = currTime;

			request->addEntity( &effect->entity );
		}
	}
}

void TransientEffectsSystem::simulatePolyEffects( int64_t currTime, float timeDeltaSeconds ) {
	for( PolyEffect *__restrict effect = m_polyEffectsHead, *next; effect; effect = next ) { next = effect->next;
		if( effect->spawnTime + effect->duration <= currTime ) [[unlikely]] {
			unlinkAndFreePolyEffect( effect );
		} else {
			vec3_t moveVec;
			VectorScale( effect->velocity, timeDeltaSeconds, moveVec );
			VectorAdd( effect->poly.origin, moveVec, effect->poly.origin );
		}
	}
}

void TransientEffectsSystem::submitPolyEffects( int64_t currTime, DrawSceneRequest *request ) {
	for( PolyEffect *__restrict effect = m_polyEffectsHead; effect; effect = effect->next ) {
		const auto lifetimeMillis = (unsigned)( currTime - effect->spawnTime );
		assert( lifetimeMillis < effect->duration );
		const float lifetimeFrac = (float)lifetimeMillis * Q_Rcp( (float)effect->duration );

		effect->poly.halfExtent = effect->scaleLifespan.getValueForLifetimeFrac( lifetimeFrac ) * effect->scaleMultiplier;
		effect->poly.animFrac   = lifetimeFrac;

		const float colorAlpha = effect->alphaLifespan.getValueForLifetimeFrac( lifetimeFrac );
		// std::variant<> interface is awful
		if( auto *beamRules = std::get_if<QuadPoly::ViewAlignedBeamRules>( &effect->poly.appearanceRules ) ) {
			beamRules->fromColor[3] = colorAlpha;
			beamRules->toColor[3]   = colorAlpha;
		} else if( auto *spriteRules = std::get_if<QuadPoly::ViewAlignedSpriteRules>( &effect->poly.appearanceRules ) ) {
			spriteRules->color[3] = colorAlpha;
		} else if( auto *orientedRules = std::get_if<QuadPoly::OrientedSpriteRules>( &effect->poly.appearanceRules ) ) {
			orientedRules->color[3] = colorAlpha;
		}

		request->addPoly( &effect->poly );
	}
}

void TransientEffectsSystem::simulateLightEffects( int64_t currTime, float timeDeltaSeconds ) {
	for( LightEffect *effect = m_lightEffectsHead, *next; effect; effect = next ) { next = effect->next;
		if( effect->spawnTime + effect->duration <= currTime ) [[unlikely]] {
			unlinkAndFreeLightEffect( effect );
		}
	}
}

void TransientEffectsSystem::submitLightEffects( int64_t currTime, DrawSceneRequest *request ) {
	for( LightEffect *effect = m_lightEffectsHead; effect; effect = effect->next ) {
		const float lifetimeFrac = (float)( currTime - effect->spawnTime ) * Q_Rcp( (float)effect->duration );

		float radius, color[3];
		effect->lightLifespan.getRadiusAndColorForLifetimeFrac( lifetimeFrac, &radius, color );

		if( radius >= 1.0f ) [[likely]] {
			request->addLight( effect->origin, radius, 0.0f, color );
		}
	}

	// TODO: Add and use a bulk submission of lights
}

void TransientEffectsSystem::simulateDelayedEffects( int64_t currTime, float timeDeltaSeconds ) {
	for( DelayedEffect *effect = m_delayedEffectsHead, *next; effect; effect = next ) { next = effect->next;
		bool isSpawningPossible = true;
		if( effect->simulation == DelayedEffect::SimulateMovement ) {
			// TODO: Normalize angles each step?
			VectorMA( effect->angles, timeDeltaSeconds, effect->angularVelocity, effect->angles );

			vec3_t idealOrigin;
			VectorMA( effect->origin, timeDeltaSeconds, effect->velocity, idealOrigin );
			idealOrigin[2] += 0.5f * effect->gravity * timeDeltaSeconds * timeDeltaSeconds;

			effect->velocity[2] += effect->gravity * timeDeltaSeconds;

			trace_t trace;
			CM_TransformedBoxTrace( cl.cms, &trace, effect->origin, idealOrigin, vec3_origin, vec3_origin,
									nullptr, MASK_SOLID | MASK_WATER, nullptr, nullptr, 0 );

			if( trace.fraction == 1.0f ) {
				VectorCopy( idealOrigin, effect->origin );
			} else if ( !( trace.startsolid | trace.allsolid ) ) {
				vec3_t velocityDir;
				VectorCopy( effect->velocity, velocityDir );
				if( const float squareSpeed = VectorLengthSquared( velocityDir ); squareSpeed > 1.0f ) {
					const float rcpSpeed = Q_RSqrt( squareSpeed );
					VectorScale( velocityDir, rcpSpeed, velocityDir );
					vec3_t reflectedDir;
					VectorReflect( velocityDir, trace.plane.normal, 0.0f, reflectedDir );
					addRandomRotationToDir( reflectedDir, &m_rng, 0.9f );
					const float newSpeed = effect->restitution * squareSpeed * rcpSpeed;
					VectorScale( reflectedDir, newSpeed, effect->velocity );
					// This is not correct but is sufficient
					VectorAdd( trace.endpos, trace.plane.normal, effect->origin );
				}
			}
			// Don't spawn in solid or while contacting solid
			isSpawningPossible = trace.fraction == 1.0f && !trace.startsolid;
		}

		const int64_t triggerAt = effect->spawnTime + effect->spawnDelay;
		if( triggerAt <= currTime ) {
			if( isSpawningPossible ) {
				spawnDelayedEffect( effect );
				unlinkAndFreeDelayedEffect( effect );
			} else if( triggerAt + 25 < currTime ) {
				// If the "grace" period for getting out of solid has expired
				unlinkAndFreeDelayedEffect( effect );
			}
		}
	}
}

void TransientEffectsSystem::spawnDelayedEffect( DelayedEffect *effect ) {
	struct Visitor {
		const int64_t m_lastTime;
		DelayedEffect *const m_effect;
		
		void operator()( const RegularHullSpawnRecord &record ) const {
			auto method = record.allocMethod;
			if( auto *hull = ( cg.simulatedHullsSystem.*method )( m_lastTime, record.timeout ) ) {
				cg.simulatedHullsSystem.setupHullVertices( hull, m_effect->origin, record.color,
														   record.speed, record.speedSpread );
				hull->colorChangeTimeline = record.colorChangeTimeline;
				hull->tesselateClosestLod = record.tesselateClosestLod;
				hull->leprNextLevelColors = record.lerpNextLevelColors;
				hull->applyVertexDynLight = record.applyVertexDynLight;
				hull->vertexViewDotFade   = record.vertexViewDotFade;
				hull->vertexZFade         = record.vertexZFade;
			}
		}
		void operator()( const ConcentricHullSpawnRecord &record ) const {
			auto method = record.allocMethod;
			if( auto *hull = ( cg.simulatedHullsSystem.*method )( m_lastTime, record.timeout ) ) {
				cg.simulatedHullsSystem.setupHullVertices( hull, m_effect->origin, record.scale,
														   record.layerParams );
				hull->vertexViewDotFade          = record.vertexViewDotFade;
				hull->layers[0].overrideHullFade = record.overrideLayer0ViewDotFade;
			}
		}
		void operator()( const ConicalFlockSpawnRecord &record ) const {
			ConicalFlockParams modifiedFlockParams;
			const ConicalFlockParams *flockParams   = &record.flockParams;
			const Particle::AppearanceRules &arules = record.appearanceRules;
			if( m_effect->simulation != TransientEffectsSystem::DelayedEffect::NoSimulation ) {
				modifiedFlockParams = record.flockParams;
				VectorCopy( m_effect->origin, modifiedFlockParams.origin );
				VectorClear( modifiedFlockParams.offset );
				AngleVectors( m_effect->angles, modifiedFlockParams.dir, nullptr, nullptr );
				flockParams = &modifiedFlockParams;
			}
			const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr;
			if( record.paramsOfParticleTrail ) {
				paramsOfParticleTrail = std::addressof( *record.paramsOfParticleTrail );
			}
			const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr;
			if( record.paramsOfPolyTrail ) {
				paramsOfPolyTrail = std::addressof( *record.paramsOfPolyTrail );
			}
			// TODO: "using enum"
			// TODO: Get rid of user-visible bins
			using Pfb = ParticleFlockBin;
			switch( record.bin ) {
				case Pfb::Small: cg.particleSystem.addSmallParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
				case Pfb::Medium: cg.particleSystem.addMediumParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
				case Pfb::Large: cg.particleSystem.addLargeParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
			}
		}
		void operator()( const EllipsoidalFlockSpawnRecord &record ) const {
			EllipsoidalFlockParams modifiedFlockParams;
			const EllipsoidalFlockParams *flockParams = &record.flockParams;
			const Particle::AppearanceRules &arules   = record.appearanceRules;
			if( m_effect->simulation != TransientEffectsSystem::DelayedEffect::NoSimulation ) {
				modifiedFlockParams = record.flockParams;
				VectorCopy( m_effect->origin, modifiedFlockParams.origin );
				VectorClear( modifiedFlockParams.offset );
				if( modifiedFlockParams.stretchScale != 1.0f ) {
					AngleVectors( m_effect->angles, modifiedFlockParams.stretchDir, nullptr, nullptr );
				}
				flockParams = &modifiedFlockParams;
			}
			const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr;
			if( record.paramsOfParticleTrail ) {
				paramsOfParticleTrail = std::addressof( *record.paramsOfParticleTrail );
			}
			const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr;
			if( record.paramsOfPolyTrail ) {
				paramsOfPolyTrail = std::addressof( *record.paramsOfPolyTrail );
			}
			// TODO: "using enum"
			// TODO: Get rid of user-visible bins
			using Pfb = ParticleFlockBin;
			switch( record.bin ) {
				case Pfb::Small: cg.particleSystem.addSmallParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
				case Pfb::Medium: cg.particleSystem.addMediumParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
				case Pfb::Large: cg.particleSystem.addLargeParticleFlock( arules, *flockParams, paramsOfParticleTrail, paramsOfPolyTrail ); break;
			}
		}
		void operator()( const ImpactRosetteSpawnRecord &record ) const {
			cg.polyEffectsSystem.spawnImpactRosette( PolyEffectsSystem::ImpactRosetteParams { record.params } );
		}
	};

	std::visit( Visitor { .m_lastTime = m_lastTime, .m_effect = effect }, effect->spawnRecord );
}