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

#ifndef WSW_c583803c_d204_40e4_8883_36cdfe1ccf20_H
#define WSW_c583803c_d204_40e4_8883_36cdfe1ccf20_H

#include <common/freelistallocator.h>
#include <common/randomgenerator.h>
#include <client/renderer/ref.h>

#include "simulatedhullssystem.h"
#include "particlesystem.h"
#include "polyeffectssystem.h"

#include <variant>

struct CMShapeList;
class DrawSceneRequest;

struct ConicalFlockParams;
struct EllipsoidalFlockParams;

/// Manages "fire-and-forget" effects that usually get spawned upon events.
class TransientEffectsSystem {
public:
	~TransientEffectsSystem();

	void spawnExplosionHulls( const float *origin, float radius = 72.0f );
	void spawnCartoonHitEffect( const float *origin, const float *dir, int damage, unsigned povPlayerMask );
	void spawnBleedingVolumeEffect( const float *origin, const float *dir, unsigned damageLevel,
									const float *bloodColor, unsigned duration, float scale, unsigned povPlayerMask );
	void spawnElectroboltHitEffect( const float *origin, const float *dir, const float *decalColor,
									const float *energyColor, bool spawnDecal );
	void spawnInstagunHitEffect( const float *origin, const float *dir, const float *decalColor,
								 const float *energyColor, bool spawnDecal );
	void spawnPlasmaImpactEffect( const float *origin, const float *dir );

	void spawnGunbladeBlastImpactEffect( const float *origin, const float *dir );
	void spawnGunbladeBladeImpactEffect( const float *origin, const float *dir );

	void spawnBulletImpactModel( const float *origin, const float *dir );

	void spawnPelletImpactModel( const float *origin, const float *dir );

	// TODO: Bins should be an implementation detail of the particle system,
	// specify absolute numbers of desired particles count!
	enum class ParticleFlockBin { Small, Medium, Large };

	// TODO: Extract generic facilities for launching delayed effects

	void addDelayedParticleEffect( unsigned delay, ParticleFlockBin bin,
								   const ConicalFlockParams &flockParams,
								   const Particle::AppearanceRules &appearanceRules,
								   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );
	void addDelayedParticleEffect( unsigned delay, ParticleFlockBin bin,
								   const EllipsoidalFlockParams &flockParams,
								   const Particle::AppearanceRules &appearanceRules,
								   const ParamsOfParticleTrailOfParticles *paramsOfParticleTrail = nullptr,
								   const ParamsOfPolyTrailOfParticles *paramsOfPolyTrail = nullptr );

	void addDelayedImpactRosetteEffect( unsigned delay, const PolyEffectsSystem::ImpactRosetteParams &params );

	void spawnDustImpactEffect( const float *origin, const float *dir, float radius );

	void spawnDashEffect( const float *origin, const float *dir );

	void clear();

	void simulateFrame( int64_t currTime );
	void submitToScene( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask );
private:
	struct EntityEffect {
		EntityEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		unsigned duration { 0 };
		unsigned povPlayerMask { ~0u };
		float velocity[3] { 0.0f, 0.0f, 0.0f };
		ValueLifespan scaleLifespan { .initial = 0.0f, .fadedIn = 1.0f, .fadedOut = 1.0f };
		ValueLifespan alphaLifespan { .initial = 1.0f, .fadedIn = 1.0f, .fadedOut = 0.0f };
		entity_t entity;
	};

	// It's almost identical to EntityEffect, so it belongs here.
	// TODO: Decouple physical simulation props and rendering rules, so we can unify these two effects?
	struct PolyEffect {
		PolyEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		unsigned duration { 0 };
		float velocity[3] { 0.0f, 0.0f, 0.0f };
		float scaleMultiplier { 1.0f };
		ValueLifespan scaleLifespan;
		ValueLifespan alphaLifespan;
		QuadPoly poly;
	};

	struct LightEffect {
		LightEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		unsigned duration { 0 };
		float origin[3] { 0.0f, 0.0f, 0.0f };

		LightLifespan lightLifespan;
	};

	struct RegularHullSpawnRecord {
		using Hull        = SimulatedHullsSystem::BaseRegularSimulatedHull;
		using AllocMethod = Hull * (SimulatedHullsSystem::*)( int64_t, unsigned );

		float scale { 1.0f };
		float speed { 250.0f };
		float speedSpread { 50.0f };
		float color[4] { 1.0f, 1.0f, 1.0f, 1.0f };
		std::span<const SimulatedHullsSystem::ColorChangeTimelineNode> colorChangeTimeline;
		unsigned timeout { 250 };
		AllocMethod allocMethod { nullptr };

		float lodCurrLevelTangentRatio { 0.25f };
		bool tesselateClosestLod { false };
		bool lerpNextLevelColors { false };
		bool applyVertexDynLight { false };
		SimulatedHullsSystem::ViewDotFade vertexViewDotFade { SimulatedHullsSystem::ViewDotFade::NoFade };
		SimulatedHullsSystem::ZFade vertexZFade { SimulatedHullsSystem::ZFade::NoFade };
	};

	struct ConcentricHullSpawnRecord {
		using LayerParams = std::span<const SimulatedHullsSystem::HullLayerParams>;
		using Hull        = SimulatedHullsSystem::BaseConcentricSimulatedHull;
		using AllocMethod = Hull * (SimulatedHullsSystem::*)( int64_t, unsigned );

		LayerParams layerParams;
		float scale { 1.0f };
		unsigned timeout { 250 };
		AllocMethod allocMethod { nullptr };

		SimulatedHullsSystem::ViewDotFade vertexViewDotFade { SimulatedHullsSystem::ViewDotFade::NoFade };
		std::optional<SimulatedHullsSystem::ViewDotFade> overrideLayer0ViewDotFade;
	};

	struct ConicalFlockSpawnRecord {
		ConicalFlockParams flockParams;
		Particle::AppearanceRules appearanceRules;
		ParticleFlockBin bin;
		// TODO: Box these items instead of wasting memory with optional?
		std::optional<ParamsOfParticleTrailOfParticles> paramsOfParticleTrail;
		std::optional<ParamsOfPolyTrailOfParticles> paramsOfPolyTrail;
	};

	struct EllipsoidalFlockSpawnRecord {
		EllipsoidalFlockParams flockParams;
		Particle::AppearanceRules appearanceRules;
		ParticleFlockBin bin;
		// TODO: Box these items instead of wasting memory with optional?
		std::optional<ParamsOfParticleTrailOfParticles> paramsOfParticleTrail;
		std::optional<ParamsOfPolyTrailOfParticles> paramsOfPolyTrail;
	};

	struct ImpactRosetteSpawnRecord {
		PolyEffectsSystem::ImpactRosetteParams params;
	};

	struct DelayedEffect {
		DelayedEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };

		unsigned spawnDelay { 250 };

		enum Simulation { NoSimulation, SimulateMovement };

		float origin[3] { 0.0f, 0.0f, 0.0f };
		float velocity[3] { 0.0f, 0.0f, 0.0f };
		float angularVelocity[3] { 0.0f, 0.0f, 0.0f };
		float angles[3] { 0.0f, 0.0f, 0.0f };
		float restitution { 1.0f };
		float gravity { 0.0f };
		Simulation simulation { NoSimulation };

		// Particles and hulls are made mutually exclusive
		// (adding extra particles to explosion clusters does not produce good visual results)
		// TODO: Add poly effects?
		using SpawnRecord = std::variant<RegularHullSpawnRecord, ConcentricHullSpawnRecord,
			ConicalFlockSpawnRecord, EllipsoidalFlockSpawnRecord, ImpactRosetteSpawnRecord>;

		SpawnRecord spawnRecord;
	};

	void unlinkAndFreeEntityEffect( EntityEffect *effect );
	void unlinkAndFreePolyEffect( PolyEffect *effect );
	void unlinkAndFreeLightEffect( LightEffect *effect );
	void unlinkAndFreeDelayedEffect( DelayedEffect *effect );

	[[nodiscard]]
	auto addModelEntityEffect( model_s *model, const float *origin, const float *dir, unsigned duration ) -> EntityEffect *;

	[[nodiscard]]
	auto addSpriteEntityEffect( shader_s *material, const float *origin, float radius, unsigned duration ) -> EntityEffect *;

	[[nodiscard]]
	auto allocEntityEffect( int64_t currTime, unsigned duration ) -> EntityEffect *;

	[[nodiscard]]
	auto allocPolyEffect( int64_t currTime, unsigned duration ) -> PolyEffect *;

	[[maybe_unused]]
	auto allocLightEffect( int64_t currTime, const float *origin, const float *offset, float offsetScale,
						   unsigned duration, LightLifespan &&lightLifespan ) -> LightEffect *;

	[[maybe_unused]]
	auto allocDelayedEffect( int64_t currTime, const float *origin, unsigned delay,
							 DelayedEffect::SpawnRecord &&spawnRecord ) -> DelayedEffect *;

	struct SmokeHullParams {
		struct { float mean, spread, maxSpike; } speed;
		struct { ValueLifespan top, bottom; } archimedesAccel;
		struct { ValueLifespan top, bottom; } xyExpansionAccel;
		SimulatedHullsSystem::ViewDotFade viewDotFade;
		SimulatedHullsSystem::ZFade zFade;
		std::span<const SimulatedHullsSystem::ColorChangeTimelineNode> colorChangeTimeline;
		SimulatedHullsSystem::AppearanceRules appearanceRules = SimulatedHullsSystem::SolidAppearanceRules {};
	};

	void spawnElectroboltLikeHitEffect( const float *origin, const float *dir, const float *decalColor,
										const float *energyColor, model_s *model, bool spawnDecal );

	void simulateEntityEffects( int64_t currTime, float timeDeltaSeconds );
	void simulatePolyEffects( int64_t currTime, float timeDeltaSeconds );
	void simulateLightEffects( int64_t currTime, float timeDeltaSeconds );
	void simulateDelayedEffects( int64_t currTime, float timeDeltaSeconds );
	void submitEntityEffects( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask );
	void submitPolyEffects( int64_t currTime, DrawSceneRequest *request );
	void submitLightEffects( int64_t currTime, DrawSceneRequest *request );

	void spawnDelayedEffect( DelayedEffect *effect );

	wsw::HeapBasedFreelistAllocator m_entityEffectsAllocator { sizeof( EntityEffect ), 128 };
	wsw::HeapBasedFreelistAllocator m_polyEffectsAllocator { sizeof( PolyEffect ), 128 };
	wsw::HeapBasedFreelistAllocator m_lightEffectsAllocator { sizeof( LightEffect ), 64 };
	wsw::HeapBasedFreelistAllocator m_delayedEffectsAllocator { sizeof( DelayedEffect ), 32 };

	EntityEffect *m_entityEffectsHead { nullptr };
	PolyEffect *m_polyEffectsHead { nullptr };
	LightEffect *m_lightEffectsHead { nullptr };
	DelayedEffect *m_delayedEffectsHead { nullptr };

	// TODO: Replace by a fixed vector/fixed buffer
	wsw::PodVector<uint8_t> m_cachedSmokeBulgeMasksBuffer;

	unsigned m_explosionCompoundMeshCounter { 0 };

	wsw::RandomGenerator m_rng;
	int64_t m_lastTime { 0 };
};

#endif
