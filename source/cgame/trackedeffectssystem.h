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

#ifndef WSW_4d42a1db_cb72_4adf_97c9_3317a0fae4b9_H
#define WSW_4d42a1db_cb72_4adf_97c9_3317a0fae4b9_H

#include "../qcommon/freelistallocator.h"
#include "../qcommon/randomgenerator.h"
#include "../gameshared/q_shared.h"
#include "particlesystem.h"
#include "polyeffectssystem.h"

class DrawSceneRequest;
struct model_s;

class TrackedEffectsSystem {
public:
	TrackedEffectsSystem() = default;
	~TrackedEffectsSystem();

	void touchRocketTrail( int entNum, const float *origin, int64_t currTime );
	void touchGrenadeTrail( int entNum, const float *origin, int64_t currTime );

	void touchBlastTrail( int entNum, const float *origin, int64_t currTime );
	void touchElectroTrail( int entNum, const float *origin, int64_t currTime );

	void touchAreaIndicator( int entNum, const float *origin, float radius, const float *color, int64_t currTime );

	void spawnPlayerTeleInEffect( int entNum, const float *origin, model_s *model ) {
		spawnPlayerTeleEffect( entNum, origin, model, 0 );
	}

	void spawnPlayerTeleOutEffect( int entNum, const float *origin, model_s *model ) {
		spawnPlayerTeleEffect( entNum, origin, model, 1 );
	}

	void resetEntityEffects( int entNum );

	void updateStraightLaserBeam( int ownerNum, const float *from, const float *to, int64_t currTime );
	void updateCurvedLaserBeam( int ownerNum, std::span<const vec3_t> points, int64_t currTime );

	void simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *drawSceneRequest );
private:
	struct ParticleTrail {
		ParticleTrail *prev { nullptr }, *next { nullptr };
		ParticleFlock *particleFlock { nullptr };
		float lastDropOrigin[3];
		int64_t touchedAt { 0 };
		int64_t lastParticleAt { 0 };
		float dropDistance { 12.0f };
		unsigned maxParticlesPerDrop { 1 };
		unsigned maxParticlesInFlock { ~0u };

		struct AttachmentIndices {
			uint16_t entNum;
			uint8_t trailNum;
		};

		std::optional<AttachmentIndices> attachmentIndices;
	};

	struct AreaIndicator {
		AreaIndicator *prev { nullptr }, *next { nullptr };
		void *hullOpaquePtr { nullptr };
		int64_t lastHullSpawnTime { 0 };
		int64_t touchedAt { 0 };
		vec3_t rgb { 1.0f, 1.0f, 1.0f };
		vec3_t origin { 0.0f, 0.0f, 0.0f };
		float radius { 128.0f };
		int entNum { std::numeric_limits<int>::max() };
	};

	struct TeleEffect {
		TeleEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		model_s *model;
		unsigned lifetime { 0 };
		float origin[3];
		float color[3];
		int clientNum { std::numeric_limits<int>::max() };
		int inOutIndex { std::numeric_limits<int>::max() };
	};

	struct AttachedClientEffects {
		TeleEffect *teleEffects[2] { nullptr, nullptr };
		PolyEffectsSystem::StraightBeam *straightLaserBeam { nullptr };
		PolyEffectsSystem::CurvedBeam *curvedLaserBeam { nullptr };
		int64_t straightLaserBeamTouchedAt { 0 }, curvedLaserBeamTouchedAt { 0 };
	};

	struct AttachedEntityEffects {
		ParticleTrail *particleTrails[2] { nullptr, nullptr };
		AreaIndicator *areaIndicator { nullptr };
	};

	void makeParticleTrailLingering( ParticleTrail *trail );

	void unlinkAndFree( ParticleTrail *particleTrail );
	void unlinkAndFree( TeleEffect *teleEffect );
	void unlinkAndFree( AreaIndicator *areaIndicator );

	[[nodiscard]]
	auto allocParticleTrail( int entNum, unsigned trailIndex,
							 const float *origin, unsigned particleSystemBin,
							 Particle::AppearanceRules &&appearanceRules  ) -> ParticleTrail *;

	void updateAttachedParticleTrail( ParticleTrail *trail, const float *origin,
									  ConeFlockParams *params, int64_t currTime );

	void spawnPlayerTeleEffect( int clientNum, const float *origin, model_s *model, int inOrOutIndex );

	static constexpr unsigned kClippedTrailsBin = ParticleSystem::kClippedTrailFlocksBin;
	static constexpr unsigned kNonClippedTrailsBin = ParticleSystem::kNonClippedTrailFlocksBin;

	ParticleTrail *m_attachedTrailsHead { nullptr };
	ParticleTrail *m_lingeringTrailsHead { nullptr };

	AreaIndicator *m_areaIndicatorsHead { nullptr };

	TeleEffect *m_teleEffectsHead { nullptr };

	wsw::HeapBasedFreelistAllocator m_particleTrailsAllocator { sizeof( ParticleTrail ), 4 * MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_teleEffectsAllocator { sizeof( TeleEffect ), 2 * MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_areaIndicatorsAllocator { sizeof( AreaIndicator ), MAX_EDICTS };

	AttachedEntityEffects m_attachedEntityEffects[MAX_EDICTS];
	AttachedClientEffects m_attachedClientEffects[MAX_CLIENTS];

	wsw::RandomGenerator m_rng;

	int64_t m_lastTime { 0 };
};

#endif