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

#include "../common/freelistallocator.h"
#include "../common/randomgenerator.h"
#include "../common/q_shared.h"
// TODO:!!!!!!!!! Lift it to the top level!
#include "../game/ai/vec3.h"
#include "particlesystem.h"
#include "polyeffectssystem.h"

class DrawSceneRequest;
struct model_s;
struct cgs_skeleton_s;

struct CurvedPolyTrailProps;
struct StraightPolyTrailProps;

struct TeleEffectParams {
	const float *origin { nullptr };
	const float *axis { nullptr };
	model_s *model { nullptr };
	cgs_skeleton_s *skel { nullptr };
	const float *colorRgb { nullptr };
	int animFrame { 0 };
};

class TrackedEffectsSystem {
public:
	TrackedEffectsSystem() = default;
	~TrackedEffectsSystem();

	void touchStrongRocketTrail( int entNum, const float *origin, int64_t currTime ) {
		touchRocketTrail( entNum, origin, currTime, false );
	}

	void touchWeakRocketTrail( int entNum, const float *origin, int64_t currTime ) {
		touchRocketTrail( entNum, origin, currTime, true );
	}

	void touchStrongGrenadeTrail( int entNum, const float *origin, int64_t currTime ) {
		// No strong/weak difference for now
		touchGrenadeTrail( entNum, origin, currTime );
	}

	void touchWeakGrenadeTrail( int entNum, const float *origin, int64_t currTime ) {
		touchGrenadeTrail( entNum, origin, currTime );
	}

	void touchBlastTrail( int entNum, const float *origin, const float *velocity, int64_t currTime );
	void touchElectroTrail( int entNum, int ownerNum, const float *spawnOrigin, const float *currOrigin, int64_t currTime );

	void touchStrongPlasmaTrail( int entNum, const float *origin, const float *velocity, int64_t currTime );
	void touchWeakPlasmaTrail( int entNum, const float *origin, const float *velocity, int64_t currTime );

	void detachPlayerTrail( int entNum );
	void touchPlayerTrail( int entNum, const float *origin, int64_t currTime );
	void touchCorpseTrail( int entNum, const float *origin, int64_t currTime );

	// It's logical to consider teleportation effects "transient", not "tracked",
	// but we track these effects as they can be severely abused by gametype scripts.

	void spawnPlayerTeleInEffect( int entNum, int64_t currTime, const TeleEffectParams &params ) {
		spawnPlayerTeleEffect( entNum, currTime, params, 0 );
	}

	void spawnPlayerTeleOutEffect( int entNum, int64_t currTime, const TeleEffectParams &params ) {
		spawnPlayerTeleEffect( entNum, currTime, params, 1 );
	}

	void resetEntityEffects( int entNum );

	void updateStraightLaserBeam( int ownerNum, bool usePovSlot, const float *from, const float *to, int64_t currTime, unsigned povPlayerMask );
	void updateCurvedLaserBeam( int ownerNum, bool usePovSlot, std::span<const vec3_t> points, int64_t currTime, unsigned povPlayerMask );

	static void updateCurvedPolyTrail( const CurvedPolyTrailProps &props, const float *origin, int64_t currTime,
									   wsw::StaticVector<Vec3, 32> *points, wsw::StaticVector<int64_t, 32> *timestamps );

	void clear();

	void simulateFrame( int64_t currTime );
	void submitToScene( int64_t currTime, DrawSceneRequest *drawSceneRequest );
private:
	struct ParticleTrail {
		ParticleTrail *prev { nullptr }, *next { nullptr };
		ParticleFlock *particleFlock { nullptr };
		ConicalFlockParams *paramsTemplate { nullptr };
		float lastDropOrigin[3];
		int64_t touchedAt { 0 };
		float dropDistance { 12.0f };
		unsigned maxParticlesPerDrop { 1 };
		unsigned maxParticlesInFlock { ~0u };

		struct AttachmentIndices {
			uint16_t entNum;
			uint8_t trailNum;
		};

		std::optional<AttachmentIndices> attachmentIndices;
		bool linger { true };
	};

	struct StraightPolyTrail {
		int64_t touchedAt { 0 };
		StraightPolyTrail *prev { nullptr }, *next { nullptr };
		PolyEffectsSystem::StraightBeam *beam { nullptr };
		const StraightPolyTrailProps *props { nullptr };

		float initialOrigin[3];

		float lastFrom[3];
		float lastTo[3];
		float lastWidth;

		std::optional<uint16_t> attachedToEntNum;
	};

	struct CurvedPolyTrail {
		int64_t touchedAt { 0 };
		CurvedPolyTrail *prev { nullptr }, *next { nullptr };
		PolyEffectsSystem::CurvedBeam *beam { nullptr };
		const CurvedPolyTrailProps *props { nullptr };

		wsw::StaticVector<Vec3, 32> points;
		wsw::StaticVector<int64_t, 32> timestamps;
		std::span<const vec3_t> lastPointsSpan;

		std::optional<uint16_t> attachedToEntNum;
	};

	struct TraceEffectParams {
		shader_s *material { nullptr };
		float fromColor[4] { 1.0f, 1.0f, 1.0f, 1.0f };
		float toColor[4] { 1.0f, 1.0f, 1.0f, 1.0f };
		float width { 16.0f };
		float tileLength { 0.0f };
		float minDisplayedLength { 384.0f };
		unsigned timeout { 500 };
		unsigned numPlanes { 1 };
	};

	struct TraceEffect {
		int64_t spawnedAt { 0 };
		int64_t touchedAt { 0 };
		TraceEffect *prev { nullptr }, *next { nullptr };
		// TODO: Should we handle it via PolyEffectsSystem? Currently, it does not seem to be worth it.
		QuadPoly poly;
		float minDisplayedLength { 0 };
		unsigned timeout { 0 };

		std::optional<uint16_t> attachedToEntNum;
	};

	struct TeleEffect {
		TeleEffect *prev { nullptr }, *next { nullptr };
		int64_t spawnTime { 0 };
		model_s *model { nullptr };
		unsigned lifetime { 0 };
		float origin[3];
		float color[3];
		mat3_t axis;
		int animFrame { 0 };
		int clientNum { std::numeric_limits<int>::max() };
		int inOutIndex { std::numeric_limits<int>::max() };
	};

	struct AttachedClientEffects {
		TeleEffect *teleEffects[2] { nullptr, nullptr };
		// There are two slots (general, POV) for each kind of beam
		PolyEffectsSystem::StraightBeam *straightLaserBeam[2] { nullptr };
		PolyEffectsSystem::CurvedBeam *curvedLaserBeam[2] { nullptr };
		CurvedPolyTrail *trails[3] { nullptr, nullptr, nullptr };
		int64_t straightLaserBeamTouchedAt[2] { 0 }, curvedLaserBeamTouchedAt[2] { 0 };
		vec4_t laserColor;
		wsw::StaticVector<Vec3, 24 + 1> curvedLaserBeamPoints[2];
	};

	struct AttachedEntityEffects {
		// TODO: Access via index in the owning allocator
		ParticleTrail *particleTrails[2] { nullptr, nullptr };
		StraightPolyTrail *straightPolyTrail { nullptr };
		CurvedPolyTrail *curvedPolyTrail { nullptr };
		TraceEffect *traceEffect { nullptr };
	};

	void makeParticleTrailLingering( ParticleTrail *trail );
	void tryMakingStraightPolyTrailLingering( StraightPolyTrail *trail );
	void tryMakingCurvedPolyTrailLingering( CurvedPolyTrail *trail );
	void detachCurvedPolyTrail( CurvedPolyTrail *trail, int entNum );

	// TODO: Lift this helper to the top level
	template <typename Effect>
	void unlinkAndFreeItemsInList( Effect *head );

	void unlinkAndFree( ParticleTrail *particleTrail );
	void unlinkAndFree( StraightPolyTrail *polyTrail );
	void unlinkAndFree( CurvedPolyTrail *polyTrail );
	void unlinkAndFree( TraceEffect *traceEffect );
	void unlinkAndFree( TeleEffect *teleEffect );

	void touchRocketTrail( int entNum, const float *origin, int64_t currTime, bool useCurvedTrail );
	void touchGrenadeTrail( int entNum, const float *origin, int64_t currTime );

	[[nodiscard]]
	auto allocParticleTrail( int entNum, unsigned trailIndex,
							 const float *origin, unsigned particleSystemBin,
							 ConicalFlockParams *flockParamsTemplate,
							 Particle::AppearanceRules &&appearanceRules  ) -> ParticleTrail *;

	void updateAttachedParticleTrail( ParticleTrail *trail, const float *origin, int64_t currTime );

	[[nodiscard]]
	auto allocStraightPolyTrail( int entNum, shader_s *material, const float *origin,
								 const StraightPolyTrailProps *props ) -> StraightPolyTrail *;

	void updateAttachedStraightPolyTrail( StraightPolyTrail *trail, const float *origin, int64_t currTime );

	[[nodiscard]]
	auto allocCurvedPolyTrail( int entNum, shader_s *material, const CurvedPolyTrailProps *props ) -> CurvedPolyTrail *;

	void updateAttachedCurvedPolyTrail( CurvedPolyTrail *trail, const float *origin, int64_t currTime );

	[[nodiscard]]
	auto allocTraceEffect( int entNum, int64_t currTime, TraceEffectParams &&params ) -> TraceEffect *;

	void updateAttachedTraceEffect( TraceEffect *effect, const float *spawnOrigin, const float *currOrigin, int64_t currTime );
	void tryMakingTraceEffectLingering( TraceEffect *effect );
	void detachTraceEffect( TraceEffect *effect, int entNum );

	void spawnPlayerTeleEffect( int entNum, int64_t currTime, const TeleEffectParams &params, int inOrOutIndex );

	static constexpr unsigned kClippedTrailsBin = ParticleSystem::kClippedTrailFlocksBin;
	static constexpr unsigned kNonClippedTrailsBin = ParticleSystem::kNonClippedTrailFlocksBin;

	ParticleTrail *m_attachedParticleTrailsHead { nullptr };
	ParticleTrail *m_lingeringParticleTrailsHead { nullptr };

	StraightPolyTrail *m_attachedStraightPolyTrailsHead { nullptr };
	StraightPolyTrail *m_lingeringStraightPolyTrailsHead { nullptr };

	CurvedPolyTrail *m_attachedCurvedPolyTrailsHead { nullptr };
	CurvedPolyTrail *m_lingeringCurvedPolyTrailsHead { nullptr };

	TraceEffect *m_attachedTraceEffectsHead { nullptr };
	TraceEffect *m_lingeringTraceEffectsHead { nullptr };

	TeleEffect *m_teleEffectsHead { nullptr };

	wsw::HeapBasedFreelistAllocator m_particleTrailsAllocator { sizeof( ParticleTrail ), 4 * MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_straightPolyTrailsAllocator { sizeof( StraightPolyTrail ), MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_curvedPolyTrailsAllocator { sizeof( CurvedPolyTrail ), 4 * MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_teleEffectsAllocator { sizeof( TeleEffect ), 2 * MAX_CLIENTS };
	wsw::HeapBasedFreelistAllocator m_traceEffectsAllocator { sizeof( TraceEffect ), 4 * MAX_CLIENTS };

	AttachedEntityEffects m_attachedEntityEffects[MAX_EDICTS];
	AttachedClientEffects m_attachedClientEffects[MAX_CLIENTS];

	wsw::RandomGenerator m_rng;

	int64_t m_lastTime { 0 };
};

#endif