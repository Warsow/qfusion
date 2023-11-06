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

#include "trackedeffectssystem.h"
#include "../common/links.h"
#include "../cgame/cg_local.h"
#include "../common/configvars.h"

struct StraightPolyTrailProps {
	unsigned lingeringLimit { 200 };
	float maxLength { 0.0f };
	float tileLength { 0.0f };
	float width { 0.0f };
	float prestep { 32.0f };
	float fromColor[4] { 1.0f, 1.0f, 1.0f, 0.0f };
	float toColor[4] { 1.0f, 1.0f, 1.0f, 0.2f };
};

TrackedEffectsSystem::~TrackedEffectsSystem() {
	clear();
}

void TrackedEffectsSystem::clear() {
	unlinkAndFreeItemsInList( m_attachedParticleTrailsHead );
	unlinkAndFreeItemsInList( m_lingeringParticleTrailsHead );
	assert( !m_attachedParticleTrailsHead && !m_lingeringParticleTrailsHead );

	unlinkAndFreeItemsInList( m_attachedStraightPolyTrailsHead );
	unlinkAndFreeItemsInList( m_lingeringStraightPolyTrailsHead );
	assert( !m_attachedStraightPolyTrailsHead && !m_lingeringStraightPolyTrailsHead );

	unlinkAndFreeItemsInList( m_attachedCurvedPolyTrailsHead );
	unlinkAndFreeItemsInList( m_lingeringCurvedPolyTrailsHead );
	assert( !m_attachedCurvedPolyTrailsHead && !m_lingeringCurvedPolyTrailsHead );

	unlinkAndFreeItemsInList( m_teleEffectsHead );
	assert( !m_teleEffectsHead );
}

template <typename Effect>
void TrackedEffectsSystem::unlinkAndFreeItemsInList( Effect *head ) {
	for( Effect *effect = head, *next; effect; effect = next ) { next = effect->next;
		unlinkAndFree( effect );
	}
}

void TrackedEffectsSystem::unlinkAndFree( ParticleTrail *particleTrail ) {
	if( particleTrail->attachmentIndices ) [[unlikely]] {
		wsw::unlink( particleTrail, &m_attachedParticleTrailsHead );
		const auto [entNum, trailIndex] = *particleTrail->attachmentIndices;
		m_attachedEntityEffects[entNum].particleTrails[trailIndex] = nullptr;
	} else {
		wsw::unlink( particleTrail, &m_lingeringParticleTrailsHead );
	}

	cg.particleSystem.destroyTrailFlock( particleTrail->particleFlock );
	particleTrail->~ParticleTrail();
	m_particleTrailsAllocator.free( particleTrail );
}

void TrackedEffectsSystem::unlinkAndFree( StraightPolyTrail *polyTrail ) {
	if( polyTrail->attachedToEntNum ) {
		wsw::unlink( polyTrail, &m_attachedStraightPolyTrailsHead );
		m_attachedEntityEffects[*polyTrail->attachedToEntNum].straightPolyTrail = nullptr;
	} else {
		wsw::unlink( polyTrail, &m_lingeringStraightPolyTrailsHead );
	}

	cg.polyEffectsSystem.destroyStraightBeamEffect( polyTrail->beam );
	polyTrail->~StraightPolyTrail();
	m_straightPolyTrailsAllocator.free( polyTrail );
}

void TrackedEffectsSystem::unlinkAndFree( CurvedPolyTrail *polyTrail ) {
	if( polyTrail->attachedToEntNum ) {
		wsw::unlink( polyTrail, &m_attachedCurvedPolyTrailsHead );
		detachCurvedPolyTrail( polyTrail, *polyTrail->attachedToEntNum );
	} else {
		wsw::unlink( polyTrail, &m_lingeringCurvedPolyTrailsHead );
	}

	cg.polyEffectsSystem.destroyCurvedBeamEffect( polyTrail->beam );
	polyTrail->~CurvedPolyTrail();
	m_curvedPolyTrailsAllocator.free( polyTrail );
}

void TrackedEffectsSystem::unlinkAndFree( TeleEffect *teleEffect ) {
	assert( (unsigned)teleEffect->clientNum < (unsigned)MAX_CLIENTS );
	assert( teleEffect->inOutIndex == 0 || teleEffect->inOutIndex == 1 );

	wsw::unlink( teleEffect, &m_teleEffectsHead );
	assert( m_attachedClientEffects[teleEffect->clientNum].teleEffects[teleEffect->inOutIndex] == teleEffect );
	m_attachedClientEffects[teleEffect->clientNum].teleEffects[teleEffect->inOutIndex] = nullptr;
	teleEffect->~TeleEffect();
	m_teleEffectsAllocator.free( teleEffect );
}

void TrackedEffectsSystem::spawnPlayerTeleEffect( int entNum, int64_t currTime, const TeleEffectParams &params, int inOrOutIndex ) {
	const int clientNum = entNum - 1;
	assert( (unsigned)clientNum < (unsigned)MAX_CLIENTS );
	assert( inOrOutIndex == 0 || inOrOutIndex == 1 );

	void *mem;
	// Note: this path seemingly requires a custom gametype script code for testing
	// (usually resetEntityEffects() kicks in just before teleportation processing).
	if( TeleEffect *existing = m_attachedClientEffects[clientNum].teleEffects[inOrOutIndex] ) {
		wsw::unlink( existing, &m_teleEffectsHead );
		existing->~TeleEffect();
		mem = existing;
	} else {
		assert( !m_teleEffectsAllocator.isFull() );
		mem = m_teleEffectsAllocator.allocOrNull();
	}

	auto *const effect = new( mem )TeleEffect;
	effect->spawnTime  = currTime;
	effect->animFrame  = params.animFrame;
	effect->lifetime   = 1000;
	effect->clientNum  = clientNum;
	effect->inOutIndex = inOrOutIndex;
	effect->model      = params.model;

	VectorCopy( params.origin, effect->origin );
	VectorCopy( params.colorRgb, effect->color );
	Matrix3_Copy( params.axis, effect->axis );

	wsw::link( effect, &m_teleEffectsHead );
	m_attachedClientEffects[clientNum].teleEffects[inOrOutIndex] = effect;
}

auto TrackedEffectsSystem::allocParticleTrail( int entNum, unsigned trailIndex,
											   const float *origin, unsigned particleSystemBin,
											   ConicalFlockParams *paramsTemplate,
											   Particle::AppearanceRules &&appearanceRules ) -> ParticleTrail * {
	// Don't try evicting other effects in case of failure
	// (this could lead to wasting CPU cycles every frame in case when it starts kicking in)
	// TODO: Try picking lingering trails in case if it turns out to be really needed?
	if( void *mem = m_particleTrailsAllocator.allocOrNull() ) [[likely]] {
		auto *__restrict trail = new( mem )ParticleTrail;

		trail->paramsTemplate = paramsTemplate;

		// Don't drop right now, just mark for computing direction next frames
		trail->particleFlock = cg.particleSystem.createTrailFlock( appearanceRules, particleSystemBin );

		VectorCopy( origin, trail->lastDropOrigin );

		assert( entNum && entNum < MAX_EDICTS );
		assert( trailIndex == 0 || trailIndex == 1 );
		assert( !m_attachedEntityEffects[entNum].particleTrails[trailIndex] );
		trail->attachmentIndices = { (uint16_t)entNum, (uint8_t)trailIndex };

		wsw::link( trail, &m_attachedParticleTrailsHead );
		return trail;
	}

	return nullptr;
}

void TrackedEffectsSystem::updateAttachedParticleTrail( ParticleTrail *trail, const float *origin, int64_t currTime ) {
	trail->touchedAt                = currTime;
	trail->particleFlock->timeoutAt = std::numeric_limits<int64_t>::max();

	updateParticleTrail( trail->particleFlock, trail->paramsTemplate, origin, trail->lastDropOrigin, &m_rng, currTime, {
		.maxParticlesPerDrop = trail->maxParticlesPerDrop,
		.dropDistance        = trail->dropDistance,
	});
}

auto TrackedEffectsSystem::allocStraightPolyTrail( int entNum, shader_s *material, const float *origin,
												   const StraightPolyTrailProps *props ) -> StraightPolyTrail * {
	// TODO: Reuse lingering trails?

	if( void *mem = m_straightPolyTrailsAllocator.allocOrNull() ) {
		if( auto *beam = cg.polyEffectsSystem.createStraightBeamEffect( material ) ) {
			auto *const trail       = new( mem )StraightPolyTrail;
			trail->attachedToEntNum = entNum;
			trail->beam             = beam;
			trail->props            = props;
			VectorCopy( origin, trail->initialOrigin );
			wsw::link( trail, &m_attachedStraightPolyTrailsHead );
			return trail;
		} else {
			m_straightPolyTrailsAllocator.free( mem );
		}
	}

	return nullptr;
}

auto TrackedEffectsSystem::allocCurvedPolyTrail( int entNum, shader_s *material,
												 const CurvedPolyTrailProps *props ) -> CurvedPolyTrail * {
	// TODO: Reuse lingering trails?

	if( void *mem = m_curvedPolyTrailsAllocator.allocOrNull() ) {
		if( auto *beam = cg.polyEffectsSystem.createCurvedBeamEffect( material ) ) {
			auto *const trail = new( mem )CurvedPolyTrail;
			wsw::link( trail, &m_attachedCurvedPolyTrailsHead );
			trail->attachedToEntNum = entNum;
			trail->beam             = beam;
			trail->props            = props;
			return trail;
		} else {
			m_curvedPolyTrailsAllocator.free( mem );
		}
	}

	return nullptr;
}

void TrackedEffectsSystem::updateAttachedStraightPolyTrail( StraightPolyTrail *trail, const float *origin,
															int64_t currTime ) {
	trail->touchedAt = currTime;
	VectorCopy( origin, trail->lastTo );
	VectorCopy( origin, trail->lastFrom );

	const StraightPolyTrailProps &__restrict props = *trail->props;

	const float squareDistance = DistanceSquared( origin, trail->initialOrigin );
	if( squareDistance > wsw::square( props.prestep ) ) {
		const float rcpDistance = Q_RSqrt( squareDistance );

		float length = props.maxLength;
		float width  = props.width;
		if( squareDistance < wsw::square( props.prestep + props.maxLength ) ) {
			length = ( squareDistance * rcpDistance ) - props.prestep;
			width  = props.width * ( length * Q_Rcp( props.maxLength ) );
		}

		if( length > 1.0f ) {
			vec3_t dir;
			VectorSubtract( trail->initialOrigin, origin, dir );
			VectorScale( dir, rcpDistance, dir );
			VectorMA( origin, length, dir, trail->lastFrom );
		}

		trail->lastWidth = width;
	}

	cg.polyEffectsSystem.updateStraightBeamEffect( trail->beam, props.fromColor, props.toColor,
												   trail->lastWidth, props.tileLength,
												   trail->lastFrom, trail->lastTo );
}

void TrackedEffectsSystem::updateCurvedPolyTrail( const CurvedPolyTrailProps &__restrict props,
												  const float *__restrict origin,
												  int64_t currTime,
												  wsw::StaticVector<Vec3, 32> *__restrict points,
												  wsw::StaticVector<int64_t, 32> *__restrict timestamps ) {
	assert( points->size() == timestamps->size() );

	if( !points->empty() ) {
		unsigned numTimedOutPoints = 0;
		for(; numTimedOutPoints < timestamps->size(); ++numTimedOutPoints ) {
			if( ( *timestamps )[numTimedOutPoints] + props.maxNodeLifetime > currTime ) {
				break;
			}
		}
		if( numTimedOutPoints ) {
			// TODO: Use a circular buffer
			points->erase( points->begin(), points->begin() + numTimedOutPoints );
			timestamps->erase( timestamps->begin(), timestamps->begin() + numTimedOutPoints );
		}
	}

	if( points->size() > 1 ) {
		float totalLength = 0.0f;
		const unsigned maxSegmentNum = points->size() - 2;
		for( unsigned segmentNum = 0; segmentNum <= maxSegmentNum; ++segmentNum ) {
			const float *__restrict pt1 = ( *points )[segmentNum + 0].Data();
			const float *__restrict pt2 = ( *points )[segmentNum + 1].Data();
			totalLength += DistanceFast( pt1, pt2 );
		}
		if( totalLength > props.maxLength ) {
			unsigned segmentNum = 0;
			for(; segmentNum <= maxSegmentNum; ++segmentNum ) {
				const float *__restrict pt1 = ( *points )[segmentNum + 0].Data();
				const float *__restrict pt2 = ( *points )[segmentNum + 1].Data();
				totalLength -= DistanceFast( pt1, pt2 );
				if( totalLength <= props.maxLength ) {
					break;
				}
			}
			// This condition preserves the last segment that breaks the loop.
			// segmentNum - 1 should be used instead if props.maxLength should never be reached.
			if( const unsigned numPointsToDrop = segmentNum ) {
				assert( numPointsToDrop <= points->size() );
				points->erase( points->begin(), points->begin() + numPointsToDrop );
				timestamps->erase( timestamps->begin(), timestamps->begin() + numPointsToDrop );
			}
		}
	}

	bool shouldAddPoint = true;
	if( !points->empty() ) [[likely]] {
		if( DistanceSquared( points->back().Data(), origin ) < wsw::square( props.minDistanceBetweenNodes ) ) {
			shouldAddPoint = false;
		} else {
			if( points->full() ) {
				points->erase( points->begin() );
				timestamps->erase( timestamps->begin() );
			}
		}
	}

	if( shouldAddPoint ) {
		points->push_back( Vec3( origin ) );
		timestamps->push_back( currTime );
	}

	assert( points->size() == timestamps->size() );
}

void TrackedEffectsSystem::updateAttachedCurvedPolyTrail( CurvedPolyTrail *trail, const float *origin,
														  int64_t currTime ) {
	const CurvedPolyTrailProps &props = *trail->props;
	updateCurvedPolyTrail( props, origin, currTime, &trail->points, &trail->timestamps );

	trail->touchedAt      = currTime;
	trail->lastPointsSpan = { (const vec3_t *)trail->points.data(), trail->points.size() };

	cg.polyEffectsSystem.updateCurvedBeamEffect( trail->beam, props.fromColor, props.toColor,
												 props.width, PolyEffectsSystem::UvModeFit {},
												 trail->lastPointsSpan );
}

static const RgbaLifespan kRocketSmokeTrailColors[1] {
	{
		.initial  = { 1.0f, 0.7f, 0.3f, 1.0f },
		.fadedIn  = { 0.7f, 0.7f, 0.7f, 0.15f },
		.fadedOut = { 0.0f, 0.0f, 0.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.4f,
		.startFadingOutAtLifetimeFrac = 0.5f,
	}
};

static const RgbaLifespan kRocketFireTrailColors[1] {
	{
		.initial  = { 1.0f, 0.5f, 0.0f, 0.0f },
		.fadedIn  = { 1.0f, 0.7f, 0.3f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.25f,
		.startFadingOutAtLifetimeFrac = 0.50f,
	}
};

static ConicalFlockParams g_rocketSmokeParticlesFlockParams {
	.gravity         = 0,
	.angle           = 45,
	.innerAngle      = 18,
	.speed           = { .min = 75, .max = 150 },
	.timeout         = { .min = 350, .max = 400 },
	.activationDelay = { .min = 8, .max = 8 },
};

static ConicalFlockParams g_rocketFireParticlesFlockParams {
	.gravity         = 0,
	.angle           = 15,
	.speed           = { .min = 75, .max = 150 },
	.timeout         = { .min = 125, .max = 250 },
	.activationDelay = { .min = 8, .max = 8 },
};

static const StraightPolyTrailProps kRocketCombinedStraightPolyTrailProps {
	.maxLength = 250.0f,
	.width     = 20.0f,
};

static const StraightPolyTrailProps kRocketStandaloneStraightPolyTrailProps {
	.maxLength = 325.0f,
	.width     = 20.0f,
};

static const CurvedPolyTrailProps kRocketCombinedCurvedPolyTrailProps {
	.maxNodeLifetime = 300u,
	.maxLength       = 300.0f,
	.width           = 20.0f,
};

static const CurvedPolyTrailProps kRocketStandaloneCurvedPolyTrailProps {
	.maxNodeLifetime = 350u,
	.maxLength       = 400.0f,
	.width           = 10.0f,
};

void TrackedEffectsSystem::touchRocketTrail( int entNum, const float *origin, int64_t currTime, bool useCurvedTrail ) {
	AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];

	if( v_projectileSmokeTrail.get() ) {
		if( !effects->particleTrails[0] ) [[unlikely]] {
			effects->particleTrails[0] = allocParticleTrail( entNum, 0, origin, kClippedTrailsBin,
															 &::g_rocketSmokeParticlesFlockParams, {
				.materials     = cgs.media.shaderRocketSmokeTrailParticle.getAddressOfHandle(),
				.colors        = kRocketSmokeTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 20.0f, .spread = 1.5f }, .sizeBehaviour = Particle::Expanding
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[0] ) [[likely]] {
			trail->dropDistance = 8.0f;
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectileFireTrail.get() ) {
		if( !effects->particleTrails[1] ) [[unlikely]] {
			effects->particleTrails[1] = allocParticleTrail( entNum, 1, origin, kClippedTrailsBin,
															 &::g_rocketFireParticlesFlockParams, {
				.materials     = cgs.media.shaderRocketFireTrailParticle.getAddressOfHandle(),
				.colors        = kRocketFireTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 7.0f, .spread = 1.0f }, .sizeBehaviour = Particle::Shrinking,
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[1] ) [[likely]] {
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectilePolyTrail.get() ) {
		[[maybe_unused]] shader_s *material;
		[[maybe_unused]] const StraightPolyTrailProps *straightPolyTrailProps;
		[[maybe_unused]] const CurvedPolyTrailProps *CurvedPolyTrailProps;
		if( v_projectileFireTrail.get() || v_projectileSmokeTrail.get() ) {
			material               = cgs.media.shaderRocketPolyTrailCombined;
			straightPolyTrailProps = &kRocketCombinedStraightPolyTrailProps;
			CurvedPolyTrailProps   = &kRocketCombinedCurvedPolyTrailProps;
		} else {
			material               = cgs.media.shaderRocketPolyTrailStandalone;
			straightPolyTrailProps = &kRocketStandaloneStraightPolyTrailProps;
			CurvedPolyTrailProps   = &kRocketStandaloneCurvedPolyTrailProps;
		}

		if( useCurvedTrail ) {
			if( !effects->curvedPolyTrail ) [[unlikely]] {
				effects->curvedPolyTrail = allocCurvedPolyTrail( entNum, material, CurvedPolyTrailProps );
			}
			if( CurvedPolyTrail *trail = effects->curvedPolyTrail ) [[likely]] {
				updateAttachedCurvedPolyTrail( trail, origin, currTime );
			}
		} else {
			if( !effects->straightPolyTrail ) [[unlikely]] {
				effects->straightPolyTrail = allocStraightPolyTrail( entNum, material, origin, straightPolyTrailProps );
			}
			if( StraightPolyTrail *trail = effects->straightPolyTrail ) [[likely]] {
				updateAttachedStraightPolyTrail( trail, origin, currTime );
			}
		}
	}
}

static const RgbaLifespan kGrenadeFuseTrailColors[1] {
	{
		.initial  = { 1.0f, 0.7f, 0.3f, 0.0f },
		.fadedIn  = { 1.0f, 0.7f, 0.3f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.075f,
	}
};

static const RgbaLifespan kGrenadeSmokeTrailColors[1] {
	{
		.initial  = { 1.0f, 0.7f, 0.3f, 0.0f },
		.fadedIn  = { 0.7f, 0.7f, 0.7f, 0.2f },
		.fadedOut = { 0.0f, 0.0f, 0.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.25f,
		.startFadingOutAtLifetimeFrac = 0.50f,
	}
};

static ConicalFlockParams g_grenadeFuseParticlesFlockParams {
	.gravity    = 0,
	.angle      = 60,
	.innerAngle = 30,
	.speed      = { .min = 50, .max = 75 },
	.timeout    = { .min = 125, .max = 175 },
};

static ConicalFlockParams g_grenadeSmokeParticlesFlockParams {
	.gravity         = 0,
	.angle           = 24,
	.innerAngle      = 9,
	.speed           = { .min = 25, .max = 50 },
	.timeout         = { .min = 300, .max = 350 },
	.activationDelay = { .min = 8, .max = 8 },
};

static const CurvedPolyTrailProps kGrenadeCombinedPolyTrailProps {
	.maxNodeLifetime = 300,
	.maxLength       = 150,
	.width           = 15,
	.fromColor       = { 1.0f, 1.0f, 1.0f, 0.0f },
	.toColor         = { 0.2f, 0.2f, 1.0f, 0.2f },
};

static const CurvedPolyTrailProps kGrenadeStandalonePolyTrailProps {
	.maxNodeLifetime = 300,
	.maxLength       = 250,
	.width           = 12,
	.fromColor       = { 1.0f, 1.0f, 1.0f, 0.0f },
	.toColor         = { 0.2f, 0.2f, 1.0f, 0.2f },
};

void TrackedEffectsSystem::touchGrenadeTrail( int entNum, const float *origin, int64_t currTime ) {
	AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];

	if( v_projectileSmokeTrail.get() ) {
		if( !effects->particleTrails[0] ) [[unlikely]] {
			effects->particleTrails[0] = allocParticleTrail( entNum, 0, origin, kClippedTrailsBin,
															 &::g_grenadeSmokeParticlesFlockParams, {
				.materials     = cgs.media.shaderGrenadeSmokeTrailParticle.getAddressOfHandle(),
				.colors        = kGrenadeSmokeTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 20.0f, .spread = 1.0f }, .sizeBehaviour = Particle::Shrinking,
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[0] ) [[likely]] {
			trail->dropDistance = 8.0f;
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectileFireTrail.get() ) {
		if( !effects->particleTrails[1] ) [[unlikely]] {
			effects->particleTrails[1] = allocParticleTrail( entNum, 1, origin, kClippedTrailsBin,
															 &::g_grenadeFuseParticlesFlockParams, {
				.materials     = cgs.media.shaderGrenadeFireTrailParticle.getAddressOfHandle(),
				.colors        = kGrenadeFuseTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 7.0f, .spread = 1.0f }, .sizeBehaviour = Particle::Shrinking,
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[1] ) {
			trail->dropDistance = 8.0f;
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectilePolyTrail.get() ) {
		[[maybe_unused]] shader_s *material;
		[[maybe_unused]] const CurvedPolyTrailProps *props;
		if( v_projectileFireTrail.get() || v_projectileSmokeTrail.get() ) {
			material = cgs.media.shaderGrenadePolyTrailCombined;
			props    = &kGrenadeCombinedPolyTrailProps;
		} else {
			material = cgs.media.shaderGrenadePolyTrailStandalone;
			props    = &kGrenadeStandalonePolyTrailProps;
		}
		if( !effects->curvedPolyTrail ) [[unlikely]] {
			effects->curvedPolyTrail = allocCurvedPolyTrail( entNum, material, props );
		}
		if( CurvedPolyTrail *trail = effects->curvedPolyTrail ) [[likely]] {
			updateAttachedCurvedPolyTrail( trail, origin, currTime );
		}
	}
}

static const RgbaLifespan kBlastSmokeTrailColors[1] {
	{
		.initial  = { 1.0f, 0.5f, 0.5f, 0.0f },
		.fadedIn  = { 1.0f, 0.8f, 0.5f, 0.1f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.1f,
		.startFadingOutAtLifetimeFrac = 0.5f,
	}
};

static const RgbaLifespan kBlastIonsTrailColors[] {
	{
		.initial  = { 1.0f, 0.7f, 0.5f, 1.0f },
		.fadedIn  = { 1.0f, 0.7f, 0.5f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 1.0f },
	},
	{
		.initial  = { 1.0f, 0.7f, 0.5f, 1.0f },
		.fadedIn  = { 1.0f, 0.8f, 0.6f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 1.0f },
	},
	{
		.initial  = { 1.0f, 0.7f, 0.5f, 1.0f },
		.fadedIn  = { 1.0f, 0.9f, 0.9f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 1.0f },
	},
	{
		.initial  = { 1.0f, 0.7f, 0.5f, 1.0f },
		.fadedIn  = { 1.0f, 0.9f, 0.8f, 1.0f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 1.0f },
	}
};

static ConicalFlockParams g_blastSmokeParticlesFlockParams {
	.gravity = 0,
	.angle   = 24,
	.speed   = { .min = 200, .max = 300 },
	.timeout = { .min = 175, .max = 225 },
};

static ConicalFlockParams g_blastIonsParticlesFlockParams {
	.gravity         = -75,
	.angle           = 30,
	.speed           = { .min = 200, .max = 300 },
	.timeout         = { .min = 300, .max = 400 },
};

static const StraightPolyTrailProps kBlastCombinedTrailProps {
	.maxLength = 250,
	.width     = 20,
};

static const StraightPolyTrailProps kBlastStandaloneTrailProps {
	.maxLength = 600,
	.width     = 12,
};

void TrackedEffectsSystem::touchBlastTrail( int entNum, const float *origin, int64_t currTime ) {
	AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];

	if( v_projectileSmokeTrail.get() ) {
		if( !effects->particleTrails[0] ) [[unlikely]] {
			effects->particleTrails[0] = allocParticleTrail( entNum, 0, origin, kClippedTrailsBin,
															 &::g_blastSmokeParticlesFlockParams, {
				.materials     = cgs.media.shaderBlastCloudTrailParticle.getAddressOfHandle(),
				.colors        = kBlastSmokeTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 10.0f, .spread = 1.0f }, .sizeBehaviour = Particle::Expanding,
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[0] ) [[likely]] {
			trail->dropDistance = 8.0f;
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectileFireTrail.get() ) {
		if( !effects->particleTrails[1] ) [[unlikely]] {
			effects->particleTrails[1] = allocParticleTrail( entNum, 1, origin, kClippedTrailsBin,
															 &::g_blastIonsParticlesFlockParams, {
				.materials     = cgs.media.shaderBlastFireTrailParticle.getAddressOfHandle(),
				.colors        = kBlastIonsTrailColors,
				.geometryRules = Particle::SpriteRules {
					.radius = { .mean = 3.0f, .spread = 0.75f }, .sizeBehaviour = Particle::Shrinking,
				},
			});
		}
		if( ParticleTrail *trail = effects->particleTrails[1] ) [[likely]] {
			updateAttachedParticleTrail( trail, origin, currTime );
		}
	}

	if( v_projectilePolyTrail.get() ) {
		[[maybe_unused]] shader_s *material;
		[[maybe_unused]] const StraightPolyTrailProps *props;
		if( v_projectileFireTrail.get() || v_projectileSmokeTrail.get() ) {
			material = cgs.media.shaderBlastPolyTrailCombined;
			props    = &kBlastCombinedTrailProps;
		} else {
			material = cgs.media.shaderBlastPolyTrailStandalone;
			props    = &kBlastStandaloneTrailProps;
		}
		if( !effects->straightPolyTrail ) {
			effects->straightPolyTrail = allocStraightPolyTrail( entNum, material, origin, props );
		}
		if( StraightPolyTrail *trail = effects->straightPolyTrail ) {
			updateAttachedStraightPolyTrail( trail, origin, currTime );
		}
	}
}

static ParticleColorsForTeamHolder g_electroCloudTrailParticleColorsHolder {
	.defaultColors = {
		.initial  = { 0.5f, 0.7f, 1.0f, 1.0f },
		.fadedIn  = { 0.7f, 0.7f, 1.0f, 0.2f },
		.fadedOut = { 1.0f, 1.0f, 1.0f, 0.0f },
		.finishFadingInAtLifetimeFrac = 0.1f,
		.startFadingOutAtLifetimeFrac = 0.5f,
	}
};

static const RgbaLifespan kElectroIonsTrailColors[5] {
	// All components are the same so we omit field designators
	{ { 1.0f, 1.0f, 1.0f, 1.0f }, { 1.0f, 1.0f, 1.0f, 1.0f }, { 1.0f, 1.0f, 1.0f, 1.0f } },
	{ { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f } },
	{ { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f } },
	{ { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f }, { 0.5f, 0.7f, 1.0f, 1.0f } },
	{ { 0.3f, 0.3f, 1.0f, 1.0f }, { 0.3f, 0.3f, 1.0f, 1.0f }, { 0.3f, 0.3f, 1.0f, 1.0f } },
};

static ParticleColorsForTeamHolder g_electroIonsParticleColorsHolder {
	.defaultColors = {
		.initial  = { 1.0f, 1.0f, 1.0f, 1.0f },
		.fadedIn  = { 1.0f, 1.0f, 1.0f, 1.0f },
		.fadedOut = { 0.5f, 0.5f, 0.5f, 1.0f }
	}
};

static ConicalFlockParams g_electroCloudParticlesFlockParams {
	.gravity = 0,
	.angle   = 30,
	.speed   = { .min = 200, .max = 300 },
	.timeout = { .min = 150, .max = 200 },
};

static ConicalFlockParams g_electroIonsParticlesFlockParams {
	.gravity         = 0,
	.angle           = 18,
	.speed           = { .min = 200, .max = 300 },
	.timeout         = { .min = 250, .max = 300 },
};

static const StraightPolyTrailProps kElectroPolyTrailProps {
	.maxLength = 700.0f,
	.width     = 20.0f,
};

void TrackedEffectsSystem::touchElectroTrail( int entNum, int ownerNum, const float *origin, int64_t currTime ) {
	std::span<const RgbaLifespan> cloudColors, ionsColors;

	bool useTeamColors = false;
	[[maybe_unused]] vec4_t teamColor;
	[[maybe_unused]] int team = TEAM_PLAYERS;
	// The trail is not a beam, but should conform to the strong beam color as well
	if( v_teamColoredBeams.get() ) {
		team = getTeamForOwner( ownerNum );
		if( team == TEAM_ALPHA || team == TEAM_BETA ) {
			CG_TeamColor( team, teamColor );
			useTeamColors = true;
		}
	}

	ParticleColorsForTeamHolder *const cloudColorsHolder = &::g_electroCloudTrailParticleColorsHolder;
	if( useTeamColors ) {
		cloudColors = { cloudColorsHolder->getColorsForTeam( team, teamColor ), 1 };
		// Use the single color, the trail appearance looks worse than default anyway.
		// TODO: Make ions colors lighter at least
		ionsColors = { ::g_electroIonsParticleColorsHolder.getColorsForTeam( team, teamColor ), 1 };
	} else {
		cloudColors = { &cloudColorsHolder->defaultColors, 1 };
		ionsColors = kElectroIonsTrailColors;
	}

	AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];
	if( !effects->particleTrails[0] ) [[unlikely]] {
		effects->particleTrails[0] = allocParticleTrail( entNum, 0, origin, kNonClippedTrailsBin,
														 &::g_electroCloudParticlesFlockParams, {
			.materials     = cgs.media.shaderElectroCloudTrailParticle.getAddressOfHandle(),
			.colors        = cloudColors,
			.geometryRules = Particle::SpriteRules {
				.radius = { .mean = 9.0f, .spread = 1.0f }, .sizeBehaviour = Particle::Expanding,
			},
		});
	}
	if( ParticleTrail *trail = effects->particleTrails[0] ) [[likely]] {
		trail->dropDistance = 16.0f;
		updateAttachedParticleTrail( trail, origin, currTime );
	}

	if( !effects->particleTrails[1] ) [[unlikely]] {
		effects->particleTrails[1] = allocParticleTrail( entNum, 1, origin, kNonClippedTrailsBin,
														 &::g_electroIonsParticlesFlockParams, {
			.materials     = cgs.media.shaderElectroIonsTrailParticle.getAddressOfHandle(),
			.colors        = ionsColors,
			.geometryRules = Particle::SpriteRules {
				.radius = { .mean = 3.0f, .spread = 0.75f }, .sizeBehaviour = Particle::Shrinking,
			},
		});
	}
	if( ParticleTrail *trail = effects->particleTrails[1] ) [[likely]] {
		trail->dropDistance = 16.0f;
		updateAttachedParticleTrail( trail, origin, currTime );
	}

	if( !effects->straightPolyTrail ) [[unlikely]] {
		effects->straightPolyTrail = allocStraightPolyTrail( entNum, cgs.media.shaderElectroPolyTrail,
															 origin, &kElectroPolyTrailProps );
	}
	if( StraightPolyTrail *trail = effects->straightPolyTrail ) [[likely]] {
		updateAttachedStraightPolyTrail( trail, origin, currTime );
	}
}

static const StraightPolyTrailProps kPlasmaStrongPolyTrailProps {
	.maxLength = 350,
	.width     = 12.0f,
	.fromColor = { 0.3f, 1.0f, 1.0f, 0.00f },
	.toColor   = { 0.1f, 0.8f, 0.4f, 0.15f },
};

static const CurvedPolyTrailProps kPlasmaCurvedPolyTrailProps {
	.maxNodeLifetime = 225,
	.maxLength       = 400,
	.width           = 12.0f,
	.fromColor       = { 0.3f, 1.0f, 1.0f, 0.00f },
	.toColor         = { 0.1f, 0.8f, 0.4f, 0.15f },
};

void TrackedEffectsSystem::touchStrongPlasmaTrail( int entNum, const float *origin, int64_t currTime ) {
	assert( entNum > 0 && entNum < MAX_EDICTS );
	if( v_plasmaTrail.get() && v_projectilePolyTrail.get() ) {
		AttachedEntityEffects *effects = &m_attachedEntityEffects[entNum];
		if( !effects->straightPolyTrail ) {
			effects->straightPolyTrail = allocStraightPolyTrail( entNum, cgs.media.shaderPlasmaPolyTrail,
																 origin, &kPlasmaStrongPolyTrailProps );
		}
		if( StraightPolyTrail *trail = effects->straightPolyTrail ) {
			updateAttachedStraightPolyTrail( trail, origin, currTime );
		}
	}
}

void TrackedEffectsSystem::touchWeakPlasmaTrail( int entNum, const float *origin, int64_t currTime ) {
	assert( entNum > 0 && entNum < MAX_EDICTS );
	if( v_plasmaTrail.get() && v_projectilePolyTrail.get() ) {
		AttachedEntityEffects *effects = &m_attachedEntityEffects[entNum];
		if( !effects->curvedPolyTrail ) {
			effects->curvedPolyTrail = allocCurvedPolyTrail( entNum, cgs.media.shaderPlasmaPolyTrail,
															 &kPlasmaCurvedPolyTrailProps );
		}
		if( CurvedPolyTrail *trail = effects->curvedPolyTrail ) {
			updateAttachedCurvedPolyTrail( trail, origin, currTime );
		}
	}
}

void TrackedEffectsSystem::detachPlayerTrail( int entNum ) {
	assert( entNum > 0 && entNum <= MAX_CLIENTS );
	AttachedClientEffects *effects = &m_attachedClientEffects[entNum - 1];
	if( effects->trails[0] ) {
		for( CurvedPolyTrail *trail: effects->trails ) {
			assert( trail );
			tryMakingCurvedPolyTrailLingering( trail );
		}
	}
}

static const CurvedPolyTrailProps kPlayerPolyTrailProps[3] {
	{
		.maxNodeLifetime = 300,
		.maxLength       = 300,
		.width           = 20.0f,
		.fromColor       = { 1.0f, 1.0f, 1.0f, 0.00f },
		.toColor         = { 1.0f, 1.0f, 1.0f, 0.06f },
	},
	{
		.maxNodeLifetime = 300,
		.maxLength       = 300,
		.width           = 36.0f,
		.fromColor       = { 1.0f, 1.0f, 1.0f, 0.00f },
		.toColor         = { 1.0f, 1.0f, 1.0f, 0.06f },
	},
	{
		.maxNodeLifetime = 300,
		.maxLength       = 300,
		.width           = 32.0f,
		.fromColor       = { 1.0f, 1.0f, 1.0f, 0.00f },
		.toColor         = { 1.0f, 1.0f, 1.0f, 0.06f },
	},
};

void TrackedEffectsSystem::touchPlayerTrail( int entNum, const float *origin, int64_t currTime ) {
	assert( entNum > 0 && entNum <= MAX_CLIENTS );
	// This attachment is specific for clients.
	// Effects of attached models, if added later, should use the generic entity effects path.
	AttachedClientEffects *effects = &m_attachedClientEffects[entNum - 1];
	// Multiple poly trails at different height approximate the fine-grain (edge-extruding) solution relatively well.
	// Require a complete allocation of the set of trails
	if( !effects->trails[0] ) {
		CurvedPolyTrail *trails[3];
		unsigned numCreatedTrails = 0;
		assert( std::size( trails ) == std::size( effects->trails ) );
		assert( std::size( trails ) == std::size( kPlayerPolyTrailProps ) );
		for(; numCreatedTrails < std::size( trails ); ++numCreatedTrails ) {
			trails[numCreatedTrails] = allocCurvedPolyTrail( entNum, cgs.shaderWhite,
															 &kPlayerPolyTrailProps[numCreatedTrails] );
			if( !trails[numCreatedTrails] ) {
				break;
			}
		}
		if( numCreatedTrails == std::size( trails ) ) [[likely]] {
			std::copy( trails, trails + std::size( trails ), effects->trails );
		} else {
			for( unsigned i = 0; i < numCreatedTrails; ++i ) {
				trails[i]->attachedToEntNum = std::nullopt;
				unlinkAndFree( trails[i] );
			}
		}
	}
	if( effects->trails[0] ) {
		assert( std::size( effects->trails ) == 3 );
		// TODO: Adjust properties fot the current bbox
		const vec3_t headOrigin { origin[0], origin[1], origin[2] + 26.0f };
		updateAttachedCurvedPolyTrail( effects->trails[0], headOrigin, currTime );
		const vec3_t bodyOrigin { origin[0], origin[1], origin[2] + 8.0f };
		updateAttachedCurvedPolyTrail( effects->trails[1], bodyOrigin, currTime );
		const vec3_t legsOrigin { origin[0], origin[1], origin[2] - 6.0f };
		updateAttachedCurvedPolyTrail( effects->trails[2], legsOrigin, currTime );
	}
}

void TrackedEffectsSystem::touchCorpseTrail( int entNum, const float *origin, int64_t currTime ) {
	assert( entNum > 0 && entNum < MAX_EDICTS );
	// Can't do much in this case, a single trail should be sufficient.
	AttachedEntityEffects *effects = &m_attachedEntityEffects[entNum];
	if( !effects->curvedPolyTrail ) {
		effects->curvedPolyTrail = allocCurvedPolyTrail( entNum, cgs.shaderWhite, &kPlayerPolyTrailProps[1] );
	}
	if( CurvedPolyTrail *trail = effects->curvedPolyTrail ) {
		updateAttachedCurvedPolyTrail( trail, origin, currTime );
	}
}

void TrackedEffectsSystem::makeParticleTrailLingering( ParticleTrail *particleTrail ) {
	wsw::unlink( particleTrail, &m_attachedParticleTrailsHead );
	wsw::link( particleTrail, &m_lingeringParticleTrailsHead );

	const auto [entNum, trailIndex] = *particleTrail->attachmentIndices;
	particleTrail->attachmentIndices = std::nullopt;

	AttachedEntityEffects *entityEffects = &m_attachedEntityEffects[entNum];
	assert( entityEffects->particleTrails[trailIndex] == particleTrail );
	entityEffects->particleTrails[trailIndex] = nullptr;
}

void TrackedEffectsSystem::tryMakingStraightPolyTrailLingering( StraightPolyTrail *trail ) {
	assert( trail->attachedToEntNum != std::nullopt );

	// If it's worth to be kept
	if( DistanceSquared( trail->lastFrom, trail->lastTo ) > wsw::square( 8.0f ) ) {
		wsw::unlink( trail, &m_attachedStraightPolyTrailsHead );
		wsw::link( trail, &m_lingeringStraightPolyTrailsHead );

		const unsigned entNum = *trail->attachedToEntNum;
		trail->attachedToEntNum = std::nullopt;

		AttachedEntityEffects *entityEffects = &m_attachedEntityEffects[entNum];
		assert( entityEffects->straightPolyTrail == trail );
		entityEffects->straightPolyTrail = nullptr;
	} else {
		unlinkAndFree( trail );
	}
}

void TrackedEffectsSystem::tryMakingCurvedPolyTrailLingering( CurvedPolyTrail *trail ) {
	assert( trail->attachedToEntNum != std::nullopt );

	// If it's worth to be kept
	if( trail->lastPointsSpan.size() > 1 ) {
		wsw::unlink( trail, &m_attachedCurvedPolyTrailsHead );
		wsw::link( trail, &m_lingeringCurvedPolyTrailsHead );

		detachCurvedPolyTrail( trail, *trail->attachedToEntNum );
		assert( trail->attachedToEntNum == std::nullopt );
	} else {
		unlinkAndFree( trail );
	}
}

void TrackedEffectsSystem::detachCurvedPolyTrail( CurvedPolyTrail *trail, int entNum ) {
	assert( entNum >= 0 && entNum < MAX_EDICTS );
	assert( trail->attachedToEntNum == std::optional( entNum ) );

	AttachedEntityEffects *const entityEffects = &m_attachedEntityEffects[entNum];
	if( entityEffects->curvedPolyTrail == trail ) {
		entityEffects->curvedPolyTrail = nullptr;
	} else {
		assert( (unsigned)( entNum - 1 ) < (unsigned)MAX_CLIENTS );
		AttachedClientEffects *const clientEffects = &m_attachedClientEffects[entNum - 1];
		[[maybe_unused]] unsigned i = 0;
		for( ; i < std::size( clientEffects->trails ); ++i ) {
			if( clientEffects->trails[i] == trail ) {
				clientEffects->trails[i] = nullptr;
				break;
			}
		}
		assert( i < std::size( clientEffects->trails ) );
	}
	trail->attachedToEntNum = std::nullopt;
}

void TrackedEffectsSystem::resetEntityEffects( int entNum ) {
	assert( entNum >= 0 && entNum < MAX_EDICTS );

	const int maybeValidClientNum = entNum - 1;
	if( (unsigned)maybeValidClientNum < (unsigned)MAX_CLIENTS ) [[unlikely]] {
		AttachedClientEffects *effects = &m_attachedClientEffects[maybeValidClientNum];
		if( effects->teleEffects[0] ) {
			unlinkAndFree( effects->teleEffects[0] );
		}
		if( effects->teleEffects[1] ) {
			unlinkAndFree( effects->teleEffects[1] );
		}
		assert( !effects->teleEffects[0] && !effects->teleEffects[1] );
		if( effects->curvedLaserBeam ) {
			cg.polyEffectsSystem.destroyCurvedBeamEffect( effects->curvedLaserBeam );
			effects->curvedLaserBeam = nullptr;
			effects->curvedLaserBeamTouchedAt = 0;
		}
		if( effects->straightLaserBeam ) {
			cg.polyEffectsSystem.destroyStraightBeamEffect( effects->straightLaserBeam );
			effects->straightLaserBeam = nullptr;
			effects->straightLaserBeamTouchedAt = 0;
		}
	}

	AttachedEntityEffects *effects = &m_attachedEntityEffects[entNum];
	if( effects->particleTrails[0] ) {
		makeParticleTrailLingering( effects->particleTrails[0] );
		assert( !effects->particleTrails[0] );
	}
	if( effects->particleTrails[1] ) {
		makeParticleTrailLingering( effects->particleTrails[1] );
		assert( !effects->particleTrails[1] );
	}
	if( effects->straightPolyTrail ) {
		tryMakingStraightPolyTrailLingering( effects->straightPolyTrail );
		assert( !effects->straightPolyTrail );
	}
	if( effects->curvedPolyTrail ) {
		tryMakingCurvedPolyTrailLingering( effects->curvedPolyTrail );
		assert( !effects->curvedPolyTrail );
	}
}

static void getLaserColorOverlayForOwner( int ownerNum, vec4_t color ) {
	if( v_teamColoredBeams.get() ) {
		if( int team = getTeamForOwner( ownerNum ); team == TEAM_ALPHA || team == TEAM_BETA ) {
			CG_TeamColor( team, color );
			return;
		}
	}
	Vector4Copy( colorWhite, color );
}

static constexpr float kLaserWidth      = 12.0f;
static constexpr float kLaserTileLength = 64.0f;

void TrackedEffectsSystem::updateStraightLaserBeam( int ownerNum, const float *from, const float *to, int64_t currTime ) {
	assert( ownerNum && ownerNum <= MAX_CLIENTS );
	AttachedClientEffects *effects = &m_attachedClientEffects[ownerNum - 1];
	if( !effects->straightLaserBeam ) {
		effects->straightLaserBeam = cg.polyEffectsSystem.createStraightBeamEffect( cgs.media.shaderLaserGunBeam );
		if( effects->straightLaserBeam ) {
			getLaserColorOverlayForOwner( ownerNum, &effects->laserColor[0] );
		}
	}
	if( effects->straightLaserBeam ) {
		effects->straightLaserBeamTouchedAt = currTime;
		cg.polyEffectsSystem.updateStraightBeamEffect( effects->straightLaserBeam,
													   effects->laserColor, effects->laserColor,
													   kLaserWidth, kLaserTileLength, from, to );
	}
}

void TrackedEffectsSystem::updateCurvedLaserBeam( int ownerNum, std::span<const vec3_t> points, int64_t currTime ) {
	assert( ownerNum && ownerNum <= MAX_CLIENTS );
	AttachedClientEffects *effects = &m_attachedClientEffects[ownerNum - 1];
	if( !effects->curvedLaserBeam ) {
		effects->curvedLaserBeam = cg.polyEffectsSystem.createCurvedBeamEffect( cgs.media.shaderLaserGunBeam );
		if( effects->curvedLaserBeam ) {
			getLaserColorOverlayForOwner( ownerNum, &effects->laserColor[0] );
		}
	}

	if( effects->curvedLaserBeam ) {
		effects->curvedLaserBeamTouchedAt = currTime;
		effects->curvedLaserBeamPoints.clear();
		for( const float *point: points ) {
			effects->curvedLaserBeamPoints.push_back( Vec3( point ) );
		}
		const std::span<const vec3_t> ownedPointsSpan {
			(const vec3_t *)effects->curvedLaserBeamPoints.data(), effects->curvedLaserBeamPoints.size()
		};
		cg.polyEffectsSystem.updateCurvedBeamEffect( effects->curvedLaserBeam,
													 effects->laserColor, effects->laserColor,
													 kLaserWidth, PolyEffectsSystem::UvModeTile { kLaserTileLength },
													 ownedPointsSpan );
	}
}

static inline void copyWithAlphaScale( const float *from, float *to, float alpha ) {
	Vector4Copy( from, to );
	to[3] *= alpha;
}

void TrackedEffectsSystem::simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *drawSceneRequest ) {
	// Collect orphans.

	// The actual drawing of trails is performed by the particle system.
	for( ParticleTrail *trail = m_attachedParticleTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		if( trail->touchedAt != currTime ) [[unlikely]] {
			makeParticleTrailLingering( trail );
		}
	}

	for( ParticleTrail *trail = m_lingeringParticleTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		if( trail->particleFlock->numActivatedParticles ) {
			// Prevent an automatic disposal of the flock
			trail->particleFlock->timeoutAt = std::numeric_limits<int64_t>::max();
		} else {
			unlinkAndFree( trail );
		}
	}

	// The actual drawing of polys is performed by the poly effects system

	for( StraightPolyTrail *trail = m_attachedStraightPolyTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		if( trail->touchedAt != currTime ) [[unlikely]] {
			tryMakingStraightPolyTrailLingering( trail );
		}
	}

	for( CurvedPolyTrail *trail = m_attachedCurvedPolyTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		if( trail->touchedAt != currTime ) [[unlikely]] {
			tryMakingCurvedPolyTrailLingering( trail );
		}
	}

	for( StraightPolyTrail *trail = m_lingeringStraightPolyTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		const int64_t lingeringTime  = currTime - trail->touchedAt;
		const int64_t lingeringLimit = trail->props->lingeringLimit;
		assert( lingeringLimit > 0 && lingeringLimit < 1000 );
		if( lingeringTime < lingeringLimit ) [[likely]] {
			const float lingeringFrac = (float)lingeringTime * Q_Rcp( (float)lingeringLimit );
			vec4_t fadingOutFromColor, fadingOutToColor;
			copyWithAlphaScale( trail->props->fromColor, fadingOutFromColor, 1.0f - lingeringFrac );
			copyWithAlphaScale( trail->props->toColor, fadingOutToColor, 1.0f - lingeringFrac );
			cg.polyEffectsSystem.updateStraightBeamEffect( trail->beam, fadingOutFromColor, fadingOutToColor,
														   trail->lastWidth, trail->props->tileLength,
														   trail->lastFrom, trail->lastTo );
		} else {
			unlinkAndFree( trail );
		}
	}

	for( CurvedPolyTrail *trail = m_lingeringCurvedPolyTrailsHead, *nextTrail = nullptr; trail; trail = nextTrail ) {
		nextTrail = trail->next;
		const int64_t lingeringTime  = currTime - trail->touchedAt;
		const int64_t lingeringLimit = trail->props->lingeringLimit;
		assert( lingeringLimit > 0 && lingeringLimit < 1000 );
		if( lingeringTime < lingeringLimit && trail->points.size() > 1 ) {
			// Update the lingering trail as usual.
			// Submit the last known position as the current one.
			// This allows trails to shrink naturally.
			updateCurvedPolyTrail( *trail->props, trail->points.back().Data(), currTime, &trail->points, &trail->timestamps );
			const float lingeringFrac = (float)lingeringTime * Q_Rcp( (float)lingeringLimit );
			vec4_t fadingOutFromColor, fadingOutToColor;
			copyWithAlphaScale( trail->props->fromColor, fadingOutFromColor, 1.0f - lingeringFrac );
			copyWithAlphaScale( trail->props->toColor, fadingOutToColor, 1.0f - lingeringFrac );
			cg.polyEffectsSystem.updateCurvedBeamEffect( trail->beam, fadingOutFromColor, fadingOutToColor,
														 trail->props->width, PolyEffectsSystem::UvModeFit {},
														 trail->lastPointsSpan );
		} else {
			unlinkAndFree( trail );
		}
	}

	for( TeleEffect *effect = m_teleEffectsHead, *nextEffect = nullptr; effect; effect = nextEffect ) {
		nextEffect = effect->next;
		if( effect->spawnTime + effect->lifetime <= currTime ) [[unlikely]] {
			unlinkAndFree( effect );
			continue;
		}

		const float lifetimeFrac  = (float)( currTime - effect->spawnTime ) * Q_Rcp( (float)effect->lifetime );
		assert( lifetimeFrac >= 0.0f && lifetimeFrac <= 1.0f );
		const float colorFadeFrac = 1.0f - lifetimeFrac;

		entity_t entity;
		memset( &entity, 0, sizeof( entity ) );

		entity.rtype        = RT_MODEL;
		entity.renderfx     = RF_NOSHADOW;
		entity.model        = effect->model;
		entity.customShader = cgs.media.shaderTeleportShellGfx;
		entity.shaderTime   = cg.time;
		entity.scale        = 1.0f;
		entity.frame        = effect->animFrame;
		entity.oldframe     = effect->animFrame;
		entity.backlerp     = 1.0f;

		entity.shaderRGBA[0] = (uint8_t)( 255.0f * effect->color[2] * colorFadeFrac );
		entity.shaderRGBA[1] = (uint8_t)( 255.0f * effect->color[1] * colorFadeFrac );
		entity.shaderRGBA[2] = (uint8_t)( 255.0f * effect->color[2] * colorFadeFrac );
		entity.shaderRGBA[3] = 255;

		Matrix3_Copy( effect->axis, entity.axis );
		VectorCopy( effect->origin, entity.origin );
		VectorCopy( effect->origin, entity.origin2 );

		CG_SetBoneposesForTemporaryEntity( &entity );
		drawSceneRequest->addEntity( &entity );
	}

	PolyEffectsSystem *const polyEffectsSystem = &cg.polyEffectsSystem;
	for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
		AttachedClientEffects *const effects = &m_attachedClientEffects[i];
		if( effects->curvedLaserBeam ) {
			if( effects->curvedLaserBeamTouchedAt < currTime ) {
				polyEffectsSystem->destroyCurvedBeamEffect( effects->curvedLaserBeam );
				effects->curvedLaserBeam = nullptr;
			}
		}
		if( effects->straightLaserBeam ) {
			if( effects->straightLaserBeamTouchedAt < currTime ) {
				polyEffectsSystem->destroyStraightBeamEffect( effects->straightLaserBeam );
				effects->straightLaserBeam = nullptr;
			}
		}
	}
}