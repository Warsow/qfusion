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
#include "../qcommon/links.h"
#include "../cgame/cg_local.h"

TrackedEffectsSystem::~TrackedEffectsSystem() {
	// TODO: unlinkAndFree() does some unnecessary extra work that slows the dtor down

	for( FireTrail *trail = m_fireTrailsHead, *nextFireTrail = nullptr; trail; trail = nextFireTrail ) {
		unlinkAndFree( trail );
	}
	for( ParticleTrail *trail = m_particleTrailsHead, *nextSmokeTrail = nullptr; trail; trail = nextSmokeTrail ) {
		unlinkAndFree( trail );
	}
	for( TeleEffect *effect = m_teleEffectsHead, *nextEffect = nullptr; effect; effect = nextEffect ) {
		unlinkAndFree( effect );
	}
}

void TrackedEffectsSystem::unlinkAndFree( FireTrail *fireTrail ) {
	assert( fireTrail->entNum > 0 && fireTrail->entNum < MAX_EDICTS );

	wsw::unlink( fireTrail, &m_fireTrailsHead );
	m_attachedEntityEffects[fireTrail->entNum].fireTrail = nullptr;
	fireTrail->~FireTrail();
	m_fireTrailsAllocator.free( fireTrail );
}

void TrackedEffectsSystem::unlinkAndFree( ParticleTrail *particleTrail ) {
	assert( particleTrail->entNum > 0 && particleTrail->entNum < MAX_EDICTS );

	wsw::unlink( particleTrail, &m_particleTrailsHead );
	m_attachedEntityEffects[particleTrail->entNum].particleTrail = nullptr;
	cg.particleSystem.destroyTrailFlock( particleTrail->particleFlock );
	particleTrail->~ParticleTrail();
	m_particleTrailsAllocator.free( particleTrail );
}

void TrackedEffectsSystem::unlinkAndFree( TeleEffect *teleEffect ) {
	assert( (unsigned)teleEffect->clientNum < (unsigned)MAX_CLIENTS );
	assert( teleEffect->inOutIndex == 0 || teleEffect->inOutIndex == 1 );

	wsw::unlink( teleEffect, &m_teleEffectsHead );
	m_attachedClientEffects[teleEffect->clientNum].teleEffects[teleEffect->inOutIndex] = nullptr;
	// TODO: Release the model!!!!
	teleEffect->~TeleEffect();
	// TODO: Release bone poses
	m_teleEffectsAllocator.free( teleEffect );
}

void TrackedEffectsSystem::spawnPlayerTeleEffect( int clientNum, const float *origin, model_s *model, int inOrOutIndex ) {
	assert( (unsigned)clientNum < (unsigned)MAX_CLIENTS );
	assert( inOrOutIndex == 0 || inOrOutIndex == 1 );

	TeleEffect **ppEffect = &m_attachedClientEffects[clientNum].teleEffects[inOrOutIndex];
	if( !*ppEffect ) [[likely]] {
		assert( !m_teleEffectsAllocator.isFull() );
		*ppEffect = new( m_teleEffectsAllocator.allocOrNull() )TeleEffect;
		wsw::link( *ppEffect, &m_teleEffectsHead );
	}

	TeleEffect *const __restrict effect = *ppEffect;
	VectorCopy( origin, effect->origin );
	effect->spawnTime = m_lastTime;
	effect->lifetime = 3000u;
	effect->clientNum = clientNum;
	effect->inOutIndex = inOrOutIndex;
	// TODO: Alloc bones, try reusing bones
	effect->model = model;
}

auto TrackedEffectsSystem::allocParticleTrail( int entNum, const float *origin, unsigned particleSystemBin,
											   Particle::AppearanceRules &&appearanceRules ) -> ParticleTrail * {
	// Don't try evicting other effects in case of failure
	// (this could lead to wasting CPU cycles every frame in case when it starts kicking in)
	if( void *mem = m_particleTrailsAllocator.allocOrNull() ) [[likely]] {
		auto *__restrict trail = new( mem )ParticleTrail;

		// Don't drop right now, just mark for computing direction next frames
		trail->particleFlock = cg.particleSystem.createTrailFlock( appearanceRules, particleSystemBin );

		trail->entNum = entNum;
		VectorCopy( origin, trail->lastDropOrigin );
		if( particleSystemBin == kClippedTrailsBin ) {
			trail->maxParticlesInFlock = ParticleSystem::kMaxClippedTrailFlockSize;
		} else {
			trail->maxParticlesInFlock = ParticleSystem::kMaxNonClippedTrailFlockSize;
		}

		wsw::link( trail, &m_particleTrailsHead );
		return trail;
	}

	return nullptr;
}

void TrackedEffectsSystem::updateParticleTrail( ParticleTrail *trail, const float *origin,
												ConeFlockFiller *filler, int64_t currTime ) {
	trail->touchedAt = currTime;

	ParticleFlock *__restrict flock = trail->particleFlock;
	// Prevent an automatic disposal by the particles system
	flock->timeoutAt = std::numeric_limits<int64_t>::max();

	if( trail->lastParticleAt < currTime && flock->numParticlesLeft < trail->maxParticlesInFlock ) {
		const float squareDistance = DistanceSquared( trail->lastDropOrigin, origin );
		if( squareDistance >= trail->dropDistance * trail->dropDistance ) {
			vec3_t dir, stepVec;
			VectorSubtract( trail->lastDropOrigin, origin, dir );

			const float rcpDistance = Q_RSqrt( squareDistance );
			const float distance = Q_Rcp( rcpDistance );
			// The dir is directed towards the old position
			VectorScale( dir, rcpDistance, dir );
			// Make steps of trail->dropDistance units towards the new position
			VectorScale( dir, -trail->dropDistance, stepVec );

			VectorCopy( trail->lastDropOrigin, filler->origin );

			const unsigned numSteps = (unsigned)std::max( 1.0f, distance * Q_Rcp( trail->dropDistance ) );
			for( unsigned i = 0; i < numSteps; ++i ) {
				if( flock->numParticlesLeft + trail->maxParticlesPerDrop >= trail->maxParticlesInFlock ) [[unlikely]] {
					break;
				}

				// Creates not less than 1 particle
				const auto [_, numParticles] = filler->fill( flock->particles + flock->numParticlesLeft,
															 trail->maxParticlesPerDrop, &m_rng, currTime );
				assert( numParticles );
				flock->numParticlesLeft += numParticles;
				VectorAdd( filler->origin, stepVec, filler->origin );
			}

			VectorCopy( filler->origin, trail->lastDropOrigin );
			trail->lastParticleAt = currTime;
		}
	}
}

static const vec4_t kRocketOrGrenadeTrailInitialColor { 1.0f, 0.7f, 0.3f, 1.0f };
static const vec4_t kRocketOrGrenadeTrailFadedInColor { 1.0f, 1.0f, 1.0f, 0.3f };
static const vec4_t kRocketOrGrenadeTrailFadedOutColor { 1.0f, 1.0f, 1.0f, 0.0f };

void TrackedEffectsSystem::touchRocketOrGrenadeTrail( int entNum, const float *origin,
													  ConeFlockFiller *flockFiller, int64_t currTime ) {
	const int hasFireTrail = cg_projectileFireTrail->integer;
	const int hasSmokeTrail = cg_projectileTrail->integer;
	if( !( hasFireTrail | hasSmokeTrail ) ) [[unlikely]] {
		return;
	}

	AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];
	if( hasSmokeTrail ) {
		if( !effects->particleTrail ) [[unlikely]] {
			effects->particleTrail = allocParticleTrail( entNum, origin, kClippedTrailsBin, {
				.material = cgs.media.shaderFlareParticle,
				.kind     = Particle::Sprite,
				.radius   = 8.0f,
				.initialColor  = kRocketOrGrenadeTrailInitialColor,
				.fadedInColor  = kRocketOrGrenadeTrailFadedInColor,
				.fadedOutColor = kRocketOrGrenadeTrailFadedOutColor,
				.fadeInLifetimeFrac  = 0.35f,
				.fadeOutLifetimeFrac = 0.50f
			});
		}
		if( effects->particleTrail ) [[likely]] {
			updateParticleTrail( effects->particleTrail, origin, flockFiller, currTime );
		}
	}
	if( hasFireTrail ) {
		if( !effects->fireTrail ) [[unlikely]] {
			if( void *mem = m_fireTrailsAllocator.allocOrNull() ) [[likely]] {
				effects->fireTrail = new( mem )FireTrail;
				wsw::link( effects->fireTrail, &m_fireTrailsHead );
				effects->fireTrail->entNum = entNum;
			}
		}
		if( FireTrail *const trail = effects->fireTrail ) {
			trail->touchedAt = currTime;
			// TODO: Update...
		}
	}
}

static const vec4_t kPlasmaTrailInitialColor { 0.2f, 1.0f, 0.5f, 0.9f };
static const vec4_t kPlasmaTrailFadedInColor { 0.7f, 1.0f, 0.8f, 0.1f };
static const vec4_t kPlasmaTrailFadedOutColor { 0.7f, 1.0f, 0.8f, 0.00f };

void TrackedEffectsSystem::touchPlasmaTrail( int entNum, const float *origin, int64_t currTime ) {
	if( cg_projectileTrail->integer ) {
		AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];
		if( !effects->particleTrail ) {
			effects->particleTrail = allocParticleTrail( entNum, origin, kNonClippedTrailsBin, {
				.material = cgs.media.shaderBlastParticle,
				.kind     = Particle::Sprite,
				.radius   = 5.0f,
				.initialColor  = kPlasmaTrailInitialColor,
				.fadedInColor  = kPlasmaTrailFadedInColor,
				.fadedOutColor = kPlasmaTrailFadedOutColor,
				.fadeInLifetimeFrac  = 0.15f,
				.fadeOutLifetimeFrac = 0.80f
			});
		}
		if( ParticleTrail *trail = effects->particleTrail ) {
			trail->dropDistance = 4.0f;
			updateParticleTrail( trail, origin, &m_plasmaParticlesFlockFiller, currTime );
		}
	}
}

static const vec4_t kBlastTrailInitialColor { 1.0f, 0.5f, 0.4f, 1.0f };
static const vec4_t kBlastTrailFadedInColor { 1.0f, 0.8f, 0.4f, 1.0f };
static const vec4_t kBlastTrailFadedOutColor { 1.0f, 1.0f, 1.0f, 0.0f };

void TrackedEffectsSystem::touchBlastTrail( int entNum, const float *origin, int64_t currTime ) {
	if( cg_projectileTrail->integer ) {
		AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];
		if( !effects->particleTrail ) {
			effects->particleTrail = allocParticleTrail( entNum, origin, kClippedTrailsBin, {
				.material = cgs.media.shaderBlastParticle,
				.kind     = Particle::Sprite,
				.radius   = 2.0f,
				.initialColor  = kBlastTrailInitialColor,
				.fadedInColor  = kBlastTrailFadedInColor,
				.fadedOutColor = kBlastTrailFadedOutColor,
			});
		}
		if( ParticleTrail *trail = effects->particleTrail ) {
			updateParticleTrail( trail, origin, &m_blastParticlesFlockFiller, currTime );
		}
	}
}

static const vec4_t kElectroTrailInitialColor { 0.5f, 0.7f, 1.0f, 1.0f };
static const vec4_t kElectroTrailFadedInColor { 0.7f, 0.7f, 1.0f, 1.0f };
static const vec4_t kElectroTrailFadedOutColor { 1.0f, 1.0f, 1.0f, 0.0f };

void TrackedEffectsSystem::touchElectroTrail( int entNum, const float *origin, int64_t currTime ) {
	if( cg_projectileTrail->integer ) {
		AttachedEntityEffects *const __restrict effects = &m_attachedEntityEffects[entNum];
		if( !effects->particleTrail ) {
			effects->particleTrail = allocParticleTrail( entNum, origin, kNonClippedTrailsBin, {
				.material = cgs.media.shaderFlareParticle,
				.kind     = Particle::Sprite,
				.radius   = 6.0f,
				.initialColor  = kElectroTrailInitialColor,
				.fadedInColor  = kElectroTrailFadedInColor,
				.fadedOutColor = kElectroTrailFadedOutColor
			});
		}
		if( ParticleTrail *trail = effects->particleTrail ) {
			trail->dropDistance = 8.0f;
			updateParticleTrail( trail, origin, &m_electroParticlesFlockFiller, currTime );
		}
	}
}

void TrackedEffectsSystem::resetEntityEffects( int entNum ) {
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

	assert( entNum >= 0 && entNum < MAX_EDICTS );
	AttachedEntityEffects *effects = &m_attachedEntityEffects[entNum];
	if( effects->particleTrail ) {
		unlinkAndFree( effects->particleTrail );
		assert( !effects->particleTrail );
	}
	if( effects->fireTrail ) {
		unlinkAndFree( effects->fireTrail );
		assert( !effects->fireTrail );
	}
}

void TrackedEffectsSystem::updateStraightLaserBeam( int ownerNum, const float *from, const float *to, int64_t currTime ) {
	assert( ownerNum && ownerNum <= MAX_CLIENTS );
	AttachedClientEffects *effects = &m_attachedClientEffects[ownerNum - 1];
	if( !effects->straightLaserBeam ) {
		effects->straightLaserBeam = cg.polyEffectsSystem.createStraightBeamEffect( cgs.media.shaderLaserGunBeam );
	}

	effects->straightLaserBeamTouchedAt = currTime;
	cg.polyEffectsSystem.updateStraightBeamEffect( effects->straightLaserBeam, colorWhite, 12.0f, 64.0f, from, to );
}

void TrackedEffectsSystem::updateCurvedLaserBeam( int ownerNum, std::span<const vec3_t> points, int64_t currTime ) {
	assert( ownerNum && ownerNum <= MAX_CLIENTS );
	AttachedClientEffects *effects = &m_attachedClientEffects[ownerNum - 1];
	if( !effects->curvedLaserBeam ) {
		effects->curvedLaserBeam = cg.polyEffectsSystem.createCurvedBeamEffect( cgs.media.shaderLaserGunBeam );
	}

	effects->curvedLaserBeamTouchedAt = currTime;
	cg.polyEffectsSystem.updateCurvedBeamEffect( effects->curvedLaserBeam, colorWhite, 12.0f, 64.0f, points );
}

void TrackedEffectsSystem::simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *drawSceneRequest ) {
	// Collect orphans
	for( FireTrail *trail = m_fireTrailsHead, *nextFireTrail = nullptr; trail; trail = nextFireTrail ) {
		nextFireTrail = trail->next;
		if( trail->touchedAt != currTime ) [[unlikely]] {
			unlinkAndFree( trail );
		}
	}

	// Collect orphans
	for( ParticleTrail *trail = m_particleTrailsHead, *nextSmokeTrail = nullptr; trail; trail = nextSmokeTrail ) {
		nextSmokeTrail = trail->next;
		if( trail->touchedAt != currTime ) [[unlikely]] {
			unlinkAndFree( trail );
		}
	}

	// Simulate
	for( TeleEffect *effect = m_teleEffectsHead, *nextEffect = nullptr; effect; effect = nextEffect ) {
		nextEffect = effect->next;
		if( effect->spawnTime + effect->lifetime >= currTime ) [[unlikely]] {
			unlinkAndFree( effect );
			continue;
		}

		const float frac = (float)( currTime - effect->spawnTime ) * Q_Rcp((float)effect->lifetime );
		// TODO: Simulate
		(void)frac;

		// TODO: Submit
	}

	// TODO: Submit fire trails

	// The actual drawing of trails is performed by the particle system

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