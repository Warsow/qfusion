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

#ifndef WSW_17bd35f8_e886_4cc0_a896_33e286002b87_H
#define WSW_17bd35f8_e886_4cc0_a896_33e286002b87_H

#include "trackedeffectssystem.h"
#include "transienteffectssystem.h"
#include "../qcommon/randomgenerator.h"

class DrawSceneRequest;
struct sfx_s;
struct trace_s;

class EffectsSystemFacade {
public:
	void spawnRocketExplosionEffect( const float *origin, const float *impactNormal, int mode );
	void spawnGrenadeExplosionEffect( const float *origin, const float *impactNormal, int mode );
	void spawnShockwaveExplosionEffect( const float *origin, const float *impactNormal, int mode );
	void spawnPlasmaExplosionEffect( const float *origin, const float *impactNormal, int mode );
	void spawnGenericExplosionEffect( const float *origin, int mode, float radius );

	void spawnGrenadeBounceEffect( int entNum, int mode );

	void spawnPlayerHitEffect( const float *origin, const float *dir, int damage );

	void spawnElectroboltHitEffect( const float *origin, const float *dir );
	void spawnInstagunHitEffect( const float *origin, const float *dir, int ownerNum );

	void spawnGunbladeBladeHitEffect( const float *origin, const float *dir );
	void spawnGunbladeBlastHitEffect( const float *origin, const float *dir );

	void spawnBulletImpactEffect( const trace_s *trace ) {
		spawnBulletLikeImpactEffect( trace, 0.10f, 0.60f );
	}

	void spawnPelletImpactEffect( const trace_s *trace ) {
		spawnBulletLikeImpactEffect( trace, 0.0f, 0.0f );
	}

	void spawnLandingDustImpactEffect( const float *origin, const float *dir ) {
		spawnDustImpactEffect( origin, dir, 50.0f );
	}

	void spawnWalljumpDustImpactEffect( const float *origin, const float *dir ) {
		spawnDustImpactEffect( origin, dir, 64.0f );
	}

	void spawnDashEffect( const float *oldOrigin, const float *newOrigin );

	void touchRocketTrail( int entNum, const float *origin, int64_t currTime ) {
		m_trackedEffectsSystem.touchRocketTrail( entNum, origin, currTime );
	}
	void touchGrenadeTrail( int entNum, const float *origin, int64_t currTime ) {
		m_trackedEffectsSystem.touchGrenadeTrail( entNum, origin, currTime );
	}
	void touchPlasmaTrail( int entNum, const float *origin, int64_t currTime ) {
		m_trackedEffectsSystem.touchPlasmaTrail( entNum, origin, currTime );
	}
	void touchBlastTrail( int entNum, const float *origin, int64_t currTime ) {
		m_trackedEffectsSystem.touchBlastTrail( entNum, origin, currTime );
	}
	void touchElectroTrail( int entNum, const float *origin, int64_t currTime ) {
		m_trackedEffectsSystem.touchElectroTrail( entNum, origin, currTime );
	}

	void spawnPlayerTeleInEffect( int entNum, const float *origin, model_s *model ) {
		m_trackedEffectsSystem.spawnPlayerTeleInEffect( entNum, origin, model );
	}
	void spawnPlayerTeleOutEffect( int entNum, const float *origin, model_s *model ) {
		m_trackedEffectsSystem.spawnPlayerTeleOutEffect( entNum, origin, model );
	}

	void resetEntityEffects( int entNum ) { m_trackedEffectsSystem.resetEntityEffects( entNum ); }

	void simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *drawSceneRequest );
private:
	void startSound( sfx_s *sfx, const float *origin, float attenuation );
	void startRelativeSound( sfx_s *sfx, int entNum, float attenuation );

	void spawnExplosionEffect( const float *origin, const float *offset, sfx_s *sfx,
							   const float *color, float radius, bool addSoundLfe );

	void spawnBulletLikeImpactEffect( const trace_s *trace, float minPercentage, float maxPercentage );
	void spawnDustImpactEffect( const float *origin, const float *dir, float radius );

	TrackedEffectsSystem m_trackedEffectsSystem;
	TransientEffectsSystem m_transientEffectsSystem;
	wsw::RandomGenerator m_rng;
};

#endif