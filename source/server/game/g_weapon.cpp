/*
Copyright (C) 1997-2001 Id Software, Inc.

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
#include "g_local.h"
#include <common/facilities/q_collision.h>

// commented out to make gcc happy
#if 0
/*
* W_Fire_Lead
* the seed is important to be as pointer for cgame prediction accuracy
*/
static void W_Fire_Lead( edict_t *self, vec3_t start, vec3_t aimdir, vec3_t axis[3], int damage,
						 int knockback, int stun, int hspread, int vspread, int *seed, int dflags,
						 int mod, int timeDelta ) {
	trace_t tr;
	vec3_t dir;
	vec3_t end;
	float r;
	float u;
	vec3_t water_start;
	int content_mask = MASK_SHOT | MASK_WATER;

	G_Trace4D( &tr, self->s.origin, NULL, NULL, start, self, MASK_SHOT, timeDelta );
	if( !( tr.fraction < 1.0 ) ) {
#if 1

		// circle
		double alpha = M_PI * Q_crandom( seed ); // [-PI ..+PI]
		double s = fabs( Q_crandom( seed ) ); // [0..1]
		r = s * cos( alpha ) * hspread;
		u = s * sin( alpha ) * vspread;
#else

		// square
		r = Q_crandom( seed ) * hspread;
		u = Q_crandom( seed ) * vspread;
#endif
		VectorMA( start, 8192, axis[0], end );
		VectorMA( end, r, axis[1], end );
		VectorMA( end, u, axis[2], end );

		if( G_PointContents4D( start, timeDelta ) & MASK_WATER ) {
			VectorCopy( start, water_start );
			content_mask &= ~MASK_WATER;
		}

		G_Trace4D( &tr, start, NULL, NULL, end, self, content_mask, timeDelta );

		// see if we hit water
		if( tr.contents & MASK_WATER ) {
			VectorCopy( tr.endpos, water_start );

			if( !VectorCompare( start, tr.endpos ) ) {
				vec3_t forward, right, up;

				// change bullet's course when it enters water
				VectorSubtract( end, start, dir );
				VecToAngles( dir, dir );
				AngleVectors( dir, forward, right, up );
#if 1

				// circle
				alpha = M_PI * Q_crandom( seed ); // [-PI ..+PI]
				s = fabs( Q_crandom( seed ) ); // [0..1]
				r = s * cos( alpha ) * hspread * 1.5;
				u = s * sin( alpha ) * vspread * 1.5;
#else
				r = Q_crandom( seed ) * hspread * 2;
				u = Q_crandom( seed ) * vspread * 2;
#endif
				VectorMA( water_start, 8192, forward, end );
				VectorMA( end, r, right, end );
				VectorMA( end, u, up, end );
			}

			// re-trace ignoring water this time
			G_Trace4D( &tr, water_start, NULL, NULL, end, self, MASK_SHOT, timeDelta );
		}
	}

	// send gun puff / flash
	if( tr.fraction < 1.0 && tr.ent != -1 ) {
		if( game.edicts[tr.ent].takedamage ) {
			G_Damage( &game.edicts[tr.ent], self, self, aimdir, aimdir, tr.endpos, tr.plane.normal, damage, knockback, stun, dflags, mod );
		} else {
			if( !( tr.surfFlags & SURF_NOIMPACT ) ) {
			}
		}
	}
}
#endif

enum {
	PROJECTILE_TOUCH_NOT = 0,
	PROJECTILE_TOUCH_DIRECTHIT,
	PROJECTILE_TOUCH_DIRECTAIRHIT,
	PROJECTILE_TOUCH_DIRECTSPLASH // treat direct hits as pseudo-splash impacts
};

/*
*
* - We will consider direct impacts as splash when the player is on the ground and the hit very close to the ground
*/
int G_Projectile_HitStyle( edict_t *projectile, edict_t *target ) {
	trace_t trace;
	vec3_t end;
	bool atGround = false;
	edict_t *attacker;
#define AIRHIT_MINHEIGHT 64

	// don't hurt owner for the first second
	if( target == projectile->r.owner && target != world ) {
		if( !g_projectile_touch_owner->integer ||
			( g_projectile_touch_owner->integer && projectile->timeStamp + 1000 > level.time ) ) {
			return PROJECTILE_TOUCH_NOT;
		}
	}

	if( !target->takedamage || ISBRUSHMODEL( target->s.modelindex ) ) {
		return PROJECTILE_TOUCH_DIRECTHIT;
	}

	if( target->waterlevel > 1 ) {
		return PROJECTILE_TOUCH_DIRECTHIT; // water hits are direct but don't count for awards

	}
	attacker = ( projectile->r.owner && projectile->r.owner->r.client ) ? projectile->r.owner : NULL;

	// see if the target is at ground or a less than a step of height
	if( target->groundentity ) {
		atGround = true;
	} else {
		VectorCopy( target->s.origin, end );
		end[2] -= STEPSIZE;

		G_Trace4D( &trace, target->s.origin, target->r.mins, target->r.maxs, end, target, MASK_DEADSOLID, 0 );
		if( ( trace.ent != -1 || trace.startsolid ) && ISWALKABLEPLANE( &trace.plane ) ) {
			atGround = true;
		}
	}

	if( atGround ) {
		// when the player is at ground we will consider a direct hit only when
		// the hit is 16 units above the feet
		if( projectile->s.origin[2] <= 16 + target->s.origin[2] + target->r.mins[2] ) {
			return PROJECTILE_TOUCH_DIRECTSPLASH;
		}
	} else {
		// it's direct hit, but let's see if it's airhit
		VectorCopy( target->s.origin, end );
		end[2] -= AIRHIT_MINHEIGHT;

		G_Trace4D( &trace, target->s.origin, target->r.mins, target->r.maxs, end, target, MASK_DEADSOLID, 0 );
		if( ( trace.ent != -1 || trace.startsolid ) && ISWALKABLEPLANE( &trace.plane ) ) {
			// add directhit and airhit to awards counter
			if( attacker && !GS_IsTeamDamage( ggs, &attacker->s, &target->s ) && G_ModToAmmo( projectile->style ) != AMMO_NONE ) {
				projectile->r.owner->r.client->stats.accuracy_hits_direct[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;
				teamlist[projectile->r.owner->s.team].stats.accuracy_hits_direct[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;

				projectile->r.owner->r.client->stats.accuracy_hits_air[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;
				teamlist[projectile->r.owner->s.team].stats.accuracy_hits_air[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;
			}

			return PROJECTILE_TOUCH_DIRECTAIRHIT;
		}
	}

	// add directhit to awards counter
	if( attacker && !GS_IsTeamDamage( ggs, &attacker->s, &target->s ) && G_ModToAmmo( projectile->style ) != AMMO_NONE ) {
		projectile->r.owner->r.client->stats.accuracy_hits_direct[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;
		teamlist[projectile->r.owner->s.team].stats.accuracy_hits_direct[G_ModToAmmo( projectile->style ) - AMMO_GUNBLADE]++;
	}

	return PROJECTILE_TOUCH_DIRECTHIT;

#undef AIRHIT_MINHEIGHT
}

/*
* W_Touch_Projectile - Generic projectile touch func. Only for replacement in tests
*/
static void W_Touch_Projectile( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	vec3_t dir, normal;
	int hitType;

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	if( other->takedamage ) {
		VectorNormalize2( ent->velocity, dir );

		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		} else {
			VectorNormalize2( ent->velocity, dir );
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, 0, ent->style );
	}

	G_RadiusDamage( ent, ent->r.owner, plane, other, MOD_EXPLOSIVE );

	if( !plane ) {
		VectorSet( normal, 0, 0, 1 );
	} else {
		VectorCopy( plane->normal, normal );
	}

	G_Gametype_ScoreEvent( NULL, "projectilehit", va( "%i %i %f %f %f", ent->s.number, surfFlags, normal[0], normal[1], normal[2] ) );
}

/*
* W_Fire_LinearProjectile - Spawn a generic linear projectile without a model, touch func, sound nor mod
*/
static edict_t *W_Fire_LinearProjectile( edict_t *self, vec3_t start, vec3_t angles, int speed,
										 float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int timeout, int timeDelta ) {
	edict_t *projectile;
	vec3_t dir;

	projectile = G_Spawn();
	VectorCopy( start, projectile->s.origin );
	VectorCopy( start, projectile->olds.origin );

	VectorCopy( angles, projectile->s.angles );
	AngleVectors( angles, dir, NULL, NULL );
	VectorScale( dir, speed, projectile->velocity );

	projectile->movetype = MOVETYPE_LINEARPROJECTILE;

	projectile->r.solid = SOLID_YES;
	projectile->r.clipmask = ( !GS_RaceGametype( *ggs ) ) ? MASK_SHOT : MASK_SOLID;

	projectile->r.svflags = SVF_PROJECTILE;

	// enable me when drawing exception is added to cgame
	VectorClear( projectile->r.mins );
	VectorClear( projectile->r.maxs );
	projectile->s.modelindex = 0;
	projectile->r.owner = self;
	projectile->s.ownerNum = ENTNUM( self );
	projectile->touch = W_Touch_Projectile; //generic one. Should be replaced after calling this func
	projectile->nextThink = level.time + timeout;
	projectile->think = G_FreeEdict;
	projectile->classname = NULL; // should be replaced after calling this func.
	projectile->style = 0;
	projectile->s.sound = 0;
	projectile->timeStamp = level.time;
	projectile->timeDelta = timeDelta;

	projectile->projectileInfo.minDamage = wsw::min( (float)minDamage, damage );
	projectile->projectileInfo.maxDamage = damage;
	projectile->projectileInfo.minKnockback = wsw::min( minKnockback, maxKnockback );
	projectile->projectileInfo.maxKnockback = maxKnockback;
	projectile->projectileInfo.stun = stun;
	projectile->projectileInfo.radius = radius;

	GClip_LinkEntity( projectile );

	// update some data required for the transmission
	projectile->s.linearMovement = true;
	VectorCopy( projectile->s.origin, projectile->s.linearMovementBegin );
	VectorCopy( projectile->velocity, projectile->s.linearMovementVelocity );
	projectile->s.linearMovementTimeStamp = game.serverTime;
	projectile->s.linearMovementPrevServerTime = game.serverTime;
	projectile->s.team = self->s.team;
	projectile->s.modelindex2 = ( abs( timeDelta ) > 255 ) ? 255 : (unsigned int)abs( timeDelta );
	return projectile;
}

/*
* W_Fire_TossProjectile - Spawn a generic projectile without a model, touch func, sound nor mod
*/
static edict_t *W_Fire_TossProjectile( edict_t *self, vec3_t start, vec3_t angles, int speed,
									   float damage, int minKnockback, int maxKnockback, int stun, int minDamage,
									   int radius, int timeout, int timeDelta, float gravityScale = 1.0f ) {
	edict_t *projectile;
	vec3_t dir;

	projectile = G_Spawn();
	VectorCopy( start, projectile->s.origin );
	VectorCopy( start, projectile->olds.origin );

	VectorCopy( angles, projectile->s.angles );
	AngleVectors( angles, dir, NULL, NULL );
	VectorScale( dir, speed, projectile->velocity );

	projectile->movetype = MOVETYPE_BOUNCEGRENADE;

	// make missile fly through players in race
	if( GS_RaceGametype( *ggs ) ) {
		projectile->r.clipmask = MASK_SOLID;
	} else {
		projectile->r.clipmask = MASK_SHOT;
	}

	projectile->r.solid = SOLID_YES;
	projectile->r.svflags = SVF_PROJECTILE;
	VectorClear( projectile->r.mins );
	VectorClear( projectile->r.maxs );

	//projectile->s.modelindex = SV_ModelIndex ("models/objects/projectile/plasmagun/proj_plasmagun2.md3");
	projectile->s.modelindex = 0;
	projectile->r.owner = self;
	projectile->touch = W_Touch_Projectile; //generic one. Should be replaced after calling this func
	projectile->nextThink = level.time + timeout;
	projectile->timeout = level.time + timeout;
	projectile->think = G_FreeEdict;
	projectile->classname = NULL; // should be replaced after calling this func.
	projectile->style = 0;
	projectile->s.sound = 0;
	projectile->timeStamp = level.time;
	projectile->timeDelta = 0;
	projectile->s.team = self->s.team;

	projectile->gravity *= gravityScale;

	projectile->projectileInfo.minDamage = wsw::min( (float)minDamage, damage );
	projectile->projectileInfo.maxDamage = damage;
	projectile->projectileInfo.minKnockback = wsw::min( minKnockback, maxKnockback );
	projectile->projectileInfo.maxKnockback = maxKnockback;
	projectile->projectileInfo.stun = stun;
	projectile->projectileInfo.radius = radius;

	GClip_LinkEntity( projectile );

	return projectile;
}


//	------------ the actual weapons --------------


/*
* W_Fire_Blade
*/
void W_Fire_Blade( edict_t *self, int range, vec3_t start, vec3_t angles, float damage, int knockback, int stun, int mod, int timeDelta ) {
	edict_t *event, *other = NULL;
	vec3_t end;
	trace_t trace;
	int mask = MASK_SHOT;
	vec3_t dir;
	int dmgflags = 0;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );
	VectorMA( start, range, dir, end );

	if( GS_RaceGametype( *ggs ) ) {
		mask = MASK_SOLID;
	}

	G_Trace4D( &trace, start, NULL, NULL, end, self, mask, timeDelta );
	if( trace.ent == -1 ) { //didn't touch anything
		return;
	}

	// find out what touched
	other = &game.edicts[trace.ent];
	if( !other->takedamage ) { // it was the world
		// wall impact
		VectorMA( trace.endpos, -0.02, dir, end );
		event = G_SpawnEvent( EV_BLADE_IMPACT, 0, end );
		event->s.ownerNum = ENTNUM( self );
		VectorScale( trace.plane.normal, 1024, event->s.origin2 );
		return;
	}

	// it was a player
	G_Damage( other, self, self, dir, dir, other->s.origin, damage, knockback, stun, dmgflags, mod );
}

/*
* W_Touch_GunbladeBlast
*/
static void W_Touch_GunbladeBlast( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	vec3_t dir;
	int hitType;

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	if( other->takedamage ) {
		VectorNormalize2( ent->velocity, dir );

		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		} else {
			VectorNormalize2( ent->velocity, dir );
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, 0, ent->style );
	}

	G_RadiusDamage( ent, ent->r.owner, plane, other, MOD_GUNBLADE_S );

	vec3_t eventDir;

	const bool looksLikeFlesh = other->takedamage && !ISBRUSHMODEL( other->s.modelindex );
	if( looksLikeFlesh || !plane ) {
		VectorNegate( ent->velocity, eventDir );
	} else {
		VectorCopy( plane->normal, eventDir );
	}

	edict_t *event = G_SpawnEvent( EV_GUNBLADEBLAST_IMPACT, DirToByte( eventDir ), ent->s.origin );
	event->s.weapon = ( ( ent->projectileInfo.radius * 1 / 8 ) > 127 ) ? 127 : ( ent->projectileInfo.radius * 1 / 8 );
	event->s.skinnum = ( ( ent->projectileInfo.maxKnockback * 1 / 8 ) > 255 ) ? 255 : ( ent->projectileInfo.maxKnockback * 1 / 8 );

	// free at next frame
	G_FreeEdict( ent );
}

/*
* W_Fire_GunbladeBlast
*/
edict_t *W_Fire_GunbladeBlast( edict_t *self, vec3_t start, vec3_t angles, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int speed, int timeout, int mod, int timeDelta ) {
	edict_t *blast;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	blast = W_Fire_LinearProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, minDamage, radius, timeout, timeDelta );
	blast->s.modelindex = SV_ModelIndex( PATH_GUNBLADEBLAST_STRONG_MODEL );
	blast->s.type = ET_BLASTER;
	blast->s.effects |= EF_STRONG_WEAPON;
	blast->touch = W_Touch_GunbladeBlast;
	blast->classname = "gunblade_blast";
	blast->style = mod;

	blast->s.sound = SV_SoundIndex( S_WEAPON_PLASMAGUN_S_FLY );
	blast->s.attenuation = ATTN_STATIC;

	return blast;
}

/*
* W_Fire_Bullet
*/
void W_Fire_Bullet( edict_t *self, vec3_t start, vec3_t angles, int seed, int range, int hspread, int vspread,
					float damage, int knockback, int stun, int mod, int timeDelta ) {
	vec3_t dir;
	edict_t *event;
	float r, u;
	double alpha, s;
	trace_t trace, waterTrace;
	int dmgflags = DAMAGE_STUN_CLAMP | DAMAGE_KNOCKBACK_SOFT;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );

	// send the event
	event = G_SpawnEvent( EV_FIRE_BULLET, seed, start );
	event->s.ownerNum = ENTNUM( self );
	VectorScale( dir, 4096, event->s.origin2 ); // DirToByte is too inaccurate
	event->s.weapon = WEAP_MACHINEGUN;
	event->s.firemode = ( mod == MOD_MACHINEGUN_S ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;

	// circle shape
	alpha = M_PI * Q_crandom( &seed ); // [-PI ..+PI]
	s = fabs( Q_crandom( &seed ) ); // [0..1]
	r = s * cos( alpha ) * hspread;
	u = s * sin( alpha ) * vspread;

	GS_TraceBullet( ggs, &trace, &waterTrace, start, dir, r, u, range, ENTNUM( self ), timeDelta );

	if( trace.ent != -1 ) {
		if( game.edicts[trace.ent].takedamage ) {
			G_Damage( &game.edicts[trace.ent], self, self, dir, dir, trace.endpos, damage, knockback, stun, dmgflags, mod );
		} else {
			if( !( trace.surfFlags & SURF_NOIMPACT ) ) {
			}
		}
	}
}

//Sunflower spiral with Fibonacci numbers
static void G_Fire_SunflowerPattern( edict_t *self, vec3_t start, vec3_t dir, int *seed, int count,
									 int hspread, int vspread, int range, float damage, int kick, int stun, int dflags, int mod, int timeDelta ) {
	int i;
	float r;
	float u;
	float fi;
	trace_t trace, waterTrace;

	for( i = 0; i < count; i++ ) {
		fi = i * 2.4; //magic value creating Fibonacci numbers
		r = cos( (float)*seed + fi ) * hspread * sqrt( fi );
		u = sin( (float)*seed + fi ) * vspread * sqrt( fi );

		GS_TraceBullet( ggs, &trace, &waterTrace, start, dir, r, u, range, ENTNUM( self ), timeDelta );

		if( trace.ent != -1 ) {
			if( game.edicts[trace.ent].takedamage ) {
				G_Damage( &game.edicts[trace.ent], self, self, dir, dir, trace.endpos, damage, kick, stun, dflags, mod );
			} else {
				if( !( trace.surfFlags & SURF_NOIMPACT ) ) {
				}
			}
		}
	}
}

#if 0
static void G_Fire_RandomPattern( edict_t *self, vec3_t start, vec3_t dir, int *seed, int count,
								  int hspread, int vspread, int range, float damage, int kick, int stun, int dflags, int mod, int timeDelta ) {
	int i;
	float r;
	float u;
	trace_t trace;

	for( i = 0; i < count; i++ ) {
		r = Q_crandom( seed ) * hspread;
		u = Q_crandom( seed ) * vspread;

		GS_TraceBullet( &trace, start, dir, r, u, range, ENTNUM( self ), timeDelta );
		if( trace.ent != -1 ) {
			if( game.edicts[trace.ent].takedamage ) {
				G_Damage( &game.edicts[trace.ent], self, self, dir, dir, trace.endpos, damage, kick, stun, dflags, mod );
			} else {
				if( !( trace.surfFlags & SURF_NOIMPACT ) ) {
				}
			}
		}
	}
}
#endif

void W_Fire_Riotgun( edict_t *self, vec3_t start, vec3_t angles, int seed, int range, int hspread, int vspread,
					 int count, float damage, int knockback, int stun, int mod, int timeDelta ) {
	vec3_t dir;
	edict_t *event;
	int dmgflags = 0;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );

	// send the event
	event = G_SpawnEvent( EV_FIRE_RIOTGUN, seed, start );
	event->s.ownerNum = ENTNUM( self );
	VectorScale( dir, 4096, event->s.origin2 ); // DirToByte is too inaccurate
	event->s.weapon = WEAP_RIOTGUN;
	event->s.firemode = ( mod == MOD_RIOTGUN_S ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;

	G_Fire_SunflowerPattern( self, start, dir, &seed, count, hspread, vspread,
							 range, damage, knockback, stun, dmgflags, mod, timeDelta );
}

/*
* W_Grenade_ExplodeDir
*/
static void W_Grenade_ExplodeDir( edict_t *ent, vec3_t normal, const edict_t *ignore ) {
	vec3_t origin;
	int radius;
	edict_t *event;
	vec3_t up = { 0, 0, 1 };
	vec_t *dir = normal ? normal : up;

	const int mod = ( ent->s.effects & EF_STRONG_WEAPON ) ? MOD_GRENADE_SPLASH_S : MOD_GRENADE_SPLASH_W;
	G_RadiusDamage( ent, ent->r.owner, NULL, ignore, mod );

	radius = ( ( ent->projectileInfo.radius * 1 / 8 ) > 127 ) ? 127 : ( ent->projectileInfo.radius * 1 / 8 );
	VectorMA( ent->s.origin, -0.02, ent->velocity, origin );
	event = G_SpawnEvent( EV_GRENADE_EXPLOSION, ( dir ? DirToByte( dir ) : 0 ), ent->s.origin );
	event->s.firemode = ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
	event->s.weapon = radius;

	G_FreeEdict( ent );
}

/*
* W_Grenade_Explode
*/
static void W_Grenade_Explode( edict_t *ent ) {
	W_Grenade_ExplodeDir( ent, NULL, NULL );
}

void W_Detonate_Grenade( edict_t *ent, const edict_t *ignore ) {
	W_Grenade_ExplodeDir( ent, NULL, ignore );
}

[[nodiscard]]
static bool isGrenadeEnemySuitable( const edict_t *grenade, const edict_t *enemy ) {
	return enemy && !G_ISGHOSTING( enemy ) && enemy->s.number != grenade->s.ownerNum;
}

[[nodiscard]]
static bool isGrenadeEnemyReachable( const edict_t *grenade, const edict_t *enemy,
									 float *outSquareDistance = nullptr,
									 float bestSquareDistanceSoFar = std::numeric_limits<float>::max() ) {
	const float distance = DistanceSquared( grenade->s.origin, enemy->s.origin );
	if( distance > wsw::square( 1.0f ) && distance < wsw::square( 192.0f ) && distance < bestSquareDistanceSoFar ) {
		trace_t trace;
		G_Trace( &trace, grenade->s.origin, nullptr, nullptr, enemy->s.origin, grenade, MASK_PLAYERSOLID );
		if( trace.ent < 0 || trace.ent == enemy->s.number ) {
			if( outSquareDistance ) {
				*outSquareDistance = distance;
			}
			return true;
		}
	}
	return false;
}

[[nodiscard]]
static auto findGrenadeEnemy( const edict_t *grenade ) -> edict_t * {
	g_teamlist_t *list;
	if( GS_TeamBasedGametype( *ggs ) ) {
		const int otherTeam = grenade->s.team == TEAM_ALPHA ? TEAM_BETA : TEAM_ALPHA;
		list = &::teamlist[otherTeam];
	} else {
		list = &::teamlist[TEAM_PLAYERS];
	}

	int bestEntNum           = -1;
	float bestSquareDistance = std::numeric_limits<float>::max();
	for( int i = 0; i < list->numplayers; ++i ) {
		const int entNum = list->playerIndices[i];
		edict_t *const ent = game.edicts + entNum;
		if( isGrenadeEnemySuitable( grenade, ent ) ) {
			float outSquareDistance = std::numeric_limits<float>::max();
			if( isGrenadeEnemyReachable( grenade, ent, &outSquareDistance, bestSquareDistance ) ) {
				bestSquareDistance = outSquareDistance;
				bestEntNum         = entNum;
			}
		}
	}

	return bestEntNum > 0 ? game.edicts + bestEntNum : nullptr;
}

static void W_Grenade_Think_Leap( edict_t *grenade ) {
	assert( grenade->enemy );
	assert( grenade->s.effects |= EF_ACTIVATED );
	if( !isGrenadeEnemySuitable( grenade, grenade->enemy ) || grenade->timeout <= level.time ) {
		W_Grenade_Explode( grenade );
	} else {
		if( isGrenadeEnemyReachable( grenade, grenade->enemy ) ) {
			vec3_t dir;
			VectorSubtract( grenade->enemy->s.origin, grenade->s.origin, dir );
			VectorNormalize( dir );
			// Don't let it push down
			dir[2] = std::max( dir[2], 0.001f );
			constexpr float accel = 750.0f;
			const float deltaSpeed = accel * ( 0.001f * game.frametime );
			VectorMA( grenade->velocity, deltaSpeed, dir, grenade->velocity );
			grenade->velocity[2] = std::max( grenade->velocity[2], 25.0f );
		}
		grenade->nextThink = level.time + 1;
	}
}

static void W_Grenade_Think_Scan( edict_t *grenade ) {
	assert( grenade->s.effects & EF_ARMED );
	if( grenade->timeout <= level.time ) {
		W_Grenade_Explode( grenade );
	} else {
		if( edict_t *enemy = findGrenadeEnemy( grenade ) ) {
			grenade->enemy = enemy;
			grenade->s.effects |= EF_ACTIVATED;
			grenade->velocity[2] = std::max( grenade->velocity[2], 175.0f );
			grenade->timeout = level.time + 500;
			grenade->think = W_Grenade_Think_Leap;
			G_AddEvent( grenade, EV_GRENADE_ACTIVATE, 0, true );
		}
		grenade->nextThink = level.time + 1;
	}
}

static void W_Grenade_Stop( edict_t *ent ) {
	if( ( ent->s.effects & EF_STRONG_WEAPON ) && ( ent->think == W_Grenade_Explode ) ) {
		if( ent->groundentity && VectorLengthSquared( ent->velocity ) < wsw::square( 300.0f ) ) {
			ent->s.effects |= EF_ARMED;
			ent->timeout += 1000;
			ent->think = W_Grenade_Think_Scan;
			ent->nextThink = level.time + 1;
			G_AddEvent( ent, EV_GRENADE_ARM, 0, true );
		}
	}
}

/*
* W_Touch_Grenade
*/
static void W_Touch_Grenade( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	int hitType;
	vec3_t dir;

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	// don't explode on doors and plats that take damage
	// except for doors and plats that take damage in race
	if( !other->takedamage || ( !GS_RaceGametype( *ggs ) && ISBRUSHMODEL( other->s.modelindex ) ) ) {
		ent->count++;
		if( !GS_RaceGametype( *ggs ) || ( ent->s.effects & EF_STRONG_WEAPON ) ) {
			// kill some velocity on each bounce
			float friction = 0.85f;
			const gs_weapon_definition_t *weapondef = GS_GetWeaponDef( ggs, WEAP_GRENADELAUNCHER );
			if( weapondef && weapondef->firedef.friction > 0 ) {
				friction = bound( 0, weapondef->firedef.friction, 2 );
			}
			VectorScale( ent->velocity, friction, ent->velocity );
		}
		// race - make grenades bounce twice
		if( GS_RaceGametype( *ggs ) ) {
			if( ent->s.effects & EF_STRONG_WEAPON ) {
				ent->health -= 1;
			}
			if( !( ent->s.effects & EF_STRONG_WEAPON ) || ( ( VectorLength( ent->velocity ) && ent->count < 2 ) || ent->timeStamp + 350 > level.time ) ) {
				G_AddEvent( ent, EV_GRENADE_BOUNCE, ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK, true );
				return;
			}
		} else {
			G_AddEvent( ent, EV_GRENADE_BOUNCE, ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK, true );
			return;
		}
	}

	if( other->takedamage ) {
		int directHitDamage = ent->projectileInfo.maxDamage;

		VectorNormalize2( ent->velocity, dir );

		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		} else {
			VectorNormalize2( ent->velocity, dir );

			// no direct hit bonuses for grenades
			/*
			if( hitType == PROJECTILE_TOUCH_DIRECTAIRHIT )
			    directHitDamage += DIRECTAIRTHIT_DAMAGE_BONUS;
			else if( hitType == PROJECTILE_TOUCH_DIRECTHIT )
			    directHitDamage += DIRECTHIT_DAMAGE_BONUS;
			*/
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, directHitDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, 0, ent->style );
	}

	ent->enemy = other;
	W_Grenade_ExplodeDir( ent, plane ? plane->normal : NULL, nullptr );
}

/*
* W_Fire_Grenade
*/
edict_t *W_Fire_Grenade( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage,
						 int minKnockback, int maxKnockback, int stun, int minDamage, float radius,
						 int timeout, int mod, int timeDelta, bool aim_up ) {
	edict_t *grenade;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	if( aim_up ) {
		angles[PITCH] -= 5.0f * cos( DEG2RAD( angles[PITCH] ) ); // aim some degrees upwards from view dir

	}
	grenade = W_Fire_TossProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, minDamage, radius, timeout, timeDelta );
	VectorClear( grenade->s.angles );
	grenade->style = mod;
	grenade->s.type = ET_GRENADE;
	grenade->r.owner = self;
	grenade->s.ownerNum = ENTNUM( self );
	grenade->movetype = MOVETYPE_BOUNCEGRENADE;
	grenade->touch = W_Touch_Grenade;
	grenade->use = NULL;
	grenade->think = W_Grenade_Explode;
	grenade->stop = W_Grenade_Stop;
	grenade->classname = "grenade";
	grenade->enemy = NULL;
	if( GS_RaceGametype( *ggs ) ) {
		const gs_weapon_definition_t *weapondef = GS_GetWeaponDef( ggs, WEAP_GRENADELAUNCHER );
		if( weapondef->firedef.gravity ) {
			grenade->gravity = weapondef->firedef.gravity;
		}
	}

	if( mod == MOD_GRENADE_S ) {
		grenade->s.modelindex = SV_ModelIndex( PATH_GRENADE_STRONG_MODEL );
		grenade->s.effects |= EF_STRONG_WEAPON;
	} else {
		grenade->s.modelindex = SV_ModelIndex( PATH_GRENADE_WEAK_MODEL );
		grenade->s.effects &= ~EF_STRONG_WEAPON;
	}

	GClip_LinkEntity( grenade );

	return grenade;
}

/*
* W_Touch_Rocket
*/
static void W_Touch_Rocket( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	const int hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	if( other->takedamage ) {
		vec3_t dir;
		VectorNormalize2( ent->velocity, dir );
		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, 0, ent->style );
	}

	W_Detonate_Rocket( ent, other, plane, surfFlags );
}

void W_Detonate_Rocket( edict_t *ent, const edict_t *ignore, const cplane_t *plane, int surfFlags ) {
	int mod = ( ent->s.effects & EF_STRONG_WEAPON ) ? MOD_ROCKET_SPLASH_S : MOD_ROCKET_SPLASH_W;
	G_RadiusDamage( ent, ent->r.owner, plane, ignore, mod );

	// spawn the explosion
	if( !( surfFlags & SURF_NOIMPACT ) ) {
		edict_t *event;
		vec3_t explosion_origin;

		VectorMA( ent->s.origin, -0.02, ent->velocity, explosion_origin );
		event = G_SpawnEvent( EV_ROCKET_EXPLOSION, DirToByte( plane ? plane->normal : NULL ), explosion_origin );
		event->s.firemode = ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
		event->s.weapon = ( ( ent->projectileInfo.radius * 1 / 8 ) > 255 ) ? 255 : ( ent->projectileInfo.radius * 1 / 8 );
	}

	// free the rocket at next frame
	G_FreeEdict( ent );
}

static void W_Think_Plasma( edict_t * );

static void convertToTossProjectile( edict_t *self ) {
	self->s.linearMovement = false;
	self->movetype = MOVETYPE_BOUNCEGRENADE;
	if( self->s.type == ET_ROCKET ) {
		self->nextThink = self->timeout;
		self->think = G_FreeEdict;
		self->gravity = g_rocket_gravity_scale->value;
	} else {
		self->think = W_Think_Plasma;
		self->gravity = g_plasma_gravity_scale->value;
		// Add instant up velocity boost, this is more important for such fast projectiles.
		// From the other hand, it has to be sufficiently small to keep the curve visually smooth.
		self->velocity[2] += g_plasma_up_speed_boost->value;
	}
}

/*
* W_Fire_Rocket
*/
edict_t *W_Fire_Rocket( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int timeout, int mod, int timeDelta ) {
	edict_t *rocket;

	// in race, rockets are slower in water
	if( GS_RaceGametype( *ggs ) ) {
		speed = self->waterlevel > 1 ? speed * 0.5 : speed;
	}

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	rocket = W_Fire_LinearProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, minDamage, radius, timeout, timeDelta );
	rocket->s.type = ET_ROCKET; //rocket trail sfx
	if( mod == MOD_ROCKET_S ) {
		rocket->s.modelindex = SV_ModelIndex( PATH_ROCKET_STRONG_MODEL );
		rocket->s.effects |= EF_STRONG_WEAPON;
		rocket->s.sound = SV_SoundIndex( S_WEAPON_ROCKET_S_FLY );
		rocket->think = G_FreeEdict;
		rocket->nextThink = level.time + timeout;
	} else {
		rocket->s.modelindex = SV_ModelIndex( PATH_ROCKET_WEAK_MODEL );
		rocket->s.effects &= ~EF_STRONG_WEAPON;
		rocket->s.sound = SV_SoundIndex( S_WEAPON_ROCKET_W_FLY );
		rocket->think     = convertToTossProjectile;
		rocket->nextThink = level.time + wsw::max( 2 * game.snapFrameTime, g_projectile_toss_morph_delay->integer );
	}

	rocket->s.attenuation = ATTN_STATIC;
	rocket->touch = W_Touch_Rocket;
	rocket->classname = "rocket";
	rocket->style = mod;
	// Set it for strong projectile as well for consistency
	rocket->timeout = level.time + timeout;

	return rocket;
}

static void W_Plasma_Explosion( edict_t *ent, edict_t *ignore, cplane_t *plane, int surfFlags ) {
	edict_t *event;
	int radius = ( ( ent->projectileInfo.radius * 1 / 8 ) > 127 ) ? 127 : ( ent->projectileInfo.radius * 1 / 8 );

	event = G_SpawnEvent( EV_PLASMA_EXPLOSION, DirToByte( plane ? plane->normal : NULL ), ent->s.origin );
	event->s.firemode = ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
	event->s.weapon = radius & 127;
	event->s.ownerNum = ENTNUM( ent->r.owner ); // race related, shouldn't matter for basewsw

	G_RadiusDamage( ent, ent->r.owner, plane, ignore, ent->style );

	G_FreeEdict( ent );
}

/*
* W_Touch_Plasma
*/
static void W_Touch_Plasma( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	int hitType;
	vec3_t dir;

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	if( other->takedamage ) {
		VectorNormalize2( ent->velocity, dir );

		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		} else {
			VectorNormalize2( ent->velocity, dir );
		}

		// race - hack for plasma shooters which shoot on buttons
		if( GS_RaceGametype( *ggs ) && (surfFlags & SURF_NOIMPACT) ) {
			G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, 0, 0, DAMAGE_NO_KNOCKBACK, ent->style );
			G_FreeEdict( ent );
			return;
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, DAMAGE_KNOCKBACK_SOFT, ent->style );
	}

	W_Plasma_Explosion( ent, other, plane, surfFlags );
}

/*
* W_Plasma_Backtrace
*/
void W_Plasma_Backtrace( edict_t *ent, const vec3_t start ) {
	trace_t tr;
	vec3_t oldorigin;
	vec3_t mins = { -2, -2, -2 }, maxs = { 2, 2, 2 };

	if( GS_RaceGametype( *ggs ) ) {
		return;
	}

	VectorCopy( ent->s.origin, oldorigin );
	VectorCopy( start, ent->s.origin );

	do {
		G_Trace4D( &tr, ent->s.origin, mins, maxs, oldorigin, ent, ( CONTENTS_BODY | CONTENTS_CORPSE ), ent->timeDelta );

		VectorCopy( tr.endpos, ent->s.origin );

		if( tr.ent == -1 ) {
			break;
		}
		if( tr.allsolid || tr.startsolid ) {
			W_Touch_Plasma( ent, &game.edicts[tr.ent], NULL, 0 );
		} else if( tr.fraction != 1.0 ) {
			W_Touch_Plasma( ent, &game.edicts[tr.ent], &tr.plane, tr.surfFlags );
		} else {
			break;
		}
	} while( ent->r.inuse && ent->s.type == ET_PLASMA && !VectorCompare( ent->s.origin, oldorigin ) );

	if( ent->r.inuse && ent->s.type == ET_PLASMA ) {
		VectorCopy( oldorigin, ent->s.origin );
	}
}

/*
* W_Think_Plasma
*/
static void W_Think_Plasma( edict_t *ent ) {
	vec3_t start;

	if( ent->timeout < level.time ) {
		G_FreeEdict( ent );
		return;
	}

	if( ent->r.inuse ) {
		ent->nextThink = level.time + 1;
	}

	VectorMA( ent->s.origin, -( game.frametime * 0.001 ), ent->velocity, start );

	W_Plasma_Backtrace( ent, start );
}

static void W_Think_Plasma2( edict_t *ent ) {
	if( ent->trigger_timeout < level.time ) {
		convertToTossProjectile( ent );
	}

	W_Think_Plasma( ent );
}

/*
* W_AutoTouch_Plasma
*/
static void W_AutoTouch_Plasma( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	W_Think_Plasma( ent );
	if( !ent->r.inuse || ent->s.type != ET_PLASMA ) {
		return;
	}

	W_Touch_Plasma( ent, other, plane, surfFlags );
}

/*
* W_Fire_Plasma
*/
edict_t *W_Fire_Plasma( edict_t *self, vec3_t start, vec3_t angles, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int speed, int timeout, int mod, int timeDelta ) {
	edict_t *plasma;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	plasma = W_Fire_LinearProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, minDamage, radius, timeout, timeDelta );

	plasma->s.type = ET_PLASMA;
	plasma->classname = "plasma";
	plasma->style = mod;

	plasma->touch = W_AutoTouch_Plasma;
	plasma->nextThink = level.time + 1;
	plasma->timeout = level.time + timeout;

	if( mod == MOD_PLASMA_S ) {
		plasma->s.modelindex = SV_ModelIndex( PATH_PLASMA_STRONG_MODEL );
		plasma->s.sound = SV_SoundIndex( S_WEAPON_PLASMAGUN_S_FLY );
		plasma->s.effects |= EF_STRONG_WEAPON;
		plasma->think = W_Think_Plasma;
	} else {
		plasma->s.modelindex = SV_ModelIndex( PATH_PLASMA_WEAK_MODEL );
		plasma->s.sound = SV_SoundIndex( S_WEAPON_PLASMAGUN_W_FLY );
		plasma->s.effects &= ~EF_STRONG_WEAPON;
		plasma->think = W_Think_Plasma2;
		plasma->trigger_timeout = level.time + wsw::max( 2 * game.snapFrameTime, g_projectile_toss_morph_delay->integer );
	}

	plasma->s.attenuation = ATTN_STATIC;
	return plasma;
}

/*
* W_Touch_Bolt
*/
static void W_Touch_Bolt( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags ) {
	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( self );
		return;
	}

	if( other == self->enemy ) {
		return;
	}

	const int hitType = G_Projectile_HitStyle( self, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	bool missed = true;
	edict_t *event = nullptr;
	if( other->takedamage ) {
		G_Damage( other, self, self->r.owner, self->velocity, self->velocity, self->s.origin, self->projectileInfo.maxDamage, self->projectileInfo.maxKnockback, self->projectileInfo.stun, 0, MOD_ELECTROBOLT_W );

		vec3_t invDir;
		VectorNormalize2( self->velocity, invDir );
		const int impactDirByte = DirToByte( invDir );

		VectorScale( invDir, -1, invDir );

		const int normalByte = DirToByte( invDir );
		const int parmByte   = ( impactDirByte << 8 ) | normalByte;

		event = G_SpawnEvent( EV_BOLT_EXPLOSION, parmByte, self->s.origin );
		event->s.firemode = FIRE_MODE_WEAK;
		if( self->r.owner ) {
			event->s.ownerNum = self->r.owner->s.number;
		}

		if( other->r.client ) {
			missed = false;
		}
	} else if( !( surfFlags & SURF_NOIMPACT ) ) {
		int parmByte = 0;
		if( plane ) {
			if( const float squareLen = VectorLengthSquared( self->velocity ); squareLen > 1.0f ) {
				const float rcpLen = Q_RSqrt( squareLen );
				vec3_t velocityDir;
				VectorScale( self->velocity, rcpLen, velocityDir );
				const int impactDirByte = DirToByte( velocityDir );
				const int normalByte    = DirToByte( plane->normal );
				const int decalByte  = ( surfFlags & ( SURF_FLESH | SURF_NOMARKS ) ) ? 0 : 1;
				parmByte = ( decalByte << 16 ) | ( impactDirByte << 8 ) | normalByte;
			}
		}

		// add explosion event
		event = G_SpawnEvent( EV_BOLT_EXPLOSION, parmByte, self->s.origin );
		event->s.firemode = FIRE_MODE_WEAK;
		if( self->r.owner ) {
			event->s.ownerNum = self->r.owner->s.number;
		}
	}

	if( event ) {
		event->s.ownerNum = ENTNUM( self->r.owner ); // race related, shouldn't matter for basewsw
	}

	if( missed && self->r.client ) {
		G_AwardPlayerMissedElectrobolt( self->r.owner, MOD_ELECTROBOLT_W ); // hit something that isnt a player

	}
	G_FreeEdict( self );
}

/*
* W_Fire_Electrobolt_Combined
*/
void W_Fire_Electrobolt_Combined( edict_t *self, vec3_t start, vec3_t angles, float maxdamage, float mindamage, float maxknockback, float minknockback, int stun, int range, int mod, int timeDelta ) {
	vec3_t from, end, dir;
	trace_t tr;
	edict_t *ignore, *event, *hit, *damaged;
	int hit_movetype;
	int mask;
	bool missed = true;
	int dmgflags = 0;
	int fireMode;

#ifdef ELECTROBOLT_TEST
	fireMode = FIRE_MODE_WEAK;
#else
	fireMode = FIRE_MODE_STRONG;
#endif

	if( GS_Instagib( *ggs ) ) {
		maxdamage = mindamage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );
	VectorMA( start, range, dir, end );
	VectorCopy( start, from );

	ignore = self;
	hit = damaged = NULL;

	mask = MASK_SHOT;
	if( GS_RaceGametype( *ggs ) ) {
		mask = MASK_SOLID;
	}

	clamp_high( mindamage, maxdamage );
	clamp_high( minknockback, maxknockback );

	const int impactDirByte = DirToByte( dir );

	tr.ent = -1;
	while( ignore ) {
		G_Trace4D( &tr, from, NULL, NULL, end, ignore, mask, timeDelta );

		VectorCopy( tr.endpos, from );
		ignore = NULL;

		if( tr.ent == -1 ) {
			break;
		}

		// some entity was touched
		hit = &game.edicts[tr.ent];
		hit_movetype = hit->movetype; // backup the original movetype as the entity may "die"
		if( hit == world ) { // stop dead if hit the world
			break;
		}

		// allow trail to go through BBOX entities (players, gibs, etc)
		if( !ISBRUSHMODEL( hit->s.modelindex ) ) {
			ignore = hit;
		}

		if( ( hit != self ) && ( hit->takedamage ) ) {
			float frac, damage, knockback;

			frac = DistanceFast( tr.endpos, start ) / (float)range;
			Q_clamp( frac, 0.0f, 1.0f );

			damage = maxdamage - ( ( maxdamage - mindamage ) * frac );
			knockback = maxknockback - ( ( maxknockback - minknockback ) * frac );

			G_Damage( hit, self, self, dir, dir, tr.endpos, damage, knockback, stun, dmgflags, mod );

			if( GS_RaceGametype( *ggs ) ) {
				// race - hit check for shootable buttons
				if( hit->movetype == MOVETYPE_NONE || hit->movetype == MOVETYPE_PUSH ) {
					break;
				}
			}

			const int normalByte = DirToByte( tr.plane.normal );
			const int parmByte   = ( impactDirByte << 8 ) | normalByte;

			// spawn a impact event on each damaged ent
			event = G_SpawnEvent( EV_BOLT_EXPLOSION, parmByte, tr.endpos );
			event->s.firemode = fireMode;
			if( hit->r.client ) {
				missed = false;
			}
			if( self ) {
				event->s.ownerNum = self->s.number;
			}

			damaged = hit;
		}

		if( hit_movetype == MOVETYPE_NONE || hit_movetype == MOVETYPE_PUSH ) {
			damaged = NULL;
			break;
		}
	}

	if( missed && self->r.client ) {
		G_AwardPlayerMissedElectrobolt( self, mod );
	}

	// send the weapon fire effect
	event = G_SpawnEvent( EV_ELECTROTRAIL, ENTNUM( self ), start );
	VectorScale( dir, 1024, event->s.origin2 );
	event->s.firemode = fireMode;

	if( !GS_Instagib( *ggs ) && tr.ent == -1 ) {  // didn't touch anything, not even a wall
		edict_t *bolt;
		const gs_weapon_definition_t *weapondef = GS_GetWeaponDef( ggs, self->s.weapon );

		// fire a weak EB from the end position
		bolt = W_Fire_Electrobolt_Weak( self, end, angles, weapondef->firedef_weak.speed, mindamage, minknockback, minknockback, stun, weapondef->firedef_weak.timeout, mod, timeDelta );
		bolt->enemy = damaged;
	}
}

void W_Fire_Electrobolt_FullInstant( edict_t *self, vec3_t start, vec3_t angles, float maxdamage, float mindamage, int maxknockback, int minknockback, int stun, int range, int minDamageRange, int mod, int timeDelta ) {
	vec3_t from, end, dir;
	trace_t tr;
	edict_t *ignore, *event, *hit;
	int hit_movetype;
	int mask;
	bool missed = true;
	int dmgflags = 0;

#define FULL_DAMAGE_RANGE g_projectile_prestep->value

	if( GS_Instagib( *ggs ) ) {
		maxdamage = mindamage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );
	VectorMA( start, range, dir, end );
	VectorCopy( start, from );

	ignore = self;
	hit = NULL;

	mask = MASK_SHOT;
	if( GS_RaceGametype( *ggs ) ) {
		mask = MASK_SOLID;
	}

	clamp_high( mindamage, maxdamage );
	clamp_high( minknockback, maxknockback );
	clamp_high( minDamageRange, range );

	if( minDamageRange <= FULL_DAMAGE_RANGE ) {
		minDamageRange = FULL_DAMAGE_RANGE + 1;
	}

	if( range <= FULL_DAMAGE_RANGE + 1 ) {
		range = FULL_DAMAGE_RANGE + 1;
	}

	const int impactDirByte = DirToByte( dir );

	tr.ent = -1;
	while( ignore ) {
		G_Trace4D( &tr, from, NULL, NULL, end, ignore, mask, timeDelta );

		VectorCopy( tr.endpos, from );
		ignore = NULL;

		if( tr.ent == -1 ) {
			break;
		}

		// some entity was touched
		hit = &game.edicts[tr.ent];
		hit_movetype = hit->movetype; // backup the original movetype as the entity may "die"
		if( hit == world ) { // stop dead if hit the world
			break;
		}

		// allow trail to go through BBOX entities (players, gibs, etc)
		if( !ISBRUSHMODEL( hit->s.modelindex ) ) {
			ignore = hit;
		}

		if( ( hit != self ) && ( hit->takedamage ) ) {
			float frac, damage, knockback, dist;

			dist = DistanceFast( tr.endpos, start );
			if( dist <= FULL_DAMAGE_RANGE ) {
				frac = 0.0f;
			} else {
				frac = ( dist - FULL_DAMAGE_RANGE ) / (float)( minDamageRange - FULL_DAMAGE_RANGE );
				Q_clamp( frac, 0.0f, 1.0f );
			}

			damage = maxdamage - ( ( maxdamage - mindamage ) * frac );
			knockback = maxknockback - ( ( maxknockback - minknockback ) * frac );

			G_Damage( hit, self, self, dir, dir, tr.endpos, damage, knockback, stun, dmgflags, mod );

			if( GS_RaceGametype( *ggs ) ) {
				// race - hit check for shootable buttons
				if( hit->movetype == MOVETYPE_NONE || hit->movetype == MOVETYPE_PUSH ) {
					break;
				}
			}

			const int normalByte = DirToByte( tr.plane.normal );
			const int parmByte   = ( impactDirByte << 8 ) | normalByte;

			// spawn a impact event on each damaged ent
			event = G_SpawnEvent( EV_BOLT_EXPLOSION, parmByte, tr.endpos );
			event->s.firemode = FIRE_MODE_STRONG;
			if( self ) {
				event->s.ownerNum = self->s.number;
			}
			if( hit->r.client ) {
				missed = false;
			}
		}

		if( hit_movetype == MOVETYPE_NONE || hit_movetype == MOVETYPE_PUSH ) {
			break;
		}
	}

	if( missed && self->r.client ) {
		G_AwardPlayerMissedElectrobolt( self, mod );
	}

	// send the weapon fire effect
	event = G_SpawnEvent( EV_ELECTROTRAIL, ENTNUM( self ), start );
	VectorScale( dir, 1024, event->s.origin2 );
	event->s.firemode = FIRE_MODE_STRONG;

#undef FULL_DAMAGE_RANGE
}

/*
* W_Fire_Electrobolt_Weak
*/
edict_t *W_Fire_Electrobolt_Weak( edict_t *self, vec3_t start, vec3_t angles, float speed, float damage, int minKnockback, int maxKnockback, int stun, int timeout, int mod, int timeDelta ) {
	edict_t *bolt;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	// projectile, weak mode
	bolt = W_Fire_LinearProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, 0, 0, timeout, timeDelta );
	bolt->s.modelindex = SV_ModelIndex( PATH_ELECTROBOLT_WEAK_MODEL );
	bolt->s.type = ET_ELECTRO_WEAK; //add particle trail and light
	bolt->s.ownerNum = ENTNUM( self );
	bolt->touch = W_Touch_Bolt;
	bolt->classname = "bolt";
	bolt->style = mod;
	bolt->s.effects &= ~EF_STRONG_WEAPON;

	return bolt;
}

static void W_Think_Wave( edict_t *ent ) {
	if( ent->timeout < level.time ) {
		G_FreeEdict( ent );
		return;
	}

	if( ent->r.inuse ) {
		ent->nextThink = level.time + 1;
	}

	// Check whether the corona damage has not been activated yet
	const firedef_t *firedef;
	if( ent->style == MOD_SHOCKWAVE_S ) {
		firedef = &GS_GetWeaponDef( ggs, WEAP_SHOCKWAVE )->firedef;
	} else {
		firedef = &GS_GetWeaponDef( ggs, WEAP_SHOCKWAVE )->firedef_weak;
	}

	if( level.time - ent->timeStamp < ( ( 1000 * firedef->splash_radius ) / firedef->speed ) + 64 ) {
		return;
	}

	// TODO: We're falling into "continuous collision detection" problem here.
	// We're not sure what is the proper way to resolve ones.
    // If we check entities in a transformed box covered by the wave, how do we check visibility?
	// Neither antilag nor projectile offset are applied for same reasons.

	int entNums[64];
	int numEnts = GClip_FindInRadius( ent->s.origin, ent->projectileInfo.radius, entNums, 64 );
	clamp_high( numEnts, 64 );

	// Let us assume if a target stands still and the ingoing wave projectile flies
	// has flown away almost hitting it, the target should get the same damage as a direct hit.
	// Thus the projectile affects the target for Ta = ( 2 * radius ) / speed
	// Consequently DPS = ( direct hit damage ) / Ta = ( direct hit damage ) * speed / ( 2 * radius )
	// All time units are assumed to be in seconds

	float dps = ent->projectileInfo.maxDamage * VectorLengthFast( ent->velocity ) / ( 2.0f * ent->projectileInfo.radius );
	float frameTimeSeconds = 0.001f * game.frametime;

	const float stun = g_shockwave_fly_stun_scale->value * ent->projectileInfo.stun * frameTimeSeconds;
	// We have discovered that theoretical best dps is not substantial enough for a player to fear it
	// since players are not standing still waiting to absorb all potential damage.
	// Thus we add an extra scale to the damage
	const float damage = g_shockwave_fly_damage_scale->value * dps * frameTimeSeconds;
	// Same applies to knockback. Moreover we want to keep explosion knockback on GB level
	// and highly increase the knockback for area damage to be able to push enemy back.
	const float knockback = g_shockwave_fly_knockback_scale->value * ent->projectileInfo.maxKnockback * frameTimeSeconds;

	trace_t trace;
	edict_t *attacker = game.edicts + ent->s.ownerNum;

	// For each entity in radius
	for( int i = 0; i < numEnts; ++i ) {
		edict_t *other = game.edicts + entNums[i];
		// If the entity is not damageable
		if( !other->takedamage ) {
			continue;
		}

		float *origin = ent->s.origin;
		edict_t *ignore = ent;
		// Try tracing from wave origin to other entity origin.
		for(;; ) {
			G_Trace( &trace, origin, NULL, NULL, other->s.origin, ignore, MASK_PLAYERSOLID );
			// Stop if there is no hit occured
			if( trace.fraction == 1.0f ) {
				break;
			}
			// Stop if a solid world is hit
			if( trace.ent <= 0 ) {
				break;
			}

			if( game.edicts + trace.ent == other ) {
				vec3_t dir;
				VectorSubtract( other->s.origin, ent->s.origin, dir );
				int mod = ( ent->style == MOD_SHOCKWAVE_S ) ? MOD_SHOCKWAVE_CORONA_S : MOD_SHOCKWAVE_CORONA_W;
				G_Damage( other, ent, attacker, dir, dir, vec3_origin, damage, knockback, stun, DAMAGE_RADIUS, mod );
				break;
			}

			// Stop if a solid and not-damageable entity is hit
			if( !game.edicts[trace.ent].takedamage ) {
				break;
			}

			// Continue tracing from the orgin of an entity the trace has hit ignoring it
			ignore = &game.edicts[trace.ent];
			origin = game.edicts[trace.ent].s.origin;
		}
	}
}

static void W_Touch_Wave( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags ) {
	vec3_t dir;
	int hitType;

	if( surfFlags & SURF_NOIMPACT ) {
		G_FreeEdict( ent );
		return;
	}

	hitType = G_Projectile_HitStyle( ent, other );
	if( hitType == PROJECTILE_TOUCH_NOT ) {
		return;
	}

	if( other->takedamage ) {
		VectorNormalize2( ent->velocity, dir );

		if( hitType == PROJECTILE_TOUCH_DIRECTSPLASH ) { // use hybrid direction from splash and projectile
			G_SplashFrac( ENTNUM( other ), ent->s.origin, ent->projectileInfo.radius, dir, NULL, NULL );
		} else {
			VectorNormalize2( ent->velocity, dir );
		}

		G_Damage( other, ent, ent->r.owner, dir, ent->velocity, ent->s.origin, ent->projectileInfo.maxDamage, ent->projectileInfo.maxKnockback, ent->projectileInfo.stun, 0, ent->style );
	}

	W_Detonate_Wave( ent, other, plane, surfFlags );
}

void W_Detonate_Wave( edict_t *ent, const edict_t *ignore, const cplane_t *plane, int surfFlags ) {
	int mod_splash = ( ent->style == MOD_SHOCKWAVE_S ) ? MOD_SHOCKWAVE_SPLASH_S : MOD_SHOCKWAVE_SPLASH_W;

	G_RadiusDamage( ent, ent->r.owner, plane, ignore, mod_splash );

	if( !( surfFlags & SURF_NOIMPACT ) ) {
		edict_t *event;
		vec3_t explosion_origin;

		VectorMA( ent->s.origin, -0.02, ent->velocity, explosion_origin );
		event = G_SpawnEvent( EV_WAVE_EXPLOSION, DirToByte( plane ? plane->normal : NULL ), explosion_origin );
		event->s.firemode = ( ent->s.effects & EF_STRONG_WEAPON ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
		event->s.weapon = ( ( ent->projectileInfo.radius * 1 / 8 ) > 255 ) ? 255 : ( ent->projectileInfo.radius * 1 / 8 );
	}

	// free at next frame
	G_FreeEdict( ent );
}

edict_t *W_Fire_Shockwave( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int timeout, int mod, int timeDelta ) {
	edict_t *wave;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	wave = W_Fire_LinearProjectile( self, start, angles, speed, damage, minKnockback, maxKnockback, stun, minDamage, radius, timeout, timeDelta );
	if( mod == MOD_SHOCKWAVE_S ) {
		wave->s.modelindex = SV_ModelIndex( PATH_WAVE_STRONG_MODEL );
		wave->s.sound = SV_SoundIndex( S_WEAPON_SHOCKWAVE_S_FLY );
		wave->s.effects |= EF_STRONG_WEAPON;
	} else {
		wave->s.modelindex = SV_ModelIndex( PATH_WAVE_WEAK_MODEL );
		wave->s.sound = SV_SoundIndex( S_WEAPON_SHOCKWAVE_W_FLY );
		wave->s.effects &= ~EF_STRONG_WEAPON;
	}

	wave->s.type = ET_WAVE;
	wave->classname = "wave";
	wave->style = mod;
	wave->s.ownerNum = ENTNUM( self );

	wave->think = W_Think_Wave;
	wave->touch = W_Touch_Wave;
	wave->timeout = level.time + timeout;
	wave->nextThink = level.time + 1;

	return wave;
}

/*
* W_Fire_Instagun_Strong
*/
void W_Fire_Instagun( edict_t *self, vec3_t start, vec3_t angles, float damage, int knockback,
					  int stun, int radius, int range, int mod, int timeDelta ) {
	vec3_t from, end, dir;
	trace_t tr;
	edict_t *ignore, *event, *hit;
	int hit_movetype;
	int mask;
	bool missed = true;
	int dmgflags = 0;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	AngleVectors( angles, dir, NULL, NULL );
	VectorMA( start, range, dir, end );
	VectorCopy( start, from );
	ignore = self;
	mask = MASK_SHOT;
	if( GS_RaceGametype( *ggs ) ) {
		mask = MASK_SOLID;
	}

	const int impactDirByte = DirToByte( dir );

	tr.ent = -1;
	while( ignore ) {
		G_Trace4D( &tr, from, NULL, NULL, end, ignore, mask, timeDelta );

		VectorCopy( tr.endpos, from );
		ignore = NULL;
		if( tr.ent == -1 ) {
			break;
		}

		// allow trail to go through SOLID_BBOX entities (players, gibs, etc)
		hit = &game.edicts[tr.ent];
		hit_movetype = hit->movetype; // backup the original movetype as the entity may "die"

		if( !ISBRUSHMODEL( hit->s.modelindex ) ) {
			ignore = hit;
		}

		if( ( hit != self ) && ( hit->takedamage ) ) {
			G_Damage( hit, self, self, dir, dir, tr.endpos, damage, knockback, stun, dmgflags, mod );

			const int normalByte = DirToByte( tr.plane.normal );
			const int parmByte   = ( impactDirByte << 8 ) | normalByte;

			// spawn a impact event on each damaged ent
			event = G_SpawnEvent( EV_INSTA_EXPLOSION, parmByte, tr.endpos );
			event->s.ownerNum = ENTNUM( self );
			event->s.firemode = FIRE_MODE_STRONG;
			if( hit->r.client ) {
				missed = false;
			}
		}

		// some entity was touched
		if( hit == world
			|| hit_movetype == MOVETYPE_NONE
			|| hit_movetype == MOVETYPE_PUSH ) {
			if( g_instajump->integer && self && self->r.client ) {
				// create a temporary inflictor entity
				edict_t *inflictor;

				inflictor = G_Spawn();
				inflictor->s.solid = SOLID_NOT;
				inflictor->timeDelta = 0;
				VectorCopy( tr.endpos, inflictor->s.origin );
				inflictor->s.ownerNum = ENTNUM( self );
				inflictor->projectileInfo.maxDamage = 0;
				inflictor->projectileInfo.minDamage = 0;
				inflictor->projectileInfo.maxKnockback = knockback;
				inflictor->projectileInfo.minKnockback = 1;
				inflictor->projectileInfo.stun = 0;
				inflictor->projectileInfo.radius = radius;

				G_RadiusDamage( inflictor, self, &tr.plane, NULL, mod );

				G_FreeEdict( inflictor );
			}
			break;
		}
	}

	if( missed && self->r.client ) {
		G_AwardPlayerMissedElectrobolt( self, mod );
	}

	// send the weapon fire effect
	event = G_SpawnEvent( EV_INSTATRAIL, ENTNUM( self ), start );
	VectorScale( dir, 1024, event->s.origin2 );
}

/*
* G_HideLaser
*/
void G_HideLaser( edict_t *ent ) {
	ent->s.modelindex = 0;
	ent->s.sound = 0;
	ent->r.svflags = SVF_NOCLIENT;

	// give it 100 msecs before freeing itself, so we can relink it if we start firing again
	ent->think = G_FreeEdict;
	ent->nextThink = level.time + 100;
}

/*
* G_Laser_Think
*/
static void G_Laser_Think( edict_t *ent ) {
	edict_t *owner;

	if( ent->s.ownerNum < 1 || ent->s.ownerNum > ggs->maxclients ) {
		G_FreeEdict( ent );
		return;
	}

	owner = &game.edicts[ent->s.ownerNum];

	if( G_ISGHOSTING( owner ) || owner->s.weapon != WEAP_LASERGUN ||
		G_GetClientState( PLAYERNUM( owner ) ) < CS_SPAWNED ||
		( owner->r.client->ps.weaponState != WEAPON_STATE_REFIRESTRONG
		  && owner->r.client->ps.weaponState != WEAPON_STATE_REFIRE ) ) {
		G_HideLaser( ent );
		return;
	}

	ent->nextThink = level.time + 1;
}

static float laser_damage;
static int laser_knockback;
static int laser_stun;
static int laser_attackerNum;
static int laser_mod;
static int laser_missed;

static void _LaserImpact( trace_t *trace, vec3_t dir ) {
	edict_t *attacker;

	if( !trace || trace->ent <= 0 ) {
		return;
	}
	if( trace->ent == laser_attackerNum ) {
		return; // should not be possible theoretically but happened at least once in practice

	}
	attacker = &game.edicts[laser_attackerNum];

	if( game.edicts[trace->ent].takedamage ) {
		G_Damage( &game.edicts[trace->ent], attacker, attacker, dir, dir, trace->endpos, laser_damage, laser_knockback, laser_stun, DAMAGE_STUN_CLAMP | DAMAGE_KNOCKBACK_SOFT, laser_mod );
		laser_missed = false;
	}
}

static edict_t *_FindOrSpawnLaser( edict_t *owner, int entType, bool *newLaser ) {
	int i, ownerNum;
	edict_t *e, *laser;

	// first of all, see if we already have a beam entity for this laser
	*newLaser = false;
	laser = NULL;
	ownerNum = ENTNUM( owner );
	for( i = ggs->maxclients + 1; i < game.maxentities; i++ ) {
		e = &game.edicts[i];
		if( !e->r.inuse ) {
			continue;
		}

		if( e->s.ownerNum == ownerNum && ( e->s.type == ET_LASERBEAM || e->s.type == ET_CURVELASERBEAM ) ) {
			laser = e;
			break;
		}
	}

	// if no ent was found we have to create one
	if( !laser || laser->s.type != entType || !laser->s.modelindex ) {
		if( !laser ) {
			*newLaser = true;
			laser = G_Spawn();
		}

		laser->s.type = entType;
		laser->s.ownerNum = ownerNum;
		laser->r.solid = SOLID_NOT;
		laser->s.modelindex = 255; // needs to have some value so it isn't filtered by the server culling
		laser->r.svflags &= ~SVF_NOCLIENT;
		// The only real utility of this flag is to prevent aggressive entity culling by anticheat.
		// The netcode/entity system needs an overhaul, use this hack as a temporary workaround.
		laser->r.svflags |= SVF_TRANSMITORIGIN2;
	}

	return laser;
}

/*
* W_Fire_Lasergun
*/
edict_t *W_Fire_Lasergun( edict_t *self, vec3_t start, vec3_t angles, float damage, int knockback, int stun, int range, int mod, int timeDelta ) {
	edict_t *laser;
	bool newLaser;
	trace_t tr;
	vec3_t dir;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	laser = _FindOrSpawnLaser( self, ET_LASERBEAM, &newLaser );
	if( newLaser ) {
		// the quad start sound is added from the server
		if( self->r.client && self->r.client->ps.inventory[POWERUP_QUAD] > 0 ) {
			G_Sound( self, CHAN_AUTO, SV_SoundIndex( S_QUAD_FIRE ), ATTN_NORM );
		}
	}

	laser_damage = damage;
	laser_knockback = knockback;
	laser_stun = stun;
	laser_attackerNum = ENTNUM( self );
	laser_mod = mod;
	laser_missed = true;

	GS_TraceLaserBeam( ggs, &tr, start, angles, range, ENTNUM( self ), timeDelta, _LaserImpact );

	laser->r.svflags |= SVF_FORCEOWNER;
	VectorCopy( start, laser->s.origin );
	AngleVectors( angles, dir, NULL, NULL );
	VectorMA( laser->s.origin, range, dir, laser->s.origin2 );

	laser->think = G_Laser_Think;
	laser->nextThink = level.time + 100;

	if( laser_missed && self->r.client ) {
		G_AwardPlayerMissedLasergun( self, mod );
	}

	// calculate laser's mins and maxs for linkEntity
	G_SetBoundsForSpanEntity( laser, 8 );

	GClip_LinkEntity( laser );

	return laser;
}

/*
* W_Fire_Lasergun_Weak
*/
edict_t *W_Fire_Lasergun_Weak( edict_t *self, vec3_t start, vec3_t end, float damage, int knockback, int stun, int range, int mod, int timeDelta ) {
	edict_t *laser;
	bool newLaser;
	trace_t trace;

	if( GS_Instagib( *ggs ) ) {
		damage = 9999;
	}

	laser = _FindOrSpawnLaser( self, ET_CURVELASERBEAM, &newLaser );
	if( newLaser ) {
		// the quad start sound is added from the server
		if( self->r.client && self->r.client->ps.inventory[POWERUP_QUAD] > 0 ) {
			G_Sound( self, CHAN_AUTO, SV_SoundIndex( S_QUAD_FIRE ), ATTN_NORM );
		}
	}

	laser_damage = damage;
	laser_knockback = knockback;
	laser_stun = stun;
	laser_attackerNum = ENTNUM( self );
	laser_mod = mod;
	laser_missed = true;

	vec3_t points[CURVELASERBEAM_SUBDIVISIONS + 1];
	GS_GetCurvedLaserBeamSegments( points, CURVELASERBEAM_SUBDIVISIONS, start, self->s.angles, (float)range, end );

	int passthrough = ENTNUM( self );
	for( unsigned segmentNum = 0; segmentNum < CURVELASERBEAM_SUBDIVISIONS; ++segmentNum ) {
		float *segmentFrom = points[segmentNum + 0];
		float *segmentTo   = points[segmentNum + 1];

		// TODO: GS_TraceLaserBeam() should not require angles
		vec3_t segmentDir, tmpangles;
		VectorSubtract( segmentTo, segmentFrom, segmentDir );
		VecToAngles( segmentDir, tmpangles );

		GS_TraceLaserBeam( ggs, &trace, segmentFrom, tmpangles, DistanceFast( segmentFrom, segmentTo ), passthrough, 0, _LaserImpact );
		if( trace.fraction != 1.0f ) {
			break;
		}

		passthrough = trace.ent;
	}

	laser->r.svflags |= SVF_FORCEOWNER;
	VectorCopy( start, laser->s.origin );
	VectorCopy( end, laser->s.origin2 );

	laser->think = G_Laser_Think;
	laser->nextThink = level.time + 100;

	if( laser_missed && self->r.client ) {
		G_AwardPlayerMissedLasergun( self, mod );
	}

	// calculate laser's mins and maxs for linkEntity
	G_SetBoundsForSpanEntity( laser, 8 );

	GClip_LinkEntity( laser );

	return laser;
}

void SV_Physics_LinearProjectile( edict_t *ent, int lookAheadTime );

static bool is_quad;

#define PLASMAHACK // ffs : hack for the plasmagun

#ifdef PLASMAHACK
void W_Plasma_Backtrace( edict_t *ent, const vec3_t start );
#endif

void Use_Weapon( edict_t *ent, const gsitem_t *item ) {
	int ammocount, weakammocount;
	const gs_weapon_definition_t *weapondef;

	//invalid weapon item
	if( item->tag < WEAP_NONE || item->tag >= WEAP_TOTAL ) {
		return;
	}

	// see if we're already changing to it
	if( ent->r.client->ps.stats[STAT_PENDING_WEAPON] == item->tag ) {
		return;
	}

	weapondef = GS_GetWeaponDef( ggs, item->tag );

	if( !g_select_empty->integer && !( item->type & IT_AMMO ) ) {
		if( weapondef->firedef.usage_count ) {
			if( weapondef->firedef.ammo_id ) {
				ammocount = ent->r.client->ps.inventory[weapondef->firedef.ammo_id];
			} else {
				ammocount = weapondef->firedef.usage_count;
			}
		} else {
			ammocount = 1; // can change weapon

		}
		if( weapondef->firedef_weak.usage_count ) {
			if( weapondef->firedef_weak.ammo_id ) {
				weakammocount = ent->r.client->ps.inventory[weapondef->firedef_weak.ammo_id];
			} else {
				weakammocount = weapondef->firedef_weak.usage_count;
			}
		} else {
			weakammocount = 1; // can change weapon

		}
		if( ammocount < weapondef->firedef.usage_count &&
			weakammocount < weapondef->firedef_weak.usage_count ) {
			return;
		}
	}

	// change to this weapon when down
	ent->r.client->ps.stats[STAT_PENDING_WEAPON] = item->tag;
}

bool Pickup_Weapon( edict_t *other, const gsitem_t *item, int flags, int ammo_count ) {
	const auto *weapondef = GS_GetWeaponDef( ggs, item->tag );

	if( !( flags & DROPPED_ITEM ) ) {
		// weapons stay in race
		if( GS_RaceGametype( *ggs ) && ( other->r.client->ps.inventory[item->tag] != 0 ) ) {
			return false;
		}
	}

	other->r.client->ps.inventory[item->tag]++;

	// never allow the player to carry more than 2 copies of the same weapon
	if( other->r.client->ps.inventory[item->tag] > item->inventory_max ) {
		other->r.client->ps.inventory[item->tag] = item->inventory_max;
	}

	const auto ammo_tag = item->weakammo_tag;
	if( !( flags & DROPPED_ITEM ) ) {
		// give them some ammo with it
		if( ammo_tag ) {
			Add_Ammo( other->r.client, GS_FindItemByTag( ggs, ammo_tag ), weapondef->firedef_weak.weapon_pickup, true );
		}
	} else {
		// it's a dropped weapon
		if( ammo_count && ammo_tag ) {
			Add_Ammo( other->r.client, GS_FindItemByTag( ggs, ammo_tag ), ammo_count, true );
		}
	}

	return true;
}

edict_t *Drop_Weapon( edict_t *ent, const gsitem_t *item ) {
	int otherweapon;
	edict_t *drop;
	int ammodrop = 0;

	if( item->tag < 1 || item->tag >= WEAP_TOTAL ) {
		G_PrintMsg( ent, "Can't drop unknown weapon\n" );
		return NULL;
	}

	// find out the amount of ammo to drop
	if( ent->r.client->ps.inventory[item->tag] > 1 && ent->r.client->ps.inventory[item->ammo_tag] > 5 ) {
		ammodrop = ent->r.client->ps.inventory[item->ammo_tag] / 2;
	} else {   // drop all
		ammodrop = ent->r.client->ps.inventory[item->ammo_tag];
	}

	drop = Drop_Item( ent, item );
	if( drop ) {
		ent->r.client->ps.inventory[item->ammo_tag] -= ammodrop;
		drop->count = ammodrop;
		drop->spawnflags |= DROPPED_PLAYER_ITEM;
		ent->r.client->ps.inventory[item->tag]--;

		if( !ent->r.client->ps.inventory[item->tag] ) {
			otherweapon = GS_SelectBestWeapon( ggs, &ent->r.client->ps );
			Use_Weapon( ent, GS_FindItemByTag( ggs, otherweapon ) );
		}
	}
	return drop;
}

static void G_ProjectileDistancePrestep( edict_t *projectile, float distance ) {
	float speed;
	vec3_t dir, dest;
	int mask, i;
	trace_t trace;
#ifdef PLASMAHACK
	vec3_t plasma_hack_start;
#endif

	if( projectile->movetype != MOVETYPE_TOSS
		&& projectile->movetype != MOVETYPE_LINEARPROJECTILE
		&& projectile->movetype != MOVETYPE_BOUNCE
		&& projectile->movetype != MOVETYPE_BOUNCEGRENADE ) {
		return;
	}

	if( !distance ) {
		return;
	}

	if( ( speed = VectorNormalize2( projectile->velocity, dir ) ) == 0.0f ) {
		return;
	}

	mask = ( projectile->r.clipmask ) ? projectile->r.clipmask : MASK_SHOT; // race trick should come set up inside clipmask

	if( GS_RaceGametype( *ggs ) && projectile->movetype == MOVETYPE_LINEARPROJECTILE ) {
		VectorCopy( projectile->s.linearMovementBegin, projectile->s.origin );
	}

#ifdef PLASMAHACK
	VectorCopy( projectile->s.origin, plasma_hack_start );
#endif

	VectorMA( projectile->s.origin, distance, dir, dest );
	G_Trace4D( &trace, projectile->s.origin, projectile->r.mins, projectile->r.maxs, dest, projectile->r.owner, mask, projectile->timeDelta );

	if( GS_RaceGametype( *ggs ) ) {
		for( i = 0; i < 3; i++ ) {
			projectile->s.origin[i] = projectile->s.linearMovementBegin[i] = projectile->olds.origin[i] = projectile->olds.linearMovementBegin[i] = trace.endpos[i];
		}
	} else {
		for( i = 0; i < 3; i++ ) {
			projectile->s.origin[i] = projectile->olds.origin[i] = trace.endpos[i];
		}
	}

	GClip_LinkEntity( projectile );
	SV_Impact( projectile, &trace );

	// set initial water state
	if( !projectile->r.inuse ) {
		return;
	}

	projectile->waterlevel = ( G_PointContents4D( projectile->s.origin, projectile->timeDelta ) & MASK_WATER ) ? true : false;

	// ffs : hack for the plasmagun
#ifdef PLASMAHACK
	if( projectile->s.type == ET_PLASMA ) {
		W_Plasma_Backtrace( projectile, plasma_hack_start );
	}
#endif
}

static void G_ProjectileTimePrestep( edict_t *projectile, int timeOffset ) {
	assert( projectile->movetype == MOVETYPE_LINEARPROJECTILE );

	if( timeOffset <= 0 ) {
		return;
	}

	// Look ahead timeOffset millis more
	SV_Physics_LinearProjectile( projectile, timeOffset );
	// Shift the linear movement initial origin
	VectorCopy( projectile->s.origin, projectile->s.linearMovementBegin );
}

static edict_t *G_Fire_Gunblade_Knife( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner ) {
	int range, knockback, stun, mod;
	float damage;
	int timeDelta;

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_GUNBLADE_S : MOD_GUNBLADE_W;
	range = firedef->timeout;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	W_Fire_Blade( owner, range, origin, angles, damage, knockback, stun, mod, timeDelta );

	return NULL;
}

static void G_LocalSpread( vec3_t angles, int spread, int seed ) {
	float r, u;
	vec3_t axis[3], dir;
	double alpha;
	double s;

	if( spread <= 0 ) {
		return;
	}

	seed &= 255;

	alpha = M_PI * Q_crandom( &seed ); // [-PI ..+PI]
	s = fabs( Q_crandom( &seed ) ); // [0..1]

	r = s * cos( alpha ) * spread;
	u = s * sin( alpha ) * spread;

	AngleVectors( angles, axis[0], axis[1], axis[2] );

	VectorMA( vec3_origin, 8192, axis[0], dir );
	VectorMA( dir, r, axis[1], dir );
	VectorMA( dir, u, axis[2], dir );

	VecToAngles( dir, angles );
}

static edict_t *G_Fire_Gunblade_Blast( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, knockback, stun, minDamage, minKnockback, radius, mod;
	float damage;
	int timeDelta;

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_GUNBLADE_S : MOD_GUNBLADE_W;
	speed = firedef->speed;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	minDamage = firedef->mindamage;
	minKnockback = firedef->minknockback;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	return W_Fire_GunbladeBlast( owner, origin, angles, damage, minKnockback, knockback, stun, minDamage,
								 radius, speed, firedef->timeout, mod, timeDelta );
}

static edict_t *G_Fire_Rocket( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, knockback, stun, minDamage, minKnockback, radius, mod;
	float damage;
	int timeDelta;

	// FIXME2: Rockets go slower underwater, do this at the actual rocket firing function

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_ROCKET_S : MOD_ROCKET_W;
	speed = firedef->speed;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	minDamage = firedef->mindamage;
	minKnockback = firedef->minknockback;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	return W_Fire_Rocket( owner, origin, angles, speed, damage, minKnockback, knockback, stun, minDamage,
						  radius, firedef->timeout, mod, timeDelta );
}

static edict_t *G_Fire_Machinegun( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int range, knockback, stun, mod;
	float damage;
	int timeDelta;

	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	} else {
		timeDelta = 0;
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_MACHINEGUN_S : MOD_MACHINEGUN_W;
	range = firedef->timeout;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	W_Fire_Bullet( owner, origin, angles, seed, range, firedef->spread, firedef->v_spread,
				   damage, knockback, stun, mod, timeDelta );

	return NULL;
}

static edict_t *G_Fire_Riotgun( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int range, knockback, stun, mod;
	float damage;
	int timeDelta;

	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	} else {
		timeDelta = 0;
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_RIOTGUN_S : MOD_RIOTGUN_W;
	range = firedef->timeout;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	W_Fire_Riotgun( owner, origin, angles, seed, range, firedef->spread, firedef->v_spread,
					firedef->projectile_count, damage, knockback, stun, mod, timeDelta );

	return NULL;
}

static edict_t *G_Fire_Grenade( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, minKnockback, knockback, stun, minDamage, radius, mod;
	float damage;
	int timeDelta;

	// FIXME2: projectiles go slower underwater, do this at the actual firing function

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_GRENADE_S : MOD_GRENADE_W;
	speed = firedef->speed;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	minDamage = firedef->mindamage;
	minKnockback = firedef->minknockback;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		minDamage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	return W_Fire_Grenade( owner, origin, angles, speed, damage, minKnockback, knockback, stun,
						   minDamage, radius, firedef->timeout, mod, timeDelta, true );
}

static edict_t *G_Fire_Plasma( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, knockback, stun, minDamage, minKnockback, radius, mod;
	float damage;
	int timeDelta;

	// FIXME2: projectiles go slower underwater, do this at the actual firing function

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_PLASMA_S : MOD_PLASMA_W;
	speed = firedef->speed;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	minDamage = firedef->mindamage;
	minKnockback = firedef->minknockback;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	return W_Fire_Plasma( owner, origin, angles, damage, minKnockback, knockback, stun, minDamage, radius,
						  speed, firedef->timeout, mod, timeDelta );
}

static edict_t *G_Fire_Lasergun( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int range, knockback, stun, mod;
	float damage;
	int timeDelta;
	vec3_t end;

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_LASERGUN_S : MOD_LASERGUN_W;
	range = firedef->timeout;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	// no need to continue if strong mode
	if( firedef->fire_mode == FIRE_MODE_STRONG ) {
		return W_Fire_Lasergun( owner, origin, angles, damage, knockback, stun, range, mod, timeDelta );
	}

	// find the endpoint into the ones in the backup trail
	if( !owner || !owner->r.client ) {
		vec3_t dir;
		AngleVectors( angles, dir, NULL, NULL );
		VectorMA( origin, range, dir, end );
	} else if( !G_GetLaserbeamPoint( &owner->r.client->trail, &owner->r.client->ps, owner->r.client->ucmd.serverTimeStamp, end ) ) {
		vec3_t dir;
		AngleVectors( angles, dir, NULL, NULL );
		VectorMA( origin, range, dir, end );
	}

	return W_Fire_Lasergun_Weak( owner, origin, end, damage, knockback, stun, range,
								 mod, timeDelta );
}

static edict_t *G_Fire_WeakBolt( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, maxknockback, minknockback, stun, mod;
	float maxdamage, mindamage;
	int timeDelta;

	// FIXME2: projectiles go slower underwater, do this at the actual firing function

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_ELECTROBOLT_S : MOD_ELECTROBOLT_W;
	speed = firedef->speed;
	maxdamage = firedef->damage;
	mindamage = firedef->mindamage;
	maxknockback = firedef->knockback;
	minknockback = firedef->minknockback;
	stun = firedef->stun;

	if( is_quad ) {
		mindamage *= QUAD_DAMAGE_SCALE;
		maxdamage *= QUAD_DAMAGE_SCALE;
		maxknockback *= QUAD_KNOCKBACK_SCALE;
	}
	return W_Fire_Electrobolt_Weak( owner, origin, angles, speed, maxdamage, minknockback, maxknockback, stun,
									firedef->timeout, mod, timeDelta );
}

static edict_t *G_Fire_StrongBolt( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int minDamageRange, stun, mod;
	float maxdamage, mindamage, maxknockback, minknockback;
	int timeDelta;

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_ELECTROBOLT_S : MOD_ELECTROBOLT_W;
	minDamageRange = firedef->timeout;
	maxdamage = firedef->damage;
	mindamage = firedef->mindamage;
	maxknockback = firedef->knockback;
	minknockback = firedef->minknockback;
	stun = firedef->stun;

	if( is_quad ) {
		mindamage *= QUAD_DAMAGE_SCALE;
		maxdamage *= QUAD_DAMAGE_SCALE;
		maxknockback *= QUAD_KNOCKBACK_SCALE;
	}
#ifdef ELECTROBOLT_TEST
	W_Fire_Electrobolt_FullInstant( owner, origin, angles, maxdamage, mindamage,
									maxknockback, minknockback, stun, ELECTROBOLT_RANGE, minDamageRange, mod, timeDelta );
#else
	W_Fire_Electrobolt_Combined( owner, origin, angles, maxdamage, mindamage,
								 maxknockback, minknockback, stun, range, mod, timeDelta );
#endif
	return NULL;
}

static edict_t *G_Fire_Shockwave( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int speed, knockback, stun, minDamage, minKnockback, radius, mod;
	float damage;
	int timeDelta;

	// FIXME2: Rockets go slower underwater, do this at the actual rocket firing function

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_SHOCKWAVE_S : MOD_SHOCKWAVE_W;
	speed = firedef->speed;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	minDamage = firedef->mindamage;
	minKnockback = firedef->minknockback;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	return W_Fire_Shockwave( owner, origin, angles, speed, damage, minKnockback, knockback, stun, minDamage,
							 radius, firedef->timeout, mod, timeDelta );
}

static edict_t *G_Fire_Instagun( vec3_t origin, vec3_t angles, const firedef_t *firedef, edict_t *owner, int seed ) {
	int range, knockback, stun, radius, mod;
	float damage;
	int timeDelta;

	timeDelta = 0;
	if( owner && owner->r.client ) {
		timeDelta = owner->r.client->timeDelta;
	}

	if( firedef->spread ) {
		G_LocalSpread( angles, firedef->spread, seed );
	}

	mod = ( firedef->fire_mode == FIRE_MODE_STRONG ) ? MOD_INSTAGUN_S : MOD_INSTAGUN_W;
	range = firedef->timeout;
	damage = firedef->damage;
	knockback = firedef->knockback;
	stun = firedef->stun;
	radius = firedef->splash_radius;

	if( is_quad ) {
		damage *= QUAD_DAMAGE_SCALE;
		knockback *= QUAD_KNOCKBACK_SCALE;
	}

	W_Fire_Instagun( owner, origin, angles, damage,
					 knockback, stun, radius, range, mod, timeDelta );

	return NULL;
}

void G_FireWeapon( edict_t *ent, int parm ) {
	vec3_t origin, angles;
	vec3_t viewoffset = { 0, 0, 0 };
	int ucmdSeed;

	const auto *const weapondef = GS_GetWeaponDef( ggs, ( parm >> 1 ) & 0x3f );
	const auto *const firedef = ( parm & 0x1 ) ? &weapondef->firedef : &weapondef->firedef_weak;

	// find this shot projection source
	if( ent->r.client ) {
		viewoffset[2] += ent->r.client->ps.viewheight;
		VectorCopy( ent->r.client->ps.viewangles, angles );
		is_quad = ( ent->r.client->ps.inventory[POWERUP_QUAD] > 0 ) ? true : false;
		ucmdSeed = ent->r.client->ucmd.serverTimeStamp & 255;
	} else {
		VectorCopy( ent->s.angles, angles );
		is_quad = false;
		ucmdSeed = rand() & 255;
	}

	VectorAdd( ent->s.origin, viewoffset, origin );


	// shoot

	edict_t *__restrict projectile = nullptr;

	switch( weapondef->weapon_id ) {
		default:
		case WEAP_NONE:
			break;

		case WEAP_GUNBLADE:
			if( firedef->fire_mode == FIRE_MODE_STRONG ) {
				projectile = G_Fire_Gunblade_Blast( origin, angles, firedef, ent, ucmdSeed );
			} else {
				projectile = G_Fire_Gunblade_Knife( origin, angles, firedef, ent );
			}
			break;

		case WEAP_MACHINEGUN:
			projectile = G_Fire_Machinegun( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_RIOTGUN:
			projectile = G_Fire_Riotgun( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_GRENADELAUNCHER:
			projectile = G_Fire_Grenade( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_ROCKETLAUNCHER:
			projectile = G_Fire_Rocket( origin, angles, firedef, ent, ucmdSeed );
			break;
		case WEAP_PLASMAGUN:
			projectile = G_Fire_Plasma( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_LASERGUN:
			projectile = G_Fire_Lasergun( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_ELECTROBOLT:
			if( firedef->fire_mode == FIRE_MODE_STRONG ) {
				projectile = G_Fire_StrongBolt( origin, angles, firedef, ent, ucmdSeed );
			} else {
				projectile = G_Fire_WeakBolt( origin, angles, firedef, ent, ucmdSeed );
			}
			break;

		case WEAP_SHOCKWAVE:
			projectile = G_Fire_Shockwave( origin, angles, firedef, ent, ucmdSeed );
			break;

		case WEAP_INSTAGUN:
			projectile = G_Fire_Instagun( origin, angles, firedef, ent, ucmdSeed );
			break;
	}

	// add stats
	if( ent->r.client && weapondef->weapon_id != WEAP_NONE ) {
		ent->r.client->stats.accuracy_shots[firedef->ammo_id - AMMO_GUNBLADE] += firedef->projectile_count;
	}

	if( !projectile ) {
		return;
	}

	if( GS_RaceGametype( *ggs ) && firedef->prestep != 0 ) {
		G_ProjectileDistancePrestep( projectile, firedef->prestep );
	} else {
		G_ProjectileDistancePrestep( projectile, g_projectile_prestep->value );
	}

	// Skip further tests if there was an impact
	if( !projectile->r.inuse ) {
		return;
	}

	if( projectile->s.linearMovement ) {
		VectorCopy( projectile->s.origin, projectile->s.linearMovementBegin );
	}

	// Hacks for limiting time offset that is used instead of former plain wrong 4D collision antilag.
	// We have to limit the offset as it makes these weapons practically instant-hit.
	// We do not want to apply this limitation to rockets as this is what players are used to.

	int timeOffset = -projectile->timeDelta;
	if( !timeOffset ) {
		return;
	}

	// Disable antilag for all projectiles regardless of type.
	projectile->timeDelta = 0;
	// Use a limited prestep for weapons with linear/initially linear trajectory.
	const auto type = projectile->s.type;
	if( type != ET_BLASTER && type != ET_ELECTRO_WEAK && type != ET_ROCKET && type != ET_PLASMA ) {
		return;
	}

	if( !GS_RaceGametype( *ggs ) ) {
		timeOffset = wsw::min( game.snapFrameTime, timeOffset );
	}

	assert( projectile->s.linearMovement );
	projectile->s.modelindex2 = 0;

	G_ProjectileTimePrestep( projectile, timeOffset );

	// If there was not an impact
	if( projectile->r.inuse ) {
		VectorCopy( projectile->s.origin, projectile->s.linearMovementBegin );
		// Re-schedule conversion to toss projectiles
		if( !( projectile->s.effects & EF_STRONG_WEAPON ) ) {
			if( type == ET_ROCKET ) {
				projectile->nextThink -= timeOffset;
				assert( projectile->nextThink > level.time );
			} else if( type == ET_PLASMA ) {
				projectile->trigger_timeout -= timeOffset;
				assert( projectile->trigger_timeout > level.time );
			}
		}
	}
}
