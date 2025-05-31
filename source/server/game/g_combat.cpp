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

// g_combat.c

#include "g_local.h"
#include <common/wswstaticvector.h>
#include "ai/vec3.h"

#include <string>
#include <span>

/*
*
*/
int G_ModToAmmo( int mod ) {
	if( mod == MOD_GUNBLADE_W ) {
		return AMMO_WEAK_GUNBLADE;
	} else if( mod == MOD_GUNBLADE_S ) {
		return AMMO_GUNBLADE;
	} else if( mod == MOD_MACHINEGUN_W ) {
		return AMMO_WEAK_BULLETS;
	} else if( mod == MOD_MACHINEGUN_S ) {
		return AMMO_BULLETS;
	} else if( mod == MOD_RIOTGUN_W ) {
		return AMMO_WEAK_SHELLS;
	} else if( mod == MOD_RIOTGUN_S ) {
		return AMMO_SHELLS;
	} else if( mod == MOD_GRENADE_W || mod == MOD_GRENADE_SPLASH_W ) {
		return AMMO_WEAK_GRENADES;
	} else if( mod == MOD_GRENADE_S || mod == MOD_GRENADE_SPLASH_S ) {
		return AMMO_GRENADES;
	} else if( mod == MOD_ROCKET_W || mod == MOD_ROCKET_SPLASH_W ) {
		return AMMO_WEAK_ROCKETS;
	} else if( mod == MOD_ROCKET_S || mod == MOD_ROCKET_SPLASH_S ) {
		return AMMO_ROCKETS;
	} else if( mod == MOD_PLASMA_W || mod == MOD_PLASMA_SPLASH_W ) {
		return AMMO_WEAK_PLASMA;
	} else if( mod == MOD_PLASMA_S || mod == MOD_PLASMA_SPLASH_S ) {
		return AMMO_PLASMA;
	} else if( mod == MOD_ELECTROBOLT_W ) {
		return AMMO_WEAK_BOLTS;
	} else if( mod == MOD_ELECTROBOLT_S ) {
		return AMMO_BOLTS;
	} else if( mod == MOD_INSTAGUN_W ) {
		return AMMO_WEAK_INSTAS;
	} else if( mod == MOD_INSTAGUN_S ) {
		return AMMO_INSTAS;
	} else if( mod == MOD_LASERGUN_W ) {
		return AMMO_WEAK_LASERS;
	} else if( mod == MOD_LASERGUN_S ) {
		return AMMO_LASERS;
	} else {
		return AMMO_NONE;
	}
}

/*
* G_Killed
*/
void G_Killed( edict_t *targ, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point, int mod ) {
	if( targ->health < -999 ) {
		targ->health = -999;
	}

	if( targ->deadflag == DEAD_DEAD ) {
		return;
	}

	targ->deadflag = DEAD_DEAD;
	targ->enemy = attacker;

	if( targ->r.client ) {
		if( attacker && targ != attacker ) {
			if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
				attacker->snap.teamkill = true;
			} else {
				attacker->snap.kill = true;
			}
		}

		// count stats
		if( GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME ) {
			targ->r.client->stats.AddDeath();
			teamlist[targ->s.team].stats.AddDeath();

			if( !attacker || !attacker->r.client || attacker == targ || attacker == world ) {
				targ->r.client->stats.AddSuicide();
				teamlist[targ->s.team].stats.AddSuicide();
			} else {
				if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
					attacker->r.client->stats.AddTeamFrag();
					teamlist[attacker->s.team].stats.AddTeamFrag();
				} else {
					attacker->r.client->stats.AddFrag();
					teamlist[attacker->s.team].stats.AddFrag();
					G_AwardPlayerKilled( targ, inflictor, attacker, mod );
				}
			}
		}
	}

	G_Gametype_ScoreEvent( attacker ? attacker->r.client : NULL, "kill", va( "%i %i %i %i", targ->s.number, ( inflictor == world ) ? -1 : ENTNUM( inflictor ), ENTNUM( attacker ), mod ) );

	G_CallDie( targ, inflictor, attacker, damage, point );
}

/*
* G_CheckArmor
*/
static float G_CheckArmor( edict_t *ent, float damage, int dflags ) {
	Client *client = ent->r.client;
	float maxsave, save, armordamage;

	if( !client ) {
		return 0.0f;
	}

	if( dflags & DAMAGE_NO_ARMOR || dflags & DAMAGE_NO_PROTECTION ) {
		return 0.0f;
	}

	maxsave = wsw::min( damage, client->armor / g_armor_degradation->value );

	if( maxsave <= 0.0f ) {
		return 0.0f;
	}

	armordamage = maxsave * g_armor_degradation->value;
	save = maxsave * g_armor_protection->value;

	client->armor -= armordamage;
	if( ARMOR_TO_INT( client->armor ) <= 0 ) {
		client->armor = 0.0f;
	}
	client->ps.stats[STAT_ARMOR] = ARMOR_TO_INT( client->armor );

	return save;
}

/*
* G_BlendFrameDamage
*/
static void G_BlendFrameDamage( edict_t *ent, float damage, float *old_damage, const vec3_t point, const vec3_t basedir, vec3_t old_point, vec3_t old_dir ) {
	vec3_t offset;
	float frac;
	vec3_t dir;
	int i;

	if( !point ) {
		VectorSet( offset, 0, 0, ent->viewheight );
	} else {
		VectorSubtract( point, ent->s.origin, offset );
	}

	VectorNormalize2( basedir, dir );

	if( *old_damage == 0 ) {
		VectorCopy( offset, old_point );
		VectorCopy( dir, old_dir );
		*old_damage = damage;
		return;
	}

	frac = damage / ( damage + *old_damage );
	for( i = 0; i < 3; i++ ) {
		old_point[i] = ( old_point[i] * ( 1.0f - frac ) ) + offset[i] * frac;
		old_dir[i] = ( old_dir[i] * ( 1.0f - frac ) ) + dir[i] * frac;
	}
	*old_damage += damage;
}

#define MIN_KNOCKBACK_SPEED 2.5

/*
* G_KnockBackPush
*/
static void G_KnockBackPush( edict_t *targ, edict_t *attacker, const vec3_t basedir, int knockback, int dflags ) {
	float mass = 75.0f;
	float push;
	vec3_t dir;

	if( targ->flags & FL_NO_KNOCKBACK ) {
		knockback = 0;
	}

	knockback *= g_knockback_scale->value;

	if( knockback < 1 ) {
		return;
	}

	if( ( targ->movetype == MOVETYPE_NONE ) ||
		( targ->movetype == MOVETYPE_PUSH ) ||
		( targ->movetype == MOVETYPE_STOP ) ||
		( targ->movetype == MOVETYPE_BOUNCE ) ) {
		return;
	}

	if( targ->mass > 75 ) {
		mass = targ->mass;
	}

	push = 1000.0f * ( (float)knockback / mass );
	if( push < MIN_KNOCKBACK_SPEED ) {
		return;
	}

	VectorNormalize2( basedir, dir );

	if( targ->r.client && targ != attacker && !( dflags & DAMAGE_KNOCKBACK_SOFT ) ) {
		targ->r.client->ps.pmove.stats[PM_STAT_KNOCKBACK] = 3 * knockback;
		Q_clamp( targ->r.client->ps.pmove.stats[PM_STAT_KNOCKBACK], 100, 250 );
	}

	VectorMA( targ->velocity, push, dir, targ->velocity );
}

/*
* G_Damage
* targ		entity that is being damaged
* inflictor	entity that is causing the damage
* attacker	entity that caused the inflictor to damage targ
* example: targ=enemy, inflictor=rocket, attacker=player
*
* dir			direction of the attack
* point		point at which the damage is being inflicted
* normal		normal vector from that point
* damage		amount of damage being inflicted
* knockback	force to be applied against targ as a result of the damage
*
* dflags		these flags are used to control how T_Damage works
*/
void G_Damage( edict_t *targ, edict_t *inflictor, edict_t *attacker, const vec3_t pushdir, const vec3_t dmgdir, const vec3_t point, float damage, float knockback, float stun, int dflags, int mod ) {
	Client *client;
	float take;
	float save;
	float asave;
	bool statDmg;

	if( !targ || !targ->takedamage ) {
		return;
	}

	if( !attacker ) {
		attacker = world;
		mod = MOD_TRIGGER_HURT;
	}

	meansOfDeath = mod;

	client = targ->r.client;

	// Cgg - race mode: players don't interact with one another
	if( GS_RaceGametype( *ggs ) ) {
		if( attacker->r.client && targ->r.client && attacker != targ ) {
			return;
		}
	}

	// push
	if( !( dflags & DAMAGE_NO_KNOCKBACK ) ) {
		G_KnockBackPush( targ, attacker, pushdir, knockback, dflags );
		AI_Knockback( targ, attacker, pushdir, knockback, dflags );
	}

	// stun
	if( g_allow_stun->integer && !( dflags & ( DAMAGE_NO_STUN | FL_GODMODE ) )
		&& (int)stun > 0 && targ->r.client && targ->r.client->takeStun &&
		!GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) && ( targ != attacker ) ) {
		if( dflags & DAMAGE_STUN_CLAMP ) {
			if( targ->r.client->ps.pmove.stats[PM_STAT_STUN] < (int)stun ) {
				targ->r.client->ps.pmove.stats[PM_STAT_STUN] = (int)stun;
			}
		} else {
			targ->r.client->ps.pmove.stats[PM_STAT_STUN] += (int)stun;
		}

		Q_clamp( targ->r.client->ps.pmove.stats[PM_STAT_STUN], 0, MAX_STUN_TIME );
	}

	// dont count self-damage cause it just adds the same to both stats
	statDmg = ( attacker != targ ) && ( mod != MOD_TELEFRAG );

	// apply handicap on the damage given
	if( statDmg && attacker->r.client && !GS_Instagib( *ggs ) ) {
		// handicap is a percentage value
		if( attacker->r.client->handicap != 0 ) {
			damage *= 1.0 - ( attacker->r.client->handicap * 0.01f );
		}
	}

	take = damage;
	save = 0;

	// check for cases where damage is protected
	if( !( dflags & DAMAGE_NO_PROTECTION ) ) {
		// check for godmode
		if( targ->flags & FL_GODMODE ) {
			take = 0;
			save = damage;
		}
		// never damage in timeout
		else if( GS_MatchPaused( *ggs ) ) {
			take = save = 0;
		}
		// ca has self splash damage disabled
		else if( ( dflags & DAMAGE_RADIUS ) && attacker == targ && !GS_SelfDamage( *ggs ) ) {
			take = save = 0;
		}
		// don't get damage from players in race
		else if( ( GS_RaceGametype( *ggs ) ) && attacker->r.client && targ->r.client &&
				 ( attacker->r.client != targ->r.client ) ) {
			take = save = 0;
		}
		// team damage avoidance
		else if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) && !G_Gametype_CanTeamDamage( dflags ) ) {
			take = save = 0;
		}
		// apply warShell powerup protection
		else if( targ->r.client && targ->r.client->ps.inventory[POWERUP_SHELL] > 0 ) {
			// warshell offers full protection in instagib
			if( GS_Instagib( *ggs ) ) {
				take = 0;
				save = damage;
			} else {
				take = damage * ( 1.0f / QUAD_DAMAGE_SCALE );
				save = damage - take;
			}

			// todo : add protection sound
		}
	}

	asave = G_CheckArmor( targ, take, dflags );
	take -= asave;

	//treat cheat/powerup savings the same as armor
	asave += save;

	// APPLY THE DAMAGES

	if( !take && !asave ) {
		return;
	}

	// do the damage
	if( take <= 0 ) {
		return;
	}

	// adding damage given/received to stats
	if( statDmg && attacker->r.client && !targ->deadflag && targ->movetype != MOVETYPE_PUSH && targ->s.type != ET_CORPSE ) {
		attacker->r.client->stats.AddDamageGiven( take + asave );
		teamlist[attacker->s.team].stats.AddDamageGiven( take + asave );
		if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
			attacker->r.client->stats.AddTeamDamageGiven( take + asave );
			teamlist[attacker->s.team].stats.AddTeamDamageGiven( take + asave );
		}
	}

	G_Gametype_ScoreEvent( attacker->r.client, "dmg", va( "%i %f %i", targ->s.number, damage, attacker->s.number ) );

	if( attacker->bot )
		AI_DamagedEntity( attacker, targ, (int)damage );

	if( statDmg && client ) {
		client->stats.AddDamageTaken( take + asave );
		teamlist[targ->s.team].stats.AddDamageTaken( take + asave );
		if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
			client->stats.AddTeamDamageTaken( take + asave );
			teamlist[targ->s.team].stats.AddTeamDamageTaken( take + asave );
		}
	}

	// accumulate received damage for snapshot effects
	{
		vec3_t dorigin;

		if( inflictor == world && mod == MOD_FALLING ) { // it's fall damage
			targ->snap.damage_fall += take + save;
		}

		if( point[0] != 0.0f || point[1] != 0.0f || point[2] != 0.0f ) {
			VectorCopy( point, dorigin );
		} else {
			VectorSet( dorigin,
					   targ->s.origin[0],
					   targ->s.origin[1],
					   targ->s.origin[2] + targ->viewheight );
		}

		G_BlendFrameDamage( targ, take, &targ->snap.damage_taken, dorigin, dmgdir, targ->snap.damage_at, targ->snap.damage_dir );
		G_BlendFrameDamage( targ, save, &targ->snap.damage_saved, dorigin, dmgdir, targ->snap.damage_at, targ->snap.damage_dir );

		if( targ->r.client ) {
			if( mod != MOD_FALLING && mod != MOD_TELEFRAG && mod != MOD_SUICIDE ) {
				if( inflictor == world || attacker == world ) {
					// for world inflicted damage use always 'frontal'
					G_ClientAddDamageIndicatorImpact( targ->r.client, take + save, NULL );
				} else if( dflags & DAMAGE_RADIUS ) {
					// for splash hits the direction is from the inflictor origin
					G_ClientAddDamageIndicatorImpact( targ->r.client, take + save, pushdir );
				} else {   // for direct hits the direction is the projectile direction
					G_ClientAddDamageIndicatorImpact( targ->r.client, take + save, dmgdir );
				}
			}
		}
	}

	targ->health = targ->health - take;

	// add damage done to stats
	if( !GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) && statDmg && G_ModToAmmo( mod ) != AMMO_NONE && client && attacker->r.client ) {
		attacker->r.client->stats.accuracy_hits[G_ModToAmmo( mod ) - AMMO_GUNBLADE]++;
		attacker->r.client->stats.accuracy_damage[G_ModToAmmo( mod ) - AMMO_GUNBLADE] += damage;
		teamlist[attacker->s.team].stats.accuracy_hits[G_ModToAmmo( mod ) - AMMO_GUNBLADE]++;
		teamlist[attacker->s.team].stats.accuracy_damage[G_ModToAmmo( mod ) - AMMO_GUNBLADE] += damage;

		G_AwardPlayerHit( targ, attacker, mod );
	}

	// accumulate given damage for hit sounds
	if( ( take || asave ) && targ != attacker && client && !targ->deadflag ) {
		if( attacker ) {
			if( GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
				attacker->snap.damageteam_given += take + asave; // we want to know how good our hit was, so saved also matters
			} else {
				attacker->snap.damage_given += take + asave;
			}
		}
	}

	if( G_IsDead( targ ) ) {
		if( client ) {
			targ->flags |= FL_NO_KNOCKBACK;
		}
		G_Killed( targ, inflictor, attacker, HEALTH_TO_INT( take ), point, mod );
	} else {
		G_CallPain( targ, attacker, knockback, take );
	}
}

/*
* G_SplashFrac
*/
void G_SplashFrac( const vec3_t origin, const vec3_t mins, const vec3_t maxs, const vec3_t point, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac ) {
#define VERTICALBIAS 0.65f // 0...1
#define CAPSULEDISTANCE

//#define SPLASH_HDIST_CLAMP 0
	vec3_t boxcenter = { 0, 0, 0 };
	vec3_t hitpoint;
	float distance;
	int i;
	float innerradius;
	float refdistance;

	if( maxradius <= 0 ) {
		if( kickFrac ) {
			*kickFrac = 0;
		}
		if( dmgFrac ) {
			*dmgFrac = 0;
		}
		if( pushdir ) {
			VectorClear( pushdir );
		}
		return;
	}

	VectorCopy( point, hitpoint );

	innerradius = ( maxs[0] + maxs[1] - mins[0] - mins[1] ) * 0.25;

#ifdef CAPSULEDISTANCE

	// Find the distance to the closest point in the capsule contained in the player bbox
	// modify the origin so the inner sphere acts as a capsule
	VectorCopy( origin, boxcenter );
	boxcenter[2] = hitpoint[2];
	Q_clamp( boxcenter[2], ( origin[2] + mins[2] ) + innerradius, ( origin[2] + maxs[2] ) - innerradius );
#else

	// find center of the box
	for( i = 0; i < 3; i++ )
		boxcenter[i] = origin[i] + ( 0.5f * ( maxs[i] + mins[i] ) );
#endif

	// find push intensity
	distance = Distance( boxcenter, hitpoint );

	if( distance >= maxradius ) {
		if( kickFrac ) {
			*kickFrac = 0;
		}
		if( dmgFrac ) {
			*dmgFrac = 0;
		}
		if( pushdir ) {
			VectorClear( pushdir );
		}
		return;
	}

	refdistance = innerradius;
	if( refdistance >= maxradius ) {
		if( kickFrac ) {
			*kickFrac = 0;
		}
		if( dmgFrac ) {
			*dmgFrac = 0;
		}
		if( pushdir ) {
			VectorClear( pushdir );
		}
		return;
	}

	maxradius -= refdistance;
	distance -= refdistance;
	if( distance < 0 ) {
		distance = 0;
	}

	distance = maxradius - distance;
	Q_clamp( distance, 0, maxradius );

	if( dmgFrac ) {
		*dmgFrac = distance / maxradius;
		*dmgFrac = std::sqrt( *dmgFrac );
		Q_clamp( *dmgFrac, 0.0f, 1.0f );
	}

	if( kickFrac ) {
		// linear kick fraction
		float kick = ( distance / maxradius );

		kick *= kick;

		//kick = maxradius / distance;
		Q_clamp( kick, 0, 1 );

		// half linear half exponential
		//*kickFrac =  ( kick + ( kick * kick ) ) * 0.5f;

		// linear
		*kickFrac = kick;

		Q_clamp( *kickFrac, 0.0f, 1.0f );
	}

	//if( dmgFrac && kickFrac )
	//	G_Printf( "SPLASH: dmgFrac %.2f kickFrac %.2f\n", *dmgFrac, *kickFrac );

	// find push direction

	if( pushdir ) {
#ifdef CAPSULEDISTANCE

		// find real center of the box again
		for( i = 0; i < 3; i++ )
			boxcenter[i] = origin[i] + ( 0.5f * ( maxs[i] + mins[i] ) );
#endif

#ifdef VERTICALBIAS

		// move the center up for the push direction
		if( origin[2] + maxs[2] > boxcenter[2] ) {
			boxcenter[2] += VERTICALBIAS * ( ( origin[2] + maxs[2] ) - boxcenter[2] );
		}
#endif // VERTICALBIAS

#ifdef SPLASH_HDIST_CLAMP

		// if pushed from below, hack the hitpoint to limit the side push direction
		if( hitpoint[2] < boxcenter[2] && SPLASH_HDIST_CLAMP >= 0 ) {
			// do not allow the hitpoint to be further away
			// than SPLASH_HDIST_CLAMP in the horizontal axis
			vec3_t vec;

			vec[0] = hitpoint[0];
			vec[1] = hitpoint[1];
			vec[2] = boxcenter[2];

			if( Distance( boxcenter, vec ) > SPLASH_HDIST_CLAMP ) {
				VectorSubtract( vec, boxcenter, pushdir );
				VectorNormalize( pushdir );
				VectorMA( boxcenter, SPLASH_HDIST_CLAMP, pushdir, hitpoint );
				hitpoint[2] = point[2]; // restore the original hitpoint height
			}
		}
#endif // SPLASH_HDIST_CLAMP

		VectorSubtract( boxcenter, hitpoint, pushdir );
		VectorNormalize( pushdir );
	}

#undef VERTICALBIAS
#undef CAPSULEDISTANCE
#undef SPLASH_HDIST_CLAMP
}

/**
 * RS_SplashFrac
 * Racesow version of G_SplashFrac by Weqo
 */
void RS_SplashFrac( const vec3_t origin, const vec3_t mins, const vec3_t maxs, const vec3_t point, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac, float splashFrac )
{
	vec3_t boxcenter = { 0, 0, 0 };
	float distance = 0;
	int i;
	float innerradius;
	float outerradius;
	float g_distance;
	float h_distance;

	if( maxradius <= 0 )
	{
		if( kickFrac )
			*kickFrac = 0;
		if( dmgFrac)
			*dmgFrac = 0;

		return;
	}

	innerradius = ( maxs[0] + maxs[1] - mins[0] - mins[1] ) * 0.25;
	outerradius = ( maxs[2] - mins[2] ); // cylinder height

	// find center of the box
	for( i = 0; i < 3; i++ ) {
		boxcenter[i] = origin[i] + maxs[i] + mins[i];
	}

	// find box radius to explosion origin direction
	VectorSubtract( boxcenter, point, pushdir );

	g_distance = sqrtf( pushdir[0]*pushdir[0] + pushdir[1]*pushdir[1] ); // distance on virtual ground
	h_distance = fabsf( pushdir[2] );				    // corrected distance in height

	if( ( h_distance <= outerradius / 2 ) || ( g_distance > innerradius ) )
		distance = g_distance - innerradius;

	if( ( h_distance > outerradius / 2 ) || ( g_distance <= innerradius ) )
		distance = h_distance - outerradius / 2;

	if( ( h_distance > outerradius / 2 ) || ( g_distance > innerradius ) )
		distance = sqrtf( ( g_distance - innerradius ) * ( g_distance - innerradius ) + ( h_distance - outerradius / 2 ) * ( h_distance - outerradius / 2 ) );

	if( dmgFrac )
	{
		// soft sin curve
		*dmgFrac = sinf( DEG2RAD( ( distance / maxradius ) * 90.0f ) );
		Q_clamp( *dmgFrac, 0.0f, 1.0f );
	}

	if( kickFrac )
	{
		distance = fabsf( distance / maxradius );
		Q_clamp( distance, 0.0f, 1.0f );
		*kickFrac = 1.0 - powf( distance, splashFrac );
	}

	VectorSubtract( boxcenter, point, pushdir );
	VectorNormalize( pushdir );
}

class ClipRegion {
	const CMShapeList *m_worldShapeList;
	const std::span<int> m_solidEntNums;
public:
	ClipRegion( const CMShapeList *worldShapeList, const std::span<int> &solidEntNums )
		: m_worldShapeList( worldShapeList ), m_solidEntNums( solidEntNums ) {}

	[[nodiscard]]
	bool castRay( const float *from, const float *to ) const;
};

bool ClipRegion::castRay( const float *from, const float *to ) const {
	trace_t trace;
	SV_ClipToShapeList( m_worldShapeList, &trace, from, to, vec3_origin, vec3_origin, MASK_SOLID );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	const auto *gameEdicts = game.edicts;
	for( const int entNum: m_solidEntNums ) {
		const auto *ent = gameEdicts + entNum;
		// TODO: Hulls should be allowed to be precached once
		// Non-vulnerable entities are assumed to always have a box shape
		const cmodel_s *model = SV_ModelForBBox( ent->r.absmin, ent->r.absmax );
		SV_TransformedBoxTrace( &trace, from, to, vec3_origin, vec3_origin, model, MASK_SOLID, nullptr, nullptr );
		if( trace.fraction != 1.0f ) {
			return false;
		}
	}

	return true;
}

class SplashPropagationSolver {
	using EntNumsVector = wsw::StaticVector<int, 32>;

	struct DamageParams {
		int entNum;
		float damage;
		float knockback;
		float stun;
		float pushDir[3];
	};

	enum : unsigned { kMaxCoordSteps = 11 };

	using DamageParamsVector = wsw::StaticVector<DamageParams, 32>;
	using DamageParamRefsVector = wsw::StaticVector<const DamageParams *, 32>;

	edict_t *const m_inflictor;
	edict_t *const m_attacker;
	const edict_t *const m_ignore;
	const cplane_t *const m_plane;
	const int m_attackerEntNum;
	const int m_mod;

	static inline CMShapeList *s_shapeListStorage { nullptr };

	[[nodiscard]]
	inline bool isVulnerable( const edict_t *ent, bool volatileExplosives ) const;
	[[nodiscard]]
	bool coarseCanSplashDamage( const edict_t *ent ) const;

	void computeDamageParams( const EntNumsVector &vulnerableEnts, DamageParamsVector &damageParams );

	void applyDamage( const DamageParams &params );

	[[nodiscard]]
	auto propagate( const DamageParamRefsVector &entParams,
					unsigned numCoordSteps,
					const ClipRegion &clipRegion,
					uint32_t allEntsMask ) -> uint32_t;

	// Can't figure out a better name
	void operator()();

	SplashPropagationSolver( edict_t *inflictor, edict_t *attacker, const edict_t *ignore, const cplane_t *plane, int mod )
		: m_inflictor( inflictor ), m_attacker( attacker )
		, m_ignore( ignore ), m_plane( plane )
		, m_attackerEntNum( attacker ? ENTNUM( attacker ) : -1 )
		, m_mod( mod ) {}
public:
	static void exec( edict_t *inflictor, edict_t *attacker, const edict_t *ignore, const cplane_t *plane, int mod );
};

auto weaponDefForSelfDamage( const edict_t *inflictor ) -> const gs_weapon_definition_t * {
	if( inflictor->s.type == ET_ROCKET ) {
		return GS_GetWeaponDef( ggs, WEAP_ROCKETLAUNCHER );
	}
	if( inflictor->s.type == ET_GRENADE ) {
		return GS_GetWeaponDef( ggs, WEAP_GRENADELAUNCHER );
	}
	if( inflictor->s.type == ET_PLASMA ) {
		return GS_GetWeaponDef( ggs, WEAP_PLASMAGUN );
	}
	if( inflictor->s.type == ET_BLASTER ) {
		return GS_GetWeaponDef( ggs, WEAP_GUNBLADE );
	}

	return nullptr;
}

inline bool SplashPropagationSolver::isVulnerable( const edict_t *ent, bool volatileExplosives ) const {
	if( ent == m_ignore ) {
		return false;
	}
	if( ent->takedamage ) {
		return true;
	}
	if( !volatileExplosives ) {
		return false;
	}
	if( ent->floodnum ) {
		return false;
	}
	if( ent->s.ownerNum != m_attackerEntNum ) {
		return false;
	}
	const auto entType = ent->s.type;
	return entType == ET_ROCKET || entType == ET_GRENADE || entType == ET_WAVE;
}

void SplashPropagationSolver::computeDamageParams( const EntNumsVector &vulnerableEnts,
												   DamageParamsVector &damageParams ) {
	damageParams.clear();

	const float maxDamage = m_inflictor->projectileInfo.maxDamage;
	float maxKnockback    = m_inflictor->projectileInfo.maxKnockback;
	assert( maxDamage > 0 || maxKnockback > 0 );

	const float minDamage = wsw::min( m_inflictor->projectileInfo.minDamage, maxDamage );
	float minKnockback    = wsw::min( m_inflictor->projectileInfo.minKnockback, maxKnockback );
	const float maxStun   = (float)m_inflictor->projectileInfo.stun;
	const float minStun   = wsw::min( 1.0f, maxStun );

	const float *const origin       = m_inflictor->s.origin;
	const float radius              = m_inflictor->projectileInfo.radius;
	const bool isRaceGametype       = GS_RaceGametype( *ggs );
	const edict_t *const gameEdicts = game.edicts;

	for( int entNum: vulnerableEnts ) {
		float kickFrac = 0.0f, dmgFrac = 0.0f;
		vec3_t pushDir { 0.0f, 0.0f, 1.0f };

		G_SplashFrac( entNum, origin, radius, pushDir, &kickFrac, &dmgFrac );

		float damage = wsw::max( 0.0f, minDamage + ( ( maxDamage - minDamage ) * dmgFrac ) );
		float stun = wsw::max( 0.0f, minStun + ( ( maxStun - minStun ) * dmgFrac ) );
		float knockback = wsw::max( 0.0f, minKnockback + ( ( maxKnockback - minKnockback ) * kickFrac ) );

		// Weapon jumps hack : when knockback on self, use strong weapon definition
		const edict_t *ent = gameEdicts + entNum;
		// If self-damage
		if( ent == m_attacker && ent->r.client ) {
			if( const auto *def = weaponDefForSelfDamage( m_inflictor ) ) {
				const float splashFrac        = def->firedef.splashfrac;
				const float selfDamageScale   = def->firedef.selfdamage;
				maxKnockback = (float)def->firedef.knockback;
				minKnockback = (float)def->firedef.minknockback;
				minKnockback = wsw::min( minKnockback, maxKnockback );
				if( isRaceGametype ) {
					RS_SplashFrac( entNum, origin, radius, pushDir, &kickFrac, nullptr, splashFrac );
				} else {
					G_SplashFrac( entNum, origin, radius, pushDir, &kickFrac, nullptr );
				}
				knockback = ( minKnockback + ( ( maxKnockback - minKnockback ) * kickFrac ) ) * g_self_knockback->value;
				damage *= selfDamageScale;
			}
		}

		if( knockback < 1.0f ) {
			knockback = 0.0f;
		}

		if( stun < 1.0f ) {
			stun = 0.0f;
		}

		if( damage > 0.0f || knockback > 0.0f || stun > 0.0f ) {
			damageParams.emplace_back( {
				.entNum    = entNum,
				.damage    = damage,
				.knockback = knockback,
				.stun      = stun,
				.pushDir   = { pushDir[0], pushDir[1], pushDir[2] }
			});
		}
	}
}

bool SplashPropagationSolver::coarseCanSplashDamage( const edict_t *ent ) const {
	vec3_t origin;
	if( m_plane ) {
		// up by 9 units to account for stairs
		VectorMA( m_inflictor->s.origin, 9, m_plane->normal, origin );
	} else {
		VectorCopy( m_inflictor->s.origin, origin );
	}

	// This is for players

	// Currently keep the old behaviour as-is
	const vec2_t testedOffsets[] = {
		{ 0, 0 }, { +15.0, +15.0 }, { +15.0, -15.0 }, { -15.0, +15.0 }, { -15.0, -15.0 }
	};

	trace_t trace;
	constexpr float minFrac = 1.0f - ( 1.0f / 32.0f );
	for( const auto &[xOffset, yOffset]: testedOffsets ) {
		const vec3_t dest { ent->s.origin[0], ent->s.origin[1] + xOffset, ent->s.origin[2] + yOffset };
		// TODO: Use the ClipRegion for these tests as well
		G_Trace4D( &trace, origin, vec3_origin, vec3_origin, dest, m_inflictor, MASK_SOLID, 0 );
		if( trace.fraction >= minFrac || trace.ent == ENTNUM( ent ) ) {
			return true;
		}
	}

	return false;
}

void SplashPropagationSolver::applyDamage( const DamageParams &params ) {
	const auto &[entNum, damage, kb, stun, pushDir] = params;
	auto *const ent = game.edicts + params.entNum;
	if( ent->takedamage ) {
		const auto &[velocity, origin] = std::make_pair( m_inflictor->velocity, m_inflictor->s.origin );
		G_Damage( ent, m_inflictor, m_attacker, pushDir, velocity, origin, damage, kb, stun, DAMAGE_RADIUS, m_mod );
	} else {
		// Make sure it is going to be skipped during recursive calls
		ent->floodnum = 1;
		if( ent->s.type == ET_ROCKET ) {
			W_Detonate_Rocket( ent, m_inflictor, nullptr, 0 );
		} else if( ent->s.type == ET_GRENADE ) {
			W_Detonate_Grenade( ent, m_inflictor );
		} else if( ent->s.type == ET_WAVE ) {
			W_Detonate_Wave( ent, m_inflictor, nullptr, 0 );
		} else {
			wsw::PodVector<char> message;
			message.append( wsw::StringView( "Unknown entity type " ) );
			message.append( std::to_string( ent->s.type ) );
			message.append( wsw::StringView( " for ent->takedamage == 0" ) );
			wsw::failWithLogicError( message.data() );
		}
	}
}

auto SplashPropagationSolver::propagate( const DamageParamRefsVector &entParams,
										 unsigned numCoordSteps,
										 const ClipRegion &clipRegion,
										 uint32_t allEntsMask ) -> uint32_t {
	assert( numCoordSteps <= kMaxCoordSteps );

	// Propagate a wave front using something like breadth-first search.

	bool isVisited[kMaxCoordSteps * kMaxCoordSteps * kMaxCoordSteps];
	memset( isVisited, 0, sizeof( bool ) * numCoordSteps * numCoordSteps * numCoordSteps );

	struct FringeEntry {
		vec3_t center;
		uint8_t coordIndices[3];
	};

	FringeEntry fringe[kMaxCoordSteps * kMaxCoordSteps * kMaxCoordSteps];

	const auto *const gameEdicts = game.edicts;
	const auto radius            = (float)m_inflictor->projectileInfo.radius;
	const auto squareRadius      = radius * radius;
	const auto coordStep         = 2.0f * radius / (float)numCoordSteps;
	const auto xStride           = numCoordSteps * numCoordSteps;
	const auto yStride           = numCoordSteps;

	const float *__restrict explosionOrigin = m_inflictor->s.origin;
	VectorCopy( explosionOrigin, fringe[0].center );
	assert( numCoordSteps % 2u == 1u );
	const auto midCoordIndex  = numCoordSteps / 2;
	fringe[0].coordIndices[0] = midCoordIndex;
	fringe[0].coordIndices[1] = midCoordIndex;
	fringe[0].coordIndices[2] = midCoordIndex;

	int fringeHead       = 0;
	int fringeTail       = 1;
	uint32_t hitEntsMask = 0;
	while( fringeHead < fringeTail ) {
		const FringeEntry &__restrict entry = fringe[fringeHead++];

		// TODO: Prefer cell mask test!
		// Note that this check still has to be performed for boundary cells (not fully within bounds)
		for( unsigned entIndex = 0; entIndex < entParams.size(); ++entIndex ) {
			auto *__restrict ent = gameEdicts + entParams[entIndex]->entNum;
			if( BoundsIntersect( ent->r.absmin, ent->r.absmax, entry.center, entry.center ) ) {
				hitEntsMask |= ( 1u << entIndex );
			}
		}

		// Check for early exit if we have hit all entities
		if( hitEntsMask == allEntsMask ) [[unlikely]] {
			return hitEntsMask;
		}

		// Propagate wave in directions of all faces of the cubic cell
		for( int side = 0; side < 6; ++side ) {
			int coordNum = side / 2;
			int sign = -1 + 2 * ( side % 2 );
			vec3_t otherCellCenter { entry.center[0], entry.center[1], entry.center[2] };
			otherCellCenter[coordNum] -= (float)sign * coordStep;
			// Testing distance first eliminates a need for checking individual coord indices
			// (x, y, z-indices are valid if the cell is within the distance)
			// so it's actually better as no additional branching is performed.
			if( DistanceSquared( otherCellCenter, explosionOrigin ) < squareRadius ) {
				// Make coordinates for the step destination cell point
				const auto newCoordIndex = wsw::clamp( (int)entry.coordIndices[coordNum] + sign, 0, (int)numCoordSteps - 1 );
				unsigned coordIndices[] { entry.coordIndices[0], entry.coordIndices[1], entry.coordIndices[2] };
				coordIndices[coordNum] = (unsigned)newCoordIndex;
				const unsigned cellIndex = xStride * coordIndices[0] + yStride * coordIndices[1] + coordIndices[2];
				// Try visiting the destination cell
				if( !isVisited[cellIndex] ) {
					if( clipRegion.castRay( entry.center, otherCellCenter ) ) {
						isVisited[cellIndex] = true;
						FringeEntry *__restrict newEntry = &fringe[fringeTail++];
						VectorCopy( otherCellCenter, newEntry->center );
						VectorCopy( coordIndices, newEntry->coordIndices );
					}
				}
			}
		}
	}

	return hitEntsMask;
}

void SplashPropagationSolver::operator()() {
	const auto *const gameEdicts = game.edicts;

	int rawEntNumsBuffer[MAX_EDICTS];
	const auto radius = (float)m_inflictor->projectileInfo.radius;
	const int numEnts = GClip_FindInRadius4D( m_inflictor->s.origin, radius, rawEntNumsBuffer, MAX_EDICTS, 0 );
	const std::span<const int> rawEntNums( rawEntNumsBuffer, rawEntNumsBuffer + numEnts );

	// Do an early shortcut.
	// A top-level call always includes the projectile in the output.
	if( !rawEntNums.empty() && !( rawEntNums.size() == 1 && rawEntNums.front() == m_inflictor->s.number ) ) {
		const bool volatileExplosives = GS_RaceGametype( *ggs ) && g_volatile_explosives->integer != 0;

		// Perform an initial pruning of invulnerable entities
		EntNumsVector vulnerableEntNums;
		if( rawEntNums.size() <= vulnerableEntNums.capacity() ) [[likely]] {
			for( const int entNum: rawEntNums ) {
				if( isVulnerable( gameEdicts + entNum, volatileExplosives ) ) {
					vulnerableEntNums.push_back( entNum );
				}
			}
		} else {
			// A rare but possible case, let's handle it.
			// Give clients a priority.
			for( const int entNum: rawEntNums ) {
				if( const edict_t *ent = gameEdicts + entNum; ent->r.client ) {
					if( isVulnerable( gameEdicts + entNum, volatileExplosives ) ) {
						vulnerableEntNums.push_back( entNum );
						if( vulnerableEntNums.full() ) [[unlikely]] {
							break;
						}
					}
				}
			}
			if( !vulnerableEntNums.full() ) {
				for( const int entNum: rawEntNums ) {
					if( const edict_t *ent = gameEdicts + entNum; !ent->r.client ) {
						if( isVulnerable( ent, volatileExplosives ) ) {
							vulnerableEntNums.push_back( entNum );
							if( vulnerableEntNums.full() ) [[unlikely]] {
								break;
							}
						}
					}
				}
			}
		}

		// Compute ideal (like there's no obstacles in-between) damage parameters.
		// We will apply these parameters to all entities regardless of obstruction status for consistency reasons.

		DamageParamsVector damageParamsStorage;
		computeDamageParams( vulnerableEntNums, damageParamsStorage );

		// Caution! Vulnerability status of other entities may change while applying damage
		// (a chain of doors/triggers is an example, not even talking about implicit recursion while exploding projectiles).
		// This means we cannot rely on computing it once and have to recalculate it where it was implied

		DamageParamRefsVector nonDamagedEnts;
		for( const DamageParams &param: damageParamsStorage ) {
			// Check again each step (damaging entities may change vulnerability status of next entities)
			if( isVulnerable( gameEdicts + param.entNum, volatileExplosives ) ) {
				// Apply damage immediately if fast and coarse checks permit.
				if( coarseCanSplashDamage( gameEdicts + param.entNum ) ) {
					applyDamage( param );
				} else {
					nonDamagedEnts.push_back( std::addressof( param ) );
				}
			}
		}

		// Check afterwards (damaging last entities may change status of previously added entities)
		unsigned numEntsToKeep = 0;
		for( unsigned checkedAtIndex = 0; checkedAtIndex < nonDamagedEnts.size(); ++checkedAtIndex ) {
			const DamageParams *const params = nonDamagedEnts[checkedAtIndex];
			if( isVulnerable( gameEdicts + params->entNum, volatileExplosives ) ) {
				nonDamagedEnts[numEntsToKeep++] = params;
			}
		}
		nonDamagedEnts.erase( nonDamagedEnts.begin() + numEntsToKeep, nonDamagedEnts.end() );

		// If some coarse checks have not lead to success
		if( !nonDamagedEnts.empty() ) {
			uint32_t nonDamagedEntsMask = 0;
			for( unsigned index = 0; index < nonDamagedEntsMask; ++index ) {
				nonDamagedEntsMask |= ( 1u << index );
			}

			EntNumsVector waveObstacles;
			for( const int entNum: rawEntNums ) {
				const edict_t *ent = gameEdicts + entNum;
				if( ent->s.solid == SOLID_YES || ent->s.solid == SOLID_BMODEL ) {
					if( !isVulnerable( ent, volatileExplosives ) ) {
						waveObstacles.push_back( entNum );
						if( waveObstacles.full() ) [[unlikely]] {
							break;
						}
					}
				}
			}

			if( !s_shapeListStorage ) {
				s_shapeListStorage = SV_AllocShapeList();
			}

			const Vec3 splashMins = Vec3( -radius, -radius, -radius ) + m_inflictor->s.origin;
			const Vec3 splashMaxs = Vec3( +radius, +radius, +radius ) + m_inflictor->s.origin;
			// TODO: Add/use a fused call
			SV_BuildShapeList( s_shapeListStorage, splashMins.Data(), splashMaxs.Data(), MASK_SOLID );
			SV_ClipShapeList( s_shapeListStorage, s_shapeListStorage, splashMins.Data(), splashMaxs.Data() );

			const ClipRegion clipRegion( s_shapeListStorage, std::span<int>( waveObstacles.begin(), waveObstacles.size() ) );

			// Do not try adjusting number of steps according to radius.
			// Use a maximal number of steps possible.
			// There is a single exception when radius is really low.
			static_assert( kMaxCoordSteps % 2u == 1u );
			static_assert( ( kMaxCoordSteps / 2 ) % 2u == 1u );
			const auto numCoordSteps = (int)( radius >= 48 ? kMaxCoordSteps : kMaxCoordSteps / 2 );

			// Propagate the wave and mark bits for entities that should accept the damage
			const uint32_t affectedEntsMask = propagate( nonDamagedEnts, numCoordSteps, clipRegion, nonDamagedEntsMask );
			for( unsigned entIndex = 0; entIndex < numEntsToKeep; ++entIndex ) {
				if( affectedEntsMask & ( 1u << entIndex ) ) {
					const DamageParams *params = nonDamagedEnts[entIndex];
					// Check again
					if( isVulnerable( gameEdicts + params->entNum, volatileExplosives ) ) {
						applyDamage( *params );
					}
				}
			}
		}
	}
}

void SplashPropagationSolver::exec( edict_t *inflictor, edict_t *attacker,
									const edict_t *ignore,
									const cplane_t *plane, int mod ) {
	if( inflictor->projectileInfo.radius > 0 ) {
		if( inflictor->projectileInfo.maxDamage > 0 ) {
			if( inflictor->projectileInfo.maxKnockback > 0 ) {
				SplashPropagationSolver solver( inflictor, attacker, ignore, plane, mod );
				solver();
			}
		}
	}
}

/*
* G_RadiusDamage
*/
void G_RadiusDamage( edict_t *inflictor, edict_t *attacker, const cplane_t *plane, const edict_t *ignore, int mod ) {
	SplashPropagationSolver::exec( inflictor, attacker, ignore, plane, mod );
}