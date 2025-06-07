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

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include "q_shared.h"
#include "q_comref.h"
#include "q_collision.h"
#include "gs_public.h"

#define SPEEDKEY    500.0f

#define PM_DASHJUMP_TIMEDELAY 1000 // delay in milliseconds
#define PM_WALLJUMP_TIMEDELAY   1300
#define PM_WALLJUMP_FAILED_TIMEDELAY    700
#define PM_SPECIAL_CROUCH_INHIBIT 400
#define PM_AIRCONTROL_BOUNCE_DELAY 200
#define PM_OVERBOUNCE       1.01f
#define PM_CROUCHSLIDE_TIMEDELAY 700
#define PM_CROUCHSLIDE_CONTROL 3
#define PM_FORWARD_ACCEL_TIMEDELAY 0 // delay before the forward acceleration kicks in

//===============================================================

// all of the locals will be zeroed before each
// pmove, just to make damn sure we don't have
// any differences when running on client or server

typedef struct {
	vec3_t origin;          // full float precision
	vec3_t velocity;        // full float precision

	vec3_t forward, right, up;
	vec3_t flatforward;     // normalized forward without z component, saved here because it needs
	// special handling for looking straight up or down
	float frametime;

	int groundsurfFlags;
	cplane_t groundplane;
	int groundcontents;

	vec3_t previous_origin;
	bool ladder;

	float forwardPush, sidePush, upPush;

	float maxPlayerSpeed;
	float maxWalkSpeed;
	float maxCrouchedSpeed;
	float jumpPlayerSpeed;
	float dashPlayerSpeed;
} pml_t;

// movement parameters

#define DEFAULT_WALKSPEED 160.0f
#define DEFAULT_CROUCHEDSPEED 100.0f
#define DEFAULT_LADDERSPEED 250.0f

const float pm_friction = 8; //  ( initially 6 )
const float pm_waterfriction = 1;
const float pm_wateraccelerate = 10; // user intended acceleration when swimming ( initially 6 )

const float pm_accelerate = 12; // user intended acceleration when on ground or fly movement ( initially 10 )
const float pm_decelerate = 12; // user intended deceleration when on ground

const float pm_airaccelerate = 1; // user intended aceleration when on air
const float pm_airdecelerate = 2.0f; // air deceleration (not +strafe one, just at normal moving).

// special movement parameters

const float pm_aircontrol = 150.0f; // aircontrol multiplier (intertia velocity to forward velocity conversion)
const float pm_strafebunnyaccel = 70; // forward acceleration when strafe bunny hopping
const float pm_wishspeed = 30;

const float pm_dashupspeed = ( 174.0f * GRAVITY_COMPENSATE );

#ifdef OLDWALLJUMP
const float pm_wjupspeed = 370;
const float pm_wjbouncefactor = 0.5f;
#define pm_wjminspeed pm_maxspeed
#else
const float pm_wjupspeed = ( 330.0f * GRAVITY_COMPENSATE );
const float pm_failedwjupspeed = ( 50.0f * GRAVITY_COMPENSATE );
const float pm_wjbouncefactor = 0.3f;
const float pm_failedwjbouncefactor = 0.1f;
#define pm_wjminspeed ( ( pml->maxWalkSpeed + pml->maxPlayerSpeed ) * 0.5f )
#endif

//
// Kurim : some functions/defines that can be useful to work on the horizontal movement of player :
//
#define VectorScale2D( in, scale, out ) ( ( out )[0] = ( in )[0] * ( scale ), ( out )[1] = ( in )[1] * ( scale ) )
#define DotProduct2D( x, y )           ( ( x )[0] * ( y )[0] + ( x )[1] * ( y )[1] )

static vec_t VectorNormalize2D( vec3_t v ) { // ByMiK : normalize horizontally (don't affect Z value)
	float length, ilength;
	length = v[0] * v[0] + v[1] * v[1];
	if( length ) {
		length = sqrt( length ); // FIXME
		ilength = 1.0f / length;
		v[0] *= ilength;
		v[1] *= ilength;
	}
	return length;
}

// Walljump wall availability check
// nbTestDir is the number of directions to test around the player
// maxZnormal is the max Z value of the normal of a poly to consider it a wall
// normal becomes a pointer to the normal of the most appropriate wall
static void PlayerTouchWall( pmove_t *pm, pml_t *pml, const gs_state_t *gs, int nbTestDir, float maxZnormal, vec3_t *normal ) {
	vec3_t zero, dir, mins, maxs;
	trace_t trace;
	int i;
	bool alternate;
	float r, d, dx, dy, m;

	VectorClear( zero );

	// if there is nothing at all within the checked area, we can skip the individual checks
	// this optimization must always overapproximate the combination of those checks
	mins[0] = pm->mins[0] - pm->maxs[0];
	mins[1] = pm->mins[1] - pm->maxs[0];
	maxs[0] = pm->maxs[0] + pm->maxs[0];
	maxs[1] = pm->maxs[1] + pm->maxs[0];
	if( pml->velocity[0] > 0 ) {
		maxs[0] += pml->velocity[0] * 0.015f;
	} else {
		mins[0] += pml->velocity[0] * 0.015f;
	}
	if( pml->velocity[1] > 0 ) {
		maxs[1] += pml->velocity[1] * 0.015f;
	} else {
		mins[1] += pml->velocity[1] * 0.015f;
	}
	mins[2] = maxs[2] = 0;
	gs->Trace( &trace, pml->origin, mins, maxs, pml->origin, pm->playerState->POVnum, pm->contentmask, 0 );
	if( !trace.allsolid && trace.fraction == 1 ) {
		return;
	}

	// determine the primary direction
	if( pml->sidePush > 0 ) {
		r = -M_PI / 2.0f;
	} else if( pml->sidePush < 0 ) {
		r = M_PI / 2.0f;
	} else if( pml->forwardPush > 0 ) {
		r = 0.0f;
	} else {
		r = M_PI;
	}
	alternate = pml->sidePush == 0 || pml->forwardPush == 0;

	d = 0.0f; // current distance from the primary direction

	for( i = 0; i < nbTestDir; i++ ) {
		if( i != 0 ) {
			if( alternate ) {
				r += M_PI; // switch front and back
			}
			if( ( !alternate && i % 2 == 0 ) || ( alternate && i % 4 == 0 ) ) { // switch from left to right
				r -= 2 * d;
			} else if( !alternate || ( alternate && i % 4 == 2 ) ) {   // switch from right to left and move further away
				r += d;
				d += M_TWOPI / nbTestDir;
				r += d;
			}
		}

		// determine the relative offsets from the origin
		dx = cos( DEG2RAD( pm->playerState->viewangles[YAW] ) + r );
		dy = sin( DEG2RAD( pm->playerState->viewangles[YAW] ) + r );

		// project onto the player box
		if( dx == 0 ) {
			m = pm->maxs[1];
		} else if( dy == 0 ) {
			m = pm->maxs[0];
		} else if( fabs( dx / pm->maxs[0] ) > fabs( dy / pm->maxs[1] ) ) {
			m = fabs( pm->maxs[0] / dx );
		} else {
			m = fabs( pm->maxs[1] / dy );
		}

		// allow a gap between the player and the wall
		m += pm->maxs[0];

		dir[0] = pml->origin[0] + dx * m + pml->velocity[0] * 0.015f;
		dir[1] = pml->origin[1] + dy * m + pml->velocity[1] * 0.015f;
		dir[2] = pml->origin[2];

		gs->Trace( &trace, pml->origin, zero, zero, dir, pm->playerState->POVnum, pm->contentmask, 0 );

		if( trace.allsolid ) {
			return;
		}

		if( trace.fraction == 1 ) {
			continue; // no wall in this direction

		}
		if( trace.surfFlags & ( SURF_SKY | SURF_NOWALLJUMP ) ) {
			continue;
		}

		if( trace.ent > 0 && gs->GetEntityState( trace.ent, 0 )->type == ET_PLAYER ) {
			continue;
		}

		if( trace.fraction > 0 && fabs( trace.plane.normal[2] ) < maxZnormal ) {
			VectorCopy( trace.plane.normal, *normal );
			return;
		}
	}
}

//
//  walking up a step should kill some velocity
//

/*
* PM_SlideMove
*
* Returns a new origin, velocity, and contact entity
* Does not modify any world state?
*/

#define MAX_CLIP_PLANES 5

static void PM_AddTouchEnt( pmove_t *pm, int entNum ) {
	int i;

	if( pm->numtouch >= MAXTOUCH || entNum < 0 ) {
		return;
	}

	// see if it is already added
	for( i = 0; i < pm->numtouch; i++ ) {
		if( pm->touchents[i] == entNum ) {
			return;
		}
	}

	// add it
	pm->touchents[pm->numtouch] = entNum;
	pm->numtouch++;
}


static int PM_SlideMove( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	vec3_t end, dir;
	vec3_t old_velocity, last_valid_origin;
	float value;
	vec3_t planes[MAX_CLIP_PLANES];
	int numplanes = 0;
	trace_t trace;
	int moves, i, j, k;
	int maxmoves = 4;
	float remainingTime = pml->frametime;
	int blockedmask = 0;

	VectorCopy( pml->velocity, old_velocity );
	VectorCopy( pml->origin, last_valid_origin );

	if( pm->groundentity != -1 ) { // clip velocity to ground, no need to wait
		// if the ground is not horizontal (a ramp) clipping will slow the player down
		if( pml->groundplane.normal[2] == 1.0f && pml->velocity[2] < 0.0f ) {
			pml->velocity[2] = 0.0f;
		}
	}

	// Do a shortcut in this case
	if( pm->skipCollision ) {
		VectorMA( pml->origin, remainingTime, pml->velocity, pml->origin );
		return blockedmask;
	}

	numplanes = 0; // clean up planes count for checking

	for( moves = 0; moves < maxmoves; moves++ ) {
		VectorMA( pml->origin, remainingTime, pml->velocity, end );
		gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, end, pm->playerState->POVnum, pm->contentmask, 0 );
		if( trace.allsolid ) { // trapped into a solid
			VectorCopy( last_valid_origin, pml->origin );
			return SLIDEMOVEFLAG_TRAPPED;
		}

		if( trace.fraction > 0 ) { // actually covered some distance
			VectorCopy( trace.endpos, pml->origin );
			VectorCopy( trace.endpos, last_valid_origin );
		}

		if( trace.fraction == 1 ) {
			break; // move done

		}
		// save touched entity for return output
		PM_AddTouchEnt( pm, trace.ent );

		// at this point we are blocked but not trapped.

		blockedmask |= SLIDEMOVEFLAG_BLOCKED;
		if( trace.plane.normal[2] < SLIDEMOVE_PLANEINTERACT_EPSILON ) { // is it a vertical wall?
			blockedmask |= SLIDEMOVEFLAG_WALL_BLOCKED;
		}

		remainingTime -= ( trace.fraction * remainingTime );

		// we got blocked, add the plane for sliding along it

		// if this is a plane we have touched before, try clipping
		// the velocity along it's normal and repeat.
		for( i = 0; i < numplanes; i++ ) {
			if( DotProduct( trace.plane.normal, planes[i] ) > ( 1.0f - SLIDEMOVE_PLANEINTERACT_EPSILON ) ) {
				VectorAdd( trace.plane.normal, pml->velocity, pml->velocity );
				break;
			}
		}
		if( i < numplanes ) { // found a repeated plane, so don't add it, just repeat the trace
			continue;
		}

		// security check: we can't store more planes
		if( numplanes >= MAX_CLIP_PLANES ) {
			VectorClear( pml->velocity );
			return SLIDEMOVEFLAG_TRAPPED;
		}

		// put the actual plane in the list
		VectorCopy( trace.plane.normal, planes[numplanes] );
		numplanes++;

		//
		// modify original_velocity so it parallels all of the clip planes
		//

		for( i = 0; i < numplanes; i++ ) {
			if( DotProduct( pml->velocity, planes[i] ) >= SLIDEMOVE_PLANEINTERACT_EPSILON ) { // would not touch it
				continue;
			}

			GS_ClipVelocity( pml->velocity, planes[i], pml->velocity, PM_OVERBOUNCE );
			// see if we enter a second plane
			for( j = 0; j < numplanes; j++ ) {
				if( j == i ) { // it's the same plane
					continue;
				}
				if( DotProduct( pml->velocity, planes[j] ) >= SLIDEMOVE_PLANEINTERACT_EPSILON ) {
					continue; // not with this one

				}
				//there was a second one. Try to slide along it too
				GS_ClipVelocity( pml->velocity, planes[j], pml->velocity, PM_OVERBOUNCE );

				// check if the slide sent it back to the first plane
				if( DotProduct( pml->velocity, planes[i] ) >= SLIDEMOVE_PLANEINTERACT_EPSILON ) {
					continue;
				}

				// bad luck: slide the original velocity along the crease
				CrossProduct( planes[i], planes[j], dir );
				VectorNormalize( dir );
				value = DotProduct( dir, pml->velocity );
				VectorScale( dir, value, pml->velocity );

				// check if there is a third plane, in that case we're trapped
				for( k = 0; k < numplanes; k++ ) {
					if( j == k || i == k ) { // it's the same plane
						continue;
					}
					if( DotProduct( pml->velocity, planes[k] ) >= SLIDEMOVE_PLANEINTERACT_EPSILON ) {
						continue; // not with this one
					}
					VectorClear( pml->velocity );
					break;
				}
			}
		}
	}

	if( pm->playerState->pmove.pm_time != 0 ) {
		VectorCopy( old_velocity, pml->velocity );
	}

	return blockedmask;
}

/*
* PM_StepSlideMove
*
* Each intersection will try to step over the obstruction instead of
* sliding along it.
*/
static void PM_StepSlideMove( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	vec3_t start_o, start_v;
	vec3_t down_o, down_v;
	trace_t trace;
	float down_dist, up_dist;
	float hspeed;
	vec3_t up, down;
	int blocked;

	VectorCopy( pml->origin, start_o );
	VectorCopy( pml->velocity, start_v );

	blocked = PM_SlideMove( pm, pml, gs );

	// We have modified the origin in PM_SlideMove() in this case.
	// No further computations are required.
	if( pm->skipCollision ) {
		return;
	}

	VectorCopy( pml->origin, down_o );
	VectorCopy( pml->velocity, down_v );

	VectorCopy( start_o, up );
	up[2] += STEPSIZE;

	gs->Trace( &trace, up, pm->mins, pm->maxs, up, pm->playerState->POVnum, pm->contentmask, 0 );
	if( trace.allsolid ) {
		return; // can't step up

	}
	// try sliding above
	VectorCopy( up, pml->origin );
	VectorCopy( start_v, pml->velocity );

	PM_SlideMove( pm, pml, gs );

	// push down the final amount
	VectorCopy( pml->origin, down );
	down[2] -= STEPSIZE;
	gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, down, pm->playerState->POVnum, pm->contentmask, 0 );
	if( !trace.allsolid ) {
		VectorCopy( trace.endpos, pml->origin );
	}

	VectorCopy( pml->origin, up );

	// decide which one went farther
	down_dist = ( down_o[0] - start_o[0] ) * ( down_o[0] - start_o[0] )
				+ ( down_o[1] - start_o[1] ) * ( down_o[1] - start_o[1] );
	up_dist = ( up[0] - start_o[0] ) * ( up[0] - start_o[0] )
			  + ( up[1] - start_o[1] ) * ( up[1] - start_o[1] );

	if( down_dist >= up_dist || trace.allsolid || ( trace.fraction != 1.0 && !ISWALKABLEPLANE( &trace.plane ) ) ) {
		VectorCopy( down_o, pml->origin );
		VectorCopy( down_v, pml->velocity );
		return;
	}

	// only add the stepping output when it was a vertical step (second case is at the exit of a ramp)
	if( ( blocked & SLIDEMOVEFLAG_WALL_BLOCKED ) || trace.plane.normal[2] == 1.0f - SLIDEMOVE_PLANEINTERACT_EPSILON ) {
		pm->step = ( pml->origin[2] - pml->previous_origin[2] );
	}

	// Preserve speed when sliding up ramps
	hspeed = sqrt( start_v[0] * start_v[0] + start_v[1] * start_v[1] );
	if( hspeed && ISWALKABLEPLANE( &trace.plane ) ) {
		if( trace.plane.normal[2] >= 1.0f - SLIDEMOVE_PLANEINTERACT_EPSILON ) {
			VectorCopy( start_v, pml->velocity );
		} else {
			VectorNormalize2D( pml->velocity );
			VectorScale2D( pml->velocity, hspeed, pml->velocity );
		}
	}

	// wsw : jal : The following line is what produces the ramp sliding.

	//!! Special case
	// if we were walking along a plane, then we need to copy the Z over
	pml->velocity[2] = down_v[2];
}

/*
* PM_Friction -- Modified for wsw
*
* Handles both ground friction and water friction
*/
static void PM_Friction( pmove_t *pm, pml_t *pml ) {
	float *vel;
	float speed, newspeed, control;
	float friction;
	float drop;

	vel = pml->velocity;

	speed = vel[0] * vel[0] + vel[1] * vel[1] + vel[2] * vel[2];
	if( speed < 1 ) {
		vel[0] = 0;
		vel[1] = 0;
		return;
	}

	speed = sqrt( speed );
	drop = 0;

	// apply ground friction
	if( ( ( ( ( pm->groundentity != -1 ) && !( pml->groundsurfFlags & SURF_SLICK ) ) )
		  && ( pm->waterlevel < 2 ) ) || pml->ladder ) {
		if( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] <= 0 ) {
			friction = pm_friction;
			control = speed < pm_decelerate ? pm_decelerate : speed;
			if( pm->playerState->pmove.pm_flags & PMF_CROUCH_SLIDING ) {
				if( pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] < PM_CROUCHSLIDE_FADE ) {
					friction *= 1 - sqrt( (float)pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] / PM_CROUCHSLIDE_FADE );
				} else {
					friction = 0;
				}
			}
			drop += control * friction * pml->frametime;
		}
	}

	// apply water friction
	if( ( pm->waterlevel >= 2 ) && !pml->ladder ) {
		drop += speed * pm_waterfriction * pm->waterlevel * pml->frametime;
	}

	// scale the velocity
	newspeed = speed - drop;
	if( newspeed <= 0 ) {
		newspeed = 0;
		VectorClear( vel );
	} else {
		newspeed /= speed;
		VectorScale( vel, newspeed, vel );
	}
}

/*
* PM_Accelerate
*
* Handles user intended acceleration
*/
static void PM_Accelerate( pmove_t *pm, pml_t *pml, vec3_t wishdir, float wishspeed, float accel ) {
	float addspeed, accelspeed, currentspeed, realspeed, newspeed;
	bool crouchslide;

	realspeed = VectorLengthFast( pml->velocity );

	currentspeed = DotProduct( pml->velocity, wishdir );
	addspeed = wishspeed - currentspeed;
	if( addspeed <= 0 ) {
		return;
	}

	accelspeed = accel * pml->frametime * wishspeed;
	if( accelspeed > addspeed ) {
		accelspeed = addspeed;
	}

	crouchslide = ( pm->playerState->pmove.pm_flags & PMF_CROUCH_SLIDING ) && pm->groundentity != -1 && !( pml->groundsurfFlags & SURF_SLICK );

	if( crouchslide ) {
		accelspeed *= PM_CROUCHSLIDE_CONTROL;
	}

	VectorMA( pml->velocity, accelspeed, wishdir, pml->velocity );

	if( crouchslide ) { // disable overacceleration while crouch sliding
		newspeed = VectorLengthFast( pml->velocity );
		if( newspeed > wishspeed && newspeed != 0 ) {
			VectorScale( pml->velocity, fmax( wishspeed, realspeed ) / newspeed, pml->velocity );
		}
	}
}

static void PM_AirAccelerate( pml_t *pml, vec3_t wishdir, float wishspeed ) {
	vec3_t curvel, wishvel, acceldir, curdir;
	float addspeed, accelspeed, curspeed;
	float dot;
	// air movement parameters:
	float airforwardaccel = 1.00001f; // Default: 1.0f : how fast you accelerate until you reach pm_maxspeed
	float bunnyaccel = 0.1593f; // (0.42 0.1593f) Default: 0.1585f how fast you accelerate after reaching pm_maxspeed
	// (it gets harder as you near bunnytopspeed)
	float bunnytopspeed = 925; // (0.42: 925) soft speed limit (can get faster with rjs and on ramps)
	float turnaccel = 4.0f;    // (0.42: 9.0) Default: 7 max sharpness of turns
	float backtosideratio = 0.8f; // (0.42: 0.8) Default: 0.8f lower values make it easier to change direction without
	// losing speed; the drawback is "understeering" in sharp turns

	if( !wishspeed ) {
		return;
	}

	VectorCopy( pml->velocity, curvel );
	curvel[2] = 0;
	curspeed = VectorLength( curvel );

	if( wishspeed > curspeed * 1.01f ) { // moving below pm_maxspeed
		accelspeed = curspeed + airforwardaccel * pml->maxPlayerSpeed * pml->frametime;
		if( accelspeed < wishspeed ) {
			wishspeed = accelspeed;
		}
	} else {
		float f = ( bunnytopspeed - curspeed ) / ( bunnytopspeed - pml->maxPlayerSpeed );
		if( f < 0 ) {
			f = 0;
		}
		wishspeed = Q_max( curspeed, pml->maxPlayerSpeed ) + bunnyaccel * f * pml->maxPlayerSpeed * pml->frametime;
	}
	VectorScale( wishdir, wishspeed, wishvel );
	VectorSubtract( wishvel, curvel, acceldir );
	addspeed = VectorNormalize( acceldir );

	accelspeed = turnaccel * pml->maxPlayerSpeed * pml->frametime;
	if( accelspeed > addspeed ) {
		accelspeed = addspeed;
	}

	if( backtosideratio < 1.0f ) {
		VectorNormalize2( curvel, curdir );
		dot = DotProduct( acceldir, curdir );
		if( dot < 0 ) {
			VectorMA( acceldir, -( 1.0f - backtosideratio ) * dot, curdir, acceldir );
		}
	}

	VectorMA( pml->velocity, accelspeed, acceldir, pml->velocity );
}

// when using +strafe convert the inertia to forward speed.
static void PM_Aircontrol( pml_t *pml, vec3_t wishdir, float wishspeed ) {
	int i;
	float zspeed, speed, dot, k;
	float smove;

	if( !pm_aircontrol ) {
		return;
	}

	// accelerate
	smove = pml->sidePush;

	if( ( smove > 0 || smove < 0 ) || ( wishspeed == 0.0 ) ) {
		return; // can't control movement if not moving forward or backward

	}
	zspeed = pml->velocity[2];
	pml->velocity[2] = 0;
	speed = VectorNormalize( pml->velocity );


	dot = DotProduct( pml->velocity, wishdir );
	k = 32.0f * pm_aircontrol * dot * dot * pml->frametime;

	if( dot > 0 ) {
		// we can't change direction while slowing down
		for( i = 0; i < 2; i++ )
			pml->velocity[i] = pml->velocity[i] * speed + wishdir[i] * k;

		VectorNormalize( pml->velocity );
	}

	for( i = 0; i < 2; i++ )
		pml->velocity[i] *= speed;

	pml->velocity[2] = zspeed;
}


/*
* PM_AddCurrents
*/
static void PM_AddCurrents( pmove_t *pm, pml_t *pml, vec3_t wishvel ) {
	//
	// account for ladders
	//

	if( pml->ladder && fabs( pml->velocity[2] ) <= DEFAULT_LADDERSPEED ) {
		if( ( pm->playerState->viewangles[PITCH] <= -15 ) && ( pml->forwardPush > 0 ) ) {
			wishvel[2] = DEFAULT_LADDERSPEED;
		} else if( ( pm->playerState->viewangles[PITCH] >= 15 ) && ( pml->forwardPush > 0 ) ) {
			wishvel[2] = -DEFAULT_LADDERSPEED;
		} else if( pml->upPush > 0 ) {
			wishvel[2] = DEFAULT_LADDERSPEED;
		} else if( pml->upPush < 0 ) {
			wishvel[2] = -DEFAULT_LADDERSPEED;
		} else {
			wishvel[2] = 0;
		}

		// limit horizontal speed when on a ladder
		Q_clamp( wishvel[0], -25, 25 );
		Q_clamp( wishvel[1], -25, 25 );
	}
}

/*
* PM_WaterMove
*
*/
static void PM_WaterMove( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	int i;
	vec3_t wishvel;
	float wishspeed;
	vec3_t wishdir;

	// user intentions
	for( i = 0; i < 3; i++ )
		wishvel[i] = pml->forward[i] * pml->forwardPush + pml->right[i] * pml->sidePush;

	if( !pml->forwardPush && !pml->sidePush && !pml->upPush ) {
		wishvel[2] -= 60; // drift towards bottom
	} else {
		wishvel[2] += pml->upPush;
	}

	PM_AddCurrents( pm, pml, wishvel );

	VectorCopy( wishvel, wishdir );
	wishspeed = VectorNormalize( wishdir );

	if( wishspeed > pml->maxPlayerSpeed ) {
		wishspeed = pml->maxPlayerSpeed / wishspeed;
		VectorScale( wishvel, wishspeed, wishvel );
		wishspeed = pml->maxPlayerSpeed;
	}
	wishspeed *= 0.5;

	PM_Accelerate( pm, pml, wishdir, wishspeed, pm_wateraccelerate );
	PM_StepSlideMove( pm, pml, gs );
}

/*
* PM_Move -- Kurim
*
*/
static void PM_Move( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	int i;
	vec3_t wishvel;
	float fmove, smove;
	vec3_t wishdir;
	float wishspeed;
	float maxspeed;
	float accel;
	float wishspeed2;

	fmove = pml->forwardPush;
	smove = pml->sidePush;

	for( i = 0; i < 2; i++ )
		wishvel[i] = pml->forward[i] * fmove + pml->right[i] * smove;
	wishvel[2] = 0;

	PM_AddCurrents( pm, pml, wishvel );

	VectorCopy( wishvel, wishdir );
	wishspeed = VectorNormalize( wishdir );

	// clamp to server defined max speed

	if( pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] ) {
		maxspeed = pml->maxCrouchedSpeed;
	} else if( ( pm->cmd.buttons & BUTTON_WALK ) && ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_WALK ) ) {
		maxspeed = pml->maxWalkSpeed;
	} else {
		maxspeed = pml->maxPlayerSpeed;
	}

	if( wishspeed > maxspeed ) {
		wishspeed = maxspeed / wishspeed;
		VectorScale( wishvel, wishspeed, wishvel );
		wishspeed = maxspeed;
	}

	if( pml->ladder ) {
		PM_Accelerate( pm, pml, wishdir, wishspeed, pm_accelerate );

		if( !wishvel[2] ) {
			if( pml->velocity[2] > 0 ) {
				pml->velocity[2] -= pm->playerState->pmove.gravity * pml->frametime;
				if( pml->velocity[2] < 0 ) {
					pml->velocity[2]  = 0;
				}
			} else {
				pml->velocity[2] += pm->playerState->pmove.gravity * pml->frametime;
				if( pml->velocity[2] > 0 ) {
					pml->velocity[2]  = 0;
				}
			}
		}

		PM_StepSlideMove( pm, pml, gs );
	} else if( pm->groundentity != -1 ) {
		// walking on ground
		if( pml->velocity[2] > 0 ) {
			pml->velocity[2] = 0; //!!! this is before the accel

		}
		PM_Accelerate( pm, pml, wishdir, wishspeed, pm_accelerate );

		// fix for negative trigger_gravity fields
		if( pm->playerState->pmove.gravity > 0 ) {
			if( pml->velocity[2] > 0 ) {
				pml->velocity[2] = 0;
			}
		} else {
			pml->velocity[2] -= pm->playerState->pmove.gravity * pml->frametime;
		}

		if( !pml->velocity[0] && !pml->velocity[1] ) {
			return;
		}

		PM_StepSlideMove( pm, pml, gs );
	} else if( ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_AIRCONTROL )
			   && !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_FWDBUNNY ) ) {
		// Air Control
		wishspeed2 = wishspeed;
		if( DotProduct( pml->velocity, wishdir ) < 0
			&& !( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING )
			&& ( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] <= 0 ) ) {
			accel = pm_airdecelerate;
		} else {
			accel = pm_airaccelerate;
		}

		// ch : remove knockback test here
		if( ( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING )
		    /* || ( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] > 0 ) */ ) {
			accel = 0; // no stopmove while walljumping

		}
		if( ( smove > 0 || smove < 0 ) && !fmove && ( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] <= 0 ) ) {
			if( wishspeed > pm_wishspeed ) {
				wishspeed = pm_wishspeed;
			}
			accel = pm_strafebunnyaccel;
		}

		// Air control
		PM_Accelerate( pm, pml, wishdir, wishspeed, accel );
		if( pm_aircontrol && !( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING ) && ( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] <= 0 ) ) { // no air ctrl while wjing
			PM_Aircontrol( pml, wishdir, wishspeed2 );
		}

		// add gravity
		pml->velocity[2] -= pm->playerState->pmove.gravity * pml->frametime;
		PM_StepSlideMove( pm, pml, gs );
	} else {   // air movement (old school)
		bool inhibit = false;
		bool accelerating, decelerating;

		accelerating = ( DotProduct( pml->velocity, wishdir ) > 0.0f ) ? true : false;
		decelerating = ( DotProduct( pml->velocity, wishdir ) < -0.0f ) ? true : false;

		if( ( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING ) &&
			( pm->playerState->pmove.stats[PM_STAT_WJTIME] >= ( PM_WALLJUMP_TIMEDELAY - PM_AIRCONTROL_BOUNCE_DELAY ) ) ) {
			inhibit = true;
		}

		if( ( pm->playerState->pmove.pm_flags & PMF_DASHING ) &&
			( pm->playerState->pmove.stats[PM_STAT_DASHTIME] >= ( PM_DASHJUMP_TIMEDELAY - PM_AIRCONTROL_BOUNCE_DELAY ) ) ) {
			inhibit = true;
		}

		if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_FWDBUNNY ) ||
			pm->playerState->pmove.stats[PM_STAT_FWDTIME] > 0 ) {
			inhibit = true;
		}

		// ch : remove this because of the knockback 'bug'?
		/*
		if( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] > 0 )
		    inhibit = true;
		*/

		// (aka +fwdbunny) pressing forward or backward but not pressing strafe and not dashing
		if( accelerating && !inhibit && !smove && fmove ) {
			PM_AirAccelerate( pml, wishdir, wishspeed );
		} else {   // strafe running
			bool aircontrol = true;

			wishspeed2 = wishspeed;
			if( decelerating &&
				!( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING ) ) {
				accel = pm_airdecelerate;
			} else {
				accel = pm_airaccelerate;
			}

			// ch : knockback out
			if( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING
			    /*	|| ( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] > 0 ) */) {
				accel = 0; // no stop-move while wall-jumping
				aircontrol = false;
			}

			if( ( pm->playerState->pmove.pm_flags & PMF_DASHING ) &&
				( pm->playerState->pmove.stats[PM_STAT_DASHTIME] >= ( PM_DASHJUMP_TIMEDELAY - PM_AIRCONTROL_BOUNCE_DELAY ) ) ) {
				aircontrol = false;
			}

			if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_AIRCONTROL ) ) {
				aircontrol = false;
			}

			// +strafe bunnyhopping
			if( aircontrol && smove && !fmove ) {
				if( wishspeed > pm_wishspeed ) {
					wishspeed = pm_wishspeed;
				}

				PM_Accelerate( pm, pml, wishdir, wishspeed, pm_strafebunnyaccel );
				PM_Aircontrol( pml, wishdir, wishspeed2 );
			} else {   // standard movement (includes strafejumping)
				PM_Accelerate( pm, pml, wishdir, wishspeed, accel );
			}
		}

		// add gravity
		pml->velocity[2] -= pm->playerState->pmove.gravity * pml->frametime;
		PM_StepSlideMove( pm, pml, gs );
	}
}

/*
* PM_GoodPosition
*/
static bool PM_GoodPosition( pmove_t *pm, const gs_state_t *gs, vec3_t origin ) {
	if( pm->playerState->pmove.pm_type == PM_SPECTATOR ) {
		return true;
	}

	trace_t trace;
	gs->Trace( &trace, origin, pm->mins, pm->maxs, origin, pm->playerState->POVnum, pm->contentmask, 0 );

	return trace.fraction == 1.0f && !trace.startsolid;
}

enum CalcPositionResult {
	WasGood,
	Corrected,
	Unsolved,
};

// TODO: Constexpr math. Unfortunately, gcem library cannot be used due to license reasons.
static const auto oneOverSqrt2 = 1.0f / sqrtf( 2.0f );
static const auto oneOverSqrt3 = 1.0f / sqrtf( 3.0f );

[[nodiscard]]
static CalcPositionResult PM_CalcGoodPosition( pmove_t *pm, const gs_state_t *gs, const vec3_t givenOrigin, vec3_t resultOrigin ) {
	constexpr float sideShiftsSigns[3] { 0.0f, -1.0f, +1.0f };
	// Push up first, if tests on the original height fail
	constexpr float zShiftSigns[3] { 0.0f, +1.0f, -1.0f };

	for( unsigned zIndex = 0; zIndex < 3; zIndex++ ) {
		for( unsigned yIndex = 0; yIndex < 3; yIndex++ ) {
			for( unsigned xIndex = 0; xIndex < 3; xIndex++ ) {
				vec3_t shift;
				shift[0] = sideShiftsSigns[xIndex];
				shift[1] = sideShiftsSigns[yIndex];
				shift[2] = zShiftSigns[zIndex];

				const auto numShiftDirs = (unsigned)( xIndex > 0 ) + (unsigned)( yIndex > 0 ) + (unsigned)( zIndex > 0 );
				if( numShiftDirs ) {
					// It was not unit in the id Software code as well, for different (coord snapping) reasons though.
					// We think that using shift of the same length for every combination is a good idea.
					constexpr float shiftDistance = 1.0f / 8.0f;
					float scale = shiftDistance;
					if( numShiftDirs == 2 ) {
						scale *= oneOverSqrt2;
					} else if( numShiftDirs == 3 ) {
						scale *= oneOverSqrt3;
					}
					VectorScale( shift, scale, shift );
					assert( fabs( VectorLengthFast( shift ) - shiftDistance ) < 0.001f );
				}

				vec3_t shiftedOrigin;
				VectorAdd( givenOrigin, shift, shiftedOrigin );
				if( PM_GoodPosition( pm, gs, shiftedOrigin ) ) {
					VectorCopy( shiftedOrigin, resultOrigin );
					if( numShiftDirs ) {
						return Corrected;
					}
					return WasGood;
				}
			}
		}
	}
	return Unsolved;
}

/*
* PM_UnstickPosition
*/
static void PM_UnstickPosition( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	if( !pm->skipCollision ) {
		vec3_t goodOrigin;
		const CalcPositionResult positionResult = PM_CalcGoodPosition( pm, gs, pml->origin, goodOrigin );
		if( positionResult != WasGood ) {
			if( positionResult == Corrected ) {
				VectorCopy( goodOrigin, pml->origin );
			} else {
				VectorCopy( pml->previous_origin, pml->origin );
			}
		}
	}
}

/*
* PM_CategorizePosition
*/
static void PM_CategorizePosition( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	vec3_t point;
	int cont;
	int sample1;
	int sample2;

	if( pml->velocity[2] > 180 ) { // !!ZOID changed from 100 to 180 (ramp accel)
		pm->playerState->pmove.pm_flags &= ~PMF_ON_GROUND;
		pm->groundentity = -1;
	} else {
		trace_t trace;
		if( !pm->skipCollision ) {
			// if the player hull point one-quarter unit down is solid, the player is on ground
			// see if standing on something solid
			point[0] = pml->origin[0];
			point[1] = pml->origin[1];
			point[2] = pml->origin[2] - 0.25;

			gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, point, pm->playerState->POVnum, pm->contentmask, 0 );
		} else {
			memset( &trace, 0, sizeof( trace_t ) );
			trace.fraction = 1.0f;
		}

		pml->groundplane = trace.plane;
		pml->groundsurfFlags = trace.surfFlags;
		pml->groundcontents = trace.contents;

		if( ( trace.fraction == 1 ) || ( !ISWALKABLEPLANE( &trace.plane ) && !trace.startsolid ) ) {
			pm->groundentity = -1;
			pm->playerState->pmove.pm_flags &= ~PMF_ON_GROUND;
		} else {
			pm->groundentity = trace.ent;
			pm->groundplane = trace.plane;
			pm->groundsurfFlags = trace.surfFlags;
			pm->groundcontents = trace.contents;

			// hitting solid ground will end a waterjump
			if( pm->playerState->pmove.pm_flags & PMF_TIME_WATERJUMP ) {
				pm->playerState->pmove.pm_flags &= ~( PMF_TIME_WATERJUMP | PMF_TIME_LAND | PMF_TIME_TELEPORT );
				pm->playerState->pmove.pm_time = 0;
			}

			if( !( pm->playerState->pmove.pm_flags & PMF_ON_GROUND ) ) { // just hit the ground
				pm->playerState->pmove.pm_flags |= PMF_ON_GROUND;
			}
		}

		if( ( pm->numtouch < MAXTOUCH ) && ( trace.fraction < 1.0 ) ) {
			pm->touchents[pm->numtouch] = trace.ent;
			pm->numtouch++;
		}
	}

	//
	// get waterlevel, accounting for ducking
	//
	pm->waterlevel = 0;
	pm->watertype = 0;

	sample2 = pm->playerState->viewheight - pm->mins[2];
	sample1 = sample2 / 2;

	point[0] = pml->origin[0];
	point[1] = pml->origin[1];
	point[2] = pml->origin[2] + pm->mins[2] + 1;
	cont = gs->PointContents( point, 0 );

	if( cont & MASK_WATER ) {
		pm->watertype = cont;
		pm->waterlevel = 1;
		point[2] = pml->origin[2] + pm->mins[2] + sample1;
		cont = gs->PointContents( point, 0 );
		if( cont & MASK_WATER ) {
			pm->waterlevel = 2;
			point[2] = pml->origin[2] + pm->mins[2] + sample2;
			cont = gs->PointContents( point, 0 );
			if( cont & MASK_WATER ) {
				pm->waterlevel = 3;
			}
		}
	}
}

static void PM_ClearDash( pmove_t *pm ) {
	pm->playerState->pmove.pm_flags &= ~PMF_DASHING;
	pm->playerState->pmove.stats[PM_STAT_DASHTIME] = 0;
}

static void PM_ClearWallJump( pmove_t *pm ) {
	pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPING;
	pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPCOUNT;
	pm->playerState->pmove.stats[PM_STAT_WJTIME] = 0;
}

static void PM_ClearStun( pmove_t *pm ) {
	pm->playerState->pmove.stats[PM_STAT_STUN] = 0;
}

/*
* PM_CheckJump
*/
static void PM_CheckJump( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	if( pml->upPush < 10 ) {
		// not holding jump
		if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CONTINOUSJUMP ) ) {
			pm->playerState->pmove.pm_flags &= ~PMF_JUMP_HELD;
		}
		return;
	}

	if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CONTINOUSJUMP ) ) {
		// must wait for jump to be released
		if( pm->playerState->pmove.pm_flags & PMF_JUMP_HELD ) {
			return;
		}
	}

	if( pm->playerState->pmove.pm_type != PM_NORMAL ) {
		return;
	}

	if( pm->waterlevel >= 2 ) { // swimming, not jumping
		pm->groundentity = -1;
		return;
	}

	if( pm->groundentity == -1 ) {
		return;
	}

	if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_JUMP ) ) {
		return;
	}

	if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CONTINOUSJUMP ) ) {
		pm->playerState->pmove.pm_flags |= PMF_JUMP_HELD;
	}

	pm->groundentity = -1;

	// clip against the ground when jumping if moving that direction
	if( pml->groundplane.normal[2] > 0 && pml->velocity[2] < 0 && DotProduct2D( pml->groundplane.normal, pml->velocity ) > 0 ) {
		GS_ClipVelocity( pml->velocity, pml->groundplane.normal, pml->velocity, PM_OVERBOUNCE );
	}

	//if( gs.module == GS_MODULE_GAME ) GS_Printf( "upvel %f\n", pml->velocity[2] );
	if( pml->velocity[2] > 100 ) {
		gs->PredictedEvent( pm->playerState->POVnum, EV_DOUBLEJUMP, 0 );
		pml->velocity[2] += pml->jumpPlayerSpeed;
	} else if( pml->velocity[2] > 0 ) {
		gs->PredictedEvent( pm->playerState->POVnum, EV_JUMP, 0 );
		pml->velocity[2] += pml->jumpPlayerSpeed;
	} else {
		gs->PredictedEvent( pm->playerState->POVnum, EV_JUMP, 0 );
		pml->velocity[2] = pml->jumpPlayerSpeed;
	}

	// remove wj count
	pm->playerState->pmove.pm_flags &= ~PMF_JUMPPAD_TIME;
	PM_ClearDash( pm );
	PM_ClearWallJump( pm );
}

/*
* PM_CheckDash -- by Kurim
*/
static void PM_CheckDash( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	float actual_velocity;
	float upspeed;
	vec3_t dashdir;

	if( !( pm->cmd.buttons & BUTTON_SPECIAL ) ) {
		pm->playerState->pmove.pm_flags &= ~PMF_SPECIAL_HELD;
	}

	if( pm->playerState->pmove.pm_type != PM_NORMAL ) {
		return;
	}

	if( pm->playerState->pmove.stats[PM_STAT_DASHTIME] > 0 ) {
		return;
	}

	if( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] > 0 ) { // can not start a new dash during knockback time
		return;
	}

	if( ( pm->cmd.buttons & BUTTON_SPECIAL ) && pm->groundentity != -1
		&& ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_DASH ) ) {
		if( pm->playerState->pmove.pm_flags & PMF_SPECIAL_HELD ) {
			return;
		}

		pm->playerState->pmove.pm_flags &= ~PMF_JUMPPAD_TIME;
		PM_ClearWallJump( pm );

		pm->playerState->pmove.pm_flags |= PMF_DASHING;
		pm->playerState->pmove.pm_flags |= PMF_SPECIAL_HELD;
		pm->groundentity = -1;

		// clip against the ground when jumping if moving that direction
		if( pml->groundplane.normal[2] > 0 && pml->velocity[2] < 0 && DotProduct2D( pml->groundplane.normal, pml->velocity ) > 0 ) {
			GS_ClipVelocity( pml->velocity, pml->groundplane.normal, pml->velocity, PM_OVERBOUNCE );
		}

		if( pml->velocity[2] <= 0.0f ) {
			upspeed = pm_dashupspeed;
		} else {
			upspeed = pm_dashupspeed + pml->velocity[2];
		}

		// ch : we should do explicit forwardPush here, and ignore sidePush ?
		VectorMA( vec3_origin, pml->forwardPush, pml->flatforward, dashdir );
		VectorMA( dashdir, pml->sidePush, pml->right, dashdir );
		dashdir[2] = 0.0;

		if( VectorLength( dashdir ) < 0.01f ) { // if not moving, dash like a "forward dash"
			VectorCopy( pml->flatforward, dashdir );
		}

		VectorNormalizeFast( dashdir );

		actual_velocity = VectorNormalize2D( pml->velocity );
		if( actual_velocity <= pml->dashPlayerSpeed ) {
			VectorScale( dashdir, pml->dashPlayerSpeed, dashdir );
		} else {
			VectorScale( dashdir, actual_velocity, dashdir );
		}

		VectorCopy( dashdir, pml->velocity );
		pml->velocity[2] = upspeed;

		pm->playerState->pmove.stats[PM_STAT_DASHTIME] = PM_DASHJUMP_TIMEDELAY;

		// return sound events
		if( fabs( pml->sidePush ) > 10 && fabs( pml->sidePush ) >= fabs( pml->forwardPush ) ) {
			if( pml->sidePush > 0 ) {
				gs->PredictedEvent( pm->playerState->POVnum, EV_DASH, 2 );
			} else {
				gs->PredictedEvent( pm->playerState->POVnum, EV_DASH, 1 );
			}
		} else if( pml->forwardPush < -10 ) {
			gs->PredictedEvent( pm->playerState->POVnum, EV_DASH, 3 );
		} else {
			gs->PredictedEvent( pm->playerState->POVnum, EV_DASH, 0 );
		}
	} else if( pm->groundentity == -1 ) {
		pm->playerState->pmove.pm_flags &= ~PMF_DASHING;
	}
}

/*
* PM_CheckWallJump -- By Kurim
*/
static void PM_CheckWallJump( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	vec3_t normal;
	float hspeed;

	if( !( pm->cmd.buttons & BUTTON_SPECIAL ) ) {
		pm->playerState->pmove.pm_flags &= ~PMF_SPECIAL_HELD;
	}

	if( pm->groundentity != -1 ) {
		pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPING;
		pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPCOUNT;
	}

	if( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING && pml->velocity[2] < 0.0 ) {
		pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPING;
	}

	if( pm->playerState->pmove.stats[PM_STAT_WJTIME] <= 0 ) { // reset the wj count after wj delay
		pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPCOUNT;
	}

	if( pm->playerState->pmove.pm_type != PM_NORMAL ) {
		return;
	}

	// don't walljump in the first 100 milliseconds of a dash jump
	if( pm->playerState->pmove.pm_flags & PMF_DASHING
		&& ( pm->playerState->pmove.stats[PM_STAT_DASHTIME] > ( PM_DASHJUMP_TIMEDELAY - 100 ) ) ) {
		return;
	}


	// markthis

	if( pm->groundentity == -1 && ( pm->cmd.buttons & BUTTON_SPECIAL )
		&& ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_WALLJUMP ) &&
		( !( pm->playerState->pmove.pm_flags & PMF_WALLJUMPCOUNT ) )
		&& pm->playerState->pmove.stats[PM_STAT_WJTIME] <= 0
		&& !pm->skipCollision
		) {
		trace_t trace;
		vec3_t point;

		point[0] = pml->origin[0];
		point[1] = pml->origin[1];
		point[2] = pml->origin[2] - STEPSIZE;

		vec3_t hvelocity { pml->velocity[0], pml->velocity[1], 0.0f };
		// don't walljump if our height is smaller than a step
		// unless jump is pressed or the player is moving faster than dash speed and upwards
		hspeed = VectorLengthFast( hvelocity );
		gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, point, pm->playerState->POVnum, pm->contentmask, 0 );

		if( pml->upPush >= 10
			|| ( hspeed > pm->playerState->pmove.stats[PM_STAT_DASHSPEED] && pml->velocity[2] > 8 )
			|| ( trace.fraction == 1 ) || ( !ISWALKABLEPLANE( &trace.plane ) && !trace.startsolid ) ) {
			VectorClear( normal );
			PlayerTouchWall( pm, pml, gs, 20, 0.3f, &normal );
			if( !VectorLength( normal ) ) {
				return;
			}

			if( !( pm->playerState->pmove.pm_flags & PMF_SPECIAL_HELD )
				&& !( pm->playerState->pmove.pm_flags & PMF_WALLJUMPING ) ) {
				float oldupvelocity = pml->velocity[2];
				pml->velocity[2] = 0.0;

				hspeed = VectorNormalize2D( pml->velocity );

				// if stunned almost do nothing
				if( pm->playerState->pmove.stats[PM_STAT_STUN] > 0 ) {
					GS_ClipVelocity( pml->velocity, normal, pml->velocity, 1.0f );
					VectorMA( pml->velocity, pm_failedwjbouncefactor, normal, pml->velocity );

					VectorNormalize( pml->velocity );

					VectorScale( pml->velocity, hspeed, pml->velocity );
					pml->velocity[2] = ( oldupvelocity + pm_failedwjupspeed > pm_failedwjupspeed ) ? oldupvelocity : oldupvelocity + pm_failedwjupspeed;
				} else {
					GS_ClipVelocity( pml->velocity, normal, pml->velocity, 1.0005f );
					VectorMA( pml->velocity, pm_wjbouncefactor, normal, pml->velocity );

					if( hspeed < pm_wjminspeed ) {
						hspeed = pm_wjminspeed;
					}

					VectorNormalize( pml->velocity );

					VectorScale( pml->velocity, hspeed, pml->velocity );
					pml->velocity[2] = ( oldupvelocity > pm_wjupspeed ) ? oldupvelocity : pm_wjupspeed; // jal: if we had a faster upwards speed, keep it
				}

				// set the walljumping state
				PM_ClearDash( pm );
				pm->playerState->pmove.pm_flags &= ~PMF_JUMPPAD_TIME;

				pm->playerState->pmove.pm_flags |= PMF_WALLJUMPING;
				pm->playerState->pmove.pm_flags |= PMF_SPECIAL_HELD;

				pm->playerState->pmove.pm_flags |= PMF_WALLJUMPCOUNT;

				if( pm->playerState->pmove.stats[PM_STAT_STUN] > 0 ) {
					pm->playerState->pmove.stats[PM_STAT_WJTIME] = PM_WALLJUMP_FAILED_TIMEDELAY;

					// Create the event
					gs->PredictedEvent( pm->playerState->POVnum, EV_WALLJUMP_FAILED, DirToByte( normal ) );
				} else {
					pm->playerState->pmove.stats[PM_STAT_WJTIME] = PM_WALLJUMP_TIMEDELAY;

					// Create the event
					gs->PredictedEvent( pm->playerState->POVnum, EV_WALLJUMP, DirToByte( normal ) );
				}
			}
		}
	} else {
		pm->playerState->pmove.pm_flags &= ~PMF_WALLJUMPING;
	}
}

/*
* PM_CheckCrouchSlide
*/
static void PM_CheckCrouchSlide( pmove_t *pm, pml_t *pml ) {
	if( !( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CROUCHSLIDING ) ) {
		return;
	}

	vec3_t hvelocity { pml->velocity[0], pml->velocity[1], 0.0f };
	if( pml->upPush < 0 && VectorLengthFast( hvelocity ) > pml->maxWalkSpeed ) {
		if( pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] > 0 ) {
			return; // cooldown or already sliding

		}
		if( pm->groundentity != -1 ) {
			return; // already on the ground

		}
		// start sliding when we land
		pm->playerState->pmove.pm_flags |= PMF_CROUCH_SLIDING;
		pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] = PM_CROUCHSLIDE + PM_CROUCHSLIDE_FADE;
	} else if( pm->playerState->pmove.pm_flags & PMF_CROUCH_SLIDING ) {
		pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] = Q_min( pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME], PM_CROUCHSLIDE_FADE );
	}
}

/*
* PM_CheckSpecialMovement
*/
static void PM_CheckSpecialMovement( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	vec3_t spot;
	int cont;
	trace_t trace;

	pm->ladder = false;

	if( pm->playerState->pmove.pm_time ) {
		return;
	}

	pml->ladder = false;

	// check for ladder
	if( !pm->skipCollision && !pm->skipLadders ) {
		VectorMA( pml->origin, 1, pml->flatforward, spot );
		gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, spot, pm->playerState->POVnum, pm->contentmask, 0 );
		if( ( trace.fraction < 1 ) && ( trace.surfFlags & SURF_LADDER ) ) {
			pml->ladder = true;
			pm->ladder = true;
		}
	}

	// check for water jump
	if( pm->waterlevel != 2 ) {
		return;
	}

	VectorMA( pml->origin, 30, pml->flatforward, spot );
	spot[2] += 4;
	cont = gs->PointContents( spot, 0 );
	if( !( cont & CONTENTS_SOLID ) ) {
		return;
	}

	spot[2] += 16;
	cont = gs->PointContents( spot, 0 );
	if( cont ) {
		return;
	}
	// jump out of water
	VectorScale( pml->flatforward, 50, pml->velocity );
	pml->velocity[2] = 350;

	pm->playerState->pmove.pm_flags |= PMF_TIME_WATERJUMP;
	pm->playerState->pmove.pm_time = 255;
}

/*
* PM_FlyMove
*/
static void PM_FlyMove( pmove_t *pm, pml_t *pml, const gs_state_t *gs, bool doclip ) {
	float speed, drop, friction, control, newspeed;
	float currentspeed, addspeed, accelspeed, maxspeed;
	int i;
	vec3_t wishvel;
	float fmove, smove;
	vec3_t wishdir;
	float wishspeed;
	vec3_t end;
	trace_t trace;

	maxspeed = pml->maxPlayerSpeed * 1.5;

	if( pm->cmd.buttons & BUTTON_SPECIAL ) {
		maxspeed *= 2;
	}

	// friction
	speed = VectorLength( pml->velocity );
	if( speed < 1 ) {
		VectorClear( pml->velocity );
	} else {
		drop = 0;

		friction = pm_friction * 1.5; // extra friction
		control = speed < pm_decelerate ? pm_decelerate : speed;
		drop += control * friction * pml->frametime;

		// scale the velocity
		newspeed = speed - drop;
		if( newspeed < 0 ) {
			newspeed = 0;
		}
		newspeed /= speed;

		VectorScale( pml->velocity, newspeed, pml->velocity );
	}

	// accelerate
	fmove = pml->forwardPush;
	smove = pml->sidePush;

	if( pm->cmd.buttons & BUTTON_SPECIAL ) {
		fmove *= 2;
		smove *= 2;
	}

	VectorNormalize( pml->forward );
	VectorNormalize( pml->right );

	for( i = 0; i < 3; i++ )
		wishvel[i] = pml->forward[i] * fmove + pml->right[i] * smove;
	wishvel[2] += pml->upPush;

	VectorCopy( wishvel, wishdir );
	wishspeed = VectorNormalize( wishdir );


	// clamp to server defined max speed
	//
	if( wishspeed > maxspeed ) {
		wishspeed = maxspeed / wishspeed;
		VectorScale( wishvel, wishspeed, wishvel );
		wishspeed = maxspeed;
	}

	currentspeed = DotProduct( pml->velocity, wishdir );
	addspeed = wishspeed - currentspeed;
	if( addspeed > 0 ) {
		accelspeed = pm_accelerate * pml->frametime * wishspeed;
		if( accelspeed > addspeed ) {
			accelspeed = addspeed;
		}

		for( i = 0; i < 3; i++ )
			pml->velocity[i] += accelspeed * wishdir[i];
	}

	if( doclip ) {
		for( i = 0; i < 3; i++ )
			end[i] = pml->origin[i] + pml->frametime * pml->velocity[i];

		gs->Trace( &trace, pml->origin, pm->mins, pm->maxs, end, pm->playerState->POVnum, pm->contentmask, 0 );

		VectorCopy( trace.endpos, pml->origin );
	} else {
		// move
		VectorMA( pml->origin, pml->frametime, pml->velocity, pml->origin );
	}
}

static void PM_CheckZoom( pmove_t *pm ) {
	if( pm->playerState->pmove.pm_type != PM_NORMAL ) {
		pm->playerState->pmove.stats[PM_STAT_ZOOMTIME] = 0;
		return;
	}

	if( ( pm->cmd.buttons & BUTTON_ZOOM ) && ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_ZOOM ) ) {
		pm->playerState->pmove.stats[PM_STAT_ZOOMTIME] += pm->cmd.msec;
		Q_clamp( pm->playerState->pmove.stats[PM_STAT_ZOOMTIME], 0, ZOOMTIME );
	} else if( pm->playerState->pmove.stats[PM_STAT_ZOOMTIME] > 0 ) {
		pm->playerState->pmove.stats[PM_STAT_ZOOMTIME] -= pm->cmd.msec;
		Q_clamp( pm->playerState->pmove.stats[PM_STAT_ZOOMTIME], 0, ZOOMTIME );
	}
}

/*
* PM_AdjustBBox
*
* Sets mins, maxs, and pm->viewheight
*/
static void PM_AdjustBBox( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	float crouchFrac;
	trace_t trace;

	if( pm->playerState->pmove.pm_type == PM_GIB ) {
		pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] = 0;
		VectorCopy( playerbox_gib_maxs, pm->maxs );
		VectorCopy( playerbox_gib_mins, pm->mins );
		pm->playerState->viewheight = playerbox_gib_viewheight;
		return;
	}

	if( pm->playerState->pmove.pm_type >= PM_FREEZE ) {
		pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] = 0;
		pm->playerState->viewheight = 0;
		return;
	}

	if( pm->playerState->pmove.pm_type == PM_SPECTATOR ) {
		pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] = 0;
		pm->playerState->viewheight = playerbox_stand_viewheight;
	}

	if( pml->upPush < 0 && ( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CROUCH ) &&
		pm->playerState->pmove.stats[PM_STAT_WJTIME] < ( PM_WALLJUMP_TIMEDELAY - PM_SPECIAL_CROUCH_INHIBIT ) &&
		pm->playerState->pmove.stats[PM_STAT_DASHTIME] < ( PM_DASHJUMP_TIMEDELAY - PM_SPECIAL_CROUCH_INHIBIT ) ) {
		pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] += pm->cmd.msec;
		Q_clamp( pm->playerState->pmove.stats[PM_STAT_CROUCHTIME], 0, CROUCHTIME );

		crouchFrac = (float)pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] / (float)CROUCHTIME;
		VectorLerp( playerbox_stand_mins, crouchFrac, playerbox_crouch_mins, pm->mins );
		VectorLerp( playerbox_stand_maxs, crouchFrac, playerbox_crouch_maxs, pm->maxs );
		pm->playerState->viewheight = playerbox_stand_viewheight - ( crouchFrac * ( playerbox_stand_viewheight - playerbox_crouch_viewheight ) );

		// it's going down, so, no need of checking for head-chomping
		return;
	}

	// it's crouched, but not pressing the crouch button anymore, try to stand up
	if( pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] != 0 ) {
		vec3_t curmins, curmaxs, wishmins, wishmaxs;
		float curviewheight, wishviewheight;
		int newcrouchtime;

		// find the current size
		crouchFrac = (float)pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] / (float)CROUCHTIME;
		VectorLerp( playerbox_stand_mins, crouchFrac, playerbox_crouch_mins, curmins );
		VectorLerp( playerbox_stand_maxs, crouchFrac, playerbox_crouch_maxs, curmaxs );
		curviewheight = playerbox_stand_viewheight - ( crouchFrac * ( playerbox_stand_viewheight - playerbox_crouch_viewheight ) );

		if( !pm->cmd.msec ) { // no need to continue
			VectorCopy( curmins, pm->mins );
			VectorCopy( curmaxs, pm->maxs );
			pm->playerState->viewheight = curviewheight;
			return;
		}

		// find the desired size
		newcrouchtime = pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] - pm->cmd.msec;
		Q_clamp( newcrouchtime, 0, CROUCHTIME );
		crouchFrac = (float)newcrouchtime / (float)CROUCHTIME;
		VectorLerp( playerbox_stand_mins, crouchFrac, playerbox_crouch_mins, wishmins );
		VectorLerp( playerbox_stand_maxs, crouchFrac, playerbox_crouch_maxs, wishmaxs );
		wishviewheight = playerbox_stand_viewheight - ( crouchFrac * ( playerbox_stand_viewheight - playerbox_crouch_viewheight ) );

		// check that the head is not blocked
		gs->Trace( &trace, pml->origin, wishmins, wishmaxs, pml->origin, pm->playerState->POVnum, pm->contentmask, 0 );
		if( trace.allsolid || trace.startsolid ) {
			// can't do the uncrouching, let the time alone and use old position
			VectorCopy( curmins, pm->mins );
			VectorCopy( curmaxs, pm->maxs );
			pm->playerState->viewheight = curviewheight;
			return;
		}

		// can do the uncrouching, use new position and update the time
		pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] = newcrouchtime;
		VectorCopy( wishmins, pm->mins );
		VectorCopy( wishmaxs, pm->maxs );
		pm->playerState->viewheight = wishviewheight;
		return;
	}

	// the player is not crouching at all
	VectorCopy( playerbox_stand_mins, pm->mins );
	VectorCopy( playerbox_stand_maxs, pm->maxs );
	pm->playerState->viewheight = playerbox_stand_viewheight;
}

/*
* PM_AdjustViewheight
*/
void PM_AdjustViewheight( pmove_t * ) {

}

static void PM_UpdateDeltaAngles( pmove_t *pm, const gs_state_t *gs ) {
	int i;

	if( gs->module != GS_MODULE_GAME ) {
		return;
	}

	for( i = 0; i < 3; i++ )
		pm->playerState->pmove.delta_angles[i] = ANGLE2SHORT( pm->playerState->viewangles[i] ) - pm->cmd.angles[i];
}

/*
* PM_ApplyMouseAnglesClamp
*
*/
#if defined ( _WIN32 ) && ( _MSC_VER >= 1400 )
#pragma warning( push )
#pragma warning( disable : 4310 )   // cast truncates constant value
#endif
static void PM_ApplyMouseAnglesClamp( pmove_t *pm, pml_t *pml ) {
	int i;
	short temp;

	for( i = 0; i < 3; i++ ) {
		temp = pm->cmd.angles[i] + pm->playerState->pmove.delta_angles[i];
		if( i == PITCH ) {
			// don't let the player look up or down more than 90 degrees
			if( temp > (short)ANGLE2SHORT( 90 ) - 1 ) {
				pm->playerState->pmove.delta_angles[i] = ( ANGLE2SHORT( 90 ) - 1 ) - pm->cmd.angles[i];
				temp = (short)ANGLE2SHORT( 90 ) - 1;
			} else if( temp < (short)ANGLE2SHORT( -90 ) + 1 ) {
				pm->playerState->pmove.delta_angles[i] = ( ANGLE2SHORT( -90 ) + 1 ) - pm->cmd.angles[i];
				temp = (short)ANGLE2SHORT( -90 ) + 1;
			}
		}

		pm->playerState->viewangles[i] = SHORT2ANGLE( (short)temp );
	}

	AngleVectors( pm->playerState->viewangles, pml->forward, pml->right, pml->up );

	VectorCopy( pml->forward, pml->flatforward );
	pml->flatforward[2] = 0.0f;
	VectorNormalize( pml->flatforward );

	VectorCopy( pml->forward, pm->forward );
	VectorCopy( pml->right, pm->right );
}
#if defined ( _WIN32 ) && ( _MSC_VER >= 1400 )
#pragma warning( pop )
#endif

/*
* PM_BeginMove
*/
static void PM_BeginMove( pmove_t *pm, pml_t *pml, const gs_state_t *gs ) {
	// clear results
	pm->numtouch = 0;
	pm->groundentity = -1;
	pm->watertype = 0;
	pm->waterlevel = 0;
	pm->step = false;

	// clear all pmove local vars
	memset( pml, 0, sizeof( pml_t ) );

	if( !pm->skipCollision && pm->snapInitially ) {
		vec3_t goodOrigin;
		const CalcPositionResult positionResult = PM_CalcGoodPosition( pm, gs, pm->playerState->pmove.origin, goodOrigin );
		if( positionResult == Corrected ) {
			VectorCopy( goodOrigin, pm->playerState->pmove.origin );
		}
	}

	VectorCopy( pm->playerState->pmove.origin, pml->origin );
	VectorCopy( pm->playerState->pmove.velocity, pml->velocity );

	// save old org in case we get stuck
	VectorCopy( pm->playerState->pmove.origin, pml->previous_origin );
}

/*
* PM_EndMove
*/
static void PM_EndMove( pmove_t *pm, pml_t *pml ) {
#if 0
	if( !PM_GoodPosition( pml->origin ) ) {
		gs->Printf( "Stuck!\n" );
	}
#endif
	VectorCopy( pml->origin, pm->playerState->pmove.origin );
	VectorCopy( pml->velocity, pm->playerState->pmove.velocity );
}

/*
* Pmove
*
* Can be called by either the server or the client
*/
void Pmove( const gs_state_t *gs, pmove_t *pm ) {
	float fallvelocity, falldelta, damage;
	int oldGroundEntity;

	if( !pm->playerState ) {
		return;
	}

	pml_t pmlStorage;
	pml_t *pml = &pmlStorage;

	// clear all pmove local vars
	PM_BeginMove( pm, pml, gs );

	fallvelocity = ( ( pml->velocity[2] < 0.0f ) ? fabs( pml->velocity[2] ) : 0.0f );

	pml->frametime = pm->cmd.msec * 0.001;

	pml->maxPlayerSpeed = pm->playerState->pmove.stats[PM_STAT_MAXSPEED];
	if( pml->maxPlayerSpeed < 0 ) {
		pml->maxPlayerSpeed = GS_DefaultPlayerSpeed( *gs );
	}

	pml->jumpPlayerSpeed = (float)pm->playerState->pmove.stats[PM_STAT_JUMPSPEED] * GRAVITY_COMPENSATE;
	if( pml->jumpPlayerSpeed < 0 ) {
		pml->jumpPlayerSpeed = DEFAULT_JUMPSPEED * GRAVITY_COMPENSATE;
	}

	pml->dashPlayerSpeed = pm->playerState->pmove.stats[PM_STAT_DASHSPEED];
	if( pml->dashPlayerSpeed < 0 ) {
		pml->dashPlayerSpeed = DEFAULT_DASHSPEED;
	}

	pml->maxWalkSpeed = DEFAULT_WALKSPEED;
	if( pml->maxWalkSpeed > pml->maxPlayerSpeed * 0.66f ) {
		pml->maxWalkSpeed = pml->maxPlayerSpeed * 0.66f;
	}

	pml->maxCrouchedSpeed = DEFAULT_CROUCHEDSPEED;
	if( pml->maxCrouchedSpeed > pml->maxPlayerSpeed * 0.5f ) {
		pml->maxCrouchedSpeed = pml->maxPlayerSpeed * 0.5f;
	}

	// assign a contentmask for the movement type
	switch( pm->playerState->pmove.pm_type ) {
		case PM_FREEZE:
		case PM_CHASECAM:
			if( gs->module == GS_MODULE_GAME ) {
				pm->playerState->pmove.pm_flags |= PMF_NO_PREDICTION;
			}
			pm->contentmask = 0;
			break;

		case PM_GIB:
			if( gs->module == GS_MODULE_GAME ) {
				pm->playerState->pmove.pm_flags |= PMF_NO_PREDICTION;
			}
			pm->contentmask = MASK_DEADSOLID;
			break;

		case PM_SPECTATOR:
			if( gs->module == GS_MODULE_GAME ) {
				pm->playerState->pmove.pm_flags &= ~PMF_NO_PREDICTION;
			}
			pm->contentmask = MASK_DEADSOLID;
			break;

		default:
		case PM_NORMAL:
			if( gs->module == GS_MODULE_GAME ) {
				pm->playerState->pmove.pm_flags &= ~PMF_NO_PREDICTION;
			}
			if( pm->playerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_GHOSTMOVE ) {
				pm->contentmask = MASK_DEADSOLID;
			} else {
				pm->contentmask = MASK_PLAYERSOLID;
			}
			break;
	}

	if( !GS_MatchPaused( *gs ) ) {
		// drop timing counters
		if( pm->playerState->pmove.pm_time ) {
			int msec;

			msec = pm->cmd.msec >> 3;
			if( !msec ) {
				msec = 1;
			}
			if( msec >= pm->playerState->pmove.pm_time ) {
				pm->playerState->pmove.pm_flags &= ~( PMF_TIME_WATERJUMP | PMF_TIME_LAND | PMF_TIME_TELEPORT );
				pm->playerState->pmove.pm_time = 0;
			} else {
				pm->playerState->pmove.pm_time -= msec;
			}
		}

		if( pm->playerState->pmove.stats[PM_STAT_NOUSERCONTROL] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_NOUSERCONTROL] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_NOUSERCONTROL] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_NOUSERCONTROL] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] = 0;
		}

		// PM_STAT_CROUCHTIME is handled at PM_AdjustBBox
		// PM_STAT_ZOOMTIME is handled at PM_CheckZoom

		if( pm->playerState->pmove.stats[PM_STAT_DASHTIME] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_DASHTIME] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_DASHTIME] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_DASHTIME] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_WJTIME] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_WJTIME] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_WJTIME] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_WJTIME] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_NOAUTOATTACK] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_NOAUTOATTACK] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_NOAUTOATTACK] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_NOAUTOATTACK] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_STUN] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_STUN] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_STUN] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_STUN] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_FWDTIME] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_FWDTIME] -= pm->cmd.msec;
		} else if( pm->playerState->pmove.stats[PM_STAT_FWDTIME] < 0 ) {
			pm->playerState->pmove.stats[PM_STAT_FWDTIME] = 0;
		}

		if( pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] > 0 ) {
			pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] -= pm->cmd.msec;
			if( pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] <= 0 ) {
				if( pm->playerState->pmove.pm_flags & PMF_CROUCH_SLIDING ) {
					pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] = PM_CROUCHSLIDE_TIMEDELAY;
				} else {
					pm->playerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] = 0;
				}
				pm->playerState->pmove.pm_flags &= ~PMF_CROUCH_SLIDING;
			}
		}
	}

	pml->forwardPush = pm->cmd.forwardmove * SPEEDKEY / 127.0f;
	pml->sidePush = pm->cmd.sidemove * SPEEDKEY / 127.0f;
	pml->upPush = pm->cmd.upmove * SPEEDKEY / 127.0f;

	if( pm->playerState->pmove.stats[PM_STAT_NOUSERCONTROL] > 0 ) {
		pml->forwardPush = 0;
		pml->sidePush = 0;
		pml->upPush = 0;
		pm->cmd.buttons = 0;
	}

	// in order the forward accelt to kick in, one has to keep +fwd pressed
	// for some time without strafing
	if( pml->forwardPush <= 0 || pml->sidePush ) {
		pm->playerState->pmove.stats[PM_STAT_FWDTIME] = PM_FORWARD_ACCEL_TIMEDELAY;
	}

	if( pm->playerState->pmove.pm_type != PM_NORMAL ) { // includes dead, freeze, chasecam...
		if( !GS_MatchPaused( *gs ) ) {
			PM_ClearDash( pm );

			PM_ClearWallJump( pm );

			PM_ClearStun( pm );

			pm->playerState->pmove.stats[PM_STAT_KNOCKBACK] = 0;
			pm->playerState->pmove.stats[PM_STAT_CROUCHTIME] = 0;
			pm->playerState->pmove.stats[PM_STAT_ZOOMTIME] = 0;
			pm->playerState->pmove.pm_flags &= ~( PMF_JUMPPAD_TIME | PMF_DOUBLEJUMPED | PMF_TIME_WATERJUMP | PMF_TIME_LAND | PMF_TIME_TELEPORT | PMF_SPECIAL_HELD );

			PM_AdjustBBox( pm, pml, gs );
		}

		PM_AdjustViewheight( pm );

		if( pm->playerState->pmove.pm_type == PM_SPECTATOR ) {
			PM_ApplyMouseAnglesClamp( pm, pml );

			PM_FlyMove( pm, pml, gs, false );
		} else {
			pml->forwardPush = 0;
			pml->sidePush = 0;
			pml->upPush = 0;
		}

		PM_UnstickPosition( pm, pml, gs );
		PM_EndMove( pm, pml );
		return;
	}

	PM_ApplyMouseAnglesClamp( pm, pml );

	// set mins, maxs, viewheight amd fov
	PM_AdjustBBox( pm, pml, gs );

	PM_CheckZoom( pm );

	// round up mins/maxs to hull size and adjust the viewheight, if needed
	PM_AdjustViewheight( pm );

	// set groundentity, watertype, and waterlevel
	PM_CategorizePosition( pm, pml, gs );

	oldGroundEntity = pm->groundentity;

	PM_CheckSpecialMovement( pm, pml, gs );

	if( pm->playerState->pmove.pm_flags & PMF_TIME_TELEPORT ) {
		// teleport pause stays exactly in place
	} else if( pm->playerState->pmove.pm_flags & PMF_TIME_WATERJUMP ) {
		// waterjump has no control, but falls
		pml->velocity[2] -= pm->playerState->pmove.gravity * pml->frametime;
		if( pml->velocity[2] < 0 ) {
			// cancel as soon as we are falling down again
			pm->playerState->pmove.pm_flags &= ~( PMF_TIME_WATERJUMP | PMF_TIME_LAND | PMF_TIME_TELEPORT );
			pm->playerState->pmove.pm_time = 0;
		}

		PM_StepSlideMove( pm, pml, gs );
	} else {
		// Kurim
		// Keep this order !
		PM_CheckJump( pm, pml, gs );

		PM_CheckDash( pm, pml, gs );

		PM_CheckWallJump( pm, pml, gs );

		PM_CheckCrouchSlide( pm, pml );

		PM_Friction( pm, pml );

		if( pm->waterlevel >= 2 ) {
			PM_WaterMove( pm, pml, gs );
		} else {
			vec3_t angles;

			VectorCopy( pm->playerState->viewangles, angles );
			if( angles[PITCH] > 180 ) {
				angles[PITCH] = angles[PITCH] - 360;
			}
			angles[PITCH] /= 3;

			AngleVectors( angles, pml->forward, pml->right, pml->up );

			// hack to work when looking straight up and straight down
			if( pml->forward[2] == -1.0f ) {
				VectorCopy( pml->up, pml->flatforward );
			} else if( pml->forward[2] == 1.0f ) {
				VectorCopy( pml->up, pml->flatforward );
				VectorNegate( pml->flatforward, pml->flatforward );
			} else {
				VectorCopy( pml->forward, pml->flatforward );
			}
			pml->flatforward[2] = 0.0f;
			VectorNormalize( pml->flatforward );

			PM_Move( pm, pml, gs );
		}
	}

	PM_UnstickPosition( pm, pml, gs );
	// set groundentity, watertype, and waterlevel for final spot
	PM_CategorizePosition( pm, pml, gs );
	PM_EndMove( pm, pml );

	// falling event

#define FALL_DAMAGE_MIN_DELTA 675
#define FALL_STEP_MIN_DELTA 400
#define MAX_FALLING_DAMAGE 15
#define FALL_DAMAGE_SCALE 1.0

	// Execute the triggers that are touched.
	// We check the entire path between the origin before the pmove and the
	// current origin to ensure no triggers are missed at high velocity.
	// Note that this method assumes the movement has been linear.
	gs->PMoveTouchTriggers( pm, pml->previous_origin );

	PM_UpdateDeltaAngles( pm, gs ); // in case some trigger action has moved the view angles (like teleported).

	// touching triggers may force groundentity off
	if( !( pm->playerState->pmove.pm_flags & PMF_ON_GROUND ) && pm->groundentity != -1 ) {
		pm->groundentity = -1;
		pml->velocity[2] = 0;
	}

	if( pm->groundentity != -1 ) { // remove wall-jump and dash bits when touching ground
		// always keep the dash flag 50 msecs at least (to prevent being removed at the start of the dash)
		if( pm->playerState->pmove.stats[PM_STAT_DASHTIME] < ( PM_DASHJUMP_TIMEDELAY - 50 ) ) {
			pm->playerState->pmove.pm_flags &= ~PMF_DASHING;
		}

		if( pm->playerState->pmove.stats[PM_STAT_WJTIME] < ( PM_WALLJUMP_TIMEDELAY - 50 ) ) {
			PM_ClearWallJump( pm );
		}
	}

	if( oldGroundEntity == -1 ) {
		falldelta = fallvelocity - ( ( pml->velocity[2] < 0.0f ) ? fabs( pml->velocity[2] ) : 0.0f );

		// scale delta if in water
		if( pm->waterlevel == 3 ) {
			falldelta = 0;
		}
		if( pm->waterlevel == 2 ) {
			falldelta *= 0.25;
		}
		if( pm->waterlevel == 1 ) {
			falldelta *= 0.5;
		}

		if( falldelta > FALL_STEP_MIN_DELTA ) {
			if( !GS_FallDamage( *gs ) || ( pml->groundsurfFlags & SURF_NODAMAGE ) || ( pm->playerState->pmove.pm_flags & PMF_JUMPPAD_TIME ) ) {
				damage = 0;
			} else {
				damage = ( ( falldelta - FALL_DAMAGE_MIN_DELTA ) / 10 ) * FALL_DAMAGE_SCALE;
				Q_clamp( damage, 0.0f, MAX_FALLING_DAMAGE );
			}

			gs->PredictedEvent( pm->playerState->POVnum, EV_FALL, damage );
		}

		pm->playerState->pmove.pm_flags &= ~PMF_JUMPPAD_TIME;
	}
}
