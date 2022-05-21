/*
Copyright (C) 2002-2003 Victor Luchits

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

#include "cg_local.h"
#include "../qcommon/cvar.h"
#include "../qcommon/qcommon.h"
#include "../ui/uisystem.h"
#include "../client/snd_public.h"

static void CG_UpdateEntities( void );

/*
* CG_FixVolumeCvars
* Don't let the user go too far away with volumes
*/
static void CG_FixVolumeCvars( void ) {
	if( developer->integer ) {
		return;
	}

	if( cg_volume_players->value < 0.0f ) {
		Cvar_SetValue( "cg_volume_players", 0.0f );
	} else if( cg_volume_players->value > 2.0f ) {
		Cvar_SetValue( "cg_volume_players", 2.0f );
	}

	if( cg_volume_effects->value < 0.0f ) {
		Cvar_SetValue( "cg_volume_effects", 0.0f );
	} else if( cg_volume_effects->value > 2.0f ) {
		Cvar_SetValue( "cg_volume_effects", 2.0f );
	}

	if( cg_volume_announcer->value < 0.0f ) {
		Cvar_SetValue( "cg_volume_announcer", 0.0f );
	} else if( cg_volume_announcer->value > 2.0f ) {
		Cvar_SetValue( "cg_volume_announcer", 2.0f );
	}

	if( cg_volume_hitsound->value < 0.0f ) {
		Cvar_SetValue( "cg_volume_hitsound", 0.0f );
	} else if( cg_volume_hitsound->value > 10.0f ) {
		Cvar_SetValue( "cg_volume_hitsound", 10.0f );
	}
}

//#define DRAWFROMWEAPON

static bool CG_UpdateLinearProjectilePosition( centity_t *cent ) {
	vec3_t origin;
	entity_state_t *state;
	int moveTime;
	int64_t serverTime;
#define MIN_DRAWDISTANCE_FIRSTPERSON 86
#define MIN_DRAWDISTANCE_THIRDPERSON 52

	state = &cent->current;

	if( !state->linearMovement ) {
		return -1;
	}

	if( GS_MatchPaused() ) {
		serverTime = cg.frame.serverTime;
	} else {
		serverTime = cg.time + cgs.extrapolationTime;
	}

	if( state->solid != SOLID_BMODEL ) {
		// add a time offset to counter antilag visualization
		if( !cgs.demoPlaying && cg_projectileAntilagOffset->value > 0.0f &&
			!ISVIEWERENTITY( state->ownerNum ) && ( cgs.playerNum + 1 != cg.predictedPlayerState.POVnum ) ) {
			serverTime += state->modelindex2 * cg_projectileAntilagOffset->value;
		}
	}

	moveTime = GS_LinearMovement( state, serverTime, origin );
	VectorCopy( origin, state->origin );

	if( ( moveTime < 0 ) && ( state->solid != SOLID_BMODEL ) ) {
		// when flyTime is negative don't offset it backwards more than PROJECTILE_PRESTEP value
		// FIXME: is this still valid?
		float maxBackOffset;

		if( ISVIEWERENTITY( state->ownerNum ) ) {
			maxBackOffset = ( PROJECTILE_PRESTEP - MIN_DRAWDISTANCE_FIRSTPERSON );
		} else {
			maxBackOffset = ( PROJECTILE_PRESTEP - MIN_DRAWDISTANCE_THIRDPERSON );
		}

		if( DistanceFast( state->origin2, state->origin ) > maxBackOffset ) {
			return false;
		}
	}

	return true;
#undef MIN_DRAWDISTANCE_FIRSTPERSON
#undef MIN_DRAWDISTANCE_THIRDPERSON
}

/*
* CG_NewPacketEntityState
*/
static void CG_NewPacketEntityState( entity_state_t *state ) {
	centity_t *cent;

	cent = &cg_entities[state->number];

	VectorClear( cent->prevVelocity );
	cent->canExtrapolatePrev = false;

	if( ISEVENTENTITY( state ) ) {
		cent->prev = cent->current;
		cent->current = *state;
		cent->serverFrame = cg.frame.serverFrame;

		VectorClear( cent->velocity );
		cent->canExtrapolate = false;
	} else if( state->linearMovement ) {
		if( cent->serverFrame != cg.oldFrame.serverFrame || state->teleported ||
			state->linearMovement != cent->current.linearMovement || state->linearMovementTimeStamp != cent->current.linearMovementTimeStamp ) {
			cent->prev = *state;
		} else {
			cent->prev = cent->current;
		}

		cent->current = *state;
		cent->serverFrame = cg.frame.serverFrame;

		VectorClear( cent->velocity );
		cent->canExtrapolate = false;

#ifdef DRAWFROMWEAPON
		if( firstTime && ISVIEWERENTITY( state->ownerNum ) && ( state->solid != SOLID_BMODEL ) ) { // experimental. Draw linear projectiles as if coming from the weapon
			orientation_t projection;
			vec3_t dir, end;

			if( CG_PModel_GetProjectionSource( state->ownerNum, &projection ) ) {
				VectorCopy( projection.origin, cent->linearProjectileViewerSource );

				// recalculate the direction from the new projection source
				VectorNormalize2( state->linearProjectileVelocity, dir );
				VectorMA( state->origin2, 16000, dir, end );
				VectorSubtract( end, cent->linearProjectileViewerSource, dir );
				VectorNormalize( dir );
				VectorScale( dir, VectorLength( state->linearProjectileVelocity ), cent->linearProjectileViewerVelocity );
			} else {
				//Com_Printf( "Couldn't get projection source\n" );
				VectorCopy( state->linearProjectileVelocity, cent->linearProjectileViewerVelocity );
				VectorCopy( state->origin2, cent->linearProjectileViewerSource );
			}
		}
#endif
		cent->linearProjectileCanDraw = CG_UpdateLinearProjectilePosition( cent );

		VectorCopy( cent->current.linearMovementVelocity, cent->velocity );
	} else {
		// if it moved too much force the teleported bit
		if(  abs( (int)( cent->current.origin[0] - state->origin[0] ) ) > 512
			 || abs( (int)( cent->current.origin[1] - state->origin[1] ) ) > 512
			 || abs( (int)( cent->current.origin[2] - state->origin[2] ) ) > 512 ) {
			cent->serverFrame = -99;
		}

		// some data changes will force no lerping
		if( state->modelindex != cent->current.modelindex
			|| state->teleported 
			|| state->linearMovement != cent->current.linearMovement ) {
			cent->serverFrame = -99;
		}

		if( cent->serverFrame != cg.oldFrame.serverFrame ) {
			// wasn't in last update, so initialize some things
			// duplicate the current state so lerping doesn't hurt anything
			cent->prev = *state;

			memset( cent->localEffects, 0, sizeof( cent->localEffects ) );
			cg.effectsSystem.resetEntityEffects( cent->current.number );

			// Init the animation when new into PVS
			if( cg.frame.valid && ( state->type == ET_PLAYER || state->type == ET_CORPSE ) ) {
				cent->lastAnims = 0;
				memset( cent->lastVelocities, 0, sizeof( cent->lastVelocities ) );
				memset( cent->lastVelocitiesFrames, 0, sizeof( cent->lastVelocitiesFrames ) );
				CG_PModel_ClearEventAnimations( state->number );
				memset( &cg_entPModels[state->number].animState, 0, sizeof( cg_entPModels[state->number].animState ) );

				// if it's a player and new in PVS, remove the old power time
				// This is way far from being the right thing. But will make it less bad for now
				cg_entPModels[state->number].flash_time = cg.time;
				cg_entPModels[state->number].barrel_time = cg.time;
			}
		} else {   // shuffle the last state to previous
			cent->prev = cent->current;
		}

		if( cent->serverFrame != cg.oldFrame.serverFrame ) {
			cent->microSmooth = 0;
		}

		cent->current = *state;
		VectorCopy( cent->velocity, cent->prevVelocity );

		//VectorCopy( cent->extrapolatedOrigin, cent->prevExtrapolatedOrigin );
		cent->canExtrapolatePrev = cent->canExtrapolate;
		cent->canExtrapolate = false;
		VectorClear( cent->velocity );
		cent->serverFrame = cg.frame.serverFrame;

		// set up velocities for this entity
		if( cgs.extrapolationTime &&
			( cent->current.type == ET_PLAYER || cent->current.type == ET_CORPSE ) ) {
			VectorCopy( cent->current.origin2, cent->velocity );
			VectorCopy( cent->prev.origin2, cent->prevVelocity );
			cent->canExtrapolate = cent->canExtrapolatePrev = true;
		} else if( !VectorCompare( cent->prev.origin, cent->current.origin ) ) {
			float snapTime = ( cg.frame.serverTime - cg.oldFrame.serverTime );

			if( !snapTime ) {
				snapTime = cgs.snapFrameTime;
			}

			VectorSubtract( cent->current.origin, cent->prev.origin, cent->velocity );
			VectorScale( cent->velocity, 1000.0f / snapTime, cent->velocity );
		}

		if( ( cent->current.type == ET_GENERIC || cent->current.type == ET_PLAYER
			  || cent->current.type == ET_GIB || cent->current.type == ET_GRENADE
			  || cent->current.type == ET_ITEM || cent->current.type == ET_CORPSE ) ) {
			cent->canExtrapolate = true;
		}

		if( ISBRUSHMODEL( cent->current.modelindex ) ) { // disable extrapolation on movers
			cent->canExtrapolate = false;
		}

		//if( cent->canExtrapolate )
		//	VectorMA( cent->current.origin, 0.001f * cgs.extrapolationTime, cent->velocity, cent->extrapolatedOrigin );
	}
}

int CG_LostMultiviewPOV( void ) {
	int best, value, fallback;
	int i, index;

	best = gs.maxclients;
	index = fallback = -1;

	for( i = 0; i < cg.frame.numplayers; i++ ) {
		value = abs( (int)cg.frame.playerStates[i].playerNum - (int)cg.multiviewPlayerNum );
		if( value == best && i > index ) {
			continue;
		}

		if( value < best ) {
			if( cg.frame.playerStates[i].pmove.pm_type == PM_SPECTATOR ) {
				fallback = i;
				continue;
			}

			best = value;
			index = i;
		}
	}

	return index > -1 ? index : fallback;
}

static void CG_SetFramePlayerState( snapshot_t *frame, int index ) {
	frame->playerState = frame->playerStates[index];
	if( cgs.demoPlaying || cg.frame.multipov ) {
		frame->playerState.pmove.pm_flags |= PMF_NO_PREDICTION;
		if( frame->playerState.pmove.pm_type != PM_SPECTATOR ) {
			frame->playerState.pmove.pm_type = PM_CHASECAM;
		}
	}
}

static void CG_UpdatePlayerState( void ) {
	int i;
	int index = 0;

	if( cg.frame.multipov ) {
		// find the playerState containing our current POV, then cycle playerStates
		index = -1;
		for( i = 0; i < cg.frame.numplayers; i++ ) {
			if( cg.frame.playerStates[i].playerNum < (unsigned)gs.maxclients
				&& cg.frame.playerStates[i].playerNum == cg.multiviewPlayerNum ) {
				index = i;
				break;
			}
		}

		// the POV was lost, find the closer one (may go up or down, but who cares)
		if( index < 0 || cg.frame.playerStates[index].pmove.pm_type == PM_SPECTATOR ) {
			index = CG_LostMultiviewPOV();
		}
		if( index < 0 ) {
			index = 0;
		}
	}

	cg.multiviewPlayerNum = cg.frame.playerStates[index].playerNum;

	// set up the playerstates

	// current
	CG_SetFramePlayerState( &cg.frame, index );

	// old
	index = -1;
	for( i = 0; i < cg.oldFrame.numplayers; i++ ) {
		if( cg.oldFrame.playerStates[i].playerNum == cg.multiviewPlayerNum ) {
			index = i;
			break;
		}
	}

	// use the current one for old frame too, if correct POV wasn't found
	if( index == -1 ) {
		cg.oldFrame.playerState = cg.frame.playerState;
	} else {
		CG_SetFramePlayerState( &cg.oldFrame, index );
	}

	cg.predictedPlayerState = cg.frame.playerState;
}

/*
* CG_NewFrameSnap
* a new frame snap has been received from the server
*/
bool CG_NewFrameSnap( snapshot_t *frame, snapshot_t *lerpframe ) {
	int i;

	assert( frame );

	if( lerpframe ) {
		cg.oldFrame = *lerpframe;
	} else {
		cg.oldFrame = *frame;
	}

	cg.frame = *frame;
	gs.gameState = frame->gameState;

	cg.portalInView = false;

	if( cg_projectileAntilagOffset->value > 1.0f || cg_projectileAntilagOffset->value < 0.0f ) {
		Cvar_ForceSet( "cg_projectileAntilagOffset", cg_projectileAntilagOffset->dvalue );
	}

	CG_UpdatePlayerState();

	static_assert( AccuracyRows::Span::extent == kNumAccuracySlots );
	wsw::ui::UISystem::instance()->updateScoreboard( frame->scoreboardData, AccuracyRows {
		.weak   = AccuracyRows::Span( frame->playerState.weakAccuracy ),
		.strong = AccuracyRows::Span( frame->playerState.strongAccuracy )
	});

	for( i = 0; i < frame->numEntities; i++ )
		CG_NewPacketEntityState( &frame->parsedEntities[i & ( MAX_PARSE_ENTITIES - 1 )] );

	if( lerpframe && ( memcmp( cg.oldFrame.areabits, cg.frame.areabits, cg.frame.areabytes ) == 0 ) ) {
		cg.oldAreabits = true;
	} else {
		cg.oldAreabits = false;
	}

	if( !cgs.precacheDone || !cg.frame.valid ) {
		return false;
	}

	cg.specStateChanged = SPECSTATECHANGED() || lerpframe == NULL || cg.firstFrame;

	// a new server frame begins now
	CG_FixVolumeCvars();

	CG_BuildSolidList();
	CG_UpdateEntities();
	CG_CheckPredictionError();

	cg.predictFrom = 0; // force the prediction to be restarted from the new snapshot
	cg.fireEvents = true;

	for( i = 0; i < cg.frame.numgamecommands; i++ ) {
		int target = cg.frame.playerState.POVnum - 1;
		if( cg.frame.gamecommands[i].all || cg.frame.gamecommands[i].targets[target >> 3] & ( 1 << ( target & 7 ) ) ) {
			CG_GameCommand( cg.frame.gamecommandsData + cg.frame.gamecommands[i].commandOffset );
		}
	}

	CG_FireEvents( true );

	if( cg.firstFrame && !cgs.demoPlaying ) {
		// request updates on our private state
		Cbuf_ExecuteText( EXEC_NOW, "upstate" );
	}

	cg.firstFrame = false; // not the first frame anymore
	return true;
}


//=============================================================


/*
==========================================================================

ADD INTERPOLATED ENTITIES TO RENDERING LIST

==========================================================================
*/

/*
* CG_CModelForEntity
*  get the collision model for the given entity, no matter if box or brush-model.
*/
const cmodel_s *CG_CModelForEntity( int entNum ) {
	int x, zd, zu;
	centity_t *cent;
	const cmodel_s *cmodel = NULL;
	vec3_t bmins, bmaxs;

	if( entNum < 0 || entNum >= MAX_EDICTS ) {
		return NULL;
	}

	cent = &cg_entities[entNum];

	if( cent->serverFrame != cg.frame.serverFrame ) { // not present in current frame
		return NULL;
	}

	// find the cmodel
	if( cent->current.solid == SOLID_BMODEL ) { // special value for bmodel
		cmodel = CG_InlineModel( cent->current.modelindex );
	} else if( cent->current.solid ) {   // encoded bbox
		x = 8 * ( cent->current.solid & 31 );
		zd = 8 * ( ( cent->current.solid >> 5 ) & 31 );
		zu = 8 * ( ( cent->current.solid >> 10 ) & 63 ) - 32;

		bmins[0] = bmins[1] = -x;
		bmaxs[0] = bmaxs[1] = x;
		bmins[2] = -zd;
		bmaxs[2] = zu;
		if( cent->type == ET_PLAYER || cent->type == ET_CORPSE ) {
			cmodel = CG_OctagonModelForBBox( bmins, bmaxs );
		} else {
			cmodel = CG_ModelForBBox( bmins, bmaxs );
		}
	}

	return cmodel;
}

/*
* CG_DrawEntityBox
* draw the bounding box (in brush models case the box containing the model)
*/
void CG_DrawEntityBox( centity_t *cent ) {
}

/*
* CG_EntAddBobEffect
*/
static void CG_EntAddBobEffect( centity_t *cent ) {
	double scale;
	double bob;

	scale = 0.005 + cent->current.number * 0.00001;
	bob = 4 + cos( ( cg.time + 1000 ) * scale ) * 4;

	cent->ent.origin2[2] += bob;
	cent->ent.origin[2] += bob;
	cent->ent.lightingOrigin[2] += bob;
}

/*
* CG_EntAddTeamColorTransitionEffect
*/
static void CG_EntAddTeamColorTransitionEffect( centity_t *cent ) {
	float f;
	uint8_t *currentcolor;
	vec4_t scaledcolor, newcolor;
	const vec4_t neutralcolor = { 1.0f, 1.0f, 1.0f, 1.0f };

	f = (float)cent->current.counterNum / 255.0f;
	Q_clamp( f, 0.0f, 1.0f );

	if( cent->current.type == ET_PLAYER || cent->current.type == ET_CORPSE ) {
		currentcolor = CG_PlayerColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	} else {
		currentcolor = CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	}

	Vector4Scale( currentcolor, 1.0 / 255.0, scaledcolor );
	VectorLerp( neutralcolor, f, scaledcolor, newcolor );

	cent->ent.shaderRGBA[0] = (uint8_t)( newcolor[0] * 255 );
	cent->ent.shaderRGBA[1] = (uint8_t)( newcolor[1] * 255 );
	cent->ent.shaderRGBA[2] = (uint8_t)( newcolor[2] * 255 );
}

/*
* CG_AddLinkedModel
*/
static void CG_AddLinkedModel( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	static entity_t ent;
	orientation_t tag;
	struct model_s *model;

	// linear projectiles can never have a linked model. Modelindex2 is used for a different purpose
	if( cent->current.linearMovement ) {
		return;
	}

	model = cgs.modelDraw[cent->current.modelindex2];
	if( !model ) {
		return;
	}

	memset( &ent, 0, sizeof( entity_t ) );
	ent.rtype = RT_MODEL;
	ent.scale = cent->ent.scale;
	ent.renderfx = cent->ent.renderfx;
	ent.shaderTime = cent->ent.shaderTime;
	Vector4Copy( cent->ent.shaderRGBA, ent.shaderRGBA );
	ent.model = model;
	ent.customShader = NULL;
	ent.customSkin = NULL;
	VectorCopy( cent->ent.origin, ent.origin );
	VectorCopy( cent->ent.origin, ent.origin2 );
	VectorCopy( cent->ent.lightingOrigin, ent.lightingOrigin );
	Matrix3_Copy( cent->ent.axis, ent.axis );

	if( cent->item && ( cent->effects & EF_AMMOBOX ) ) { // ammobox icon hack
		ent.customShader = R_RegisterPic( cent->item->icon );
	}

	if( cent->item && ( cent->item->type & IT_WEAPON ) ) {
		if( CG_GrabTag( &tag, &cent->ent, "tag_barrel" ) ) {
			CG_PlaceModelOnTag( &ent, &cent->ent, &tag );
		}
	} else {
		if( CG_GrabTag( &tag, &cent->ent, "tag_linked" ) ) {
			CG_PlaceModelOnTag( &ent, &cent->ent, &tag );
		}
	}

	CG_AddColoredOutLineEffect( &ent, cent->effects,
								cent->outlineColor[0], cent->outlineColor[1], cent->outlineColor[2], cent->outlineColor[3] );
	CG_AddEntityToScene( &ent, drawSceneRequest );
	CG_AddShellEffects( &ent, cent->effects, drawSceneRequest );
}

/*
* CG_AddCentityOutLineEffect
*/
void CG_AddCentityOutLineEffect( centity_t *cent ) {
	CG_AddColoredOutLineEffect( &cent->ent, cent->effects,
								cent->outlineColor[0], cent->outlineColor[1], cent->outlineColor[2], cent->outlineColor[3] );
}

//==========================================================================
//		ET_GENERIC
//==========================================================================

/*
* CG_UpdateGenericEnt
*/
static void CG_UpdateGenericEnt( centity_t *cent ) {
	int modelindex;

	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	cent->ent.scale = 1.0f;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	if( cent->effects & EF_OUTLINE ) {
		Vector4Set( cent->outlineColor, 0, 0, 0, 255 );
	}

	// set frame
	cent->ent.frame = cent->current.frame;
	cent->ent.oldframe = cent->prev.frame;

	// set up the model
	cent->ent.rtype = RT_MODEL;

	modelindex = cent->current.modelindex;
	if( modelindex > 0 && modelindex < MAX_MODELS ) {
		cent->ent.model = cgs.modelDraw[modelindex];
	}

	cent->skel = CG_SkeletonForModel( cent->ent.model );
}

/*
* CG_ExtrapolateLinearProjectile
*/
void CG_ExtrapolateLinearProjectile( centity_t *cent ) {
	int i;

	cent->linearProjectileCanDraw = CG_UpdateLinearProjectilePosition( cent );

	cent->ent.backlerp = 1.0f;

	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->ent.origin2[i] = cent->ent.lightingOrigin[i] = cent->current.origin[i];

	AnglesToAxis( cent->current.angles, cent->ent.axis );
}

/*
* CG_LerpGenericEnt
*/
void CG_LerpGenericEnt( centity_t *cent ) {
	int i;
	vec3_t ent_angles = { 0, 0, 0 };

	cent->ent.backlerp = 1.0f - cg.lerpfrac;

	if( ISVIEWERENTITY( cent->current.number ) || cg.view.POVent == cent->current.number ) {
		VectorCopy( cg.predictedPlayerState.viewangles, ent_angles );
	} else {
		// interpolate angles
		for( i = 0; i < 3; i++ )
			ent_angles[i] = LerpAngle( cent->prev.angles[i], cent->current.angles[i], cg.lerpfrac );
	}

	if( ent_angles[0] || ent_angles[1] || ent_angles[2] ) {
		AnglesToAxis( ent_angles, cent->ent.axis );
	} else {
		Matrix3_Copy( axis_identity, cent->ent.axis );
	}

	if( cent->renderfx & RF_FRAMELERP ) {
		// step origin discretely, because the frames
		// do the animation properly
		vec3_t delta, move;

		// FIXME: does this still work?
		VectorSubtract( cent->current.origin2, cent->current.origin, move );
		Matrix3_TransformVector( cent->ent.axis, move, delta );
		VectorMA( cent->current.origin, cent->ent.backlerp, delta, cent->ent.origin );
	} else if( ISVIEWERENTITY( cent->current.number ) || cg.view.POVent == cent->current.number ) {
		VectorCopy( cg.predictedPlayerState.pmove.origin, cent->ent.origin );
		VectorCopy( cent->ent.origin, cent->ent.origin2 );
	} else {
		if( cgs.extrapolationTime && cent->canExtrapolate ) { // extrapolation
			vec3_t origin, xorigin1, xorigin2;

			float lerpfrac = cg.lerpfrac;
			Q_clamp( lerpfrac, 0.0f, 1.0f );

			// extrapolation with half-snapshot smoothing
			if( cg.xerpTime >= 0 || !cent->canExtrapolatePrev ) {
				VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin1 );
			} else {
				VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin1 );
				if( cent->canExtrapolatePrev ) {
					vec3_t oldPosition;

					VectorMA( cent->prev.origin, cg.oldXerpTime, cent->prevVelocity, oldPosition );
					VectorLerp( oldPosition, cg.xerpSmoothFrac, xorigin1, xorigin1 );
				}
			}


			// extrapolation with full-snapshot smoothing
			VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin2 );
			if( cent->canExtrapolatePrev ) {
				vec3_t oldPosition;

				VectorMA( cent->prev.origin, cg.oldXerpTime, cent->prevVelocity, oldPosition );
				VectorLerp( oldPosition, lerpfrac, xorigin2, xorigin2 );
			}

			VectorLerp( xorigin1, 0.5f, xorigin2, origin );


			/*
			// Interpolation between 2 extrapolated positions
			if( !cent->canExtrapolatePrev )
			    VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin2 );
			else
			{
			    float frac = cg.lerpfrac;
			    clamp( frac, 0.0f, 1.0f );
			    VectorLerp( cent->prevExtrapolatedOrigin, frac, cent->extrapolatedOrigin, xorigin2 );
			}
			*/

			if( cent->microSmooth == 2 ) {
				vec3_t oldsmoothorigin;

				VectorLerp( cent->microSmoothOrigin2, 0.65f, cent->microSmoothOrigin, oldsmoothorigin );
				VectorLerp( origin, 0.5f, oldsmoothorigin, cent->ent.origin );
			} else if( cent->microSmooth == 1 ) {
				VectorLerp( origin, 0.5f, cent->microSmoothOrigin, cent->ent.origin );
			} else {
				VectorCopy( origin, cent->ent.origin );
			}

			if( cent->microSmooth ) {
				VectorCopy( cent->microSmoothOrigin, cent->microSmoothOrigin2 );
			}

			VectorCopy( origin, cent->microSmoothOrigin );
			cent->microSmooth++;
			clamp_high( cent->microSmooth, 2 );

			VectorCopy( cent->ent.origin, cent->ent.origin2 );
		} else {   // plain interpolation
			for( i = 0; i < 3; i++ )
				cent->ent.origin[i] = cent->ent.origin2[i] = cent->prev.origin[i] + cg.lerpfrac *
															 ( cent->current.origin[i] - cent->prev.origin[i] );
		}
	}

	VectorCopy( cent->ent.origin, cent->ent.lightingOrigin );
}

/*
* CG_AddGenericEnt
*/
static void CG_AddGenericEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex && !( cent->effects & EF_FLAG_TRAIL ) ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
		Matrix3_Copy( cg.autorotateAxis, cent->ent.axis );
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	// add to refresh list
	CG_AddCentityOutLineEffect( cent );

	// render effects
	cent->ent.renderfx = cent->renderfx;

	if( cent->item ) {
		gsitem_t *item = cent->item;

		if( item->type & ( IT_HEALTH | IT_POWERUP ) ) {
			cent->ent.renderfx |= RF_NOSHADOW;
		}

		if( cent->effects & EF_AMMOBOX ) {
#ifdef DOWNSCALE_ITEMS // Ugly hack for the release. Armor models are way too big
			cent->ent.scale *= 0.90f;
#endif

			// find out the ammo box color
			if( cent->item->color && strlen( cent->item->color ) > 1 ) {
				vec4_t scolor;
				Vector4Copy( color_table[ColorIndex( cent->item->color[1] )], scolor );
				cent->ent.shaderRGBA[0] = ( uint8_t )( 255 * scolor[0] );
				cent->ent.shaderRGBA[1] = ( uint8_t )( 255 * scolor[1] );
				cent->ent.shaderRGBA[2] = ( uint8_t )( 255 * scolor[2] );
			} else {   // set white
				VectorSet( cent->ent.shaderRGBA, 255, 255, 255 );
			}
		}

		if( cent->effects & EF_GHOST ) {
			cent->ent.renderfx |= RF_ALPHAHACK | RF_GREYSCALE;
			cent->ent.shaderRGBA[3] = 100;

			// outlines don't work on transparent objects...
			cent->ent.outlineHeight = 0;
		} else {
			cent->ent.shaderRGBA[3] = 255;
		}

		cent->ent.renderfx |= RF_MINLIGHT;

		// offset weapon items by their special tag
		if( cent->item->type & IT_WEAPON ) {
			CG_PlaceModelOnTag( &cent->ent, &cent->ent, &cgs.weaponItemTag );
		}
	} else {
		cent->ent.renderfx |= RF_NOSHADOW;
	}

	if( cent->skel ) {
		// get space in cache, interpolate, transform, link
		cent->ent.boneposes = cent->ent.oldboneposes = CG_RegisterTemporaryExternalBoneposes( cent->skel );
		CG_LerpSkeletonPoses( cent->skel, cent->ent.frame, cent->ent.oldframe, cent->ent.boneposes, 1.0 - cent->ent.backlerp );
		CG_TransformBoneposes( cent->skel, cent->ent.boneposes, cent->ent.boneposes );
	}

	// flags are special
	if( cent->effects & EF_FLAG_TRAIL ) {
		CG_AddFlagModelOnTag( cent, cent->ent.shaderRGBA, "tag_linked", drawSceneRequest );
	}

	if( !cent->current.modelindex ) {
		return;
	}

	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	if( cent->current.modelindex2 ) {
		CG_AddLinkedModel( cent, drawSceneRequest );
	}
}

//==========================================================================
//		ET_FLAG_BASE
//==========================================================================

/*
* CG_AddFlagModelOnTag
*/
void CG_AddFlagModelOnTag( centity_t *cent, byte_vec4_t teamcolor, const char *tagname, DrawSceneRequest *drawSceneRequest ) {
	static entity_t flag;
	orientation_t tag;

	if( !( cent->effects & EF_FLAG_TRAIL ) ) {
		return;
	}

	memset( &flag, 0, sizeof( entity_t ) );
	flag.model = R_RegisterModel( PATH_FLAG_MODEL );
	if( !flag.model ) {
		return;
	}

	flag.rtype = RT_MODEL;
	flag.scale = 1.0f;
	flag.renderfx = cent->ent.renderfx;
	flag.customShader = NULL;
	flag.customSkin = NULL;
	flag.shaderRGBA[0] = ( uint8_t )teamcolor[0];
	flag.shaderRGBA[1] = ( uint8_t )teamcolor[1];
	flag.shaderRGBA[2] = ( uint8_t )teamcolor[2];
	flag.shaderRGBA[3] = ( uint8_t )teamcolor[3];

	VectorCopy( cent->ent.origin, flag.origin );
	VectorCopy( cent->ent.origin, flag.origin2 );
	VectorCopy( cent->ent.lightingOrigin, flag.lightingOrigin );

	// place the flag on the tag if available
	if( tagname && CG_GrabTag( &tag, &cent->ent, tagname ) ) {
		Matrix3_Copy( cent->ent.axis, flag.axis );
		CG_PlaceModelOnTag( &flag, &cent->ent, &tag );
	} else {   // Flag dropped
		vec3_t angles;

		// quick & dirty client-side rotation animation, rotate once every 2 seconds
		if( !cent->fly_stoptime ) {
			cent->fly_stoptime = cg.time;
		}

		angles[0] = LerpAngle( cent->prev.angles[0], cent->current.angles[0], cg.lerpfrac ) - 75; // Let it stand up 75 degrees
		angles[1] = ( 360.0 * ( ( cent->fly_stoptime - cg.time ) % 2000 ) ) / 2000.0;
		angles[2] = LerpAngle( cent->prev.angles[2], cent->current.angles[2], cg.lerpfrac );

		AnglesToAxis( angles, flag.axis );
		VectorMA( flag.origin, 16, &flag.axis[AXIS_FORWARD], flag.origin ); // Move the flag up a bit
	}

	CG_AddColoredOutLineEffect( &flag, EF_OUTLINE,
								(uint8_t)( teamcolor[0] * 0.3 ),
								(uint8_t)( teamcolor[1] * 0.3 ),
								(uint8_t)( teamcolor[2] * 0.3 ),
								255 );

	CG_AddEntityToScene( &flag, drawSceneRequest );

	// add the light & energy effects
	if( CG_GrabTag( &tag, &flag, "tag_color" ) ) {
		CG_PlaceModelOnTag( &flag, &flag, &tag );
	}

	// FIXME: convert this to an autosprite mesh in the flag model
	if( !( cent->ent.renderfx & RF_VIEWERMODEL ) ) {
		flag.rtype = RT_SPRITE;
		flag.model = NULL;
		flag.renderfx = RF_NOSHADOW | RF_FULLBRIGHT;
		flag.frame = flag.oldframe = 0;
		flag.radius = 32.0f;
		flag.customShader = cgs.media.shaderFlagFlare;
		flag.outlineHeight = 0;

		CG_AddEntityToScene( &flag, drawSceneRequest );
	}

	drawSceneRequest->addLight( flag.origin, 128.0f, 0.0f, teamcolor[0] / 255, teamcolor[1] / 255, teamcolor[2] / 255 );

	// TODO: We have disabled the flag particles trail effects as they were god-awful
}

/*
* CG_UpdateFlagBaseEnt
*/
static void CG_UpdateFlagBaseEnt( centity_t *cent ) {
	int modelindex;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	if( cent->effects & EF_OUTLINE ) {
		CG_SetOutlineColor( cent->outlineColor, cent->ent.shaderRGBA );
	}

	cent->ent.scale = 1.0f;

	cent->item = GS_FindItemByTag( cent->current.itemNum );
	if( cent->item ) {
		cent->effects |= cent->item->effects;
	}

	cent->ent.rtype = RT_MODEL;
	cent->ent.frame = cent->current.frame;
	cent->ent.oldframe = cent->prev.frame;

	// set up the model
	modelindex = cent->current.modelindex;
	if( modelindex > 0 && modelindex < MAX_MODELS ) {
		cent->ent.model = cgs.modelDraw[modelindex];
	}
	cent->skel = CG_SkeletonForModel( cent->ent.model );
}

/*
* CG_AddFlagBaseEnt
*/
static void CG_AddFlagBaseEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->current.type != ET_PLAYER && cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
		Matrix3_Copy( cg.autorotateAxis, cent->ent.axis );
	}

	// render effects
	cent->ent.renderfx = cent->renderfx | RF_NOSHADOW;

	// let's see: We add first the modelindex 1 (the base)

	if( cent->skel ) {
		// get space in cache, interpolate, transform, link
		cent->ent.boneposes = cent->ent.oldboneposes = CG_RegisterTemporaryExternalBoneposes( cent->skel );
		CG_LerpSkeletonPoses( cent->skel, cent->ent.frame, cent->ent.oldframe, cent->ent.boneposes, 1.0 - cent->ent.backlerp );
		CG_TransformBoneposes( cent->skel, cent->ent.boneposes, cent->ent.boneposes );
	}

	// add to refresh list
	CG_AddCentityOutLineEffect( cent );

	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	//CG_DrawTestBox( cent->ent.origin, item_box_mins, item_box_maxs, vec3_origin );

	cent->ent.customSkin = NULL;
	cent->ent.customShader = NULL;  // never use a custom skin on others

	// see if we have to add a flag
	if( cent->effects & EF_FLAG_TRAIL ) {
		byte_vec4_t teamcolor;

		CG_AddFlagModelOnTag( cent, CG_TeamColorForEntity( cent->current.number, teamcolor ), "tag_flag1", drawSceneRequest );
	}
}

//==========================================================================
//		ET_PLAYER
//==========================================================================

/*
* CG_AddPlayerEnt
*/
static void CG_AddPlayerEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	// render effects
	cent->ent.renderfx = cent->renderfx;
#ifndef CELSHADEDMATERIAL
	cent->ent.renderfx |= RF_MINLIGHT;
#endif

	if( ISVIEWERENTITY( cent->current.number ) ) {
		cg.effects = cent->effects;
		VectorCopy( cent->ent.lightingOrigin, cg.lightingOrigin );
		if( !cg.view.thirdperson && cent->current.modelindex ) {
			cent->ent.renderfx |= RF_VIEWERMODEL; // only draw from mirrors
		}
	}

	// if set to invisible, skip
	if( !cent->current.modelindex || cent->current.team == TEAM_SPECTATOR ) {
		return;
	}

	CG_AddPModel( cent, drawSceneRequest );

	// corpses can never have a model in modelindex2
	if( cent->current.type == ET_CORPSE ) {
		return;
	}

	if( cent->current.modelindex2 ) {
		CG_AddLinkedModel( cent, drawSceneRequest );
	}
}

//==========================================================================
//		ET_SPRITE, ET_RADAR
//==========================================================================

/*
* CG_AddSpriteEnt
*/
static void CG_AddSpriteEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	// render effects
	cent->ent.renderfx = cent->renderfx;

	// add to refresh list
	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	if( cent->current.modelindex2 ) {
		CG_AddLinkedModel( cent, drawSceneRequest );
	}
}

/*
* CG_LerpSpriteEnt
*/
static void CG_LerpSpriteEnt( centity_t *cent ) {
	int i;

	// interpolate origin
	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->ent.origin2[i] = cent->ent.lightingOrigin[i] = cent->prev.origin[i] + cg.lerpfrac * ( cent->current.origin[i] - cent->prev.origin[i] );

	cent->ent.radius = cent->prev.frame + cg.lerpfrac * ( cent->current.frame - cent->prev.frame );
}

/*
* CG_UpdateSpriteEnt
*/
static void CG_UpdateSpriteEnt( centity_t *cent ) {
	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	cent->ent.scale = 1.0f;
	cent->ent.renderfx = cent->renderfx;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the model
	cent->ent.rtype = RT_SPRITE;
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	cent->ent.radius = cent->prev.frame;
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin, cent->ent.origin2 );
	VectorCopy( cent->prev.origin, cent->ent.lightingOrigin );
	Matrix3_Identity( cent->ent.axis );
}

//==========================================================================
//		ET_DECAL
//==========================================================================

/*
* CG_AddDecalEnt
*/
static void CG_AddDecalEnt( centity_t *cent ) {
	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	vec3_t rgb;
	VectorScale( cent->ent.shaderRGBA, ( 1.0f / 255.0f ), rgb );
	cg.effectsSystem.touchAreaIndicator( cent->current.number, cent->ent.origin, cent->ent.radius, rgb, cg.time );
}

/*
* CG_LerpDecalEnt
*/
static void CG_LerpDecalEnt( centity_t *cent ) {
	int i;
	float a1, a2;

	// interpolate origin
	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->prev.origin[i] + cg.lerpfrac * ( cent->current.origin[i] - cent->prev.origin[i] );

	cent->ent.radius = cent->prev.frame + cg.lerpfrac * ( cent->current.frame - cent->prev.frame );

	a1 = cent->prev.modelindex2 / 255.0 * 360;
	a2 = cent->current.modelindex2 / 255.0 * 360;
	cent->ent.rotation = LerpAngle( a1, a2, cg.lerpfrac );
}

/*
* CG_UpdateDecalEnt
*/
static void CG_UpdateDecalEnt( centity_t *cent ) {
	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the null model, may be potentially needed for linked model
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	cent->ent.radius = cent->prev.frame;
	cent->ent.rotation = cent->prev.modelindex2 / 255.0 * 360;
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin2, cent->ent.origin2 );
}

//==========================================================================
//		ET_ITEM
//==========================================================================

/*
* CG_UpdateItemEnt
*/
static void CG_UpdateItemEnt( centity_t *cent ) {
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	Vector4Set( cent->ent.shaderRGBA, 255, 255, 255, 255 );

	cent->item = GS_FindItemByTag( cent->current.itemNum );
	if( !cent->item ) {
		return;
	}

	cent->effects |= cent->item->effects;

	if( cg_simpleItems->integer && cent->item->simpleitem ) {
		cent->ent.rtype = RT_SPRITE;
		cent->ent.model = NULL;
		cent->skel = NULL;
		cent->ent.renderfx = RF_NOSHADOW | RF_FULLBRIGHT;
		cent->ent.frame = cent->ent.oldframe = 0;

		cent->ent.radius = cg_simpleItemsSize->value <= 32 ? cg_simpleItemsSize->value : 32;
		if( cent->ent.radius < 1.0f ) {
			cent->ent.radius = 1.0f;
		}

		if( cg_simpleItems->integer == 2 ) {
			cent->effects &= ~EF_ROTATE_AND_BOB;
		}

		cent->ent.customShader = NULL;
		cent->ent.customShader = R_RegisterPic( cent->item->simpleitem );
	} else {
		cent->ent.rtype = RT_MODEL;
		cent->ent.frame = cent->current.frame;
		cent->ent.oldframe = cent->prev.frame;

		if( cent->effects & EF_OUTLINE ) {
			Vector4Set( cent->outlineColor, 0, 0, 0, 255 ); // black

		}

		// set up the model
		cent->ent.model = cgs.modelDraw[cent->current.modelindex];
		cent->skel = CG_SkeletonForModel( cent->ent.model );
	}
}

static void CG_AddItemEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	int msec;

	if( !cent->item ) {
		return;
	}

	// respawning items
	if( cent->respawnTime ) {
		msec = cg.time - cent->respawnTime;
	} else {
		msec = ITEM_RESPAWN_TIME;
	}

	if( msec >= 0 && msec < ITEM_RESPAWN_TIME ) {
		cent->ent.scale = (float)msec / ITEM_RESPAWN_TIME;
	} else {
		cent->ent.scale = 1.0f;
	}

	if( cent->ent.rtype != RT_SPRITE ) {
		// weapons are special
		if( cent->item && cent->item->type & IT_WEAPON ) {
			cent->ent.scale *= 1.40f;
		}

#ifdef DOWNSCALE_ITEMS // Ugly hack for release. Armor models are way too big
		if( cent->item ) {
			if( cent->item->type & IT_ARMOR ) {
				cent->ent.scale *= 0.85f;
			}
			if( cent->item->tag == HEALTH_SMALL ) {
				cent->ent.scale *= 0.85f;
			}
		}
#endif

		// flags are special
		if( cent->effects & EF_FLAG_TRAIL ) {
			CG_AddFlagModelOnTag( cent, cent->ent.shaderRGBA, NULL, drawSceneRequest );
			return;
		}

		CG_AddGenericEnt( cent, drawSceneRequest );
		return;
	} else {
		if( cent->effects & EF_GHOST ) {
			cent->ent.shaderRGBA[3] = 100;
			cent->ent.renderfx |= RF_GREYSCALE;
		}
	}

	// offset the item origin up
	cent->ent.origin[2] += cent->ent.radius + 2;
	cent->ent.origin2[2] += cent->ent.radius + 2;
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
	}

	Matrix3_Identity( cent->ent.axis );
	CG_AddEntityToScene( &cent->ent, drawSceneRequest );
}

//==========================================================================
//		ET_ITEM_TIMER
//==========================================================================

#define MAX_ITEM_TIMERS 8

static int cg_num_item_timers = 0;
static centity_t *cg_item_timers[MAX_ITEM_TIMERS];

/*
* CG_ResetItemTimers
*/
void CG_ResetItemTimers( void ) {
	cg_num_item_timers = 0;
}

/*
* CG_UpdateItemTimerEnt
*/
static void CG_UpdateItemTimerEnt( centity_t *cent ) {
	if( GS_MatchState() >= MATCH_STATE_POSTMATCH ) {
		return;
	}

	cent->item = GS_FindItemByTag( cent->current.itemNum );
	if( !cent->item ) {
		return;
	}

	if( cg_num_item_timers == MAX_ITEM_TIMERS ) {
		return;
	}

	cent->ent.frame = cent->current.frame;
	cg_item_timers[cg_num_item_timers++] = cent;
}

/*
* CG_CompareItemTimers
*/
static int CG_CompareItemTimers( const centity_t **first, const centity_t **second ) {
	const centity_t *e1 = *first, *e2 = *second;
	const entity_state_t *s1 = &( e1->current ), *s2 = &( e2->current );
	const gsitem_t *i1 = e1->item, *i2 = e2->item;
	int t1 = s1->modelindex - 1, t2 = s2->modelindex - 1;

	// special hack to order teams like this: alpha -> neutral -> beta
	if( ( !t1 || !t2 ) && ( GS_MAX_TEAMS - TEAM_ALPHA ) == 2 ) {
		if( t2 == TEAM_ALPHA || t1 == TEAM_BETA ) {
			return 1;
		}
		if( t2 == TEAM_BETA || t1 == TEAM_ALPHA ) {
			return -1;
		}
	}

	if( t2 > t1 ) {
		return -11;
	}
	if( t2 < t1 ) {
		return 1;
	}

	if( s2->origin[2] > s1->origin[2] ) {
		return 1;
	}
	if( s2->origin[2] < s1->origin[2] ) {
		return -1;
	}

	if( i2->type > i1->type ) {
		return 1;
	}
	if( i2->type < i1->type ) {
		return -1;
	}

	if( s2->number > s1->number ) {
		return 1;
	}
	if( s2->number < s1->number ) {
		return -1;
	}

	return 0;
}

/*
* CG_SortItemTimers
*/
static void CG_SortItemTimers( void ) {
	qsort( cg_item_timers, cg_num_item_timers, sizeof( cg_item_timers[0] ), ( int ( * )( const void *, const void * ) )CG_CompareItemTimers );
}

/*
* CG_GetItemTimerEnt
*/
centity_t *CG_GetItemTimerEnt( int num ) {
	if( num < 0 || num >= cg_num_item_timers ) {
		return NULL;
	}
	return cg_item_timers[num];
}

//==========================================================================
//		ET_BEAM
//==========================================================================

/*
* CG_AddBeamEnt
*/
static void CG_AddBeamEnt( centity_t *cent ) {
	const float width = cent->current.frame * 0.5f;
	cg.effectsSystem.spawnWorldLaserBeam( cent->current.origin, cent->current.origin2, width );
}

//==========================================================================
//		ET_LASERBEAM
//==========================================================================

/*
* CG_UpdateLaserbeamEnt
*/
static void CG_UpdateLaserbeamEnt( centity_t *cent ) {
	centity_t *owner;

	if( cg.view.playerPrediction && cg_predictLaserBeam->integer
		&& ISVIEWERENTITY( cent->current.ownerNum ) ) {
		return;
	}

	owner = &cg_entities[cent->current.ownerNum];
	if( owner->serverFrame != cg.frame.serverFrame ) {
		CG_Error( "CG_UpdateLaserbeamEnt: owner is not in the snapshot\n" );
	}

	owner->localEffects[LOCALEFFECT_LASERBEAM] = cg.time + 10;
	owner->laserCurved = ( cent->current.type == ET_CURVELASERBEAM ) ? true : false;

	// laser->s.origin is beam start
	// laser->s.origin2 is beam end

	VectorCopy( cent->prev.origin, owner->laserOriginOld );
	VectorCopy( cent->prev.origin2, owner->laserPointOld );

	VectorCopy( cent->current.origin, owner->laserOrigin );
	VectorCopy( cent->current.origin2, owner->laserPoint );
}

/*
* CG_LerpLaserbeamEnt
*/
static void CG_LerpLaserbeamEnt( centity_t *cent ) {
	centity_t *owner = &cg_entities[cent->current.ownerNum];

	if( cg.view.playerPrediction && cg_predictLaserBeam->integer
		&& ISVIEWERENTITY( cent->current.ownerNum ) ) {
		return;
	}

	owner->localEffects[LOCALEFFECT_LASERBEAM] = cg.time + 1;
	owner->laserCurved = ( cent->current.type == ET_CURVELASERBEAM ) ? true : false;
}

//==========================================================================
//		ET_PORTALSURFACE
//==========================================================================

/*
* CG_UpdatePortalSurfaceEnt
*/
static void CG_UpdatePortalSurfaceEnt( centity_t *cent ) {
	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );

	cent->ent.rtype = RT_PORTALSURFACE;
	Matrix3_Identity( cent->ent.axis );
	VectorCopy( cent->current.origin, cent->ent.origin );
	VectorCopy( cent->current.origin2, cent->ent.origin2 );

	if( !VectorCompare( cent->ent.origin, cent->ent.origin2 ) ) {
		cg.portalInView = true;
		cent->ent.frame = cent->current.skinnum;
	}

	if( cent->current.effects & EF_NOPORTALENTS ) {
		cent->ent.renderfx |= RF_NOPORTALENTS;
	}
}

/*
* CG_AddPortalSurfaceEnt
*/
static void CG_AddPortalSurfaceEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	if( !VectorCompare( cent->ent.origin, cent->ent.origin2 ) ) { // construct the view matrix for portal view
		if( cent->current.effects & EF_ROTATE_AND_BOB ) {
			float phase = cent->current.frame / 256.0f;
			float speed = cent->current.modelindex2 ? cent->current.modelindex2 : 50;

			Matrix3_Identity( cent->ent.axis );
			Matrix3_Rotate( cent->ent.axis, 5 * sin( ( phase + cg.time * 0.001 * speed * 0.01 ) * M_TWOPI ),
							1, 0, 0, cent->ent.axis );
		}
	}

	CG_AddEntityToScene( &cent->ent, drawSceneRequest );
}

static void CG_AddParticlesEnt( centity_t *cent ) {
}

void CG_UpdateParticlesEnt( centity_t *cent ) {
	// TODO: This forces TEAM_PLAYERS color for these entities
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the data in the old position
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin2, cent->ent.origin2 );
}

//==================================================
// ET_SOUNDEVENT
//==================================================

void CG_SoundEntityNewState( centity_t *cent ) {
	int channel, soundindex, owner;
	float attenuation;
	bool fixed;

	soundindex = cent->current.sound;
	owner = cent->current.ownerNum;
	channel = cent->current.channel & ~CHAN_FIXED;
	fixed = ( cent->current.channel & CHAN_FIXED ) ? true : false;
	attenuation = cent->current.attenuation;

	if( attenuation == ATTN_NONE ) {
		if( cgs.soundPrecache[soundindex] ) {
			SoundSystem::Instance()->StartGlobalSound( cgs.soundPrecache[soundindex], channel & ~CHAN_FIXED, 1.0f );
		}
		return;
	}

	if( owner ) {
		if( owner < 0 || owner >= MAX_EDICTS ) {
			Com_Printf( "CG_SoundEntityNewState: bad owner number" );
			return;
		}
		if( cg_entities[owner].serverFrame != cg.frame.serverFrame ) {
			owner = 0;
		}
	}

	if( !owner ) {
		fixed = true;
	}

	// sexed sounds are not in the sound index and ignore attenuation
	if( !cgs.soundPrecache[soundindex] ) {
		if( owner ) {
			auto string = cgs.configStrings.getSound( soundindex );
			if( string && string->startsWith( '*' ) ) {
				CG_SexedSound( owner, channel | ( fixed ? CHAN_FIXED : 0 ), string->data(), 1.0f, attenuation );
			}
		}
		return;
	}

	if( fixed ) {
		SoundSystem::Instance()->StartFixedSound( cgs.soundPrecache[soundindex], cent->current.origin, channel, 1.0f, attenuation );
	} else if( ISVIEWERENTITY( owner ) ) {
		SoundSystem::Instance()->StartGlobalSound( cgs.soundPrecache[soundindex], channel, 1.0f );
	} else {
		SoundSystem::Instance()->StartRelativeSound( cgs.soundPrecache[soundindex], owner, channel, 1.0f, attenuation );
	}
}

//==========================================================================
//		PACKET ENTITIES
//==========================================================================

void CG_EntityLoopSound( entity_state_t *state, float attenuation ) {
	if( !state->sound ) {
		return;
	}

	SoundSystem::Instance()->AddLoopSound( cgs.soundPrecache[state->sound], state->number, cg_volume_effects->value, ISVIEWERENTITY( state->number ) ? ATTN_NONE : ATTN_IDLE );
}

/*
* CG_AddPacketEntitiesToScene
* Add the entities to the rendering list
*/
void CG_AddEntities( DrawSceneRequest *drawSceneRequest ) {
	vec3_t autorotate;
	// bonus items rotate at a fixed rate
	VectorSet( autorotate, 0, ( cg.time % 3600 ) * 0.1 * ( cg.view.flipped ? -1.0f : 1.0f ), 0 );
	AnglesToAxis( autorotate, cg.autorotateAxis );

	// TODO: Sort all other entities by type as well
	auto *const plasmaStateIndices = (uint16_t *)alloca( sizeof( uint16_t ) * cg.frame.numEntities );
	unsigned numPlasmaEnts = 0;

	for( int pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		const int stateIndex  = pnum & ( MAX_PARSE_ENTITIES - 1 );
		entity_state_t *state = &cg.frame.parsedEntities[stateIndex];
		centity_t *cent       = &cg_entities[state->number];

		if( cent->current.linearMovement ) {
			if( !cent->linearProjectileCanDraw ) {
				continue;
			}
		}

		bool canLight = !state->linearMovement;

		switch( cent->type ) {
			case ET_GENERIC:
				CG_AddGenericEnt( cent, drawSceneRequest );
				if( cg_drawEntityBoxes->integer ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_STATIC );
				canLight = true;
				break;
			case ET_GIB:
				if( false ) {
					CG_AddGenericEnt( cent, drawSceneRequest );
					CG_EntityLoopSound( state, ATTN_STATIC );
					canLight = true;
				}
				break;
			case ET_BLASTER:
				CG_AddGenericEnt( cent, drawSceneRequest );
				cg.effectsSystem.touchBlastTrail( cent->current.number, cent->ent.origin, cg.time );
				CG_EntityLoopSound( state, ATTN_STATIC );
				// We use relatively large light radius because this projectile moves very fast, so make it noticeable
				drawSceneRequest->addLight( cent->ent.origin, 192.0f, 144.0f, 0.9f, 0.7f, 0.0f );
				break;

			case ET_ELECTRO_WEAK:
				cent->current.frame = cent->prev.frame = 0;
				cent->ent.frame =  cent->ent.oldframe = 0;

				CG_AddGenericEnt( cent, drawSceneRequest );
				cg.effectsSystem.touchElectroTrail( cent->current.number, cent->ent.origin, cg.time );
				CG_EntityLoopSound( state, ATTN_STATIC );
				drawSceneRequest->addLight( cent->ent.origin, 192.0f, 144.0f, 0.9f, 0.9f, 1.0f );
				break;
			case ET_ROCKET:
				CG_AddGenericEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_NORM );
				cg.effectsSystem.touchRocketTrail( cent->current.number, cent->ent.origin, cg.time );
				if( cent->current.effects & EF_STRONG_WEAPON ) {
					drawSceneRequest->addLight( cent->ent.origin, 300.0f, 192.0f, 1.0f, 0.7f, 0.3f );
				} else {
					drawSceneRequest->addLight( cent->ent.origin, 300.0f - 48.0f, 192.0f - 32.0f, 1.0f, 0.7f, 0.3f );
				}
				break;
			case ET_GRENADE:
				CG_AddGenericEnt( cent, drawSceneRequest );
				cg.effectsSystem.touchGrenadeTrail( cent->current.number, cent->ent.origin, cg.time );
				CG_EntityLoopSound( state, ATTN_STATIC );
				drawSceneRequest->addLight( cent->ent.origin, 200.0f, 96.0f, 0.0f, 0.3f, 1.0f );
				break;
			case ET_PLASMA:
				plasmaStateIndices[numPlasmaEnts++] = (uint16_t)stateIndex;
				break;
			case ET_WAVE:
				CG_AddGenericEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_STATIC );
				// Add the core light
				drawSceneRequest->addLight( cent->ent.origin, 128.0f, 128.0f, 0.0f, 0.3f, 1.0f );
				// Add the corona light
				// We have initially thought to activate corona light only when corona damage is enabled,
				// but it is not a good idea since it requires synchronization/prediction
				// and the projectile gets activated rather fast anyway.
				// Otherwise high ping players would only see an activated wave.
				drawSceneRequest->addLight( cent->ent.origin, 300.0f, 192.0f, 1.0f, 1.0f, 1.0f );
				break;
			case ET_SPRITE:
			case ET_RADAR:
				CG_AddSpriteEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_STATIC );
				canLight = true;
				break;

			case ET_ITEM:
				CG_AddItemEnt( cent, drawSceneRequest );
				if( cg_drawEntityBoxes->integer ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE );
				canLight = true;
				break;

			case ET_PLAYER:
				CG_AddPlayerEnt( cent, drawSceneRequest );
				if( cg_drawEntityBoxes->integer ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE );
				CG_LaserBeamEffect( cent, drawSceneRequest );
				CG_WeaponBeamEffect( cent );
				canLight = true;
				break;

			case ET_CORPSE:
				CG_AddPlayerEnt( cent, drawSceneRequest );
				if( cg_drawEntityBoxes->integer ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE );
				canLight = true;
				break;

			case ET_BEAM:
				CG_AddBeamEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC );
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				break;

			case ET_PORTALSURFACE:
				CG_AddPortalSurfaceEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_STATIC );
				break;

			case ET_FLAG_BASE:
				CG_AddFlagBaseEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_STATIC );
				canLight = true;
				break;

			case ET_MINIMAP_ICON:
				if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
					CG_EntAddTeamColorTransitionEffect( cent );
				}
				break;

			case ET_DECAL:
				CG_AddDecalEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC );
				break;

			case ET_PUSH_TRIGGER:
				if( cg_drawEntityBoxes->integer ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_STATIC );
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				break;

			case ET_PARTICLES:
				CG_AddParticlesEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC );
				break;

				// TODO: Remove once the net protocol gets updated
			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_AddPacketEntities: unknown entity type" );
				break;
		}

		// glow if light is set
		if( canLight && state->light ) {
			drawSceneRequest->addLight( cent->ent.origin,
								COLOR_A( state->light ) * 4.0, 0.0f,
								COLOR_R( state->light ) * ( 1.0 / 255.0 ),
								COLOR_G( state->light ) * ( 1.0 / 255.0 ),
								COLOR_B( state->light ) * ( 1.0 / 255.0 ) );
		}
	}

	for( unsigned i = 0; i < numPlasmaEnts; ++i ) {
		entity_state_t *state = &cg.frame.parsedEntities[plasmaStateIndices[i]];
		centity_t *cent       = &cg_entities[state->number];
		CG_AddGenericEnt( cent, drawSceneRequest );
		CG_EntityLoopSound( state, ATTN_STATIC );

		constexpr float desiredProgramLightRadius = 128.0f;
		float programLightRadius = 0.0f;

		// TODO: This should be handled at rendering layer during culling/light prioritization
		const float squareDistance = DistanceSquared( cg.view.refdef.vieworg, state->origin );
		// TODO: Using fov tangent ratio is a more correct approach (but nobody actually notices in zooomed in state)
		if( squareDistance < 384.0f * 384.0f ) {
			programLightRadius = desiredProgramLightRadius;
		} else if( squareDistance < 768.0f * 768.0f ) {
			if( ( ( cg.frameCount % 2 ) == ( state->number % 2 ) ) ) {
				programLightRadius = desiredProgramLightRadius;
			}
		} else {
			if( ( cg.frameCount % 4 ) == ( state->number % 4 ) ) {
				programLightRadius = desiredProgramLightRadius;
			}
		}

		drawSceneRequest->addLight( cent->ent.origin, programLightRadius, 64.0f, 0.0f, 1.0f, 0.5f );
	}
}

/*
* CG_LerpEntities
* Interpolate the entity states positions into the entity_t structs
*/
void CG_LerpEntities( void ) {
	entity_state_t *state;
	int pnum;
	centity_t *cent;

	for( pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		int number;
		bool spatialize;

		state = &cg.frame.parsedEntities[pnum & ( MAX_PARSE_ENTITIES - 1 )];
		number = state->number;
		cent = &cg_entities[number];
		spatialize = true;

		switch( cent->type ) {
			case ET_GENERIC:
			case ET_GIB:
			case ET_BLASTER:
			case ET_ELECTRO_WEAK:
			case ET_ROCKET:
			case ET_PLASMA:
			case ET_GRENADE:
			case ET_WAVE:
			case ET_ITEM:
			case ET_PLAYER:
			case ET_CORPSE:
			case ET_FLAG_BASE:
				if( state->linearMovement ) {
					CG_ExtrapolateLinearProjectile( cent );
				} else {
					CG_LerpGenericEnt( cent );
				}
				break;

			case ET_SPRITE:
			case ET_RADAR:
				CG_LerpSpriteEnt( cent );
				break;

			case ET_DECAL:
				CG_LerpDecalEnt( cent );
				break;

			case ET_BEAM:

				// beams aren't interpolated
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				CG_LerpLaserbeamEnt( cent );
				break;

			case ET_MINIMAP_ICON:
				break;

			case ET_PORTALSURFACE:

				//portals aren't interpolated
				break;

			case ET_PUSH_TRIGGER:
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				break;

			case ET_PARTICLES:
				break;

			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_LerpEntities: unknown entity type" );
				break;
		}

		if( spatialize ) {
			vec3_t origin, velocity;
			CG_GetEntitySpatilization( number, origin, velocity );
			SoundSystem::Instance()->SetEntitySpatialization( number, origin, velocity );
		}
	}
}

/*
* CG_UpdateEntities
* Called at receiving a new serverframe. Sets up the model, type, etc to be drawn later on
*/
void CG_UpdateEntities( void ) {
	entity_state_t *state;
	int pnum;
	centity_t *cent;

	CG_ResetItemTimers();

	for( pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		state = &cg.frame.parsedEntities[pnum & ( MAX_PARSE_ENTITIES - 1 )];
		cent = &cg_entities[state->number];
		cent->type = state->type;
		cent->effects = state->effects;
		cent->item = NULL;
		cent->renderfx = 0;

		switch( cent->type ) {
			case ET_GENERIC:
				CG_UpdateGenericEnt( cent );
				break;
			case ET_GIB:
				if( false ) {
					cent->renderfx |= RF_NOSHADOW;
					CG_UpdateGenericEnt( cent );

					// set the gib model ignoring the modelindex one
					// TODO: Disabled old shitty gibs for now
					cent->ent.model = nullptr;
				}
				break;

			// projectiles with linear trajectories
			case ET_BLASTER:
			case ET_ELECTRO_WEAK:
			case ET_ROCKET:
			case ET_PLASMA:
			case ET_GRENADE:
			case ET_WAVE:
				cent->renderfx |= ( RF_NOSHADOW | RF_FULLBRIGHT );
				CG_UpdateGenericEnt( cent );
				break;

			case ET_RADAR:
				cent->renderfx |= RF_NODEPTHTEST;
			case ET_SPRITE:
				cent->renderfx |= ( RF_NOSHADOW | RF_FULLBRIGHT );
				CG_UpdateSpriteEnt( cent );
				break;

			case ET_ITEM:
				CG_UpdateItemEnt( cent );
				break;
			case ET_PLAYER:
			case ET_CORPSE:
				CG_UpdatePlayerModelEnt( cent );
				break;

			case ET_BEAM:
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				CG_UpdateLaserbeamEnt( cent );
				break;

			case ET_FLAG_BASE:
				CG_UpdateFlagBaseEnt( cent );
				break;

			case ET_MINIMAP_ICON:
			{
				CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
				if( cent->current.modelindex > 0 && cent->current.modelindex < MAX_IMAGES ) {
					cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
				} else {
					cent->ent.customShader = NULL;
				}
			}
			break;

			case ET_DECAL:
				CG_UpdateDecalEnt( cent );
				break;

			case ET_PORTALSURFACE:
				CG_UpdatePortalSurfaceEnt( cent );
				break;

			case ET_PUSH_TRIGGER:
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				CG_UpdateItemTimerEnt( cent );
				break;

			case ET_PARTICLES:
				CG_UpdateParticlesEnt( cent );
				break;

			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_UpdateEntities: unknown entity type %i", cent->type );
				break;
		}
	}

	CG_SortItemTimers();
}

//=============================================================

/*
* CG_GetEntitySpatilization
*
* Called to get the sound spatialization origin and velocity
*/
void CG_GetEntitySpatilization( int entNum, vec3_t origin, vec3_t velocity ) {
	centity_t *cent;
	const cmodel_s *cmodel;
	vec3_t mins, maxs;

	if( entNum < -1 || entNum >= MAX_EDICTS ) {
		CG_Error( "CG_GetEntitySoundOrigin: bad entnum" );
		return;
	}

	// hack for client side floatcam
	if( entNum == -1 ) {
		if( origin != NULL ) {
			VectorCopy( cg.frame.playerState.pmove.origin, origin );
		}
		if( velocity != NULL ) {
			VectorCopy( cg.frame.playerState.pmove.velocity, velocity );
		}
		return;
	}

	cent = &cg_entities[entNum];

	// normal
	if( cent->current.solid != SOLID_BMODEL ) {
		if( origin != NULL ) {
			VectorCopy( cent->ent.origin, origin );
		}
		if( velocity != NULL ) {
			VectorCopy( cent->velocity, velocity );
		}
		return;
	}

	// bmodel
	if( origin != NULL ) {
		cmodel = CG_InlineModel( cent->current.modelindex );
		CG_InlineModelBounds( cmodel, mins, maxs );
		VectorAdd( maxs, mins, origin );
		VectorMA( cent->ent.origin, 0.5f, origin, origin );
	}
	if( velocity != NULL ) {
		VectorCopy( cent->velocity, velocity );
	}
}
