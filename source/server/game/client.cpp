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
#include "chat.h"

#include <common/facilities/fscompat.h>
#include <common/facilities/gs_qrespath.h>
#define IMPLEMENT_REMOVECOLORTOKENS
#include <common/helpers/q_libc.h>

#include <tuple>
#include <cctype>

using wsw::operator""_asView;
using wsw::operator""_asHView;

static const wsw::HashedStringView kInfoKeyIP( "ip"_asHView );
static const wsw::HashedStringView kInfoKeySocket( "socket"_asHView );
static const wsw::HashedStringView kInfoKeyColor( "color"_asHView );
static const wsw::HashedStringView kInfoKeyName( "name"_asHView );
static const wsw::HashedStringView kInfoKeyClan( "clan"_asHView );
static const wsw::HashedStringView kInfoKeyHand( "hand"_asHView );
static const wsw::HashedStringView kInfoKeyHandicap( "handicap"_asHView );
static const wsw::HashedStringView kInfoKeyNoAutoHop( "cg_noAutoHop"_asHView );
static const wsw::HashedStringView kInfoKeyMultiview( "cg_multiview"_asHView );
static const wsw::HashedStringView kInfoKeyMovementStyle( "cg_movementStyle"_asHView );
static const wsw::HashedStringView kInfoKeyMMSession( "cl_mm_session"_asHView );
static const wsw::HashedStringView kInfoKeySkin( "skin"_asHView );
static const wsw::HashedStringView kInfoKeyModel( "model"_asHView );

#define PLAYER_MASS 200

void Client::resetLevelState() {
	levelTimestamp = 0;

	respawnCount = 0;
	memset( &matchmessage, 0, sizeof( matchmessage ) );
	helpmessage = 0;
	last_activity = 0;

	stats.Clear();

	showscores = false;

	flood_locktill = 0;
	m_floodState.clear();
	m_floodTeamState.clear();

	callvote_when = 0;
	memset( quickMenuItems, 0, sizeof( quickMenuItems ) );
}

void Client::resetSpawnState() {
	memset( &snap, 0, sizeof( snap ) );
	memset( &chase, 0, sizeof( chase ) );
	memset( &awardInfo, 0, sizeof( awardInfo ) );

	spawnStateTimestamp = 0;
	memset( events, 0, sizeof( events ) );
	eventsCurrent = eventsHead = 0;

	memset( &trail, 0, sizeof( trail ) );

	takeStun = false;
	armor = 0.0;
	instashieldCharge = 0.0;

	next_drown_time = 0;
	drowningDamage = 0;
	old_waterlevel = 0;
	old_watertype = 0;

	pickup_msg_time = 0;
}

void Client::resetTeamState() {
	is_coach = false;
	readyUpWarningNext = 0;
	readyUpWarningCount = 0;

	position_saved = false;
	VectorClear( position_origin );
	VectorClear( position_angles );
	position_lastcmd = 0;

	last_drop_item = nullptr;
	VectorClear( last_drop_location );
	last_pickup = nullptr;
	last_killer = nullptr;
}

void Client::reset() {
	resetShared();

	resetSpawnState();
	resetLevelState();
	resetTeamState();
	teamStateTimestamp = 0;

	m_userInfo.clear();

	netname.clear();
	clanname.clear();
	ip.clear();
	socket.clear();

	mm_session = Uuid_ZeroUuid();

	connecting = false;

	Vector4Clear( color );
	team = 0;
	hand = 0;
	handicap = 0;
	movestyle = 0;
	movestyle_latched = 0;
	isoperator = false;
	queueTimeStamp = 0;

	memset( &ucmd, 0, sizeof( ucmd ) );
	timeDelta = 0;
	deltas.clear();
	memset( &old_pmove, 0, sizeof( old_pmove ) );
}

void player_pain( edict_t *self, edict_t *other, float kick, int damage ) {
	// player pain is handled at the end of the frame in P_DamageFeedback
}

void player_think( edict_t *self ) {
	// player entities do not think
}

static void ClientObituary( edict_t *self, edict_t *inflictor, edict_t *attacker ) {
	if( level.gametype.disableObituaries ) {
		return;
	}

	int mod = meansOfDeath;
	char message[64];
	char message2[64];
	GS_Obituary( self, G_PlayerGender( self ), attacker, mod, message, message2 );

	// duplicate message at server console for logging
	if( attacker && attacker->r.client ) {
		if( attacker != self ) { // regular death message
			self->enemy = attacker;
			if( dedicated->integer ) {
				G_Printf( "%s%s %s %s%s%s\n", self->r.client->netname.data(), S_COLOR_WHITE, message,
						  attacker->r.client->netname.data(), S_COLOR_WHITE, message2 );
			}
		} else {      // suicide
			self->enemy = NULL;
			if( dedicated->integer ) {
				G_Printf( "%s %s%s\n", self->r.client->netname.data(), S_COLOR_WHITE, message );
			}
		}

		G_Obituary( self, attacker, mod );
	} else {      // wrong place, suicide, etc.
		self->enemy = NULL;
		if( dedicated->integer ) {
			G_Printf( "%s %s%s\n", self->r.client->netname.data(), S_COLOR_WHITE, message );
		}

		G_Obituary( self, ( attacker == self ) ? self : world, mod );
	}
}

static void G_Client_UnlinkBodies( edict_t *ent ) {
	// find bodies linked to us
	edict_t *body = &game.edicts[ggs->maxclients + 1];
	for( int i = 0; i < BODY_QUEUE_SIZE; body++, i++ ) {
		if( !body->r.inuse ) {
			continue;
		}

		if( body->activator == ent ) {
			// this is our body
			body->activator = NULL;
		}
	}
}

void G_InitBodyQueue( void ) {
	level.body_que = 0;
	for( int i = 0; i < BODY_QUEUE_SIZE; i++ ) {
		edict_t *ent = G_Spawn();
		ent->classname = "bodyque";
	}
}

static void body_die( edict_t *self, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point ) {
	if( self->health >= GIB_HEALTH ) {
		return;
	}

	ThrowSmallPileOfGibs( self, damage );
	self->s.origin[2] -= 48;
	ThrowClientHead( self, damage );
	self->nextThink = level.time + 3000 + random() * 3000;
}

static void body_think( edict_t *self ) {
	self->health = GIB_HEALTH - 1;

	//effect: small gibs, and only when it is still a body, not a gibbed head.
	if( self->s.type == ET_CORPSE ) {
		ThrowSmallPileOfGibs( self, 25 );
	}

	//disallow interaction with the world.
	self->takedamage = DAMAGE_NO;
	self->r.solid = SOLID_NOT;
	self->s.sound = 0;
	self->flags |= FL_NO_KNOCKBACK;
	self->s.type = ET_GENERIC;
	self->r.svflags &= ~SVF_CORPSE;
	self->r.svflags |= SVF_NOCLIENT;
	self->s.modelindex = 0;
	self->s.modelindex2 = 0;
	VectorClear( self->velocity );
	VectorClear( self->avelocity );
	self->movetype = MOVETYPE_NONE;
	self->think = NULL;

	//memset( &self->snap, 0, sizeof(self->snap) );
	GClip_UnlinkEntity( self );
}

static void body_ready( edict_t *body ) {
	body->takedamage = DAMAGE_YES;
	body->r.solid = SOLID_YES;
	body->think = body_think; // body self destruction countdown
	body->nextThink = level.time + g_deadbody_autogib_delay->integer + ( crandom() * g_deadbody_autogib_delay->value * 0.25f ) ;
	GClip_LinkEntity( body );
}

static edict_t *CopyToBodyQue( edict_t *ent, edict_t *attacker, int damage ) {
	edict_t *body;
	int contents;

	if( GS_RaceGametype( *ggs ) ) {
		return NULL;
	}

	contents = G_PointContents( ent->s.origin );
	if( contents & CONTENTS_NODROP ) {
		return NULL;
	}

	G_Client_UnlinkBodies( ent );

	// grab a body que and cycle to the next one
	body = &game.edicts[ggs->maxclients + level.body_que + 1];
	level.body_que = ( level.body_que + 1 ) % BODY_QUEUE_SIZE;

	// send an effect on the removed body
	if( body->s.modelindex && body->s.type == ET_CORPSE ) {
		ThrowSmallPileOfGibs( body, 10 );
	}

	GClip_UnlinkEntity( body );

	memset( body, 0, sizeof( edict_t ) ); //clean up garbage

	//init body edict
	G_InitEdict( body );
	body->classname = "body";
	body->health = ent->health;
	body->mass = ent->mass;
	body->r.owner = ent->r.owner;
	body->s.type = ent->s.type;
	body->s.team = ent->s.team;
	body->s.effects = 0;
	body->r.svflags = SVF_CORPSE;
	body->r.svflags &= ~SVF_NOCLIENT;
	body->activator = ent;
	if( g_deadbody_followkiller->integer ) {
		body->enemy = attacker;
	}

	//use flat yaw
	body->s.angles[PITCH] = 0;
	body->s.angles[ROLL] = 0;
	body->s.angles[YAW] = ent->s.angles[YAW];
	body->s.modelindex2 = 0; // <-  is bodyOwner when in ET_CORPSE, but not in ET_GENERIC or ET_PLAYER
	body->s.weapon = 0;

	//copy player position and box size
	VectorCopy( ent->s.origin, body->s.origin );
	VectorCopy( ent->s.origin, body->olds.origin );
	VectorCopy( ent->r.mins, body->r.mins );
	VectorCopy( ent->r.maxs, body->r.maxs );
	VectorCopy( ent->r.absmin, body->r.absmin );
	VectorCopy( ent->r.absmax, body->r.absmax );
	VectorCopy( ent->r.size, body->r.size );
	VectorCopy( ent->velocity, body->velocity );
	body->r.maxs[2] = body->r.mins[2] + 8;

	body->r.solid = SOLID_YES;
	body->takedamage = DAMAGE_YES;
	body->r.clipmask = CONTENTS_SOLID | CONTENTS_PLAYERCLIP;
	body->movetype = MOVETYPE_TOSS;
	body->die = body_die;
	body->think = body_think; // body self destruction countdown

	if( ent->health < GIB_HEALTH
		|| meansOfDeath == MOD_ELECTROBOLT_S /* electrobolt always gibs */ ) {
		ThrowSmallPileOfGibs( body, damage );

		// reset gib impulse
		VectorClear( body->velocity );
		ThrowClientHead( body, damage ); // sets ET_GIB

		body->s.frame = 0;
		body->nextThink = level.time + 3000 + random() * 3000;
		body->deadflag = DEAD_DEAD;
	} else if( ent->s.type == ET_PLAYER ) {
		// copy the model
		body->s.type = ET_CORPSE;
		body->s.modelindex = ent->s.modelindex;
		body->s.bodyOwner = ent->s.number; // bodyOwner is the same as modelindex2
		body->s.skinnum = ent->s.skinnum;
		body->s.teleported = true;

		// launch the death animation on the body
		{
			static int i;
			i = ( i + 1 ) % 3;
			G_AddEvent( body, EV_DIE, i, true );
			switch( i ) {
				default:
				case 0:
					body->s.frame = ( ( BOTH_DEAD1 & 0x3F ) | ( BOTH_DEAD1 & 0x3F ) << 6 | ( 0 & 0xF ) << 12 );
					break;
				case 1:
					body->s.frame = ( ( BOTH_DEAD2 & 0x3F ) | ( BOTH_DEAD2 & 0x3F ) << 6 | ( 0 & 0xF ) << 12 );
					break;
				case 2:
					body->s.frame = ( ( BOTH_DEAD3 & 0x3F ) | ( BOTH_DEAD3 & 0x3F ) << 6 | ( 0 & 0xF ) << 12 );
					break;
			}
		}

		body->think = body_ready;
		body->takedamage = DAMAGE_NO;
		body->r.solid = SOLID_NOT;
		body->nextThink = level.time + 500; // make damageable in 0.5 seconds
	} else {   // wasn't a player, just copy it's model
		VectorClear( body->velocity );
		body->s.modelindex = ent->s.modelindex;
		body->s.frame = ent->s.frame;
		body->nextThink = level.time + 5000 + random() * 10000;
	}

	GClip_LinkEntity( body );
	return body;
}

void player_die( edict_t *ent, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point ) {
	const auto ent_snap_backup = ent->snap;
	const auto client_snap_backup = ent->r.client->snap;

	VectorClear( ent->avelocity );

	ent->s.angles[0] = 0;
	ent->s.angles[2] = 0;
	ent->s.sound = 0;

	ent->r.solid = SOLID_NOT;

	ent->r.client->last_killer = attacker;

	// player death
	ent->s.angles[YAW] = ent->r.client->ps.viewangles[YAW] = LookAtKillerYAW( ent, inflictor, attacker );
	ClientObituary( ent, inflictor, attacker );

	// create a body
	CopyToBodyQue( ent, attacker, damage );
	ent->enemy = NULL;

	// clear his combo stats
	G_AwardResetPlayerComboStats( ent );

	// go ghost (also resets snap)
	G_GhostClient( ent );

	ent->deathTimeStamp = level.time;

	VectorClear( ent->velocity );
	VectorClear( ent->avelocity );
	ent->snap = ent_snap_backup;
	ent->r.client->snap = client_snap_backup;
	ent->r.client->snap.buttons = 0;
	GClip_LinkEntity( ent );
}

void G_Client_UpdateActivity( Client *client ) {
	if( client ) {
		client->last_activity = level.time;
	}
}

void G_Client_InactivityRemove( Client *client, int64_t inactivityMillis ) {
	assert( inactivityMillis > 0 );

	if( G_GetClientState( client - game.clients ) < CS_SPAWNED ) {
		return;
	}

	if( client->ps.pmove.pm_type != PM_NORMAL ) {
		return;
	}

	if( ( GS_MatchState( *ggs ) != MATCH_STATE_PLAYTIME ) || !level.gametype.removeInactivePlayers ) {
		return;
	}

	// inactive for too long
	if( client->last_activity && client->last_activity + inactivityMillis < level.time ) {
		if( client->team >= TEAM_PLAYERS && client->team < GS_MAX_TEAMS ) {
			edict_t *ent = &game.edicts[ client - game.clients + 1 ];

			// move to spectators and reset the queue time, effectively removing from the challengers queue
			G_Teams_SetTeam( ent, TEAM_SPECTATOR );
			client->queueTimeStamp = 0;

			G_PrintMsg( NULL, "%s" S_COLOR_YELLOW " has been moved to spectator after %.1f seconds of inactivity\n", client->netname.data(), g_inactivity_maxtime->value );
		}
	}
}

[[nodiscard]]
static bool couldBeValidModelOrSkin( const wsw::StringView &s ) {
	if( !s.contains( '/' ) ) {
		if( s.isZeroTerminated() ) {
			if( COM_ValidateRelativeFilename( s.data() ) ) {
				return true;
			}
		} else {
			wsw::StaticString<MAX_QPATH> zts( s );
			if( COM_ValidateRelativeFilename( zts.data() ) ) {
				return true;
			}
		}
	}
	return false;
}

void Client::setSkinFromInfo() {
	std::optional<wsw::StringView> maybeSkin = GS_TeamSkinName( ggs, getEntity()->s.team ); // is it a team skin?
	if( !maybeSkin ) {
		maybeSkin = getNonEmptyInfoValue( kInfoKeySkin );
		if( maybeSkin && ( !couldBeValidModelOrSkin( *maybeSkin ) || maybeSkin->contains( "invisibility"_asView ) ) ) {
			maybeSkin = std::nullopt;
		}
	}

	// index player model
	std::optional<wsw::StringView> maybeModel = getNonEmptyInfoValue( kInfoKeyModel );
	if( maybeModel && !couldBeValidModelOrSkin( *maybeModel ) ) {
		maybeModel = std::nullopt;
	}

	wsw::StringView model( DEFAULT_PLAYERMODEL ), skin( DEFAULT_PLAYERSKIN );
	if( maybeSkin && maybeModel ) {
		std::tie( model, skin ) = std::make_pair( *maybeModel, *maybeSkin );
	}

	wsw::StaticString<MAX_QPATH> modelPath, skinPath;
	modelPath << "$models/players/"_asView << model;
	skinPath << "models/players/"_asView << model << '/' << skin;

	auto *ent = getEntity();
	if( !ent->deadflag ) {
		ent->s.modelindex = SV_ModelIndex( modelPath.data() );
	}
	ent->s.skinnum = SV_SkinIndex( skinPath.data() );
}

void G_ClientClearStats( edict_t *ent ) {
	if( ent && ent->r.client ) {
		ent->r.client->stats.Clear();
	}
}

void G_GhostClient( edict_t *ent ) {
	G_DeathAwards( ent );

	ent->movetype = MOVETYPE_NONE;
	ent->r.solid = SOLID_NOT;

	memset( &ent->snap, 0, sizeof( ent->snap ) );
	memset( &ent->r.client->snap, 0, sizeof( ent->r.client->snap ) );
	memset( &ent->r.client->chase, 0, sizeof( ent->r.client->chase ) );
	memset( &ent->r.client->awardInfo, 0, sizeof( ent->r.client->awardInfo ) );
	ent->r.client->next_drown_time = 0;
	ent->r.client->old_waterlevel = 0;
	ent->r.client->old_watertype = 0;

	ent->s.modelindex = ent->s.modelindex2 = ent->s.skinnum = 0;
	ent->s.effects = 0;
	ent->s.weapon = 0;
	ent->s.sound = 0;
	ent->s.light = 0;
	ent->viewheight = 0;
	ent->takedamage = DAMAGE_NO;

	// clear inventory
	memset( ent->r.client->ps.inventory, 0, sizeof( ent->r.client->ps.inventory ) );

	ent->r.client->ps.stats[STAT_WEAPON] = ent->r.client->ps.stats[STAT_PENDING_WEAPON] = WEAP_NONE;
	ent->r.client->ps.weaponState = WEAPON_STATE_READY;
	ent->r.client->ps.stats[STAT_WEAPON_TIME] = 0;

	G_SetPlayerHelpMessage( ent, 0 );

	GClip_LinkEntity( ent );
}

void G_ClientRespawn( edict_t *self, bool ghost ) {
	G_DeathAwards( self );

	G_SpawnQueue_RemoveClient( self );

	self->r.svflags &= ~SVF_NOCLIENT;

	//if invalid be spectator
	if( self->r.client->team < 0 || self->r.client->team >= GS_MAX_TEAMS ) {
		self->r.client->team = TEAM_SPECTATOR;
	}

	// force ghost always to true when in spectator team
	if( self->r.client->team == TEAM_SPECTATOR ) {
		ghost = true;
	}

	const int old_team = self->s.team;
	if( self->r.client->is_coach ) {
		ghost = true;
	}

	GClip_UnlinkEntity( self );

	Client *const client = self->r.client;

	client->resetSpawnState();
	memset( &client->ps, 0, sizeof( client->ps ) );
	client->spawnStateTimestamp = level.time;
	client->ps.playerNum = PLAYERNUM( self );

	// clear entity values
	memset( &self->snap, 0, sizeof( self->snap ) );
	memset( &self->s, 0, sizeof( self->s ) );
	memset( &self->olds, 0, sizeof( self->olds ) );
	memset( &self->invpak, 0, sizeof( self->invpak ) );

	self->s.number = self->olds.number = ENTNUM( self );

	// relink client struct
	self->r.client = &game.clients[PLAYERNUM( self )];

	// update team
	self->s.team = client->team;

	self->deadflag = DEAD_NO;
	self->s.type = ET_PLAYER;
	self->groundentity = NULL;
	self->takedamage = DAMAGE_AIM;
	self->think = player_think;
	self->pain = player_pain;
	self->die = player_die;
	self->viewheight = playerbox_stand_viewheight;
	self->r.inuse = true;
	self->mass = PLAYER_MASS;
	self->air_finished = level.time + ( 12 * 1000 );
	self->r.clipmask = MASK_PLAYERSOLID;
	self->waterlevel = 0;
	self->watertype = 0;
	self->flags &= ~FL_NO_KNOCKBACK;
	self->r.svflags &= ~SVF_CORPSE;
	self->enemy = NULL;
	self->r.owner = NULL;
	self->max_health = 100;
	self->health = self->max_health;

	if( self->bot ) {
		self->think = NULL;
		self->classname = "bot";
	} else if( self->r.svflags & SVF_FAKECLIENT ) {
		self->classname = "fakeclient";
	} else {
		self->classname = "player";
	}

	VectorCopy( playerbox_stand_mins, self->r.mins );
	VectorCopy( playerbox_stand_maxs, self->r.maxs );
	VectorClear( self->velocity );
	VectorClear( self->avelocity );

	client->ps.POVnum = ENTNUM( self );

	// set movement info
	client->ps.pmove.stats[PM_STAT_MAXSPEED] = (short)GS_DefaultPlayerSpeed( *ggs );
	client->ps.pmove.stats[PM_STAT_JUMPSPEED] = (short)DEFAULT_JUMPSPEED;
	client->ps.pmove.stats[PM_STAT_DASHSPEED] = (short)DEFAULT_DASHSPEED;

	if( ghost ) {
		self->r.solid = SOLID_NOT;
		self->movetype = MOVETYPE_NOCLIP;
		if( self->s.team == TEAM_SPECTATOR ) {
			self->r.svflags |= SVF_NOCLIENT;
		}
	} else {
		self->r.client->takeStun = true;
		self->r.solid = SOLID_YES;
		self->movetype = MOVETYPE_PLAYER;
		client->ps.pmove.stats[PM_STAT_FEATURES] = static_cast<unsigned short>( PMFEAT_DEFAULT );
		if( !g_allow_bunny->integer ) {
			client->ps.pmove.stats[PM_STAT_FEATURES] &= ~( PMFEAT_AIRCONTROL | PMFEAT_FWDBUNNY );
		}
	}

	self->r.client->handleUserInfoChanges();

	if( old_team != self->s.team ) {
		G_Teams_UpdateMembersList();
	}

	edict_t *spawnpoint;
	vec3_t spawn_origin, spawn_angles;
	SelectSpawnPoint( self, &spawnpoint, spawn_origin, spawn_angles );
	VectorCopy( spawn_origin, client->ps.pmove.origin );
	VectorCopy( spawn_origin, self->s.origin );

	// set angles
	self->s.angles[PITCH] = 0;
	self->s.angles[YAW] = anglemod( spawn_angles[YAW] );
	self->s.angles[ROLL] = 0;
	VectorCopy( self->s.angles, client->ps.viewangles );

	// set the delta angle
	for( int i = 0; i < 3; i++ ) {
		client->ps.pmove.delta_angles[i] = ANGLE2SHORT( client->ps.viewangles[i] ) - client->ucmd.angles[i];
	}

	// don't put spectators in the game
	if( !ghost ) {
		if( KillBox( self ) ) {
		}
	}

	self->s.attenuation = ATTN_NORM;

	self->s.teleported = true;

	self->aiIntrinsicEnemyWeight = 1.0f;
	self->aiVisibilityDistance = 999999.9f;

	// hold in place briefly
	client->ps.pmove.pm_flags = PMF_TIME_TELEPORT;
	client->ps.pmove.pm_time = 14;
	client->ps.pmove.stats[PM_STAT_NOUSERCONTROL] = CLIENT_RESPAWN_FREEZE_DELAY;
	client->ps.pmove.stats[PM_STAT_NOAUTOATTACK] = 1000;

	// set race stats to invisible
	client->ps.stats[STAT_TIME_SELF] = STAT_NOTSET;
	client->ps.stats[STAT_TIME_BEST] = STAT_NOTSET;
	client->ps.stats[STAT_TIME_RECORD] = STAT_NOTSET;
	client->ps.stats[STAT_TIME_ALPHA] = STAT_NOTSET;
	client->ps.stats[STAT_TIME_BETA] = STAT_NOTSET;

	self->r.client->respawnCount++;

	G_UseTargets( spawnpoint, self );

	GClip_LinkEntity( self );

	// let the gametypes perform their changes
	GT_asCallPlayerRespawn( self, old_team, self->s.team );

	AI_Respawn( self );
}

bool G_PlayerCanTeleport( edict_t *player ) {
	if( !player->r.client ) {
		return false;
	}
	if( player->r.client->ps.pmove.pm_type > PM_SPECTATOR ) {
		return false;
	}
	if( GS_MatchState( *ggs ) == MATCH_STATE_COUNTDOWN ) { // match countdown
		return false;
	}
	return true;
}

void G_TeleportPlayer( edict_t *player, edict_t *dest ) {
	if( !dest ) {
		return;
	}

	Client *const client = player->r.client;
	if( !client ) {
		return;
	}

	// draw the teleport entering effect
	G_TeleportEffect( player, false );

	//
	// teleport the player
	//

	vec3_t velocity;
	mat3_t axis;
	// from racesow - use old pmove velocity
	VectorCopy( client->old_pmove.velocity, velocity );

	velocity[2] = 0; // ignore vertical velocity
	const float speed = VectorLengthFast( velocity );

	AnglesToAxis( dest->s.angles, axis );
	VectorScale( &axis[AXIS_FORWARD], speed, client->ps.pmove.velocity );

	VectorCopy( dest->s.angles, client->ps.viewangles );
	VectorCopy( dest->s.origin, client->ps.pmove.origin );

	// set the delta angle
	for( int i = 0; i < 3; i++ ) {
		client->ps.pmove.delta_angles[i] = ANGLE2SHORT( client->ps.viewangles[i] ) - client->ucmd.angles[i];
	}

	client->ps.pmove.pm_flags |= PMF_TIME_TELEPORT;
	client->ps.pmove.pm_time = 1; // force the minimum no control delay
	player->s.teleported = true;

	// update the entity from the pmove
	VectorCopy( client->ps.viewangles, player->s.angles );
	VectorCopy( client->ps.pmove.origin, player->s.origin );
	VectorCopy( client->ps.pmove.origin, player->olds.origin );
	VectorCopy( client->ps.pmove.velocity, player->velocity );

	// unlink to make sure it can't possibly interfere with KillBox
	GClip_UnlinkEntity( player );

	// kill anything at the destination
	KillBox( player );

	GClip_LinkEntity( player );

	// add the teleport effect at the destination
	G_TeleportEffect( player, true );
}

/*
* ClientBegin
* called when a client has finished connecting, and is ready
* to be placed into the game.  This will happen every level load.
*/
void G_ClientBegin( edict_t *ent ) {
	Client *client = ent->r.client;

	memset( &client->ucmd, 0, sizeof( client->ucmd ) );
	client->resetLevelState();
	client->levelTimestamp = level.time;
	G_Client_UpdateActivity( client ); // activity detected

	client->team = TEAM_SPECTATOR;
	G_ClientRespawn( ent, true ); // respawn as ghost
	ent->movetype = MOVETYPE_NOCLIP; // allow freefly

	G_UpdatePlayerMatchMsg( ent );

	if( !level.gametype.disableObituaries || !( ent->r.svflags & SVF_FAKECLIENT ) ) {
		G_PrintMsg( NULL, "%s" S_COLOR_WHITE " entered the game\n", client->netname.data() );
	}

	client->respawnCount = 0; // clear respawncount
	client->connecting = false;

	G_ClientEndSnapFrame( ent ); // make sure all view stuff is valid

	// let the gametype scripts now this client just entered the level
	G_Gametype_ScoreEvent( client, "enterGame", NULL );
}

template <unsigned N>
static void normalizeNameOrClanInputString( wsw::StaticString<N> *buffer, const wsw::StringView &input ) {
	const wsw::StringView trimmed( input.trim().take( N ) );
	wsw::StaticString<N> asciiBuffer;
	for( char ch: trimmed ) {
		if( (unsigned char)ch < 127 ) {
			asciiBuffer.append( ch );
		}
	}

	buffer->resize( N, '\0' );
	COM_SanitizeColorString( asciiBuffer.data(), buffer->data(), (int)buffer->size(), -1, COLOR_WHITE );
	buffer->erase( std::strlen( buffer->data() ) );
}

template <unsigned N>
static void normalizeColorTokensInString( wsw::StaticString<N> *buffer, const wsw::StringView &s, unsigned maxPrintableChars ) {
	assert( s.isZeroTerminated() );
	buffer->resize( N, '\0' );
	COM_SanitizeColorString( s.data(), buffer->data(), (int)buffer->size(), (int)maxPrintableChars, COLOR_WHITE );
	buffer->erase( std::strlen( buffer->data() ) );
}

[[nodiscard]]
static bool hasPrintableChars( const wsw::StringView &s ) {
	for( char ch: s ) {
		if( (unsigned char)ch > 32u && (unsigned char)ch < 127u ) {
			return true;
		}
	}
	return false;
}

static const wsw::StringView kIllegalNamePrefixes[] = {
	"console"_asView, "[team]"_asView, "[spec]"_asView, "[bot]"_asView, "[coach]"_asView
};

static const wsw::StringView kDefaultPlayerName( "Player"_asView );

void Client::setName( const wsw::StringView &inputName ) {
	wsw::StaticString<MAX_NAME_BYTES> normalizedName;
	normalizeNameOrClanInputString( &normalizedName, inputName );
	// This should be a good idea
	for( char &ch: normalizedName ) {
		if( std::isspace( (int)ch ) && ch != ' ' ) {
			ch = ' ';
		}
	}

	wsw::StringView chosenName( normalizedName.asView() );
	wsw::StaticString<MAX_NAME_BYTES> colorlessName( chosenName );
	removeColorTokens( &colorlessName );

	if( !hasPrintableChars( colorlessName.asView() ) ) {
		chosenName = kDefaultPlayerName;
	} else if( !isFakeClient() ) {
		for( const auto &prefix: kIllegalNamePrefixes ) {
			// startsWithIgnoringCase()
			if( colorlessName.asView().take( prefix.length() ).equalsIgnoreCase( prefix ) ) {
				chosenName = kDefaultPlayerName;
				break;
			}
		}
	}

	wsw::StaticString<MAX_NAME_BYTES> choppedName;
	if( chosenName != kDefaultPlayerName ) {
		normalizeColorTokensInString( &choppedName, chosenName, MAX_NAME_CHARS );
		chosenName = choppedName.asView();
	}

	setCleanNameResolvingNameClash( chosenName );
}

void Client::setCleanNameResolvingNameClash( const wsw::StringView &inputName ) {
	wsw::StaticString<MAX_NAME_BYTES> chosenColorlessName( inputName );
	wsw::StaticString<MAX_NAME_BYTES> chosenName( inputName );
	removeColorTokens( &chosenColorlessName );

	for( int tryNum = 1; tryNum <= MAX_CLIENTS; tryNum++ ) {
		if( !findClientWithTheSameColorlessName( chosenColorlessName.asView() ) ) {
			break;
		}

		wsw::StaticString<MAX_NAME_BYTES> tryName( inputName );
		static_assert( MAX_CLIENTS >= 10 && MAX_CLIENTS < 100 );
		unsigned suffixLen = 2 + ( tryNum > 9 ? 2 : 1 );
		if( tryName.length() + suffixLen > tryName.capacity() ) {
			tryName.erase( tryName.length() - suffixLen );
		}
		tryName << '(' <<  tryNum << ')';

		normalizeColorTokensInString( &chosenName, tryName.asView(), MAX_NAME_BYTES );
		chosenColorlessName.assign( chosenName.asView() );
		removeColorTokens( &chosenColorlessName );
	}

	this->netname.assign( chosenName.asView() );
	this->colorlessNetname.assign( chosenColorlessName.asView() );
}

auto Client::findClientWithTheSameColorlessName( const wsw::StringView &colorlessName ) -> const Client * {
	const auto *const self = getEntity();
	for( int i = 0; i < ggs->maxclients; ++i ) {
		if( const edict_t *other = game.edicts + 1 + i; ( other->r.inuse && other->r.client && other != self ) ) {
			if( colorlessName.equalsIgnoreCase( other->r.client->colorlessNetname.asView() ) ) {
				return other->r.client;
			}
		}
	}
	return nullptr;
}

static const wsw::StringView kIllegalClanPrefixes[] {
	"console"_asView, "spec"_asView, "bot"_asView, "coach"_asView, "tv"_asView
};

void Client::setClan( const wsw::StringView &inputClan ) {
	wsw::StaticString<MAX_CLANNAME_BYTES> normalizedClan;
	normalizeNameOrClanInputString( &normalizedClan, inputClan );

	// Replace whitespace groups by underscores
	bool prevSpace = false;
	unsigned writeIndex = 0;
	for( char ch: normalizedClan ) {
		if( std::isspace( (int)ch ) ) {
			if( !prevSpace ) {
				normalizedClan[writeIndex++] = '_';
				prevSpace = true;
			}
		} else {
			prevSpace = false;
			normalizedClan[writeIndex++] = ch;
		}
	}
	normalizedClan.erase( writeIndex );

	wsw::StaticString<MAX_CLANNAME_BYTES> colorlessClan( normalizedClan );
	removeColorTokens( &colorlessClan );

	for( const auto &prefix: kIllegalClanPrefixes ) {
		// startsWithIgnoringCase()
		if( normalizedClan.asView().take( prefix.length() ).equalsIgnoreCase( prefix ) ) {
			normalizedClan.clear();
			break;
		}
	}

	// This call assigns the clan name member data
	normalizeColorTokensInString( &clanname, normalizedClan.asView(), MAX_CLANNAME_CHARS );
}

void think_MoveTypeSwitcher( edict_t *ent ) {
	if( ent->s.ownerNum > 0 && ent->s.ownerNum <= ggs->maxclients ) {
		edict_t *owner = &game.edicts[ent->s.ownerNum];
		if( owner->r.client ) {
			owner->r.client->movestyle = owner->r.client->movestyle_latched;
			owner->r.client->handleUserInfoChanges();
			G_PrintMsg( owner, "Your movement style has been updated to %i\n", owner->r.client->movestyle );
		}
	}

	G_FreeEdict( ent );
}

static void G_UpdatePlayerInfoString( int playerNum ) {
	assert( playerNum >= 0 && playerNum < ggs->maxclients );
	Client *client = &game.clients[playerNum];

	wsw::UserInfo info;
	(void)info.set( kInfoKeyName, client->netname.asView() );
	(void)info.set( kInfoKeyClan, client->clanname.asView() );
	(void)info.set( kInfoKeyHand, wsw::StringView( va( "%i", client->hand ) ) );
	(void)info.set( kInfoKeyColor, wsw::StringView( va( "%i %i %i", client->color[0], client->color[1], client->color[2] ) ) );

	wsw::StaticString<MAX_INFO_STRING> string;
	info.serialize( &string );
	SV_SetConfigString( CS_PLAYERINFOS + playerNum, string.data() );
}

static void G_UpdateMMPlayerInfoString( int playerNum ) {
	assert( playerNum >= 0 && playerNum < ggs->maxclients );
	if( playerNum >= MAX_MMPLAYERINFOS ) {
		return; // oops
	}

	char playerString[MAX_INFO_STRING];
	// update client information in cgame
	playerString[0] = 0;

	playerString[MAX_CONFIGSTRING_CHARS - 1] = 0;
	SV_SetConfigString( CS_MMPLAYERINFOS + playerNum, playerString );
}


void Client::handleUserInfoChanges() {
	const auto maybeIP = m_userInfo.get( kInfoKeyIP );
	if( !maybeIP ) {
		G_DropClient( getEntity(), ReconnectBehaviour::OfUserChoice, "Error: Server didn't provide client IP" );
		return;
	}
	ip.assign( *maybeIP );

	const auto maybeSocket = m_userInfo.get( kInfoKeySocket );
	if( !maybeSocket ) {
		G_DropClient( getEntity(), ReconnectBehaviour::OfUserChoice, "Error: Server didn't provide client socket" );
		return;
	}
	socket.assign( *maybeSocket );

	int rgbcolor = -1;
	if( const auto maybeColor = m_userInfo.get( kInfoKeyColor ) ) {
		wsw::StaticString<256> buffer( *maybeColor );
		rgbcolor = COM_ReadColorRGBString( buffer.data() );
	}

	if( rgbcolor != -1 ) {
		rgbcolor = COM_ValidatePlayerColor( rgbcolor );
		Vector4Set( color, COLOR_R( rgbcolor ), COLOR_G( rgbcolor ), COLOR_B( rgbcolor ), 255 );
	} else {
		Vector4Set( color, 255, 255, 255, 255 );
	}

	wsw::StaticString<MAX_INFO_VALUE> oldName( netname );
	// set name, it's validated and possibly changed first
	setName( m_userInfo.getOrEmpty( kInfoKeyName ) );

	if( !oldName.empty() && !oldName.equals( netname ) ) {
		if( !ChatHandlersChain::instance()->detectFlood( getEntity() ) ) {
			G_PrintMsg( NULL, "%s%s is now known as %s%s\n", oldName.data(), S_COLOR_WHITE, netname.data(), S_COLOR_WHITE );
		}
	}
	if( !m_userInfo.set( kInfoKeyName, netname.asView() ) ) {
		G_DropClient( getEntity(), ReconnectBehaviour::OfUserChoice, "Error: Couldn't set userinfo (name)" );
		return;
	}

	setClan( m_userInfo.getOrEmpty( kInfoKeyClan ) );

	hand = 2;
	if( const auto maybeHandString = m_userInfo.get( kInfoKeyHand ) ) {
		if( const auto maybeHand = wsw::toNum<unsigned>( *maybeHandString ) ) {
			if( maybeHand <= 2 ) {
				hand = (int)*maybeHand;
			}
		}
	}

	handicap = 0;
	if( const auto maybeHandicapString = m_userInfo.get( kInfoKeyHandicap ) ) {
		if( const auto maybeHandicap = wsw::toNum<unsigned>( *maybeHandicapString ) ) {
			if( maybeHandicap > 90 ) {
				G_PrintMsg( getEntity(), "Handicap must be defined in the [0-90] range.\n" );
			} else {
				handicap = (int)*maybeHandicap;
			}
		}
	}

	if( const auto maybeStyleString = m_userInfo.get( kInfoKeyMovementStyle ) ) {
		const int style = wsw::clamp( wsw::toNum<int>( *maybeStyleString ).value_or( 0 ), 0, GS_MAXBUNNIES - 1 );
		if( G_GetClientState( PLAYERNUM( getEntity() ) ) < CS_SPAWNED ) {
			if( style != movestyle ) {
				movestyle = movestyle_latched = style;
			}
		} else if( movestyle_latched != movestyle ) {
			G_PrintMsg( getEntity(), "A movement style change is already in progress. Please wait.\n" );
		} else if( style != movestyle_latched ) {
			movestyle_latched = style;
			if( movestyle_latched != movestyle ) {
				edict_t *switcher = G_Spawn();
				switcher->think = think_MoveTypeSwitcher;
				switcher->nextThink = level.time + 10000;
				switcher->s.ownerNum = ENTNUM( getEntity() );
				G_PrintMsg( getEntity(), "Movement style will change in 10 seconds.\n" );
			}
		}
	}

	// update the movement features depending on the movestyle
	if( !G_ISGHOSTING( getEntity() ) && g_allow_bunny->integer ) {
		if( movestyle == GS_CLASSICBUNNY ) {
			ps.pmove.stats[PM_STAT_FEATURES] &= ~PMFEAT_FWDBUNNY;
		} else {
			ps.pmove.stats[PM_STAT_FEATURES] |= PMFEAT_FWDBUNNY;
		}
	}

	if( const auto maybeNoAutohopString = m_userInfo.get( kInfoKeyNoAutoHop ) ) {
		if( const auto maybeNum = wsw::toNum<int>( *maybeNoAutohopString ); maybeNum && *maybeNum ) {
			ps.pmove.stats[PM_STAT_FEATURES] &= ~PMFEAT_CONTINOUSJUMP;
		} else {
			ps.pmove.stats[PM_STAT_FEATURES] |= PMFEAT_CONTINOUSJUMP;
		}
	}

	if( const auto maybeMultiviewString = m_userInfo.get( kInfoKeyMultiview ) ) {
		if( const auto maybeNum = wsw::toNum<int>( *maybeMultiviewString ); maybeNum && *maybeNum ) {
			m_multiview = true;
		} else {
			m_multiview = false;
		}
	}

	/*
	if( const auto maybeSessionString = m_userInfo.get( kInfoKeyMMSession ) ) {
		if( const auto maybeUuid = Uuid_FromString( *maybeSessionString ) ) {
			mm_session = *maybeUuid;
		} else {
			mm_session = Uuid_ZeroUuid();
		}
	}*/
	mm_session = Uuid_ZeroUuid();

	if( !G_ISGHOSTING( getEntity() ) && G_GetClientState( PLAYERNUM( getEntity() ) ) >= CS_SPAWNED ) {
		setSkinFromInfo();
	}

	G_UpdatePlayerInfoString( PLAYERNUM( getEntity() ) );
	G_UpdateMMPlayerInfoString( PLAYERNUM( getEntity() ) );

	G_Gametype_ScoreEvent( this, "userinfochanged", oldName.data() );
	
	ChatHandlersChain::instance()->onUserInfoChanged( getEntity() );
}

/*
* ClientConnect
* Called when a player begins connecting to the server.
* The game can refuse entrance to a client by returning false.
* If the client is allowed, the connection process will continue
* and eventually get to ClientBegin()
* Changing levels will NOT cause this to be called again, but
* loadgames will.
*/
bool G_ClientConnect( edict_t *ent, char *userinfo, bool fakeClient ) {
	assert( ent );
	assert( userinfo && Info_Validate( userinfo ) );

	// verify that server gave us valid data
	if( !Info_Validate( userinfo ) ) {
		Info_SetValueForKey( userinfo, "rejbehavior", va( "%i", (unsigned)ReconnectBehaviour::OfUserChoice ) );
		Info_SetValueForKey( userinfo, "rejmsg", "Invalid userinfo" );
		return false;
	}

	if( !Info_ValueForKey( userinfo, "ip" ) ) {
		Info_SetValueForKey( userinfo, "rejbehavior", va( "%i", (unsigned)ReconnectBehaviour::OfUserChoice ) );
		Info_SetValueForKey( userinfo, "rejmsg", "Error: Server didn't provide client IP" );
		return false;
	}

	if( !Info_ValueForKey( userinfo, "socket" ) ) {
		Info_SetValueForKey( userinfo, "rejbehavior", va( "%i", (unsigned)ReconnectBehaviour::OfUserChoice ) );
		Info_SetValueForKey( userinfo, "rejmsg", "Error: Server didn't provide client socket" );
		return false;
	}

	char *value;
	// check to see if they are on the banned IP list
	value = Info_ValueForKey( userinfo, "ip" );
	if( SV_FilterPacket( value ) ) {
		Info_SetValueForKey( userinfo, "rejbehavior", va( "%i", (unsigned)ReconnectBehaviour::DontReconnect ) );
		Info_SetValueForKey( userinfo, "rejmsg", "You're banned from this server" );
		return false;
	}

	// check for a password
	value = Info_ValueForKey( userinfo, "password" );
	if( !fakeClient && ( *password->string && ( !value || strcmp( password->string, value ) ) ) ) {
		Info_SetValueForKey( userinfo, "rejbehavior", va( "%i", (unsigned)ReconnectBehaviour::RequestPassword ) );
		if( value && value[0] ) {
			Info_SetValueForKey( userinfo, "rejmsg", "Incorrect password" );
		} else {
			Info_SetValueForKey( userinfo, "rejmsg", "Password required" );
		}
		return false;
	}

	// they can connect

	G_InitEdict( ent );
	ent->s.modelindex = 0;
	ent->r.solid = SOLID_NOT;
	ent->r.client = game.clients + PLAYERNUM( ent );
	ent->r.svflags = ( SVF_NOCLIENT | ( fakeClient ? SVF_FAKECLIENT : 0 ) );
	ent->r.client->reset();
	ent->r.client->ps.playerNum = PLAYERNUM( ent );
	ent->r.client->connecting = true;
	ent->r.client->team = TEAM_SPECTATOR;
	G_Client_UpdateActivity( ent->r.client ); // activity detected

	G_ClientUserinfoChanged( ent, userinfo );

	if( !fakeClient ) {
		char message[MAX_STRING_CHARS];
		Q_snprintfz( message, sizeof( message ), "%s%s connected", ent->r.client->netname.data(), S_COLOR_WHITE );
		G_PrintMsg( NULL, "%s\n", message );
		G_Printf( "%s%s connected from %s\n", ent->r.client->netname.data(), S_COLOR_WHITE, ent->r.client->ip.data() );
	}

	// let the gametype scripts know this client just connected
	G_Gametype_ScoreEvent( ent->r.client, "connect", NULL );

	G_CallVotes_ResetClient( PLAYERNUM( ent ) );

	return true;
}

/*
* ClientDisconnect
* Called when a player drops from the server.
* Will not be called between levels.
*/
void G_ClientDisconnect( edict_t *ent, const char *reason ) {
	if( !ent->r.client || !ent->r.inuse ) {
		return;
	}

	//StatsowFacade::Instance()->OnClientDisconnected( ent );
	ChatHandlersChain::instance()->onClientDisconnected( ent );

	for( int team = TEAM_PLAYERS; team < GS_MAX_TEAMS; team++ )
		G_Teams_UnInvitePlayer( team, ent );

	if( !level.gametype.disableObituaries || !( ent->r.svflags & SVF_FAKECLIENT ) ) {
		if( !reason ) {
			G_PrintMsg( NULL, "%s" S_COLOR_WHITE " disconnected\n", ent->r.client->netname.data() );
		} else {
			G_PrintMsg( NULL, "%s" S_COLOR_WHITE " disconnected (%s" S_COLOR_WHITE ")\n", ent->r.client->netname.data(), reason );
		}
	}

	// send effect
	if( ent->s.team > TEAM_SPECTATOR ) {
		G_TeleportEffect( ent, false );
	}

	ent->r.client->team = TEAM_SPECTATOR;
	G_ClientRespawn( ent, true ); // respawn as ghost
	ent->movetype = MOVETYPE_NOCLIP; // allow freefly

	// let the gametype scripts know this client just disconnected
	G_Gametype_ScoreEvent( ent->r.client, "disconnect", NULL );

	ent->r.inuse = false;
	ent->r.svflags = SVF_NOCLIENT;

	ent->r.client->reset();
	ent->r.client->ps.playerNum = PLAYERNUM( ent );

	SV_SetConfigString( CS_PLAYERINFOS + PLAYERNUM( ent ), "" );
	GClip_UnlinkEntity( ent );

	G_Match_CheckReadys();
}

void G_PredictedEvent( int entNum, int ev, int parm ) {
	edict_t *ent = &game.edicts[entNum];
	switch( ev ) {
		case EV_FALL:
		{
			int dflags, damage;
			dflags = 0;
			damage = parm;

			if( damage ) {
				const vec3_t upDir = { 0, 0, 1 };
				G_Damage( ent, world, world, vec3_origin, upDir, ent->s.origin, damage, 0, 0, dflags, MOD_FALLING );
			}

			G_AddEvent( ent, ev, damage, true );
		}
		break;

		case EV_SMOOTHREFIREWEAPON: // update the firing
			G_FireWeapon( ent, parm );
			break; // don't send the event

		case EV_FIREWEAPON:
			G_FireWeapon( ent, parm );
			G_AddEvent( ent, ev, parm, true );
			break;

		case EV_WEAPONDROP:
			G_AddEvent( ent, ev, parm, true );
			break;

		case EV_WEAPONACTIVATE:
			ent->s.weapon = parm;
			G_AddEvent( ent, ev, parm, true );
			break;

		default:
			G_AddEvent( ent, ev, parm, true );
			break;
	}
}

static void G_ProjectThirdPersonView( vec3_t vieworg, vec3_t viewangles, edict_t *passent ) {
	const float thirdPersonRange = 60;
	const float thirdPersonAngle = 0;
	const vec3_t mins { -4, -4, -4 };
	const vec3_t maxs { +4, +4, +4 };

	vec3_t forward, right, up;
	AngleVectors( viewangles, forward, right, up );

	// calc exact destination
	vec3_t chase_dest;
	VectorCopy( vieworg, chase_dest );
	const float radians = DEG2RAD( thirdPersonAngle );
	const float forwardScale = -std::cos( radians );
	const float rightScale = -std::sin( radians );
	VectorMA( chase_dest, thirdPersonRange * forwardScale, forward, chase_dest );
	VectorMA( chase_dest, thirdPersonRange * rightScale, right, chase_dest );
	chase_dest[2] += 8;

	vec3_t dest, stop;
	trace_t trace;
	// find the spot the player is looking at
	VectorMA( vieworg, 512, forward, dest );
	G_Trace( &trace, vieworg, mins, maxs, dest, passent, MASK_SOLID );

	// calculate pitch to look at the same spot from camera
	VectorSubtract( trace.endpos, vieworg, stop );
	float dist = std::sqrt( stop[0] * stop[0] + stop[1] * stop[1] );
	if( dist < 1 ) {
		dist = 1;
	}

	viewangles[PITCH] = RAD2DEG( -std::atan2( stop[2], dist ) );
	viewangles[YAW] -= thirdPersonAngle;
	AngleVectors( viewangles, forward, right, up );

	// move towards destination
	G_Trace( &trace, vieworg, mins, maxs, chase_dest, passent, MASK_SOLID );

	if( trace.fraction != 1.0 ) {
		VectorCopy( trace.endpos, stop );
		stop[2] += ( 1.0f - trace.fraction ) * 32;
		G_Trace( &trace, vieworg, mins, maxs, stop, passent, MASK_SOLID );
		VectorCopy( trace.endpos, chase_dest );
	}

	VectorCopy( chase_dest, vieworg );
}

static void G_Client_DeadView( edict_t *ent ) {
	Client *client = ent->r.client;

	// find the body
	edict_t *body = game.edicts + ggs->maxclients;
	for(; ENTNUM( body ) < ggs->maxclients + BODY_QUEUE_SIZE + 1; body++ ) {
		if( !body->r.inuse || body->r.svflags & SVF_NOCLIENT ) {
			continue;
		}
		if( body->activator == ent ) { // this is our body
			break;
		}
	}

	if( body->activator != ent ) { // ran all the list and didn't find our body
		return;
	}

	// move us to body position
	VectorCopy( body->s.origin, ent->s.origin );
	ent->s.teleported = true;
	client->ps.viewangles[ROLL] = 0;
	client->ps.viewangles[PITCH] = 0;

	// see if our killer is still in view
	if( body->enemy && ( body->enemy != ent ) ) {
		trace_t trace;
		G_Trace( &trace, ent->s.origin, vec3_origin, vec3_origin, body->enemy->s.origin, body, MASK_OPAQUE );
		if( trace.fraction != 1.0f ) {
			body->enemy = NULL;
		} else {
			client->ps.viewangles[YAW] = LookAtKillerYAW( ent, NULL, body->enemy );
		}
	} else {   // nobody killed us, so just circle around the body ?

	}

	G_ProjectThirdPersonView( ent->s.origin, client->ps.viewangles, body );
	VectorCopy( client->ps.viewangles, ent->s.angles );
	VectorCopy( ent->s.origin, client->ps.pmove.origin );
	VectorClear( client->ps.pmove.velocity );
}

void G_ClientAddDamageIndicatorImpact( Client *client, int damage, const vec3_t basedir ) {
	if( damage < 1 ) {
		return;
	}

	if( !client || client - game.clients < 0 || client - game.clients >= ggs->maxclients ) {
		return;
	}

	vec3_t dir;
	if( !basedir ) {
		VectorCopy( vec3_origin, dir );
	} else {
		VectorNormalize2( basedir, dir );
	}

	const float frac = (float)damage / ( damage + client->snap.damageTaken );
	VectorLerp( client->snap.damageTakenDir, frac, dir, client->snap.damageTakenDir );
	client->snap.damageTaken += damage;
}

void G_ClientDamageFeedback( edict_t *ent ) {
	if( ent->r.client->snap.damageTaken ) {
		int damage = ent->r.client->snap.damageTaken;
		int byteDir = DirToByte( ent->r.client->snap.damageTakenDir );

		if( damage <= 20 ) {
			G_AddPlayerStateEvent( ent->r.client, PSEV_DAMAGE_20, byteDir );
		} else if( damage <= 40 ) {
			G_AddPlayerStateEvent( ent->r.client, PSEV_DAMAGE_40, byteDir );
		} else if( damage <= 60 ) {
			G_AddPlayerStateEvent( ent->r.client, PSEV_DAMAGE_60, byteDir );
		} else {
			G_AddPlayerStateEvent( ent->r.client, PSEV_DAMAGE_80, byteDir );
		}
	}

	// add hitsounds from given damage
	if( ent->snap.damage_given || ent->snap.damageteam_given || ent->snap.kill || ent->snap.teamkill ) {
		// we can't make team damage hit sound at the same time as we do damage hit sound
		// let's determine what's more relevant
		if( ent->snap.teamkill || ent->snap.damageteam_given > 50 ||
			( ent->snap.damageteam_given > 2 * ent->snap.damage_given && !ent->snap.kill ) ) {
			G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 5 );
		} else {
			if( ent->snap.kill ) {
				G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 4 );
			} else if( ent->snap.damage_given >= 70 ) {
				G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 0 );
			} else if( ent->snap.damage_given >= 40 ) {
				G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 1 );
			} else if( ent->snap.damage_given >= 20 ) {
				G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 2 );
			} else {
				G_AddPlayerStateEvent( ent->r.client, PSEV_HIT, 3 );
			}
		}
	}
}

static void G_PlayerWorldEffects( edict_t *ent ) {
	int waterlevel, old_waterlevel;
	int watertype, old_watertype;

	if( ent->movetype == MOVETYPE_NOCLIP ) {
		ent->air_finished = level.time + ( 12 * 1000 ); // don't need air
		return;
	}

	waterlevel = ent->waterlevel;
	watertype = ent->watertype;
	old_waterlevel = ent->r.client->old_waterlevel;
	old_watertype = ent->r.client->old_watertype;
	ent->r.client->old_waterlevel = waterlevel;
	ent->r.client->old_watertype = watertype;

	//
	// if just entered a water volume, play a sound
	//
	if( !old_waterlevel && waterlevel ) {
		if( ent->watertype & CONTENTS_LAVA ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_LAVA_IN ), ATTN_NORM );
		} else if( ent->watertype & CONTENTS_SLIME ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_SLIME_IN ), ATTN_NORM );
		} else if( ent->watertype & CONTENTS_WATER ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_WATER_IN ), ATTN_NORM );
		}

		ent->flags |= FL_INWATER;
	}

	//
	// if just completely exited a water volume, play a sound
	//
	if( old_waterlevel && !waterlevel ) {
		if( old_watertype & CONTENTS_LAVA ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_LAVA_OUT ), ATTN_NORM );
		} else if( old_watertype & CONTENTS_SLIME ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_SLIME_OUT ), ATTN_NORM );
		} else if( old_watertype & CONTENTS_WATER ) {
			G_Sound( ent, CHAN_AUTO, SV_SoundIndex( S_WORLD_WATER_OUT ), ATTN_NORM );
		}

		ent->flags &= ~FL_INWATER;
	}

	//
	// check for head just coming out of water
	//
	if( old_waterlevel == 3 && waterlevel != 3 ) {
		if( ent->air_finished < level.time ) { // gasp for air
			// wsw : jal : todo : better variations of gasp sounds
			G_AddEvent( ent, EV_SEXEDSOUND, 1, true );
		} else if( ent->air_finished < level.time + 11000 ) {   // just break surface
			// wsw : jal : todo : better variations of gasp sounds
			G_AddEvent( ent, EV_SEXEDSOUND, 2, true );
		}
	}

	//
	// check for drowning
	//
	if( waterlevel == 3 ) {
		// if out of air, start drowning
		if( ent->air_finished < level.time ) { // drown!
			if( ent->r.client->next_drown_time < level.time && !G_IsDead( ent ) ) {
				ent->r.client->next_drown_time = level.time + 1000;

				// take more damage the longer underwater
				ent->r.client->drowningDamage += 2;
				if( ent->r.client->drowningDamage > 15 ) {
					ent->r.client->drowningDamage = 15;
				}

				// wsw : jal : todo : better variations of gasp sounds
				// play a gurp sound instead of a normal pain sound
				if( HEALTH_TO_INT( ent->health ) - ent->r.client->drowningDamage <= 0 ) {
					G_AddEvent( ent, EV_SEXEDSOUND, 2, true );
				} else {
					G_AddEvent( ent, EV_SEXEDSOUND, 1, true );
				}
				ent->pain_debounce_time = level.time;

				G_Damage( ent, world, world, vec3_origin, vec3_origin, ent->s.origin, ent->r.client->drowningDamage, 0, 0, DAMAGE_NO_ARMOR, MOD_WATER );
			}
		}
	} else {
		ent->air_finished = level.time + 12000;
		ent->r.client->drowningDamage = 2;
	}

	//
	// check for sizzle damage
	//
	if( waterlevel && ( ent->watertype & ( CONTENTS_LAVA | CONTENTS_SLIME ) ) ) {
		if( ent->watertype & CONTENTS_LAVA ) {
			// wsw: Medar: We don't have the sounds yet and this seems to overwrite the normal pain sounds
			//if( !G_IsDead(ent) && ent->pain_debounce_time <= level.time )
			//{
			//	G_Sound( ent, CHAN_BODY, SV_SoundIndex(va(S_PLAYER_BURN_1_to_2, (rand()&1)+1)), 1, ATTN_NORM );
			//	ent->pain_debounce_time = level.time + 1000;
			//}
			G_Damage( ent, world, world, vec3_origin, vec3_origin, ent->s.origin,
					  ( 30 * waterlevel ) * game.snapFrameTime / 1000.0f, 0, 0, 0, MOD_LAVA );
		}

		if( ent->watertype & CONTENTS_SLIME ) {
			G_Damage( ent, world, world, vec3_origin, vec3_origin, ent->s.origin,
					  ( 10 * waterlevel ) * game.snapFrameTime / 1000.0f, 0, 0, 0, MOD_SLIME );
		}
	}
}

static void G_SetClientEffects( edict_t *ent ) {
	Client *client = ent->r.client;

	if( G_IsDead( ent ) || GS_MatchState( *ggs ) >= MATCH_STATE_POSTMATCH ) {
		return;
	}

	if( client->ps.inventory[POWERUP_QUAD] > 0 ) {
		ent->s.effects |= EF_QUAD;
		if( client->ps.inventory[POWERUP_QUAD] < 6 ) {
			ent->s.effects |= EF_EXPIRING_QUAD;
		}
	}

	if( client->ps.inventory[POWERUP_SHELL] > 0 ) {
		ent->s.effects |= EF_SHELL;
		if( client->ps.inventory[POWERUP_SHELL] < 6 ) {
			ent->s.effects |= EF_EXPIRING_SHELL;
		}
	}

	if( client->ps.inventory[POWERUP_REGEN] > 0 ) {
		ent->s.effects |= EF_REGEN;
		if( client->ps.inventory[POWERUP_REGEN] < 6 ) {
			ent->s.effects |= EF_EXPIRING_REGEN;
		}
	}

	if( ent->s.weapon ) {
		const firedef_t *firedef = GS_FiredefForPlayerState( ggs, &client->ps, ent->s.weapon );
		if( firedef && firedef->fire_mode == FIRE_MODE_STRONG ) {
			ent->s.effects |= EF_STRONG_WEAPON;
		}
	}

	if( client->ps.pmove.stats[PM_STAT_STUN] ) {
		ent->s.effects |= EF_PLAYER_STUNNED;
	} else {
		ent->s.effects &= ~EF_PLAYER_STUNNED;
	}

	// show cheaters!!!
	if( ent->flags & FL_GODMODE ) {
		ent->s.effects |= EF_GODMODE;
	}

	// add chatting icon effect
	if( ent->r.client->snap.buttons & BUTTON_BUSYICON ) {
		ent->s.effects |= EF_BUSYICON;
	}
}

static void G_SetClientSound( edict_t *ent ) {
	if( ent->waterlevel == 3 ) {
		if( ent->watertype & CONTENTS_LAVA ) {
			ent->s.sound = SV_SoundIndex( S_WORLD_UNDERLAVA );
		} else if( ent->watertype & CONTENTS_SLIME ) {
			ent->s.sound = SV_SoundIndex( S_WORLD_UNDERSLIME );
		} else if( ent->watertype & CONTENTS_WATER ) {
			ent->s.sound = SV_SoundIndex( S_WORLD_UNDERWATER );
		}
	} else {
		ent->s.sound = 0;
	}
}

void G_SetClientFrame( edict_t *ent ) {
	if( ent->s.type != ET_PLAYER ) {
		return;
	}

	ent->s.frame = 0;
}

/*
* G_ClientEndSnapFrame
*
* Called for each player at the end of the server frame
* and right after spawning
*/
void G_ClientEndSnapFrame( edict_t *ent ) {
	if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
		return;
	}

	Client *client = ent->r.client;

	// If the end of unit layout is displayed, don't give
	// the player any normal movement attributes
	if( GS_MatchState( *ggs ) >= MATCH_STATE_POSTMATCH ) {
		client->setReplicatedStats();
	} else {
		if( G_IsDead( ent ) && !level.gametype.customDeadBodyCam ) {
			G_Client_DeadView( ent );
		}

		G_PlayerWorldEffects( ent ); // burn from lava, etc
		G_ClientDamageFeedback( ent ); // show damage taken along the snap
		client->setReplicatedStats();
		G_SetClientEffects( ent );
		G_SetClientSound( ent );
		G_SetClientFrame( ent );

		client->ps.plrkeys = client->snap.plrkeys;
	}

	G_ReleaseClientPSEvent( client );

	// set the delta angle
	for( int i = 0; i < 3; i++ ) {
		client->ps.pmove.delta_angles[i] = ANGLE2SHORT( client->ps.viewangles[i] ) - client->ucmd.angles[i];
	}

	// this is pretty hackish
	if( !G_ISGHOSTING( ent ) ) {
		VectorCopy( ent->velocity, ent->s.origin2 );
	}
}

void G_ClientThink( edict_t *ent ) {
	if( !ent || !ent->r.client ) {
		return;
	}

	if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
		return;
	}

	ent->r.client->ps.POVnum = ENTNUM( ent ); // set self

	// load instashield
	if( GS_Instagib( *ggs ) && g_instashield->integer ) {
		if( ent->s.team >= TEAM_PLAYERS && ent->s.team < GS_MAX_TEAMS ) {
			if( ent->r.client->ps.inventory[POWERUP_SHELL] > 0 ) {
				ent->r.client->instashieldCharge -= ( game.frametime * 0.001f ) * 60.0f;
				Q_clamp( ent->r.client->instashieldCharge, 0, INSTA_SHIELD_MAX );
				if( ent->r.client->instashieldCharge == 0 ) {
					ent->r.client->ps.inventory[POWERUP_SHELL] = 0;
				}
			} else {
				ent->r.client->instashieldCharge += ( game.frametime * 0.001f ) * 20.0f;
				Q_clamp( ent->r.client->instashieldCharge, 0, INSTA_SHIELD_MAX );
			}
		}
	}

	// run bots thinking with the rest of clients
	if( ent->r.svflags & SVF_FAKECLIENT ) {
		if( !ent->think && ent->bot ) {
			AI_Think( ent );
		}
	}

	SV_ExecuteClientThinks( PLAYERNUM( ent ) );
}

void Client::executeUcmd( const usercmd_t &ucmd_, int timeDelta_ ) {
	const usercmd_t oldUcmd( ucmd );
	ucmd = ucmd_;

	ps.POVnum = ENTNUM( getEntity() );
	ps.playerNum = PLAYERNUM( getEntity() );

	deltas.add( timeDelta_ );
	timeDelta = wsw::clamp( deltas.avg(), -g_antilag_maxtimedelta->integer, 0 );

	if( hasNewActivity( oldUcmd ) ) {
		last_activity = level.time;
	}

	// can exit intermission after two seconds, not counting postmatch
	if( GS_MatchState( *ggs ) == MATCH_STATE_WAITEXIT && game.serverTime > GS_MatchStartTime( *ggs ) + 2000 ) {
		if( ucmd.buttons & BUTTON_ATTACK ) {
			level.exitNow = true;
		}
	}

	pmove_t pm;
	runPMove( &pm );

	GS_AddLaserbeamPoint( ggs, &trail, &ps, ucmd.serverTimeStamp );

	checkRegeneration();

	runTouch( pm.touchents, pm.numtouch );

	auto *const ent = getEntity();
	ent->s.weapon = GS_ThinkPlayerWeapon( ggs, &ps, ucmd.buttons, ucmd.msec, timeDelta );

	if( G_IsDead( ent ) ) {
		if( ent->deathTimeStamp + g_respawn_delay_min->integer <= level.time ) {
			snap.buttons |= ucmd.buttons;
		}
	} else if( ps.pmove.stats[PM_STAT_NOUSERCONTROL] <= 0 ) {
		snap.buttons |= ucmd.buttons;
	}

	checkInstaShield();

	if( ps.pmove.pm_type == PM_NORMAL ) {
		if( GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME ) {
			stats.had_playtime = true;
			// StatsowFacade::Instance()->OnClientHadPlaytime( this );
		}
	}

	snap.plrkeys |= ucmdToPlayerKeys();
}

void Client::runPMove( pmove_t *pm ) {
	auto *const ent = getEntity();

	// (is this really needed?:only if not cared enough about ps in the rest of the code)
	// refresh player state position from the entity
	VectorCopy( ent->s.origin, ps.pmove.origin );
	VectorCopy( ent->velocity, ps.pmove.velocity );
	VectorCopy( ent->s.angles, ps.viewangles );

	ps.pmove.gravity = level.gravity;

	bool freeze = GS_MatchState( *ggs ) >= MATCH_STATE_POSTMATCH || GS_MatchPaused( *ggs );
	if( !freeze ) {
		if( ent->movetype != MOVETYPE_PLAYER && ent->movetype != MOVETYPE_NOCLIP ) {
			freeze = true;
		}
	}

	if( freeze ) {
		ps.pmove.pm_type = PM_FREEZE;
	} else if( ent->s.type == ET_GIB ) {
		ps.pmove.pm_type = PM_GIB;
	} else if( ent->movetype == MOVETYPE_NOCLIP ) {
		ps.pmove.pm_type = PM_SPECTATOR;
	} else {
		ps.pmove.pm_type = PM_NORMAL;
	}

	std::memset( pm, 0, sizeof( pmove_t ) );
	pm->playerState = &ps;
	pm->cmd = ucmd;

	// A grand hack to disable occasional ladder usage for bots/AI beings without intrusive changes to bot code
	if( getEntity()->bot ) {
		pm->skipLadders = true;
	}

	// TODO: Comparing structs via memcmp is a bug waiting to happen
	if( std::memcmp( (const void *)&old_pmove, (const void *)&ps.pmove, sizeof( pmove_state_t ) ) != 0 ) {
		pm->snapInitially = true;
	}

	// perform a pmove
	Pmove( ggs, pm );

	// save results of pmove
	old_pmove = ps.pmove;

	// update the entity with the new position
	VectorCopy( ps.pmove.origin, ent->s.origin );
	VectorCopy( ps.pmove.velocity, ent->velocity );
	VectorCopy( ps.viewangles, ent->s.angles );
	ent->viewheight = ps.viewheight;
	VectorCopy( pm->mins, ent->r.mins );
	VectorCopy( pm->maxs, ent->r.maxs );

	ent->waterlevel = pm->waterlevel;
	ent->watertype = pm->watertype;
	if( pm->groundentity == -1 ) {
		ent->groundentity = nullptr;
	} else {
		G_AwardResetPlayerComboStats( ent );

		ent->groundentity = &game.edicts[pm->groundentity];
		ent->groundentity_linkcount = ent->groundentity->linkcount;
	}

	GClip_LinkEntity( ent );
}

void Client::checkRegeneration() {
	auto *const ent = getEntity();
	if( ps.inventory[POWERUP_REGEN] > 0 && ent->health < 200 ) {
		ent->health += ( game.frametime * 0.001f ) * 10.0f;

		// Regen expires if health reaches 200
		if( ent->health >= 199.0f ) {
			ps.inventory[POWERUP_REGEN]--;
		}
	}
}

void Client::runTouch( int *entNums, int numTouchEnts ) {
	auto *const ent = getEntity();
	// fire touch functions
	if( ent->movetype != MOVETYPE_NOCLIP ) {
		edict_t *other;

		int i, j;
		// touch other objects
		for( i = 0; i < numTouchEnts; i++ ) {
			other = &game.edicts[entNums[i]];
			for( j = 0; j < i; j++ ) {
				if( &game.edicts[entNums[j]] == other ) {
					break;
				}
			}
			if( j != i ) {
				continue; // duplicated
			}
			// player can't touch projectiles, only projectiles can touch the player
			G_CallTouch( other, ent, NULL, 0 );
		}
	}
}

void Client::checkInstaShield() {
	if( GS_Instagib( *ggs ) && g_instashield->integer ) {
		if( ps.pmove.pm_type == PM_NORMAL && ucmd.upmove < 0 ) {
			if( instashieldCharge == INSTA_SHIELD_MAX && ps.inventory[POWERUP_SHELL] == 0 ) {
				ps.inventory[POWERUP_SHELL] = instashieldCharge;
				int soundIndex = SV_SoundIndex( GS_FindItemByTag( ggs, POWERUP_SHELL )->pickup_sound );
				G_Sound( getEntity(), CHAN_AUTO, soundIndex, ATTN_NORM );
			}
		}
	}
}

[[nodiscard]]
static inline auto getBitForCondition( bool condition, int shift ) -> uint8_t {
	return (uint8_t)( condition ? ( 1 << shift ) : 0 );
}

auto Client::ucmdToPlayerKeys() const -> uint8_t {
	uint8_t result = 0;
	result |= getBitForCondition( ucmd.forwardmove > 0, KEYICON_FORWARD );
	result |= getBitForCondition( ucmd.forwardmove < 0, KEYICON_BACKWARD );
	result |= getBitForCondition( ucmd.sidemove > 0, KEYICON_RIGHT );
	result |= getBitForCondition( ucmd.sidemove < 0, KEYICON_LEFT );
	result |= getBitForCondition( ucmd.upmove > 0, KEYICON_JUMP );
	result |= getBitForCondition( ucmd.upmove < 0, KEYICON_CROUCH );
	result |= getBitForCondition( ( ucmd.buttons & BUTTON_ATTACK ) != 0, KEYICON_FIRE );
	result |= getBitForCondition( ( ucmd.buttons & BUTTON_SPECIAL ) != 0, KEYICON_SPECIAL );
	return result;
}

bool Client::hasNewActivity( const usercmd_t &oldUcmd ) const {
	if( ucmd.forwardmove || ucmd.sidemove || ucmd.upmove ) {
		return true;
	}
	if( ucmd.buttons & ~BUTTON_BUSYICON ) {
		return true;
	}
	if( !VectorCompare( ucmd.angles, oldUcmd.angles ) ) {
		return true;
	}
	return false;
}

void G_ClientThink( edict_t *ent, usercmd_t *ucmd, int timeDelta ) {
	ent->r.client->executeUcmd( *ucmd, timeDelta );
}

void G_CheckClientRespawnClick( edict_t *ent ) {
	if( !ent->r.inuse || !ent->r.client || !G_IsDead( ent ) ) {
		return;
	}

	if( GS_MatchState( *ggs ) >= MATCH_STATE_POSTMATCH ) {
		return;
	}

	if( G_GetClientState( PLAYERNUM( ent ) ) >= CS_SPAWNED ) {
		// if the spawnsystem doesn't require to click
		if( G_SpawnQueue_GetSystem( ent->s.team ) != SPAWNSYSTEM_INSTANT ) {
			int minDelay = g_respawn_delay_min->integer;

			// waves system must wait for at least 500 msecs (to see the death, but very short for selfkilling tactics).
			if( G_SpawnQueue_GetSystem( ent->s.team ) == SPAWNSYSTEM_WAVES ) {
				minDelay = ( g_respawn_delay_min->integer < 500 ) ? 500 : g_respawn_delay_min->integer;
			}

			// hold system must wait for at least 1000 msecs (to see the death properly)
			if( G_SpawnQueue_GetSystem( ent->s.team ) == SPAWNSYSTEM_HOLD ) {
				minDelay = ( g_respawn_delay_min->integer < 1300 ) ? 1300 : g_respawn_delay_min->integer;
			}

			if( level.time >= ent->deathTimeStamp + minDelay ) {
				G_SpawnQueue_AddClient( ent );
			}
		}
		// clicked
		else if( ent->r.client->snap.buttons & BUTTON_ATTACK ) {
			if( level.time > ent->deathTimeStamp + g_respawn_delay_min->integer ) {
				G_SpawnQueue_AddClient( ent );
			}
		}
		// didn't click, but too much time passed
		else if( g_respawn_delay_max->integer && ( level.time > ent->deathTimeStamp + g_respawn_delay_max->integer ) ) {
			G_SpawnQueue_AddClient( ent );
		}
	}
}

#undef PLAYER_MASS

static unsigned int G_FindPointedPlayer( const edict_t *self ) {
	if( G_IsDead( self ) ) {
		return 0;
	}

	vec3_t vieworg, dir, viewforward;
	const auto &ps = self->r.client->ps;
	// we can't handle the thirdperson modifications in server side :/
	VectorSet( vieworg, ps.pmove.origin[0], ps.pmove.origin[1], ps.pmove.origin[2] + ps.viewheight );
	AngleVectors( ps.viewangles, viewforward, NULL, NULL );

	int bestNum = 0;
	float bestValue = 0.90f;
	for( int i = 0; i < ggs->maxclients; i++ ) {
		const edict_t *other = PLAYERENT( i );
		if( !other->r.inuse ) {
			continue;
		}
		if( !other->r.client ) {
			continue;
		}
		if( other == self ) {
			continue;
		}
		if( !other->r.solid || ( other->r.svflags & SVF_NOCLIENT ) ) {
			continue;
		}

		VectorSubtract( other->s.origin, self->s.origin, dir );
		const float dist = VectorNormalize2( dir, dir );
		if( dist > 1000 ) {
			continue;
		}

		if( const float value = DotProduct( dir, viewforward ); value > bestValue ) {
			vec3_t boxpoints[8];
			BuildBoxPoints( boxpoints, other->s.origin, tv( 4, 4, 4 ), tv( 4, 4, 4 ) );
			for( int j = 0; j < 8; j++ ) {
				trace_t trace;
				G_Trace( &trace, vieworg, vec3_origin, vec3_origin, boxpoints[j], self, MASK_SHOT | MASK_OPAQUE );
				if( trace.ent && trace.ent == ENTNUM( other ) ) {
					bestValue = value;
					bestNum = ENTNUM( other );
					break;
				}
			}
		}
	}

	return bestNum;
}

auto Client::getEntity() -> edict_t * {
	return game.edicts + ENTNUM( this );
}

auto Client::getEntity() const -> const edict_t * {
	return game.edicts + ENTNUM( this );
}

bool Client::isFakeClient() const {
	return ( getEntity()->r.svflags & SVF_FAKECLIENT ) != 0;
}

void G_ClientUserinfoChanged( edict_t *ent, char *userinfo ) {
	ent->r.client->setUserInfo( wsw::StringView( userinfo ) );
}

void Client::setUserInfo( const wsw::StringView &rawInfo ) {
	if( m_userInfo.parse( rawInfo ) ) {
		handleUserInfoChanges();
	} else {
		G_DropClient( getEntity(), ReconnectBehaviour::OfUserChoice, "Error: Invalid userinfo" );
	}
}

static inline auto calcAccuracy( int64_t hits, int64_t shots ) {
	// The ratio may go > 1 for beams / AOE weapons
	hits = wsw::min( hits, shots );
	int result;
	// Try forcing a short integer division
	if( shots < (int64_t)std::numeric_limits<uint16_t>::max() / 100 ) [[likely]] {
		result = ( (uint16_t)100 * (uint16_t)hits ) / (uint16_t)shots;
	} else if( shots < (int64_t)std::numeric_limits<int>::max() / 100 ) {
		result = ( 100 * (int)hits ) / (int)shots;
	} else {
		result = (int)( ( 100 * hits ) / shots );
	}
	if( hits ) {
		result = wsw::max( 1, result );
	}
	assert( result >= 0 && result <= 100 );
	return (uint8_t)result;
}

void Client::setReplicatedStats() {
	if( chase.active ) { // in chasecam it copies the other player stats
		return;
	}

	ps.stats[STAT_FLAGS] = 0;

	// don't force scoreboard when dead during timeout
	if( showscores || GS_MatchState( *ggs ) >= MATCH_STATE_POSTMATCH ) {
		ps.stats[STAT_FLAGS] |= STAT_FLAG_SCOREBOARD;
	}
	if( GS_HasChallengers( *ggs ) && queueTimeStamp ) {
		ps.stats[STAT_FLAGS] |= STAT_FLAG_CHALLENGER;
	}
	if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP && level.ready[PLAYERNUM( this )] ) {
		ps.stats[STAT_FLAGS] |= STAT_FLAG_READY;
	}
	if( isoperator ) {
		ps.stats[STAT_FLAGS] |= STAT_FLAG_OPERATOR;
	}

	const auto *ent = getEntity();
	if( G_IsDead( ent ) ) {
		ps.stats[STAT_FLAGS] |= STAT_FLAG_DEADPOV;
	}

	ps.stats[STAT_TEAM] = ps.stats[STAT_REALTEAM] = ent->s.team;

	if( ent->s.team == TEAM_SPECTATOR ) {
		ps.stats[STAT_HEALTH] = STAT_NOTSET; // no health for spectator
	} else {
		ps.stats[STAT_HEALTH] = HEALTH_TO_INT( ent->health );
	}

	m_frags = ps.stats[STAT_SCORE];

	if( GS_Instagib( *ggs ) ) {
		if( g_instashield->integer ) {
			ps.stats[STAT_ARMOR] = ARMOR_TO_INT( 100.0f * ( instashieldCharge / INSTA_SHIELD_MAX ) );
		} else {
			ps.stats[STAT_ARMOR] = 0;
		}
	} else {
		ps.stats[STAT_ARMOR] = ARMOR_TO_INT( armor );
	}

	if( level.time > pickup_msg_time ) {
		ps.stats[STAT_PICKUP_ITEM] = 0;
	}

	if( ent->s.team == TEAM_SPECTATOR ) {
		ps.stats[STAT_SCORE] = STAT_NOTSET; // no frags for spectators
	} else {
		ps.stats[STAT_SCORE] = ent->r.client->stats.score;
	}

	//
	// Team scores
	//
	if( GS_TeamBasedGametype( *ggs ) ) {
		// team based
		for( int i = 0, team_ = TEAM_ALPHA; team_ < GS_MAX_TEAMS; i++, team_++ ) {
			ps.stats[STAT_TEAM_ALPHA_SCORE + i] = teamlist[team_].stats.score;
		}
	} else {
		// not team based
		for( int i = 0, team_ = TEAM_ALPHA; team_ < GS_MAX_TEAMS; i++, team_++ ) {
			ps.stats[STAT_TEAM_ALPHA_SCORE + i] = STAT_NOTSET;
		}
	}

	// spawn system
	ps.stats[STAT_NEXT_RESPAWN] = ceil( G_SpawnQueue_NextRespawnTime( team ) * 0.001f );

	// pointed player
	const int pointedPlayerEntNum = G_FindPointedPlayer( ent );
	ps.stats[STAT_POINTED_TEAMPLAYER] = 0;
	ps.stats[STAT_POINTED_PLAYER] = pointedPlayerEntNum;
	if( pointedPlayerEntNum && GS_TeamBasedGametype( *ggs ) ) {
		const edict_t *e = &game.edicts[pointedPlayerEntNum];
		if( e->s.team == ent->s.team ) {
			int pointedhealth = HEALTH_TO_INT( e->health );
			int pointedarmor = 0;
			int available_bits = 0;
			bool mega = false;

			if( pointedhealth < 0 ) {
				pointedhealth = 0;
			}
			if( pointedhealth > 100 ) {
				pointedhealth -= 100;
				mega = true;
				if( pointedhealth > 100 ) {
					pointedhealth = 100;
				}
			}
			pointedhealth /= 3.2;

			if( GS_Armor_TagForCount( ggs, e->r.client->armor ) ) {
				pointedarmor = ARMOR_TO_INT( e->r.client->armor );
			}
			if( pointedarmor > 150 ) {
				pointedarmor = 150;
			}
			pointedarmor /= 5;

			ps.stats[STAT_POINTED_TEAMPLAYER] = (short)( ( pointedhealth & 0x1F ) | ( pointedarmor & 0x3F ) << 6 | ( available_bits & 0xF ) << 12 );
			if( mega ) {
				ps.stats[STAT_POINTED_TEAMPLAYER] |= 0x20;
			}
		}
	}

	ps.stats[STAT_LAST_KILLER] = 0;
	// last killer. ignore world and team kills
	if( const auto *attacker = last_killer ) {
		if( attacker->r.client && !GS_IsTeamDamage( ggs, &ent->s, &attacker->s ) ) {
			ps.stats[STAT_LAST_KILLER] = (short)ENTNUM( attacker );
		}
	}

	std::memset( ps.strongAccuracy, 0, sizeof( ps.strongAccuracy ) );
	std::memset( ps.weakAccuracy, 0, sizeof( ps.weakAccuracy ) );
	static_assert( kNumAccuracySlots + 1 == WEAP_TOTAL );
	static_assert( AMMO_WEAK_GUNBLADE > AMMO_GUNBLADE );
	const unsigned weakOffset = AMMO_WEAK_GUNBLADE - AMMO_GUNBLADE;
	const unsigned strongOffset = 0;
	for( unsigned i = 0; i < kNumAccuracySlots; ++i ) {
		if( const int weakShots = stats.accuracy_shots[weakOffset + i] ) {
			const int weakHits = stats.accuracy_hits[weakOffset + i];
			ps.weakAccuracy[i] = calcAccuracy( weakHits, weakShots );
		}
		if( const int strongShots = stats.accuracy_shots[strongOffset + i] ) {
			const int strongHits = stats.accuracy_hits[strongOffset + i];
			ps.strongAccuracy[i] = calcAccuracy( strongHits, strongShots );
		}
	}
}

