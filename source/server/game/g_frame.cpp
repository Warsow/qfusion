/*
Copyright (C) 2006 Pekka Lampila ("Medar"), Damien Deville ("Pb")
and German Garcia Fernandez ("Jal") for Chasseur de bots association.


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
#include <common/helpers/stringsplitter.h>
#include <common/types/staticstring.h>
#include <common/types/staticvector.h>
#include <common/facilities/cvar.h>
#include <common/common.h>
#include <common/facilities/profilerscope.h>
#include "ai/vec3.h"

#include <chrono>
#include <tuple>

extern cvar_t *g_votable_gametypes;
extern cvar_t *g_disable_vote_gametype;

//===================================================================

/*
* G_Timeout_Reset
*/
void G_Timeout_Reset( void ) {
	int i;

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_PAUSED, false );
	level.timeout.time = 0;
	level.timeout.endtime = 0;
	level.timeout.caller = 0;
	for( i = 0; i < MAX_CLIENTS; i++ )
		level.timeout.used[i] = 0;
}

/*
* G_Timeout_Update
*
* Updates the timeout struct and informs clients about the status of the pause
*/
static void G_Timeout_Update( unsigned int msec ) {
	static int timeout_printtime = 0;
	static int timeout_last_endtime = 0;
	static int countdown_set = 1;

	if( !GS_MatchPaused( *ggs ) ) {
		return;
	}

	game.frametime = 0;

	if( timeout_last_endtime != level.timeout.endtime ) { // force print when endtime is changed
		timeout_printtime = 0;
		timeout_last_endtime = level.timeout.endtime;
	}

	level.timeout.time += msec;
	if( level.timeout.endtime && level.timeout.time >= level.timeout.endtime ) {
		level.timeout.time = 0;
		level.timeout.caller = -1;
		GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_PAUSED, false );

		timeout_printtime = 0;
		timeout_last_endtime = -1;

		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_MATCH_RESUMED_1_to_2, ( rand() & 1 ) + 1 ) ),
						  GS_MAX_TEAMS, true, NULL );
		G_CenterPrintMsg( NULL, "Match resumed" );
		G_PrintMsg( NULL, "Match resumed\n" );
	} else if( timeout_printtime == 0 || level.timeout.time - timeout_printtime >= 1000 ) {
		if( level.timeout.endtime ) {
			int seconds_left = (int)( ( level.timeout.endtime - level.timeout.time ) / 1000.0 + 0.5 );

			if( seconds_left == ( TIMEIN_TIME * 2 ) / 1000 ) {
				G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_READY_1_to_2, ( rand() & 1 ) + 1 ) ),
								  GS_MAX_TEAMS, false, NULL );
				countdown_set = ( rand() & 1 ) + 1;
			} else if( seconds_left >= 1 && seconds_left <= 3 ) {
				G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, seconds_left,
															 countdown_set ) ), GS_MAX_TEAMS, false, NULL );
			}

			if( seconds_left > 1 ) {
				G_CenterPrintFormatMsg( NULL, 1, "Match will resume in %s seconds", va( "%i", seconds_left ) );
			} else {
				G_CenterPrintMsg( NULL, "Match will resume in 1 second" );
			}
		} else {
			G_CenterPrintMsg( NULL, "Match paused" );
		}

		timeout_printtime = level.timeout.time;
	}
}

/*
* G_UpdateServerInfo
* update the cvars which show the match state at server browsers
*/
static void G_UpdateServerInfo( void ) {
	// g_match_time
	if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP ) {
		Cvar_ForceSet( "g_match_time", "Warmup" );
	} else if( GS_MatchState( *ggs ) == MATCH_STATE_COUNTDOWN ) {
		Cvar_ForceSet( "g_match_time", "Countdown" );
	} else if( GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME ) {
		// partly from G_GetMatchState
		char extra[MAX_INFO_VALUE];
		int clocktime, timelimit, mins, secs;

		if( GS_MatchDuration( *ggs ) ) {
			timelimit = ( ( GS_MatchDuration( *ggs ) ) * 0.001 ) / 60;
		} else {
			timelimit = 0;
		}

		clocktime = (float)( game.serverTime - GS_MatchStartTime( *ggs ) ) * 0.001f;

		if( clocktime <= 0 ) {
			mins = 0;
			secs = 0;
		} else {
			mins = clocktime / 60;
			secs = clocktime - mins * 60;
		}

		extra[0] = 0;
		if( GS_MatchExtended( *ggs ) ) {
			if( timelimit ) {
				Q_strncatz( extra, " overtime", sizeof( extra ) );
			} else {
				Q_strncatz( extra, " suddendeath", sizeof( extra ) );
			}
		}
		if( GS_MatchPaused( *ggs ) ) {
			Q_strncatz( extra, " (in timeout)", sizeof( extra ) );
		}

		if( timelimit ) {
			Cvar_ForceSet( "g_match_time", va( "%02i:%02i / %02i:00%s", mins, secs, timelimit, extra ) );
		} else {
			Cvar_ForceSet( "g_match_time", va( "%02i:%02i%s", mins, secs, extra ) );
		}
	} else {
		Cvar_ForceSet( "g_match_time", "Finished" );
	}

	// g_match_score
	if( GS_MatchState( *ggs ) >= MATCH_STATE_PLAYTIME && GS_TeamBasedGametype( *ggs ) ) {
		char score[MAX_INFO_STRING];

		score[0] = 0;
		Q_strncatz( score, va( " %s: %d", GS_TeamName( ggs, TEAM_ALPHA ), teamlist[TEAM_ALPHA].stats.score ), sizeof( score ) );
		Q_strncatz( score, va( " %s: %d", GS_TeamName( ggs, TEAM_BETA ), teamlist[TEAM_BETA].stats.score ), sizeof( score ) );

		if( strlen( score ) >= MAX_INFO_VALUE ) {
			// prevent "invalid info cvar value" flooding
			score[0] = '\0';
		}
		Cvar_ForceSet( "g_match_score", score );
	} else {
		Cvar_ForceSet( "g_match_score", "" );
	}

	// g_needpass
	if( password->modified ) {
		if( password->string && strlen( password->string ) ) {
			Cvar_ForceSet( "g_needpass", "1" );
		} else {
			Cvar_ForceSet( "g_needpass", "0" );
		}
		password->modified = false;
	}

	// g_gametypes_available
	if( g_votable_gametypes->modified || g_disable_vote_gametype->modified ) {
		if( g_disable_vote_gametype->integer || !g_votable_gametypes->string || !strlen( g_votable_gametypes->string ) ) {
			Cvar_ForceSet( "g_gametypes_available", "" );
		} else {
			size_t len = 0;

			const wsw::StringView viewOfGametypesList( g_gametypes_list->string );
			do {
				wsw::StringSplitter splitter( viewOfGametypesList );
				while( const auto maybeName = splitter.getNext( CHAR_GAMETYPE_SEPARATOR ) ) {
					if( G_Gametype_IsVotable( *maybeName ) ) {
						len += maybeName->length() + 1;
					}
				}
			} while( false );

			len++;
			char *votable = ( char * )Q_malloc( len );
			votable[0] = 0;

			wsw::StringSplitter splitter( viewOfGametypesList );
			while( const auto maybeName = splitter.getNext( CHAR_GAMETYPE_SEPARATOR ) ) {
				if( G_Gametype_IsVotable( *maybeName ) ) {
					wsw::StaticString<256> name( *maybeName );
					Q_strncatz( votable, name.data(), len );
					Q_strncatz( votable, " ", len );
				}
			}

			//votable[ strlen( votable )-2 ] = 0; // remove the last space
			Cvar_ForceSet( "g_gametypes_available", votable );
			Q_free( votable );
		}

		g_votable_gametypes->modified = false;
		g_disable_vote_gametype->modified = false;
	}

	if( GS_RaceGametype( *ggs ) ) {
		Cvar_ForceSet( "g_race_gametype", "1" );
	} else {
		Cvar_ForceSet( "g_race_gametype", "0" );
	}
}

/*
* G_CheckCvars
* Check for cvars that have been modified and need the game to be updated
*/
void G_CheckCvars( void ) {
	if( g_antilag_maxtimedelta->modified ) {
		if( g_antilag_maxtimedelta->integer < 0 ) {
			Cvar_SetValue( "g_antilag_maxtimedelta", abs( g_antilag_maxtimedelta->integer ) );
		}
		g_antilag_maxtimedelta->modified = false;
		g_antilag_timenudge->modified = true;
	}

	if( g_antilag_timenudge->modified ) {
		if( g_antilag_timenudge->integer > g_antilag_maxtimedelta->integer ) {
			Cvar_SetValue( "g_antilag_timenudge", g_antilag_maxtimedelta->integer );
		} else if( g_antilag_timenudge->integer < -g_antilag_maxtimedelta->integer ) {
			Cvar_SetValue( "g_antilag_timenudge", -g_antilag_maxtimedelta->integer );
		}
		g_antilag_timenudge->modified = false;
	}

	if( g_warmup_timelimit->modified ) {
		// if we are inside timelimit period, update the endtime
		if( GS_MatchState( *ggs ) == MATCH_STATE_WARMUP ) {
			ggs->gameState.stats[GAMESTAT_MATCHDURATION] = (int64_t)fabs( 60.0f * 1000 * g_warmup_timelimit->integer );
		}
		g_warmup_timelimit->modified = false;
	}

	if( g_timelimit->modified ) {
		// if we are inside timelimit period, update the endtime
		if( GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME &&
			!GS_MatchExtended( *ggs ) ) {
			if( g_timelimit->value ) {
				ggs->gameState.stats[GAMESTAT_MATCHDURATION] = (int64_t)fabs( 60.0f * 1000 * g_timelimit->value );
			} else {
				ggs->gameState.stats[GAMESTAT_MATCHDURATION] = 0;
			}
		}
		g_timelimit->modified = false;
	}

	if( g_match_extendedtime->modified ) {
		// if we are inside extended_time period, update the endtime
		if( GS_MatchExtended( *ggs ) ) {
			if( g_match_extendedtime->integer ) {
				ggs->gameState.stats[GAMESTAT_MATCHDURATION] = (int64_t)fabs( 60 * 1000 * g_match_extendedtime->value );
			}
		}
		g_match_extendedtime->modified = false;
	}

	if( g_allow_falldamage->modified ) {
		g_allow_falldamage->modified = false;
	}

	// update common server settings

	// FIXME: This should be restructured so common settings are the master settings
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_INSTAGIB, ( g_instagib->integer != 0 ) );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_FALLDAMAGE, ( g_allow_falldamage->integer != 0 ) );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_SELFDAMAGE, ( g_allow_selfdamage->integer != 0 ) );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_HASCHALLENGERS, level.gametype.hasChallengersQueue );

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_ISTEAMBASED, level.gametype.isTeamBased );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_ISRACE, level.gametype.isRace );

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_COUNTDOWN, level.gametype.countdownEnabled );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_INHIBITSHOOTING, level.gametype.shootingDisabled );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_INFINITEAMMO, ( level.gametype.infiniteAmmo || GS_Instagib( *ggs ) ) );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_CANFORCEMODELS, level.gametype.canForceModels );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_CANSHOWMINIMAP, level.gametype.canShowMinimap );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_TEAMONLYMINIMAP, level.gametype.teamOnlyMinimap );

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_MMCOMPATIBLE, level.gametype.mmCompatible );

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_ISTUTORIAL, level.gametype.isTutorial );
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_CANDROPWEAPON, ( level.gametype.dropableItemsMask & IT_WEAPON ) != 0 );

	ggs->gameState.stats[GAMESTAT_MAXPLAYERSINTEAM] = level.gametype.maxPlayersPerTeam;
	Q_clamp( ggs->gameState.stats[GAMESTAT_MAXPLAYERSINTEAM], 0, 255 );

}

//===================================================================
//		SNAP FRAMES
//===================================================================

static bool g_snapStarted = false;

/*
* G_SnapClients
*/
void G_SnapClients( void ) {
	if( g_inactivity_maxtime->modified ) {
		if( g_inactivity_maxtime->value <= 0.0f ) {
			Cvar_ForceSet( "g_inactivity_maxtime", "0.0" );
		} else if( g_inactivity_maxtime->value < 15.0f ) {
			Cvar_ForceSet( "g_inactivity_maxtime", "15.0" );
		}

		g_inactivity_maxtime->modified = false;
	}

	const int64_t inactivityMillis = g_inactivity_maxtime->integer * 1000;

	// calc the player views now that all pushing and damage has been added
	for( int i = 0; i < ggs->maxclients; i++ ) {
		if( edict_t *const ent = game.edicts + 1 + i; ent->r.inuse ) {
			if( auto *const client = ent->r.client ) {
				if( inactivityMillis ) {
					G_Client_InactivityRemove( client, inactivityMillis );
				}

				G_ClientEndSnapFrame( ent );

				if ( ent->s.effects & EF_BUSYICON ) {
					ent->flags |= FL_BUSY;
				} else {
					ent->flags &= ~FL_BUSY;
				}
			}
		}
	}

	G_EndServerFrames_UpdateChaseCam();
}

/*
* G_EdictsAddSnapEffects
* add effects based on accumulated info along the server frame
*/
static void G_SnapEntities( void ) {
	edict_t *ent;
	int i;
	vec3_t dir, origin;

	for( i = 0, ent = &game.edicts[0]; i < game.numentities; i++, ent++ ) {
		if( !ent->r.inuse || ( ent->r.svflags & SVF_NOCLIENT ) ) {
			continue;
		}

		if( ent->s.type == ET_PARTICLES ) { // particles use a special configuration
			ent->s.frame = ent->particlesInfo.speed;
			ent->s.modelindex = ent->particlesInfo.shaderIndex;
			ent->s.modelindex2 = ent->particlesInfo.spread;
			ent->s.counterNum = ent->particlesInfo.time;
			ent->s.weapon = ent->particlesInfo.frequency;

			ent->s.effects = ent->particlesInfo.size & 0xFF;
			if( ent->particlesInfo.spherical ) {
				ent->s.effects |= ( 1 << 8 );
			}
			if( ent->particlesInfo.bounce ) {
				ent->s.effects |= ( 1 << 9 );
			}
			if( ent->particlesInfo.gravity ) {
				ent->s.effects |= ( 1 << 10 );
			}
			if( ent->particlesInfo.expandEffect ) {
				ent->s.effects |= ( 1 << 11 );
			}
			if( ent->particlesInfo.shrinkEffect ) {
				ent->s.effects |= ( 1 << 11 );
			}

			GClip_LinkEntity( ent );

			continue;
		}

		if( ent->s.type == ET_PLAYER || ent->s.type == ET_CORPSE ) {
			// this is pretty hackish
			if( !G_ISGHOSTING( ent ) ) {
				VectorCopy( ent->velocity, ent->s.origin2 );
			}
		}

		if( ISEVENTENTITY( ent ) || G_ISGHOSTING( ent ) || !ent->takedamage ) {
			continue;
		}

		// types which can have accumulated damage effects
		if( ( ent->s.type == ET_GENERIC || ent->s.type == ET_PLAYER || ent->s.type == ET_CORPSE ) ) { // doors don't bleed
			// Until we get a proper damage saved effect, we accumulate both into the blood fx
			// so, at least, we don't send 2 entities where we can send one
			ent->snap.damage_taken += ent->snap.damage_saved;

			//ent->snap.damage_saved = 0;

			//spawn accumulated damage
			if( ent->snap.damage_taken && !( ent->flags & FL_GODMODE ) && HEALTH_TO_INT( ent->health ) > 0 ) {
				edict_t *event;
				float damage = ent->snap.damage_taken;
				if( damage > 120 ) {
					damage = 120;
				}

				VectorCopy( ent->snap.damage_dir, dir );
				VectorNormalize( dir );
				VectorAdd( ent->s.origin, ent->snap.damage_at, origin );

				if( ent->s.type == ET_PLAYER || ent->s.type == ET_CORPSE ) {
					event = G_SpawnEvent( EV_BLOOD, DirToByte( dir ), origin );
					event->s.damage = HEALTH_TO_INT( damage );
					event->s.ownerNum = i; // set owner

					// ET_PLAYERS can also spawn sound events
					if( ent->s.type == ET_PLAYER ) {
						// play an apropriate pain sound
						if( level.time >= ent->pain_debounce_time ) {
							// see if it should pain for a FALL or for damage received
							if( ent->snap.damage_fall ) {
								ent->pain_debounce_time = level.time + 200;
							} else if( !G_IsDead( ent ) ) {
								if( ent->r.client->ps.inventory[POWERUP_SHELL] > 0 ) {
									G_AddEvent( ent, EV_PAIN, PAIN_WARSHELL, true );
								} else if( ent->health <= kPain1UpperInclusiveBound ) {
									G_AddEvent( ent, EV_PAIN, PAIN_1, true );
								} else if( ent->health <= kPain2UpperInclusiveBound ) {
									G_AddEvent( ent, EV_PAIN, PAIN_2, true );
								} else if( ent->health <= kPain3UpperInclusiveBound ) {
									G_AddEvent( ent, EV_PAIN, PAIN_3, true );
								} else {
									G_AddEvent( ent, EV_PAIN, PAIN_4, true );
								}

								ent->pain_debounce_time = level.time + 400;
							}
						}
					}
				} else {
					event = G_SpawnEvent( EV_SPARKS, DirToByte( dir ), origin );
					event->s.damage = HEALTH_TO_INT( damage );
				}
			}
		}
	}
}

/*
* G_StartFrameSnap
* a snap was just sent, set up for new one
*/
static void G_StartFrameSnap( void ) {
	g_snapStarted = true;
}

// backup entitiy sounds in timeout
static int entity_sound_backup[MAX_EDICTS];

/*
* G_ClearSnap
* We just run G_SnapFrame, the server just sent the snap to the clients,
* it's now time to clean up snap specific data to start the next snap from clean.
*/
void G_ClearSnap( void ) {
	edict_t *ent;

	game.realtime = Sys_Milliseconds(); // level.time etc. might not be real time

	// clear gametype's clock override
	ggs->gameState.stats[GAMESTAT_CLOCKOVERRIDE] = 0;

	// clear all events in the snap
	for( ent = &game.edicts[0]; ENTNUM( ent ) < game.numentities; ent++ ) {
		if( ISEVENTENTITY( &ent->s ) ) { // events do not persist after a snapshot
			G_FreeEdict( ent );
			continue;
		}

		// events only last for a single message
		ent->s.events[0] = ent->s.events[1] = 0;
		ent->s.eventParms[0] = ent->s.eventParms[1] = 0;
		ent->numEvents = 0;
		ent->eventPriority[0] = ent->eventPriority[1] = false;
		ent->s.teleported = false; // remove teleported bit.

		// remove effect bits that are (most likely) added from gametypes
		// TODO: Is it really needed? It became an issue during development of homing grenades.
		ent->s.effects = ( ent->s.effects & ( EF_TAKEDAMAGE | EF_CARRIER | EF_FLAG_TRAIL | EF_ROTATE_AND_BOB | EF_STRONG_WEAPON | EF_GHOST | EF_ARMED | EF_ACTIVATED ) );
	}

	// recover some info, let players respawn and finally clear the snap structures
	for( ent = &game.edicts[0]; ENTNUM( ent ) < game.numentities; ent++ ) {
		if( !GS_MatchPaused( *ggs ) ) {
			// copy origin to old origin ( this old_origin is for snaps )
			G_CheckClientRespawnClick( ent );
		}

		if( GS_MatchPaused( *ggs ) ) {
			ent->s.sound = entity_sound_backup[ENTNUM( ent )];
		}

		// clear the snap temp info
		memset( &ent->snap, 0, sizeof( ent->snap ) );
		if( ent->r.client && G_GetClientState( PLAYERNUM( ent ) ) >= CS_SPAWNED ) {
			memset( &ent->r.client->snap, 0, sizeof( ent->r.client->snap ) );

			// set race stats to invisible
			ent->r.client->ps.stats[STAT_TIME_SELF] = STAT_NOTSET;
			ent->r.client->ps.stats[STAT_TIME_BEST] = STAT_NOTSET;
			ent->r.client->ps.stats[STAT_TIME_RECORD] = STAT_NOTSET;
			ent->r.client->ps.stats[STAT_TIME_ALPHA] = STAT_NOTSET;
			ent->r.client->ps.stats[STAT_TIME_BETA] = STAT_NOTSET;
		}
	}

	g_snapStarted = false;
}

/*
* G_SnapFrame
* It's time to send a new snap, so set the world up for sending
*/
void G_SnapFrame( void ) {
	edict_t *ent;
	game.realtime = Sys_Milliseconds(); // level.time etc. might not be real time

	//others
	G_UpdateServerInfo();

	// exit level
	if( level.exitNow ) {
		G_ExitLevel();
		return;
	}

	// finish snap
	G_SnapClients(); // build the playerstate_t structures for all players
	G_SnapEntities(); // add effects based on accumulated info along the frame

	// set entity bits (prepare entities for being sent in the snap)
	for( ent = &game.edicts[0]; ENTNUM( ent ) < game.numentities; ent++ ) {
		if( ent->s.number != ENTNUM( ent ) ) {
			if( developer->integer ) {
				G_Printf( "fixing ent->s.number (etype:%i, classname:%s)\n", ent->s.type, ent->classname ? ent->classname : "noclassname" );
			}
			ent->s.number = ENTNUM( ent );
		}

		// temporary filter (Q2 system to ensure reliability)
		// ignore ents without visible models unless they have an effect
		if( !ent->r.inuse ) {
			ent->r.svflags |= SVF_NOCLIENT;
			continue;
		} else if( ent->s.type >= ET_TOTAL_TYPES || ent->s.type < 0 ) {
			if( developer->integer ) {
				G_Printf( "'G_SnapFrame': Inhibiting invalid entity type %i\n", ent->s.type );
			}
			ent->r.svflags |= SVF_NOCLIENT;
			continue;
		} else if( !( ent->r.svflags & SVF_NOCLIENT ) && !ent->s.modelindex && !ent->s.effects
				   && !ent->s.sound && !ISEVENTENTITY( &ent->s ) && !ent->s.light && !ent->r.client ) {
			if( developer->integer ) {
				G_Printf( "'G_SnapFrame': fixing missing SVF_NOCLIENT flag (no effect)\n" );
			}
			ent->r.svflags |= SVF_NOCLIENT;
			continue;
		}

		ent->s.effects &= ~EF_TAKEDAMAGE;
		if( ent->takedamage ) {
			ent->s.effects |= EF_TAKEDAMAGE;
		}

		if( GS_MatchPaused( *ggs ) ) {
			// when in timeout, we don't send entity sounds
			entity_sound_backup[ENTNUM( ent )] = ent->s.sound;
			ent->s.sound = 0;
		}
	}
}

//===================================================================
//		WORLD FRAMES
//===================================================================

/*
* G_UpdateFrameTime
*/
static void G_UpdateFrameTime( unsigned int msec ) {
	game.frametime = msec;
	G_Timeout_Update( msec );
	game.realtime = Sys_Milliseconds(); // level.time etc. might not be real time
}

/*
* G_RunEntities
* treat each object in turn
* even the world and clients get a chance to think
*/
static void G_RunEntities( void ) {
	edict_t *ent;

	for( ent = &game.edicts[0]; ENTNUM( ent ) < game.numentities; ent++ ) {
		if( !ent->r.inuse ) {
			continue;
		}
		if( ISEVENTENTITY( &ent->s ) ) {
			continue; // events do not think

		}
		level.current_entity = ent;

		// backup oldstate ( for world frame ).
		ent->olds = ent->s;

		// if the ground entity moved, make sure we are still on it
		if( !ent->r.client ) {
			if( ( ent->groundentity ) && ( ent->groundentity->linkcount != ent->groundentity_linkcount ) ) {
				G_CheckGround( ent );
			}
		}

		G_RunEntity( ent );

		if( ent->takedamage ) {
			ent->s.effects |= EF_TAKEDAMAGE;
		} else {
			ent->s.effects &= ~EF_TAKEDAMAGE;
		}
	}
}

/*
* G_RunClients
*/
static void G_RunClients( void ) {
	int i, step;
	edict_t *ent;

	if( level.framenum & 1 ) {
		i = ggs->maxclients - 1;
		step = -1;
	} else {
		i = 0;
		step = 1;
	}

	for( ; i < ggs->maxclients && i >= 0; i += step ) {
		ent = game.edicts + 1 + i;
		if( !ent->r.inuse ) {
			continue;
		}

		G_ClientThink( ent );

		if( ent->takedamage ) {
			ent->s.effects |= EF_TAKEDAMAGE;
		} else {
			ent->s.effects &= ~EF_TAKEDAMAGE;
		}
	}
}

/*
* G_GetNextThinkClient
* Cycles between connected clients
*/
static edict_t *G_GetNextThinkClient( edict_t *current ) {
	edict_t *check, *start;
	edict_t *first, *last;

	first = game.edicts + 1;
	last = game.edicts + ggs->maxclients + 1;
	start = current ? current + 1 : first;

	for( check = start; ; check++ ) {
		if( check > last ) {
			// wrap
			check = first;
		}
		if( check->r.inuse ) {
			return check;
		}
		if( check == start ) {
			break;
		}
	}
	return NULL;
}

#ifndef PUBLIC_BUILD

class CMBenchmark {
	MovingAverage<64, int64_t> m_fastQueue;
	MovingAverage<512, int64_t> m_mediumQueue;
	MovingAverage<2048, int64_t> m_slowQueue;

	CMShapeList *m_baseLists[MAX_EDICTS];
	CMShapeList *m_clippedLists[MAX_EDICTS];
	wsw::StaticVector<const edict_t *, MAX_EDICTS> m_ents;

	static inline const char *kTriggerNames[] { "trigger_teleport", "trigger_push", "info_player_deathmatch" };

	[[nodiscard]]
	static bool isASuitableEntity( const edict_t *ent ) {
		if( ent->item ) {
			return true;
		}
		if( !ent->classname ) {
			return false;
		}
		for( const char *name: kTriggerNames ) {
			if( !Q_stricmp( name, ent->classname ) ) {
				return true;
			}
		}
		return false;
	}

	void runEntityTest( const edict_t *ent ) {
		CMShapeList *const baseList = m_baseLists[ent->s.number];
		CMShapeList *const clippedList = m_clippedLists[ent->s.number];

		const Vec3 listMins( Vec3( -16, -16, -16 ) + ent->r.absmin );
		const Vec3 listMaxs( Vec3( +16, +16, +16 ) + ent->r.absmax );

		SV_BuildShapeList( baseList, listMins.Data(), listMaxs.Data(), MASK_SOLID );
		SV_ClipShapeList( clippedList, baseList, listMins.Data(), listMaxs.Data() );

		wsw::StaticVector<Vec3, 8> vertices;
		const float *bounds[2] { ent->r.absmin, ent->r.absmax };
		for( unsigned i = 0; i < 8; ++i ) {
			float x = bounds[(i >> 2) & 1][0];
			float y = bounds[(i >> 1) & 1][1];
			float z = bounds[(i >> 0) & 1][2];
			vertices.emplace_back( Vec3( x, y, z ) );
		}

		trace_t tr;
		for( unsigned i = 0; i < 8; ++i ) {
			for( unsigned j = 0; j < 8; ++j ) {
				if( i == j ) {
					continue;
				}
				SV_ClipToShapeList( clippedList, &tr, vertices[i].Data(),
												vertices[j].Data(), vec3_origin, vec3_origin, MASK_SOLID );
				//GAME_IMPORT.CM_TransformedBoxTrace( &tr, vertices[i].Data(), vertices[j].Data(),
				// vec3_origin, vec3_origin, nullptr, MASK_SOLID, nullptr, nullptr, 0 );
			}
		}
	}
public:
	CMBenchmark() noexcept {
		const auto *const ents = game.edicts;
		for( int i = ggs->maxclients + 1, end = game.numentities; i < end; ++i ) {
			if( const auto *ent = &ents[i]; isASuitableEntity( ent ) ) {
				m_ents.push_back( ent );
			}
		}
		for( const auto *ent: m_ents ) {
			m_baseLists[ent->s.number] = SV_AllocShapeList();
			m_clippedLists[ent->s.number] = SV_AllocShapeList();
		}
	}

	[[nodiscard]]
	auto run() -> std::tuple<int64_t, int64_t, int64_t> {
		const auto before = std::chrono::high_resolution_clock::now();

		// This produces ~300K trace calls on wca1 + DM that still complete under 16 ms on the workstation
		constexpr auto numOuterIterations = 128;
		// Making this loop outer should make the cache more likely to be washed out so it's more realistic
		for( int i = 0; i < numOuterIterations; ++i ) {
			for( const auto *ent: m_ents ) {
				runEntityTest( ent );
			}
		}

		const auto after = std::chrono::high_resolution_clock::now();
		const auto micros = std::chrono::duration_cast<std::chrono::microseconds>( after - before );

		m_fastQueue.add( micros.count() );
		m_mediumQueue.add( micros.count() );
		m_slowQueue.add( micros.count() );
		return std::make_tuple( m_fastQueue.avg(), m_mediumQueue.avg(), m_slowQueue.avg() );
	}
};

#endif

static void G_RunCMBenchmark() {
#if 0
	static wsw::StaticVector<CMBenchmark, 1> g_benchmarkHolder;
	if( ::g_benchmarkHolder.empty() ) {
		new( ::g_benchmarkHolder.unsafe_grow_back() )CMBenchmark;
	}
	const auto [t1, t2, t3] = ::g_benchmarkHolder.front().run();
	Com_Printf( "%.2f %.2f %.2f\n", (float)t1 * 1e-3f, (float)t2 * 1e-3f, (float)t3 * 1e-3f );
#endif
}

/*
* G_RunFrame
* Advances the world
*/
void G_RunFrame( unsigned int msec, int64_t serverTime ) {
	WSW_PROFILER_SCOPE();

	G_CheckCvars();

	const auto utcTime = std::chrono::system_clock::now().time_since_epoch();
	game.utcTimeMillis = std::chrono::duration_cast<std::chrono::milliseconds>( utcTime ).count();

	const auto matchState = GS_MatchState( *ggs );
	// Set or reset the match start time if needed
	if( matchState <= MATCH_STATE_PLAYTIME ) {
		if( matchState < MATCH_STATE_PLAYTIME ) {
			game.utcMatchStartTime = 0;
		} else if( !game.utcMatchStartTime ) {
			game.utcMatchStartTime = game.utcTimeMillis;
		}
	}

	unsigned int serverTimeDelta = serverTime - game.serverTime;
	game.serverTime = serverTime;
	G_UpdateFrameTime( msec );

	if( !g_snapStarted ) {
		G_StartFrameSnap();
	}

	G_CallVotes_Think();

	ChatHandlersChain::instance()->frame();

	if( GS_MatchPaused( *ggs ) ) {
		// freeze match clock and linear projectiles
		ggs->gameState.stats[GAMESTAT_MATCHSTART] += serverTimeDelta;
		for( edict_t *ent = game.edicts + ggs->maxclients; ENTNUM( ent ) < game.numentities; ent++ ) {
			if( ent->s.linearMovement ) {
				ent->s.linearMovementTimeStamp += serverTimeDelta;
				ent->s.linearMovementPrevServerTime += serverTimeDelta;
			}
		}

		G_RunClients();
		G_RunGametype();
		return;
	}

	// reset warmup clock if not enough players
	if( GS_MatchWaiting( *ggs ) ) {
		ggs->gameState.stats[GAMESTAT_MATCHSTART] = game.serverTime;
	}

	level.framenum++;
	level.time += msec;
	level.think_client_entity = G_GetNextThinkClient( level.think_client_entity );

	G_SpawnQueue_Think();

	// run the world
	G_asCallMapPreThink();
	AI_CommonFrame();
	G_RunClients();
	G_RunEntities();
	G_RunGametype();
	G_RunCMBenchmark();
	G_asCallMapPostThink();
	GClip_BackUpCollisionFrame();
}
