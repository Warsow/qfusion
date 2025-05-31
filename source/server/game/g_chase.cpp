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

/*
* G_Chase_SetChaseActive
*/
static void G_Chase_SetChaseActive( edict_t *ent, bool active ) {
	ent->r.client->chase.active = active;
	G_UpdatePlayerMatchMsg( ent );
}

/*
* G_Chase_IsValidTarget
*/
static bool G_Chase_IsValidTarget( edict_t *ent, edict_t *target, bool teamonly ) {
	if( !ent || !target ) {
		return false;
	}

	if( !target->r.inuse || !target->r.client || G_GetClientState( PLAYERNUM( target ) ) < CS_SPAWNED ) {
		return false;
	}

	if( target->s.team < TEAM_PLAYERS || target->s.team >= GS_MAX_TEAMS || target == ent ) {
		return false;
	}

	if( teamonly && !ent->r.client->is_coach && G_ISGHOSTING( target ) ) {
		return false;
	}

	if( teamonly && target->s.team != ent->s.team ) {
		return false;
	}

	if( G_ISGHOSTING( target ) && !target->deadflag && target->s.team != TEAM_SPECTATOR ) {
		return false; // ghosts that are neither dead, nor speccing (probably originating from gt-specific rules)

	}
	return true;
}

/*
* G_Chase_FindFollowPOV
*/
static int G_Chase_FindFollowPOV( edict_t *ent ) {
	int i, j;
	int quad, warshell, regen, scorelead;
	int maxteam;
	int flags[GS_MAX_TEAMS];
	int newctfpov, newpoweruppov;
	int score_best;
	int newpov = -1;
	edict_t *target;
	static int ctfpov = -1, poweruppov = -1;
	static int64_t flagswitchTime = 0;
	static int64_t pwupswitchTime = 0;
#define CARRIERSWITCHDELAY 8000

	if( !ent->r.client || !ent->r.client->chase.active || !ent->r.client->chase.followmode ) {
		return newpov;
	}

	// follow killer, with a small delay
	if( ( ent->r.client->chase.followmode & 8 ) ) {
		edict_t *targ;

		targ = &game.edicts[ent->r.client->chase.target];
		if( G_IsDead( targ ) && targ->deathTimeStamp + 400 < level.time ) {
			edict_t *attacker = targ->r.client->last_killer;

			// ignore world and team kills
			if( attacker && attacker->r.client && !GS_IsTeamDamage( ggs, &targ->s, &attacker->s ) ) {
				newpov = ENTNUM( attacker );
			}
		}

		return newpov;
	}

	// find what players have what
	score_best = -999999999;
	if( level.gametype.inverseScore ) {
		score_best *= -1;
	}
	quad = warshell = regen = scorelead = -1;
	memset( flags, -1, sizeof( flags ) );
	newctfpov = newpoweruppov = -1;
	maxteam = 0;

	for( i = 1; PLAYERNUM( ( game.edicts + i ) ) < ggs->maxclients; i++ ) {
		target = game.edicts + i;

		if( !target->r.inuse || G_GetClientState( PLAYERNUM( target ) ) < CS_SPAWNED ) {
			// check if old targets are still valid
			if( ctfpov == ENTNUM( target ) ) {
				ctfpov = -1;
			}
			if( poweruppov == ENTNUM( target ) ) {
				poweruppov = -1;
			}
			continue;
		}
		if( target->s.team <= 0 || target->s.team >= (int)( sizeof( flags ) / sizeof( flags[0] ) ) ) {
			continue;
		}
		if( ent->r.client->chase.teamonly && ent->s.team != target->s.team ) {
			continue;
		}

		if( target->s.effects & EF_QUAD ) {
			quad = ENTNUM( target );
		}
		if( target->s.effects & EF_SHELL ) {
			warshell = ENTNUM( target );
		}
		if( target->s.effects & EF_REGEN ) {
			regen = ENTNUM( target );
		}

		if( target->s.team && ( target->s.effects & EF_CARRIER ) ) {
			if( target->s.team > maxteam ) {
				maxteam = target->s.team;
			}
			flags[target->s.team - 1] = ENTNUM( target );
		}

		// find the scoring leader
		if( ( !level.gametype.inverseScore && target->r.client->ps.stats[STAT_SCORE] > score_best )
			|| ( level.gametype.inverseScore && target->r.client->ps.stats[STAT_SCORE] < score_best ) ) {
			score_best = target->r.client->ps.stats[STAT_SCORE];
			scorelead = ENTNUM( target );
		}
	}

	// do some categorization

	for( i = 0; i < maxteam; i++ ) {
		if( flags[i] == -1 ) {
			continue;
		}

		// default new ctfpov to the first flag carrier
		if( newctfpov == -1 ) {
			newctfpov = flags[i];
		} else {
			break;
		}
	}

	// do we have more than one flag carrier?
	if( i < maxteam ) {
		// default to old ctfpov
		if( ctfpov >= 0 ) {
			newctfpov = ctfpov;
		}
		if( ctfpov < 0 || level.time > flagswitchTime ) {
			// alternate between flag carriers
			for( i = 0; i < maxteam; i++ ) {
				if( flags[i] != ctfpov ) {
					continue;
				}

				for( j = 0; j < maxteam - 1; j++ ) {
					if( flags[( i + j + 1 ) % maxteam] != -1 ) {
						newctfpov = flags[( i + j + 1 ) % maxteam];
						break;
					}
				}
				break;
			}
		}

		if( newctfpov != ctfpov ) {
			ctfpov = newctfpov;
			flagswitchTime = level.time + CARRIERSWITCHDELAY;
		}
	} else {
		ctfpov = newctfpov;
		flagswitchTime = 0;
	}

	if( quad != -1 && warshell != -1 && quad != warshell ) {
		// default to old powerup
		if( poweruppov >= 0 ) {
			newpoweruppov = poweruppov;
		}
		if( poweruppov < 0 || level.time > pwupswitchTime ) {
			if( poweruppov == quad ) {
				newpoweruppov = warshell;
			} else if( poweruppov == warshell ) {
				newpoweruppov = quad;
			} else {
				newpoweruppov = ( rand() & 1 ) ? quad : warshell;
			}
		}

		if( poweruppov != newpoweruppov ) {
			poweruppov = newpoweruppov;
			pwupswitchTime = level.time + CARRIERSWITCHDELAY;
		}
	} else {
		if( quad != -1 ) {
			newpoweruppov = quad;
		} else if( warshell != -1 ) {
			newpoweruppov = warshell;
		} else if( regen != -1 ) {
			newpoweruppov = regen;
		}

		poweruppov = newpoweruppov;
		pwupswitchTime = 0;
	}

	// so, we got all, select what we prefer to show
	if( ctfpov != -1 && ( ent->r.client->chase.followmode & 4 ) ) {
		newpov = ctfpov;
	} else if( poweruppov != -1 && ( ent->r.client->chase.followmode & 2 ) ) {
		newpov = poweruppov;
	} else if( scorelead != -1 && ( ent->r.client->chase.followmode & 1 ) ) {
		newpov = scorelead;
	}

	return newpov;
#undef CARRIERSWITCHDELAY
}

/*
* G_EndFrame_UpdateChaseCam
*/
static void G_EndFrame_UpdateChaseCam( edict_t *ent ) {
	edict_t *targ;
	int followpov;

	// not in chasecam
	if( !ent->r.client->chase.active ) {
		return;
	}

	if( ( followpov = G_Chase_FindFollowPOV( ent ) ) != -1 ) {
		ent->r.client->chase.target = followpov;
	}

	// is our chase target gone?
	targ = &game.edicts[ent->r.client->chase.target];

	if( !G_Chase_IsValidTarget( ent, targ, ent->r.client->chase.teamonly ) ) {
		if( game.realtime < ent->r.client->chase.timeout ) { // wait for timeout
			return;
		}

		ent->r.client->chase.timeout = game.realtime + 1500; // update timeout

		G_ChasePlayer( ent, NULL, ent->r.client->chase.teamonly, ent->r.client->chase.followmode );
		targ = &game.edicts[ent->r.client->chase.target];
		if( !G_Chase_IsValidTarget( ent, targ, ent->r.client->chase.teamonly ) ) {
			return;
		}
	}

	ent->r.client->chase.timeout = game.realtime + 1500; // update timeout

	if( targ == ent ) {
		return;
	}

	// free our psev buffer when in chasecam
	G_ClearPlayerStateEvents( ent->r.client );

	// copy target playerState to me
	ent->r.client->ps = targ->r.client->ps;

	// fix some stats we don't want copied from the target
	ent->r.client->ps.stats[STAT_REALTEAM] = ent->s.team;

	ent->r.client->ps.stats[STAT_FLAGS] &= ~STAT_FLAG_CHALLENGER;
	ent->r.client->ps.stats[STAT_FLAGS] &= ~STAT_FLAG_READY;
	ent->r.client->ps.stats[STAT_FLAGS] &= ~STAT_FLAG_OPERATOR;

	if( GS_HasChallengers( *ggs ) && ent->r.client->queueTimeStamp ) {
		ent->r.client->ps.stats[STAT_FLAGS] |= STAT_FLAG_CHALLENGER;
	}

	if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP && level.ready[PLAYERNUM( ent )] ) {
		ent->r.client->ps.stats[STAT_FLAGS] |= STAT_FLAG_READY;
	}

	if( ent->r.client->isoperator ) {
		ent->r.client->ps.stats[STAT_FLAGS] |= STAT_FLAG_OPERATOR;
	}

	// chasecam uses PM_CHASECAM
	ent->r.client->ps.pmove.pm_type = PM_CHASECAM;
	ent->r.client->ps.pmove.pm_flags |= PMF_NO_PREDICTION;

	VectorCopy( targ->s.origin, ent->s.origin );
	VectorCopy( targ->s.angles, ent->s.angles );
	GClip_LinkEntity( ent );
}

/*
* G_EndServerFrames_UpdateChaseCam
*/
void G_EndServerFrames_UpdateChaseCam( void ) {
	int i, team;
	edict_t *ent;

	// do it by teams, so spectators can copy the chasecam information from players
	for( team = TEAM_PLAYERS; team < GS_MAX_TEAMS; team++ ) {
		for( i = 0; i < teamlist[team].numplayers; i++ ) {
			ent = game.edicts + teamlist[team].playerIndices[i];
			if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
				G_Chase_SetChaseActive( ent, false );
				continue;
			}

			G_EndFrame_UpdateChaseCam( ent );
		}
	}

	// Do spectators last
	team = TEAM_SPECTATOR;
	for( i = 0; i < teamlist[team].numplayers; i++ ) {
		ent = game.edicts + teamlist[team].playerIndices[i];
		if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
			G_Chase_SetChaseActive( ent, false );
			continue;
		}

		G_EndFrame_UpdateChaseCam( ent );
	}
}

/*
* G_ChasePlayer
*/
void G_ChasePlayer( edict_t *ent, const char *name, bool teamonly, int followmode ) {
	int i;
	edict_t *e;
	Client *client;
	int targetNum = -1;
	int oldTarget;
	bool can_follow = true;
	char colorlessname[MAX_NAME_BYTES];

	client = ent->r.client;

	oldTarget = client->chase.target;

	if( teamonly && !client->is_coach ) {
		can_follow = false;
	}

	if( !can_follow && followmode ) {
		G_PrintMsg( ent, "Chasecam follow mode unavailable\n" );
		followmode = false;
	}

	if( ent->r.client->chase.followmode && !followmode ) {
		G_PrintMsg( ent, "Disabling chasecam follow mode\n" );
	}

	// always disable chasing as a start
	memset( &client->chase, 0, sizeof( chasecam_t ) );

	// locate the requested target
	if( name && name[0] ) {
		// find it by player names
		for( e = game.edicts + 1; PLAYERNUM( e ) < ggs->maxclients; e++ ) {
			if( !G_Chase_IsValidTarget( ent, e, teamonly ) ) {
				continue;
			}

			Q_strncpyz( colorlessname, COM_RemoveColorTokens( e->r.client->netname.data() ), sizeof( colorlessname ) );

			if( !Q_stricmp( COM_RemoveColorTokens( name ), colorlessname ) ) {
				targetNum = PLAYERNUM( e );
				break;
			}
		}

		// didn't find it by name, try by numbers
		if( targetNum == -1 ) {
			i = atoi( name );
			if( i >= 0 && i < ggs->maxclients ) {
				e = game.edicts + 1 + i;
				if( G_Chase_IsValidTarget( ent, e, teamonly ) ) {
					targetNum = PLAYERNUM( e );
				}
			}
		}

		if( targetNum == -1 ) {
			G_PrintMsg( ent, "Requested chasecam target is not available\n" );
		}
	}

	// try to reuse old target if we didn't find a valid one
	if( targetNum == -1 && oldTarget > 0 && oldTarget < ggs->maxclients ) {
		e = game.edicts + 1 + oldTarget;
		if( G_Chase_IsValidTarget( ent, e, teamonly ) ) {
			targetNum = PLAYERNUM( e );
		}
	}

	// if we still don't have a target, just pick the first valid one
	if( targetNum == -1 ) {
		for( e = game.edicts + 1; PLAYERNUM( e ) < ggs->maxclients; e++ ) {
			if( !G_Chase_IsValidTarget( ent, e, teamonly ) ) {
				continue;
			}

			targetNum = PLAYERNUM( e );
			break;
		}
	}

	// make the client a ghost
	G_GhostClient( ent );
	if( targetNum != -1 ) {
		// we found a target, set up the chasecam
		client->chase.target = targetNum + 1;
		client->chase.teamonly = teamonly;
		client->chase.followmode = followmode;
		G_Chase_SetChaseActive( ent, true );
	} else {
		// stay as observer
		if( !teamonly ) {
			ent->movetype = MOVETYPE_NOCLIP;
		}
		client->showscores = false;
		G_Chase_SetChaseActive( ent, false );
		G_CenterPrintMsg( ent, "No one to chase" );
	}
}

/*
* ChaseStep
*/
void G_ChaseStep( edict_t *ent, int step ) {
	int i, j, team;
	bool player_found;
	int actual;
	int start;
	edict_t *newtarget = NULL;

	assert( step == -1 || step == 0 || step == 1 );

	if( !ent->r.client->chase.active ) {
		return;
	}

	start = ent->r.client->chase.target;
	i = -1;
	player_found = false; // needed to prevent an infinite loop if there are no players
	// find the team of the previously chased player and his index in the sorted teamlist
	for( team = TEAM_PLAYERS; team < GS_MAX_TEAMS; team++ ) {
		for( j = 0; j < teamlist[team].numplayers; j++ ) {
			player_found = true;
			if( teamlist[team].playerIndices[j] == start ) {
				i = j;
				break;
			}
		}
		if( j != teamlist[team].numplayers ) {
			break;
		}
	}

	if( step == 0 ) {
		// keep chasing the current player if possible
		if( i >= 0 && G_Chase_IsValidTarget( ent, game.edicts + start, ent->r.client->chase.teamonly ) ) {
			newtarget = game.edicts + start;
		} else {
			step = 1;
		}
	}

	if( !newtarget && player_found ) {
		// reset the team if the previously chased player was not found
		if( team == GS_MAX_TEAMS ) {
			team = TEAM_PLAYERS;
		}
		for( j = 0; j < ggs->maxclients; j++ ) {
			// at this point step is -1 or 1
			i += step;

			// change to the previous team if we skipped before the start of this one
			// the loop assures empty teams before this team are skipped as well
			while( i < 0 ) {
				team--;
				if( team < TEAM_PLAYERS ) {
					team = GS_MAX_TEAMS - 1;
				}
				i = teamlist[team].numplayers - 1;
			}

			// similarly, change to the next team if we skipped past the end of this one
			while( i >= teamlist[team].numplayers ) {
				team++;
				if( team == GS_MAX_TEAMS ) {
					team = TEAM_PLAYERS;
				}
				i = 0;
			}
			actual = teamlist[team].playerIndices[i];
			if( actual == start ) {
				break; // back at the original player, no need to waste time
			}
			if( G_Chase_IsValidTarget( ent, game.edicts + actual, ent->r.client->chase.teamonly ) ) {
				newtarget = game.edicts + actual;
				break;
			}

			// make another step if this player is not valid
		}
	}

	if( newtarget ) {
		G_ChasePlayer( ent, va( "%i", PLAYERNUM( newtarget ) ), ent->r.client->chase.teamonly, ent->r.client->chase.followmode );
	}
}

/*
* Cmd_ChaseCam_f
*/
void Cmd_ChaseCam_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	bool team_only;
	const char *arg1;

	if( ent->s.team != TEAM_SPECTATOR && !ent->r.client->is_coach ) {
		G_Teams_JoinTeam( ent, TEAM_SPECTATOR );
		// prevent 'joined spectators' spam
		if( !ChatHandlersChain::instance()->detectFlood( ent ) ) {
			G_PrintMsg( NULL, "%s%s joined the %s%s team.\n", ent->r.client->netname.data(),
						S_COLOR_WHITE, GS_TeamName( ggs, ent->s.team ), S_COLOR_WHITE );
		}
	}

	// & 1 = scorelead
	// & 2 = powerups
	// & 4 = objectives
	// & 8 = fragger

	if( ent->r.client->is_coach && GS_TeamBasedGametype( *ggs ) ) {
		team_only = true;
	} else {
		team_only = false;
	}

	arg1 = Cmd_Argv( 1 );

	if( Cmd_Argc() < 2 ) {
		G_ChasePlayer( ent, NULL, team_only, 0 );
	} else if( !Q_stricmp( arg1, "auto" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'auto'. It will follow the score leader when no powerup nor flag is carried.\n" );
		G_ChasePlayer( ent, NULL, team_only, 7 );
	} else if( !Q_stricmp( arg1, "carriers" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'carriers'. It will switch to flag or powerup carriers when any of these items is picked up.\n" );
		G_ChasePlayer( ent, NULL, team_only, 6 );
	} else if( !Q_stricmp( arg1, "powerups" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'powerups'. It will switch to powerup carriers when any of these items is picked up.\n" );
		G_ChasePlayer( ent, NULL, team_only, 2 );
	} else if( !Q_stricmp( arg1, "objectives" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'objectives'. It will switch to objectives carriers when any of these items is picked up.\n" );
		G_ChasePlayer( ent, NULL, team_only, 4 );
	} else if( !Q_stricmp( arg1, "score" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'score'. It will always follow the player with the best score.\n" );
		G_ChasePlayer( ent, NULL, team_only, 1 );
	} else if( !Q_stricmp( arg1, "fragger" ) ) {
		G_PrintMsg( ent, "Chasecam mode is 'fragger'. The last fragging player will be followed.\n" );
		G_ChasePlayer( ent, NULL, team_only, 8 );
	} else if( !Q_stricmp( arg1, "help" ) ) {
		G_PrintMsg( ent, "Chasecam modes:\n" );
		G_PrintMsg( ent, "- 'auto': Chase the score leader unless there's an objective carrier or a powerup carrier.\n" );
		G_PrintMsg( ent, "- 'carriers': User has pov control unless there's an objective carrier or a powerup carrier.\n" );
		G_PrintMsg( ent, "- 'objectives': User has pov control unless there's an objective carrier.\n" );
		G_PrintMsg( ent, "- 'powerups': User has pov control unless there's a flag carrier.\n" );
		G_PrintMsg( ent, "- 'score': Always follow the score leader. User has no pov control.\n" );
		G_PrintMsg( ent, "- 'none': Disable chasecam.\n" );
		return;
	} else {
		G_ChasePlayer( ent, arg1, team_only, 0 );
	}

	G_Teams_LeaveChallengersQueue( ent, cmdArgs );
}

/*
* G_SpectatorMode
*/
void G_SpectatorMode( edict_t *ent ) {
	// join spectator team
	if( ent->s.team != TEAM_SPECTATOR ) {
		G_Teams_JoinTeam( ent, TEAM_SPECTATOR );
		G_PrintMsg( NULL, "%s%s joined the %s%s team.\n", ent->r.client->netname.data(),
					S_COLOR_WHITE, GS_TeamName( ggs, ent->s.team ), S_COLOR_WHITE );
	}

	// was in chasecam
	if( ent->r.client->chase.active ) {
		ent->r.client->showscores = false;
		G_Chase_SetChaseActive( ent, false );

		// reset movement speeds
		ent->r.client->ps.pmove.stats[PM_STAT_MAXSPEED] = GS_DefaultPlayerSpeed( *ggs );
		ent->r.client->ps.pmove.stats[PM_STAT_JUMPSPEED] = DEFAULT_JUMPSPEED;
		ent->r.client->ps.pmove.stats[PM_STAT_DASHSPEED] = DEFAULT_DASHSPEED;
	}

	ent->movetype = MOVETYPE_NOCLIP;
}

/*
* Cmd_Spec_f
*/
void Cmd_Spec_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( ent->s.team == TEAM_SPECTATOR && !ent->r.client->queueTimeStamp ) {
		G_PrintMsg( ent, "You are already a spectator.\n" );
		return;
	}

	G_SpectatorMode( ent );
	G_Teams_LeaveChallengersQueue( ent, cmdArgs );
}

/*
* Cmd_SwitchChaseCamMode_f
*/
void Cmd_SwitchChaseCamMode_f( edict_t *ent, const CmdArgs & ) {
	if( ent->s.team == TEAM_SPECTATOR ) {
		if( ent->r.client->chase.active ) {
			G_SpectatorMode( ent );
		} else {
			G_Chase_SetChaseActive( ent, true );
			G_ChaseStep( ent, 0 );
		}
	}
}
