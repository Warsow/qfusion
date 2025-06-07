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
#include "commandshandler.h"

#include <common/helpers/singletonholder.h>
#include "ai/navigation/aasworld.h"

#include <sstream>

using wsw::operator""_asView;
using wsw::operator""_asHView;

/*
* G_Teleport
*
* Teleports client to specified position
* If client is not spectator teleporting is only done if position is free and teleport effects are drawn.
*/
static bool G_Teleport( edict_t *ent, vec3_t origin, vec3_t angles ) {
	int i;

	if( !ent->r.inuse || !ent->r.client ) {
		return false;
	}

	if( ent->r.client->ps.pmove.pm_type != PM_SPECTATOR ) {
		trace_t tr;

		G_Trace( &tr, origin, ent->r.mins, ent->r.maxs, origin, ent, MASK_PLAYERSOLID );
		if( tr.fraction != 1.0f || tr.startsolid ) {
			return false;
		}

		G_TeleportEffect( ent, false );
	}

	VectorCopy( origin, ent->s.origin );
	VectorCopy( origin, ent->olds.origin );
	ent->s.teleported = true;

	VectorClear( ent->velocity );
	ent->r.client->ps.pmove.pm_time = 1;
	ent->r.client->ps.pmove.pm_flags |= PMF_TIME_TELEPORT;

	if( ent->r.client->ps.pmove.pm_type != PM_SPECTATOR ) {
		G_TeleportEffect( ent, true );
	}

	// set angles
	VectorCopy( angles, ent->s.angles );
	VectorCopy( angles, ent->r.client->ps.viewangles );

	// set the delta angle
	for( i = 0; i < 3; i++ )
		ent->r.client->ps.pmove.delta_angles[i] = ANGLE2SHORT( ent->r.client->ps.viewangles[i] ) - ent->r.client->ucmd.angles[i];

	return true;
}


//=================================================================================

/*
* Cmd_Give_f
*
* Give items to a client
*/
static void Cmd_Give_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	const char *name;
	const gsitem_t    *it;
	int i;
	bool give_all;

	if( !sv_cheats->integer ) {
		G_PrintMsg( ent, "Cheats are not enabled on this server.\n" );
		return;
	}

	name = Cmd_Args();

	if( !Q_stricmp( name, "all" ) ) {
		give_all = true;
	} else {
		give_all = false;
	}

	if( give_all || !Q_stricmp( Cmd_Argv( 1 ), "health" ) ) {
		if( Cmd_Argc() == 3 ) {
			ent->health = atoi( Cmd_Argv( 2 ) );
		} else {
			ent->health = ent->max_health;
		}
		if( !give_all ) {
			return;
		}
	}

	if( give_all || !Q_stricmp( name, "weapons" ) ) {
		for( i = 0; i < GS_MAX_ITEM_TAGS; i++ ) {
			it = GS_FindItemByTag( ggs, i );
			if( !it ) {
				continue;
			}

			if( !( it->flags & ITFLAG_PICKABLE ) ) {
				continue;
			}

			if( !( it->type & IT_WEAPON ) ) {
				continue;
			}

			ent->r.client->ps.inventory[i] += 1;
		}
		if( !give_all ) {
			return;
		}
	}

	if( give_all || !Q_stricmp( name, "ammo" ) ) {
		for( i = 0; i < GS_MAX_ITEM_TAGS; i++ ) {
			it = GS_FindItemByTag( ggs, i );
			if( !it ) {
				continue;
			}

			if( !( it->flags & ITFLAG_PICKABLE ) ) {
				continue;
			}

			if( !( it->type & IT_AMMO ) ) {
				continue;
			}

			Add_Ammo( ent->r.client, it, 1000, true );
		}
		if( !give_all ) {
			return;
		}
	}

	if( give_all || !Q_stricmp( name, "armor" ) ) {
		ent->r.client->armor = GS_Armor_MaxCountForTag( ggs, ARMOR_RA );
		if( !give_all ) {
			return;
		}
	}

	if( give_all ) {
		for( i = 0; i < GS_MAX_ITEM_TAGS; i++ ) {
			it = GS_FindItemByTag( ggs, i );
			if( !it ) {
				continue;
			}

			if( !( it->flags & ITFLAG_PICKABLE ) ) {
				continue;
			}

			if( it->type & ( IT_ARMOR | IT_WEAPON | IT_AMMO ) ) {
				continue;
			}

			ent->r.client->ps.inventory[i] = 1;
		}
		return;
	}

	it = GS_FindItemByName( ggs, name );
	if( !it ) {
		name = Cmd_Argv( 1 );
		it = GS_FindItemByName( ggs, name );
		if( !it ) {
			G_PrintMsg( ent, "unknown item\n" );
			return;
		}
	}

	if( !( it->flags & ITFLAG_PICKABLE ) ) {
		G_PrintMsg( ent, "non-pickup (givable) item\n" );
		return;
	}

	if( it->type & IT_AMMO ) {
		if( Cmd_Argc() == 3 ) {
			ent->r.client->ps.inventory[it->tag] = atoi( Cmd_Argv( 2 ) );
		} else {
			ent->r.client->ps.inventory[it->tag] += it->quantity;
		}
	} else {
		if( it->tag && ( it->tag > 0 ) && ( it->tag < GS_MAX_ITEM_TAGS ) ) {
			if( GS_FindItemByTag( ggs, it->tag ) != NULL ) {
				ent->r.client->ps.inventory[it->tag]++;
			}
		} else {
			G_PrintMsg( ent, "non-pickup (givable) item\n" );
		}
	}
}

/*
* Cmd_God_f
* Sets client to godmode
* argv(0) god
*/
static void Cmd_God_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	const char *msg;

	if( !sv_cheats->integer ) {
		G_PrintMsg( ent, "Cheats are not enabled on this server.\n" );
		return;
	}

	ent->flags ^= FL_GODMODE;
	if( !( ent->flags & FL_GODMODE ) ) {
		msg = "godmode OFF\n";
	} else {
		msg = "godmode ON\n";
	}

	G_PrintMsg( ent, "%s", msg );
}

/*
* Cmd_Noclip_f
*
* argv(0) noclip
*/
static void Cmd_Noclip_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	const char *msg;

	if( !sv_cheats->integer ) {
		G_PrintMsg( ent, "Cheats are not enabled on this server.\n" );
		return;
	}

	if( ent->movetype == MOVETYPE_NOCLIP ) {
		ent->movetype = MOVETYPE_PLAYER;
		msg = "noclip OFF\n";
	} else {
		ent->movetype = MOVETYPE_NOCLIP;
		msg = "noclip ON\n";
	}

	G_PrintMsg( ent, "%s", msg );
}

/*
* Cmd_GameOperator_f
*/
static void Cmd_GameOperator_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( !g_operator_password->string[0] ) {
		G_PrintMsg( ent, "Operator is disabled in this server\n" );
		return;
	}

	if( Cmd_Argc() < 2 ) {
		G_PrintMsg( ent, "Usage: 'operator <password>' or 'op <password>'\n" );
		return;
	}

	if( !Q_stricmp( Cmd_Argv( 1 ), g_operator_password->string ) ) {
		if( !ent->r.client->isoperator ) {
			G_PrintMsg( NULL, "%s" S_COLOR_WHITE " is now a game operator\n", ent->r.client->netname.data() );
		}

		ent->r.client->isoperator = true;
		return;
	}

	G_PrintMsg( ent, "Incorrect operator password.\n" );
}

/*
* Cmd_Use_f
* Use an inventory item
*/
static void Cmd_Use_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	const gsitem_t    *it;

	assert( ent && ent->r.client );

	it = GS_Cmd_UseItem( ggs, &ent->r.client->ps, Cmd_Args(), 0 );
	if( !it ) {
		return;
	}

	G_UseItem( ent, it );
}

/*
* Cmd_Kill_f
*/
static void Cmd_Kill_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( ent->r.solid == SOLID_NOT ) {
		return;
	}

	// can suicide after 5 seconds
	if( level.time < ent->r.client->spawnStateTimestamp + (GS_RaceGametype( *ggs ) ? 1000 : 5000 ) ) {
		return;
	}

	ent->flags &= ~FL_GODMODE;
	ent->health = 0;
	meansOfDeath = MOD_SUICIDE;

	// wsw : pb : fix /kill command
	G_Killed( ent, ent, ent, 100000, vec3_origin, MOD_SUICIDE );
}

void Cmd_ChaseNext_f( edict_t *ent, const CmdArgs & ) {
	G_ChaseStep( ent, 1 );
}

void Cmd_ChasePrev_f( edict_t *ent, const CmdArgs & ) {
	G_ChaseStep( ent, -1 );
}

/*
* Cmd_PutAway_f
*/
static void Cmd_PutAway_f( edict_t *ent, const CmdArgs & ) {
	ent->r.client->showscores = false;
}

/*
* Cmd_Score_f
*/
static void Cmd_Score_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	bool newvalue;

	if( Cmd_Argc() == 2 ) {
		newvalue = ( atoi( Cmd_Argv( 1 ) ) != 0 ) ? true : false;
	} else {
		newvalue = !ent->r.client->showscores ? true : false;
	}

	ent->r.client->showscores = newvalue;
}

/*
* Cmd_CvarInfo_f - Contains a cvar name and string provided by the client
*/
static void Cmd_CvarInfo_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		G_PrintMsg( ent, "Cmd_CvarInfo_f: invalid argument count\n" );
		return;
	}

	// see if the gametype script is requesting this info
	if( !GT_asCallGameCommand( ent->r.client, "cvarinfo"_asView, cmdArgs.argsString, cmdArgs.size() - 1 ) ) {
		// if the gametype script wasn't interested in this command, print the output to console
		G_Printf( "%s%s's cvar '%s' is '%s%s'\n", ent->r.client->netname.data(), S_COLOR_WHITE, Cmd_Argv( 1 ), Cmd_Argv( 2 ), S_COLOR_WHITE );
	}
}

static bool CheckStateForPositionCmd( edict_t *ent ) {
	if( sv_cheats->integer ) {
		return true;
	}
	if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP ) {
		return true;
	}
	if ( ent->r.client->ps.pmove.pm_type != PM_SPECTATOR ) {
		return true;
	}
	G_PrintMsg( ent, "Position command is only available in warmup and in spectator mode.\n" );
	return false;
}

class PositionArgSeqMatcher {
protected:
	const CmdArgs &cmdArgs;
	const char *const name;
	vec3_t values { 0, 0, 0 };
	const int numExpectedValues;
	bool hasResult { false };

	float ArgToFloat( int argNum );
	virtual int Parse( int startArgNum );

	template <typename Iterable>
	int MatchByFirst( edict_t *user, int argNum, const char *prefix, const Iterable &matchers );

	virtual const char *Validate() { return nullptr; }
public:
	explicit PositionArgSeqMatcher( const CmdArgs &cmdArgs, const char *name_, int numExpectedValues_ )
		: cmdArgs( cmdArgs ), name( name_ ), numExpectedValues( numExpectedValues_ ) {}

	virtual ~PositionArgSeqMatcher() = default;

	bool CanMatch( const char *argSeqHead ) const {
		return ( Q_stricmp( argSeqHead, name ) ) == 0;
	}

	const char *Name() const { return name; }
	bool HasResult() const { return hasResult; }

	void CopyTo( float *dest ) const {
		if( hasResult ) {
			std::copy_n( values, numExpectedValues, dest );
		} else {
			std::fill_n( dest, numExpectedValues, 0.0f );
		}
	}

	void CopyToIfHasResult( float *dest ) const {
		if( hasResult ) {
			std::copy_n( values, numExpectedValues, dest );
		}
	}

	template <typename Iterable>
	bool MatchAndValidate( edict_t *user, int startArgNum, const char *argSeqPrefix, const Iterable &matchers );
};

struct OriginMatcher : public PositionArgSeqMatcher {
	OriginMatcher( const CmdArgs &cmdArgs ) : PositionArgSeqMatcher( cmdArgs, "origin", 3 ) {}

	const char *Validate() override {
		vec3_t worldMins, worldMaxs;
		const float *playerMins = playerbox_stand_mins;
		const float *playerMaxs = playerbox_stand_maxs;
		SV_InlineModelBounds( SV_InlineModel( 0 ), worldMins, worldMaxs );
		for( int i = 0; i < 3; ++i ) {
			if( values[i] + playerMins[i] <= worldMins[i] || values[i] + playerMaxs[i] >= worldMaxs[i] ) {
				return "The position is outside of world bounds";
			}
		}
		return nullptr;
	}
};

struct AnglesMatcher : public PositionArgSeqMatcher {
	AnglesMatcher( const CmdArgs &cmdArgs ) : PositionArgSeqMatcher( cmdArgs, "angles", 2 ) {}

	const char *Validate() override {
		if( values[PITCH] < -90.0f || values[PITCH] > +90.0f ) {
			return "The roll angle must be within [-90, +90] degrees range";
		}
		if( values[YAW] < 0 || values[YAW] > 360.0f ) {
			return "The yaw angle must be within [-180, +180] degrees range";
		}
		if( values[ROLL] != 0 ) {
			return "The roll angle must be zero";
		}
		return nullptr;
	}
};

struct VelocityMatcher : public PositionArgSeqMatcher {
	VelocityMatcher( const CmdArgs &cmdArgs ) : PositionArgSeqMatcher( cmdArgs, "velocity", 3 ) {}

	const char *Validate() override {
		if( VectorLengthSquared( values ) > 9999 * 9999 ) {
			return "The velocity magnitude is way too large";
		}
		return nullptr;
	}
};

struct SpeedMatcher : public PositionArgSeqMatcher {
	SpeedMatcher( const CmdArgs &cmdArgs ): PositionArgSeqMatcher( cmdArgs, "speed", 1 ) {}

	const char *Validate() override {
		if( std::fabs( values[0] ) > 9999 ) {
			return "The speed is way too large";
		}
		return nullptr;
	}
};

int PositionArgSeqMatcher::Parse( int startArgNum ) {
	assert( startArgNum > 0 && numExpectedValues > 0 );
	if( startArgNum + numExpectedValues > Cmd_Argc() ) {
		return -1;
	}
	for( int i = 0; i < numExpectedValues; ++i ) {
		float val = ArgToFloat( startArgNum + i );
		if( !std::isfinite( val ) ) {
			return -1;
		}
		values[i] = val;
	}
	hasResult = true;
	return numExpectedValues;
}

float PositionArgSeqMatcher::ArgToFloat( int argNum ) {
	const char *arg = Cmd_Argv( argNum );
	if( !arg || !*arg ) {
		return std::numeric_limits<float>::infinity();
	}
	// C/C++ scrubs do not have sane portable locale-independent conversions... just use ::atof()
	double val = ::atof( arg );
	if( !std::isfinite( val ) ) {
		return std::numeric_limits<float>::infinity();
	}
	if( val == 0.0f ) {
		// This is not 100% correct but generally does a good job at making hints.
		// Otherwise keep following "garbage in, garbage out" as expected.
		if( ::strspn( arg, "0." ) != ::strlen( arg ) ) {
			return std::numeric_limits<float>::infinity();
		}
	}
	return (float)val;
}

template <typename Iterable>
bool PositionArgSeqMatcher::MatchAndValidate( edict_t *user, int argNum, const char *prefix, const Iterable &matchers ) {
    const int endArgNum = Cmd_Argc();
    while( argNum < endArgNum ) {
    	int matchedLength = MatchByFirst( user, argNum, prefix, matchers );
    	if( matchedLength < 0 ) {
    		return false;
    	}
    	argNum += matchedLength;
    }

	bool validationSuccess = true;
    for ( auto it = std::begin( matchers ); it != std::end( matchers ); ++it ) {
    	PositionArgSeqMatcher *matcher = *it;
    	if( !matcher->HasResult() ) {
    		continue;
    	}
    	const char *err = matcher->Validate();
    	if( !err ) {
    		continue;
    	}
    	G_PrintMsg( user, "%s\n", err );
    	validationSuccess = false;
    }

    return validationSuccess;
}

template <typename Iterable>
int PositionArgSeqMatcher::MatchByFirst( edict_t *user, int argNum, const char *prefix, const Iterable &matchers ) {
	const char *seqHead = Cmd_Argv( argNum++ );
	size_t prefixOffset = ::strlen( prefix );
	if( Q_strnicmp( seqHead, prefix, prefixOffset ) != 0 ) {
		G_PrintMsg( user, "Unknown arg sequence `%s`\n", seqHead );
		return -1;
	}

	for ( auto it = std::begin( matchers ); it != std::end( matchers ); ++it ) {
		PositionArgSeqMatcher *matcher = *it;
		if( !matcher->CanMatch( seqHead + prefixOffset ) ) {
			continue;
		}
		int numParsedArgs = matcher->Parse( argNum );
		if( numParsedArgs > 0 ) {
			return numParsedArgs + 1;
		}
		G_PrintMsg( user, "Failed to parse `%s`\n", matcher->Name() );
		return -1;
	}

	G_PrintMsg( user, "Unknown arg sequence `%s`\n", seqHead );
	return -1;
}

static void SetOrLoadPosition( edict_t *ent, const char *argSeqPrefix, const char *verb, bool tryLoadingMissing, const CmdArgs &cmdArgs ) {
	vec3_t origin { 0, 0, 0 };
	vec3_t angles { 0, 0, 0 };

	// Fill by saved values if needed
	if( tryLoadingMissing && ent->r.client->position_saved ) {
		VectorCopy( ent->r.client->position_origin, origin );
		VectorCopy( ent->r.client->position_angles, angles );
	}

	OriginMatcher originMatcher( cmdArgs );
	AnglesMatcher anglesMatcher( cmdArgs );
	VelocityMatcher velocityMatcher( cmdArgs );
	SpeedMatcher speedMatcher( cmdArgs );
	PositionArgSeqMatcher *matchers[] = { &originMatcher, &anglesMatcher, &velocityMatcher, &speedMatcher };
	if( !originMatcher.MatchAndValidate( ent, 2, argSeqPrefix, matchers ) ) {
		return;
	}

	if( !originMatcher.HasResult() ) {
		// If we are not allowed to load saved origin
		if( !tryLoadingMissing ) {
			G_PrintMsg( ent, "An origin must always be specified\n" );
			return;
		}
		// If there's nothing saved
		if( !ent->r.client->position_saved ) {
			G_PrintMsg( ent, "A saved or specified origin must always be present\n" );
			return;
		}
	}

	originMatcher.CopyToIfHasResult( origin );
	anglesMatcher.CopyToIfHasResult( angles );

	if( ent->r.client->chase.active ) {
		G_SpectatorMode( ent );
	}

	if( !G_Teleport( ent, origin, angles ) ) {
		G_PrintMsg( ent, "Position not available.\n" );
		return;
	}

	if( !velocityMatcher.HasResult() && !speedMatcher.HasResult() ) {
		G_PrintMsg( ent, "Position %s\n", verb );
		return;
	}

	vec3_t velocity;
	VectorCopy( ent->velocity, velocity );
	velocityMatcher.CopyToIfHasResult( velocity );
	if( speedMatcher.HasResult() ) {
		float newSpeed = 0.0f;
		speedMatcher.CopyTo( &newSpeed );
		const float oldSpeed = VectorLength( velocity );
		if( oldSpeed < 1 ) {
			AngleVectors( angles, velocity, nullptr, nullptr );
			VectorScale( velocity, newSpeed, velocity );
		} else {
			float scale = newSpeed / oldSpeed;
			VectorScale( velocity, scale, velocity );
		}
	}

	VectorCopy( velocity, ent->velocity );
	G_PrintMsg( ent, "Position %s with modified velocity.\n", verb );
}

/*
* Cmd_Position_f
*/
static void Cmd_Position_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	// flood protect
	if( ent->r.client->position_lastcmd + 500 > game.realtime ) {
		return;
	}

	ent->r.client->position_lastcmd = game.realtime;

	const char *action = Cmd_Argv( 1 );

	if( !Q_stricmp( action, "save" ) ) {
		if( !CheckStateForPositionCmd( ent ) ) {
			return;
		}
		ent->r.client->position_saved = true;
		VectorCopy( ent->s.origin, ent->r.client->position_origin );
		VectorCopy( ent->s.angles, ent->r.client->position_angles );
		G_PrintMsg( ent, "Position saved.\n" );
		return;
	}

	if( !Q_stricmp( action, "showSaved" ) ) {
		if( !ent->r.client->position_saved ) {
			G_PrintMsg( ent, "There's no saved position\n" );
			return;
		}
		const float *o = ent->r.client->position_origin;
		const float *a = ent->r.client->position_angles;
		G_PrintMsg( ent, "Saved origin: %.6f %.6f %.6f, angles: %.6f %.6f\n", o[0], o[1], o[2], a[0], a[1] );
		return;
	}

	if( !Q_stricmp( action, "load" ) ) {
		if( CheckStateForPositionCmd( ent ) ) {
			SetOrLoadPosition( ent, "with", "loaded", true, cmdArgs );
		}
		return;
	}

	if( !Q_stricmp( action, "set" ) ) {
		if( CheckStateForPositionCmd( ent ) ) {
			SetOrLoadPosition( ent, "", "set", false, cmdArgs );
		}
		return;
	}

	if( !Q_stricmp( action, "details" ) ) {
		std::stringstream ss;
		ss << va( "Origin: %.6f %.6f %.6f, ", ent->s.origin[0], ent->s.origin[1], ent->s.origin[2] );
		ss << va( "angles: %.6f %.6f\n", ent->s.angles[0], ent->s.angles[1] );
		if( G_ISGHOSTING( ent ) ) {
			G_PrintMsg( ent, "%s\n", ss.str().c_str() );
			return;
		}

		const int topNode = SV_FindTopNodeForBox( ent->r.absmin, ent->r.absmax );
		int tmp, nums[32];
		const int numLeaves = SV_BoxLeafnums( ent->r.absmin, ent->r.absmax, nums, 32, &tmp, topNode );
		ss << "CM top node for box: " << topNode << ", leaves: [";
		for( int i = 0; i < numLeaves; ++i ) {
			ss << nums[i] << ( ( i + 1 != numLeaves ) ? "," : "" );
		}
		ss << "]";

		const auto *aasWorld = AiAasWorld::instance();
		if( !aasWorld->isLoaded() ) {
			G_PrintMsg( ent, "%s\n", ss.str().c_str() );
			return;
		}

		const auto boxAreaNums = aasWorld->findAreasInBox( ent->r.absmin, ent->r.absmax, nums, 32 );
		ss << ", AAS areas for box: [";
		for( size_t i = 0; i < boxAreaNums.size(); ++i ) {
			ss << nums[i] << ( ( i + 1 != boxAreaNums.size() ) ? "," : "" );
		}
		ss << "]";

		G_PrintMsg( ent, "%s", ss.str().c_str() );
		return;
	}

	std::stringstream ss;
	ss << "Usage:\n";
	ss << "position save - Save the current position (origin and angles)\n";
	ss << "position showSaved - Displays an information about the saved position\n";
	ss << "position load - Teleport to a saved position. Saved parameters can be overridden:\n";
	ss << "position load [withOrigin <x> >y> <z>] [withAngles <pitch> <yaw>]";
	   ss << " [withVelocity <vx> <vy> <vz>] [withSpeed <s>]\n";
	ss << "position set - Teleport to a specified position. Parameters should be specified this way:\n";
	ss << "position set origin <x> <y> <z> [angles <pitch> <yaw>] [velocity <vx> <vy> <vz>] [speed <s>]\n";
	ss << "position details - Display a detailed information about the current position\n";

	G_PrintMsg( ent, "%s", ss.str().c_str() );
}

/*
* Cmd_PlayersExt_f
*/
static void Cmd_PlayersExt_f( edict_t *ent, bool onlyspecs, const CmdArgs &cmdArgs ) {
	int i;
	int count = 0;
	int start = 0;
	char line[64];
	char msg[1024];

	if( Cmd_Argc() > 1 ) {
		start = atoi( Cmd_Argv( 1 ) );
	}
	Q_clamp( start, 0, ggs->maxclients - 1 );

	// print information
	msg[0] = 0;

	for( i = start; i < ggs->maxclients; i++ ) {
		if( G_GetClientState( i ) >= CS_SPAWNED ) {
			edict_t *clientEnt = &game.edicts[i + 1];
			Client *cl;

			if( onlyspecs && clientEnt->s.team != TEAM_SPECTATOR ) {
				continue;
			}

			cl = clientEnt->r.client;

			wsw::StringView login;
			if( cl->mm_session.IsValidSessionId() ) {
				login = cl->getInfoValueOrEmpty( wsw::HashedStringView( "cl_mm_login" ) );
			}

			Q_snprintfz( line, sizeof( line ), "%3i %s" S_COLOR_WHITE "%s%s%s%s\n", i, cl->netname.data(),
						 login[0] ? "(" S_COLOR_YELLOW : "", login.data(), login[0] ? S_COLOR_WHITE ")" : "",
						 cl->isoperator ? " op" : "" );

			if( strlen( line ) + strlen( msg ) > sizeof( msg ) - 100 ) {
				// can't print all of them in one packet
				Q_strncatz( msg, "...\n", sizeof( msg ) );
				break;
			}

			if( count == 0 ) {
				Q_strncatz( msg, "num name\n", sizeof( msg ) );
				Q_strncatz( msg, "--- ------------------------------\n", sizeof( msg ) );
			}

			Q_strncatz( msg, line, sizeof( msg ) );
			count++;
		}
	}

	if( count ) {
		Q_strncatz( msg, "--- ------------------------------\n", sizeof( msg ) );
	}
	Q_strncatz( msg, va( "%3i %s\n", count, Cmd_Argv( 0 ) ), sizeof( msg ) );
	G_PrintMsg( ent, "%s", msg );

	if( i < ggs->maxclients ) {
		G_PrintMsg( ent, "Type '%s %i' for more %s\n", Cmd_Argv( 0 ), i, Cmd_Argv( 0 ) );
	}
}

/*
* Cmd_Players_f
*/
static void Cmd_Players_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	Cmd_PlayersExt_f( ent, false, cmdArgs );
}

/*
* Cmd_Spectators_f
*/
static void Cmd_Spectators_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	Cmd_PlayersExt_f( ent, true, cmdArgs );
}

void ChatHandlersChain::frame() {
	if( g_floodprotection_messages->modified ) {
		if( g_floodprotection_messages->integer < 0 ) {
			Cvar_Set( "g_floodprotection_messages", "0" );
		}
		if( g_floodprotection_messages->integer > MAX_FLOOD_MESSAGES ) {
			Cvar_Set( "g_floodprotection_messages", va( "%i", MAX_FLOOD_MESSAGES ) );
		}
		g_floodprotection_messages->modified = false;
	}

	if( g_floodprotection_team->modified ) {
		if( g_floodprotection_team->integer < 0 ) {
			Cvar_Set( "g_floodprotection_team", "0" );
		}
		if( g_floodprotection_team->integer > MAX_FLOOD_MESSAGES ) {
			Cvar_Set( "g_floodprotection_team", va( "%i", MAX_FLOOD_MESSAGES ) );
		}
		g_floodprotection_team->modified = false;
	}

	if( g_floodprotection_seconds->modified ) {
		if( g_floodprotection_seconds->value <= 0 ) {
			Cvar_Set( "g_floodprotection_seconds", "4" );
		}
		g_floodprotection_seconds->modified = false;
	}

	if( g_floodprotection_penalty->modified ) {
		if( g_floodprotection_penalty->value < 0 ) {
			Cvar_Set( "g_floodprotection_penalty", "10" );
		}
		g_floodprotection_penalty->modified = false;
	}

	if( m_respectHandler.m_lastFrameMatchState == MATCH_STATE_PLAYTIME && GS_MatchState( *ggs ) == MATCH_STATE_POSTMATCH ) {
		// Unlock to say `gg` postmatch
		m_muteFilter.reset();
		m_floodFilter.reset();
	}

	m_respectHandler.frame();
}

auto FloodFilter::detectFlood( const edict_t *ent, unsigned flags ) -> std::optional<uint16_t> {
	// TODO: Rewrite so the client do not actually have to maintain its flood state
	auto *const client = ent->r.client;
	const int64_t realTime = game.realtime;

	// old protection still active
	if( !( flags & TeamOnly ) || g_floodprotection_team->integer ) {
		if( const int64_t diff = client->flood_locktill - realTime; diff > 0 ) {
			if( !( flags & DontPrint ) ) {
				G_PrintMsg( ent, "You can't talk for %d more seconds\n", (int)( diff / 1000 ) + 1 );
			}
			return wsw::min( std::numeric_limits<uint16_t>::max(), (uint16_t)diff );
		}
	}

	const int protectionMillis = wsw::max( 1000 * g_floodprotection_seconds->integer, 0 );
	const int penaltySeconds   = wsw::max( g_floodprotection_penalty->integer, 0 );
	const int penaltyMillis    = 1000 * penaltySeconds;

	FloodState *state = nullptr;
	int messages = 0;

	if( flags & TeamOnly ) {
		messages = g_floodprotection_team->integer;
		state = &client->m_floodTeamState;
	} else {
		messages = g_floodprotection_messages->integer;
		state = &client->m_floodState;
	}

	if( messages > 0 && protectionMillis > 0 && penaltyMillis > 0 ) {
		if( state->shouldBeActive( realTime, messages, protectionMillis ) ) {
			client->flood_locktill = realTime + penaltyMillis;
			if( !( flags & DontPrint ) ) {
				G_PrintMsg( ent, "Flood protection: You can't talk for %d seconds.\n", penaltySeconds );
			}
			return penaltyMillis;
		}
	}

	state->touch( realTime );

	return std::nullopt;
}

auto FloodFilter::detectFlood( const ChatMessage &message, unsigned flags ) -> std::optional<uint16_t> {
	return detectFlood( PLAYERENT( message.clientNum ), flags );
}

static void Cmd_CoinToss_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	bool qtails;
	char *s;
	char upper[MAX_STRING_CHARS];

	if( GS_MatchState( *ggs ) > MATCH_STATE_WARMUP && !GS_MatchPaused( *ggs ) ) {
		G_PrintMsg( ent, "You can only toss coins during warmup or timeouts\n" );
		return;
	}
	if( ChatHandlersChain::instance()->detectFlood( ent ) ) {
		return;
	}

	if( Cmd_Argc() < 2 || ( Q_stricmp( "heads", Cmd_Argv( 1 ) ) && Q_stricmp( "tails", Cmd_Argv( 1 ) ) ) ) {
		//it isn't a valid token
		G_PrintMsg( ent, "You have to choose heads or tails when tossing a coin\n" );
		return;
	}

	Q_strncpyz( upper, Cmd_Argv( 1 ), sizeof( upper ) );
	s = upper;
	while( *s ) {
		*s = toupper( *s );
		s++;
	}

	qtails = ( Q_stricmp( "heads", Cmd_Argv( 1 ) ) != 0 ) ? true : false;
	if( qtails == ( rand() & 1 ) ) {
		G_PrintMsg( NULL, S_COLOR_YELLOW "COINTOSS %s: " S_COLOR_WHITE "It was %s! %s " S_COLOR_WHITE "tossed a coin and " S_COLOR_GREEN "won!\n", upper, Cmd_Argv( 1 ), ent->r.client->netname.data() );
		return;
	}

	G_PrintMsg( NULL, S_COLOR_YELLOW "COINTOSS %s: " S_COLOR_WHITE "It was %s! %s " S_COLOR_WHITE "tossed a coin and " S_COLOR_RED "lost!\n", upper, qtails ? "heads" : "tails", ent->r.client->netname.data() );
}

static void sendMessageFault( edict_t *ent, const MessageFault &fault ) {
	wsw::StaticString<64> buffer;
	buffer << "flt "_asView << fault.clientCommandNum;
	buffer << ' ' << (unsigned)fault.kind;
	buffer << ' ' << (unsigned)fault.timeout;
	SV_DispatchGameCmd( ent, buffer.data() );
}

void Cmd_Say_f( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		return;
	}

	wsw::StringView text( Cmd_Args() );
	if( text.startsWith( '"' ) ) {
		if( text.endsWith( '"' ) ) {
			text = text.drop( 1 ).dropRight( 1 );
		}
	}

	text = text.take( MAX_CHAT_BYTES - 1 );

	const ChatMessage message { clientCommandNum, text, (unsigned)PLAYERNUM( ent ) };
	if( const auto maybeFault = ChatHandlersChain::instance()->handleMessage( message ) ) {
		sendMessageFault( ent, *maybeFault );
	}
}

static void Cmd_SayCmd_f( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) {
	Cmd_Say_f( ent, clientCommandNum, cmdArgs );
}

static void Cmd_SayTeam_f( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) {
	auto *const filter = ChatHandlersChain::instance();
	auto maybeFloodProtectionMillis = filter->detectFlood( ent, FloodFilter::TeamOnly | FloodFilter::DontPrint );
	if( !maybeFloodProtectionMillis ) {
		// if speccing, also check for non-team flood
		if( ent->s.team == TEAM_SPECTATOR ) {
			maybeFloodProtectionMillis = filter->detectFlood( ent, FloodFilter::DontPrint );
		}
	}

	if( !maybeFloodProtectionMillis ) {
		G_Say_Team( ent, Cmd_Args() );
	} else {
		sendMessageFault( ent, MessageFault { clientCommandNum, MessageFault::Flood, *maybeFloodProtectionMillis } );
	}
}

/*
* Cmd_Join_f
*/
static void Cmd_Join_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( !ChatHandlersChain::instance()->detectFlood( ent ) ) {
		G_Teams_Join_Cmd( ent, cmdArgs );
	}
}

/*
* Cmd_Timeout_f
*/
static void Cmd_Timeout_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	int num;

	if( ent->s.team == TEAM_SPECTATOR || GS_MatchState( *ggs ) != MATCH_STATE_PLAYTIME ) {
		return;
	}

	if( GS_TeamBasedGametype( *ggs ) ) {
		num = ent->s.team;
	} else {
		num = ENTNUM( ent ) - 1;
	}

	if( GS_MatchPaused( *ggs ) && ( level.timeout.endtime - level.timeout.time ) >= 2 * TIMEIN_TIME ) {
		G_PrintMsg( ent, "Timeout already in progress\n" );
		return;
	}

	if( g_maxtimeouts->integer != -1 && level.timeout.used[num] >= g_maxtimeouts->integer ) {
		if( g_maxtimeouts->integer == 0 ) {
			G_PrintMsg( ent, "Timeouts are not allowed on this server\n" );
		} else if( GS_TeamBasedGametype( *ggs ) ) {
			G_PrintMsg( ent, "Your team doesn't have any timeouts left\n" );
		} else {
			G_PrintMsg( ent, "You don't have any timeouts left\n" );
		}
		return;
	}

	G_PrintMsg( NULL, "%s%s called a timeout\n", ent->r.client->netname.data(), S_COLOR_WHITE );

	if( !GS_MatchPaused( *ggs ) ) {
		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEOUT_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
	}

	level.timeout.used[num]++;
	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_PAUSED, true );
	level.timeout.caller = num;
	level.timeout.endtime = level.timeout.time + TIMEOUT_TIME + FRAMETIME;
}

/*
* Cmd_Timeout_f
*/
static void Cmd_Timein_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	int num;

	if( ent->s.team == TEAM_SPECTATOR ) {
		return;
	}

	if( !GS_MatchPaused( *ggs ) ) {
		G_PrintMsg( ent, "No timeout in progress.\n" );
		return;
	}

	if( level.timeout.endtime - level.timeout.time <= 2 * TIMEIN_TIME ) {
		G_PrintMsg( ent, "The timeout is about to end already.\n" );
		return;
	}

	if( GS_TeamBasedGametype( *ggs ) ) {
		num = ent->s.team;
	} else {
		num = ENTNUM( ent ) - 1;
	}

	if( level.timeout.caller != num ) {
		if( GS_TeamBasedGametype( *ggs ) ) {
			G_PrintMsg( ent, "Your team didn't call this timeout.\n" );
		} else {
			G_PrintMsg( ent, "You didn't call this timeout.\n" );
		}
		return;
	}

	level.timeout.endtime = level.timeout.time + TIMEIN_TIME + FRAMETIME;

	G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEIN_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );

	G_PrintMsg( NULL, "%s%s called a timein\n", ent->r.client->netname.data(), S_COLOR_WHITE );
}

/*
* Cmd_Awards_f
*/
static void Cmd_Awards_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	Client *client;
	static char entry[MAX_TOKEN_CHARS];

	assert( ent && ent->r.client );
	client = ent->r.client;

	Q_snprintfz( entry, sizeof( entry ), "Awards for %s\n", client->netname.data() );

	const auto &awards = client->stats.awardsSequence;
	if( !awards.empty() ) {
		for( const LoggedAward &ga: awards ) {
			Q_strncatz( entry, va( "\t%dx %s\n", ga.count, ga.name.data() ), sizeof( entry ) );
		}
		G_PrintMsg( ent, "%s", entry );
	}
}

/*
* G_StatsMessage
*
* Generates stats message for the entity
* The returned string must be freed by the caller using G_Free
* Note: This string must never contain " characters
*/
char *G_StatsMessage( edict_t *ent ) {
	Client *client;
	const gsitem_t *item;
	int i, shot_weak, hit_weak, shot_strong, hit_strong, shot_total, hit_total;
	static char entry[MAX_TOKEN_CHARS];

	assert( ent && ent->r.client );
	client = ent->r.client;

	// message header
	Q_snprintfz( entry, sizeof( entry ), "%d", PLAYERNUM( ent ) );

	for( i = WEAP_GUNBLADE; i < WEAP_TOTAL; i++ ) {
		item = GS_FindItemByTag( ggs, i );
		assert( item );

		hit_weak = hit_strong = 0;
		shot_weak = shot_strong = 0;

		if( item->weakammo_tag != AMMO_NONE ) {
			hit_weak = client->stats.accuracy_hits[item->weakammo_tag - AMMO_GUNBLADE];
			shot_weak = client->stats.accuracy_shots[item->weakammo_tag - AMMO_GUNBLADE];
		}

		if( item->ammo_tag != AMMO_NONE ) {
			hit_strong = client->stats.accuracy_hits[item->ammo_tag - AMMO_GUNBLADE];
			shot_strong = client->stats.accuracy_shots[item->ammo_tag - AMMO_GUNBLADE];
		}

		hit_total = hit_weak + hit_strong;
		shot_total = shot_weak + shot_strong;

		Q_strncatz( entry, va( " %d", shot_total ), sizeof( entry ) );
		if( shot_total < 1 ) {
			continue;
		}
		Q_strncatz( entry, va( " %d", hit_total ), sizeof( entry ) );

		// strong
		Q_strncatz( entry, va( " %d", shot_strong ), sizeof( entry ) );
		if( shot_strong != shot_total ) {
			Q_strncatz( entry, va( " %d", hit_strong ), sizeof( entry ) );
		}
	}

	Q_strncatz( entry, va( " %d %d", (int)client->stats.GetEntry( "dmg_given" ), (int)client->stats.GetEntry( "dmg_taken" ) ), sizeof( entry ) );
	Q_strncatz( entry, va( " %d %d", (int)client->stats.GetEntry( "health_taken" ), (int)client->stats.GetEntry( "armor_taken" ) ), sizeof( entry ) );

	// add enclosing quote
	Q_strncatz( entry, "\"", sizeof( entry ) );

	return entry;
}

/*
* Cmd_ShowStats_f
*/
static void Cmd_ShowStats_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	edict_t *target;

	if( Cmd_Argc() > 2 ) {
		G_PrintMsg( ent, "Usage: stats [player]\n" );
		return;
	}

	if( Cmd_Argc() == 2 ) {
		target = G_PlayerForText( Cmd_Argv( 1 ) );
		if( target == NULL ) {
			G_PrintMsg( ent, "No such player\n" );
			return;
		}
	} else {
		if( ent->r.client->chase.active && game.edicts[ent->r.client->chase.target].r.client ) {
			target = &game.edicts[ent->r.client->chase.target];
		} else {
			target = ent;
		}
	}

	if( target->s.team == TEAM_SPECTATOR ) {
		G_PrintMsg( ent, "No stats for spectators\n" );
		return;
	}

	SV_DispatchGameCmd( ent, va( "plstats 1 \"%s\"", G_StatsMessage( target ) ) );
}

/*
* Cmd_Whois_f
*/
static void Cmd_Whois_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	edict_t *target;
	Client *cl;

	if( Cmd_Argc() > 2 ) {
		G_PrintMsg( ent, "Usage: whois [player]\n" );
		return;
	}

	if( Cmd_Argc() == 2 ) {
		target = G_PlayerForText( Cmd_Argv( 1 ) );
		if( target == NULL ) {
			G_PrintMsg( ent, "No such player\n" );
			return;
		}
	} else {
		if( ent->r.client->chase.active && game.edicts[ent->r.client->chase.target].r.client ) {
			target = &game.edicts[ent->r.client->chase.target];
		} else {
			target = ent;
		}
	}

	cl = target->r.client;

	if( !cl->mm_session.IsValidSessionId() ) {
		G_PrintMsg( ent, "Unregistered player\n" );
		return;
	}

	const char *login = "unknown";
	if( const auto maybeLoginName = cl->getMMLoginName() ) {
		assert( maybeLoginName->isZeroTerminated() );
		login = maybeLoginName->data();
	}

	G_PrintMsg( ent, "%s%s is %s\n", cl->netname.data(), S_COLOR_WHITE, login );
}

/*
* Cmd_Upstate_f
*
* Update client on the state of things
*/
static void Cmd_Upstate_f( edict_t *ent, const CmdArgs &cmdArgs ) {
	G_UpdatePlayerMatchMsg( ent, true );
	G_SetPlayerHelpMessage( ent, ent->r.client->helpmessage, true );
	SV_DispatchServerCmd( ent, "reloadoptions" );
}

//===========================================================
//	client commands
//===========================================================



static SingletonHolder<ClientCommandsHandler> clientCommandsHandlerHolder;

void ClientCommandsHandler::init() {
	::clientCommandsHandlerHolder.init();
}

void ClientCommandsHandler::shutdown() {
	::clientCommandsHandlerHolder.shutdown();
}

ClientCommandsHandler *ClientCommandsHandler::instance() {
	return ::clientCommandsHandlerHolder.instance();
}

void ClientCommandsHandler::precacheCommands() {
	wsw::PodVector<char> ztName;
	int i = 0;
	for( Callback *callback = m_listHead; callback; callback = callback->nextInList() ) {
		ztName.assign( callback->getName() );
		ztName.append( '\0' );
		SV_SetConfigString( CS_GAMECOMMANDS + i, ztName.data() );
		i++;
	}
	for(; i < MAX_GAMECOMMANDS; ++i ) {
		SV_SetConfigString( CS_GAMECOMMANDS + i, "" );
	}
}

bool ClientCommandsHandler::checkNotWriteProtected( const wsw::StringView &name ) {
	if( name.equalsIgnoreCase( "callvoteValidate"_asView ) || name.equalsIgnoreCase( "callvotePassed"_asView ) ) {
		G_Printf( "WARNING: G_AddCommand: command name '%s' is write protected\n", name.data() );
		return false;
	}
	return true;
}

void ClientCommandsHandler::addBuiltin( const wsw::HashedStringView &name, void (*handler)( edict_t *, const CmdArgs & ) ) {
	// Getting the name from varargs is tricky so it is called explicitly
	if( checkNotWriteProtected( name ) ) {
		addAndNotify( new( m_allocator.allocOrNull() )Builtin1ArgCallback( name, handler ), false );
	}
}

void ClientCommandsHandler::addBuiltin( const wsw::HashedStringView &name,
										void (*handler)( edict_t *, uint64_t, const CmdArgs &cmdArgs ) ) {
	if( checkNotWriteProtected( name ) ) {
		addAndNotify( new( m_allocator.allocOrNull() )Builtin2ArgsCallback( name, handler ), false );
	}
}

void ClientCommandsHandler::addScriptCommand( const wsw::StringView &name ) {
	if( checkNotWriteProtected( name ) ) {
		void *const mem = m_allocator.allocOrNull();
		auto *const callback = new( mem )ScriptCommandCallback( wsw::PodVector<char>( name.data(), name.size() ) );
		addAndNotify( callback, true );
	}
}

ClientCommandsHandler::ClientCommandsHandler() {
	addBuiltin( "cvarinfo"_asHView, Cmd_CvarInfo_f );
	addBuiltin( "position"_asHView, Cmd_Position_f );
	addBuiltin( "players"_asHView, Cmd_Players_f );
	addBuiltin( "spectators"_asHView, Cmd_Spectators_f );
	addBuiltin( "stats"_asHView, Cmd_ShowStats_f );
	addBuiltin( "say"_asHView, Cmd_SayCmd_f );
	addBuiltin( "say_team"_asHView, Cmd_SayTeam_f );
	addBuiltin( "svscore"_asHView, Cmd_Score_f );
	addBuiltin( "god"_asHView, Cmd_God_f );
	addBuiltin( "noclip"_asHView, Cmd_Noclip_f );
	addBuiltin( "use"_asHView, Cmd_Use_f );
	addBuiltin( "give"_asHView, Cmd_Give_f );
	addBuiltin( "kill"_asHView, Cmd_Kill_f );
	addBuiltin( "putaway"_asHView, Cmd_PutAway_f );
	addBuiltin( "chase"_asHView, Cmd_ChaseCam_f );
	addBuiltin( "chasenext"_asHView, Cmd_ChaseNext_f );
	addBuiltin( "chaseprev"_asHView, Cmd_ChasePrev_f );
	addBuiltin( "spec"_asHView, Cmd_Spec_f );
	addBuiltin( "enterqueue"_asHView, G_Teams_JoinChallengersQueue );
	addBuiltin( "leavequeue"_asHView, G_Teams_LeaveChallengersQueue );
	addBuiltin( "camswitch"_asHView, Cmd_SwitchChaseCamMode_f );
	addBuiltin( "timeout"_asHView, Cmd_Timeout_f );
	addBuiltin( "timein"_asHView, Cmd_Timein_f );
	addBuiltin( "cointoss"_asHView, Cmd_CoinToss_f );
	addBuiltin( "whois"_asHView, Cmd_Whois_f );

	// callvotes commands
	addBuiltin( "callvote"_asHView, G_CallVote_Cmd );
	addBuiltin( "vote"_asHView, G_CallVotes_CmdVote );

	addBuiltin( "opcall"_asHView, G_OperatorVote_Cmd );
	addBuiltin( "operator"_asHView, Cmd_GameOperator_f );
	addBuiltin( "op"_asHView, Cmd_GameOperator_f );

	// teams commands
	addBuiltin( "ready"_asHView, G_Match_Ready );
	addBuiltin( "unready"_asHView, G_Match_NotReady );
	addBuiltin( "notready"_asHView, G_Match_NotReady );
	addBuiltin( "toggleready"_asHView, G_Match_ToggleReady );
	addBuiltin( "join"_asHView, Cmd_Join_f );

	// coach commands
	addBuiltin( "coach"_asHView, G_Teams_Coach );
	addBuiltin( "lockteam"_asHView, G_Teams_CoachLockTeam );
	addBuiltin( "unlockteam"_asHView, G_Teams_CoachUnLockTeam );
	addBuiltin( "invite"_asHView, G_Teams_Invite_f );

	// bot commands
	addBuiltin( "botnotarget"_asHView, AI_Cheat_NoTarget );

	addBuiltin( "awards"_asHView, Cmd_Awards_f );

	// ignore-related commands
	addBuiltin( "ignore"_asHView, ChatHandlersChain::handleIgnoreCommand );
	addBuiltin( "unignore"_asHView, ChatHandlersChain::handleUnignoreCommand );
	addBuiltin( "ignorelist"_asHView, ChatHandlersChain::handleIgnoreListCommand );

	// misc
	addBuiltin( "upstate"_asHView, Cmd_Upstate_f );
}

ClientCommandsHandler::~ClientCommandsHandler() {
	const auto dtor = [this]( Callback *cb ) {
		cb->~Callback();
		m_allocator.free( cb );
	};
	removeByTag( kBuiltinTag, dtor );
	removeByTag( kScriptTag, dtor );
}

void ClientCommandsHandler::handleClientCommand( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) {
	// Check whether the client is fully in-game
	if( ent->r.client && G_GetClientState( PLAYERNUM( ent ) ) >= CS_SPAWNED ) {
		const wsw::HashedStringView name( Cmd_Argv( 0 ) );

		// Consider commands as activity. Skip cvarinfo commands as they are automatic responses
		if( !name.equalsIgnoreCase( "cvarinfo"_asHView ) ) {
			G_Client_UpdateActivity( ent->r.client );
		}

		bool callResult = false;
		if( Callback *callback = findByName( name ) ) {
			callResult = callback->operator()( ent, clientCommandNum, cmdArgs );
		}

		if( !callResult ) {
			G_PrintMsg( ent, "Bad user command: %s\n", name.data() );
		}
	}
}

void ClientCommandsHandler::addAndNotify( Callback *newCallback, [[maybe_unused]] bool allowToReplace ) {
	const auto oldSize = m_size;
	Callback *const oldCallback = this->addOrReplace( newCallback );
	if( oldCallback ) {
		assert( allowToReplace );
		assert( oldSize == m_size );
		oldCallback->~Callback();
		m_allocator.free( oldCallback );
	} else {
		if( m_size > MAX_GAMECOMMANDS ) {
			G_Error( "Too many game commands\n" );
		}
		if( level.canSpawnEntities ) {
			wsw::PodVector<char> ztName( newCallback->getName() );
			ztName.append( '\0' );
			// Update the configstring if the precache process was already done
			SV_SetConfigString( CS_GAMECOMMANDS + ( m_size - 1 ), ztName.data() );
		}
	}
}

bool ClientCommandsHandler::ScriptCommandCallback::operator()( edict_t *ent, uint64_t, const CmdArgs &cmdArgs ) {
	return GT_asCallGameCommand( ent->r.client, getName(), cmdArgs.argsString, cmdArgs.size() - 1 );
}

void G_ClientCommand( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) {
	ClientCommandsHandler::instance()->handleClientCommand( ent, clientCommandNum, cmdArgs );
}
