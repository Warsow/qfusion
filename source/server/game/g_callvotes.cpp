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
#include <common/helpers/base64.h>
#include <common/facilities/configstringstorage.h>
#include <common/facilities/maplist.h>
#include <common/facilities/cvar.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/gs_qrespath.h>
#include <common/helpers/stringsplitter.h>
#include <common/types/staticstring.h>
#include <common/types/vector.h>

using wsw::operator""_asView;

//===================================================================

int clientVoted[MAX_CLIENTS];
int clientVoteChanges[MAX_CLIENTS];

cvar_t *g_callvote_electpercentage;
cvar_t *g_callvote_electtime;          // in seconds
cvar_t *g_callvote_enabled;
cvar_t *g_callvote_maxchanges;
cvar_t *g_callvote_cooldowntime;

enum
{
	VOTED_NOTHING = 0,
	VOTED_YES,
	VOTED_NO
};

// Data that can be used by the vote specific functions
typedef struct
{
	edict_t *caller;
	bool operatorcall;
	struct callvotetype_s *callvote;
	int argc;
	char *argv[MAX_STRING_TOKENS];
	char *string;               // can be used to overwrite the displayed vote string
	void *data;                 // any data vote wants to carry over multiple calls of validate and to execute
} callvotedata_t;

typedef struct callvotetype_s
{
	char *name;
	int expectedargs;               // -1 = any amount, -2 = any amount except 0
	bool ( *validate )( callvotedata_t *data, bool first );
	void ( *execute )( callvotedata_t *vote );
	const char *( *current )( void );
	void ( *extraHelp )( edict_t *ent, const CmdArgs & );
	void ( *describeClientArgs )( int configStringIndex );
	char *argument_format;
	char *help;
	const char *group;
	int registrationNum;
	bool isVotingEnabled;
	bool isOpcallEnabled;
	wsw::StaticString<MAX_STRING_CHARS> lastCurrent;
	struct callvotetype_s *next;
} callvotetype_t;

// Data that will only be used by the common callvote functions
typedef struct
{
	int64_t timeout;           // time to finish
	callvotedata_t vote;
} callvotestate_t;

static callvotestate_t callvoteState;

static callvotetype_t *callvotesHeadNode = NULL;

static int registrationNum = 0;

/*
* shuffle/rebalance
*/
typedef struct
{
	int ent;
	int weight;
} weighted_player_t;

static int G_VoteCompareWeightedPlayers( const void *a, const void *b ) {
	const weighted_player_t *pa = ( const weighted_player_t * )a;
	const weighted_player_t *pb = ( const weighted_player_t * )b;
	return pb->weight - pa->weight;
}

static void G_DescribeBooleanArg( int configStringIndex ) {
	SV_SetConfigString( configStringIndex, "boolean" );
}

static void G_DescribeNumberArg( int configStringIndex ) {
	SV_SetConfigString( configStringIndex, "number" );
}

static void G_DescribePlayerArg( int configStringIndex ) {
	SV_SetConfigString( configStringIndex, "player" );
}

static void G_DescribeMinutesArg( int configStringIndex ) {
	SV_SetConfigString( configStringIndex, "minutes" );
}

/*
* map
*/

const wsw::CharLookup kMapListSeparators( ", "_asView );

static void G_VoteMapExtraHelp( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( g_enforce_map_pool->integer && strlen( g_map_pool->string ) > 2 ) {
		G_PrintMsg( ent, "Maps available [map pool enforced]:\n %s\n", g_map_pool->string );
		return;
	}

	wsw::StaticString<3 * MAX_STRING_CHARS / 4> message;
	// update the maplist
	ML_Update();
	const auto numMaps = (int)ML_GetListSize();

	message << "- Available maps:"_asView;

	const int start = wsw::max( 0, atoi( Cmd_Argv( 2 ) - 1 ) );

	int i = start;
	while( auto maybeNames = ML_GetMapByNum( i ) ) {
		auto fileName = maybeNames->fileName;
		i++;

		if( message.size() + fileName.length() + 3 >= message.capacity() ) {
			break;
		}

		message << ' ' << fileName;
	}

	if( i == start ) {
		message << "\nNone"_asView;
	}

	G_PrintMsg( ent, "%s\n", message.data() );
	if( i < numMaps ) {
		G_PrintMsg( ent, "Type 'callvote map %i' for more maps\n", i + 1 );
	}
}

class StringListEncoder {
	wsw::PodVector<char> m_rawStrings;
	wsw::PodVector<uint8_t> m_zipBuffer;
	wsw::PodVector<char> m_base64;
public:
	void reserve( size_t size ) {
		m_rawStrings.reserve( size );
	}

	void add( const wsw::StringView &string ) {
		if( !m_rawStrings.empty() ) {
			m_rawStrings.push_back( ' ' );
		}
		m_rawStrings.append( string.data(), string.size() );
	}

	auto encode() -> const wsw::PodVector<char> & {
		m_zipBuffer.resize( 1u << 15u );
		size_t compressedSize = m_zipBuffer.size();
		if( !SV_CompressBytes( m_zipBuffer.data(), &compressedSize, m_rawStrings.data(), m_rawStrings.size() ) ) {
			G_Error( "Failed to compress the map list raw strings buffer (size=%d)", (int)m_rawStrings.size() );
		}
		size_t base64Size;
		auto *rawBase64 = base64_encode( m_zipBuffer.data(), compressedSize, &base64Size );
		if( !rawBase64 ) {
			G_Error( "Failed to base64-encode the compressed map list data" );
		}
		m_base64.assign( (const char *)rawBase64, base64Size );
		free( rawBase64 );
		return m_base64;
	}
};

static void addMapListToEncode( StringListEncoder &encoder ) {
	ML_Update();

	if( g_enforce_map_pool->integer ) {
		if( const auto len = std::strlen( g_enforce_map_pool->string ); len > 2 ) {
			encoder.reserve( len );
			wsw::StringSplitter splitter( wsw::StringView( g_enforce_map_pool->string, len ) );
			while( auto maybeToken = splitter.getNext( kMapListSeparators ) ) {
				if( maybeToken->length() >= MAX_QPATH ) {
					continue;
				}
				// A token is very likely not zero-terminated...
				wsw::StaticString<MAX_QPATH> buffer( *maybeToken );
				if( !ML_FilenameExists( buffer.data() ) ) {
					continue;
				}
				encoder.add( *maybeToken );
			}
			return;
		}
	}

	// Assume that an average map name is of this size in this case.
	// This is a quite realistic estimation for race maps
	encoder.reserve( ( MAX_QPATH / 3 ) * ML_GetListSize() );

	int num = 0;
	while( auto maybeMapNames = ML_GetMapByNum( num++ ) ) {
		encoder.add( maybeMapNames->fileName );
	}
}

static void G_VoteMapDescribeClientArgs( int configStringIndex ) {
	// TODO: Allow specifying string prefix to reduce the redundant copying?
	StringListEncoder encoder;
	addMapListToEncode( encoder );

	const wsw::PodVector<char> &base64 = encoder.encode();
	wsw::PodVector<char> buffer;
	buffer.reserve( base64.size() + 16 );
	buffer.append( wsw::StringView( "maplist " ) );
	buffer.append( base64 );
	buffer.append( '\0' );

	SV_SetConfigString( configStringIndex, buffer.data() );
}

static bool G_VoteMapValidate( callvotedata_t *data, bool first ) {
	char mapname[MAX_QPATH];

	if( !first ) { // map can't become invalid while voting
		return true;
	}
	if( Q_isdigit( data->argv[0] ) ) { // FIXME
		return false;
	}

	if( strlen( "maps/" ) + strlen( data->argv[0] ) + strlen( ".bsp" ) >= MAX_QPATH ) {
		G_PrintMsg( data->caller, "%sToo long map name\n", S_COLOR_RED );
		return false;
	}

	Q_strncpyz( mapname, data->argv[0], sizeof( mapname ) );
	COM_SanitizeFilePath( mapname );

	if( !Q_stricmp( level.mapname, mapname ) ) {
		G_PrintMsg( data->caller, "%sYou are already on that map\n", S_COLOR_RED );
		return false;
	}

	if( !COM_ValidateRelativeFilename( mapname ) || strchr( mapname, '/' ) || strchr( mapname, '.' ) ) {
		G_PrintMsg( data->caller, "%sInvalid map name\n", S_COLOR_RED );
		return false;
	}

	if( ML_FilenameExists( mapname ) ) {
		char msg[MAX_STRING_CHARS];
		char fullname[MAX_TOKEN_CHARS];

		Q_strncpyz( fullname, COM_RemoveColorTokens( ML_GetFullname( mapname ) ), sizeof( fullname ) );
		if( !Q_stricmp( mapname, fullname ) ) {
			fullname[0] = '\0';
		}

		// check if valid map is in map pool when on
		if( g_enforce_map_pool->integer ) {
			// if map pool is empty, basically turn it off
			if( strlen( g_map_pool->string ) < 2 ) {
				return true;
			}

			const wsw::StringView mapNameView( mapname );
			wsw::StringSplitter splitter( wsw::StringView( g_map_pool->string ) );
			while( const auto maybeToken = splitter.getNext( kMapListSeparators ) ) {
				if( maybeToken->equalsIgnoreCase( mapNameView ) ) {
					goto valid_map;
				}
			}

			G_PrintMsg( data->caller, "%sMap is not in map pool.\n", S_COLOR_RED );
			return false;
		}

valid_map:
		if( fullname[0] != '\0' ) {
			Q_snprintfz( msg, sizeof( msg ), "%s (%s)", mapname, fullname );
		} else {
			Q_strncpyz( msg, mapname, sizeof( msg ) );
		}

		if( data->string ) {
			Q_free( data->string );
		}
		data->string = Q_strdup( msg );
		return true;
	}

	G_PrintMsg( data->caller, "%sNo such map available on this server\n", S_COLOR_RED );

	return false;
}

static void G_VoteMapPassed( callvotedata_t *vote ) {
	Q_strncpyz( level.forcemap, Q_strlwr( vote->argv[0] ), sizeof( level.forcemap ) );
	G_EndMatch();
}

static const char *G_VoteMapCurrent( void ) {
	return level.mapname;
}

/*
* restart
*/

static void G_VoteRestartPassed( callvotedata_t *vote ) {
	G_RestartLevel();
}


/*
* nextmap
*/

static void G_VoteNextMapPassed( callvotedata_t *vote ) {
	level.forcemap[0] = 0;
	G_EndMatch();
}


/*
* scorelimit
*/

static bool G_VoteScorelimitValidate( callvotedata_t *vote, bool first ) {
	int scorelimit = atoi( vote->argv[0] );

	if( scorelimit < 0 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't set negative scorelimit\n", S_COLOR_RED );
		}
		return false;
	}

	if( scorelimit == g_scorelimit->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sScorelimit is already set to %i\n", S_COLOR_RED, scorelimit );
		}
		return false;
	}

	return true;
}

static void G_VoteScorelimitPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_scorelimit", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteScorelimitCurrent( void ) {
	return va( "%i", g_scorelimit->integer );
}

/*
* timelimit
*/

static bool G_VoteTimelimitValidate( callvotedata_t *vote, bool first ) {
	int timelimit = atoi( vote->argv[0] );

	if( timelimit < 0 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't set negative timelimit\n", S_COLOR_RED );
		}
		return false;
	}

	if( timelimit == g_timelimit->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sTimelimit is already set to %i\n", S_COLOR_RED, timelimit );
		}
		return false;
	}

	return true;
}

static void G_VoteTimelimitPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_timelimit", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteTimelimitCurrent( void ) {
	return va( "%i", g_timelimit->integer );
}


/*
* gametype
*/

static void G_VoteGametypeExtraHelp( edict_t *ent, const CmdArgs &cmdArgs ) {
	wsw::StaticString<MAX_STRING_CHARS> message;

	if( const auto *latched = g_gametype->latched_string ) {
		if( latched && G_Gametype_Exists( latched ) ) {
			message << "- Will be changed to: "_asView << wsw::StringView( latched ) << '\n';
		}
	}

	message << "- Available gametypes:"_asView;

	wsw::StringSplitter splitter( wsw::StringView( g_gametypes_list->string ) );
	while( const auto maybeName = splitter.getNext( CHAR_GAMETYPE_SEPARATOR ) ) {
		const auto name = *maybeName;
		if( G_Gametype_IsVotable( name ) ) {
			if( name.size() + 1 >= message.capacity() ) {
				break;
			}
			message << ' ' << name;
		}
	}

	G_PrintMsg( ent, "%s\n", message.data() );
}

static void G_VoteGametypeDescribeClientArgs( int configStringIndex ) {
	StringListEncoder encoder;
	wsw::StringSplitter splitter( wsw::StringView( g_gametypes_list->string ) );
	while( const auto maybeName = splitter.getNext( CHAR_GAMETYPE_SEPARATOR ) ) {
		if( G_Gametype_IsVotable( *maybeName ) ) {
			encoder.add( *maybeName );
		}
	}

	wsw::PodVector<char> configString;
	configString.append( wsw::StringView( "options " ) );
	const wsw::PodVector<char> &encodedOptions = encoder.encode();
	configString.append( encodedOptions.data(), encodedOptions.size() );
	configString.append( '\0' );
	SV_SetConfigString( configStringIndex, configString.data() );
}

static bool G_VoteGametypeValidate( callvotedata_t *vote, bool first ) {
	if( !G_Gametype_Exists( vote->argv[0] ) ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sgametype %s is not available\n", S_COLOR_RED, vote->argv[0] );
		}
		return false;
	}

	if( g_gametype->latched_string && G_Gametype_Exists( g_gametype->latched_string ) ) {
		if( ( GS_MatchState( *ggs ) > MATCH_STATE_PLAYTIME ) &&
			!Q_stricmp( vote->argv[0], g_gametype->latched_string ) ) {
			if( first ) {
				G_PrintMsg( vote->caller, "%s%s is already the next gametype\n", S_COLOR_RED, vote->argv[0] );
			}
			return false;
		}
	}

	if( ( GS_MatchState( *ggs ) <= MATCH_STATE_PLAYTIME || g_gametype->latched_string == NULL )
		&& !Q_stricmp( ggs->gametypeName, vote->argv[0] ) ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%s%s is the current gametype\n", S_COLOR_RED, vote->argv[0] );
		}
		return false;
	}

	// if the g_votable_gametypes is empty, allow all gametypes
	if( !G_Gametype_IsVotable( wsw::StringView( vote->argv[0] ) ) ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sVoting gametype %s is not allowed on this server\n",
						S_COLOR_RED, vote->argv[0] );
		}
		return false;
	}

	return true;
}

static void G_VoteGametypePassed( callvotedata_t *vote ) {
	char *gametype_string;
	char next_gametype_string[MAX_STRING_TOKENS];

	gametype_string = vote->argv[0];
	Q_strncpyz( next_gametype_string, gametype_string, sizeof( next_gametype_string ) );

	Cvar_Set( "g_gametype", gametype_string );

	if( GS_MatchState( *ggs ) == MATCH_STATE_COUNTDOWN || GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME || !G_RespawnLevel() ) {
		// go to scoreboard if in game
		Q_strncpyz( level.forcemap, level.mapname, sizeof( level.forcemap ) );
		G_EndMatch();
	}

	// we can't use gametype_string here, because there's a big chance it has just been freed after G_EndMatch
	G_PrintMsg( NULL, "Gametype changed to %s\n", next_gametype_string );
}

static const char *G_VoteGametypeCurrent( void ) {
	return ggs->gametypeName;
}


/*
* warmup_timelimit
*/

static bool G_VoteWarmupTimelimitValidate( callvotedata_t *vote, bool first ) {
	int warmup_timelimit = atoi( vote->argv[0] );

	if( warmup_timelimit < 0 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't set negative warmup timelimit\n", S_COLOR_RED );
		}
		return false;
	}

	if( warmup_timelimit == g_warmup_timelimit->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sWarmup timelimit is already set to %i\n", S_COLOR_RED, warmup_timelimit );
		}
		return false;
	}

	return true;
}

static void G_VoteWarmupTimelimitPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_warmup_timelimit", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteWarmupTimelimitCurrent( void ) {
	return va( "%i", g_warmup_timelimit->integer );
}


/*
* extended_time
*/

static bool G_VoteExtendedTimeValidate( callvotedata_t *vote, bool first ) {
	int extended_time = atoi( vote->argv[0] );

	if( extended_time < 0 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't set negative extended time\n", S_COLOR_RED );
		}
		return false;
	}

	if( extended_time == g_match_extendedtime->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sExtended time is already set to %i\n", S_COLOR_RED, extended_time );
		}
		return false;
	}

	return true;
}

static void G_VoteExtendedTimePassed( callvotedata_t *vote ) {
	Cvar_Set( "g_match_extendedtime", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteExtendedTimeCurrent( void ) {
	return va( "%i", g_match_extendedtime->integer );
}

/*
* allready
*/

static bool G_VoteAllreadyValidate( callvotedata_t *vote, bool first ) {
	int notreadys = 0;
	edict_t *ent;

	if( GS_MatchState( *ggs ) >= MATCH_STATE_COUNTDOWN ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sThe game is not in warmup mode\n", S_COLOR_RED );
		}
		return false;
	}

	for( ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
		if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
			continue;
		}

		if( ent->s.team > TEAM_SPECTATOR && !level.ready[PLAYERNUM( ent )] ) {
			notreadys++;
		}
	}

	if( !notreadys ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sEveryone is already ready\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteAllreadyPassed( callvotedata_t *vote ) {
	edict_t *ent;

	for( ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
		if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
			continue;
		}

		if( ent->s.team > TEAM_SPECTATOR && !level.ready[PLAYERNUM( ent )] ) {
			level.ready[PLAYERNUM( ent )] = true;
			G_UpdatePlayerMatchMsg( ent );
			G_Match_CheckReadys();
		}
	}
}

/*
* maxteamplayers
*/

static bool G_VoteMaxTeamplayersValidate( callvotedata_t *vote, bool first ) {
	int maxteamplayers = atoi( vote->argv[0] );

	if( maxteamplayers < 1 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sThe maximum number of players in team can't be less than 1\n",
						S_COLOR_RED );
		}
		return false;
	}

	if( g_teams_maxplayers->integer == maxteamplayers ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sMaximum number of players in team is already %i\n",
						S_COLOR_RED, maxteamplayers );
		}
		return false;
	}

	return true;
}

static void G_VoteMaxTeamplayersPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_teams_maxplayers", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteMaxTeamplayersCurrent( void ) {
	return va( "%i", g_teams_maxplayers->integer );
}

/*
* lock
*/

static bool G_VoteLockValidate( callvotedata_t *vote, bool first ) {
	if( GS_MatchState( *ggs ) > MATCH_STATE_PLAYTIME ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't lock teams after the match\n", S_COLOR_RED );
		}
		return false;
	}

	if( level.teamlock ) {
		if( GS_MatchState( *ggs ) < MATCH_STATE_COUNTDOWN && first ) {
			G_PrintMsg( vote->caller, "%sTeams are already set to be locked on match start\n", S_COLOR_RED );
		} else if( first ) {
			G_PrintMsg( vote->caller, "%sTeams are already locked\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteLockPassed( callvotedata_t *vote ) {
	int team;

	level.teamlock = true;

	// if we are inside a match, update the teams state
	if( GS_MatchState( *ggs ) >= MATCH_STATE_COUNTDOWN && GS_MatchState( *ggs ) <= MATCH_STATE_PLAYTIME ) {
		if( GS_TeamBasedGametype( *ggs ) ) {
			for( team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ )
				G_Teams_LockTeam( team );
		} else {
			G_Teams_LockTeam( TEAM_PLAYERS );
		}
		G_PrintMsg( NULL, "Teams locked\n" );
	} else {
		G_PrintMsg( NULL, "Teams will be locked when the match starts\n" );
	}
}

/*
* unlock
*/

static bool G_VoteUnlockValidate( callvotedata_t *vote, bool first ) {
	if( GS_MatchState( *ggs ) > MATCH_STATE_PLAYTIME ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sCan't unlock teams after the match\n", S_COLOR_RED );
		}
		return false;
	}

	if( !level.teamlock ) {
		if( GS_MatchState( *ggs ) < MATCH_STATE_COUNTDOWN && first ) {
			G_PrintMsg( vote->caller, "%sTeams are not set to be locked\n", S_COLOR_RED );
		} else if( first ) {
			G_PrintMsg( vote->caller, "%sTeams are not locked\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteUnlockPassed( callvotedata_t *vote ) {
	int team;

	level.teamlock = false;

	// if we are inside a match, update the teams state
	if( GS_MatchState( *ggs ) >= MATCH_STATE_COUNTDOWN && GS_MatchState( *ggs ) <= MATCH_STATE_PLAYTIME ) {
		if( GS_TeamBasedGametype( *ggs ) ) {
			for( team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ )
				G_Teams_UnLockTeam( team );
		} else {
			G_Teams_UnLockTeam( TEAM_PLAYERS );
		}
		G_PrintMsg( NULL, "Teams unlocked\n" );
	} else {
		G_PrintMsg( NULL, "Teams will no longer be locked when the match starts\n" );
	}
}

/*
* remove
*/

static void G_VoteRemoveExtraHelp( edict_t *ent, const CmdArgs &cmdArgs ) {
	int i;
	edict_t *e;
	char msg[1024];

	msg[0] = 0;
	Q_strncatz( msg, "- List of players in game:\n", sizeof( msg ) );

	if( GS_TeamBasedGametype( *ggs ) ) {
		int team;

		for( team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ ) {
			Q_strncatz( msg, va( "%s:\n", GS_TeamName( ggs, team ) ), sizeof( msg ) );
			for( i = 0, e = game.edicts + 1; i < ggs->maxclients; i++, e++ ) {
				if( !e->r.inuse || e->s.team != team ) {
					continue;
				}

				Q_strncatz( msg, va( "%3i: %s\n", PLAYERNUM( e ), e->r.client->netname.data() ), sizeof( msg ) );
			}
		}
	} else {
		for( i = 0, e = game.edicts + 1; i < ggs->maxclients; i++, e++ ) {
			if( !e->r.inuse || e->s.team != TEAM_PLAYERS ) {
				continue;
			}

			Q_strncatz( msg, va( "%3i: %s\n", PLAYERNUM( e ), e->r.client->netname.data() ), sizeof( msg ) );
		}
	}

	G_PrintMsg( ent, "%s", msg );
}

static bool G_VoteRemoveValidate( callvotedata_t *vote, bool first ) {
	int who = -1;

	if( first ) {
		edict_t *tokick = G_PlayerForText( vote->argv[0] );

		if( tokick ) {
			who = PLAYERNUM( tokick );
		} else {
			who = -1;
		}

		if( who == -1 ) {
			G_PrintMsg( vote->caller, "%sNo such player\n", S_COLOR_RED );
			return false;
		} else if( tokick->s.team == TEAM_SPECTATOR ) {
			G_PrintMsg( vote->caller, "Player %s%s%s is already spectator.\n", S_COLOR_WHITE,
						tokick->r.client->netname.data(), S_COLOR_RED );

			return false;
		} else {
			// we save the player id to be removed, so we don't later get confused by new ids or players changing names
			vote->data = Q_malloc( sizeof( int ) );
			memcpy( vote->data, &who, sizeof( int ) );
		}
	} else {
		memcpy( &who, vote->data, sizeof( int ) );
	}

	if( !game.edicts[who + 1].r.inuse || game.edicts[who + 1].s.team == TEAM_SPECTATOR ) {
		return false;
	} else {
		if( !vote->string || Q_stricmp( vote->string, game.edicts[who + 1].r.client->netname.data() ) ) {
			if( vote->string ) {
				Q_free( vote->string );
			}
			vote->string = Q_strdup( game.edicts[who + 1].r.client->netname.data() );
		}

		return true;
	}
}

static edict_t *G_Vote_GetValidDeferredVoteTarget( callvotedata_t *vote ) {
	int who;
	memcpy( &who, vote->data, sizeof( int ) );

	edict_t *ent = game.edicts + who + 1;
	if( !ent->r.inuse || !ent->r.client ) {
		return nullptr;
	}

	return ent;
}

static void G_VoteRemovePassed( callvotedata_t *vote ) {
	edict_t *ent = G_Vote_GetValidDeferredVoteTarget( vote );

	// may have disconnect along the callvote time
	if( !ent || ent->s.team == TEAM_SPECTATOR ) {
		return;
	}

	G_PrintMsg( NULL, "Player %s%s removed from team %s%s.\n", ent->r.client->netname.data(), S_COLOR_WHITE,
				GS_TeamName( ggs, ent->s.team ), S_COLOR_WHITE );

	G_Teams_SetTeam( ent, TEAM_SPECTATOR );
	ent->r.client->queueTimeStamp = 0;
}


/*
* kick
*/

static void G_VoteHelp_ShowPlayersList( edict_t *ent, const CmdArgs & ) {
	int i;
	edict_t *e;
	char msg[1024];

	msg[0] = 0;
	Q_strncatz( msg, "- List of current players:\n", sizeof( msg ) );

	for( i = 0, e = game.edicts + 1; i < ggs->maxclients; i++, e++ ) {
		if( !e->r.inuse ) {
			continue;
		}

		Q_strncatz( msg, va( "%2d: %s\n", PLAYERNUM( e ), e->r.client->netname.data() ), sizeof( msg ) );
	}

	G_PrintMsg( ent, "%s", msg );
}

static bool G_SetOrValidateKickLikeCmdTarget( callvotedata_t *vote, bool first ) {
	int who = -1;
	if( first ) {
		edict_t *tokick = G_PlayerForText( vote->argv[0] );
		if( tokick ) {
			who = PLAYERNUM( tokick );
		} else {
			who = -1;
		}

		if( who != -1 ) {
			if( game.edicts[who + 1].r.client->isoperator ) {
				G_PrintMsg( vote->caller, S_COLOR_RED "%s is a game operator.\n", game.edicts[who + 1].r.client->netname.data() );
				return false;
			}

			// we save the player id to be kicked, so we don't later get
			//confused by new ids or players changing names

			vote->data = Q_malloc( sizeof( int ) );
			memcpy( vote->data, &who, sizeof( int ) );
		} else {
			G_PrintMsg( vote->caller, S_COLOR_RED "%s: No such player\n", vote->argv[0] );
			return false;
		}
	} else {
		memcpy( &who, vote->data, sizeof( int ) );
	}

	edict_t *ent = game.edicts + who + 1;
	if( ent->r.inuse && ent->r.client ) {
		if( !vote->string || Q_stricmp( vote->string, ent->r.client->netname.data() ) ) {
			if( vote->string ) {
				Q_free( vote->string );
			}
			vote->string = Q_strdup( ent->r.client->netname.data() );
		}
		return true;
	}

	return false;
}

static bool G_VoteKickValidate( callvotedata_t *vote, bool first ) {
	return G_SetOrValidateKickLikeCmdTarget( vote, first );
}

const char *G_GetClientHostForFilter( const edict_t *ent ) {
	if( ent->r.svflags & SVF_FAKECLIENT ) {
		return nullptr;
	}

	if( ent->r.client->ip.asView().equalsIgnoreCase( wsw::StringView( "loopback" ) ) ) {
		return nullptr;
	}

	// We have to strip port from the address since only host part is expected by a caller.
	// We are sure the port is present if we have already cut off special cases above.
	static char hostBuffer[MAX_INFO_VALUE];
	ent->r.client->ip.copyTo( hostBuffer, sizeof( hostBuffer ) );

	// If it is IPv6 host and port
	if( *hostBuffer == '[' ) {
		// Chop the buffer string at the index of the right bracket
		*( strchr( hostBuffer + 1, ']' ) ) = '\0';
		return hostBuffer + 1;
	}

	// Chop the buffer string at the index of the port separator
	*strchr( hostBuffer, ':' ) = '\0';
	return hostBuffer;
}

static void G_VoteKickPassed( callvotedata_t *vote ) {
	if( edict_t *ent = G_Vote_GetValidDeferredVoteTarget( vote ) ) {
		// If the address can be supplied for the filter
		if( const char *host = G_GetClientHostForFilter( ent ) ) {
			// Ban the player for 1 minute to prevent an instant reconnect
			SV_Cmd_ExecuteText( EXEC_APPEND, va( "addip %s 1", host ) );
		}
		G_DropClient( ent, ReconnectBehaviour::DontReconnect, "Kicked" );
	}
}


static bool G_VoteKickBanValidate( callvotedata_t *vote, bool first ) {
	if( !filterban->integer ) {
		G_PrintMsg( vote->caller, "%sFilterban is disabled on this server\n", S_COLOR_RED );
		return false;
	}

	return G_SetOrValidateKickLikeCmdTarget( vote, first );
}

static void G_VoteKickBanPassed( callvotedata_t *vote ) {
	if( edict_t *ent = G_Vote_GetValidDeferredVoteTarget( vote ) ) {
		// If the address can be supplied for the filter
		if( const char *host = G_GetClientHostForFilter( ent ) ) {
			SV_Cmd_ExecuteText( EXEC_APPEND, va( "addip %s 15\n", host ) );
		}
		G_DropClient( ent, ReconnectBehaviour::DontReconnect, "Kicked" );
	}
}

static bool G_VoteMuteValidate( callvotedata_t *vote, bool first ) {
	return G_SetOrValidateKickLikeCmdTarget( vote, first );
}

// chat mute
static void G_VoteMutePassed( callvotedata_t *vote ) {
	if( edict_t *ent = G_Vote_GetValidDeferredVoteTarget( vote ) ) {
		ChatHandlersChain::instance()->mute( ent );
		ent->r.client->stats.AddToEntry( "muted_count", 1 );
	}
}

static bool G_VoteUnmuteValidate( callvotedata_t *vote, bool first ) {
	return G_SetOrValidateKickLikeCmdTarget( vote, first );
}

// chat unmute
static void G_VoteUnmutePassed( callvotedata_t *vote ) {
	if( edict_t *ent = G_Vote_GetValidDeferredVoteTarget( vote ) ) {
		ChatHandlersChain::instance()->unmute( ent );
	}
}

static bool G_ValidateBooleanSwitchVote( int presentValue, const char *desc, callvotedata_t *vote, bool first );

static bool G_ValidateBooleanSwitchVote( const cvar_t *var, const char *desc, callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( var->integer, desc, vote, first );
}

static bool G_ValidateBooleanSwitchVote( int presentValue, const char *desc, callvotedata_t *vote, bool first ) {
	int value = atoi( vote->argv[0] );
	if( value != 0 && value != 1 ) {
		return false;
	}

	if( value && presentValue ) {
		if( first ) {
			G_PrintMsg( vote->caller, S_COLOR_RED "%s is already allowed\n", desc );
		}
		return false;
	}

	if( !value && !presentValue ) {
		if( first ) {
			G_PrintMsg( vote->caller, S_COLOR_RED "%s is already disabled\n", desc );
		}
		return false;
	}

	return true;
}

/*
* addbots
*/

static bool G_VoteNumBotsValidate( callvotedata_t *vote, bool first ) {
	int numbots = atoi( vote->argv[0] );

	if( g_numbots->integer == numbots ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sNumber of bots is already %i\n", S_COLOR_RED, numbots );
		}
		return false;
	}

	if( numbots < 0 ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sNegative number of bots is not allowed\n", S_COLOR_RED );
		}
		return false;
	}

	if( numbots > ggs->maxclients ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sNumber of bots can't be higher than the number of client spots (%i)\n",
						S_COLOR_RED, ggs->maxclients );
		}
		return false;
	}

	return true;
}

static void G_VoteNumBotsPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_numbots", vote->argv[0] );
}

static const char *G_VoteNumBotsCurrent( void ) {
	return va( "%i", g_numbots->integer );
}

/*
* allow_teamdamage
*/

static bool G_VoteAllowTeamDamageValidate( callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( g_allow_teamdamage, "Team damage", vote, first );
}

static void G_VoteAllowTeamDamagePassed( callvotedata_t *vote ) {
	Cvar_Set( "g_allow_teamdamage", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteAllowTeamDamageCurrent( void ) {
	if( g_allow_teamdamage->integer ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* instajump
*/

static bool G_VoteAllowInstajumpValidate( callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( g_instajump, "Instajump", vote, first );
}

static void G_VoteAllowInstajumpPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_instajump", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteAllowInstajumpCurrent( void ) {
	if( g_instajump->integer ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* instashield
*/

static bool G_VoteAllowInstashieldValidate( callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( g_instashield, "Instashield", vote, first );
}

static void G_VoteAllowInstashieldPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_instashield", va( "%i", atoi( vote->argv[0] ) ) );

	// remove the shield from all players
	if( !g_instashield->integer ) {
		int i;

		for( i = 0; i < ggs->maxclients; i++ ) {
			if( G_GetClientState( i ) < CS_SPAWNED ) {
				continue;
			}

			game.clients[i].ps.inventory[POWERUP_SHELL] = 0;
		}
	}
}

static const char *G_VoteAllowInstashieldCurrent( void ) {
	if( g_instashield->integer ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* allow_falldamage
*/

static bool G_VoteAllowFallDamageValidate( callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( (int)GS_FallDamage( *ggs ), "Fall damage", vote, first );
}

static void G_VoteAllowFallDamagePassed( callvotedata_t *vote ) {
	Cvar_Set( "g_allow_falldamage", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteAllowFallDamageCurrent( void ) {
	if( GS_FallDamage( *ggs ) ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* allow_selfdamage
*/

static bool G_VoteAllowSelfDamageValidate( callvotedata_t *vote, bool first ) {
	return G_ValidateBooleanSwitchVote( (int)GS_SelfDamage( *ggs ), "Self damage", vote, first );
}

static void G_VoteAllowSelfDamagePassed( callvotedata_t *vote ) {
	Cvar_Set( "g_allow_selfdamage", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteAllowSelfDamageCurrent( void ) {
	if( GS_SelfDamage( *ggs ) ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* timeout
*/
static bool G_VoteTimeoutValidate( callvotedata_t *vote, bool first ) {
	if( GS_MatchPaused( *ggs ) && ( level.timeout.endtime - level.timeout.time ) >= 2 * TIMEIN_TIME ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sTimeout already in progress\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteTimeoutPassed( callvotedata_t *vote ) {
	if( !GS_MatchPaused( *ggs ) ) {
		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEOUT_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
	}

	GS_GamestatSetFlag( *ggs, GAMESTAT_FLAG_PAUSED, true );
	level.timeout.caller = 0;
	level.timeout.endtime = level.timeout.time + TIMEOUT_TIME + FRAMETIME;
}

/*
* timein
*/
static bool G_VoteTimeinValidate( callvotedata_t *vote, bool first ) {
	if( !GS_MatchPaused( *ggs ) ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sNo timeout in progress\n", S_COLOR_RED );
		}
		return false;
	}

	if( level.timeout.endtime - level.timeout.time <= 2 * TIMEIN_TIME ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sTimeout is about to end already\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteTimeinPassed( callvotedata_t *vote ) {
	G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEIN_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
	level.timeout.endtime = level.timeout.time + TIMEIN_TIME + FRAMETIME;
}

/*
* allow_uneven
*/
static bool G_VoteAllowUnevenValidate( callvotedata_t *vote, bool first ) {
	int allow_uneven = atoi( vote->argv[0] );

	if( allow_uneven != 0 && allow_uneven != 1 ) {
		return false;
	}

	if( allow_uneven && g_teams_allow_uneven->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sUneven teams is already allowed.\n", S_COLOR_RED );
		}
		return false;
	}

	if( !allow_uneven && !g_teams_allow_uneven->integer ) {
		if( first ) {
			G_PrintMsg( vote->caller, "%sUneven teams is already disallowed\n", S_COLOR_RED );
		}
		return false;
	}

	return true;
}

static void G_VoteAllowUnevenPassed( callvotedata_t *vote ) {
	Cvar_Set( "g_teams_allow_uneven", va( "%i", atoi( vote->argv[0] ) ) );
}

static const char *G_VoteAllowUnevenCurrent( void ) {
	if( g_teams_allow_uneven->integer ) {
		return "1";
	} else {
		return "0";
	}
}

/*
* Shuffle
*/
static void G_VoteShufflePassed( callvotedata_t *vote ) {
	int i;
	int p1, p2, inc;
	int team;
	int numplayers;
	weighted_player_t players[MAX_CLIENTS];

	numplayers = 0;
	for( team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ ) {
		if( !teamlist[team].numplayers ) {
			continue;
		}
		for( i = 0; i < teamlist[team].numplayers; i++ ) {
			players[numplayers].ent = teamlist[team].playerIndices[i];
			players[numplayers].weight = rand();
			numplayers++;
		}
	}

	if( !numplayers ) {
		return;
	}

	qsort( players, numplayers, sizeof( weighted_player_t ), ( int ( * )( const void *, const void * ) )G_VoteCompareWeightedPlayers );

	if( rand() & 1 ) {
		p1 = 0;
		p2 = numplayers - 1;
		inc = 1;
	} else {
		p1 = numplayers - 1;
		p2 = 0;
		inc = -1;
	}

	// put players into teams
	team = rand() % numplayers;
	for( i = p1; ; i += inc ) {
		edict_t *e = game.edicts + players[i].ent;
		int newteam = TEAM_ALPHA + team++ % ( GS_MAX_TEAMS - TEAM_ALPHA );

		if( e->s.team != newteam ) {
			G_Teams_SetTeam( e, newteam );
		}

		if( i == p2 ) {
			break;
		}
	}

	G_Gametype_ScoreEvent( NULL, "shuffle", "" );
}

static bool G_VoteShuffleValidate( callvotedata_t *vote, bool first ) {
	if( !GS_TeamBasedGametype( *ggs ) || level.gametype.maxPlayersPerTeam == 1 ) {
		if( first ) {
			G_PrintMsg( vote->caller, S_COLOR_RED "Shuffle only works in team-based game modes\n" );
		}
		return false;
	}

	return true;
}

/*
* Rebalance
*/
static void G_VoteRebalancePassed( callvotedata_t *vote ) {
	int i;
	int team;
	int lowest_team, lowest_score;
	int numplayers;
	weighted_player_t players[MAX_CLIENTS];

	numplayers = 0;
	lowest_team = GS_MAX_TEAMS;
	lowest_score = 999999;
	for( team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ ) {
		if( !teamlist[team].numplayers ) {
			continue;
		}

		if( teamlist[team].stats.score < lowest_score ) {
			lowest_team = team;
			lowest_score = teamlist[team].stats.score;
		}

		for( i = 0; i < teamlist[team].numplayers; i++ ) {
			int ent = teamlist[team].playerIndices[i];
			players[numplayers].ent = ent;
			players[numplayers].weight = game.edicts[ent].r.client->stats.score;
			numplayers++;
		}
	}

	if( !numplayers || lowest_team == GS_MAX_TEAMS ) {
		return;
	}

	qsort( players, numplayers, sizeof( weighted_player_t ), ( int ( * )( const void *, const void * ) )G_VoteCompareWeightedPlayers );

	// put players into teams
	// start with the lowest scoring team
	team = lowest_team - TEAM_ALPHA;
	for( i = 0; i < numplayers; i++ ) {
		edict_t *e = game.edicts + players[i].ent;
		int newteam = TEAM_ALPHA + team % ( GS_MAX_TEAMS - TEAM_ALPHA );

		if( e->s.team != newteam ) {
			G_Teams_SetTeam( e, newteam );
		}

		e->r.client->stats.Clear();

		if( i % 2 == 0 ) {
			team++;
		}
	}

	G_Gametype_ScoreEvent( NULL, "rebalance", "" );
}

static bool G_VoteRebalanceValidate( callvotedata_t *vote, bool first ) {
	if( !GS_TeamBasedGametype( *ggs ) || level.gametype.maxPlayersPerTeam == 1 ) {
		if( first ) {
			G_PrintMsg( vote->caller, S_COLOR_RED "Rebalance only works in team-based game modes\n" );
		}
		return false;
	}

	return true;
}

//================================================
//
//================================================


callvotetype_t *G_RegisterCallvote( const char *name ) {
	for( auto *callvote = callvotesHeadNode; callvote != NULL; callvote = callvote->next ) {
		if( !Q_stricmp( callvote->name, name ) ) {
			return callvote;
		}
	}

	if( registrationNum >= MAX_CALLVOTES ) {
		G_Printf( S_COLOR_YELLOW "Failed to register `%s`: too many callvotes\n", name );
		return nullptr;
	}

	// create a new callvote
	auto *callvote = ( callvotetype_t * )Q_malloc( sizeof( callvotetype_t ) );
	memset( callvote, 0, sizeof( callvotetype_t ) );
	callvote->registrationNum = registrationNum++;
	callvote->next = callvotesHeadNode;
	callvotesHeadNode = callvote;

	callvote->name = Q_strdup( name );
	return callvote;
}

void G_FreeCallvotes( void ) {
	callvotetype_t *callvote;

	while( callvotesHeadNode ) {
		callvote = callvotesHeadNode->next;

		if( callvotesHeadNode->name ) {
			Q_free( callvotesHeadNode->name );
		}
		if( callvotesHeadNode->argument_format ) {
			Q_free( callvotesHeadNode->argument_format );
		}
		if( callvotesHeadNode->help ) {
			Q_free( callvotesHeadNode->help );
		}

		Q_free( callvotesHeadNode );
		callvotesHeadNode = callvote;
	}

	callvotesHeadNode = NULL;
	registrationNum = 0;
}

//===================================================================
// Common functions
//===================================================================

/*
* G_CallVotes_ResetClient
*/
void G_CallVotes_ResetClient( int n ) {
	clientVoted[n] = VOTED_NOTHING;
	clientVoteChanges[n] = g_callvote_maxchanges->integer;
	if( clientVoteChanges[n] < 1 ) {
		clientVoteChanges[n] = 1;
	}
}

/*
* G_CallVotes_Reset
*/
void G_CallVotes_Reset( void ) {
	int i;

	if( callvoteState.vote.caller && callvoteState.vote.caller->r.client ) {
		callvoteState.vote.caller->r.client->callvote_when = game.realtime;
	}

	callvoteState.vote.callvote = NULL;
	for( i = 0; i < ggs->maxclients; i++ )
		G_CallVotes_ResetClient( i );
	callvoteState.timeout = 0;

	callvoteState.vote.caller = NULL;
	if( callvoteState.vote.string ) {
		Q_free( callvoteState.vote.string );
	}
	if( callvoteState.vote.data ) {
		Q_free( callvoteState.vote.data );
	}
	for( i = 0; i < callvoteState.vote.argc; i++ ) {
		if( callvoteState.vote.argv[i] ) {
			Q_free( callvoteState.vote.argv[i] );
		}
	}

	SV_SetConfigString( CS_ACTIVE_CALLVOTE, "" );

	memset( &callvoteState, 0, sizeof( callvoteState ) );
}

/*
* G_CallVotes_PrintUsagesToPlayer
*/
static void G_CallVotes_PrintUsagesToPlayer( edict_t *ent ) {
	callvotetype_t *callvote;

	G_PrintMsg( ent, "Available votes:\n" );
	for( callvote = callvotesHeadNode; callvote != NULL; callvote = callvote->next ) {
		if( Cvar_Value( va( "g_disable_vote_%s", callvote->name ) ) ) {
			continue;
		}

		if( callvote->argument_format ) {
			G_PrintMsg( ent, " %s %s\n", callvote->name, callvote->argument_format );
		} else {
			G_PrintMsg( ent, " %s\n", callvote->name );
		}
	}
}

/*
* G_CallVotes_PrintHelpToPlayer
*/
static void G_CallVotes_PrintHelpToPlayer( edict_t *ent, callvotetype_t *callvote, const CmdArgs &cmdArgs ) {

	if( !callvote ) {
		return;
	}

	G_PrintMsg( ent, "Usage: %s %s\n%s%s%s\n", callvote->name,
				( callvote->argument_format ? callvote->argument_format : "" ),
				( callvote->current ? va( "Current: %s\n", callvote->current() ) : "" ),
				( callvote->help ? "- " : "" ), ( callvote->help ? callvote->help : "" ) );
	if( callvote->extraHelp != NULL ) {
		callvote->extraHelp( ent, cmdArgs );
	}
}

/*
* G_CallVotes_ArgsToString
*/
static const char *G_CallVotes_ArgsToString( const callvotedata_t *vote ) {
	static char argstring[MAX_STRING_CHARS];
	int i;

	argstring[0] = 0;

	if( vote->argc > 0 ) {
		Q_strncatz( argstring, vote->argv[0], sizeof( argstring ) );
	}
	for( i = 1; i < vote->argc; i++ ) {
		Q_strncatz( argstring, " ", sizeof( argstring ) );
		Q_strncatz( argstring, vote->argv[i], sizeof( argstring ) );
	}

	return argstring;
}

/*
* G_CallVotes_Arguments
*/
static const char *G_CallVotes_Arguments( const callvotedata_t *vote ) {
	const char *arguments;
	if( vote->string ) {
		arguments = vote->string;
	} else {
		arguments = G_CallVotes_ArgsToString( vote );
	}
	return arguments;
}

/*
* G_CallVotes_String
*/
static const char *G_CallVotes_String( const callvotedata_t *vote ) {
	const char *arguments;
	static char string[MAX_CONFIGSTRING_CHARS];

	arguments = G_CallVotes_Arguments( vote );
	if( arguments[0] ) {
		Q_snprintfz( string, sizeof( string ), "%s %s", vote->callvote->name, arguments );
		return string;
	}
	return vote->callvote->name;
}

static void RequestActions() {
	const wsw::StringView yellow( S_COLOR_YELLOW ), white( S_COLOR_WHITE );
	wsw::StaticString<64> title, desc;
	const wsw::StringView vote( G_CallVotes_String( &callvoteState.vote ) );
	title << "A vote "_asView << yellow << vote << white << " is in progress"_asView;
	desc << "Press "_asView;
	desc << yellow << "F1"_asView << white << " to vote yes, "_asView;
	desc << yellow << "F2"_asView << white << " to vote no"_asView;
	const std::pair<wsw::StringView, wsw::StringView> actions[2] {
		{ "F1"_asView, "vote yes"_asView },
		{ "F2"_asView, "vote no"_asView }
	};

	for( const edict_t *ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
		if( !ent->r.inuse || G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
			continue;
		}

		if( ( ent->r.svflags & SVF_FAKECLIENT ) ) {
			continue;
		}

		if( clientVoted[PLAYERNUM( ent )] == VOTED_NOTHING ) {
			G_SendActionRequest( ent, "vote"_asView, 2500, title.asView(), desc.asView(), actions );
		}
	}
}

/*
* G_CallVotes_CheckState
*/
static void G_CallVotes_CheckState( void ) {
	edict_t *ent;
	int needvotes, yeses = 0, voters = 0, noes = 0;
	static int64_t warntimer;

	if( !callvoteState.vote.callvote ) {
		warntimer = 0;
		return;
	}

	if( callvoteState.vote.callvote->validate != NULL &&
		!callvoteState.vote.callvote->validate( &callvoteState.vote, false ) ) {
		// fixme: should be vote cancelled or something
		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_FAILED_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
		G_PrintMsg( NULL, "Vote is no longer valid\nVote %s%s%s canceled\n", S_COLOR_YELLOW,
					G_CallVotes_String( &callvoteState.vote ), S_COLOR_WHITE );
		G_CallVotes_Reset();
		return;
	}

	//analize votation state
	for( ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
		Client *client = ent->r.client;

		if( !ent->r.inuse || G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
			continue;
		}

		if( ( ent->r.svflags & SVF_FAKECLIENT ) ) {
			continue;
		}

		// ignore inactive players unless they have voted
		if( client->last_activity && client->last_activity + ( g_inactivity_maxtime->value * 1000 ) < level.time &&
			clientVoted[PLAYERNUM( ent )] == VOTED_NOTHING ) {
			continue;
		}

		voters++;
		if( clientVoted[PLAYERNUM( ent )] == VOTED_YES ) {
			yeses++;
		} else if( clientVoted[PLAYERNUM( ent )] == VOTED_NO ) {
			noes++;
		}
	}

	// passed?
	needvotes = (int)( ( voters * g_callvote_electpercentage->value ) / 100 );
	if( yeses > needvotes || callvoteState.vote.operatorcall ) {
		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_PASSED_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
		G_PrintMsg( NULL, "Vote %s%s%s passed\n", S_COLOR_YELLOW,
					G_CallVotes_String( &callvoteState.vote ), S_COLOR_WHITE );
		if( callvoteState.vote.callvote->execute != NULL ) {
			callvoteState.vote.callvote->execute( &callvoteState.vote );
		}
		G_CallVotes_Reset();
		return;
	}

	// failed?
	if( game.realtime > callvoteState.timeout || voters - noes <= needvotes ) { // no change to pass anymore
		G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_FAILED_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );
		G_PrintMsg( NULL, "Vote %s%s%s failed\n", S_COLOR_YELLOW,
					G_CallVotes_String( &callvoteState.vote ), S_COLOR_WHITE );
		G_CallVotes_Reset();
		return;
	}

	if( warntimer < game.realtime ) {
		if( callvoteState.timeout - game.realtime <= 7500 && callvoteState.timeout - game.realtime > 2500 ) {
			G_AnnouncerSound( NULL, SV_SoundIndex( S_ANNOUNCER_CALLVOTE_VOTE_NOW ), GS_MAX_TEAMS, true, NULL );
		}
		G_PrintMsg( NULL, "A vote %s%s%s is in progress: %i voted yes, %i voted no. %i required\n", S_COLOR_YELLOW,
					G_CallVotes_String( &callvoteState.vote ), S_COLOR_WHITE, yeses, noes,
					needvotes + 1 );
		warntimer = game.realtime + 5 * 1000;
		RequestActions();
	}
}

/*
* G_CallVotes_CmdVote
*/
void G_CallVotes_CmdVote( edict_t *ent, const CmdArgs &cmdArgs ) {
	const char *vote;
	int vote_id;

	if( !ent->r.client ) {
		return;
	}
	if( ( ent->r.svflags & SVF_FAKECLIENT ) ) {
		return;
	}

	if( !callvoteState.vote.callvote ) {
		G_PrintMsg( ent, "%sThere's no vote in progress\n", S_COLOR_RED );
		return;
	}

	vote = Cmd_Argv( 1 );
	if( !Q_stricmp( vote, "yes" ) ) {
		vote_id = VOTED_YES;
	} else if( !Q_stricmp( vote, "no" ) ) {
		vote_id = VOTED_NO;
	} else {
		G_PrintMsg( ent, "%sInvalid vote: %s%s%s. Use yes or no\n", S_COLOR_RED,
					S_COLOR_YELLOW, vote, S_COLOR_RED );
		return;
	}

	if( clientVoted[PLAYERNUM( ent )] == vote_id ) {
		G_PrintMsg( ent, "%sYou have already voted %s\n", S_COLOR_RED, vote );
		return;
	}

	if( clientVoteChanges[PLAYERNUM( ent )] == 0 ) {
		G_PrintMsg( ent, "%sYou cannot change your vote anymore\n", S_COLOR_RED );
		return;
	}

	clientVoted[PLAYERNUM( ent )] = vote_id;
	clientVoteChanges[PLAYERNUM( ent )]--;
	G_CallVotes_CheckState();
}

void G_CallVotes_UpdateCurrentStatus() {
	for( auto *callvote = callvotesHeadNode; callvote; callvote = callvote->next ) {
		if( !callvote->isVotingEnabled && !callvote->isOpcallEnabled ) {
			continue;
		}

		bool modified = false;
		if( auto method = callvote->current ) {
			const wsw::StringView current( method() );
			if( !callvote->lastCurrent.equals( current ) ) {
				callvote->lastCurrent.assign( current );
				modified = true;
			}
		}
		if( !modified ) {
			continue;
		}

		wsw::StaticString<MAX_STRING_CHARS> status;
		if( callvote->isVotingEnabled ) {
			status << 'v';
		}
		if( callvote->isOpcallEnabled ) {
			status << 'o';
		}
		if( !callvote->lastCurrent.empty() ) {
			status << ' ' << callvote->lastCurrent;
		}

		using Storage = wsw::ConfigStringStorage;
		int index = CS_CALLVOTEINFOS;
		index += callvote->registrationNum * Storage::kNumCallvoteFields + (int)Storage::CallvoteFields::Status;
		SV_SetConfigString( index, status.data() );
	}
}

/*
* G_CallVotes_Think
*/
void G_CallVotes_Think( void ) {
	static int64_t callvotethinktimer = 0;

	if( callvotethinktimer < game.realtime ) {
		G_CallVotes_UpdateCurrentStatus();

		G_CallVotes_CheckState();
		callvotethinktimer = game.realtime + 1000;
	}
}

static bool G_CallVotes_CheckFlood( const edict_t *ent ) {
	const auto lastClientVoteAt = ent->r.client->callvote_when;
	if( !lastClientVoteAt || lastClientVoteAt + g_callvote_cooldowntime->integer * 1000 <= game.realtime ) {
		return true;
	}

	G_PrintMsg( ent, "%sYou can not call a vote right now\n", S_COLOR_RED );
	return false;
}

/*
* G_CallVote
*/
static void G_CallVote( edict_t *ent, bool isopcall, const CmdArgs &cmdArgs ) {
	int i;
	const char *votename;
	callvotetype_t *callvote;

	if( !isopcall && ent->s.team == TEAM_SPECTATOR && GS_IndividualGametype( *ggs )
		&& GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME && !GS_MatchPaused( *ggs ) ) {
		int team, count;
		edict_t *e;

		for( count = 0, team = TEAM_ALPHA; team < GS_MAX_TEAMS; team++ ) {
			if( !teamlist[team].numplayers ) {
				continue;
			}

			for( i = 0; i < teamlist[team].numplayers; i++ ) {
				e = game.edicts + teamlist[team].playerIndices[i];
				if( e->r.inuse && e->r.svflags & SVF_FAKECLIENT ) {
					count++;
				}
			}
		}

		if( !count ) {
			G_PrintMsg( ent, "%sSpectators cannot start a vote while a match is in progress\n", S_COLOR_RED );
			return;
		}
	}

	if( !g_callvote_enabled->integer ) {
		G_PrintMsg( ent, "%sCallvoting is disabled on this server\n", S_COLOR_RED );
		return;
	}

	if( callvoteState.vote.callvote ) {
		G_PrintMsg( ent, "%sA vote is already in progress\n", S_COLOR_RED );
		return;
	}

	votename = Cmd_Argv( 1 );
	if( !votename || !votename[0] ) {
		G_CallVotes_PrintUsagesToPlayer( ent );
		return;
	}

	if( strlen( votename ) > MAX_QPATH ) {
		G_PrintMsg( ent, "%sInvalid vote\n", S_COLOR_RED );
		G_CallVotes_PrintUsagesToPlayer( ent );
		return;
	}

	//find the actual callvote command
	for( callvote = callvotesHeadNode; callvote != NULL; callvote = callvote->next ) {
		if( callvote->name && !Q_stricmp( callvote->name, votename ) ) {
			break;
		}
	}

	//unrecognized callvote type
	if( callvote == NULL ) {
		G_PrintMsg( ent, "%sUnrecognized vote: %s\n", S_COLOR_RED, votename );
		G_CallVotes_PrintUsagesToPlayer( ent );
		return;
	}

	// wsw : pb : server admin can now disable a specific vote command (g_disable_vote_<vote name>)
	// check if vote is disabled
	if( !isopcall && !callvote->isVotingEnabled ) {
		G_PrintMsg( ent, "%sCallvote %s is disabled on this server\n", S_COLOR_RED, callvote->name );
		return;
	}

	// allow a second cvar specific for opcall
	if( isopcall && !callvote->isOpcallEnabled ) {
		G_PrintMsg( ent, "%sOpcall %s is disabled on this server\n", S_COLOR_RED, callvote->name );
		return;
	}

	// Apply flood check in this case early (avoid a fruitless state creation)
	if( !callvote->validate ) {
		if( !G_CallVotes_CheckFlood( ent ) ) {
			return;
		}
	}

	//we got a valid type. Get the parameters if any
	if( callvote->expectedargs != Cmd_Argc() - 2 ) {
		if( callvote->expectedargs != -1 &&
			( callvote->expectedargs != -2 || Cmd_Argc() - 2 > 0 ) ) {
			// wrong number of parametres
			G_CallVotes_PrintHelpToPlayer( ent, callvote, cmdArgs );
			return;
		}
	}

	callvoteState.vote.argc = Cmd_Argc() - 2;
	for( i = 0; i < callvoteState.vote.argc; i++ )
		callvoteState.vote.argv[i] = Q_strdup( Cmd_Argv( i + 2 ) );

	callvoteState.vote.callvote = callvote;
	callvoteState.vote.caller = ent;
	callvoteState.vote.operatorcall = isopcall;

	//validate if there's a validation func
	if( callvote->validate != NULL && !callvote->validate( &callvoteState.vote, true ) ) {
		// Hack... save the old timestamp before G_CallVotes_Reset()
		// and restore it afterwards. All of this begs for refactoring anyway.
		const auto oldClientTimestamp = ent->r.client->callvote_when;
		G_CallVotes_PrintHelpToPlayer( ent, callvote, cmdArgs );
		G_CallVotes_Reset(); // free the args
		ent->r.client->callvote_when = oldClientTimestamp;
		return;
	}

	if( !isopcall ) {
		// Actually apply flood check if it has not been performed yet
		if( callvote->validate && !G_CallVotes_CheckFlood( ent ) ) {
			G_CallVotes_Reset();
			return;
		}
	}

	//we're done. Proceed launching the election
	for( i = 0; i < ggs->maxclients; i++ )
		G_CallVotes_ResetClient( i );
	callvoteState.timeout = game.realtime + ( g_callvote_electtime->integer * 1000 );

	//caller is assumed to vote YES
	clientVoted[PLAYERNUM( ent )] = VOTED_YES;
	clientVoteChanges[PLAYERNUM( ent )]--;

	ent->r.client->callvote_when = callvoteState.timeout;

	SV_SetConfigString( CS_ACTIVE_CALLVOTE, G_CallVotes_String( &callvoteState.vote ) );

	G_AnnouncerSound( NULL, SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_CALLED_1_to_2, ( rand() & 1 ) + 1 ) ), GS_MAX_TEAMS, true, NULL );

	G_PrintMsg( NULL, "%s" S_COLOR_WHITE " requested to vote " S_COLOR_YELLOW "%s\n",
				ent->r.client->netname.data(), G_CallVotes_String( &callvoteState.vote ) );

	G_PrintMsg( NULL, "Press " S_COLOR_YELLOW "F1" S_COLOR_WHITE " to " S_COLOR_YELLOW "vote yes"
				S_COLOR_WHITE " or " S_COLOR_YELLOW "F2" S_COLOR_WHITE " to " S_COLOR_YELLOW "vote no\n" );

	RequestActions();

	G_CallVotes_Think(); // make the first think
}

/*
* G_CallVote_Cmd
*/
void G_CallVote_Cmd( edict_t *ent, const CmdArgs &cmdArgs ) {
	if( ( ent->r.svflags & SVF_FAKECLIENT ) ) {
		return;
	}
	G_CallVote( ent, false, cmdArgs );
}

/*
* G_OperatorVote_Cmd
*/
void G_OperatorVote_Cmd( edict_t *ent, const CmdArgs &cmdArgs ) {
	edict_t *other;
	int forceVote;

	if( !ent->r.client ) {
		return;
	}
	if( ( ent->r.svflags & SVF_FAKECLIENT ) ) {
		return;
	}

	if( !ent->r.client->isoperator ) {
		G_PrintMsg( ent, "You are not a game operator\n" );
		return;
	}

	if( !Q_stricmp( Cmd_Argv( 1 ), "help" ) ) {
		G_PrintMsg( ent, "Opcall can be used with all callvotes and the following commands:\n" );
		G_PrintMsg( ent, "-help\n - passvote\n- cancelvote\n- putteam\n" );
		return;
	}

	if( !Q_stricmp( Cmd_Argv( 1 ), "cancelvote" ) ) {
		forceVote = VOTED_NO;
	} else if( !Q_stricmp( Cmd_Argv( 1 ), "passvote" ) ) {
		forceVote = VOTED_YES;
	} else {
		forceVote = VOTED_NOTHING;
	}

	if( forceVote != VOTED_NOTHING ) {
		if( !callvoteState.vote.callvote ) {
			G_PrintMsg( ent, "There's no callvote to cancel.\n" );
			return;
		}

		for( other = game.edicts + 1; PLAYERNUM( other ) < ggs->maxclients; other++ ) {
			if( !other->r.inuse || G_GetClientState( PLAYERNUM( other ) ) < CS_SPAWNED ) {
				continue;
			}
			if( ( other->r.svflags & SVF_FAKECLIENT ) ) {
				continue;
			}

			clientVoted[PLAYERNUM( other )] = forceVote;
		}

		G_PrintMsg( NULL, "Callvote has been %s by %s\n",
					forceVote == VOTED_NO ? "cancelled" : "passed", ent->r.client->netname.data() );
		return;
	}

	if( !Q_stricmp( Cmd_Argv( 1 ), "putteam" ) ) {
		const char *splayer = Cmd_Argv( 2 );
		const char *steam = Cmd_Argv( 3 );
		edict_t *playerEnt;
		int newTeam;

		if( !steam || !steam[0] || !splayer || !splayer[0] ) {
			G_PrintMsg( ent, "Usage 'putteam <player id > <team name>'.\n" );
			return;
		}

		if( ( newTeam = GS_Teams_TeamFromName( ggs, steam ) ) < 0 ) {
			G_PrintMsg( ent, "The team '%s' doesn't exist.\n", steam );
			return;
		}

		if( ( playerEnt = G_PlayerForText( splayer ) ) == NULL ) {
			G_PrintMsg( ent, "The player '%s' couldn't be found.\n", splayer );
			return;
		}

		if( playerEnt->s.team == newTeam ) {
			G_PrintMsg( ent, "The player '%s' is already in team '%s'.\n", playerEnt->r.client->netname.data(), GS_TeamName( ggs, newTeam ) );
			return;
		}

		G_Teams_SetTeam( playerEnt, newTeam );
		G_PrintMsg( NULL, "%s was moved to team %s by %s.\n", playerEnt->r.client->netname.data(), GS_TeamName( ggs, newTeam ), ent->r.client->netname.data() );

		return;
	}

	G_CallVote( ent, true, cmdArgs );
}

/*
* G_VoteFromScriptValidate
*/
static bool G_VoteFromScriptValidate( callvotedata_t *vote, bool first ) {
	char argsString[MAX_STRING_CHARS];
	int i;

	if( !vote || !vote->callvote || !vote->caller ) {
		return false;
	}

	Q_snprintfz( argsString, MAX_STRING_CHARS, "\"%s\"", vote->callvote->name );
	for( i = 0; i < vote->argc; i++ ) {
		Q_strncatz( argsString, " ", MAX_STRING_CHARS );
		Q_strncatz( argsString, va( " \"%s\"", vote->argv[i] ), MAX_STRING_CHARS );
	}

	return GT_asCallGameCommand( vote->caller->r.client, "callvotevalidate"_asView, wsw::StringView( argsString ), vote->argc + 1 );
}

/*
* G_VoteFromScriptPassed
*/
static void G_VoteFromScriptPassed( callvotedata_t *vote ) {
	char argsString[MAX_STRING_CHARS];
	int i;

	if( !vote || !vote->callvote || !vote->caller ) {
		return;
	}

	Q_snprintfz( argsString, MAX_STRING_CHARS, "\"%s\"", vote->callvote->name );
	for( i = 0; i < vote->argc; i++ ) {
		Q_strncatz( argsString, " ", MAX_STRING_CHARS );
		Q_strncatz( argsString, va( " \"%s\"", vote->argv[i] ), MAX_STRING_CHARS );
	}

	GT_asCallGameCommand( vote->caller->r.client, "callvotepassed"_asView, wsw::StringView( argsString ), vote->argc + 1 );
}

/*
* G_RegisterGametypeScriptCallvote
*/
void G_RegisterGametypeScriptCallvote( const char *name, const char *usage, const char *type, const char *help ) {
	if( !name ) {
		return;
	}

	auto *vote = G_RegisterCallvote( name );
	if( !vote ) {
		return;
	}

	vote->expectedargs = -1;
	vote->validate = G_VoteFromScriptValidate;
	vote->execute = G_VoteFromScriptPassed;
	vote->current = NULL;
	vote->extraHelp = NULL;
	vote->argument_format = usage ? Q_strdup( usage ) : NULL;
	vote->help = help ? Q_strdup( va( "%s", help ) ) : NULL;
	// TODO: Make the group scriptable
	vote->group = "other";
}

/*
* G_CallVotes_Init
*/
void G_CallVotes_Init( void ) {
	callvotetype_t *callvote;

	g_callvote_electpercentage =    Cvar_Get( "g_vote_percent", "55", CVAR_ARCHIVE );
	g_callvote_electtime =      Cvar_Get( "g_vote_electtime", "40", CVAR_ARCHIVE );
	g_callvote_enabled =        Cvar_Get( "g_vote_allowed", "1", CVAR_ARCHIVE );
	g_callvote_maxchanges =     Cvar_Get( "g_vote_maxchanges", "3", CVAR_ARCHIVE );
	g_callvote_cooldowntime =   Cvar_Get( "g_vote_cooldowntime", "5", CVAR_ARCHIVE );

	// register all callvotes

	callvote = G_RegisterCallvote( "map" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteMapValidate;
	callvote->execute = G_VoteMapPassed;
	callvote->current = G_VoteMapCurrent;
	callvote->extraHelp = G_VoteMapExtraHelp;
	callvote->describeClientArgs = G_VoteMapDescribeClientArgs;
	callvote->argument_format = Q_strdup( "<name>" );
	callvote->help = Q_strdup( "Changes map" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "restart" );
	callvote->expectedargs = 0;
	callvote->validate = NULL;
	callvote->execute = G_VoteRestartPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Restarts current map" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "nextmap" );
	callvote->expectedargs = 0;
	callvote->validate = NULL;
	callvote->execute = G_VoteNextMapPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Jumps to the next map" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "scorelimit" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteScorelimitValidate;
	callvote->execute = G_VoteScorelimitPassed;
	callvote->current = G_VoteScorelimitCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<number>" );
	callvote->describeClientArgs = G_DescribeNumberArg;
	callvote->help = Q_strdup( "Sets the number of frags or caps needed to win the match\nSpecify 0 to disable" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "timelimit" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteTimelimitValidate;
	callvote->execute = G_VoteTimelimitPassed;
	callvote->current = G_VoteTimelimitCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<minutes>" );
	callvote->describeClientArgs = G_DescribeMinutesArg;
	callvote->help = Q_strdup( "Sets number of minutes after which the match ends\nSpecify 0 to disable" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "gametype" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteGametypeValidate;
	callvote->execute = G_VoteGametypePassed;
	callvote->current = G_VoteGametypeCurrent;
	callvote->extraHelp = G_VoteGametypeExtraHelp;
	callvote->argument_format = Q_strdup( "<name>" );
	callvote->describeClientArgs = G_VoteGametypeDescribeClientArgs;
	callvote->help = Q_strdup( "Changes the gametype" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "warmup_timelimit" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteWarmupTimelimitValidate;
	callvote->execute = G_VoteWarmupTimelimitPassed;
	callvote->current = G_VoteWarmupTimelimitCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<minutes>" );
	callvote->describeClientArgs = G_DescribeMinutesArg;
	callvote->help = Q_strdup( "Sets the number of minutes after which the warmup ends\nSpecify 0 to disable" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "extended_time" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteExtendedTimeValidate;
	callvote->execute = G_VoteExtendedTimePassed;
	callvote->current = G_VoteExtendedTimeCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<minutes>" );
	callvote->describeClientArgs = G_DescribeMinutesArg;
	callvote->help = Q_strdup( "Sets the length of the overtime\nSpecify 0 to enable sudden death mode" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "maxteamplayers" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteMaxTeamplayersValidate;
	callvote->execute = G_VoteMaxTeamplayersPassed;
	callvote->current = G_VoteMaxTeamplayersCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<number>" );
	callvote->describeClientArgs = G_DescribeNumberArg;
	callvote->help = Q_strdup( "Sets the maximum number of players in one team" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "lock" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteLockValidate;
	callvote->execute = G_VoteLockPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Locks teams to disallow players joining in mid-game" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "unlock" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteUnlockValidate;
	callvote->execute = G_VoteUnlockPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Unlocks teams to allow players joining in mid-game" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "allready" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteAllreadyValidate;
	callvote->execute = G_VoteAllreadyPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Sets all players as ready so the match can start" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "remove" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteRemoveValidate;
	callvote->execute = G_VoteRemovePassed;
	callvote->current = NULL;
	callvote->extraHelp = G_VoteRemoveExtraHelp;
	callvote->argument_format = Q_strdup( "<player>" );
	callvote->describeClientArgs = G_DescribePlayerArg;
	callvote->help = Q_strdup( "Forces player back to spectator mode" );
	callvote->group = "players";

	callvote = G_RegisterCallvote( "kick" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteKickValidate;
	callvote->execute = G_VoteKickPassed;
	callvote->current = NULL;
	callvote->extraHelp = G_VoteHelp_ShowPlayersList;
	callvote->argument_format = Q_strdup( "<player>" );
	callvote->describeClientArgs = G_DescribePlayerArg;
	callvote->help = Q_strdup( "Removes player from the server" );
	callvote->group = "players";

	callvote = G_RegisterCallvote( "kickban" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteKickBanValidate;
	callvote->execute = G_VoteKickBanPassed;
	callvote->current = NULL;
	callvote->extraHelp = G_VoteHelp_ShowPlayersList;
	callvote->argument_format = Q_strdup( "<player>" );
	callvote->describeClientArgs = G_DescribePlayerArg;
	callvote->help = Q_strdup( "Removes player from the server and bans his IP-address for 15 minutes" );
	callvote->group = "players";

	callvote = G_RegisterCallvote( "mute" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteMuteValidate;
	callvote->execute = G_VoteMutePassed;
	callvote->current = NULL;
	callvote->extraHelp = G_VoteHelp_ShowPlayersList;
	callvote->argument_format = Q_strdup( "<player>" );
	callvote->describeClientArgs = G_DescribePlayerArg;
	callvote->help = Q_strdup( "Disallows chat messages from the muted player" );
	callvote->group = "players";

	callvote = G_RegisterCallvote( "unmute" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteUnmuteValidate;
	callvote->execute = G_VoteUnmutePassed;
	callvote->current = NULL;
	callvote->extraHelp = G_VoteHelp_ShowPlayersList;
	callvote->argument_format = Q_strdup( "<player>" );
	callvote->describeClientArgs = G_DescribePlayerArg;
	callvote->help = Q_strdup( "Reallows chat messages from the unmuted player" );
	callvote->group = "players";

	callvote = G_RegisterCallvote( "numbots" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteNumBotsValidate;
	callvote->execute = G_VoteNumBotsPassed;
	callvote->current = G_VoteNumBotsCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<number>" );
	callvote->describeClientArgs = G_DescribeNumberArg;
	callvote->help = Q_strdup( "Sets the number of bots to play on the server" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "allow_teamdamage" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowTeamDamageValidate;
	callvote->execute = G_VoteAllowTeamDamagePassed;
	callvote->current = G_VoteAllowTeamDamageCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->help = Q_strdup( "Toggles whether shooting teammates will do damage to them" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "instajump" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowInstajumpValidate;
	callvote->execute = G_VoteAllowInstajumpPassed;
	callvote->current = G_VoteAllowInstajumpCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->help = Q_strdup( "Toggles whether instagun can be used for weapon jumping" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "instashield" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowInstashieldValidate;
	callvote->execute = G_VoteAllowInstashieldPassed;
	callvote->current = G_VoteAllowInstashieldCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->help = Q_strdup( "Toggles the availability of instashield in instagib" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "allow_falldamage" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowFallDamageValidate;
	callvote->execute = G_VoteAllowFallDamagePassed;
	callvote->current = G_VoteAllowFallDamageCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->help = Q_strdup( "Toggles whether falling long distances deals damage" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "allow_selfdamage" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowSelfDamageValidate;
	callvote->execute = G_VoteAllowSelfDamagePassed;
	callvote->current = G_VoteAllowSelfDamageCurrent;
	callvote->extraHelp = NULL;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->help = Q_strdup( "Toggles whether weapon splashes can damage self" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "timeout" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteTimeoutValidate;
	callvote->execute = G_VoteTimeoutPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Pauses the game" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "timein" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteTimeinValidate;
	callvote->execute = G_VoteTimeinPassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Resumes the game if in timeout" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "allow_uneven" );
	callvote->expectedargs = 1;
	callvote->validate = G_VoteAllowUnevenValidate;
	callvote->execute = G_VoteAllowUnevenPassed;
	callvote->current = G_VoteAllowUnevenCurrent;
	callvote->extraHelp = NULL;
	callvote->describeClientArgs = G_DescribeBooleanArg;
	callvote->argument_format = Q_strdup( "<1 or 0>" );
	callvote->help = Q_strdup( "Allows uneven number of players in teams" );
	callvote->group = "rules";

	callvote = G_RegisterCallvote( "shuffle" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteShuffleValidate;
	callvote->execute = G_VoteShufflePassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Shuffles teams" );
	callvote->group = "actions";

	callvote = G_RegisterCallvote( "rebalance" );
	callvote->expectedargs = 0;
	callvote->validate = G_VoteRebalanceValidate;
	callvote->execute = G_VoteRebalancePassed;
	callvote->current = NULL;
	callvote->extraHelp = NULL;
	callvote->argument_format = NULL;
	callvote->help = Q_strdup( "Rebalances teams" );
	callvote->group = "actions";

	SV_SetConfigString( CS_CALLVOTE_GROUPS, "all, All, actions, Gameplay actions, rules, Gameplay rules, "
										   "players, Player actions, other, Other" );

	// wsw : pb : server admin can now disable a specific callvote command (g_disable_vote_<callvote name>)
	for( callvote = callvotesHeadNode; callvote != NULL; callvote = callvote->next ) {
		wsw::StaticString<256> votingVarName( "g_disable_voting_%s", callvote->name );
		wsw::StaticString<256> opcallVarName( "g_disable_opcall_%s", callvote->name );
		callvote->isVotingEnabled = !Cvar_Get( votingVarName.data(), "0", CVAR_ARCHIVE )->integer;
		callvote->isOpcallEnabled = !Cvar_Get( opcallVarName.data(), "0", CVAR_ARCHIVE )->integer;
		if( !callvote->isVotingEnabled && !callvote->isOpcallEnabled ) {
			continue;
		}

		const auto configStringIndex = CS_CALLVOTEINFOS + callvote->registrationNum * 5;

		using Storage = wsw::ConfigStringStorage;
		assert( configStringIndex < CS_CALLVOTEINFOS + MAX_CALLVOTEINFOS );

		SV_SetConfigString( configStringIndex + (unsigned)Storage::CallvoteFields::Name, callvote->name );
		SV_SetConfigString( configStringIndex + (unsigned)Storage::CallvoteFields::Desc, callvote->help );
		SV_SetConfigString( configStringIndex + (unsigned)Storage::CallvoteFields::Group, callvote->group );

		if( auto method = callvote->describeClientArgs ) {
			method( configStringIndex + (unsigned)Storage::CallvoteFields::Args );
		} else {
			SV_SetConfigString( configStringIndex + (unsigned)Storage::CallvoteFields::Args, "" );
		}

		wsw::StaticString<MAX_STRING_CHARS> status;
		if( callvote->isVotingEnabled ) {
			status << 'v';
		}
		if( callvote->isOpcallEnabled ) {
			status << 'o';
		}

		if( auto method = callvote->current ) {
			status << ' ' << wsw::StringView( method() );
		}

		SV_SetConfigString( configStringIndex + (unsigned)Storage::CallvoteFields::Status, status.data() );

		assert( configStringIndex <= CS_CALLVOTEINFOS + MAX_CALLVOTEINFOS );
	}

	G_CallVotes_Reset();
}
