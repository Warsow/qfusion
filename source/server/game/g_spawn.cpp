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
#include <common/helpers/parsecompat.h>
#include <common/helpers/stringsplitter.h>
#include <common/facilities/gs_qrespath.h>
#include "commandshandler.h"

const field_t fields[] = {
	{ "classname", FOFS( classname ), F_LSTRING },
	{ "origin", FOFS( s.origin ), F_VECTOR },
	{ "model", FOFS( model ), F_LSTRING },
	{ "model2", FOFS( model2 ), F_LSTRING },
	{ "spawnflags", FOFS( spawnflags ), F_INT },
	{ "speed", FOFS( speed ), F_FLOAT },
	{ "target", FOFS( target ), F_LSTRING },
	{ "targetname", FOFS( targetname ), F_LSTRING },
	{ "pathtarget", FOFS( pathtarget ), F_LSTRING },
	{ "killtarget", FOFS( killtarget ), F_LSTRING },
	{ "message", FOFS( message ), F_LSTRING },
	{ "helpmessage", FOFS( helpmessage ), F_LSTRING },
	{ "team", FOFS( team ), F_LSTRING },
	{ "wait", FOFS( wait ), F_FLOAT },
	{ "delay", FOFS( delay ), F_FLOAT },
	{ "style", FOFS( style ), F_INT },
	{ "count", FOFS( count ), F_INT },
	{ "health", FOFS( health ), F_FLOAT },
	{ "sounds", FOFS( sounds ), F_LSTRING },
	{ "light", FOFS( light ), F_FLOAT },
	{ "color", FOFS( color ), F_VECTOR },
	{ "dmg", FOFS( dmg ), F_INT },
	{ "angles", FOFS( s.angles ), F_VECTOR },
	{ "mangle", FOFS( s.angles ), F_VECTOR },
	{ "angle", FOFS( s.angles ), F_ANGLEHACK },
	{ "mass", FOFS( mass ), F_INT },
	{ "attenuation", FOFS( attenuation ), F_FLOAT },
	{ "map", FOFS( map ), F_LSTRING },
	{ "random", FOFS( random ), F_FLOAT },

	// temp spawn vars -- only valid when the spawn function is called
	{ "lip", STOFS( lip ), F_INT, FFL_SPAWNTEMP },
	{ "distance", STOFS( distance ), F_INT, FFL_SPAWNTEMP },
	{ "radius", STOFS( radius ), F_FLOAT, FFL_SPAWNTEMP },
	{ "roll", STOFS( roll ), F_FLOAT, FFL_SPAWNTEMP },
	{ "height", STOFS( height ), F_INT, FFL_SPAWNTEMP },
	{ "phase", STOFS( phase ), F_FLOAT, FFL_SPAWNTEMP },
	{ "noise", STOFS( noise ), F_LSTRING, FFL_SPAWNTEMP },
	{ "noise_start", STOFS( noise_start ), F_LSTRING, FFL_SPAWNTEMP },
	{ "noise_stop", STOFS( noise_stop ), F_LSTRING, FFL_SPAWNTEMP },
	{ "pausetime", STOFS( pausetime ), F_FLOAT, FFL_SPAWNTEMP },
	{ "item", STOFS( item ), F_LSTRING, FFL_SPAWNTEMP },
	{ "gravity", STOFS( gravity ), F_LSTRING, FFL_SPAWNTEMP },
	{ "music", STOFS( music ), F_LSTRING, FFL_SPAWNTEMP },
	{ "fov", STOFS( fov ), F_FLOAT, FFL_SPAWNTEMP },
	{ "nextmap", STOFS( nextmap ), F_LSTRING, FFL_SPAWNTEMP },
	{ "notsingle", STOFS( notsingle ), F_INT, FFL_SPAWNTEMP },
	{ "notteam", STOFS( notteam ), F_INT, FFL_SPAWNTEMP },
	{ "notfree", STOFS( notfree ), F_INT, FFL_SPAWNTEMP },
	{ "notduel", STOFS( notduel ), F_INT, FFL_SPAWNTEMP },
	{ "noents", STOFS( noents ), F_INT, FFL_SPAWNTEMP },
	{ "gameteam", STOFS( gameteam ), F_INT, FFL_SPAWNTEMP },
	{ "weight", STOFS( weight ), F_INT, FFL_SPAWNTEMP },
	{ "scale", STOFS( scale ), F_FLOAT, FFL_SPAWNTEMP },
	{ "gametype", STOFS( gametype ), F_LSTRING, FFL_SPAWNTEMP },
	{ "not_gametype", STOFS( not_gametype ), F_LSTRING, FFL_SPAWNTEMP },
	{ "debris1", STOFS( debris1 ), F_LSTRING, FFL_SPAWNTEMP },
	{ "debris2", STOFS( debris2 ), F_LSTRING, FFL_SPAWNTEMP },
	{ "shaderName", STOFS( shaderName ), F_LSTRING, FFL_SPAWNTEMP },
	{ "size", STOFS( size ), F_INT, FFL_SPAWNTEMP },
	{ "colorCorrection", STOFS( colorCorrection ), F_LSTRING, FFL_SPAWNTEMP },

	{ NULL, 0, F_INT, 0 }
};


typedef struct
{
	const char *name;
	void ( *spawn )( edict_t *ent );
} spawn_t;

static void SP_worldspawn( edict_t *ent );

spawn_t spawns[] = {
	{ "info_player_start", SP_info_player_start },
	{ "info_player_deathmatch", SP_info_player_deathmatch },
	{ "info_player_intermission", SP_info_player_intermission },

	{ "func_plat", SP_func_plat },
	{ "func_button", SP_func_button },
	{ "func_door", SP_func_door },
	{ "func_door_rotating", SP_func_door_rotating },
	{ "func_door_secret", SP_func_door_secret },
	{ "func_water", SP_func_water },
	{ "func_rotating", SP_func_rotating },
	{ "func_train", SP_func_train },
	{ "func_timer", SP_func_timer },
	{ "func_conveyor", SP_func_conveyor },
	{ "func_wall", SP_func_wall },
	{ "func_object", SP_func_object },
	{ "func_explosive", SP_func_explosive },
	{ "func_killbox", SP_func_killbox },
	{ "func_static", SP_func_static },
	{ "func_bobbing", SP_func_bobbing },
	{ "func_pendulum", SP_func_pendulum },

	{ "trigger_always", SP_trigger_always },
	{ "trigger_once", SP_trigger_once },
	{ "trigger_multiple", SP_trigger_multiple },
	{ "trigger_relay", SP_trigger_relay },
	{ "trigger_push", SP_trigger_push },
	{ "trigger_hurt", SP_trigger_hurt },
	{ "trigger_counter", SP_trigger_counter },
	{ "trigger_elevator", SP_trigger_elevator },
	{ "trigger_gravity", SP_trigger_gravity },

	{ "target_temp_entity", SP_target_temp_entity },
	{ "target_speaker", SP_target_speaker },
	{ "target_explosion", SP_target_explosion },
	{ "target_crosslevel_trigger", SP_target_crosslevel_trigger },
	{ "target_crosslevel_target", SP_target_crosslevel_target },
	{ "target_laser", SP_target_laser },
	{ "target_lightramp", SP_target_lightramp },
	{ "target_string", SP_target_string },
	{ "target_location", SP_target_location },
	{ "target_position", SP_target_position },
	{ "target_print", SP_target_print },
	{ "target_give", SP_target_give },
	{ "target_push", SP_info_notnull },
	{ "target_changelevel", SP_target_changelevel },
	{ "target_relay", SP_target_relay },
	{ "target_delay", SP_target_delay },
	{ "target_teleporter", SP_target_teleporter },
	{ "target_kill", SP_target_kill },

	{ "worldspawn", SP_worldspawn },

	{ "light", SP_light },
	{ "light_mine1", SP_light_mine },
	{ "info_null", SP_info_null },
	{ "func_group", SP_info_null },
	{ "info_notnull", SP_info_notnull },
	{ "info_camp", SP_info_camp },
	{ "path_corner", SP_path_corner },

	{ "misc_teleporter_dest", SP_misc_teleporter_dest },

	{ "trigger_teleport", SP_trigger_teleport },
	{ "info_teleport_destination", SP_info_teleport_destination },

	{ "misc_model", SP_misc_model },
	{ "misc_portal_surface", SP_misc_portal_surface },
	{ "misc_portal_camera", SP_misc_portal_camera },
	{ "misc_skyportal", SP_skyportal },
	{ "props_skyportal", SP_skyportal },
	{ "misc_particles", SP_misc_particles },
	{ "misc_video_speaker", SP_misc_video_speaker },

	{ NULL, NULL }
};

static const gsitem_t *G_ItemForEntity( edict_t *ent ) {
	const gsitem_t *item;

	// check item spawn functions
	if( ( item = GS_FindItemByClassname( ggs, ent->classname ) ) != NULL ) {
		return item;
	}

	return NULL;
}

/*
* G_GametypeFilterMatch
*
* Returns true if there's a direct match
*/
static bool G_GametypeFilterMatch( const char *filter ) {
	const wsw::CharLookup separators( ( wsw::StringView( ", " ) ) );
	wsw::StringSplitter splitter( ( wsw::StringView( filter ) ) );
	const wsw::StringView gametypeView( ggs->gametypeName );
	while( const auto maybeToken = splitter.getNext( separators ) ) {
		if( maybeToken->equalsIgnoreCase( gametypeView ) ) {
			return true;
		}
	}
	return false;
}

/*
* G_CanSpawnEntity
*/
static bool G_CanSpawnEntity( edict_t *ent ) {
	const gsitem_t *item;

	if( ent == world ) {
		return true;
	}

	if( !GS_TeamBasedGametype( *ggs ) && st.notfree ) {
		return false;
	}
	if( ( GS_TeamBasedGametype( *ggs ) && ( GS_MaxPlayersInTeam( *ggs ) == 1 ) ) && ( st.notduel || st.notfree ) ) {
		return false;
	}
	if( ( GS_TeamBasedGametype( *ggs ) && ( GS_MaxPlayersInTeam( *ggs ) != 1 ) ) && st.notteam ) {
		return false;
	}

	// check for Q3TA-style inhibition key
	if( st.gametype ) {
		if( !G_GametypeFilterMatch( st.gametype ) ) {
			return false;
		}
	}
	if( st.not_gametype ) {
		if( G_GametypeFilterMatch( st.not_gametype ) ) {
			return false;
		}
	}

	if( ( item = G_ItemForEntity( ent ) ) != NULL ) {
		// not pickable items aren't either spawnable
		if( !( item->flags & ITFLAG_PICKABLE ) ) {
			return false;
		}

		if( !G_Gametype_CanSpawnItem( item ) ) {
			return false;
		}
	}

	return true;
}

/*
* G_CallSpawn
*
* Finds the spawn function for the entity and calls it
*/
bool G_CallSpawn( edict_t *ent ) {
	spawn_t *s;
	const gsitem_t *item;

	if( !ent->classname ) {
		if( developer->integer ) {
			G_Printf( "G_CallSpawn: NULL classname\n" );
		}
		return false;
	}

	if( ( item = G_ItemForEntity( ent ) ) != NULL ) {
		SpawnItem( ent, item );
		return true;
	}

	// check normal spawn functions
	for( s = spawns; s->name; s++ ) {
		if( !Q_stricmp( s->name, ent->classname ) ) {
			s->spawn( ent );
			return true;
		}
	}

	// see if there's a spawn definition in the gametype scripts
	if( G_asCallMapEntitySpawnScript( ent->classname, ent ) ) {
		return true; // handled by the script
	}

	if( sv_cheats->integer || developer->integer ) { // mappers load their maps with devmap
		G_Printf( "%s doesn't have a spawn function\n", ent->classname );
	}

	return false;
}

/*
* G_GetEntitySpawnKey
*/
const char *G_GetEntitySpawnKey( const char *key, edict_t *self ) {
	static char value[MAX_TOKEN_CHARS];
	char keyname[MAX_TOKEN_CHARS];
	char *com_token;
	const char *data = NULL;

	value[0] = 0;

	if( self ) {
		data = self->spawnString;
	}

	if( data && data[0] && key && key[0] ) {
		// go through all the dictionary pairs
		while( 1 ) {
			// parse key
			com_token = COM_Parse( &data );
			if( com_token[0] == '}' ) {
				break;
			}

			if( !data ) {
				G_Error( "G_GetEntitySpawnKey: EOF without closing brace" );
			}

			Q_strncpyz( keyname, com_token, sizeof( keyname ) );

			// parse value
			com_token = COM_Parse( &data );
			if( !data ) {
				G_Error( "G_GetEntitySpawnKey: EOF without closing brace" );
			}

			if( com_token[0] == '}' ) {
				G_Error( "G_GetEntitySpawnKey: closing brace without data" );
			}

			// key names with a leading underscore are used for utility comments and are immediately discarded
			if( keyname[0] == '_' ) {
				continue;
			}

			if( !Q_stricmp( key, keyname ) ) {
				Q_strncpyz( value, com_token, sizeof( value ) );
				break;
			}
		}
	}

	return value;
}

/*
* ED_NewString
*/
static char *ED_NewString( const char *string ) {
	char *newb, *new_p;
	size_t i, l;

	l = strlen( string ) + 1;
	newb = &level.map_parsed_ents[level.map_parsed_len];
	level.map_parsed_len += l;

	new_p = newb;

	for( i = 0; i < l; i++ ) {
		if( string[i] == '\\' && i < l - 1 ) {
			i++;
			if( string[i] == 'n' ) {
				*new_p++ = '\n';
			} else {
				*new_p++ = '/';
				*new_p++ = string[i];
			}
		} else {
			*new_p++ = string[i];
		}
	}

	*new_p = '\0';
	return newb;
}

/*
* ED_ParseField
*
* Takes a key/value pair and sets the binary values
* in an edict
*/
static void ED_ParseField( char *key, char *value, edict_t *ent ) {
	const field_t *f;
	uint8_t *b;
	float v;
	vec3_t vec;

	for( f = fields; f->name; f++ ) {
		if( !Q_stricmp( f->name, key ) ) {
			// found it
			if( f->flags & FFL_SPAWNTEMP ) {
				b = (uint8_t *)&st;
			} else {
				b = (uint8_t *)ent;
			}

			switch( f->type ) {
				case F_LSTRING:
					*(char **)( b + f->ofs ) = ED_NewString( value );
					break;
				case F_VECTOR:
					sscanf( value, "%f %f %f", &vec[0], &vec[1], &vec[2] );
					( (float *)( b + f->ofs ) )[0] = vec[0];
					( (float *)( b + f->ofs ) )[1] = vec[1];
					( (float *)( b + f->ofs ) )[2] = vec[2];
					break;
				case F_INT:
					*(int *)( b + f->ofs ) = atoi( value );
					break;
				case F_FLOAT:
					*(float *)( b + f->ofs ) = atof( value );
					break;
				case F_ANGLEHACK:
					v = atof( value );
					( (float *)( b + f->ofs ) )[0] = 0;
					( (float *)( b + f->ofs ) )[1] = v;
					( (float *)( b + f->ofs ) )[2] = 0;
					break;
				case F_IGNORE:
					break;
				default:
					break; // FIXME: Should this be error?
			}
			return;
		}
	}

	if( developer->integer ) {
		G_Printf( "%s is not a field\n", key );
	}
}

/*
* ED_ParseEdict
*
* Parses an edict out of the given string, returning the new position
* ed should be a properly initialized empty edict.
*/
static char *ED_ParseEdict( char *data, edict_t *ent ) {
	bool init;
	char keyname[256];
	char *com_token;

	init = false;
	memset( &st, 0, sizeof( st ) );
	level.spawning_entity = ent;

	// go through all the dictionary pairs
	while( 1 ) {
		// parse key
		com_token = COM_Parse( &data );
		if( com_token[0] == '}' ) {
			break;
		}
		if( !data ) {
			G_Error( "ED_ParseEntity: EOF without closing brace" );
		}

		Q_strncpyz( keyname, com_token, sizeof( keyname ) );

		// parse value
		com_token = COM_Parse( &data );
		if( !data ) {
			G_Error( "ED_ParseEntity: EOF without closing brace" );
		}

		if( com_token[0] == '}' ) {
			G_Error( "ED_ParseEntity: closing brace without data" );
		}

		init = true;

		// keynames with a leading underscore are used for utility comments,
		// and are immediately discarded by quake
		if( keyname[0] == '_' ) {
			continue;
		}

		ED_ParseField( keyname, com_token, ent );
	}

	if( !init ) {
		ent->classname = NULL;
	}
	if( ent->classname && ent->helpmessage ) {
		ent->mapmessage_index = G_RegisterHelpMessage( ent->helpmessage );
	}

	return data;
}

/*
* G_FindTeams
*
* Chain together all entities with a matching team field.
*
* All but the first will have the FL_TEAMFOLLOWER flag set.
* All but the last will have the teamchain field set to the next one
*/
static void G_FindTeams( void ) {
	edict_t *e, *e2, *chain;
	int i, j;
	int c, c2;

	c = 0;
	c2 = 0;
	for( i = 1, e = game.edicts + i; i < game.numentities; i++, e++ ) {
		if( !e->r.inuse ) {
			continue;
		}
		if( !e->team ) {
			continue;
		}
		if( e->flags & FL_TEAMFOLLOWER ) {
			continue;
		}
		chain = e;
		e->teamleader = e;
		c++;
		c2++;
		for( j = i + 1, e2 = e + 1; j < game.numentities; j++, e2++ ) {
			if( !e2->r.inuse ) {
				continue;
			}
			if( !e2->team ) {
				continue;
			}
			if( e2->flags & FL_TEAMFOLLOWER ) {
				continue;
			}
			if( !strcmp( e->team, e2->team ) ) {
				c2++;
				chain->teamchain = e2;
				e2->teamleader = e;
				chain = e2;
				e2->flags |= FL_TEAMFOLLOWER;
			}
		}
	}

	if( developer->integer ) {
		G_Printf( "%i teams with %i entities\n", c, c2 );
	}
}

void G_PrecacheMedia( void ) {
	//
	// MODELS
	//

	// THIS ORDER MUST MATCH THE DEFINES IN gs_public.h
	// you can add more, max 255

	SV_ModelIndex( "#gunblade/gunblade.md3" );      // WEAP_GUNBLADE
	SV_ModelIndex( "#machinegun/machinegun.md3" );    // WEAP_MACHINEGUN
	SV_ModelIndex( "#riotgun/riotgun.md3" );        // WEAP_RIOTGUN
	SV_ModelIndex( "#glauncher/glauncher.md3" );    // WEAP_GRENADELAUNCHER
	SV_ModelIndex( "#rlauncher/rlauncher.md3" );    // WEAP_ROCKETLAUNCHER
	SV_ModelIndex( "#plasmagun/plasmagun.md3" );    // WEAP_PLASMAGUN
	SV_ModelIndex( "#lasergun/lasergun.md3" );      // WEAP_LASERGUN
	SV_ModelIndex( "#electrobolt/electrobolt.md3" ); // WEAP_ELECTROBOLT
	SV_ModelIndex( "#shockwave/shockwave.md3");     // WEAP_SHOCKWAVE
	SV_ModelIndex( "#instagun/instagun.md3" );      // WEAP_INSTAGUN

	//-------------------

	// precache our basic player models, they are just a very few
	SV_ModelIndex( "$models/players/bigvic" );

	SV_SkinIndex( "models/players/bigvic/default" );

	// FIXME: Temporarily use normal gib until the head is fixed
	SV_ModelIndex( "models/objects/gibs/illuminati1/illuminati1.md3" );

	//
	// SOUNDS
	//

	// jalfixme : most of these sounds can be played from the clients

	SV_SoundIndex( S_WORLD_WATER_IN );    // feet hitting water
	SV_SoundIndex( S_WORLD_WATER_OUT );       // feet leaving water
	SV_SoundIndex( S_WORLD_UNDERWATER );

	SV_SoundIndex( S_WORLD_SLIME_IN );
	SV_SoundIndex( S_WORLD_SLIME_OUT );
	SV_SoundIndex( S_WORLD_UNDERSLIME );

	SV_SoundIndex( S_WORLD_LAVA_IN );
	SV_SoundIndex( S_WORLD_LAVA_OUT );
	SV_SoundIndex( S_WORLD_UNDERLAVA );

	SV_SoundIndex( va( S_PLAYER_BURN_1_to_2, 1 ) );
	SV_SoundIndex( va( S_PLAYER_BURN_1_to_2, 2 ) );

	//wsw: pb disable unreferenced sounds
	//SV_SoundIndex (S_LAND);				// landing thud
	SV_SoundIndex( S_HIT_WATER );

	SV_SoundIndex( S_WEAPON_NOAMMO );

	// announcer

	// readyup
	SV_SoundIndex( S_ANNOUNCER_READY_UP_POLITE );
	SV_SoundIndex( S_ANNOUNCER_READY_UP_PISSEDOFF );

	// countdown
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_GET_READY_TO_FIGHT_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_GET_READY_TO_FIGHT_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_READY_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_READY_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 1, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 3, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 1, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 3, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_FIGHT_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_COUNTDOWN_FIGHT_1_to_2, 2 ) );

	// postmatch
	SV_SoundIndex( va( S_ANNOUNCER_POSTMATCH_GAMEOVER_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_POSTMATCH_GAMEOVER_1_to_2, 2 ) );

	// timeout
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_MATCH_RESUMED_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_MATCH_RESUMED_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEOUT_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEOUT_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEIN_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_TIMEOUT_TIMEIN_1_to_2, 2 ) );

	// callvote
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_CALLED_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_CALLED_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_FAILED_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_FAILED_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_PASSED_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_CALLVOTE_PASSED_1_to_2, 2 ) );
	SV_SoundIndex( S_ANNOUNCER_CALLVOTE_VOTE_NOW );

	// overtime
	SV_SoundIndex( S_ANNOUNCER_OVERTIME_GOING_TO_OVERTIME );
	SV_SoundIndex( S_ANNOUNCER_OVERTIME_OVERTIME );
	SV_SoundIndex( va( S_ANNOUNCER_OVERTIME_SUDDENDEATH_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_OVERTIME_SUDDENDEATH_1_to_2, 2 ) );

	// score
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_TAKEN_LEAD_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_TAKEN_LEAD_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_LOST_LEAD_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_LOST_LEAD_1_to_2, 2 ) );
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_TIED_LEAD_1_to_2, 1 ) );
	SV_SoundIndex( va( S_ANNOUNCER_SCORE_TIED_LEAD_1_to_2, 2 ) );

	if( GS_TeamBasedGametype( *ggs ) ) {
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TAKEN_LEAD_1_to_2, 1 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TAKEN_LEAD_1_to_2, 2 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_LOST_LEAD_1_to_2, 1 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_LOST_LEAD_1_to_2, 2 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TIED_LEAD_1_to_2, 1 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TIED_LEAD_1_to_2, 2 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TIED_LEAD_1_to_2, 1 ) );
		SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_TIED_LEAD_1_to_2, 2 ) );

		//SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_1_to_4_TAKEN_LEAD_1_to_2, 3, 1 ) );
		//SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_1_to_4_TAKEN_LEAD_1_to_2, 3, 2 ) );
		//SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_1_to_4_TAKEN_LEAD_1_to_2, 4, 1 ) );
		//SV_SoundIndex( va( S_ANNOUNCER_SCORE_TEAM_1_to_4_TAKEN_LEAD_1_to_2, 4, 2 ) );
	}

	//
	// LIGHTSTYLES
	//
	SV_SetConfigString( CS_LIGHTS + 0, LS_NORMAL );
	SV_SetConfigString( CS_LIGHTS + 1, LS_FLICKER1 );
	SV_SetConfigString( CS_LIGHTS + 2, LS_SLOW_STRONG_PULSE );
	SV_SetConfigString( CS_LIGHTS + 3, LS_CANDLE1 );
	SV_SetConfigString( CS_LIGHTS + 4, LS_FAST_STROBE );
	SV_SetConfigString( CS_LIGHTS + 5, LS_GENTLE_PULSE_1 );
	SV_SetConfigString( CS_LIGHTS + 6, LS_FLICKER2 );
	SV_SetConfigString( CS_LIGHTS + 7, LS_CANDLE2 );
	SV_SetConfigString( CS_LIGHTS + 8, LS_CANDLE3 );
	SV_SetConfigString( CS_LIGHTS + 9, LS_SLOW_STROBE );
	SV_SetConfigString( CS_LIGHTS + 10, LS_FLUORESCENT_FLICKER );
	SV_SetConfigString( CS_LIGHTS + 11, LS_SLOW_PULSE_NOT_FADE );

	// styles 32-62 are assigned by the light program for switchable lights
	SV_SetConfigString( CS_LIGHTS + 63, "a" );
}

/*
* G_FreeEntities
*/
static void G_FreeEntities( void ) {
	int i;

	if( !level.time ) {
		memset( game.edicts, 0, game.maxentities * sizeof( game.edicts[0] ) );
	} else {
		G_FreeEdict( world );
		for( i = ggs->maxclients + 1; i < game.maxentities; i++ ) {
			if( game.edicts[i].r.inuse ) {
				G_FreeEdict( game.edicts + i );
			}
		}
	}

	game.numentities = ggs->maxclients + 1;
}

/*
* G_SpawnEntities
*/
static void G_SpawnEntities( void ) {
	int i;
	edict_t *ent;
	char *token;
	const gsitem_t *item;
	char *entities;

	game.levelSpawnCount++;
	level.spawnedTimeStamp = game.realtime;
	level.canSpawnEntities = true;

	G_InitBodyQueue(); // reserve some spots for dead player bodies

	entities = level.mapString;
	level.map_parsed_ents[0] = 0;
	level.map_parsed_len = 0;

	i = 0;
	ent = NULL;
	while( 1 ) {
		level.spawning_entity = NULL;

		// parse the opening brace
		token = COM_Parse( &entities );
		if( !entities ) {
			break;
		}
		if( token[0] != '{' ) {
			G_Error( "G_SpawnMapEntities: found %s when expecting {", token );
		}

		if( !ent ) {
			ent = world;
			G_InitEdict( world );
		} else {
			ent = G_Spawn();
		}

		ent->spawnString = entities; // keep track of string definition of this entity

		entities = ED_ParseEdict( entities, ent );
		if( !ent->classname ) {
			i++;
			G_FreeEdict( ent );
			continue;
		}

		if( !G_CanSpawnEntity( ent ) ) {
			i++;
			G_FreeEdict( ent );
			continue;
		}

		if( !G_CallSpawn( ent ) ) {
			i++;
			G_FreeEdict( ent );
			continue;
		}

		// check whether an item is allowed to spawn
		if( ( item = ent->item ) ) {
			// not pickable items aren't spawnable
			if( item->flags & ITFLAG_PICKABLE ) {
				if( G_Gametype_CanSpawnItem( item ) ) {
					// override entity's classname with whatever item specifies
					ent->classname = item->classname;
					PrecacheItem( item );
					continue;
				}
			}

			i++;
			G_FreeEdict( ent );
			continue;
		}
	}

	// is the parsing string sane?
	assert( level.map_parsed_len < level.mapStrlen );
	level.map_parsed_ents[level.map_parsed_len] = 0;

	G_FindTeams();

	// make sure server got the edicts data
	SV_LocateEntities( game.edicts, sizeof( game.edicts[0] ), game.numentities, game.maxentities );

	// items need brush model entities spawned before they are linked
	G_Items_FinishSpawningItems();
}

/*
* G_InitLevel
*
* Creates a server's entity / program execution context by
* parsing textual entity definitions out of an ent file.
*/
void G_InitLevel( char *mapname, char *entities, int entstrlen, int64_t levelTime, int64_t serverTime, int64_t realTime ) {
	char *mapString = NULL;
	char name[MAX_QPATH];
	int i;

	G_asGarbageCollect( true );

	GT_asCallShutdown();
	G_asCallMapExit();

	AI_BeforeLevelLevelScriptShutdown();

	G_asShutdownMapScript();
	GT_asShutdownScript();

	AI_AfterLevelScriptShutdown();

	G_FreeCallvotes();

	game.serverTime = serverTime;
	game.realtime = realTime;

	GClip_ClearWorld(); // clear areas links

	if( !entities ) {
		G_Error( "G_SpawnLevel: NULL entities string\n" );
	}

	// make a copy of the raw entities string so it's not freed with the pool
	mapString = ( char * )Q_malloc( entstrlen + 1 );
	memcpy( mapString, entities, entstrlen );
	Q_strncpyz( name, mapname, sizeof( name ) );

	// clear old data

	G_StringPoolInit();

	memset( &level, 0, sizeof( level_locals_t ) );
	memset( &ggs->gameState, 0, sizeof( ggs->gameState ) );

	level.time = levelTime;
	level.gravity = g_gravity->value;

	// get the strings back
	Q_strncpyz( level.mapname, name, sizeof( level.mapname ) );
	level.mapString = ( char * )Q_malloc( entstrlen + 1 );
	level.mapStrlen = entstrlen;
	memcpy( level.mapString, mapString, entstrlen );
	Q_free( mapString );
	mapString = NULL;

	// make a copy of the raw entities string for parsing
	level.map_parsed_ents = ( char * )Q_malloc( entstrlen + 1 );
	level.map_parsed_ents[0] = 0;

	G_FreeEntities();

	// link client fields on player ents
	for( i = 0; i < ggs->maxclients; i++ ) {
		game.edicts[i + 1].s.number = i + 1;
		game.edicts[i + 1].r.client = &game.clients[i];
		game.edicts[i + 1].r.inuse = ( G_GetClientState( i ) >= CS_CONNECTED ) ? true : false;
		game.clients[i].resetLevelState();
		game.clients[i].levelTimestamp = level.time;
	}

	// initialize game subsystems
	SV_SetConfigString( CS_MAPNAME, level.mapname );
	SV_SetConfigString( CS_SKYBOX, "" );
	SV_SetConfigString( CS_AUDIOTRACK, "" );
	SV_SetConfigString( CS_STATNUMS, va( "%i %i %i", STAT_SCORE, STAT_HEALTH, STAT_LAST_KILLER ) );
	SV_SetConfigString( CS_POWERUPEFFECTS, va( "%i %i %i %i", EF_QUAD, EF_SHELL, EF_CARRIER, EF_REGEN ) );
	SV_SetConfigString( CS_SCOREBOARD_ASSETS, "" );
	SV_SetConfigString( CS_SCOREBOARD_SCHEMA, "" );
	SV_SetConfigString( CS_MATCHNAME, "" );
	SV_SetConfigString( CS_MATCHSCORE, "" );
	SV_SetConfigString( CS_GAMETYPE_OPTIONS_TITLE, "" );

	// reset gametype options
	for( i = 0; i < MAX_GAMETYPE_OPTIONS; i++ ) {
		SV_SetConfigString( CS_GAMETYPE_OPTIONS + i, "" );
	}

	// reset map messages
	for( i = 0; i < MAX_HELPMESSAGES; i++ ) {
		SV_SetConfigString( CS_HELPMESSAGES + i, "" );
	}

	ClientCommandsHandler::shutdown();
	for( i = 0; i < MAX_HELPMESSAGES; i++ ) {
		SV_SetConfigString( CS_GAMECOMMANDS + i, "" );
	}

	ClientCommandsHandler::init();

	G_SpawnQueue_Init();
	G_Teams_Init();

	// load map script
	G_asLoadMapScript( level.mapname );
	G_Gametype_Init();

	G_CallVotes_Init();

	// ch : this would be the location to "transfer ratings"
	G_PrecacheItems(); // set configstrings for items (gametype must be initialized)
	G_PrecacheMedia();
	ClientCommandsHandler::instance()->precacheCommands(); // adding commands after this point won't update them to the client
	AI_InitLevel();

	// start spawning entities
	G_SpawnEntities();

	//
	// initialize game subsystems which require entities initialized
	//

	// Clear dangling gametype-specific stats
	for( i = 0; i < ggs->maxclients; ++i ) {
		if( Client *client = game.edicts[i + 1].r.client ) {
			std::fill( client->ps.stats + GS_GAMETYPE_STATS_START, client->ps.stats + GS_GAMETYPE_STATS_END, 0 );
		}
	}

	// call gametype specific
	GT_asCallSpawn();

	// call map specific
	G_asCallMapInit();

	// always start in warmup match state and let the thinking code
	// revert it to wait state if empty ( so gametype based item masks are setup )
	G_Match_LaunchState( MATCH_STATE_WARMUP );

	G_asGarbageCollect( true );

	SV_DispatchServerCmd( nullptr, "reloadcommands" );
	SV_DispatchServerCmd( nullptr, "reloadoptions" );
}

void G_ResetLevel( void ) {
	int i;

	G_FreeEdict( world );
	for( i = ggs->maxclients + 1; i < game.maxentities; i++ ) {
		if( game.edicts[i].r.inuse ) {
			G_FreeEdict( game.edicts + i );
		}
	}

	G_SpawnEntities();

	// call gametype specific
	GT_asCallSpawn();

	// call map specific
	G_asCallMapInit();

	SV_DispatchServerCmd( nullptr, "reloadoptions" );
}

bool G_RespawnLevel( void ) {
	G_InitLevel( level.mapname, level.mapString, level.mapStrlen, level.time, game.serverTime, game.realtime );
	return true;
}

//===================================================================

//QUAKED worldspawn (0 0 0) ?
//
//Only used for the world.
//"sky"	environment map name
//"skyaxis"	vector axis for rotating sky
//"skyrotate"	speed of rotation in degrees/second
//"sounds"	music cd track number
//"gravity"	800 is default gravity
//"message"	text to print at user logon
//========================
static void SP_worldspawn( edict_t *ent ) {
	ent->movetype = MOVETYPE_PUSH;
	ent->r.solid = SOLID_YES;
	ent->r.inuse = true;       // since the world doesn't use G_Spawn()
	VectorClear( ent->s.origin );
	VectorClear( ent->s.angles );
	GClip_SetBrushModel( ent, "*0" ); // sets mins / maxs and modelindex 1
	G_PureModel( "*0" );

	if( st.nextmap ) {
		Q_strncpyz( level.nextmap, st.nextmap, sizeof( level.nextmap ) );
	}

	// make some data visible to the server
	/*
	message = trap_GetFullnameFromMapList( level.mapname );
	if( message && message[0] )
	    ent->message = Q_strdup( message );
	*/

	if( ent->message && ent->message[0] ) {
		SV_SetConfigString( CS_MESSAGE, ent->message );
		Q_strncpyz( level.level_name, ent->message, sizeof( level.level_name ) );
	} else {
		SV_SetConfigString( CS_MESSAGE, level.mapname );
		Q_strncpyz( level.level_name, level.mapname, sizeof( level.level_name ) );
	}

	// send music
	if( st.music ) {
		SV_SetConfigString( CS_AUDIOTRACK, st.music );
		SV_PureSound( st.music );
	}

	if( st.gravity ) {
		level.gravity = atof( st.gravity );
	}

	if( st.colorCorrection ) {
		level.colorCorrection = SV_ImageIndex( st.colorCorrection );
		ggs->gameState.stats[GAMESTAT_COLORCORRECTION] = level.colorCorrection;
	}
}
