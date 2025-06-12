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

// g_utils.c -- misc utility functions for game module

#include "g_local.h"
#include <common/facilities/wswfs.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/gs_qrespath.h>

//==============================================================================

#define STRINGPOOL_SIZE         1024 * 1024
#define STRINGPOOL_HASH_SIZE    32

typedef struct g_poolstring_s {
	char *buf;
	struct g_poolstring_s *hash_next;
} g_poolstring_t;

static uint8_t *g_stringpool;
static size_t g_stringpool_offset;
static g_poolstring_t *g_stringpool_hash[STRINGPOOL_HASH_SIZE];

/*
* G_StringPoolInit
*
* Preallocates a memory region to permanently store level strings
*/
void G_StringPoolInit( void ) {
	memset( g_stringpool_hash, 0, sizeof( g_stringpool_hash ) );

	g_stringpool = ( uint8_t * )Q_malloc( STRINGPOOL_SIZE );
	g_stringpool_offset = 0;
}

/*
* G_StringPoolHashKey
*/
static unsigned int G_StringPoolHashKey( const char *string ) {
	int i;
	unsigned int v;
	unsigned int c;

	v = 0;
	for( i = 0; string[i]; i++ ) {
		c = string[i];
		v = ( v + i ) * 37 + c;
	}

	return v % STRINGPOOL_HASH_SIZE;
}

/*
* G_RegisterLevelString
*
* Registers a unique string which is guaranteed to exist until the level reloads
*/
const char *_G_RegisterLevelString( const char *string, const char *filename, int fileline ) {
	size_t size;
	g_poolstring_t *ps;
	unsigned int hashkey;

	if( !string ) {
		return NULL;
	}
	if( !*string ) {
		return "";
	}

	size = strlen( string ) + 1;
	if( sizeof( *ps ) + size > STRINGPOOL_SIZE ) {
		G_Error( "G_RegisterLevelString: out of memory (str:%s at %s:%i)\n", string, filename, fileline );
		return NULL;
	}

	// find a matching registered string
	hashkey = G_StringPoolHashKey( string );
	for( ps = g_stringpool_hash[hashkey]; ps; ps = ps->hash_next ) {
		if( !strcmp( ps->buf, string ) ) {
			return ps->buf;
		}
	}

	// no match, register a new one
	ps = ( g_poolstring_t * )( g_stringpool + g_stringpool_offset );
	g_stringpool_offset += sizeof( *ps );

	ps->buf = ( char * )( g_stringpool + g_stringpool_offset );
	ps->hash_next = g_stringpool_hash[hashkey];
	g_stringpool_hash[hashkey] = ps;

	memcpy( ps->buf, string, size );
	g_stringpool_offset += size;

	return ps->buf;
}

//==============================================================================

/*
* G_AllocCreateNamesList
*/
char *G_AllocCreateNamesList( const char *path, const char *extension, const char separator ) {
	if( !path || !extension || ( extension[0] != '.' ) ) {
		return nullptr;
	}

	wsw::fs::SearchResultHolder searchResultHolder;
	auto maybeCallResult = searchResultHolder.findDirFiles( wsw::StringView( path ), wsw::StringView( extension ) );
	if( !maybeCallResult ) {
		return nullptr;
	}

	char *const result = (char *)Q_malloc( maybeCallResult->getNumFiles() * ( MAX_QPATH + 2 ) + 1 );
	char *p = result;
	for( const wsw::StringView &fileName: *maybeCallResult ) {
		fileName.copyTo( p, MAX_QPATH );
		COM_StripExtension( p );
		const auto len = std::strlen( p );
		p[len + 0] = separator;
		p[len + 1] = '\0';
		p += len + 1;
	}

	return result;
}

void G_ProjectSource( vec3_t point, vec3_t distance, vec3_t forward, vec3_t right, vec3_t result ) {
	result[0] = point[0] + forward[0] * distance[0] + right[0] * distance[1];
	result[1] = point[1] + forward[1] * distance[0] + right[1] * distance[1];
	result[2] = point[2] + forward[2] * distance[0] + right[2] * distance[1] + distance[2];
}


/*
* G_Find
*
* Searches all active entities for the next one that holds
* the matching string at fieldofs (use the FOFS() macro) in the structure.
*
* Searches beginning at the edict after from, or the beginning if NULL
* NULL will be returned if the end of the list is reached.
*
*/
edict_t *G_Find( edict_t *from, size_t fieldofs, const char *match ) {
	char *s;

	if( !from ) {
		from = world;
	} else {
		from++;
	}

	for(; from <= &game.edicts[game.numentities - 1]; from++ ) {
		if( !from->r.inuse ) {
			continue;
		}
		s = *(char **) ( (uint8_t *)from + fieldofs );
		if( !s ) {
			continue;
		}
		if( !Q_stricmp( s, match ) ) {
			return from;
		}
	}

	return NULL;
}

/*
* G_PickTarget
*
* Searches all active entities for the next one that holds
* the matching string at fieldofs (use the FOFS() macro) in the structure.
*
* Searches beginning at the edict after from, or the beginning if NULL
* NULL will be returned if the end of the list is reached.
*
*/
#define MAXCHOICES  8

edict_t *G_PickTarget( const char *targetname ) {
	edict_t *ent = NULL;
	int num_choices = 0;
	edict_t *choice[MAXCHOICES];

	if( !targetname ) {
		G_Printf( "G_PickTarget called with NULL targetname\n" );
		return NULL;
	}

	while( 1 ) {
		ent = G_Find( ent, FOFS( targetname ), targetname );
		if( !ent ) {
			break;
		}
		choice[num_choices++] = ent;
		if( num_choices == MAXCHOICES ) {
			break;
		}
	}

	if( !num_choices ) {
		G_Printf( "G_PickTarget: target %s not found\n", targetname );
		return NULL;
	}

	return choice[rand() % num_choices];
}



static void Think_Delay( edict_t *ent ) {
	G_UseTargets( ent, ent->activator );
	G_FreeEdict( ent );
}

/*
* G_UseTargets
*
* the global "activator" should be set to the entity that initiated the firing.
*
* If self.delay is set, a DelayedUse entity will be created that will actually
* do the SUB_UseTargets after that many seconds have passed.
*
* Centerprints any self.message to the activator.
*
* Search for (string)targetname in all entities that
* match (string)self.target and call their .use function
*
*/
void G_UseTargets( edict_t *ent, edict_t *activator ) {
	edict_t *t;

	//
	// check for a delay
	//
	if( ent->delay ) {
		// create a temp object to fire at a later time
		t = G_Spawn();
		t->classname = "delayed_use";
		t->nextThink = level.time + 1000 * ent->delay;
		t->think = Think_Delay;
		t->activator = activator;
		if( !activator ) {
			G_Printf( "Think_Delay with no activator\n" );
		}
		t->message = ent->message;
		t->target = ent->target;
		t->killtarget = ent->killtarget;
		return;
	}


	//
	// print the message
	//
	if( ent->message ) {
		G_CenterPrintMsg( activator, "%s", ent->message );

		if( ent->noise_index ) {
			G_Sound( activator, CHAN_AUTO, ent->noise_index, ATTN_NORM );
		} else {
			G_Sound( activator, CHAN_AUTO, SV_SoundIndex( S_WORLD_MESSAGE ), ATTN_NORM );
		}
	}

	//
	// set the help message
	//
	if( ent->helpmessage && ent->mapmessage_index <= MAX_HELPMESSAGES ) {
		G_SetPlayerHelpMessage( activator, ent->mapmessage_index );

		if( !ent->message ) {
			if( ent->noise_index ) {
				G_Sound( activator, CHAN_AUTO, ent->noise_index, ATTN_NORM );
			} else {
				G_Sound( activator, CHAN_AUTO, SV_SoundIndex( S_WORLD_MESSAGE ), ATTN_NORM );
			}
		}
	}

	//
	// kill killtargets
	//
	if( ent->killtarget ) {
		t = NULL;
		while( ( t = G_Find( t, FOFS( targetname ), ent->killtarget ) ) ) {
			G_FreeEdict( t );
			if( !ent->r.inuse ) {
				G_Printf( "entity was removed while using killtargets\n" );
				return;
			}
		}
	}

	//	G_Printf ("TARGET: activating %s\n", ent->target);

	//
	// fire targets
	//
	if( ent->target ) {
		t = NULL;
		while( ( t = G_Find( t, FOFS( targetname ), ent->target ) ) ) {
			if( t == ent ) {
				G_Printf( "WARNING: Entity used itself.\n" );
			} else {
				G_CallUse( t, ent, activator );
			}
			if( !ent->r.inuse ) {
				G_Printf( "entity was removed while using targets\n" );
				return;
			}
		}
	}
}


vec3_t VEC_UP       = { 0, -1, 0 };
vec3_t MOVEDIR_UP   = { 0, 0, 1 };
vec3_t VEC_DOWN     = { 0, -2, 0 };
vec3_t MOVEDIR_DOWN = { 0, 0, -1 };

void G_SetMovedir( vec3_t angles, vec3_t movedir ) {
	if( VectorCompare( angles, VEC_UP ) ) {
		VectorCopy( MOVEDIR_UP, movedir );
	} else if( VectorCompare( angles, VEC_DOWN ) ) {
		VectorCopy( MOVEDIR_DOWN, movedir );
	} else {
		AngleVectors( angles, movedir, NULL, NULL );
	}

	VectorClear( angles );
}


float vectoyaw( vec3_t vec ) {
	float yaw;

	if( vec[PITCH] == 0 ) {
		yaw = 0;
		if( vec[YAW] > 0 ) {
			yaw = 90;
		} else if( vec[YAW] < 0 ) {
			yaw = -90;
		}
	} else {
		yaw = RAD2DEG( atan2( vec[YAW], vec[PITCH] ) );
		if( yaw < 0 ) {
			yaw += 360;
		}
	}

	return yaw;
}

/*
* G_FreeEdict
*
* Marks the edict as free
*/
void G_FreeEdict( edict_t *ed ) {
	bool evt = ISEVENTENTITY( &ed->s );

	GClip_UnlinkEntity( ed );   // unlink from world

	AI_RemoveNavEntity( ed );
	G_FreeAI( ed );

	G_asReleaseEntityBehaviors( ed );

	memset( ed, 0, sizeof( *ed ) );
	ed->r.inuse = false;
	ed->s.number = ENTNUM( ed );
	ed->r.svflags = SVF_NOCLIENT;
	ed->scriptSpawned = false;

	if( !evt && ( level.spawnedTimeStamp != game.realtime ) ) {
		ed->freetime = game.realtime; // ET_EVENT or ET_SOUND don't need to wait to be reused
	}
}

/*
* G_InitEdict
*/
void G_InitEdict( edict_t *e ) {
	e->r.inuse = true;
	e->classname = NULL;
	e->gravity = 1.0;
	e->timeDelta = 0;
	e->deadflag = DEAD_NO;
	e->timeStamp = 0;
	e->scriptSpawned = false;

	memset( &e->s, 0, sizeof( entity_state_t ) );
	e->s.attenuation = ATTN_NORM;
	e->s.number = ENTNUM( e );

	G_asResetEntityBehaviors( e );

	// Reset AI intrinsic properties
	e->aiIntrinsicEnemyWeight = 0.0f;
	e->aiVisibilityDistance = 999999.9f;

	// mark all entities to not be sent by default
	e->r.svflags = SVF_NOCLIENT | (e->r.svflags & SVF_FAKECLIENT);

	// clear the old state data
	memset( &e->olds, 0, sizeof( e->olds ) );
	memset( &e->snap, 0, sizeof( e->snap ) );

	//wsw clean up the backpack counts
	memset( e->invpak, 0, sizeof( e->invpak ) );
}

/*
* G_Spawn
*
* Either finds a free edict, or allocates a new one.
* Try to avoid reusing an entity that was recently freed, because it
* can cause the client to think the entity morphed into something else
* instead of being removed and recreated, which can cause interpolated
* angles and bad trails.
*/
edict_t *G_Spawn( void ) {
	int i;
	edict_t *e, *freed;

	if( !level.canSpawnEntities ) {
		G_Printf( "WARNING: Spawning entity before map entities have been spawned\n" );
	}

	freed = NULL;
	e = &game.edicts[ggs->maxclients + 1];
	for( i = ggs->maxclients + 1; i < game.numentities; i++, e++ ) {
		if( e->r.inuse ) {
			continue;
		}

		// the first couple seconds of server time can involve a lot of
		// freeing and allocating, so relax the replacement policy
		if( e->freetime < level.spawnedTimeStamp + 2000 || game.realtime > e->freetime + 500 ) {
			G_InitEdict( e );
			return e;
		}

		// this is going to be our second chance to spawn an entity in case all free
		// entities have been freed only recently
		if( !freed ) {
			freed = e;
		}
	}

	if( i == game.maxentities ) {
		if( freed ) {
			G_InitEdict( freed );
			return freed;
		}
		G_Error( "G_Spawn: no free edicts" );
	}

	game.numentities++;

	SV_LocateEntities( game.edicts, sizeof( game.edicts[0] ), game.numentities, game.maxentities );

	G_InitEdict( e );

	return e;
}

/*
* G_AddEvent
*/
void G_AddEvent( edict_t *ent, int event, int parm, bool highPriority ) {
	if( !ent || ent == world || !ent->r.inuse ) {
		return;
	}
	if( !event ) {
		return;
	}

	int eventNum = ent->numEvents & 1;
	if( ent->eventPriority[eventNum] && !ent->eventPriority[( eventNum + 1 ) & 1] ) {
		eventNum = ( eventNum + 1 ) & 1; // prefer overwriting low priority events
	} else if( !highPriority && ent->eventPriority[eventNum] ) {
		return; // no low priority event to overwrite
	} else {
		ent->numEvents++; // numEvents is only used to vary the overwritten event

	}
	ent->s.events[eventNum] = event;
	ent->s.eventParms[eventNum] = parm;
	ent->eventPriority[eventNum] = highPriority;

	AI_RegisterEvent( ent, event, parm );
}

/*
* G_SpawnEvent
*/
edict_t *G_SpawnEvent( int event, int parm, vec3_t origin ) {
	edict_t *ent;

	ent = G_Spawn();
	ent->s.type = ET_EVENT;
	ent->r.solid = SOLID_NOT;
	ent->r.svflags &= ~SVF_NOCLIENT;
	if( origin != NULL ) {
		VectorCopy( origin, ent->s.origin );
	}
	G_AddEvent( ent, event, parm, true );

	GClip_LinkEntity( ent );

	return ent;
}

/*
* G_MorphEntityIntoEvent
*/
void G_MorphEntityIntoEvent( edict_t *ent, int event, int parm ) {
	ent->s.type = ET_EVENT;
	ent->r.solid = SOLID_NOT;
	ent->r.svflags &= ~SVF_PROJECTILE; // FIXME: Medar: should be remove all or remove this one elsewhere?
	ent->s.linearMovement = false;
	G_AddEvent( ent, event, parm, true );

	GClip_LinkEntity( ent );
}

/*
* G_InitMover
*/
void G_InitMover( edict_t *ent ) {
	ent->r.solid = SOLID_YES;
	ent->movetype = MOVETYPE_PUSH;
	ent->r.svflags &= ~SVF_NOCLIENT;

	GClip_SetBrushModel( ent, ent->model );
	G_PureModel( ent->model );

	if( ent->model2 ) {
		ent->s.modelindex2 = SV_ModelIndex( ent->model2 );
		G_PureModel( ent->model2 );
	}

	if( ent->light || !VectorCompare( ent->color, vec3_origin ) ) {
		int r, g, b, i;

		if( !ent->light ) {
			i = 100;
		} else {
			i = ent->light;
		}

		i /= 4;
		i = wsw::min( i, 255 );

		r = ent->color[0];
		if( r <= 1.0 ) {
			r *= 255;
		}
		Q_clamp( r, 0, 255 );

		g = ent->color[1];
		if( g <= 1.0 ) {
			g *= 255;
		}
		Q_clamp( g, 0, 255 );

		b = ent->color[2];
		if( b <= 1.0 ) {
			b *= 255;
		}
		Q_clamp( b, 0, 255 );

		ent->s.light = COLOR_RGBA( r, g, b, i );
	}
}

/*
* G_CallThink
*/
void G_CallThink( edict_t *ent ) {
	if( ent->think ) {
		ent->think( ent );
	} else if( ent->scriptSpawned && ent->asThinkFunc ) {
		G_asCallMapEntityThink( ent );
	} else if( developer->integer ) {
		G_Printf( "NULL ent->think in %s\n", ent->classname ? ent->classname : va( "'no classname. Entity type is %i", ent->s.type ) );
	}
}

/*
* G_CallTouch
*/
void G_CallTouch( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags ) {
	bool touched = false;

	if( self == other ) {
		return;
	}

	if( self->touch ) {
		touched = true;
		self->touch( self, other, plane, surfFlags );
	} else if( self->scriptSpawned && self->asTouchFunc ) {
		touched = true;
		G_asCallMapEntityTouch( self, other, plane, surfFlags );
	}

	if( touched && other->bot ) {
		AI_TouchedEntity( other, self );
	}
}

/*
* G_CallUse
*/
void G_CallUse( edict_t *self, edict_t *other, edict_t *activator ) {
	if( self->use ) {
		self->use( self, other, activator );
	} else if( self->scriptSpawned && self->asUseFunc ) {
		G_asCallMapEntityUse( self, other, activator );
	}
}

/*
* G_CallStop
*/
void G_CallStop( edict_t *self ) {
	if( self->stop ) {
		self->stop( self );
	} else if( self->scriptSpawned && self->asStopFunc ) {
		G_asCallMapEntityStop( self );
	}
}

/*
* G_CallPain
*/
void G_CallPain( edict_t *ent, edict_t *attacker, float kick, float damage ) {
	if( ent->bot ) {
		AI_Pain( ent, attacker, kick, damage );
	}

	if( ent->pain ) {
		ent->pain( ent, attacker, kick, damage );
	} else if( ent->scriptSpawned && ent->asPainFunc ) {
		G_asCallMapEntityPain( ent, attacker, kick, damage );
	}
}

/*
* G_CallDie
*/
void G_CallDie( edict_t *ent, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point ) {
	if( ent->die ) {
		ent->die( ent, inflictor, attacker, damage, point );
	} else if( ent->scriptSpawned && ent->asDieFunc ) {
		G_asCallMapEntityDie( ent, inflictor, attacker, damage, point );
	}
}


/*
* G_PlayerGender
* server doesn't know the model gender, so all are neutrals in console prints.
*/
int G_PlayerGender( edict_t *player ) {
	return GENDER_NEUTRAL;
}

/*
* G_PrintMsg
*
* NULL sends to all the message to all clients
*/
void G_PrintMsg( const edict_t *ent, const char *format, ... ) {
	char msg[MAX_STRING_CHARS];
	va_list argptr;
	char *s, *p;

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	// double quotes are bad
	p = msg;
	while( ( p = strchr( p, '\"' ) ) != NULL )
		*p = '\'';

	s = va( "pr \"%s\"", msg );

	if( !ent ) {
		// mirror at server console
		if( dedicated->integer ) {
			G_Printf( "%s", msg );
		}
		SV_DispatchGameCmd( NULL, s );
	} else {
		if( ent->r.inuse && ent->r.client ) {
			SV_DispatchGameCmd( ent, s );
		}
	}
}

void G_PrintChasersf( const edict_t *self, const char *format, ... ) {
	char msg[1024];
	va_list argptr;
	edict_t *ent;

	if( !self ) {
		return;
	}

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	for( ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
		if( ent->r.client->chase.active && ent->r.client->chase.target == ENTNUM( self ) ) {
			G_PrintMsg( ent, "%s", msg );
		}
	}
}

/*
* G_CenterPrintMsg
*
* NULL sends to all the message to all clients
*/
void G_CenterPrintMsg( const edict_t *ent, const char *format, ... ) {
	char msg[1024];
	char cmd[MAX_STRING_CHARS];
	va_list argptr;
	char *p;
	edict_t *other;

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	// double quotes are bad
	p = msg;
	while( ( p = strchr( p, '\"' ) ) != NULL )
		*p = '\'';

	Q_snprintfz( cmd, sizeof( cmd ), "cp \"%s\"", msg );
	SV_DispatchGameCmd( ent, cmd );

	if( ent != NULL ) {
		// add it to every player who's chasing this player
		for( other = game.edicts + 1; PLAYERNUM( other ) < ggs->maxclients; other++ ) {
			if( !other->r.client || !other->r.inuse || !other->r.client->chase.active ) {
				continue;
			}

			if( other->r.client->chase.target == ENTNUM( ent ) ) {
				SV_DispatchGameCmd( other, cmd );
			}
		}
	}
}

/*
* G_CenterPrintFormatMsg
*
* MUST be passed NULL as the last variadic argument
*
* NULL sends to all the message to all clients
*/
void G_CenterPrintFormatMsg( const edict_t *ent, int numVargs, const char *format, ... ) {
	int i;
	char cmd[MAX_STRING_CHARS];
	char arg_fmt[MAX_TOKEN_CHARS];
	va_list argptr;
	char *p, *arg_p;
	bool overflow = false;
	edict_t *other;

	if( !numVargs ) {
		// can't transmit formatted message with no arguments or
		// no strings to replace the placeholders
		return;
	}

	Q_strncpyz( cmd, "cpf ", sizeof( cmd ) );

	// double quotes are bad
	Q_strncpyz( arg_fmt, format, sizeof( arg_fmt ) );
	arg_p = arg_fmt;

	va_start( argptr, format );

	for( i = 0; i <= numVargs; i++ ) {
		size_t cmd_len;
		size_t arg_len;

		// double quotes are bad
		p = arg_p;
		if( !p ) {
			overflow = true;
			break;
		}

		while( ( p = strchr( p, '\"' ) ) != NULL )
			*p = '\'';

		cmd_len = strlen( cmd );
		arg_len = strlen( arg_p );
		if( arg_len > MAX_TOKEN_CHARS ) {
			overflow = true;
			break;
		}

		if( cmd_len + arg_len + 3 >= sizeof( cmd ) ) {
			overflow = true;
			break;
		}

		cmd[cmd_len + 0] = ' ';
		cmd[cmd_len + 1] = '"';
		memcpy( &cmd[cmd_len + 2], arg_p, arg_len );
		cmd[cmd_len + 2 + arg_len] = '"';
		cmd[cmd_len + 3 + arg_len] = '\0';

		arg_p = va_arg( argptr, char * );
	}

	va_end( argptr );

	if( overflow ) {
		// couldn't fit it all into the cmd buffer
		return;
	}

	SV_DispatchGameCmd( ent, cmd );

	if( ent != NULL ) {
		// add it to every player who's chasing this player
		for( other = game.edicts + 1; PLAYERNUM( other ) < ggs->maxclients; other++ ) {
			if( !other->r.client || !other->r.inuse || !other->r.client->chase.active ) {
				continue;
			}

			if( other->r.client->chase.target == ENTNUM( ent ) ) {
				SV_DispatchGameCmd( other, cmd );
			}
		}
	}
}


void G_Obituary( edict_t *victim, edict_t *attacker, int mod ) {
	if( victim && attacker ) {
		SV_DispatchGameCmd( NULL, va( "fra %i %i %i", (int)(victim - game.edicts), (int)(attacker - game.edicts), mod ) );
	}
}

/*
* G_UpdatePlayerMatchMsg
*
* Sends correct match msg to one client
* Must be called whenever client's team, ready status or chase mode changes
*/
void G_UpdatePlayerMatchMsg( edict_t *ent, bool force ) {
	matchmessage_t newmm;

	if( GS_MatchWaiting( *ggs ) ) {
		newmm = MATCHMESSAGE_WAITING_FOR_PLAYERS;
	} else if( GS_MatchState( *ggs ) > MATCH_STATE_PLAYTIME ) {
		newmm = MATCHMESSAGE_NONE;
	} else if( ent->s.team == TEAM_SPECTATOR ) {
		if( GS_HasChallengers( *ggs ) ) { // He is in the queue
			newmm = ( ent->r.client->queueTimeStamp ? MATCHMESSAGE_CHALLENGERS_QUEUE : MATCHMESSAGE_ENTER_CHALLENGERS_QUEUE );
		} else {
			newmm = ( ent->r.client->chase.active ? MATCHMESSAGE_NONE : MATCHMESSAGE_SPECTATOR_MODES );
		}
	} else {
		if( GS_MatchState( *ggs ) == MATCH_STATE_WARMUP ) {
			newmm = ( !level.ready[PLAYERNUM( ent )] ? MATCHMESSAGE_GET_READY : MATCHMESSAGE_NONE );
		} else {
			newmm = MATCHMESSAGE_NONE;
		}
	}

	if( newmm != ent->r.client->matchmessage || force ) {
		ent->r.client->matchmessage = newmm;
		SV_DispatchGameCmd( ent, va( "mm %i", newmm ) );
	}
}

/*
* G_UpdatePlayerMatchMsg
*
* Sends correct match msg to every client
* Must be called whenever match state changes
*/
void G_UpdatePlayersMatchMsgs( void ) {
	int i;
	edict_t *cl_ent;

	for( i = 0; i < ggs->maxclients; i++ ) {
		cl_ent = game.edicts + 1 + i;
		if( !cl_ent->r.inuse ) {
			continue;
		}
		G_UpdatePlayerMatchMsg( cl_ent );
	}
}

//==================================================
// MAP MESSAGES
//==================================================

/*
* G_RegisterHelpMessage
*/
unsigned G_RegisterHelpMessage( const char *str ) {
	unsigned i;

	if( !str || !*str ) {
		return 0;
	}

	for( i = 0; i < MAX_HELPMESSAGES; i++ ) {
		const char *cs = SV_GetConfigString( CS_HELPMESSAGES + i );
		if( !cs[0] ) {
			break;
		}
		if( !strcmp( cs, str ) ) {
			return i + 1;
		}
	}

	if( i < MAX_HELPMESSAGES ) {
		SV_SetConfigString( CS_HELPMESSAGES + i, str );
	}
	return i + 1;
}

/*
* G_SetPlayerHelpMessage
*/
void G_SetPlayerHelpMessage( edict_t *ent, unsigned index, bool force ) {
	if( index > MAX_HELPMESSAGES ) {
		return;
	}
	if( !ent || !ent->r.client ) {
		return;
	}

	if( index != ent->r.client->helpmessage || force ) {
		ent->r.client->helpmessage = index;
		SV_DispatchGameCmd( ent, va( "mapmsg %i", index ) );
	}
}

//==================================================
// SOUNDS
//==================================================

/*
* _G_SpawnSound
*/
static edict_t *_G_SpawnSound( int channel, int soundindex, float attenuation ) {
	edict_t *ent;

	if( attenuation <= 0.0f ) {
		attenuation = ATTN_NONE;
	}

	ent = G_Spawn();
	ent->r.svflags &= ~SVF_NOCLIENT;
	ent->r.svflags |= SVF_SOUNDCULL;
	ent->s.type = ET_SOUNDEVENT;
	ent->s.attenuation = attenuation;
	ent->s.channel = channel;
	ent->s.sound = soundindex;

	return ent;
}

/*
* G_Sound
*/
edict_t *G_Sound( edict_t *owner, int channel, int soundindex, float attenuation ) {
	edict_t *ent;

	if( !soundindex ) {
		return NULL;
	}

	if( owner == NULL || owner == world ) {
		attenuation = ATTN_NONE;
	} else if( ISEVENTENTITY( &owner->s ) ) {
		return NULL; // event entities can't be owner of sound entities

	}
	ent = _G_SpawnSound( channel, soundindex, attenuation );
	if( attenuation != ATTN_NONE ) {
		assert( owner );
		ent->s.ownerNum = owner->s.number;

		if( owner->s.solid != SOLID_BMODEL ) {
			VectorCopy( owner->s.origin, ent->s.origin );
		} else {
			VectorAdd( owner->r.mins, owner->r.maxs, ent->s.origin );
			VectorMA( owner->s.origin, 0.5f, ent->s.origin, ent->s.origin );
		}
	} else {
		ent->r.svflags |= SVF_BROADCAST;
	}

	GClip_LinkEntity( ent );
	return ent;
}

/*
* G_PositionedSound
*/
edict_t *G_PositionedSound( vec3_t origin, int channel, int soundindex, float attenuation ) {
	edict_t *ent;

	if( !soundindex ) {
		return NULL;
	}

	if( origin == NULL ) {
		attenuation = ATTN_NONE;
	}

	ent = _G_SpawnSound( channel, soundindex, attenuation );
	if( attenuation != ATTN_NONE ) {
		assert( origin );
		ent->s.channel |= CHAN_FIXED;
		VectorCopy( origin, ent->s.origin );
	} else {
		ent->r.svflags |= SVF_BROADCAST;
	}

	GClip_LinkEntity( ent );
	return ent;
}

/*
* G_GlobalSound
*/
void G_GlobalSound( int channel, int soundindex ) {
	G_PositionedSound( NULL, channel, soundindex, ATTN_NONE );
}

/*
* G_LocalSound
*/
void G_LocalSound( edict_t *owner, int channel, int soundindex ) {
	edict_t *ent;

	if( !soundindex ) {
		return;
	}
	if( ISEVENTENTITY( &owner->s ) ) {
		return; // event entities can't be owner of sound entities
	}

	ent = _G_SpawnSound( channel, soundindex, ATTN_NONE );
	ent->s.ownerNum = ENTNUM( owner );
	ent->r.svflags |= SVF_ONLYOWNER | SVF_BROADCAST;

	GClip_LinkEntity( ent );
}

//==============================================================================
//
//Kill box
//
//==============================================================================

/*
* KillBox
*
* Kills all entities that would touch the proposed new positioning
* of ent.  Ent should be unlinked before calling this!
*/
bool KillBox( edict_t *ent ) {
	trace_t tr;
	bool telefragged = false;

	while( 1 ) {
		G_Trace( &tr, ent->s.origin, ent->r.mins, ent->r.maxs, ent->s.origin, world, MASK_PLAYERSOLID );
		if( ( tr.fraction == 1.0f && !tr.startsolid ) || tr.ent < 0 ) {
			return telefragged;
		}

		if( tr.ent == ENTNUM( world ) ) {
			return telefragged; // found the world (but a player could be in there too). suicide?

		}
		// nail it
		G_Damage( &game.edicts[tr.ent], ent, ent, vec3_origin, vec3_origin, ent->s.origin, 100000, 0, 0, DAMAGE_NO_PROTECTION, MOD_TELEFRAG );
		telefragged = true;

		// if we didn't kill it, fail
		if( game.edicts[tr.ent].r.solid ) {
			return telefragged;
		}
	}

	return telefragged; // all clear
}

/*
* LookAtKillerYAW
* returns the YAW angle to look at our killer
*/
float LookAtKillerYAW( edict_t *self, edict_t *inflictor, edict_t *attacker ) {
	vec3_t dir;
	float killer_yaw;

	if( attacker && attacker != world && attacker != self ) {
		VectorSubtract( attacker->s.origin, self->s.origin, dir );
	} else if( inflictor && inflictor != world && inflictor != self ) {
		VectorSubtract( inflictor->s.origin, self->s.origin, dir );
	} else {
		killer_yaw = self->s.angles[YAW];
		return killer_yaw;
	}

	if( dir[0] ) {
		killer_yaw = RAD2DEG( atan2( dir[1], dir[0] ) );
	} else {
		killer_yaw = 0;
		if( dir[1] > 0 ) {
			killer_yaw = 90;
		} else if( dir[1] < 0 ) {
			killer_yaw = -90;
		}
	}
	if( killer_yaw < 0 ) {
		killer_yaw += 360;
	}

	return killer_yaw;
}

//==============================================================================
//
//		Warsow: more miscelanea tools
//
//==============================================================================

/*
* G_SpawnTeleportEffect
*/
static void G_SpawnTeleportEffect( edict_t *ent, bool respawn, bool in ) {
	edict_t *event;

	if( !ent || !ent->r.client ) {
		return;
	}

	if( G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED || ent->r.solid == SOLID_NOT ) {
		return;
	}

	// add a teleportation effect
	event = G_SpawnEvent( respawn ? EV_PLAYER_RESPAWN : ( in ? EV_PLAYER_TELEPORT_IN : EV_PLAYER_TELEPORT_OUT ), 0, ent->s.origin );
	event->s.ownerNum = ENTNUM( ent );
}

void G_TeleportEffect( edict_t *ent, bool in ) {
	G_SpawnTeleportEffect( ent, false, in );
}

void G_RespawnEffect( edict_t *ent ) {
	G_SpawnTeleportEffect( ent, true, false );
}

/*
* G_SolidMaskForEnt
*/
int G_SolidMaskForEnt( edict_t *ent ) {
	return ent->r.clipmask ? ent->r.clipmask : MASK_SOLID;
}

/*
* G_CheckEntGround
*/
void G_CheckGround( edict_t *ent ) {
	vec3_t point;
	trace_t trace;

	if( ent->flags & ( FL_SWIM | FL_FLY ) ) {
		ent->groundentity = NULL;
		ent->groundentity_linkcount = 0;
		return;
	}

	if( ent->r.client && ent->velocity[2] > 180 ) {
		ent->groundentity = NULL;
		ent->groundentity_linkcount = 0;
		return;
	}

	// if the hull point one-quarter unit down is solid the entity is on ground
	point[0] = ent->s.origin[0];
	point[1] = ent->s.origin[1];
	point[2] = ent->s.origin[2] - 0.25;

	G_Trace( &trace, ent->s.origin, ent->r.mins, ent->r.maxs, point, ent, G_SolidMaskForEnt( ent ) );

	// check steepness
	if( !ISWALKABLEPLANE( &trace.plane ) && !trace.startsolid ) {
		ent->groundentity = NULL;
		ent->groundentity_linkcount = 0;
		return;
	}

	if( ( ent->velocity[2] > 1 && !ent->r.client ) && !trace.startsolid ) {
		ent->groundentity = NULL;
		ent->groundentity_linkcount = 0;
		return;
	}

	if( !trace.startsolid && !trace.allsolid ) {
		//VectorCopy( trace.endpos, ent->s.origin );
		ent->groundentity = &game.edicts[trace.ent];
		ent->groundentity_linkcount = ent->groundentity->linkcount;
		if( ent->velocity[2] < 0 ) {
			ent->velocity[2] = 0;
		}
	}
}

/*
* G_CategorizePosition
*/
void G_CategorizePosition( edict_t *ent ) {
	vec3_t point;
	int cont;

	//
	// get waterlevel
	//
	point[0] = ent->s.origin[0];
	point[1] = ent->s.origin[1];
	point[2] = ent->s.origin[2] + ent->r.mins[2] + 1;
	cont = G_PointContents( point );

	if( !( cont & MASK_WATER ) ) {
		ent->waterlevel = 0;
		ent->watertype = 0;
		return;
	}

	ent->watertype = cont;
	ent->waterlevel = 1;
	point[2] += 26;
	cont = G_PointContents( point );
	if( !( cont & MASK_WATER ) ) {
		return;
	}

	ent->waterlevel = 2;
	point[2] += 22;
	cont = G_PointContents( point );
	if( cont & MASK_WATER ) {
		ent->waterlevel = 3;
	}
}

/*
* G_DropToFloor
*/
void G_DropToFloor( edict_t *ent ) {
	vec3_t end;
	trace_t trace;

	ent->s.origin[2] += 1;
	VectorCopy( ent->s.origin, end );
	end[2] -= 256;

	G_Trace( &trace, ent->s.origin, ent->r.mins, ent->r.maxs, end, ent, G_SolidMaskForEnt( ent ) );

	if( trace.fraction == 1 || trace.allsolid ) {
		return;
	}

	VectorCopy( trace.endpos, ent->s.origin );

	GClip_LinkEntity( ent );
	G_CheckGround( ent );
	G_CategorizePosition( ent );
}

/*
* G_DropSpawnpointToFloor
*/
void G_DropSpawnpointToFloor( edict_t *ent ) {
	vec3_t start, end;
	trace_t trace;

	VectorCopy( ent->s.origin, start );
	start[2] += 16;
	VectorCopy( ent->s.origin, end );
	end[2] -= 16000;

	G_Trace( &trace, start, playerbox_stand_mins, playerbox_stand_maxs, end, ent, MASK_PLAYERSOLID );
	if( trace.startsolid || trace.allsolid ) {
		G_Printf( "Warning: %s %s spawns inside solid. Inhibited\n", ent->classname, vtos( ent->s.origin ) );
		G_FreeEdict( ent );
		return;
	}

	if( ent->spawnflags & 1 ) { //  floating items flag, we test that they are not inside solid too
		return;
	}

	if( trace.fraction < 1.0f ) {
		VectorMA( trace.endpos, 1.0f, trace.plane.normal, ent->s.origin );
	}
}

/*
* G_CheckBottom
*
* Returns false if any part of the bottom of the entity is off an edge that
* is not a staircase.
*
*/
int c_yes, c_no;
bool G_CheckBottom( edict_t *ent ) {
	vec3_t mins, maxs, start, stop;
	trace_t trace;
	int x, y;
	float mid, bottom;

	VectorAdd( ent->s.origin, ent->r.mins, mins );
	VectorAdd( ent->s.origin, ent->r.maxs, maxs );

	// if all of the points under the corners are solid world, don't bother
	// with the tougher checks
	// the corners must be within 16 of the midpoint
	start[2] = mins[2] - 1;
	for( x = 0; x <= 1; x++ )
		for( y = 0; y <= 1; y++ ) {
			start[0] = x ? maxs[0] : mins[0];
			start[1] = y ? maxs[1] : mins[1];
			if( G_PointContents( start ) != CONTENTS_SOLID ) {
				goto realcheck;
			}
		}

	c_yes++;
	return true;       // we got out easy

realcheck:
	c_no++;

	//
	// check it for real...
	//
	start[2] = mins[2];

	// the midpoint must be within 16 of the bottom
	start[0] = stop[0] = ( mins[0] + maxs[0] ) * 0.5;
	start[1] = stop[1] = ( mins[1] + maxs[1] ) * 0.5;
	stop[2] = start[2] - 2 * STEPSIZE;
	G_Trace( &trace, start, vec3_origin, vec3_origin, stop, ent, G_SolidMaskForEnt( ent ) );

	if( trace.fraction == 1.0 ) {
		return false;
	}
	mid = bottom = trace.endpos[2];

	// the corners must be within 16 of the midpoint
	for( x = 0; x <= 1; x++ ) {
		for( y = 0; y <= 1; y++ ) {
			start[0] = stop[0] = x ? maxs[0] : mins[0];
			start[1] = stop[1] = y ? maxs[1] : mins[1];

			G_Trace( &trace, start, vec3_origin, vec3_origin, stop, ent, G_SolidMaskForEnt( ent ) );

			if( trace.fraction != 1.0 && trace.endpos[2] > bottom ) {
				bottom = trace.endpos[2];
			}
			if( trace.fraction == 1.0 || mid - trace.endpos[2] > STEPSIZE ) {
				return false;
			}
		}
	}

	c_yes++;
	return true;
}

/*
* G_SetBoundsForSpanEntity
*
* Set origin and origin2 and then call this before linkEntity
* for laser entities for proper clipping against world leafs/clusters.
*/
void G_SetBoundsForSpanEntity( edict_t *ent, vec_t size ) {
	vec3_t sizeVec;

	VectorSet( sizeVec, size, size, size );
	ClearBounds( ent->r.absmin, ent->r.absmax );
	AddPointToBounds( ent->s.origin, ent->r.absmin, ent->r.absmax );
	AddPointToBounds( ent->s.origin2, ent->r.absmin, ent->r.absmax );
	VectorSubtract( ent->r.absmin, sizeVec, ent->r.absmin );
	VectorAdd( ent->r.absmax, sizeVec, ent->r.absmax );
	VectorSubtract( ent->r.absmin, ent->s.origin, ent->r.mins );
	VectorSubtract( ent->r.absmax, ent->s.origin, ent->r.maxs );
}

/*
* G_ReleaseClientPSEvent
*/
void G_ReleaseClientPSEvent( Client *client ) {
	int i;

	if( client ) {
		for( i = 0; i < 2; i++ ) {
			if( client->eventsCurrent < client->eventsHead ) {
				client->ps.event[i] = client->events[client->eventsCurrent & MAX_CLIENT_EVENTS_MASK] & 127;
				client->ps.eventParm[i] = ( client->events[client->eventsCurrent & MAX_CLIENT_EVENTS_MASK] >> 8 ) & 0xFF;
				client->eventsCurrent++;
			} else {
				client->ps.event[i] = PSEV_NONE;
				client->ps.eventParm[i] = 0;
			}
		}
	}
}

/*
* G_AddPlayerStateEvent
* This event is only sent to this client inside its player_state_t.
*/
void G_AddPlayerStateEvent( Client *client, int event, int parm ) {
	int eventdata;
	if( client ) {
		if( !event || event > PSEV_MAX_EVENTS || parm > 0xFF ) {
			return;
		}
		if( client ) {
			eventdata = ( ( event & 0xFF ) | ( parm & 0xFF ) << 8 );
			client->events[client->eventsHead & MAX_CLIENT_EVENTS_MASK] = eventdata;
			client->eventsHead++;
		}
	}
}

/*
* G_ClearPlayerStateEvents
*/
void G_ClearPlayerStateEvents( Client *client ) {
	if( client ) {
		memset( client->events, PSEV_NONE, sizeof( client->events ) );
		client->eventsCurrent = client->eventsHead = 0;
	}
}

/*
* G_PlayerForText
* Returns player matching given text. It can be either number of the player or player's name.
*/
edict_t *G_PlayerForText( const char *text ) {
	int pnum;

	if( !text || !text[0] ) {
		return NULL;
	}

	pnum = atoi( text );

	if( !Q_stricmp( text, va( "%i", pnum ) ) && pnum >= 0 && pnum < ggs->maxclients && game.edicts[pnum + 1].r.inuse ) {
		return &game.edicts[atoi( text ) + 1];
	} else {
		int i;
		edict_t *e;
		char colorless[MAX_INFO_VALUE];

		Q_strncpyz( colorless, COM_RemoveColorTokens( text ), sizeof( colorless ) );

		// check if it's a known player name
		for( i = 0, e = game.edicts + 1; i < ggs->maxclients; i++, e++ ) {
			if( !e->r.inuse ) {
				continue;
			}

			if( !Q_stricmp( colorless, COM_RemoveColorTokens( e->r.client->netname.data() ) ) ) {
				return e;
			}
		}

		// nothing found
		return NULL;
	}
}

/*
* G_AnnouncerSound - sends inmediatly. queue client side (excepting at player's ps events queue)
*/
void G_AnnouncerSound( edict_t *targ, int soundindex, int team, bool queued, edict_t *ignore ) {
	int psev = queued ? PSEV_ANNOUNCER_QUEUED : PSEV_ANNOUNCER;
	int playerTeam;

	if( targ ) { // only for a given player
		if( !targ->r.client || G_GetClientState( PLAYERNUM( targ ) ) < CS_SPAWNED ) {
			return;
		}

		if( targ == ignore ) {
			return;
		}

		G_AddPlayerStateEvent( targ->r.client, psev, soundindex );
	} else {   // add it to all players
		edict_t *ent;

		for( ent = game.edicts + 1; PLAYERNUM( ent ) < ggs->maxclients; ent++ ) {
			if( !ent->r.inuse || G_GetClientState( PLAYERNUM( ent ) ) < CS_SPAWNED ) {
				continue;
			}

			if( ent == ignore ) {
				continue;
			}

			// team filter
			if( team >= TEAM_SPECTATOR && team < GS_MAX_TEAMS ) {
				playerTeam = ent->s.team;

				// if in chasecam, assume the player is in the chased player team
				if( playerTeam == TEAM_SPECTATOR && ent->r.client->chase.active && ent->r.client->chase.target > 0 ) {
					playerTeam = game.edicts[ent->r.client->chase.target].s.team;
				}

				if( playerTeam != team ) {
					continue;
				}
			}

			G_AddPlayerStateEvent( ent->r.client, psev, soundindex );
		}
	}
}

/*
* G_PureSound
*/
void G_PureSound( const char *sound ) {
	assert( sound && sound[0] && strlen( sound ) < MAX_QPATH );

	if( sound[0] == '*' ) {
		// sexed sounds
		// jal : this isn't correct. Sexed sounds don't have the full path because
		// the path depends on the model, so how can they be pure anyway?
		SV_PureSound( sound + 1 );
	} else {
		SV_PureSound( sound );
	}
}

/*
* G_PureModel
*/
void G_PureModel( const char *model ) {
	assert( model && model[0] && strlen( model ) < MAX_QPATH );

	SV_PureModel( model );
}

/*
* G_PrecacheWeapondef
*/
void G_PrecacheWeapondef( int weapon ) {	
	assert( weapon < ( MAX_WEAPONDEFS / 4 ) );

	bool race, strong;
	for( int index = weapon; index < MAX_WEAPONDEFS; index += MAX_WEAPONDEFS / 4 )
	{
		// see CG_OverrideWeapondef, uses same operations
		weapon = index % ( MAX_WEAPONDEFS / 4 );
		race = index > ( MAX_WEAPONDEFS / 2 );
		strong = ( index % ( MAX_WEAPONDEFS / 2 ) ) > ( MAX_WEAPONDEFS / 4 );

		const auto *weaponDef = GS_GetWeaponDefExt( ggs, weapon, race );
		const auto *firedef = strong ? &weaponDef->firedef : &weaponDef->firedef_weak;
		
		char cstring[MAX_CONFIGSTRING_CHARS];

		Q_snprintfz( cstring, sizeof( cstring ), "%i %i %u %u %u %u %u %i %i %i",
					 firedef->usage_count,
					 firedef->projectile_count,
					 firedef->weaponup_time,
					 firedef->weapondown_time,
					 firedef->reload_time,
					 firedef->cooldown_time,
					 firedef->timeout,
					 firedef->speed,
					 firedef->spread,
					 firedef->v_spread
					 );

		SV_SetConfigString( CS_WEAPONDEFS + index, cstring );
	}
}