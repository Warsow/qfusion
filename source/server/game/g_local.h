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

// g_local.h -- local definitions for game module

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/q_collision.h>
#include <common/facilities/gs_public.h>

#include "g_public.h"
#include "g_gametypes.h"

#include <common/helpers/mmuuid.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/messagestreams.h>

extern gs_state_t *ggs;

//==================================================================
// round(x)==floor(x+0.5f)

// FIXME: Medar: Remove the spectator test and just make sure they always have health
#define G_IsDead( ent )       ( ( !( ent )->r.client || ( ent )->s.team != TEAM_SPECTATOR ) && HEALTH_TO_INT( ( ent )->health ) <= 0 )

// Quad scale for damage and knockback
#define QUAD_DAMAGE_SCALE 2
#define QUAD_KNOCKBACK_SCALE 2
#define MAX_STUN_TIME 2000

#define CLIENT_RESPAWN_FREEZE_DELAY 300

// edict->flags
#define FL_FLY              0x00000001
#define FL_SWIM             0x00000002  // implied immunity to drowining
#define FL_IMMUNE_LASER     0x00000004
#define FL_INWATER          0x00000008
#define FL_GODMODE          0x00000010
#define FL_NOTARGET         0x00000020
#define FL_IMMUNE_SLIME     0x00000040
#define FL_IMMUNE_LAVA      0x00000080
#define FL_PARTIALGROUND    0x00000100  // not all corners are valid
#define FL_WATERJUMP        0x00000200  // player jumping out of water
#define FL_TEAMFOLLOWER     0x00000400  // not the first on the team
#define FL_NO_KNOCKBACK     0x00000800
#define FL_BUSY             0x00001000

#define FRAMETIME ( (float)game.frametime * 0.001f )

#define BODY_QUEUE_SIZE     8
#define MAX_FLOOD_MESSAGES 32

typedef enum {
	DAMAGE_NO,
	DAMAGE_YES,     // will take damage if hit
	DAMAGE_AIM      // auto targeting recognizes this
} damage_t;

// deadflag
#define DEAD_NO         0
#define DEAD_DYING      1
#define DEAD_DEAD       2
#define DEAD_RESPAWNABLE    3

// monster ai flags
#define AI_STAND_GROUND     0x00000001
#define AI_TEMP_STAND_GROUND    0x00000002
#define AI_SOUND_TARGET     0x00000004
#define AI_LOST_SIGHT       0x00000008
#define AI_PURSUIT_LAST_SEEN    0x00000010
#define AI_PURSUE_NEXT      0x00000020
#define AI_PURSUE_TEMP      0x00000040
#define AI_HOLD_FRAME       0x00000080
#define AI_GOOD_GUY     0x00000100
#define AI_BRUTAL       0x00000200
#define AI_NOSTEP       0x00000400
#define AI_DUCKED       0x00000800
#define AI_COMBAT_POINT     0x00001000
#define AI_MEDIC        0x00002000
#define AI_RESURRECTING     0x00004000

// game.serverflags values
#define SFL_CROSS_TRIGGER_1 0x00000001
#define SFL_CROSS_TRIGGER_2 0x00000002
#define SFL_CROSS_TRIGGER_3 0x00000004
#define SFL_CROSS_TRIGGER_4 0x00000008
#define SFL_CROSS_TRIGGER_5 0x00000010
#define SFL_CROSS_TRIGGER_6 0x00000020
#define SFL_CROSS_TRIGGER_7 0x00000040
#define SFL_CROSS_TRIGGER_8 0x00000080
#define SFL_CROSS_TRIGGER_MASK  0x000000ff

// handedness values
#define RIGHT_HANDED        0
#define LEFT_HANDED     1
#define CENTER_HANDED       2

// milliseconds before allowing fire after respawn
#define WEAPON_RESPAWN_DELAY            350

// edict->movetype values
typedef enum {
	MOVETYPE_NONE,      // never moves
	MOVETYPE_PLAYER,    // never moves (but is moved by pmove)
	MOVETYPE_NOCLIP,    // like MOVETYPE_PLAYER, but not clipped
	MOVETYPE_PUSH,      // no clip to world, push on box contact
	MOVETYPE_STOP,      // no clip to world, stops on box contact
	MOVETYPE_FLY,
	MOVETYPE_TOSS,      // gravity
	MOVETYPE_LINEARPROJECTILE, // extra size to monsters
	MOVETYPE_BOUNCE,
	MOVETYPE_BOUNCEGRENADE,
	MOVETYPE_TOSSSLIDE
} movetype_t;

//
// this structure is left intact through an entire game
// it should be initialized at dll load time, and read/written to
// the server.ssv file for savegames
//
typedef struct {
	edict_t *edicts;        // [maxentities]
	Client *clients;     // [maxclients]

	int protocol;
	char demoExtension[MAX_QPATH];

	// store latched cvars here that we want to get at often
	int maxentities;
	int numentities;

	// cross level triggers
	int serverflags;

	// AngelScript engine object
	void *asEngine;
	bool asGlobalsInitialized;

	unsigned int frametime;         // in milliseconds
	int snapFrameTime;              // in milliseconds
	int64_t realtime;				// actual time, set with Sys_Milliseconds every frame
	int64_t serverTime;				// actual time in the server

	int64_t utcTimeMillis;          // local time since epoch
	int64_t utcMatchStartTime;      // utcTimeMillis at the actual match start
	                                // (we can't compute it based on the current time due to possible timeouts)

	int numBots;

	unsigned int levelSpawnCount;   // the number of times G_InitLevel was called
} game_locals_t;

#define TIMEOUT_TIME                    180000
#define TIMEIN_TIME                     5000

#define SIGNIFICANT_MATCH_DURATION      66 * 1000

typedef struct {
	int64_t time;
	int64_t endtime;
	int caller;
	int used[MAX_CLIENTS];
} timeout_t;

//
// this structure is cleared as each map is entered
// it is read/written to the level.sav file for savegames
//
typedef struct {
	int64_t framenum;
	int64_t time; // time in milliseconds
	int64_t spawnedTimeStamp; // time when map was restarted
	int64_t finalMatchDuration;

	char level_name[MAX_QPATH];    // the descriptive name (Outer Base, etc)
	char mapname[MAX_QPATH];       // the server name (q3dm0, etc)
	char nextmap[MAX_QPATH];       // go here when match is finished
	char forcemap[MAX_QPATH];      // go here
	char autorecord_name[128];

	// backup entities string
	char *mapString;
	size_t mapStrlen;

	// string used for parsing entities
	char *map_parsed_ents;      // string used for storing parsed key values
	size_t map_parsed_len;

	bool canSpawnEntities; // security check to prevent entities being spawned before map entities

	// intermission state
	bool exitNow;
	bool hardReset;

	// gametype definition and execution
	gametype_descriptor_t gametype;

	// map scripts
	struct {
		void *initFunc;
		void *preThinkFunc;
		void *postThinkFunc;
		void *exitFunc;
		void *gametypeFunc;
	} mapscript;

	bool teamlock;
	bool ready[MAX_CLIENTS];
	bool forceStart;    // force starting the game, when warmup timelimit is up
	bool forceExit;     // just exit, ignore extended time checks

	edict_t *current_entity;    // entity running from G_RunFrame
	edict_t *spawning_entity;   // entity being spawned from G_InitLevel
	int body_que;               // dead bodies

	edict_t *think_client_entity;// cycles between connected clients each frame

	int numCheckpoints;
	int numLocations;

	timeout_t timeout;
	float gravity;

	int colorCorrection;
} level_locals_t;


// spawn_temp_t is only used to hold entity field values that
// can be set from the editor, but aren't actualy present
// in edict_t during gameplay
typedef struct {
	// world vars
	float fov;
	const char *nextmap;

	const char *music;

	int lip;
	int distance;
	int height;
	float roll;
	float radius;
	float phase;
	const char *noise;
	const char *noise_start;
	const char *noise_stop;
	float pausetime;
	const char *item;
	const char *gravity;
	const char *debris1, *debris2;

	int notsingle;
	int notteam;
	int notfree;
	int notduel;
	int noents;

	int gameteam;

	int weight;
	float scale;
	const char *gametype;
	const char *not_gametype;
	const char *shaderName;
	int size;

	const char *colorCorrection;
} spawn_temp_t;


extern game_locals_t game;
extern level_locals_t level;
extern spawn_temp_t st;

extern int meansOfDeath;


#define FOFS( x ) offsetof( edict_t,x )
#define STOFS( x ) offsetof( spawn_temp_t,x )
#define LLOFS( x ) offsetof( level_locals_t,x )
#define CLOFS( x ) offsetof( Client,x )
#define AWOFS( x ) offsetof( award_info_t,x )

extern cvar_t *password;
extern cvar_t *g_operator_password;
extern cvar_t *g_select_empty;
extern cvar_t *dedicated;
extern cvar_t *developer;

extern cvar_t *filterban;

extern cvar_t *g_gravity;
extern cvar_t *g_maxvelocity;

extern cvar_t *sv_cheats;
extern cvar_t *sv_mm_enable;

extern cvar_t *cm_mapHeader;
extern cvar_t *cm_mapVersion;

extern cvar_t *g_floodprotection_messages;
extern cvar_t *g_floodprotection_team;
extern cvar_t *g_floodprotection_seconds;
extern cvar_t *g_floodprotection_penalty;

extern cvar_t *g_inactivity_maxtime;

extern cvar_t *g_maplist;
extern cvar_t *g_maprotation;

extern cvar_t *g_enforce_map_pool;
extern cvar_t *g_map_pool;

extern cvar_t *g_scorelimit;
extern cvar_t *g_timelimit;

extern cvar_t *g_projectile_touch_owner;
extern cvar_t *g_projectile_prestep;

extern cvar_t *g_shockwave_fly_stun_scale;
extern cvar_t *g_shockwave_fly_damage_scale;
extern cvar_t *g_shockwave_fly_knockback_scale;

extern cvar_t *g_numbots;
extern cvar_t *g_maxtimeouts;

extern cvar_t *g_self_knockback;
extern cvar_t *g_knockback_scale;
extern cvar_t *g_rocket_gravity_scale;
extern cvar_t *g_plasma_gravity_scale;
extern cvar_t *g_plasma_up_speed_boost;
extern cvar_t *g_projectile_toss_morph_delay;
extern cvar_t *g_volatile_explosives;
extern cvar_t *g_allow_stun;
extern cvar_t *g_armor_degradation;
extern cvar_t *g_armor_protection;
extern cvar_t *g_allow_falldamage;
extern cvar_t *g_allow_selfdamage;
extern cvar_t *g_allow_teamdamage;
extern cvar_t *g_allow_bunny;
extern cvar_t *g_ammo_respawn;
extern cvar_t *g_weapon_respawn;
extern cvar_t *g_health_respawn;
extern cvar_t *g_armor_respawn;
extern cvar_t *g_respawn_delay_min;
extern cvar_t *g_respawn_delay_max;
extern cvar_t *g_deadbody_followkiller;
extern cvar_t *g_deadbody_autogib_delay;
extern cvar_t *g_antilag_timenudge;
extern cvar_t *g_antilag_maxtimedelta;

extern cvar_t *g_teams_maxplayers;
extern cvar_t *g_teams_allow_uneven;

extern cvar_t *g_autorecord;
extern cvar_t *g_autorecord_maxdemos;
extern cvar_t *g_allow_spectator_voting;
extern cvar_t *g_instagib;
extern cvar_t *g_instajump;
extern cvar_t *g_instashield;

extern cvar_t *g_asGC_stats;
extern cvar_t *g_asGC_interval;

extern cvar_t *g_skillRating;

edict_t **G_Teams_ChallengersQueue( void );
void G_Teams_Join_Cmd( edict_t *ent, const CmdArgs &cmdArgs );
bool G_Teams_JoinTeam( edict_t *ent, int team );
void G_Teams_UnInvitePlayer( int team, edict_t *ent );
void G_Teams_RemoveInvites( void );
bool G_Teams_TeamIsLocked( int team );
bool G_Teams_LockTeam( int team );
bool G_Teams_UnLockTeam( int team );
void G_Teams_Invite_f( edict_t *ent, const CmdArgs & );
void G_Teams_UpdateMembersList( void );
bool G_Teams_JoinAnyTeam( edict_t *ent, bool silent );
void G_Teams_SetTeam( edict_t *ent, int team );

void Cmd_Say_f( edict_t *ent, uint64_t clientCommandNum, const CmdArgs & );
void G_Say_Team( edict_t *who, const char *inmsg );

void G_Match_Ready( edict_t *ent, const CmdArgs & );
void G_Match_NotReady( edict_t *ent, const CmdArgs & );
void G_Match_ToggleReady( edict_t *ent, const CmdArgs & );
void G_Match_CheckReadys( void );
void G_EndMatch( void );

void G_Teams_JoinChallengersQueue( edict_t *ent, const CmdArgs & );
void G_Teams_LeaveChallengersQueue( edict_t *ent, const CmdArgs & );
void G_InitChallengersQueue( void );

void G_MoveClientToPostMatchScoreBoards( edict_t *ent, edict_t *spawnpoint );
void G_Gametype_Init( void );
void G_Gametype_GenerateAllowedGametypesList( void );
bool G_Gametype_IsVotable( const wsw::StringView &name );
void G_Gametype_ScoreEvent( Client *client, const char *score_event, const char *args );
void G_RunGametype( void );
bool G_Gametype_CanPickUpItem( const gsitem_t *item );
bool G_Gametype_CanSpawnItem( const gsitem_t *item );
bool G_Gametype_CanRespawnItem( const gsitem_t *item );
bool G_Gametype_CanDropItem( const gsitem_t *item, bool ignoreMatchState );
bool G_Gametype_CanTeamDamage( int damageflags );
int G_Gametype_RespawnTimeForItem( const gsitem_t *item );
int G_Gametype_DroppedItemTimeout( const gsitem_t *item );

//
// g_spawnpoints.c
//
enum {
	SPAWNSYSTEM_INSTANT,
	SPAWNSYSTEM_WAVES,
	SPAWNSYSTEM_HOLD,

	SPAWNSYSTEM_TOTAL
};

void G_SpawnQueue_Init( void );
void G_SpawnQueue_SetTeamSpawnsystem( int team, int spawnsystem, int wave_time, int wave_maxcount, bool spectate_team );
int G_SpawnQueue_NextRespawnTime( int team );
void G_SpawnQueue_ResetTeamQueue( int team );
int G_SpawnQueue_GetSystem( int team );
void G_SpawnQueue_ReleaseTeamQueue( int team );
void G_SpawnQueue_AddClient( edict_t *ent );
void G_SpawnQueue_RemoveClient( edict_t *ent );
void G_SpawnQueue_Think( void );

edict_t *SelectDeathmatchSpawnPoint( edict_t *ent );
void SelectSpawnPoint( edict_t *ent, edict_t **spawnpoint, vec3_t origin, vec3_t angles );
edict_t *G_SelectIntermissionSpawnPoint( void );
float PlayersRangeFromSpot( edict_t *spot, int ignore_team );
void SP_info_player_start( edict_t *ent );
void SP_info_player_deathmatch( edict_t *ent );
void SP_info_player_intermission( edict_t *ent );

//
// g_func.c
//
void G_AssignMoverSounds( edict_t *ent, const char *start, const char *move, const char *stop );
bool G_EntIsADoor( edict_t *ent );

void SP_func_plat( edict_t *ent );
void SP_func_rotating( edict_t *ent );
void SP_func_button( edict_t *ent );
void SP_func_door( edict_t *ent );
void SP_func_door_rotating( edict_t *ent );
void SP_func_door_secret( edict_t *self );
void SP_func_water( edict_t *self );
void SP_func_train( edict_t *ent );
void SP_func_timer( edict_t *self );
void SP_func_conveyor( edict_t *self );
void SP_func_wall( edict_t *self );
void SP_func_object( edict_t *self );
void SP_func_explosive( edict_t *self );
void SP_func_killbox( edict_t *ent );
void SP_func_static( edict_t *ent );
void SP_func_bobbing( edict_t *ent );
void SP_func_pendulum( edict_t *ent );

//
// g_ascript.c
//
bool GT_asLoadScript( const char *gametypeName );
void GT_asShutdownScript( void );
void GT_asCallSpawn( void );
void GT_asCallMatchStateStarted( void );
bool GT_asCallMatchStateFinished( int incomingMatchState );
void GT_asCallThinkRules( void );
void GT_asCallPlayerKilled( edict_t *targ, edict_t *inflictor, edict_t *attacker, int damage, vec3_t point, int mod );
void GT_asCallPlayerRespawn( edict_t *ent, int old_team, int new_team );
void GT_asCallScoreEvent( Client *client, const char *score_event, const char *args );
void GT_asCallUpdateScoreboard();
edict_t *GT_asCallSelectSpawnPoint( edict_t *ent );
bool GT_asCallGameCommand( Client *client, const wsw::StringView &cmd, const wsw::StringView &args, int argc );
bool GT_asCallBotStatus( edict_t *ent );
void GT_asCallShutdown( void );

void G_asCallMapEntityThink( edict_t *ent );
void G_asCallMapEntityTouch( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags );
void G_asCallMapEntityUse( edict_t *ent, edict_t *other, edict_t *activator );
void G_asCallMapEntityPain( edict_t *ent, edict_t *other, float kick, float damage );
void G_asCallMapEntityDie( edict_t *ent, edict_t *inflicter, edict_t *attacker, int damage, const vec3_t point );
void G_asCallMapEntityStop( edict_t *ent );
void G_asResetEntityBehaviors( edict_t *ent );
void G_asClearEntityBehaviors( edict_t *ent );
void G_asReleaseEntityBehaviors( edict_t *ent );

bool G_asLoadMapScript( const char *mapname );
void G_asShutdownMapScript( void );
void G_asCallMapInit( void );
void G_asCallMapPreThink( void );
void G_asCallMapPostThink( void );
void G_asCallMapExit( void );
const char *G_asCallMapGametype( void );

bool G_asCallMapEntitySpawnScript( const char *classname, edict_t *ent );

void G_asInitGameModuleEngine( void );
void G_asShutdownGameModuleEngine( void );
void G_asGarbageCollect( bool force );
void G_asDumpAPI_f( const CmdArgs & );

#define world   ( (edict_t *)game.edicts )

// item spawnflags
#define ITEM_TRIGGER_SPAWN  0x00000001
#define ITEM_NO_TOUCH       0x00000002
// 6 bits reserved for editor flags
// 8 bits used as power cube id bits for coop games
#define DROPPED_ITEM        0x00010000
#define DROPPED_PLAYER_ITEM 0x00020000
#define ITEM_TARGETS_USED   0x00040000
#define ITEM_IGNORE_MAX     0x00080000
#define ITEM_TIMED          0x00100000
//
// fields are needed for spawning from the entity string
//
#define FFL_SPAWNTEMP       1

typedef enum {
	F_INT,
	F_FLOAT,
	F_LSTRING,      // string on disk, pointer in memory, TAG_LEVEL
	F_VECTOR,
	F_ANGLEHACK,
	F_IGNORE
} fieldtype_t;

typedef struct {
	const char *name;
	size_t ofs;
	fieldtype_t type;
	int flags;
} field_t;

extern const field_t fields[];


//
// g_cmds.c
//

char *G_StatsMessage( edict_t *ent );

//
// g_items.c
//
void DoRespawn( edict_t *ent );
void PrecacheItem( const gsitem_t *it );
void G_PrecacheItems( void );
edict_t *Drop_Item( edict_t *ent, const gsitem_t *item );
void SetRespawn( edict_t *ent, int delay );
void G_Items_RespawnByType( unsigned int typeMask, int item_tag, float delay );
void G_FireWeapon( edict_t *ent, int parm );
void SpawnItem( edict_t *ent, const gsitem_t *item );
void G_Items_FinishSpawningItems( void );
int PowerArmorType( edict_t *ent );
const gsitem_t *GetItemByTag( int tag );
bool Add_Ammo( Client *client, const gsitem_t *item, int count, bool add_it );
void Touch_ItemSound( edict_t *other, const gsitem_t *item );
void Touch_Item( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags );
bool G_PickupItem( edict_t *other, const gsitem_t *it, int flags, int count, const int *invpack );
void G_UseItem( struct edict_s *ent, const gsitem_t *item );
edict_t *G_DropItem( struct edict_s *ent, const gsitem_t *item );
bool Add_Armor( edict_t *ent, edict_t *other, bool pick_it );

//
// g_utils.c
//
#define G_LEVELPOOL_BASE_SIZE   45 * 1024 * 1024

bool KillBox( edict_t *ent );
float LookAtKillerYAW( edict_t *self, edict_t *inflictor, edict_t *attacker );
edict_t *G_Find( edict_t *from, size_t fieldofs, const char *match );
edict_t *G_PickTarget( const char *targetname );
void G_UseTargets( edict_t *ent, edict_t *activator );
void G_SetMovedir( vec3_t angles, vec3_t movedir );
void G_InitMover( edict_t *ent );
void G_DropSpawnpointToFloor( edict_t *ent );

void G_InitEdict( edict_t *e );
edict_t *G_Spawn( void );
void G_FreeEdict( edict_t *e );

void G_StringPoolInit( void );
const char *_G_RegisterLevelString( const char *string, const char *filename, int fileline );
#define G_RegisterLevelString( in ) _G_RegisterLevelString( in, __FILE__, __LINE__ )

char *G_AllocCreateNamesList( const char *path, const char *extension, const char separator );

void G_ProjectSource( vec3_t point, vec3_t distance, vec3_t forward, vec3_t right, vec3_t result );

void G_AddEvent( edict_t *ent, int event, int parm, bool highPriority );
edict_t *G_SpawnEvent( int event, int parm, vec3_t origin );
void G_MorphEntityIntoEvent( edict_t *ent, int event, int parm );

void G_CallThink( edict_t *ent );
void G_CallTouch( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags );
void G_CallUse( edict_t *self, edict_t *other, edict_t *activator );
void G_CallStop( edict_t *self );
void G_CallPain( edict_t *ent, edict_t *attacker, float kick, float damage );
void G_CallDie( edict_t *ent, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );

int G_PlayerGender( edict_t *player );

#ifndef _MSC_VER
void G_PrintMsg( const edict_t *ent, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
void G_PrintChasersf( const edict_t *self, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
void G_CenterPrintMsg( const edict_t *ent, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
void G_CenterPrintFormatMsg( const edict_t *ent, int numVargs, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
void G_PrintMsg( const edict_t *ent, _Printf_format_string_ const char *format, ... );
void G_PrintChasersf( const edict_t *self, _Printf_format_string_ const char *format, ... );
void G_CenterPrintMsg( const edict_t *ent, _Printf_format_string_ const char *format, ... );
void G_CenterPrintFormatMsg( const edict_t *ent, int numVargs, _Printf_format_string_ const char *format, ... );
#endif

class PendingPlayerMessage {
public:
	// Note: Supplying boolean arguments is bad in general
	// but this constructor is not supposed to be called by users directly
	PendingPlayerMessage( const edict_t *traget, bool centerPrint );
	~PendingPlayerMessage();

	[[nodiscard]]
	auto getWriter() -> wsw::TextStreamWriter & { return m_writer; }
private:
	// Note: We don't follow the design of PendingRegularMessage with external stream allocation
	// as printing to clients is very unlikely to be performed in a recursive call chain
	// and we can afford allocating the buffer on stack (TODO: Really?)
	char m_buffer[MAX_PRINTMSG];
	wsw::OutputMessageStream m_stream;
	wsw::TextStreamWriter m_writer;
	const edict_t *const m_target;
	const bool m_centerPrint;
};

#define gPrintTo( target ) PendingPlayerMessage( target, false ).getWriter()
#define gCenterPrintTo( target ) PendingPlayerMessage( target, true ).getWriter()

void G_UpdatePlayerMatchMsg( edict_t *ent, bool force = false );
void G_UpdatePlayersMatchMsgs( void );
void G_Obituary( edict_t *victim, edict_t *attacker, int mod );

unsigned G_RegisterHelpMessage( const char *str );
void G_SetPlayerHelpMessage( edict_t *ent, unsigned index, bool force = false );

edict_t *G_Sound( edict_t *owner, int channel, int soundindex, float attenuation );
edict_t *G_PositionedSound( vec3_t origin, int channel, int soundindex, float attenuation );
void G_GlobalSound( int channel, int soundindex );
void G_LocalSound( edict_t *owner, int channel, int soundindex );

float vectoyaw( vec3_t vec );

void G_PureSound( const char *sound );
void G_PureModel( const char *model );

extern game_locals_t game;

#define G_ISGHOSTING( x ) ( ( ( x )->s.modelindex == 0 ) && ( ( x )->r.solid == SOLID_NOT ) )
#define ISBRUSHMODEL( x ) ( ( ( x > 0 ) && ( (int)x < SV_NumInlineModels() ) ) ? true : false )

void G_TeleportEffect( edict_t *ent, bool in );
void G_RespawnEffect( edict_t *ent );
bool G_Visible( edict_t *self, edict_t *other );
bool G_InFront( edict_t *self, edict_t *other );
bool G_EntNotBlocked( edict_t *viewer, edict_t *targ );
void G_DropToFloor( edict_t *ent );
int G_SolidMaskForEnt( edict_t *ent );
void G_CheckGround( edict_t *ent );
void G_CategorizePosition( edict_t *ent );
bool G_CheckBottom( edict_t *ent );
void G_ReleaseClientPSEvent( Client *client );
void G_AddPlayerStateEvent( Client *client, int event, int parm );
void G_ClearPlayerStateEvents( Client *client );

// announcer events
void G_AnnouncerSound( edict_t *targ, int soundindex, int team, bool queued, edict_t *ignore );
edict_t *G_PlayerForText( const char *text );

void G_PrecacheWeapondef( int weapon );

void G_SetBoundsForSpanEntity( edict_t *ent, vec_t size );

//
// g_callvotes.c
//
void G_CallVotes_Init( void );
void G_FreeCallvotes( void );
void G_CallVotes_Reset( void );
void G_CallVotes_ResetClient( int n );
void G_CallVotes_CmdVote( edict_t *ent, const CmdArgs & );
void G_CallVotes_Think( void );
void G_CallVote_Cmd( edict_t *ent, const CmdArgs & );
void G_OperatorVote_Cmd( edict_t *ent, const CmdArgs & );
void G_RegisterGametypeScriptCallvote( const char *name, const char *usage, const char *type, const char *help );

// Warning: not reentrant
const char *G_GetClientHostForFilter( const edict_t *ent );

//
// g_trigger.c
//
void SP_trigger_teleport( edict_t *ent );
void SP_info_teleport_destination( edict_t *ent );
void SP_trigger_always( edict_t *ent );
void SP_trigger_once( edict_t *ent );
void SP_trigger_multiple( edict_t *ent );
void SP_trigger_relay( edict_t *ent );
void SP_trigger_push( edict_t *ent );
void SP_trigger_hurt( edict_t *ent );
void SP_trigger_key( edict_t *ent );
void SP_trigger_counter( edict_t *ent );
void SP_trigger_elevator( edict_t *ent );
void SP_trigger_gravity( edict_t *ent );

//
// g_clip.c
//
#define MAX_ENT_AREAS 16

typedef struct link_s {
	struct link_s *prev, *next;
	int entNum;
} link_t;

int G_PointContents( const vec3_t p );
void G_Trace( trace_t *tr, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, const edict_t *passedict, int contentmask );
int G_PointContents4D( const vec3_t p, int timeDelta );
void G_Trace4D( trace_t *tr, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, const edict_t *passedict, int contentmask, int timeDelta );
void GClip_BackUpCollisionFrame( void );
int GClip_FindInRadius4D( const vec3_t org, float rad, int *list, int maxcount, int timeDelta );
void G_SplashFrac( int entNum, const vec3_t hitpoint, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac );
void RS_SplashFrac( int entNum, const vec3_t hitpoint, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac, float splashFrac ); // racesow
void GClip_ClearWorld( void );
void GClip_SetBrushModel( edict_t *ent, const char *name );
void GClip_SetAreaPortalState( edict_t *ent, bool open );
void GClip_LinkEntity( edict_t *ent );
void GClip_UnlinkEntity( edict_t *ent );
void GClip_TouchTriggers( edict_t *ent );
void G_PMoveTouchTriggers( pmove_t *pm, const vec3_t previous_origin );
entity_state_t *G_GetEntityStateForDeltaTime( int entNum, int deltaTime );
int GClip_FindInRadius( const vec3_t org, float rad, int *list, int maxcount );

// BoxEdicts() can return a list of either solid or trigger entities
// FIXME: eliminate AREA_ distinction?
#define AREA_ALL       -1
#define AREA_SOLID      1
#define AREA_TRIGGERS   2
int GClip_AreaEdicts( const vec3_t mins, const vec3_t maxs, int *list, int maxcount, int areatype, int timeDelta );
bool GClip_EntityContact( const vec3_t mins, const vec3_t maxs, const edict_t *ent );

//
// g_combat.c
//
void G_Killed( edict_t *targ, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point, int mod );
int G_ModToAmmo( int mod );
bool CheckTeamDamage( edict_t *targ, edict_t *attacker );
void G_SplashFrac( const vec3_t origin, const vec3_t mins, const vec3_t maxs, const vec3_t point, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac );
void RS_SplashFrac( const vec3_t origin, const vec3_t mins, const vec3_t maxs, const vec3_t point, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac, float splashFrac );
void G_Damage( edict_t *targ, edict_t *inflictor, edict_t *attacker, const vec3_t pushdir, const vec3_t dmgdir, const vec3_t point, float damage, float knockback, float stun, int dflags, int mod );
void G_RadiusDamage( edict_t *inflictor, edict_t *attacker, const cplane_t *plane, const edict_t *ignore, int mod );

// damage flags
#define DAMAGE_RADIUS 0x00000001  // damage was indirect
#define DAMAGE_NO_ARMOR 0x00000002  // armour does not protect from this damage
#define DAMAGE_NO_PROTECTION 0x00000004
#define DAMAGE_NO_KNOCKBACK 0x00000008
#define DAMAGE_NO_STUN 0x00000010
#define DAMAGE_STUN_CLAMP 0x00000020
#define DAMAGE_KNOCKBACK_SOFT 0x00000040

#define GIB_HEALTH      -40


//
// g_misc.c
//
void ThrowClientHead( edict_t *self, int damage );
void ThrowSmallPileOfGibs( edict_t *self, int damage );

void BecomeExplosion1( edict_t *self );

void SP_light( edict_t *self );
void SP_light_mine( edict_t *ent );
void SP_info_null( edict_t *self );
void SP_info_notnull( edict_t *self );
void SP_info_camp( edict_t *self );
void SP_path_corner( edict_t *self );

void SP_misc_teleporter_dest( edict_t *self );
void SP_misc_model( edict_t *ent );
void SP_misc_portal_surface( edict_t *ent );
void SP_misc_portal_camera( edict_t *ent );
void SP_skyportal( edict_t *ent );
void SP_misc_particles( edict_t *ent );
void SP_misc_video_speaker( edict_t *ent );

//
// g_weapon.c
//
void ThrowDebris( edict_t *self, int modelindex, float speed, vec3_t origin );
bool fire_hit( edict_t *self, vec3_t aim, int damage, int kick );
void G_HideLaser( edict_t *ent );

void W_Fire_Blade( edict_t *self, int range, vec3_t start, vec3_t angles, float damage, int knockback, int stun, int mod, int timeDelta );
void W_Fire_Bullet( edict_t *self, vec3_t start, vec3_t angles, int seed, int range, int hspread, int vspread, float damage, int knockback, int stun, int mod, int timeDelta );
edict_t *W_Fire_GunbladeBlast( edict_t *self, vec3_t start, vec3_t angles, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int speed, int timeout, int mod, int timeDelta );
void W_Fire_Riotgun( edict_t *self, vec3_t start, vec3_t angles, int seed, int range, int hspread, int vspread, int count, float damage, int knockback, int stun, int mod, int timeDelta );
edict_t *W_Fire_Grenade( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, float radius, int timeout, int mod, int timeDelta, bool aim_up );
edict_t *W_Fire_Rocket( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int timeout, int mod, int timeDelta );
edict_t *W_Fire_Plasma( edict_t *self, vec3_t start, vec3_t angles, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int speed, int timeout, int mod, int timeDelta );
void W_Fire_Electrobolt_FullInstant( edict_t *self, vec3_t start, vec3_t angles, float maxdamage, float mindamage, int maxknockback, int minknockback, int stun, int range, int minDamageRange, int mod, int timeDelta );
void W_Fire_Electrobolt_Combined( edict_t *self, vec3_t start, vec3_t angles, float maxdamage, float mindamage, float maxknockback, float minknockback, int stun, int range, int mod, int timeDelta );
edict_t *W_Fire_Shockwave( edict_t *self, vec3_t start, vec3_t angles, int speed, float damage, int minKnockback, int maxKnockback, int stun, int minDamage, int radius, int timeout, int mod, int timeDelta );
edict_t *W_Fire_Electrobolt_Weak( edict_t *self, vec3_t start, vec3_t angles, float speed, float damage, int minKnockback, int maxKnockback, int stun, int timeout, int mod, int timeDelta );
edict_t *W_Fire_Lasergun( edict_t *self, vec3_t start, vec3_t angles, float damage, int knockback, int stun, int timeout, int mod, int timeDelta );
edict_t *W_Fire_Lasergun_Weak( edict_t *self, vec3_t start, vec3_t end, float damage, int knockback, int stun, int timeout, int mod, int timeDelta );
void W_Fire_Instagun( edict_t *self, vec3_t start, vec3_t angles, float damage, int knockback, int stun, int radius, int range, int mod, int timeDelta );

void W_Detonate_Rocket( edict_t *ent, const edict_t *ignore, const cplane_t *plane, int surfFlags );
void W_Detonate_Grenade( edict_t *self, const edict_t *ignore );
void W_Detonate_Wave( edict_t *ent, const edict_t *ignore, const cplane_t *plane, int surfFlags );

bool Pickup_Weapon( edict_t *other, const gsitem_t *item, int flags, int ammo_count );
edict_t *Drop_Weapon( edict_t *ent, const gsitem_t *item );
void Use_Weapon( edict_t *ent, const gsitem_t *item );

//
// g_chasecam	//newgametypes
//
void G_SpectatorMode( edict_t *ent );
void G_ChasePlayer( edict_t *ent, const char *name, bool teamonly, int followmode );
void G_ChaseStep( edict_t *ent, int step );
void Cmd_SwitchChaseCamMode_f( edict_t *ent, const CmdArgs & );
void Cmd_ChaseCam_f( edict_t *ent, const CmdArgs & );
void Cmd_Spec_f( edict_t *ent, const CmdArgs & );
void G_EndServerFrames_UpdateChaseCam( void );

//
// g_client.c
//
void G_InitBodyQueue( void );
void G_Client_UpdateActivity( Client *client );
void G_Client_InactivityRemove( Client *client, int64_t inactivityMillis );
void G_ClientRespawn( edict_t *self, bool ghost );
void G_ClientClearStats( edict_t *ent );
void G_GhostClient( edict_t *self );
void G_ClientThink( edict_t *ent );
void G_CheckClientRespawnClick( edict_t *ent );
void G_PredictedEvent( int entNum, int ev, int parm );
void G_TeleportPlayer( edict_t *player, edict_t *dest );
bool G_PlayerCanTeleport( edict_t *player );

//
// g_player.c
//
void player_pain( edict_t *self, edict_t *other, float kick, int damage );
void player_die( edict_t *self, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );
void player_think( edict_t *self );

//
// g_target.c
//
void target_laser_start( edict_t *self );

void SP_target_temp_entity( edict_t *ent );
void SP_target_speaker( edict_t *ent );
void SP_target_explosion( edict_t *ent );
void SP_target_crosslevel_trigger( edict_t *ent );
void SP_target_crosslevel_target( edict_t *ent );
void SP_target_laser( edict_t *self );
void SP_target_lightramp( edict_t *self );
void SP_target_string( edict_t *ent );
void SP_target_location( edict_t *self );
void SP_target_position( edict_t *self );
void SP_target_print( edict_t *self );
void SP_target_give( edict_t *self );
void SP_target_changelevel( edict_t *ent );
void SP_target_relay( edict_t *self );
void SP_target_delay( edict_t *ent );
void SP_target_teleporter( edict_t *self );
void SP_target_kill( edict_t *self );


//
// g_svcmds.c
//
bool SV_FilterPacket( const char *from );
void G_AddServerCommands( void );
void G_RemoveCommands( void );
void SV_InitIPList( void );
void SV_ShutdownIPList( void );

//
// p_view.c
//
void G_ClientEndSnapFrame( edict_t *ent );
void G_ClientAddDamageIndicatorImpact( Client *client, int damage, const vec3_t dir );
void G_ClientDamageFeedback( edict_t *ent );

//
// g_phys.c
//
void SV_Impact( edict_t *e1, trace_t *trace );
void G_RunEntity( edict_t *ent );
int G_BoxSlideMove( edict_t *ent, int contentmask, float slideBounce, float friction );

//
// g_main.c
//

#ifndef _MSC_VER
void G_Error( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) ) __attribute__( ( noreturn ) );
void G_Printf( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
#else
__declspec( noreturn ) void G_Error( _Printf_format_string_ const char *format, ... );
void G_Printf( _Printf_format_string_ const char *format, ... );
#endif

void G_ExitLevel( void );
void G_RestartLevel( void );

void G_Timeout_Reset( void );


//
// g_frame.c
//
void G_CheckCvars( void );
void G_SnapClients( void );

//
// g_spawn.c
//
bool G_CallSpawn( edict_t *ent );
bool G_RespawnLevel( void );
void G_ResetLevel( void );
const char *G_GetEntitySpawnKey( const char *key, edict_t *self );

//
// g_awards.c
//
void G_PlayerAward( const edict_t *ent, const char *awardMsg );
void G_PlayerMetaAward( const edict_t *ent, const char *awardMsg );
void G_AwardPlayerHit( edict_t *targ, edict_t *attacker, int mod );
void G_AwardPlayerMissedElectrobolt( edict_t *self, int mod );
void G_AwardPlayerMissedLasergun( edict_t *self, int mod );
void G_AwardPlayerKilled( edict_t *self, edict_t *inflictor, edict_t *attacker, int mod );
void G_AwardPlayerPickup( edict_t *self, edict_t *item );
void G_AwardResetPlayerComboStats( edict_t *ent );
void G_AwardRaceRecord( edict_t *self );
void G_DeathAwards( edict_t *ent );

void G_SendActionRequest( const edict_t *ent, const wsw::StringView &tag, unsigned timeout,
						  const wsw::StringView &title, const wsw::StringView &desc,
						  const std::pair<wsw::StringView, wsw::StringView> *actionsBegin,
						  const std::pair<wsw::StringView, wsw::StringView> *actionsEnd );

// A workaround for the lack of ranges in the current target language version
template <typename ActionsRange>
void G_SendActionRequest( const edict_t *ent, const wsw::StringView &tag, unsigned timeout,
						  const wsw::StringView &title, const wsw::StringView &desc,
						  const ActionsRange &actions ) {
	G_SendActionRequest( ent, tag, timeout, title, desc, std::begin( actions ), std::end( actions ) );
}

//============================================================================

#include "ai/ai.h"

typedef struct {
	int radius;
	float minDamage;
	float maxDamage;
	float minKnockback;
	float maxKnockback;
	int stun;
} projectileinfo_t;

typedef struct {
	bool active;
	int target;
	int mode;                   //3rd or 1st person
	int range;
	bool teamonly;
	int64_t timeout;           //delay after loosing target
	int followmode;
} chasecam_t;

typedef struct {
	// fixed data
	vec3_t start_origin;
	vec3_t start_angles;
	vec3_t end_origin;
	vec3_t end_angles;

	int sound_start;
	int sound_middle;
	int sound_end;

	vec3_t movedir;  // direction defined in the bsp

	float speed;
	float distance;    // used by binary movers

	float wait;

	float phase;

	// state data
	int state;
	vec3_t dir;             // used by func_bobbing and func_pendulum
	float current_speed;    // used by func_rotating

	void ( *endfunc )( edict_t * );
	void ( *blocked )( edict_t *self, edict_t *other );

	vec3_t dest;
	vec3_t destangles;
} moveinfo_t;

typedef struct {
	int ebhit_count;
	int multifrag_timer;
	int multifrag_count;
	int frag_count;

	int accuracy_award;
	int directrocket_award;
	int directgrenade_award;
	int directwave_award;
	int multifrag_award;
	int spree_award;
	int gl_midair_award;
	int rl_midair_award;
	int sw_midair_award;

	int uh_control_award;
	int mh_control_award;
	int ra_control_award;

	uint8_t combo[MAX_CLIENTS]; // combo management for award
	edict_t *lasthit;
	int64_t lasthit_time;

	bool fairplay_award;
} award_info_t;

#define MAX_CLIENT_EVENTS   16
#define MAX_CLIENT_EVENTS_MASK ( MAX_CLIENT_EVENTS - 1 )

#define G_MAX_TIME_DELTAS   8
#define G_MAX_TIME_DELTAS_MASK ( G_MAX_TIME_DELTAS - 1 )

#include <common/types/staticstring.h>
#include <common/helpers/userinfo.h>
#include <common/helpers/tonum.h>

class FloodState {
	int64_t m_timestamps[MAX_FLOOD_MESSAGES] {};
	int m_head { 0 };
public:
	void clear() {
		std::memset( m_timestamps, 0, sizeof( m_timestamps ) );
		m_head = 0;
	}

	[[nodiscard]]
	bool shouldBeActive( int64_t realTime, int protectionMessages, int64_t protectionMillis ) const {
		assert( protectionMessages >= 0 );
		assert( protectionMillis >= 0 );

		int index = m_head - protectionMessages + 1;
		if( index < 0 ) {
			index += MAX_FLOOD_MESSAGES;
		}

		if( m_timestamps[index] && m_timestamps[index] <= realTime ) {
			if( realTime < m_timestamps[index] + protectionMillis ) {
				return true;
			}
		}

		return false;
	}

	void touch( int64_t realTime ) {
		m_head = ( m_head + 1 ) % MAX_FLOOD_MESSAGES;
		m_timestamps[m_head] = realTime;
	}
};

#define MAX_CLANNAME_BYTES 16
#define MAX_CLANNAME_CHARS 8

struct Client : public ServersideClientBase {
	wsw::UserInfo m_userInfo;

	void setName( const wsw::StringView &inputName );
	void setClan( const wsw::StringView &inputClan );
	void setCleanNameResolvingNameClash( const wsw::StringView &name );
	[[nodiscard]]
	auto findClientWithTheSameColorlessName( const wsw::StringView &colorlessName ) -> const Client *;
	void setSkinFromInfo();

	void runPMove( pmove_t *pm );
	void checkRegeneration();
	void runTouch( int *entNums, int numTouchEnts );
	void checkInstaShield();
	[[nodiscard]]
	auto ucmdToPlayerKeys() const -> uint8_t;
	[[nodiscard]]
	bool hasNewActivity( const usercmd_t &oldUcmd ) const;
	void executeUcmd( const usercmd_t &ucmd_, int timeDelta_ );

	// The maximum name length in characters without counting color tokens is controlled by MAX_NAME_CHARS constant
	wsw::StaticString<MAX_NAME_BYTES> netname;
	wsw::StaticString<MAX_NAME_BYTES> colorlessNetname;
	wsw::StaticString<MAX_CLANNAME_BYTES> clanname;

	wsw::StaticString<MAX_INFO_VALUE> ip;
	wsw::StaticString<MAX_INFO_VALUE> socket;

	// is a zero UUID if a client is not authenticated
	// is an all-bits-set UUID if the session is local
	// is a valid UUID if a client is authenticated
	mm_uuid_t mm_session;

	bool connecting;

	byte_vec4_t color;
	int team;
	int hand;
	int handicap;
	int movestyle;
	int movestyle_latched;
	bool isoperator;
	int64_t queueTimeStamp;

	usercmd_t ucmd;
	int64_t timeDelta;
	MovingAverage<8> deltas;

	pmove_state_t old_pmove;    // for detecting out-of-pmove changes

	// Team state
	int64_t teamStateTimestamp; // last time it was reset

	bool is_coach;

	int64_t readyUpWarningNext; // (timer) warn people to ready up
	int readyUpWarningCount;

	// for position command
	bool position_saved;
	vec3_t position_origin;
	vec3_t position_angles;
	int64_t position_lastcmd;

	const gsitem_t *last_drop_item;
	vec3_t last_drop_location;
	edict_t *last_pickup;
	edict_t *last_killer;

	// Respawn

	struct {
		int buttons;
		uint8_t plrkeys;
		int damageTaken;
		vec3_t damageTakenDir;
	} snap;

	chasecam_t chase;
	award_info_t awardInfo;

	int64_t spawnStateTimestamp; // last time it was reset

	// player_state_t event
	int events[MAX_CLIENT_EVENTS];
	unsigned int eventsCurrent;
	unsigned int eventsHead;

	gs_laserbeamtrail_t trail;

	bool takeStun;
	float armor;
	float instashieldCharge;

	int64_t next_drown_time;
	int drowningDamage;
	int old_waterlevel;
	int old_watertype;

	int64_t pickup_msg_time;

	int64_t levelTimestamp;				// last time it was reset

	unsigned int respawnCount;
	matchmessage_t matchmessage;
	unsigned int helpmessage;

	int64_t last_activity;

	score_stats_t stats;
	bool showscores;

	// flood protection
	FloodState m_floodState;
	FloodState m_floodTeamState;

	int64_t flood_locktill;			// locked from talking

	int64_t callvote_when;

	char quickMenuItems[1024];

	void resetLevelState();
	void resetSpawnState();
	void resetTeamState();

	void reset();

	[[nodiscard]]
	auto getInfoValue( const wsw::HashedStringView &key ) const -> std::optional<wsw::StringView> {
		return m_userInfo.get( key );
	}
	[[nodiscard]]
	auto getInfoValueOrEmpty( const wsw::HashedStringView &key ) const -> wsw::StringView {
		return m_userInfo.getOrEmpty( key );
	}
	[[nodiscard]]
	auto getInfoValueOrThrow( const wsw::HashedStringView &key ) const -> wsw::StringView {
		return m_userInfo.getOrThrow( key );
	}
	[[nodiscard]]
	auto getNonEmptyInfoValue( const wsw::HashedStringView &key ) const -> std::optional<wsw::StringView> {
		if( const auto maybeValue = m_userInfo.get( key ); maybeValue && !maybeValue->empty() ) {
			return maybeValue;
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto getMMLoginName() const -> std::optional<wsw::StringView> {
		return getNonEmptyInfoValue( wsw::HashedStringView( "cl_mm_login" ) );
	}

	[[nodiscard]]
	auto getChatFilterFlags() const -> int {
		if( const auto maybeString = m_userInfo.get( wsw::HashedStringView( "cg_chatFilter" ) ) ) {
			if( const auto maybeValue = wsw::toNum<int>( *maybeString ) ) {
				return *maybeValue;
			}
		}
		return 0;
	}

	void setReplicatedStats();

	void setUserInfo( const wsw::StringView &rawInfo );

	void handleUserInfoChanges();

	[[nodiscard]]
	auto getEntity() -> edict_t *;

	[[nodiscard]]
	auto getEntity() const -> const edict_t *;

	[[nodiscard]]
	bool isFakeClient() const;
};

struct RespectStats : public GVariousStats {
	RespectStats(): GVariousStats( 31 ) {}

	bool hasIgnoredCodex { false };
	bool hasViolatedCodex { false };
};

class ChatHandlersChain;

typedef struct snap_edict_s {
	// whether we have killed anyone this snap
	bool kill;
	bool teamkill;

	// ents can accumulate damage along the frame, so they spawn less events
	float damage_taken;
	float damage_saved;         // saved by the armor.
	vec3_t damage_dir;
	vec3_t damage_at;
	float damage_given;             // for hitsounds
	float damageteam_given;
	float damage_fall;
} snap_edict_t;

typedef struct {
	int speed;
	int shaderIndex;
	int spread;
	int size;
	int time;
	bool spherical;
	bool bounce;
	bool gravity;
	bool expandEffect;
	bool shrinkEffect;
	int frequency;
} particles_edict_t;

class Bot;

struct edict_s {
	entity_state_t s;
	entity_shared_t r;

	// DO NOT MODIFY ANYTHING ABOVE THIS, THE SERVER
	// EXPECTS THE FIELDS IN THAT ORDER!

	//================================

	int linkcount;

	// physics grid areas this edict is linked into
	link_t areagrid[MAX_ENT_AREAS];

	entity_state_t olds; // state in the last sent frame snap

	int movetype;
	int flags;

	const char *model;
	const char *model2;
	int64_t freetime;          // time when the object was freed

	int numEvents;
	bool eventPriority[2];

	//
	// only used locally in game, not by server
	//

	const char *classname;
	const char *spawnString;            // keep track of string definition of this entity
	int spawnflags;

	int64_t nextThink;

	void ( *think )( edict_t *self );
	void ( *touch )( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags );
	void ( *use )( edict_t *self, edict_t *other, edict_t *activator );
	void ( *pain )( edict_t *self, edict_t *other, float kick, int damage );
	void ( *die )( edict_t *self, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );
	void ( *stop )( edict_t *self );

	const char *target;
	const char *targetname;
	const char *killtarget;
	const char *team;
	const char *pathtarget;
	edict_t *target_ent;

	vec3_t velocity;
	vec3_t avelocity;

	float angle;                // set in qe3, -1 = up, -2 = down
	float speed;
	float accel, decel;         // usef for func_rotating

	int64_t timeStamp;
	int64_t deathTimeStamp;
	int timeDelta;              // SVF_PROJECTILE only. Used for 4D collision detection

	projectileinfo_t projectileInfo;    // specific for projectiles
	particles_edict_t particlesInfo;        // specific for ET_PARTICLES

	int dmg;

	const char *message;
	const char *helpmessage;
	unsigned mapmessage_index;

	int mass;
	int64_t air_finished;
	float gravity;              // per entity gravity multiplier (1.0 is normal) // use for lowgrav artifact, flares

	edict_t *goalentity;
	edict_t *movetarget;
	float yaw_speed;

	int64_t pain_debounce_time;

	float health;
	int max_health;
	int gib_health;
	int deadflag;

	const char *map;			// target_changelevel

	int viewheight;				// height above origin where eyesight is determined
	int takedamage;
	int floodnum;
	
	const char *sounds;         // make this a spawntemp var?
	int count;

	int64_t timeout;			// for SW and fat PG

	edict_t *chain;
	edict_t *enemy;
	edict_t *oldenemy;
	edict_t *activator;
	edict_t *groundentity;
	int groundentity_linkcount;
	edict_t *teamchain;
	edict_t *teamleader;
	int noise_index;
	float attenuation;

	// timing variables
	float wait;
	float delay;                // before firing targets
	float random;

	int watertype;
	int waterlevel;

	int style;                  // also used as areaportal number

	float light;
	vec3_t color;

	const gsitem_t *item;       // for bonus items
	int invpak[AMMO_TOTAL];     // small inventory-like for dropped backpacks. Handles weapons and ammos of both types

	// common data blocks
	moveinfo_t moveinfo;        // func movers movement

	Bot *bot;

	float aiIntrinsicEnemyWeight;
	float aiVisibilityDistance;

	snap_edict_t snap; // information that is cleared each frame snap

	//JALFIXME
	bool is_swim;
	bool is_step;
	bool is_ladder;
	bool was_swim;
	bool was_step;

	edict_t *trigger_entity;
	int64_t trigger_timeout;

	bool linked;

	bool scriptSpawned;
	void *asScriptModule;
	void *asSpawnFunc, *asThinkFunc, *asUseFunc, *asTouchFunc, *asPainFunc, *asDieFunc, *asStopFunc;
};

static inline int ENTNUM( const edict_t *x ) { return x - game.edicts; }
static inline int ENTNUM( const Client *x ) { return x - game.clients + 1; }

static inline int PLAYERNUM( const edict_t *x ) { return x - game.edicts - 1; }
static inline int PLAYERNUM( const Client *x ) { return x - game.clients; }

static inline edict_t *PLAYERENT( int x ) { return game.edicts + x + 1; }

void *Q_malloc( size_t size );
void *Q_realloc( void *buf, size_t newsize );
void Q_free( void *buf );
char *Q_strdup( const char *str );

#define gDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Game, wsw::MessageCategory::Debug ) ).getWriter()
#define gNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Game, wsw::MessageCategory::Notice ) ).getWriter()
#define gWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Game, wsw::MessageCategory::Warning ) ).getWriter()
#define gError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Game, wsw::MessageCategory::Error ) ).getWriter()