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

// cg_local.h -- local definitions for client game module

#define CGAME_HARD_LINKED

#include "../gameshared/q_arch.h"
#include "../gameshared/q_math.h"
#include "../gameshared/q_shared.h"
#include "../gameshared/q_cvar.h"
#include "../gameshared/q_comref.h"
#include "../gameshared/q_collision.h"

#include "../gameshared/gs_public.h"
#include "../ref/ref.h"

#include "cg_public.h"

#include <algorithm>
#include <cmath>
#include <memory>
#include <new>
#include <utility>

#define ITEM_RESPAWN_TIME   1000

#define FLAG_TRAIL_DROP_DELAY 300
#define HEADICON_TIMEOUT 4000

#define GAMECHAT_STRING_SIZE    1024
#define GAMECHAT_STACK_SIZE     20

#define CG_MAX_TOUCHES 10

enum {
	LOCALEFFECT_EV_PLAYER_TELEPORT_IN
	, LOCALEFFECT_EV_PLAYER_TELEPORT_OUT
	, LOCALEFFECT_LASERBEAM
	, LOCALEFFECT_LASERBEAM_SMOKE_TRAIL
	, LOCALEFFECT_EV_WEAPONBEAM
	, MAX_LOCALEFFECTS = 64
};

typedef struct {
	int x, y, width, height;
} vrect_t;

typedef struct {
	entity_state_t current;
	entity_state_t prev;        // will always be valid, but might just be a copy of current

	int serverFrame;            // if not current, this ent isn't in the frame
	int64_t fly_stoptime;

	int64_t respawnTime;

	entity_t ent;                   // interpolated, to be added to render list
	unsigned int type;
	unsigned int renderfx;
	unsigned int effects;
	struct cgs_skeleton_s *skel;

	vec3_t velocity;

	bool canExtrapolate;
	bool canExtrapolatePrev;
	vec3_t prevVelocity;
	int microSmooth;
	vec3_t microSmoothOrigin;
	vec3_t microSmoothOrigin2;
	//vec3_t prevExtrapolatedOrigin;
	//vec3_t extrapolatedOrigin;

	gsitem_t    *item;

	// local effects from events timers
	int64_t localEffects[MAX_LOCALEFFECTS];

	// attached laser beam
	vec3_t laserOrigin;
	vec3_t laserPoint;
	vec3_t laserOriginOld;
	vec3_t laserPointOld;
	bool laserCurved;

	bool linearProjectileCanDraw;
	vec3_t linearProjectileViewerSource;
	vec3_t linearProjectileViewerVelocity;

	vec3_t teleportedTo;
	vec3_t teleportedFrom;
	byte_vec4_t outlineColor;

	// used for client side animation of player models
	bool pendingAnimationsUpdate;
	int lastAnims;
	int lastVelocitiesFrames[4];
	float lastVelocities[4][4];
	bool jumpedLeft;
	vec3_t animVelocity;
	float yawVelocity;
} centity_t;

#include "cg_pmodels.h"

#include "mediacache.h"
#include "crosshairstate.h"

#define STAT_MINUS              10  // num frame for '-' stats digit

typedef struct bonenode_s {
	int bonenum;
	int numbonechildren;
	struct bonenode_s **bonechildren;
} bonenode_t;

typedef struct cg_tagmask_s {
	char tagname[64];
	char bonename[64];
	int bonenum;
	struct cg_tagmask_s *next;
	vec3_t offset;
	vec3_t rotate;
} cg_tagmask_t;

typedef struct {
	char name[MAX_QPATH];
	int flags;
	int parent;
	struct bonenode_s *node;
} cgs_bone_t;

typedef struct cgs_skeleton_s {
	struct model_s *model;

	int numBones;
	cgs_bone_t *bones;

	int numFrames;
	bonepose_t **bonePoses;

	struct cgs_skeleton_s *next;

	// store the tagmasks as part of the skeleton (they are only used by player models, tho)
	struct cg_tagmask_s *tagmasks;

	struct bonenode_s *bonetree;
} cgs_skeleton_t;

#include "cg_boneposes.h"

typedef struct cg_sexedSfx_s {
	char *name;
	struct sfx_s *sfx;
	struct cg_sexedSfx_s *next;
} cg_sexedSfx_t;

typedef struct {
	char name[MAX_QPATH];
	char cleanname[MAX_QPATH];
	char clan[MAX_QPATH];
	char cleanclan[MAX_QPATH];
	int hand;
	byte_vec4_t color;
	struct shader_s *icon;
} cg_clientInfo_t;

#define MAX_ANGLES_KICKS 3

typedef struct {
	int64_t timestamp;
	int64_t kicktime;
	float v_roll, v_pitch;
} cg_kickangles_t;

#define MAX_COLORBLENDS 3

typedef struct {
	int64_t timestamp;
	int64_t blendtime;
	float blend[4];
} cg_viewblend_t;

#define PREDICTED_STEP_TIME 150 // stairs smoothing time

// view types
enum {
	VIEWDEF_CAMERA,
	VIEWDEF_PLAYERVIEW,

	VIEWDEF_MAXTYPES
};

typedef struct {
	int type;
	int POVent;
	bool thirdperson;
	bool playerPrediction;
	bool drawWeapon;
	bool draw2D;
	refdef_t refdef;
	float fracDistFOV;
	vec3_t origin;
	vec3_t angles;
	mat3_t axis;
	vec3_t velocity;
	bool flipped;
} cg_viewdef_t;

#include "cg_democams.h"

#include "../qcommon/configstringstorage.h"

// this is not exactly "static" but still...
typedef struct {
	const char *serverName;
	const char *demoName;
	unsigned int playerNum;

	// shaders
	struct shader_s *shaderWhite;
	struct shader_s *shaderMiniMap;

	// fonts
	char fontSystemFamily[MAX_QPATH];
	char fontSystemMonoFamily[MAX_QPATH];
	int fontSystemSmallSize;
	int fontSystemMediumSize;
	int fontSystemBigSize;

	struct qfontface_s *fontSystemSmall;
	struct qfontface_s *fontSystemMedium;
	struct qfontface_s *fontSystemBig;

	MediaCache media;

	bool precacheDone;

	int vidWidth, vidHeight;
	float pixelRatio;

	bool demoPlaying;
	bool demoTutorial;
	bool pure;
	bool gameMenuRequested;
	int gameProtocol;
	char demoExtension[MAX_QPATH];
	unsigned snapFrameTime;
	unsigned extrapolationTime;

	char *demoAudioStream;

	//
	// locally derived information from server state
	//
	wsw::ConfigStringStorage configStrings;
	wsw::ConfigStringStorage baseConfigStrings;

	bool hasGametypeMenu;

	char weaponModels[WEAP_TOTAL][MAX_QPATH];
	int numWeaponModels;
	weaponinfo_t *weaponInfos[WEAP_TOTAL];    // indexed list of weapon model infos
	orientation_t weaponItemTag;

	cg_clientInfo_t clientInfo[MAX_CLIENTS];

	struct model_s *modelDraw[MAX_MODELS];

	struct pmodelinfo_s *pModelsIndex[MAX_MODELS];
	struct pmodelinfo_s *basePModelInfo; //fall back replacements
	struct Skin *baseSkin;

	// force models
	struct pmodelinfo_s *teamModelInfo[GS_MAX_TEAMS];
	struct Skin *teamCustomSkin[GS_MAX_TEAMS]; // user defined
	int teamColor[GS_MAX_TEAMS];

	struct sfx_s *soundPrecache[MAX_SOUNDS];
	struct shader_s *imagePrecache[MAX_IMAGES];
	struct Skin *skinPrecache[MAX_SKINFILES];

	int precacheModelsStart;
	int precacheSoundsStart;
	int precacheShadersStart;
	int precacheSkinsStart;
	int precacheClientsStart;

	char checkname[MAX_QPATH];
	char loadingstring[MAX_QPATH];
	int precacheCount, precacheTotal, precacheStart;
	int64_t precacheStartMsec;
} cg_static_t;

typedef struct {
	int64_t time;
	char text[GAMECHAT_STRING_SIZE];
} cg_gamemessage_t;

#define MAX_HELPMESSAGE_CHARS 4096

#include "particlesystem.h"
#include "polyeffectssystem.h"
#include "effectssystemfacade.h"
#include "simulatedhullssystem.h"

typedef struct cg_state_s {
	int64_t time;
	float delay;

	int64_t realTime;
	int frameTime;
	int realFrameTime;
	int frameCount;

	int64_t firstViewRealTime;
	int viewFrameCount;
	bool startedMusic;

	snapshot_t frame, oldFrame;
	bool frameSequenceRunning;
	bool oldAreabits;
	bool portalInView;
	bool fireEvents;
	bool firstFrame;

	float predictedOrigins[CMD_BACKUP][3];              // for debug comparing against server

	float predictedStep;                // for stair up smoothing
	int64_t predictedStepTime;

	int64_t predictingTimeStamp;
	int64_t predictedEventTimes[PREDICTABLE_EVENTS_MAX];
	vec3_t predictionError;
	player_state_t predictedPlayerState;     // current in use, predicted or interpolated
	int predictedWeaponSwitch;              // inhibit shooting prediction while a weapon change is expected
	int predictedGroundEntity;
	gs_laserbeamtrail_t weaklaserTrail;

	// prediction optimization (don't run all ucmds in not needed)
	int64_t predictFrom;
	entity_state_t predictFromEntityState;
	player_state_t predictFromPlayerState;

	int lastWeapon;

	mat3_t autorotateAxis;

	float lerpfrac;                     // between oldframe and frame
	float xerpTime;
	float oldXerpTime;
	float xerpSmoothFrac;

	int effects;

	vec3_t lightingOrigin;

	bool showScoreboard;            // demos and multipov
	bool specStateChanged;

	unsigned int multiviewPlayerNum;       // for multipov chasing, takes effect on next snap

	int pointedNum;
	int64_t pointRemoveTime;
	int pointedHealth;
	int pointedArmor;

	//
	// all cyclic walking effects
	//
	float xyspeed;

	float oldBobTime;
	int bobCycle;                   // odd cycles are right foot going forward
	float bobFracSin;               // sin(bobfrac*M_PI)

	//
	// kick angles and color blend effects
	//

	cg_kickangles_t kickangles[MAX_ANGLES_KICKS];
	cg_viewblend_t colorblends[MAX_COLORBLENDS];
	int64_t damageBlends[4];
	int64_t fallEffectTime;
	int64_t fallEffectRebounceTime;

	//
	// transient data from server
	//
	const char *matchmessage;
	char helpmessage[MAX_HELPMESSAGE_CHARS];
	int64_t helpmessage_time;
	char *motd;
	int64_t motd_time;

	cg_viewweapon_t weapon;
	cg_viewdef_t view;

	CrosshairState crosshairState { CrosshairState::Weak, 350 };
	CrosshairState strongCrosshairState { CrosshairState::Strong, 300 };

	ParticleSystem particleSystem;
	SimulatedHullsSystem simulatedHullsSystem;
	EffectsSystemFacade effectsSystem;
	PolyEffectsSystem polyEffectsSystem;
} cg_state_t;

extern cg_static_t cgs;
extern cg_state_t cg;

#define ISVIEWERENTITY( entNum )  ( ( cg.predictedPlayerState.POVnum > 0 ) && ( (int)cg.predictedPlayerState.POVnum == entNum ) && ( cg.view.type == VIEWDEF_PLAYERVIEW ) )
#define ISBRUSHMODEL( x ) ( ( ( x > 0 ) && ( (int)x < CG_NumInlineModels() ) ) ? true : false )

#define ISREALSPECTATOR()       ( cg.frame.playerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR )
#define SPECSTATECHANGED()      ( ( cg.frame.playerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR ) != ( cg.oldFrame.playerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR ) )

extern centity_t cg_entities[MAX_EDICTS];

//
// cg_ents.c
//
extern cvar_t *cg_gun;
extern cvar_t *cg_gun_alpha;

const struct cmodel_s *CG_CModelForEntity( int entNum );
void CG_SoundEntityNewState( centity_t *cent );
void CG_AddEntities( DrawSceneRequest *drawSceneRequest );
void CG_LerpEntities( void );
void CG_LerpGenericEnt( centity_t *cent );

void CG_SetOutlineColor( byte_vec4_t outlineColor, byte_vec4_t color );
void CG_AddColoredOutLineEffect( entity_t *ent, int effects, uint8_t r, uint8_t g, uint8_t b, uint8_t a );
void CG_AddCentityOutLineEffect( centity_t *cent );

void CG_AddFlagModelOnTag( centity_t *cent, byte_vec4_t teamcolor, const char *tagname, DrawSceneRequest * );

void CG_ResetItemTimers( void );
centity_t *CG_GetItemTimerEnt( int num );

//
// cg_draw.c
//
int CG_HorizontalAlignForWidth( const int x, int align, int width );
int CG_VerticalAlignForHeight( const int y, int align, int height );

void CG_RegisterLevelMinimap( void );
void CG_RegisterFonts( void );

struct model_s *CG_RegisterModel( const char *name );

//
// cg_players.c
//
extern cvar_t *cg_model;
extern cvar_t *cg_skin;
extern cvar_t *cg_hand;

void CG_ResetClientInfos( void );
void CG_LoadClientInfo( unsigned client, const wsw::StringView &configString );
void CG_UpdateSexedSoundsRegistration( pmodelinfo_t *pmodelinfo );
void CG_SexedSound( int entnum, int entchannel, const char *name, float fvol, float attn );
struct sfx_s *CG_RegisterSexedSound( int entnum, const char *name );

//
// cg_predict.c
//
extern cvar_t *cg_predict;
extern cvar_t *cg_predict_optimize;
extern cvar_t *cg_showMiss;

void CG_PredictedEvent( int entNum, int ev, int parm );
void CG_Predict_ChangeWeapon( int new_weapon );
void CG_PredictMovement( void );
void CG_CheckPredictionError( void );
void CG_BuildSolidList( void );
void CG_Trace( trace_t *t, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, int ignore, int contentmask );
int CG_PointContents( const vec3_t point );
void CG_Predict_TouchTriggers( pmove_t *pm, const vec3_t previous_origin );

//
// cg_screen.c
//
extern vrect_t scr_vrect;

void CG_ScreenInit( void );
void CG_Draw2D( void );
void CG_CalcVrect( void );
void CG_CenterPrint( const char *str );

void CG_InitHUD();
void CG_ShutdownHUD();
void CG_DrawHUD();

void CG_LoadingString( const char *str );
bool CG_LoadingItemName( const char *str );

void CG_DrawCrosshair();
void CG_DrawKeyState( int x, int y, int w, int h, int align, const char *key );

void CG_ScreenCrosshairDamageUpdate( void );

void CG_DrawPlayerNames();
void CG_DrawTeamMates( void );
void CG_DrawNet( int x, int y, int w, int h, int align, vec4_t color );

void CG_ClearPointedNum( void );

void CG_SC_ResetFragsFeed( void );

//
// cg_scoreboard.c
//
void CG_ScoresOn_f();
void CG_ScoresOff_f();
bool CG_IsScoreboardShown();

void CG_MessageMode();
void CG_MessageMode2();

//
// cg_main.c
//
extern cvar_t *developer;
extern cvar_t *cg_showClamp;

// wsw
extern cvar_t *cg_volume_hitsound;    // hit sound volume
extern cvar_t *cg_autoaction_demo;
extern cvar_t *cg_autoaction_screenshot;
extern cvar_t *cg_autoaction_stats;
extern cvar_t *cg_autoaction_spectator;
extern cvar_t *cg_simpleItems; // simple items
extern cvar_t *cg_simpleItemsSize; // simple items
extern cvar_t *cg_volume_players; // players sound volume
extern cvar_t *cg_volume_effects; // world sound volume
extern cvar_t *cg_volume_announcer; // announcer sounds volume
extern cvar_t *cg_projectileTrail;
extern cvar_t *cg_projectileFireTrail;
extern cvar_t *cg_bloodTrailTime;
extern cvar_t *cg_bloodTrailPalette;
extern cvar_t *cg_showPOVBlood;
extern cvar_t *cg_projectileFireTrailAlpha;

extern cvar_t *cg_cartoonEffects;
extern cvar_t *cg_cartoonHitEffect;

extern cvar_t *cg_heavyRocketExplosions;
extern cvar_t *cg_heavyGrenadeExplosions;
extern cvar_t *cg_heavyShockwaveExplosions;

extern cvar_t *cg_explosionsWave;
extern cvar_t *cg_explosionsSmoke;
extern cvar_t *cg_gibs;
extern cvar_t *cg_outlineModels;
extern cvar_t *cg_outlineWorld;
extern cvar_t *cg_outlinePlayers;

extern cvar_t *cg_drawEntityBoxes;
extern cvar_t *cg_fov;
extern cvar_t *cg_zoomfov;
extern cvar_t *cg_movementStyle;
extern cvar_t *cg_noAutohop;
extern cvar_t *cg_particles;
extern cvar_t *cg_showhelp;
extern cvar_t *cg_predictLaserBeam;
extern cvar_t *cg_showSelfShadow;
extern cvar_t *cg_laserBeamSubdivisions;
extern cvar_t *cg_projectileAntilagOffset;
extern cvar_t *cg_raceGhosts;
extern cvar_t *cg_raceGhostsAlpha;
extern cvar_t *cg_chatBeep;
extern cvar_t *cg_chatFilter;
extern cvar_t *cg_chatShowIgnored;

//force models
extern cvar_t *cg_teamPLAYERSmodel;
extern cvar_t *cg_teamPLAYERSmodelForce;
extern cvar_t *cg_teamALPHAmodel;
extern cvar_t *cg_teamALPHAmodelForce;
extern cvar_t *cg_teamBETAmodel;
extern cvar_t *cg_teamBETAmodelForce;

extern cvar_t *cg_teamPLAYERSskin;
extern cvar_t *cg_teamALPHAskin;
extern cvar_t *cg_teamBETAskin;

extern cvar_t *cg_teamPLAYERScolor;
extern cvar_t *cg_teamPLAYERScolorForce;
extern cvar_t *cg_teamALPHAcolor;
extern cvar_t *cg_teamBETAcolor;

extern cvar_t *cg_forceMyTeamAlpha;

extern cvar_t *cg_teamColoredBeams;
extern cvar_t *cg_teamColoredInstaBeams;

extern cvar_t *cg_playList;
extern cvar_t *cg_playListShuffle;

extern cvar_t *cg_flashWindowCount;

extern cvar_t *cg_autoRespectMenu;

void CG_ValidateItemDef( int tag, const char *name );

#ifndef _MSC_VER
void CG_Error( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) ) __attribute__( ( noreturn ) );
void CG_LocalPrint( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
#else
__declspec( noreturn ) void CG_Error( _Printf_format_string_ const char *format, ... );
void CG_LocalPrint( _Printf_format_string_ const char *format, ... );
#endif

void CG_Precache( void );

void CG_UseItem( const char *name );
void CG_RegisterCGameCommands( void );
void CG_UnregisterCGameCommands( void );
void CG_OverrideWeapondef( int index, const char *cstring );

void CG_StartBackgroundTrack( void );

const char *CG_TranslateColoredString( const char *string, char *dst, size_t dst_size );

//
// cg_svcmds.c
//

void CG_SC_AutoRecordAction( const char *action );

//
// cg_teams.c
//
int CG_TeamToForcedTeam( int team );
void CG_RegisterTeamColor( int team );
void CG_RegisterForceModels( void );
void CG_SetSceneTeamColors( void );
bool CG_PModelForCentity( centity_t *cent, pmodelinfo_t **pmodelinfo, struct Skin **skin );
vec_t *CG_TeamColor( int team, vec4_t color );
void AdjustTeamColorValue( vec4_t color );
uint8_t *CG_TeamColorForEntity( int entNum, byte_vec4_t color );
uint8_t *CG_PlayerColorForEntity( int entNum, byte_vec4_t color );

//
// cg_view.c
//
enum {
	CAM_INEYES,
	CAM_THIRDPERSON,
	CAM_MODES
};

typedef struct {
	int mode;
	unsigned int cmd_mode_delay;
} cg_chasecam_t;

extern cg_chasecam_t chaseCam;

extern cvar_t *cg_flip;

extern cvar_t *cg_thirdPerson;
extern cvar_t *cg_thirdPersonAngle;
extern cvar_t *cg_thirdPersonRange;

extern cvar_t *cg_colorCorrection;

// Viewport bobbing on fall/high jumps
extern cvar_t *cg_viewBob;

void CG_ResetKickAngles( void );
void CG_ResetColorBlend( void );
void CG_ResetDamageIndicator( void );
void CG_DamageIndicatorAdd( int damage, const vec3_t dir );
void CG_StartKickAnglesEffect( vec3_t source, float knockback, float radius, int time );
void CG_StartColorBlendEffect( float r, float g, float b, float a, int time );
void CG_StartFallKickEffect( int bounceTime );
void CG_ViewSmoothPredictedSteps( vec3_t vieworg );
float CG_ViewSmoothFallKick( void );
void CG_AddKickAngles( vec3_t viewangles );
bool CG_ChaseStep( int step );
bool CG_SwitchChaseCamMode( void );
void CG_ClearChaseCam();

//
// cg_lents.c
//
void CG_ClearLocalEntities();
void CG_AddLocalEntities( DrawSceneRequest *request );
void CG_FreeLocalEntities();

inline void CG_SmallPileOfGibs( const vec3_t origin, int damage, const vec3_t initialVelocity, int team ) {}
void CG_PModel_SpawnTeleportEffect( centity_t *cent );
void CG_LaserGunImpact( const vec3_t pos, const vec3_t dir, float radius, const vec3_t laser_dir, const vec4_t color, DrawSceneRequest *drawSceneRequest );

//
// cg_decals.c
//
extern cvar_t *cg_addDecals;

//
// cg_polys.c	-	wsw	: jal
//
extern cvar_t *cg_ebbeam_old;
extern cvar_t *cg_ebbeam_width;
extern cvar_t *cg_ebbeam_time;
extern cvar_t *cg_instabeam_width;
extern cvar_t *cg_instabeam_time;

//
// cg_effects.c
//
inline void CG_ClearLightStyles( void ) {}
inline void CG_RunLightStyles( void ) {}
inline void CG_SetLightStyle( unsigned i, const wsw::StringView &s ) {}

//
//	cg_vweap.c - client weapon
//
void CG_AddViewWeapon( cg_viewweapon_t *viewweapon, DrawSceneRequest *drawSceneRequest );
void CG_CalcViewWeapon( cg_viewweapon_t *viewweapon );
void CG_ViewWeapon_StartAnimationEvent( int newAnim );
void CG_ViewWeapon_RefreshAnimation( cg_viewweapon_t *viewweapon );

//
// cg_events.c
//
//extern cvar_t *cg_footSteps;
extern cvar_t *cg_damage_indicator;
extern cvar_t *cg_damage_indicator_time;
extern cvar_t *cg_pickup_flash;
extern cvar_t *cg_weaponAutoSwitch;

void CG_FireEvents( bool early );
void CG_EntityEvent( entity_state_t *ent, int ev, int parm, bool predicted );
void CG_AddAnnouncerEvent( struct sfx_s *sound, bool queued );
void CG_ReleaseAnnouncerEvents( void );
void CG_ClearAnnouncerEvents( void );

// I don't know where to put these ones
void CG_WeaponBeamEffect( centity_t *cent );
void CG_LaserBeamEffect( centity_t *owner, DrawSceneRequest *drawSceneRequest );


//
// cg_input.cpp
//
void CG_InitInputVars();
void CG_InitInput( void );
void CG_ShutdownInput( void );

/**
 * Gets up to two bound keys for a command.
 *
 * @param cmd      console command to get binds for
 * @param keys     output string
 * @param keysSize output string buffer size
 */
void CG_GetBoundKeysString( const char *cmd, char *keys, size_t keysSize );

void NET_GetUserCmd( int frame, usercmd_t *cmd );
int NET_GetCurrentUserCmdNum();
void NET_GetCurrentState( int64_t *incomingAcknowledged, int64_t *outgoingSequence, int64_t *outgoingSent );

struct cmodel_s;

const cmodel_s *CG_InlineModel( int num );
const cmodel_s *CG_ModelForBBox( const vec3_t mins, const vec3_t maxs );
const cmodel_s *CG_OctagonModelForBBox( const vec3_t mins, const vec3_t maxs );
int CG_NumInlineModels();
void CG_TransformedBoxTrace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins, const vec3_t maxs, const cmodel_s *cmodel, int brushmask, const vec3_t origin, const vec3_t angles );
int CG_TransformedPointContents( const vec3_t p, const cmodel_s *cmodel, const vec3_t origin, const vec3_t angles );
void CG_InlineModelBounds( const cmodel_s *cmodel, vec3_t mins, vec3_t maxs );

