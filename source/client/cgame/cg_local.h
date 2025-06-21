/*
Copyright (C) 2002-2003 Victor Luchits
Copyright (C) 2024 Chasseur de bots

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

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/cvar.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/q_collision.h>
#include <common/facilities/gs_public.h>
#include <common/facilities/messagestreams.h>
#include <client/renderer/ref.h>

#include "cg_public.h"
#include "delayedexecutionsystem.h"
#include <client/ui/cgameimports.h>

extern gs_state_t *cggs;

#define ITEM_RESPAWN_TIME   1000

#define FLAG_TRAIL_DROP_DELAY 300
#define HEADICON_TIMEOUT 4000

#define GAMECHAT_STRING_SIZE    1024
#define GAMECHAT_STACK_SIZE     20

struct SoundSet;

enum {
	LOCALEFFECT_EV_PLAYER_TELEPORT_IN
	, LOCALEFFECT_EV_PLAYER_TELEPORT_OUT
	, LOCALEFFECT_LASERBEAM
	, LOCALEFFECT_LASERBEAM_SMOKE_TRAIL
	, LOCALEFFECT_EV_WEAPONBEAM
	, MAX_LOCALEFFECTS = 5
};

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

	const gsitem_t *item;

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

enum {
	WEAPMODEL_NOANIM,
	WEAPMODEL_STANDBY,
	WEAPMODEL_ATTACK_WEAK,
	WEAPMODEL_ATTACK_STRONG,
	WEAPMODEL_WEAPDOWN,
	WEAPMODEL_WEAPONUP,

	VWEAP_MAXANIMS
};

#define WEAPONINFO_MAX_FIRE_SOUNDS 4

//equivalent to pmodelinfo_t. Shared by different players, etc.
typedef struct weaponinfo_s {
	char name[MAX_QPATH];
	bool inuse;

	struct  model_s *model[WEAPMODEL_PARTS]; //one weapon consists of several models

	int firstframe[VWEAP_MAXANIMS];         //animation script
	int lastframe[VWEAP_MAXANIMS];
	int loopingframes[VWEAP_MAXANIMS];
	unsigned int frametime[VWEAP_MAXANIMS];

	orientation_t tag_projectionsource;
	byte_vec4_t outlineColor;

	// handOffset
	vec3_t handpositionOrigin;
	vec3_t handpositionAngles;

	// flash
	int64_t flashTime;
	bool flashFade;
	float flashRadius;
	vec3_t flashColor;

	// barrel
	int64_t barrelTime;
	float barrelSpeed;

	// sfx
	int num_fire_sounds;
	const SoundSet *sound_fire[WEAPONINFO_MAX_FIRE_SOUNDS];
	int num_strongfire_sounds;
	const SoundSet *sound_strongfire[WEAPONINFO_MAX_FIRE_SOUNDS];
	const SoundSet *sound_reload;
} weaponinfo_t;

extern weaponinfo_t cg_pWeaponModelInfos[WEAP_TOTAL];

enum {
	BASE_CHANNEL,
	EVENT_CHANNEL,
	PLAYERANIM_CHANNELS
};

typedef struct {
	int newanim[PMODEL_PARTS];
} gs_animationbuffer_t;

typedef struct {
	int anim;
	int frame;
	int64_t startTimestamp;
	float lerpFrac;
} gs_animstate_t;

typedef struct {
	// animations in the mixer
	gs_animstate_t curAnims[PMODEL_PARTS][PLAYERANIM_CHANNELS];
	gs_animationbuffer_t buffer[PLAYERANIM_CHANNELS];

	// results
	int frame[PMODEL_PARTS];
	int oldframe[PMODEL_PARTS];
	float lerpFrac[PMODEL_PARTS];
} gs_pmodel_animationstate_t;

typedef struct {
	int firstframe[PMODEL_TOTAL_ANIMATIONS];
	int lastframe[PMODEL_TOTAL_ANIMATIONS];
	int loopingframes[PMODEL_TOTAL_ANIMATIONS];
	float frametime[PMODEL_TOTAL_ANIMATIONS];
} gs_pmodel_animationset_t;

int GS_UpdateBaseAnims( entity_state_t *state, vec3_t velocity );
void GS_PModel_AnimToFrame( int64_t curTime, gs_pmodel_animationset_t *animSet, gs_pmodel_animationstate_t *anim );
void GS_PlayerModel_ClearEventAnimations( gs_pmodel_animationset_t *animSet, gs_pmodel_animationstate_t *animState );
void GS_PlayerModel_AddAnimation( gs_pmodel_animationstate_t *animState, int loweranim, int upperanim, int headanim, int channel );

#define SKM_MAX_BONES 256

//pmodelinfo_t is the playermodel structure as originally readed
//Consider it static 'read-only', cause it is shared by different players
typedef struct pmodelinfo_s {
	char *name;
	int sex;

	struct  model_s *model;
	struct cg_sexedSfx_s *sexedSfx;

	int numRotators[PMODEL_PARTS];
	int rotator[PMODEL_PARTS][16];
	int rootanims[PMODEL_PARTS];

	gs_pmodel_animationset_t animSet; // animation script

	struct pmodelinfo_s *next;
} pmodelinfo_t;

typedef struct {
	//static data
	pmodelinfo_t *pmodelinfo;
	struct Skin *skin;

	//dynamic
	gs_pmodel_animationstate_t animState;

	vec3_t angles[PMODEL_PARTS];                // for rotations
	vec3_t oldangles[PMODEL_PARTS];             // for rotations

	//effects
	orientation_t projectionSource;     // for projectiles
	// weapon. Not sure about keeping it here
	int64_t flash_time;
	int64_t barrel_time;
} pmodel_t;

extern pmodel_t cg_entPModels[MAX_EDICTS];      //a pmodel handle for each cg_entity

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

typedef struct {
	entity_t ent;

	unsigned int POVnum;
	int weapon;

	// animation
	int baseAnim;
	int64_t baseAnimStartTime;
	int eventAnim;
	int64_t eventAnimStartTime;

	// other effects
	orientation_t projectionSource;
} cg_viewweapon_t;

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

struct ViewState {
	[[nodiscard]]
	bool isViewerEntity( int entNum ) const;
	[[nodiscard]]
	bool canBeAMultiviewChaseTarget() const;
	[[nodiscard]]
	bool isUsingChasecam() const;

	// current in use, predicted or interpolated
	player_state_t predictedPlayerState;

	bool allowSounds;

	float xyspeed;
	float oldBobTime;
	int bobCycle;                   // odd cycles are right foot going forward
	float bobFracSin;               // sin(bobfrac*M_PI)

	// TODO: All of this belongs to viewstate
	cg_kickangles_t kickangles[MAX_ANGLES_KICKS];
	cg_viewblend_t colorblends[MAX_COLORBLENDS];
	int64_t damageBlends[4];
	int64_t fallEffectTime;
	int64_t fallEffectRebounceTime;

	int64_t pointRemoveTime;
	int pointedNum;
	int pointedHealth;
	int pointedArmor;

	cg_viewweapon_t weapon;
	cg_viewdef_t view;

	gs_laserbeamtrail_t weaklaserTrail;

	int64_t crosshairDamageTimestamp;

	float predictedOrigins[CMD_BACKUP][3];              // for debug comparing against server

	float predictedStep;                // for stair up smoothing
	int64_t predictedStepTime;

	int64_t predictingTimeStamp;
	int64_t predictedEventTimes[PREDICTABLE_EVENTS_MAX];
	vec3_t predictionError;
	int predictedWeaponSwitch;              // inhibit shooting prediction while a weapon change is expected
	int predictedGroundEntity;

	// prediction optimization (don't run all ucmds in not needed)
	int64_t predictFrom;
	entity_state_t predictFromEntityState;
	player_state_t predictFromPlayerState;

	float predictedSteps[CMD_BACKUP]; // for step smoothing

	int lastWeapon;

	player_state_t snapPlayerState;
	player_state_t oldSnapPlayerState;
};

//
// cg_pmodels.c
//

//utils
void CG_AddShellEffects( entity_t *ent, int effects, DrawSceneRequest *drawSceneRequest );
bool CG_GrabTag( orientation_t *tag, entity_t *ent, const char *tagname );
void CG_PlaceModelOnTag( entity_t *ent, entity_t *dest, orientation_t *tag );
void CG_PlaceRotatedModelOnTag( entity_t *ent, entity_t *dest, orientation_t *tag );
void CG_MoveToTag( vec3_t move_origin,
				   mat3_t move_axis,
				   const vec3_t space_origin,
				   const mat3_t space_axis,
				   const vec3_t tag_origin,
				   const mat3_t tag_axis );

//pmodels
void CG_PModelsInit( void );
void CG_PModelsShutdown( void );
void CG_ResetPModels( void );
void CG_RegisterBasePModel( void );
struct pmodelinfo_s *CG_RegisterPlayerModel( const char *filename );
void CG_AddPModel( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *viewState );
bool CG_PModel_GetProjectionSource( int entnum, orientation_t *tag_result, const ViewState *viewState );
void CG_UpdatePlayerModelEnt( centity_t *cent );
void CG_PModel_AddAnimation( int entNum, int loweranim, int upperanim, int headanim, int channel );
void CG_PModel_ClearEventAnimations( int entNum );

//
// cg_wmodels.c
//
void CG_WModelsInit();
void CG_WModelsShutdown();
struct weaponinfo_s *CG_CreateWeaponZeroModel( char *cgs_name );
struct weaponinfo_s *CG_RegisterWeaponModel( char *cgs_name, int weaponTag );
void CG_AddWeaponOnTag( entity_t *ent, orientation_t *tag, int weapon, int effects, bool addCoronaLight, orientation_t *projectionSource, int64_t flash_time, int64_t barrel_time, DrawSceneRequest *drawSceneRequest, ViewState *viewState );
struct weaponinfo_s *CG_GetWeaponInfo( int currentweapon );

#include "mediacache.h"

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
	const SoundSet *sfx;
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

#define PREDICTED_STEP_TIME 150 // stairs smoothing time

// view types
enum {
	VIEWDEF_CAMERA,
	VIEWDEF_PLAYERVIEW,

	VIEWDEF_MAXTYPES
};

#include <common/facilities/configstringstorage.h>

// this is not exactly "static" but still...
typedef struct {
	const char *serverName;
	const char *demoName;
	unsigned int playerNum;

	// shaders
	struct shader_s *shaderWhite;
	struct shader_s *shaderMiniMap;

	int fullclipShaderNum;

	struct qfontface_s *fontPlayerNameTiny;
	struct qfontface_s *fontPlayerNameSmall;
	struct qfontface_s *fontPlayerNameLarge;

	MediaCache media;

	bool precacheDone;

	int vidWidth, vidHeight;
	int pixelRatio;

	bool demoPlaying;
	bool demoTutorial;
	bool pure;
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

	const SoundSet *soundPrecache[MAX_SOUNDS];
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

struct shader_s;

struct CrosshairSizeProps { unsigned minSize, maxSize, defaultSize; };

// TODO: We do not need this if we are able to query var bounds in a generic fashion
constexpr const CrosshairSizeProps kRegularCrosshairSizeProps { 16, 48, 32 };
constexpr const CrosshairSizeProps kStrongCrosshairSizeProps { 48, 72, 64 };

typedef struct cg_state_s {
	int64_t time;
	float delay;

	int64_t realTime;
	int frameTime;
	int realFrameTime;
	int frameCount;

	snapshot_t frame, oldFrame;

	// Unused?
	bool oldAreabits;

	bool fireEvents;
	bool firstFrame;



	mat3_t autorotateAxis;

	float lerpfrac;                     // between oldframe and frame
	float xerpTime;
	float oldXerpTime;
	float xerpSmoothFrac;

	int effects;

	vec3_t lightingOrigin;

	// Addressed by index in snapshot
	// The last is for demo TODO fix
	ViewState viewStates[MAX_CLIENTS + 1];
	// Addressed by player number (entity number + 1)
	uint32_t snapViewStatePresentMask;
	unsigned numSnapViewStates;

	unsigned ourClientViewportIndex;
	unsigned chasedPlayerNum;
	unsigned chasedViewportIndex;
	std::optional<unsigned> pendingChasedPlayerNum;
	bool hasPendingSwitchFromTiledMode;

	int chaseMode;
	int64_t chaseSwitchTimestamp;

	wsw::StaticVector<uint8_t, MAX_CLIENTS> hudControlledMiniviewViewStateIndicesForPane[2];
	wsw::StaticVector<uint8_t, MAX_CLIENTS> tileMiniviewViewStateIndices;
	wsw::StaticVector<Rect, MAX_CLIENTS> tileMiniviewPositions;

	vec3_t demoFreeCamOrigin;
	vec3_t demoFreeCamAngles;
	vec3_t demoFreeCamVelocity;
	short demoFreeCamDeltaAngles[3];
	bool isDemoCamFree;

	//
	// transient data from server
	//
	const char *matchmessage;
	char helpmessage[MAX_HELPMESSAGE_CHARS];
	int64_t helpmessage_time;
	char *motd;
	int64_t motd_time;

	DelayedExecutionSystem delayedExecutionSystem;
	ParticleSystem particleSystem;
	SimulatedHullsSystem simulatedHullsSystem;
	PolyEffectsSystem polyEffectsSystem;
	EffectsSystemFacade effectsSystem;
} cg_state_t;

extern cg_static_t cgs;
extern cg_state_t cg;

[[nodiscard]]
auto getPrimaryViewState() -> ViewState *;
[[nodiscard]]
auto getOurClientViewState() -> ViewState *;
// The results may be legitimately nullable (multipov snaps do not imply that all players are included)
[[nodiscard]]
auto getViewStateForEntity( int number ) -> ViewState *;

#define ISBRUSHMODEL( x ) ( ( ( x > 0 ) && ( (int)x < CG_NumInlineModels() ) ) ? true : false )

#define ISREALSPECTATOR( vs )       ( vs->snapPlayerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR )
#define SPECSTATECHANGED()      ( ( cg.frame.playerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR ) != ( cg.oldFrame.playerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR ) )

extern centity_t cg_entities[MAX_EDICTS];

const struct cmodel_s *CG_CModelForEntity( int entNum );
void CG_SoundEntityNewState( centity_t *cent );
void CG_AddEntities( DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState );
void CG_LerpEntities( ViewState *drawFromViewState );
void CG_LerpGenericEnt( centity_t *cent, ViewState *drawFromViewState );

void CG_SetOutlineColor( byte_vec4_t outlineColor, byte_vec4_t color );
void CG_AddColoredOutLineEffect( entity_t *ent, int effects, uint8_t r, uint8_t g, uint8_t b, uint8_t a, const ViewState *drawFromViewState );
void CG_AddCentityOutLineEffect( centity_t *cent, const ViewState *drawFromViewState );

void CG_AddFlagModelOnTag( centity_t *cent, byte_vec4_t teamcolor, const char *tagname, DrawSceneRequest *, ViewState *drawFromViewState );

void CG_ResetItemTimers( void );

//
// cg_draw.c
//
int CG_HorizontalAlignForWidth( const int x, int align, int width );
int CG_VerticalAlignForHeight( const int y, int align, int height );

void CG_RegisterLevelMinimap( void );
void CG_RegisterFonts( void );
void CG_InitCrosshairs();
void CG_ShutdownCrosshairs();

struct model_s *CG_RegisterModel( const char *name );

void CG_ResetClientInfos( void );
void CG_LoadClientInfo( unsigned client, const wsw::StringView &configString );
void CG_UpdateSexedSoundsRegistration( pmodelinfo_t *pmodelinfo );
void playSexedSoundInPrimaryView( int entnum, int attachmentTag, int entchannel, const char *name, float fvol, float attn );
const SoundSet *CG_RegisterSexedSound( int entnum, const char *name );

void CG_PredictedEvent( int entNum, int ev, int parm );
void CG_Predict_ChangeWeapon( int new_weapon );
void CG_PredictMovement( void );
void CG_CheckPredictionError();
void CG_BuildSolidList( void );
void CG_Trace( trace_t *t, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, int ignore, int contentmask );
int CG_PointContents( const vec3_t point );
void CG_Predict_TouchTriggers( pmove_t *pm, const vec3_t previous_origin );

//
// cg_screen.c
//
void CG_CenterPrint( ViewState *viewState, const char *str );

void CG_LoadingString( const char *str );
bool CG_LoadingItemName( const char *str );

void CG_CheckSharedCrosshairState( bool initial );

void CG_ClearPointedNum( ViewState *viewState );

//
// cg_scoreboard.c
//
void CG_ScoresOn_f( const CmdArgs & );
void CG_ScoresOff_f( const CmdArgs & );

void CG_MessageMode( const CmdArgs & );
void CG_MessageMode2( const CmdArgs & );

//
// cg_main.c
//
extern cvar_t *developer;

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


class BoolConfigVar;
class IntConfigVar;
class FloatConfigVar;

extern IntConfigVar v_hand;
extern BoolConfigVar v_gun, v_gunBob;
extern FloatConfigVar v_gunFov;
extern FloatConfigVar v_gunX, v_gunY, v_gunZ;
extern FloatConfigVar v_gunAlpha;
extern IntConfigVar v_weaponFlashes;
extern FloatConfigVar v_handOffset;

extern BoolConfigVar v_predict, v_predictOptimize;
extern BoolConfigVar v_thirdPerson;
extern FloatConfigVar v_thirdPersonAngle, v_thirdPersonRange;
extern FloatConfigVar v_fov, v_zoomfov;
extern BoolConfigVar v_viewBob;
extern BoolConfigVar v_colorCorrection;
extern BoolConfigVar v_outlineWorld;

extern IntConfigVar v_showTeamInfo;
extern IntConfigVar v_showPlayerNames, v_showPointedPlayer;
extern FloatConfigVar v_showPlayerNames_alpha, v_showPlayerNames_zfar, v_showPlayerNames_barWidth;

extern BoolConfigVar v_showViewBlends, v_showZoomEffect;
extern BoolConfigVar v_draw2D, v_showHud;
extern FloatConfigVar v_viewSize;

extern IntConfigVar v_damageIndicator, v_damageIndicatorTime;

extern BoolConfigVar v_showClamp;

extern IntConfigVar v_flashWindowCount;

extern BoolConfigVar v_outlinePlayers, v_outlineModels;
extern BoolConfigVar v_raceGhosts;
extern FloatConfigVar v_raceGhostsAlpha;

extern BoolConfigVar v_particles;
extern BoolConfigVar v_heavyRocketExplosions, v_heavyGrenadeExplosions, v_heavyShockwaveExplosions;
extern BoolConfigVar v_explosionWave, v_explosionSmoke, v_explosionClusters;
extern IntConfigVar v_bloodStyle, v_bloodTime, v_bloodPalette;
extern FloatConfigVar v_volumeEffects;
extern FloatConfigVar v_ebBeamWidth, v_ebBeamTime;
extern FloatConfigVar v_instaBeamWidth, v_instaBeamTime;
extern BoolConfigVar v_teamColoredBeams, v_teamColoredInstaBeams;
extern BoolConfigVar v_projectileFireTrail, v_projectileSmokeTrail, v_projectilePolyTrail, v_plasmaTrail;

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
void CG_ShutdownSoundAndMiscState();

//
// cg_svcmds.c
//

void CG_SC_AutoRecordAction( ViewState *viewState, const char *action );

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
	// TODO: Freecam should be another mode
	CAM_TILED,
	CAM_MODES
};

void CG_DemocamInit( void );
void CG_DemocamShutdown( void );

void CG_ResetKickAngles( ViewState *viewState );
void CG_ResetColorBlend( ViewState *viewState );
void CG_ResetDamageIndicator( ViewState *viewState );
void CG_DamageIndicatorAdd( int damage, const vec3_t dir, ViewState *viewState );
void CG_StartKickAnglesEffect( vec3_t source, float knockback, float radius, int time );
void CG_StartColorBlendEffect( float r, float g, float b, float a, int time, ViewState *viewState );
void CG_StartFallKickEffect( int bounceTime, ViewState *viewState );
void CG_ViewSmoothPredictedSteps( vec3_t vieworg, ViewState *viewState );
float CG_ViewSmoothFallKick( ViewState *viewState );
void CG_AddKickAngles( vec3_t viewangles, ViewState *viewState );
bool CG_ChaseStep( int step );
bool CG_SwitchChaseCamMode( void );
void CG_ClearChaseCam();

std::optional<std::pair<unsigned, unsigned>> CG_FindMultiviewPovToChase();
std::optional<unsigned> CG_FindChaseableViewportForPlayernum( unsigned playerNum );

inline void CG_SmallPileOfGibs( const vec3_t origin, int damage, const vec3_t initialVelocity, int team ) {}

//
// cg_effects.c
//
inline void CG_ClearLightStyles( void ) {}
inline void CG_RunLightStyles( void ) {}
inline void CG_SetLightStyle( unsigned i, const wsw::StringView &s ) {}

inline void CG_ClearFragmentedDecals( void ) {}
inline void CG_AddFragmentedDecal( vec3_t origin, vec3_t dir, float orient, float radius,
							float r, float g, float b, float a, struct shader_s *shader ) {}

//
//	cg_vweap.c - client weapon
//
void CG_AddViewWeapon( cg_viewweapon_t *viewweapon, DrawSceneRequest *drawSceneRequest, ViewState *viewState );
void CG_CalcViewWeapon( cg_viewweapon_t *viewweapon, ViewState *viewState );
void CG_ViewWeapon_StartAnimationEvent( int newAnim, ViewState *viewState );
void CG_ViewWeapon_RefreshAnimation( cg_viewweapon_t *viewweapon, ViewState *viewState );

void CG_FireEvents( bool early );
void CG_EntityEvent( entity_state_t *ent, int ev, int parm, bool predicted );
void CG_AddAnnouncerEvent( const SoundSet *sound, bool queued );
void CG_ReleaseAnnouncerEvents( void );
void CG_ClearAnnouncerEvents( void );

// I don't know where to put these ones
void CG_WeaponBeamEffect( centity_t *cent, ViewState *viewState );
void CG_LaserBeamEffect( centity_t *owner, DrawSceneRequest *drawSceneRequest, ViewState *viewState );


//
// cg_input.cpp
//
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

[[nodiscard]]
bool getElectroboltTeamColor( int team, float *color );
[[nodiscard]]
bool getInstagunTeamColor( int team, float *color );
[[nodiscard]]
auto getTeamForOwner( int ownerNum ) -> int;

struct FlockOrientation {
	const float origin[3];
	const float offset[3];
	const float dir[3];

	void copyToFlockParams( ConicalFlockParams *params ) const {
		VectorCopy( this->origin, params->origin );
		VectorCopy( this->offset, params->offset );
		VectorCopy( this->dir, params->dir );
	}

	void copyToFlockParams( EllipsoidalFlockParams *params ) const {
		VectorCopy( this->origin, params->origin );
		VectorCopy( this->offset, params->offset );
		VectorCopy( this->dir, params->stretchDir );
	}
};

void addRandomRotationToDir( float *dir, wsw::RandomGenerator *rng, float minConeAngleCosine, float maxConeAngleCosine );
void addRandomRotationToDir( float *dir, wsw::RandomGenerator *rng, float coneAngleCosine );
void addRotationToDir( float *dir, float coneAngleCosine, float angleAlongCone );

[[nodiscard]]
auto getSurfFlagsForImpact( const trace_t &trace, const float *impactDir ) -> int;

struct ParticleColorsForTeamHolder {
	RgbaLifespan colorsForTeam[2];
	const RgbaLifespan defaultColors;

	[[nodiscard]]
	auto getColorsForTeam( int team, const vec4_t overlayColor ) -> const RgbaLifespan * {
		assert( team == TEAM_ALPHA || team == TEAM_BETA );
		RgbaLifespan *const resultColors = &colorsForTeam[team - TEAM_ALPHA];

		// TODO: Preserve HSV value, or make consistently lighter
		VectorCopy( overlayColor, resultColors->initial );
		VectorCopy( overlayColor, resultColors->fadedIn );
		VectorCopy( overlayColor, resultColors->fadedOut );

		// Preserve the reference alpha
		resultColors->initial[3]  = defaultColors.initial[3];
		resultColors->fadedIn[3]  = defaultColors.fadedIn[3];
		resultColors->fadedOut[3] = defaultColors.fadedOut[3];

		// Preserve fading properties
		resultColors->finishFadingInAtLifetimeFrac = defaultColors.finishFadingInAtLifetimeFrac;
		resultColors->startFadingOutAtLifetimeFrac = defaultColors.startFadingOutAtLifetimeFrac;

		return resultColors;
	}
};

#define cgDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::CGame, wsw::MessageCategory::Debug ) ).getWriter()
#define cgNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::CGame, wsw::MessageCategory::Notice ) ).getWriter()
#define cgWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::CGame, wsw::MessageCategory::Warning ) ).getWriter()
#define cgError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::CGame, wsw::MessageCategory::Error ) ).getWriter()