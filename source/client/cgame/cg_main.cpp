/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2003 Victor Luchits
Copyright (C) 2006 Pekka Lampila ("Medar"), Damien Deville ("Pb")
and German Garcia Fernandez ("Jal") for Chasseur de bots association.
Copyright (C) 2009 German Garcia Fernandez ("Jal")
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

#include <common/types/staticstring.h>
#include <common/facilities/cmdargs.h>
#include <client/client.h>
#include <common/facilities/configvars.h>
#include <common/facilities/sysclock.h>
#include <common/helpers/tonum.h>
#include <common/helpers/parsecompat.h>
#include <common/facilities/cmdargssplitter.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/cvar.h>
#include <client/ui/uisystem.h>
#include <common/facilities/gs_public.h>
#include <client/snd_public.h>
#include "cg_local.h"
#include <client/ui/huddatamodel.h>
#include <common/helpers/noise.h>
#include <client/renderer/local.h>

using wsw::operator""_asView;

BoolConfigVar v_predict( "cg_predict"_asView, { .byDefault = true } );
BoolConfigVar v_predictOptimize( "cg_predictOptimize"_asView, { .byDefault = true } );
static BoolConfigVar v_showMiss( "cg_showMiss"_asView, { .byDefault = false } );

// These vars are unused on client side, but are important as a part of userinfo
[[maybe_unused]] static StringConfigVar v_model( "model"_asView, { .byDefault = wsw::StringView( DEFAULT_PLAYERMODEL ), .flags = CVAR_USERINFO | CVAR_ARCHIVE });
[[maybe_unused]] static StringConfigVar v_skin( "skin"_asView, { .byDefault = wsw::StringView( DEFAULT_PLAYERSKIN ), .flags = CVAR_USERINFO | CVAR_ARCHIVE } );
[[maybe_unused]] static IntConfigVar v_handicap( "handicap"_asView, { .byDefault = 0, .flags = CVAR_USERINFO | CVAR_ARCHIVE } );
[[maybe_unused]] static StringConfigVar v_clan( "clan"_asView, { .byDefault = wsw::StringView(), .flags = CVAR_USERINFO | CVAR_ARCHIVE } );
[[maybe_unused]] static IntConfigVar v_movementStyle( "cg_movementStyle"_asView, { .byDefault = 1, .flags = CVAR_USERINFO | CVAR_ARCHIVE } );
[[maybe_unused]] static BoolConfigVar v_noAutohop( "cg_noAutohop"_asView, { .byDefault = false, .flags = CVAR_USERINFO | CVAR_ARCHIVE } );
[[maybe_unused]] static BoolConfigVar v_multiview( "cg_multiview"_asView, { .byDefault = true, .flags = CVAR_USERINFO | CVAR_ARCHIVE } );

IntConfigVar v_hand( "hand"_asView, { .byDefault = 0, .flags = CVAR_USERINFO | CVAR_ARCHIVE } );

FloatConfigVar v_fov( "fov"_asView, { .byDefault = DEFAULT_FOV, .min = inclusive( MIN_FOV ), .max = inclusive( MAX_FOV ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_zoomfov( "zoomfov"_asView, { .byDefault = DEFAULT_ZOOMFOV, .min = inclusive( MIN_ZOOMFOV ), .max = inclusive( MAX_ZOOMFOV ), .flags = CVAR_ARCHIVE } );
BoolConfigVar v_thirdPerson( "cg_thirdPerson"_asView, { .byDefault = false, .flags = CVAR_CHEAT } );
FloatConfigVar v_thirdPersonAngle( "cg_thirdPersonAngle"_asView, { .byDefault = 0.0f } );
FloatConfigVar v_thirdPersonRange( "cg_thirdPersonRange"_asView, { .byDefault = 90.0f } );

BoolConfigVar v_gun( "cg_gun"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_gunX( "cg_gunx"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_gunY( "cg_guny"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_gunZ( "cg_gunz"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_gunBob( "cg_gunBob"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_gunFov( "cg_gunFov"_asView, { .byDefault = 75.0f, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_gunAlpha( "cg_gunAlpha"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );

BoolConfigVar v_viewBob( "cg_viewBob"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_colorCorrection( "cg_colorCorrection"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

FloatConfigVar v_handOffset( "cg_handOffset"_asView, { .byDefault = 5.0f, .flags = CVAR_ARCHIVE } );
IntConfigVar v_weaponFlashes( "cg_weaponFlashes"_asView, { .byDefault = 2, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_projectileFireTrail( "cg_projectileFireTrail"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_projectileSmokeTrail( "cg_projectileSmokeTrail"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_projectilePolyTrail( "cg_projectilePolyTrail"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_plasmaTrail( "cg_plasmaTrail"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_playerTrail( "cg_playerTrail"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
IntConfigVar v_bloodTime( "cg_bloodTime"_asView, { .byDefault = 300, .flags = CVAR_ARCHIVE } );
IntConfigVar v_bloodStyle( "cg_bloodStyle"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
IntConfigVar v_bloodPalette( "cg_bloodPalette"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showPovBlood( "cg_showPovBlood"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_heavyRocketExplosions( "cg_heavyRocketExplosions"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_heavyGrenadeExplosions( "cg_heavyGrenadeExplosions"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_heavyShockwaveExplosions( "cg_heavyShockwaveExplosions"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_particles( "cg_particles"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_outlineModels( "cg_outlineModels"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_outlinePlayers( "cg_outlinePlayers"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_outlineWorld( "cg_outlineWorld"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_explosionWave( "cg_explosionWave"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_explosionSmoke( "cg_explosionSmoke"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_explosionClusters( "cg_explosionClusters"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

FloatConfigVar v_volumePlayers( "cg_volume_players"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_volumeEffects( "cg_volume_effects"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_volumeHitsound( "cg_volume_hitsound"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_volumeAnnouncer( "cg_volume_announcer"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );

IntConfigVar v_showPointedPlayer( "cg_showPointedPlayer"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
IntConfigVar v_showPlayerNames( "cg_showPlayerNames"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_showPlayerNames_alpha( "cg_showPlayerNames_alpha"_asView, { .byDefault = 0.4f, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_showPlayerNames_zfar( "cg_showPlayerNames_zfar"_asView, { .byDefault = 1024.0f, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_showPlayerNames_barWidth( "cg_showPlayerNames_barWidth"_asView, { .byDefault = 8.0f, .flags = CVAR_ARCHIVE } );

static BoolConfigVar v_autoactionDemo( "cg_autoactionDemo"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_autoactionSpectator( "cg_autoactionSpectator"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_autoactionScreenshot( "cg_autoactionScreenshot"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );

FloatConfigVar v_viewSize( "cg_viewSize"_asView, { .byDefault = 100.0f, .min = inclusive( 40.0f ), .max = inclusive( 100.0f ), .flags = CVAR_ARCHIVE } );
BoolConfigVar v_draw2D( "cg_draw2D"_asView, { .byDefault = true } );
BoolConfigVar v_showHud( "cg_showHud"_asView, { .byDefault = true } );
BoolConfigVar v_showViewBlends( "cg_showViewBlends"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
IntConfigVar v_showTeamInfo( "cg_showTeamInfo"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showFps( "cg_showFps"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showNet( "cg_showNet"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showZoomEffect( "cg_showZoomEffect"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showPressedKeys( "cg_showPressedKeys"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showSpeed( "cg_showSpeed"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showPickup( "cg_showPickup"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
IntConfigVar v_showTimer( "cg_showTimer"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showAwards( "cg_showAwards"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showFragsFeed( "cg_showFragsFeed"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showMessageFeed( "cg_showMessageFeed"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_showCaptureAreas( "cg_showCaptureAreas"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

// TODO: Default width values are way too large (even if they don't visually look like that), clip the image
// TODO: Implement OptionalFloatVar, allow turning beams off in a regular fashion
FloatConfigVar v_ebBeamWidth( "cg_ebBeamWidth"_asView, { .byDefault = 32.0f, .min = inclusive( 12.0f ), .max = inclusive( 48.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_ebBeamTime( "cg_ebBeamTime"_asView, { .byDefault = 0.4f, .min = inclusive( 0.1f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_instaBeamWidth( "cg_instaBeamWidth"_asView, { .byDefault = 48.0f, .min = inclusive( 12.0f ), .max = inclusive( 48.0f ), .flags = CVAR_ARCHIVE } );
FloatConfigVar v_instaBeamTime( "cg_instaBeamTime"_asView, { .byDefault = 0.4f, .min = inclusive( 0.1f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );

static IntConfigVar v_weaponAutoSwitch( "cg_weaponAutoSwitch"_asView, { .byDefault = 2, .flags = CVAR_ARCHIVE } );

IntConfigVar v_flashWindowCount( "cg_flashWindowCount"_asView, { .byDefault = 4, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_showClamp( "cg_showClamp"_asView, { .byDefault = false, .flags = CVAR_DEVELOPER } );

static BoolConfigVar v_predictLaserBeam( "cg_predictLaserBeam"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_projectileAntilagOffset( "cg_projectileAntilagOffset"_asView, { .byDefault = 1.0f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );

BoolConfigVar v_raceGhosts( "cg_raceGhosts"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
FloatConfigVar v_raceGhostsAlpha( "cg_raceGhostsAlpha"_asView, { .byDefault = 0.25f, .min = inclusive( 0.0f ), .max = inclusive( 1.0f ), .flags = CVAR_ARCHIVE } );

static BoolConfigVar v_chatBeep( "cg_chatBeep"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_chatShowIgnored( "cg_chatShowIgnored"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
static IntConfigVar v_chatFilter( "cg_chatFilter"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_USERINFO } );

static IntConfigVar v_simpleItems( "cg_simpleItems"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_simpleItemsSize( "cg_simpleItemsSize"_asView, { .byDefault = 16.0f, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_cartoonHitEffect( "cg_cartoonHitEffect"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_showHelp( "cg_showHelp"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

static BoolConfigVar v_drawEntityBoxes( "cg_drawEntityBoxes"_asView, { .byDefault = false, .flags = CVAR_DEVELOPER } );
static IntConfigVar v_laserBeamSubdivisions( "cg_laserBeamSubdivisions"_asView, { .byDefault = 24, .flags = CVAR_ARCHIVE } );

static StringConfigVar v_playList( "cg_playList"_asView, { .byDefault = wsw::StringView( S_PLAYLIST_MATCH ), .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_playListShuffle( "cg_playListShuffle"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

BoolConfigVar v_teamColoredBeams( "cg_teamColoredBeams"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
BoolConfigVar v_teamColoredInstaBeams( "cg_teamColoredInstaBeams"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

static BoolConfigVar v_pickupFlash( "cg_pickupFlash"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );

IntConfigVar v_damageIndicator( "cg_damageIndicator"_asView, { .byDefault = 1, .flags = CVAR_ARCHIVE } );
IntConfigVar v_damageIndicatorTime( "cg_damageIndicatorTime"_asView, { .byDefault = 25, .min = inclusive( 0 ), .flags = CVAR_ARCHIVE } );

cg_static_t cgs;
cg_state_t cg;

centity_t cg_entities[MAX_EDICTS];

// force models
cvar_t *cg_teamPLAYERSmodel;
cvar_t *cg_teamPLAYERSmodelForce;
cvar_t *cg_teamALPHAmodel;
cvar_t *cg_teamALPHAmodelForce;
cvar_t *cg_teamBETAmodel;
cvar_t *cg_teamBETAmodelForce;

cvar_t *cg_teamPLAYERSskin;
cvar_t *cg_teamALPHAskin;
cvar_t *cg_teamBETAskin;

cvar_t *cg_teamPLAYERScolor;
cvar_t *cg_teamPLAYERScolorForce;
cvar_t *cg_teamALPHAcolor;
cvar_t *cg_teamBETAcolor;

static int cg_numSolids;
static entity_state_t *cg_solidList[MAX_PARSE_ENTITIES];

static int cg_numTriggers;
static entity_state_t *cg_triggersList[MAX_PARSE_ENTITIES];
static bool cg_triggersListTriggered[MAX_PARSE_ENTITIES];

static bool ucmdReady = false;

static bool demo_requested = false;
static bool autorecording = false;

#define CG_MAX_ANNOUNCER_EVENTS 32
#define CG_MAX_ANNOUNCER_EVENTS_MASK ( CG_MAX_ANNOUNCER_EVENTS - 1 )
#define CG_ANNOUNCER_EVENTS_FRAMETIME 1500 // the announcer will speak each 1.5 seconds

typedef struct cg_announcerevent_s
{
	const SoundSet *sound;
} cg_announcerevent_t;
cg_announcerevent_t cg_announcerEvents[CG_MAX_ANNOUNCER_EVENTS];

static int cg_announcerEventsCurrent = 0;
static int cg_announcerEventsHead = 0;
static int cg_announcerEventsDelay = 0;

static centity_t *laserOwner = nullptr;
static DrawSceneRequest *laserDrawSceneRequest = nullptr;
static bool laserViewStateMuted = false;

#define MAX_ITEM_TIMERS 8

static int cg_num_item_timers = 0;
static centity_t *cg_item_timers[MAX_ITEM_TIMERS];

bool ViewState::isViewerEntity( int entNum ) const {
	return ( ( predictedPlayerState.POVnum > 0 ) && ( (int)predictedPlayerState.POVnum == (int)( entNum ) ) && ( this->view.type == VIEWDEF_PLAYERVIEW ) );
}

bool ViewState::canBeAMultiviewChaseTarget() const {
	return predictedPlayerState.stats[STAT_REALTEAM] != TEAM_SPECTATOR && predictedPlayerState.POVnum == predictedPlayerState.playerNum + 1;
}

bool ViewState::isUsingChasecam() const {
	return predictedPlayerState.pmove.pm_type == PM_CHASECAM && predictedPlayerState.POVnum != predictedPlayerState.playerNum + 1;
}

[[nodiscard]]
auto getPrimaryViewState() -> ViewState * {
	return &cg.viewStates[cg.chasedViewportIndex];
}

[[nodiscard]]
auto getOurClientViewState() -> ViewState * {
	return &cg.viewStates[cg.ourClientViewportIndex];
}

[[nodiscard]]
auto getViewStateForEntity( int number ) -> ViewState * {
	if( cg.frame.multipov ) {
		// TODO: Built a lookup table during new frame updates
		for( unsigned i = 0; i < cg.numSnapViewStates; ++i ) {
			if( cg.viewStates[i].predictedPlayerState.playerNum + 1 == number ) {
				return &cg.viewStates[i];
			}
		}
		return nullptr;
	}
	return &cg.viewStates[cg.chasedViewportIndex];
}

[[nodiscard]]
bool shouldHandleChatLikeCommandOfViewState( ViewState *viewState ) {
	if( cgs.demoPlaying ) {
		return viewState == getPrimaryViewState() && !CG_UsesTiledView();
	} else {
		return viewState == getOurClientViewState();
	}
}

static void CG_SC_Print( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( shouldHandleChatLikeCommandOfViewState( viewState ) ) {
		CG_LocalPrint( "%s", Cmd_Argv( 1 ) );
	}
}

static void CG_SC_ChatPrint( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( shouldHandleChatLikeCommandOfViewState( viewState ) ) {
		const wsw::StringView commandName( Cmd_Argv( 0 ) );
		const bool teamonly = commandName.startsWith( 't' );
		std::optional<uint64_t> sendCommandNum;
		int whoArgNum = 1;
		if( commandName.endsWith( 'a' ) ) {
			if( ( sendCommandNum = wsw::toNum<uint64_t>( wsw::StringView( Cmd_Argv( 1 ) ) ) ) ) {
				whoArgNum = 2;
			} else {
				// TODO??? What to do in this case?
				return;
			}
		}

		const int who    = atoi( Cmd_Argv( whoArgNum ) );
		const char *name = ( who && who == bound( 1, who, MAX_CLIENTS ) ? cgs.clientInfo[who - 1].name : "console" );
		const char *text = Cmd_Argv( whoArgNum + 1 );

		const wsw::StringView nameView( name );
		const wsw::StringView textView( text );

		if( teamonly ) {
			CG_LocalPrint( S_COLOR_YELLOW "[%s]" S_COLOR_WHITE "%s" S_COLOR_YELLOW ": %s\n",
						   viewState->snapPlayerState.stats[STAT_REALTEAM] == TEAM_SPECTATOR ? "SPEC" : "TEAM", name, text );
			wsw::ui::UISystem::instance()->addToTeamChat( { nameView, textView, sendCommandNum } );
		} else {
			CG_LocalPrint( "%s" S_COLOR_GREEN ": %s\n", name, text );
			wsw::ui::UISystem::instance()->addToChat( { nameView, textView, sendCommandNum } );
		}

		if( v_chatBeep.get() ) {
			cg.soundSystem->startLocalSound( cgs.media.sndChat, 1.0f );
		}
	}
}

static void CG_SC_IgnoreCommand( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( shouldHandleChatLikeCommandOfViewState( viewState ) ) {
		const char *firstArg = Cmd_Argv( 1 );
		// TODO: Is there a more generic method of setting client vars?
		// In fact this is actually a safer alternative so it should be kept
		if( !Q_stricmp( "setVar", firstArg ) ) {
			v_chatFilter.setImmediately( atoi(Cmd_Argv( 2 ) ) );
			return;
		}

		if( !v_chatShowIgnored.get() ) {
			return;
		}

		const int who = ::atoi( firstArg );
		if( !who ) {
			return;
		}

		if( who != bound( 1, who, MAX_CLIENTS ) ) {
			return;
		}

		const char *format = S_COLOR_GREY "A message from " S_COLOR_WHITE "%s" S_COLOR_GREY " was ignored\n";
		CG_LocalPrint( format, cgs.clientInfo[who - 1].name );
	}
}

static void CG_SC_MessageFault( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( shouldHandleChatLikeCommandOfViewState( viewState ) ) {
		// TODO: Check what view state does it belong to
		const char *const commandNumString = Cmd_Argv( 1 );
		const char *const faultKindString  = Cmd_Argv( 2 );
		const char *const timeoutString    = Cmd_Argv( 3 );
		if( commandNumString && faultKindString && timeoutString ) {
			if( const auto maybeCommandNum = wsw::toNum<uint64_t>( commandNumString ) ) {
				if( const auto maybeFaultKind = wsw::toNum<unsigned>( wsw::StringView( faultKindString ) ) ) {
					// Check timeout values for sanity by specifying a lesser type
					if( const auto maybeTimeoutValue = wsw::toNum<uint16_t>( wsw::StringView( timeoutString ) ) ) {
						if( *maybeFaultKind >= MessageFault::kMaxKind && *maybeFaultKind <= MessageFault::kMaxKind ) {
							const auto kind    = (MessageFault::Kind)*maybeFaultKind;
							const auto timeout = *maybeTimeoutValue;
							if( kind == MessageFault::Flood ) {
								const auto secondsLeft = (int)( *maybeTimeoutValue / 1000 ) + 1;
								CG_LocalPrint( "Flood protection. You can talk again in %d second(s)\n", secondsLeft );
							} else if( kind == MessageFault::Muted ) {
								CG_LocalPrint( "You are muted on this server\n" );
							} else {
								wsw::failWithLogicError( "unreachable" );
							}
							wsw::ui::UISystem::instance()->handleMessageFault( { *maybeCommandNum, kind, timeout } );
						}
					}
				}
			}
		}
	}
}

static void CG_SC_CenterPrint( ViewState *viewState, const CmdArgs &cmdArgs ) {
	CG_CenterPrint( viewState, Cmd_Argv( 1 ) );
}

static void CG_SC_CenterPrintFormat( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() == 8 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ), Cmd_Argv( 3 ), Cmd_Argv( 4 ), Cmd_Argv( 5 ), Cmd_Argv( 6 ), Cmd_Argv( 7 ) ) );
	} else if( Cmd_Argc() == 7 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ), Cmd_Argv( 3 ), Cmd_Argv( 4 ), Cmd_Argv( 5 ), Cmd_Argv( 6 ) ) );
	} else if( Cmd_Argc() == 6 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ), Cmd_Argv( 3 ), Cmd_Argv( 4 ), Cmd_Argv( 5 ) ) );
	} else if( Cmd_Argc() == 5 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ), Cmd_Argv( 3 ), Cmd_Argv( 4 ) ) );
	} else if( Cmd_Argc() == 4 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ), Cmd_Argv( 3 ) ) );
	} else if( Cmd_Argc() == 3 ) {
		CG_CenterPrint( viewState, va( Cmd_Argv( 1 ), Cmd_Argv( 2 ) ) );
	} else if( Cmd_Argc() == 2 ) {
		CG_CenterPrint( viewState, Cmd_Argv( 1 ) ); // theoretically, shouldn't happen
	}
}

void CG_ConfigString( int i, const wsw::StringView &string ) {
	cgs.configStrings.set( i, string );

	// do something apropriate
	if( i == CS_MAPNAME ) {
		CG_RegisterLevelMinimap();
	} else if( i == CS_GAMETYPETITLE ) {
	} else if( i == CS_GAMETYPENAME ) {
		GS_SetGametypeName( cggs, string.data() );
	} else if( i == CS_AUTORECORDSTATE ) {
		CG_SC_AutoRecordAction( getOurClientViewState(), string.data() );
	} else if( i >= CS_MODELS && i < CS_MODELS + MAX_MODELS ) {
		if( string.startsWith( '$' ) ) {  // indexed pmodel
			cgs.pModelsIndex[i - CS_MODELS] = CG_RegisterPlayerModel( string.data() + 1 );
		} else {
			cgs.modelDraw[i - CS_MODELS] = CG_RegisterModel( string.data() );
		}
	} else if( i >= CS_SOUNDS && i < CS_SOUNDS + MAX_SOUNDS ) {
		if( !string.startsWith( '*' ) ) {
			cgs.soundPrecache[i - CS_SOUNDS] = cg.soundSystem->registerSound( SoundSetProps {
				.name = SoundSetProps::Exact { string },
			});
		}
	} else if( i >= CS_IMAGES && i < CS_IMAGES + MAX_IMAGES ) {
		if( string.indexOf( "correction/"_asView ) != std::nullopt ) { // HACK HACK HACK -- for color correction LUTs
			cgs.imagePrecache[i - CS_IMAGES] = R_RegisterLinearPic( string.data() );
		} else {
			cgs.imagePrecache[i - CS_IMAGES] = R_RegisterPic( string.data() );
		}
	} else if( i >= CS_SKINFILES && i < CS_SKINFILES + MAX_SKINFILES ) {
		cgs.skinPrecache[i - CS_SKINFILES] = R_RegisterSkinFile( string.data() );
	} else if( i >= CS_LIGHTS && i < CS_LIGHTS + MAX_LIGHTSTYLES ) {
		CG_SetLightStyle( i - CS_LIGHTS, string );
	} else if( i >= CS_ITEMS && i < CS_ITEMS + MAX_ITEMS ) {
		CG_ValidateItemDef( i - CS_ITEMS, string.data() );
	} else if( i >= CS_PLAYERINFOS && i < CS_PLAYERINFOS + MAX_CLIENTS ) {
		CG_LoadClientInfo( i - CS_PLAYERINFOS, string );
	} else if( i >= CS_GAMECOMMANDS && i < CS_GAMECOMMANDS + MAX_GAMECOMMANDS ) {
		if( !string.empty() && !cgs.demoPlaying ) {
			CL_Cmd_Register( wsw::StringView( string ), NULL );
		}
	} else if( i >= CS_WEAPONDEFS && i < CS_WEAPONDEFS + MAX_WEAPONDEFS ) {
		CG_OverrideWeapondef( i - CS_WEAPONDEFS, string.data() );
	}

	// Let the UI system decide whether it could handle the config string as well
	wsw::ui::UISystem::instance()->handleConfigString( i, string );
}

static const char *CG_SC_AutoRecordName( ViewState *viewState ) {
	assert( viewState == getOurClientViewState() );

	time_t long_time;
	struct tm *newtime;
	static char name[MAX_STRING_CHARS];
	char mapname[MAX_QPATH];
	const char *cleanplayername, *cleanplayername2;

	// get date from system
	time( &long_time );
	newtime = localtime( &long_time );

	if( viewState->view.POVent <= 0 ) {
		cleanplayername2 = "";
	} else {
		// remove color tokens from player names (doh)
		cleanplayername = COM_RemoveColorTokens( cgs.clientInfo[viewState->view.POVent - 1].name );

		// remove junk chars from player names for files
		cleanplayername2 = COM_RemoveJunkChars( cleanplayername );
	}

	// lowercase mapname
	Q_strncpyz( mapname, cgs.configStrings.getMapName()->data(), sizeof( mapname ) );
	Q_strlwr( mapname );

	// make file name
	// duel_year-month-day_hour-min_map_player
	Q_snprintfz( name, sizeof( name ), "%s_%04d-%02d-%02d_%02d-%02d_%s_%s_%04i",
				 cggs->gametypeName,
				 newtime->tm_year + 1900, newtime->tm_mon + 1, newtime->tm_mday,
				 newtime->tm_hour, newtime->tm_min,
				 mapname,
				 cleanplayername2,
				 (int)brandom( 0, 9999 )
	);

	return name;
}

void CG_SC_AutoRecordAction( ViewState *viewState, const char *action ) {
	if( viewState == getOurClientViewState() ) {
		const char *name;
		bool spectator;

		if( !action[0] ) {
			return;
		}

		// filter out autorecord commands when playing a demo
		if( cgs.demoPlaying ) {
			return;
		}

		// let configstrings and other stuff arrive before taking any action
		if( !cgs.precacheDone ) {
			return;
		}

		const auto *playerState = &getOurClientViewState()->snapPlayerState;
		if( playerState->pmove.pm_type == PM_SPECTATOR || playerState->pmove.pm_type == PM_CHASECAM ) {
			spectator = true;
		} else {
			spectator = false;
		}

		name = CG_SC_AutoRecordName( viewState );

		if( !Q_stricmp( action, "start" ) ) {
			if( v_autoactionDemo.get() && ( !spectator || v_autoactionSpectator.get() ) ) {
				CL_Cmd_ExecuteNow( "stop silent" );
				CL_Cmd_ExecuteNow( va( "record autorecord/%s/%s silent",
									   cggs->gametypeName, name ) );
				autorecording = true;
			}
		} else if( !Q_stricmp( action, "altstart" ) ) {
			if( v_autoactionDemo.get() && ( !spectator || v_autoactionSpectator.get() ) ) {
				CL_Cmd_ExecuteNow( va( "record autorecord/%s/%s silent",
									   cggs->gametypeName, name ) );
				autorecording = true;
			}
		} else if( !Q_stricmp( action, "stop" ) ) {
			if( autorecording ) {
				CL_Cmd_ExecuteNow( "stop silent" );
				autorecording = false;
			}

			if( v_autoactionScreenshot.get() && ( !spectator || v_autoactionSpectator.get() ) ) {
				CL_Cmd_ExecuteNow( va( "screenshot autorecord/%s/%s silent",
									   cggs->gametypeName, name ) );
			}
		} else if( !Q_stricmp( action, "cancel" ) ) {
			if( autorecording ) {
				CL_Cmd_ExecuteNow( "stop cancel silent" );
				autorecording = false;
			}
		} else if( developer->integer ) {
			Com_Printf( "CG_SC_AutoRecordAction: Unknown action: %s\n", action );
		}
	}
}

static const char *CG_MatchMessageString( matchmessage_t mm ) {
	switch( mm ) {
		case MATCHMESSAGE_CHALLENGERS_QUEUE:
			return "'ESC' for in-game menu or 'ENTER' for in-game chat.\n"
				   "You are inside the challengers queue waiting for your turn to play.\n"
				   "Use the in-game menu to exit the queue.\n"
				   "\nUse the mouse buttons for switching spectator modes.";

		case MATCHMESSAGE_ENTER_CHALLENGERS_QUEUE:
			return "'ESC' for in-game menu or 'ENTER' for in-game chat.\n"
				   "Use the in-game menu or press 'F3' to enter the challengers queue.\n"
				   "Only players in the queue will have a turn to play against the last winner.\n"
				   "\nUse the mouse buttons for switching spectator modes.";

		case MATCHMESSAGE_SPECTATOR_MODES:
			return "'ESC' for in-game menu or 'ENTER' for in-game chat.\n"
				   "Mouse buttons for switching spectator modes.\n"
				   "This message can be hidden by disabling 'help' in player setup menu.";

		case MATCHMESSAGE_GET_READY:
			return "Set yourself READY to start the match!\n"
				   "You can use the in-game menu or simply press 'F4'.\n"
				   "'ESC' for in-game menu or 'ENTER' for in-game chat.";

		case MATCHMESSAGE_WAITING_FOR_PLAYERS:
			return "Waiting for players.\n"
				   "'ESC' for in-game menu.";

		default:
			return "";
	}

	return "";
}

static void CG_SC_MatchMessage( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( viewState == getOurClientViewState() ) {
		matchmessage_t mm;
		const char *matchmessage;

		cg.matchmessage = NULL;

		mm = (matchmessage_t)atoi( Cmd_Argv( 1 ) );
		matchmessage = CG_MatchMessageString( mm );
		if( !matchmessage || !matchmessage[0] ) {
			return;
		}

		cg.matchmessage = matchmessage;
	}
}

static void CG_SC_HelpMessage( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( viewState == getOurClientViewState() ) {
		cg.helpmessage[0] = '\0';

		unsigned index = atoi( Cmd_Argv( 1 ) );
		if( !index || index > MAX_HELPMESSAGES ) {
			return;
		}

		const auto maybeConfigString = cgs.configStrings.getHelpMessage( index - 1 );
		if( !maybeConfigString ) {
			return;
		}

		unsigned outlen = 0;
		int c;
		const char *helpmessage = maybeConfigString->data();
		while( ( c = helpmessage[0] ) && ( outlen < MAX_HELPMESSAGE_CHARS - 1 ) ) {
			helpmessage++;

			if( c == '{' ) { // template
				int t = *( helpmessage++ );
				switch( t ) {
					case 'B': // key binding
					{
						char cmd[MAX_STRING_CHARS];
						unsigned cmdlen = 0;
						while( ( c = helpmessage[0] ) != '\0' ) {
							helpmessage++;
							if( c == '}' ) {
								break;
							}
							if( cmdlen < MAX_STRING_CHARS - 1 ) {
								cmd[cmdlen++] = c;
							}
						}
						cmd[cmdlen] = '\0';
						CG_GetBoundKeysString( cmd, cg.helpmessage + outlen, MAX_HELPMESSAGE_CHARS - outlen );
						outlen += strlen( cg.helpmessage + outlen );
					}
						continue;
					default:
						helpmessage--;
						break;
				}
			}

			cg.helpmessage[outlen++] = c;
		}
		cg.helpmessage[outlen] = '\0';
		Q_FixTruncatedUtf8( cg.helpmessage );

		cg.helpmessage_time = cg.time;
	}
}

static void CG_Cmd_DemoGet_f( const CmdArgs &cmdArgs ) {
	if( demo_requested ) {
		cgNotice() << "Already requesting a demo";
		return;
	}

	if( Cmd_Argc() != 2 || ( atoi( Cmd_Argv( 1 ) ) <= 0 && Cmd_Argv( 1 )[0] != '.' ) ) {
		cgNotice() << "Usage: demoget <number>";
		cgNotice() << "Downloads a demo from the server";
		cgNotice() << "Use the demolist command to see list of demos on the server";
		return;
	}

	CL_Cmd_ExecuteNow( va( "cmd demoget %s", Cmd_Argv( 1 ) ) );

	demo_requested = true;
}

static void CG_SC_DemoGet( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( viewState == getOurClientViewState() ) {
		const char *filename, *extension;

		if( cgs.demoPlaying ) {
			// ignore download commands coming from demo files
			return;
		}

		if( !demo_requested ) {
			cgWarning() << cmdArgs[0] << "when not requested, ignored";
			return;
		}

		demo_requested = false;

		if( Cmd_Argc() < 2 ) {
			cgWarning() << "No such demo found";
			return;
		}

		filename = Cmd_Argv( 1 );
		extension = COM_FileExtension( filename );
		if( !COM_ValidateRelativeFilename( filename ) ||
			!extension || Q_stricmp( extension, cgs.demoExtension ) ) {
			cgWarning() << cmdArgs[0] << "Invalid filename, ignored";
			return;
		}

		CL_DownloadRequest( filename, false );
	}
}

static void CG_SC_MOTD( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( viewState == getOurClientViewState() ) {
		const char *motd;

		if( cg.motd ) {
			Q_free(   cg.motd );
		}
		cg.motd = NULL;

		motd = Cmd_Argv( 2 );
		if( !motd[0] ) {
			return;
		}

		if( !strcmp( Cmd_Argv( 1 ), "1" ) ) {
			cg.motd = Q_strdup( motd );
			cg.motd_time = cg.time + 50 * strlen( motd );
			if( cg.motd_time < cg.time + 5000 ) {
				cg.motd_time = cg.time + 5000;
			}
		}

		Com_Printf( "\nMessage of the Day:\n%s", motd );
	}
}

static void CG_SC_AddAward( ViewState *viewState, const CmdArgs &cmdArgs ) {
	const char *str = Cmd_Argv( 1 );
	if( str && *str ) {
		wsw::ui::UISystem::instance()->addAward( viewState->snapPlayerState.playerNum, wsw::StringView( str ) );
	}
}

static void CG_SC_ActionRequest( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( viewState == getOurClientViewState() ) {
		int argNum = 1;
		// Expect a timeout
		if( const auto maybeTimeout = wsw::toNum<unsigned>( wsw::StringView( Cmd_Argv( argNum++ ) ) ) ) {
			// Expect a tag
			if( const wsw::StringView tag( Cmd_Argv( argNum++ ) ); !tag.empty() ) {
				// Expect a title
				if( const wsw::StringView title( Cmd_Argv( argNum++ ) ); !title.empty() ) {
					const wsw::StringView desc( Cmd_Argv( argNum++ ) );
					// Expect a number of commands
					if ( const auto maybeNumCommands = wsw::toNum<unsigned>( wsw::StringView( Cmd_Argv( argNum++ ) ) ) ) {
						// Read (key, command) pairs
						const auto maxArgNum = (int)wsw::min( 9u, *maybeNumCommands ) + argNum;
						wsw::StaticVector<std::pair<wsw::StringView, int>, 9> actions;
						const auto *bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
						while( argNum < maxArgNum ) {
							const wsw::StringView keyView( Cmd_Argv( argNum++ ) );
							const auto maybeKey = bindingsSystem->getKeyForName( keyView );
							if( !maybeKey ) {
								return;
							}
							actions.emplace_back( { wsw::StringView( Cmd_Argv( argNum++ ) ), *maybeKey } );
						}
						wsw::ui::UISystem::instance()->touchActionRequest( tag, *maybeTimeout, title, desc, actions );
					}
				}
			}
		}
	}
}

static void CG_SC_FragEvent( ViewState *viewState, const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() == 4 ) {
		unsigned args[3];
		for( int i = 0; i < 3; ++i ) {
			if( const auto maybeNum = wsw::toNum<unsigned>( wsw::StringView( Cmd_Argv( i + 1 ) ) ) ) {
				args[i] = *maybeNum;
			} else {
				return;
			}
		}
		const auto victim = args[0], attacker = args[1], meansOfDeath = args[2];
		if( ( victim && victim < (unsigned)( MAX_CLIENTS + 1 ) ) && attacker < (unsigned)( MAX_CLIENTS + 1 ) ) {
			if( meansOfDeath >= (unsigned)MOD_GUNBLADE_W && meansOfDeath < (unsigned)MOD_COUNT ) {
				if( const wsw::StringView victimName( cgs.clientInfo[victim - 1].name ); !victimName.empty() ) {
					std::optional<std::pair<wsw::StringView, int>> attackerAndTeam;
					if( attacker ) {
						if( const wsw::StringView view( cgs.clientInfo[attacker - 1].name ); !view.empty() ) {
							const int attackerRealTeam = cg_entities[attacker].current.team;
							attackerAndTeam = std::make_pair( view, attackerRealTeam );
						}
					}
					const int victimTeam = cg_entities[victim].current.team;
					const auto victimAndTeam( std::make_pair( victimName, victimTeam ) );
					// TODO: Is it the right condition?
					if( viewState == getPrimaryViewState() ) {
						wsw::ui::UISystem::instance()->addFragEvent( victimAndTeam, meansOfDeath, attackerAndTeam );
					}
					if( attacker && attacker != victim && viewState->isViewerEntity( attacker ) ) {
						wsw::StaticString<256> message;
						if( cg_entities[attacker].current.team == cg_entities[victim].current.team ) {
							if( GS_TeamBasedGametype( *cggs ) ) {
								message << wsw::StringView( S_COLOR_ORANGE ) <<
										wsw::StringView( "You teamfragged " ) << wsw::StringView( S_COLOR_WHITE );
							}
						}
						if( message.empty() ) {
							message << wsw::StringView( "Cool! You fragged " );
						}
						message << victimName;
						wsw::ui::UISystem::instance()->addStatusMessage( viewState->snapPlayerState.playerNum, message.asView() );
					}
				}
			}
		}
	}
}

typedef struct {
	const char *name;
	void ( *func )( ViewState *viewState, const CmdArgs & );
} svcmd_t;

static const svcmd_t cg_svcmds[] = {
	{ "pr", CG_SC_Print },

	// Chat-related commands
	{ "ch", CG_SC_ChatPrint },
	{ "tch", CG_SC_ChatPrint },
	// 'a' stands for "Acknowledge"
	{ "cha", CG_SC_ChatPrint },
	{ "tcha", CG_SC_ChatPrint },
	{ "ign", CG_SC_IgnoreCommand },
	{ "flt", CG_SC_MessageFault },

	{ "tflt", CG_SC_MessageFault },
	{ "cp", CG_SC_CenterPrint },
	{ "cpf", CG_SC_CenterPrintFormat },
	{ "fra", CG_SC_FragEvent },
	{ "mm", CG_SC_MatchMessage },
	{ "mapmsg", CG_SC_HelpMessage },
	{ "demoget", CG_SC_DemoGet },
	{ "motd", CG_SC_MOTD },
	{ "aw", CG_SC_AddAward },
	{ "arq", CG_SC_ActionRequest },

	{ NULL }
};

void CG_GameCommand( ViewState *viewState, const wsw::StringView &fullText ) {
	static CmdArgsSplitter argsSplitter;
	const CmdArgs &cmdArgs = argsSplitter.exec( fullText );

	for( const svcmd_t *cmd = cg_svcmds; cmd->name; cmd++ ) {
		if( !strcmp( cmdArgs[0].data(), cmd->name ) ) {
			cmd->func( viewState, cmdArgs );
			return;
		}
	}

	cgNotice() << "Unknown game command" << cmdArgs[0];
}

void CG_OptionsStatus( const CmdArgs &cmdArgs ) {
	wsw::ui::UISystem::instance()->handleOptionsStatusCommand( cmdArgs[1] );
}

void CG_ReloadOptions( const CmdArgs & ) {
	// We have to reload commands first
	// as the UI system checks whether specific commands are actually available and enqueues needed commands immediately
	CG_ReloadCommands( {} );
	wsw::ui::UISystem::instance()->reloadOptions();
}

void CG_ReloadCommands( const CmdArgs & ) {
	// We have to invalidate commands explicitly,
	// otherwise old commands keep staying registered while being actually invalid, and that breaks the UI assumptions.
	CG_UnregisterCGameCommands();
	CG_RegisterCGameCommands();
}

void CG_UseItem( const char *name ) {
	if( name && cg.frame.valid && !cgs.demoPlaying ) {
		ViewState *const viewState = getOurClientViewState();
		if( const gsitem_t *item = GS_Cmd_UseItem( cggs, &viewState->snapPlayerState, name, 0 ) ) {
			if( item->type & IT_WEAPON ) {
				CG_Predict_ChangeWeapon( item->tag );
				viewState->lastWeapon = viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON];
			}
			CL_Cmd_ExecuteNow( va( "cmd use %i", item->tag ) );
		}
	}
}

static void CG_Cmd_UseItem_f( const CmdArgs &cmdArgs ) {
	if( !Cmd_Argc() ) {
		cgNotice() << "Usage: 'use <item name>' or 'use <item index>'";
	} else {
		CG_UseItem( Cmd_Args() );
	}
}

static void CG_Cmd_NextWeapon_f( const CmdArgs & ) {
	if( cg.frame.valid ) {
		ViewState *const viewState = getOurClientViewState();
		if( cgs.demoPlaying || viewState->predictedPlayerState.pmove.pm_type == PM_CHASECAM ) {
			CG_ChaseStep( 1 );
		} else {
			if( const gsitem_t *item = GS_Cmd_NextWeapon_f( cggs, &viewState->snapPlayerState, viewState->predictedWeaponSwitch ) ) {
				CG_Predict_ChangeWeapon( item->tag );
				CL_Cmd_ExecuteNow( va( "cmd use %i", item->tag ) );
				viewState->lastWeapon = viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON];
			}
		}
	}
}

static void CG_Cmd_PrevWeapon_f( const CmdArgs & ) {
	if( cg.frame.valid ) {
		ViewState *const viewState = getOurClientViewState();
		if( cgs.demoPlaying || viewState->predictedPlayerState.pmove.pm_type == PM_CHASECAM ) {
			CG_ChaseStep( -1 );
		} else {
			if( const gsitem_t *item = GS_Cmd_PrevWeapon_f( cggs, &viewState->snapPlayerState, viewState->predictedWeaponSwitch ) ) {
				CG_Predict_ChangeWeapon( item->tag );
				CL_Cmd_ExecuteNow( va( "cmd use %i", item->tag ) );
				viewState->lastWeapon = viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON];
			}
		}
	}
}

static void CG_Cmd_LastWeapon_f( const CmdArgs & ) {
	if( cg.frame.valid && !cgs.demoPlaying ) {
		ViewState *const viewState = getOurClientViewState();
		if( viewState->lastWeapon != WEAP_NONE ) {
			if( viewState->lastWeapon != viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON] ) {
				if( const gsitem_t *item = GS_Cmd_UseItem( cggs, &viewState->snapPlayerState, va( "%i", viewState->lastWeapon ), IT_WEAPON ) ) {
					if( item->type & IT_WEAPON ) {
						CG_Predict_ChangeWeapon( item->tag );
					}
					CL_Cmd_ExecuteNow( va( "cmd use %i", item->tag ) );
					viewState->lastWeapon = viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON];
				}
			}
		}
	}
}

static void CG_Viewpos_f( const CmdArgs & ) {
	const ViewState *const viewState = getPrimaryViewState();
	const float *const origin        = viewState->view.origin;
	const float *const angles        = viewState->view.angles;
	Com_Printf( "\"origin\" \"%i %i %i\"\n", (int)origin[0], (int)origin[1], (int)origin[2] );
	Com_Printf( "\"angles\" \"%i %i %i\"\n", (int)angles[0], (int)angles[1], (int)angles[2] );
}

typedef struct {
	const char *name;
	void ( *func )( const CmdArgs & );
	bool allowdemo;
} cgcmd_t;

static const cgcmd_t cgcmds[] = {
	{ "+scores", CG_ScoresOn_f, true },
	{ "-scores", CG_ScoresOff_f, true },
	{ "messagemode", CG_MessageMode, false },
	{ "messagemode2", CG_MessageMode2, false },
	{ "demoget", CG_Cmd_DemoGet_f, false },
	{ "demolist", NULL, false },
	{ "use", CG_Cmd_UseItem_f, false },
	{ "weapnext", CG_Cmd_NextWeapon_f, true },
	{ "weapprev", CG_Cmd_PrevWeapon_f, true },
	{ "weaplast", CG_Cmd_LastWeapon_f, true },
	{ "viewpos", CG_Viewpos_f, true },
	{ "players", NULL, false },
	{ "spectators", NULL, false },

	{ NULL, NULL, false }
};

void CG_RegisterCGameCommands( void ) {
	if( !cgs.demoPlaying ) {
		// add game side commands
		for( unsigned i = 0; i < MAX_GAMECOMMANDS; i++ ) {
			const auto maybeName = cgs.configStrings.getGameCommand( i );
			if( !maybeName ) {
				continue;
			}

			const auto name = *maybeName;

			// check for local command overrides
			const cgcmd_t *cmd;
			for( cmd = cgcmds; cmd->name; cmd++ ) {
				if( !Q_stricmp( cmd->name, name.data() ) ) {
					break;
				}
			}
			if( cmd->name ) {
				continue;
			}

			CL_Cmd_Register( name, nullptr, nullptr, "game" );
		}
	}

	// add local commands
	for( const auto *cmd = cgcmds; cmd->name; cmd++ ) {
		if( cgs.demoPlaying && !cmd->allowdemo ) {
			continue;
		}
		CL_Cmd_Register( wsw::StringView( cmd->name ), cmd->func );
	}
}

void CG_UnregisterCGameCommands( void ) {
	if( !cgs.demoPlaying ) {
		// remove game commands
		CL_Cmd_UnregisterByTag( "game"_asView );
	}

	// remove local commands
	for( const auto *cmd = cgcmds; cmd->name; cmd++ ) {
		if( cgs.demoPlaying && !cmd->allowdemo ) {
			continue;
		}
		CL_Cmd_Unregister( wsw::StringView( cmd->name ) );
	}
}

static void CG_Event_WeaponBeam( vec3_t origin, vec3_t dir, int ownerNum, int weapon, int firemode ) {
	const gs_weapon_definition_t *weapondef;
	int range;
	vec3_t end;
	trace_t trace;

	switch( weapon ) {
		case WEAP_ELECTROBOLT:
			weapondef = GS_GetWeaponDef( cggs, WEAP_ELECTROBOLT );
			range = ELECTROBOLT_RANGE;
			break;

		case WEAP_INSTAGUN:
			weapondef = GS_GetWeaponDef( cggs, WEAP_INSTAGUN );
			range = weapondef->firedef.timeout;
			break;

		default:
			return;
	}

	VectorNormalizeFast( dir );

	VectorMA( origin, range, dir, end );

	// retrace to spawn wall impact
	// TODO: Check against the owner, not the view state!
	CG_Trace( &trace, origin, vec3_origin, vec3_origin, end, ownerNum, MASK_SOLID );
	if( trace.ent != -1 ) {
		[[maybe_unused]] bool spawnDecal = ( trace.surfFlags & ( SURF_FLESH | SURF_NOMARKS ) ) == 0;
		if( weapondef->weapon_id == WEAP_ELECTROBOLT ) {
			cg.effectsSystem.spawnElectroboltHitEffect( trace.endpos, trace.plane.normal, dir, spawnDecal, ownerNum );
		} else if( weapondef->weapon_id == WEAP_INSTAGUN ) {
			cg.effectsSystem.spawnInstagunHitEffect( trace.endpos, trace.plane.normal, dir, spawnDecal, ownerNum );
		}
	}

	// when it's predicted we have to delay the drawing until the view weapon is calculated
	cg_entities[ownerNum].localEffects[LOCALEFFECT_EV_WEAPONBEAM] = weapon;
	VectorCopy( origin, cg_entities[ownerNum].laserOrigin );
	VectorCopy( trace.endpos, cg_entities[ownerNum].laserPoint );
}

void CG_WeaponBeamEffect( centity_t *cent, ViewState *drawFromViewState ) {
	orientation_t projection;

	if( !cent->localEffects[LOCALEFFECT_EV_WEAPONBEAM] ) {
		return;
	}

	// now find the projection source for the beam we will draw
	if( !CG_PModel_GetProjectionSource( cent->current.number, &projection, drawFromViewState ) ) {
		VectorCopy( cent->laserOrigin, projection.origin );
	}

	if( cent->localEffects[LOCALEFFECT_EV_WEAPONBEAM] == WEAP_ELECTROBOLT ) {
		cg.effectsSystem.spawnElectroboltBeam( projection.origin, cent->laserPoint, cent->current.team );
	} else {
		cg.effectsSystem.spawnInstagunBeam( projection.origin, cent->laserPoint, cent->current.team );
	}

	cent->localEffects[LOCALEFFECT_EV_WEAPONBEAM] = 0;
}

static vec_t *_LaserColor( vec4_t color ) {
	Vector4Set( color, 1, 1, 1, 1 );
	if( v_teamColoredBeams.get() && ( laserOwner != NULL ) && ( laserOwner->current.team == TEAM_ALPHA || laserOwner->current.team == TEAM_BETA ) ) {
		CG_TeamColor( laserOwner->current.team, color );
		AdjustTeamColorValue( color );
	}
	return color;
}

static ParticleColorsForTeamHolder laserImpactParticleColorsHolder {
	.defaultColors = {
		.initial  = { 1.0f, 1.0f, 0.88f, 1.0f },
		.fadedIn  = { 1.0f, 1.0f, 0.39f, 1.0f },
		.fadedOut = { 0.88f, 0.25f, 0.07f, 1.0f },
		.finishFadingInAtLifetimeFrac = 0.1f,
		.startFadingOutAtLifetimeFrac = 0.35f
	}
};

static void CG_LaserGunImpact( const vec3_t pos, const vec3_t dir, float radius, const vec3_t laser_dir,
							   const vec4_t color, DrawSceneRequest *drawSceneRequest ) {
	entity_t ent;
	vec3_t ndir;
	vec3_t angles;

	memset( &ent, 0, sizeof( ent ) );
	VectorCopy( pos, ent.origin );
	VectorMA( ent.origin, 2, dir, ent.origin );
	ent.renderfx = RF_FULLBRIGHT | RF_NOSHADOW;
	ent.scale = 1.45f;
	Vector4Set( ent.shaderRGBA, color[0] * 255, color[1] * 255, color[2] * 255, color[3] * 255 );
	ent.model = cgs.media.modLasergunWallExplo;
	VectorNegate( laser_dir, ndir );
	VecToAngles( ndir, angles );
	angles[2] = anglemod( -360.0f * cg.time * 0.001f );

	AnglesToAxis( angles, ent.axis );

	drawSceneRequest->addEntity( &ent );
}

static void _LaserImpact( trace_t *trace, vec3_t dir ) {
	if( !trace || trace->ent < 0 ) {
		return;
	}

	if( laserOwner && !trace->allsolid ) {
		// Track it regardless of cg_particles settings to prevent hacks with toggling the var on/off
		if( laserOwner->localEffects[LOCALEFFECT_LASERBEAM_SMOKE_TRAIL] + 32 <= cg.time ) {
			laserOwner->localEffects[LOCALEFFECT_LASERBEAM_SMOKE_TRAIL] = cg.time;

			if( v_particles.get() ) {
				bool useTeamColors = false;
				if( v_teamColoredBeams.get() ) {
					if( const int team = laserOwner->current.team; team == TEAM_ALPHA || team == TEAM_BETA ) {
						useTeamColors = true;
					}
				}

				const RgbaLifespan *singleColorAddress;
				ParticleColorsForTeamHolder *holder = &::laserImpactParticleColorsHolder;
				if( useTeamColors ) {
					vec4_t teamColor;
					const int team = laserOwner->current.team;
					CG_TeamColor( team, teamColor );
					singleColorAddress = holder->getColorsForTeam( team, teamColor );
				} else {
					singleColorAddress = &holder->defaultColors;
				}

				const Particle::AppearanceRules appearanceRules{
					.materials = cgs.media.shaderLaserImpactParticle.getAddressOfHandle(),
					.colors    = { singleColorAddress, singleColorAddress + 1 },
					.geometryRules = Particle::SpriteRules{
						.radius = { .mean = 5.0f, .spread = 2.5f },
						.sizeBehaviour = Particle::Shrinking,
					}
				};

				ConicalFlockParams flockParams {
					.origin       = { trace->endpos[0], trace->endpos[1], trace->endpos[2] },
					.offset       = { trace->plane.normal[0], trace->plane.normal[1], trace->plane.normal[2] },
					.gravity      = 0.0f,
					.drag         = 0.02f,
					.angle        = 12.0f,
					.bounceCount  = { .minInclusive = 1, .maxInclusive = 1 },
					.speed        = { .min = 0.0f, .max = 400.0f },
					.percentage   = { .min = 0.0f, .max = 1.0f },
					.timeout      = { .min = 180, .max = 240 },
				};

				constexpr float innerAngle = 30.0f;
				constexpr float angle      = 85.0f;
				const float maxAngleCos   = std::cos( (float) DEG2RAD( innerAngle ) );
				const float minAngleCos   = std::cos( (float) DEG2RAD( angle ) );
				const float angleCosRange = maxAngleCos - minAngleCos;

				constexpr unsigned numSpikes  = 4;
				constexpr float spikeSpeed    = 5e-4f;
				constexpr float spikeFraction = 1.0f / numSpikes;
				constexpr float laserShotTime = 1.0f / 20.0f; // should be equal to fire rate

				const unsigned spikeNum = (unsigned)( (float)cg.time * laserShotTime ) % numSpikes;
				const float coord       = (float)cg.time * spikeSpeed + (float)spikeNum * 10.0f;

				const float coneAngleCosine = minAngleCos + calcSimplexNoise2D( -coord, 0.0f ) * angleCosRange;
				const float angleAlongCone  = DEG2RAD( AngleNormalize360(
					360.0f * ( (float)spikeNum * spikeFraction + calcSimplexNoise2D( coord, 0.0f ) ) ) );

				addRotationToDir( flockParams.dir, coneAngleCosine, angleAlongCone );

				cg.particleSystem.addSmallParticleFlock( appearanceRules, flockParams );
			}

			if( !laserViewStateMuted ) {
				cg.soundSystem->startFixedSound( cgs.media.sndLasergunHit, trace->endpos, CHAN_AUTO,
														  v_volumeEffects.get(), ATTN_STATIC );
			}
		}
#undef TRAILTIME
	}

	vec3_t lightOrigin;
	// Offset the light origin from the impact surface
	VectorMA( trace->endpos, 4.0f, trace->plane.normal, lightOrigin );

	// it's a brush model
	if( trace->ent == 0 || !( cg_entities[trace->ent].current.effects & EF_TAKEDAMAGE ) ) {
		vec4_t color;
		CG_LaserGunImpact( trace->endpos, trace->plane.normal, 15.0f, dir, _LaserColor( color ), laserDrawSceneRequest );
	} else {
		// it's a player
		// TODO: add player-impact model
	}

	laserDrawSceneRequest->addLight( lightOrigin, 144.0f, 0.0f, 0.75f, 0.75f, 0.375f );
}

// TODO: It not only adds entity to scene but also touches persistent/tracked effects
void CG_LaserBeamEffect( centity_t *owner, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	const signed ownerEntNum = owner->current.number;
	const bool isOwnerThePov = drawFromViewState->isViewerEntity( ownerEntNum );
	const bool isCurved      = owner->laserCurved;

	// TODO: Move the entire handling of lasers to the effects system and get rid of this state
	if( owner->localEffects[LOCALEFFECT_LASERBEAM] <= cg.time ) {
		if( owner->localEffects[LOCALEFFECT_LASERBEAM] ) {
			if( drawFromViewState->allowSounds ) {
				const SoundSet *sound = isCurved ? cgs.media.sndLasergunWeakStop : cgs.media.sndLasergunStrongStop;
				cg.soundSystem->startRelativeSound( sound, SoundSystem::WeaponAttachment, ownerEntNum, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
			}
		}
		owner->localEffects[LOCALEFFECT_LASERBEAM] = 0;
		return;
	}

	const bool usePovSlot        = isOwnerThePov && !drawFromViewState->view.thirdperson;
	const unsigned povBit        = ownerEntNum ? 1u << ( ownerEntNum - 1 ) : 0;
	const unsigned povPlayerMask = usePovSlot ? povBit : ( ~0u & ~povBit );

	vec3_t laserOrigin, laserAngles, laserPoint;
	if( usePovSlot ) {
		VectorCopy( drawFromViewState->predictedPlayerState.pmove.origin, laserOrigin );
		laserOrigin[2] += drawFromViewState->predictedPlayerState.viewheight;
		VectorCopy( drawFromViewState->predictedPlayerState.viewangles, laserAngles );

		VectorLerp( owner->laserPointOld, cg.lerpfrac, owner->laserPoint, laserPoint );
	} else {
		VectorLerp( owner->laserOriginOld, cg.lerpfrac, owner->laserOrigin, laserOrigin );
		VectorLerp( owner->laserPointOld, cg.lerpfrac, owner->laserPoint, laserPoint );
		if( isCurved ) {
			// Use player entity angles
			for( int i = 0; i < 3; i++ ) {
				laserAngles[i] = LerpAngle( owner->prev.angles[i], owner->current.angles[i], cg.lerpfrac );
			}
		} else {
			// Make up the angles from the start and end points (s->angles is not so precise)
			vec3_t dir;
			VectorSubtract( laserPoint, laserOrigin, dir );
			VecToAngles( dir, laserAngles );
		}
	}

	// draw the beam: for drawing we use the weapon projection source (already handles the case of viewer entity)
	orientation_t projectsource;
	if( !CG_PModel_GetProjectionSource( ownerEntNum, &projectsource, drawFromViewState ) ) {
		VectorCopy( laserOrigin, projectsource.origin );
	}

	laserOwner = owner;
	laserDrawSceneRequest = drawSceneRequest;
	laserViewStateMuted = !drawFromViewState->allowSounds;

	if( isCurved ) {
		vec3_t from, dir, blendPoint, blendAngles;
		// we redraw the full beam again, and trace each segment for stop dead impact
		VectorCopy( laserPoint, blendPoint );
		VectorCopy( projectsource.origin, from );
		VectorSubtract( blendPoint, projectsource.origin, dir );
		VecToAngles( dir, blendAngles );

		int passthrough             = ownerEntNum;
		const auto range            = (float)GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef_weak.timeout;
		const int minSubdivisions   = CURVELASERBEAM_SUBDIVISIONS;
		const int maxSubdivisions   = MAX_CURVELASERBEAM_SUBDIVISIONS;
		const int subdivisions      = wsw::clamp( v_laserBeamSubdivisions.get(), minSubdivisions, maxSubdivisions );

		unsigned numAddedPoints = 1;
		vec3_t points[MAX_CURVELASERBEAM_SUBDIVISIONS + 1];
		GS_GetCurvedLaserBeamSegments( points, subdivisions, projectsource.origin, laserAngles, range, blendPoint );

		for( int segmentNum = 0; segmentNum < subdivisions; segmentNum++ ) {
			float *stepFrom = points[segmentNum + 0];
			float *stepTo   = points[segmentNum + 1];

			// TODO: GS_TraceLaserBeam() should not require angles
			vec3_t stepDir, tmpangles;
			VectorSubtract( stepTo, stepFrom, stepDir );
			VecToAngles( stepDir, tmpangles );

			trace_t trace;
			GS_TraceLaserBeam( cggs, &trace, stepFrom, tmpangles, DistanceFast( stepFrom, stepTo ), passthrough, 0, _LaserImpact );
			numAddedPoints++;
			if( trace.fraction != 1.0f ) {
				VectorCopy( trace.endpos, stepTo );
				break;
			}

			passthrough = trace.ent;
		}

		std::span<const vec3_t> pointsSpan( points, numAddedPoints );
		cg.effectsSystem.updateCurvedLaserBeam( ownerEntNum, usePovSlot, pointsSpan, cg.time, povPlayerMask );
	} else {
		const auto range = (float)GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef.timeout;

		trace_t trace;
		// trace the beam: for tracing we use the real beam origin
		GS_TraceLaserBeam( cggs, &trace, laserOrigin, laserAngles, range, ownerEntNum, 0, _LaserImpact );

		cg.effectsSystem.updateStraightLaserBeam( ownerEntNum, usePovSlot, projectsource.origin, trace.endpos, cg.time, povPlayerMask );
	}

	// enable continuous flash on the weapon owner
	if( v_weaponFlashes.get() != 0 ) {
		cg_entPModels[ownerEntNum].flash_time = cg.time + CG_GetWeaponInfo( WEAP_LASERGUN )->flashTime;
	}

	if( drawFromViewState->allowSounds ) {
		const SoundSet *sound;
		if( isCurved ) {
			sound = owner->current.effects & EF_QUAD ? cgs.media.sndLasergunWeakQuadHum : cgs.media.sndLasergunWeakHum;
		} else {
			sound = owner->current.effects & EF_QUAD ? cgs.media.sndLasergunStrongQuadHum : cgs.media.sndLasergunStrongHum;
		}
		if( sound ) {
			// Tokens in range [1, MAX_EDICTS] are reserved for generic server-sent attachments
			const uintptr_t loopIdentifyingToken = ownerEntNum + MAX_EDICTS;
			cg.soundSystem->addLoopSound( sound, SoundSystem::WeaponAttachment, ownerEntNum, loopIdentifyingToken, v_volumeEffects.get(), ATTN_NORM );
		}
	}

	laserOwner = nullptr;
	laserDrawSceneRequest = nullptr;
	laserViewStateMuted   = false;
}

void CG_Event_LaserBeam( int entNum, int weapon, int fireMode, ViewState *viewState ) {
	centity_t *cent = &cg_entities[entNum];
	unsigned int timeout;
	vec3_t dir;

	if( !v_predictLaserBeam.get() ) {
		return;
	}

	// lasergun's smooth refire
	if( fireMode == FIRE_MODE_STRONG ) {
		cent->laserCurved = false;
		timeout = GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef.reload_time + 10;

		// find destiny point
		VectorCopy( viewState->predictedPlayerState.pmove.origin, cent->laserOrigin );
		cent->laserOrigin[2] += viewState->predictedPlayerState.viewheight;
		AngleVectors( viewState->predictedPlayerState.viewangles, dir, NULL, NULL );
		VectorMA( cent->laserOrigin, GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef.timeout, dir, cent->laserPoint );
	} else {
		cent->laserCurved = true;
		timeout = GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef_weak.reload_time + 10;

		// find destiny point
		VectorCopy( viewState->predictedPlayerState.pmove.origin, cent->laserOrigin );
		cent->laserOrigin[2] += viewState->predictedPlayerState.viewheight;
		if( !G_GetLaserbeamPoint( &viewState->weaklaserTrail, &viewState->predictedPlayerState, viewState->predictingTimeStamp, cent->laserPoint ) ) {
			AngleVectors( viewState->predictedPlayerState.viewangles, dir, NULL, NULL );
			VectorMA( cent->laserOrigin, GS_GetWeaponDef( cggs, WEAP_LASERGUN )->firedef.timeout, dir, cent->laserPoint );
		}
	}

	// it appears that 64ms is that maximum allowed time interval between prediction events on localhost
	if( timeout < 65 ) {
		timeout = 65;
	}

	VectorCopy( cent->laserOrigin, cent->laserOriginOld );
	VectorCopy( cent->laserPoint, cent->laserPointOld );
	cent->localEffects[LOCALEFFECT_LASERBEAM] = cg.time + timeout;
}

static void CG_FireWeaponEvent( int entNum, int weapon, int fireMode, bool altFrameHint, ViewState *updateViewWeaponInViewState ) {
	if( !weapon ) {
		return;
	}

	const weaponinfo_t *const weaponInfo = CG_GetWeaponInfo( weapon );

	if( const ViewState *playSoundsFromViewState = getPrimaryViewState(); playSoundsFromViewState->allowSounds ) {
		const SoundSet *sound = nullptr;
		// sound
		if( fireMode == FIRE_MODE_STRONG ) {
			if( weaponInfo->num_strongfire_sounds ) {
				sound = weaponInfo->sound_strongfire[(int)brandom( 0, weaponInfo->num_strongfire_sounds )];
			}
		} else {
			if( weaponInfo->num_fire_sounds ) {
				sound = weaponInfo->sound_fire[(int)brandom( 0, weaponInfo->num_fire_sounds )];
			}
		}

		if( sound ) {
			float attenuation;
			// hack idle attenuation on the plasmagun to reduce sound flood on the scene
			if( weapon == WEAP_PLASMAGUN ) {
				attenuation = ATTN_IDLE;
			} else {
				attenuation = ATTN_NORM;
			}
			int channel = CHAN_AUTO;
			if( weapon == WEAP_MACHINEGUN && fireMode == FIRE_MODE_STRONG ) {
				channel = CHAN_MUZZLEFLASH + ( altFrameHint ? 1 : 0 );
			}

			cg.soundSystem->startRelativeSound( sound, SoundSystem::WeaponAttachment, entNum,
														 channel, v_volumeEffects.get(), attenuation );
			if( ( cg_entities[entNum].current.effects & EF_QUAD ) && ( weapon != WEAP_LASERGUN ) ) {
				const SoundSet *quadSound = cgs.media.sndQuadFireSound;
				cg.soundSystem->startRelativeSound( quadSound, SoundSystem::WeaponAttachment, entNum,
															 CHAN_MUZZLEFLASH + 2, v_volumeEffects.get(), attenuation );
			}
		}
	}

	// flash and barrel effects

	if( weapon == WEAP_GUNBLADE ) { // gunblade is special
		if( fireMode == FIRE_MODE_STRONG ) {
			// light flash
			if( v_weaponFlashes.get() != 0 && weaponInfo->flashTime ) {
				cg_entPModels[entNum].flash_time = cg.time + weaponInfo->flashTime;
			}
		} else {
			// start barrel rotation or offsetting
			if( weaponInfo->barrelTime ) {
				cg_entPModels[entNum].barrel_time = cg.time + weaponInfo->barrelTime;
			}
		}
	} else {
		// light flash
		if( v_weaponFlashes.get() != 0 && weaponInfo->flashTime ) {
			cg_entPModels[entNum].flash_time = cg.time + weaponInfo->flashTime;
		}

		// start barrel rotation or offsetting
		if( weaponInfo->barrelTime ) {
			cg_entPModels[entNum].barrel_time = cg.time + weaponInfo->barrelTime;
		}
	}

	// add animation to the player model
	switch( weapon ) {
		case WEAP_NONE:
			break;

		case WEAP_GUNBLADE:
			if( fireMode == FIRE_MODE_WEAK ) {
				CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_BLADE, 0, EVENT_CHANNEL );
			} else {
				CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_PISTOL, 0, EVENT_CHANNEL );
			}
			break;

		case WEAP_LASERGUN:
			CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_PISTOL, 0, EVENT_CHANNEL );
			break;

		default:
		case WEAP_RIOTGUN:
		case WEAP_PLASMAGUN:
			CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_LIGHTWEAPON, 0, EVENT_CHANNEL );
			break;

		case WEAP_ROCKETLAUNCHER:
		case WEAP_GRENADELAUNCHER:
			CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_HEAVYWEAPON, 0, EVENT_CHANNEL );
			break;

		case WEAP_ELECTROBOLT:
			CG_PModel_AddAnimation( entNum, 0, TORSO_SHOOT_AIMWEAPON, 0, EVENT_CHANNEL );
			break;
	}

	// add animation to the view weapon model
	if( updateViewWeaponInViewState && updateViewWeaponInViewState->isViewerEntity( entNum ) && !updateViewWeaponInViewState->view.thirdperson ) {
		CG_ViewWeapon_StartAnimationEvent( fireMode == FIRE_MODE_STRONG ? WEAPMODEL_ATTACK_STRONG : WEAPMODEL_ATTACK_WEAK, updateViewWeaponInViewState );
	}
}

[[nodiscard]]
static bool canShowBulletImpactForDirAndTrace( const float *incidentDir, const trace_t &trace ) {
	if( trace.allsolid ) {
		return false;
	}
	if( trace.surfFlags & ( SURF_NOIMPACT | SURF_FLESH ) ) {
		return false;
	}
	// Wtf how does it happen
	if( DotProduct( trace.plane.normal, incidentDir ) > 0 ) {
		return false;
	}
	const auto entNum = trace.ent;
	if( entNum < 0 ) {
		return false;
	}
	if( const auto entType = cg_entities[entNum].current.type; entType == ET_PLAYER || entType == ET_CORPSE ) {
		return false;
	}
	return true;
}

auto getSurfFlagsForImpact( const trace_t &trace, const float *impactDir ) -> int {
	// Hacks
	// TODO: Trace against brush submodels as well
	if( trace.shaderNum == cgs.fullclipShaderNum ) {
		VisualTrace visualTrace {};
		vec3_t testPoint;

		// Check behind
		VectorMA( trace.endpos, 30.0f, impactDir, testPoint );
		wsw::ref::traceAgainstBspWorld( &visualTrace, trace.endpos, testPoint );
		if( visualTrace.fraction != 1.0f ) {
			return visualTrace.surfFlags;
		}

		// Check in front
		VectorMA( trace.endpos, -4.0f, impactDir, testPoint );
		wsw::ref::traceAgainstBspWorld( &visualTrace, testPoint, trace.endpos );
		if( visualTrace.fraction != 1.0f ) {
			return visualTrace.surfFlags;
		}
	}

	return trace.surfFlags;
}

static void CG_Event_FireMachinegun( vec3_t origin, vec3_t dir, int weapon, int firemode, int seed, int owner ) {
	const auto *weaponDef = GS_GetWeaponDef( cggs, weapon );
	const auto *fireDef   = firemode ? &weaponDef->firedef : &weaponDef->firedef_weak;

	// circle shape
	const float alpha = M_PI * Q_crandom( &seed ); // [-PI ..+PI]
	const float s     = fabs( Q_crandom( &seed ) ); // [0..1]
	const float r     = s * (float)fireDef->spread * std::cos( alpha );
	const float u     = s * (float)fireDef->v_spread * std::sin( alpha );

	VectorNormalizeFast( dir );

	trace_t trace, waterTraceBuffer;

	[[maybe_unused]]
	const trace_t *maybeWaterTrace = GS_TraceBullet( cggs, &trace, &waterTraceBuffer, origin, dir, r, u, (int)fireDef->timeout, owner, 0 );
	if( maybeWaterTrace ) {
		[[maybe_unused]] const unsigned delay = cg.effectsSystem.spawnBulletTracer( owner, maybeWaterTrace->endpos );

		if( canShowBulletImpactForDirAndTrace( dir, trace ) ) {
			cg.effectsSystem.spawnUnderwaterBulletImpactEffect( delay, trace.endpos, trace.plane.normal );
		}

		if( !VectorCompare( maybeWaterTrace->endpos, origin ) ) {
			cg.effectsSystem.spawnBulletLiquidImpactEffect( delay, LiquidImpact {
				.origin   = {maybeWaterTrace->endpos[0], maybeWaterTrace->endpos[1], maybeWaterTrace->endpos[2] },
				.burstDir = {maybeWaterTrace->plane.normal[0], maybeWaterTrace->plane.normal[1], maybeWaterTrace->plane.normal[2] },
				.contents = maybeWaterTrace->contents,
			});
		}
	} else {
		[[maybe_unused]] const unsigned delay = cg.effectsSystem.spawnBulletTracer( owner, trace.endpos );
		if( canShowBulletImpactForDirAndTrace( dir, trace ) ) {
			cg.effectsSystem.spawnBulletImpactEffect( delay, SolidImpact {
				.origin      = { trace.endpos[0], trace.endpos[1], trace.endpos[2] },
				.normal      = { trace.plane.normal[0], trace.plane.normal[1], trace.plane.normal[2] },
				.incidentDir = { dir[0], dir[1], dir[2] },
				.surfFlags   = getSurfFlagsForImpact( trace, dir ),
			});
		}
	}
}

static void CG_Fire_SunflowerPattern( vec3_t start, vec3_t dir, int *seed, int owner, int count,
									  int hspread, int vspread, int range ) {
	assert( seed );
	assert( count > 0 && count < 64 );
	assert( std::abs( VectorLengthFast( dir ) - 1.0f ) < 0.001f );

	auto *const solidImpacts  = (SolidImpact *)alloca( sizeof( SolidImpact ) * count );
	auto *const liquidImpacts = (LiquidImpact *)alloca( sizeof( LiquidImpact ) * count );
	auto *const tracerTargets = (vec3_t *)alloca( sizeof( vec3_t ) * count );

	auto *const underwaterImpactOrigins       = (vec3_t *)alloca( sizeof( vec3_t ) * count );
	auto *const underwaterImpactNormals       = (vec3_t *)alloca( sizeof( vec3_t ) * count );
	auto *const underwaterImpactTracerIndices = (int *)alloca( sizeof( int ) * count );

	auto *const solidImpactTracerIndices  = (unsigned *)alloca( sizeof( unsigned ) * count );
	auto *const liquidImpactTracerIndices = (unsigned *)alloca( sizeof( unsigned ) * count );

	unsigned numSolidImpacts = 0, numLiquidImpacts = 0, numUnderwaterImpacts = 0, numTracerTargets = 0;

	for( int i = 0; i < count; i++ ) {
		// TODO: Is this correct?
		const float phi = 2.4f * (float)i; //magic value creating Fibonacci numbers
		const float sqrtPhi = std::sqrt( phi );

		// TODO: Is this correct?
		const float r = std::cos( (float)*seed + phi ) * (float)hspread * sqrtPhi;
		const float u = std::sin( (float)*seed + phi ) * (float)vspread * sqrtPhi;

		trace_t trace, waterTraceBuffer;
		const trace_t *maybeWaterTrace = GS_TraceBullet( cggs, &trace, &waterTraceBuffer, start, dir, r, u, range, owner, 0 );
		if( maybeWaterTrace ) {
			const bool shouldShowUnderwaterImpact = canShowBulletImpactForDirAndTrace( dir, trace );
			if( shouldShowUnderwaterImpact ) {
				// We don't know the delay yet
				VectorCopy( trace.endpos, underwaterImpactOrigins[numUnderwaterImpacts] );
				VectorCopy( trace.plane.normal, underwaterImpactNormals[numUnderwaterImpacts] );
			}
			if( !VectorCompare( maybeWaterTrace->endpos, start ) ) {
				liquidImpacts[numLiquidImpacts] = LiquidImpact {
					.origin   = {maybeWaterTrace->endpos[0], maybeWaterTrace->endpos[1], maybeWaterTrace->endpos[2] },
					.burstDir = {maybeWaterTrace->plane.normal[0], maybeWaterTrace->plane.normal[1], maybeWaterTrace->plane.normal[2] },
					.contents = maybeWaterTrace->contents,
				};
				liquidImpactTracerIndices[numLiquidImpacts] = i;
				++numLiquidImpacts;
				if( shouldShowUnderwaterImpact ) {
					underwaterImpactTracerIndices[numUnderwaterImpacts] = (int)i;
				}
			} else {
				if( shouldShowUnderwaterImpact ) {
					underwaterImpactTracerIndices[numUnderwaterImpacts] = -1;
				}
			}
			if( shouldShowUnderwaterImpact ) {
				numUnderwaterImpacts++;
			}
			VectorCopy( maybeWaterTrace->endpos, tracerTargets[numTracerTargets] );
			numTracerTargets++;
		} else {
			if( canShowBulletImpactForDirAndTrace( dir, trace ) ) {
				solidImpacts[numSolidImpacts] = SolidImpact {
					.origin      = { trace.endpos[0], trace.endpos[1], trace.endpos[2] },
					.normal      = { trace.plane.normal[0], trace.plane.normal[1], trace.plane.normal[2] },
					.incidentDir = { dir[0], dir[1], dir[2] },
					.surfFlags   = getSurfFlagsForImpact( trace, dir ),
				};
				solidImpactTracerIndices[numSolidImpacts] = i;
				numSolidImpacts++;
			}
			VectorCopy( trace.endpos, tracerTargets[numTracerTargets] );
			numTracerTargets++;
		}
	}

	auto *const tracerDelaysBuffer = (unsigned *)alloca( sizeof( unsigned ) * count );
	auto *const liquidImpactDelays = (unsigned *)alloca( sizeof( unsigned ) * count );
	auto *const solidImpactDelays  = (unsigned *)alloca( sizeof( unsigned ) * count );

	// TODO: Pass the origin stride plus impacts?
	cg.effectsSystem.spawnPelletTracers( owner, { tracerTargets, numTracerTargets }, tracerDelaysBuffer );

	for( unsigned i = 0; i < numSolidImpacts; ++i ) {
		solidImpactDelays[i] = tracerDelaysBuffer[solidImpactTracerIndices[i]];
	}
	for( unsigned i = 0; i < numLiquidImpacts; ++i ) {
		liquidImpactDelays[i] = tracerDelaysBuffer[liquidImpactTracerIndices[i]];
	}

	for( unsigned i = 0; i < numUnderwaterImpacts; ++i ) {
		unsigned delay = 0;
		if( const int tracerIndex = underwaterImpactTracerIndices[i]; tracerIndex >= 0 ) {
			// Using the tracer delay is not really correct as there's underwater added distance but this issue is minor
			delay = tracerDelaysBuffer[tracerIndex];
		}
		cg.effectsSystem.spawnUnderwaterPelletImpactEffect( delay, underwaterImpactOrigins[i], underwaterImpactNormals[i] );
	}

	cg.effectsSystem.spawnMultiplePelletImpactEffects( { solidImpacts, numSolidImpacts }, { solidImpactDelays, numSolidImpacts } );

	cg.effectsSystem.spawnMultipleLiquidImpactEffects( { liquidImpacts, numLiquidImpacts }, 0.1f, { 0.3f, 0.9f },
													   std::span<const unsigned> { liquidImpactDelays, numLiquidImpacts } );
}

static void CG_Event_FireRiotgun( vec3_t origin, vec3_t dirVec, int weapon, int firemode, int seed, int owner ) {
	vec3_t dir;
	VectorCopy( dirVec, dir );
	VectorNormalizeFast( dir );

	const gs_weapon_definition_t *weapondef = GS_GetWeaponDef( cggs, weapon );
	const firedef_t *firedef = ( firemode ) ? &weapondef->firedef : &weapondef->firedef_weak;

	CG_Fire_SunflowerPattern( origin, dir, &seed, owner, firedef->projectile_count,
							  firedef->spread, firedef->v_spread, firedef->timeout );
}

void CG_ClearAnnouncerEvents( void ) {
	cg_announcerEventsCurrent = cg_announcerEventsHead = 0;
}

void CG_AddAnnouncerEvent( const SoundSet *sound, bool queued ) {
	if( sound ) [[likely]] {
		cg.soundSystem->startLocalSound( sound, v_volumeAnnouncer.get() );
		cg_announcerEventsDelay = CG_ANNOUNCER_EVENTS_FRAMETIME; // wait
	} else {
		if( cg_announcerEventsCurrent + CG_MAX_ANNOUNCER_EVENTS >= cg_announcerEventsHead ) {
			// full buffer (we do nothing, just let it overwrite the oldest
		}
		// add it
		cg_announcerEvents[cg_announcerEventsHead & CG_MAX_ANNOUNCER_EVENTS_MASK].sound = sound;
		cg_announcerEventsHead++;
	}
}

void CG_ReleaseAnnouncerEvents( void ) {
	// see if enough time has passed
	cg_announcerEventsDelay -= cg.realFrameTime;
	if( cg_announcerEventsDelay > 0 ) {
		return;
	}

	if( cg_announcerEventsCurrent < cg_announcerEventsHead ) {
		// play the event
		const SoundSet *sound = cg_announcerEvents[cg_announcerEventsCurrent & CG_MAX_ANNOUNCER_EVENTS_MASK].sound;
		if( sound ) {
			cg.soundSystem->startLocalSound( sound, v_volumeAnnouncer.get() );
			cg_announcerEventsDelay = CG_ANNOUNCER_EVENTS_FRAMETIME; // wait
		}
		cg_announcerEventsCurrent++;
	} else {
		cg_announcerEventsDelay = 0; // no wait
	}
}

void CG_Event_Fall( entity_state_t *state, int parm ) {
	ViewState *const ownerViewState = getViewStateForEntity( state->number );
	// Isn't the latter redundant?
	if( ownerViewState && ownerViewState->isViewerEntity( state->number ) ) {
		if( ownerViewState->snapPlayerState.pmove.pm_type != PM_NORMAL ) {
			playSexedSoundInPrimaryView( state->number, SoundSystem::FeetAttachment, CHAN_AUTO,
										 "*fall_0", v_volumePlayers.get(), state->attenuation );
			return;
		}

		CG_StartFallKickEffect( ( parm + 5 ) * 10, ownerViewState );

		if( parm >= 15 ) {
			CG_DamageIndicatorAdd( parm, tv( 0, 0, 1 ), ownerViewState );
		}
	}

	// TODO: There should be two separate sounds with different attachments, possibly mergeable for optimization
	if( parm > 10 ) {
		playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_PAIN, "*fall_2", v_volumePlayers.get(), state->attenuation );
		switch( (int)brandom( 0, 3 ) ) {
			case 0:
				CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN1, 0, EVENT_CHANNEL );
				break;
			case 1:
				CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN2, 0, EVENT_CHANNEL );
				break;
			case 2:
			default:
				CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN3, 0, EVENT_CHANNEL );
				break;
		}
	} else if( parm > 0 ) {
		playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_PAIN, "*fall_1", v_volumePlayers.get(), state->attenuation );
	} else {
		playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_PAIN, "*fall_0", v_volumePlayers.get(), state->attenuation );
	}

	// smoke effect
	if( parm > 0 ) {
		vec3_t start, end;
		trace_t trace;

		if( ownerViewState && ownerViewState->isViewerEntity( state->number ) ) {
			VectorCopy( ownerViewState->predictedPlayerState.pmove.origin, start );
		} else {
			VectorCopy( state->origin, start );
		}

		VectorCopy( start, end );
		end[2] += playerbox_stand_mins[2] - 48.0f;

		CG_Trace( &trace, start, vec3_origin, vec3_origin, end, state->number, MASK_PLAYERSOLID );
		if( trace.ent == -1 ) {
			start[2] += playerbox_stand_mins[2] + 8;
			cg.effectsSystem.spawnLandingDustImpactEffect( start, tv( 0, 0, 1 ) );
		} else if( !( trace.surfFlags & SURF_NODAMAGE ) ) {
			VectorMA( trace.endpos, 8, trace.plane.normal, end );
			cg.effectsSystem.spawnLandingDustImpactEffect( end, trace.plane.normal );
		}
	}
}

void CG_Event_Pain( entity_state_t *state, int parm ) {
	if( parm == PAIN_WARSHELL ) {
		// TODO: What if it's not fullscreen?
		if( getPrimaryViewState()->isViewerEntity( state->number ) ) {
			cg.soundSystem->startRelativeSound( cgs.media.sndShellHit, SoundSystem::HeadAttachment,
														 state->number, CHAN_PAIN, v_volumePlayers.get(), state->attenuation );
		}
	} else {
		playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_PAIN, va( S_PLAYER_PAINS, 25 * parm ), v_volumePlayers.get(), state->attenuation );
	}

	switch( (int)brandom( 0, 3 ) ) {
		case 0:
			CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN1, 0, EVENT_CHANNEL );
			break;
		case 1:
			CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN2, 0, EVENT_CHANNEL );
			break;
		case 2:
		default:
			CG_PModel_AddAnimation( state->number, 0, TORSO_PAIN3, 0, EVENT_CHANNEL );
			break;
	}
}

void CG_Event_Die( entity_state_t *state, int parm ) {
	// TODO: What attachment tag should we use
	playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_PAIN, S_PLAYER_DEATH, v_volumePlayers.get(), state->attenuation );

	switch( parm ) {
		case 0:
		default:
			CG_PModel_AddAnimation( state->number, BOTH_DEATH1, BOTH_DEATH1, ANIM_NONE, EVENT_CHANNEL );
			break;
		case 1:
			CG_PModel_AddAnimation( state->number, BOTH_DEATH2, BOTH_DEATH2, ANIM_NONE, EVENT_CHANNEL );
			break;
		case 2:
			CG_PModel_AddAnimation( state->number, BOTH_DEATH3, BOTH_DEATH3, ANIM_NONE, EVENT_CHANNEL );
			break;
	}
}

void CG_Event_Dash( entity_state_t *state, int parm ) {
	switch( parm ) {
		default:
			break;
		case 0: // dash front
			CG_PModel_AddAnimation( state->number, LEGS_DASH, 0, 0, EVENT_CHANNEL );
			playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
										 va( S_PLAYER_DASH_1_to_2, ( rand() & 1 ) + 1 ),
										 v_volumePlayers.get(), state->attenuation );
			break;
		case 1: // dash left
			CG_PModel_AddAnimation( state->number, LEGS_DASH_LEFT, 0, 0, EVENT_CHANNEL );
			playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
										 va( S_PLAYER_DASH_1_to_2, ( rand() & 1 ) + 1 ),
										 v_volumePlayers.get(), state->attenuation );
			break;
		case 2: // dash right
			CG_PModel_AddAnimation( state->number, LEGS_DASH_RIGHT, 0, 0, EVENT_CHANNEL );
			playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
										 va( S_PLAYER_DASH_1_to_2, ( rand() & 1 ) + 1 ),
										 v_volumePlayers.get(), state->attenuation );
			break;
		case 3: // dash back
			CG_PModel_AddAnimation( state->number, LEGS_DASH_BACK, 0, 0, EVENT_CHANNEL );
			playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
										 va( S_PLAYER_DASH_1_to_2, ( rand() & 1 ) + 1 ),
										 v_volumePlayers.get(), state->attenuation );
			break;
	}

	cg.effectsSystem.spawnDashEffect( cg_entities[state->number].prev.origin, state->origin );

	// since most dash animations jump with right leg, reset the jump to start with left leg after a dash
	cg_entities[state->number].jumpedLeft = true;
}

void CG_Event_WallJump( entity_state_t *state, int parm, int ev ) {
	vec3_t normal, forward, right;

	ByteToDir( parm, normal );

	AngleVectors( tv( state->angles[0], state->angles[1], 0 ), forward, right, NULL );

	if( DotProduct( normal, right ) > 0.3 ) {
		CG_PModel_AddAnimation( state->number, LEGS_WALLJUMP_RIGHT, 0, 0, EVENT_CHANNEL );
	} else if( -DotProduct( normal, right ) > 0.3 ) {
		CG_PModel_AddAnimation( state->number, LEGS_WALLJUMP_LEFT, 0, 0, EVENT_CHANNEL );
	} else if( -DotProduct( normal, forward ) > 0.3 ) {
		CG_PModel_AddAnimation( state->number, LEGS_WALLJUMP_BACK, 0, 0, EVENT_CHANNEL );
	} else {
		CG_PModel_AddAnimation( state->number, LEGS_WALLJUMP, 0, 0, EVENT_CHANNEL );
	}

	if( ev == EV_WALLJUMP_FAILED ) {
		cg.soundSystem->startRelativeSound( cgs.media.sndWalljumpFailed, SoundSystem::FeetAttachment,
													 state->number, CHAN_BODY, v_volumeEffects.get(), ATTN_NORM );
	} else {
		// TODO: Voice and thud should be split and should use different attachments (mergeable for optimization)
		playSexedSoundInPrimaryView( state->number, SoundSystem::FeetAttachment, CHAN_BODY,
									 va( S_PLAYER_WALLJUMP_1_to_2, ( rand() & 1 ) + 1 ),
									 v_volumePlayers.get(), state->attenuation );

		// smoke effect
		vec3_t pos;
		VectorCopy( state->origin, pos );
		pos[2] += 15;
		cg.effectsSystem.spawnWalljumpDustImpactEffect( pos, normal );
	}
}

void CG_Event_DoubleJump( entity_state_t *state, int parm ) {
	playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
								 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ),
								 v_volumePlayers.get(), state->attenuation );
}

void CG_Event_Jump( entity_state_t *state, int parm ) {
	float attenuation = state->attenuation;
	// Hack for the bobot jump sound.
	// Amplifying it is not an option as it becomes annoying at close range.
	// Note that this can not and should not be handled at the game-server level as a client may use an arbitrary model.
	if( const char *modelName = cg_entPModels[state->number].pmodelinfo->name ) {
		if( ::strstr( modelName, "bobot" ) ) {
			attenuation = ATTN_DISTANT;
		}
	}

	centity_t *cent = &cg_entities[state->number];
	float xyspeedcheck = Q_Sqrt( cent->animVelocity[0] * cent->animVelocity[0] + cent->animVelocity[1] * cent->animVelocity[1] );
	if( xyspeedcheck < 100 ) { // the player is jumping on the same place, not running
		CG_PModel_AddAnimation( state->number, LEGS_JUMP_NEUTRAL, 0, 0, EVENT_CHANNEL );
		playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
									 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ),
									 v_volumePlayers.get(), attenuation );
	} else {
		vec3_t movedir;
		mat3_t viewaxis;

		movedir[0] = cent->animVelocity[0];
		movedir[1] = cent->animVelocity[1];
		movedir[2] = 0;
		VectorNormalizeFast( movedir );

		Matrix3_FromAngles( tv( 0, cent->current.angles[YAW], 0 ), viewaxis );

		// see what's his relative movement direction
		if( DotProduct( movedir, &viewaxis[AXIS_FORWARD] ) > 0.25f ) {
			cent->jumpedLeft = !cent->jumpedLeft;
			if( !cent->jumpedLeft ) {
				CG_PModel_AddAnimation( state->number, LEGS_JUMP_LEG2, 0, 0, EVENT_CHANNEL );
				playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
											 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ),
											 v_volumePlayers.get(), attenuation );
			} else {
				CG_PModel_AddAnimation( state->number, LEGS_JUMP_LEG1, 0, 0, EVENT_CHANNEL );
				playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
											 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ),
											 v_volumePlayers.get(), attenuation );
			}
		} else {
			CG_PModel_AddAnimation( state->number, LEGS_JUMP_NEUTRAL, 0, 0, EVENT_CHANNEL );
			playSexedSoundInPrimaryView( state->number, SoundSystem::HeadAttachment, CHAN_BODY,
										 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ),
										 v_volumePlayers.get(), attenuation );
		}
	}
}

static void handleWeaponActivateEvent( entity_state_t *ent, int parm, bool predicted ) {
	const int weapon          = ( parm >> 1 ) & 0x3f;
	const int fireMode        = ( parm & 0x1 ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
	ViewState *ownerViewState = getViewStateForEntity( ent->number );
	const bool viewer         = ownerViewState && ownerViewState->isViewerEntity( ent->number );

	CG_PModel_AddAnimation( ent->number, 0, TORSO_WEAPON_SWITCHIN, 0, EVENT_CHANNEL );

	if( predicted ) {
		cg_entities[ent->number].current.weapon = weapon;
		if( fireMode == FIRE_MODE_STRONG ) {
			cg_entities[ent->number].current.effects |= EF_STRONG_WEAPON;
		}

		if( ownerViewState ) {
			CG_ViewWeapon_RefreshAnimation( &ownerViewState->weapon, ownerViewState );
		}
	}

	if( viewer && ownerViewState == getOurClientViewState() ) {
		ownerViewState->predictedWeaponSwitch = 0;
	}

	const ViewState *primaryViewState = getPrimaryViewState();
	if( primaryViewState->allowSounds ) {
		cg.soundSystem->startRelativeSound( cgs.media.sndWeaponUp, SoundSystem::WeaponAttachment,
													 ent->number, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
	}
}

static void handleSmoothRefireWeaponEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( predicted ) {
		const int weapon = ( parm >> 1 ) & 0x3f;
		const int fireMode = ( parm & 0x1 ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;

		cg_entities[ent->number].current.weapon = weapon;
		if( fireMode == FIRE_MODE_STRONG ) {
			cg_entities[ent->number].current.effects |= EF_STRONG_WEAPON;
		}

		ViewState *ownerViewState = getViewStateForEntity( ent->number );
		assert( ownerViewState == getOurClientViewState() );
		CG_ViewWeapon_RefreshAnimation( &ownerViewState->weapon, ownerViewState );

		if( weapon == WEAP_LASERGUN ) {
			CG_Event_LaserBeam( ent->number, weapon, fireMode, ownerViewState );
		}
	}
}

static void handleFireWeaponEvent( entity_state_t *ent, int parm, bool predicted ) {
	const int weapon = ( parm >> 1 ) & 0x3f;
	const int fireMode = ( parm & 0x1 ) ? FIRE_MODE_STRONG : FIRE_MODE_WEAK;
	const bool altFrameHint = ( parm & 128 ) != 0;

	if( predicted ) {
		cg_entities[ent->number].current.weapon = weapon;
		if( fireMode == FIRE_MODE_STRONG ) {
			cg_entities[ent->number].current.effects |= EF_STRONG_WEAPON;
		}
	}

	ViewState *const attackerViewState = getViewStateForEntity( ent->number );
	CG_FireWeaponEvent( ent->number, weapon, fireMode, altFrameHint, attackerViewState );

	// riotgun bullets, electrobolt and instagun beams are predicted when the weapon is fired
	if( predicted ) {
		assert( attackerViewState );
		vec3_t origin, dir;

		if( ( weapon == WEAP_ELECTROBOLT && fireMode == FIRE_MODE_STRONG ) || weapon == WEAP_INSTAGUN ) {
			VectorCopy( attackerViewState->predictedPlayerState.pmove.origin, origin );
			origin[2] += attackerViewState->predictedPlayerState.viewheight;
			AngleVectors( attackerViewState->predictedPlayerState.viewangles, dir, NULL, NULL );
			CG_Event_WeaponBeam( origin, dir, attackerViewState->predictedPlayerState.POVnum, weapon, fireMode );
		} else if( weapon == WEAP_RIOTGUN || weapon == WEAP_MACHINEGUN ) {
			const int seed = attackerViewState->predictedEventTimes[EV_FIREWEAPON] & 255;

			VectorCopy( attackerViewState->predictedPlayerState.pmove.origin, origin );
			origin[2] += attackerViewState->predictedPlayerState.viewheight;
			AngleVectors( attackerViewState->predictedPlayerState.viewangles, dir, NULL, NULL );

			if( weapon == WEAP_RIOTGUN ) {
				CG_Event_FireRiotgun( origin, dir, weapon, fireMode, seed, attackerViewState->predictedPlayerState.POVnum );
			} else {
				CG_Event_FireMachinegun( origin, dir, weapon, fireMode, seed, attackerViewState->predictedPlayerState.POVnum );
			}
		} else if( weapon == WEAP_LASERGUN ) {
			CG_Event_LaserBeam( ent->number, weapon, fireMode, attackerViewState );
		}
	}
}

static void handleElectroTrailEvent( entity_state_t *ent, int parm, bool predicted ) {
	ViewState *const viewState = getOurClientViewState();
	// check the owner for predicted case
	if( viewState->isViewerEntity( parm ) && ( predicted != viewState->view.playerPrediction ) ) {
		return;
	}

	CG_Event_WeaponBeam( ent->origin, ent->origin2, parm, WEAP_ELECTROBOLT, ent->firemode );
}

static void handleInstaTrailEvent( entity_state_t *ent, int parm, bool predicted ) {
	ViewState *const viewState = getOurClientViewState();
	// check the owner for predicted case
	if( viewState->isViewerEntity( parm ) && ( predicted != viewState->view.playerPrediction ) ) {
		return;
	}

	CG_Event_WeaponBeam( ent->origin, ent->origin2, parm, WEAP_INSTAGUN, FIRE_MODE_STRONG );
}

static void handleFireRiotgunEvent( entity_state_t *ent, int parm, bool predicted ) {
	ViewState *const viewState = getOurClientViewState();
	// check the owner for predicted case
	if( viewState->isViewerEntity( ent->ownerNum ) && ( predicted != viewState->view.playerPrediction ) ) {
		return;
	}

	CG_Event_FireRiotgun( ent->origin, ent->origin2, ent->weapon, ent->firemode, parm, ent->ownerNum );
}

static void handleFireBulletEvent( entity_state_t *ent, int parm, bool predicted ) {
	ViewState *const viewState = getOurClientViewState();
	// check the owner for predicted case
	if( viewState->isViewerEntity( ent->ownerNum ) && ( predicted != viewState->view.playerPrediction ) ) {
		return;
	}

	CG_Event_FireMachinegun( ent->origin, ent->origin2, ent->weapon, ent->firemode, parm, ent->ownerNum );
}

static void handleNoAmmoClickEvent( entity_state_t *ent, int parm, bool predicted ) {
	ViewState *const primaryViewState = getPrimaryViewState();
	if( primaryViewState->allowSounds ) {
		cg.soundSystem->startRelativeSound( cgs.media.sndWeaponUpNoAmmo, SoundSystem::WeaponAttachment,
													 ent->number, CHAN_ITEM, v_volumeEffects.get(), ATTN_IDLE );
	}
}

static void handleJumppadEvent( entity_state_t *ent, bool predicted ) {
	playSexedSoundInPrimaryView( ent->number, SoundSystem::HeadAttachment, CHAN_BODY,
								 va( S_PLAYER_JUMP_1_to_2, ( rand() & 1 ) + 1 ), v_volumePlayers.get(), ent->attenuation );
	CG_PModel_AddAnimation( ent->number, LEGS_JUMP_NEUTRAL, 0, 0, EVENT_CHANNEL );
}

static void handleSexedSoundEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( parm == 2 ) {
		playSexedSoundInPrimaryView( ent->number, SoundSystem::HeadAttachment,
									 CHAN_AUTO, S_PLAYER_GASP, v_volumePlayers.get(), ent->attenuation );
	} else if( parm == 1 ) {
		playSexedSoundInPrimaryView( ent->number, SoundSystem::HeadAttachment,
									 CHAN_AUTO, S_PLAYER_DROWN, v_volumePlayers.get(), ent->attenuation );
	}
}

static void handlePnodeEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec4_t color;
	color[0] = COLOR_R( ent->colorRGBA ) * ( 1.0 / 255.0 );
	color[1] = COLOR_G( ent->colorRGBA ) * ( 1.0 / 255.0 );
	color[2] = COLOR_B( ent->colorRGBA ) * ( 1.0 / 255.0 );
	color[3] = COLOR_A( ent->colorRGBA ) * ( 1.0 / 255.0 );
	cg.effectsSystem.spawnGameDebugBeam( ent->origin, ent->origin2, color, parm );
}

static const RgbaLifespan kSparksColor {
	.initial  = { 1.0f, 0.5f, 0.1f, 0.0f },
	.fadedIn  = { 1.0f, 1.0f, 1.0f, 1.0f },
	.fadedOut = { 0.5f, 0.5f, 0.5f, 0.5f },
};

static void handleSparksEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( v_particles.get() ) {
		vec3_t dir;
		ByteToDir( parm, dir );

		int count;
		if( ent->damage > 0 ) {
			count = (int)( ent->damage * 0.25f );
			Q_clamp( count, 1, 10 );
		} else {
			count = 6;
		}

		ConicalFlockParams flockParams {
			.origin = { ent->origin[0], ent->origin[1], ent->origin[2] },
			.offset = { dir[0], dir[1], dir[2] },
			.dir    = { dir[0], dir[1], dir[2] }
		};

		Particle::AppearanceRules appearanceRules {
			.materials     = cgs.media.shaderMetalRicochetParticle.getAddressOfHandle(),
			.colors        = { &kSparksColor, 1 },
			.geometryRules = Particle::SparkRules { .length = { .mean = 4.0f }, .width = { .mean = 1.0f } },
		};

		cg.particleSystem.addSmallParticleFlock( appearanceRules, flockParams );
	}
}

static void handleBulletSparksEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	ByteToDir( parm, dir );
	// TODO???
}

static void handleItemRespawnEvent( entity_state_t *ent, int parm, bool predicted ) {
	cg_entities[ent->number].respawnTime = cg.time;
	if( getPrimaryViewState()->allowSounds ) {
		cg.soundSystem->startFixedSound( cgs.media.sndItemRespawn, ent->origin, CHAN_AUTO, v_volumeEffects.get(), ATTN_IDLE );
	}
}

static void handlePlayerRespawnEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( getPrimaryViewState()->allowSounds ) {
		cg.soundSystem->startFixedSound( cgs.media.sndPlayerRespawn, ent->origin, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
	}

	if( ent->ownerNum && ent->ownerNum < cggs->maxclients + 1 ) {
		if( ViewState *const viewState = getViewStateForEntity( ent->ownerNum ) ) {
			CG_ResetKickAngles( viewState );
			CG_ResetColorBlend( viewState );
			CG_ResetDamageIndicator( viewState );
			if( viewState == getPrimaryViewState() ) {
				wsw::ui::UISystem::instance()->handleGCSafepoint( wsw::ui::UISystem::GCSafepointKind::Respawn );
			}
		}

		cg_entities[ent->ownerNum].localEffects[LOCALEFFECT_EV_PLAYER_TELEPORT_IN] = cg.time;
		VectorCopy( ent->origin, cg_entities[ent->ownerNum].teleportedTo );
	}
}

static void handlePlayerTeleportInEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( getPrimaryViewState()->allowSounds ) {
		cg.soundSystem->startFixedSound( cgs.media.sndTeleportIn, ent->origin, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
	}

	if( ent->ownerNum && ent->ownerNum < cggs->maxclients + 1 ) {
		cg_entities[ent->ownerNum].localEffects[LOCALEFFECT_EV_PLAYER_TELEPORT_IN] = cg.time;
		VectorCopy( ent->origin, cg_entities[ent->ownerNum].teleportedTo );
		if( getViewStateForEntity( ent->ownerNum ) == getPrimaryViewState() ) {
			wsw::ui::UISystem::instance()->handleGCSafepoint( wsw::ui::UISystem::GCSafepointKind::Teleport );
		}
	}
}

static void handlePlayerTeleportOutEvent( entity_state_t *ent, int parm, bool predicted ) {
	if( getPrimaryViewState()->allowSounds ) {
		cg.soundSystem->startFixedSound( cgs.media.sndTeleportOut, ent->origin, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
	}

	if( ent->ownerNum && ent->ownerNum < cggs->maxclients + 1 ) {
		cg_entities[ent->ownerNum].localEffects[LOCALEFFECT_EV_PLAYER_TELEPORT_OUT] = cg.time;
		VectorCopy( ent->origin, cg_entities[ent->ownerNum].teleportedFrom );
		if( getViewStateForEntity( ent->ownerNum ) == getPrimaryViewState() ) {
			wsw::ui::UISystem::instance()->handleGCSafepoint( wsw::ui::UISystem::GCSafepointKind::Teleport );
		}
	}
}

static void handlePlasmaExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	ByteToDir( parm, dir );

	cg.effectsSystem.spawnPlasmaExplosionEffect( ent->origin, dir, ent->firemode );

	if( ent->firemode == FIRE_MODE_STRONG ) {
		CG_StartKickAnglesEffect( ent->origin, 50, ent->weapon * 8, 100 );
	} else {
		CG_StartKickAnglesEffect( ent->origin, 30, ent->weapon * 8, 75 );
	}
}

static inline void decodeBoltImpact( int parm, vec3_t impactNormal, vec3_t impactDir, bool *spawnWallImpact ) {
	const unsigned impactDirByte    = ( (unsigned)parm >> 8 ) & 0xFF;
	const unsigned impactNormalByte = ( (unsigned)parm >> 0 ) & 0xFF;

	ByteToDir( (int)impactNormalByte, impactNormal );
	ByteToDir( (int)impactDirByte, impactDir );

	*spawnWallImpact = ( (unsigned)parm >> 16 ) != 0;
}

static void handleBoltExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t impactNormal, impactDir;
	bool spawnWallImpact;
	decodeBoltImpact( parm, impactNormal, impactDir, &spawnWallImpact );

	cg.effectsSystem.spawnElectroboltHitEffect( ent->origin, impactNormal, impactDir, spawnWallImpact, ent->ownerNum );
}

static void handleInstaExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t impactNormal, impactDir;
	bool spawnWallImpact;
	decodeBoltImpact( parm, impactNormal, impactDir, &spawnWallImpact );

	cg.effectsSystem.spawnInstagunHitEffect( ent->origin, impactNormal, impactDir, spawnWallImpact, ent->ownerNum );
}

static void handleGrenadeExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	if( parm ) {
		// we have a direction
		ByteToDir( parm, dir );
		cg.effectsSystem.spawnGrenadeExplosionEffect( ent->origin, dir, ent->firemode );
	} else {
		cg.effectsSystem.spawnGrenadeExplosionEffect( ent->origin, &axis_identity[AXIS_UP], ent->firemode );
	}

	if( ent->firemode == FIRE_MODE_STRONG ) {
		CG_StartKickAnglesEffect( ent->origin, 135, ent->weapon * 8, 325 );
	} else {
		CG_StartKickAnglesEffect( ent->origin, 125, ent->weapon * 8, 300 );
	}
}

static void handleRocketExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	ByteToDir( parm, dir );

	cg.effectsSystem.spawnRocketExplosionEffect( ent->origin, dir, ent->firemode );

	if( ent->firemode == FIRE_MODE_STRONG ) {
		CG_StartKickAnglesEffect( ent->origin, 135, ent->weapon * 8, 300 );
	} else {
		CG_StartKickAnglesEffect( ent->origin, 125, ent->weapon * 8, 275 );
	}
}

static void handleShockwaveExplosionEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	ByteToDir( parm, dir );

	cg.effectsSystem.spawnShockwaveExplosionEffect( ent->origin, dir, ent->firemode );

	if( ent->firemode == FIRE_MODE_STRONG ) {
		CG_StartKickAnglesEffect( ent->origin, 90, ent->weapon * 8, 200 );
	} else {
		CG_StartKickAnglesEffect( ent->origin, 90, ent->weapon * 8, 200 );
	}
}

static void handleGrenadeBounceEvent( entity_state_t *ent, int parm ) {
	if( getPrimaryViewState()->allowSounds ) {
		const SoundSet *sound = nullptr;
		if( parm == FIRE_MODE_STRONG ) {
			sound = cgs.media.sndGrenadeStrongBounce;
		} else {
			sound = cgs.media.sndGrenadeWeakBounce;
		}
		cg.soundSystem->startRelativeSound( sound, SoundSystem::OriginAttachment, ent->number, CHAN_AUTO, v_volumeEffects.get(), ATTN_IDLE );
	}
}

static void handleGrenadeArmOrActivateEvent( entity_state_t *ent ) {
	if( getPrimaryViewState()->allowSounds ) {
		const SoundSet *sound = nullptr;
		if( ent->effects & EF_ACTIVATED ) {
			sound = cgs.media.sndGrenadeActivate;
		} else {
			sound = cgs.media.sndGrenadeArm;
		}
		cg.soundSystem->startRelativeSound( sound, SoundSystem::OriginAttachment, ent->number, CHAN_AUTO, v_volumeEffects.get(), ATTN_IDLE );
	}
}

static void handleGunbladeBlastImpactEvent( entity_state_t *ent, int parm, bool predicted ) {
	vec3_t dir;
	ByteToDir( parm, dir );

	cg.effectsSystem.spawnGunbladeBlastHitEffect( ent->origin, dir );

	//ent->skinnum is knockback value
	CG_StartKickAnglesEffect( ent->origin, ent->skinnum * 8, ent->weapon * 8, 200 );
}

static void handleBloodEvent( entity_state_t *ent, int parm, bool predicted ) {
	unsigned povPlayerMask = ~0u;
	if( !v_showPovBlood.get() ) {
		if( ent->ownerNum > 0 && ent->ownerNum <= cggs->maxclients ) {
			povPlayerMask &= ~( 1u << ( ent->ownerNum - 1 ) );
		}
	}

	vec3_t dir;
	ByteToDir( parm, dir );
	if( VectorCompare( dir, vec3_origin ) ) {
		dir[2] = 1.0f;
	}
	cg.effectsSystem.spawnPlayerHitEffect( ent->origin, dir, ent->damage, povPlayerMask );
}

static void handleMoverEvent( entity_state_t *ent, int parm ) {
	if( getPrimaryViewState()->allowSounds ) {
		[[maybe_unused]] vec3_t so, velocity;
		[[maybe_unused]] mat3_t axis;
		CG_GetEntitySpatilization( ent->number, so, velocity, axis );
		cg.soundSystem->startFixedSound( cgs.soundPrecache[parm], so, CHAN_AUTO, v_volumeEffects.get(), ATTN_STATIC );
	}
}

void CG_EntityEvent( entity_state_t *ent, int ev, int parm, bool predicted ) {
	if( ev == EV_NONE ) {
		return;
	}

	const ViewState *ourClientViewState = getOurClientViewState();
	if( ourClientViewState->isViewerEntity( ent->number ) ) {
		if( ev < PREDICTABLE_EVENTS_MAX && ( predicted != ourClientViewState->view.playerPrediction ) ) {
			return;
		}
	}

	switch( ev ) {
		//  PREDICTABLE EVENTS

		case EV_WEAPONACTIVATE: return handleWeaponActivateEvent( ent, parm, predicted );
		case EV_SMOOTHREFIREWEAPON: return handleSmoothRefireWeaponEvent( ent, parm, predicted );
		case EV_FIREWEAPON: return handleFireWeaponEvent( ent, parm, predicted );
		case EV_ELECTROTRAIL: return handleElectroTrailEvent( ent, parm, predicted );
		case EV_INSTATRAIL: return handleInstaTrailEvent( ent, parm, predicted );
		case EV_FIRE_RIOTGUN: return handleFireRiotgunEvent( ent, parm, predicted );
		case EV_FIRE_BULLET: return handleFireBulletEvent( ent, parm, predicted );
		case EV_NOAMMOCLICK: return handleNoAmmoClickEvent( ent, parm, predicted );
		case EV_DASH: return CG_Event_Dash( ent, parm );
		case EV_WALLJUMP: [[fallthrough]];
		case EV_WALLJUMP_FAILED: return CG_Event_WallJump( ent, parm, ev );
		case EV_DOUBLEJUMP: return CG_Event_DoubleJump( ent, parm );
		case EV_JUMP: return CG_Event_Jump( ent, parm );
		case EV_JUMP_PAD: return handleJumppadEvent( ent, predicted );
		case EV_FALL: return CG_Event_Fall( ent, parm );

			//  NON PREDICTABLE EVENTS

		case EV_WEAPONDROP: return CG_PModel_AddAnimation( ent->number, 0, TORSO_WEAPON_SWITCHOUT, 0, EVENT_CHANNEL );
		case EV_SEXEDSOUND: return handleSexedSoundEvent( ent, parm, predicted );
		case EV_PAIN: return CG_Event_Pain( ent, parm );
		case EV_DIE: return CG_Event_Die( ent, parm );
		case EV_GIB: return;
		case EV_EXPLOSION1: return cg.effectsSystem.spawnGenericExplosionEffect( ent->origin, FIRE_MODE_WEAK, parm * 8 );
		case EV_EXPLOSION2: return cg.effectsSystem.spawnGenericExplosionEffect( ent->origin, FIRE_MODE_STRONG, parm * 16 );
		case EV_GREEN_LASER: return;
		case EV_PNODE: return handlePnodeEvent( ent, parm, predicted );
		case EV_SPARKS: return handleSparksEvent( ent, parm, predicted );
		case EV_BULLET_SPARKS: return handleBulletSparksEvent( ent, parm, predicted );
		case EV_LASER_SPARKS: return;
		case EV_GESTURE: return playSexedSoundInPrimaryView( ent->number, SoundSystem::HeadAttachment, CHAN_BODY,
															 "*taunt", v_volumePlayers.get(), ent->attenuation );
		case EV_DROP: return CG_PModel_AddAnimation( ent->number, 0, TORSO_DROP, 0, EVENT_CHANNEL );
		case EV_SPOG: return CG_SmallPileOfGibs( ent->origin, parm, ent->origin2, ent->team );
		case EV_ITEM_RESPAWN: return handleItemRespawnEvent( ent, parm, predicted );
		case EV_PLAYER_RESPAWN: return handlePlayerRespawnEvent( ent, parm, predicted );
		case EV_PLAYER_TELEPORT_IN: return handlePlayerTeleportInEvent( ent, parm, predicted );
		case EV_PLAYER_TELEPORT_OUT: return handlePlayerTeleportOutEvent( ent, parm, predicted );
		case EV_PLASMA_EXPLOSION: return handlePlasmaExplosionEvent( ent, parm, predicted );
		case EV_BOLT_EXPLOSION: return handleBoltExplosionEvent( ent, parm, predicted );
		case EV_INSTA_EXPLOSION: return handleInstaExplosionEvent( ent, parm, predicted );
		case EV_GRENADE_EXPLOSION: return handleGrenadeExplosionEvent( ent, parm, predicted );
		case EV_ROCKET_EXPLOSION: return handleRocketExplosionEvent( ent, parm, predicted );
		case EV_WAVE_EXPLOSION: return handleShockwaveExplosionEvent( ent, parm, predicted );
		case EV_GRENADE_BOUNCE: return handleGrenadeBounceEvent( ent, parm );
		case EV_GRENADE_ARM: return handleGrenadeArmOrActivateEvent( ent );
		case EV_GRENADE_ACTIVATE: return handleGrenadeArmOrActivateEvent( ent );
		case EV_BLADE_IMPACT: return cg.effectsSystem.spawnGunbladeBladeHitEffect( ent->origin, ent->origin2, ent->ownerNum );
		case EV_GUNBLADEBLAST_IMPACT: return handleGunbladeBlastImpactEvent( ent, parm, predicted );
		case EV_BLOOD: return handleBloodEvent( ent, parm, predicted );

			// func movers

		case EV_PLAT_HIT_TOP: [[fallthrough]];
		case EV_PLAT_HIT_BOTTOM: [[fallthrough]];
		case EV_PLAT_START_MOVING: [[fallthrough]];
		case EV_DOOR_HIT_TOP: [[fallthrough]];
		case EV_DOOR_HIT_BOTTOM: [[fallthrough]];
		case EV_DOOR_START_MOVING: [[fallthrough]];
		case EV_BUTTON_FIRE: [[fallthrough]];
		case EV_TRAIN_STOP: [[fallthrough]];
		case EV_TRAIN_START: return handleMoverEvent( ent, parm );

		default: return;
	}
}

static void CG_FireEntityEvents( bool early ) {
	for( int pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		entity_state_t *state = &cg.frame.parsedEntities[pnum & ( MAX_PARSE_ENTITIES - 1 )];

		if( state->type == ET_SOUNDEVENT ) {
			if( early ) {
				CG_SoundEntityNewState( &cg_entities[state->number] );
			}
		} else {
			for( int j = 0; j < 2; j++ ) {
				if( early == ( state->events[j] == EV_WEAPONDROP ) ) {
					CG_EntityEvent( state, state->events[j], state->eventParms[j], false );
				}
			}
		}
	}
}

static void handlePlayerStateHitSoundEvent( unsigned event, unsigned parm, ViewState *playerViewState ) {
	const ViewState *primaryViewState = getPrimaryViewState();
	const bool allowPlayingSounds     = primaryViewState == playerViewState && primaryViewState->allowSounds;
	if( parm < 4 ) {
		// hit of some caliber
		if( allowPlayingSounds ) {
			cg.soundSystem->startLocalSound( cgs.media.sndWeaponHit[parm], v_volumeHitsound.get() );
			cg.soundSystem->startLocalSound( cgs.media.sndWeaponHit2[parm], v_volumeHitsound.get() );
		}
		playerViewState->crosshairDamageTimestamp = cg.time;
	} else if( parm == 4 ) {
		// killed an enemy
		if( allowPlayingSounds ) {
			cg.soundSystem->startLocalSound( cgs.media.sndWeaponKill, v_volumeHitsound.get() );
		}
		playerViewState->crosshairDamageTimestamp = cg.time;
	} else if( parm <= 6 ) {
		// hit a teammate
		if( allowPlayingSounds ) {
			cg.soundSystem->startLocalSound( cgs.media.sndWeaponHitTeam, v_volumeHitsound.get() );
		}
		if( v_showHelp.get() ) {
			if( random() <= 0.5f ) {
				CG_CenterPrint( playerViewState, "Don't shoot at members of your team!" );
			} else {
				CG_CenterPrint( playerViewState, "You are shooting at your team-mates!" );
			}
		}
	}
}

static void handlePlayerStatePickupEvent( unsigned event, unsigned parm, ViewState *playerViewState ) {
	if( v_pickupFlash.get() && !playerViewState->view.thirdperson ) {
		CG_StartColorBlendEffect( 1.0f, 1.0f, 1.0f, 0.25f, 150, playerViewState );
	}

	bool processAutoSwitch = false;
	const int autoSwitchVarValue = v_weaponAutoSwitch.get();
	if( autoSwitchVarValue && !cgs.demoPlaying && ( parm > WEAP_NONE && parm < WEAP_TOTAL ) ) {
		if( playerViewState == getOurClientViewState() ) {
			// If not chasing somebody
			if( playerViewState->predictedPlayerState.pmove.pm_type == PM_NORMAL && playerViewState->predictedPlayerState.POVnum == cgs.playerNum + 1 ) {
				if( !playerViewState->oldSnapPlayerState.inventory[parm] ) {
					processAutoSwitch = true;
				}
			}
		}
	}

	if( processAutoSwitch ) {
		// Auto-switch only works when the user didn't have the just-picked weapon
		if( autoSwitchVarValue == 2 ) {
			// Switch when player's only weapon is gunblade
			unsigned i;
			for( i = WEAP_GUNBLADE + 1; i < WEAP_TOTAL; i++ ) {
				if( i != parm && playerViewState->predictedPlayerState.inventory[i] ) {
					break;
				}
			}
			if( i == WEAP_TOTAL ) { // didn't have any weapon
				CG_UseItem( va( "%i", parm ) );
			}
		} else if( autoSwitchVarValue == 1 ) {
			// Switch when the new weapon improves player's selected weapon
			unsigned best = WEAP_GUNBLADE;
			for( unsigned i = WEAP_GUNBLADE + 1; i < WEAP_TOTAL; i++ ) {
				if( i != parm && playerViewState->predictedPlayerState.inventory[i] ) {
					best = i;
				}
			}
			if( best < parm ) {
				CG_UseItem( va( "%i", parm ) );
			}
		}
	}
}

/*
* CG_FirePlayerStateEvents
* This events are only received by this player state, and only affect it.
*/
static void CG_FirePlayerStateEvents( ViewState *playerViewState ) {
	if( playerViewState->view.POVent != (int)playerViewState->snapPlayerState.POVnum ) {
		return;
	}

	[[maybe_unused]] const ViewState *const primaryViewState = getPrimaryViewState();
	[[maybe_unused]] const bool allowPlayingSounds = primaryViewState == playerViewState && primaryViewState->allowSounds;

	vec3_t dir;
	for( unsigned count = 0; count < 2; count++ ) {
		// first byte is event number, second is parm
		const unsigned event = playerViewState->snapPlayerState.event[count] & 127;
		const unsigned parm  = playerViewState->snapPlayerState.eventParm[count] & 0xFF;

		switch( event ) {
			case PSEV_HIT:
				handlePlayerStateHitSoundEvent( event, parm, playerViewState );
				break;

			case PSEV_PICKUP:
				handlePlayerStatePickupEvent( event, parm, playerViewState );
				break;

			case PSEV_DAMAGE_20:
				ByteToDir( parm, dir );
				CG_DamageIndicatorAdd( 20, dir, playerViewState );
				break;

			case PSEV_DAMAGE_40:
				ByteToDir( parm, dir );
				CG_DamageIndicatorAdd( 40, dir, playerViewState );
				break;

			case PSEV_DAMAGE_60:
				ByteToDir( parm, dir );
				CG_DamageIndicatorAdd( 60, dir, playerViewState );
				break;

			case PSEV_DAMAGE_80:
				ByteToDir( parm, dir );
				CG_DamageIndicatorAdd( 80, dir, playerViewState );
				break;

			case PSEV_INDEXEDSOUND:
				if( const SoundSet *sound = cgs.soundPrecache[parm] ) {
					if( allowPlayingSounds ) {
						cg.soundSystem->startRelativeSound( sound, SoundSystem::OriginAttachment,
																	 playerViewState->view.POVent, CHAN_AUTO, v_volumeEffects.get(), ATTN_NORM );
					}
				}
				break;

			case PSEV_ANNOUNCER:
				if( allowPlayingSounds ) {
					CG_AddAnnouncerEvent( cgs.soundPrecache[parm], false );
				}
				break;

			case PSEV_ANNOUNCER_QUEUED:
				if( allowPlayingSounds ) {
					CG_AddAnnouncerEvent( cgs.soundPrecache[parm], true );
				}
				break;

			default:
				break;
		}
	}
}

void CG_FireEvents( bool early ) {
	if( !cg.fireEvents ) {
		return;
	}

	CG_FireEntityEvents( early );

	if( early ) {
		return;
	}

	for( unsigned viewIndex = 0; viewIndex < cg.numSnapViewStates; ++viewIndex ) {
		CG_FirePlayerStateEvents( &cg.viewStates[viewIndex] );
	}

	cg.fireEvents = false;
}

static void CG_UpdateEntities( void );

static bool CG_UpdateLinearProjectilePosition( centity_t *cent, ViewState *viewState ) {
	vec3_t origin;
	entity_state_t *state;
	int moveTime;
	int64_t serverTime;
#define MIN_DRAWDISTANCE_FIRSTPERSON 86
#define MIN_DRAWDISTANCE_THIRDPERSON 52

	state = &cent->current;

	if( !state->linearMovement ) {
		return -1;
	}

	if( GS_MatchPaused( *cggs ) ) {
		serverTime = cg.frame.serverTime;
	} else {
		serverTime = cg.time + cgs.extrapolationTime;
	}

	if( state->solid != SOLID_BMODEL ) {
		// add a time offset to counter antilag visualization
		if( !cgs.demoPlaying && v_projectileAntilagOffset.get() > 0.0f &&
			!viewState->isViewerEntity( state->ownerNum ) && ( cgs.playerNum + 1 != viewState->predictedPlayerState.POVnum ) ) {
			serverTime += state->modelindex2 * v_projectileAntilagOffset.get();
		}
	}

	moveTime = GS_LinearMovement( state, serverTime, origin );
	VectorCopy( origin, state->origin );

	if( ( moveTime < 0 ) && ( state->solid != SOLID_BMODEL ) ) {
		// when flyTime is negative don't offset it backwards more than PROJECTILE_PRESTEP value
		// FIXME: is this still valid?
		float maxBackOffset;

		if( viewState->isViewerEntity( state->ownerNum ) ) {
			maxBackOffset = ( PROJECTILE_PRESTEP - MIN_DRAWDISTANCE_FIRSTPERSON );
		} else {
			maxBackOffset = ( PROJECTILE_PRESTEP - MIN_DRAWDISTANCE_THIRDPERSON );
		}

		if( DistanceFast( state->origin2, state->origin ) > maxBackOffset ) {
			return false;
		}
	}

	return true;
#undef MIN_DRAWDISTANCE_FIRSTPERSON
#undef MIN_DRAWDISTANCE_THIRDPERSON
}

static void CG_NewPacketEntityState( entity_state_t *state ) {
	centity_t *cent;

	cent = &cg_entities[state->number];

	VectorClear( cent->prevVelocity );
	cent->canExtrapolatePrev = false;

	if( ISEVENTENTITY( state ) ) {
		cent->prev = cent->current;
		cent->current = *state;
		cent->serverFrame = cg.frame.serverFrame;

		VectorClear( cent->velocity );
		cent->canExtrapolate = false;
	} else if( state->linearMovement ) {
		if( cent->serverFrame != cg.oldFrame.serverFrame || state->teleported ||
			state->linearMovement != cent->current.linearMovement || state->linearMovementTimeStamp != cent->current.linearMovementTimeStamp ) {
			cent->prev = *state;
		} else {
			cent->prev = cent->current;
		}

		cent->current = *state;
		cent->serverFrame = cg.frame.serverFrame;

		VectorClear( cent->velocity );
		cent->canExtrapolate = false;

		// TODO: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		cent->linearProjectileCanDraw = CG_UpdateLinearProjectilePosition( cent, getPrimaryViewState() );

		VectorCopy( cent->current.linearMovementVelocity, cent->velocity );
	} else {
		// if it moved too much force the teleported bit
		if(  abs( (int)( cent->current.origin[0] - state->origin[0] ) ) > 512
			 || abs( (int)( cent->current.origin[1] - state->origin[1] ) ) > 512
			 || abs( (int)( cent->current.origin[2] - state->origin[2] ) ) > 512 ) {
			cent->serverFrame = -99;
		}

		// some data changes will force no lerping
		if( state->modelindex != cent->current.modelindex
			|| state->teleported
			|| state->linearMovement != cent->current.linearMovement ) {
			cent->serverFrame = -99;
		}

		if( cent->serverFrame != cg.oldFrame.serverFrame ) {
			// wasn't in last update, so initialize some things
			// duplicate the current state so lerping doesn't hurt anything
			cent->prev = *state;

			memset( cent->localEffects, 0, sizeof( cent->localEffects ) );
			cg.effectsSystem.resetEntityEffects( cent->current.number );

			// Init the animation when new into PVS
			if( cg.frame.valid && ( state->type == ET_PLAYER || state->type == ET_CORPSE ) ) {
				cent->lastAnims = 0;
				memset( cent->lastVelocities, 0, sizeof( cent->lastVelocities ) );
				memset( cent->lastVelocitiesFrames, 0, sizeof( cent->lastVelocitiesFrames ) );
				CG_PModel_ClearEventAnimations( state->number );
				memset( &cg_entPModels[state->number].animState, 0, sizeof( cg_entPModels[state->number].animState ) );

				// if it's a player and new in PVS, remove the old power time
				// This is way far from being the right thing. But will make it less bad for now
				cg_entPModels[state->number].flash_time = cg.time;
				cg_entPModels[state->number].barrel_time = cg.time;
			}
		} else {   // shuffle the last state to previous
			cent->prev = cent->current;
		}

		if( cent->serverFrame != cg.oldFrame.serverFrame ) {
			cent->microSmooth = 0;
		}

		cent->current = *state;
		VectorCopy( cent->velocity, cent->prevVelocity );

		//VectorCopy( cent->extrapolatedOrigin, cent->prevExtrapolatedOrigin );
		cent->canExtrapolatePrev = cent->canExtrapolate;
		cent->canExtrapolate = false;
		VectorClear( cent->velocity );
		cent->serverFrame = cg.frame.serverFrame;

		// set up velocities for this entity
		if( cgs.extrapolationTime &&
			( cent->current.type == ET_PLAYER || cent->current.type == ET_CORPSE ) ) {
			VectorCopy( cent->current.origin2, cent->velocity );
			VectorCopy( cent->prev.origin2, cent->prevVelocity );
			cent->canExtrapolate = cent->canExtrapolatePrev = true;
		} else if( !VectorCompare( cent->prev.origin, cent->current.origin ) ) {
			float snapTime = ( cg.frame.serverTime - cg.oldFrame.serverTime );

			if( !snapTime ) {
				snapTime = cgs.snapFrameTime;
			}

			VectorSubtract( cent->current.origin, cent->prev.origin, cent->velocity );
			VectorScale( cent->velocity, 1000.0f / snapTime, cent->velocity );
		}

		if( ( cent->current.type == ET_GENERIC || cent->current.type == ET_PLAYER
			  || cent->current.type == ET_GIB || cent->current.type == ET_GRENADE
			  || cent->current.type == ET_ITEM || cent->current.type == ET_CORPSE ) ) {
			cent->canExtrapolate = true;
		}

		if( ISBRUSHMODEL( cent->current.modelindex ) ) { // disable extrapolation on movers
			cent->canExtrapolate = false;
		}

		//if( cent->canExtrapolate )
		//	VectorMA( cent->current.origin, 0.001f * cgs.extrapolationTime, cent->velocity, cent->extrapolatedOrigin );
	}
}

// TODO: Should take teams/etc into account
// TODO: Share the code with the game module
// TODO: The old code used to make players with closest index preferred but that seems to be useless
std::optional<std::pair<unsigned, unsigned>> CG_FindMultiviewPovToChase() {
	for( unsigned viewStateIndex = 0; viewStateIndex < cg.numSnapViewStates; ++viewStateIndex ) {
		const ViewState &viewState = cg.viewStates[viewStateIndex];
		if( viewState.canBeAMultiviewChaseTarget() ) {
			return std::make_pair( viewStateIndex, viewState.predictedPlayerState.playerNum );
		}
	}
	return std::nullopt;
}

std::optional<unsigned> CG_FindChaseableViewportForPlayernum( unsigned playerNum ) {
	const ViewState *viewStateForPlayer = nullptr;
	for( unsigned i = 0; i < cg.numSnapViewStates; ++i ) {
		const ViewState *viewState = &cg.viewStates[i];
		if( viewState->snapPlayerState.playerNum == playerNum ) {
			viewStateForPlayer = viewState;
			break;
		}
	}
	if( viewStateForPlayer && viewStateForPlayer->canBeAMultiviewChaseTarget() ) {
		return (unsigned)( viewStateForPlayer - cg.viewStates );
	}
	return std::nullopt;
}

static void CG_SetFramePlayerState( player_state_t *playerState, snapshot_t *frame, int index, bool forcePrediction ) {
	*playerState = frame->playerStates[index];
	if( cgs.demoPlaying || ( cg.frame.multipov && !forcePrediction ) ) {
		playerState->pmove.pm_flags |= PMF_NO_PREDICTION;
		if( playerState->pmove.pm_type != PM_SPECTATOR ) {
			playerState->pmove.pm_type = PM_CHASECAM;
		}
	}
}

struct TileLayoutSpecs {
	unsigned numRows;
	unsigned numColumns;
	int cellWidth;
	int cellHeight;
};

[[nodiscard]]
static auto findBestLayoutForTiles( int fieldWidth, int fieldHeight, unsigned numTargetViews,
									int horizontalSpacing, int verticalSpacing,
									std::span<const float> allowedAspectRatio ) -> TileLayoutSpecs {
	assert( numTargetViews > 0 && numTargetViews <= MAX_CLIENTS );
	// TODO: Put a restriction on the aspect ratio as well?
	assert( !allowedAspectRatio.empty() );

	unsigned chosenNumRows    = 0;
	unsigned chosenNumColumns = 0;
	int chosenCellWidth       = 0;
	int chosenCellHeight      = 0;

	float bestScore = 0.0f;
	for( unsigned numRows = 1; numRows <= numTargetViews; ++numRows ) {
		const int initialCellHeight = ( fieldHeight - (int)( numRows - 1 ) * verticalSpacing ) / (int)numRows;
		for( unsigned numColumns = 1; numColumns <= numTargetViews; ++numColumns ) {
			// Make sure all views may theoretically fit, and also there won't be empty rows
			if( numRows * numColumns >= numTargetViews && numRows * numColumns < numTargetViews + numColumns ) {
				const int initialCellWidth = ( fieldWidth - (int)( numColumns - 1 ) * horizontalSpacing ) / (int)numColumns;
				for( unsigned fixedDimensionStep = 0; fixedDimensionStep < 2; ++fixedDimensionStep ) {
					for( const float ratio: allowedAspectRatio ) {
						int cellWidthToTest  = -1;
						int cellHeightToTest = -1;
						if( fixedDimensionStep == 0 ) {
							// Alternate width for the fixed height
							const int cellWidthForRatio = (int)std::round( (float)initialCellHeight * ratio );
							if( initialCellWidth >= cellWidthForRatio ) {
								cellWidthToTest  = cellWidthForRatio;
								cellHeightToTest = initialCellHeight;
							} else {
								// Check whether the increased width still fits
								if( (int)( cellWidthForRatio * numColumns + horizontalSpacing * ( numColumns - 1 ) ) <= fieldWidth ) {
									cellWidthToTest  = cellWidthForRatio;
									cellHeightToTest = initialCellHeight;
								}
							}
						} else {
							// Alternate height for the fixed width
							const int cellHeightForRatio = (int)std::round( (float)initialCellWidth / ratio );
							if( initialCellHeight >= cellHeightForRatio ) {
								cellWidthToTest  = initialCellWidth;
								cellHeightToTest = cellHeightForRatio;
							} else {
								// Check whether the increased height still fits
								if( (int)( cellHeightForRatio * numRows + verticalSpacing * ( numRows - 1 ) ) <= fieldHeight ) {
									cellWidthToTest  = initialCellWidth;
									cellHeightToTest = initialCellHeight;
								}
							}
						}
						if( cellWidthToTest > cellHeightToTest ) {
							float score = (float)cellWidthToTest * (float)cellHeightToTest;
							if( numColumns > numRows ) {
								if( fieldWidth <= fieldHeight ) {
									score *= 0.9f;
								}
							} else if( numColumns < numRows ) {
								if( fieldWidth >= fieldHeight ) {
									score *= 0.9f;
								}
							} else {
								if( fieldWidth != fieldHeight ) {
									score *= 0.9f;
								}
							}
							if( bestScore < score ) {
								bestScore        = score;
								chosenNumRows    = numRows;
								chosenNumColumns = numColumns;
								chosenCellWidth  = cellWidthToTest;
								chosenCellHeight = cellHeightToTest;
							}
						}
					}
				}
			}
		}
	}

	assert( bestScore > 0.0f );

	return TileLayoutSpecs {
		.numRows = chosenNumRows, .numColumns = chosenNumColumns, .cellWidth = chosenCellWidth, .cellHeight = chosenCellHeight,
	};
}

static void prepareMiniviewTiles( const TileLayoutSpecs &layoutSpecs, const Rect &fieldRect,
								  unsigned numTargetViews, int horizontalSpacing, int verticalSpacing,
								  wsw::StaticVector<Rect, MAX_CLIENTS> *targetViewRects ) {
	const auto actualGridWidth  = (int)( layoutSpecs.cellWidth * layoutSpecs.numColumns + horizontalSpacing * ( layoutSpecs.numColumns - 1 ) );
	const auto actualGridHeight = (int)( layoutSpecs.cellHeight * layoutSpecs.numRows + verticalSpacing * ( layoutSpecs.numRows - 1 ) );

	int y           = fieldRect.y + ( fieldRect.height - actualGridHeight ) / 2;
	const int baseX = fieldRect.x + ( fieldRect.width - actualGridWidth ) / 2;

	unsigned numFilledRects = 0;
	for( unsigned rowNum = 0; rowNum < layoutSpecs.numRows && numFilledRects < numTargetViews; ++rowNum ) {
		int x = baseX;
		// Center cells of incomplete last row
		if( numFilledRects + layoutSpecs.numColumns > numTargetViews ) {
			const unsigned numEmptyCells = numFilledRects + layoutSpecs.numColumns - numTargetViews;
			assert( numEmptyCells > 0 && numEmptyCells < layoutSpecs.numColumns );
			const int extraWidth = (int)numEmptyCells * ( layoutSpecs.cellWidth + horizontalSpacing );
			x += extraWidth / 2;
		}
		for( unsigned columnNum = 0; columnNum < layoutSpecs.numColumns && numFilledRects < numTargetViews; ++columnNum ) {
			targetViewRects->emplace_back( Rect {
				.x      = x,
				.y      = y,
				.width  = layoutSpecs.cellWidth,
				.height = layoutSpecs.cellHeight,
			});
			numFilledRects++;
			x += layoutSpecs.cellWidth + horizontalSpacing;
		}
		y += layoutSpecs.cellHeight + verticalSpacing;
	}

	assert( numFilledRects == numTargetViews );
}

static void CG_UpdatePlayerState() {
	const uint32_t oldSnapViewStatePresentMask = cg.snapViewStatePresentMask;

	cg.snapViewStatePresentMask = 0;
	cg.numSnapViewStates        = 0;
	cg.ourClientViewportIndex   = ~0u;

	for( int playerIndex = 0; playerIndex < cg.frame.numplayers; ++playerIndex ) {
		ViewState *const viewState             = &cg.viewStates[playerIndex];
		const player_state_t *framePlayerState = &cg.frame.playerStates[playerIndex];

		bool forcePrediction = false;
		if( framePlayerState->playerNum == cgs.playerNum ) {
			cg.ourClientViewportIndex = playerIndex;
			if( !cgs.demoPlaying ) {
				forcePrediction = true;
			}
		}

		CG_SetFramePlayerState( &viewState->snapPlayerState, &cg.frame, playerIndex, forcePrediction );

		int oldSnapIndex = -1;
		// If it was present in the previous snapshot
		if( oldSnapViewStatePresentMask & ( 1u << framePlayerState->playerNum ) ) {
			for( int indexInOldFrame = 0; indexInOldFrame < cg.oldFrame.numplayers; ++indexInOldFrame ) {
				if( cg.oldFrame.playerStates[indexInOldFrame].playerNum == framePlayerState->playerNum ) {
					oldSnapIndex = indexInOldFrame;
					break;
				}
			}
			assert( oldSnapIndex >= 0 );
		}
		if( oldSnapIndex >= 0 ) {
			CG_SetFramePlayerState( &viewState->oldSnapPlayerState, &cg.oldFrame, oldSnapIndex, forcePrediction );
		} else {
			viewState->oldSnapPlayerState = viewState->snapPlayerState;
		}

		// Predict from the snap state
		viewState->predictedPlayerState = viewState->snapPlayerState;
		viewState->allowSounds          = false;

		cg.snapViewStatePresentMask |= ( 1u << framePlayerState->playerNum );
		cg.numSnapViewStates++;
	}

	//assert( cg.ourClientViewportIndex < cg.numSnapViewStates );
	// TODO: Can we do something better?
	unsigned fallbackPlayerNum = cgs.playerNum;
	if( cg.ourClientViewportIndex >= cg.numSnapViewStates ) {
		assert( cgs.demoPlaying );
		if( cg.frame.multipov ) {
			std::memset( &cg.viewStates[MAX_CLIENTS], 0, sizeof( ViewState ) );
			cg.ourClientViewportIndex = MAX_CLIENTS;
			// TODO: There should be default initializers for that
			cg.viewStates[MAX_CLIENTS].predictedPlayerState.pmove.pm_type   = PM_SPECTATOR;
			cg.viewStates[MAX_CLIENTS].predictFromPlayerState.pmove.pm_type = PM_SPECTATOR;
		} else {
			cg.ourClientViewportIndex = 0;
			fallbackPlayerNum         = 0;
		}
	}

	const auto ourActualMoveType = cg.viewStates[cg.ourClientViewportIndex].predictedPlayerState.pmove.pm_type;
	// Force view from our in-game client
	if( ourActualMoveType != PM_SPECTATOR && ourActualMoveType != PM_CHASECAM ) {
		cg.chasedViewportIndex = cg.ourClientViewportIndex;
		cg.chasedPlayerNum     = cgs.playerNum;
		cg.chaseMode           = CAM_INEYES;
	} else {
		const unsigned requestedPlayerNum = cg.pendingChasedPlayerNum.value_or( cg.chasedPlayerNum );
		// Check whether we have lost the pov.
		// Update the viewport index for pov in case if it's changed.
		if( const std::optional<unsigned> maybeIndex = CG_FindChaseableViewportForPlayernum( requestedPlayerNum ) ) {
			cg.chasedViewportIndex = *maybeIndex;
			cg.chasedPlayerNum     = requestedPlayerNum;
		} else if( const std::optional<std::pair<unsigned, unsigned>> maybePlayerNumAndIndex = CG_FindMultiviewPovToChase() ) {
			cg.chasedViewportIndex = maybePlayerNumAndIndex->first;
			cg.chasedPlayerNum     = maybePlayerNumAndIndex->second;
		} else {
			cg.chasedViewportIndex = cg.ourClientViewportIndex;
			cg.chasedPlayerNum     = fallbackPlayerNum;
		}
	}

	assert( cg.chasedViewportIndex < cg.numSnapViewStates || ( cgs.demoPlaying && cg.chasedViewportIndex == MAX_CLIENTS ) );
	assert( cg.chasedPlayerNum < MAX_CLIENTS || cgs.demoPlaying );

	if( cg.hasPendingSwitchFromTiledMode ) {
		if( cg.chaseMode == CAM_TILED ) {
			if( CG_SwitchChaseCamMode() ) {
				wsw::ui::UISystem::instance()->playForwardSound();
			}
		}
		cg.hasPendingSwitchFromTiledMode = false;
	}

	if( cg.chaseMode != CAM_TILED ) {
		cg.viewStates[cg.chasedViewportIndex].allowSounds = true;
	}

	cg.pendingChasedPlayerNum = std::nullopt;

	cg.hudControlledMiniviewViewStateIndicesForPane[0].clear();
	cg.hudControlledMiniviewViewStateIndicesForPane[1].clear();
	cg.tileMiniviewViewStateIndices.clear();
	cg.tileMiniviewPositions.clear();

	unsigned numMiniviewViewStates  = 0;
	const unsigned limitOfMiniviews = wsw::ui::UISystem::instance()->retrieveLimitOfMiniviews();

	const unsigned numPanes = wsw::ui::UISystem::instance()->retrieveNumberOfHudMiniviewPanes();
	assert( numPanes >= 0 && numPanes <= 2 );

	const bool hasTwoTeams = GS_TeamBasedGametype( *cggs ) && !GS_IndividualGametype( *cggs );
	const auto ourTeam     = cg.viewStates[cg.ourClientViewportIndex].predictedPlayerState.stats[STAT_REALTEAM];

	wsw::StaticVector<uint8_t, MAX_CLIENTS> tmpIndicesForTwoTilePanes[2];

	// Disallow anything for our client in PLAYERS team. See the "Pruning by team" remark below.
	if( ourTeam != TEAM_PLAYERS ) {
		for( unsigned viewStateIndex = 0; viewStateIndex < cg.numSnapViewStates; ++viewStateIndex ) {
			bool isViewStateAcceptable = false;
			if( viewStateIndex != cg.ourClientViewportIndex ) {
				// If we're in freecam/tiled view state, the primary pov is put to miniviews
				if( viewStateIndex != cg.chasedViewportIndex || ( cg.isDemoCamFree || cg.chaseMode == CAM_TILED ) ) {
					isViewStateAcceptable = true;
				}
			}
			if( isViewStateAcceptable ) {
				// TODO: Should we exclude view states that should not be shown from cg.viewStates[]?
				// Seemingly no, as we already have to include view state for our client, regardless of its in-game activity.
				const player_state_t &playerState = cg.viewStates[viewStateIndex].predictedPlayerState;
				bool isPlayerStateAcceptable      = false;
				if( const auto playerTeam = playerState.stats[STAT_REALTEAM]; playerTeam != TEAM_SPECTATOR ) {
					if( playerState.POVnum == playerState.playerNum + 1 ) {
						// Pruning by team could be redundant, but it adds a protection against malicious servers
						if( !hasTwoTeams ) {
							isPlayerStateAcceptable = true;
						} else {
							if( ourTeam == playerTeam || ourTeam == TEAM_SPECTATOR ) {
								isPlayerStateAcceptable = true;
							}
						}
					}
				}
				if( isPlayerStateAcceptable ) {
					if( cg.chaseMode != CAM_TILED ) {
						wsw::StaticVector<uint8_t, MAX_CLIENTS> *indices = cg.hudControlledMiniviewViewStateIndicesForPane;
						if( numPanes == 2 ) {
							unsigned chosenPaneIndex;
							if( hasTwoTeams && ourTeam == TEAM_SPECTATOR ) {
								chosenPaneIndex = ( playerState.stats[STAT_REALTEAM] == TEAM_ALPHA ) ? 0 : 1;
							} else {
								chosenPaneIndex = indices[0].size() < indices[1].size() ? 0 : 1;
							}
							indices[chosenPaneIndex].push_back( (uint8_t)viewStateIndex );
						} else if( numPanes == 1 ) {
							indices[0].push_back( (uint8_t)viewStateIndex );
						}
					} else {
						if( hasTwoTeams && ourTeam == TEAM_SPECTATOR ) {
							const unsigned chosenPaneIndex = playerState.stats[STAT_REALTEAM] == TEAM_ALPHA ? 0 : 1;
							tmpIndicesForTwoTilePanes[chosenPaneIndex].push_back( (uint8_t)viewStateIndex );
						} else {
							tmpIndicesForTwoTilePanes[0].push_back( (uint8_t)viewStateIndex );
						}
					}
					numMiniviewViewStates++;
					if( numMiniviewViewStates == limitOfMiniviews ) {
						break;
					}
				}
			}
		}
	}

	// Don't even try making tile view indices unless there are at least two views
	if( tmpIndicesForTwoTilePanes[0].size() + tmpIndicesForTwoTilePanes[1].size() > 1 ) {
		wsw::StaticVector<float, 8> allowedAspectRatio;
		const auto primaryRatio     = (float)cgs.vidWidth / (float)cgs.vidHeight;
		bool addedThePrimaryRatio   = false;
		// Disallow everything that is lower than 3/2 on wide screens (otherwise it looks ugly)
		const float minAllowedRatio = wsw::min( primaryRatio, 3.0f / 2.0f );
		for( const float ratio: { 5.0f / 4.0f, 3.0f / 4.0f, 3.0f / 2.0f, 16.0f / 10.0f } ) {
			if( ratio >= minAllowedRatio ) {
				allowedAspectRatio.push_back( ratio );
				if( ratio == primaryRatio ) {
					addedThePrimaryRatio = true;
				}
			}
		}
		if( !addedThePrimaryRatio ) {
			allowedAspectRatio.push_back( primaryRatio );
		}

		const int horizontalSpacing = 24 * cgs.pixelRatio;
		const int verticalSpacing   = 24 * cgs.pixelRatio;

		const int topMargin      = cgs.vidHeight / 4;
		const int bottomMargin   = cgs.vidHeight / 8;
		const int fieldHeight    = cgs.vidHeight - topMargin - bottomMargin;
		const int baseFieldWidth = ( 5 * cgs.vidWidth ) / 6;

		if( hasTwoTeams ) {
			const int fieldWidth = baseFieldWidth / 2;

			unsigned useLayoutOfPane;
			if( tmpIndicesForTwoTilePanes[0].size() < tmpIndicesForTwoTilePanes[1].size() ) {
				useLayoutOfPane = 1;
			} else {
				useLayoutOfPane = 0;
			}

			// Generally, use the same tile specs for both panes
			const TileLayoutSpecs &sharedSpecs = findBestLayoutForTiles( fieldWidth, fieldHeight,
																		 tmpIndicesForTwoTilePanes[useLayoutOfPane].size(),
																		 horizontalSpacing, verticalSpacing,
																		 allowedAspectRatio );

			const int spaceBetweenPanes = ( cgs.vidWidth - 2 * fieldWidth ) / 2;
			int fieldX                  = ( cgs.vidWidth - spaceBetweenPanes - 2 * fieldWidth ) / 2;
			const int fieldY            = topMargin;

			for( unsigned paneIndex = 0; paneIndex < 2; ++paneIndex ) {
				if( const auto &indices = tmpIndicesForTwoTilePanes[paneIndex]; !indices.empty() ) {
					bool didSpecificHandling = false;
					// If we have a single tile left in this pane and the other pane has multiple tiles
					if( indices.size() == 1 && tmpIndicesForTwoTilePanes[(paneIndex + 1) % 2].size() > 1 ) {
						// Select new array of allowed aspect ratio for a single huge tile.
						// Disallow ratio values which are lesser than the shared one.
						const auto sharedAspectRatio = (float)sharedSpecs.cellWidth / (float)sharedSpecs.cellHeight;
						wsw::StaticVector<float, 8> singleTileAllowedAspectRatio;
						assert( !allowedAspectRatio.empty() );
						bool hasAddedTheActualRatio = false;
						for( float ratio: allowedAspectRatio ) {
							if( ratio >= sharedAspectRatio ) {
								singleTileAllowedAspectRatio.push_back( ratio );
								if( ratio == sharedAspectRatio ) {
									hasAddedTheActualRatio = true;
								}
							}
						}
						// Protect from various numeric precision issues
						if( !hasAddedTheActualRatio ) {
							singleTileAllowedAspectRatio.push_back( sharedAspectRatio );
						}
						// Don't let it be taller than the opposite pane
						const auto otherPaneActualGridHeight  = (int)( sharedSpecs.cellHeight * sharedSpecs.numRows +
							verticalSpacing * ( sharedSpecs.numRows - 1 ) );
						// The exception to use of sharedSpecs
						const TileLayoutSpecs &singleTileSpecs = findBestLayoutForTiles( fieldWidth, otherPaneActualGridHeight,
																						 1,
																						 horizontalSpacing, verticalSpacing,
																						 singleTileAllowedAspectRatio );
						// Align it to the top of the other pane
						cg.tileMiniviewPositions.emplace_back( Rect {
							.x      = fieldX + ( fieldWidth - singleTileSpecs.cellWidth ) / 2,
							.y      = fieldY + ( fieldHeight - otherPaneActualGridHeight ) / 2,
							.width  = singleTileSpecs.cellWidth,
							.height = singleTileSpecs.cellHeight,
						});
						didSpecificHandling = true;
					}
					const Rect fieldRect { .x = fieldX, .y = fieldY, .width  = fieldWidth, .height = fieldHeight };
					if( !didSpecificHandling ) {
						prepareMiniviewTiles( sharedSpecs, fieldRect, indices.size(),
											  horizontalSpacing, verticalSpacing, &cg.tileMiniviewPositions );
					}
					cg.tileMiniviewViewStateIndices.insert( cg.tileMiniviewViewStateIndices.end(), indices.begin(), indices.end() );
				}
				fieldX += fieldWidth + spaceBetweenPanes;
			}
		} else {
			assert( !tmpIndicesForTwoTilePanes[0].empty() && tmpIndicesForTwoTilePanes[1].empty() );
			const auto &indices = tmpIndicesForTwoTilePanes[0];

			const int fieldWidth = baseFieldWidth;
			const Rect fieldRect {
				.x      = ( cgs.vidWidth - fieldWidth ) / 2,
				.y      = topMargin,
				.width  = fieldWidth,
				.height = fieldHeight,
			};

			const TileLayoutSpecs &layoutSpecs = findBestLayoutForTiles( fieldWidth, fieldHeight, indices.size(),
																		 horizontalSpacing, verticalSpacing,
																		 allowedAspectRatio );
			prepareMiniviewTiles( layoutSpecs, fieldRect, indices.size(),
								  horizontalSpacing, verticalSpacing, &cg.tileMiniviewPositions );
			cg.tileMiniviewViewStateIndices.insert(
				cg.tileMiniviewViewStateIndices.end(), indices.begin(), indices.end() );
		}
	}
}

bool CG_NewFrameSnap( snapshot_t *frame, snapshot_t *lerpframe ) {
	assert( frame );

	if( lerpframe ) {
		cg.oldFrame = *lerpframe;
	} else {
		cg.oldFrame = *frame;
	}

	cg.frame = *frame;
	cggs->gameState = frame->gameState;

	CG_UpdatePlayerState();

	auto *const uiSystem = wsw::ui::UISystem::instance();
	// Must be performed immediately if there were POV property updates
	// TODO: Don't call it twice per frame in this case
	uiSystem->refreshProperties();

	static_assert( AccuracyRows::Span::extent == kNumAccuracySlots );
	uiSystem->updateScoreboard( frame->scoreboardData, AccuracyRows {
		.weak   = AccuracyRows::Span( getPrimaryViewState()->snapPlayerState.weakAccuracy ),
		.strong = AccuracyRows::Span( getPrimaryViewState()->snapPlayerState.strongAccuracy ),
	});

	for( int i = 0; i < frame->numEntities; i++ ) {
		CG_NewPacketEntityState( &frame->parsedEntities[i & ( MAX_PARSE_ENTITIES - 1 )] );
	}

	if( lerpframe && ( memcmp( cg.oldFrame.areabits, cg.frame.areabits, cg.frame.areabytes ) == 0 ) ) {
		cg.oldAreabits = true;
	} else {
		cg.oldAreabits = false;
	}

	if( !cgs.precacheDone || !cg.frame.valid ) {
		return false;
	}

	//cg.specStateChanged = SPECSTATECHANGED() || lerpframe == NULL || cg.firstFrame;

	// a new server frame begins now

	CG_BuildSolidList();
	CG_UpdateEntities();
	CG_CheckPredictionError();

	// force the prediction to be restarted from the new snapshot
	getOurClientViewState()->predictFrom = 0;
	cg.fireEvents = true;

	for( int commandIndex = 0; commandIndex < cg.frame.numgamecommands; commandIndex++ ) {
		const auto &gameCommand = cg.frame.gamecommands[commandIndex];
		const wsw::StringView cmdView( cg.frame.gamecommandsData + gameCommand.commandOffset );
		if( gameCommand.all ) {
			// Dispatch to each target
			for( unsigned viewStateIndex = 0; viewStateIndex < cg.numSnapViewStates; ++viewStateIndex ) {
				CG_GameCommand( cg.viewStates + viewStateIndex, cmdView );
			}
		} else {
			wsw::StaticVector<unsigned, MAX_CLIENTS> targetViewStateIndices;
			for( unsigned viewStateIndex = 0; viewStateIndex < cg.numSnapViewStates; ++viewStateIndex ) {
				const int playerBitIndex = (int)cg.viewStates[viewStateIndex].snapPlayerState.playerNum;
				if( gameCommand.targets[playerBitIndex >> 3] & ( 1 << ( playerBitIndex & 7 ) ) ) {
					CG_GameCommand( cg.viewStates + viewStateIndex, cmdView );
				}
			}
		}
	}

	CG_FireEvents( true );

	if( cg.firstFrame && !cgs.demoPlaying ) {
		// request updates on our private state
		CL_Cmd_ExecuteNow( "upstate" );
	}

	cg.firstFrame = false; // not the first frame anymore
	return true;
}

const cmodel_s *CG_CModelForEntity( int entNum ) {
	int x, zd, zu;
	centity_t *cent;
	const cmodel_s *cmodel = NULL;
	vec3_t bmins, bmaxs;

	if( entNum < 0 || entNum >= MAX_EDICTS ) {
		return NULL;
	}

	cent = &cg_entities[entNum];

	if( cent->serverFrame != cg.frame.serverFrame ) { // not present in current frame
		return NULL;
	}

	// find the cmodel
	if( cent->current.solid == SOLID_BMODEL ) { // special value for bmodel
		cmodel = CG_InlineModel( cent->current.modelindex );
	} else if( cent->current.solid ) {   // encoded bbox
		x = 8 * ( cent->current.solid & 31 );
		zd = 8 * ( ( cent->current.solid >> 5 ) & 31 );
		zu = 8 * ( ( cent->current.solid >> 10 ) & 63 ) - 32;

		bmins[0] = bmins[1] = -x;
		bmaxs[0] = bmaxs[1] = x;
		bmins[2] = -zd;
		bmaxs[2] = zu;
		if( cent->type == ET_PLAYER || cent->type == ET_CORPSE ) {
			cmodel = CG_OctagonModelForBBox( bmins, bmaxs );
		} else {
			cmodel = CG_ModelForBBox( bmins, bmaxs );
		}
	}

	return cmodel;
}

void CG_DrawEntityBox( centity_t *cent ) {
}

static void CG_EntAddBobEffect( centity_t *cent ) {
	double scale;
	double bob;

	scale = 0.005 + cent->current.number * 0.00001;
	bob = 4 + cos( ( cg.time + 1000 ) * scale ) * 4;

	cent->ent.origin2[2] += bob;
	cent->ent.origin[2] += bob;
	cent->ent.lightingOrigin[2] += bob;
}

static void CG_EntAddTeamColorTransitionEffect( centity_t *cent ) {
	float f;
	uint8_t *currentcolor;
	vec4_t scaledcolor, newcolor;
	const vec4_t neutralcolor = { 1.0f, 1.0f, 1.0f, 1.0f };

	f = (float)cent->current.counterNum / 255.0f;
	Q_clamp( f, 0.0f, 1.0f );

	if( cent->current.type == ET_PLAYER || cent->current.type == ET_CORPSE ) {
		currentcolor = CG_PlayerColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	} else {
		currentcolor = CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	}

	Vector4Scale( currentcolor, 1.0 / 255.0, scaledcolor );
	VectorLerp( neutralcolor, f, scaledcolor, newcolor );

	cent->ent.shaderRGBA[0] = (uint8_t)( newcolor[0] * 255 );
	cent->ent.shaderRGBA[1] = (uint8_t)( newcolor[1] * 255 );
	cent->ent.shaderRGBA[2] = (uint8_t)( newcolor[2] * 255 );
}

static void CG_AddLinkedModel( centity_t *cent, DrawSceneRequest *drawSceneRequest, const ViewState *drawFromViewState ) {
	static entity_t ent;
	orientation_t tag;
	struct model_s *model;

	// linear projectiles can never have a linked model. Modelindex2 is used for a different purpose
	if( cent->current.linearMovement ) {
		return;
	}

	model = cgs.modelDraw[cent->current.modelindex2];
	if( !model ) {
		return;
	}

	memset( &ent, 0, sizeof( entity_t ) );
	ent.rtype = RT_MODEL;
	ent.scale = cent->ent.scale;
	ent.renderfx = cent->ent.renderfx;
	ent.shaderTime = cent->ent.shaderTime;
	Vector4Copy( cent->ent.shaderRGBA, ent.shaderRGBA );
	ent.model = model;
	ent.customShader = NULL;
	ent.customSkin = NULL;
	VectorCopy( cent->ent.origin, ent.origin );
	VectorCopy( cent->ent.origin, ent.origin2 );
	VectorCopy( cent->ent.lightingOrigin, ent.lightingOrigin );
	Matrix3_Copy( cent->ent.axis, ent.axis );

	if( cent->item && ( cent->effects & EF_AMMOBOX ) ) { // ammobox icon hack
		ent.customShader = R_RegisterPic( cent->item->icon );
	}

	if( cent->item && ( cent->item->type & IT_WEAPON ) ) {
		if( CG_GrabTag( &tag, &cent->ent, "tag_barrel" ) ) {
			CG_PlaceModelOnTag( &ent, &cent->ent, &tag );
		}
	} else {
		if( CG_GrabTag( &tag, &cent->ent, "tag_linked" ) ) {
			CG_PlaceModelOnTag( &ent, &cent->ent, &tag );
		}
	}

	CG_AddColoredOutLineEffect( &ent, cent->effects,
								cent->outlineColor[0], cent->outlineColor[1], cent->outlineColor[2], cent->outlineColor[3], drawFromViewState );
	CG_AddEntityToScene( &ent, drawSceneRequest );
	CG_AddShellEffects( &ent, cent->effects, drawSceneRequest );
}

void CG_AddCentityOutLineEffect( centity_t *cent, const ViewState *drawFromViewState ) {
	CG_AddColoredOutLineEffect( &cent->ent, cent->effects, cent->outlineColor[0], cent->outlineColor[1], cent->outlineColor[2], cent->outlineColor[3], drawFromViewState );
}

static void CG_UpdateGenericEnt( centity_t *cent ) {
	int modelindex;

	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	cent->ent.scale = 1.0f;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	if( cent->effects & EF_OUTLINE ) {
		Vector4Set( cent->outlineColor, 0, 0, 0, 255 );
	}

	// set frame
	cent->ent.frame = cent->current.frame;
	cent->ent.oldframe = cent->prev.frame;

	// set up the model
	cent->ent.rtype = RT_MODEL;

	modelindex = cent->current.modelindex;
	if( modelindex > 0 && modelindex < MAX_MODELS ) {
		cent->ent.model = cgs.modelDraw[modelindex];
	}

	cent->skel = CG_SkeletonForModel( cent->ent.model );
}

void CG_ExtrapolateLinearProjectile( centity_t *cent ) {
	int i;

	// TODO: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	cent->linearProjectileCanDraw = CG_UpdateLinearProjectilePosition( cent, getPrimaryViewState() );

	cent->ent.backlerp = 1.0f;

	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->ent.origin2[i] = cent->ent.lightingOrigin[i] = cent->current.origin[i];

	AnglesToAxis( cent->current.angles, cent->ent.axis );
}

void CG_LerpGenericEnt( centity_t *cent, ViewState *drawFromViewState ) {
	int i;
	vec3_t ent_angles = { 0, 0, 0 };

	cent->ent.backlerp = 1.0f - cg.lerpfrac;

	if( drawFromViewState->isViewerEntity( cent->current.number ) || drawFromViewState->view.POVent == cent->current.number ) {
		VectorCopy( drawFromViewState->predictedPlayerState.viewangles, ent_angles );
	} else {
		// interpolate angles
		for( i = 0; i < 3; i++ )
			ent_angles[i] = LerpAngle( cent->prev.angles[i], cent->current.angles[i], cg.lerpfrac );
	}

	if( ent_angles[0] || ent_angles[1] || ent_angles[2] ) {
		AnglesToAxis( ent_angles, cent->ent.axis );
	} else {
		Matrix3_Copy( axis_identity, cent->ent.axis );
	}

	if( cent->renderfx & RF_FRAMELERP ) {
		// step origin discretely, because the frames
		// do the animation properly
		vec3_t delta, move;

		// FIXME: does this still work?
		VectorSubtract( cent->current.origin2, cent->current.origin, move );
		Matrix3_TransformVector( cent->ent.axis, move, delta );
		VectorMA( cent->current.origin, cent->ent.backlerp, delta, cent->ent.origin );
	} else if( drawFromViewState->isViewerEntity( cent->current.number ) || drawFromViewState->view.POVent == cent->current.number ) {
		VectorCopy( drawFromViewState->predictedPlayerState.pmove.origin, cent->ent.origin );
		VectorCopy( cent->ent.origin, cent->ent.origin2 );
	} else {
		if( cgs.extrapolationTime && cent->canExtrapolate ) { // extrapolation
			vec3_t origin, xorigin1, xorigin2;

			float lerpfrac = cg.lerpfrac;
			Q_clamp( lerpfrac, 0.0f, 1.0f );

			// extrapolation with half-snapshot smoothing
			if( cg.xerpTime >= 0 || !cent->canExtrapolatePrev ) {
				VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin1 );
			} else {
				VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin1 );
				if( cent->canExtrapolatePrev ) {
					vec3_t oldPosition;

					VectorMA( cent->prev.origin, cg.oldXerpTime, cent->prevVelocity, oldPosition );
					VectorLerp( oldPosition, cg.xerpSmoothFrac, xorigin1, xorigin1 );
				}
			}


			// extrapolation with full-snapshot smoothing
			VectorMA( cent->current.origin, cg.xerpTime, cent->velocity, xorigin2 );
			if( cent->canExtrapolatePrev ) {
				vec3_t oldPosition;

				VectorMA( cent->prev.origin, cg.oldXerpTime, cent->prevVelocity, oldPosition );
				VectorLerp( oldPosition, lerpfrac, xorigin2, xorigin2 );
			}

			VectorLerp( xorigin1, 0.5f, xorigin2, origin );

			if( cent->microSmooth == 2 ) {
				vec3_t oldsmoothorigin;

				VectorLerp( cent->microSmoothOrigin2, 0.65f, cent->microSmoothOrigin, oldsmoothorigin );
				VectorLerp( origin, 0.5f, oldsmoothorigin, cent->ent.origin );
			} else if( cent->microSmooth == 1 ) {
				VectorLerp( origin, 0.5f, cent->microSmoothOrigin, cent->ent.origin );
			} else {
				VectorCopy( origin, cent->ent.origin );
			}

			if( cent->microSmooth ) {
				VectorCopy( cent->microSmoothOrigin, cent->microSmoothOrigin2 );
			}

			VectorCopy( origin, cent->microSmoothOrigin );
			cent->microSmooth++;
			clamp_high( cent->microSmooth, 2 );

			VectorCopy( cent->ent.origin, cent->ent.origin2 );
		} else {   // plain interpolation
			for( i = 0; i < 3; i++ )
				cent->ent.origin[i] = cent->ent.origin2[i] = cent->prev.origin[i] + cg.lerpfrac *
																					( cent->current.origin[i] - cent->prev.origin[i] );
		}
	}

	VectorCopy( cent->ent.origin, cent->ent.lightingOrigin );
}

static void CG_AddGenericEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex && !( cent->effects & EF_FLAG_TRAIL ) ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
		Matrix3_Copy( cg.autorotateAxis, cent->ent.axis );
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	// add to refresh list
	CG_AddCentityOutLineEffect( cent, drawFromViewState );

	// render effects
	cent->ent.renderfx = cent->renderfx;

	if( cent->item ) {
		const gsitem_t *item = cent->item;

		if( item->type & ( IT_HEALTH | IT_POWERUP ) ) {
			cent->ent.renderfx |= RF_NOSHADOW;
		}

		if( cent->effects & EF_AMMOBOX ) {
#ifdef DOWNSCALE_ITEMS // Ugly hack for the release. Armor models are way too big
			cent->ent.scale *= 0.90f;
#endif

			// find out the ammo box color
			if( cent->item->color && strlen( cent->item->color ) > 1 ) {
				vec4_t scolor;
				Vector4Copy( color_table[ColorIndex( cent->item->color[1] )], scolor );
				cent->ent.shaderRGBA[0] = ( uint8_t )( 255 * scolor[0] );
				cent->ent.shaderRGBA[1] = ( uint8_t )( 255 * scolor[1] );
				cent->ent.shaderRGBA[2] = ( uint8_t )( 255 * scolor[2] );
			} else {   // set white
				VectorSet( cent->ent.shaderRGBA, 255, 255, 255 );
			}
		}

		if( cent->effects & EF_GHOST ) {
			cent->ent.renderfx |= RF_ALPHAHACK | RF_GREYSCALE;
			cent->ent.shaderRGBA[3] = 100;

			// outlines don't work on transparent objects...
			cent->ent.outlineHeight = 0;
		} else {
			cent->ent.shaderRGBA[3] = 255;
		}

		cent->ent.renderfx |= RF_MINLIGHT;

		// offset weapon items by their special tag
		if( cent->item->type & IT_WEAPON ) {
			CG_PlaceModelOnTag( &cent->ent, &cent->ent, &cgs.weaponItemTag );
		}
	} else {
		cent->ent.renderfx |= RF_NOSHADOW;
	}

	if( cent->skel ) {
		// get space in cache, interpolate, transform, link
		cent->ent.boneposes = cent->ent.oldboneposes = CG_RegisterTemporaryExternalBoneposes( cent->skel );
		CG_LerpSkeletonPoses( cent->skel, cent->ent.frame, cent->ent.oldframe, cent->ent.boneposes, 1.0 - cent->ent.backlerp );
		CG_TransformBoneposes( cent->skel, cent->ent.boneposes, cent->ent.boneposes );
	}

	// flags are special
	if( cent->effects & EF_FLAG_TRAIL ) {
		CG_AddFlagModelOnTag( cent, cent->ent.shaderRGBA, "tag_linked", drawSceneRequest, drawFromViewState );
	}

	if( !cent->current.modelindex ) {
		return;
	}

	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	if( cent->current.modelindex2 ) {
		CG_AddLinkedModel( cent, drawSceneRequest, drawFromViewState );
	}
}

void CG_AddFlagModelOnTag( centity_t *cent, byte_vec4_t teamcolor, const char *tagname, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	static entity_t flag;
	orientation_t tag;

	if( !( cent->effects & EF_FLAG_TRAIL ) ) {
		return;
	}

	memset( &flag, 0, sizeof( entity_t ) );
	flag.model = R_RegisterModel( PATH_FLAG_MODEL );
	if( !flag.model ) {
		return;
	}

	flag.rtype = RT_MODEL;
	flag.scale = 1.0f;
	flag.renderfx = cent->ent.renderfx;
	flag.customShader = NULL;
	flag.customSkin = NULL;
	flag.shaderRGBA[0] = ( uint8_t )teamcolor[0];
	flag.shaderRGBA[1] = ( uint8_t )teamcolor[1];
	flag.shaderRGBA[2] = ( uint8_t )teamcolor[2];
	flag.shaderRGBA[3] = ( uint8_t )teamcolor[3];

	VectorCopy( cent->ent.origin, flag.origin );
	VectorCopy( cent->ent.origin, flag.origin2 );
	VectorCopy( cent->ent.lightingOrigin, flag.lightingOrigin );

	// place the flag on the tag if available
	if( tagname && CG_GrabTag( &tag, &cent->ent, tagname ) ) {
		Matrix3_Copy( cent->ent.axis, flag.axis );
		CG_PlaceModelOnTag( &flag, &cent->ent, &tag );
	} else {   // Flag dropped
		vec3_t angles;

		// quick & dirty client-side rotation animation, rotate once every 2 seconds
		if( !cent->fly_stoptime ) {
			cent->fly_stoptime = cg.time;
		}

		angles[0] = LerpAngle( cent->prev.angles[0], cent->current.angles[0], cg.lerpfrac ) - 75; // Let it stand up 75 degrees
		angles[1] = ( 360.0 * ( ( cent->fly_stoptime - cg.time ) % 2000 ) ) / 2000.0;
		angles[2] = LerpAngle( cent->prev.angles[2], cent->current.angles[2], cg.lerpfrac );

		AnglesToAxis( angles, flag.axis );
		VectorMA( flag.origin, 16, &flag.axis[AXIS_FORWARD], flag.origin ); // Move the flag up a bit
	}

	CG_AddColoredOutLineEffect( &flag, EF_OUTLINE,
								(uint8_t)( teamcolor[0] * 0.3 ),
								(uint8_t)( teamcolor[1] * 0.3 ),
								(uint8_t)( teamcolor[2] * 0.3 ),
								255, drawFromViewState );

	CG_AddEntityToScene( &flag, drawSceneRequest );

	// add the light & energy effects
	if( CG_GrabTag( &tag, &flag, "tag_color" ) ) {
		CG_PlaceModelOnTag( &flag, &flag, &tag );
	}

	// FIXME: convert this to an autosprite mesh in the flag model
	if( !( cent->ent.renderfx & RF_VIEWERMODEL ) ) {
		flag.rtype = RT_SPRITE;
		flag.model = NULL;
		flag.renderfx = RF_NOSHADOW | RF_FULLBRIGHT;
		flag.frame = flag.oldframe = 0;
		flag.radius = 32.0f;
		flag.customShader = cgs.media.shaderFlagFlare;
		flag.outlineHeight = 0;

		CG_AddEntityToScene( &flag, drawSceneRequest );
	}

	drawSceneRequest->addLight( flag.origin, 128.0f, 0.0f, teamcolor[0] / 255, teamcolor[1] / 255, teamcolor[2] / 255 );

	// TODO: We have disabled the flag particles trail effects as they were god-awful
}

static void CG_UpdateFlagBaseEnt( centity_t *cent ) {
	int modelindex;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
	if( cent->effects & EF_OUTLINE ) {
		CG_SetOutlineColor( cent->outlineColor, cent->ent.shaderRGBA );
	}

	cent->ent.scale = 1.0f;

	cent->item = GS_FindItemByTag( cggs, cent->current.itemNum );
	if( cent->item ) {
		cent->effects |= cent->item->effects;
	}

	cent->ent.rtype = RT_MODEL;
	cent->ent.frame = cent->current.frame;
	cent->ent.oldframe = cent->prev.frame;

	// set up the model
	modelindex = cent->current.modelindex;
	if( modelindex > 0 && modelindex < MAX_MODELS ) {
		cent->ent.model = cgs.modelDraw[modelindex];
	}
	cent->skel = CG_SkeletonForModel( cent->ent.model );
}

static void CG_AddFlagBaseEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *viewState ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->current.type != ET_PLAYER && cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
		Matrix3_Copy( cg.autorotateAxis, cent->ent.axis );
	}

	// render effects
	cent->ent.renderfx = cent->renderfx | RF_NOSHADOW;

	// let's see: We add first the modelindex 1 (the base)

	if( cent->skel ) {
		// get space in cache, interpolate, transform, link
		cent->ent.boneposes = cent->ent.oldboneposes = CG_RegisterTemporaryExternalBoneposes( cent->skel );
		CG_LerpSkeletonPoses( cent->skel, cent->ent.frame, cent->ent.oldframe, cent->ent.boneposes, 1.0 - cent->ent.backlerp );
		CG_TransformBoneposes( cent->skel, cent->ent.boneposes, cent->ent.boneposes );
	}

	// add to refresh list
	CG_AddCentityOutLineEffect( cent, viewState );

	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	//CG_DrawTestBox( cent->ent.origin, item_box_mins, item_box_maxs, vec3_origin );

	cent->ent.customSkin = NULL;
	cent->ent.customShader = NULL;  // never use a custom skin on others

	// see if we have to add a flag
	if( cent->effects & EF_FLAG_TRAIL ) {
		byte_vec4_t teamcolor;

		CG_AddFlagModelOnTag( cent, CG_TeamColorForEntity( cent->current.number, teamcolor ), "tag_flag1", drawSceneRequest, viewState );
	}
}

static void CG_AddPlayerEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	// render effects
	cent->ent.renderfx = cent->renderfx;
#ifndef CELSHADEDMATERIAL
	cent->ent.renderfx |= RF_MINLIGHT;
#endif

	if( drawFromViewState->isViewerEntity( cent->current.number ) ) {
		cg.effects = cent->effects;
		VectorCopy( cent->ent.lightingOrigin, cg.lightingOrigin );
		if( !drawFromViewState->view.thirdperson && cent->current.modelindex ) {
			cent->ent.renderfx |= RF_VIEWERMODEL; // only draw from mirrors
		}
	}

	// if set to invisible, skip
	if( !cent->current.modelindex || cent->current.team == TEAM_SPECTATOR ) {
		return;
	}

	CG_AddPModel( cent, drawSceneRequest, drawFromViewState );

	if( v_playerTrail.get() ) {
		if( cent->current.number != (int)drawFromViewState->predictedPlayerState.POVnum ) {
			const float timeDeltaSeconds  = 1e-3f * (float)cgs.snapFrameTime;
			const float speedThreshold    = 0.5f * ( GS_DefaultPlayerSpeed( *cggs ) + DEFAULT_DASHSPEED );
			const float distanceThreshold = speedThreshold * timeDeltaSeconds;
			// This condition effectively detaches trails from slowly walking players/stopped corpses.
			// A detached trail dissolves relatively quickly.
			// Calling touch() prevents from that, even no new nodes are added due to distance threshold.
			// TODO: Should attachment trails be immune to the speed loss?
			if( DistanceSquared( cent->current.origin, cent->prev.origin ) > wsw::square( distanceThreshold ) ) {
				if( cent->current.type != ET_CORPSE ) {
					if( cent->current.teleported ) {
						cg.effectsSystem.detachPlayerTrail( cent->current.number );
					}
					cg.effectsSystem.touchPlayerTrail( cent->current.number, cent->ent.origin, cg.time );
				} else {
					cg.effectsSystem.touchCorpseTrail( cent->current.number, cent->ent.origin, cg.time );
				}
			}
		}
	}

	// corpses can never have a model in modelindex2
	if( cent->current.type != ET_CORPSE ) {
		if( cent->current.modelindex2 ) {
			CG_AddLinkedModel( cent, drawSceneRequest, drawFromViewState );
		}
	}
}

static void CG_AddSpriteEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	if( !cent->ent.scale ) {
		return;
	}

	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	// bobbing & auto-rotation
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	// render effects
	cent->ent.renderfx = cent->renderfx;

	// add to refresh list
	CG_AddEntityToScene( &cent->ent, drawSceneRequest );

	if( cent->current.modelindex2 ) {
		CG_AddLinkedModel( cent, drawSceneRequest, drawFromViewState );
	}
}

static void CG_LerpSpriteEnt( centity_t *cent ) {
	int i;

	// interpolate origin
	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->ent.origin2[i] = cent->ent.lightingOrigin[i] = cent->prev.origin[i] + cg.lerpfrac * ( cent->current.origin[i] - cent->prev.origin[i] );

	cent->ent.radius = cent->prev.frame + cg.lerpfrac * ( cent->current.frame - cent->prev.frame );
}

static void CG_UpdateSpriteEnt( centity_t *cent ) {
	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	cent->ent.scale = 1.0f;
	cent->ent.renderfx = cent->renderfx;

	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the model
	cent->ent.rtype = RT_SPRITE;
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	cent->ent.radius = cent->prev.frame;
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin, cent->ent.origin2 );
	VectorCopy( cent->prev.origin, cent->ent.lightingOrigin );
	Matrix3_Identity( cent->ent.axis );
}

static void CG_AddDecalEnt( centity_t *cent ) {
	// if set to invisible, skip
	if( !cent->current.modelindex ) {
		return;
	}

	if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
		CG_EntAddTeamColorTransitionEffect( cent );
	}

	CG_AddFragmentedDecal( cent->ent.origin, cent->ent.origin2,
						   cent->ent.rotation, cent->ent.radius,
						   cent->ent.shaderRGBA[0] * ( 1.0 / 255.0 ), cent->ent.shaderRGBA[1] * ( 1.0 / 255.0 ), cent->ent.shaderRGBA[2] * ( 1.0 / 255.0 ),
						   cent->ent.shaderRGBA[3] * ( 1.0 / 255.0 ), cent->ent.customShader );
}

static void CG_LerpDecalEnt( centity_t *cent ) {
	int i;
	float a1, a2;

	// interpolate origin
	for( i = 0; i < 3; i++ )
		cent->ent.origin[i] = cent->prev.origin[i] + cg.lerpfrac * ( cent->current.origin[i] - cent->prev.origin[i] );

	cent->ent.radius = cent->prev.frame + cg.lerpfrac * ( cent->current.frame - cent->prev.frame );

	a1 = cent->prev.modelindex2 / 255.0 * 360;
	a2 = cent->current.modelindex2 / 255.0 * 360;
	cent->ent.rotation = LerpAngle( a1, a2, cg.lerpfrac );
}

static void CG_UpdateDecalEnt( centity_t *cent ) {
	// set entity color based on team
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the null model, may be potentially needed for linked model
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	cent->ent.radius = cent->prev.frame;
	cent->ent.rotation = cent->prev.modelindex2 / 255.0 * 360;
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin2, cent->ent.origin2 );
}

static void CG_UpdateItemEnt( centity_t *cent ) {
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	Vector4Set( cent->ent.shaderRGBA, 255, 255, 255, 255 );

	cent->item = GS_FindItemByTag( cggs, cent->current.itemNum );
	if( !cent->item ) {
		return;
	}

	cent->effects |= cent->item->effects;

	if( v_simpleItems.get() && cent->item->simpleitem ) {
		cent->ent.rtype = RT_SPRITE;
		cent->ent.model = NULL;
		cent->skel = NULL;
		cent->ent.renderfx = RF_NOSHADOW | RF_FULLBRIGHT;
		cent->ent.frame = cent->ent.oldframe = 0;

		cent->ent.radius = v_simpleItemsSize.get() <= 32.0f ? v_simpleItemsSize.get() : 32.0f;
		if( cent->ent.radius < 1.0f ) {
			cent->ent.radius = 1.0f;
		}

		if( v_simpleItems.get() == 2 ) {
			cent->effects &= ~EF_ROTATE_AND_BOB;
		}

		cent->ent.customShader = NULL;
		cent->ent.customShader = R_RegisterPic( cent->item->simpleitem );
	} else {
		cent->ent.rtype = RT_MODEL;
		cent->ent.frame = cent->current.frame;
		cent->ent.oldframe = cent->prev.frame;

		if( cent->effects & EF_OUTLINE ) {
			Vector4Set( cent->outlineColor, 0, 0, 0, 255 ); // black

		}

		// set up the model
		cent->ent.model = cgs.modelDraw[cent->current.modelindex];
		cent->skel = CG_SkeletonForModel( cent->ent.model );
	}
}

static void CG_AddItemEnt( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	int msec;

	if( !cent->item ) {
		return;
	}

	// respawning items
	if( cent->respawnTime ) {
		msec = cg.time - cent->respawnTime;
	} else {
		msec = ITEM_RESPAWN_TIME;
	}

	if( msec >= 0 && msec < ITEM_RESPAWN_TIME ) {
		cent->ent.scale = (float)msec / ITEM_RESPAWN_TIME;
	} else {
		cent->ent.scale = 1.0f;
	}

	if( cent->ent.rtype != RT_SPRITE ) {
		// weapons are special
		if( cent->item && cent->item->type & IT_WEAPON ) {
			cent->ent.scale *= 1.40f;
		}

#ifdef DOWNSCALE_ITEMS // Ugly hack for release. Armor models are way too big
		if( cent->item ) {
			if( cent->item->type & IT_ARMOR ) {
				cent->ent.scale *= 0.85f;
			}
			if( cent->item->tag == HEALTH_SMALL ) {
				cent->ent.scale *= 0.85f;
			}
		}
#endif

		// flags are special
		if( cent->effects & EF_FLAG_TRAIL ) {
			CG_AddFlagModelOnTag( cent, cent->ent.shaderRGBA, NULL, drawSceneRequest, drawFromViewState );
			return;
		}

		CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
		return;
	} else {
		if( cent->effects & EF_GHOST ) {
			cent->ent.shaderRGBA[3] = 100;
			cent->ent.renderfx |= RF_GREYSCALE;
		}
	}

	// offset the item origin up
	cent->ent.origin[2] += cent->ent.radius + 2;
	cent->ent.origin2[2] += cent->ent.radius + 2;
	if( cent->effects & EF_ROTATE_AND_BOB ) {
		CG_EntAddBobEffect( cent );
	}

	Matrix3_Identity( cent->ent.axis );
	CG_AddEntityToScene( &cent->ent, drawSceneRequest );
}

void CG_ResetItemTimers( void ) {
	cg_num_item_timers = 0;
}

static void CG_UpdateItemTimerEnt( centity_t *cent ) {
	if( GS_MatchState( *cggs ) >= MATCH_STATE_POSTMATCH ) {
		return;
	}

	cent->item = GS_FindItemByTag( cggs, cent->current.itemNum );
	if( !cent->item ) {
		return;
	}

	if( cg_num_item_timers == MAX_ITEM_TIMERS ) {
		return;
	}

	cent->ent.frame = cent->current.frame;
	cg_item_timers[cg_num_item_timers++] = cent;
}

static int CG_CompareItemTimers( const centity_t **first, const centity_t **second ) {
	const centity_t *e1 = *first, *e2 = *second;
	const entity_state_t *s1 = &( e1->current ), *s2 = &( e2->current );
	const gsitem_t *i1 = e1->item, *i2 = e2->item;
	int t1 = s1->modelindex - 1, t2 = s2->modelindex - 1;

	// special hack to order teams like this: alpha -> neutral -> beta
	if( ( !t1 || !t2 ) && ( GS_MAX_TEAMS - TEAM_ALPHA ) == 2 ) {
		if( t2 == TEAM_ALPHA || t1 == TEAM_BETA ) {
			return 1;
		}
		if( t2 == TEAM_BETA || t1 == TEAM_ALPHA ) {
			return -1;
		}
	}

	if( t2 > t1 ) {
		return -11;
	}
	if( t2 < t1 ) {
		return 1;
	}

	if( s2->origin[2] > s1->origin[2] ) {
		return 1;
	}
	if( s2->origin[2] < s1->origin[2] ) {
		return -1;
	}

	if( i2->type > i1->type ) {
		return 1;
	}
	if( i2->type < i1->type ) {
		return -1;
	}

	if( s2->number > s1->number ) {
		return 1;
	}
	if( s2->number < s1->number ) {
		return -1;
	}

	return 0;
}

static void CG_SortItemTimers( void ) {
	qsort( cg_item_timers, cg_num_item_timers, sizeof( cg_item_timers[0] ), ( int ( * )( const void *, const void * ) )CG_CompareItemTimers );
}

static void CG_AddBeamEnt( centity_t *cent ) {
	const float width = cent->current.frame * 0.5f;
	cg.effectsSystem.spawnWorldLaserBeam( cent->current.origin, cent->current.origin2, width );
}

static void CG_UpdateLaserbeamEnt( centity_t *cent ) {
	centity_t *owner;

	const ViewState *const viewState = getOurClientViewState();

	// TODO: Check whether this condition holds for individual owners?
	// TODO: We can keep this code as-is (only the primary view gets predicted)
	if( viewState->view.playerPrediction && v_predictLaserBeam.get() && viewState->isViewerEntity( cent->current.ownerNum ) ) {
		return;
	}

	owner = &cg_entities[cent->current.ownerNum];
	if( owner->serverFrame != cg.frame.serverFrame ) {
		CG_Error( "CG_UpdateLaserbeamEnt: owner is not in the snapshot\n" );
	}

	owner->localEffects[LOCALEFFECT_LASERBEAM] = cg.time + 10;
	owner->laserCurved = ( cent->current.type == ET_CURVELASERBEAM ) ? true : false;

	// laser->s.origin is beam start
	// laser->s.origin2 is beam end

	VectorCopy( cent->prev.origin, owner->laserOriginOld );
	VectorCopy( cent->prev.origin2, owner->laserPointOld );

	VectorCopy( cent->current.origin, owner->laserOrigin );
	VectorCopy( cent->current.origin2, owner->laserPoint );
}

static void CG_LerpLaserbeamEnt( centity_t *cent, ViewState *drawFromViewState ) {
	centity_t *owner = &cg_entities[cent->current.ownerNum];

	if( drawFromViewState->view.playerPrediction && v_predictLaserBeam.get() ) {
		if( drawFromViewState->isViewerEntity( cent->current.ownerNum ) ) {
			return;
		}
	}

	// Disallow artificially triggering the effect if the owner does no longer set it
	if( !owner->localEffects[LOCALEFFECT_LASERBEAM] ) {
		return;
	}

	// Disallow resetting the beam prematurely for the "our client" pov
	const int64_t minNextTime = cg.time + 1;
	if( owner->localEffects[LOCALEFFECT_LASERBEAM] < minNextTime ) {
		owner->localEffects[LOCALEFFECT_LASERBEAM] = minNextTime;
	}

	owner->laserCurved = ( cent->current.type == ET_CURVELASERBEAM ) ? true : false;
}

static void CG_UpdatePortalSurfaceEnt( centity_t *cent ) {
	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );

	cent->ent.rtype = RT_PORTALSURFACE;
	Matrix3_Identity( cent->ent.axis );
	VectorCopy( cent->current.origin, cent->ent.origin );
	VectorCopy( cent->current.origin2, cent->ent.origin2 );

	if( !VectorCompare( cent->ent.origin, cent->ent.origin2 ) ) {
		cent->ent.frame = cent->current.skinnum;
	}

	if( cent->current.effects & EF_NOPORTALENTS ) {
		cent->ent.renderfx |= RF_NOPORTALENTS;
	}
}

static void CG_AddParticlesEnt( centity_t *cent ) {
}

void CG_UpdateParticlesEnt( centity_t *cent ) {
	// TODO: This forces TEAM_PLAYERS color for these entities
	CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// set up the data in the old position
	cent->ent.model = NULL;
	cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
	VectorCopy( cent->prev.origin, cent->ent.origin );
	VectorCopy( cent->prev.origin2, cent->ent.origin2 );
}

void CG_SoundEntityNewState( centity_t *cent ) {
	int channel, soundindex, owner;
	float attenuation;
	bool fixed;

	soundindex = cent->current.sound;
	owner = cent->current.ownerNum;
	channel = cent->current.channel & ~CHAN_FIXED;
	fixed = ( cent->current.channel & CHAN_FIXED ) ? true : false;
	attenuation = cent->current.attenuation;

	const ViewState *const primaryViewState = getPrimaryViewState();

	if( attenuation == ATTN_NONE ) {
		if( cgs.soundPrecache[soundindex] && primaryViewState->allowSounds ) {
			//cg.soundSystem->startGlobalSound( cgs.soundPrecache[soundindex], channel & ~CHAN_FIXED, 1.0f );
		}
		return;
	}

	if( owner ) {
		if( owner < 0 || owner >= MAX_EDICTS ) {
			Com_Printf( "CG_SoundEntityNewState: bad owner number" );
			return;
		}
		if( cg_entities[owner].serverFrame != cg.frame.serverFrame ) {
			owner = 0;
		}
	}

	if( !owner ) {
		fixed = true;
	}

	// sexed sounds are not in the sound index and ignore attenuation
	if( !cgs.soundPrecache[soundindex] ) {
		if( owner ) {
			auto string = cgs.configStrings.getSound( soundindex );
			if( string && string->startsWith( '*' ) ) {
				playSexedSoundInPrimaryView( owner, SoundSystem::OriginAttachment,
											 channel | ( fixed ? CHAN_FIXED : 0 ), string->data(), 1.0f, attenuation );
			}
		}
		return;
	}

	if( primaryViewState->allowSounds ) {
		if( fixed ) {
			cg.soundSystem->startFixedSound( cgs.soundPrecache[soundindex], cent->current.origin, channel, 1.0f, attenuation );
		} else {
			cg.soundSystem->startRelativeSound( cgs.soundPrecache[soundindex], SoundSystem::OriginAttachment, owner, channel, 1.0f, attenuation );
		}
	}
}

static void CG_EntityLoopSound( entity_state_t *state, float attenuation, const ViewState *drawFromViewState ) {
	if( state->sound && drawFromViewState->allowSounds ) {
		const SoundSet *const sound          = cgs.soundPrecache[state->sound];
		const int entNum                     = state->number;
		const uintptr_t loopIdentifyingToken = state->number;

		cg.soundSystem->addLoopSound( sound, SoundSystem::OriginAttachment, entNum, loopIdentifyingToken, v_volumeEffects.get(), attenuation );
	}
}

void CG_AddEntities( DrawSceneRequest *drawSceneRequest, ViewState *drawFromViewState ) {
	vec3_t autorotate;
	// bonus items rotate at a fixed rate
	VectorSet( autorotate, 0, ( cg.time % 3600 ) * 0.1, 0 );
	AnglesToAxis( autorotate, cg.autorotateAxis );

	// TODO: Sort all other entities by type as well
	auto *const plasmaStateIndices = (uint16_t *)alloca( sizeof( uint16_t ) * cg.frame.numEntities );
	unsigned numPlasmaEnts = 0;

	for( int pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		const int stateIndex  = pnum & ( MAX_PARSE_ENTITIES - 1 );
		entity_state_t *state = &cg.frame.parsedEntities[stateIndex];
		centity_t *cent       = &cg_entities[state->number];

		if( cent->current.linearMovement ) {
			if( !cent->linearProjectileCanDraw ) {
				continue;
			}
		}

		bool canLight = !state->linearMovement;

		switch( cent->type ) {
			case ET_GENERIC:
				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				if( v_drawEntityBoxes.get() ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				canLight = true;
				break;
			case ET_GIB:
				if( false ) {
					CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
					CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
					canLight = true;
				}
				break;
			case ET_BLASTER:
				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				cg.effectsSystem.touchBlastTrail( cent->current.number, cent->ent.origin, cent->velocity, cg.time );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );

				// We use relatively large light radius because this projectile moves very fast, so make it noticeable
				drawSceneRequest->addLight( cent->ent.origin, 192.0f, 144.0f, 0.9f, 0.7f, 0.0f );
				break;

			case ET_ELECTRO_WEAK:
				cent->current.frame = cent->prev.frame = 0;
				cent->ent.frame =  cent->ent.oldframe = 0;

				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				cg.effectsSystem.touchElectroTrail( cent->current.number, cent->current.ownerNum,
													cent->current.linearMovementBegin, cent->ent.origin, cg.time );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				drawSceneRequest->addLight( cent->ent.origin, 192.0f, 144.0f, 0.9f, 0.9f, 1.0f );
				break;
			case ET_ROCKET:
				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				CG_EntityLoopSound( state, ATTN_NORM, drawFromViewState );
				if( cent->current.effects & EF_STRONG_WEAPON ) {
					cg.effectsSystem.touchStrongRocketTrail( cent->current.number, cent->ent.origin, cg.time );
					drawSceneRequest->addLight( cent->ent.origin, 300.0f, 192.0f, 1.0f, 0.7f, 0.3f );
				} else {
					cg.effectsSystem.touchWeakRocketTrail( cent->current.number, cent->ent.origin, cg.time );
					drawSceneRequest->addLight( cent->ent.origin, 300.0f - 48.0f, 192.0f - 32.0f, 1.0f, 0.7f, 0.3f );
				}
				break;
			case ET_GRENADE:
				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				if( cent->current.effects & EF_STRONG_WEAPON ) {
					cg.effectsSystem.touchStrongGrenadeTrail( cent->current.number, cent->ent.origin, cg.time );
				} else {
					cg.effectsSystem.touchWeakGrenadeTrail( cent->current.number, cent->ent.origin, cg.time );
				}
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				if( cent->current.effects & EF_STRONG_WEAPON ) {
					if( cent->current.effects & EF_ACTIVATED ) {
						drawSceneRequest->addLight( cent->ent.origin, 200.0f, 72.0f, 1.0f, 0.0f, 0.0f );
					} else if( cent->current.effects & EF_ARMED ) {
						drawSceneRequest->addLight( cent->ent.origin, 200.0f, 72.0f, 1.0f, 0.8f, 0.3f );
					} else {
						drawSceneRequest->addLight( cent->ent.origin, 200.0f, 72.0f, 0.0f, 0.3f, 1.0f );
					}
				} else {
					drawSceneRequest->addLight( cent->ent.origin, 200.0f, 72.0f, 0.0f, 0.3f, 1.0f );
				}
				break;
			case ET_PLASMA:
				plasmaStateIndices[numPlasmaEnts++] = (uint16_t)stateIndex;
				break;
			case ET_WAVE:
				CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				// Add the core light
				drawSceneRequest->addLight( cent->ent.origin, 128.0f, 128.0f, 0.0f, 0.3f, 1.0f );
				// Add the corona light
				// We have initially thought to activate corona light only when corona damage is enabled,
				// but it is not a good idea since it requires synchronization/prediction
				// and the projectile gets activated rather fast anyway.
				// Otherwise high ping players would only see an activated wave.
				drawSceneRequest->addLight( cent->ent.origin, 300.0f, 192.0f, 1.0f, 1.0f, 1.0f );
				break;
			case ET_SPRITE:
			case ET_RADAR:
				CG_AddSpriteEnt( cent, drawSceneRequest, drawFromViewState );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				canLight = true;
				break;

			case ET_ITEM:
				CG_AddItemEnt( cent, drawSceneRequest, drawFromViewState );
				if( v_drawEntityBoxes.get() ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE, drawFromViewState );
				canLight = true;
				break;

			case ET_PLAYER:
				CG_AddPlayerEnt( cent, drawSceneRequest, drawFromViewState );
				if( v_drawEntityBoxes.get() ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE, drawFromViewState );
				CG_LaserBeamEffect( cent, drawSceneRequest, drawFromViewState );
				CG_WeaponBeamEffect( cent, drawFromViewState );
				canLight = true;
				break;

			case ET_CORPSE:
				CG_AddPlayerEnt( cent, drawSceneRequest, drawFromViewState );
				if( v_drawEntityBoxes.get() ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_IDLE, drawFromViewState );
				canLight = true;
				break;

			case ET_BEAM:
				CG_AddBeamEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				break;

			case ET_PORTALSURFACE:
				// We add it separately. Refer to creation of draw scene requests
				// CG_AddPortalSurfaceEnt( cent, drawSceneRequest );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				break;

			case ET_FLAG_BASE:
				CG_AddFlagBaseEnt( cent, drawSceneRequest, drawFromViewState );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				canLight = true;
				break;

			case ET_MINIMAP_ICON:
				if( cent->effects & EF_TEAMCOLOR_TRANSITION ) {
					CG_EntAddTeamColorTransitionEffect( cent );
				}
				break;

			case ET_DECAL:
				CG_AddDecalEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				break;

			case ET_PUSH_TRIGGER:
				if( v_drawEntityBoxes.get() ) {
					CG_DrawEntityBox( cent );
				}
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				break;

			case ET_PARTICLES:
				CG_AddParticlesEnt( cent );
				CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );
				break;

				// TODO: Remove once the net protocol gets updated
			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_AddPacketEntities: unknown entity type" );
				break;
		}

		// glow if light is set
		if( canLight && state->light ) {
			drawSceneRequest->addLight( cent->ent.origin,
										COLOR_A( state->light ) * 4.0, 0.0f,
										COLOR_R( state->light ) * ( 1.0 / 255.0 ),
										COLOR_G( state->light ) * ( 1.0 / 255.0 ),
										COLOR_B( state->light ) * ( 1.0 / 255.0 ) );
		}
	}

	for( unsigned i = 0; i < numPlasmaEnts; ++i ) {
		entity_state_t *state = &cg.frame.parsedEntities[plasmaStateIndices[i]];
		centity_t *cent       = &cg_entities[state->number];
		CG_AddGenericEnt( cent, drawSceneRequest, drawFromViewState );
		CG_EntityLoopSound( state, ATTN_STATIC, drawFromViewState );

		if( state->effects & EF_STRONG_WEAPON ) {
			//cgNotice() << "velocity" << cent->velocity[0] << cent->velocity[1] << cent->velocity[2];
			cg.effectsSystem.touchStrongPlasmaTrail( cent->current.number, cent->current.origin, cent->velocity, cg.time );
		} else {
			//cgNotice() << "velocity" << cent->velocity[0] << cent->velocity[1] << cent->velocity[2];
			cg.effectsSystem.touchWeakPlasmaTrail( cent->current.number, cent->current.origin, cent->velocity, cg.time );
		}

		constexpr float desiredProgramLightRadius = 128.0f;
		float programLightRadius                  = 0.0f;

		// TODO: This should be handled at rendering layer during culling/light prioritization
		const float squareDistance = DistanceSquared( drawFromViewState->view.refdef.vieworg, state->origin );
		// TODO: Using fov tangent ratio is a more correct approach (but nobody actually notices in zooomed in state)
		if( squareDistance < 384.0f * 384.0f ) {
			programLightRadius = desiredProgramLightRadius;
		} else if( squareDistance < 768.0f * 768.0f ) {
			if( ( ( cg.frameCount % 2 ) == ( state->number % 2 ) ) ) {
				programLightRadius = desiredProgramLightRadius;
			}
		} else {
			if( ( cg.frameCount % 4 ) == ( state->number % 4 ) ) {
				programLightRadius = desiredProgramLightRadius;
			}
		}

		drawSceneRequest->addLight( cent->ent.origin, programLightRadius, 64.0f, 0.0f, 1.0f, 0.5f );
	}
}

/*
* Interpolate the entity states positions into the entity_t structs
*/
void CG_LerpEntities( ViewState *drawFromViewState ) {
	entity_state_t *state;
	int pnum;
	centity_t *cent;

	for( pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		int number;
		bool spatialize;

		state = &cg.frame.parsedEntities[pnum & ( MAX_PARSE_ENTITIES - 1 )];
		number = state->number;
		cent = &cg_entities[number];
		spatialize = true;

		switch( cent->type ) {
			case ET_GENERIC:
			case ET_GIB:
			case ET_BLASTER:
			case ET_ELECTRO_WEAK:
			case ET_ROCKET:
			case ET_PLASMA:
			case ET_GRENADE:
			case ET_WAVE:
			case ET_ITEM:
			case ET_PLAYER:
			case ET_CORPSE:
			case ET_FLAG_BASE:
				if( state->linearMovement ) {
					CG_ExtrapolateLinearProjectile( cent );
				} else {
					CG_LerpGenericEnt( cent, drawFromViewState );
				}
				break;

			case ET_SPRITE:
			case ET_RADAR:
				CG_LerpSpriteEnt( cent );
				break;

			case ET_DECAL:
				CG_LerpDecalEnt( cent );
				break;

			case ET_BEAM:

				// beams aren't interpolated
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				CG_LerpLaserbeamEnt( cent, drawFromViewState );
				break;

			case ET_MINIMAP_ICON:
				break;

			case ET_PORTALSURFACE:

				//portals aren't interpolated
				break;

			case ET_PUSH_TRIGGER:
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				break;

			case ET_PARTICLES:
				break;

			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_LerpEntities: unknown entity type" );
				break;
		}

		if( spatialize ) {
			vec3_t origin, velocity;
			mat3_t axis;
			CG_GetEntitySpatilization( number, origin, velocity, axis );
			cg.soundSystem->setEntitySpatialParams( number, origin, velocity, axis );
		}
	}
}

/*
* CG_UpdateEntities
* Called at receiving a new serverframe. Sets up the model, type, etc to be drawn later on
*/
void CG_UpdateEntities( void ) {
	entity_state_t *state;
	int pnum;
	centity_t *cent;

	CG_ResetItemTimers();

	for( pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		state = &cg.frame.parsedEntities[pnum & ( MAX_PARSE_ENTITIES - 1 )];
		cent = &cg_entities[state->number];
		cent->type = state->type;
		cent->effects = state->effects;
		cent->item = NULL;
		cent->renderfx = 0;

		switch( cent->type ) {
			case ET_GENERIC:
				CG_UpdateGenericEnt( cent );
				break;
			case ET_GIB:
				if( false ) {
					cent->renderfx |= RF_NOSHADOW;
					CG_UpdateGenericEnt( cent );

					// set the gib model ignoring the modelindex one
					// TODO: Disabled old shitty gibs for now
					cent->ent.model = nullptr;
				}
				break;

				// projectiles with linear trajectories
			case ET_BLASTER:
			case ET_ELECTRO_WEAK:
			case ET_ROCKET:
			case ET_PLASMA:
			case ET_GRENADE:
			case ET_WAVE:
				cent->renderfx |= ( RF_NOSHADOW | RF_FULLBRIGHT );
				CG_UpdateGenericEnt( cent );
				break;

			case ET_RADAR:
				cent->renderfx |= RF_NODEPTHTEST;
			case ET_SPRITE:
				cent->renderfx |= ( RF_NOSHADOW | RF_FULLBRIGHT );
				CG_UpdateSpriteEnt( cent );
				break;

			case ET_ITEM:
				CG_UpdateItemEnt( cent );
				break;
			case ET_PLAYER:
			case ET_CORPSE:
				CG_UpdatePlayerModelEnt( cent );
				break;

			case ET_BEAM:
				break;

			case ET_LASERBEAM:
			case ET_CURVELASERBEAM:
				CG_UpdateLaserbeamEnt( cent );
				break;

			case ET_FLAG_BASE:
				CG_UpdateFlagBaseEnt( cent );
				break;

			case ET_MINIMAP_ICON:
			{
				CG_TeamColorForEntity( cent->current.number, cent->ent.shaderRGBA );
				if( cent->current.modelindex > 0 && cent->current.modelindex < MAX_IMAGES ) {
					cent->ent.customShader = cgs.imagePrecache[ cent->current.modelindex ];
				} else {
					cent->ent.customShader = NULL;
				}
			}
				break;

			case ET_DECAL:
				CG_UpdateDecalEnt( cent );
				break;

			case ET_PORTALSURFACE:
				CG_UpdatePortalSurfaceEnt( cent );
				break;

			case ET_PUSH_TRIGGER:
				break;

			case ET_EVENT:
			case ET_SOUNDEVENT:
				break;

			case ET_ITEM_TIMER:
				CG_UpdateItemTimerEnt( cent );
				break;

			case ET_PARTICLES:
				CG_UpdateParticlesEnt( cent );
				break;

			case ET_VIDEO_SPEAKER:
				break;

			default:
				CG_Error( "CG_UpdateEntities: unknown entity type %i", cent->type );
				break;
		}
	}

	CG_SortItemTimers();
}

void CG_GetEntitySpatilization( int entNum, float *origin, float *velocity, float *axis ) {
	centity_t *cent;
	const cmodel_s *cmodel;
	vec3_t mins, maxs;

	if( entNum < 0 || entNum >= MAX_EDICTS ) {
		CG_Error( "CG_GetEntitySoundOrigin: bad entnum" );
		return;
	}

	cent = &cg_entities[entNum];

	// normal
	if( cent->current.solid != SOLID_BMODEL ) {
		VectorCopy( cent->ent.origin, origin );
		VectorCopy( cent->velocity, velocity );
		VectorCopy( cent->ent.axis, axis );
		return;
	}

	// bmodel
	cmodel = CG_InlineModel( cent->current.modelindex );
	CG_InlineModelBounds( cmodel, mins, maxs );
	VectorAdd( maxs, mins, origin );
	VectorMA( cent->ent.origin, 0.5f, origin, origin );
	VectorCopy( cent->velocity, velocity );
	VectorCopy( cent->ent.axis, axis );
}

/*
* CG_PredictedEvent - shared code can fire events during prediction
*/
void CG_PredictedEvent( int entNum, int ev, int parm ) {
	if( ev >= PREDICTABLE_EVENTS_MAX ) {
		return;
	}

	ViewState *const viewState = getOurClientViewState();

	// ignore this action if it has already been predicted (the unclosed ucmd has timestamp zero)
	if( ucmdReady && ( viewState->predictingTimeStamp > viewState->predictedEventTimes[ev] ) ) {
		// inhibit the fire event when there is a weapon change predicted
		if( ev == EV_FIREWEAPON ) {
			if( viewState->predictedWeaponSwitch ) {
				if( viewState->predictedWeaponSwitch != viewState->predictedPlayerState.stats[STAT_PENDING_WEAPON] ) {
					return;
				}
			}
		}

		viewState->predictedEventTimes[ev] = viewState->predictingTimeStamp;
		CG_EntityEvent( &cg_entities[entNum].current, ev, parm, true );
	}
}

void CG_Predict_ChangeWeapon( int new_weapon ) {
	ViewState *const viewState = getOurClientViewState();
	if( viewState->view.playerPrediction ) {
		viewState->predictedWeaponSwitch = new_weapon;
	}
}

void CG_CheckPredictionError() {
	// TODO: Accept as an argument!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	ViewState *const viewState = getOurClientViewState();
	if( !viewState->view.playerPrediction ) {
		return;
	}

	// calculate the last usercmd_t we sent that the server has processed
	const int frame = cg.frame.ucmdExecuted & CMD_MASK;

	vec3_t origin;
	// compare what the server returned with what we had predicted it to be
	VectorCopy( viewState->predictedOrigins[frame], origin );

	if( viewState->predictedGroundEntity != -1 ) {
		entity_state_t *ent = &cg_entities[viewState->predictedGroundEntity].current;
		if( ent->solid == SOLID_BMODEL ) {
			if( ent->linearMovement ) {
				vec3_t move;
				GS_LinearMovementDelta( ent, cg.oldFrame.serverTime, cg.frame.serverTime, move );
				VectorAdd( viewState->predictedOrigins[frame], move, origin );
			}
		}
	}

	// TODO: Why int
	// TODO: Should a proper rounding be used at least
	int delta[3];
	VectorSubtract( viewState->snapPlayerState.pmove.origin, origin, delta );

	// save the prediction error for interpolation
	if( abs( delta[0] ) > 128 || abs( delta[1] ) > 128 || abs( delta[2] ) > 128 ) {
		if( v_showMiss.get() ) {
			Com_Printf( "prediction miss on %" PRIi64 ": %i\n", cg.frame.serverFrame, abs( delta[0] ) + abs( delta[1] ) + abs( delta[2] ) );
		}
		VectorClear( viewState->predictionError );          // a teleport or something
	} else {
		if( v_showMiss.get() && ( delta[0] || delta[1] || delta[2] ) ) {
			Com_Printf( "prediction miss on %" PRIi64" : %i\n", cg.frame.serverFrame, abs( delta[0] ) + abs( delta[1] ) + abs( delta[2] ) );
		}
		VectorCopy( viewState->snapPlayerState.pmove.origin, viewState->predictedOrigins[frame] );
		VectorCopy( delta, viewState->predictionError ); // save for error interpolation
	}
}

void CG_BuildSolidList( void ) {
	int i;
	entity_state_t *ent;

	cg_numSolids = 0;
	cg_numTriggers = 0;
	for( i = 0; i < cg.frame.numEntities; i++ ) {
		ent = &cg.frame.parsedEntities[i & ( MAX_PARSE_ENTITIES - 1 )];
		if( ISEVENTENTITY( ent ) ) {
			continue;
		}

		if( ent->solid ) {
			switch( ent->type ) {
				// the following entities can never be solid
				case ET_BEAM:
				case ET_PORTALSURFACE:
				case ET_BLASTER:
				case ET_ELECTRO_WEAK:
				case ET_ROCKET:
				case ET_GRENADE:
				case ET_PLASMA:
				case ET_LASERBEAM:
				case ET_CURVELASERBEAM:
				case ET_MINIMAP_ICON:
				case ET_DECAL:
				case ET_ITEM_TIMER:
				case ET_PARTICLES:
					break;

				case ET_PUSH_TRIGGER:
					cg_triggersList[cg_numTriggers++] = &cg_entities[ ent->number ].current;
					break;

				default:
					cg_solidList[cg_numSolids++] = &cg_entities[ ent->number ].current;
					break;
			}
		}
	}
}

static bool CG_ClipEntityContact( const vec3_t origin, const vec3_t mins, const vec3_t maxs, int entNum ) {
	centity_t *cent;
	const cmodel_s *cmodel;
	trace_t tr;
	vec3_t absmins, absmaxs;
	vec3_t entorigin, entangles;
	int64_t serverTime = cg.frame.serverTime;

	if( !mins ) {
		mins = vec3_origin;
	}
	if( !maxs ) {
		maxs = vec3_origin;
	}

	// find the cmodel
	cmodel = CG_CModelForEntity( entNum );
	if( !cmodel ) {
		return false;
	}

	cent = &cg_entities[entNum];

	// find the origin
	if( cent->current.solid == SOLID_BMODEL ) { // special value for bmodel
		if( cent->current.linearMovement ) {
			GS_LinearMovement( &cent->current, serverTime, entorigin );
		} else {
			VectorCopy( cent->current.origin, entorigin );
		}
		VectorCopy( cent->current.angles, entangles );
	} else {   // encoded bbox
		VectorCopy( cent->current.origin, entorigin );
		VectorClear( entangles ); // boxes don't rotate
	}

	// convert the box to compare to absolute coordinates
	VectorAdd( origin, mins, absmins );
	VectorAdd( origin, maxs, absmaxs );
	CG_TransformedBoxTrace( &tr, vec3_origin, vec3_origin, absmins, absmaxs, cmodel, MASK_ALL, entorigin, entangles );
	return tr.startsolid == true || tr.allsolid == true;
}

void CG_Predict_TouchTriggers( pmove_t *pm, const vec3_t previous_origin ) {
	int i;
	entity_state_t *state;

	// fixme: more accurate check for being able to touch or not
	if( pm->playerState->pmove.pm_type != PM_NORMAL ) {
		return;
	}

	for( i = 0; i < cg_numTriggers; i++ ) {
		state = cg_triggersList[i];

		if( state->type == ET_PUSH_TRIGGER ) {
			if( !cg_triggersListTriggered[i] ) {
				if( CG_ClipEntityContact( pm->playerState->pmove.origin, pm->mins, pm->maxs, state->number ) ) {
					GS_TouchPushTrigger( cggs, pm->playerState, state );
					cg_triggersListTriggered[i] = true;
				}
			}
		}
	}
}

static void CG_ClipMoveToEntities( const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, int ignore, int contentmask, trace_t *tr ) {
	int i, x, zd, zu;
	trace_t trace;
	vec3_t origin, angles;
	entity_state_t *ent;
	const cmodel_s *cmodel;
	vec3_t bmins, bmaxs;
	int64_t serverTime = cg.frame.serverTime;

	for( i = 0; i < cg_numSolids; i++ ) {
		ent = cg_solidList[i];

		if( ent->number == ignore ) {
			continue;
		}
		if( !( contentmask & CONTENTS_CORPSE ) && ( ( ent->type == ET_CORPSE ) || ( ent->type == ET_GIB ) ) ) {
			continue;
		}

		if( ent->solid == SOLID_BMODEL ) { // special value for bmodel
			cmodel = CG_InlineModel( ent->modelindex );
			if( !cmodel ) {
				continue;
			}

			if( ent->linearMovement ) {
				GS_LinearMovement( ent, serverTime, origin );
			} else {
				VectorCopy( ent->origin, origin );
			}

			VectorCopy( ent->angles, angles );
		} else {   // encoded bbox
			x = 8 * ( ent->solid & 31 );
			zd = 8 * ( ( ent->solid >> 5 ) & 31 );
			zu = 8 * ( ( ent->solid >> 10 ) & 63 ) - 32;

			bmins[0] = bmins[1] = -x;
			bmaxs[0] = bmaxs[1] = x;
			bmins[2] = -zd;
			bmaxs[2] = zu;

			VectorCopy( ent->origin, origin );
			VectorClear( angles ); // boxes don't rotate

			if( ent->type == ET_PLAYER || ent->type == ET_CORPSE ) {
				cmodel = CG_OctagonModelForBBox( bmins, bmaxs );
			} else {
				cmodel = CG_ModelForBBox( bmins, bmaxs );
			}
		}

		CG_TransformedBoxTrace( &trace, (vec_t *)start, (vec_t *)end, (vec_t *)mins, (vec_t *)maxs, cmodel, contentmask, origin, angles );
		if( trace.allsolid || trace.fraction < tr->fraction ) {
			trace.ent = ent->number;
			*tr = trace;
		} else if( trace.startsolid ) {
			tr->startsolid = true;
		}

		if( tr->allsolid ) {
			return;
		}
	}
}

void CG_Trace( trace_t *t, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, int ignore, int contentmask ) {
	// check against world
	CG_TransformedBoxTrace( t, start, end, mins, maxs, NULL, contentmask, NULL, NULL );
	t->ent = t->fraction < 1.0 ? 0 : -1; // world entity is 0
	if( t->fraction == 0 ) {
		return; // blocked by the world

	}

	// check all other solid models
	CG_ClipMoveToEntities( start, mins, maxs, end, ignore, contentmask, t );
}

int CG_PointContents( const vec3_t point ) {
	int i;
	entity_state_t *ent;
	const cmodel_s *cmodel;
	int contents;

	contents = CG_TransformedPointContents( (vec_t *)point, NULL, NULL, NULL );

	for( i = 0; i < cg_numSolids; i++ ) {
		ent = cg_solidList[i];
		if( ent->solid != SOLID_BMODEL ) { // special value for bmodel
			continue;
		}

		cmodel = CG_InlineModel( ent->modelindex );
		if( cmodel ) {
			contents |= CG_TransformedPointContents( (vec_t *)point, cmodel, ent->origin, ent->angles );
		}
	}

	return contents;
}

static void CG_PredictSmoothSteps() {
	ViewState *const viewState = getPrimaryViewState();

	viewState->predictedStepTime = 0;
	viewState->predictedStep     = 0;

	int64_t outgoing;
	NET_GetCurrentState( NULL, &outgoing, NULL );

	int64_t index = outgoing;
	int predictiontime = 0;
	while( predictiontime < PREDICTED_STEP_TIME && outgoing - index < CMD_BACKUP ) {
		usercmd_t cmd;
		NET_GetUserCmd( (int)( index & CMD_MASK ), &cmd );
		predictiontime += cmd.msec;
		index--;
	}

	// run frames
	int virtualtime = 0;
	while( ++index <= outgoing ) {
		usercmd_t cmd;
		const int64_t frame = index & CMD_MASK;
		NET_GetUserCmd( (int)frame, &cmd );
		virtualtime += cmd.msec;

		if( const float stepSize = viewState->predictedSteps[frame]; stepSize != 0.0f ) {
			float oldStep = 0.0f;
			// check for stepping up before a previous step is completed
			if( const int64_t delta = cg.realTime - viewState->predictedStepTime; delta < PREDICTED_STEP_TIME ) {
				oldStep = viewState->predictedStep * ( (float)( PREDICTED_STEP_TIME - delta ) / (float)PREDICTED_STEP_TIME );
			}

			viewState->predictedStep     = oldStep + stepSize;
			viewState->predictedStepTime = cg.realTime - ( predictiontime - virtualtime );
		}
	}
}

/*
* CG_PredictMovement
*
* Sets cg.predictedVelocty, cg.predictedOrigin and cg.predictedAngles
*/
void CG_PredictMovement() {
	ViewState *const viewState = getOurClientViewState();

	int64_t ucmdHead = 0;
	NET_GetCurrentState( NULL, &ucmdHead, NULL );
	int64_t ucmdExecuted = cg.frame.ucmdExecuted;

	if( !v_predictOptimize.get() || ( ucmdHead - viewState->predictFrom >= CMD_BACKUP ) ) {
		viewState->predictFrom = 0;
	}

	if( viewState->predictFrom > 0 ) {
		ucmdExecuted = viewState->predictFrom;
		viewState->predictedPlayerState = viewState->predictFromPlayerState;
		cg_entities[viewState->snapPlayerState.POVnum].current = viewState->predictFromEntityState;
	} else {
		viewState->predictedPlayerState = viewState->snapPlayerState; // start from the final position
	}

	viewState->predictedPlayerState.POVnum = cgs.playerNum + 1;

	// if we are too far out of date, just freeze
	if( ucmdHead - ucmdExecuted >= CMD_BACKUP ) {
		if( v_showMiss.get() ) {
			Com_Printf( "exceeded CMD_BACKUP\n" );
		}
		viewState->predictingTimeStamp = cg.time;
	} else {
		pmove_t pm;
		// copy current state to pmove
		memset( &pm, 0, sizeof( pm ) );
		pm.playerState = &viewState->predictedPlayerState;

		// clear the triggered toggles for this prediction round
		memset( &cg_triggersListTriggered, false, sizeof( cg_triggersListTriggered ) );

		// run frames
		while( ++ucmdExecuted <= ucmdHead ) {
			const int64_t frame = ucmdExecuted & CMD_MASK;
			NET_GetUserCmd( (int)frame, &pm.cmd );

			ucmdReady = ( pm.cmd.serverTimeStamp != 0 );
			if( ucmdReady ) {
				viewState->predictingTimeStamp = pm.cmd.serverTimeStamp;
			}

			Pmove( cggs, &pm );

			// copy for stair smoothing
			viewState->predictedSteps[frame] = pm.step;

			if( ucmdReady ) { // hmm fixme: the wip command may not be run enough time to get proper key presses
				if( ucmdExecuted >= ucmdHead - 1 ) {
					GS_AddLaserbeamPoint( cggs, &viewState->weaklaserTrail, &viewState->predictedPlayerState, pm.cmd.serverTimeStamp );
				}

				cg_entities[viewState->predictedPlayerState.POVnum].current.weapon =
					GS_ThinkPlayerWeapon( cggs, &viewState->predictedPlayerState, pm.cmd.buttons, pm.cmd.msec, 0 );
			}

			// save for debug checking
			VectorCopy( viewState->predictedPlayerState.pmove.origin, viewState->predictedOrigins[frame] ); // store for prediction error checks

			// backup the last predicted ucmd which has a timestamp (it's closed)
			if( v_predictOptimize.get() && ucmdExecuted == ucmdHead - 1 ) {
				if( ucmdExecuted != viewState->predictFrom ) {
					viewState->predictFrom            = ucmdExecuted;
					viewState->predictFromPlayerState = viewState->predictedPlayerState;
					viewState->predictFromEntityState = cg_entities[viewState->snapPlayerState.POVnum].current;
				}
			}
		}

		viewState->predictedGroundEntity = pm.groundentity;

		// compensate for ground entity movement
		if( pm.groundentity != -1 ) {
			entity_state_t *ent = &cg_entities[pm.groundentity].current;

			if( ent->solid == SOLID_BMODEL ) {
				if( ent->linearMovement ) {
					vec3_t move;
					int64_t serverTime;

					serverTime = GS_MatchPaused( *cggs ) ? cg.frame.serverTime : cg.time + cgs.extrapolationTime;
					GS_LinearMovementDelta( ent, cg.frame.serverTime, serverTime, move );
					VectorAdd( viewState->predictedPlayerState.pmove.origin, move, viewState->predictedPlayerState.pmove.origin );
				}
			}
		}

		CG_PredictSmoothSteps();
	}
}

void CG_CenterPrint( ViewState *viewState, const char *str ) {
	if( str && *str ) {
		wsw::ui::UISystem::instance()->addStatusMessage( viewState->snapPlayerState.playerNum, wsw::StringView( str ) );
	}
}

int CG_RealClientTeam() {
	// TODO: What state should be used?
	return getOurClientViewState()->snapPlayerState.stats[STAT_REALTEAM];
}

std::optional<unsigned> CG_ActivePovOfViewState( unsigned viewStateIndex ) {
	assert( viewStateIndex <= cg.numSnapViewStates || ( cgs.demoPlaying && viewStateIndex == MAX_CLIENTS ) );
	const ViewState *viewState = cg.viewStates + viewStateIndex;
	// Don't even bother in another case
	if( const unsigned statePovNum = viewState->predictedPlayerState.POVnum; statePovNum > 0 ) {
		unsigned chosenPovNum = 0;
		if( !ISREALSPECTATOR( viewState ) ) {
			chosenPovNum = statePovNum;
		} else {
			if( statePovNum != viewState->predictedPlayerState.playerNum + 1 ) {
				chosenPovNum = statePovNum;
			}
		}
		// Protect against chasing other entities (is it a thing?)
		if( chosenPovNum && chosenPovNum < (unsigned)( cggs->maxclients + 1 ) ) {
			return chosenPovNum - 1u;
		}
	}

	return std::nullopt;
}

bool CG_IsUsingChasePov( unsigned viewStateIndex ) {
	if( !cg.frame.multipov ) {
		assert( viewStateIndex == cg.chasedViewportIndex );
		const auto &playerState = cg.viewStates[viewStateIndex].predictedPlayerState;
		return ( playerState.POVnum != playerState.playerNum + 1 ) || ( cgs.demoPlaying && CG_IsViewAttachedToPlayer() );
	}
	if( viewStateIndex == cg.chasedViewportIndex ) {
		return !( cg.ourClientViewportIndex == cg.chasedViewportIndex && cgs.playerNum == cg.chasedPlayerNum );
	}
	// Consider it true for miniviews
	return true;
}

bool CG_IsPovAlive( unsigned viewStateIndex ) {
	assert( viewStateIndex <= cg.numSnapViewStates || ( cgs.demoPlaying && viewStateIndex == MAX_CLIENTS ) );
	return !( cg.viewStates[viewStateIndex].predictedPlayerState.stats[STAT_FLAGS] & STAT_FLAG_DEADPOV );
}

bool CG_HasTwoTeams() {
	return GS_TeamBasedGametype( *cggs );
}

bool CG_CanBeReady() {
	return !ISREALSPECTATOR( getOurClientViewState() ) && GS_MatchState( *cggs ) == MATCH_STATE_WARMUP;
}

bool CG_IsReady() {
	return ( getOurClientViewState()->predictedPlayerState.stats[STAT_FLAGS] & STAT_FLAG_READY ) != 0;
}

bool CG_IsOperator() {
	return ( getOurClientViewState()->predictedPlayerState.stats[STAT_FLAGS] & STAT_FLAG_OPERATOR ) != 0;
}

bool CG_IsChallenger() {
	return ( getOurClientViewState()->predictedPlayerState.stats[STAT_FLAGS] & STAT_FLAG_CHALLENGER ) != 0;
}

int CG_MyRealTeam() {
	// TODO: What state should be used?
	return getOurClientViewState()->predictedPlayerState.stats[STAT_REALTEAM];
}

int CG_ActiveWeapon( unsigned viewStateIndex ) {
	return cg.viewStates[viewStateIndex].predictedPlayerState.stats[STAT_WEAPON];
}

bool CG_HasWeapon( unsigned viewStateIndex, int weapon ) {
	assert( (unsigned)weapon < (unsigned)WEAP_TOTAL );
	return cg.viewStates[viewStateIndex].predictedPlayerState.inventory[weapon];
}

int CG_Health( unsigned viewStateIndex ) {
	return cg.viewStates[viewStateIndex].predictedPlayerState.stats[STAT_HEALTH];
}

int CG_Armor( unsigned viewStateIndex ) {
	return cg.viewStates[viewStateIndex].predictedPlayerState.stats[STAT_ARMOR];
}

[[nodiscard]]
static auto getTeamColor( int team ) -> int {
	vec4_t color;
	CG_TeamColor( team, color );
	return COLOR_RGB( (int)( color[0] * 255 ), (int)( color[1] * 255 ), (int)( color[2] * 255 ) );
}

int CG_TeamAlphaDisplayedColor() {
	return getTeamColor( TEAM_ALPHA );
}

int CG_TeamBetaDisplayedColor() {
	return getTeamColor( TEAM_BETA );
}

auto CG_HudIndicatorState( int num ) -> BasicObjectiveIndicatorState {
	assert( (unsigned)num < 3 );
	const auto *const stats = getPrimaryViewState()->predictedPlayerState.stats;

	int anim = stats[STAT_INDICATOR_1_ANIM + num];
	if( (unsigned)anim > (unsigned)HUD_INDICATOR_ACTION_ANIM ) {
		anim = HUD_INDICATOR_NO_ANIM;
	}

	static_assert( MAX_GENERAL < std::numeric_limits<uint8_t>::max() );

	int iconNum = stats[STAT_INDICATOR_1_ICON + num];
	if( (unsigned)iconNum >= (unsigned)MAX_GENERAL ) {
		iconNum = 0;
	}
	int stringNum = stats[STAT_INDICATOR_1_STATUS_STRING + num];
	if( (unsigned)stringNum >= (unsigned)MAX_GENERAL ) {
		stringNum = 0;
	}

	const int progress = wsw::clamp<int>( stats[STAT_INDICATOR_1_PROGRESS + num], -100, +100 );
	const bool enabled = stats[STAT_INDICATOR_1_ENABLED + num] != 0;

	int packedColor = ~0;
	if( const int colorTeam = stats[STAT_INDICATOR_1_COLORTEAM + num] ) {
		if( colorTeam == TEAM_ALPHA ) {
			packedColor = CG_TeamAlphaDisplayedColor();
		} else if( colorTeam == TEAM_BETA ) {
			packedColor = CG_TeamBetaDisplayedColor();
		}
	}

	return BasicObjectiveIndicatorState {
		.color = { (uint8_t)COLOR_R( packedColor ), (uint8_t)COLOR_G( packedColor ), (uint8_t)COLOR_B( packedColor ) },
		.anim = (uint8_t)anim, .progress = (int8_t)progress, .iconNum = (uint8_t)iconNum,
		.stringNum = (uint8_t)stringNum, .enabled = enabled
	};
}

unsigned CG_GetPrimaryViewStateIndex() {
	const auto result = (unsigned)( getPrimaryViewState() - cg.viewStates );
	assert( result <= MAX_CLIENTS );
	return result;
}

unsigned CG_GetOurClientViewStateIndex() {
	const auto result = (unsigned)( getOurClientViewState() - cg.viewStates );
	assert( result <= MAX_CLIENTS );
	return result;
}

std::optional<unsigned> CG_GetPlayerNumForViewState( unsigned viewStateIndex ) {
	if( viewStateIndex < cg.numSnapViewStates ) {
		if( cg.frame.multipov ) {
			return cg.viewStates[viewStateIndex].snapPlayerState.playerNum;
		}
		const auto &playerState = cg.viewStates[viewStateIndex].predictedPlayerState;
		if( playerState.POVnum > 0 && playerState.POVnum < cggs->maxclients + 1 ) {
			return playerState.POVnum - 1;
		}
	}
	return std::nullopt;
}

bool CG_IsViewAttachedToPlayer() {
	if( CG_UsesTiledView() ) {
		return false;
	}
	if( cgs.demoPlaying && cg.isDemoCamFree ) {
		return false;
	}
	return true;
}

void CG_GetMultiviewConfiguration( std::span<const uint8_t> *pane1ViewStateNums,
								   std::span<const uint8_t> *pane2ViewStateNums,
								   std::span<const uint8_t> *tileViewStateNums,
								   std::span<const Rect> *tilePositions ) {
	*pane1ViewStateNums = cg.hudControlledMiniviewViewStateIndicesForPane[0];
	*pane2ViewStateNums = cg.hudControlledMiniviewViewStateIndicesForPane[1];
	*tileViewStateNums  = cg.tileMiniviewViewStateIndices;
	*tilePositions      = cg.tileMiniviewPositions;
}

auto CG_HudIndicatorIconPath( int iconNum ) -> std::optional<wsw::StringView> {
	assert( (unsigned)iconNum < (unsigned)MAX_GENERAL );
	if( iconNum ) {
		return cgs.configStrings.get( (unsigned)( CS_GENERAL + iconNum - 1 ) );
	}
	return std::nullopt;
}

auto CG_HudIndicatorStatusString( int stringNum ) -> std::optional<wsw::StringView> {
	assert( (unsigned)stringNum < (unsigned)MAX_GENERAL );
	if( stringNum ) {
		return cgs.configStrings.get( (unsigned)( CS_GENERAL + stringNum - 1 ) );
	}
	return std::nullopt;
}

std::pair<int, int> CG_WeaponAmmo( unsigned viewStateIndex, int weapon ) {
	const auto *weaponDef = GS_GetWeaponDef( cggs, weapon );
	const int *inventory = cg.viewStates[viewStateIndex].predictedPlayerState.inventory;
	return { inventory[weaponDef->firedef_weak.ammo_id], inventory[weaponDef->firedef.ammo_id] };
}

auto CG_GetMatchClockTime() -> std::pair<int, int> {
	int64_t clocktime, startTime, duration, curtime;
	double seconds;
	int minutes;

	if( !v_showTimer.get() ) {
		return { 0, 0 };
	}

	if( GS_MatchState( *cggs ) > MATCH_STATE_PLAYTIME ) {
		return { 0, 0 };
	}

	if( GS_RaceGametype( *cggs ) ) {
		const ViewState *viewState = getPrimaryViewState();
		if( viewState->predictedPlayerState.stats[STAT_TIME_SELF] != STAT_NOTSET ) {
			clocktime = viewState->predictedPlayerState.stats[STAT_TIME_SELF] * 100;
		} else {
			clocktime = 0;
		}
	} else if( GS_MatchClockOverride( *cggs ) ) {
		clocktime = GS_MatchClockOverride( *cggs );
	} else {
		curtime = ( GS_MatchWaiting( *cggs ) || GS_MatchPaused( *cggs ) ) ? cg.frame.serverTime : cg.time;
		duration = GS_MatchDuration( *cggs );
		startTime = GS_MatchStartTime( *cggs );

		// count downwards when having a duration
		if( duration && ( v_showTimer.get() != 3 ) ) {
			if( duration + startTime < curtime ) {
				duration = curtime - startTime; // avoid negative results

			}
			clocktime = startTime + duration - curtime;
		} else {
			if( curtime >= startTime ) { // avoid negative results
				clocktime = curtime - startTime;
			} else {
				clocktime = 0;
			}
		}
	}

	seconds = (double)clocktime * 0.001;
	minutes = (int)( seconds / 60 );
	seconds -= minutes * 60;

	return { minutes, seconds };
}

wsw::StringView CG_PlayerName( unsigned playerNum ) {
	assert( playerNum < (unsigned)MAX_CLIENTS );
	return wsw::StringView( cgs.clientInfo[playerNum].name );
}

wsw::StringView CG_PlayerClan( unsigned playerNum ) {
	assert( playerNum < (unsigned)MAX_CLIENTS );
	return wsw::StringView( cgs.clientInfo[playerNum].clan );
}

void CG_ScoresOn_f( const CmdArgs & ) {
	wsw::ui::UISystem::instance()->setScoreboardShown( true );
}

void CG_ScoresOff_f( const CmdArgs & ) {
	wsw::ui::UISystem::instance()->setScoreboardShown( false );
}

void CG_MessageMode( const CmdArgs & ) {
	wsw::ui::UISystem::instance()->toggleChatPopup();
	CL_ClearInputState();
}

void CG_MessageMode2( const CmdArgs & ) {
	wsw::ui::UISystem::instance()->toggleTeamChatPopup();
	CL_ClearInputState();
}

// Only covers the demo playback case, otherwise the UI grabs it first on its own
bool CG_GrabsMouseMovement() {
	if( CG_UsesTiledView() ) {
		return false;
	}
	if( cgs.demoPlaying ) {
		return cg.isDemoCamFree;
	}
	return true;
}

bool CG_UsesTiledView() {
	assert( cg.tileMiniviewViewStateIndices.empty() || cg.tileMiniviewViewStateIndices.size() > 1 );
	return cg.chaseMode == CAM_TILED && !cg.tileMiniviewViewStateIndices.empty();
}

void CG_SwitchToPlayerNum( unsigned playerNum ) {
	assert( cg.chaseMode == CAM_TILED );
	assert( playerNum >= 0 && playerNum < cggs->maxclients );
	cg.pendingChasedPlayerNum        = playerNum;
	cg.hasPendingSwitchFromTiledMode = true;
}

void CG_Error( const char *format, ... ) {
	char msg[1024];
	va_list argptr;

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	Com_Error( ERR_DROP, "%s\n", msg );
}

void CG_LocalPrint( const char *format, ... ) {
	va_list argptr;
	char msg[GAMECHAT_STRING_SIZE];

	va_start( argptr, format );
	const int res = Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	unsigned playerNum = cgs.playerNum;
	if( ViewState *primaryViewState = getPrimaryViewState(); primaryViewState != getOurClientViewState() ) {
		playerNum = primaryViewState->predictedPlayerState.playerNum;
	}

	const wsw::StringView msgView( msg, wsw::min( (size_t)res, sizeof( msg ) ) );
	wsw::ui::UISystem::instance()->addToMessageFeed( playerNum, msgView.trim() );

	Con_PrintSilent( msg );
}

static void *CG_GS_Malloc( size_t size ) {
	return Q_malloc( size );
}

static void CG_GS_Free( void *data ) {
	Q_free(   data );
}

static void CG_GS_Trace( trace_t *t, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, int ignore, int contentmask, int timeDelta ) {
	assert( !timeDelta );
	CG_Trace( t, start, mins, maxs, end, ignore, contentmask );
}

static int CG_GS_PointContents( const vec3_t point, int timeDelta ) {
	assert( !timeDelta );
	return CG_PointContents( point );
}

static entity_state_t *CG_GS_GetEntityState( int entNum, int deltaTime ) {
	centity_t *cent;

	if( entNum == -1 ) {
		return NULL;
	}

	assert( entNum >= 0 && entNum < MAX_EDICTS );
	cent = &cg_entities[entNum];

	if( cent->serverFrame != cg.frame.serverFrame ) {
		return NULL;
	}
	return &cent->current;
}

static const char *CG_GS_GetConfigString( int index ) {
	if( index < 0 || index >= MAX_CONFIGSTRINGS ) {
		return NULL;
	}

	if( auto maybeConfigString = cgs.configStrings.get( index ) ) {
		return maybeConfigString->data();
	}

	return nullptr;
}

static gs_state_t storageOfGS;
gs_state_t *cggs { &storageOfGS };

static void CG_InitGameShared() {
	cggs->clear();
	cggs->module = GS_MODULE_CGAME;
	cggs->maxclients = atoi( cl.configStrings.getMaxClients()->data() );
	if( cggs->maxclients < 1 || cggs->maxclients > MAX_CLIENTS ) {
		cggs->maxclients = MAX_CLIENTS;
	}

	cggs->PredictedEvent = CG_PredictedEvent;
	cggs->Error = CG_Error;
	cggs->Printf = Com_Printf;
	cggs->Malloc = CG_GS_Malloc;
	cggs->Free = CG_GS_Free;
	cggs->Trace = CG_GS_Trace;
	cggs->GetEntityState = CG_GS_GetEntityState;
	cggs->PointContents = CG_GS_PointContents;
	cggs->PMoveTouchTriggers = CG_Predict_TouchTriggers;
	cggs->GetConfigString = CG_GS_GetConfigString;

	GS_InitWeapons( cggs );
}

static void CG_RegisterWeaponModels( void ) {
	int i;

	for( i = 0; i < cgs.numWeaponModels; i++ ) {
		cgs.weaponInfos[i] = CG_RegisterWeaponModel( cgs.weaponModels[i], i );
	}

	// special case for weapon 0. Must always load the animation script
	if( !cgs.weaponInfos[0] ) {
		cgs.weaponInfos[0] = CG_CreateWeaponZeroModel( cgs.weaponModels[0] );
	}

}

static void CG_RegisterModels( void ) {
	if( cgs.precacheModelsStart == MAX_MODELS ) {
		return;
	}

	if( cgs.precacheModelsStart == 0 ) {
		const auto maybeName = cgs.configStrings.getWorldModel();
		if( maybeName ) {
			const auto name = *maybeName;
			if( !CG_LoadingItemName( name.data() ) ) {
				return;
			}
			CG_LoadingString( name.data() );
			R_RegisterWorldModel( name.data() );
		}

		CG_LoadingString( "models" );

		cgs.numWeaponModels = 1;
		Q_strncpyz( cgs.weaponModels[0], "generic/generic.md3", sizeof( cgs.weaponModels[0] ) );

		cgs.precacheModelsStart = 1;
	}

	for( unsigned i = cgs.precacheModelsStart; i < MAX_MODELS; i++ ) {
		const auto maybeName = cgs.configStrings.getModel( i );
		if( !maybeName ) {
			cgs.precacheModelsStart = MAX_MODELS;
			break;
		}

		const auto name = *maybeName;
		cgs.precacheModelsStart = i;

		if( name.startsWith( '#' ) ) {
			// special player weapon model
			if( cgs.numWeaponModels >= WEAP_TOTAL ) {
				continue;
			}

			if( !CG_LoadingItemName( name.data() ) ) {
				return;
			}

			Q_strncpyz( cgs.weaponModels[cgs.numWeaponModels], name.data() + 1, sizeof( cgs.weaponModels[cgs.numWeaponModels] ) );
			cgs.numWeaponModels++;
		} else if( name.startsWith( '$' ) ) {
			if( !CG_LoadingItemName( name.data() ) ) {
				return;
			}

			// indexed pmodel
			cgs.pModelsIndex[i] = CG_RegisterPlayerModel( name.data() + 1 );
		} else {
			if( !CG_LoadingItemName( name.data() ) ) {
				return;
			}
			cgs.modelDraw[i] = CG_RegisterModel( name.data() );
		}
	}

	if( cgs.precacheModelsStart != MAX_MODELS ) {
		return;
	}

	cgs.media.registerModels();

	CG_RegisterBasePModel(); // never before registering the weapon models
	CG_RegisterWeaponModels();

	// precache forcemodels if defined
	CG_RegisterForceModels();

	// create a tag to offset the weapon models when seen in the world as items
	VectorSet( cgs.weaponItemTag.origin, 0, 0, 0 );
	Matrix3_Copy( axis_identity, cgs.weaponItemTag.axis );
	VectorMA( cgs.weaponItemTag.origin, -14, &cgs.weaponItemTag.axis[AXIS_FORWARD], cgs.weaponItemTag.origin );
}

static void CG_RegisterSounds( void ) {
	if( cgs.precacheSoundsStart == MAX_SOUNDS ) {
		return;
	}

	if( !cgs.precacheSoundsStart ) {
		CG_LoadingString( "sounds" );

		cgs.precacheSoundsStart = 1;
	}

	for( unsigned i = cgs.precacheSoundsStart; i < MAX_SOUNDS; i++ ) {
		const auto maybeName = cgs.configStrings.getSound( i );
		if( !maybeName ) {
			cgs.precacheSoundsStart = MAX_SOUNDS;
			break;
		}

		const auto name = *maybeName;
		cgs.precacheSoundsStart = i;

		if( !name.startsWith( '*' ) ) {
			if( !CG_LoadingItemName( name.data() ) ) {
				return;
			}
			cgs.soundPrecache[i] = cg.soundSystem->registerSound( SoundSetProps {
				.name = SoundSetProps::Exact { name },
			});
		}
	}

	if( cgs.precacheSoundsStart != MAX_SOUNDS ) {
		return;
	}

	cgs.media.registerSounds();
}

static void CG_RegisterShaders( void ) {
	if( cgs.precacheShadersStart == MAX_IMAGES ) {
		return;
	}

	if( !cgs.precacheShadersStart ) {
		CG_LoadingString( "shaders" );

		cgs.precacheShadersStart = 1;
	}

	for( unsigned i = cgs.precacheShadersStart; i < MAX_IMAGES; i++ ) {
		const auto maybeName = cgs.configStrings.getImage( i );
		if( !maybeName ) {
			cgs.precacheShadersStart = MAX_IMAGES;
			break;
		}

		const auto name = *maybeName;
		cgs.precacheShadersStart = i;

		if( !CG_LoadingItemName( name.data() ) ) {
			return;
		}

		if( strstr( name.data(), "correction/" ) ) { // HACK HACK HACK -- for color correction LUTs
			cgs.imagePrecache[i] = R_RegisterLinearPic( name.data() );
		} else {
			cgs.imagePrecache[i] = R_RegisterPic( name.data() );
		}
	}

	if( cgs.precacheShadersStart != MAX_IMAGES ) {
		return;
	}

	cgs.media.registerMaterials();
}

static void CG_RegisterSkinFiles( void ) {
	if( cgs.precacheSkinsStart == MAX_SKINFILES ) {
		return;
	}

	if( !cgs.precacheSkinsStart ) {
		CG_LoadingString( "skins" );

		cgs.precacheSkinsStart = 1;
	}

	for( unsigned i = cgs.precacheSkinsStart; i < MAX_SKINFILES; i++ ) {
		const auto maybeName = cgs.configStrings.getSkinFile( i );
		if( !maybeName ) {
			cgs.precacheSkinsStart = MAX_SKINFILES;
			break;
		}

		const auto name = *maybeName;
		cgs.precacheSkinsStart = i;

		if( !CG_LoadingItemName( name.data() ) ) {
			return;
		}

		cgs.skinPrecache[i] = R_RegisterSkinFile( name.data() );
	}

	cgs.precacheSkinsStart = MAX_SKINFILES;
}

static void CG_RegisterClients( void ) {
	if( cgs.precacheClientsStart == MAX_CLIENTS ) {
		return;
	}

	if( !cgs.precacheClientsStart ) {
		CG_LoadingString( "clients" );
	}

	for( unsigned i = cgs.precacheClientsStart; i < MAX_CLIENTS; i++ ) {
		const auto maybeString = cgs.configStrings.getPlayerInfo( i );
		cgs.precacheClientsStart = i;

		if( !maybeString ) {
			continue;
		}
		if( !CG_LoadingItemName( maybeString->data() ) ) {
			return;
		}

		CG_LoadClientInfo( i, *maybeString );
	}

	cgs.precacheClientsStart = MAX_CLIENTS;
}

static void CG_RegisterLightStyles( void ) {
	for( unsigned i = 0; i < MAX_LIGHTSTYLES; i++ ) {
		if( auto maybeString = cgs.configStrings.getLightStyle( i ) ) {
			CG_SetLightStyle( i, *maybeString );
		} else {
			break;
		}
	}
}

void CG_ValidateItemDef( int tag, const char *name ) {
	const gsitem_t *item;

	item = GS_FindItemByName( cggs, name );
	if( !item ) {
		CG_Error( "Client/Server itemlist missmatch (Game and Cgame version/mod differs). Item '%s' not found\n", name );
	}

	if( item->tag != tag ) {
		CG_Error( "Client/Server itemlist missmatch (Game and Cgame version/mod differs).\n" );
	}
}

void CG_OverrideWeapondef( int index, const char *cstring ) {
	int weapon, i;
	bool strong, race;
	gs_weapon_definition_t *weapondef;
	firedef_t *firedef;

	// see G_PrecacheWeapondef, uses same operations
	weapon = index % ( MAX_WEAPONDEFS / 4 );
	race = index > ( MAX_WEAPONDEFS / 2 );
	strong = ( index % ( MAX_WEAPONDEFS / 2 ) ) > ( MAX_WEAPONDEFS / 4 );

	weapondef = GS_GetWeaponDefExt( cggs, weapon, race );
	if( !weapondef ) {
		CG_Error( "CG_OverrideWeapondef: Invalid weapon index\n" );
	}

	firedef = strong ? &weapondef->firedef : &weapondef->firedef_weak;

	i = sscanf( cstring, "%7i %7i %7u %7u %7u %7u %7u %7i %7i %7i",
				&firedef->usage_count,
				&firedef->projectile_count,
				&firedef->weaponup_time,
				&firedef->weapondown_time,
				&firedef->reload_time,
				&firedef->cooldown_time,
				&firedef->timeout,
				&firedef->speed,
				&firedef->spread,
				&firedef->v_spread
				);

	if( i != 10 ) {
		CG_Error( "CG_OverrideWeapondef: Bad configstring: %s \"%s\" (%i)\n", weapondef->name, cstring, i );
	}
}

static void CG_ValidateItemList( void ) {
	for( unsigned i = 0; i < MAX_ITEMS; i++ ) {
		if( const auto s = cgs.configStrings.getItem( i ) ) {
			CG_ValidateItemDef( i, s->data() );
		}
	}

	for( unsigned i = 0; i < MAX_WEAPONDEFS; i++ ) {
		if( const auto s = cgs.configStrings.getWeaponDef( i ) ) {
			CG_OverrideWeapondef( i, s->data() );
		}
	}
}

void CG_Precache( void ) {
	if( cgs.precacheDone ) {
		return;
	}

	cgs.precacheStart = cgs.precacheCount;
	cgs.precacheStartMsec = Sys_Milliseconds();

	CG_RegisterModels();
	if( cgs.precacheModelsStart < MAX_MODELS ) {
		return;
	}

	CG_RegisterSounds();
	if( cgs.precacheSoundsStart < MAX_SOUNDS ) {
		return;
	}

	CG_RegisterShaders();
	if( cgs.precacheShadersStart < MAX_IMAGES ) {
		return;
	}

	CG_RegisterSkinFiles();
	if( cgs.precacheSkinsStart < MAX_SKINFILES ) {
		return;
	}

	CG_RegisterClients();
	if( cgs.precacheClientsStart < MAX_CLIENTS ) {
		return;
	}

	cgs.precacheDone = true;
}

static void CG_RegisterConfigStrings( void ) {
	cgs.precacheCount = cgs.precacheTotal = 0;

	for( unsigned i = 0; i < MAX_CONFIGSTRINGS; i++ ) {
		const auto maybeString = cl.configStrings.get( i );
		if( maybeString ) {
			cgs.configStrings.set( i, *maybeString );
		} else {
			// TODO: Add clear() call or set( unsinged, std::optional... ) call?
			cgs.configStrings.set( i, wsw::StringView() );
			continue;
		}

		if( i == CS_WORLDMODEL ) {
			cgs.precacheTotal++;
		} else if( i >= CS_MODELS && i < CS_MODELS + MAX_MODELS ) {
			cgs.precacheTotal++;
		} else if( i >= CS_SOUNDS && i < CS_SOUNDS + MAX_SOUNDS ) {
			cgs.precacheTotal++;
		} else if( i >= CS_IMAGES && i < CS_IMAGES + MAX_IMAGES ) {
			cgs.precacheTotal++;
		} else if( i >= CS_SKINFILES && i < CS_SKINFILES + MAX_SKINFILES ) {
			cgs.precacheTotal++;
		} else if( i >= CS_PLAYERINFOS && i < CS_PLAYERINFOS + MAX_CLIENTS ) {
			cgs.precacheTotal++;
		}
	}

	// backup initial configstrings for CG_Reset
	cgs.baseConfigStrings.copyFrom( cgs.configStrings );

	GS_SetGametypeName( cggs, cgs.configStrings.getGametypeName().value_or( wsw::StringView() ).data() );

	CL_Cmd_ExecuteNow( va( "exec configs/client/%s.cfg silent", cggs->gametypeName ) );

	CG_SC_AutoRecordAction( getOurClientViewState(), cgs.configStrings.getAutoRecordState().value_or( wsw::StringView() ).data() );
}

void CG_StartBackgroundTrack( void ) {
	if( const auto maybeConfigString = cgs.configStrings.getAudioTrack() ) {
		if( const auto configString = *maybeConfigString; configString.length() < 256 ) {
			wsw::StaticString<256> buffer( configString );
			char intro[MAX_QPATH], loop[MAX_QPATH];
			char *string = buffer.data();
			Q_strncpyz( intro, COM_Parse( &string ), sizeof( intro ) );
			Q_strncpyz( loop, COM_Parse( &string ), sizeof( loop ) );
			if( intro[0] ) {
				cg.soundSystem->startBackgroundTrack( intro, loop, 0 );
				return;
			}
		}
	}

	if( const wsw::StringView playList = v_playList.get(); !playList.empty() ) {
		assert( playList.isZeroTerminated() );
		cg.soundSystem->startBackgroundTrack( playList.data(), NULL, v_playListShuffle.get() ? 1 : 0 );
	}
}

void CG_ClearEffects() {
	// TODO: Should we reset localEffects[] stuff as well
	for( int entNum = 0; entNum < MAX_EDICTS; ++entNum ) {
		cg.effectsSystem.resetEntityEffects( entNum );
	}

	// TODO: calling effectsSystem.clear() should be sufficient (it is not in the current codebase state)
	cg.effectsSystem.clear();
	cg.particleSystem.clear();
	cg.polyEffectsSystem.clear();
	cg.simulatedHullsSystem.clear();
}

void CG_Reset( void ) {
	cgs.configStrings.copyFrom( cgs.baseConfigStrings );

	CG_ResetClientInfos();

	CG_ResetPModels();

	std::memset( cg.viewStates, 0, sizeof( ViewState ) );

	CG_ResetItemTimers();

	wsw::ui::UISystem::instance()->resetHudFeed();

	CG_ClearEffects();

	// start up announcer events queue from clean
	CG_ClearAnnouncerEvents();

	CG_ClearInputState();

	CG_ClearChaseCam();

	cg.time = 0;
	cg.realTime = 0;
	cg.helpmessage_time = 0;
	cg.motd_time = 0;

	memset( cg_entities, 0, sizeof( cg_entities ) );
}

void CG_InitPersistentState() {
	//team models
	cg_teamPLAYERSmodel = Cvar_Get( "cg_teamPLAYERSmodel", DEFAULT_TEAMPLAYERS_MODEL, CVAR_ARCHIVE );
	cg_teamPLAYERSmodelForce = Cvar_Get( "cg_teamPLAYERSmodelForce", "0", CVAR_ARCHIVE );
	cg_teamPLAYERSskin = Cvar_Get( "cg_teamPLAYERSskin", DEFAULT_PLAYERSKIN, CVAR_ARCHIVE );
	cg_teamPLAYERScolor = Cvar_Get( "cg_teamPLAYERScolor", DEFAULT_TEAMBETA_COLOR, CVAR_ARCHIVE );
	cg_teamPLAYERScolorForce = Cvar_Get( "cg_teamPLAYERScolorForce", "0", CVAR_ARCHIVE );

	cg_teamPLAYERSmodel->modified = true;
	cg_teamPLAYERSmodelForce->modified = true;
	cg_teamPLAYERSskin->modified = true;
	cg_teamPLAYERScolor->modified = true;
	cg_teamPLAYERScolorForce->modified = true;

	cg_teamALPHAmodel = Cvar_Get( "cg_teamALPHAmodel", DEFAULT_TEAMALPHA_MODEL, CVAR_ARCHIVE );
	cg_teamALPHAmodelForce = Cvar_Get( "cg_teamALPHAmodelForce", "1", CVAR_ARCHIVE );
	cg_teamALPHAskin = Cvar_Get( "cg_teamALPHAskin", DEFAULT_PLAYERSKIN, CVAR_ARCHIVE );
	cg_teamALPHAcolor = Cvar_Get( "cg_teamALPHAcolor", DEFAULT_TEAMALPHA_COLOR, CVAR_ARCHIVE );

	cg_teamALPHAmodel->modified = true;
	cg_teamALPHAmodelForce->modified = true;
	cg_teamALPHAskin->modified = true;
	cg_teamALPHAcolor->modified = true;

	cg_teamBETAmodel = Cvar_Get( "cg_teamBETAmodel", DEFAULT_TEAMBETA_MODEL, CVAR_ARCHIVE );
	cg_teamBETAmodelForce = Cvar_Get( "cg_teamBETAmodelForce", "1", CVAR_ARCHIVE );
	cg_teamBETAskin = Cvar_Get( "cg_teamBETAskin", DEFAULT_PLAYERSKIN, CVAR_ARCHIVE );
	cg_teamBETAcolor = Cvar_Get( "cg_teamBETAcolor", DEFAULT_TEAMBETA_COLOR, CVAR_ARCHIVE );

	cg_teamBETAmodel->modified = true;
	cg_teamBETAmodelForce->modified = true;
	cg_teamBETAskin->modified = true;
	cg_teamBETAcolor->modified = true;

	CG_CheckSharedCrosshairState( true );
	CG_InitTemporaryBoneposesCache();
}

void CG_Init( SoundSystem *soundSystem, const char *serverName, unsigned int playerNum,
			  int vidWidth, int vidHeight, int pixelRatio,
			  bool demoplaying, const char *demoName, bool pure,
			  unsigned snapFrameTime, int protocol, const char *demoExtension,
			  int sharedSeed, bool gameStart ) {
	CG_InitGameShared();

	// Hacks, see below
	cg.~cg_state_t();
	memset( &cg, 0, sizeof( cg_state_t ) );
	new( &cg )cg_state_t;
	cg.soundSystem = soundSystem;

	// Hacks, see a related comment in the client/ code
	cgs.~cg_static_t();
	memset( (void *)&cgs, 0, sizeof( cg_static_t ) );
	new( &cgs )cg_static_t;

	memset( cg_entities, 0, sizeof( cg_entities ) );

	srand( time( NULL ) );

	// save server name
	cgs.serverName = Q_strdup( serverName );

	// save local player number
	cgs.playerNum = playerNum;

	// save current width and height
	cgs.vidWidth = vidWidth;
	cgs.vidHeight = vidHeight;
	cgs.pixelRatio = pixelRatio;

	// demo
	cgs.demoPlaying = demoplaying == true;
	cgs.demoName = demoName;
	Q_strncpyz( cgs.demoExtension, demoExtension, sizeof( cgs.demoExtension ) );

	// whether to only allow pure files
	cgs.pure = pure == true;

	// game protocol number
	cgs.gameProtocol = protocol;
	cgs.snapFrameTime = snapFrameTime;

	cgs.fullclipShaderNum = CM_ShaderrefForName( cl.cms, "textures/common/fullclip" );

	CG_InitInput();

	CG_PModelsInit();
	CG_WModelsInit();

	CG_InitCrosshairs();

	CG_ClearLightStyles();

	// get configstrings
	CG_RegisterConfigStrings();

	// register fonts here so loading screen works
	CG_RegisterFonts();
	cgs.shaderWhite = R_RegisterPic( "$whiteimage" );

	CG_RegisterLevelMinimap();

	CG_RegisterCGameCommands();
	CG_RegisterLightStyles();

	CG_ValidateItemList();

	CG_LoadingString( "" );

	CG_ClearChaseCam();

	// start up announcer events queue from clean
	CG_ClearAnnouncerEvents();

	cgs.demoTutorial = cgs.demoPlaying && ( strstr( cgs.demoName, "tutorials/" ) != NULL );

	cg.firstFrame = true; // think of the next frame in CG_NewFrameSnap as of the first one

	// now that we're done with precaching, let the autorecord actions do something
	CG_ConfigString( CS_AUTORECORDSTATE, cgs.configStrings.get( CS_AUTORECORDSTATE ).value_or( wsw::StringView() ) );

	CG_DemocamInit();
}

void CG_Shutdown( void ) {
	wsw::ui::UISystem::instance()->resetHudFeed();
	CG_DemocamShutdown();
	CG_UnregisterCGameCommands();
	CG_ShutdownCrosshairs();
	CG_WModelsShutdown();
	CG_PModelsShutdown();
	CG_FreeTemporaryBoneposesCache();
	CG_ShutdownInput();
	CG_ShutdownSoundAndMiscState();
}
