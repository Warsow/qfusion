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
// cl_main.c  -- client main loop

#include "client.h"

#include <common/version.h>
#include <common/common.h>
#include <common/local.h>
#include <common/facilities/asyncstream.h>
#include <common/facilities/cmdsystem.h>
#include <common/facilities/protocol.h>
#include <common/helpers/mmuuid.h>
#include <common/helpers/singletonholder.h>
#include <common/helpers/pipeutils.h>
#include <common/facilities/maplist.h>
#include <common/helpers/hash.h>
#include <common/facilities/profilerscope.h>
#include <common/types/q_trie.h>
#include <common/helpers/textstreamwriterextras.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/tonum.h>
#include <common/facilities/wswfs.h>
#include <common/facilities/wswcurl.h>
#include <common/facilities/sysclock.h>
#include <common/facilities/syspublic.h>
#include "ui/uisystem.h"
#include <server/server.h>

#include "serverlist.h"

#include <ctime>
#include <random>
#include <unordered_map>

using wsw::operator""_asView;

static cvar_t *rcon_client_password;
static cvar_t *rcon_address;

static cvar_t *cl_timeout;
static cvar_t *cl_maxfps;
static cvar_t *cl_sleep;
static cvar_t *cl_pps;
static cvar_t *cl_shownet;

static cvar_t *cl_extrapolationTime;
static cvar_t *cl_extrapolate;

static cvar_t *cl_timedemo;

static cvar_t *info_password;
static cvar_t *cl_infoservers;

// wsw : debug netcode
static cvar_t *cl_debug_serverCmd;
static cvar_t *cl_debug_timeDelta;

static cvar_t *cl_downloads;
static cvar_t *cl_downloads_from_web;
static cvar_t *cl_downloads_from_web_timeout;
static cvar_t *cl_download_allow_modules;

static cvar_t *cl_checkForUpdate;
static cvar_t *cl_checkForUpdateTimeout;

static char cl_nextString[MAX_STRING_CHARS];
static char cl_connectChain[MAX_STRING_CHARS];

client_static_t cls;
client_state_t cl;

static entity_state_t cl_baselines[MAX_EDICTS];

static bool cl_initialized = false;
static bool in_initialized = false;

static async_stream_module_t *cl_async_stream;

cvar_t *cl_ucmdMaxResend;
cvar_t *cl_ucmdFPS;

static int precache_check; // for autodownload of precache items
static int precache_spawncount;
static int precache_tex;
static int precache_pure;

static cvar_t *s_module = nullptr;

SoundSystem *SoundSystem::s_instance = nullptr;

static unsigned vid_num_modes;
static unsigned vid_max_width_mode_index;
static unsigned vid_max_height_mode_index;
static vidmode_t *vid_modes;

static cvar_t *vid_width, *vid_height;
static cvar_t *vid_pixelRatio;
cvar_t *vid_xpos;          // X coordinate of window position
cvar_t *vid_ypos;          // Y coordinate of window position
cvar_t *vid_fullscreen;
static cvar_t *vid_borderless;
cvar_t *vid_displayfrequency;
cvar_t *vid_multiscreen_head;
static cvar_t *vid_parentwid;      // parent window identifier
cvar_t *win_noalttab;
cvar_t *win_nowinkeys;

cvar_t *cl_multithreading;

// Global variables used internally by this module
viddef_t viddef;             // global video state; used by other modules

static int vid_ref_prevwidth, vid_ref_prevheight;
static bool vid_ref_modified;
static bool vid_ref_verbose;
static bool vid_ref_sound_restart;
static bool vid_ref_active;
static bool vid_initialized;
static bool vid_app_active;
static bool vid_app_minimized;

static float scr_con_current;    // aproaches scr_conlines at scr_conspeed
static float scr_conlines;       // 0.0 to 1.0 lines of console to display

static bool scr_initialized;    // ready to draw

static int scr_draw_loading;

static cvar_t *scr_consize;
static cvar_t *scr_conspeed;

#define PLAYER_MULT 5

// ENV_CNT is map load
#define ENV_CNT ( CS_PLAYERINFOS + MAX_CLIENTS * PLAYER_MULT )
#define TEXTURE_CNT ( ENV_CNT + 1 )

//extern qbufPipe_s *g_svCmdPipe;
//extern qbufPipe_s *g_clCmdPipe;

static void *cge = nullptr;

static uint8_t g_netchanCompressionBuffer[MAX_MSGLEN];
static netchan_t g_netchanInstanceBackup;

static int64_t oldKeyboardTimestamp;
static int64_t oldMouseTimestamp;

// These are system specific functions
// wrapper around R_Init
rserr_t VID_Sys_Init( const char *applicationName, const char *screenshotsPrefix, int startupColor, const int *iconXPM,
					  void *parentWindow, bool verbose );
void VID_UpdateWindowPosAndSize( int x, int y );
void VID_EnableAltTab( bool enable );
void VID_EnableWinKeys( bool enable );

static void CL_InitServerDownload( const char *filename, size_t size, unsigned checksum, bool allow_localhttpdownload,
								   const char *url, bool initial );
void CL_StopServerDownload( void );

int CG_NumInlineModels( void ) {
	return CM_NumInlineModels( cl.cms );
}

int CG_TransformedPointContents( const vec3_t p, const cmodel_s *cmodel, const vec3_t origin, const vec3_t angles ) {
	return CM_TransformedPointContents( cl.cms, p, cmodel, origin, angles );
}

void CG_TransformedBoxTrace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins, const vec3_t maxs,
							 const cmodel_s *cmodel, int brushmask, const vec3_t origin, const vec3_t angles ) {
	CM_TransformedBoxTrace( cl.cms, tr, start, end, mins, maxs, cmodel, brushmask, origin, angles );
}

const cmodel_s *CG_InlineModel( int num ) {
	return CM_InlineModel( cl.cms, num );
}

void CG_InlineModelBounds( const cmodel_s *cmodel, vec3_t mins, vec3_t maxs ) {
	CM_InlineModelBounds( cl.cms, cmodel, mins, maxs );
}

const cmodel_s *CG_ModelForBBox( const vec3_t mins, const vec3_t maxs ) {
	return CM_ModelForBBox( cl.cms, mins, maxs );
}

const cmodel_s *CG_OctagonModelForBBox( const vec3_t mins, const vec3_t maxs ) {
	return CM_OctagonModelForBBox( cl.cms, mins, maxs );
}

void NET_GetUserCmd( int frame, usercmd_t *cmd ) {
	if( cmd ) {
		if( frame < 0 ) {
			frame = 0;
		}
		*cmd = cl.cmds[frame & CMD_MASK];
	}
}

int NET_GetCurrentUserCmdNum( void ) {
	return cls.ucmdHead;
}

void NET_GetCurrentState( int64_t *incomingAcknowledged, int64_t *outgoingSequence, int64_t *outgoingSent ) {
	if( incomingAcknowledged ) {
		*incomingAcknowledged = cls.ucmdAcknowledged;
	}
	if( outgoingSequence ) {
		*outgoingSequence = cls.ucmdHead;
	}
	if( outgoingSent ) {
		*outgoingSent = cls.ucmdSent;
	}
}

void CL_GameModule_Init( void ) {
	// stop all playing sounds
	SoundSystem::instance()->stopSounds( SoundSystem::StopMusic | SoundSystem::RetainLocal );

	CL_GameModule_Shutdown();

	SCR_EnableQuickMenu( false );

	const int64_t start = Sys_Milliseconds();
	CG_Init( cls.servername, cl.playernum,
			 viddef.width, viddef.height, VID_GetPixelRatio(),
			 cls.demoPlayer.playing, cls.demoPlayer.playing ? cls.demoPlayer.filename : "",
			 cls.sv_pure, cl.snapFrameTime, APP_PROTOCOL_VERSION, APP_DEMO_EXTENSION_STR,
			 cls.mediaRandomSeed, cl.gamestart );

	cge = (void *)1;

	Com_DPrintf( "CL_GameModule_Init: %.2f seconds\n", (float)( Sys_Milliseconds() - start ) * 0.001f );

	cl.gamestart = false;
	cls.cgameActive = true;
}

void CL_GameModule_Reset( void ) {
	if( cge ) {
		CG_Reset();
	}
}

void CL_GameModule_Shutdown( void ) {
	if( cge ) {
		cls.cgameActive = false;
		CG_Shutdown();
		cge = NULL;
	}
}

void CL_GameModule_ConfigString( int number, const wsw::StringView &string ) {
	if( cge ) {
		CG_ConfigString( number, string );
	}
}

bool CL_GameModule_NewSnapshot( int pendingSnapshot ) {
	if( cge ) {
		snapshot_t *currentSnap = ( cl.currentSnapNum <= 0 ) ? NULL : &cl.snapShots[cl.currentSnapNum & UPDATE_MASK];
		snapshot_t *newSnap = &cl.snapShots[pendingSnapshot & UPDATE_MASK];
		return CG_NewFrameSnap( newSnap, currentSnap );
	}

	return false;
}

std::pair<bool, bool> CL_GameModule_RenderView() {

	return { false, false };
}

void CL_GameModule_InputFrame( int64_t inputTimestamp, int keyboardDeltaMillis, float mouseDeltaMillis ) {
	if( cge ) {
		CG_InputFrame( inputTimestamp, keyboardDeltaMillis, mouseDeltaMillis );
	}
}

void CL_GameModule_ClearInputState( void ) {
	if( cge ) {
		CG_ClearInputState();
	}
}

bool CL_GameModule_GrabsMouseMovement() {
	if( cge ) {
		return CG_GrabsMouseMovement();
	}
	return false;
}

unsigned CL_GameModule_GetButtonBits( void ) {
	if( cge ) {
		return CG_GetButtonBits();
	}
	return 0;
}

void CL_GameModule_AddViewAngles( vec3_t viewAngles ) {
	if( cge ) {
		CG_AddViewAngles( viewAngles );
	}
}

void CL_GameModule_AddMovement( vec3_t movement ) {
	if( cge ) {
		CG_AddMovement( movement );
	}
}

void CL_GameModule_MouseMove( int dx, int dy ) {
	if( cge ) {
		CG_MouseMove( dx, dy );
	}
}

bool CG_HasKeyboardFocus() {
	return cge && !Con_HasKeyboardFocus() && !wsw::ui::UISystem::instance()->grabsKeyboardAndMouseButtons();
}

static void CL_CreateNewUserCommand( int realMsec );

void CL_ClearInputState( void ) {
	wsw::cl::KeyHandlingSystem::instance()->clearStates();

	if( CG_HasKeyboardFocus() ) {
		CL_GameModule_ClearInputState();
	}
}

/*
* CL_UpdateGameInput
*
* Notifies cgame of new frame, refreshes input timings, coordinates and angles
*/
static void CL_UpdateGameInput( int64_t inputTimestamp, int keyboardDeltaMillis, float mouseDeltaMillis ) {
	int mx, my;
	IN_GetMouseMovement( &mx, &my );

	// refresh input in cgame
	CL_GameModule_InputFrame( inputTimestamp, keyboardDeltaMillis, mouseDeltaMillis );

	bool handledByCGame = false;
	auto *const uiSystem = wsw::ui::UISystem::instance();
	if( !uiSystem->grabsMouseMovement() ) {
		if( CL_GameModule_GrabsMouseMovement() ) {
			CL_GameModule_MouseMove( mx, my );
			CL_GameModule_AddViewAngles( cl.viewangles );
			handledByCGame = true;
		}
	}

	if( !handledByCGame ) {
		// Let the UI handle it for non-modal stuff like the demo player bar
		(void)uiSystem->handleMouseMovement( mouseDeltaMillis, mx, my );
	}
}

void CL_UserInputFrame( int64_t inputTimestamp, int keyboardDeltaMillis, float mouseDeltaMillis, int realMsec ) {
	// let the mouse activate or deactivate
	IN_Frame();

	// get new key events
	Sys_SendKeyEvents();

	// get new key events from mice or external controllers
	IN_Commands();

	// refresh mouse angles and movement velocity
	CL_UpdateGameInput( inputTimestamp, keyboardDeltaMillis, mouseDeltaMillis );

	// create a new usercmd_t structure for this frame
	CL_CreateNewUserCommand( realMsec );
}

void CL_InitInput( void ) {
	if( !in_initialized ) {
		CL_Cmd_Register( "in_restart"_asView, IN_Restart );

		IN_Init();

		cl_ucmdMaxResend =  Cvar_Get( "cl_ucmdMaxResend", "3", CVAR_ARCHIVE );
		cl_ucmdFPS =        Cvar_Get( "cl_ucmdFPS", "62", CVAR_DEVELOPER );

		in_initialized = true;
	}
}

void CL_ShutdownInput( void ) {
	if( in_initialized ) {
		CL_Cmd_Unregister( "in_restart"_asView );

		IN_Shutdown();

		in_initialized = false;
	}
}

static void CL_SetUcmdMovement( usercmd_t *ucmd ) {
	vec3_t movement = { 0.0f, 0.0f, 0.0f };

	if( CG_HasKeyboardFocus() ) {
		CL_GameModule_AddMovement( movement );
	}

	ucmd->sidemove    = bound( -127, (int)(movement[0] * 127.0f), 127 );
	ucmd->forwardmove = bound( -127, (int)(movement[1] * 127.0f), 127 );
	ucmd->upmove      = bound( -127, (int)(movement[2] * 127.0f), 127 );
}

static void CL_SetUcmdButtons( usercmd_t *ucmd ) {
	if( CG_HasKeyboardFocus() ) {
		ucmd->buttons |= CL_GameModule_GetButtonBits();
		if( wsw::cl::KeyHandlingSystem::instance()->isAnyKeyDown() ) {
			ucmd->buttons |= BUTTON_ANY;
		}
	} else {
		// add chat/console/ui icon as a button
		ucmd->buttons |=  BUTTON_BUSYICON;
	}
}

static void CL_RefreshUcmd( usercmd_t *ucmd, int msec, bool ready ) {
	ucmd->msec += msec;

	if( ucmd->msec ) {
		CL_SetUcmdMovement( ucmd );

		CL_SetUcmdButtons( ucmd );
	}

	if( ready ) {
		ucmd->serverTimeStamp = cl.serverTime; // return the time stamp to the server

		if( cl.cmdNum > 0 ) {
			ucmd->msec = ucmd->serverTimeStamp - cl.cmds[( cl.cmdNum - 1 ) & CMD_MASK].serverTimeStamp;
		} else {
			ucmd->msec = 20;
		}
		if( ucmd->msec < 1 ) {
			ucmd->msec = 1;
		}
	}

	ucmd->angles[0] = ANGLE2SHORT( cl.viewangles[0] );
	ucmd->angles[1] = ANGLE2SHORT( cl.viewangles[1] );
	ucmd->angles[2] = ANGLE2SHORT( cl.viewangles[2] );
}

void CL_WriteUcmdsToMessage( msg_t *msg ) {
	// TODO: Convert to assertions?
	if( !msg || cls.state < CA_ACTIVE || cls.demoPlayer.playing ) {
		return;
	}

	// find out what ucmds we have to send
	unsigned ucmdFirst = cls.ucmdAcknowledged + 1;
	const unsigned ucmdHead = cl.cmdNum + 1;

	if( cl_ucmdMaxResend->integer > CMD_BACKUP * 0.5 ) {
		Cvar_SetValue( "cl_ucmdMaxResend", CMD_BACKUP * 0.5 );
	} else if( cl_ucmdMaxResend->integer < 1 ) {
		Cvar_SetValue( "cl_ucmdMaxResend", 1 );
	}

	// find what is our resend count (resend doesn't include the newly generated ucmds)
	// and move the start back to the resend start
	unsigned resendCount;
	if( ucmdFirst <= cls.ucmdSent + 1 ) {
		resendCount = 0;
	} else {
		resendCount = ( cls.ucmdSent + 1 ) - ucmdFirst;
	}
	if( resendCount > (unsigned int)cl_ucmdMaxResend->integer ) {
		resendCount = (unsigned int)cl_ucmdMaxResend->integer;
	}

	if( ucmdFirst > ucmdHead ) {
		ucmdFirst = ucmdHead;
	}

	// if this happens, the player is in a freezing lag. Send him the less possible data
	if( ( ucmdHead - ucmdFirst ) + resendCount > CMD_MASK * 0.5 ) {
		resendCount = 0;
	}

	// move the start backwards to the resend point
	ucmdFirst = ( ucmdFirst > resendCount ) ? ucmdFirst - resendCount : ucmdFirst;

	if( ( ucmdHead - ucmdFirst ) > CMD_MASK ) { // ran out of updates, seduce the send to try to recover activity
		ucmdFirst = ucmdHead - 3;
	}

	// begin a client move command
	MSG_WriteUint8( msg, clc_move );

	// (acknowledge server frame snap)
	// let the server know what the last frame we
	// got was, so the next message can be delta compressed
	if( cl.receivedSnapNum <= 0 ) {
		MSG_WriteInt32( msg, -1 );
	} else {
		MSG_WriteInt32( msg, cl.snapShots[cl.receivedSnapNum & UPDATE_MASK].serverFrame );
	}

	// Write the actual ucmds

	// write the id number of first ucmd to be sent, and the count
	MSG_WriteInt32( msg, ucmdHead );
	MSG_WriteUint8( msg, (uint8_t)( ucmdHead - ucmdFirst ) );

	// write the ucmds
	unsigned i;
	for( i = ucmdFirst; i < ucmdHead; i++ ) {
		if( i == ucmdFirst ) { // first one isn't delta-compressed
			usercmd_t *cmd = &cl.cmds[i & CMD_MASK];
			usercmd_t nullcmd;
			memset( &nullcmd, 0, sizeof( nullcmd ) );
			MSG_WriteDeltaUsercmd( msg, &nullcmd, cmd );
		} else {   // delta compress to previous written
			usercmd_t *cmd = &cl.cmds[i & CMD_MASK];
			usercmd_t *oldcmd = &cl.cmds[( i - 1 ) & CMD_MASK];
			MSG_WriteDeltaUsercmd( msg, oldcmd, cmd );
		}
	}

	cls.ucmdSent = i;
}

static bool CL_NextUserCommandTimeReached( int realMsec ) {
	static int minMsec = 1, allMsec = 0, extraMsec = 0;
	static float roundingMsec = 0.0f;

	float maxucmds;
	if( cls.state < CA_ACTIVE ) {
		maxucmds = 10; // reduce ratio while connecting
	} else {
		maxucmds = cl_ucmdFPS->value;
	}

	// the cvar is developer only
	//clamp( maxucmds, 10, 90 ); // don't let people abuse cl_ucmdFPS

	if( cls.demoPlayer.playing ) {
		minMsec = 1;
	} else {
		minMsec = ( 1000.0f / maxucmds );
		roundingMsec += ( 1000.0f / maxucmds ) - minMsec;
	}

	if( roundingMsec >= 1.0f ) {
		minMsec += (int)roundingMsec;
		roundingMsec -= (int)roundingMsec;
	}

	if( minMsec > extraMsec ) { // remove, from min frametime, the extra time we spent in last frame
		minMsec -= extraMsec;
	}

	allMsec += realMsec;
	if( allMsec < minMsec ) {
		return false;
	} else {
		extraMsec = allMsec - minMsec;
		if( extraMsec > minMsec ) {
			extraMsec = minMsec - 1;
		}

		allMsec = 0;
		// send a new user command message to the server
		return cls.state >= CA_ACTIVE;
	}
}

static void CL_CreateNewUserCommand( int realMsec ) {
	if( !CL_NextUserCommandTimeReached( realMsec ) ) {
		// refresh current command with up to date data for movement prediction
		CL_RefreshUcmd( &cl.cmds[cls.ucmdHead & CMD_MASK], realMsec, false );
	} else {
		cl.cmdNum = cls.ucmdHead;
		cl.cmd_time[cl.cmdNum & CMD_MASK] = cls.realtime;

		usercmd_t *ucmd = &cl.cmds[cl.cmdNum & CMD_MASK];

		CL_RefreshUcmd( ucmd, realMsec, true );

		// advance head and init the new command
		cls.ucmdHead++;
		ucmd = &cl.cmds[cls.ucmdHead & CMD_MASK];
		memset( ucmd, 0, sizeof( usercmd_t ) );

		// start up with the most recent viewangles
		CL_RefreshUcmd( ucmd, 0, false );
	}
}

auto SoundSystem::getPathForName( const wsw::StringView &name ) -> wsw::PodVector<char> {
	wsw::PodVector<char> result;
	if( name.empty() ) {
		result.append( '\0' );
		return result;
	}

	result.append( name );
	result.append( '\0' );
	if( wsw::fs::getExtension( name ) != std::nullopt ) {
		return result;
	}

	wsw::StaticVector<wsw::StringView, 4> allowedExtensions;
	for( const char *ext: SOUND_EXTENSIONS ) {
		allowedExtensions.push_back( wsw::StringView( ext ) );
	}

	const wsw::StringView ztName( result.data(), result.size() - 1, wsw::StringView::ZeroTerminated );
	if( auto maybeNameAndExtension = wsw::fs::findFirstExtension( ztName, allowedExtensions.begin(), allowedExtensions.end() ) ) {
		result.pop_back();
		result.append( maybeNameAndExtension->second );
		result.append( '\0' );
	}

	return result;
}

// TODO: We need generic FS facilities for things like this
bool SoundSystem::getPathListForPattern( const wsw::StringView &pattern, wsw::StringSpanStorage<unsigned, unsigned> *pathListStorage ) {
	pathListStorage->clear();

	wsw::StringView dirName;
	wsw::StringView extension;
	wsw::StringView baseName = pattern;

	const std::optional<unsigned> dotIndex   = pattern.indexOf( '.' );
	const std::optional<unsigned> slashIndex = pattern.lastIndexOf( '/' );

	if( dotIndex != std::nullopt ) {
		if( slashIndex == std::nullopt || *slashIndex < *dotIndex ) {
			extension = pattern.drop( *dotIndex + 1 );
			baseName  = baseName.take( *dotIndex );
		}
	}
	if( slashIndex != std::nullopt ) {
		dirName  = pattern.take( *slashIndex );
		baseName = baseName.drop( *slashIndex + 1 );
	} else {
		dirName = wsw::StringView( "/" );
	}

	// For now, we limit patterns to basename
	wsw::PodVector<char> ztPattern( baseName.data(), baseName.size() );
	ztPattern.append( '\0' );
	wsw::PodVector<char> ztBaseName;

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeSearchResult = searchResultHolder.findDirFiles( dirName, extension ) ) {
		for( const wsw::StringView &fileName: *maybeSearchResult ) {
			bool isAcceptedByPattern = false;
			if( const std::optional<wsw::StringView> maybeBaseName = wsw::fs::stripExtension( fileName ) ) {
				ztBaseName.assign( maybeBaseName->data(), maybeBaseName->size() );
				ztBaseName.append( '\0' );
				if( Com_GlobMatch( ztPattern.data(), ztBaseName.data(), false ) ) {
					isAcceptedByPattern = true;
				}
			}
			if( isAcceptedByPattern ) {
				wsw::StaticString<MAX_QPATH> fullNameBuffer;
				fullNameBuffer << dirName << '/' << fileName;
				if( extension.empty() ) {
					if( const char *foundExtension = FS_FirstExtension( fullNameBuffer.data(), SOUND_EXTENSIONS, std::size( SOUND_EXTENSIONS ) ) ) {
						if( const char *existingExtension = COM_FileExtension( fullNameBuffer.data() ) ) {
							fullNameBuffer.erase( (unsigned)( existingExtension - fullNameBuffer.data() ) );
							fullNameBuffer << wsw::StringView( foundExtension );
						}
						bool alreadyPresent = false;
						for( const wsw::StringView &addedView : *pathListStorage ) {
							if( addedView.equalsIgnoreCase( fullNameBuffer.asView() ) ) {
								alreadyPresent = true;
								break;
							}
						}
						if( !alreadyPresent ) {
							pathListStorage->add( fullNameBuffer.asView() );
						}
					}
				} else {
					pathListStorage->add( fullNameBuffer.asView() );
				}
			}
		}
	}

	return !pathListStorage->empty();
}

void CL_SoundModule_Init( bool verbose ) {
	if( !s_module ) {
		s_module = Cvar_Get( "s_module", "1", CVAR_LATCH_SOUND );
	}

	// unload anything we have now
	CL_SoundModule_Shutdown( verbose );

	Cvar_GetLatchedVars( CVAR_LATCH_SOUND );

	if( s_module->integer < 0 || s_module->integer > 1 ) {
		clNotice() << "Invalid value for s_module" << s_module->integer << "reseting to default\n";
		Cvar_ForceSet( s_module->name, "1" );
	}

	const SoundSystem::InitOptions options { .verbose = verbose, .useNullSystem = !s_module->integer };
	// TODO: Is the HWND really needed?
	if( !SoundSystem::init( &cl, options ) ) {
		Cvar_ForceSet( s_module->name, "0" );
	}
}

void CL_SoundModule_Shutdown( bool verbose ) {
	SoundSystem::shutdown( verbose );
}

/*
** VID_Restart_f
*
* Console command to re-start the video mode and refresh DLL. We do this
* simply by setting the vid_ref_modified variable, which will
* cause the entire video mode and refresh DLL to be reset on the next frame.
*/
void VID_Restart( bool verbose, bool soundRestart ) {
	vid_ref_modified = true;
	vid_ref_verbose = verbose;
	vid_ref_sound_restart = soundRestart;
}

void VID_Restart_f( const CmdArgs &cmdArgs ) {
	VID_Restart( ( Cmd_Argc() >= 2 ? true : false ), false );
}

bool VID_GetModeInfo( int *width, int *height, unsigned int mode ) {
	if( mode < vid_num_modes ) {
		*width  = vid_modes[mode].width;
		*height = vid_modes[mode].height;
		return true;
	}
	return false;
}

static void VID_ModeList_f( const CmdArgs & ) {
	for( unsigned i = 0; i < vid_num_modes; i++ ) {
		Com_Printf( "* %ix%i\n", vid_modes[i].width, vid_modes[i].height );
	}
}

static rserr_t VID_Sys_Init_( void *parentWindow, bool verbose ) {
	return VID_Sys_Init( APPLICATION_UTF8, APP_SCREENSHOTS_PREFIX, APP_STARTUP_COLOR, nullptr, parentWindow, verbose );
}

void VID_AppActivate( bool active, bool minimize, bool destroy ) {
	vid_app_active = active;
	vid_app_minimized = minimize;
	RF_AppActivate( active, minimize, destroy );
}

bool VID_AppIsActive( void ) {
	return vid_app_active;
}

bool VID_AppIsMinimized( void ) {
	return vid_app_minimized;
}

bool VID_RefreshIsActive( void ) {
	return vid_ref_active;
}

int VID_GetWindowWidth( void ) {
	return viddef.width;
}

int VID_GetWindowHeight( void ) {
	return viddef.height;
}

int VID_GetPixelRatio( void ) {
	if( int value = vid_pixelRatio->integer; value > 0 ) {
		return wsw::clamp( value, 1, 2 );
	}
	return wsw::clamp( Sys_GetPixelRatio(), 1, 2 );
}

static rserr_t VID_ChangeMode( void ) {
	vid_fullscreen->modified = false;

	const int frequency = vid_displayfrequency->integer;

	VidModeOptions options { .fullscreen = vid_fullscreen->integer != 0, .borderless = vid_borderless->integer != 0 };

	int x, y, w, h;
	if( options.fullscreen && options.borderless ) {
		x = 0, y = 0;
		if( !VID_GetDefaultMode( &w, &h ) ) {
			w = vid_modes[0].width;
			h = vid_modes[0].height;
		}
	} else {
		x = vid_xpos->integer;
		y = vid_ypos->integer;
		w = vid_width->integer;
		h = vid_height->integer;
	}

	if( vid_ref_active && ( w != (int)viddef.width || h != (int)viddef.height ) ) {
		return rserr_restart_required;
	}

	rserr_t err = R_TrySettingMode( x, y, w, h, frequency, options );
	if( err == rserr_restart_required ) {
		return err;
	}

	if( err == rserr_ok ) {
		// store fallback mode
		vid_ref_prevwidth = w;
		vid_ref_prevheight = h;
	} else {
		/* Try to recover from all possible kinds of mode-related failures.
		 *
		 * rserr_invalid_fullscreen may be returned only if fullscreen is requested, but at this
		 * point the system may not be totally sure whether the requested mode is windowed-only
		 * or totally unsupported, so there's a possibility of rserr_invalid_mode as well.
		 *
		 * However, the previously working mode may be windowed-only, but the user may request
		 * fullscreen, so this case is handled too.
		 *
		 * In the end, in the worst case, the windowed safe mode will be selected, and the system
		 * should not return rserr_invalid_fullscreen or rserr_invalid_mode anymore.
		 */

		// TODO: Take the borderless flag into account (could it fail?)

		if( err == rserr_invalid_fullscreen ) {
			clWarning() << "Fullscreen unavailable in this mode";

			Cvar_ForceSet( vid_fullscreen->name, "0" );
			vid_fullscreen->modified = false;

			// Try again without the fullscreen flag
			options.fullscreen = false;
			err = R_TrySettingMode( x, y, w, h, frequency, options );
		}

		if( err == rserr_invalid_mode ) {
			clWarning() << "Invalid video mode";

			// Try setting it back to something safe
			if( w != vid_ref_prevwidth || h != vid_ref_prevheight ) {
				w = vid_ref_prevwidth;
				Cvar_ForceSet( vid_width->name, va( "%i", w ) );
				h = vid_ref_prevheight;
				Cvar_ForceSet( vid_height->name, va( "%i", h ) );

				err = R_TrySettingMode( x, y, w, h, frequency, options );
				if( err == rserr_invalid_fullscreen ) {
					clWarning() << "Could not revert to safe fullscreen mode";

					Cvar_ForceSet( vid_fullscreen->name, "0" );
					vid_fullscreen->modified = false;

					// Try again without the fullscreen flag
					options.fullscreen = false;
					err = R_TrySettingMode( x, y, w, h, frequency, options );
				}
			}

			if( err != rserr_ok ) {
				clWarning() << "Could not revert to safe mode";
			}
		}
	}

	if( err == rserr_ok ) {
		viddef.width  = w;
		viddef.height = h;
		// Let various subsystems know about the new window
		VID_WindowInitialized();
	}

	return err;
}

static void VID_UnloadRefresh( void ) {
	if( vid_ref_active ) {
		RF_Shutdown( false );
		vid_ref_active = false;
	}
}

static bool VID_LoadRefresh() {
	VID_UnloadRefresh();

	// TODO: Try applying changes immediately?
	return true;
}

[[nodiscard]]
static auto getBestFittingMode( int requestedWidth, int requestedHeight ) -> std::pair<int, int> {
	assert( vid_num_modes );

	int width = -1;
	unsigned leastWidthPenalty = std::numeric_limits<unsigned>::max();
	// Get a best matching mode for width first (which has a priority over height)
	for( unsigned i = 0; i < vid_num_modes; ++i ) {
		const auto &mode = vid_modes[i];
		const auto absDiff = std::abs( mode.width - requestedWidth );
		assert( absDiff < std::numeric_limits<int>::max() >> 1 );
		// Set a penalty bit for modes with lesser than requested width
		const unsigned penalty = ( absDiff << 1u ) | ( mode.width >= requestedWidth ? 0 : 1 );
		if( leastWidthPenalty > penalty ) {
			leastWidthPenalty = penalty;
			width = mode.width;
			if( width == requestedWidth ) [[unlikely]] {
				break;
			}
		}
	}

	int height = -1;
	unsigned leastHeightPenalty = std::numeric_limits<unsigned>::max();
	// Get a best matching mode for height preserving the selected width
	for( unsigned i = 0; i < vid_num_modes; ++i ) {
		// Require an exact match of the chosen width
		if( const auto &mode = vid_modes[i]; mode.width == width ) {
			const auto absDiff = (unsigned)std::abs( mode.height - requestedHeight );
			assert( absDiff < std::numeric_limits<unsigned>::max() >> 1 );
			// Set a penalty bit for modes with lesser than requested height
			const unsigned penalty = ( absDiff << 1u ) | ( mode.height >= requestedHeight ? 0 : 1 );
			if( leastHeightPenalty > penalty ) {
				leastHeightPenalty = penalty;
				height = mode.height;
				if( height == requestedHeight ) [[unlikely]] {
					break;
				}
			}
		}
	}

	assert( width > 0 && height > 0 );
	return { width, height };
}

static void RestartVideoAndAllMedia( bool vid_ref_was_active, bool verbose ) {
	const bool cgameActive = cls.cgameActive;
	cls.disable_screen = 1;

	CL_ShutdownMedia();

	// stop and free all sounds
	CL_SoundModule_Shutdown( false );

	FTLIB_FreeFonts( false );

	Cvar_GetLatchedVars( CVAR_LATCH_VIDEO );

	// TODO: Eliminate this
	if( !VID_LoadRefresh() ) {
		Sys_Error( "VID_LoadRefresh() failed" );
	}

	char buffer[16];

	// handle vid size changes
	if( ( vid_width->integer <= 0 ) || ( vid_height->integer <= 0 ) ) {
		// set the mode to the default
		int w, h;
		if( !VID_GetDefaultMode( &w, &h ) ) {
			w = vid_modes[0].width;
			h = vid_modes[0].height;
		}
		Cvar_ForceSet( vid_width->name, va_r( buffer, sizeof( buffer ), "%d", w ) );
		Cvar_ForceSet( vid_height->name, va_r( buffer, sizeof( buffer ), "%d", h ) );
	}

	if( const auto &mode = vid_modes[vid_max_width_mode_index]; vid_width->integer > mode.width ) {
		Cvar_ForceSet( vid_width->name, va_r( buffer, sizeof( buffer ), "%d", mode.width ) );
	}
	if( const auto &mode = vid_modes[vid_max_height_mode_index]; vid_height->integer > mode.height ) {
		Cvar_ForceSet( vid_height->name, va_r( buffer, sizeof( buffer ), "%d", mode.width ) );
	}

	if( vid_fullscreen->integer ) {
		// snap to the closest fullscreen resolution, width has priority over height
		const int requestedWidth = vid_width->integer;
		const int requestedHeight = vid_height->integer;
		const auto [bestWidth, bestHeight] = getBestFittingMode( requestedWidth, requestedHeight );
		if( bestWidth != requestedWidth ) {
			Cvar_ForceSet( vid_width->name, va_r( buffer, sizeof( buffer ), "%d", bestWidth ) );
		}
		if( bestHeight != requestedHeight ) {
			Cvar_ForceSet( vid_height->name, va_r( buffer, sizeof( buffer ), "%d", bestHeight ) );
		}
	}

	if( rserr_t err = VID_Sys_Init_( STR_TO_POINTER( vid_parentwid->string ), vid_ref_verbose ); err != rserr_ok ) {
		Sys_Error( "VID_Init() failed with code %i", err );
	}
	if( rserr_t err = VID_ChangeMode(); err != rserr_ok ) {
		Sys_Error( "VID_ChangeMode() failed with code %i", err );
	}

	vid_ref_active = true;

	// stop and free all sounds
	CL_SoundModule_Init( verbose );

	RF_BeginRegistration();
	SoundSystem::instance()->beginRegistration();

	FTLIB_PrecacheFonts( verbose );

	if( vid_ref_was_active ) {
		IN_Restart( CmdArgs {} );
	}

	CL_InitMedia();

	cls.disable_screen = 0;

	SCR_CloseConsole();

	if( cgameActive ) {
		CL_GameModule_Init();
		SCR_CloseConsole();
	}

	RF_EndRegistration();
	SoundSystem::instance()->endRegistration();

	vid_ref_modified = false;
	vid_ref_verbose = true;
}

/*
** VID_CheckChanges
*
* This function gets called once just before drawing each frame, and its sole purpose in life
* is to check to see if any of the video mode parameters have changed, and if they have to
* update the rendering DLL and/or video mode to match.
*/
void VID_CheckChanges( void ) {
	const bool vid_ref_was_active = vid_ref_active;
	const bool verbose = vid_ref_verbose || vid_ref_sound_restart;

	if( win_noalttab->modified ) {
		VID_EnableAltTab( win_noalttab->integer ? false : true );
		win_noalttab->modified = false;
	}

	if( win_nowinkeys->modified ) {
		VID_EnableWinKeys( win_nowinkeys->integer ? false : true );
		win_nowinkeys->modified = false;
	}

	if( vid_fullscreen->modified ) {
		if( vid_ref_active ) {
			// try to change video mode without vid_restart
			if( const rserr_t err = VID_ChangeMode(); err == rserr_restart_required ) {
				vid_ref_modified = true;
			}
		}

		vid_fullscreen->modified = false;
	}

	if( vid_ref_modified ) {
		RestartVideoAndAllMedia( vid_ref_was_active, verbose );
	}

	if( vid_xpos->modified || vid_ypos->modified ) {
		if( !vid_fullscreen->integer && !vid_borderless->integer ) {
			VID_UpdateWindowPosAndSize( vid_xpos->integer, vid_ypos->integer );
		}
		vid_xpos->modified = false;
		vid_ypos->modified = false;
	}
}

static int VID_CompareModes( const vidmode_t *first, const vidmode_t *second ) {
	if( first->width == second->width ) {
		return first->height - second->height;
	}

	return first->width - second->width;
}

void VID_InitModes( void ) {
	const unsigned numAllModes = VID_GetSysModes( nullptr );
	if( !numAllModes ) {
		Sys_Error( "Failed to get video modes" );
	}

	assert( !vid_modes );
	vid_modes = (vidmode_t *)Q_malloc( numAllModes * sizeof( vidmode_t ) );

	if( const unsigned nextNumAllModes = VID_GetSysModes( vid_modes ); nextNumAllModes != numAllModes ) {
		Sys_Error( "Failed to get video modes again" );
	}

	unsigned numModes = 0;
	for( unsigned i = 0; i < numAllModes; ++i ) {
		if( vid_modes[i].width >= 1024 && vid_modes[i].height >= 720 ) {
			vid_modes[numModes++] = vid_modes[i];
		}
	}

	if( !numModes ) {
		Sys_Error( "Failed to find at least a single supported video mode" );
	}

	qsort( vid_modes, numModes, sizeof( vidmode_t ), ( int ( * )( const void *, const void * ) )VID_CompareModes );

	// Remove duplicate modes in case the sys code failed to do so.
	vid_num_modes = 0;
	vid_max_height_mode_index = 0;
	int prevWidth = 0, prevHeight = 0;
	for( unsigned i = 0; i < numModes; i++ ) {
		const int width = vid_modes[i].width;
		const int height = vid_modes[i].height;
		if( width != prevWidth || height != prevHeight ) {
			if( height > vid_modes[vid_max_height_mode_index].height ) {
				vid_max_height_mode_index = i;
			}
			vid_modes[vid_num_modes++] = vid_modes[i];
			prevWidth = width;
			prevHeight = height;
		}
	}

	vid_max_width_mode_index = vid_num_modes - 1;
}

void VID_Init( void ) {
	if( !vid_initialized ) {
		VID_InitModes();

		vid_width = Cvar_Get( "vid_width", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
		vid_height = Cvar_Get( "vid_height", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
		vid_pixelRatio = Cvar_Get( "vid_pixelRatio", "0", CVAR_DEVELOPER | CVAR_LATCH_VIDEO );
		vid_xpos = Cvar_Get( "vid_xpos", "0", CVAR_ARCHIVE );
		vid_ypos = Cvar_Get( "vid_ypos", "0", CVAR_ARCHIVE );
		vid_fullscreen = Cvar_Get( "vid_fullscreen", "1", CVAR_ARCHIVE );
		vid_borderless = Cvar_Get( "vid_borderless", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
		vid_displayfrequency = Cvar_Get( "vid_displayfrequency", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );
		vid_multiscreen_head = Cvar_Get( "vid_multiscreen_head", "-1", CVAR_ARCHIVE );
		vid_parentwid = Cvar_Get( "vid_parentwid", "0", CVAR_NOSET );

		win_noalttab = Cvar_Get( "win_noalttab", "0", CVAR_ARCHIVE );
		win_nowinkeys = Cvar_Get( "win_nowinkeys", "0", CVAR_ARCHIVE );

		cl_multithreading = Cvar_Get( "cl_multithreading", "0", CVAR_ARCHIVE | CVAR_LATCH_VIDEO );

		/* Add some console commands that we want to handle */
		CL_Cmd_Register( "vid_restart"_asView, VID_Restart_f );
		CL_Cmd_Register( "vid_modelist"_asView, VID_ModeList_f );

		/* Start the graphics mode and load refresh DLL */
		vid_ref_modified = true;
		vid_ref_active = false;
		vid_ref_verbose = true;
		vid_initialized = true;
		vid_ref_sound_restart = false;
		vid_fullscreen->modified = false;
		vid_borderless->modified = false;
		vid_ref_prevwidth = vid_modes[0].width; // the smallest mode is the "safe mode"
		vid_ref_prevheight = vid_modes[0].height;

		FTLIB_Init( true );

		VID_CheckChanges();
	}
}

void VID_Shutdown( void ) {
	if( vid_initialized ) {
		VID_UnloadRefresh();

		FTLIB_Shutdown( true );

		CL_Cmd_Unregister( "vid_restart"_asView );
		CL_Cmd_Unregister( "vid_modelist"_asView );

		Q_free( vid_modes );

		vid_initialized = false;
	}
}

qfontface_t *SCR_RegisterFont( const char *family, int style, unsigned int size ) {
	return FTLIB_RegisterFont( family, DEFAULT_SYSTEM_FONT_FAMILY_FALLBACK, style, size );
}

static void SCR_RegisterConsoleFont( void ) {
	const int size = ceilf( 15 * VID_GetPixelRatio() );
	cls.consoleFont = SCR_RegisterFont( DEFAULT_SYSTEM_FONT_FAMILY_MONO, DEFAULT_SYSTEM_FONT_STYLE, size );
	if( !cls.consoleFont ) {
		Com_Error( ERR_FATAL, "Couldn't load default font \"%s\"", DEFAULT_SYSTEM_FONT_FAMILY_MONO );
	}
	Con_CheckResize();
}

static void SCR_InitFonts( void ) {
	SCR_RegisterConsoleFont();
}

static void SCR_ShutdownFonts( void ) {
	cls.consoleFont = NULL;
}

static void SCR_CheckSystemFontsModified( void ) {
}

static int SCR_HorizontalAlignForString( const int x, int align, int width ) {
	int nx = x;

	if( align % 3 == 0 ) { // left
		nx = x;
	}
	if( align % 3 == 1 ) { // center
		nx = x - width / 2;
	}
	if( align % 3 == 2 ) { // right
		nx = x - width;
	}

	return nx;
}

static int SCR_VerticalAlignForString( const int y, int align, int height ) {
	int ny = y;

	if( align / 3 == 0 ) { // top
		ny = y;
	} else if( align / 3 == 1 ) { // middle
		ny = y - height / 2;
	} else if( align / 3 == 2 ) { // bottom
		ny = y - height;
	}

	return ny;
}

size_t SCR_FontHeight( qfontface_t *font ) {
	return FTLIB_FontHeight( font );
}

size_t SCR_strWidth( const char *str, qfontface_t *font, size_t maxlen, int flags ) {
	return FTLIB_StringWidth( str, font, maxlen, flags );
}

void SCR_DrawRawChar( int x, int y, wchar_t num, qfontface_t *font, const vec4_t color ) {
	FTLIB_DrawRawChar( x, y, num, font, color );
}

void SCR_DrawClampString( int x, int y, const char *str, int xmin, int ymin, int xmax, int ymax, qfontface_t *font, const vec4_t color, int flags ) {
	FTLIB_DrawClampString( x, y, str, xmin, ymin, xmax, ymax, font, color, flags );
}

int SCR_DrawString( int x, int y, int align, const char *str, qfontface_t *font, const float *color, int flags ) {
	if( !str ) {
		return 0;
	}

	if( !font ) {
		font = cls.consoleFont;
	}

	const int fontHeight = FTLIB_FontHeight( font );

	if( ( align % 3 ) != 0 ) { // not left - don't precalculate the width if not needed
		x = SCR_HorizontalAlignForString( x, align, FTLIB_StringWidth( str, font, 0, flags ) );
	}
	y = SCR_VerticalAlignForString( y, align, fontHeight );

	int width = 0;
	FTLIB_DrawRawString( x, y, str, 0, &width, font, color, flags );

	return width;
}

int SCR_DrawString( int x, int y, int align, const wsw::StringView &str, qfontface_t *font, const float *color, int flags ) {
	if( str.isZeroTerminated() ) {
		return SCR_DrawString( x, y, align, str.data(), font, color, flags );
	}
	if( str.length() < 256 ) {
		wsw::StaticString<256> ztStr;
		ztStr << str;
		return SCR_DrawString( x, y, align, ztStr.data(), font, color, flags );
	}
	static wsw::PodVector<char> ztStr;
	ztStr.clear();
	ztStr.append( str );
	ztStr.append( '\0' );
	return SCR_DrawString( x, y, align, ztStr.data(), font, color, flags );
}

void SCR_DrawFillRect( int x, int y, int w, int h, const vec4_t color ) {
	R_DrawStretchPic( x, y, w, h, 0, 0, 1, 1, color, cls.whiteShader );
}

void SCR_InitScreen( void ) {
	scr_consize = Cvar_Get( "scr_consize", "0.4", CVAR_ARCHIVE );
	scr_conspeed = Cvar_Get( "scr_conspeed", "3", CVAR_ARCHIVE );

	scr_initialized = true;
}

void SCR_ShutdownScreen( void ) {
	scr_initialized = false;
}

void SCR_EnableQuickMenu( bool enable ) {
	cls.quickmenu = enable;
}

void SCR_RunConsole( int msec ) {
	// decide on the height of the console
	if( Con_HasKeyboardFocus() ) {
		scr_conlines = bound( 0.1f, scr_consize->value, 1.0f );
	} else {
		scr_conlines = 0;
	}
	if( scr_conlines < scr_con_current ) {
		scr_con_current -= scr_conspeed->value * msec * 0.001f;
		if( scr_conlines > scr_con_current ) {
			scr_con_current = scr_conlines;
		}
	} else if( scr_conlines > scr_con_current ) {
		scr_con_current += scr_conspeed->value * msec * 0.001f;
		if( scr_conlines < scr_con_current ) {
			scr_con_current = scr_conlines;
		}
	}
}

void SCR_CloseConsole() {
	scr_con_current = 0.0f;
	Con_Close();
}

void SCR_BeginLoadingPlaque( void ) {
	SoundSystem::instance()->stopSounds( SoundSystem::StopMusic | SoundSystem::RetainLocal );

	cl.configStrings.clear();

	scr_conlines = 0;       // none visible
	scr_draw_loading = 2;   // clear to black first

	//
	//SCR_UpdateScreen();
}

void SCR_EndLoadingPlaque( void ) {
	cls.disable_screen = 0;
	Con_ClearNotify();
}

void SCR_RegisterConsoleMedia() {
	cls.whiteShader = R_RegisterPic( "$whiteimage" );

	SCR_InitFonts();
}

void SCR_ShutDownConsoleMedia( void ) {
	SCR_ShutdownFonts();
}

void SCR_UpdateScreen( void ) {
	WSW_PROFILER_SCOPE();

	assert( !cls.disable_screen && scr_initialized && con_initialized && cls.mediaInitialized );

	Con_CheckResize();

	SCR_CheckSystemFontsModified();

	bool canRenderView = false;
	bool canDrawConsole = false;
	bool canDrawDebug = false;

	auto *const uiSystem = wsw::ui::UISystem::instance();

	if( scr_draw_loading == 2 ) {
		// loading plaque over APP_STARTUP_COLOR screen
		scr_draw_loading = 0;
	} else if( cls.state == CA_DISCONNECTED ) {
		canDrawConsole = true;
	} else if( cls.state == CA_CONNECTED ) {
		if( cls.cgameActive ) {
			canRenderView = true;
		}
	} else if( cls.state == CA_ACTIVE ) {
		canRenderView = true;

		assert( cls.frametime > 0 );
		uiSystem->addToFrametimeTimeline( cls.realtime, cls.frametime );

		canDrawConsole = true;
		canDrawDebug = true;
	}

	// Perform UI refresh (that may include binding UI GL context and unbinding it) first
	uiSystem->refreshProperties();

	bool hasRenderedUI = false;
	if( !( canRenderView && cl.cms && cge && cls.cgameActive ) ) {
		uiSystem->renderInternally();
		hasRenderedUI = true;
	}

	// TODO: Pass as flags
	const bool forcevsync = cls.state == CA_DISCONNECTED || uiSystem->suggestsUsingVSync();
	const bool forceclear = true;
	const bool timedemo   = cl_timedemo->integer && cls.demoPlayer.playing;

	RF_BeginFrame( forceclear, forcevsync, timedemo );

	// CGame may render the menu on its own above the primary view but beneath miniviews
	CGRenderViewResult cgRenderViewResult;
	if( canRenderView ) {
		if( timedemo ) {
			if( !cl.timedemo.startTime ) {
				cl.timedemo.startTime = Sys_Milliseconds();
			}
			cl.timedemo.frames++;
		}

		// frame is not valid until we load the CM data
		if( cl.cms ) {
			if( cge && cls.cgameActive ) {
				assert( !hasRenderedUI );
				unsigned extrapolationTime = cl_extrapolate->integer && !cls.demoPlayer.playing ? cl_extrapolationTime->integer : 0;
				cgRenderViewResult = CG_RenderView( cls.frametime, cls.realFrameTime, cls.realtime, cl.serverTime, extrapolationTime );
				hasRenderedUI = cgRenderViewResult.hasRenderedUIInternally;
			}
		}
	}

	if( !cgRenderViewResult.hasRunGCIfNeeded ) {
		uiSystem->runGCIfNeeded();
	}

	if( !hasRenderedUI ) {
		uiSystem->renderInternally();
	}

	// TODO: This should not belong to the UI module, let client manage it!
	uiSystem->drawBackgroundMapIfNeeded();

	R_Set2DMode( true );
	RF_Set2DScissor( 0, 0, viddef.width, viddef.height );

	if( !cgRenderViewResult.hasBlittedTheMenu ) {
		uiSystem->drawMenuPartInMainContext();
	}
	if( !cgRenderViewResult.hasBlittedTheHud ) {
		uiSystem->drawHudPartInMainContext();
	}

	uiSystem->drawCursorInMainContext();

	const bool shouldDrawConsole = canDrawConsole && scr_con_current > 0.0f;
	if( canDrawDebug && !shouldDrawConsole ) {
		Con_DrawNotify( viddef.width, viddef.height );
		CL_ProfilerHud_Draw( viddef.width, viddef.height );
	}

	if( shouldDrawConsole ) {
		Con_DrawConsole( viddef.width, viddef.height * scr_con_current );
	}

	RF_EndFrame();
}

/*
* CL_AddReliableCommand
*
* The given command will be transmitted to the server, and is gauranteed to
* not have future usercmd_t executed before it is executed
*/
void CL_AddReliableCommand( const char *cmd ) {
	if( cmd && std::strlen( cmd ) > 0 ) {
		// if we would be losing an old command that hasn't been acknowledged,
		// we must drop the connection
		if( cls.reliableSequence > cls.reliableAcknowledge + MAX_RELIABLE_COMMANDS ) {
			cls.reliableAcknowledge = cls.reliableSequence; // try to avoid loops
			Com_Error( ERR_DROP, "Client command overflow %" PRIi64 "%" PRIi64, cls.reliableAcknowledge, cls.reliableSequence );
		}

		cls.reliableSequence++;
		const auto index = (int)( cls.reliableSequence & ( MAX_RELIABLE_COMMANDS - 1 ) );
		Q_strncpyz( cls.reliableCommands[index], cmd, sizeof( cls.reliableCommands[index] ) );
	}
}

/*
* CL_UpdateClientCommandsToServer
*
* Add the pending commands to the message
*/
void CL_UpdateClientCommandsToServer( msg_t *msg ) {
	// write any unacknowledged clientCommands
	for( int64_t i = cls.reliableAcknowledge + 1; i <= cls.reliableSequence; i++ ) {
		const char *cmd = cls.reliableCommands[i & ( MAX_RELIABLE_COMMANDS - 1 )];
		if( std::strlen( cmd ) > 0 ) {
			MSG_WriteUint8( msg, clc_clientcommand );
			if( !cls.reliable ) {
				MSG_WriteIntBase128( msg, i );
			}
			MSG_WriteString( msg, cmd );
		}
	}

	cls.reliableSent = cls.reliableSequence;
	if( cls.reliable ) {
		cls.reliableAcknowledge = cls.reliableSent;
	}
}

void CL_ForwardToServer_f( const CmdArgs &cmdArgs ) {
	if( !cls.demoPlayer.playing ) {
		if( cls.state != CA_CONNECTED && cls.state != CA_ACTIVE ) {
			clNotice() << "Can't" << cmdArgs[0] << "not connected";
		} else {
			// don't forward the first argument
			if( Cmd_Argc() > 1 ) {
				CL_AddReliableCommand( Cmd_Args() );
			}
		}
	}
}

void CL_ServerDisconnect_f( const CmdArgs &cmdArgs ) {
	auto reconnectBehaviour = ReconnectBehaviour::DontReconnect;
	if( const auto rawAutoconnectBehaviour = wsw::toNum<unsigned>( wsw::StringView( Cmd_Argv( 1 ) ) ) ) {
		for( const ReconnectBehaviour behaviourValue: kReconnectBehaviourValues ) {
			if( (unsigned)behaviourValue == *rawAutoconnectBehaviour ) {
				reconnectBehaviour = behaviourValue;
				break;
			}
		}
	}

	wsw::StaticString<MAX_STRING_CHARS> reason;
	reason << cmdArgs[2];

	CL_Disconnect_f( {} );

	auto connectionDropStage = ConnectionDropStage::FunctioningError;
	if( const auto rawDropStage = wsw::toNum<unsigned>( wsw::StringView( Cmd_Argv( 1 ) ) ) ) {
		for( const ConnectionDropStage dropStageValue : kConnectionDropStageValues ) {
			if( (unsigned)dropStageValue == *rawDropStage ) {
				connectionDropStage = dropStageValue;
				break;
			}
		}
	}

	clNotice() << "Connection was closed by server" << reason;
	wsw::ui::UISystem::instance()->notifyOfDroppedConnection( reason.asView(), reconnectBehaviour, connectionDropStage );
}

void CL_Quit( void ) {
	CL_Disconnect( NULL );
	Com_Quit( {} );
}

static void CL_Quit_f( const CmdArgs & ) {
	CL_Quit();
}

/*
* CL_SendConnectPacket
*
* We have gotten a challenge from the server, so try and
* connect.
*/
static void CL_SendConnectPacket( void ) {
	userinfo_modified = false;

	const char *ticketString = ""; // CLStatsowFacade::Instance()->GetTicketString().data();
	Netchan_OutOfBandPrint( cls.socket, &cls.serveraddress, "connect %i %i %i \"%s\" %i %s\n",
							APP_PROTOCOL_VERSION, Netchan_GamePort(), cls.challenge, Cvar_Userinfo(), 0, ticketString );
}

/*
* CL_CheckForResend
*
* Resend a connect message if the last one has timed out
*/
static void CL_CheckForResend( void ) {
	if( cls.demoPlayer.playing ) {
		return;
	}

	if( cls.state == CA_DISCONNECTED ) {
		// if the local server is running and we aren't then connect
		if( Com_ServerState() ) {
			CL_SetClientState( CA_CONNECTING );
			if( cls.servername ) {
				Q_free( cls.servername );
			}
			cls.servername = Q_strdup( "localhost" );
			cls.servertype = SOCKET_LOOPBACK;
			NET_InitAddress( &cls.serveraddress, NA_LOOPBACK );
			if( !NET_OpenSocket( &cls.socket_loopback, cls.servertype, &cls.serveraddress, false ) ) {
				Com_Error( ERR_FATAL, "Couldn't open the loopback socket\n" );
			}
			cls.socket = &cls.socket_loopback;
		}
	} else if( cls.state == CA_CONNECTING ) {
		// FIXME: should use cls.realtime, but it can be old here after starting a server
		const int64_t realtime = Sys_Milliseconds();
		if( cls.reliable ) {
			if( realtime - cls.connect_time >= 3000 ) {
				CL_Disconnect( "Connection timed out" );
			}
		} else {
			if( realtime - cls.connect_time >= 3000 ) {
				if( cls.connect_count > 3 ) {
					CL_Disconnect( "Connection timed out" );
				} else {
					cls.connect_count++;
					cls.connect_time = realtime; // for retransmit requests

					clNotice() << "Connecting to" << wsw::StringView( cls.servername );

					Netchan_OutOfBandPrint( cls.socket, &cls.serveraddress, "getchallenge\n" );
				}
			}
		}
	}
}

static void CL_Connect( const char *servername, socket_type_t type, netadr_t *address, const char *serverchain ) {
	cl_connectChain[0] = '\0';
	cl_nextString[0] = '\0';

	CL_Disconnect( NULL );

	switch( type ) {
		case SOCKET_LOOPBACK:
			netadr_t socketaddress;
			NET_InitAddress( &socketaddress, NA_LOOPBACK );
			if( !NET_OpenSocket( &cls.socket_loopback, SOCKET_LOOPBACK, &socketaddress, false ) ) {
				Com_Error( ERR_FATAL, "Couldn't open the loopback socket: %s\n", NET_ErrorString() ); // FIXME
			}
			cls.socket = &cls.socket_loopback;
			cls.reliable = false;
			break;

		case SOCKET_UDP:
			cls.socket = ( address->type == NA_IP6 ?  &cls.socket_udp6 :  &cls.socket_udp );
			cls.reliable = false;
			break;

		default:
			wsw::failWithLogicError( "Unreachable" );
	}

	cls.servertype = type;
	cls.serveraddress = *address;
	if( NET_GetAddressPort( &cls.serveraddress ) == 0 ) {
		NET_SetAddressPort( &cls.serveraddress, PORT_SERVER );
	}

	if( cls.servername ) {
		Q_free( cls.servername );
	}
	cls.servername = Q_strdup( servername );

	cl.configStrings.clear();

	// If the server supports matchmaking and that we are authenticated, try getting a matchmaking ticket before joining the server
	connstate_t newstate = CA_CONNECTING;
	/*
	if( CLStatsowFacade::Instance()->IsValid() ) {
		// if( MM_GetStatus() == MM_STATUS_AUTHENTICATED && CL_MM_GetTicket( serversession ) )
		if( CLStatsowFacade::Instance()->StartConnecting( &cls.serveraddress ) ) {
			newstate = CA_GETTING_TICKET;
		}
	}*/
	CL_SetClientState( newstate );

	if( serverchain[0] ) {
		Q_strncpyz( cl_connectChain, serverchain, sizeof( cl_connectChain ) );
	}

	cls.connect_time = -99999; // CL_CheckForResend() will fire immediately
	cls.connect_count = 0;
	cls.lastPacketReceivedTime = cls.realtime; // reset the timeout limit
	cls.mv = false;
}

static void CL_Connect_Cmd_f( socket_type_t socket, const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		clNotice() << "Usage:" << wsw::unquoted( cmdArgs[0] ) << "<server>";
		return;
	}

	const char *const scheme = APP_URI_SCHEME, *const proto_scheme = APP_URI_PROTO_SCHEME;

	char *connectstring_base = Q_strdup( Cmd_Argv( 1 ) );
	char *connectstring = connectstring_base;
	const char *const serverchain = Cmd_Argc() >= 3 ? Cmd_Argv( 2 ) : "";

	if( !Q_strnicmp( connectstring, proto_scheme, strlen( proto_scheme ) ) ) {
		connectstring += strlen( proto_scheme );
	} else if( !Q_strnicmp( connectstring, scheme, strlen( scheme ) ) ) {
		connectstring += strlen( scheme );
	}

	const char *extension = COM_FileExtension( connectstring );
	if( extension && !Q_stricmp( extension, APP_DEMO_EXTENSION_STR ) ) {
		char *temp;
		size_t temp_size;
		const char *http_scheme = "http://";

		if( !Q_strnicmp( connectstring, http_scheme, strlen( http_scheme ) ) ) {
			connectstring += strlen( http_scheme );
		}

		temp_size = strlen( "demo " ) + strlen( http_scheme ) + strlen( connectstring ) + 1;
		temp = (char *)Q_malloc( temp_size );
		Q_snprintfz( temp, temp_size, "demo %s%s", http_scheme, connectstring );

		CL_Cmd_ExecuteNow( temp );

		Q_free( temp );
		Q_free( connectstring_base );
	} else {
		const char *tmp;
		char password[64] {}, autowatch[64] {};

		if( ( tmp = Q_strrstr( connectstring, "@" ) ) != NULL ) {
			assert( tmp - connectstring >= 0 );
			Q_strncpyz( password, connectstring, wsw::min( sizeof( password ), (size_t)( tmp - connectstring + 1 ) ) );
			Cvar_Set( "password", password );
			connectstring = connectstring + ( tmp - connectstring ) + 1;
		}

		if( ( tmp = Q_strrstr( connectstring, "#" ) ) != NULL ) {
			Q_strncpyz( autowatch, COM_RemoveColorTokens( tmp + 1 ), sizeof( autowatch ) );
			connectstring[tmp - connectstring] = '\0';
		}

		if( ( tmp = Q_strrstr( connectstring, "/" ) ) != NULL ) {
			connectstring[tmp - connectstring] = '\0';
		}

		Cvar_ForceSet( "autowatch", autowatch );

		netadr_t serveraddress;
		if( !NET_StringToAddress( connectstring, &serveraddress ) ) {
			Q_free( connectstring_base );
			clNotice() << "Bad server address";
		} else {
			char *servername = Q_strdup( connectstring );
			CL_Connect( servername, ( serveraddress.type == NA_LOOPBACK ? SOCKET_LOOPBACK : socket ),
					&serveraddress, serverchain );

			Q_free( servername );
			Q_free( connectstring_base );
		}
	}
}

static void CL_Connect_f( const CmdArgs &cmdArgs ) {
	CL_Connect_Cmd_f( SOCKET_UDP, cmdArgs );
}

/*
* CL_Rcon_f
*
* Send the rest of the command line over as
* an unconnected command.
*/
static void CL_Rcon_f( const CmdArgs &cmdArgs ) {
	if( cls.demoPlayer.playing ) {
		return;
	}

	if( rcon_client_password->string[0] == '\0' ) {
		clNotice() << "You must set 'rcon_password' before issuing an rcon command";
		return;
	}

	char message[1024];
	// wsw : jal : check for msg len abuse (thx to r1Q2)
	if( strlen( Cmd_Args() ) + strlen( rcon_client_password->string ) + 16 >= sizeof( message ) ) {
		clNotice() << "Length of password + command exceeds maximum allowed length";
		return;
	}

	if( cls.state < CA_CONNECTED ) {
		if( !strlen( rcon_address->string ) ) {
			clNotice() << "You must be connected, or set the 'rcon_address' cvar to issue rcon commands";
			return;
		}
		if( rcon_address->modified ) {
			if( NET_StringToAddress( rcon_address->string, &cls.rconaddress ) ) {
				if( NET_GetAddressPort( &cls.rconaddress ) == 0 ) {
					NET_SetAddressPort( &cls.rconaddress, PORT_SERVER );
				}
				rcon_address->modified = false;
			} else {
				clNotice() << "Bad rcon_address";
				// we don't clear modified, so it will whine the next time too
				return;
			}
		}
	}

	message[0] = (uint8_t)255;
	message[1] = (uint8_t)255;
	message[2] = (uint8_t)255;
	message[3] = (uint8_t)255;
	message[4] = 0;

	Q_strncatz( message, "rcon ", sizeof( message ) );

	Q_strncatz( message, rcon_client_password->string, sizeof( message ) );
	Q_strncatz( message, " ", sizeof( message ) );

	for( int i = 1; i < Cmd_Argc(); i++ ) {
		Q_strncatz( message, "\"", sizeof( message ) );
		Q_strncatz( message, Cmd_Argv( i ), sizeof( message ) );
		Q_strncatz( message, "\" ", sizeof( message ) );
	}

	const socket_t *socket;
	const netadr_t *address;
	if( cls.state >= CA_CONNECTED ) {
		socket = cls.netchan.socket;
		address = &cls.netchan.remoteAddress;
	} else {
		socket = ( cls.rconaddress.type == NA_IP6 ? &cls.socket_udp6 : &cls.socket_udp );
		address = &cls.rconaddress;
	}

	NET_SendPacket( socket, message, (int)strlen( message ) + 1, address );
}

char *CL_GetClipboardData( void ) {
	return Sys_GetClipboardData();
}

void CL_SetClipboardData( const char *data ) {
	Sys_SetClipboardData( data );
}

void CL_FreeClipboardData( char *data ) {
	Sys_FreeClipboardData( data );
}

bool CL_IsBrowserAvailable( void ) {
	return Sys_IsBrowserAvailable();
}

void CL_OpenURLInBrowser( const char *url ) {
	Sys_OpenURLInBrowser( url );
}

size_t CL_GetBaseServerURL( char *buffer, size_t buffer_size ) {
	const char *web_url = cls.httpbaseurl;

	if( !buffer || !buffer_size ) {
		return 0;
	}
	if( !web_url || !*web_url ) {
		*buffer = '\0';
		return 0;
	}

	Q_strncpyz( buffer, web_url, buffer_size );
	return strlen( web_url );
}

void CL_ResetServerCount( void ) {
	cl.servercount = -1;
}

static void CL_BeginRegistration( void ) {
	if( !cls.registrationOpen ) {
		cls.registrationOpen = true;

		RF_BeginRegistration();
		wsw::ui::UISystem::instance()->beginRegistration();
		SoundSystem::instance()->beginRegistration();
	}
}

static void CL_EndRegistration( void ) {
	if( cls.registrationOpen ) {
		cls.registrationOpen = false;

		FTLIB_TouchAllFonts();
		RF_EndRegistration();
		wsw::ui::UISystem::instance()->endRegistration();
		SoundSystem::instance()->endRegistration();
	}
}

void CL_ClearState( void ) {
	if( cl.cms ) {
		CM_ReleaseReference( cl.cms );
		cl.cms = NULL;
	}

	if( cl.frames_areabits ) {
		Q_free( cl.frames_areabits );
		cl.frames_areabits = NULL;
	}

	if( cl.cmds ) {
		Q_free( cl.cmds );
		cl.cmds = NULL;
	}

	if( cl.cmd_time ) {
		Q_free( cl.cmd_time );
		cl.cmd_time = NULL;
	}

	if( cl.snapShots ) {
		Q_free( cl.snapShots );
		cl.snapShots = NULL;
	}

	// wipe the entire cl structure

	// Hacks just to avoid writing redundant clear() methods
	// for stuff that must be eventually rewritten.
	cl.~client_state_t();
	memset( (void *)&cl, 0, sizeof( client_state_t ) );
	new( &cl )client_state_t;

	memset( cl_baselines, 0, sizeof( cl_baselines ) );

	cl.cmds = (usercmd_t *)Q_malloc( sizeof( *cl.cmds ) * CMD_BACKUP );
	cl.cmd_time = (int *)Q_malloc( sizeof( *cl.cmd_time ) * CMD_BACKUP );
	cl.snapShots = (snapshot_t *)Q_malloc( sizeof( *cl.snapShots ) * CMD_BACKUP );

	//userinfo_modified = true;
	cls.lastExecutedServerCommand = 0;
	cls.reliableAcknowledge = 0;
	cls.reliableSequence = 0;
	cls.reliableSent = 0;
	memset( cls.reliableCommands, 0, sizeof( cls.reliableCommands ) );
	// reset ucmds buffer
	cls.ucmdHead = 0;
	cls.ucmdSent = 0;
	cls.ucmdAcknowledged = 0;

	//restart realtime and lastPacket times
	cls.realtime = 0;
	cls.gametime = 0;
	cls.lastPacketSentTime = 0;
	cls.lastPacketReceivedTime = 0;
	cls.lastReceivedRealServerTime = 0;
}

/*
* CL_SetNext_f
*
* Next is used to set an action which is executed at disconnecting.
*/
static void CL_SetNext_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		clNotice() << "Usage: next <commands>\n";
	} else {
		// jalfixme: I'm afraid of this being too powerful, since it basically
		// is allowed to execute everything. Shall we check for something?
		Q_strncpyz( cl_nextString, Cmd_Args(), sizeof( cl_nextString ) );
		clNotice() << "Next:" << wsw::StringView( cl_nextString );
	}
}

static void CL_ExecuteNext( void ) {
	if( std::strlen( cl_nextString ) > 0 ) {
		CL_Cbuf_AppendCommand( cl_nextString );
		memset( cl_nextString, 0, sizeof( cl_nextString ) );
	}
}

/*
* CL_Disconnect
*
* Goes from a connected state to full screen console state
* Sends a disconnect message to the server
* This is also called on Com_Error, so it shouldn't cause any errors
*/
void CL_Disconnect( const char *message, bool isCalledByBuiltinServer /* TODO!!!!! */ ) {
	// We have to shut down webdownloading first
	if( cls.download.web && !cls.download.disconnect ) {
		cls.download.disconnect = true;
		return;
	}

	if( cls.state == CA_UNINITIALIZED ) {
		return;
	}

	if( cls.state != CA_DISCONNECTED ) {
		if( !isCalledByBuiltinServer ) {
			SV_NotifyBuiltinServerOfShutdownGameRequest();
		}

		if( cl_timedemo && cl_timedemo->integer ) {
			int64_t sumcounts = 0;

			Com_Printf( "\n" );
			for( int i = 1; i < 100; i++ ) {
				if( cl.timedemo.counts[i] > 0 ) {
					float fps = 1000.0 / i;
					float perc = cl.timedemo.counts[i] * 100.0 / cl.timedemo.frames;
					sumcounts += i * cl.timedemo.counts[i];

					Com_Printf( "%2ims - %7.2ffps: %6.2f%%\n", i, fps, perc );
				}
			}

			Com_Printf( "\n" );
			if( sumcounts ) {
				float mean = 1000.0 / (double)sumcounts * cl.timedemo.frames;
				int64_t duration = Sys_Milliseconds() - cl.timedemo.startTime;
				Com_Printf( "%3.1f seconds: %3.1f mean fps\n", duration / 1000.0, mean );
			}
		}

		cls.connect_time = 0;
		cls.connect_count = 0;

		if( cls.demoRecorder.recording ) {
			CL_Stop_f( {} );
		}

		if( cls.demoPlayer.playing ) {
			CL_DemoCompleted();
		} else {
			// send a disconnect message to the server
			// wsw : jal : send the packet 3 times to make sure isn't lost
			for( int i = 0; i < 3; ++i ) {
				CL_AddReliableCommand( "disconnect" );
				CL_SendMessagesToServer( true );
			}
		}

		FS_RemovePurePaks();

		Com_FreePureList( &cls.purelist );

		cls.sv_pure = false;

		// udp is kept open all the time, for connectionless messages
		if( cls.socket && cls.socket->type != SOCKET_UDP ) {
			NET_CloseSocket( cls.socket );
		}

		cls.socket = NULL;
		cls.reliable = false;
		cls.mv = false;

		if( cls.httpbaseurl ) {
			Q_free( cls.httpbaseurl );
			cls.httpbaseurl = NULL;
		}

		R_Finish();

		CL_EndRegistration();

		CL_RestartMedia();

		CL_ClearState();
		CL_SetClientState( CA_DISCONNECTED );

		if( cls.download.requestname ) {
			cls.download.pending_reconnect = false;
			cls.download.cancelled = true;
			CL_DownloadDone();
		}

		if( cl_connectChain[0] == '\0' ) {
			if( message ) {
				// TODO: Remove the "message" parameter, call directly in the client code
				wsw::ui::UISystem::instance()->notifyOfDroppedConnection( wsw::StringView( message ),
																		  ReconnectBehaviour::OfUserChoice,
																		  ConnectionDropStage::FunctioningError );
			}
		} else {
			const char *s = strchr( cl_connectChain, ',' );
			if( s ) {
				cl_connectChain[s - cl_connectChain] = '\0';
			} else {
				s = cl_connectChain + strlen( cl_connectChain ) - 1;
			}
			Q_snprintfz( cl_nextString, sizeof( cl_nextString ), "connect \"%s\" \"%s\"", cl_connectChain, s + 1 );
		}
	}

	SCR_EndLoadingPlaque(); // get rid of loading plaque

	// in case we disconnect while in download phase
	CL_FreeDownloadList();

	CL_ExecuteNext(); // start next action if any is defined
}

void CL_Disconnect_f( const CmdArgs & ) {
	cl_connectChain[0] = '\0';
	cl_nextString[0] = '\0';

	// We have to shut down webdownloading first
	if( cls.download.web ) {
		cls.download.disconnect = true;
	} else {
		CL_Disconnect( NULL );
	}
}

/*
* CL_Changing_f
*
* Just sent as a hint to the client that they should
* drop to full console
*/
void CL_Changing_f( const CmdArgs & ) {
	//ZOID
	//if we are downloading, we don't change!  This so we don't suddenly stop downloading a map
	if( ( cls.download.filenum || cls.download.web ) ) {
		if( cls.demoRecorder.recording ) {
			CL_Stop_f( {} );
		}

		Com_DPrintf( "CL:Changing\n" );

		cl.configStrings.clear();

		// ignore snapshots from previous connection
		cl.pendingSnapNum = cl.currentSnapNum = cl.receivedSnapNum = 0;

		CL_SetClientState( CA_CONNECTED ); // not active anymore, but not disconnected
	}
}

/*
* CL_ServerReconnect_f
*
* The server is changing levels
*/
void CL_ServerReconnect_f( const CmdArgs & ) {
	if( cls.demoPlayer.playing ) {
		return;
	}

	//if we are downloading, we don't change!  This so we don't suddenly stop downloading a map
	if( cls.download.filenum || cls.download.web ) {
		cls.download.pending_reconnect = true;
		return;
	}

	if( cls.state < CA_CONNECTED ) {
		Com_Printf( "Error: CL_ServerReconnect_f while not connected\n" );
	} else {
		if( cls.demoRecorder.recording ) {
			CL_Stop_f( {} );
		}

		cls.connect_count = 0;

		CL_GameModule_Shutdown();
		SoundSystem::instance()->stopSounds( SoundSystem::StopMusic | SoundSystem::RetainLocal );

		clNotice() << "Reconnecting";

		cls.connect_time = Sys_Milliseconds() - 1500;
		cl.configStrings.clear();

		CL_SetClientState( CA_HANDSHAKE );
		CL_AddReliableCommand( "new" );
	}
}

/*
* CL_Reconnect_f
*
* User reconnect command.
*/
void CL_Reconnect_f( const CmdArgs & ) {
	if( !cls.servername ) {
		clNotice() << "Can't reconnect, never connected";
		return;
	}

	cl_connectChain[0] = '\0';
	cl_nextString[0] = '\0';

	char *servername = Q_strdup( cls.servername );
	socket_type_t servertype = cls.servertype;
	netadr_t serveraddress = cls.serveraddress;
	CL_Disconnect( NULL );
	CL_Connect( servername, servertype, &serveraddress, "" );
	Q_free( servername );
}

[[nodiscard]]
static bool CheckOobConnectionAddress( const netadr_t *address, const char *command ) {
	// these two are from Q3
	if( cls.state != CA_CONNECTING ) {
		clWarning() << "client_connect packet while not connecting, ignored";
		return false;
	}
	if( !NET_CompareAddress( address, &cls.serveraddress ) ) {
		wsw::StaticString<64> addressString, serverAddressString;
		addressString.append( wsw::StringView( NET_AddressToString( address ) ) );
		serverAddressString.append( wsw::StringView( NET_AddressToString( &cls.serveraddress ) ) );
		clWarning() << wsw::StringView( command ) << "from a different address, ignored: was"_asView
					<< addressString << "should have been"_asView << serverAddressString;
		return false;
	}
	return true;
}

static void HandleOob_ClientConnect( const socket_t *socket, const netadr_t *address, msg_t *msg ) {
	if( cls.state == CA_CONNECTED ) {
		clWarning() << "Dup connect received, ignored";
		return;
	}
	if( !CheckOobConnectionAddress( address, "client_connect" ) ) {
		return;
	}

	Q_strncpyz( cls.session, MSG_ReadStringLine( msg ), sizeof( cls.session ) );

	Netchan_Setup( &cls.netchan, socket, address, Netchan_GamePort() );
	cl.configStrings.clear();
	CL_SetClientState( CA_HANDSHAKE );
	CL_AddReliableCommand( "new" );
}

static void HandleOob_Reject( const socket_t *socket, const netadr_t *address, msg_t *msg ) {
	if( !CheckOobConnectionAddress( address, "reject" ) ) {
		return;
	}

	// Skip the legacy reject type
	std::ignore = MSG_ReadStringLine( msg );
	// Skip the legacy reject flag
	std::ignore = MSG_ReadStringLine( msg );

	const wsw::PodVector<char> rejectMessage( wsw::StringView( MSG_ReadStringLine( msg ) ) );
	clNotice() << "Connection refused" << rejectMessage;

	auto connectionDropStage = ConnectionDropStage::EstablishingFailed;
	auto reconnectBehaviour  = ReconnectBehaviour::DontReconnect;

	// 2.6+ sends protocol version
	if( const auto protocolVersion = wsw::toNum<unsigned>( wsw::StringView( MSG_ReadStringLine( msg ) ) ) ) {
		// Parse 2.6+ extensions
		clNotice() << "Parsing reject packet extensions" << wsw::named( "protocol version", *protocolVersion );
		std::optional<ConnectionDropStage> parsedStage;
		std::optional<ReconnectBehaviour> parsedBehaviour;
		if( const auto rawDropStage = wsw::toNum<unsigned>( wsw::StringView( MSG_ReadStringLine( msg ) ) ) ) {
			for( const ConnectionDropStage dropStageValue: kConnectionDropStageValues ) {
				if( *rawDropStage == (unsigned)dropStageValue ) {
					parsedStage = dropStageValue;
					break;
				}
			}
		}
		if( parsedStage ) {
			if( const auto rawBehaviour = wsw::toNum<unsigned>( wsw::StringView( MSG_ReadStringLine( msg ) ) ) ) {
				for( const ReconnectBehaviour behaviourValue: kReconnectBehaviourValues ) {
					if( *rawBehaviour == (unsigned)behaviourValue ) {
						parsedBehaviour = behaviourValue;
						break;
					}
				}
			}
		}
		if( parsedStage && parsedBehaviour ) {
			connectionDropStage = *parsedStage;
			reconnectBehaviour  = *parsedBehaviour;
		} else {
			clWarning() << "Failed to parse the connection drop stage/reconnect behaviour, using defaults";
		}
	}

	if( reconnectBehaviour == ReconnectBehaviour::Autoreconnect ) {
		clNotice() << "Automatic reconnecting allowed";
		// TODO: What's next?
	} else {
		clNotice() << "Automatic reconnecting not allowed";
		CL_Disconnect( nullptr );
		auto *const uiSystem = wsw::ui::UISystem::instance();
		const wsw::StringView rejectMessageView( rejectMessage.data(), rejectMessage.size() );
		uiSystem->notifyOfDroppedConnection( rejectMessageView, reconnectBehaviour, connectionDropStage );
	}
}

static void HandleOob_Cmd( const socket_t *socket, const netadr_t *address, msg_t *msg ) {
	if( !NET_IsLocalAddress( address ) ) {
		clWarning() << "Command packet from remote host, ignored";
	} else {
		Sys_AppActivate();
		const char *s = MSG_ReadString( msg );
		CL_Cbuf_AppendCommand( s );
		CL_Cbuf_AppendCommand( "\n" );
	}
}

static void HandleOob_Print( const socket_t *socket, const netadr_t *address, msg_t *msg ) {
	// CA_CONNECTING is allowed, because old servers send protocol mismatch connection error message with it
	if( ( ( cls.state != CA_UNINITIALIZED && cls.state != CA_DISCONNECTED ) &&
		  NET_CompareAddress( address, &cls.serveraddress ) ) ||
		( rcon_address->string[0] != '\0' && NET_CompareAddress( address, &cls.rconaddress ) ) ) {
		const char *s = MSG_ReadString( msg );
		Com_Printf( "%s", s );
	} else {
		clWarning() << "Print packet from unknown host, ignored";
	}
}

static void HandleOob_Challenge( const socket_t *socket, const netadr_t *address, msg_t *msg, const char *challenge ) {
	if( !CheckOobConnectionAddress( address, "challenge" ) ) {
		return;
	}

	cls.challenge = atoi( challenge );
	//wsw : r1q2[start]
	//r1: reset the timer so we don't send dup. getchallenges
	cls.connect_time = Sys_Milliseconds();
	//wsw : r1q2[end]
	CL_SendConnectPacket();
}

/*
* CL_ConnectionlessPacket
*
* Responses to broadcasts, etc
*/
static void CL_ConnectionlessPacket( const socket_t *socket, const netadr_t *address, msg_t *msg ) {
	MSG_BeginReading( msg );
	MSG_ReadInt32( msg ); // skip the -1

	const char *s = MSG_ReadStringLine( msg );
	Com_DPrintf( "%s: %s\n", NET_AddressToString( address ), s );

	if( !strncmp( s, "getserversResponse", 18 ) ) {
		ServerList::instance()->parseGetServersResponse( socket, *address, msg );
	} else {
		static CmdArgsSplitter argsSplitter;
		const CmdArgs &cmdArgs = argsSplitter.exec( wsw::StringView( s ) );
		const char *cmdName    = cmdArgs[0].data();

		// jal : wsw
		// server responding to a detailed info broadcast
		if( !strcmp( cmdName, "infoResponse" ) ) {
			ServerList::instance()->parseGetInfoResponse( socket, *address, msg );
		} else if( !strcmp( cmdName, "statusResponse" ) ) {
			ServerList::instance()->parseGetStatusResponse( socket, *address, msg );
		} else {
			if( cls.demoPlayer.playing ) {
				Com_DPrintf( "Received connectionless cmd \"%s\" from %s while playing a demo\n", s, NET_AddressToString( address ) );
			} else {
				if( !strcmp( cmdName, "client_connect" ) ) {
					HandleOob_ClientConnect( socket, address, msg );
				} else if( !strcmp( cmdName, "reject" ) ) {
					HandleOob_Reject( socket, address, msg );
				} else if( !strcmp( cmdName, "cmd" ) ) {
					HandleOob_Cmd( socket, address, msg );
				} else if( !strcmp( cmdName, "print" ) ) {
					HandleOob_Print( socket, address, msg );
				} else if( !strcmp( cmdName, "ping" ) ) {
					// send any args back with the acknowledgement
					Netchan_OutOfBandPrint( socket, address, "ack %s", Cmd_Args() );
				} else if( !strcmp( cmdName, "ack" ) ) {
					;
				} else if( !strcmp( cmdName, "challenge" ) ) {
					HandleOob_Challenge( socket, address, msg, Cmd_Argv( 1 ) );
				} else if( !strcmp( cmdName, "echo" ) ) {
					Netchan_OutOfBandPrint( socket, address, "%s", Cmd_Argv( 1 ));
				} else {
					Com_Printf( "Unknown connectionless packet from %s\n%s\n", NET_AddressToString( address ), cmdName );
				}
			}
		}
	}
}

static bool CL_ProcessPacket( netchan_t *netchan, msg_t *msg ) {
	// TODO: Do something more sophisticated
	g_netchanInstanceBackup = *netchan;
	// wasn't accepted for some reason
	if( !Netchan_Process( netchan, msg ) ) {
		return false;
	}

	// now if compressed, expand it
	MSG_BeginReading( msg );
	MSG_ReadInt32( msg ); // sequence
	MSG_ReadInt32( msg ); // sequence_ack
	if( msg->compressed ) {
		if( const int zerror = Netchan_DecompressMessage( msg, g_netchanCompressionBuffer ); zerror < 0 ) {
			// compression error. Drop the packet
			clWarning() << "CL_ProcessPacket: Compression error" << zerror << "Dropping packet";
			*netchan = g_netchanInstanceBackup;
			return false;
		}
	}

	return true;
}

void CL_ReadPackets( void ) {
	// TODO: Should be members or locals
	static msg_t msg;
	static uint8_t msgData[MAX_MSGLEN];

	MSG_Init( &msg, msgData, sizeof( msgData ) );

	for( socket_t *const socket : { &cls.socket_loopback, &cls.socket_udp, &cls.socket_udp6 } ) {
		int ret;
		netadr_t address;
		while( socket->open && ( ret = NET_GetPacket( socket, &address, &msg ) ) != 0 ) {
			if( ret == -1 ) {
				Com_Printf( "Error receiving packet with %s: %s\n", NET_SocketToString( socket ), NET_ErrorString() );
				if( cls.reliable && cls.socket == socket ) {
					CL_Disconnect( va( "Error receiving packet: %s\n", NET_ErrorString() ) );
				}
			} else {
				// remote command packet
				if( *(int *)msg.data == -1 ) {
					CL_ConnectionlessPacket( socket, &address, &msg );
				} else {
					if( !cls.demoPlayer.playing ) {
						if( cls.state == CA_DISCONNECTED || cls.state == CA_GETTING_TICKET || cls.state == CA_CONNECTING ) {
							Com_DPrintf( "%s: Not connected\n", NET_AddressToString( &address ) );
						} else {
							// TODO: Move this condition to CL_ProcessPacket()/Netchan_Process()?
							if( msg.cursize < 8 ) {
								//wsw : r1q2[start]
								Com_DPrintf( "%s: Runt packet\n", NET_AddressToString( &address ) );
								//wsw : r1q2[end]
							} else {
								if( !NET_CompareAddress( &address, &cls.netchan.remoteAddress ) ) {
									Com_DPrintf( "%s: Sequenced packet without connection\n", NET_AddressToString( &address ) );
								} else {
									if( CL_ProcessPacket( &cls.netchan, &msg ) ) {
										CL_ParseServerMessage( &msg );
										cls.lastPacketReceivedTime = cls.realtime;
									}
									// Otherwise, wasn't accepted for some reason, like only one fragment of bigger message
								}
							}
						}
					}
				}
			}
		}
	}

	if( !cls.demoPlayer.playing ) {
		// not expected, but could happen if cls.realtime is cleared and lastPacketReceivedTime is not
		if( cls.lastPacketReceivedTime > cls.realtime ) {
			cls.lastPacketReceivedTime = cls.realtime;
		}

		// check timeout
		if( cls.state >= CA_HANDSHAKE && cls.lastPacketReceivedTime ) {
			if( cls.lastPacketReceivedTime + cl_timeout->value * 1000 < cls.realtime ) {
				if( ++cl.timeoutcount > 5 ) { // timeoutcount saves debugger
					clNotice() << "Server connection timed out";
					CL_Disconnect( "Connection timed out" );
				}
			}
		} else {
			cl.timeoutcount = 0;
		}
	}
}

/*
* CL_DownloadRequest
*
* Request file download
* return false if couldn't request it for some reason
* Files with .pk3 or .pak extension have to have gamedir attached
* Other files must not have gamedir
*/
bool CL_DownloadRequest( const char *filename, bool requestpak ) {
	if( cls.download.requestname ) {
		clWarning() << "Can't download" << wsw::StringView( filename ) << "A download is already in progress";
		return false;
	}

	if( !COM_ValidateRelativeFilename( filename ) ) {
		clWarning() << "Can't download" << wsw::StringView( filename ) << "Invalid filename";
		return false;
	}

	if( FS_CheckPakExtension( filename ) ) {
		if( FS_PakFileExists( filename ) ) {
			clWarning() << "Can't download" << wsw::StringView( filename ) << "The file already exists";
			return false;
		}
		if( !Q_strnicmp( COM_FileBase( filename ), "modules", strlen( "modules" ) ) ) {
			return false;
		}
	} else {
		if( FS_FOpenFile( filename, NULL, FS_READ ) != -1 ) {
			clWarning() << "Can't download" << wsw::StringView( filename ) << "File already exists";
			return false;
		}
		if( !requestpak ) {
			// only allow demo downloads
			const char *extension = COM_FileExtension( filename );
			if( !extension || Q_stricmp( extension, APP_DEMO_EXTENSION_STR ) ) {
				clWarning() << "Can't download, got arbitrary file type" << wsw::StringView( filename );
				return false;
			}
		}
	}

	if( cls.socket->type == SOCKET_LOOPBACK ) {
		clWarning() << "Can't download" << wsw::StringView( filename ) << "Loopback server";
		return false;
	}

	clNotice() << "Asking to download" << wsw::StringView( filename );

	cls.download.requestpak = requestpak;
	cls.download.requestname = (char *)Q_malloc( sizeof( char ) * ( strlen( filename ) + 1 ) );
	Q_strncpyz( cls.download.requestname, filename, sizeof( char ) * ( strlen( filename ) + 1 ) );
	cls.download.timeout = Sys_Milliseconds() + 5000;
	CL_AddReliableCommand( va( "download %i \"%s\"", requestpak, filename ) );

	return true;
}

/*
* CL_CheckOrDownloadFile
*
* Returns true if the file exists or couldn't send download request
* Files with .pk3 or .pak extension have to have gamedir attached
* Other files must not have gamedir
*/
bool CL_CheckOrDownloadFile( const char *filename ) {
	if( !cl_downloads->integer ) {
		return true;
	}

	if( !COM_ValidateRelativeFilename( filename ) ) {
		return true;
	}

	const char *ext = COM_FileExtension( filename );
	if( !ext ) {
		return true;
	}

	if( FS_CheckPakExtension( filename ) ) {
		if( FS_PakFileExists( filename ) ) {
			return true;
		}
	} else {
		if( FS_FOpenFile( filename, NULL, FS_READ ) != -1 ) {
			return true;
		}
	}

	if( !CL_DownloadRequest( filename, true ) ) {
		return true;
	}

	cls.download.requestnext = true; // call CL_RequestNextDownload when done

	return false;
}

/*
* CL_DownloadComplete
*
* Checks downloaded file's checksum, renames it and adds to the filesystem.
*/
static void CL_DownloadComplete( void ) {
	FS_FCloseFile( cls.download.filenum );
	cls.download.filenum = 0;

	unsigned checksum = 0;
	bool isAValidFile = false;

	if( FS_CheckPakExtension( cls.download.name ) ) {
		if( FS_IsPakValid( cls.download.tempname, &checksum ) ) {
			isAValidFile = true;
		}
	} else {
		// TODO: Just stat() it?
		if( const int length = FS_LoadBaseFile( cls.download.tempname, NULL, NULL, 0 ); length < 0 ) {
			checksum = FS_ChecksumBaseFile( cls.download.tempname, false );
			isAValidFile = true;
		}
	}

	if( !isAValidFile ) {
		clWarning() << "Downloaded file is corrupt. Removing it";
		FS_RemoveBaseFile( cls.download.tempname );
		return;
	}

	if( cls.download.checksum != checksum ) {
		clWarning() << "Downloaded file has wrong checksum. Removing it";
		FS_RemoveBaseFile( cls.download.tempname );
		return;
	}

	if( !FS_MoveBaseFile( cls.download.tempname, cls.download.name ) ) {
		clWarning() << "Failed to rename the downloaded file";
		return;
	}

	// Maplist hook so we also know when a new map is added
	if( FS_CheckPakExtension( cls.download.name ) ) {
		ML_Update();
	}

	cls.download.successCount++;
	cls.download.timeout = 0;
}

void CL_FreeDownloadList( void ) {
	while( cls.download.list ) {
		download_list_t *next = cls.download.list->next;
		Q_free( cls.download.list->filename );
		Q_free( cls.download.list );
		cls.download.list = next;
	}
	cls.download.list = nullptr;
}

void CL_DownloadDone( void ) {
	if( cls.download.name ) {
		CL_StopServerDownload();
	}

	Q_free( cls.download.requestname );
	cls.download.requestname = NULL;

	const bool requestnext = cls.download.requestnext;
	cls.download.requestnext = false;
	cls.download.requestpak = false;
	cls.download.timeout = 0;
	cls.download.timestart = 0;
	cls.download.offset = cls.download.baseoffset = 0;
	cls.download.web = false;
	cls.download.filenum = 0;
	cls.download.cancelled = false;

	// the server has changed map during the download
	if( cls.download.pending_reconnect ) {
		cls.download.pending_reconnect = false;
		CL_FreeDownloadList();
		CL_ServerReconnect_f( {} );
	} else {
		if( requestnext && cls.state > CA_DISCONNECTED ) {
			CL_RequestNextDownload();
		}
	}
}

static void CL_WebDownloadDoneCb( int status, const char *contentType, void *privatep ) {
	const download_t download = cls.download;
	const bool disconnect = download.disconnect;
	const bool cancelled = download.cancelled;
	const bool success = ( download.offset == download.size ) && ( status > -1 );
	const bool try_non_official = download.web_official && !download.web_official_only;

	clNotice() << "Web download" << wsw::StringView( download.tempname ) << wsw::StringView( success ? "successful" : "failed" );

	if( success ) {
		CL_DownloadComplete();
	}
	if( cancelled ) {
		cls.download.requestnext = false;
	}

	// check if user pressed escape to stop the downloa
	if( disconnect ) {
		CL_Disconnect( NULL ); // this also calls CL_DownloadDone()
	} else {
		// try a non-official mirror (the builtin HTTP server or a remote mirror)
		if( !success && !cancelled && try_non_official ) {
			const int size = download.size;
			char *filename = Q_strdup( download.origname );
			const unsigned checksum = download.checksum;
			char *url = Q_strdup( download.web_url );
			const bool allow_localhttp = download.web_local_http;

			cls.download.cancelled = true; // remove the temp file
			CL_StopServerDownload();
			CL_InitServerDownload( filename, size, checksum, allow_localhttp, url, false );

			Q_free( filename );
			Q_free( url );
		} else {
			CL_DownloadDone();
		}
	}
}

static size_t CL_WebDownloadReadCb( const void *buf, size_t numb, float percentage, int status,
									const char *contentType, void *privatep ) {
	bool stop = cls.download.disconnect || cls.download.cancelled || status < 0 || status >= 300;
	size_t write = 0;

	if( !stop ) {
		write = FS_Write( buf, numb, cls.download.filenum );
	}

	// ignore percentage passed by the downloader as it doesn't account for total file size
	// of resumed downloads
	cls.download.offset += write;
	cls.download.percent = (double)cls.download.offset / (double)cls.download.size;
	Q_clamp( cls.download.percent, 0, 1 );

	Cvar_ForceSet( "cl_download_percent", va( "%.1f", cls.download.percent * 100 ) );

	cls.download.timeout = 0;

	// abort if disconnected, canclled or writing failed
	return stop ? !numb : write;
}

/*
* CL_InitDownload
*
* Hanldles server's initdownload message, starts web or server download if possible
*/
static void CL_InitServerDownload( const char *filename, size_t size, unsigned checksum, bool allow_localhttpdownload,
								   const char *url, bool initial ) {
	// ignore download commands coming from demo files
	if( cls.demoPlayer.playing ) {
		return;
	}

	if( !cls.download.requestname ) {
		clWarning() << "Got init download message without request";
		return;
	}

	if( cls.download.filenum || cls.download.web ) {
		clWarning() << "Got init download message while already downloading";
		return;
	}

	if( size == (size_t)-1 ) {
		// means that download was refused
		// if it's refused, url field holds the reason
		clWarning() << "Server refused download request" << wsw::StringView( url );
		CL_DownloadDone();
		return;
	}

	if( size <= 0 ) {
		clWarning() << "Server gave invalid size, not downloading";
		CL_DownloadDone();
		return;
	}

	if( checksum == 0 ) {
		clWarning() << "Server didn't provide checksum, not downloading";
		CL_DownloadDone();
		return;
	}

	if( !COM_ValidateRelativeFilename( filename ) ) {
		clWarning() << "Not downloading, invalid filename" << wsw::StringView( filename );
		CL_DownloadDone();
		return;
	}

	if( FS_CheckPakExtension( filename ) != cls.download.requestpak ) {
		const char *requested = cls.download.requestpak ? "pak" : "normal";
		const char *got = cls.download.requestpak ? "normal" : "pak";
		Com_Printf( "Got a %s file when requesting a %s file, not downloading\n", got, requested );
		CL_DownloadDone();
		return;
	}

	if( !strchr( filename, '/' ) ) {
		clWarning() << "Refusing to download file with no gamedir", wsw::StringView( filename );
		CL_DownloadDone();
		return;
	}

	// check that it is in game or basegame dir
	const wsw::StringView fileNameView( filename );
	if( !fileNameView.startsWith( kDataDirectory ) || fileNameView.maybeAt( kDataDirectory.size() ) != std::optional( '/' ) ) {
		clWarning() << "Can't download, invalid game directory: %s\n" << wsw::StringView( filename );
		CL_DownloadDone();
		return;
	}

	bool modules_download = false;
	bool explicit_pure_download = false;
	if( FS_CheckPakExtension( filename ) ) {
		if( strchr( strchr( filename, '/' ) + 1, '/' ) ) {
			clWarning() << "Refusing to download pack file to subdirectory: %s\n" << wsw::StringView( filename );
			CL_DownloadDone();
			return;
		}

		modules_download = !Q_strnicmp( COM_FileBase( filename ), "modules", strlen( "modules" ) );
		if( modules_download ) {
			CL_DownloadDone();
			return;
		}

		if( FS_PakFileExists( filename ) ) {
			clWarning() << "Can't download, file already exists" << wsw::StringView( filename );
			CL_DownloadDone();
			return;
		}

		explicit_pure_download = FS_IsExplicitPurePak( filename, NULL );
	} else {
		if( strcmp( cls.download.requestname, strchr( filename, '/' ) + 1 ) ) {
			clWarning() << "Can't download, got different file than requested" << wsw::StringView( filename );
			CL_DownloadDone();
			return;
		}
	}

	if( initial ) {
		if( cls.download.requestnext ) {
			download_list_s *dl = cls.download.list;
			while( dl != NULL ) {
				if( !Q_stricmp( dl->filename, filename ) ) {
					clWarning() << "Skipping, already tried downloading" << wsw::StringView( filename );
					CL_DownloadDone();
					return;
				}
				dl = dl->next;
			}
		}
	}

	const bool force_web_official = initial && cls.download.requestpak;

	const bool official_web_only = modules_download || explicit_pure_download;
	const bool official_web_download = force_web_official || official_web_only;

	size_t alloc_size = strlen( "downloads" ) + 1 /* '/' */ + strlen( filename ) + 1;
	cls.download.name = (char *)Q_malloc( alloc_size );
	if( official_web_download || !cls.download.requestpak ) {
		// it's an official pak, otherwise
		// if we're not downloading a pak, this must be a demo so drop it into the gamedir
		Q_snprintfz( cls.download.name, alloc_size, "%s", filename );
	} else {
		if( FS_DownloadsDirectory() == NULL ) {
			clWarning() << "Can't download, downloads directory is disabled";
			CL_DownloadDone();
			return;
		}
		Q_snprintfz( cls.download.name, alloc_size, "%s/%s", "downloads", filename );
	}

	alloc_size = strlen( cls.download.name ) + strlen( ".tmp" ) + 1;
	cls.download.tempname = (char *)Q_malloc( alloc_size );
	Q_snprintfz( cls.download.tempname, alloc_size, "%s.tmp", cls.download.name );

	cls.download.origname = Q_strdup( filename );
	cls.download.web = false;
	cls.download.web_official = official_web_download;
	cls.download.web_official_only = official_web_only;
	cls.download.web_url = Q_strdup( url );
	cls.download.web_local_http = allow_localhttpdownload;
	cls.download.cancelled = false;
	cls.download.disconnect = false;
	cls.download.size = size;
	cls.download.checksum = checksum;
	cls.download.percent = 0;
	cls.download.timeout = 0;
	cls.download.retries = 0;
	cls.download.timestart = Sys_Milliseconds();
	cls.download.offset = 0;
	cls.download.baseoffset = 0;
	cls.download.pending_reconnect = false;

	Cvar_ForceSet( "cl_download_name", COM_FileBase( filename ) );
	Cvar_ForceSet( "cl_download_percent", "0" );

	if( initial ) {
		if( cls.download.requestnext ) {
			download_list_s *dl = (download_list_t *)Q_malloc( sizeof( download_list_t ) );
			dl->filename = Q_strdup( filename );
			dl->next = cls.download.list;
			cls.download.list = dl;
		}
	}

	const char *baseurl = cls.httpbaseurl;
	if( official_web_download ) {
		baseurl = APP_UPDATE_URL APP_SERVER_UPDATE_DIRECTORY;
		allow_localhttpdownload = false;
	}

	if( official_web_download ) {
		cls.download.web = true;
		Com_Printf( "Web download: %s from %s/%s\n", cls.download.tempname, baseurl, filename );
	} else if( cl_downloads_from_web->integer && allow_localhttpdownload && url && url[0] != 0 ) {
		cls.download.web = true;
		Com_Printf( "Web download: %s from %s/%s\n", cls.download.tempname, baseurl, url );
	} else if( cl_downloads_from_web->integer && url && url[0] != 0 ) {
		cls.download.web = true;
		Com_Printf( "Web download: %s from %s\n", cls.download.tempname, url );
	} else {
		Com_Printf( "Server download: %s\n", cls.download.tempname );
	}

	cls.download.baseoffset = cls.download.offset =
		(size_t)FS_FOpenBaseFile( cls.download.tempname, &cls.download.filenum, FS_APPEND );

	if( !cls.download.filenum ) {
		Com_Printf( "Can't download, couldn't open %s for writing\n", cls.download.tempname );
		CL_DownloadDone();
		return;
	}

	if( cls.download.web ) {
		char *referer, *fullurl;
		const char *headers[] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL };

		if( cls.download.offset == cls.download.size ) {
			// special case for completed downloads to avoid passing empty HTTP range
			CL_WebDownloadDoneCb( 200, "", NULL );
			return;
		}

		alloc_size = strlen( APP_URI_SCHEME ) + strlen( NET_AddressToString( &cls.serveraddress ) ) + 1;
		referer = (char *)alloca( alloc_size );
		Q_snprintfz( referer, alloc_size, APP_URI_SCHEME "%s", NET_AddressToString( &cls.serveraddress ) );
		Q_strlwr( referer );

		if( official_web_download ) {
			alloc_size = strlen( baseurl ) + 1 + strlen( filename ) + 1;
			fullurl = (char *)alloca( alloc_size );
			Q_snprintfz( fullurl, alloc_size, "%s/%s", baseurl, filename );
		} else if( allow_localhttpdownload ) {
			alloc_size = strlen( baseurl ) + 1 + strlen( url ) + 1;
			fullurl = (char *)alloca( alloc_size );
			Q_snprintfz( fullurl, alloc_size, "%s/%s", baseurl, url );
		} else {
			size_t url_len = strlen( url );
			alloc_size = url_len + 1 + strlen( filename ) * 3 + 1;
			fullurl = (char *)alloca( alloc_size );
			Q_snprintfz( fullurl, alloc_size, "%s/", url );
			Q_urlencode_unsafechars( filename, fullurl + url_len + 1, alloc_size - url_len - 1 );
		}

		headers[0] = "Referer";
		headers[1] = referer;

		CL_AddSessionHttpRequestHeaders( fullurl, &headers[2] );

		CL_AsyncStreamRequest( fullurl, headers, cl_downloads_from_web_timeout->integer / 100, (int)cls.download.offset,
							   CL_WebDownloadReadCb, CL_WebDownloadDoneCb, NULL, NULL, false );
	} else {
		// TODO: Get rid of non-HTTP downloads?

		cls.download.timeout = Sys_Milliseconds() + 3000;
		cls.download.retries = 0;

		CL_AddReliableCommand( va( "nextdl \"%s\" %" PRIu64, cls.download.name, (uint64_t)cls.download.offset ) );
	}
}

static void CL_InitDownload_f( const CmdArgs &cmdArgs ) {
	// ignore download commands coming from demo files
	if( cls.demoPlayer.playing ) {
		return;
	}

	// read the data
	const char *filename = Cmd_Argv( 1 );
	const int size = atoi( Cmd_Argv( 2 ) );
	const unsigned checksum = (unsigned)strtoul( Cmd_Argv( 3 ), NULL, 10 );
	const bool allow_localhttpdownload = ( atoi( Cmd_Argv( 4 ) ) != 0 ) && cls.httpbaseurl != NULL;
	const char *url = Cmd_Argv( 5 );

	CL_InitServerDownload( filename, size, (unsigned)checksum, allow_localhttpdownload, url, true );
}

void CL_StopServerDownload( void ) {
	if( cls.download.filenum > 0 ) {
		FS_FCloseFile( cls.download.filenum );
		cls.download.filenum = 0;
	}

	if( cls.download.cancelled ) {
		FS_RemoveBaseFile( cls.download.tempname );
	}

	Q_free( cls.download.name );
	cls.download.name = NULL;

	Q_free( cls.download.tempname );
	cls.download.tempname = NULL;

	Q_free( cls.download.origname );
	cls.download.origname = NULL;

	Q_free( cls.download.web_url );
	cls.download.web_url = NULL;

	cls.download.offset = 0;
	cls.download.size = 0;
	cls.download.percent = 0;
	cls.download.timeout = 0;
	cls.download.retries = 0;
	cls.download.web = false;

	Cvar_ForceSet( "cl_download_name", "" );
	Cvar_ForceSet( "cl_download_percent", "0" );
}

/*
* CL_RetryDownload
* Resends download request
* Also aborts download if we have retried too many times
*/
static void CL_RetryDownload( void ) {
	if( ++cls.download.retries > 5 ) {
		clNotice() << "Download timed out" << wsw::StringView( cls.download.name );
		// let the server know we're done
		CL_AddReliableCommand( va( "nextdl \"%s\" %i", cls.download.name, -2 ) );
		CL_DownloadDone();
	} else {
		cls.download.timeout = Sys_Milliseconds() + 3000;
		CL_AddReliableCommand( va( "nextdl \"%s\" %" PRIu64, cls.download.name, (uint64_t)cls.download.offset ) );
	}
}

/*
* CL_CheckDownloadTimeout
* Retry downloading if too much time has passed since last download packet was received
*/
void CL_CheckDownloadTimeout( void ) {
	if( cls.download.timeout && cls.download.timeout <= Sys_Milliseconds() ) {
		if( cls.download.filenum ) {
			CL_RetryDownload();
		} else {
			clWarning() << "Download request timed out";
			CL_DownloadDone();
		}
	}
}

void CL_DownloadStatus_f( const CmdArgs & ) {
	if( !cls.download.requestname ) {
		clNotice() << "No download active";
	} else if( !cls.download.name ) {
		Com_Printf( "%s: Requesting\n", COM_FileBase( cls.download.requestname ) );
	} else {
		Com_Printf( "%s: %s download %3.2f%c done\n", COM_FileBase( cls.download.name ),
			( cls.download.web ? "Web" : "Server" ), cls.download.percent * 100.0f, '%' );
	}
}

void CL_DownloadCancel_f( const CmdArgs & ) {
	if( !cls.download.requestname ) {
		clNotice() << "No download active";
	} else if( !cls.download.name ) {
		CL_DownloadDone();
		clNotice() << "Canceled download request";
	} else {
		clNotice() << "Canceled download of" << wsw::StringView( cls.download.name );
		cls.download.cancelled = true;
		if( !cls.download.web ) {
			CL_AddReliableCommand( va( "nextdl \"%s\" %i", cls.download.name, -2 ) ); // let the server know we're done
			CL_DownloadDone();
		}
	}
}

/*
* CL_ParseDownload
* Handles download message from the server.
* Writes data to the file and requests next download block.
*/
// TODO: Get rid of non-HTTP downloads
static void CL_ParseDownload( msg_t *msg ) {
	const char *svFilename = MSG_ReadString( msg );
	const size_t offset = MSG_ReadInt32( msg );
	const size_t size = MSG_ReadInt32( msg );

	if( cls.demoPlayer.playing ) {
		// ignore download commands coming from demo files
		return;
	}

	if( msg->readcount + size > msg->cursize ) {
		clWarning() << "Download message didn't have as much data as it promised";
		CL_RetryDownload();
		return;
	}

	if( !cls.download.filenum ) {
		clWarning() << "Download message while not dowloading";
		msg->readcount += size;
		return;
	}

	if( Q_stricmp( cls.download.name, svFilename ) ) {
		clWarning() << "Download message for wrong file";
		msg->readcount += size;
		return;
	}

	if( offset + size > cls.download.size ) {
		clWarning() << "Invalid download message";
		msg->readcount += size;
		CL_RetryDownload();
		return;
	}

	if( cls.download.offset != offset ) {
		clWarning() << "Download message for wrong position";
		msg->readcount += size;
		CL_RetryDownload();
		return;
	}

	FS_Write( msg->data + msg->readcount, size, cls.download.filenum );
	msg->readcount += size;
	cls.download.offset += size;
	cls.download.percent = (double)cls.download.offset / (double)cls.download.size;
	Q_clamp( cls.download.percent, 0, 1 );

	Cvar_ForceSet( "cl_download_percent", va( "%.1f", cls.download.percent * 100 ) );

	if( cls.download.offset < cls.download.size ) {
		cls.download.timeout = Sys_Milliseconds() + 3000;
		cls.download.retries = 0;

		CL_AddReliableCommand( va( "nextdl \"%s\" %" PRIu64, cls.download.name, (uint64_t)cls.download.offset ) );
	} else {
		clWarning() << "Download complete" << wsw::StringView( cls.download.name );

		CL_DownloadComplete();

		// let the server know we're done
		CL_AddReliableCommand( va( "nextdl \"%s\" %i", cls.download.name, -1 ) );

		CL_DownloadDone();
	}
}

static void CL_ParseServerData( msg_t *msg ) {
	clDebug() << "Serverdata packet received";

	// wipe the client_state_t struct

	CL_ClearState();
	CL_SetClientState( CA_CONNECTED );

	// parse protocol version number
	const int version = MSG_ReadInt32( msg );

	if( version != APP_PROTOCOL_VERSION && !( cls.demoPlayer.playing && version == APP_DEMO_PROTOCOL_VERSION ) ) {
		Com_Error( ERR_DROP, "Server returned version %i, not %i", version, APP_PROTOCOL_VERSION );
	}

	cl.servercount = MSG_ReadInt32( msg );
	cl.snapFrameTime = (unsigned int)MSG_ReadInt16( msg );
	cl.gamestart = true;

	// set extrapolation time to half snapshot time
	Cvar_ForceSet( "cl_extrapolationTime", va( "%i", (unsigned int)( cl.snapFrameTime * 0.5 ) ) );
	cl_extrapolationTime->modified = false;

	// parse player entity number
	cl.playernum = MSG_ReadInt16( msg );

	// get the full level name
	Q_strncpyz( cl.servermessage, MSG_ReadString( msg ), sizeof( cl.servermessage ) );

	const int sv_bitflags = MSG_ReadUint8( msg );

	if( cls.demoPlayer.playing ) {
		cls.reliable = ( sv_bitflags & SV_BITFLAGS_RELIABLE );
	} else {
		if( cls.reliable != ( ( sv_bitflags & SV_BITFLAGS_RELIABLE ) != 0 ) ) {
			Com_Error( ERR_DROP, "Server and client disagree about connection reliability" );
		}
	}

	// builting HTTP server port
	if( cls.httpbaseurl ) {
		Q_free( cls.httpbaseurl );
		cls.httpbaseurl = NULL;
	}

	if( ( sv_bitflags & SV_BITFLAGS_HTTP ) != 0 ) {
		if( ( sv_bitflags & SV_BITFLAGS_HTTP_BASEURL ) != 0 ) {
			// read base upstream url
			cls.httpbaseurl = Q_strdup( MSG_ReadString( msg ) );
		} else {
			const int http_portnum = MSG_ReadInt16( msg ) & 0xffff;
			cls.httpaddress = cls.serveraddress;
			if( cls.httpaddress.type == NA_IP6 ) {
				cls.httpaddress.address.ipv6.port = BigShort( http_portnum );
			} else {
				cls.httpaddress.address.ipv4.port = BigShort( http_portnum );
			}
			if( http_portnum ) {
				if( cls.httpaddress.type == NA_LOOPBACK ) {
					cls.httpbaseurl = Q_strdup( va( "http://localhost:%hu/", (unsigned short)http_portnum ) );
				} else {
					cls.httpbaseurl = Q_strdup( va( "http://%s/", NET_AddressToString( &cls.httpaddress ) ) );
				}
			}
		}
	}

	// pure list

	// clean old, if necessary
	Com_FreePureList( &cls.purelist );

	// add new
	int numpure = MSG_ReadInt16( msg );
	while( numpure > 0 ) {
		const char *pakname = MSG_ReadString( msg );
		const unsigned checksum = MSG_ReadInt32( msg );

		Com_AddPakToPureList( &cls.purelist, pakname, checksum );

		numpure--;
	}

	//assert( numpure == 0 );

	// get the configstrings request
	CL_AddReliableCommand( va( "configstrings %i 0", cl.servercount ) );

	const bool old_sv_pure = cls.sv_pure;
	cls.sv_pure = ( sv_bitflags & SV_BITFLAGS_PURE ) != 0;
	cls.pure_restart = cls.sv_pure && old_sv_pure == false;

#ifdef PURE_CHEAT
	cls.sv_pure = cls.pure_restart = false;
#endif

	clNotice() << wsw::StringView( cl.servermessage );
}

static void CL_ParseBaseline( msg_t *msg ) {
	SNAP_ParseBaseline( msg, cl_baselines );
}

static void CL_ParseFrame( msg_t *msg ) {
	snapshot_t *oldSnap = ( cl.receivedSnapNum > 0 ) ? &cl.snapShots[cl.receivedSnapNum & UPDATE_MASK] : NULL;
	const snapshot_t *snap = SNAP_ParseFrame( msg, oldSnap, cl.snapShots, cl_baselines, cl_shownet->integer );
	if( snap->valid ) {
		cl.receivedSnapNum = snap->serverFrame;

		if( cls.demoRecorder.recording ) {
			if( cls.demoRecorder.waiting && !snap->delta ) {
				cls.demoRecorder.waiting = false; // we can start recording now
				cls.demoRecorder.basetime = snap->serverTime;
				cls.demoRecorder.localtime = time( NULL );

				// write out messages to hold the startup information
				SNAP_BeginDemoRecording( cls.demoRecorder.file, 0x10000 + cl.servercount, cl.snapFrameTime,
										 cl.servermessage, cls.reliable ? SV_BITFLAGS_RELIABLE : 0, cls.purelist,
										 cl.configStrings, cl_baselines );

				// the rest of the demo file will be individual frames
			}

			if( !cls.demoRecorder.waiting ) {
				cls.demoRecorder.duration = snap->serverTime - cls.demoRecorder.basetime;
			}
			cls.demoRecorder.time = cls.demoRecorder.duration;
		}

		if( cl_debug_timeDelta->integer ) {
			if( oldSnap != NULL && ( oldSnap->serverFrame + 1 != snap->serverFrame ) ) {
				clWarning() << "Snapshot lost";
			}
		}

		// the first snap, fill all the timeDeltas with the same value
		// don't let delta add big jumps to the smoothing ( a stable connection produces jumps inside +-3 range)
		int delta = ( snap->serverTime - cl.snapFrameTime ) - cls.gametime;
		if( cl.currentSnapNum <= 0 || delta < cl.newServerTimeDelta - 175 || delta > cl.newServerTimeDelta + 175 ) {
			CL_RestartTimeDeltas( delta );
		} else {
			if( cl_debug_timeDelta->integer ) {
				if( delta < cl.newServerTimeDelta - (int)cl.snapFrameTime ) {
					Com_Printf( S_COLOR_CYAN "***** timeDelta low clamp\n" );
				} else if( delta > cl.newServerTimeDelta + (int)cl.snapFrameTime ) {
					Com_Printf( S_COLOR_CYAN "***** timeDelta high clamp\n" );
				}
			}

			Q_clamp( delta, cl.newServerTimeDelta - (int)cl.snapFrameTime, cl.newServerTimeDelta + (int)cl.snapFrameTime );

			cl.serverTimeDeltas[cl.receivedSnapNum & MASK_TIMEDELTAS_BACKUP] = delta;
		}

		float ping = 0;
		for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
			if( snap->scoreboardData.getPlayerNum( i ) == (unsigned)cl.playernum ) {
				ping = snap->scoreboardData.getPlayerShort( i, snap->scoreboardData.pingSlot );
			}
		}
		auto *const uiSystem = wsw::ui::UISystem::instance();
		uiSystem->addToPingTimeline( cls.realtime, ping );
		uiSystem->addToPacketlossTimeline( cls.realtime, cls.netchan.dropped != 0 );
	}
}

static void CL_Multiview_f( const CmdArgs &cmdArgs ) {
	cls.mv = ( atoi( Cmd_Argv( 1 ) ) != 0 );
	clNotice() << "Multiview:" << cls.mv;
}

static void CL_CvarInfoRequest_f( const CmdArgs &cmdArgs ) {
	char string[MAX_STRING_CHARS];
	const char *cvarName;
	const char *cvarString;

	if( cls.demoPlayer.playing ) {
		return;
	}

	if( Cmd_Argc() < 1 ) {
		return;
	}

	cvarName = Cmd_Argv( 1 );

	string[0] = 0;
	Q_strncatz( string, "cvarinfo \"", sizeof( string ) );

	if( strlen( string ) + strlen( cvarName ) + 1 /*quote*/ + 1 /*space*/ >= MAX_STRING_CHARS - 1 ) {
		CL_AddReliableCommand( "cvarinfo \"invalid\"" );
	} else {
		Q_strncatz( string, cvarName, sizeof( string ) );
		Q_strncatz( string, "\" ", sizeof( string ) );

		cvarString = Cvar_String( cvarName );
		if( !cvarString[0] ) {
			cvarString = "not found";
		}

		if( strlen( string ) + strlen( cvarString ) + 2 /*quotes*/ >= MAX_STRING_CHARS - 1 ) {
			if( strlen( string ) + strlen( " \"too long\"" ) < MAX_STRING_CHARS - 1 ) {
				CL_AddReliableCommand( va( "%s\"too long\"", string ) );
			} else {
				CL_AddReliableCommand( "cvarinfo \"invalid\"" );
			}
		} else {
			Q_strncatz( string, "\"", sizeof( string ) );
			Q_strncatz( string, cvarString, sizeof( string ) );
			Q_strncatz( string, "\"", sizeof( string ) );

			CL_AddReliableCommand( string );
		}
	}
}

static void CL_OptionsStatus_f( const CmdArgs &cmdArgs ) {
	if( cge ) {
		CG_OptionsStatus( cmdArgs );
	}
}

static void CL_ReloadOptions_f( const CmdArgs &cmdArgs ) {
	if( cge ) {
		CG_ReloadOptions( cmdArgs );
	}
}

static void CL_ReloadCommands_f( const CmdArgs &cmdArgs ) {
	if( cge ) {
		CG_ReloadCommands( cmdArgs );
	}
}

static void CL_UpdateConfigString( int idx, const char *s ) {
	if( s ) {
		if( cl_debug_serverCmd->integer && ( cls.state >= CA_ACTIVE || cls.demoPlayer.playing ) ) {
			Com_Printf( "CL_UpdateConfigString(%i): \"%s\"\n", idx, s );
		}

		if( idx < 0 || idx >= MAX_CONFIGSTRINGS ) {
			Com_Error( ERR_DROP, "configstring > MAX_CONFIGSTRINGS" );
		}
		if( !COM_ValidateConfigstring( s ) ) {
			Com_Error( ERR_DROP, "Invalid configstring (%i): %s", idx, s );
		}

		const wsw::StringView string( s );
		cl.configStrings.set( idx, string );
		CL_GameModule_ConfigString( idx, string );
	}
}

static void CL_AddConfigStringFragment( int index, int fragmentNum, int numFragments, int specifiedLen, const char *s ) {
	if( (unsigned)index >= (unsigned)MAX_CONFIGSTRINGS ) {
		Com_Error( ERR_DROP, "A configstring fragment index > MAX_CONFIGSTRINGS" );
	}

	if( (unsigned)numFragments >= kMaxConfigStringFragments ) {
		Com_Error( ERR_DROP, "A configstring fragment numFragments >= kMaxConfigStringFragments" );
	}

	if( (unsigned)fragmentNum >= (unsigned)numFragments ) {
		Com_Error( ERR_DROP, "configstring fragment fragmentNum is out of a valid range" );
	}

	// TODO: We should accept a string view parameter

	const size_t actualLen = std::strlen( s );
	if( (size_t)specifiedLen != actualLen ) {
		Com_Error( ERR_DROP, "A configstring fragment len mismatches the specified one" );
	}

	if( cl.configStringFragmentIndex && cl.configStringFragmentIndex != index ) {
		Com_Error( ERR_DROP, "Got a configstring fragment while the current one is incompletely assembled" );
	}

	if( cl.configStringFragmentNum && cl.configStringFragmentNum + 1 != fragmentNum ) {
		Com_Error( ERR_DROP, "Got an illegal configstring fragments sequence" );
	}

	const size_t fragmentOffset = fragmentNum * kMaxConfigStringFragmentLen;
	std::memcpy( cl.configStringFragmentsBuffer + fragmentOffset, s, specifiedLen );

	if( fragmentNum + 1 != numFragments ) {
		cl.configStringFragmentIndex = index;
		cl.configStringFragmentNum = fragmentNum;
	} else {
		cl.configStringFragmentIndex = 0;
		cl.configStringFragmentNum = 0;

		const size_t combinedLen = fragmentOffset + specifiedLen;
		cl.configStringFragmentsBuffer[combinedLen] = '\0';

		const wsw::StringView view( cl.configStringFragmentsBuffer, combinedLen, wsw::StringView::ZeroTerminated );

		cl.configStrings.set( index, view );
		CL_GameModule_ConfigString( index, view );
	}
}

static void CL_ParseConfigstringCommand( const CmdArgs &cmdArgs ) {
	const int argc = Cmd_Argc();
	if( argc < 3 ) {
		return;
	}

	// ch : configstrings may come batched now, so lets loop through them
	for( int i = 1; i < argc - 1; i += 2 ) {
		const int idx = atoi( Cmd_Argv( i ) );
		const char *s = Cmd_Argv( i + 1 );

		CL_UpdateConfigString( idx, s );
	}
}

static void CL_ParseConfigStringFragmentCommand( const CmdArgs &cmdArgs ) {
	const int argc = Cmd_Argc();
	if( argc != 6 ) {
		return;
	}

	const int index = atoi( Cmd_Argv( 1 ) );
	const int fragmentNum = atoi( Cmd_Argv( 2 ) );
	const int numFragments = atoi( Cmd_Argv( 3 ) );
	const int specifiedLen = atoi( Cmd_Argv( 4 ) );

	CL_AddConfigStringFragment( index, fragmentNum, numFragments, specifiedLen, Cmd_Argv( 5 ) );
}

typedef struct {
	const char *name;
	void ( *func )( const CmdArgs & );
} svcmd_t;

static svcmd_t svcmds[] =
	{
		{ "forcereconnect", CL_Reconnect_f },
		{ "reconnect", CL_ServerReconnect_f },
		{ "changing", CL_Changing_f },
		{ "precache", CL_Precache_f },
		{ "cmd", CL_ForwardToServer_f },
		{ "cs", CL_ParseConfigstringCommand },
		{ "csf", CL_ParseConfigStringFragmentCommand },
		{ "disconnect", CL_ServerDisconnect_f },
		{ "initdownload", CL_InitDownload_f },
		{ "multiview", CL_Multiview_f },
		{ "cvarinfo", CL_CvarInfoRequest_f },
		// UI expects these commands to be reliable
		{ "optionsstatus", CL_OptionsStatus_f },
		// TODO: There should be better/generic facilities to notify of gametype change/restart
		{ "reloadoptions", CL_ReloadOptions_f },
		{ "reloadcommands", CL_ReloadCommands_f },

		{ NULL, NULL }
	};

static void CL_ParseServerCommand( msg_t *msg ) {
	const char *text = MSG_ReadString( msg );

	static CmdArgsSplitter argsSplitter;
	const CmdArgs &cmdArgs = argsSplitter.exec( wsw::StringView( text ) );

	if( cl_debug_serverCmd->integer && ( cls.state < CA_ACTIVE || cls.demoPlayer.playing ) ) {
		Com_Printf( "CL_ParseServerCommand: \"%s\"\n", text );
	}

	// filter out these server commands to be called from the client
	for( svcmd_t *cmd = svcmds; cmd->name; cmd++ ) {
		if( !strcmp( cmdArgs[0].data(), cmd->name ) ) {
			cmd->func( cmdArgs );
			return;
		}
	}

	Com_Printf( "Unknown server command: %s\n", cmdArgs[0].data() );
}

const char * const svc_strings[256] =
	{
		"svc_bad",
		"svc_nop",
		"svc_servercmd",
		"svc_serverdata",
		"svc_spawnbaseline",
		"svc_download",
		"svc_playerinfo",
		"svc_packetentities",
		"svc_gamecommands",
		"svc_match",
		"svc_clcack",
		"svc_servercs", // reliable command as unreliable for demos
		"svc_frame",
		"svc_demoinfo",
		"svc_extension"
	};


void CL_ParseServerMessage( msg_t *msg ) {
	if( cl_shownet->integer == 1 ) {
		Com_Printf( "%" PRIu64 " ", (uint64_t)msg->cursize );
	} else if( cl_shownet->integer >= 2 ) {
		Com_Printf( "------------------\n" );
	}

	// parse the message
	while( msg->readcount < msg->cursize ) {
		size_t meta_data_maxsize;

		const int cmd = MSG_ReadUint8( msg );
		if( cl_debug_serverCmd->integer & 4 ) {
			const char *format = "%3" PRIi64 ":CMD %i %s\n";
			Com_Printf( format, (int64_t)( msg->readcount - 1 ), cmd, !svc_strings[cmd] ? "bad" : svc_strings[cmd] );
		}

		if( cl_shownet->integer >= 2 ) {
			if( !svc_strings[cmd] ) {
				const char *format = "%3" PRIi64 ":BAD CMD %i\n";
				Com_Printf( format, (int64_t)( msg->readcount - 1 ), cmd );
			} else {
				SHOWNET( msg, svc_strings[cmd] );
			}
		}

		// other commands
		switch( cmd ) {
			default:
				Com_Error( ERR_DROP, "CL_ParseServerMessage: Illegible server message" );
				break;

			case svc_nop:
				// Com_Printf( "svc_nop\n" );
				break;

			case svc_servercmd:
				if( !cls.reliable ) {
					int cmdNum = MSG_ReadInt32( msg );
					if( cmdNum < 0 ) {
						Com_Error( ERR_DROP, "CL_ParseServerMessage: Invalid cmdNum value received: %i\n",
								   cmdNum );
						return;
					}
					if( cmdNum <= cls.lastExecutedServerCommand ) {
						MSG_ReadString( msg ); // read but ignore
						break;
					}
					cls.lastExecutedServerCommand = cmdNum;
				}
				[[fallthrough]];
			case svc_servercs: // configstrings from demo files. they don't have acknowledge
				CL_ParseServerCommand( msg );
				break;

			case svc_serverdata:
				if( cls.state == CA_HANDSHAKE ) {
					CL_Cbuf_ExecutePendingCommands(); // make sure any stuffed commands are done
					CL_ParseServerData( msg );
				} else {
					return; // ignore rest of the packet (serverdata is always sent alone)
				}
				break;

			case svc_spawnbaseline:
				CL_ParseBaseline( msg );
				break;

			case svc_download:
				CL_ParseDownload( msg );
				break;

			case svc_clcack:
				if( cls.reliable ) {
					Com_Error( ERR_DROP, "CL_ParseServerMessage: clack message for reliable client\n" );
					return;
				}
				cls.reliableAcknowledge = MSG_ReadUintBase128( msg );
				cls.ucmdAcknowledged = MSG_ReadUintBase128( msg );
				cls.lastReceivedRealServerTime = MSG_ReadIntBase128( msg );
				if( cl_debug_serverCmd->integer & 4 ) {
					const char *format = "svc_clcack:reliable cmd ack:%" PRIi64 " ucmdack:%" PRIi64 "\n";
					Com_Printf( format, cls.reliableAcknowledge, cls.ucmdAcknowledged );
				}
				break;

			case svc_frame:
				CL_ParseFrame( msg );
				break;

			case svc_demoinfo:
				assert( cls.demoPlayer.playing );

				MSG_ReadInt32( msg );
				MSG_ReadInt32( msg );
				cls.demoPlayer.meta_data_realsize = (size_t)MSG_ReadInt32( msg );
				meta_data_maxsize = (size_t)MSG_ReadInt32( msg );

				// sanity check
				if( cls.demoPlayer.meta_data_realsize > meta_data_maxsize ) {
					cls.demoPlayer.meta_data_realsize = meta_data_maxsize;
				}
				if( cls.demoPlayer.meta_data_realsize > sizeof( cls.demoPlayer.meta_data ) ) {
					cls.demoPlayer.meta_data_realsize = sizeof( cls.demoPlayer.meta_data );
				}

				MSG_ReadData( msg, cls.demoPlayer.meta_data, cls.demoPlayer.meta_data_realsize );
				MSG_SkipData( msg, meta_data_maxsize - cls.demoPlayer.meta_data_realsize );
				break;

			case svc_playerinfo:
			case svc_packetentities:
			case svc_match:
				Com_Error( ERR_DROP, "Out of place frame data" );
				break;

			case svc_extension:
				const int ext = MSG_ReadUint8( msg );  // extension id
				MSG_ReadUint8( msg );        // version number
				const int len = MSG_ReadInt16( msg ); // command length

				switch( ext ) {
					default:
						// unsupported
						MSG_SkipData( msg, len );
						break;
				}
				break;
		}
	}

	if( msg->readcount > msg->cursize ) {
		Com_Error( ERR_DROP, "CL_ParseServerMessage: Bad server message" );
		return;
	}

	if( cl_debug_serverCmd->integer & 4 ) {
		Com_Printf( "%3" PRIi64 ":CMD %i %s\n", (int64_t)( msg->readcount ), -1, "EOF" );
	}
	SHOWNET( msg, "END OF MESSAGE" );

	//
	// if recording demos, copy the message out
	//
	//
	// we don't know if it is ok to save a demo message until
	// after we have parsed the frame
	//
	if( cls.demoRecorder.recording && !cls.demoRecorder.waiting ) {
		CL_WriteDemoMessage( msg );
	}
}

void _SHOWNET( msg_t *msg, const char *s, int shownet ) {
	if( shownet >= 2 ) {
		Com_Printf( "%3" PRIi64 ":%s\n", (int64_t)( msg->readcount - 1 ), s );
	}
}

static void SNAP_ParseDeltaGameState( msg_t *msg, snapshot_t *oldframe, snapshot_t *newframe ) {
	MSG_ReadDeltaGameState( msg, oldframe ? &oldframe->gameState : NULL, &newframe->gameState );
}

static void SNAP_ParsePlayerstate( msg_t *msg, const player_state_t *oldstate, player_state_t *state ) {
	MSG_ReadDeltaPlayerState( msg, oldstate, state );
}

static void SNAP_ParseScoreboard( msg_t *msg, snapshot_t *oldframe, snapshot_t *newframe ) {
	MSG_ReadDeltaScoreboardData( msg, oldframe ? &oldframe->scoreboardData : NULL, &newframe->scoreboardData );
}

/*
* SNAP_ParseDeltaEntity
*
* Parses deltas from the given base and adds the resulting entity to the current frame
*/
static void SNAP_ParseDeltaEntity( msg_t *msg, snapshot_t *frame, int newnum, entity_state_t *old, unsigned byteMask ) {
	entity_state_t *state = &frame->parsedEntities[frame->numEntities & ( MAX_PARSE_ENTITIES - 1 )];
	frame->numEntities++;
	MSG_ReadDeltaEntity( msg, old, state, newnum, byteMask );
}

void SNAP_ParseBaseline( msg_t *msg, entity_state_t *baselines ) {
	bool remove = false;
	unsigned byteMask = 0;
	const int newnum = MSG_ReadEntityNumber( msg, &remove, &byteMask );
	assert( remove == false );

	if( !remove ) {
		entity_state_t nullstate, tmp;
		memset( &nullstate, 0, sizeof( nullstate ) );

		entity_state_t *const es = ( baselines ? &baselines[newnum] : &tmp );
		MSG_ReadDeltaEntity( msg, &nullstate, es, newnum, byteMask );
	}
}

/*
* SNAP_ParsePacketEntities
*
* An svc_packetentities has just been parsed, deal with the
* rest of the data stream.
*/
static void SNAP_ParsePacketEntities( msg_t *msg, snapshot_t *oldframe, snapshot_t *newframe, entity_state_t *baselines, int shownet ) {
	newframe->numEntities = 0;

	int oldEntNum;
	int oldEntIndex          = 0;
	entity_state_t *oldstate = nullptr;
	if( !oldframe ) {
		oldEntNum = 99999;
	} else if( oldEntIndex >= oldframe->numEntities ) {
		oldEntNum = 99999;
	} else {
		oldstate = &oldframe->parsedEntities[oldEntIndex & ( MAX_PARSE_ENTITIES - 1 )];
		oldEntNum = oldstate->number;
	}

	// TODO: This logic is convoluted, clean it up
	for(;;) {
		bool remove         = false;
		unsigned byteMask   = 0;
		const int newEntNum = MSG_ReadEntityNumber( msg, &remove, &byteMask );
		if( newEntNum >= MAX_EDICTS ) {
			Com_Error( ERR_DROP, "CL_ParsePacketEntities: bad number:%i", newEntNum );
		}
		if( msg->readcount > msg->cursize ) {
			Com_Error( ERR_DROP, "CL_ParsePacketEntities: end of message" );
		}

		if( !newEntNum ) {
			break;
		}

		// One or more entities from the old packet are unchanged.
		// Add these entities to the new frame.
		while( oldEntNum < newEntNum ) {
			if( shownet == 3 ) {
				Com_Printf( "   unchanged: %i\n", oldEntNum );
			}

			// TODO: The name should make the fact that it adds an entity to frame obvious
			SNAP_ParseDeltaEntity( msg, newframe, oldEntNum, oldstate, 0 );

			oldEntIndex++;
			if( oldEntIndex >= oldframe->numEntities ) {
				oldEntNum = 99999;
			} else {
				oldstate = &oldframe->parsedEntities[oldEntIndex & ( MAX_PARSE_ENTITIES - 1 )];
				oldEntNum = oldstate->number;
			}
		}

		assert( oldEntNum >= newEntNum );

		// delta from baseline
		if( oldEntNum > newEntNum ) {
			if( remove ) {
				Com_Printf( "U_REMOVE: oldnum > newnum (can't remove from baseline!)\n" );
			} else {
				// delta from baseline
				if( shownet == 3 ) {
					Com_Printf( "   baseline: %i\n", newEntNum );
				}
				SNAP_ParseDeltaEntity( msg, newframe, newEntNum, &baselines[newEntNum], byteMask );
			}
		} else {
			if( remove ) {
				// the entity present in oldframe is not in the current frame
				if( shownet == 3 ) {
					Com_Printf( "   remove: %i\n", newEntNum );
				}

				if( oldEntNum != newEntNum ) {
					Com_Printf( "U_REMOVE: oldnum != newnum\n" );
				}

				oldEntIndex++;
				if( oldEntIndex >= oldframe->numEntities ) {
					oldEntNum = 99999;
				} else {
					oldstate = &oldframe->parsedEntities[oldEntIndex & ( MAX_PARSE_ENTITIES - 1 )];
					oldEntNum = oldstate->number;
				}
			} else {
				// delta from previous state
				if( shownet == 3 ) {
					Com_Printf( "   delta: %i\n", newEntNum );
				}

				SNAP_ParseDeltaEntity( msg, newframe, newEntNum, oldstate, byteMask );

				oldEntIndex++;
				if( oldEntIndex >= oldframe->numEntities ) {
					oldEntNum = 99999;
				} else {
					oldstate = &oldframe->parsedEntities[oldEntIndex & ( MAX_PARSE_ENTITIES - 1 )];
					oldEntNum = oldstate->number;
				}
			}
		}
	}

	// any remaining entities in the old frame are copied over
	while( oldEntNum != 99999 ) {
		// one or more entities from the old packet are unchanged
		if( shownet == 3 ) {
			Com_Printf( "   unchanged: %i\n", oldEntNum );
		}

		SNAP_ParseDeltaEntity( msg, newframe, oldEntNum, oldstate, 0 );

		oldEntIndex++;
		if( oldEntIndex >= oldframe->numEntities ) {
			oldEntNum = 99999;
		} else {
			oldstate = &oldframe->parsedEntities[oldEntIndex & ( MAX_PARSE_ENTITIES - 1 )];
			oldEntNum = oldstate->number;
		}
	}
}

static snapshot_t *SNAP_ParseFrameHeader( msg_t *msg, snapshot_t *newframe, snapshot_t *backup, bool skipBody ) {
	// get total length
	const int len = MSG_ReadInt16( msg );
	const int pos = msg->readcount;

	// get the snapshot id
	const int64_t serverTime = MSG_ReadIntBase128( msg );
	const int snapNum = MSG_ReadUintBase128( msg );

	if( backup ) {
		newframe = &backup[snapNum & UPDATE_MASK];
	}

	int areabytes = newframe->areabytes;
	uint8_t *areabits = newframe->areabits;
	memset( newframe, 0, sizeof( snapshot_t ) );
	newframe->areabytes = areabytes;
	newframe->areabits = areabits;

	newframe->serverTime = serverTime;
	newframe->serverFrame = snapNum;
	newframe->deltaFrameNum = MSG_ReadUintBase128( msg );
	newframe->ucmdExecuted = MSG_ReadUintBase128( msg );

	const int flags = MSG_ReadUint8( msg );
	newframe->delta = ( flags & FRAMESNAP_FLAG_DELTA ) ? true : false;
	newframe->multipov = ( flags & FRAMESNAP_FLAG_MULTIPOV ) ? true : false;
	newframe->allentities = ( flags & FRAMESNAP_FLAG_ALLENTITIES ) ? true : false;

	// Skip suppress count (TODO remove it)
	(void)MSG_ReadUint8( msg );

	// validate the new frame
	newframe->valid = false;

	// If the frame is delta compressed from data that we
	// no longer have available, we must suck up the rest of
	// the frame, but not use it, then ask for a non-compressed
	// message
	if( !newframe->delta ) {
		newframe->valid = true; // uncompressed frame
	} else {
		if( newframe->deltaFrameNum <= 0 ) {
			clWarning() << "Invalid delta frame (not supposed to happen!)";
		} else if( backup ) {
			snapshot_t *deltaframe = &backup[newframe->deltaFrameNum & UPDATE_MASK];
			if( !deltaframe->valid ) {
				// should never happen
				clWarning() << "Delta from invalid frame (not supposed to happen!)";
			} else if( deltaframe->serverFrame != newframe->deltaFrameNum ) {
				// The frame that the server did the delta from
				// is too old, so we can't reconstruct it properly.
				clWarning() << "Delta frame too old";
			} else {
				newframe->valid = true; // valid delta parse
			}
		} else {
			newframe->valid = skipBody;
		}
	}

	if( skipBody ) {
		MSG_SkipData( msg, len - ( msg->readcount - pos ) );
	}

	return newframe;
}

void SNAP_SkipFrame( msg_t *msg, snapshot_t *header ) {
	static snapshot_t frame;
	SNAP_ParseFrameHeader( msg, header ? header : &frame, NULL, true );
}

snapshot_t *SNAP_ParseFrame( msg_t *msg, snapshot_t *lastFrame, snapshot_t *backup, entity_state_t *baselines, int showNet ) {
	// read header
	snapshot_t *newframe = SNAP_ParseFrameHeader( msg, NULL, backup, false );
	snapshot_t *deltaframe = NULL;

	if( showNet == 3 ) {
		Com_Printf( "   frame:%" PRIi64 "  old:%" PRIi64 "%s\n", newframe->serverFrame, newframe->deltaFrameNum,
					( newframe->delta ? "" : " no delta" ) );
	}

	if( newframe->delta ) {
		if( newframe->deltaFrameNum > 0 ) {
			deltaframe = &backup[newframe->deltaFrameNum & UPDATE_MASK];
		}
	}

	// read game commands
	int cmd = MSG_ReadUint8( msg );
	if( cmd != svc_gamecommands ) {
		Com_Error( ERR_DROP, "SNAP_ParseFrame: not gamecommands" );
	}

	int numtargets = 0, framediff;
	while( ( framediff = MSG_ReadInt16( msg ) ) != -1 ) {
		const char *text = MSG_ReadString( msg );

		// see if it's valid and not yet handled
		if( newframe->valid && ( !lastFrame || !lastFrame->valid || newframe->serverFrame > lastFrame->serverFrame + framediff ) ) {
			newframe->numgamecommands++;
			if( newframe->numgamecommands > MAX_PARSE_GAMECOMMANDS ) {
				Com_Error( ERR_DROP, "SNAP_ParseFrame: too many gamecommands" );
			}
			if( newframe->gamecommandsDataHead + strlen( text ) >= sizeof( newframe->gamecommandsData ) ) {
				Com_Error( ERR_DROP, "SNAP_ParseFrame: too much gamecommands" );
			}

			gcommand_t *gcmd = &newframe->gamecommands[newframe->numgamecommands - 1];
			gcmd->all = true;

			Q_strncpyz( newframe->gamecommandsData + newframe->gamecommandsDataHead, text,
						sizeof( newframe->gamecommandsData ) - newframe->gamecommandsDataHead );
			gcmd->commandOffset = newframe->gamecommandsDataHead;
			newframe->gamecommandsDataHead += strlen( text ) + 1;

			if( newframe->multipov ) {
				numtargets = MSG_ReadUint8( msg );
				if( numtargets ) {
					if( numtargets > (int)sizeof( gcmd->targets ) ) {
						Com_Error( ERR_DROP, "SNAP_ParseFrame: too many gamecommand targets" );
					}
					gcmd->all = false;
					MSG_ReadData( msg, gcmd->targets, numtargets );
				}
			}
		} else if( newframe->multipov ) {   // otherwise, ignore it
			numtargets = MSG_ReadUint8( msg );
			MSG_SkipData( msg, numtargets );
		}
	}

	// read areabits
	const size_t len = (size_t)MSG_ReadUint8( msg );
	if( len > newframe->areabytes ) {
		Com_Error( ERR_DROP, "Invalid areabits size: %" PRIu64 " > %" PRIu64, (uint64_t)len, (uint64_t)newframe->areabytes );
	}

	memset( newframe->areabits, 0, newframe->areabytes );
	MSG_ReadData( msg, newframe->areabits, len );

	// read match info
	cmd = MSG_ReadUint8( msg );
	_SHOWNET( msg, svc_strings[cmd], showNet );
	if( cmd != svc_match ) {
		Com_Error( ERR_DROP, "SNAP_ParseFrame: not match info" );
	}
	SNAP_ParseDeltaGameState( msg, deltaframe, newframe );
	cmd = MSG_ReadUint8( msg );
	if( cmd != svc_scoreboard ) {
		Com_Error( ERR_DROP, "SNAP_ParseFrame: not scoreboard" );
	}
	SNAP_ParseScoreboard( msg, deltaframe, newframe );

	// read playerinfos
	int numplayers = 0;
	while( ( cmd = MSG_ReadUint8( msg ) ) ) {
		_SHOWNET( msg, svc_strings[cmd], showNet );
		if( cmd != svc_playerinfo ) {
			Com_Error( ERR_DROP, "SNAP_ParseFrame: not playerinfo" );
		}
		if( deltaframe && deltaframe->numplayers >= numplayers ) {
			SNAP_ParsePlayerstate( msg, &deltaframe->playerStates[numplayers], &newframe->playerStates[numplayers] );
		} else {
			SNAP_ParsePlayerstate( msg, NULL, &newframe->playerStates[numplayers] );
		}
		numplayers++;
	}
	newframe->numplayers = numplayers;

	// read packet entities
	cmd = MSG_ReadUint8( msg );
	_SHOWNET( msg, svc_strings[cmd], showNet );
	if( cmd != svc_packetentities ) {
		Com_Error( ERR_DROP, "SNAP_ParseFrame: not packetentities" );
	}
	SNAP_ParsePacketEntities( msg, deltaframe, newframe, baselines, showNet );

	return newframe;
}

static void CL_PauseDemo( bool paused );

/*
* CL_WriteDemoMessage
*
* Dumps the current net message, prefixed by the length
*/
void CL_WriteDemoMessage( msg_t *msg ) {
	if( cls.demoRecorder.file <= 0 ) {
		cls.demoRecorder.recording = false;
	} else {
		// the first eight bytes are just packet sequencing stuff
		SNAP_RecordDemoMessage( cls.demoRecorder.file, msg, 8 );
	}
}

/*
* CL_Stop_f
*
* stop recording a demo
*/
void CL_Stop_f( const CmdArgs &cmdArgs ) {
	// look through all the args
	bool silent = false;
	bool cancel = false;
	for( int arg = 1; arg < Cmd_Argc(); arg++ ) {
		if( !Q_stricmp( Cmd_Argv( arg ), "silent" ) ) {
			silent = true;
		} else if( !Q_stricmp( Cmd_Argv( arg ), "cancel" ) ) {
			cancel = true;
		}
	}

	if( !cls.demoRecorder.recording ) {
		if( !silent ) {
			clNotice() << "Not recording a demo";
		}
		return;
	}

	// finish up
	SNAP_StopDemoRecording( cls.demoRecorder.file );

	using namespace wsw;

	char metadata[SNAP_MAX_DEMO_META_DATA_SIZE];
	DemoMetadataWriter writer( metadata );

	// write some meta information about the match/demo
	writer.writePair( kDemoKeyServerName, ::cl.configStrings.getHostName().value() );
	writer.writePair( kDemoKeyTimestamp, wsw::StringView( va( "%" PRIu64, (uint64_t)cls.demoRecorder.localtime ) ) );
	writer.writePair( kDemoKeyDuration, wsw::StringView( va( "%u", (int)ceil( cls.demoRecorder.duration / 1000.0f ) ) ) );
	writer.writePair( kDemoKeyMapName, ::cl.configStrings.getMapName().value() );
	writer.writePair( kDemoKeyMapChecksum, ::cl.configStrings.getMapCheckSum().value() );
	writer.writePair( kDemoKeyGametype, ::cl.configStrings.getGametypeName().value() );

	writer.writeTag( kDemoTagSinglePov );

	FS_FCloseFile( cls.demoRecorder.file );

	const auto [metadataSize, wasComplete] = writer.markCurrentResult();
	if( !wasComplete ) {
		clWarning() << "The demo metadata was truncated";
	}

	SNAP_WriteDemoMetaData( cls.demoRecorder.filename, metadata, metadataSize );

	// cancel the demos
	if( cancel ) {
		// remove the file that correspond to cls.demoRecorder.file
		if( !silent ) {
			clNotice() << "Canceling demo" << wsw::StringView( cls.demoRecorder.filename );
		}
		if( !FS_RemoveFile( cls.demoRecorder.filename ) && !silent ) {
			clWarning() << "Error canceling demo";
		}
	}

	if( !silent ) {
		clNotice() << "Stopped demo" << wsw::StringView( cls.demoRecorder.filename );
	}

	cls.demoRecorder.file = 0; // file id
	Q_free( cls.demoRecorder.filename );
	Q_free( cls.demoRecorder.name );
	cls.demoRecorder.filename = NULL;
	cls.demoRecorder.name = NULL;
	cls.demoRecorder.recording = false;
}

/*
* CL_Record_f
*
* record <demoname>
*
* Begins recording a demo from the current position
*/
void CL_Record_f( const CmdArgs &cmdArgs ) {
	if( cls.state != CA_ACTIVE ) {
		clNotice() << "You must be in a level to record";
		return;
	}

	if( Cmd_Argc() < 2 ) {
		clNotice() << "record <demoname>";
		return;
	}

	const bool silent = Cmd_Argc() > 2 && !Q_stricmp( Cmd_Argv( 2 ), "silent" );
	if( cls.demoPlayer.playing ) {
		if( !silent ) {
			clNotice() << "You can't record from another demo";
		}
		return;
	}

	if( cls.demoRecorder.recording ) {
		if( !silent ) {
			clNotice() << "Already recording";
		}
		return;
	}

	//
	// open the demo file
	//
	const char *demoname = Cmd_Argv( 1 );
	const size_t name_size = sizeof( char ) * ( strlen( "demos/" ) + strlen( demoname ) + strlen( APP_DEMO_EXTENSION_STR ) + 1 );
	char *name = (char *)Q_malloc( name_size );

	Q_snprintfz( name, name_size, "demos/%s", demoname );
	COM_SanitizeFilePath( name );
	COM_DefaultExtension( name, APP_DEMO_EXTENSION_STR, name_size );

	if( !COM_ValidateRelativeFilename( name ) ) {
		if( !silent ) {
			clNotice() << "Invalid filename";
		}
		Q_free( name );
	} else {
		if( FS_FOpenFile( name, &cls.demoRecorder.file, FS_WRITE | SNAP_DEMO_GZ ) == -1 ) {
			clWarning() << "Couldn't create the demo file" << wsw::StringView( name );
			Q_free( name );
		} else {
			if( !silent ) {
				clNotice() << "Recording demo" << wsw::StringView( name );
			}

			// store the name in case we need it later
			cls.demoRecorder.filename = name;
			cls.demoRecorder.recording = true;
			cls.demoRecorder.basetime = cls.demoRecorder.duration = cls.demoRecorder.time = 0;
			cls.demoRecorder.name = Q_strdup( demoname );

			// don't start saving messages until a non-delta compressed message is received
			CL_AddReliableCommand( "nodelta" ); // request non delta compressed frame from server
			cls.demoRecorder.waiting = true;
		}
	}
}

/*
* CL_DemoCompleted
*
* Close the demo file and disable demo state. Called from disconnection proccess
*/
void CL_DemoCompleted( void ) {
	if( cls.demoPlayer.demofilehandle ) {
		FS_FCloseFile( cls.demoPlayer.demofilehandle );
		cls.demoPlayer.demofilehandle = 0;
	}

	cls.demoPlayer.demofilelen = cls.demoPlayer.demofilelentotal = 0;

	cls.demoPlayer.playing = false;
	cls.demoPlayer.time = 0;
	Q_free( cls.demoPlayer.filename );
	cls.demoPlayer.filename = NULL;
	Q_free( cls.demoPlayer.name );
	cls.demoPlayer.name = NULL;

	Com_SetDemoPlaying( false );

	CL_PauseDemo( false );

	clNotice() << "Demo completed";

	memset( &cls.demoPlayer, 0, sizeof( cls.demoPlayer ) );
}

/*
* CL_ReadDemoMessage
*
* Read a packet from the demo file and send it to the messages parser
*/
static void CL_ReadDemoMessage( void ) {
	// TODO: Is it reachable???
	if( !cls.demoPlayer.demofilehandle ) {
		CL_Disconnect( NULL );
		return;
	}

	// TODO: This should be a member or a local
	static uint8_t msgbuf[MAX_MSGLEN];
	static msg_t demomsg;
	static bool init = true;

	if( init ) {
		MSG_Init( &demomsg, msgbuf, sizeof( msgbuf ) );
		init = false;
	}

	if( const int read = SNAP_ReadDemoMessage( cls.demoPlayer.demofilehandle, &demomsg ); read == -1 ) {
		if( cls.demoPlayer.pause_on_stop ) {
			cls.demoPlayer.paused = true;
		} else {
			CL_Disconnect( NULL );
		}
	} else {
		CL_ParseServerMessage( &demomsg );
	}
}

/*
* CL_ReadDemoPackets
*
* See if it's time to read a new demo packet
*/
void CL_ReadDemoPackets( void ) {
	if( cls.demoPlayer.paused ) {
		return;
	}

	while( cls.demoPlayer.playing && ( cl.receivedSnapNum <= 0 || !cl.snapShots[cl.receivedSnapNum & UPDATE_MASK].valid || cl.snapShots[cl.receivedSnapNum & UPDATE_MASK].serverTime < cl.serverTime ) ) {
		CL_ReadDemoMessage();
		if( cls.demoPlayer.paused ) {
			return;
		}
	}

	cls.demoPlayer.time = cls.gametime;
	cls.demoPlayer.play_jump = false;
}

/*
* CL_LatchedDemoJump
*
* See if it's time to read a new demo packet
*/
void CL_LatchedDemoJump( void ) {
	if( cls.demoPlayer.paused || !cls.demoPlayer.play_jump_latched ) {
		return;
	}

	cls.gametime = cls.demoPlayer.play_jump_time;

	if( cl.serverTime < cl.snapShots[cl.receivedSnapNum & UPDATE_MASK].serverTime ) {
		cl.pendingSnapNum = 0;
	}

	CL_AdjustServerTime( 1 );

	if( cl.serverTime < cl.snapShots[cl.receivedSnapNum & UPDATE_MASK].serverTime ) {
		cls.demoPlayer.demofilelen = cls.demoPlayer.demofilelentotal;
		FS_Seek( cls.demoPlayer.demofilehandle, 0, FS_SEEK_SET );
		cl.currentSnapNum = cl.receivedSnapNum = 0;
	}

	cls.demoPlayer.play_jump = true;
	cls.demoPlayer.play_jump_latched = false;
}

static void CL_StartDemo( const char *demoname, bool pause_on_stop ) {
	// have to copy the argument now, since next actions will lose it
	char *servername = Q_strdup( demoname );
	COM_SanitizeFilePath( servername );

	size_t name_size = sizeof( char ) * ( strlen( "demos/" ) + strlen( servername ) + strlen( APP_DEMO_EXTENSION_STR ) + 1 );
	char *name = (char *)Q_malloc( name_size );

	Q_snprintfz( name, name_size, "demos/%s", servername );
	COM_DefaultExtension( name, APP_DEMO_EXTENSION_STR, name_size );

	const char *filename = NULL;
	if( COM_ValidateRelativeFilename( name ) ) {
		filename = name;
	}

	int tempdemofilehandle = 0, tempdemofilelen = -1;
	if( filename ) {
		tempdemofilelen = FS_FOpenFile( filename, &tempdemofilehandle, FS_READ | SNAP_DEMO_GZ );  // open the demo file
	}

	if( !tempdemofilehandle ) {
		// relative filename didn't work, try launching a demo from absolute path
		Q_snprintfz( name, name_size, "%s", servername );
		COM_DefaultExtension( name, APP_DEMO_EXTENSION_STR, name_size );
		tempdemofilelen = FS_FOpenAbsoluteFile( name, &tempdemofilehandle, FS_READ | SNAP_DEMO_GZ );
	}

	if( !tempdemofilehandle ) {
		clWarning() << "No valid demo file found";
		FS_FCloseFile( tempdemofilehandle );
	} else {
		// make sure a local server is killed
		CL_Cmd_ExecuteNow( "killserver\n" );
		CL_Disconnect( NULL );

		memset( &cls.demoPlayer, 0, sizeof( cls.demoPlayer ) );

		cls.demoPlayer.demofilehandle = tempdemofilehandle;
		cls.demoPlayer.demofilelentotal = tempdemofilelen;
		cls.demoPlayer.demofilelen = cls.demoPlayer.demofilelentotal;

		cls.servername = Q_strdup( COM_FileBase( servername ) );
		COM_StripExtension( cls.servername );

		CL_SetClientState( CA_HANDSHAKE );
		Com_SetDemoPlaying( true );
		cls.demoPlayer.playing = true;
		cls.demoPlayer.time = 0;

		cls.demoPlayer.pause_on_stop = pause_on_stop;
		cls.demoPlayer.play_ignore_next_frametime = false;
		cls.demoPlayer.play_jump = false;
		cls.demoPlayer.filename = Q_strdup( name );
		cls.demoPlayer.name = Q_strdup( servername );

		CL_PauseDemo( false );

		// set up for timedemo settings
		memset( &cl.timedemo, 0, sizeof( cl.timedemo ) );
	}

	Q_free( name );
	Q_free( servername );
}

/*
* CL_PlayDemo_f
*
* demo <demoname>
*/
void CL_PlayDemo_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() < 2 ) {
		clNotice() << "demo <demoname> [pause_on_stop]\n";
	} else {
		CL_StartDemo(Cmd_Argv( 1 ), atoi(Cmd_Argv( 2 )) != 0 );
	}
}

static void CL_PauseDemo( bool paused ) {
	cls.demoPlayer.paused = paused;
}

void CL_PauseDemo_f( const CmdArgs &cmdArgs ) {
	if( !cls.demoPlayer.playing ) {
		clNotice() << "Can only demopause when playing a demo";
		return;
	}

	if( Cmd_Argc() > 1 ) {
		if( !Q_stricmp( Cmd_Argv( 1 ), "on" ) ) {
			CL_PauseDemo( true );
		} else if( !Q_stricmp( Cmd_Argv( 1 ), "off" ) ) {
			CL_PauseDemo( false );
		}
	} else {
		CL_PauseDemo( !cls.demoPlayer.paused );
	}
}

void CL_DemoJump_f( const CmdArgs &cmdArgs ) {
	if( !cls.demoPlayer.playing ) {
		clNotice() << "Can only demojump when playing a demo";
		return;
	}

	if( Cmd_Argc() != 2 ) {
		clNotice() << "Usage: demojump <time>";
		clNotice() << "Time format is [minutes:]seconds";
		clNotice() << "Use '+' or '-' in front of the time to specify it in relation to current position";
		return;
	}

	const char *p = Cmd_Argv( 1 );

	bool relative;
	if( Cmd_Argv( 1 )[0] == '+' || Cmd_Argv( 1 )[0] == '-' ) {
		relative = true;
		p++;
	} else {
		relative = false;
	}

	int time;
	if( strchr( p, ':' ) ) {
		time = ( atoi( p ) * 60 + atoi( strchr( p, ':' ) + 1 ) ) * 1000;
	} else {
		time = atoi( p ) * 1000;
	}

	if( Cmd_Argv( 1 )[0] == '-' ) {
		time = -time;
	}

	if( relative ) {
		cls.demoPlayer.play_jump_time = cls.gametime + time;
	} else {
		cls.demoPlayer.play_jump_time = time; // gametime always starts from 0
	}
	cls.demoPlayer.play_jump_latched = true;
}

static void CL_Userinfo_f( const CmdArgs & ) {
	clNotice() << "User info settings";
	Info_Print( Cvar_Userinfo() );
}

static unsigned int CL_LoadMap( const char *name ) {
	assert( !cl.cms );

	// TODO: If local server is running, share the collision model, increasing the ref counter
	// TODO: That requires making some calls that modify cms state reentrant

	cl.cms = CM_New();
	unsigned map_checksum = 0;
	CM_LoadMap( cl.cms, name, true, &map_checksum );

	CM_AddReference( cl.cms );

	assert( cl.cms );

	// allocate memory for areabits
	const int areas = CM_NumAreas( cl.cms ) * CM_AreaRowSize( cl.cms );

	cl.frames_areabits = (uint8_t *)Q_malloc( UPDATE_BACKUP * areas );
	for( int i = 0; i < UPDATE_BACKUP; i++ ) {
		cl.snapShots[i].areabytes = areas;
		cl.snapShots[i].areabits = cl.frames_areabits + i * areas;
	}

	return map_checksum;
}

[[nodiscard]]
static bool TryStartingPureFileDownloadIfNeeded() {
	// try downloading
	if( precache_pure != -1 ) {
		int skipped = 0;
		purelist_t *purefile = cls.purelist;
		while( skipped < precache_pure && purefile ) {
			purefile = purefile->next;
			skipped++;
		}

		while( purefile ) {
			precache_pure++;
			if( !CL_CheckOrDownloadFile( purefile->filename ) ) {
				return true;
			}
			purefile = purefile->next;
		}
		precache_pure = -1;
	}

	return false;
}

[[nodiscard]]
static bool CheckPureFiles( wsw::PodVector<char> *errorMessage ) {
	errorMessage->clear();

	purelist_t *purefile = cls.purelist;
	while( purefile ) {
		Com_DPrintf( "Adding pure file: %s\n", purefile->filename );
		if( !FS_AddPurePak( purefile->checksum ) ) {
			errorMessage->append( ' ' );
			errorMessage->append( wsw::StringView( purefile->filename ) );
		}
		purefile = purefile->next;
	}

	if( !errorMessage->empty() ) {
		errorMessage->insert( errorMessage->begin(), "Pure check failed:"_asView );
		errorMessage->append( '\0' );
		return false;
	}
	return true;
}

[[nodiscard]]
static bool TryStartingMapDownloadIfNeeded() {
	//ZOID
	if( precache_check == CS_WORLDMODEL ) { // confirm map
		precache_check = CS_MODELS; // 0 isn't used

		if( !CL_CheckOrDownloadFile( cl.configStrings.getWorldModel()->data() ) ) {
			return true; // started a download
		}
	}
	return false;
}

[[nodiscard]]
static bool TryStartingModelDownloadIfNeeded() {
	if( precache_check >= CS_MODELS && precache_check < CS_MODELS + MAX_MODELS ) {
		for(;; ) {
			if( precache_check >= CS_MODELS + MAX_MODELS ) {
				break;
			}
			const auto maybeConfigString = cl.configStrings.get( precache_check );
			if( !maybeConfigString ) {
				break;
			}

			const auto string = *maybeConfigString;
			precache_check++;

			// disable playermodel downloading for now
			if( !string.startsWith( '*' ) && !string.startsWith( '$' ) && !string.startsWith( '#' ) ) {
				// started a download
				if( !CL_CheckOrDownloadFile( string.data() ) ) {
					return true;
				}
			}
		}
		precache_check = CS_SOUNDS;
	}

	return false;
}

[[nodiscard]]
static bool TryStartingSoundDownloadIfNeeded() {
	if( precache_check >= CS_SOUNDS && precache_check < CS_SOUNDS + MAX_SOUNDS ) {
		if( precache_check == CS_SOUNDS ) {
			precache_check++; // zero is blank
		}
		for(;; ) {
			if( precache_check >= CS_SOUNDS + MAX_SOUNDS ) {
				break;
			}
			const auto maybeConfigString = cl.configStrings.get( precache_check );
			if( !maybeConfigString ) {
				break;
			}

			const auto string = *maybeConfigString;
			precache_check++;

			// ignore sexed sounds
			if( !string.startsWith( '*' ) ) {
				char tempname[MAX_QPATH + 4];
				Q_strncpyz( tempname, string.data(), sizeof( tempname ) );
				if( !COM_FileExtension( tempname ) ) {
					if( !FS_FirstExtension( tempname, SOUND_EXTENSIONS, std::size( SOUND_EXTENSIONS ) ) ) {
						COM_DefaultExtension( tempname, ".wav", sizeof( tempname ) );
						if( !CL_CheckOrDownloadFile( tempname ) ) {
							return true; // started a download
						}
					}
				} else {
					if( !CL_CheckOrDownloadFile( tempname ) ) {
						return true; // started a download
					}
				}
			}
		}
		precache_check = CS_IMAGES;
	}

	return false;
}

[[nodiscard]]
static bool TryStartingImageDownloadIfNeeded() {
	// TODO: Why don't we download images?
	if( precache_check >= CS_IMAGES && precache_check < CS_IMAGES + MAX_IMAGES ) {
		if( precache_check == CS_IMAGES ) {
			precache_check++; // zero is blank
		}
		// precache phase completed
		precache_check = ENV_CNT;
	}

	return false;
}

void CL_RequestNextDownload( void ) {
	// TODO: Convert this to assertion?
	if( cls.state != CA_CONNECTED ) {
		return;
	}

	// pure list
	if( cls.sv_pure ) {
		// skip
		if( !cl_downloads->integer ) {
			precache_pure = -1;
		} else {
			if( TryStartingPureFileDownloadIfNeeded() ) {
				return;
			}
		}
		if( precache_pure == -1 ) {
			wsw::PodVector<char> errorMessage;
			if( !CheckPureFiles( &errorMessage ) ) {
				Com_Error( ERR_DROP, "%s", errorMessage.data() );
				return;
			}
		}
	}

	// skip if download not allowed
	if( !cl_downloads->integer && precache_check < ENV_CNT ) {
		precache_check = ENV_CNT;
	} else {
		// TODO: The state modification should be transparent
		// TODO: Change function pointers for "what to do next" actions, so we don't have to check everything each call?
		if( TryStartingMapDownloadIfNeeded() ) {
			return;
		}
		if( TryStartingModelDownloadIfNeeded() ) {
			return;
		}
		if( TryStartingSoundDownloadIfNeeded() ) {
			return;
		}
		if( TryStartingImageDownloadIfNeeded() ) {
			return;
		}
	}

	if( precache_check == ENV_CNT ) {
		bool restart = false;
		bool vid_restart = false;
		const char *restart_msg = "";

		// we're done with the download phase, so clear the list
		CL_FreeDownloadList();
		if( cls.pure_restart ) {
			restart = true;
			restart_msg = "Pure server. Restarting media...";
		}
		if( cls.download.successCount ) {
			restart = true;
			vid_restart = true;
			restart_msg = "Files downloaded. Restarting media...";
		}

		CL_BeginRegistration();

		if( restart ) {
			clNotice() << wsw::StringView( restart_msg );

			if( vid_restart ) {
				// no media is going to survive a vid_restart...
				CL_Cmd_ExecuteNow( "vid_restart\n" );
			} else {
				// make sure all media assets will be freed
				CL_EndRegistration();
				CL_BeginRegistration();
			}
		}

		if( !vid_restart ) {
			CL_RestartMedia();
		}

		cls.download.successCount = 0;

		const unsigned map_checksum = CL_LoadMap( cl.configStrings.getWorldModel()->data() );
		if( map_checksum != (unsigned)atoi( cl.configStrings.getMapCheckSum()->data() ) ) {
			Com_Error( ERR_DROP, "Local map version differs from server: %u != '%s'",
					   map_checksum, cl.configStrings.getMapCheckSum()->data() );
		}

		// TODO: We don't download textures, do we?
		precache_check = TEXTURE_CNT;
	}

	if( precache_check == TEXTURE_CNT ) {
		precache_check = TEXTURE_CNT + 1;
		precache_tex = 0;
	}

	// confirm existance of textures, download any that don't exist
	if( precache_check == TEXTURE_CNT + 1 ) {
		precache_check = TEXTURE_CNT + 999;
	}

	// load client game module
	CL_GameModule_Init();
	CL_AddReliableCommand( va( "begin %i\n", precache_spawncount ) );
}

/*
* CL_Precache_f
*
* The server will send this command right
* before allowing the client into the server
*/
void CL_Precache_f( const CmdArgs &cmdArgs ) {
	FS_RemovePurePaks();

	if( cls.demoPlayer.playing ) {
		if( !cls.demoPlayer.play_jump ) {
			CL_BeginRegistration();
			CL_RestartMedia();

			CL_LoadMap( cl.configStrings.getWorldModel()->data() );

			CL_GameModule_Init();
		} else {
			CL_GameModule_Reset();
			SoundSystem::instance()->stopSounds( SoundSystem::RetainLocal );
		}

		cls.demoPlayer.play_ignore_next_frametime = true;
	} else {
		precache_pure = 0;
		precache_check = CS_WORLDMODEL;
		precache_spawncount = atoi( Cmd_Argv( 1 ) );

		CL_RequestNextDownload();
	}
}

static void CL_Cmd_WriteAliases( int file );

/*
* CL_WriteConfiguration
*
* Writes key bindings, archived cvars and aliases to a config file
*/
static void CL_WriteConfiguration( const char *name, bool warn ) {
	int file = 0;
	if( FS_FOpenFile( name, &file, FS_WRITE ) == -1 ) {
		clWarning() << "Couldn't write" << wsw::StringView( name );
		return;
	}

	if( warn ) {
		// Write 'Warsow' with UTF-8 encoded section sign in place of the s to aid
		// text editors in recognizing the file's encoding
		FS_Printf( file, "// This file is automatically generated by " APPLICATION_UTF8 ", do not modify.\r\n" );
	}

	FS_Printf( file, "\r\n// key bindings\r\n" );
	Key_WriteBindings( file );

	FS_Printf( file, "\r\n// variables\r\n" );
	Cvar_WriteVariables( file );

	FS_Printf( file, "\r\n// aliases\r\n" );
	CL_Cmd_WriteAliases( file );

	FS_FCloseFile( file );
}


/*
* CL_WriteConfig_f
*/
static void CL_WriteConfig_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() != 2 ) {
		clNotice() << "Usage: writeconfig <filename>";
		return;
	}

	int name_size = sizeof( char ) * ( strlen( Cmd_Argv( 1 ) ) + strlen( ".cfg" ) + 1 );
	char *name = (char *)Q_malloc( name_size );
	Q_strncpyz( name, Cmd_Argv( 1 ), name_size );
	COM_SanitizeFilePath( name );

	if( !COM_ValidateRelativeFilename( name ) ) {
		clNotice() << "Invalid filename";
	} else {
		COM_DefaultExtension( name, ".cfg", name_size );
		clNotice() << "Writing" << wsw::StringView( name );
		CL_WriteConfiguration( name, false );
	}

	Q_free( name );
}

static void CL_Help_f( const CmdArgs & ) {
	clNotice() << "Type commands here. Use TAB key for getting suggestions. Use PgUp/PgDn keys for scrolling";
	clNotice() << "These commands can be useful:";
	clNotice() << "cvarlist - Displays the list of all console vars";
	clNotice() << "cvarlist <pattern> - Displays a list of console vars that match the pattern";
	clNotice() << "Example: cvarlist zoom* - Displays a list of console vars that are related to zoom";
	clNotice() << "cmdlist - Displays the list of all console commands";
	clNotice() << "cmdlist <pattern> - Displays a list of console commands that match the pattern";
	clNotice() << "Example: cmdlist *restart - Displays a list of commands that restart their corresponding subsystems";
}

void CL_SetClientState( int state ) {
	cls.state = (connstate_t)state;
	Com_SetClientState( state );

	switch( state ) {
		case CA_DISCONNECTED:
			SCR_CloseConsole();
			break;
		case CA_GETTING_TICKET:
		case CA_CONNECTING:
			cls.cgameActive = false;
			SCR_CloseConsole();
			SoundSystem::instance()->stopBackgroundTrack();
			break;
		case CA_CONNECTED:
			cls.cgameActive = false;
			SCR_CloseConsole();
			Cvar_FixCheatVars();
			break;
		case CA_ACTIVE:
			cl_connectChain[0] = '\0';
			CL_EndRegistration();
			SCR_CloseConsole();
			CL_AddReliableCommand( "svmotd 1" );
			break;
		default:
			break;
	}
}

connstate_t CL_GetClientState() {
	return cls.state;
}

void CL_InitMedia() {
	// TODO: Should some of these guards be omitted/turned into assertions?
	if( !cls.mediaInitialized && cls.state != CA_UNINITIALIZED && VID_RefreshIsActive() ) {
		// random seed to be shared among game modules so pseudo-random stuff is in sync
		if( cls.state != CA_CONNECTED ) {
			srand( time( NULL ) );
			cls.mediaRandomSeed = rand();
		}

		cls.mediaInitialized = true;

		SoundSystem::instance()->stopSounds( SoundSystem::StopMusic );

		// register console font and background
		SCR_RegisterConsoleMedia();

		SCR_EnableQuickMenu( false );

		// load user interface
		wsw::ui::UISystem::init( VID_GetWindowWidth(), VID_GetWindowHeight(), VID_GetPixelRatio() );
	}
}

void CL_ShutdownMedia( void ) {
	if( cls.mediaInitialized && VID_RefreshIsActive() ) {
		cls.mediaInitialized = false;

		SoundSystem::instance()->stopSounds( SoundSystem::StopMusic );

		// shutdown cgame
		CL_GameModule_Shutdown();

		// shutdown user interface
		wsw::ui::UISystem::shutdown();

		SCR_ShutDownConsoleMedia();
	}
}

void CL_RestartMedia( void ) {
	if( VID_RefreshIsActive() ) {
		if( cls.mediaInitialized ) {
			// shutdown cgame
			CL_GameModule_Shutdown();

			cls.mediaInitialized = false;
		}

		SoundSystem::instance()->stopSounds( SoundSystem::StopMusic | SoundSystem::RetainLocal );

		// random seed to be shared among game modules so pseudo-random stuff is in sync
		if( cls.state != CA_CONNECTED ) {
			srand( time( NULL ) );
			cls.mediaRandomSeed = rand();
		}

		cls.mediaInitialized = true;

		FTLIB_TouchAllFonts();

		// register console font and background
		SCR_RegisterConsoleMedia();
	}
}

/*
* CL_S_Restart
*
* Restart the sound subsystem so it can pick up new parameters and flush all sounds
*/
void CL_S_Restart( bool noVideo, const CmdArgs &cmdArgs ) {
	bool verbose = ( Cmd_Argc() >= 2 ? true : false );

	// The cgame and game must also be forced to restart because handles will become invalid
	// VID_Restart also forces an audio restart
	if( !noVideo ) {
		VID_Restart( verbose, true );
		VID_CheckChanges();
	} else {
		CL_SoundModule_Shutdown( verbose );
		CL_SoundModule_Init( verbose );
	}
}

/*
* CL_S_Restart_f
*
* Restart the sound subsystem so it can pick up new parameters and flush all sounds
*/
static void CL_S_Restart_f( const CmdArgs &cmdArgs ) {
	CL_S_Restart( false, cmdArgs );
}

/*
* CL_ShowIP_f - wsw : jal : taken from Q3 (it only shows the ip when server was started)
*/
static void CL_ShowIP_f( const CmdArgs & ) {
	NET_ShowIP();
}

/*
* CL_ShowServerIP_f - wsw : pb : show the ip:port of the server the client is connected to
*/
static void CL_ShowServerIP_f( const CmdArgs & ) {
	if( cls.state != CA_CONNECTED && cls.state != CA_ACTIVE ) {
		clNotice() << "Not connected to a server";
	} else {
		clNotice() << "Connected to server";
		clNotice() << wsw::named( "Name", wsw::StringView( cls.servername ) );
		clNotice() << wsw::named( "Address", wsw::StringView( NET_AddressToString( &cls.serveraddress ) ) );
	}
}

static void CL_InitLocal( void ) {
	cvar_t *name, *color;

	cls.state = CA_DISCONNECTED;
	Com_SetClientState( CA_DISCONNECTED );

	cl_maxfps =     Cvar_Get( "cl_maxfps", "250", CVAR_ARCHIVE );
	cl_sleep =      Cvar_Get( "cl_sleep", "1", CVAR_ARCHIVE );
	cl_pps =        Cvar_Get( "cl_pps", "62", CVAR_ARCHIVE );

	cl_extrapolationTime =  Cvar_Get( "cl_extrapolationTime", "0", CVAR_DEVELOPER );
	cl_extrapolate = Cvar_Get( "cl_extrapolate", "1", CVAR_ARCHIVE );

	cl_infoservers =  Cvar_Get( "infoservers", DEFAULT_INFO_SERVERS_IPS, 0 );

	cl_shownet =        Cvar_Get( "cl_shownet", "0", 0 );
	cl_timeout =        Cvar_Get( "cl_timeout", "120", 0 );
	cl_timedemo =       Cvar_Get( "timedemo", "0", CVAR_CHEAT );

	rcon_client_password =  Cvar_Get( "rcon_password", "", 0 );
	rcon_address =      Cvar_Get( "rcon_address", "", 0 );

	// wsw : debug netcode
	cl_debug_serverCmd =    Cvar_Get( "cl_debug_serverCmd", "0", CVAR_ARCHIVE | CVAR_CHEAT );
	cl_debug_timeDelta =    Cvar_Get( "cl_debug_timeDelta", "0", CVAR_ARCHIVE /*|CVAR_CHEAT*/ );

	cl_downloads =      Cvar_Get( "cl_downloads", "1", CVAR_ARCHIVE );
	cl_downloads_from_web = Cvar_Get( "cl_downloads_from_web", "1", CVAR_ARCHIVE | CVAR_READONLY );
	cl_downloads_from_web_timeout = Cvar_Get( "cl_downloads_from_web_timeout", "600", CVAR_ARCHIVE );
	cl_download_allow_modules = Cvar_Get( "cl_download_allow_modules", "1", CVAR_ARCHIVE );

	cl_checkForUpdate        = Cvar_Get( "cl_checkForUpdate", "1", CVAR_ARCHIVE );
	cl_checkForUpdateTimeout = Cvar_Get( "cl_checkForUpdateTimeout", "3", CVAR_ARCHIVE );

	//
	// userinfo
	//
	info_password =     Cvar_Get( "password", "", CVAR_USERINFO );

	name = Cvar_Get( "name", "", CVAR_USERINFO | CVAR_ARCHIVE );
	if( !name->string[0] ) {
		char buffer[MAX_NAME_BYTES];
		// Avoid using the default random() macro as it has a default seed.
		std::minstd_rand0 randomEngine( (std::minstd_rand0::result_type)time( nullptr ) );
		// Avoid using black and grey colors.
		int colorNum;
		do {
			colorNum = (int)( randomEngine() % 10 );
		} while( colorNum == 0 || colorNum == 9 );
		int parts[3];
		for( int &part: parts ) {
			part = (int)( randomEngine() % 100 );
		}
		Q_snprintfz( buffer, sizeof( buffer ), "^%dplayer%02d%02d%02d", colorNum, parts[0], parts[1], parts[2] );
		Cvar_Set( name->name, buffer );
	}

	Cvar_Get( "clan", "", CVAR_USERINFO | CVAR_ARCHIVE );
	Cvar_Get( "model", DEFAULT_PLAYERMODEL, CVAR_USERINFO | CVAR_ARCHIVE );
	Cvar_Get( "skin", DEFAULT_PLAYERSKIN, CVAR_USERINFO | CVAR_ARCHIVE );
	Cvar_Get( "hand", "0", CVAR_USERINFO | CVAR_ARCHIVE );
	Cvar_Get( "handicap", "0", CVAR_USERINFO | CVAR_ARCHIVE );

	Cvar_Get( "cl_download_name", "", CVAR_READONLY );
	Cvar_Get( "cl_download_percent", "0", CVAR_READONLY );

	color = Cvar_Get( "color", "", CVAR_ARCHIVE | CVAR_USERINFO );
	if( COM_ReadColorRGBString( color->string ) == -1 ) {
		time_t long_time; // random isn't working fine at this point.
		time( &long_time );
		const unsigned hash = COM_SuperFastHash64BitInt( ( uint64_t )long_time );
		const int rgbcolor = COM_ValidatePlayerColor( COLOR_RGB( hash & 0xff, ( hash >> 8 ) & 0xff, ( hash >> 16 ) & 0xff ) );
		Cvar_Set( color->name, va( "%i %i %i", COLOR_R( rgbcolor ), COLOR_G( rgbcolor ), COLOR_B( rgbcolor ) ) );
	}

	//
	// register our commands
	//
	CL_Cmd_Register( "s_restart"_asView, CL_S_Restart_f );
	CL_Cmd_Register( "cmd"_asView, CL_ForwardToServer_f );
	CL_Cmd_Register( "userinfo"_asView, CL_Userinfo_f );
	CL_Cmd_Register( "disconnect"_asView, CL_Disconnect_f );
	CL_Cmd_Register( "record"_asView, CL_Record_f );
	CL_Cmd_Register( "stop"_asView, CL_Stop_f );
	CL_Cmd_Register( "quit"_asView, CL_Quit_f );
	CL_Cmd_Register( "connect"_asView, CL_Connect_f );
	CL_Cmd_Register( "reconnect"_asView, CL_Reconnect_f );
	CL_Cmd_Register( "rcon"_asView, CL_Rcon_f );
	CL_Cmd_Register( "writeconfig"_asView, CL_WriteConfig_f );
	CL_Cmd_Register( "showip"_asView, CL_ShowIP_f ); // jal : wsw : print our ip
	CL_Cmd_Register( "demo"_asView, CL_PlayDemo_f );
	CL_Cmd_Register( "next"_asView, CL_SetNext_f );
	CL_Cmd_Register( "demopause"_asView, CL_PauseDemo_f );
	CL_Cmd_Register( "demojump"_asView, CL_DemoJump_f );
	CL_Cmd_Register( "showserverip"_asView, CL_ShowServerIP_f );
	CL_Cmd_Register( "downloadstatus"_asView, CL_DownloadStatus_f );
	CL_Cmd_Register( "downloadcancel"_asView, CL_DownloadCancel_f );
	CL_Cmd_Register( "help"_asView, CL_Help_f );
}

static void CL_ShutdownLocal( void ) {
	cls.state = CA_UNINITIALIZED;
	Com_SetClientState( CA_UNINITIALIZED );

	CL_Cmd_Unregister( "s_restart"_asView );
	CL_Cmd_Unregister( "cmd"_asView );
	CL_Cmd_Unregister( "userinfo"_asView );
	CL_Cmd_Unregister( "disconnect"_asView );
	CL_Cmd_Unregister( "record"_asView );
	CL_Cmd_Unregister( "stop"_asView );
	CL_Cmd_Unregister( "quit"_asView );
	CL_Cmd_Unregister( "connect"_asView );
	CL_Cmd_Unregister( "reconnect"_asView );
	CL_Cmd_Unregister( "rcon"_asView );
	CL_Cmd_Unregister( "writeconfig"_asView );
	CL_Cmd_Unregister( "showip"_asView );
	CL_Cmd_Unregister( "demo"_asView );
	CL_Cmd_Unregister( "next"_asView );
	CL_Cmd_Unregister( "demopause"_asView );
	CL_Cmd_Unregister( "demojump"_asView );
	CL_Cmd_Unregister( "showserverip"_asView );
	CL_Cmd_Unregister( "downloadstatus"_asView );
	CL_Cmd_Unregister( "downloadcancel"_asView );
	CL_Cmd_Unregister( "help"_asView );
}

static void CL_TimedemoStats( void ) {
	if( cl_timedemo->integer && cls.demoPlayer.playing ) {
		const int64_t lastTime = cl.timedemo.lastTime;
		if( lastTime != 0 ) {
			const int msec = RF_GetAverageFrametime();
			const int64_t curTime = Sys_Milliseconds();
			if( msec  >= 100 ) {
				cl.timedemo.counts[99]++;
			} else {
				cl.timedemo.counts[msec]++;
			}
			cl.timedemo.lastTime = curTime;
		} else {
			cl.timedemo.lastTime = Sys_Milliseconds();
		}
	}
}

void CL_AdjustServerTime( unsigned int gameMsec ) {
	// hurry up if coming late (unless in demos)
	if( !cls.demoPlayer.playing ) {
		if( ( cl.newServerTimeDelta < cl.serverTimeDelta ) && gameMsec > 0 ) {
			cl.serverTimeDelta--;
		}
		if( cl.newServerTimeDelta > cl.serverTimeDelta ) {
			cl.serverTimeDelta++;
		}
	}

	const auto oldServerTime = cl.serverTime;
	cl.serverTime = cls.gametime + cl.serverTimeDelta;
	if( oldServerTime > cl.serverTime ) {
		// Reset timestamp-dependent effects
		if( cge ) {
			CG_ClearEffects();
		}
	}

	// it launches a new snapshot when the timestamp of the CURRENT snap is reached.
	if( cl.pendingSnapNum && ( cl.serverTime >= cl.snapShots[cl.currentSnapNum & UPDATE_MASK].serverTime ) ) {
		// fire next snapshot
		if( CL_GameModule_NewSnapshot( cl.pendingSnapNum ) ) {
			cl.previousSnapNum = cl.currentSnapNum;
			cl.currentSnapNum = cl.pendingSnapNum;
			cl.pendingSnapNum = 0;

			// getting a valid snapshot ends the connection process
			if( cls.state == CA_CONNECTED ) {
				CL_SetClientState( CA_ACTIVE );
			}
		}
	}
}

void CL_RestartTimeDeltas( int newTimeDelta ) {
	cl.serverTimeDelta = cl.newServerTimeDelta = newTimeDelta;
	for( int i = 0; i < MAX_TIMEDELTAS_BACKUP; i++ )
		cl.serverTimeDeltas[i] = newTimeDelta;

	if( cl_debug_timeDelta->integer ) {
		Com_Printf( S_COLOR_CYAN "***** timeDelta restarted\n" );
	}
}

int CL_SmoothTimeDeltas( void ) {
	if( cls.demoPlayer.playing ) {
		if( cl.currentSnapNum <= 0 ) { // if first snap
			return cl.serverTimeDeltas[cl.pendingSnapNum & MASK_TIMEDELTAS_BACKUP];
		}
		return cl.serverTimeDeltas[cl.currentSnapNum & MASK_TIMEDELTAS_BACKUP];
	}

	int i = cl.receivedSnapNum - wsw::min( MAX_TIMEDELTAS_BACKUP, 8 );
	if( i < 0 ) {
		i = 0;
	}

	int count;
	double delta = 0.0;
	for( count = 0; i <= cl.receivedSnapNum; i++ ) {
		snapshot_t *snap = &cl.snapShots[i & UPDATE_MASK];
		if( snap->valid && snap->serverFrame == i ) {
			delta += (double)cl.serverTimeDeltas[i & MASK_TIMEDELTAS_BACKUP];
			count++;
		}
	}

	if( !count ) {
		return 0;
	}

	return (int)( delta / (double)count );
}

void CL_UpdateSnapshot( void ) {
	// see if there is any pending snap to be fired
	if( !cl.pendingSnapNum && ( cl.currentSnapNum != cl.receivedSnapNum ) ) {
		snapshot_t *snap = NULL;
		for( int i = cl.currentSnapNum + 1; i <= cl.receivedSnapNum; i++ ) {
			if( cl.snapShots[i & UPDATE_MASK].valid && ( cl.snapShots[i & UPDATE_MASK].serverFrame > cl.currentSnapNum ) ) {
				snap = &cl.snapShots[i & UPDATE_MASK];
				//torbenh: this break was the source of the lag bug at cl_fps < sv_pps
				//break;
			}
		}

		if( snap ) { // valid pending snap found
			cl.pendingSnapNum = snap->serverFrame;

			cl.newServerTimeDelta = CL_SmoothTimeDeltas();

			if( cl_extrapolationTime->modified ) {
				if( cl_extrapolationTime->integer > (int)cl.snapFrameTime - 1 ) {
					Cvar_ForceSet( "cl_extrapolationTime", va( "%i", (int)cl.snapFrameTime - 1 ) );
				} else if( cl_extrapolationTime->integer < 0 ) {
					Cvar_ForceSet( "cl_extrapolationTime", "0" );
				}

				cl_extrapolationTime->modified = false;
			}

			if( !cls.demoPlayer.playing && cl_extrapolate->integer ) {
				cl.newServerTimeDelta += cl_extrapolationTime->integer;
			}

			// if we don't have current snap (or delay is too big) don't wait to fire the pending one
			if( ( !cls.demoPlayer.play_jump && cl.currentSnapNum <= 0 ) ||
				( !cls.demoPlayer.playing && abs( cl.newServerTimeDelta - cl.serverTimeDelta ) > 200 ) ) {
				cl.serverTimeDelta = cl.newServerTimeDelta;
			}

			// don't either wait if in a timedemo
			if( cls.demoPlayer.playing && cl_timedemo->integer ) {
				cl.serverTimeDelta = cl.newServerTimeDelta;
			}
		}
	}
}

void CL_Netchan_Transmit( msg_t *msg ) {
	// if we got here with unsent fragments, fire them all now
	Netchan_PushAllFragments( &cls.netchan );

	if( msg->cursize > 60 ) {
		// it's compression error, just send uncompressed
		if( const int zerror = Netchan_CompressMessage( msg, g_netchanCompressionBuffer ); zerror < 0 ) {
			Com_DPrintf( "CL_Netchan_Transmit (ignoring compression): Compression error %i\n", zerror );
		}
	}

	Netchan_Transmit( &cls.netchan, msg );
	cls.lastPacketSentTime = cls.realtime;
}

static bool CL_MaxPacketsReached( void ) {
	static int64_t lastPacketTime = 0;
	static float roundingMsec = 0.0f;

	if( lastPacketTime > cls.realtime ) {
		lastPacketTime = cls.realtime;
	}

	if( cl_pps->integer > 62 || cl_pps->integer < 20 ) {
		clWarning() << "'cl_pps' value is out of valid range, resetting to default";
		Cvar_ForceSet( "cl_pps", va( "%s", cl_pps->dvalue ) );
	}

	int minpackettime;
	const int elapsedTime = cls.realtime - lastPacketTime;
	if( cls.mv ) {
		minpackettime = ( 1000.0f / 2 );
	} else {
		float minTime = ( 1000.0f / cl_pps->value );

		// don't let cl_pps be smaller than sv_pps
		if( cls.state == CA_ACTIVE && !cls.demoPlayer.playing && cl.snapFrameTime ) {
			if( (unsigned int)minTime > cl.snapFrameTime ) {
				minTime = cl.snapFrameTime;
			}
		}

		minpackettime = (int)minTime;
		roundingMsec += minTime - (int)minTime;
		if( roundingMsec >= 1.0f ) {
			minpackettime += (int)roundingMsec;
			roundingMsec -= (int)roundingMsec;
		}
	}

	if( elapsedTime < minpackettime ) {
		return false;
	}

	lastPacketTime = cls.realtime;
	return true;
}

void CL_SendMessagesToServer( bool sendNow ) {
	if( cls.state == CA_DISCONNECTED || cls.state == CA_GETTING_TICKET || cls.state == CA_CONNECTING ) {
		return;
	}

	if( cls.demoPlayer.playing ) {
		return;
	}

	msg_t message;
	uint8_t messageData[MAX_MSGLEN];
	MSG_Init( &message, messageData, sizeof( messageData ) );
	MSG_Clear( &message );

	// send only reliable commands during connecting time
	if( cls.state < CA_ACTIVE ) {
		if( sendNow || cls.realtime > 100 + cls.lastPacketSentTime ) {
			// write the command ack
			if( !cls.reliable ) {
				MSG_WriteUint8( &message, clc_svcack );
				MSG_WriteIntBase128( &message, cls.lastExecutedServerCommand );
				MSG_WriteIntBase128( &message, cls.lastReceivedRealServerTime );
			}
			//write up the clc commands
			CL_UpdateClientCommandsToServer( &message );
			if( message.cursize > 0 ) {
				CL_Netchan_Transmit( &message );
			}
		}
	} else if( sendNow || CL_MaxPacketsReached() ) {
		// write the command ack
		if( !cls.reliable ) {
			MSG_WriteUint8( &message, clc_svcack );
			MSG_WriteIntBase128( &message, cls.lastExecutedServerCommand );
			MSG_WriteIntBase128( &message, cls.lastReceivedRealServerTime );
		}
		// send a userinfo update if needed
		if( userinfo_modified ) {
			userinfo_modified = false;
			CL_AddReliableCommand( va( "usri \"%s\"", Cvar_Userinfo() ) );
		}
		CL_UpdateClientCommandsToServer( &message );
		CL_WriteUcmdsToMessage( &message );
		if( message.cursize > 0 ) {
			CL_Netchan_Transmit( &message );
		}
	}
}

static void CL_NetFrame( int realMsec, int gameMsec ) {
	// read packets from server
	if( realMsec > 5000 ) { // if in the debugger last frame, don't timeout
		cls.lastPacketReceivedTime = cls.realtime;
	}

	if( cls.demoPlayer.playing ) {
		// Fetch results from demo file
		CL_ReadDemoPackets();
	}

	// Fetch results from server (TODO: is it needed during demo playback?)
	CL_ReadPackets();

	// send packets to server
	if( cls.netchan.unsentFragments ) {
		Netchan_TransmitNextFragment( &cls.netchan );
	} else {
		CL_SendMessagesToServer( false );
	}

	// resend a connection request if necessary
	CL_CheckForResend();
	CL_CheckDownloadTimeout();

	ServerList::instance()->frame();
}

void CL_Frame( int realMsec, int gameMsec ) {
	WSW_PROFILER_SCOPE();

	static int allRealMsec = 0, allGameMsec = 0, extraMsec = 0;
	static float roundingMsec = 0.0f;

	assert( !dedicated->integer );

	cls.realtime += realMsec;

	if( cls.demoPlayer.playing && cls.demoPlayer.play_ignore_next_frametime ) {
		gameMsec = 0;
		cls.demoPlayer.play_ignore_next_frametime = false;
	}

	if( cls.demoPlayer.playing ) {
		if( cls.demoPlayer.paused ) {
			gameMsec = 0;
		} else {
			CL_LatchedDemoJump();
		}
	}

	cls.gametime += gameMsec;

	allRealMsec += realMsec;
	allGameMsec += gameMsec;

	CL_UpdateSnapshot();
	CL_AdjustServerTime( gameMsec );

	const auto currTimestampMicros = (int64_t)Sys_Microseconds();
	const auto currTimestampMillis = currTimestampMicros / 1000;
	if( currTimestampMillis > oldKeyboardTimestamp ) {
		const int64_t keyboardDelta = currTimestampMillis - oldKeyboardTimestamp;
		const int64_t mouseDelta    = currTimestampMicros - oldMouseTimestamp;
		oldKeyboardTimestamp = currTimestampMillis;
		oldMouseTimestamp    = currTimestampMicros;

		CL_UserInputFrame( currTimestampMicros, (int)keyboardDelta, ( (float)mouseDelta * 1e-3f ), realMsec );
	}

	CL_Cbuf_ExecutePendingCommands();

	CL_NetFrame( realMsec, gameMsec );

	int minMsec;
	float maxFps;
	if( cl_maxfps->integer > 0 && !( cl_timedemo->integer && cls.demoPlayer.playing ) ) {
		const int absMinFps = 24;

		// do not allow setting cl_maxfps to very low values to prevent cheating
		if( cl_maxfps->integer < absMinFps ) {
			Cvar_ForceSet( "cl_maxfps", STR_TOSTR( absMinFps ) );
		}
		maxFps = VID_AppIsMinimized() ? absMinFps : cl_maxfps->value;
		minMsec = (int)wsw::max( ( 1000.0f / maxFps ), 1.0f );
		roundingMsec += wsw::max( ( 1000.0f / maxFps ), 1.0f ) - minMsec;
	} else {
		maxFps = 10000.0f;
		minMsec = 1;
		roundingMsec = 0;
	}

	if( roundingMsec >= 1.0f ) {
		minMsec += (int)roundingMsec;
		roundingMsec -= (int)roundingMsec;
	}

	if( allRealMsec + extraMsec < minMsec ) {
		// let CPU sleep while playing fullscreen video, while minimized
		// or when cl_sleep is enabled
		const bool sleep = cl_sleep->integer != 0 || cls.state == CA_DISCONNECTED || !VID_AppIsActive() || VID_AppIsMinimized(); // FIXME: not sure about listen server here..

		if( sleep && minMsec - extraMsec > 1 ) {
			Sys_Sleep( minMsec - extraMsec - 1 );
		}
	} else {
		cls.frametime = allGameMsec;
		cls.realFrameTime = allRealMsec;
		CL_ProfilerHud_Update( allGameMsec, allRealMsec );

#if 1
		if( allRealMsec < minMsec ) { // is compensating for a too slow frame
			extraMsec -= ( minMsec - allRealMsec );
			Q_clamp( extraMsec, 0, 100 );
		} else {   // too slow, or exact frame
			extraMsec = allRealMsec - minMsec;
			Q_clamp( extraMsec, 0, 100 );
		}
#else
		extraMsec = allRealMsec - minMsec;
		Q_clamp( extraMsec, 0, minMsec );
#endif

		CL_TimedemoStats();

		// allow rendering DLL change
		VID_CheckChanges();

		if( !cls.disable_screen && scr_initialized && con_initialized && cls.mediaInitialized ) {
			SCR_UpdateScreen();
		}

		// update audio
		if( cls.state != CA_ACTIVE ) {
			// if the loading plaque is up, clear everything out to make sure we aren't looping a dirty
			// dma buffer while loading
			if( cls.disable_screen ) {
				SoundSystem::instance()->stopSounds( SoundSystem::StopMusic );
			} else {
				SoundSystem::instance()->updateListener( -1, vec3_origin, vec3_origin, axis_identity );
			}
		}

		// advance local effects for next frame
		SCR_RunConsole( allRealMsec );

		SoundSystem::instance()->processFrameUpdates();

		allRealMsec = 0;
		allGameMsec = 0;

		cls.framecount++;
	}
}

static void *CL_AsyncStream_Alloc( size_t size, const char *filename, int fileline ) {
	return Q_malloc( size );
}

static void CL_AsyncStream_Free( void *data, const char *filename, int fileline ) {
	Q_free( data );
}

static void CL_InitAsyncStream( void ) {
	cl_async_stream = AsyncStream_InitModule( "Client", CL_AsyncStream_Alloc, CL_AsyncStream_Free );
}

static void CL_ShutdownAsyncStream( void ) {
	if( !cl_async_stream ) {
		AsyncStream_ShutdownModule( cl_async_stream );
		cl_async_stream = NULL;
	}
}

int CL_AddSessionHttpRequestHeaders( const char *url, const char **headers ) {
	static char pH[32];

	if( cls.httpbaseurl && *cls.httpbaseurl ) {
		if( !strncmp( url, cls.httpbaseurl, strlen( cls.httpbaseurl ) ) ) {
			Q_snprintfz( pH, sizeof( pH ), "%i", cl.playernum );

			headers[0] = "X-Client";
			headers[1] = pH;
			headers[2] = "X-Session";
			headers[3] = cls.session;
			return 4;
		}
	}
	return 0;
}

void CL_AsyncStreamRequest( const char *url, const char **headers, int timeout, int resumeFrom,
							size_t ( *read_cb )( const void *, size_t, float, int, const char *, void * ),
							void ( *done_cb )( int, const char *, void * ),
							void ( *header_cb )( const char *, void * ), void *privatep, bool urlencodeUnsafe ) {
	char *tmpUrl = NULL;
	const char *safeUrl;

	if( urlencodeUnsafe ) {
		// urlencode unsafe characters
		size_t allocSize = strlen( url ) * 3 + 1;
		tmpUrl = ( char * )Q_malloc( allocSize );
		AsyncStream_UrlEncodeUnsafeChars( url, tmpUrl, allocSize );

		safeUrl = tmpUrl;
	} else {
		safeUrl = url;
	}

	AsyncStream_PerformRequestExt( cl_async_stream, safeUrl, "GET", NULL, headers, timeout,
								   resumeFrom, read_cb, done_cb, (async_stream_header_cb_t)header_cb, NULL );

	if( urlencodeUnsafe ) {
		Q_free( tmpUrl );
	}
}

static wsw::PodVector<char> updateRemoteData;
static volatile bool isAwaitingUpdateCheck = false;

static void CL_CheckForUpdateDoneCb( int status, const char *contentType, void *privatep ) {
	char net_version_str[16];
	net_version_str[0] = '\0';

	if( status == 200 ) {
		updateRemoteData.append( '\0' );

		// got the file
		// this look stupid but is the safe way to do it
		const float localVersion = atof( va( "%4.3f", APP_VERSION ) );
		const float netVersion = atof( updateRemoteData.data() );

		// we have the version
		//Com_Printf("CheckForUpdate: local: %f net: %f\n", local_version, net_version);
		if( netVersion > localVersion ) {
			char cmd[1024];

			Q_snprintfz( net_version_str, sizeof( net_version_str ), "%4.3f", netVersion );
			const char *s = net_version_str + strlen( net_version_str ) - 1;
			while( *s == '0' ) {
				s--;
			}
			if( *s == '.' && *( s+1 ) == '0' ) {
				// for whole version numbers
				s++;
			}

			net_version_str[s-net_version_str+1] = '\0';
			Com_Printf( APPLICATION " version %s is available.\nVisit " APP_URL " for more information\n", net_version_str );
			//Cbuf_ExecuteText( EXEC_APPEND, cmd );
		} else if( netVersion == localVersion ) {
			Com_Printf( "Your %s version is up-to-date.\n", APPLICATION );
		}
	}

	updateRemoteData.clear();
	updateRemoteData.shrink_to_fit();
	isAwaitingUpdateCheck = false;

	if( net_version_str[0] != '\0' ) {
		wsw::ui::UISystem::instance()->notifyOfUpdateAvailable( wsw::StringView( net_version_str ) );
	} else {
		wsw::ui::UISystem::instance()->notifyOfUpdateNotFound();
	}
}

static size_t CL_CheckForUpdateReadCb( const void *buf, size_t numb, float percentage,
									   int status, const char *contentType, void *privatep ) {
	if( status < 0 || status >= 300 ) {
		return 0;
	}
	updateRemoteData.append( (const char *)buf, numb );
	return numb;
}

static void CL_CheckForUpdate() {
	if( cl_checkForUpdate->integer ) {
		Com_Printf( "Checking for " APPLICATION " update.\n" );

		char url[MAX_STRING_CHARS];
		Q_snprintfz( url, sizeof( url ), "%s%s", APP_UPDATE_URL, APP_CLIENT_UPDATE_FILE );

		const char *headers[4] { nullptr, nullptr, nullptr, nullptr };
		CL_AddSessionHttpRequestHeaders( url, &headers[0] );

		isAwaitingUpdateCheck = true;

		const int timeout = wsw::clamp( cl_checkForUpdateTimeout->integer, 1, 5 );
		CL_AsyncStreamRequest( url, headers, timeout, 0, CL_CheckForUpdateReadCb, CL_CheckForUpdateDoneCb, nullptr, nullptr, false );

		// It's easier to check it once at start in a blocking fashion, considering the tiny amount of data to download
		do {
			Sys_Sleep( 16 );
			wswcurl_perform();
		} while( isAwaitingUpdateCheck );
	} else {
		// Hide the update plaque
		wsw::ui::UISystem::instance()->notifyOfUpdateNotFound();
	}
}

static void sanitizeEnvironment( std::initializer_list<wsw::StringView> disallowedVarPrefixes ) {
	wsw::StringSpanStorage<size_t, size_t> qtVarNames;

	for( char **envPair = Sys_GetEnvironmentVariables(); *envPair; ++envPair ) {
		const wsw::StringView envPairView( *envPair );
		if( const auto maybeIndex = envPairView.indexOf( '=' ) ) {
			const wsw::StringView name( envPairView.take( *maybeIndex ) );
			if( wsw::any_of( disallowedVarPrefixes, [&]( const wsw::StringView &prefix ) {
				return name.startsWith( prefix, wsw::IgnoreCase );
			})) {
				qtVarNames.add( name );
			}
		}
	}

	for( const wsw::StringView &name: qtVarNames ) {
		assert( name.isZeroTerminated() );
		Sys_DeleteEnvironmentVariable( name.data() );
	}
}

void CL_Init( void ) {

	assert( !cl_initialized );
	assert( !dedicated->integer );

	cl_initialized = true;

	// all archived variables will now be loaded

	Con_Init();

	CL_ProfilerHud_Init();

	CL_Sys_Init();

	sanitizeEnvironment( { "QT"_asView, "QML"_asView, "QSG"_asView, "QV4"_asView, "Q_"_asView } );

	VID_Init();

	CL_ClearState();

	// IPv4
	netadr_t address;
	NET_InitAddress( &address, NA_IP );
	cvar_t *cl_port = Cvar_Get( "cl_port", "0", CVAR_NOSET );
	NET_SetAddressPort( &address, cl_port->integer );
	if( !NET_OpenSocket( &cls.socket_udp, SOCKET_UDP, &address, false ) ) {
		Com_Error( ERR_FATAL, "Couldn't open UDP socket: %s", NET_ErrorString() );
	}

	// IPv6
	NET_InitAddress( &address, NA_IP6 );
	cvar_t *cl_port6 = Cvar_Get( "cl_port6", "0", CVAR_NOSET );
	NET_SetAddressPort( &address, cl_port6->integer );
	if( !NET_OpenSocket( &cls.socket_udp6, SOCKET_UDP, &address, false ) ) {
		Com_Printf( "Error: Couldn't open UDP6 socket: %s", NET_ErrorString() );
	}

	SCR_InitScreen();
	cls.disable_screen = true; // don't draw yet

	CL_InitLocal();
	CL_InitInput();

	CL_InitAsyncStream();
	// Caution! The UI system relies on the server list being in a valid state.
	ServerList::init();

	// Initialize some vars that could be used by the UI prior to the UI loading
	CG_InitPersistentState();

	CL_InitMedia();

	ML_Init();

	CL_CheckForUpdate();
}

/*
* CL_Shutdown
*
* FIXME: this is a callback from Sys_Quit and Com_Error.  It would be better
* to run quit through here before the final handoff to the sys code.
*/
void CL_Shutdown( void ) {
	if( cl_initialized ) {
		SoundSystem::instance()->stopSounds( SoundSystem::StopMusic );

		ML_Shutdown();

		CL_WriteConfiguration( "config.cfg", true );

		CL_Disconnect( NULL );
		NET_CloseSocket( &cls.socket_udp );
		NET_CloseSocket( &cls.socket_udp6 );
		// TOCHECK: Shouldn't we close the TCP socket too?
		if( cls.servername ) {
			Q_free( cls.servername );
			cls.servername = NULL;
		}

		wsw::ui::UISystem::shutdown();
		CL_GameModule_Shutdown();
		CL_SoundModule_Shutdown( true );
		CL_ShutdownInput();
		VID_Shutdown();

		CL_ShutdownMedia();

		CL_ShutdownAsyncStream();
		ServerList::shutdown();

		CL_ShutdownLocal();

		SCR_ShutdownScreen();

		CL_Sys_Shutdown();

		CL_ProfilerHud_Shutdown();

		Con_Shutdown();

		cls.state = CA_UNINITIALIZED;
		cl_initialized = false;
	}
}

template <>
struct std::hash<wsw::HashedStringView> {
	[[nodiscard]]
	auto operator()( const wsw::HashedStringView &view ) const noexcept -> std::size_t {
		return view.getHash();
	}
};

class CLCmdSystem : public CmdSystem {
public:
	void submitCompletionRequest( const wsw::StringView &name, unsigned requestId, const wsw::StringView &partial ) {
		checkCallingThread();
		if( const auto it = m_completionEntries.find( wsw::HashedStringView( name ) ); it != m_completionEntries.end() ) {
			it->second.executionFunc( name, requestId, partial, it->second.queryFunc );
		}
	}

	[[maybe_unused]]
	bool registerCommandExt( const wsw::StringView &name, CmdFunc cmdFunc, CompletionQueryFunc queryFunc, CompletionExecutionFunc executionFunc, const char *tag ) {
		assert( ( queryFunc && executionFunc ) || tag );
		checkCallingThread();
		if( CmdSystem::registerCommand( name, cmdFunc ) ) {
			const CmdEntry *cmdEntry = m_cmdEntries.findByName( wsw::HashedStringView( name ) );
			if( queryFunc ) {
				// Let the map key reside in the cmdEntry memory block
				m_completionEntries[cmdEntry->m_nameAndHash] = CompletionEntry { queryFunc, executionFunc };
			}
			if( tag ) {
				m_namesForTags.emplace_back( std::make_pair( wsw::StringView( tag ), cmdEntry->m_nameAndHash ) );
			}
			return true;
		} else {
			assert( findTagsEntryByCmdName( wsw::HashedStringView( name ) ) == m_namesForTags.end() );
		}
		return false;
	}

	bool registerCommand( const wsw::StringView &name, CmdFunc cmdFunc ) override {
		checkCallingThread();
		if( CmdSystem::registerCommand( name, cmdFunc ) ) {
			const wsw::HashedStringView hashedNameView( name );
			// The method resets/overrides all callbacks, if any. This means removing completion callbacks.
			m_completionEntries.erase( hashedNameView );
			if( auto it = findTagsEntryByCmdName( hashedNameView ); it != m_namesForTags.end() ) {
				m_namesForTags.erase( it );
				assert( findTagsEntryByCmdName( hashedNameView ) == m_namesForTags.end() );
			}
			return true;
		} else {
			assert( findTagsEntryByCmdName( wsw::HashedStringView( name ) ) == m_namesForTags.end() );
		}
		return false;
	}

	bool unregisterCommand( const wsw::StringView &name ) override {
		checkCallingThread();
		// Prevent use-after-free by removing the entry first
		if( const CmdEntry *cmdEntry = m_cmdEntries.findByName( wsw::HashedStringView( name ) ) ) {
			m_completionEntries.erase( cmdEntry->m_nameAndHash );
			if( auto it = findTagsEntryByCmdName( cmdEntry->m_nameAndHash ); it != m_namesForTags.end() ) {
				m_namesForTags.erase( it );
				assert( findTagsEntryByCmdName( cmdEntry->m_nameAndHash ) == m_namesForTags.end() );
			}
		} else {
			assert( findTagsEntryByCmdName( cmdEntry->m_nameAndHash ) == m_namesForTags.end() );
		}
		return CmdSystem::unregisterCommand( name );
	}

	void unregisterCommandsByTag( const wsw::StringView &tag ) {
		for( size_t tagEntryNum = 0; tagEntryNum < m_namesForTags.size(); ) {
			if( m_namesForTags[tagEntryNum].first.equalsIgnoreCase( tag ) ) {
				CmdSystem::unregisterCommand( m_namesForTags[tagEntryNum].second );
				// Swap with the last and remove last
				std::swap( m_namesForTags[tagEntryNum], m_namesForTags.back() );
				m_namesForTags.pop_back();
			} else {
				++tagEntryNum;
			}
		}
	}

	[[nodiscard]]
	auto getPossibleCommands( const wsw::StringView &partial ) const -> CompletionResult {
		return getPossibleCompletions( partial, m_cmdEntries );
	}

	[[nodiscard]]
	auto getPossibleAliases( const wsw::StringView &partial ) const -> CompletionResult {
		return getPossibleCompletions( partial, m_aliasEntries );
	}

	void writeAliases( int file ) {
		checkCallingThread();
		const CompletionResult sortedNames( getPossibleAliases( wsw::StringView() ) );
		for( const wsw::StringView &name: sortedNames ) {
			const AliasEntry *entry = m_aliasEntries.findByName( wsw::HashedStringView( name ) );
			if( entry && entry->m_isArchive ) {
				assert( entry->m_nameAndHash.isZeroTerminated() && entry->m_text.isZeroTerminated() );
				FS_Printf( file, "aliasa %s \"%s\"\r\n", entry->m_nameAndHash.data(), entry->m_text.data() );
			}
		}
	}
private:
	[[nodiscard]]
	auto findTagsEntryByCmdName( const wsw::HashedStringView &cmdName )
		-> wsw::PodVector<std::pair<wsw::StringView, wsw::HashedStringView>>::iterator {
		const auto cmp = [&]( const std::pair<wsw::StringView, wsw::HashedStringView> &entry ) {
			return entry.second.equalsIgnoreCase( cmdName );
		};
		return wsw::find_if( m_namesForTags.begin(), m_namesForTags.end(), cmp );
	}

	template <typename Entry, unsigned N>
	[[nodiscard]]
	auto getPossibleCompletions( const wsw::StringView &partial,
								 const MapOfBoxedNamedEntries<Entry, N, wsw::IgnoreCase> &container ) const -> CompletionResult {
		checkCallingThread();

		// TODO: Make the CompletionResult be trie-based, so we don't have to output so much duplicated character data
		trie_t *trie = nullptr;
		Trie_Create( TRIE_CASE_INSENSITIVE, &trie );
		for( const auto *entry: container ) {
			assert( entry->m_nameAndHash.isZeroTerminated() );
			// TODO: Add case insensitive wsw::StringView::startsWith()
			if( entry->m_nameAndHash.length() >= partial.length() ) {
				Trie_Insert( trie, entry->m_nameAndHash.data(), (void *)(uintptr_t)entry->m_nameAndHash.length() );
			}
		}
		trie_dump_s *dump = nullptr;
		wsw::PodVector<char> ztPartial( partial.data(), partial.size() );
		ztPartial.append( '\0' );
		Trie_Dump( trie, ztPartial.data(), TRIE_DUMP_BOTH, &dump );
		CompletionResult result;
		for( unsigned i = 0; i < dump->size; ++i ) {
			result.add( wsw::StringView( dump->key_value_vector[i].key, (uintptr_t)dump->key_value_vector[i].value ) );
		}
		Trie_FreeDump( dump );
		Trie_Destroy( trie );
		return result;
	}

	void registerSystemCommands() override {
		checkCallingThread();
		registerCommand( "exec"_asView, handlerOfExec );
		registerCommand( "echo"_asView, handlerOfEcho );
		registerCommand( "alias"_asView, handlerOfAlias );
		registerCommand( "aliasa"_asView, handlerOfAliasa );
		registerCommand( "unalias"_asView, handlerOfUnalias );
		registerCommand( "unaliasall"_asView, handlerOfUnaliasall );
		registerCommand( "wait"_asView, handlerOfWait );
		registerCommand( "vstr"_asView, handlerOfVstr );
	}

	static void handlerOfExec( const CmdArgs & );
	static void handlerOfEcho( const CmdArgs & );
	static void handlerOfAlias( const CmdArgs & );
	static void handlerOfAliasa( const CmdArgs & );
	static void handlerOfUnalias( const CmdArgs & );
	static void handlerOfUnaliasall( const CmdArgs & );
	static void handlerOfWait( const CmdArgs & );
	static void handlerOfVstr( const CmdArgs & );

	struct CompletionEntry {
		CompletionQueryFunc queryFunc;
		CompletionExecutionFunc executionFunc;
	};

	std::unordered_map<wsw::HashedStringView, CompletionEntry> m_completionEntries;
	// A band-aid workaround for lack of tags in the base facilities
	// Tags are assumed to have 'static lifetime
	wsw::PodVector<std::pair<wsw::StringView, wsw::HashedStringView>> m_namesForTags;
};

static SingletonHolder<CLCmdSystem> g_clCmdSystemHolder;

void CLCmdSystem::handlerOfExec( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfExec( cmdArgs );
}

void CLCmdSystem::handlerOfEcho( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfEcho( cmdArgs );
}

void CLCmdSystem::handlerOfAlias( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfAlias( false, cmdArgs );
}

void CLCmdSystem::handlerOfAliasa( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfAlias( true, cmdArgs );
}

void CLCmdSystem::handlerOfUnalias( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfUnalias( cmdArgs );
}

void CLCmdSystem::handlerOfUnaliasall( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfUnaliasall( cmdArgs );
}

void CLCmdSystem::handlerOfWait( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfWait( cmdArgs );
}

void CLCmdSystem::handlerOfVstr( const CmdArgs &cmdArgs ) {
	g_clCmdSystemHolder.instance()->helperForHandlerOfVstr( cmdArgs );
}

void CL_InitCmdSystem() {
	g_clCmdSystemHolder.init();
}

CmdSystem *CL_GetCmdSystem() {
	return g_clCmdSystemHolder.instance();
}

void CL_ShutdownCmdSystem() {
	g_clCmdSystemHolder.shutdown();
}

void Con_AcceptCompletionResult( unsigned requestId, const CompletionResult &completionResult );

void CL_RunCompletionFuncSync( const wsw::StringView &, unsigned requestId, const wsw::StringView &partial,
							   CompletionQueryFunc queryFunc ) {
	Con_AcceptCompletionResult( requestId, queryFunc( partial ) );
}

void CL_Cmd_Register( const wsw::StringView &name, CmdFunc cmdFunc, CompletionQueryFunc completionQueryFunc, const char *tag ) {
	if( completionQueryFunc ) {
		g_clCmdSystemHolder.instance()->registerCommandExt( name, cmdFunc, completionQueryFunc, CL_RunCompletionFuncSync, tag );
	} else if( tag ) {
		g_clCmdSystemHolder.instance()->registerCommandExt( name, cmdFunc, nullptr, nullptr, tag );
	} else {
		g_clCmdSystemHolder.instance()->registerCommand( name, cmdFunc );
	}
}

void CL_Cmd_Unregister( const wsw::StringView &name ) {
	g_clCmdSystemHolder.instance()->unregisterCommand( name );
}

void CL_Cmd_UnregisterByTag( const wsw::StringView &tag ) {
	g_clCmdSystemHolder.instance()->unregisterCommandsByTag( tag );
}

void CL_Cmd_SubmitCompletionRequest( const wsw::StringView &name, unsigned requestId, const wsw::StringView &partial ) {
	g_clCmdSystemHolder.instance()->submitCompletionRequest( name, requestId, partial );
}

void CL_RegisterCmdWithCompletion( const wsw::StringView &name, CmdFunc cmdFunc,
								   CompletionQueryFunc queryFunc, CompletionExecutionFunc executionFunc ) {
	g_clCmdSystemHolder.instance()->registerCommandExt( name, cmdFunc, queryFunc, executionFunc, nullptr );
}

void CL_Cmd_WriteAliases( int file ) {
	g_clCmdSystemHolder.instance()->writeAliases( file );
}

CompletionResult CL_GetPossibleCommands( const wsw::StringView &partial ) {
	return g_clCmdSystemHolder.instance()->getPossibleCommands( partial );
}

CompletionResult CL_GetPossibleAliases( const wsw::StringView &partial ) {
	return g_clCmdSystemHolder.instance()->getPossibleAliases( partial );
}

bool CL_Cmd_Exists( const wsw::StringView &name ) {
	return g_clCmdSystemHolder.instance()->isARegisteredCommand( wsw::HashedStringView( name ) );
}

void CL_Cmd_ExecuteNow( const char *text ) {
	g_clCmdSystemHolder.instance()->executeNow( wsw::StringView( text ) );
}

void CL_Cmd_ExecuteNow( const wsw::StringView &text ) {
	g_clCmdSystemHolder.instance()->executeNow( text );
}

void CL_Cbuf_AppendCommand( const char *text ) {
	g_clCmdSystemHolder.instance()->appendCommand( wsw::StringView( text ) );
}

void CL_Cbuf_AppendCommand( const wsw::StringView &text ) {
	g_clCmdSystemHolder.instance()->appendCommand( text );
}

void CL_Cbuf_PrependCommand( const char *text ) {
	g_clCmdSystemHolder.instance()->prependCommand( wsw::StringView( text ) );
}

void CL_Cbuf_PrependCommand( const wsw::StringView &text ) {
	g_clCmdSystemHolder.instance()->prependCommand( text );
}

void CL_Cbuf_ExecutePendingCommands() {
	g_clCmdSystemHolder.instance()->executeBufferCommands();
}