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

#include "videosystem.h"
#include "client.h"

#include <common/version.h>
#include <common/helpers/exceptions.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/wswbasicmath.h>
#include <common/facilities/cvar.h>
#include <common/facilities/configvars.h>
#include <common/facilities/syspublic.h>

#include <limits>
#include <span>
#include <optional>

rserr_t VID_Sys_Init( const char *applicationName, const char *screenshotsPrefix, int startupColor, const int *iconXPM,
					  void *parentWindow, bool verbose );
void VID_UpdateWindowPosAndSize( int x, int y );
void VID_EnableAltTab( bool enable );
void VID_EnableWinKeys( bool enable );

void RestartVideoAndAllMedia( bool vid_ref_was_active, bool verbose );

using wsw::operator""_asView;

static UnsignedConfigVar v_width { "vid_width"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
static UnsignedConfigVar v_height { "vid_height"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
static UnsignedConfigVar v_pixelRatio { "vid_pixelRatio"_asView, { .byDefault = 0, .flags = CVAR_DEVELOPER | CVAR_LATCH_VIDEO } };

IntConfigVar v_xPos { "vid_xPos"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } };
IntConfigVar v_yPos { "vid_yPos"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } };
VarModificationTracker g_xPosVarTracker { &v_xPos };
VarModificationTracker g_yPosVarTracker { &v_yPos };

BoolConfigVar v_fullscreen { "vid_fullscreen"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } };
static BoolConfigVar v_borderless { "vid_borderless"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };
static VarModificationTracker g_fullscreenVarTracker { &v_fullscreen };
static VarModificationTracker g_borderlessVarTracker { &v_borderless };

static UnsignedConfigVar v_displayFrequency { "v_displayFrequency"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE | CVAR_LATCH_VIDEO } };

BoolConfigVar v_winNoAltTab { "win_noAltTab"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } };
BoolConfigVar v_winNoWinKeys { "win_noWinKeys"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } };

static VarModificationTracker g_noAltTabVarTracker { &v_winNoAltTab };
static VarModificationTracker g_noWinKeysVarTracker { &v_winNoWinKeys };

typedef struct {
	unsigned width, height;             // coordinates from main game
} viddef_t;

static viddef_t viddef;             // global video state

static unsigned vid_ref_prevwidth, vid_ref_prevheight;
static bool vid_ref_modified;
static bool vid_ref_verbose;
static bool vid_ref_sound_restart;
static bool vid_ref_active;
static bool vid_initialized;
static bool vid_app_active;
static bool vid_app_minimized;

class VideoModeCache {
public:
	VideoModeCache() {
		if( !VID_GetSysModes( &m_modes ) ) {
			wsw::failWithRuntimeError( "Failed to retrieve system video modes" );
		}

		size_t numKeptModes = 0;
		for( const VideoMode &mode: m_modes ) {
			if( mode.width >= 1024 && mode.height >= 720 ) {
				m_modes[numKeptModes++] = mode;
			}
		}

		if( !numKeptModes ) {
			wsw::failWithRuntimeError( "Failed to find compatible (width >= 1024, height >= 720) video modes" );
		}

		m_modes.erase( m_modes.begin() + numKeptModes, m_modes.end() );
		assert( m_modes.size() == numKeptModes );
		wsw::sortPodNonSpeedCritical( m_modes.begin(), m_modes.end(), []( const VideoMode &lhs, const VideoMode &rhs ) {
			if( lhs.width == rhs.width ) {
				return lhs.height < rhs.height;
			}
			return lhs.width < rhs.width;
		});

		// Remove duplicate modes in case the sys code failed to do so.

		numKeptModes        = 0;
		unsigned prevWidth  = 0;
		unsigned prevHeight = 0;

		assert( m_maxHeight == 0 );
		for( const VideoMode &mode: m_modes ) {
			if( mode.width != prevWidth || mode.height != prevHeight ) {
				prevWidth   = mode.width;
				prevHeight  = mode.height;
				m_maxHeight = wsw::max( m_maxHeight, mode.height );
			}
			m_modes[numKeptModes++] = mode;
		}

		assert( numKeptModes );
		m_modes.erase( m_modes.begin() + numKeptModes, m_modes.end() );
		m_modes.shrink_to_fit();
	}

	[[nodiscard]]
	auto getBestFittingMode( unsigned requestedWidth, unsigned requestedHeight ) -> VideoMode {
		unsigned chosenWidth  = 0;
		unsigned chosenHeight = 0;

		unsigned leastWidthPenalty = std::numeric_limits<unsigned>::max();
		// Get a best matching mode for width first (which has a priority over height)
		for( const VideoMode &mode: m_modes ) {
			const auto absDiff = (unsigned)std::abs( (int)mode.width - (int)requestedWidth );
			assert( absDiff < std::numeric_limits<int>::max() >> 1 );
			// Set a penalty bit for modes with lesser than requested width
			const unsigned penalty = ( absDiff << 1u ) | ( mode.width >= requestedWidth ? 0 : 1 );
			if( leastWidthPenalty > penalty ) {
				leastWidthPenalty = penalty;
				chosenWidth = mode.width;
				if( chosenWidth == requestedWidth ) [[unlikely]] {
					break;
				}
			}
		}

		unsigned leastHeightPenalty = std::numeric_limits<unsigned>::max();
		// Get a best matching mode for height preserving the selected width
		for( const VideoMode &mode: m_modes ) {
			// Require an exact match of the chosen width
			if( mode.width == chosenWidth ) {
				const auto absDiff = (unsigned)std::abs( (int)mode.height - (int)requestedHeight );
				assert( absDiff < std::numeric_limits<unsigned>::max() >> 1 );
				// Set a penalty bit for modes with lesser than requested height
				const unsigned penalty = ( absDiff << 1u ) | ( mode.height >= requestedHeight ? 0 : 1 );
				if( leastHeightPenalty > penalty ) {
					leastHeightPenalty = penalty;
					chosenHeight = mode.height;
					if( chosenHeight == requestedHeight ) [[unlikely]] {
						break;
					}
				}
			}
		}

		assert( chosenWidth > 0 && chosenHeight > 0 );
		return VideoMode { .width = chosenWidth, .height = chosenHeight };
	}

	[[nodiscard]]
	auto getMaxWidth() const { return m_modes.back().width; }
	[[nodiscard]]
	auto getMaxHeight() const { return m_maxHeight; }

	[[nodiscard]]
	std::span<const VideoMode> getAllModes() const { return m_modes; }

	[[nodiscard]]
	auto getSafestMode() const -> VideoMode { return m_modes[0]; }

	[[nodiscard]]
	auto getDefaultMode() const -> VideoMode {
		int width = 0, height = 0;
		// TODO: Cache it?
		// TODO: Check whether it really belongs to the list?
		if( VID_GetDefaultMode( &width, &height ) ) {
			return VideoMode { (unsigned)width, (unsigned)height };
		}
		return getSafestMode();
	}
private:
	wsw::PodVector<VideoMode> m_modes;
	unsigned m_maxHeight { 0 };
};

static std::optional<VideoModeCache> g_videoModeCache;

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

static void VID_ModeList_f( const CmdArgs & ) {
	for( const VideoMode &mode: g_videoModeCache->getAllModes() ) {
		Com_Printf( "* %ix%i\n", mode.width, mode.height );
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
	// TODO: Should we limit the var upper bound? It's more consistent if it's applied here though.
	if( int value = (int)v_pixelRatio.get(); value > 0 ) {
		return wsw::clamp( value, 1, 2 );
	}
	return wsw::clamp( Sys_GetPixelRatio(), 1, 2 );
}

std::span<const VideoMode> VID_GetValidVideoModes() {
	return g_videoModeCache->getAllModes();
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

	if( g_noAltTabVarTracker.checkAndReset() ) {
		VID_EnableAltTab( !v_winNoAltTab.get() );
	}

	if( g_noWinKeysVarTracker.checkAndReset() ) {
		VID_EnableWinKeys( !v_winNoWinKeys.get() );
	}

	if( g_fullscreenVarTracker.checkAndReset() ) {
		if( vid_ref_active ) {
			// try to change video mode without vid_restart
			if( const rserr_t err = VID_ApplyPendingMode( R_TrySettingMode ); err == rserr_restart_required ) {
				vid_ref_modified = true;
			}
		}
	}

	if( vid_ref_modified ) {
		RestartVideoAndAllMedia( vid_ref_was_active, verbose );
	}

	if( g_xPosVarTracker.checkAndReset() | g_yPosVarTracker.checkAndReset() ) {
		if( !v_fullscreen.get() && !v_borderless.get() ) {
			VID_UpdateWindowPosAndSize( v_xPos.get(), v_yPos.get() );
		}
	}
}

void VID_DoRestart() {
	if( vid_ref_active ) {
		RF_Shutdown( false );
		vid_ref_active = false;
	}

	// handle vid size changes
	if( v_width.get() <= 0 || v_height.get() <= 0 ) {
		const VideoMode &defaultMode = g_videoModeCache->getDefaultMode();
		v_width.setImmediately( defaultMode.width );
		v_height.setImmediately( defaultMode.height );
	}

	if( const unsigned maxModeWidth = g_videoModeCache->getMaxWidth(); v_width.get() > maxModeWidth ) {
		v_width.setImmediately( maxModeWidth );
	}
	if( const unsigned maxModeHeight = g_videoModeCache->getMaxHeight(); v_height.get() > maxModeHeight ) {
		v_height.setImmediately( maxModeHeight );
	}

	if( v_fullscreen.get() ) {
		// snap to the closest fullscreen resolution, width has priority over height
		const unsigned requestedWidth  = v_width.get();
		const unsigned requestedHeight = v_height.get();
		const VideoMode &bestMode      = g_videoModeCache->getBestFittingMode( requestedWidth, requestedHeight );
		if( bestMode.width != requestedWidth ) {
			v_width.setImmediately( bestMode.width );
		}
		if( bestMode.height != requestedHeight ) {
			v_height.setImmediately( bestMode.height );
		}
	}

	if( rserr_t err = VID_Sys_Init_( nullptr, vid_ref_verbose ); err != rserr_ok ) {
		Sys_Error( "VID_Init() failed with code %i", err );
	}

	vid_ref_active = true;
	vid_ref_modified = false;
	vid_ref_verbose = true;
}

rserr_t VID_ApplyPendingMode( rserr_t ( *tryToApplyFn )( int, int, int, int, int, const VidModeOptions & ) ) {
	(void)g_fullscreenVarTracker.checkAndReset();

	const int frequency = v_displayFrequency.get();

	VidModeOptions options { .fullscreen = v_fullscreen.get(), .borderless = v_borderless.get() };

	int x, y;
	unsigned w, h;
	if( options.fullscreen && options.borderless ) {
		x = 0, y = 0;
		const VideoMode defaultMode = g_videoModeCache->getDefaultMode();
		w = defaultMode.width;
		h = defaultMode.height;
	} else {
		x = v_xPos.get();
		y = v_yPos.get();
		w = v_width.get();
		h = v_height.get();
	}

	if( vid_ref_active && ( w != viddef.width || h != viddef.height ) ) {
		return rserr_restart_required;
	}

	rserr_t err = tryToApplyFn( x, y, w, h, frequency, options );
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

			v_fullscreen.setImmediately( false );
			(void)g_fullscreenVarTracker.checkAndReset();

			// Try again without the fullscreen flag
			options.fullscreen = false;
			err = tryToApplyFn( x, y, w, h, frequency, options );
		}

		if( err == rserr_invalid_mode ) {
			clWarning() << "Invalid video mode";

			// Try setting it back to something safe
			if( w != vid_ref_prevwidth || h != vid_ref_prevheight ) {
				w = vid_ref_prevwidth;
				v_width.setImmediately( w );
				h = vid_ref_prevheight;
				v_height.setImmediately( h );

				err = tryToApplyFn( x, y, w, h, frequency, options );
				if( err == rserr_invalid_fullscreen ) {
					clWarning() << "Could not revert to safe fullscreen mode";

					v_fullscreen.setImmediately( false );
					(void)g_fullscreenVarTracker.checkAndReset();

					// Try again without the fullscreen flag
					options.fullscreen = false;
					err = tryToApplyFn( x, y, w, h, frequency, options );
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

void VID_Init( void ) {
	if( !vid_initialized ) {
		g_videoModeCache = VideoModeCache();

		/* Add some console commands that we want to handle */
		CL_Cmd_Register( "vid_restart"_asView, VID_Restart_f );
		CL_Cmd_Register( "vid_modelist"_asView, VID_ModeList_f );

		/* Start the graphics mode and load refresh DLL */
		vid_ref_modified = true;
		vid_ref_active = false;
		vid_ref_verbose = true;
		vid_initialized = true;
		vid_ref_sound_restart = false;

		(void)g_fullscreenVarTracker.checkAndReset();
		(void)g_borderlessVarTracker.checkAndReset();

		vid_ref_prevwidth = g_videoModeCache->getSafestMode().width;
		vid_ref_prevheight = g_videoModeCache->getSafestMode().height;

		FTLIB_Init( true );

		VID_CheckChanges();
	} else {
		abort();
	}
}

void VID_Shutdown( void ) {
	if( vid_initialized ) {
		if( vid_ref_active ) {
			RF_Shutdown( false );
			vid_ref_active = false;
		}

		FTLIB_Shutdown( true );

		CL_Cmd_Unregister( "vid_restart"_asView );
		CL_Cmd_Unregister( "vid_modelist"_asView );

		vid_initialized = false;
	} else {
		abort();
	}
}