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

#ifndef WSW_9f8b11be_6d3e_4138_927d_dd7f70cc0bb0_H
#define WSW_9f8b11be_6d3e_4138_927d_dd7f70cc0bb0_H

#include <common/types/podvector.h>
#include <span>

struct VideoMode {
	unsigned width;
	unsigned height;
};

struct VidModeOptions {
	bool fullscreen { false };
	bool borderless { false };
};

enum rserr_t : int {
	rserr_ok,
	rserr_invalid_fullscreen,
	rserr_invalid_mode,
	rserr_invalid_driver,
	rserr_restart_required,
	rserr_unknown
};

// Video module initialisation etc
void VID_Init( void );
void VID_Shutdown( void );
rserr_t VID_ApplyPendingMode( rserr_t ( *tryToApplyFn )( int, int, int, int, int, const VidModeOptions & ) );
void VID_Restart( bool verbose, bool soundRestart );
void VID_DoRestart();
// The sound module may require the handle when using directsound
void *VID_GetWindowHandle( void );
void VID_CheckChanges();
void VID_WindowInitialized();
void VID_FlashWindow( int count );
bool VID_GetDefaultMode( int *width, int *height );
bool VID_GetSysModes( wsw::PodVector<VideoMode> *modes );
std::span<const VideoMode> VID_GetValidVideoModes();
void VID_AppActivate( bool active, bool minimize, bool destroy );
bool VID_RefreshIsActive( void );
bool VID_AppIsActive( void );
bool VID_AppIsMinimized( void );
int VID_GetWindowWidth( void );
int VID_GetWindowHeight( void );
// Explicitly state that we don't support fractional scaling
int VID_GetPixelRatio( void );

#endif
