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

#include <client/client.h>
#include "sdl_glw.h"

SDL_Window *sdl_window;

static int VID_WndProc( void *wnd, int ev, int p1, int p2 ) {
	sdl_window = (SDL_Window *)wnd;
	return 0;
}

rserr_t VID_Sys_Init( const char *applicationName, const char *screenshotsPrefix, int startupColor,
					  const int *iconXPM, void *parentWindow, bool verbose ) {
	return R_Init( applicationName, screenshotsPrefix, startupColor, 0, iconXPM, NULL, (void *)VID_WndProc, parentWindow, verbose );
}

void VID_UpdateWindowPosAndSize( int x, int y ) {
	SDL_SetWindowPosition( sdl_window, x, y );
}

void VID_EnableAltTab( bool enable ) {
}

void *VID_GetWindowHandle( void ) {
	return (void *)NULL;
}

void VID_EnableWinKeys( bool enable ) {
}

void VID_FlashWindow( int count ) {
}

unsigned int VID_GetSysModes( vidmode_t *modes ) {
#ifdef __APPLE__
	// only support borderless fullscreen because Alt+Tab doesn't work in fullscreen
	if( modes ) {
		VID_GetDefaultMode( &modes[0].width, &modes[0].height );
	}
	return 1;
#else
	int num;
	SDL_DisplayMode mode;
	int prevwidth = 0, prevheight = 0;
	unsigned int ret = 0;

	num = SDL_GetNumDisplayModes( 0 );
	if( num < 1 ) {
		return 0;
	}

	while( num-- ) { // reverse to help the sorting a little
		if( SDL_GetDisplayMode( 0, num, &mode ) ) {
			continue;
		}

		if( SDL_BITSPERPIXEL( mode.format ) < 15 ) {
			continue;
		}

		if( ( mode.w == prevwidth ) && ( mode.h == prevheight ) ) {
			continue;
		}

		if( modes ) {
			modes[ret].width = mode.w;
			modes[ret].height = mode.h;
		}

		prevwidth = mode.w;
		prevheight = mode.h;

		ret++;
	}

	return ret;
#endif
}

bool VID_GetDefaultMode( int *width, int *height ) {
	SDL_DisplayMode mode;
	SDL_GetDesktopDisplayMode( 0, &mode );

	*width = mode.w;
	*height = mode.h;

	return true;
}

int Sys_GetPixelRatio() {
#if SDL_VERSION_ATLEAST( 2,0,4 )
	float vdpi = 96.0f;
	(void)SDL_GetDisplayDPI( 0, NULL, NULL, &vdpi );
	return (int)std::round( vdpi / 96.0f );
#else
#error Unsupported SDL version
#endif
}
