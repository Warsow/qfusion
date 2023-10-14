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

#include <SDL.h>
#include <SDL_syswm.h>

#include "../ref/local.h"
#include "sdl_glw.h"

glwstate_t glw_state = {NULL, NULL};

static bool GLimp_InitGL( int stencilbits );

void GLimp_SetWindowIcon( void ) {
#ifndef __APPLE__
	const int *xpm_icon = glw_state.applicationIcon;

	if( xpm_icon ) {
		SDL_Surface *surface;

		surface = SDL_CreateRGBSurfaceFrom( (void *)( xpm_icon + 2 ), xpm_icon[0], xpm_icon[1], 32, xpm_icon[0] * 4,
#ifdef ENDIAN_LITTLE
											0x00ff0000, 0x0000ff00, 0x000000ff, 0xff000000 );
#else
											0x000000ff, 0x0000ff00, 0x00ff0000, 0xff000000 );
#endif

		SDL_SetWindowIcon( glw_state.sdl_window, surface );

		SDL_FreeSurface( surface );
	}
#endif
}

rserr_t GLimp_SetFullscreenMode( int displayFrequency, bool fullscreen ) {
	Uint32 flags = 0;
	bool borderless = glConfig.borderless;

	if( fullscreen ) {
		flags = SDL_WINDOW_FULLSCREEN;
	}
	if( borderless ) {
		// we need to use SDL_WINDOW_FULLSCREEN_DESKTOP to support Alt+Tab from fullscreen on OS X
		flags = SDL_WINDOW_FULLSCREEN_DESKTOP;
	}

	if( SDL_SetWindowFullscreen( glw_state.sdl_window, flags ) == 0 ) {
		glConfig.fullScreen = fullscreen;
		return rserr_ok;
	}

	return rserr_invalid_fullscreen;
}

static void GLimp_CreateWindow( int x, int y, int width, int height ) {
	unsigned flags = SDL_WINDOW_OPENGL;

	glw_state.sdl_window = SDL_CreateWindow( glw_state.applicationName,
											 SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, flags );

	if( !glw_state.sdl_window ) {
		Sys_Error( "Couldn't create window: \"%s\"", SDL_GetError() );
	}

	if( glw_state.wndproc ) {
		glw_state.wndproc( glw_state.sdl_window, 0, 0, 0 );
	}

	SDL_SetWindowPosition( glw_state.sdl_window, x, y );

	GLimp_SetWindowIcon();
}

rserr_t GLimp_SetMode( int x, int y, int width, int height, int displayFrequency, const VidModeOptions &options ) {
	const char *win_fs[] = {"W", "FS"};

	const bool fullscreen = options.fullscreen;
#ifndef __APPLE__
	const bool borderless = options.borderless;
#else
	const bool borderless = options.fullscreen;
#endif

	Com_Printf( "Initializing OpenGL display\n" );
	Com_Printf( "...setting mode:" );
	Com_Printf( " %d %d %s\n", width, height, win_fs[fullscreen] );

	// destroy the existing window
	if( glw_state.sdl_window ) {
		GLimp_Shutdown();
	}

	GLimp_CreateWindow( x, y, width, height );

	// init all the gl stuff for the window
	if( !GLimp_InitGL( r_stencilbits->integer ) ) {
		Com_Printf( "VID_CreateWindow() - GLimp_InitGL failed\n" );
		return rserr_invalid_mode;
	}

	glConfig.width = width;
	glConfig.height = height;
	glConfig.borderless = borderless;
	glConfig.fullScreen = fullscreen;
	if( GLimp_SetFullscreenMode( displayFrequency, fullscreen ) == rserr_ok ) {
		glConfig.fullScreen = fullscreen;
	} else {
		glConfig.fullScreen = !fullscreen;
	}

	return glConfig.fullScreen == fullscreen ? rserr_ok : rserr_invalid_fullscreen;
}

void GLimp_Shutdown() {
	SDL_DestroyWindow( glw_state.sdl_window );

	free( glw_state.applicationName );
	free( glw_state.applicationIcon );

	memset( &glw_state, 0, sizeof( glw_state ) );

	glConfig.width = 0;
	glConfig.height = 0;
}

bool GLimp_Init( const char *applicationName, void *hinstance, void *wndproc, void *parenthWnd,
				 int iconResource, const int *iconXPM ) {
	glw_state.wndproc = (wndproc_t)wndproc;
	glw_state.applicationName = strdup( applicationName );
	glw_state.applicationIcon = NULL;
	memcpy( glw_state.applicationName, applicationName, strlen( applicationName ) + 1 );

	if( iconXPM ) {
		size_t icon_memsize = iconXPM[0] * iconXPM[1] * sizeof( int );
		glw_state.applicationIcon = (int *)malloc( icon_memsize );
		memcpy( glw_state.applicationIcon, iconXPM, icon_memsize );
	}

	return true;
}

static bool GLimp_InitGL( int stencilbits ) {
	int colorBits, depthBits, stencilBits;

	SDL_GL_SetAttribute( SDL_GL_STENCIL_SIZE, wsw::max( 0, stencilbits ) );
	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
	SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 3 );
	SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE );

	glw_state.sdl_glcontext = SDL_GL_CreateContext( glw_state.sdl_window );
	if( glw_state.sdl_glcontext == 0 ) {
		Com_Printf( "GLimp_Init() - SDL_GL_CreateContext failed: \"%s\"\n", SDL_GetError() );
		goto fail;
	}

	if( SDL_GL_MakeCurrent( glw_state.sdl_window, glw_state.sdl_glcontext ) ) {
		Com_Printf( "GLimp_Init() - SDL_GL_MakeCurrent failed: \"%s\"\n", SDL_GetError() );
		goto fail;
	}

	/*
	 ** print out PFD specifics
	 */
	SDL_GL_GetAttribute( SDL_GL_BUFFER_SIZE, &colorBits );
	SDL_GL_GetAttribute( SDL_GL_DEPTH_SIZE, &depthBits );
	SDL_GL_GetAttribute( SDL_GL_STENCIL_SIZE, &stencilBits );

	glConfig.stencilBits = stencilBits;

	Com_Printf( "GL PFD: color(%d-bits) Z(%d-bit) stencil(%d-bits)\n", colorBits, depthBits, stencilBits );

	return true;

fail:
	return false;
}

void GLimp_BeginFrame( void ) {
}


bool GLimp_GetGammaRamp( size_t stride, unsigned short *psize, unsigned short *ramp ) {
	unsigned short ramp256[3 * 256];

	if( stride < 256 ) {
		// SDL only supports gamma ramps with 256 mappings per channel
		return false;
	}

	if( SDL_GetWindowGammaRamp( glw_state.sdl_window, ramp256, ramp256 + 256, ramp256 + ( 256 << 1 ) ) != -1 ) {
		*psize = 256;
		memcpy( ramp, ramp256, 256 * sizeof( *ramp ) );
		memcpy( ramp + stride, ramp256 + 256, 256 * sizeof( *ramp ) );
		memcpy( ramp + 2 * stride, ramp256 + 2 * 256, 256 * sizeof( *ramp ) );
		return true;
	}
	return false;
}

void GLimp_SetGammaRamp( size_t stride, unsigned short size, unsigned short *ramp ) {
	unsigned short ramp256[3 * 256];

	if( size != 256 ) {
		return;
	}

	memcpy( ramp256, ramp, size * sizeof( *ramp ) );
	memcpy( ramp256 + 256, ramp + stride, size * sizeof( *ramp ) );
	memcpy( ramp256 + 2 * 256, ramp + 2 * stride, size * sizeof( *ramp ) );
	if( SDL_SetWindowGammaRamp( glw_state.sdl_window, ramp256, ramp256 + 256, ramp256 + ( 256 << 1 ) ) == -1 ) {
		Com_Printf( "SDL_SetWindowGammaRamp() failed: \"%s\"\n", SDL_GetError() );
	}
}

void GLimp_AppActivate( bool active, bool minimize, bool destroy ) {
}

void GLimp_SetSwapInterval( int swapInterval ) {
	SDL_GL_SetSwapInterval( swapInterval );
}