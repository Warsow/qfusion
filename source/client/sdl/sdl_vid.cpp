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

#include <QVariant>
#include <QtPlatformHeaders/QGLXNativeContext>

#include <SDL.h>
#include <SDL_syswm.h>

#include <SDL.h>
#include <SDL_syswm.h>

#include <client/renderer/local.h>
#include "sdl_glw.h"

// TODO: Make Win32 SDL builds up-to-date
#ifndef X_PROTOCOL
#error This code is X11-specific
#endif

typedef Drawable ( *GlxGetCurrentDrawableFn )();
typedef int ( *GlxMakeContextCurrentFn )( Display *, GLXDrawable, GLXDrawable, GLXContext );

static GlxMakeContextCurrentFn qglXMakeContextCurrent;

static Display *g_savedDisplay      = nullptr;
static Window g_savedWindow         = None;
static Drawable g_savedDrawable     = None;
static Drawable g_savedReadDrawable = None;

glwstate_t glw_state = {NULL, NULL};

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

void GLimp_EndFrame( void ) {
	SDL_GL_SwapWindow( glw_state.sdl_window );
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

void VID_WindowInitialized() {
	SDL_SysWMinfo info;
	SDL_GetVersion( &info.version );
	if( !SDL_GetWindowWMInfo( sdl_window, &info ) ) {
		Com_Error( ERR_FATAL, "Failed to get SDL_SysWMinfo" );
	}

	const auto getGlxAddress = []( const char *name ) -> void * {
		auto result = SDL_GL_GetProcAddress( name );
		if( !result ) {
			Com_Error( ERR_FATAL, "Failed to retrieve %s address", name );
		}
		return result;
	};

	const auto getDrawable     = (GlxGetCurrentDrawableFn)getGlxAddress( "glXGetCurrentDrawable" );
	const auto getReadDrawable = (GlxGetCurrentDrawableFn)getGlxAddress( "glXGetCurrentReadDrawable" );
	qglXMakeContextCurrent     = (GlxMakeContextCurrentFn)getGlxAddress( "glXMakeContextCurrent" );

	Drawable drawable = getDrawable();
	if( drawable == None ) {
		Com_Error( ERR_FATAL, "Failed to retrieve the current Drawable" );
	}
	Drawable readDrawable = getReadDrawable();
	if( readDrawable == None ) {
		Com_Error( ERR_FATAL, "Failed to retrieve the current read Drawable" );
	}
	if( drawable != info.info.x11.window ) {
		Com_Error( ERR_FATAL, "The current drawable does not match the current window" );
	}

	// Save these values once for further use.
	// Note that trying to save inside GLimp_BeginUIRenderingHacks() leads to saving wrong drawables
	// in some cases (namely in case of using some of Qml graphical effects)

	g_savedDisplay      = info.info.x11.display;
	g_savedWindow       = info.info.x11.window;
	g_savedDrawable     = drawable;
	g_savedReadDrawable = readDrawable;
}

QVariant VID_GetMainContextHandle() {
	auto context = (GLXContext)glw_state.sdl_glcontext;
	assert( context && g_savedDisplay && g_savedWindow );
	return QVariant::fromValue( QGLXNativeContext( context, g_savedDisplay, g_savedWindow ) );
}

bool GLimp_BeginUIRenderingHacks() {
	return true;
}

bool GLimp_EndUIRenderingHacks() {
	assert( g_savedDisplay && g_savedDrawable && g_savedReadDrawable && glw_state.sdl_glcontext );
	return qglXMakeContextCurrent( g_savedDisplay, g_savedDrawable, g_savedReadDrawable, (GLXContext)glw_state.sdl_glcontext ) == True;
}