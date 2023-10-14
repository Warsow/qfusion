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

#include <QVariant>
#include <QtPlatformHeaders/QGLXNativeContext>

#include <SDL.h>
#include <SDL_syswm.h>
#include "../client/client.h"
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

float VID_GetPixelRatio( void ) {
#if SDL_VERSION_ATLEAST( 2,0,4 )
	float vdpi;

	if( SDL_GetDisplayDPI( 0, NULL, NULL, &vdpi ) == 0 ) {
		return vdpi / 96.0f;
	}
#endif

	return 1.0f; // TODO: check if retina?
}

QVariant VID_GetMainContextHandle() {
	SDL_SysWMinfo info;
	SDL_GetVersion( &info.version );
	if( !SDL_GetWindowWMInfo( sdl_window, &info ) ) {
		return QVariant();
	}

	Display *display = info.info.x11.display;
	Window window = info.info.x11.window;
	auto context = (GLXContext)glw_state.sdl_glcontext;
	QGLXNativeContext result( context, display, window );
	return QVariant::fromValue( result );
}

typedef Drawable (*GlxGetCurrentDrawbleFn)();
typedef int (*GlxMakeContextCurrentFn)( Display *, GLXDrawable, GLXDrawable, GLXContext );
typedef GLXContext (*GlxGetCurrentContextFn)();
typedef Display *(*GlxGetCurrentDisplayFn)();
typedef void (*GlxSwapBuffersFn)(Display *, GLXDrawable);

static GlxGetCurrentDrawbleFn qglXGetCurrentDrawable;
static GlxGetCurrentDrawbleFn qglXGetCurrentReadDrawable;
static GlxGetCurrentDisplayFn qglXGetCurrentDisplay;
static GlxGetCurrentContextFn qglXGetCurrentContext;
static GlxMakeContextCurrentFn qglXMakeContextCurrent;
static GlxSwapBuffersFn qglXSwapBuffers;

static Drawable savedDrawable;
static Drawable savedReadDrawable;

#define LOAD_GLX_PROC( name )                                               \
do {                                                                        \
	q ## name = ( decltype( q ## name ) )SDL_GL_GetProcAddress( #name );    \
	if( !q ## name ) {                                                      \
		Com_Error( ERR_FATAL, "Failed to load an address %s\n", #name );    \
	}																		\
} while( 0 )

static void loadGlxStuff() {
	LOAD_GLX_PROC( glXGetCurrentDrawable );
	LOAD_GLX_PROC( glXGetCurrentReadDrawable );
	LOAD_GLX_PROC( glXMakeContextCurrent );
	LOAD_GLX_PROC( glXGetCurrentDisplay );
	LOAD_GLX_PROC( glXGetCurrentContext );
	LOAD_GLX_PROC( glXSwapBuffers );
}

bool GLimp_BeginUIRenderingHacks() {
	loadGlxStuff();

	SDL_SysWMinfo info;
	SDL_GetVersion( &info.version );
	SDL_GetWindowWMInfo( glw_state.sdl_window, &info );
	auto display = info.info.x11.display;
	auto window  = info.info.x11.window;
	clNotice() << ">>>>>>>>>>>>>>>>> Begin hacks";
	clNotice() << "Window" << window;
	clNotice() << "Saved drawables:" << (uintptr_t)savedDrawable << (uintptr_t)savedReadDrawable;
	clNotice() << "Actual drawables:" << (uintptr_t)qglXGetCurrentDrawable() << (uintptr_t)qglXGetCurrentReadDrawable();
	// Fixes the Qml Glow bug
	// TODO: Flush on restart
	if( !savedDrawable ) {
		savedDrawable = qglXGetCurrentDrawable();
	}
	if( !savedReadDrawable ) {
		savedReadDrawable = qglXGetCurrentReadDrawable();
	}
	clNotice() << "SDL Display" << (uintptr_t)display << "Current display" << (uintptr_t)qglXGetCurrentDisplay();
	clNotice() << "SDL Context" << (uintptr_t)glw_state.sdl_glcontext << "Current context" << (uintptr_t)qglXGetCurrentContext();
	assert( savedDrawable == info.info.x11.window );
	return true;
}

static int counter = 0;

bool GLimp_EndUIRenderingHacks() {
	loadGlxStuff();

	SDL_SysWMinfo info;
	SDL_GetVersion( &info.version );
	SDL_GetWindowWMInfo( glw_state.sdl_window, &info );
	auto display = info.info.x11.display;

	assert( savedDrawable && savedReadDrawable );
	clNotice() << "================ End hacks";
	clNotice() << "Saved drawables:" << (uintptr_t)savedDrawable << (uintptr_t)savedReadDrawable;
	clNotice() << "Actual drawables:" << (uintptr_t)qglXGetCurrentDrawable() << (uintptr_t)qglXGetCurrentReadDrawable();
	clNotice() << "SDL Display" << (uintptr_t)display << "Current display" << (uintptr_t)qglXGetCurrentDisplay();
	clNotice() << "SDL Context" << (uintptr_t)glw_state.sdl_glcontext << "Current context" << (uintptr_t)qglXGetCurrentContext();

	//qglXMakeContextCurrent( qglXGetCurrentDisplay(), 0, 0, 0 );

	auto res = qglXMakeContextCurrent( display, savedDrawable, savedReadDrawable, (GLXContext)glw_state.sdl_glcontext );
	clNotice() << "<<<<<<<<<<<<<<<< Done with hacks";
	clNotice() << "Saved drawables:" << (uintptr_t)savedDrawable << (uintptr_t)savedReadDrawable;
	clNotice() << "Actual drawables:" << (uintptr_t)qglXGetCurrentDrawable() << (uintptr_t)qglXGetCurrentReadDrawable();
	clNotice() << "SDL Display" << (uintptr_t)display << "Current display" << (uintptr_t)qglXGetCurrentDisplay();
	clNotice() << "SDL Context" << (uintptr_t)glw_state.sdl_glcontext << "Current context" << (uintptr_t)qglXGetCurrentContext();

	//savedDrawable = savedReadDrawable = 0;
	/*
	if( counter++ > 2 ) {
		abort();
	}*/
	return res;
}

void GLimp_EndFrame( void ) {
	SDL_SysWMinfo info;
	SDL_GetVersion( &info.version );
	SDL_GetWindowWMInfo( glw_state.sdl_window, &info );
	auto display = info.info.x11.display;
	auto window  = info.info.x11.window;
	loadGlxStuff();
	clNotice() << "!!!!!!!!!!!!!!!! End frame";
	clNotice() << "Saved drawables:" << (uintptr_t)savedDrawable << (uintptr_t)savedReadDrawable;
	clNotice() << "Actual drawables:" << (uintptr_t)qglXGetCurrentDrawable() << (uintptr_t)qglXGetCurrentReadDrawable();
	clNotice() << "SDL Display" << (uintptr_t)display << "Current display" << (uintptr_t)qglXGetCurrentDisplay();
	clNotice() << "SDL Context" << (uintptr_t)glw_state.sdl_glcontext << "Current context" << (uintptr_t)qglXGetCurrentContext();
	assert( !savedDrawable || savedDrawable == qglXGetCurrentDrawable() );
	assert( !savedReadDrawable || savedReadDrawable == qglXGetCurrentReadDrawable() );
	assert( !savedDrawable || savedDrawable == window );
	assert( display ==  qglXGetCurrentDisplay() );
	assert( glw_state.sdl_glcontext == qglXGetCurrentContext() );
	SDL_GL_SwapWindow( glw_state.sdl_window );

}