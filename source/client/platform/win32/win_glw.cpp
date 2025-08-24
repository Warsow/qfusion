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
/*
** GLW_IMP.C
**
** This file contains ALL Win32 specific stuff having to do with the
** OpenGL refresh.  When a port is being made the following functions
** must be implemented by the port:
**
** GLimp_EndFrame
** GLimp_Init
** GLimp_Shutdown
**
*/
#include <assert.h>
#include <client/renderer/local.h>
#include <common/facilities/syspublic.h>
#include "win_glw.h"

#include <QVariant>
#include <QtPlatformHeaders/QWGLNativeContext>

// It's better to load all this stuff locally here

#define WGL_CONTEXT_MAJOR_VERSION_ARB           0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB           0x2092
#define WGL_CONTEXT_PROFILE_MASK_ARB            0x9126
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB        0x00000001

#define WGL_DRAW_TO_WINDOW_ARB            0x2001
#define WGL_SUPPORT_OPENGL_ARB            0x2010
#define WGL_DOUBLE_BUFFER_ARB             0x2011
#define WGL_STEREO_ARB                    0x2012
#define WGL_PIXEL_TYPE_ARB                0x2013
#define WGL_COLOR_BITS_ARB                0x2014
#define WGL_RED_BITS_ARB                  0x2015
#define WGL_GREEN_BITS_ARB                0x2017
#define WGL_BLUE_BITS_ARB                 0x2019
#define WGL_ALPHA_BITS_ARB                0x201B
#define WGL_DEPTH_BITS_ARB                0x2022
#define WGL_STENCIL_BITS_ARB              0x2023
#define WGL_TYPE_RGBA_ARB                 0x202B

#define WGL_SAMPLE_BUFFERS_ARB            0x2041
#define WGL_SAMPLES_ARB                   0x2042

static const char *( APIENTRY *qwglGetExtensionsStringEXT )();
static BOOL ( APIENTRY *qwglGetDeviceGammaRamp3DFX )( HDC, WORD * );
static BOOL ( APIENTRY *qwglSetDeviceGammaRamp3DFX )( HDC, WORD * );
static BOOL ( APIENTRY *qwglSwapIntervalEXT )( int interval );

static BOOL ( APIENTRY *qwglGetPixelFormatAttribivARB )( HDC hdc, int iPixelFormat, int iLayerPlane, UINT nAttributes, const int *piAttributes, int *piValues );
static BOOL ( APIENTRY *qwglGetPixelFormatAttribfvARB )( HDC hdc, int iPixelFormat, int iLayerPlane, UINT nAttributes, const int *piAttributes, FLOAT * pfValues );
static BOOL ( APIENTRY *qwglChoosePixelFormatARB )( HDC hdc, const int *piAttribIList, const FLOAT * pfAttribFList, UINT nMaxFormats, int *piFormats, UINT * nNumFormats );
static HGLRC ( APIENTRY *qwglCreateContextAttribsARB )( HDC hdc, HGLRC hshareContext, const int *attribList );

/*
** qglGetProcAddress
*/
void *qglGetProcAddress( const GLubyte *procName ) {
	return (void *)qwglGetProcAddress( (LPCSTR)procName );
}

const char *qglGetGLWExtensionsString() {
	return qwglGetExtensionsStringEXT();
}

#define WINDOW_STYLE    ( WS_OVERLAPPED | WS_BORDER | WS_CAPTION | WS_VISIBLE | WS_SYSMENU | WS_MINIMIZEBOX )

static bool GLimp_InitGL( bool isInitialDummyContext );
glwstate_t glw_state;

/*
** GLimp_CreateWindow
*/
#define WITH_UTF8

#pragma warning( disable : 4055 )

static int GLimp_GetWindowStyle( bool fullscreen, bool borderless, int *pexstyle ) {
	int stylebits;
	int exstyle;
	HWND parentHWND = glw_state.parenthWnd;

	if( fullscreen ) {
		exstyle = WS_EX_TOPMOST;
		stylebits = ( WS_POPUP | WS_VISIBLE );
		parentHWND = NULL;
	} else {
		if( parentHWND ) {
			exstyle = 0;
			stylebits = WS_CHILD | WS_CLIPSIBLINGS | WS_CLIPCHILDREN | WS_VISIBLE;
		} else {
			exstyle = 0;
			stylebits = WINDOW_STYLE;
		}
	}

	*pexstyle = exstyle;

	return stylebits;
}

static void GLimp_SetWindowSize( bool fullscreen, bool borderless ) {
	RECT r;
	int stylebits;
	int exstyle;
	int x = glw_state.win_x, y = glw_state.win_y;
	int width = glConfig.width, height = glConfig.height;
	HWND parentHWND = glw_state.parenthWnd;

	if( !glw_state.hWnd ) {
		return;
	}

	r.left = 0;
	r.top = 0;
	r.right  = width;
	r.bottom = height;

	stylebits = GLimp_GetWindowStyle( fullscreen, borderless, &exstyle );

	AdjustWindowRect( &r, stylebits, FALSE );

	width = r.right - r.left;
	height = r.bottom - r.top;

	if( fullscreen ) {
		x = 0;
		y = 0;
	} else if( parentHWND ) {
		RECT parentWindowRect;

		GetWindowRect( parentHWND, &parentWindowRect );

		// share centre with the parent window
		x = ( parentWindowRect.right - parentWindowRect.left - width ) / 2;
		y = ( parentWindowRect.bottom - parentWindowRect.top - height ) / 2;
	}

	SetActiveWindow( glw_state.hWnd );

	SetWindowLong( glw_state.hWnd, GWL_EXSTYLE, exstyle );
	SetWindowLong( glw_state.hWnd, GWL_STYLE, stylebits );

	SetWindowPos( glw_state.hWnd, HWND_TOP, x, y, width, height, SWP_FRAMECHANGED );

	ShowWindow( glw_state.hWnd, SW_SHOW );
	UpdateWindow( glw_state.hWnd );

	SetForegroundWindow( glw_state.hWnd );
	SetFocus( glw_state.hWnd );
}

static void GLimp_CreateWindow( bool dummy ) {
	HWND parentHWND = glw_state.parenthWnd;
#ifdef WITH_UTF8
	WNDCLASSW wc;
#else
	WNDCLASS wc;
#endif

	Q_snprintfz( glw_state.windowClassName, sizeof( glw_state.windowClassName ), "%sWndClass", glw_state.applicationName );
#ifdef WITH_UTF8
	MultiByteToWideChar( CP_UTF8, 0, glw_state.windowClassName, -1, glw_state.windowClassNameW, sizeof( glw_state.windowClassNameW ) );
	glw_state.windowClassNameW[sizeof( glw_state.windowClassNameW ) / sizeof( glw_state.windowClassNameW[0] ) - 1] = 0;
#endif

	/* Register the frame class */
	wc.style         = 0;
	wc.lpfnWndProc   = (WNDPROC)glw_state.wndproc;
	wc.cbClsExtra    = 0;
	wc.cbWndExtra    = 0;
	wc.hInstance     = glw_state.hInstance;
	wc.hIcon         = LoadIcon( glw_state.hInstance, MAKEINTRESOURCE( glw_state.applicationIconResourceID ) );
	wc.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wc.hbrBackground = (HBRUSH)GetStockObject( BLACK_BRUSH );
	wc.lpszMenuName  = 0;
#ifdef WITH_UTF8
	wc.lpszClassName = (LPCWSTR)glw_state.windowClassNameW;
	if( !RegisterClassW( &wc ) )
#else
	wc.lpszClassName = (LPCSTR)glw_state.windowClassName;
	if( !RegisterClass( &wc ) )
#endif
	{ Sys_Error( "Couldn't register window class" );}

	glw_state.hWnd =
#ifdef WITH_UTF8
		CreateWindowExW(
#else
		CreateWindowEx(
#endif
			0,
#ifdef WITH_UTF8
			glw_state.windowClassNameW,
			glw_state.applicationNameW,
#else
			glw_state.windowClassName,
			glw_state.applicationName,
#endif
			0,
			0, 0, 0, 0,
			parentHWND,
			NULL,
			glw_state.hInstance,
			NULL );

	if( !glw_state.hWnd ) {
		Sys_Error( "Couldn't create window" );
	}

	if( !dummy ) {
		GLimp_SetWindowSize( glConfig.fullScreen, glConfig.borderless );
	}
}

/*
** GLimp_SetFullscreenMode
*/
rserr_t GLimp_SetFullscreenMode( int displayFrequency, bool fullscreen ) {
	glConfig.fullScreen = false;

	// do a CDS if needed
	if( fullscreen && !glConfig.borderless ) {
		int a;
		DEVMODE dm;

		Com_Printf( "...attempting fullscreen\n" );

		memset( &dm, 0, sizeof( dm ) );

		dm.dmSize = sizeof( dm );

		dm.dmPelsWidth  = glConfig.width;
		dm.dmPelsHeight = glConfig.height;
		dm.dmFields     = DM_PELSWIDTH | DM_PELSHEIGHT;

		if( displayFrequency > 0 ) {
			dm.dmFields |= DM_DISPLAYFREQUENCY;
			dm.dmDisplayFrequency = displayFrequency;
			Com_Printf( "...using display frequency %i\n", dm.dmDisplayFrequency );
		}

		Com_Printf( "...calling CDS: " );
		a = ChangeDisplaySettings( &dm, CDS_FULLSCREEN );
		if( a == DISP_CHANGE_SUCCESSFUL ) {
			Com_Printf( "ok\n" );
			glConfig.fullScreen = true;
			GLimp_SetWindowSize( true, glConfig.borderless );
			return rserr_ok;
		}

		Com_Printf( "failed: %x\n", a );
		return rserr_invalid_fullscreen;
	}

	ChangeDisplaySettings( 0, 0 );
	GLimp_SetWindowSize( fullscreen, glConfig.borderless );
	glConfig.fullScreen = fullscreen;

	return rserr_ok;
}

rserr_t GLimp_SetMode( int x, int y, int width, int height, int displayFrequency, const VidModeOptions &options ) {
	bool fullscreen = options.fullscreen;

	const char *win_fs[] = { "W", "FS" };

	Com_Printf( "Setting video mode:" );

	// disable fullscreen if rendering to a parent window
	if( glw_state.parenthWnd ) {
		fullscreen = false;

		RECT parentWindowRect;
		GetWindowRect( glw_state.parenthWnd, &parentWindowRect );
		width = parentWindowRect.right - parentWindowRect.left;
		height = parentWindowRect.bottom - parentWindowRect.top;
	}

	Com_Printf( " %d %d %s\n", width, height, win_fs[fullscreen] );

	// destroy the existing window
	if( glw_state.hWnd ) {
		GLimp_Shutdown();
	}

	glw_state.win_x = x;
	glw_state.win_y = y;

	glConfig.width = width;
	glConfig.height = height;
	glConfig.borderless = options.borderless;

	GLimp_SetFullscreenMode( displayFrequency, fullscreen );

	const int varValue = v_stencilBits.get();
	if( varValue == 8 || varValue == 16 ) {
		glConfig.stencilBits = varValue;
	} else {
		glConfig.stencilBits = 0;
	}

	GLimp_CreateWindow( false );

	// init all the gl stuff for the window
	if( !GLimp_InitGL( false ) ) {
		Com_Printf( "GLimp_CreateWindow() - GLimp_InitGL failed\n" );
		return rserr_unknown;
	}

	return ( fullscreen == glConfig.fullScreen ) ? rserr_ok : rserr_invalid_fullscreen;
}

void GLimp_Shutdown( void ) {
	if( qwglMakeCurrent && !qwglMakeCurrent( NULL, NULL ) ) {
		Com_Printf( "ref::R_Shutdown() - wglMakeCurrent failed\n" );
	}
	if( glw_state.hGLRC ) {
		if( qwglDeleteContext && !qwglDeleteContext( glw_state.hGLRC ) ) {
			Com_Printf( "ref::R_Shutdown() - wglDeleteContext failed\n" );
		}
		glw_state.hGLRC = NULL;
	}
	if( glw_state.hDC ) {
		if( !ReleaseDC( glw_state.hWnd, glw_state.hDC ) ) {
			Com_Printf( "ref::R_Shutdown() - ReleaseDC failed\n" );
		}
		glw_state.hDC   = NULL;
	}
	if( glw_state.hWnd ) {
		ShowWindow( glw_state.hWnd, SW_HIDE );
		DestroyWindow( glw_state.hWnd );
		glw_state.hWnd = NULL;
	}

#ifdef WITH_UTF8
	UnregisterClassW( glw_state.windowClassNameW, glw_state.hInstance );
#else
	UnregisterClass( glw_state.windowClassName, glw_state.hInstance );
#endif

	if( glConfig.fullScreen ) {
		ChangeDisplaySettings( 0, 0 );
		glConfig.fullScreen = false;
	}

	if( glw_state.applicationName ) {
		free( glw_state.applicationName );
		glw_state.applicationName = NULL;
	}

	if( glw_state.applicationNameW ) {
		free( glw_state.applicationNameW );
		glw_state.applicationNameW = NULL;
	}

	glw_state.applicationIconResourceID = 0;

	glw_state.win_x = 0;
	glw_state.win_y = 0;

	glConfig.width = 0;
	glConfig.height = 0;
}

#define GET_WGL_EXTENSION( name ) \
	do { \
		if( !( q ## name ) ) { \
			q ## name = ( decltype( q ## name ) )qwglGetProcAddress( #name ); \
			if( !( q ## name ) ) { \
				Com_DPrintf( "Missing a WGL extension %s\n", #name ); \
			} \
		} \
	} while( 0 )

static int GLimp_Init_( const char *applicationName, void *hinstance, void *wndproc, void *parenthWnd,
						int iconResource, const int *iconXPM, bool needPixelFormatARB ) {
	size_t applicationNameSize = strlen( applicationName ) + 1;
    // save off hInstance and wndproc
	glw_state.applicationName = (char *)malloc( applicationNameSize );
	memcpy( glw_state.applicationName, applicationName, applicationNameSize );
#ifdef WITH_UTF8
	glw_state.applicationNameW = (WCHAR *)malloc( applicationNameSize * sizeof( WCHAR ) ); // may be larger than needed, but not smaller
	MultiByteToWideChar( CP_UTF8, 0, applicationName, -1, glw_state.applicationNameW, applicationNameSize * sizeof( WCHAR ) );
	glw_state.applicationNameW[applicationNameSize - 1] = 0;
#endif
	glw_state.hInstance = ( HINSTANCE ) hinstance;
	glw_state.wndproc = (decltype( glw_state.wndproc ))wndproc;
	glw_state.parenthWnd = ( HWND )parenthWnd;
	glw_state.applicationIconResourceID = iconResource;

    // create a temporary window and startup temporary OpenGL context
    // to get the function pointer to wglChoosePixelFormatARB
	if( needPixelFormatARB ) {
		GLimp_CreateWindow( true );

		if( !GLimp_InitGL( true ) ) {
			return false;
		}

		GET_WGL_EXTENSION( wglGetExtensionsStringEXT );
		GET_WGL_EXTENSION( wglGetDeviceGammaRamp3DFX );
		GET_WGL_EXTENSION( wglSetDeviceGammaRamp3DFX );
		GET_WGL_EXTENSION( wglSwapIntervalEXT );

		GET_WGL_EXTENSION( wglGetPixelFormatAttribivARB );
		GET_WGL_EXTENSION( wglGetPixelFormatAttribfvARB );
		GET_WGL_EXTENSION( wglChoosePixelFormatARB );
		GET_WGL_EXTENSION( wglCreateContextAttribsARB );

		GLimp_Shutdown();
	}

	return true;
}

#undef GET_WGL_EXTENSION

bool GLimp_Init( const char *applicationName, void *hinstance, void *wndproc, void *parenthWnd,
				 int iconResource, const int *iconXPM ) {
	if( !GLimp_Init_( applicationName, hinstance, wndproc, parenthWnd, iconResource, iconXPM, true ) ) {
		return false;
	}
	return GLimp_Init_( applicationName, hinstance, wndproc, parenthWnd, iconResource, iconXPM, false );
}

static bool GLimp_InitGL( bool isInitialDummyContext ) {
	PIXELFORMATDESCRIPTOR pfd =
	{
		sizeof( PIXELFORMATDESCRIPTOR ), // size of this pfd
		1,                      // version number
		PFD_DRAW_TO_WINDOW |    // support window
		PFD_SUPPORT_OPENGL |    // support OpenGL
		PFD_DOUBLEBUFFER,       // double buffered
		PFD_TYPE_RGBA,          // RGBA type
		32,                     // 32-bit color depth
		0, 0, 0, 0, 0, 0,       // color bits ignored
		0,                      // no alpha buffer
		0,                      // shift bit ignored
		0,                      // no accumulation buffer
		0, 0, 0, 0,             // accum bits ignored
		24,                     // 32-bit z-buffer
		0,                      // no stencil buffer
		0,                      // no auxiliary buffer
		PFD_MAIN_PLANE,         // main layer
		0,                      // reserved
		0, 0, 0                 // layer masks ignored
	};
	int iAttributes[] = {
		WGL_STENCIL_BITS_ARB, 0,
		WGL_SAMPLE_BUFFERS_ARB, GL_FALSE,
		WGL_SAMPLES_ARB, 0,
		WGL_STEREO_ARB, GL_FALSE,
		WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
		WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
		WGL_DOUBLE_BUFFER_ARB, GL_TRUE,
		WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
		WGL_COLOR_BITS_ARB, 32,
		WGL_RED_BITS_ARB, 8,
		WGL_GREEN_BITS_ARB, 8,
		WGL_BLUE_BITS_ARB, 8,
		WGL_ALPHA_BITS_ARB, 8,
		WGL_DEPTH_BITS_ARB, 24,
		0
	};
	int pixelformat = 0;

	if( glConfig.stencilBits ) {
		Com_DPrintf( "...attempting to set stencil bits\n" );
		pfd.cStencilBits = glConfig.stencilBits;
		iAttributes[0 * 2 + 1] = glConfig.stencilBits; // WGL_STENCIL_BITS_ARB
	}

	/*
	** Get a DC for the specified window
	*/
	if( glw_state.hDC != NULL ) {
		Com_Printf( "GLimp_Init() - non-NULL DC exists\n" );
	}

	if( ( glw_state.hDC = GetDC( glw_state.hWnd ) ) == NULL ) {
		Com_Printf( "GLimp_Init() - GetDC failed\n" );
		return false;
	}

	if( !isInitialDummyContext ) {
		UINT numFormats;
		assert( qwglChoosePixelFormatARB );
		if( qwglChoosePixelFormatARB( glw_state.hDC, iAttributes, NULL, 1, &pixelformat, &numFormats ) == 0 ) {
			Com_Printf( "GLimp_Init() - wglChoosePixelFormatARB failed\n" );
			return false;
		}
	} else {
		if( ( pixelformat = ChoosePixelFormat( glw_state.hDC, &pfd ) ) == 0 ) {
			Com_Printf( "GLimp_Init() - ChoosePixelFormat failed\n" );
			return false;
		}
	}

	if( SetPixelFormat( glw_state.hDC, pixelformat, &pfd ) == FALSE ) {
		Com_Printf( "GLimp_Init() - SetPixelFormat failed\n" );
		return false;
	}
	DescribePixelFormat( glw_state.hDC, pixelformat, sizeof( pfd ), &pfd );

	glConfig.stencilBits = pfd.cStencilBits;

	/*
	** startup the OpenGL subsystem by creating a context and making
	** it current
	*/

	if( isInitialDummyContext ) {
		if( !( glw_state.hGLRC = qwglCreateContext( glw_state.hDC ) ) ) {
			Com_Printf("GLimp_Init() - qwglCreateContext failed\n");
			goto fail;
		}
	} else {
#if 1
		assert( qwglCreateContextAttribsARB );

		int attribs[] = {
			WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
			WGL_CONTEXT_MINOR_VERSION_ARB, 3,
			WGL_CONTEXT_PROFILE_MASK_ARB, WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
			0, 0
		};

		if( !( glw_state.hGLRC = qwglCreateContextAttribsARB( glw_state.hDC, nullptr, attribs ) ) ) {
			Com_Printf( "GLimp_Init() - qwglCreateContextAttribsARB failed\n" );
			goto fail;
		}
#else
		if( !( glw_state.hGLRC = qwglCreateContext( glw_state.hDC ) ) ) {
			Com_Printf( "GLimp_Init() - qwglCreateContextAttribsARB failed\n" );
			goto fail;
		}
#endif
	}

	if( !qwglMakeCurrent( glw_state.hDC, glw_state.hGLRC ) ) {
		Com_Printf("GLimp_Init() - qwglMakeCurrent failed\n");
		goto fail;
	}

	/*
	** print out PFD specifics
	*/
	Com_DPrintf( "GL PFD: color(%d-bits) Z(%d-bit) stencil(%d-bits)\n", ( int ) pfd.cColorBits, ( int ) pfd.cDepthBits, ( int )pfd.cStencilBits );

	return true;

fail:
	if( glw_state.hGLRC ) {
		qwglDeleteContext( glw_state.hGLRC );
		glw_state.hGLRC = NULL;
	}

	if( glw_state.hDC ) {
		ReleaseDC( glw_state.hWnd, glw_state.hDC );
		glw_state.hDC = NULL;
	}
	return false;
}

bool GLimp_GetGammaRamp( size_t stride, unsigned short *psize, unsigned short *ramp ) {
	unsigned short ramp256[3 * 256];

	if( stride < 256 ) {
        // only supports gamma ramps with 256 mappings per channel
		return false;
	}

	if( qwglGetDeviceGammaRamp3DFX ) {
		if( qwglGetDeviceGammaRamp3DFX( glw_state.hDC, ramp256 ) ) {
			*psize = 256;
			memcpy( ramp,          ramp256,       256 * sizeof( *ramp ) );
			memcpy( ramp +  stride, ramp256 +  256, 256 * sizeof( *ramp ) );
			memcpy( ramp + 2 * stride, ramp256 + 2 * 256, 256 * sizeof( *ramp ) );
			return true;
		}
	}

	if( GetDeviceGammaRamp( glw_state.hDC, ramp256 ) ) {
		*psize = 256;
		memcpy( ramp,          ramp256,       256 * sizeof( *ramp ) );
		memcpy( ramp +  stride, ramp256 +  256, 256 * sizeof( *ramp ) );
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

	memcpy( ramp256,       ramp, size * sizeof( *ramp ) );
	memcpy( ramp256 +  256, ramp +  stride, size * sizeof( *ramp ) );
	memcpy( ramp256 + 2 * 256, ramp + 2 * stride, size * sizeof( *ramp ) );

	if( qwglGetDeviceGammaRamp3DFX ) {
		qwglSetDeviceGammaRamp3DFX( glw_state.hDC, ramp256 );
	} else {
		SetDeviceGammaRamp( glw_state.hDC, ramp256 );
	}
}

void GLimp_BeginFrame( void ) {

}

void GLimp_EndFrame( void ) {
	if( !qwglSwapBuffers( glw_state.hDC ) ) {
		Sys_Error( "GLimp_EndFrame() - SwapBuffers() failed!" );
	}
}

void GLimp_AppActivate( bool active, bool minimize, bool destroy ) {
	if( active ) {
		Cvar_Set( "gl_drawbuffer", "GL_BACK" );
	} else {
		if( glConfig.fullScreen ) {
			Cvar_Set( "gl_drawbuffer", "GL_NONE" );
		} else {
			if( destroy ) {
				Cvar_Set( "gl_drawbuffer", "GL_NONE" );
			}
		}
	}
}

void GLimp_SetSwapInterval( int swapInterval ) {
	if( qwglSwapIntervalEXT ) {
		qwglSwapIntervalEXT( swapInterval );
	}
}

bool GLimp_MakeCurrent( void *context, void *surface ) {
	if( qwglMakeCurrent && !qwglMakeCurrent( glw_state.hDC, (HGLRC)context ) ) {
		return false;
	}
	return true;
}

QVariant VID_GetMainContextHandle() {
	return QVariant::fromValue( QWGLNativeContext( glw_state.hGLRC, glw_state.hWnd ) );
}

bool GLimp_BeginUIRenderingHacks() {
	return true;
}

bool GLimp_EndUIRenderingHacks() {
	return GLimp_MakeCurrent( glw_state.hGLRC, nullptr );
}