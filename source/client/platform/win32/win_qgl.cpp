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
This code is part of DynGL, a method of dynamically loading an OpenGL
library without much pain designed by Joseph Carter and is based
loosely on previous work done both by Zephaniah E. Hull and Joseph.

Both contributors have decided to disclaim all Copyright to this work.
It is released to the Public Domain WITHOUT ANY WARRANTY whatsoever,
express or implied, in the hopes that others will use it instead of
other less-evolved hacks which usually don't work right.  ;)
*/

/*
The following code is loosely based on DynGL code by Joseph Carter
and Zephaniah E. Hull. Adapted by Victor Luchits for qfusion project.
*/

/*
** QGL_WIN.C
**
** This file implements the operating system binding of GL to QGL function
** pointers.  When doing a port of Qfusion you must implement the following
** two functions:
**
** QGL_Init() - loads libraries, assigns function pointers, etc.
** QGL_Shutdown() - unloads libraries, NULLs function pointers
*/

#define GL_GLEXT_LEGACY

#include <windows.h>
#include <GL/gl.h>
#include "win_glw.h"

#include <common/facilities/messagestreams.h>
#include <common/helpers/q_libc.h>

#define QGL_EXTERN

#define QGL_FUNC( type, name, params ) QGL_FUNC_VAR( type, name, params );
#define QGL_FUNC_OPT( type, name, params ) QGL_FUNC_VAR( type, name, params );
#define QGL_EXT( type, name, params ) QGL_FUNC_VAR( type, name, params );
#define QGL_WGL( type, name, params ) type( APIENTRY * q ## name ) params;
#define QGL_WGL_EXT( type, name, params ) type( APIENTRY * q ## name ) params;
#define QGL_GLX( type, name, params )
#define QGL_GLX_EXT( type, name, params )
#define QGL_EGL( type, name, params )
#define QGL_EGL_EXT( type, name, params )

#include <client/renderer/qgl.h>

#undef QGL_EGL_EXT
#undef QGL_EGL
#undef QGL_GLX_EXT
#undef QGL_GLX
#undef QGL_WGL_EXT
#undef QGL_WGL
#undef QGL_EXT
#undef QGL_FUNC_OPT
#undef QGL_FUNC

/*
** QGL_Shutdown
**
** Unloads the specified DLL then nulls out all the proc pointers.
*/
void QGL_Shutdown( void ) {
	if( glw_state.hinstOpenGL ) {
		FreeLibrary( glw_state.hinstOpenGL );
	}
	glw_state.hinstOpenGL = NULL;

#define QGL_FUNC( type, name, params ) ( q ## name ) = nullptr;
#define QGL_FUNC_OPT( type, name, params ) ( q ## name ) = nullptr;
#define QGL_EXT( type, name, params ) ( q ## name ) = nullptr;
#define QGL_WGL( type, name, params ) ( q ## name ) = nullptr;
#define QGL_WGL_EXT( type, name, params ) ( q ## name ) = nullptr;
#define QGL_GLX( type, name, params )
#define QGL_GLX_EXT( type, name, params )
#define QGL_EGL( type, name, params )
#define QGL_EGL_EXT( type, name, params )

#include <client/renderer/qgl.h>

#undef QGL_EGL_EXT
#undef QGL_EGL
#undef QGL_GLX_EXT
#undef QGL_GLX
#undef QGL_WGL_EXT
#undef QGL_WGL
#undef QGL_EXT
#undef QGL_FUNC_OPT
#undef QGL_FUNC
}

#pragma warning( disable : 4113 4133 4047 )

/*
** QGL_GetDriverInfo
**
** Returns information about the GL DLL.
*/
const qgl_driverinfo_t *QGL_GetDriverInfo( void ) {
	static const qgl_driverinfo_t driver =
	{
		"opengl32.dll"
	};
	return &driver;
}

#define QGL_FUNC( type, name, params ) \
    QGL_ASSIGN_VAR( q ## name, GetProcAddress( glw_state.hinstOpenGL, # name ) );
#define QGL_FUNC_OPT( type, name, params ) \
    QGL_ASSIGN_VAR( q ## name, GetProcAddress( glw_state.hinstOpenGL, # name ) );

#define QGL_EXT( type, name, params )

#define QGL_WGL( type, name, params ) \
	do { \
	    ( q ## name ) = ( decltype( q ## name ) )GetProcAddress( glw_state.hinstOpenGL, # name ); \
	    if( !( q ## name ) ) { \
	        Com_Printf( "QGL_Init: Failed to get address for %s\n", # name ); \
	        return qgl_initerr_invalid_driver; \
	    } \
	} while( 0 )

// WGL extensions are handled in win_glw.cpp at its own
#define QGL_WGL_EXT( type, name, params )
#define QGL_GLX( type, name, params )
#define QGL_GLX_EXT( type, name, params )
#define QGL_EGL( type, name, params )
#define QGL_EGL_EXT( type, name, params )

qgl_initerr_t QGL_Init( const char *dllname ) {
	if( ( glw_state.hinstOpenGL = LoadLibrary( dllname ) ) == 0 ) {
		char *buf = NULL;
		FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(), MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ), (LPTSTR) &buf, 0, NULL );
		if( buf ) {
			Com_Printf( "%s\n", buf );
			MessageBox( NULL, va( "QGL_Init: Failed to load %s: %s\n", dllname, buf ), "Error", 0 /* MB_OK */ );
			LocalFree( buf );
		} else {
			MessageBox( NULL, va( "QGL_Init: Failed to load %s\n", dllname ), "Error", 0 /* MB_OK */ );
		}
		return qgl_initerr_invalid_driver;
	}

#include <client/renderer/qgl.h>
	return qgl_initerr_ok;
}

#undef QGL_EGL_EXT
#undef QGL_EGL
#undef QGL_GLX_EXT
#undef QGL_GLX
#undef QGL_WGL_EXT
#undef QGL_WGL
#undef QGL_EXT
#undef QGL_FUNC_OPT
#undef QGL_FUNC

// Stage 2

#define QGL_FUNC( type, name, params ) \
    do { \
        if( !( q ## name ) ) { \
            QGL_ASSIGN_VAR( q ## name, qwglGetProcAddress( # name ) ); \
            if( !( q ## name ) ) { \
                Com_Printf( "QGL_Init: Failed to get address for %s using qwglGetProcAddress()\n", #name ); \
                return qgl_initerr_invalid_driver; \
            } \
        } \
    } while( 0 );

#define QGL_FUNC_OPT( type, name, params ) \
    do { \
        if( !( q ## name ) ) { \
            QGL_ASSIGN_VAR( q ## name, qwglGetProcAddress( # name ) ); \
        } \
    } while( 0 );

#define QGL_EXT( type, name, params )
#define QGL_WGL( type, name, params )
#define QGL_WGL_EXT( type, name, params )
#define QGL_GLX( type, name, params )
#define QGL_GLX_EXT( type, name, params )
#define QGL_EGL( type, name, params )
#define QGL_EGL_EXT( type, name, params )

qgl_initerr_t QGL_PostInit() {
#include <client/renderer/qgl.h>
    return qgl_initerr_ok;
}

#undef QGL_EGL_EXT
#undef QGL_EGL
#undef QGL_GLX_EXT
#undef QGL_GLX
#undef QGL_WGL_EXT
#undef QGL_WGL
#undef QGL_EXT
#undef QGL_FUNC_OPT
#undef QGL_FUNC
