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

/* we need __APPLE__ here because __MACOSX__ is defined in ../game/q_shared.h from ../common/common.h
which defines HAVE_STRCASECMP if SDL.h isn't called first, causing a bunch of warnings
FIXME:  This will be remidied once a native Mac port is complete
*/
#if defined ( __APPLE__ ) && !defined ( DEDICATED_ONLY )
#include <SDL.h>
#include <CoreFoundation/CoreFoundation.h>
#include <sys/param.h>
#endif

#include <signal.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <dlfcn.h>

#if defined ( __FreeBSD__ )
#include <machine/param.h>
#endif

#include <common/helpers/q_libc.h>
#include <common/facilities/fscompat.h>

// TODO... This does not make sense for dedicated server
bool Sys_IsBrowserAvailable( void ) {
	return true;
}

void Sys_OpenURLInBrowser( const char *url ) {
	int r;

	r = system( va( "xdg-open \"%s\"", url ) );
	if( r == 0 ) {
		// FIXME: XIconifyWindow does minimize the window, however
		// it seems that FocusIn even which follows grabs the input afterwards
		// XIconifyWindow( x11display.dpy, x11display.win, x11display.scr );
	}
}

int Sys_GetCurrentProcessId( void ) {
	return getpid();
}

char **Sys_GetEnvironmentVariables() {
	return environ;
}

void Sys_DeleteEnvironmentVariable( const char *name ) {
	unsetenv( name );
}

bool Sys_Library_Close( void *lib ) {
	return !dlclose( lib );
}

const char *Sys_Library_GetFullName( const char *name ) {
	return FS_AbsoluteNameForBaseFile( name );
}

void *Sys_Library_Open( const char *name ) {
	return dlopen( name, RTLD_NOW );
}

void *Sys_Library_ProcAddress( void *lib, const char *apifuncname ) {
	return (void *)dlsym( lib, apifuncname );
}

const char *Sys_Library_ErrorString( void ) {
	return dlerror();
}

static unsigned long sys_secbase;
uint64_t Sys_Microseconds( void ) {
	struct timeval tp;

	gettimeofday( &tp, NULL );

	if( !sys_secbase ) {
		sys_secbase = tp.tv_sec;
		return tp.tv_usec;
	}

	// TODO handle the wrap
	return (uint64_t)( tp.tv_sec - sys_secbase ) * 1000000 + tp.tv_usec;
}

int64_t Sys_Milliseconds( void ) {
	return Sys_Microseconds() / 1000;
}