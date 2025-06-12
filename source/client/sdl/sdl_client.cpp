/*
Copyright (C) 2015 SiPlus, Chasseur de bots

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
#include <client/client.h>
#include <common/local.h>
#include <common/version.h>
#include <common/facilities/sysclock.h>

#if defined( _WIN32 )
#include "../win32/resource.h"
#endif

#if defined( __APPLE__ )
#include <CoreFoundation/CoreFoundation.h>
#include <sys/param.h>
#endif

int64_t sys_frame_time;

void Sys_InitTime( void ) {}

void CL_Sys_Init() {}
void CL_Sys_Shutdown() {}

void Sys_Sleep( unsigned int millis ) {
	SDL_Delay( millis );
}

void Sys_Error( const char *format, ... ) {
	va_list argptr;
	char msg[1024];

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, APPLICATION, msg, NULL );

	exit( 1 );
}

/*
* Sys_Init
*/
void Sys_Init( int, char ** ) {
	Sys_InitTime();
}

/*
* Sys_InitDynvars
*/
void Sys_InitDynvars( void ) {
}

/*
* Sys_Quit
*/
void Sys_Quit( void ) {
	Qcommon_Shutdown();

	exit( 0 );
}

/*
* Sys_AppActivate
*/
void Sys_AppActivate( void ) {
}

/*
* Sys_SendKeyEvents
*/
void Sys_SendKeyEvents( void ) {
	// grab frame time
	sys_frame_time = Sys_Milliseconds();
}

/*****************************************************************************/

int main( int argc, char **argv ) {
	int64_t oldtime, newtime;

#if defined( __APPLE__ ) && !defined( DEDICATED_ONLY )
	char resourcesPath[MAXPATHLEN];
	CFURLGetFileSystemRepresentation( CFBundleCopyResourcesDirectoryURL( CFBundleGetMainBundle() ), 1, (UInt8 *)resourcesPath, MAXPATHLEN );
	chdir( resourcesPath );
#endif

#if defined( __WIN32__ )
	#if defined( _DEBUG )
	SDL_SetHint( SDL_HINT_ALLOW_TOPMOST, "0" );
#endif
#endif

	SDL_Init( SDL_INIT_VIDEO );

	Qcommon_Init( argc, argv );

	unsigned gameMsec = 0;
	float extraTime = 0.0f;

	oldtime = Sys_Milliseconds();
	while( true ) {
		int time;
		// find time spent rendering last frame
		do {
			newtime = Sys_Milliseconds();
			time = newtime - oldtime;
			if( time > 0 ) {
				break;
			}
			Sys_Sleep( 0 );
		} while( 1 );
		oldtime = newtime;

		Qcommon_Frame( time, &gameMsec, &extraTime );
	}

	SDL_Quit();
}

/*
* Sys_GetClipboardData
*/
char *Sys_GetClipboardData( void ) {
	if( SDL_HasClipboardText() == SDL_TRUE ) {
		return SDL_GetClipboardText();
	}
	return NULL;
}

/*
* Sys_SetClipboardData
*/
bool Sys_SetClipboardData( const char *data ) {
	return SDL_SetClipboardText( data );
}

/*
* Sys_FreeClipboardData
*/
void Sys_FreeClipboardData( char *data ) {
	SDL_free( data );
}