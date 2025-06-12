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
// sys_win.h

#include <common/facilities/cmdargs.h>
#include <common/facilities/syspublic.h>
#include <common/facilities/cvar.h>
#include <common/common.h>
#include <common/local.h>
#include <client/keys.h>

#include <windows.h>

#include <errno.h>
#include <float.h>
#include <fcntl.h>
#include <stdio.h>
#include <io.h>
#include <conio.h>
#include <limits.h>

#include <server/win32/conproc.h>

#if !defined( USE_SDL2 ) || defined( DEDICATED_ONLY )

int starttime;
int ActiveApp;
int Minimized;
int AppFocused;

int64_t sys_msg_time;

#define MAX_NUM_ARGVS   128
static int parsedArgc;
static char *parsedArgv[MAX_NUM_ARGVS];

void Sys_InitTime( void );

static LARGE_INTEGER hwTimerFrequency;
static LARGE_INTEGER startupTimestamp;

void Sys_InitTime() {
	(void)::QueryPerformanceFrequency( &hwTimerFrequency );
	(void)::QueryPerformanceCounter( &startupTimestamp );
}

int64_t Sys_Milliseconds() {
	LARGE_INTEGER counter;
	(void)::QueryPerformanceCounter( &counter );
	// Isn't really needed as the return value is 64-bit but should make stuff more robust
	counter.QuadPart -= startupTimestamp.QuadPart;
	counter.QuadPart *= 1000LL;
	counter.QuadPart /= hwTimerFrequency.QuadPart;
	return counter.QuadPart;
}

uint64_t Sys_Microseconds() {
	LARGE_INTEGER counter;
	(void)::QueryPerformanceCounter( &counter );
	counter.QuadPart -= startupTimestamp.QuadPart;
	counter.QuadPart *= 1000LL * 1000LL;
	counter.QuadPart /= hwTimerFrequency.QuadPart;
	return (uint64_t)counter.QuadPart;
}

void Sys_Sleep( unsigned int millis ) {
	Sleep( millis );
}

//===============================================================================



/*
* myTranslateMessage
* A wrapper around TranslateMessage to avoid garbage if the toggleconsole
* key happens to be a dead key (like in the German layout)
*/
BOOL myTranslateMessage( MSG *msg );

/*
* Sys_SendKeyEvents
*
* Send Key_Event calls
*/
void Sys_SendKeyEvents( void ) {
	MSG msg;

	while( PeekMessageW( &msg, NULL, 0, 0, PM_NOREMOVE ) ) {
		if( !GetMessageW( &msg, NULL, 0, 0 ) ) {
			Sys_Quit();
		}
		sys_msg_time = msg.time;
		myTranslateMessage( &msg );
		DispatchMessageW( &msg );
	}
}

#endif // !defined(USE_SDL2) || defined(DEDICATED_ONLY)

/*
* Sys_IsBrowserAvailable
*/
bool Sys_IsBrowserAvailable( void ) {
	return true;
}

/*
* Sys_OpenURLInBrowser
*/
void Sys_OpenURLInBrowser( const char *url ) {
	ShellExecute( NULL, "open", url, NULL, NULL, SW_SHOWNORMAL );
}

/*
* Sys_GetCurrentProcessId
*/
int Sys_GetCurrentProcessId( void ) {
	return GetCurrentProcessId();
}

#if !defined( USE_SDL2 ) || defined( DEDICATED_ONLY )

//========================================================================

static char exe[4] = "exe";

/*
* ParseCommandLine
*/
static void ParseCommandLine( LPSTR lpCmdLine ) {
	parsedArgc = 1;
	parsedArgv[0] = exe;

	while( *lpCmdLine && ( parsedArgc < MAX_NUM_ARGVS ) ) {
		while( *lpCmdLine && ( *lpCmdLine <= 32 || *lpCmdLine > 126 ) )
			lpCmdLine++;

		if( *lpCmdLine ) {
			char quote = ( ( '"' == *lpCmdLine || '\'' == *lpCmdLine ) ? *lpCmdLine++ : 0 );

			parsedArgv[parsedArgc++] = lpCmdLine;
			if( quote ) {
				while( *lpCmdLine && *lpCmdLine != quote && *lpCmdLine >= 32 && *lpCmdLine <= 126 )
					lpCmdLine++;
			} else {
				while( *lpCmdLine && *lpCmdLine > 32 && *lpCmdLine <= 126 )
					lpCmdLine++;
			}

			if( *lpCmdLine ) {
				*lpCmdLine++ = 0;
			}
		}
	}
}

/*
* WinMain
*/
HINSTANCE global_hInstance;
int WINAPI WinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow ) {
	MSG msg;
	int64_t oldtime, newtime, time;

	/* previous instances do not exist in Win32 */
	if( hPrevInstance ) {
		return 0;
	}

	global_hInstance = hInstance;

	ParseCommandLine( lpCmdLine );

	Qcommon_Init( parsedArgc, parsedArgv );

	oldtime = Sys_Milliseconds();

	unsigned gameMsec = 0;
	float extraTime   = 0.0f;

	/* main window message loop */
	while( 1 ) {
		// if at a full screen console, don't update unless needed
		if( Minimized || ( dedicated && dedicated->integer ) ) {
			Sleep( 1 );
		}

		while( PeekMessageW( &msg, NULL, 0, 0, PM_NOREMOVE ) ) {
			if( !GetMessageW( &msg, NULL, 0, 0 ) ) {
				Com_Quit( {} );
			}
			sys_msg_time = msg.time;
			myTranslateMessage( &msg );
			DispatchMessageW( &msg );
		}

		do {
			newtime = Sys_Milliseconds();
			time = newtime - oldtime; // no warp problem as unsigned
			if( time > 0 ) {
				break;
			}
			Sys_Sleep( 0 );
		} while( 1 );
		//Com_Printf ("time:%5.2u - %5.2u = %5.2u\n", newtime, oldtime, time);
		oldtime = newtime;

		// do as q3 (use the default floating point precision)
		//	_controlfp( ~( _EM_ZERODIVIDE /*| _EM_INVALID*/ ), _MCW_EM );
		//_controlfp( _PC_24, _MCW_PC );
		Qcommon_Frame( time, &gameMsec, &extraTime );
	}

	// never gets here
	return TRUE;
}

#endif // !defined(USE_SDL2) || defined(DEDICATED_ONLY)
