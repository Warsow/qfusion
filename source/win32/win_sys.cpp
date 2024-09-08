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

#include "../common/common.h"
#include "../common/cmdargs.h"
#include "../client/keys.h"

#include "winquake.h"
#include "resource.h"
#include <errno.h>
#include <float.h>
#include <fcntl.h>
#include <stdio.h>
#include <io.h>
#include <conio.h>
#include <limits.h>

#include "../win32/conproc.h"

#if !defined( DEDICATED_ONLY )
extern "C" QF_DLL_EXPORT DWORD NvOptimusEnablement = 0x00000001;
extern "C" QF_DLL_EXPORT int AmdPowerXpressRequestHighPerformance = 1;
#endif

#if !defined( USE_SDL2 ) || defined( DEDICATED_ONLY )

int starttime;
int ActiveApp;
int Minimized;
int AppFocused;

int64_t sys_msg_time;

#define MAX_NUM_ARGVS   128
int argc;
char *argv[MAX_NUM_ARGVS];

void Sys_InitTime( void );

/*
===============================================================================

SYSTEM IO

===============================================================================
*/

void Sys_Error( const char *format, ... ) {
	va_list argptr;
	char msg[1024];

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	MessageBox( NULL, msg, "Error", 0 /* MB_OK */ );

	// shut down QHOST hooks if necessary
	DeinitConProc();

	exit( 1 );
}

void Sys_Quit( void ) {
	timeEndPeriod( 1 );

	SV_Shutdown( "Server quit\n" );
#ifndef DEDICATED_ONLY
	CL_Shutdown();
#endif

	if( ( dedicated && dedicated->integer ) || ( developer && developer->integer ) ) {
		FreeConsole();
	}

	// shut down QHOST hooks if necessary
	DeinitConProc();

	Qcommon_Shutdown();

	exit( 0 );
}

//================================================================

void Sys_Sleep( unsigned int millis ) {
	Sleep( millis );
}

//===============================================================================

/*
* Sys_Init
*/
void Sys_Init( void ) {
	timeBeginPeriod( 1 );

	Sys_InitTime();

	if( dedicated->integer || developer->integer ) {
		SetPriorityClass( GetCurrentProcess(), HIGH_PRIORITY_CLASS );

		if( !AllocConsole() ) {
			Sys_Error( "Couldn't create a system console" );
		}

		// let QHOST hook in
		InitConProc( argc, argv );
	}
}

/*
* myTranslateMessage
* A wrapper around TranslateMessage to avoid garbage if the toggleconsole
* key happens to be a dead key (like in the German layout)
*/
#ifdef DEDICATED_ONLY
#define myTranslateMessage( msg ) TranslateMessage( msg )
#else
int IN_MapKey( int key );
static BOOL myTranslateMessage( MSG *msg ) {
	if( msg->message == WM_KEYDOWN ) {
		int key = IN_MapKey( msg->lParam );
		if( auto *system = wsw::cl::KeyHandlingSystem::instance(); system && system->isAToggleConsoleKey( key ) ) {
			return TRUE;
		} else {
			return TranslateMessage( msg );
		}
	}
	return TranslateMessage( msg );
}
#endif

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

/*
* Sys_GetPreferredLanguage
* Get the preferred language through the MUI API. Works on Vista and newer.
*/
const char *Sys_GetPreferredLanguage( void ) {
	typedef BOOL ( WINAPI * GetUserPreferredUILanguages_t )( DWORD, PULONG, PWSTR, PULONG );
	BOOL hr;
	ULONG numLanguages = 0;
	DWORD cchLanguagesBuffer = 0;
	HINSTANCE kernel32Dll;
	GetUserPreferredUILanguages_t GetUserPreferredUILanguages_f;
	static char lang[10];

// mingw doesn't define this
#ifndef MUI_LANGUAGE_NAME
# define MUI_LANGUAGE_NAME 0x8
#endif

	lang[0] = '\0';

	kernel32Dll = LoadLibrary( "kernel32.dll" );

	hr = FALSE;
	GetUserPreferredUILanguages_f = ( decltype( GetUserPreferredUILanguages_f ) )(void *)GetProcAddress( kernel32Dll, "GetUserPreferredUILanguages" );
	if( GetUserPreferredUILanguages_f ) {
		hr = GetUserPreferredUILanguages_f( MUI_LANGUAGE_NAME, &numLanguages, NULL, &cchLanguagesBuffer );
	}

	if( hr ) {
		WCHAR *pwszLanguagesBuffer;

		pwszLanguagesBuffer = (WCHAR *)Q_malloc( sizeof( WCHAR ) * cchLanguagesBuffer );
		hr = GetUserPreferredUILanguages_f( MUI_LANGUAGE_NAME, &numLanguages, pwszLanguagesBuffer, &cchLanguagesBuffer );

		if( hr ) {
			char *p;

			WideCharToMultiByte( CP_ACP, 0, pwszLanguagesBuffer, cchLanguagesBuffer, lang, sizeof( lang ), NULL, NULL );
			lang[sizeof( lang ) - 1] = '\0';

			p = strchr( lang, '-' );
			if( p ) {
				*p = '_';
			}
		}

		Q_free( pwszLanguagesBuffer );
	}

	FreeLibrary( kernel32Dll );

	if( !lang[0] ) {
		return APP_DEFAULT_LANGUAGE;
	}
	return Q_strlwr( lang );
}

#if !defined( USE_SDL2 ) || defined( DEDICATED_ONLY )

/*
* Sys_AcquireWakeLock
*/
void *Sys_AcquireWakeLock( void ) {
	return NULL;
}

/*
* Sys_ReleaseWakeLock
*/
void Sys_ReleaseWakeLock( void *wl ) {
}

/*
* Sys_AppActivate
*/
void Sys_AppActivate( void ) {
#ifndef DEDICATED_ONLY
	ShowWindow( cl_hwnd, SW_RESTORE );
	SetForegroundWindow( cl_hwnd );
#endif
}

//========================================================================

static char exe[4] = "exe";

/*
* ParseCommandLine
*/
static void ParseCommandLine( LPSTR lpCmdLine ) {
	argc = 1;
	argv[0] = exe;

	while( *lpCmdLine && ( argc < MAX_NUM_ARGVS ) ) {
		while( *lpCmdLine && ( *lpCmdLine <= 32 || *lpCmdLine > 126 ) )
			lpCmdLine++;

		if( *lpCmdLine ) {
			char quote = ( ( '"' == *lpCmdLine || '\'' == *lpCmdLine ) ? *lpCmdLine++ : 0 );

			argv[argc++] = lpCmdLine;
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

	Qcommon_Init( argc, argv );

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
