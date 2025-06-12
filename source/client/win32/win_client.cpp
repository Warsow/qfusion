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

#include <client/client.h>
#include <common/local.h>
#include <server/server.h>
#include "winquake.h"

extern "C" QF_DLL_EXPORT DWORD NvOptimusEnablement = 0x00000001;
extern "C" QF_DLL_EXPORT int AmdPowerXpressRequestHighPerformance = 1;

void Sys_InitTime();

void Sys_Init( int, char ** ) {
	timeBeginPeriod( 1 );

	Sys_InitTime();
}

void Sys_AppActivate( void ) {
	ShowWindow( cl_hwnd, SW_RESTORE );
	SetForegroundWindow( cl_hwnd );
}

int IN_MapKey( int key );
BOOL myTranslateMessage( MSG *msg ) {
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

void Sys_Error( const char *format, ... ) {
	va_list argptr;
	char msg[1024];

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	MessageBox( NULL, msg, "Error", 0 /* MB_OK */ );

	exit( 1 );
}

void Sys_Quit( void ) {
	timeEndPeriod( 1 );

	SV_Shutdown( "Server quit\n" );
	CL_Shutdown();

	Qcommon_Shutdown();

	exit( 0 );
}

void VID_SetProcessDPIAware( void );

void CL_Sys_Init( void ) {
	VID_SetProcessDPIAware();
}

void CL_Sys_Shutdown( void ) {
}

char **Sys_GetEnvironmentVariables() {
	return environ;
}

void Sys_DeleteEnvironmentVariable( const char *name ) {
	(void)_putenv_s( name, "" );
	assert( !getenv( name ) );
}
