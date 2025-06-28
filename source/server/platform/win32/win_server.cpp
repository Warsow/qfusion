#include <windows.h>
#include "conproc.h"
#include <server/server.h>
#include <common/local.h>
#include <common/facilities/syspublic.h>

void Sys_InitTime();

/*
* Sys_Init
*/
void Sys_Init( int argc, char **argv ) {
	timeBeginPeriod( 1 );

	Sys_InitTime();

	SetPriorityClass( GetCurrentProcess(), HIGH_PRIORITY_CLASS );

	if( !AllocConsole() ) {
		Sys_Error( "Couldn't create a system console" );
	}

	// let QHOST hook in
	InitConProc( argc, argv );
}


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

	FreeConsole();

	// shut down QHOST hooks if necessary
	DeinitConProc();

	Qcommon_Shutdown();

	exit( 0 );
}

BOOL myTranslateMessage( MSG *msg ) {
	return TranslateMessage( msg );
}
