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

#include <signal.h>
#include <fcntl.h>

#include <common/local.h>
#include <common/syslocal.h>
#include <common/facilities/syspublic.h>
#include <common/facilities/sysclock.h>
#include <common/helpers/q_libc.h>

void Sys_Quit( void ) {
	fcntl( 0, F_SETFL, fcntl( 0, F_GETFL, 0 ) & ~O_NONBLOCK );

	Qcommon_Shutdown();

	exit( 0 );
}

void Sys_Init( int, char ** ) {
}

void Sys_Error( const char *format, ... ) {
	va_list argptr;
	char string[1024];

	// change stdin to non blocking
	fcntl( 0, F_SETFL, fcntl( 0, F_GETFL, 0 ) & ~O_NONBLOCK );

	va_start( argptr, format );
	Q_vsnprintfz( string, sizeof( string ), format, argptr );
	va_end( argptr );

	fprintf( stderr, "Error: %s\n", string );

	_exit( 1 );
}

static void sigusr_handler( int sig ) {
	if( sig == SIGUSR1 ) {
		Com_DeferConsoleLogReopen();
	}
	return;
}

static void signal_handler( int sig ) {
	static int try_ = 0;

	switch( try_++ ) {
		case 0:
			if( sig == SIGINT || sig == SIGTERM ) {
				printf( "Received signal %d, exiting...\n", sig );
			} else {
				fprintf( stderr, "Received signal %d\n", sig );
			}
			Com_DeferQuit();
			break;
		case 1:
			printf( "Received signal %d, exiting...\n", sig );
			_exit( 1 );
			break;
		default:
			_exit( 2 );
			break;
	}
}

static void catchsig( int sig, void ( *handler )( int ) ) {
	struct sigaction new_action, old_action;
	new_action.sa_handler = handler;
	sigemptyset( &new_action.sa_mask );
	new_action.sa_flags = SA_RESTART;
	sigaction( sig, &new_action, &old_action );
}

static void InitSig( void ) {
	catchsig( SIGHUP, signal_handler );
	catchsig( SIGQUIT, signal_handler );
	catchsig( SIGILL, signal_handler );
	catchsig( SIGTRAP, signal_handler );
	catchsig( SIGIOT, signal_handler );
	catchsig( SIGBUS, signal_handler );
	catchsig( SIGFPE, signal_handler );
	catchsig( SIGSEGV, signal_handler );
	catchsig( SIGTERM, signal_handler );
	catchsig( SIGINT, signal_handler );
	catchsig( SIGPIPE, SIG_IGN );
	catchsig( SIGUSR1, sigusr_handler );
}

/*****************************************************************************/

int main( int argc, char **argv ) {
	unsigned int oldtime, newtime, time;

	InitSig();

	Qcommon_Init( argc, argv );

	fcntl( 0, F_SETFL, fcntl( 0, F_GETFL, 0 ) | O_NONBLOCK );

	unsigned gameMsec = 0;
	float extraTime = 0.0f;

	oldtime = Sys_Milliseconds();
	while( true ) {
		// find time spent rendering last frame
		do {
			newtime = Sys_Milliseconds();
			time = newtime - oldtime;
			if( time > 0 ) {
				break;
			}
#ifdef PUTCPU2SLEEP
			Sys_Sleep( 0 );
#endif
		} while( 1 );
		oldtime = newtime;

		Qcommon_Frame( time, &gameMsec, &extraTime );
	}
}

