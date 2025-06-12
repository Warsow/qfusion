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
// common.c -- misc functions used in client and server
#include "common/version.h"
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/configvars.h>
#include <common/facilities/cmdsystem.h>
#include <common/helpers/pipeutils.h>
#include "local.h"
#include <common/facilities/profiler.h>
#include <common/facilities/wswcurl.h>
#include <common/facilities/syspublic.h>
#include <common/facilities/sysclock.h>
#include <common/helpers/compression.h>
#include <common/helpers/textstreamwriterextras.h>
#include <server/server.h>
#ifndef DEDICATED_ONLY
#include <client/client.h>
#else
#include <common/facilities/cmodel.h>
#include <common/facilities/fscompat.h>
#endif

#include <atomic>
#include <clocale>
#include <setjmp.h>

using wsw::operator""_asView;

static char com_errormsg[MAX_PRINTMSG];

static bool com_quit;

static jmp_buf abortframe;     // an ERR_DROP occured, exit the entire frame

cvar_t *developer;
cvar_t *timescale;
cvar_t *dedicated;
cvar_t *versioncvar;

static UnsignedConfigVar v_fixedTime( "fixedtime"_asView, { .byDefault = 0, .flags = CVAR_CHEAT } );

cvar_t *logconsole;
cvar_t *logconsole_append;
cvar_t *logconsole_flush;
cvar_t *logconsole_timestamp;

qmutex_t *com_print_mutex;

int log_file = 0;

static bool cmd_preinitialized = false;
static bool cmd_initialized = false;

#ifndef DEDICATED_ONLY
void CL_InitCmdSystem();
CmdSystem *CL_GetCmdSystem();
void CL_ShutdownCmdSystem();

void CL_RunCompletionFuncSync( const wsw::StringView &, unsigned, const wsw::StringView &, CompletionQueryFunc );

void Key_Init( void );
void Key_Shutdown( void );
void SCR_EndLoadingPlaque( void );
#endif

void SV_InitCmdSystem();
CmdSystem *SV_GetCmdSystem();
void SV_ShutdownCmdSystem();

static void SV_StopThread();

static void Cmd_PreInit( void );
static void Cmd_Init( void );
static void Cmd_Shutdown( void );

static std::atomic<int> client_state { CA_UNINITIALIZED };
static std::atomic<int> server_state { ss_dead };
static std::atomic<bool> demo_playing { false };

#ifndef DEDICATED_ONLY
// TODO: This is a hack.
// Client state value management is way too convoluted, hence its easier to track by timestamps.
static std::atomic<int64_t> g_lastClientExecutionTimestamp;
#endif

// Another list is defined in the game module
DeclaredConfigVar *DeclaredConfigVar::s_listHead;

void Com_Error( com_error_code_t code, const char *format, ... ) {
	va_list argptr;
	char *msg = com_errormsg;
	const size_t sizeof_msg = sizeof( com_errormsg );
	static bool recursive = false;

	if( recursive ) {
		Com_Printf( "recursive error after: %s", msg ); // wsw : jal : log it
		Sys_Error( "recursive error after: %s", msg );
	}
	recursive = true;

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof_msg, format, argptr );
	va_end( argptr );

	if( code == ERR_DROP ) {
		Com_Printf( "********************\nERROR: %s\n********************\n", msg );
		SV_ShutdownGame( va( "Server crashed: %s\n", msg ), false );
#ifndef DEDICATED_ONLY
		CL_Disconnect( msg );
#endif
		recursive = false;
		longjmp( abortframe, -1 );
	} else {
		Com_Printf( "********************\nERROR: %s\n********************\n", msg );
		SV_StopThread();
		SV_Shutdown( va( "Server fatal crashed: %s\n", msg ) );
#ifndef DEDICATED_ONLY
		CL_Shutdown();
#endif
	}

	if( log_file ) {
		FS_FCloseFile( log_file );
		log_file = 0;
	}

	Sys_Error( "%s", msg );
}

void Com_DeferQuit( void ) {
	com_quit = true;
}

void Com_Quit( const CmdArgs & ) {
	SV_StopThread();
	SV_Shutdown( "Server quit\n" );
#ifndef DEDICATED_ONLY
	CL_Shutdown();
#endif

	Sys_Quit();
}

int Com_ServerState( void ) {
	return server_state;
}

void Com_SetServerState( int state ) {
	server_state = state;
}

int Com_ClientState( void ) {
	return client_state;
}

void Com_SetClientState( int state ) {
	client_state = state;
}

bool Com_DemoPlaying( void ) {
	return demo_playing;
}

void Com_SetDemoPlaying( bool state ) {
	demo_playing = state;
}

#ifndef DEDICATED_ONLY

static qbufPipe_t *g_clCmdPipe;
static qbufPipe_t *g_svCmdPipe;

static qthread_s *g_svThread;

static void redirectCmdExecutionToBuiltinServer( const CmdArgs &cmdArgs ) {
	wsw::StaticString<MAX_STRING_CHARS> text;
	// TODO: Preserve the original string?
	for( const wsw::StringView &arg: cmdArgs.allArgs ) {
		text << arg << ' ';
	}
	text[text.size() - 1] = '\n';

	wsw::PodVector<char> boxedText( text );
	callOverPipe( g_svCmdPipe, &SV_Cmd_ExecuteNow2, boxedText );
}

static void executeCmdCompletionByBuiltinServer( unsigned requestId, const wsw::PodVector<char> &partial, CompletionQueryFunc queryFunc ) {
	// The point is in executing the queryFunc safely in the server thread in a robust fashion
	CompletionResult queryResult = queryFunc( wsw::StringView { partial.data(), partial.size() } );
	callOverPipe( g_clCmdPipe, Con_AcceptCompletionResult, requestId, queryResult );
}

static void redirectCmdCompletionToBuiltinServer( const wsw::StringView &, unsigned requestId,
												  const wsw::StringView &partial, CompletionQueryFunc queryFunc ) {
	wsw::PodVector<char> boxedPartial( partial );
	callOverPipe( g_svCmdPipe, executeCmdCompletionByBuiltinServer, requestId, boxedPartial, queryFunc );
}

static void registerBuiltinServerCmdOnClientSide( const wsw::PodVector<char> &name, CompletionQueryFunc completionFunc ) {
	const wsw::StringView nameView( name.data(), name.size() );
	if( completionFunc ) {
		CL_RegisterCmdWithCompletion( nameView, redirectCmdExecutionToBuiltinServer, completionFunc,
									  redirectCmdCompletionToBuiltinServer );
	} else {
		CL_GetCmdSystem()->registerCommand( nameView, redirectCmdExecutionToBuiltinServer );
	}
}

static void unregisterBuiltinServerCmdOnClientSide( const wsw::PodVector<char> &name ) {
	CL_GetCmdSystem()->unregisterCommand( wsw::StringView { name.data(), name.size() } );
}

// TODO: !!!!! We should merge this thread with the sound background thread

static void *SV_Thread( void * ) {
	[[maybe_unused]] volatile wsw::ThreadProfilingAttachment profilingAttachment( wsw::ProfilingSystem::ServerGroup );

	SV_GetCmdSystem()->markCurrentThreadForFurtherAccessChecks();

	uint64_t oldtime = 0, newtime;

	unsigned gameMsec = 0;
	float extraTime   = 0.0f;
	bool running      = true;

	while( true ) {
		int realMsec;
		do {
			// While waiting for positive time delta or client ready status, keep checking the pipe for ingoing commands
			if( QBufPipe_ReadCmds( g_svCmdPipe ) < 0 ) {
				running = false;
				break;
			}
			// As we have moved the builtin server to a separate thread, it needs its own call for pumping commands.
			// The old shared command buffer processing used to be initiated by the client code every frame.
			// The client code is kept untouched with regard to this, but it does not longer manage server commands.
			// The command buffer of the dedicated server gets executed in Qcommon_Frame().
			SV_GetCmdSystem()->executeBufferCommands();

			newtime = Sys_Milliseconds();
			realMsec = newtime - oldtime;
			bool clientBlocked = false;
			if( realMsec > 0 ) {
				// Note: CA_ACTIVE is not our friend wrt
				if( Com_ClientState() == CA_DISCONNECTED ) {
					break;
				}
				if( const int64_t clTimestamp = g_lastClientExecutionTimestamp; !clTimestamp || newtime - clTimestamp < 100 ) {
					break;
				}
				clientBlocked = true;
			}

			if( Com_ServerState() >= ss_game && !clientBlocked ) {
				Sys_Sleep( 0 );
			} else {
				// TODO: We can just wait on pipe cmds, especially if we process sound too
				Sys_Sleep( 16 );
			}
		} while( true );

		if( running ) {
			oldtime = newtime;
			if( const unsigned fixedTime = v_fixedTime.get(); fixedTime > 0 ) {
				gameMsec = fixedTime;
			} else if( timescale->value >= 0 ) {
				gameMsec = extraTime + (float)realMsec * timescale->value;
				extraTime = ( extraTime + (float)realMsec * timescale->value ) - (float)gameMsec;
			} else {
				gameMsec = realMsec;
			}
			wsw::ProfilingSystem::beginFrame( wsw::ProfilingSystem::ServerGroup, CL_GetProfilerArgsSupplier() );
			SV_Frame( realMsec, gameMsec );
			wsw::ProfilingSystem::endFrame( wsw::ProfilingSystem::ServerGroup, CL_GetProfilerResultSink() );
		} else {
			// TODO: It should've been a break @ label construct
			break;
		}
	}

	// Allow accessing the cmd system from the main thread during shutdown
	SV_GetCmdSystem()->clearThreadForFurtherAccessChecks();
	return nullptr;
}

#endif

void SV_NotifyBuiltinServerOfShutdownGameRequest() {
#ifndef DEDICATED_ONLY
	callOverPipe( g_svCmdPipe, SV_ShutdownGame, nullptr, false );
	QBufPipe_Finish( g_svCmdPipe );
#endif
}

void SV_NotifyClientOfStartedBuiltinServer() {
#ifndef DEDICATED_ONLY
	callOverPipe( g_clCmdPipe, CL_Disconnect, nullptr, true );
	callOverPipe( g_clCmdPipe, SCR_BeginLoadingPlaque );
	QBufPipe_Finish( g_clCmdPipe );
#endif
}

#ifdef DEDICATED_ONLY
void Con_Print( const char * ) {}
#endif

void Qcommon_Init( int argc, char **argv ) {
	(void)std::setlocale( LC_ALL, "C" );

	if( setjmp( abortframe ) ) {
		Sys_Error( "Error during initialization: %s", com_errormsg );
	}

	wsw::PodVector<wsw::StringView> setArgs;
	wsw::PodVector<wsw::StringView> setAndExecArgs;
	wsw::PodVector<std::optional<wsw::StringView>> otherArgs;
	CmdSystem::classifyExecutableCmdArgs( argc, argv, &setArgs, &setAndExecArgs, &otherArgs );

	com_print_mutex = QMutex_Create();

#ifndef DEDICATED_ONLY
	g_svCmdPipe = QBufPipe_Create( 16 * 1024, 1 );
	g_clCmdPipe = QBufPipe_Create( 16 * 1024, 1 );
#endif

	// initialize cmd/cvar tries
	Cmd_PreInit();
	Cvar_PreInit();

	// create basic commands and cvars
	Cmd_Init();
	Cvar_Init();

	wswcurl_init();

#ifndef DEDICATED_ONLY
	Key_Init();
#endif

	// we need to add the early commands twice, because
	// a basepath or cdpath needs to be set before execing
	// config files, but we want other parms to override
	// the settings of the config files

	CmdSystem *primaryCmdSystem;
	CmdSystem *svCmdSystem = SV_GetCmdSystem();
#ifdef DEDICATED_ONLY
	primaryCmdSystem = svCmdSystem;
#else
	CmdSystem *clCmdSystem = CL_GetCmdSystem();
	clCmdSystem->markCurrentThreadForFurtherAccessChecks();
	primaryCmdSystem = clCmdSystem;
#endif

	primaryCmdSystem->appendEarlySetCommands( setArgs );
	primaryCmdSystem->executeBufferCommands();

#ifdef DEDICATED_ONLY
	dedicated = Cvar_Get( "dedicated", "1", CVAR_NOSET );
	Cvar_ForceSet( "dedicated", "1" );
#else
	dedicated = Cvar_Get( "dedicated", "0", CVAR_NOSET );
#endif
	developer = Cvar_Get( "developer", "0", 0 );

	Com_LoadCompressionLibraries();

	FS_Init();

	primaryCmdSystem->appendCommand( wsw::StringView( "exec default.cfg\n" ) );
	primaryCmdSystem->executeBufferCommands();

#ifndef DEDICATED_ONLY
	clCmdSystem->appendCommand( wsw::StringView( "exec config.cfg\n" ) );
	clCmdSystem->appendCommand( wsw::StringView( "exec autoexec.cfg\n" ) );
	clCmdSystem->executeBufferCommands();
#else
	svCmdSystem->appendCommand( wsw::StringView( "exec dedicated_autoexec.cfg\n" ) );
	svCmdSystem->executeBufferCommands();
#endif

	primaryCmdSystem->appendEarlySetAndExecCommands( setAndExecArgs );
	primaryCmdSystem->executeBufferCommands();

#ifdef DEDICATED_ONLY
	svCmdSystem->registerCommand( wsw::StringView( "quit" ), Com_Quit );
#endif

	timescale = Cvar_Get( "timescale", "1.0", CVAR_CHEAT );
	//fixedtime = Cvar_Get( "fixedtime", "0", CVAR_CHEAT );

	if( dedicated->integer ) {
		logconsole = Cvar_Get( "logconsole", "wswconsole.log", CVAR_ARCHIVE );
	} else {
		logconsole = Cvar_Get( "logconsole", "", CVAR_ARCHIVE );
	}

	logconsole_append = Cvar_Get( "logconsole_append", "1", CVAR_ARCHIVE );
	logconsole_flush =  Cvar_Get( "logconsole_flush", "0", CVAR_ARCHIVE );
	logconsole_timestamp =  Cvar_Get( "logconsole_timestamp", "0", CVAR_ARCHIVE );

	Cvar_Get( "gamename", APPLICATION, CVAR_READONLY );
	versioncvar = Cvar_Get( "version", APP_VERSION_STR " " CPUSTRING " " __DATE__ " " BUILDSTRING, CVAR_SERVERINFO | CVAR_READONLY );

	Sys_Init( argc, argv );

	NET_Init();
	Netchan_Init();

	CM_Init();

	SV_Init();
#ifdef DEDICATED_ONLY
	wsw::ProfilingSystem::attachToThisThread( wsw::ProfilingSystem::ServerGroup );
#else
	wsw::ProfilingSystem::attachToThisThread( wsw::ProfilingSystem::ClientGroup );
	CL_Init();
	SCR_EndLoadingPlaque();
#endif

#ifndef DEDICATED_ONLY
	clCmdSystem->appendCommand( wsw::StringView( "exec autoexec_postinit.cfg\n" ) );
	clCmdSystem->executeBufferCommands();
#else
	svCmdSystem->appendCommand( wsw::StringView( "exec dedicated_autoexec_postinit.cfg\n" ) );
	svCmdSystem->executeBufferCommands();
#endif

	if( !otherArgs.empty() ) {
		// add + commands from command line
		primaryCmdSystem->appendLateCommands( otherArgs );
		// the user asked for something explicit
		// so drop the loading plaque
#ifndef DEDICATED_ONLY
		SCR_EndLoadingPlaque();
#endif
	}

#ifndef DEDICATED_ONLY
	g_svThread = QThread_Create( SV_Thread, nullptr );
#endif

	comNotice() << "=====" << APPLICATION << "Initialized =====";

#ifndef DEDICATED_ONLY
	clCmdSystem->executeBufferCommands();
	callMethodOverPipe( g_svCmdPipe, svCmdSystem, &CmdSystem::executeBufferCommands );
	QBufPipe_Finish( g_svCmdPipe );
#else
	svCmdSystem->executeBufferCommands();
#endif
};

void Qcommon_Frame( unsigned realMsec, unsigned *gameMsec, float *extraTime ) {
#ifndef DEDICATED_ONLY
	g_lastClientExecutionTimestamp = Sys_Milliseconds();
#endif

	if( com_quit ) {
		Com_Quit( {} );
	}

	if( setjmp( abortframe ) ) {
		return; // an ERR_DROP was thrown
	}

#ifndef DEDICATED_ONLY
	// Wait for the builtin server (if we are connecting online it does not have the loading state)
	if( Com_ServerState() == ss_loading ) {
		// While waiting for server, keep reading from the pipe and executing buffer commands (if any).
		(void)QBufPipe_ReadCmds( g_clCmdPipe );
		CL_GetCmdSystem()->executeBufferCommands();
		return;
	}
#endif

	if( logconsole && logconsole->modified ) {
		logconsole->modified = false;
		Com_ReopenConsoleLog();
	}

	if( const unsigned fixedTime = v_fixedTime.get(); fixedTime > 0 ) {
		*gameMsec = fixedTime;
	} else if( timescale->value >= 0 ) {
		*gameMsec = *extraTime + (float)realMsec * timescale->value;
		*extraTime = ( *extraTime + (float)realMsec * timescale->value ) - (float)*gameMsec;
	} else {
		*gameMsec = realMsec;
	}

	wswcurl_perform();

	FS_Frame();

#ifdef DEDICATED_ONLY
	CmdSystem *svCmdSystem = SV_GetCmdSystem();
	for(;; ) {
		if( const char *consoleInput = Sys_ConsoleInput() ) {
			svCmdSystem->appendCommand( wsw::StringView( va( "%s\n", consoleInput ) ) );
		} else {
			break;
		}
	}

	svCmdSystem->executeBufferCommands();
#endif

	// keep the random time dependent
	rand();

#ifdef DEDICATED_ONLY
	SV_Frame( realMsec, *gameMsec );
#else
	(void)QBufPipe_ReadCmds( g_clCmdPipe );
	wsw::ProfilingSystem::beginFrame( wsw::ProfilingSystem::ClientGroup, CL_GetProfilerArgsSupplier() );
	CL_Frame( realMsec, *gameMsec );
	wsw::ProfilingSystem::endFrame( wsw::ProfilingSystem::ClientGroup, CL_GetProfilerResultSink() );
#endif
}

static void SV_StopThread() {
#ifndef DEDICATED_ONLY
	sendTerminateCmd( g_svCmdPipe );
	QBufPipe_Finish( g_svCmdPipe );

	QThread_Join( g_svThread );
#endif
}

void Qcommon_Shutdown( void ) {
	static bool isdown = false;

	if( isdown ) {
		printf( "Recursive shutdown\n" );
		return;
	}
	isdown = true;

#ifndef DEDICATED_ONLY
	QBufPipe_Destroy( &g_svCmdPipe );
	QBufPipe_Destroy( &g_clCmdPipe );
#endif

	CM_Shutdown();
	Netchan_Shutdown();
	NET_Shutdown();

#ifndef DEDICATED_ONLY
	Key_Shutdown();
#endif

#ifdef DEDICATED_ONLY
	SV_GetCmdSystem()->unregisterCommand( wsw::StringView( "quit" ) );
	wsw::ProfilingSystem::detachFromThisThread( wsw::ProfilingSystem::ServerGroup );
#else
	wsw::ProfilingSystem::detachFromThisThread( wsw::ProfilingSystem::ClientGroup );
#endif

	Com_CloseConsoleLog( true, true );

	FS_Shutdown();

	Com_UnloadCompressionLibraries();

	wswcurl_cleanup();

	Cvar_Shutdown();
	Cmd_Shutdown();

	QMutex_Destroy( &com_print_mutex );
}

void Cmd_PreInit( void ) {
	assert( !cmd_preinitialized );
	assert( !cmd_initialized );

#ifndef DEDICATED_ONLY
	CL_InitCmdSystem();
#endif
	SV_InitCmdSystem();

	cmd_preinitialized = true;
}

void SV_Cmd_Register( const char *name, CmdFunc cmdFunc, CompletionQueryFunc completionFunc ) {
	SV_Cmd_Register( wsw::StringView( name ), cmdFunc, completionFunc );
}

void SV_Cmd_Unregister( const char *name ) {
	SV_Cmd_Unregister( wsw::StringView( name ) );
}

void SV_Cmd_Register( const wsw::StringView &name, CmdFunc cmdFunc, CompletionQueryFunc completionFunc ) {
	SV_GetCmdSystem()->registerCommand( name, cmdFunc );
#ifndef DEDICATED_ONLY
	callOverPipe( g_clCmdPipe, registerBuiltinServerCmdOnClientSide, wsw::PodVector<char>( name.data(), name.size() ), completionFunc );
#endif
}

void SV_Cmd_Unregister( const wsw::StringView &name ) {
	SV_GetCmdSystem()->unregisterCommand( name );
#ifndef DEDICATED_ONLY
	callOverPipe( g_clCmdPipe, unregisterBuiltinServerCmdOnClientSide, wsw::PodVector<char>( name.data(), name.size() ) );
#endif
}

void Cmd_AddClientAndServerCommand( const char *name, CmdFunc cmdFunc, CompletionQueryFunc completionQueryFunc ) {
	const wsw::StringView nameView( name );
#ifndef DEDICATED_ONLY
	if( completionQueryFunc ) {
		CL_RegisterCmdWithCompletion( nameView, cmdFunc, completionQueryFunc, CL_RunCompletionFuncSync );
	} else {
		CL_GetCmdSystem()->registerCommand( nameView, cmdFunc );
	}
#endif
	SV_GetCmdSystem()->registerCommand( nameView, cmdFunc );
}

void Cmd_RemoveClientAndServerCommand( const char *name ) {
	const wsw::StringView nameView( name );
#ifndef DEDICATED_ONLY
	CL_GetCmdSystem()->unregisterCommand( nameView );
#endif
	SV_GetCmdSystem()->unregisterCommand( nameView );
}

static void Cmd_Init( void ) {
	assert( !cmd_initialized );
	assert( cmd_preinitialized );

#ifndef DEDICATED_ONLY
	CL_GetCmdSystem()->registerSystemCommands();
#endif
	SV_GetCmdSystem()->registerSystemCommands();

	cmd_initialized = true;
}

static void Cmd_Shutdown( void ) {
	if( cmd_initialized ) {

#ifndef DEDICATED_ONLY
		CL_GetCmdSystem()->unregisterSystemCommands();
#endif
		SV_GetCmdSystem()->unregisterSystemCommands();

		cmd_initialized = false;
	}

	if( cmd_preinitialized ) {
#ifndef DEDICATED_ONLY
		CL_ShutdownCmdSystem();
#endif
		SV_ShutdownCmdSystem();

		cmd_preinitialized = false;
	}
}