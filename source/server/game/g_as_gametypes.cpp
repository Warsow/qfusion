/*
Copyright (C) 2008 German Garcia

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

#include "g_local.h"
#include "g_as_local.h"

static void GT_ResetScriptData( void ) {
	level.gametype.initFunc = NULL;
	level.gametype.spawnFunc = NULL;
	level.gametype.matchStateStartedFunc = NULL;
	level.gametype.matchStateFinishedFunc = NULL;
	level.gametype.thinkRulesFunc = NULL;
	level.gametype.playerRespawnFunc = NULL;
	level.gametype.scoreEventFunc = NULL;
	level.gametype.updateScoreboardFunc = NULL;
	level.gametype.selectSpawnPointFunc = NULL;
	level.gametype.clientCommandFunc = NULL;
	level.gametype.shutdownFunc = NULL;

	AI_ResetGametypeScript();
}

void GT_asShutdownScript( void ) {
	int i;
	edict_t *e;

	if( game.asEngine == NULL ) {
		return;
	}

	// release the callback and any other objects obtained from the script engine before releasing the engine
	for( i = 0; i < game.numentities; i++ ) {
		e = &game.edicts[i];

		if( e->scriptSpawned && e->asScriptModule &&
			!strcmp( ( static_cast<asIScriptModule*>( e->asScriptModule ) )->GetName(), GAMETYPE_SCRIPTS_MODULE_NAME ) ) {
			G_asReleaseEntityBehaviors( e );
			e->asScriptModule = NULL;
		}
	}

	GT_ResetScriptData();

	GAME_AS_ENGINE()->DiscardModule( GAMETYPE_SCRIPTS_MODULE_NAME );
}

//"void GT_SpawnGametype()"
void GT_asCallSpawn( void ) {
	int error;
	asIScriptContext *ctx;

	if( !level.gametype.spawnFunc ) {
		return;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.spawnFunc ) );
	if( error < 0 ) {
		return;
	}

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

//"void GT_MatchStateStarted()"
void GT_asCallMatchStateStarted( void ) {
	int error;
	asIScriptContext *ctx;

	if( !level.gametype.matchStateStartedFunc ) {
		return;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.matchStateStartedFunc ) );
	if( error < 0 ) {
		return;
	}

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

//"bool GT_MatchStateFinished( int incomingMatchState )"
bool GT_asCallMatchStateFinished( int incomingMatchState ) {
	int error;
	asIScriptContext *ctx;
	bool result;

	if( !level.gametype.matchStateFinishedFunc ) {
		return true;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.matchStateFinishedFunc ) );
	if( error < 0 ) {
		return true;
	}

	// Now we need to pass the parameters to the script function.
	ctx->SetArgDWord( 0, incomingMatchState );

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}

	// Retrieve the return from the context
	result = ctx->GetReturnByte() == 0 ? false : true;

	return result;
}

//"void GT_ThinkRules( void )"
void GT_asCallThinkRules( void ) {
	int error;
	asIScriptContext *ctx;

	if( !level.gametype.thinkRulesFunc ) {
		return;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.thinkRulesFunc ) );
	if( error < 0 ) {
		return;
	}

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

//"void GT_playerRespawn( Entity @ent, int old_team, int new_team )"
void GT_asCallPlayerRespawn( edict_t *ent, int old_team, int new_team ) {
	int error;
	asIScriptContext *ctx;

	if( !level.gametype.playerRespawnFunc ) {
		return;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.playerRespawnFunc ) );
	if( error < 0 ) {
		return;
	}

	// Now we need to pass the parameters to the script function.
	ctx->SetArgObject( 0, ent );
	ctx->SetArgDWord( 1, old_team );
	ctx->SetArgDWord( 2, new_team );

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

//"void GT_scoreEvent( Client @client, String &score_event, String &args )"
void GT_asCallScoreEvent( Client *client, const char *score_event, const char *args ) {
	int error;
	asIScriptContext *ctx;
	asstring_t *s1, *s2;

	if( !level.gametype.scoreEventFunc ) {
		return;
	}

	if( !score_event || !score_event[0] ) {
		return;
	}

	if( !args ) {
		args = "";
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.scoreEventFunc ) );
	if( error < 0 ) {
		return;
	}

	// Now we need to pass the parameters to the script function.
	s1 = qasStringFactoryBuffer( score_event, strlen( score_event ) );
	s2 = qasStringFactoryBuffer( args, strlen( args ) );

	ctx->SetArgObject( 0, client );
	ctx->SetArgObject( 1, s1 );
	ctx->SetArgObject( 2, s2 );

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}

	qasStringRelease( s1 );
	qasStringRelease( s2 );
}

void GT_asCallUpdateScoreboard() {
	if( !level.gametype.updateScoreboardFunc ) {
		return;
	}

	asIScriptContext *ctx = qasAcquireContext( GAME_AS_ENGINE() );
	int error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.updateScoreboardFunc ) );
	if( error < 0 ) {
		return;
	}

	// Now we need to pass the parameters to the script function.
	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

//"Entity @GT_SelectSpawnPoint( Entity @ent )"
edict_t *GT_asCallSelectSpawnPoint( edict_t *ent ) {
	int error;
	asIScriptContext *ctx;
	edict_t *spot;

	if( !level.gametype.selectSpawnPointFunc ) {
		return SelectDeathmatchSpawnPoint( ent ); // should have a hardcoded backup

	}
	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.selectSpawnPointFunc ) );
	if( error < 0 ) {
		return SelectDeathmatchSpawnPoint( ent );
	}

	// Now we need to pass the parameters to the script function.
	ctx->SetArgObject( 0, ent );

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}

	spot = ( edict_t * )ctx->GetReturnObject();
	if( !spot ) {
		spot = SelectDeathmatchSpawnPoint( ent );
	}

	return spot;
}

//"bool GT_Command( Client @client, String &cmdString, String &argsString, int argc )"
bool GT_asCallGameCommand( Client *client, const wsw::StringView &cmd, const wsw::StringView &args, int argc ) {
	int error;
	asIScriptContext *ctx;
	asstring_t *s1, *s2;

	if( !level.gametype.clientCommandFunc ) {
		return false; // should have a hardcoded backup

	}

	// check for having any command to parse
	if( cmd.empty() ) {
		return false;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.clientCommandFunc ) );
	if( error < 0 ) {
		return false;
	}

	// Now we need to pass the parameters to the script function.
	s1 = qasStringFactoryBuffer( cmd.data(), cmd.size() );
	s2 = qasStringFactoryBuffer( args.data(), args.size() );

	ctx->SetArgObject( 0, client );
	ctx->SetArgObject( 1, s1 );
	ctx->SetArgObject( 2, s2 );
	ctx->SetArgDWord( 3, argc );

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}

	qasStringRelease( s1 );
	qasStringRelease( s2 );

	// Retrieve the return from the context
	return ctx->GetReturnByte() == 0 ? false : true;
}

//"void GT_Shutdown()"
void GT_asCallShutdown( void ) {
	int error;
	asIScriptContext *ctx;

	if( !level.gametype.shutdownFunc ) {
		return;
	}

	ctx = qasAcquireContext( GAME_AS_ENGINE() );

	error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.shutdownFunc ) );
	if( error < 0 ) {
		return;
	}

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		GT_asShutdownScript();
	}
}

static void *LoadScriptFunc( asIScriptModule *module, const char *decl ) {
	void *pfn = module->GetFunctionByDecl( decl );
	if( !pfn ) {
		if( developer->integer || sv_cheats->integer ) {
			G_Printf( "* The function '%s' was not present in the script.\n", decl );
		}
	}
	return pfn;
}

static bool G_asInitializeGametypeScript( asIScriptModule *asModule ) {
	if( !( level.gametype.initFunc = asModule->GetFunctionByDecl( "void GT_InitGametype()" ) ) ) {
		G_Printf( "Failed to find GT_InitGametype() in the script\n" );
		return false;
	}

	auto *const lgt = &level.gametype;
	std::pair<void **, const char *> defs[] = {
		{ &lgt->spawnFunc, "void GT_SpawnGametype()" },
		{ &lgt->matchStateStartedFunc, "void GT_MatchStateStarted()" },
		{ &lgt->matchStateFinishedFunc, "bool GT_MatchStateFinished( int incomingMatchState )" },
		{ &lgt->thinkRulesFunc, "void GT_ThinkRules()" },
		{ &lgt->playerRespawnFunc, "void GT_PlayerRespawn( Entity @ent, int old_team, int new_team )" },
		{ &lgt->scoreEventFunc, "void GT_ScoreEvent( Client @client, const String &score_event, const String &args )" },
		{ &lgt->updateScoreboardFunc, "void GT_UpdateScoreboard()" },
		{ &lgt->selectSpawnPointFunc, "Entity @GT_SelectSpawnPoint( Entity @ent )" },
		{ &lgt->clientCommandFunc, "bool GT_Command( Client @client, const String &cmdString, const String &argsString, int argc )" },
		{ &lgt->shutdownFunc, "void GT_Shutdown()" }
	};

	for( auto [ppfn, decl]: defs ) {
		*ppfn = LoadScriptFunc( asModule, decl );
	}

	//
	// Initialize AI gametype exports
	//
	AI_InitGametypeScript( asModule );

	//
	// execute the GT_InitGametype function
	//

	asIScriptContext *ctx = qasAcquireContext( GAME_AS_ENGINE() );

	int error = ctx->Prepare( static_cast<asIScriptFunction *>( level.gametype.initFunc ) );
	if( error < 0 ) {
		return false;
	}

	error = ctx->Execute();
	if( G_ExecutionErrorReport( error ) ) {
		return false;
	}

	return true;
}

bool GT_asLoadScript( const char *gametypeName ) {
	const char *moduleName = GAMETYPE_SCRIPTS_MODULE_NAME;
	asIScriptModule *asModule;

	GT_ResetScriptData();

	// Load the script
	asModule = G_LoadGameScript( moduleName, GAMETYPE_SCRIPTS_DIRECTORY, gametypeName, GAMETYPE_PROJECT_EXTENSION );
	if( asModule == NULL ) {
		return false;
	}

	// Initialize the script
	if( !G_asInitializeGametypeScript( asModule ) ) {
		GT_asShutdownScript();
		return false;
	}

	return true;
}
