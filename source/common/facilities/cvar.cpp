/*
Copyright (C) 2008 Chasseur de bots

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

#include <common/facilities/q_comref.h>
#include <common/types/q_trie.h>
#include <client/console.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/cvar.h>
#include <common/facilities/fscompat.h>
#include "configvars.h"
#include <common/common.h>
#include <common/local.h>
#include <common/helpers/qthreads.h>
#include <common/helpers/userinfo.h>
#include <common/helpers/textstreamwriterextras.h>
#include <common/facilities/messagestreams.h>

#include <atomic>

static bool cvar_initialized = false;
static bool cvar_preinitialized = false;

static trie_t *cvar_trie = NULL;
static qmutex_t *cvar_mutex = NULL;
static const trie_casing_t CVAR_TRIE_CASING = CON_CASE_SENSITIVE ? TRIE_CASE_SENSITIVE : TRIE_CASE_INSENSITIVE;;

static int Cvar_HasFlags( void *cvar, void *flags ) {
	assert( cvar );
	return Cvar_FlagIsSet( ( (cvar_t *) cvar )->flags, *(cvar_flag_t *) flags );
}

static int Cvar_IsLatched( void *cvar, void *flags ) {
	const cvar_t *const var = (cvar_t *) cvar;
	assert( cvar );
	return Cvar_FlagIsSet( var->flags, *(cvar_flag_t *) flags ) && var->latched_string;
}

static bool Cvar_CheatsAllowed() {
	return ( Com_ClientState() < CA_CONNECTED ) ||          // not connected
		   Com_DemoPlaying() ||                          // playing demo
		   ( Com_ServerState() && Cvar_Value( "sv_cheats" ) ); // local server, sv_cheats
}

static int Cvar_PatternMatches( void *cvar, void *pattern ) {
	return !pattern || Com_GlobMatch( (const char *) pattern, ( (cvar_t *) cvar )->name, false );
}

/*
* Cvar_InfoValidate
*/
static bool Cvar_InfoValidate( const char *s, bool name ) {
	return !( ( strlen( s ) >= (unsigned)( name ? MAX_INFO_KEY : MAX_INFO_VALUE ) ) ||
			  ( strchr( s, '\\' ) ) ||
			  ( strchr( s, '"' ) ) ||
			  ( strchr( s, ';' ) ) ||
			  ( name && strchr( s, Q_COLOR_ESCAPE ) ) );
}

/*
* Cvar_Initialized
*/
bool Cvar_Initialized( void ) {
	return cvar_initialized;
}

/*
* Cvar_Find
*/
cvar_t *Cvar_Find( const char *var_name ) {
	cvar_t *cvar;
	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_Find( cvar_trie, var_name, TRIE_EXACT_MATCH, (void **)&cvar );
	QMutex_Unlock( cvar_mutex );
	return cvar;
}

/*
* Cvar_Value
* Returns 0 if not defined or non numeric
*/
float Cvar_Value( const char *var_name ) {
	const cvar_t *const var = Cvar_Find( var_name );
	return var
		   ? atof( var->string )
		   : 0;
}


/*
* Cvar_String
* Returns an empty string if not defined
*/
const char *Cvar_String( const char *var_name ) {
	const cvar_t *const var = Cvar_Find( var_name );
	return var
		   ? var->string
		   : "";
}

/*
* Cvar_Integer
* Returns 0 if not defined or non numeric
*/
int Cvar_Integer( const char *var_name ) {
	const cvar_t *const var = Cvar_Find( var_name );
	return var
		   ? var->integer
		   : 0;
}

int Cvar_Flags( const char *var_name ) {
	const cvar_t *const var = Cvar_Find( var_name );
	return var
		   ? var->flags
		   : 0;
}

[[nodiscard]]
static auto getCorrectedValue( const cvar_t *var, const char *string ) -> char * {
	if( DeclaredConfigVar *const controller = var->controller ) {
		wsw::PodVector<char> tmpBuffer;
		const wsw::StringView stringView( string );
		if( const auto maybeCorrectedValue = controller->correctValue( stringView, &tmpBuffer ) ) {
			assert( maybeCorrectedValue->isZeroTerminated() );
			comWarning() << "Correcting the value" << stringView << "to" << *maybeCorrectedValue << "for" << wsw::StringView( var->name );
			return Q_strdup( maybeCorrectedValue->data() );
		} else {
			return Q_strdup( string );
		}
	} else {
		return Q_strdup( string );
	}
}

static void setValueString( cvar_t *var, const char *string ) {
	if( DeclaredConfigVar *const controller = var->controller ) {
		wsw::PodVector<char> tmpBuffer;
		const wsw::StringView stringView( string );
		if( const auto maybeCorrectedValue = controller->handleValueChanges( stringView, &tmpBuffer ) ) {
			assert( maybeCorrectedValue->isZeroTerminated() );
			comWarning() << "Correcting the value" << stringView << "to" << *maybeCorrectedValue << "for" << wsw::StringView( var->name );
			var->string = Q_strdup( maybeCorrectedValue->data() );
		} else {
			var->string = Q_strdup( string );
		}
	} else {
		var->string = Q_strdup( string );
	}
}

static void Cvar_SetModified( cvar_t *var ) {
	// TODO: This field should be gone
	var->modified = true;
	( (std::atomic<uint64_t> *)&var->modificationId )->fetch_add( 1 );
}

/*
* Cvar_Get
* Creates the variable if it doesn't exist.
* If the variable already exists, the value will not be set
* The flags will be or'ed and default value overwritten in if the variable exists.
*/
cvar_t *Cvar_Get( const char *var_name, const char *var_value, cvar_flag_t flags, DeclaredConfigVar *controller ) {
	cvar_t *var;

	if( !var_name || !var_name[0] ) {
		return NULL;
	}

	if( Cvar_FlagIsSet( flags, CVAR_USERINFO ) || Cvar_FlagIsSet( flags, CVAR_SERVERINFO ) ) {
		if( !Cvar_InfoValidate( var_name, true ) ) {
			comError() << "invalid info cvar name" << wsw::StringView( var_name );
			return NULL;
		}
	}

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_Find( cvar_trie, var_name, TRIE_EXACT_MATCH, (void **)&var );
	QMutex_Unlock( cvar_mutex );

	if( !var_value ) {
		return NULL;
	}

	if( var ) {
		bool reset = false;

		// Update the controller prior to resetting the default value
		if( var->controller != controller ) {
			if( var->controller && controller ) {
				comWarning() << "replacing a controller for cvar" << wsw::StringView( var_name );
			}
			var->controller = controller;
			reset = true;
		}

		if( var->dvalue ) {
			Q_free( var->dvalue ); // free the old default value string
		}
		var->dvalue = getCorrectedValue( var, var_value );

		if( Cvar_FlagIsSet( flags, CVAR_USERINFO ) || Cvar_FlagIsSet( flags, CVAR_SERVERINFO ) ) {
			if( var->string && !Cvar_InfoValidate( var->string, false ) ) {
				reset = true;
			}
		}

#ifdef PUBLIC_BUILD
		reset = reset || ( Cvar_FlagIsSet( flags, CVAR_READONLY ) || Cvar_FlagIsSet( flags, CVAR_DEVELOPER ) );
#else
		reset = reset || ( Cvar_FlagIsSet( flags, CVAR_READONLY ) );
#endif
		if( reset ) {
			if( var->string ) {
				Q_free( var->string );
			}
			setValueString( var, var_value );
			var->value   = atof( var->string );
			var->integer = Q_rint( var->value );
			var->flags   = flags;
		}

		if( Cvar_FlagIsSet( flags, CVAR_USERINFO ) && !Cvar_FlagIsSet( var->flags, CVAR_USERINFO ) ) {
			userinfo_modified = true; // transmit at next oportunity

		}
		Cvar_FlagSet( &var->flags, flags );
		return var;
	}

	if( Cvar_FlagIsSet( flags, CVAR_USERINFO ) || Cvar_FlagIsSet( flags, CVAR_SERVERINFO ) ) {
		if( !Cvar_InfoValidate( var_value, false ) ) {
			comError() << "Invalid info cvar value" << wsw::StringView( var_value );
			return NULL;
		}
	}

	var = (cvar_t *)Q_malloc( (int)( sizeof( *var ) + strlen( var_name ) + 1 ) );
	var->controller = controller;
	var->name = (char *)( (uint8_t *)var + sizeof( *var ) );
	strcpy( var->name, var_name );
	setValueString( var, var_value );
	var->dvalue  = Q_strdup( var->string );
	var->value   = atof( var->string );
	var->integer = Q_rint( var->value );
	var->flags   = flags;
	Cvar_SetModified( var );

	QMutex_Lock( cvar_mutex );
	Trie_Insert( cvar_trie, var_name, var );
	QMutex_Unlock( cvar_mutex );

	return var;
}

/*
* Cvar_Set2
*/
cvar_t *Cvar_Set2( const char *var_name, const char *value, bool force ) {
	cvar_t *var = Cvar_Find( var_name );

	if( !var ) {
		// create it
		return Cvar_Get( var_name, value, 0 );
	}

	if( Cvar_FlagIsSet( var->flags, CVAR_USERINFO ) || Cvar_FlagIsSet( var->flags, CVAR_SERVERINFO ) ) {
		if( !Cvar_InfoValidate( value, false ) ) {
			Com_Printf( "invalid info cvar value\n" );
			return var;
		}
	}

	if( !force ) {
#ifdef PUBLIC_BUILD
		if( Cvar_FlagIsSet( var->flags, CVAR_NOSET ) || Cvar_FlagIsSet( var->flags, CVAR_READONLY ) || Cvar_FlagIsSet( var->flags, CVAR_DEVELOPER ) ) {
#else
		if( Cvar_FlagIsSet( var->flags, CVAR_NOSET ) || Cvar_FlagIsSet( var->flags, CVAR_READONLY ) ) {
#endif
			comNotice() << wsw::StringView( var_name ) << "is write protected";
			return var;
		}

		if( Cvar_FlagIsSet( var->flags, CVAR_CHEAT ) && strcmp( value, var->dvalue ) ) {
			if( !Cvar_CheatsAllowed() ) {
				comNotice() << wsw::StringView( var_name ) << "is cheat protected";
				return var;
			}
		}

		if( Cvar_FlagIsSet( var->flags, CVAR_LATCH ) || Cvar_FlagIsSet( var->flags, CVAR_LATCH_VIDEO ) || Cvar_FlagIsSet( var->flags, CVAR_LATCH_SOUND ) ) {
			if( var->latched_string ) {
				Q_free( var->latched_string );
			} else {
				if( !strcmp( value, var->string ) ) {
					return var;
				}
			}

			if( Com_ServerState() ) {
				comNotice() << wsw::StringView( var->name ) << "will be changed upon restarting";
				// TODO: Don't modify var contents!
				var->latched_string = getCorrectedValue( var, value );
			} else {
				if( Cvar_FlagIsSet( var->flags, CVAR_LATCH_VIDEO ) ) {
					comNotice() << wsw::StringView( var->name ) << "will be changed upon restarting video";
					var->latched_string = getCorrectedValue( var, value );
				} else if( Cvar_FlagIsSet( var->flags, CVAR_LATCH_SOUND ) ) {
					comNotice() << wsw::StringView( var->name ) << "will be changed upon restarting sound";
					var->latched_string = getCorrectedValue( var, value );
				} else {
					Q_free( var->string ); // free the old value string
					setValueString( var, value );
					var->value   = atof( var->string );
					var->integer = Q_rint( var->value );
					Cvar_SetModified( var );
				}
			}
			return var;
		}
	} else {
		if( var->latched_string ) {
			Q_free( var->latched_string );
			var->latched_string = NULL;
		}
	}

	if( !strcmp( value, var->string ) ) {
		return var; // not changed
	}
	if( Cvar_FlagIsSet( var->flags, CVAR_USERINFO ) ) {
		userinfo_modified = true; // transmit at next oportunity
	}

	Q_free( var->string ); // free the old value string

	setValueString( var, value );
	var->value   = atof( var->string );
	var->integer = Q_rint( var->value );
	Cvar_SetModified( var );

	return var;
}

/*
* Cvar_ForceSet
* Set the variable even if NOSET or LATCH
*/
cvar_t *Cvar_ForceSet( const char *var_name, const char *value ) {
	return Cvar_Set2( var_name, value, true );
}

/*
* Cvar_Set
* Create the variable if it doesn't exist
*/
cvar_t *Cvar_Set( const char *var_name, const char *value ) {
	return Cvar_Set2( var_name, value, false );
}

/*
* Cvar_FullSet
*/
cvar_t *Cvar_FullSet( const char *var_name, const char *value, cvar_flag_t flags, bool overwrite_flags ) {
	cvar_t *var;

	var = Cvar_Find( var_name );
	if( !var ) {
		return Cvar_Get( var_name, value, flags );
	}

	if( overwrite_flags ) {
		var->flags = flags;
	} else {
		Cvar_FlagSet( &var->flags, flags );
	}

	// if we overwrite the flags, we will also force the value
	return Cvar_Set2( var_name, value, overwrite_flags );
}

/*
* Cvar_SetValue
* Expands value to a string and calls Cvar_Set
*/
void Cvar_SetValue( const char *var_name, float value ) {
	char val[32];
	if( value == Q_rint( value ) ) {
		Q_snprintfz( val, sizeof( val ), "%i", Q_rint( value ) );
	} else {
		Q_snprintfz( val, sizeof( val ), "%f", value );
	}
	Cvar_Set( var_name, val );
}

/*
* Cvar_GetLatchedVars
*
* Any variables with CVAR_LATCHED will now be updated
*/
void Cvar_GetLatchedVars( cvar_flag_t flags ) {
	unsigned int i;
	struct trie_dump_s *dump = NULL;
	cvar_flag_t latchFlags;

	Cvar_FlagsClear( &latchFlags );
	Cvar_FlagSet( &latchFlags, CVAR_LATCH );
	Cvar_FlagSet( &latchFlags, CVAR_LATCH_VIDEO );
	Cvar_FlagSet( &latchFlags, CVAR_LATCH_SOUND );
	Cvar_FlagUnset( &flags, ~latchFlags );
	if( !flags ) {
		return;
	}

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_DumpIf( cvar_trie, "", TRIE_DUMP_VALUES, Cvar_IsLatched, &flags, &dump );
	QMutex_Unlock( cvar_mutex );
	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *) dump->key_value_vector[i].value;
		Q_free( var->string );
		var->string = var->latched_string;
		var->latched_string = NULL;
		var->value = atof( var->string );
		var->integer = Q_rint( var->value );
	}
	Trie_FreeDump( dump );
}

/*
* Cvar_FixCheatVars
*
* All cheat variables with be reset to default unless cheats are allowed
*/
void Cvar_FixCheatVars( void ) {
	struct trie_dump_s *dump = NULL;
	unsigned int i;
	cvar_flag_t flags = CVAR_CHEAT;

	if( Cvar_CheatsAllowed() ) {
		return;
	}

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_DumpIf( cvar_trie, "", TRIE_DUMP_VALUES, Cvar_HasFlags, &flags, &dump );
	QMutex_Unlock( cvar_mutex );
	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *) dump->key_value_vector[i].value;
		Q_free( var->string );
		var->string = Q_strdup( var->dvalue );
		var->value = atof( var->string );
		var->integer = Q_rint( var->value );
	}
	Trie_FreeDump( dump );
}


/*
* Cvar_Command
*
* Handles variable inspection and changing from the console
*
* Called by Cmd_ExecuteString when Cmd_Argv(0) doesn't match a known
* command.  Returns true if the command was a variable reference that
* was handled. (print or change)
*/
bool Cvar_Command( const CmdArgs &cmdArgs ) {
	cvar_t *v;

	// check variables
	v = Cvar_Find( cmdArgs[0].data() );
	if( !v ) {
		return false;
	}

	// perform a variable print or set
	if( cmdArgs.size() == 1 ) {
		Com_Printf( "\"%s\" is \"%s%s\" default: \"%s%s\"\n", v->name,
					v->string, Q_ColorStringTerminator( v->string, ColorIndex( COLOR_WHITE ) ),
					v->dvalue, Q_ColorStringTerminator( v->dvalue, ColorIndex( COLOR_WHITE ) ) );
		if( v->latched_string ) {
			Com_Printf( "latched: \"%s%s\"\n", v->latched_string,
						Q_ColorStringTerminator( v->latched_string, ColorIndex( COLOR_WHITE ) ) );
		}
		return true;
	}

	Cvar_Set( v->name, Cmd_Argv( 1 ) );
	return true;
}


/*
* Cvar_Set_f
*
* Allows setting and defining of arbitrary cvars from console
*/
static void Cvar_Set_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() != 3 ) {
		comNotice() << "usage: set <variable> <value>";
		return;
	}
	Cvar_Set( Cmd_Argv( 1 ), Cmd_Argv( 2 ) );
}

static void Cvar_SetWithFlag_f( const CmdArgs &cmdArgs, cvar_flag_t flag ) {
	if( Cmd_Argc() != 3 ) {
		comNotice() << "usage:" << wsw::unquoted( cmdArgs[0] ) << "<variable> <value>";
		return;
	}
	Cvar_FullSet( Cmd_Argv( 1 ), Cmd_Argv( 2 ), flag, false );
}

static void Cvar_Seta_f( const CmdArgs &cmdArgs ) {
	Cvar_SetWithFlag_f( cmdArgs, CVAR_ARCHIVE );
}

static void Cvar_Setau_f( const CmdArgs &cmdArgs ) {
	Cvar_SetWithFlag_f( cmdArgs, CVAR_ARCHIVE | CVAR_USERINFO );
}

static void Cvar_Setas_f( const CmdArgs &cmdArgs ) {
	Cvar_SetWithFlag_f( cmdArgs, CVAR_ARCHIVE | CVAR_SERVERINFO );
}

static void Cvar_Sets_f( const CmdArgs &cmdArgs ) {
	Cvar_SetWithFlag_f( cmdArgs, CVAR_SERVERINFO );
}

static void Cvar_Setu_f( const CmdArgs &cmdArgs ) {
	Cvar_SetWithFlag_f( cmdArgs, CVAR_USERINFO );
}

static void Cvar_Reset_f( const CmdArgs &cmdArgs ) {
	cvar_t *v;

	if( Cmd_Argc() != 2 ) {
		comNotice() << "usage: reset <variable>";
		return;
	}

	v = Cvar_Find( Cmd_Argv( 1 ) );
	if( !v ) {
		return;
	}

	Cvar_Set( v->name, v->dvalue );
}

/*
* Cvar_Toggle_f
*/
static void Cvar_Toggle_f( const CmdArgs &cmdArgs ) {
	int i;
	cvar_t *var;

	if( Cmd_Argc() < 2 ) {
		comNotice() << "Usage: toggle <list of variables>";
		return;
	}

	for( i = 1; i < Cmd_Argc(); i++ ) {
		var = Cvar_Find( Cmd_Argv( i ) );
		if( !var ) {
			comNotice() << "No such variable:" << cmdArgs[i];
			return;
		}
		Cvar_Set( var->name, var->integer ? "0" : "1" );
	}
}

/*
* Cvar_WriteVariables
*
* Appends lines containing "set variable value" for all variables
* with the archive flag set to true.
*/
void Cvar_WriteVariables( int file ) {
	char buffer[MAX_PRINTMSG];
	struct trie_dump_s *dump = NULL;
	unsigned int i;
	cvar_flag_t cvar_archive = CVAR_ARCHIVE;

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_DumpIf( cvar_trie, "", TRIE_DUMP_VALUES, Cvar_HasFlags, &cvar_archive, &dump );
	QMutex_Unlock( cvar_mutex );
	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *)dump->key_value_vector[i].value;
		const char *cmd;

		if( Cvar_FlagIsSet( var->flags, CVAR_USERINFO ) ) {
			cmd = "setau";
		} else if( Cvar_FlagIsSet( var->flags, CVAR_SERVERINFO ) ) {
			cmd = "setas";
		} else {
			cmd = "seta";
		}

		if( Cvar_FlagIsSet( var->flags, CVAR_LATCH ) || Cvar_FlagIsSet( var->flags, CVAR_LATCH_VIDEO ) ||
			Cvar_FlagIsSet( var->flags, CVAR_LATCH_SOUND ) ) {
			if( var->latched_string ) {
				Q_snprintfz( buffer, sizeof( buffer ), "%s %s \"%s\"\r\n", cmd, var->name, var->latched_string );
			} else {
				Q_snprintfz( buffer, sizeof( buffer ), "%s %s \"%s\"\r\n", cmd, var->name, var->string );
			}
		} else {
			Q_snprintfz( buffer, sizeof( buffer ), "%s %s \"%s\"\r\n", cmd, var->name, var->string );
		}
		FS_Printf( file, "%s", buffer );
	}
	Trie_FreeDump( dump );
}

/*
* Cvar_List_f
*/
static void Cvar_List_f( const CmdArgs &cmdArgs ) {
	struct trie_dump_s *dump = NULL;
	unsigned int i;
	const char *pattern;

	if( Cmd_Argc() == 1 ) {
		pattern = NULL;
	} else {
		pattern = Cmd_Args();
	}

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_DumpIf( cvar_trie, "", TRIE_DUMP_VALUES, Cvar_PatternMatches, (void *)pattern, &dump );
	QMutex_Unlock( cvar_mutex );

	Com_Printf( "\nConsole variables:\n" );
	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *)dump->key_value_vector[i].value;
#ifdef PUBLIC_BUILD
		if( Cvar_FlagIsSet( var->flags, CVAR_DEVELOPER ) ) {
			continue;
		}
#endif
		if( Cvar_FlagIsSet( var->flags, CVAR_ARCHIVE ) ) {
			Com_Printf( "*" );
		} else {
			Com_Printf( " " );
		}
		if( Cvar_FlagIsSet( var->flags, CVAR_USERINFO ) ) {
			Com_Printf( "U" );
		} else {
			Com_Printf( " " );
		}
		if( Cvar_FlagIsSet( var->flags, CVAR_SERVERINFO ) ) {
			Com_Printf( "S" );
		} else {
			Com_Printf( " " );
		}
		if( Cvar_FlagIsSet( var->flags, CVAR_NOSET ) || Cvar_FlagIsSet( var->flags, CVAR_READONLY ) ) {
			Com_Printf( "-" );
		} else if( Cvar_FlagIsSet( var->flags, CVAR_LATCH ) || Cvar_FlagIsSet( var->flags, CVAR_LATCH_VIDEO ) ||
				   Cvar_FlagIsSet( var->flags, CVAR_LATCH_SOUND ) ) {
			Com_Printf( "L" );
		} else {
			Com_Printf( " " );
		}
		if( Cvar_FlagIsSet( var->flags, CVAR_CHEAT ) ) {
			Com_Printf( "C" );
		} else {
			Com_Printf( " " );
		}
		Com_Printf( " %s \"%s%s\", default: \"%s%s\"\n", var->name,
					var->string, Q_ColorStringTerminator( var->string, ColorIndex( COLOR_WHITE ) ),
					var->dvalue, Q_ColorStringTerminator( var->dvalue, ColorIndex( COLOR_WHITE ) ) );
	}
	Com_Printf( "%i variables\n", i );

	Trie_FreeDump( dump );
}

#ifndef PUBLIC_BUILD
/*
* Cvar_ArchiveList_f
*/
static void Cvar_ArchiveList_f( const CmdArgs & ) {
	struct trie_dump_s *dump;
	unsigned int i;

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_Dump( cvar_trie, "", TRIE_DUMP_VALUES, &dump );
	QMutex_Unlock( cvar_mutex );

	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *)dump->key_value_vector[i].value;
		if( Cvar_FlagIsSet( var->flags, CVAR_DEVELOPER ) ) {
			continue;
		}
		if( !Cvar_FlagIsSet( var->flags, CVAR_ARCHIVE ) ) {
			continue;
		}
		Com_Printf( "set %s \"%s\"\n", var->name, var->dvalue );
	}

	Trie_FreeDump( dump );
}
#endif

bool userinfo_modified;

static char *Cvar_BitInfo( int bit ) {
	static char info[MAX_INFO_STRING];
	struct trie_dump_s *dump = NULL;
	unsigned int i;

	info[0] = 0;

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );
	Trie_DumpIf( cvar_trie, "", TRIE_DUMP_VALUES, Cvar_HasFlags, &bit, &dump );
	QMutex_Unlock( cvar_mutex );

	// make sure versioncvar comes first
	for( i = dump->size; i > 0; --i ) {
		cvar_t *const var = (cvar_t *)dump->key_value_vector[i - 1].value;
		if( var == versioncvar ) {
			Info_SetValueForKey( info, var->name, var->string );
			break;
		}
	}

	// dump other cvars
	for( i = 0; i < dump->size; ++i ) {
		cvar_t *const var = (cvar_t *)dump->key_value_vector[i].value;
		if( var != versioncvar ) {
			Info_SetValueForKey( info, var->name, var->string );
		}
	}

	Trie_FreeDump( dump );

	return info;
}

/*
* Cvar_Userinfo
* Returns an info string containing all the CVAR_USERINFO cvars
*/
char *Cvar_Userinfo( void ) {
	return Cvar_BitInfo( CVAR_USERINFO );
}

/*
* Cvar_Serverinfo
* Returns an info string containing all the CVAR_SERVERINFO cvars
*/
char *Cvar_Serverinfo( void ) {
	return Cvar_BitInfo( CVAR_SERVERINFO );
}

/*
* Cvar_NotDeveloper
*/
#ifdef PUBLIC_BUILD
static int Cvar_NotDeveloper( void *cvar, void *nothing ) {
	return !Cvar_FlagIsSet( ( (cvar_t *)cvar )->flags, CVAR_DEVELOPER );
}
#endif

CompletionResult Cvar_CompleteBuildList( const wsw::StringView &partial ) {
	wsw::PodVector<char> ztPartial( partial.data(), partial.size() );
	ztPartial.push_back( '\0' );

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );

	struct trie_dump_s *dump = nullptr;
#ifdef PUBLIC_BUILD
	Trie_DumpIf( cvar_trie, ztPartial.data(), TRIE_DUMP_VALUES, Cvar_NotDeveloper, NULL, &dump );
#else
	Trie_Dump( cvar_trie, ztPartial.data(), TRIE_DUMP_VALUES, &dump );
#endif
	QMutex_Unlock( cvar_mutex );

	CompletionResult result;
	for( unsigned i = 0; i < dump->size; ++i ) {
		result.add( wsw::StringView( ( (cvar_t *)dump->key_value_vector[i].value )->name ) );
	}

	Trie_FreeDump( dump );

	return result;
}

static CompletionResult Cvar_CompleteBuildListWithFlag( const wsw::StringView &partial, cvar_flag_t flag ) {
	wsw::PodVector<char> ztPartial( partial.data(), partial.size() );
	ztPartial.push_back( '\0' );

	assert( cvar_trie );
	QMutex_Lock( cvar_mutex );

	struct trie_dump_s *dump = nullptr;
	Trie_DumpIf( cvar_trie, ztPartial.data(), TRIE_DUMP_VALUES, Cvar_HasFlags, &flag, &dump );
	QMutex_Unlock( cvar_mutex );

	CompletionResult result;
	for( unsigned i = 0; i < dump->size; ++i ) {
		result.add( wsw::StringView( ( (cvar_t *)dump->key_value_vector[i].value )->name ) );
	}

	Trie_FreeDump( dump );

	return result;
}

static CompletionResult Cvar_CompleteBuildListUser( const wsw::StringView &partial ) {
	return Cvar_CompleteBuildListWithFlag( partial, CVAR_USERINFO );
}

static CompletionResult Cvar_CompleteBuildListServer( const wsw::StringView &partial ) {
	return Cvar_CompleteBuildListWithFlag( partial, CVAR_SERVERINFO );
}

/*
* Cvar_PreInit
*/
void Cvar_PreInit( void ) {
	assert( !cvar_initialized );
	assert( !cvar_preinitialized );

	assert( !cvar_trie );

	cvar_mutex = QMutex_Create();

	Trie_Create( CVAR_TRIE_CASING, &cvar_trie );

	cvar_preinitialized = true;
}

/*
* Cvar_Init
*
* Reads in all archived cvars
*/
void Cvar_Init( void ) {
	assert( !cvar_initialized );
	assert( cvar_preinitialized );

	assert( cvar_trie );

	Cmd_AddClientAndServerCommand( "set", Cvar_Set_f, Cvar_CompleteBuildList );
	Cmd_AddClientAndServerCommand( "seta", Cvar_Seta_f, Cvar_CompleteBuildList );
	Cmd_AddClientAndServerCommand( "setau", Cvar_Setau_f, Cvar_CompleteBuildListUser );
	Cmd_AddClientAndServerCommand( "setas", Cvar_Setas_f, Cvar_CompleteBuildListServer );
	Cmd_AddClientAndServerCommand( "setu", Cvar_Setu_f, Cvar_CompleteBuildListUser );
	Cmd_AddClientAndServerCommand( "sets", Cvar_Sets_f, Cvar_CompleteBuildListServer );
	Cmd_AddClientAndServerCommand( "reset", Cvar_Reset_f, Cvar_CompleteBuildList );
	Cmd_AddClientAndServerCommand( "toggle", Cvar_Toggle_f, Cvar_CompleteBuildList );
	Cmd_AddClientAndServerCommand( "cvarlist", Cvar_List_f );

#ifndef PUBLIC_BUILD
	Cmd_AddClientAndServerCommand( "cvararchivelist", Cvar_ArchiveList_f );
#endif

	DeclaredConfigVar::registerAllVars( DeclaredConfigVar::s_listHead );
	cvar_initialized = true;
}

/*
* Cvar_Shutdown
*
* Reads in all archived cvars
*/
void Cvar_Shutdown( void ) {
	if( cvar_initialized ) {
		DeclaredConfigVar::unregisterAllVars( DeclaredConfigVar::s_listHead );

		unsigned int i;
		struct trie_dump_s *dump;
		extern cvar_t *developer;

		assert( cvar_trie );

		// NULL out some console variables so that we won't try to read from
		// the memory pointers after the data has already been freed but before we
		// reset the pointers to NULL
		developer = NULL;
		dedicated = NULL;

		Cmd_RemoveClientAndServerCommand( "set" );
		Cmd_RemoveClientAndServerCommand( "seta" );
		Cmd_RemoveClientAndServerCommand( "setau" );
		Cmd_RemoveClientAndServerCommand( "setas" );
		Cmd_RemoveClientAndServerCommand( "setu" );
		Cmd_RemoveClientAndServerCommand( "sets" );
		Cmd_RemoveClientAndServerCommand( "reset" );
		Cmd_RemoveClientAndServerCommand( "toggle" );
		Cmd_RemoveClientAndServerCommand( "cvarlist" );
#ifndef PUBLIC_BUILD
		Cmd_RemoveClientAndServerCommand( "cvararchivelist" );
#endif

		QMutex_Lock( cvar_mutex );
		Trie_Dump( cvar_trie, "", TRIE_DUMP_VALUES, &dump );
		QMutex_Unlock( cvar_mutex );
		for( i = 0; i < dump->size; ++i ) {
			cvar_t *const var = (cvar_t *)dump->key_value_vector[i].value;

			if( var->string ) {
				Q_free( var->string );
			}
			if( var->dvalue ) {
				Q_free( var->dvalue );
			}
			Q_free( var );
		}
		Trie_FreeDump( dump );

		cvar_initialized = false;
	}

	if( cvar_preinitialized ) {
		assert( cvar_trie );

		QMutex_Lock( cvar_mutex );
		Trie_Destroy( cvar_trie );
		QMutex_Unlock( cvar_mutex );
		cvar_trie = NULL;

		QMutex_Destroy( &cvar_mutex );

		cvar_preinitialized = false;
	}
}
