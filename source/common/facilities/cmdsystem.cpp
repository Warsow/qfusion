/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2023 Chasseur de Bots

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

#include "cmdsystem.h"
#include "cmdcompat.h"
#include <common/local.h>
#include <common/helpers/links.h>
#include <common/common.h>
#include <common/types/staticstring.h>
#include <common/helpers/exceptions.h>

using wsw::operator""_asView;

void CmdSystem::TextBuffer::prepend( const wsw::StringView &text ) {
	if( !text.endsWith( '\n' ) && !text.endsWith( ';' ) ) {
		if( text.isZeroTerminated() ) {
			// Copy including the zero character just to overwrite it
			m_data.insert( m_data.begin() + m_headOffset, text.begin(), text.end() + 1 );
			m_data[m_headOffset + text.length()] = '\n';
		} else {
			m_data.insert( m_data.begin() + m_headOffset, text.begin(), text.end() );
			m_data.insert( m_data.begin() + m_headOffset + text.length(), '\n' );
		}
	} else {
		m_data.insert( m_data.begin() + m_headOffset, text.begin(), text.end() );
	}
}

void CmdSystem::TextBuffer::append( const wsw::StringView &text ) {
	m_data.insert( m_data.end(), text.begin(), text.end() );
	// TODO: Is this mandatory?
	if( !text.endsWith( '\n' ) && !text.endsWith( ';' ) ) {
		m_data.push_back( '\n' );
	}
}

auto CmdSystem::TextBuffer::fetchNextCmd() -> std::optional<wsw::StringView> {
	const unsigned oldHeadOffset = m_headOffset;

	bool isInsideQuotes       = false;
	bool hasPendingEscapeChar = false;
	unsigned numCmdChars      = 0;

	while( m_headOffset < m_data.size() ) {
		const char ch = m_data[m_headOffset];

		if( hasPendingEscapeChar ) [[unlikely]] {
			hasPendingEscapeChar = false;
		} else {
			if( ch == '"' ) {
				isInsideQuotes = !isInsideQuotes;
			} else if( ch == '\\' ) {
				hasPendingEscapeChar = true;
			}
		}

		++m_headOffset;

		if( ch == '\n' || ( !isInsideQuotes && ch == ';' ) ) {
			break;
		}

		++numCmdChars;
	}

	if( m_headOffset > oldHeadOffset ) [[likely]] {
		return wsw::StringView( m_data.data() + oldHeadOffset, numCmdChars );
	}

	return std::nullopt;
}

void CmdSystem::TextBuffer::shrinkToFit() {
	const unsigned charsToClear = m_headOffset;
	constexpr unsigned limit    = 4 * 4096u;
	m_data.erase( m_data.begin(), m_data.begin() + charsToClear );
	if( charsToClear > limit || m_data.size() > limit ) [[unlikely]] {
		m_data.shrink_to_fit();
	}
	m_headOffset = 0;
}

bool CmdSystem::registerCommand( const wsw::StringView &name, CmdFunc cmdFunc ) {
	checkCallingThread();

	if( name.empty() ) {
		comError() << "Failed to register a command: The name is empty";
		return false;
	}

	wsw::PodVector<char> ztNameChars;
	const char *nameChars = name.data();
	if( !name.isZeroTerminated() ) {
		ztNameChars.assign( name.data(), name.size() );
		nameChars = ztNameChars.data();
	}
	if( Cvar_String( nameChars )[0] ) {
		comError() << "Failed to register a command:" << name << "is already defined as a var";
		return false;
	}

	const wsw::HashedStringView nameAndHash( name );
	if( CmdEntry *existing = m_cmdEntries.findByName( nameAndHash ) ) {
		comDebug() << "The command" << name << "is already registered, just updating the handler";
		existing->m_cmdFunc = cmdFunc;
		return true;
	}

	void *const mem = ::malloc( sizeof( CmdEntry ) + nameAndHash.length() + 1 );
	if( !mem ) {
		comDebug() << "Failed to register a command: Allocation failure";
		return false;
	}

	auto *entry = new( mem )CmdEntry;
	auto *chars = (char *)( entry + 1 );
	nameAndHash.copyTo( chars, nameAndHash.size() + 1 );

	entry->m_nameAndHash = wsw::HashedStringView( chars, name.length(), wsw::StringView::ZeroTerminated );
	entry->m_cmdFunc     = cmdFunc;

	m_cmdEntries.insertUniqueTakingOwneship( entry );

	return true;
}

bool CmdSystem::unregisterCommand( const wsw::StringView &name ) {
	checkCallingThread();

	if( !name.empty() ) {
		if( CmdEntry *entry = m_cmdEntries.findByName( wsw::HashedStringView( name ) ) ) {
			m_cmdEntries.remove( entry );
			return true;
		} else {
			assert( name.isZeroTerminated() );
			comWarning() << "Failed to unregister a command: the name" << name << "is not registered";
			return false;
		}
	} else {
		comWarning() << "Failed to unregister a command: the name is empty";
		return false;
	}
}

void CmdSystem::executeBufferCommands() {
	checkCallingThread();

	m_aliasRecursionDepth = 0;

	while( const std::optional<wsw::StringView> maybeNextCmd = m_textBuffer.fetchNextCmd() ) {
		executeNow( *maybeNextCmd );
		if( m_interruptExecutionLoop ) {
			m_interruptExecutionLoop = false;
			break;
		}
	}

	m_textBuffer.shrinkToFit();
}

void CmdSystem::executeNow( const wsw::StringView &text ) {
	checkCallingThread();

	if( const CmdArgs &cmdArgs = m_argsSplitter.exec( text ); !cmdArgs.allArgs.empty() ) {
		// FIXME: This routine defines the order in which identifiers are looked-up, but
		// there are no checks for name-clashes. If a user sets a cvar with the name of
		// an existing command, alias, or dynvar, that cvar becomes shadowed!
		// We need a global namespace data-structure and a way to check for name-clashes
		// that does not break seperation of concerns.
		// Aiwa, 07-14-2006

		const wsw::HashedStringView testedName( cmdArgs[0] );
		if( const CmdEntry *cmdEntry = m_cmdEntries.findByName( testedName ) ) {
			if( cmdEntry->m_cmdFunc ) {
				cmdEntry->m_cmdFunc( cmdArgs );
			} else {
				// forward to server command
				wsw::StaticString<MAX_TOKEN_CHARS> forwardingBuffer;
				forwardingBuffer << wsw::StringView( "cmd " ) << text;
				executeNow( forwardingBuffer.asView() );
			}
		} else if( const AliasEntry *aliasEntry = m_aliasEntries.findByName( testedName ) ) {
			m_aliasRecursionDepth++;
			if( m_aliasRecursionDepth < 16 ) {
				prependCommand( wsw::StringView( "\n" ) );
				prependCommand( aliasEntry->m_text );
			} else {
				comError() << "Alias recursion depth has reached its limit";
				m_aliasRecursionDepth = 0;
			}
		} else if( !Cvar_Command( cmdArgs ) ) {
			comWarning() << "Unknown command:" << cmdArgs[0];
		}
	}
}

void CmdSystem::unregisterSystemCommands() {
	checkCallingThread();

	unregisterCommand( "exec"_asView );
	unregisterCommand( "echo"_asView );
	unregisterCommand( "alias"_asView );
	unregisterCommand( "aliasa"_asView );
	unregisterCommand( "unalias"_asView );
	unregisterCommand( "unaliasall"_asView );
	unregisterCommand( "wait"_asView );
	unregisterCommand( "vstr"_asView );
}

void CmdSystem::classifyExecutableCmdArgs( int argc, char **argv,
										   wsw::PodVector<wsw::StringView> *setArgs,
										   wsw::PodVector<wsw::StringView> *setAndExecArgs,
										   wsw::PodVector<std::optional<wsw::StringView>> *otherArgs ) {
	for( int i = 1; i < argc; ++i ) {
		const wsw::StringView arg( argv[i] );
		if( arg.startsWith( "+set"_asView ) ) {
			if( i + 2 < argc ) {
				const wsw::StringView args[] { arg.drop( 1 ), wsw::StringView( argv[i + 1] ), wsw::StringView( argv[i + 2] ) };
				setArgs->insert( setArgs->end(), std::begin( args ), std::end( args ) );
				setAndExecArgs->insert( setAndExecArgs->end(), std::begin( args ), std::end( args ) );
				i += 2;
			}
		} else if( arg.startsWith( "+exec"_asView ) ) {
			if( i + 1 < argc ) {
				setAndExecArgs->push_back( arg.drop( 1 ) );
				setAndExecArgs->push_back( wsw::StringView( argv[i + 1 ] ) );
				i += 1;
			}
		} else {
			if( arg.startsWith( '+' ) ) {
				if( arg.length() > 1 ) {
					otherArgs->push_back( std::nullopt );
					otherArgs->push_back( arg.drop( 1 ) );
				}
			} else {
				otherArgs->push_back( arg );
			}
		}
	}
}

void CmdSystem::appendEarlySetCommands( std::span<const wsw::StringView> args ) {
	checkCallingThread();

	assert( args.size() % 3 == 0 );
	for( size_t i = 0; i < args.size(); i += 3 ) {
		wsw::StaticString<MAX_TOKEN_CHARS> text;
		text << args[i + 0] << " \""_asView << args[i + 1] << "\" \""_asView << args[i + 2] << "\"\n"_asView;
		appendCommand( text.asView() );
	}
}

void CmdSystem::appendEarlySetAndExecCommands( std::span<const wsw::StringView> args ) {
	checkCallingThread();

	size_t i = 0;
	while( i < args.size() ) {
		const wsw::StringView &cmdName = args[0];
		if( cmdName.startsWith( "set"_asView ) ) {
			wsw::StaticString<MAX_TOKEN_CHARS> text;
			text << cmdName << " \""_asView << args[i + 1] << "\" \""_asView << args[i + 2] << "\"\n"_asView;
			appendCommand( text.asView() );
			i += 3;
		} else if( cmdName.startsWith( "exec"_asView ) ) {
			wsw::StaticString<MAX_TOKEN_CHARS> text;
			text << cmdName << " \""_asView << args[i + 1] << "\"\n"_asView;
			appendCommand( text.asView() );
			i += 2;
		} else {
			wsw::failWithLogicError( "Unexpected command name" );
		}
	}
}

void CmdSystem::appendLateCommands( std::span<const std::optional<wsw::StringView>> args ) {
	checkCallingThread();

	assert( !args.empty() );

	wsw::PodVector<char> text;
	for( size_t i = 0; i < args.size(); ++i ) {
		const std::optional<wsw::StringView> &argOrNewCmdSeparator = args[i];
		if( argOrNewCmdSeparator.has_value() ) {
			text.push_back( '"' );
			text.append( argOrNewCmdSeparator->data(), argOrNewCmdSeparator->size() );
			text.push_back( '"' );
		} else {
			text.push_back( '\n' );
		}
	}
	if( text.empty() || text.back() != '\n' ) {
		text.push_back( '\n' );
	}

	appendCommand( wsw::StringView( text.data(), text.size() ) );
}

void CmdSystem::helperForHandlerOfExec( const CmdArgs &cmdArgs ) {
	checkCallingThread();

	char *f = NULL, *name;
	const char *arg = Cmd_Argv( 1 );
	bool silent = Cmd_Argc() >= 3 && !Q_stricmp( Cmd_Argv( 2 ), "silent" );
	int len = -1, name_size;
	const char *basename;

	if( Cmd_Argc() < 2 || !arg[0] ) {
		comNotice() << "Usage: exec <filename>";
		return;
	}

	name_size = sizeof( char ) * ( strlen( arg ) + strlen( ".cfg" ) + 1 );
	name = (char *)Q_malloc( name_size );

	Q_strncpyz( name, arg, name_size );
	COM_SanitizeFilePath( name );

	if( !COM_ValidateRelativeFilename( name ) ) {
		if( !silent ) {
			comError() << "The filename" << wsw::StringView( name ) << "is invalid";
		}
		Q_free( name );
		return;
	}

	COM_DefaultExtension( name, ".cfg", name_size );

	basename = FS_BaseNameForFile( name );
	if( basename ) {
		len = FS_LoadBaseFile( basename, (void **)&f, NULL, 0 );
	}

	if( !f ) {
		if( !silent ) {
			comWarning() << "Couldn't execute" << wsw::StringView( name );
		}
		Q_free( name );
		return;
	}

	if( !silent ) {
		comNotice() << "Executing" << wsw::StringView( name );
	}

	prependCommand( "\n"_asView );
	if( len >= 3 && ( (uint8_t)f[0] == 0xEF && (uint8_t)f[1] == 0xBB && (uint8_t)f[2] == 0xBF ) ) {
		prependCommand( wsw::StringView( f + 3 ) ); // skip Windows UTF-8 marker
	} else {
		prependCommand( wsw::StringView( f ) );
	}

	FS_FreeFile( f );
	Q_free( name );
}

void CmdSystem::helperForHandlerOfEcho( const CmdArgs &cmdArgs ) {
	checkCallingThread();
	for( size_t i = 1; i < cmdArgs.allArgs.size(); ++i ) {
		Com_Printf( "%s", cmdArgs.allArgs[i].data() );
	}
	Com_Printf( "\n" );
}

void CmdSystem::helperForHandlerOfVstr( const CmdArgs &cmdArgs ) {
	checkCallingThread();

	if( Cmd_Argc() != 2 ) {
		comNotice() << "vstr <variable> : execute a variable command";
	} else {
		prependCommand( wsw::StringView( Cvar_String( Cmd_Argv( 1 ) ) ) );
	};
}

void CmdSystem::helperForHandlerOfWait( const CmdArgs & ) {
	checkCallingThread();
	m_interruptExecutionLoop = true;
}

[[maybe_unused]]
static auto writeAliasText( char *buffer, const CmdArgs &cmdArgs ) -> unsigned {
	unsigned totalTextSize = 0;
	for( int i = 2; i < cmdArgs.size(); ++i ) {
		const wsw::StringView &arg = cmdArgs[i];
		if( buffer ) {
			arg.copyTo( buffer + totalTextSize, arg.size() + 1 );
		}
		totalTextSize += arg.size();
		if( i + 1 < cmdArgs.size() ) {
			if( buffer ) {
				buffer[totalTextSize] = ' ';
			}
			totalTextSize++;
		}
	}
	if( buffer ) {
		buffer[totalTextSize] = '\0';
	}
	return totalTextSize;
}

void CmdSystem::helperForHandlerOfAlias( bool archive, const CmdArgs &cmdArgs ) {
	checkCallingThread();

	if( Cmd_Argc() < 2 ) {
		Com_Printf( "Usage: alias <name> <command>\n" );
		return;
	}

	constexpr auto kMaxNameLength = 32u;
	constexpr auto kMaxTextSize   = 1024u;

	const wsw::StringView &name = cmdArgs[1];
	assert( name.isZeroTerminated() );
	if( name.length() > kMaxNameLength ) {
		comError() << "Failed to register an alias: The alias name" << name << "is too long";
		return;
	}

	if( Cvar_String( name.data() )[0] ) {
		comError() << "Failed to register an alias: The name" << name << "is already defined as a var";
		return;
	}

	const wsw::HashedStringView nameAndHash( name );
	assert( nameAndHash.isZeroTerminated() );

	if( m_cmdEntries.findByName( nameAndHash ) ) {
		comError() << "Failed to register an alias: The name" << name << "is already defined as a command";
		return;
	}

	[[maybe_unused]] unsigned requiredTextSize = 0;
	if( Cmd_Argc() > 2 ) {
		requiredTextSize = writeAliasText( nullptr, cmdArgs );
		if( requiredTextSize > kMaxTextSize ) {
			comError() << "Failed to register an alias: The alias text is too long";
			return;
		}
	}

	AliasEntry *existing = m_aliasEntries.findByName( nameAndHash );
	if( existing ) {
		if( Cmd_Argc() == 2 ) {
			assert( existing->m_text.isZeroTerminated() );
			comNotice() << "alias" << nameAndHash << "is" << existing->m_text;
			if( archive ) {
				existing->m_isArchive = true;
			}
			return;
		}
		existing = m_aliasEntries.releaseOwnership( existing );
	} else {
		if( Cmd_Argc() == 2 ) {
			comError() << "Failed to register an alias: The alias text is empty";
			return;
		}
	}

	// Try reusing the same entry, as the reallocation could fail
	if( existing && existing->m_text.size() <= requiredTextSize ) {
		const auto textDataOffset  = (uintptr_t)existing->m_text.data() - (uintptr_t)existing;
		char *const writableText   = (char *)existing + textDataOffset;
		writeAliasText( writableText, cmdArgs );
		existing->m_text       = wsw::StringView( writableText, requiredTextSize, wsw::StringView::ZeroTerminated );
		existing->m_isArchive = archive;
		m_aliasEntries.insertUniqueTakingOwneship( existing );
		return;
	}

	// TODO: Try reallocating the old memory block?

	const size_t allocationSize = sizeof( AliasEntry ) + name.size() + 1 + requiredTextSize + 1;
	void *const mem = std::malloc( allocationSize );
	if( mem ) {
		if( existing ) {
			existing->~AliasEntry();
			std::free( existing );
		}
	} else {
		comError() << "Failed to register an alias: Allocation failure";
		if( existing ) {
			// TODO: Should we keep it in this case?
			m_aliasEntries.insertUniqueTakingOwneship( existing );
		}
		return;
	}

	auto *const newEntry     = new( mem )AliasEntry;
	auto *const writableName = (char *)( newEntry + 1 );
	auto *const writableText = (char *)( newEntry + 1 ) + name.size() + 1;

	name.copyTo( writableName, name.size() + 1 );
	const unsigned actualTextSize = writeAliasText( writableText, cmdArgs );
	assert( actualTextSize == requiredTextSize );

	newEntry->m_nameAndHash = wsw::HashedStringView( writableName, name.size(), wsw::StringView::ZeroTerminated );
	newEntry->m_text        = wsw::StringView( writableText, actualTextSize, wsw::StringView::ZeroTerminated );
	newEntry->m_isArchive   = archive;

	m_aliasEntries.insertUniqueTakingOwneship( newEntry );
}

void CmdSystem::helperForHandlerOfUnalias( const CmdArgs &cmdArgs ) {
	checkCallingThread();

	if( Cmd_Argc() < 2 ) {
		comNotice() << "Usage: unalias <name>";
		return;
	}

	for( int i = 1; i < cmdArgs.size(); ++i ) {
		const wsw::StringView &arg = cmdArgs[i];
		if( AliasEntry *entry = m_aliasEntries.findByName( wsw::HashedStringView( arg ) ) ) {
			m_aliasEntries.remove( entry );
		} else {
			comError() << "Failed to unalias an alias: Failed to find an alias with name" << arg;
		}
	}
}

void CmdSystem::helperForHandlerOfUnaliasall( const CmdArgs &cmdArgs ) {
	checkCallingThread();
	m_aliasEntries.clear();
}