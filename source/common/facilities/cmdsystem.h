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

#ifndef WSW_a84d2200_4f2e_4b79_bc26_b210c74cbdb6_H
#define WSW_a84d2200_4f2e_4b79_bc26_b210c74cbdb6_H

#include "cmdargssplitter.h"
#include "cmdcompat.h"
#include <common/types/mapofboxednamedentries.h>
#include <common/helpers/qthreads.h>

class CmdSystem : protected CallingThreadChecker {
public:
	virtual ~CmdSystem() = default;

	// TODO: Check whether no command text is submitted partially, track submitted boundaries

	void prependCommand( const wsw::StringView &text ) {
		m_textBuffer.prepend( text );
	}
	void appendCommand( const wsw::StringView &text ) {
		m_textBuffer.append( text );
	}

	void executeNow( const wsw::StringView &text );

	[[maybe_unused]]
	virtual bool registerCommand( const wsw::StringView &name, CmdFunc cmdFunc );
	[[maybe_unused]]
	virtual bool unregisterCommand( const wsw::StringView &name );

	[[nodiscard]]
	bool isARegisteredCommand( const wsw::HashedStringView &name ) const {
		return m_cmdEntries.constFindByName( name ) != nullptr;
	}
	[[nodiscard]]
	bool isARegisteredAlias( const wsw::HashedStringView &name ) const {
		return m_aliasEntries.constFindByName( name ) != nullptr;
	}

	void executeBufferCommands();
	void interuptExecutionLoop() { m_interruptExecutionLoop = true; };

	virtual void registerSystemCommands() = 0;
	virtual void unregisterSystemCommands();

	static void classifyExecutableCmdArgs( int argc, char **argv,
										   wsw::PodVector<wsw::StringView> *setArgs,
										   wsw::PodVector<wsw::StringView> *setAndExecArgs,
										   wsw::PodVector<std::optional<wsw::StringView>> *otherArgs );

	void appendEarlySetCommands( std::span<const wsw::StringView> args );
	void appendEarlySetAndExecCommands( std::span<const wsw::StringView> args );
	void appendLateCommands( std::span<const std::optional<wsw::StringView>> args );

	using CallingThreadChecker::markCurrentThreadForFurtherAccessChecks;
	using CallingThreadChecker::clearThreadForFurtherAccessChecks;
protected:
	void helperForHandlerOfExec( const CmdArgs &cmdArgs );
	void helperForHandlerOfEcho( const CmdArgs &cmdArgs );
	void helperForHandlerOfAlias( bool archive, const CmdArgs &cmdArgs );
	void helperForHandlerOfUnalias( const CmdArgs &cmdArgs );
	void helperForHandlerOfUnaliasall( const CmdArgs &cmdArgs );
	void helperForHandlerOfWait( const CmdArgs &cmdArgs );
	void helperForHandlerOfVstr( const CmdArgs &cmdArgs );

	struct CmdEntry : public BoxedHashMapNamedEntry<CmdEntry>, public BoxedListEntry<CmdEntry> {
		friend class CmdSystem;
		CmdFunc m_cmdFunc { nullptr };
	};

	struct AliasEntry : public BoxedHashMapNamedEntry<AliasEntry>, public BoxedListEntry<AliasEntry> {
		friend class CmdSystem;
		wsw::StringView m_text;
		bool m_isArchive { false };
	};

	class TextBuffer {
	public:
		void prepend( const wsw::StringView &text );
		void append( const wsw::StringView &text );
		[[nodiscard]]
		auto fetchNextCmd() -> std::optional<wsw::StringView>;
		void shrinkToFit();
	private:
		wsw::PodVector<char> m_data;
		unsigned m_headOffset { 0 };
	};

	MapOfBoxedNamedEntries<CmdEntry, 51, wsw::IgnoreCase> m_cmdEntries;
	MapOfBoxedNamedEntries<AliasEntry, 37, wsw::IgnoreCase> m_aliasEntries;

	TextBuffer m_textBuffer;
	CmdArgsSplitter m_argsSplitter;

	unsigned m_aliasRecursionDepth { 0 };
	bool m_interruptExecutionLoop { false };
};

#endif