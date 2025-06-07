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

#ifndef WSW_3ab65fca_320a_4ff1_8592_259b7bf8c026_H
#define WSW_3ab65fca_320a_4ff1_8592_259b7bf8c026_H

#include <common/types/stringspanstorage.h>

struct CmdArgs;

class CompletionResult {
public:
	using const_iterator = wsw::StringSpanStorage<unsigned, unsigned>::const_iterator;

	void add( const wsw::StringView &entry ) {
		const unsigned oldSize = m_storage.size();
		m_storage.add( entry );
		// TODO: Allow skipping these computations if strings are already naturally classified?
		if( oldSize == 0 ) [[unlikely]] {
			m_currLongestPrefixIndex  = 0;
			m_currLongestPrefixLength = entry.length();
		} else {
			// Don't bother doing further checks if it's already zero
			if( m_currLongestPrefixLength > 0 ) [[likely]] {
				const wsw::StringView &prefix = m_storage[m_currLongestPrefixIndex].take( m_currLongestPrefixLength );
				const auto length             = (unsigned)prefix.getCommonPrefixLength( entry, wsw::IgnoreCase );
				if( m_currLongestPrefixLength > length ) [[unlikely]] {
					m_currLongestPrefixLength = length;
					m_currLongestPrefixIndex  = oldSize;
				}
			}
		}
	}

	[[nodiscard]] bool empty() const { return m_storage.empty(); }
	[[nodiscard]] auto size() const -> unsigned { return m_storage.size(); }

	[[nodiscard]]
	auto getCommonPrefix() const -> wsw::StringView {
		return m_storage[m_currLongestPrefixIndex].take( m_currLongestPrefixLength );
	}

	[[nodiscard]] auto begin() const -> const_iterator { return m_storage.begin(); }
	[[nodiscard]] auto end() const -> const_iterator { return m_storage.end(); }
	[[nodiscard]] auto cbegin() const -> const_iterator { return m_storage.cbegin(); }
	[[nodiscard]] auto cend() const -> const_iterator { return m_storage.cend(); }

	[[nodiscard]] auto front() const -> wsw::StringView { return m_storage.front(); }
	[[nodiscard]] auto back() const -> wsw::StringView { return m_storage.back(); }

	[[nodiscard]]
	auto operator[]( unsigned index ) const -> wsw::StringView { return m_storage[index]; }
private:
	unsigned m_currLongestPrefixIndex { 0 };
	unsigned m_currLongestPrefixLength { 0 };
	wsw::StringSpanStorage<unsigned, unsigned> m_storage;
};

using CmdFunc = void (*)( const CmdArgs & );
// This is what has to be implemented for an individual command
using CompletionQueryFunc = CompletionResult (*)( const wsw::StringView &partial );
// Initiates completion processing, different for client and server command buffers
using CompletionExecutionFunc = void (*)( const wsw::StringView &name, unsigned requestId,
	const wsw::StringView &partial, CompletionQueryFunc queryFunc );

// These subroutines should be used if executing a command via both server and client command systems
// by the same handler makes sense and is correct. Cvar-related commands are an obvious example.
void Cmd_AddClientAndServerCommand( const char *cmd_name, CmdFunc cmdFunc, CompletionQueryFunc completionFunc = nullptr );
void Cmd_RemoveClientAndServerCommand( const char *cmd_name );

#define Cmd_Argc()      ( cmdArgs.size() )
#define Cmd_Argv( arg ) ( cmdArgs[arg].data() )
#define Cmd_Args()      ( cmdArgs.argsString.data() )

#endif