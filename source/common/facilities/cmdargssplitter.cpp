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

#include "cmdargssplitter.h"
#include <common/helpers/parsecompat.h>

auto CmdArgsSplitter::exec( const wsw::StringView &cmdString ) -> CmdArgs {
	if( cmdString.empty() ) [[unlikely]] {
		return CmdArgs {};
	}

	// We have to make a deep copy for Com_Parse()
	// TODO: Get rid of Com_Parse()
	m_tmpStringBuffer.assign( cmdString.begin(), cmdString.end() );
	m_tmpStringBuffer.push_back( '\0' );

	m_argsViewsBuffer.clear();
	m_argsDataBuffer.clear();
	m_tmpSpansBuffer.clear();
	m_argsStringBuffer.clear();

	const char *const oldArgsDataAddress = m_argsDataBuffer.data();
	bool hadArgsDataReallocations        = false;

	wsw::StringView rest( m_tmpStringBuffer.data(), m_tmpStringBuffer.size() - 1, wsw::StringView::ZeroTerminated );
	std::optional<wsw::StringView> argsStringStart;

	for(;; ) {
		// Skip whitespace characters, except '\n' which terminates commands in the buffer
		// TODO: Should the buffer truncate the trailing '\n' on its own?
		rest = rest.dropWhile( []( char ch ) {
			// TODO: should dropWhile() take care of this?
			if( ch == '\0' ) [[unlikely]] {
				return false;
			}
			return ( (unsigned)ch <= (unsigned)' ' ) && ch != '\n';
		});

		// Stop upon finding a newline
		if( rest.empty() || rest.startsWith( '\n' ) ) {
			break;
		}

		if( m_tmpSpansBuffer.size() == 1 ) [[unlikely]] {
			argsStringStart = rest;
		}

		// TODO: Get rid of COM_Parse()
		char *parsePtr    = m_tmpStringBuffer.data() + ( rest.data() - m_tmpStringBuffer.data() );
		const char *token = COM_Parse( &parsePtr );
		if( !parsePtr ) {
			break;
		}

		const auto tokenLen = (unsigned)std::strlen( token );
		const auto oldSize  = (unsigned)m_argsDataBuffer.size();
		// Include the trailing "\0" of the parsed token
		m_argsDataBuffer.insert( m_argsDataBuffer.end(), token, token + tokenLen + 1 );
		hadArgsDataReallocations |= ( m_argsDataBuffer.data() != oldArgsDataAddress );

		m_tmpSpansBuffer.push_back( { oldSize, tokenLen } );
		// Append a view optimistically, assuming there won't be relocations
		if( !hadArgsDataReallocations ) {
			m_argsViewsBuffer.push_back( { m_argsDataBuffer.data() + oldSize, tokenLen, wsw::StringView::ZeroTerminated } );
		}

		// TODO...
		rest = wsw::StringView( parsePtr );
	}

	if( m_tmpSpansBuffer.empty() ) [[unlikely]] {
		return CmdArgs {};
	}

	// Patch string views if there were data relocations.
	// This should be rarely needed assuming reuse of the splitter instance.
	if( hadArgsDataReallocations ) {
		m_argsViewsBuffer.clear();
		for( const auto &[off, len]: m_tmpSpansBuffer ) {
			m_argsViewsBuffer.push_back( { m_argsDataBuffer.data() + off, len, wsw::StringView::ZeroTerminated } );
		}
	}

	assert( m_argsViewsBuffer.size() == m_tmpSpansBuffer.size() );

	if( argsStringStart ) {
		const wsw::StringView trimmedArgsString = argsStringStart->trimRight();
		m_argsStringBuffer.append( trimmedArgsString.data(), trimmedArgsString.size() );
	}
	m_argsStringBuffer.push_back( '\0' );

	return CmdArgs {
		.allArgs    = m_argsViewsBuffer,
		.argsString = { m_argsStringBuffer.data(), m_argsStringBuffer.size() - 1, wsw::StringView::ZeroTerminated },
	};
}