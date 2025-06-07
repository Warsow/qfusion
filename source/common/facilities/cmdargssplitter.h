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

#ifndef WSW_168e957c_1b27_4bbe_9cc7_65cf2ba6842a_H
#define WSW_168e957c_1b27_4bbe_9cc7_65cf2ba6842a_H

#include "cmdargs.h"

#include <common/types/podvector.h>
#include <utility>

// Instances hold temporary buffers and are supposed to be reused.
class CmdArgsSplitter {
public:
	// The result lifetime is tied to this splitter object instance.
	[[nodiscard]]
	auto exec( const wsw::StringView &cmdString ) -> CmdArgs;
private:
	wsw::PodVector<wsw::StringView> m_argsViewsBuffer;
	wsw::PodVector<std::pair<unsigned, unsigned>> m_tmpSpansBuffer;
	wsw::PodVector<char> m_tmpStringBuffer;
	wsw::PodVector<char> m_argsDataBuffer;
	wsw::PodVector<char> m_argsStringBuffer;
};

#endif
