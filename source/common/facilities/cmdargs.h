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

#ifndef WSW_81738fef_c52c_4bac_8def_4718a39b5500_H
#define WSW_81738fef_c52c_4bac_8def_4718a39b5500_H

#include <common/types/stringview.h>
#include <span>

struct CmdArgs {
	std::span<const wsw::StringView> allArgs;
	wsw::StringView argsString;

	[[nodiscard]]
	auto size() const -> int { return (int)allArgs.size(); }

	[[nodiscard]]
	auto operator[]( size_t index ) const -> wsw::StringView {
		return index < allArgs.size() ? allArgs[index] : wsw::StringView();
	}
};

#endif