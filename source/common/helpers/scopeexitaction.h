/*
Copyright (C) 2025 Chasseur de bots

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

#ifndef WSW_993e70bd_e9a4_45af_9345_aba046fefa1e_H
#define WSW_993e70bd_e9a4_45af_9345_aba046fefa1e_H

#include <utility>
#include <common/types/function.h>

namespace wsw {

template <size_t StorageSize = 32>
class ScopeExitAction {
public:
	ScopeExitAction() = default;
	~ScopeExitAction() {
		if( m_fn ) {
			m_fn();
		}
	}

	template <typename Fn>
	explicit ScopeExitAction( Fn &&fn ) : m_fn( std::forward<Fn>( fn ) ) {}

	template <typename Fn>
	[[maybe_unused]]
	auto operator=( Fn &&fn ) -> ScopeExitAction & {
		m_fn = std::forward<Fn>( fn );
		return *this;
	}

	ScopeExitAction( const ScopeExitAction<StorageSize> & ) = delete;
	auto operator=( const ScopeExitAction<StorageSize> & ) -> ScopeExitAction & = delete;
	ScopeExitAction( ScopeExitAction<StorageSize> && ) = delete;
	auto operator=( ScopeExitAction<StorageSize> && ) -> ScopeExitAction & = delete;

	void cancel() { m_fn = nullptr; }
private:
	wsw::Function<void(), StorageSize> m_fn;
};

}

#endif

