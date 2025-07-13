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

#ifndef WSW_3505df04_4292_451b_a34d_f51d56b7bdcc_H
#define WSW_3505df04_4292_451b_a34d_f51d56b7bdcc_H

#include <utility>

namespace wsw {

template <typename T, typename Ops>
class ScopedResource {
public:
	ScopedResource() = default;
	explicit ScopedResource( T &&value ) : m_value( std::forward<T>( value ) ) {}
	ScopedResource( T &&value, Ops &&ops ) : m_value( std::forward<T>( value ) ), m_ops( std::forward<Ops>( ops ) ) {}
	~ScopedResource() { destroy(); }

	ScopedResource( const ScopedResource<T, Ops> & ) = delete;
	auto operator=( const ScopedResource<T, Ops> & ) -> ScopedResource<T, Ops> & = delete;

	ScopedResource( ScopedResource<T, Ops> &&that ) noexcept {
		moveFrom( that );
	}

	[[maybe_unused]]
	auto operator=( ScopedResource<T, Ops> &&that ) noexcept -> ScopedResource<T, Ops> & {
		destroy();
		moveFrom( that );
		return *this;
	}

	[[nodiscard]]
	auto get() -> T & { return m_value; }
	[[nodiscard]]
	auto get() const -> const T & { return m_value; }

	[[nodiscard]]
	auto operator->() -> T & requires std::is_pointer_v<T> { return m_value; }
	[[nodiscard]]
	auto operator->() const -> const T & requires std::is_pointer_v<T> { return m_value; }

#if 0
	[[nodiscard]]
	auto take() -> T && {
		T result = std::move( m_value );
		m_value  = Ops::emptyValue();
		return std::forward<T>( result );
	}
#endif

	[[nodiscard]]
	operator bool() const { return Ops::isPresent( m_value ); }

private:
	void destroy() {
		// Ensure that we don't try to use moved/empty ops
		if( Ops::isPresent( m_value ) ) {
			m_ops.destroy( m_value );
		}
	}
	void moveFrom( ScopedResource<T, Ops> &that ) {
		m_value      = std::forward<T>( that.m_value );
		that.m_value = Ops::emptyValue();
		m_ops        = std::forward<Ops>( that.m_ops );
		that.m_ops   = {};
	}

	T m_value {};
	[[no_unique_address]] Ops m_ops {};
};

template <typename T>
struct DefaultScopedPointerOps {
	void destroy( T *value ) { delete value; }
	[[nodiscard]]
	static auto emptyValue() -> T * { return nullptr; }
	[[nodiscard]]
	static bool isPresent( const T *value ) { return value != nullptr; }
};

template <typename T>
using ScopedPointer = ScopedResource<T *, DefaultScopedPointerOps<T>>;

}

#endif