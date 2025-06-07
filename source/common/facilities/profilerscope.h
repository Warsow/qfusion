/*
Copyright (C) 2024-2025 Chasseur de bots

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

#ifndef WSW_e401c6b9_39bb_4196_8594_bbd69d226b6d_H
#define WSW_e401c6b9_39bb_4196_8594_bbd69d226b6d_H

#include <common/types/stringview.h>

namespace wsw {

class ProfilerScopeLabel {
	friend class ProfilerScope;
	friend class ProfilingSystem;
	friend class ProfilerThreadInstance;
protected:
	ProfilerScopeLabel( const wsw::HashedStringView &function, int line ) : m_function( function ), m_line( line ) {}

	static void doRegisterSelf( const wsw::StringView &file, int line, const wsw::HashedStringView &function );

	wsw::HashedStringView m_function;
	int m_line;
};

class ProfilerScope {
	friend class ProfilerThreadInstance;
public:
	explicit ProfilerScope( const ProfilerScopeLabel &m_label );
	~ProfilerScope();

	ProfilerScope( const ProfilerScope & ) = delete;
	auto operator=( const ProfilerScope & ) -> ProfilerScope & = delete;
	ProfilerScope( ProfilerScope && ) = delete;
	auto operator=( ProfilerScope && ) -> ProfilerScope & = delete;
private:
	ProfilerScopeLabel m_label;
};

// http://quantumgraphics.blogspot.com/2014/11/abusing-static-initialization.html
template <typename T>
class ScopeRegistrator {
protected:
	ScopeRegistrator() {
		// https://stackoverflow.com/a/27672755
		(void)s_proxy;
	}
private:
	struct Proxy {
		Proxy() { T::registerSelf(); }
	};
	[[maybe_unused]] static Proxy s_proxy;
};

template<typename T> typename ScopeRegistrator<T>::Proxy ScopeRegistrator<T>::s_proxy;

}

// https://stackoverflow.com/a/71899854
#define MAKE_UNIQUE_NAME_CONCAT_( prefix, suffix ) prefix##suffix
#define MAKE_UNIQUE_NAME_CONCAT( prefix, suffix ) MAKE_UNIQUE_NAME_CONCAT_( prefix, suffix )
#define MAKE_UNIQUE_NAME( prefix ) MAKE_UNIQUE_NAME_CONCAT( prefix##_, __LINE__ )

#define WSW_PROFILER_SCOPE_IMPL( file, line, functionMagic ) \
	static constexpr const char *MAKE_UNIQUE_NAME( functionName ) = functionMagic; \
class MAKE_UNIQUE_NAME( Label ) : \
	public wsw::ProfilerScopeLabel, wsw::ScopeRegistrator<MAKE_UNIQUE_NAME( Label )> { \
public: \
	MAKE_UNIQUE_NAME( Label )() : \
		wsw::ProfilerScopeLabel( wsw::HashedStringView( MAKE_UNIQUE_NAME( functionName ) ), line ) {} \
	static void registerSelf() {                                 \
		doRegisterSelf( wsw::StringView( file ), line, wsw::HashedStringView( MAKE_UNIQUE_NAME( functionName ) ) ); \
	} \
} MAKE_UNIQUE_NAME( _labelName ); \
[[maybe_unused]] volatile wsw::ProfilerScope MAKE_UNIQUE_NAME( _label )( MAKE_UNIQUE_NAME( _labelName ) )

#ifndef _MSC_VER
#define WSW_PROFILER_SCOPE() WSW_PROFILER_SCOPE_IMPL( __FILE__, __LINE__, __PRETTY_FUNCTION__ )
#else
#define WSW_PROFILER_SCOPE() WSW_PROFILER_SCOPE_IMPL( __FILE__, __LINE__, __FUNCSIG__ )
#endif

#endif
