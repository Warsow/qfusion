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

#include "wswstringview.h"

namespace wsw {

class ProfilerScopeName {
	friend class ProfilerScope;
	friend class ProfilingSystem;
protected:
	explicit ProfilerScopeName( const wsw::HashedStringView &name ) : m_name( name ) {}

	static void doRegisterSelf( const wsw::StringView &file, int line, const wsw::StringView &function );

	wsw::HashedStringView m_name;
};

class ProfilerScope {
public:
	explicit ProfilerScope( const ProfilerScopeName &name );
	~ProfilerScope();

	ProfilerScope( const ProfilerScope & ) = delete;
	auto operator=( const ProfilerScope & ) -> ProfilerScope & = delete;
	ProfilerScope( ProfilerScope && ) = delete;
	auto operator=( ProfilerScope && ) -> ProfilerScope & = delete;
private:
	ProfilerScopeName m_name;
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

#ifndef _MSC_VER
#define WSW_PROFILER_SCOPE() \
class MAKE_UNIQUE_NAME( Scope ) : \
	public wsw::ProfilerScopeName, wsw::ScopeRegistrator<MAKE_UNIQUE_NAME( Scope )> { \
public: \
	MAKE_UNIQUE_NAME( Scope )() : wsw::ProfilerScopeName( wsw::HashedStringView( __PRETTY_FUNCTION__ ) ) {} \
	static void registerSelf() { doRegisterSelf( wsw::StringView( __FILE__ ), __LINE__, wsw::StringView( __PRETTY_FUNCTION__ ) ); } \
} MAKE_UNIQUE_NAME( _scopeName ); \
[[maybe_unused]] volatile wsw::ProfilerScope MAKE_UNIQUE_NAME( _scope )( MAKE_UNIQUE_NAME( _scopeName ) )
#else
#define WSW_PROFILER_SCOPE() \
class MAKE_UNIQUE_NAME( Scope ) : \
	public wsw::ProfilerScopeName, wsw::ScopeRegistrator<MAKE_UNIQUE_NAME( Scope )> { \
public: \
	MAKE_UNIQUE_NAME( Scope )() : wsw::ProfilerScopeName( wsw::HashedStringView( __FUNCSIG__ ) ) {} \
	static void registerSelf() { doRegisterSelf( wsw::StringView( __FILE__ ), __LINE__, wsw::StringView( __FUNCSIG__ ) ); } \
} MAKE_UNIQUE_NAME( _scopeName ); \
[[maybe_unused]] volatile wsw::ProfilerScope MAKE_UNIQUE_NAME( _scope )( MAKE_UNIQUE_NAME( _scopeName ) )
#endif

#endif
