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

#include "wswprofiler.h"
#include "wswpodvector.h"
#include "wswexceptions.h"
#include "profilerscope.h"
#include "links.h"
#include "local.h"
#include "common.h"
#include "qthreads.h"

#include <unordered_map>
#include <unordered_set>

template <>
struct std::hash<wsw::HashedStringView> {
	[[nodiscard]]
	auto operator()( const wsw::HashedStringView &view ) const noexcept -> std::size_t {
		return view.getHash();
	}
};

namespace wsw {

class ProfilerThreadInstance {
public:
	friend class ProfilerScope;
	friend class ProfilingSystem;

	explicit ProfilerThreadInstance( wsw::ProfilingSystem::FrameGroup group_ ) : group( group_ ) {}

	ProfilerThreadInstance *prev { nullptr }, *next { nullptr };

	void enterScope( ProfilerScope *scope, const wsw::HashedStringView &name );
	void leaveScope( ProfilerScope *scope, const wsw::HashedStringView &name );

	void dumpFrameStats( unsigned threadIndex, ProfilerResultSink *dataSink );

	struct DescendantEntry {
		uint64_t enterTimestamp { 0 };
		uint64_t accumTime { 0 };
		int enterCount { 0 };
	};

	// Reset each frame
	wsw::PodVector<char> m_targetName;

	std::unordered_set<wsw::HashedStringView> m_frameRoots;

	const wsw::ProfilingSystem::FrameGroup group;

	uint64_t m_enterTimestamp { 0 };
	uint64_t m_accumTime { 0 };
	int m_enterCount { 0 };

	int m_globalScopeDepth { 0 };
	int m_targetScopeReentrancyCounter { 0 };
	int m_scopeDepthFromTheTargetScope { 0 };

	bool m_discoverRootScopes { false };

	std::unordered_map<wsw::HashedStringView, DescendantEntry> m_statsOfDescendantScopes;
};

void ProfilerThreadInstance::dumpFrameStats( unsigned threadIndex, ProfilerResultSink *dataSink ) {
	if( m_discoverRootScopes ) {
		assert( m_globalScopeDepth == 0 );
		for( const auto &name: m_frameRoots ) {
			dataSink->addDiscoveredRoot( threadIndex, name );
		}
	} else {
		dataSink->addCallStats( threadIndex, { m_targetName.data(), m_targetName.size() },
								{ .totalTime = m_accumTime, .enterCount = m_enterCount } );
		for( const auto &[k, v] : m_statsOfDescendantScopes ) {
			dataSink->addCallChildStats( threadIndex, k, { .totalTime = v.accumTime, .enterCount = v.enterCount } );
		}
	}

	m_targetName.clear();
	m_frameRoots.clear();

	m_discoverRootScopes = false;
	m_globalScopeDepth   = 0;

	m_enterTimestamp = 0;
	m_accumTime      = 0;
	m_enterCount     = 0;

	m_targetScopeReentrancyCounter = 0;
	m_scopeDepthFromTheTargetScope = 0;

	m_statsOfDescendantScopes.clear();
}

static thread_local ProfilerThreadInstance *tl_profilerThreadInstance;

ProfilerScope::ProfilerScope( const ProfilerScopeName &name ) : m_name( name ) {
	// That's why these fields are of non-boolean type
	if( ProfilingSystem::s_isProfilingEnabled[0] | ProfilingSystem::s_isProfilingEnabled[1] ) {
		// We avoid touching thread-locals by checking the outer condition first
		if( ProfilerThreadInstance *instance = tl_profilerThreadInstance ) {
			assert( instance->group == 0 || instance->group == 1 );
			if( ProfilingSystem::s_isProfilingEnabled[instance->group] ) {
				instance->enterScope( this, m_name.m_name );
			}
		}
	}
}

ProfilerScope::~ProfilerScope() {
	if( ProfilingSystem::s_isProfilingEnabled[0] | ProfilingSystem::s_isProfilingEnabled[1] ) {
		if( ProfilerThreadInstance *instance = tl_profilerThreadInstance ) {
			assert( instance->group == 0 || instance->group == 1 );
			if( ProfilingSystem::s_isProfilingEnabled[instance->group] ) {
				instance->leaveScope( this, m_name.m_name );
			}
		}
	}
}

static wsw::PodVector<ProfilingSystem::RegisteredScope> g_registeredScopes;

void ProfilerScopeName::doRegisterSelf( const wsw::StringView &givenFile, int line, const wsw::StringView &givenFunction ) {
	wsw::StringView file = givenFile;
	for( const wsw::StringView &sourceRoot: { "/source/"_asView, "\\source\\"_asView } ) {
		if( const std::optional<unsigned> maybeIndex = file.indexOf( sourceRoot ) ) {
			file = file.drop( *maybeIndex + sourceRoot.length() );
			break;
		}
	}

	wsw::StringView readableFunction = givenFunction;
	if( const std::optional<unsigned> maybeIndex = readableFunction.lastIndexOf( "::Scope_"_asView ) ) {
		readableFunction = readableFunction.take( *maybeIndex );
	}
	// TODO: Should we care of displaying overloads?
	if( const std::optional<unsigned> maybeIndex = readableFunction.lastIndexOf( '(' ) ) {
		readableFunction = readableFunction.take( *maybeIndex );
	}
	// Note: This has to be performed after stripping arguments
	if( const std::optional<unsigned> maybeIndex = readableFunction.lastIndexOf( ' ' ) ) {
		readableFunction = readableFunction.drop( *maybeIndex + 1 );
	}

	g_registeredScopes.emplace_back( ProfilingSystem::RegisteredScope {
		.file             = file,
		.line             = line,
		.exactFunction    = givenFunction,
		.readableFunction = readableFunction,
	});
}

ProfilerThreadInstance *ProfilingSystem::s_instances[2];
volatile unsigned ProfilingSystem::s_isProfilingEnabled[2];

static wsw::Mutex g_mutex;

auto ProfilingSystem::getRegisteredScopes() -> std::span<const RegisteredScope> {
	return g_registeredScopes;
}

void ProfilingSystem::attachToThisThread( FrameGroup group ) {
	assert( group == 0 || group == 1 );

	if( tl_profilerThreadInstance ) {
		wsw::failWithLogicError( "Already attached to this thread" );
	}

	auto *newInstance = new ProfilerThreadInstance( group );
	do {
		// Even if we split groups, we still have to guard the linked list by mutex
		[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &g_mutex );
		wsw::link( newInstance, &s_instances[group] );
	} while( false );

	tl_profilerThreadInstance = newInstance;
}

void ProfilingSystem::detachFromThisThread( FrameGroup group ) {
	assert( group == 0 || group == 1 );

	if( auto *instance = tl_profilerThreadInstance ) {
		do {
			[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &g_mutex );
			wsw::unlink( instance, &s_instances[group] );
		} while( false );

		delete instance;
		tl_profilerThreadInstance = nullptr;
	}
}

void ProfilingSystem::beginFrame( FrameGroup group, ProfilerArgsSupplier *argsSupplier ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &g_mutex );

	assert( group == 0 || group == 1 );
	// TODO: Use scope-guards
	argsSupplier->beginSupplyingArgs();
	try {
		s_isProfilingEnabled[group] = false;
		const ProfilerArgs &args = argsSupplier->getArgs( group );
		if( const auto *targetName = std::get_if<wsw::StringView>( &args.args ) ) {
			assert( !targetName->empty() );
			for( ProfilerThreadInstance *instance = s_instances[group]; instance; instance = instance->next ) {
				instance->m_targetName.assign( *targetName );
			}
			s_isProfilingEnabled[group] = true;
		} else if( std::holds_alternative<ProfilerArgs::DiscoverRootScopes>( args.args ) ) {
			for( ProfilerThreadInstance *instance = s_instances[group]; instance; instance = instance->next ) {
				instance->m_discoverRootScopes = true;
			}
			s_isProfilingEnabled[group] = true;
		}
	} catch( ... ) {
		argsSupplier->endSupplyingArgs();
		throw;
	}
	argsSupplier->endSupplyingArgs();
}

void ProfilingSystem::endFrame( FrameGroup group, ProfilerResultSink *resultSink ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &g_mutex );

	unsigned totalThreads = 0;
	for( ProfilerThreadInstance *instance = s_instances[group]; instance; instance = instance->next ) {
		totalThreads++;
	}

	resultSink->beginAcceptingResults( group, totalThreads );
	try {
		if( s_isProfilingEnabled[group] ) {
			unsigned threadIndex = 0;
			for( ProfilerThreadInstance *instance = s_instances[group]; instance; instance = instance->next ) {
				instance->dumpFrameStats( threadIndex, resultSink );
				threadIndex++;
			}
		}
	} catch( ... ) {
		resultSink->endAcceptingResults( group );
		throw;
	}

	resultSink->endAcceptingResults( group );
}

[[nodiscard]]
static inline bool matchesName( const wsw::PodVector<char> &expected, const wsw::StringView &given ) {
	return expected.size() == given.size() && std::memcmp( expected.data(), given.data(), expected.size() ) == 0;
}

void ProfilerThreadInstance::enterScope( ProfilerScope *, const wsw::HashedStringView &name ) {
	if( m_discoverRootScopes ) {
		m_globalScopeDepth++;
		if( m_globalScopeDepth == 1 ) {
			m_frameRoots.insert( name );
		}
	} else {
		assert( !m_targetName.empty() );
		// If we are in the target scope
		if( m_targetScopeReentrancyCounter > 0 ) {
			m_scopeDepthFromTheTargetScope++;
		}
		if( matchesName( m_targetName, name ) ) {
			m_targetScopeReentrancyCounter++;
			if( m_targetScopeReentrancyCounter == 1 ) {
				m_enterTimestamp = Sys_Microseconds();
				m_enterCount++;
			}
		} else {
			// If it's a direct call from the target scope
			if( m_scopeDepthFromTheTargetScope == 1 ) {
				DescendantEntry &entry = m_statsOfDescendantScopes[name];
				entry.enterTimestamp = Sys_Microseconds();
				entry.enterCount++;
			}
		}
	}
}

void ProfilerThreadInstance::leaveScope( ProfilerScope *, const wsw::HashedStringView &name ) {
	if( m_discoverRootScopes ) {
		m_globalScopeDepth--;
	} else {
		assert( !m_targetName.empty() );
		// If we are in the target scope
		const bool isInTargetScope = m_targetScopeReentrancyCounter > 0;
		if( isInTargetScope ) {
			m_scopeDepthFromTheTargetScope--;
		}
		if( matchesName( m_targetName, name ) ) {
			m_targetScopeReentrancyCounter--;
			if( m_targetScopeReentrancyCounter == 0 ) {
				m_accumTime += ( Sys_Microseconds() - m_enterTimestamp );
			}
		} else {
			// If it's a direct call from the target scope
			if( isInTargetScope && m_scopeDepthFromTheTargetScope == 0 ) {
				DescendantEntry &entry = m_statsOfDescendantScopes[name];
				entry.accumTime += ( Sys_Microseconds() - entry.enterTimestamp );
			}
		}
	}
}

}