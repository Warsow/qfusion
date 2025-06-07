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

#ifndef WSW_462961de_963c_4da6_b80b_b0ff67392202_H
#define WSW_462961de_963c_4da6_b80b_b0ff67392202_H

#include <common/types/stringview.h>
#include <common/types/podvector.h>
#include <optional>
#include <span>
#include <variant>

namespace wsw {

class ProfilerThreadInstance;
class ProfilerArgsSupplier;
class ProfilerResultSink;

struct ProfilerArgs {
	struct DiscoverRootScopes {};
	struct ProfileCall { unsigned scopeIndex { 0 }; };
	std::variant<DiscoverRootScopes, ProfileCall, std::monostate> args;
};

class ProfilingSystem {
public:
	friend class ProfilerScope;
	friend class ThreadProfilingAttachment;

	enum FrameGroup { ClientGroup, ServerGroup };

	static void attachToThisThread( FrameGroup group );
	static void detachFromThisThread( FrameGroup group );

	static void beginFrame( FrameGroup group, ProfilerArgsSupplier *argsSupplier );
	static void endFrame( FrameGroup group, ProfilerResultSink *resultSink );

	struct RegisteredScope {
		wsw::StringView file;
		int line { 0 };
		wsw::HashedStringView exactFunction;
		wsw::StringView readableFunction;
	};

	[[nodiscard]]
	static auto getRegisteredScopes() -> std::span<const RegisteredScope>;
private:
	// Per-group
	static class ProfilerThreadInstance *s_instances[2];
	static volatile unsigned s_isProfilingEnabled[2];
};

class ProfilerArgsSupplier {
public:
	virtual ~ProfilerArgsSupplier() = default;

	virtual void beginSupplyingArgs() = 0;
	[[nodiscard]]
	virtual auto getArgs( ProfilingSystem::FrameGroup group ) -> ProfilerArgs = 0;
	virtual void endSupplyingArgs() = 0;
};

class ProfilerResultSink {
public:
	virtual ~ProfilerResultSink() = default;

	virtual void beginAcceptingResults( ProfilingSystem::FrameGroup group ) = 0;
	virtual void endAcceptingResults( ProfilingSystem::FrameGroup group ) = 0;

	virtual void addDiscoveredRoot( uint64_t threadId, unsigned scopeId ) = 0;

	struct CallStats {
		uint64_t totalTime { 0 };
		int enterCount { 0 };
	};

	virtual void addCallStats( uint64_t threadId, unsigned callScopeId, const CallStats &callStats ) = 0;
	virtual void addCallChildStats( uint64_t threadId, unsigned childScopeId, const CallStats &callStats ) = 0;
};

class ThreadProfilingAttachment {
public:
	ThreadProfilingAttachment( ProfilingSystem::FrameGroup group ) : m_group( group ) {
		ProfilingSystem::attachToThisThread( group );
	}
	~ThreadProfilingAttachment() {
		ProfilingSystem::detachFromThisThread( m_group );
	}

	ThreadProfilingAttachment( const ThreadProfilingAttachment & ) = delete;
	auto operator=( const ThreadProfilingAttachment & ) -> ThreadProfilingAttachment & = delete;
	ThreadProfilingAttachment( ThreadProfilingAttachment && ) = delete;
	auto operator=( ThreadProfilingAttachment && ) -> ThreadProfilingAttachment & = delete;
private:
	const ProfilingSystem::FrameGroup m_group;
};

}

#endif