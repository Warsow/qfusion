/*
Copyright (C) 2024 Chasseur de bots

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

#ifndef WSW_3519ba0b_7fae_40be_94a2_0b394ea5b6b7_H
#define WSW_3519ba0b_7fae_40be_94a2_0b394ea5b6b7_H

#include <cstdint>
#include <coroutine>
#include <span>
#include <cassert>

class alignas( alignof( void * ) ) [[nodiscard]] TaskHandle {
	friend class TaskSystem;
public:
	[[nodiscard]]
	operator bool() const { return m_opaque != 0; }
	intptr_t m_opaque { 0 };
};

class TaskSystem;

class [[nodiscard]] TaskAwaiter {
public:
	TaskAwaiter( TaskSystem *taskSystem, std::span<const TaskHandle> dependencies )
		: m_taskSystem( taskSystem ), m_dependencies( dependencies ) {
		assert( taskSystem );
	}

	[[nodiscard]] bool await_ready() const noexcept { return false; }
	void await_suspend( std::coroutine_handle<> h ) const;
	void await_resume() const noexcept {}
private:
	mutable TaskSystem *m_taskSystem { nullptr };
	std::span<const TaskHandle> m_dependencies;
};

class [[nodiscard]] CoroTask {
	friend class TaskSystem;
public:
	enum Affinity : uint8_t { AnyThread, OnlyMainThread };

	CoroTask( const CoroTask & ) = delete;
	auto operator=( const CoroTask & ) -> CoroTask & = delete;

	CoroTask( CoroTask &&that ) noexcept {
		m_handle      = that.m_handle;
		that.m_handle = {};
	}

	[[maybe_unused]]
	auto operator=( CoroTask &&that ) noexcept -> CoroTask & {
		if( m_handle != that.m_handle ) [[likely]] {
			if( m_handle ) {
				m_handle.destroy();
			}
			m_handle      = that.m_handle;
			that.m_handle = {};
		}
		return *this;
	}

	~CoroTask() {
		if( m_handle ) {
			m_handle.destroy();
			m_handle = {};
		}
	}

	struct StartInfo {
		TaskSystem *taskSystem;
		// Caution: it usually points to temporary (i.e. stack) memory, don't use upon initial suspend!
		std::span<const TaskHandle> initialDependencies;
		Affinity affinity { AnyThread };
	};

	struct promise_type {
		friend class TaskSystem;
		friend class TaskAwaiter;
		struct InitialSuspend {
			bool await_ready() noexcept { return false; }
			void await_suspend( std::coroutine_handle<promise_type> h ) const noexcept;
			void await_resume() const noexcept {}
			TaskSystem *m_taskSystem;
		};
		struct FinalSuspend {
			bool await_ready() noexcept { return false; }
			void await_suspend( std::coroutine_handle<promise_type> h ) const noexcept {
				h.promise().m_completed = true;
			}
			void await_resume() const noexcept { assert( false ); }
		};

		[[nodiscard]] auto initial_suspend() noexcept -> InitialSuspend {
			return { m_startInfo.taskSystem };
		}
		[[nodiscard]] auto final_suspend() noexcept -> FinalSuspend { return {}; }
		[[nodiscard]] auto get_return_object() -> CoroTask {
			return CoroTask( std::coroutine_handle<promise_type>::from_promise( *this ) );
		}

		void return_void() noexcept {}
		void unhandled_exception() { assert( false ); }

		[[nodiscard]]
		auto operator new( size_t size ) -> void *;
		void operator delete( void * );

		template <typename... Args>
		promise_type( const StartInfo &startInfo, Args... args ) : m_startInfo( startInfo ) {}

		promise_type() = delete;
		promise_type( const promise_type & ) = delete;
		auto operator=( const promise_type & ) = delete;

	private:
		StartInfo m_startInfo;
		// The head task of the coroutine
		TaskHandle m_task;
		// An opaque storage for atomic value (actually, it gets read under mutex)
		alignas( void *) volatile bool m_completed { false };
	};

private:
	explicit CoroTask( std::coroutine_handle<promise_type> handle ) : m_handle( handle ) {}

	std::coroutine_handle<promise_type> m_handle;
};

#endif