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

#ifndef WSW_7616b4db_a476_495a_bb0a_0fdaadc45935_H
#define WSW_7616b4db_a476_495a_bb0a_0fdaadc45935_H

#include <common/freelistallocator.h>
#include <common/wswpodvector.h>
#include <common/wswstaticvector.h>

#include <utility>

class DelayedExecutionSystem {
	struct Callable {
		virtual ~Callable() = default;
		virtual void call() = 0;
		int64_t timeoutAt { 0 };
		wsw::FreelistAllocator *allocator { nullptr };
	};
public:
	DelayedExecutionSystem();
	~DelayedExecutionSystem();

	template <typename Fn>
	void post( unsigned timeout, Fn &&fn ) {
		struct CallableImpl : public Callable {
			Fn m_fn;
			explicit CallableImpl( Fn &&fn ) : m_fn( std::forward<Fn>( fn ) ) {}
			void call() override { m_fn(); }
		};
		static_assert( sizeof( CallableImpl ) <= kMaxBlockSize );

		// Ensure that we perform a throwing operation prior to memory allocation which is more complex to rollback
		m_heapOfCallables.reserve( m_heapOfCallables.size() + 1 );

		wsw::FreelistAllocator *allocator = getBestAllocator( sizeof( CallableImpl ) );
		if( void *mem = allocator->allocOrNull() ) [[likely]] {
			auto *callable      = new( mem )CallableImpl( std::forward<Fn>( fn ) );
			callable->allocator = allocator;
			// Do all the registration work in non-template part
			registerCallable( timeout, callable );
		}
	}

	void run();
private:
	[[nodiscard]]
	auto getBestAllocator( size_t requiredSize ) -> wsw::FreelistAllocator *;
	void registerCallable( unsigned timeout, Callable *callable );
	static void destroyCallable( Callable *callable );

	static constexpr auto kCallableLess = []( const Callable *lhs, const Callable *rhs ) {
		// Let callables with lesser timestamps be evicted from the heap first
		return lhs->timeoutAt > rhs->timeoutAt;
	};
	static constexpr unsigned kNumAllocators = 8;
	static constexpr unsigned kMinBlockSize  = 16;
	static constexpr unsigned kMaxBlockSize  = 384;

	wsw::PodVector<Callable *> m_heapOfCallables;
	// Note: we don't store the original chunk size in allocators
	// (only the internal adjusted one), hence we have to do it externally
	wsw::StaticVector<std::pair<wsw::HeapBasedFreelistAllocator *, size_t>, kNumAllocators> m_allocators;
};

#endif