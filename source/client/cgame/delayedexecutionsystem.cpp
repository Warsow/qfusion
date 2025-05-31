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

#include "delayedexecutionsystem.h"
#include "cg_local.h"
#include <common/wswalgorithm.h>

DelayedExecutionSystem::DelayedExecutionSystem() {
	constexpr unsigned step     = ( kMaxBlockSize - kMinBlockSize ) / kNumAllocators;
	constexpr unsigned capacity = 256;
	static_assert( kMinBlockSize + step * kNumAllocators == kMaxBlockSize );
	try {
		for( unsigned allocatorNum  = 0; allocatorNum < kNumAllocators; ++allocatorNum ) {
			const unsigned size   = sizeof( Callable ) + kMinBlockSize + step * ( allocatorNum + 1 );
			auto *const allocator = new wsw::HeapBasedFreelistAllocator( size, capacity );
			assert( allocatorNum + 1 < kNumAllocators || size == sizeof( Callable ) + kMaxBlockSize );
			m_allocators.push_back( { allocator, size } );
		}
	} catch( ... ) {
		for( auto &[allocator, _]: m_allocators ) {
			delete allocator;
		}
		throw;
	}
}

DelayedExecutionSystem::~DelayedExecutionSystem() {
	for( Callable *callable: m_heapOfCallables ) {
		destroyCallable( callable );
	}
	for( auto &[allocator, _]: m_allocators ) {
		delete allocator;
	}
}

void DelayedExecutionSystem::registerCallable( unsigned timeout, Callable *callable ) {
	// Note: We don't require the minimal timeout to be non-zero,
	// so it is useful for spawning event effects in the same frame
	// at the more appropriate moment of time when the renderer frontend
	// prepares the static content for rendering using worker threads
	callable->timeoutAt = cg.time + timeout;
	m_heapOfCallables.push_back( callable );
	wsw::push_heap( m_heapOfCallables.begin(), m_heapOfCallables.end(), kCallableLess );
}

void DelayedExecutionSystem::destroyCallable( Callable *callable ) {
	auto *allocator = callable->allocator;
	callable->~Callable();
	allocator->free( callable );
}

void DelayedExecutionSystem::run() {
	// TODO: Consider using parallel execution?
	// TODO: Traverse the heap first, then execute callables to reduce icache pollution
	// (this design is also more parallel-friendly)?
	while( !m_heapOfCallables.empty() && m_heapOfCallables.front()->timeoutAt <= cg.time ) {
		wsw::pop_heap( m_heapOfCallables.begin(), m_heapOfCallables.end() );
		Callable *const callable = m_heapOfCallables.back();
		m_heapOfCallables.pop_back();
		try {
			callable->call();
		} catch( ... ) {
			destroyCallable( callable );
			throw;
		}
		destroyCallable( callable );
	}
}

auto DelayedExecutionSystem::getBestAllocator( size_t requiredSize ) -> wsw::FreelistAllocator * {
	assert( requiredSize <= kMaxBlockSize );
	// TODO: Use binary search
	for( auto &[allocator, blockSize] : m_allocators ) {
		if( requiredSize <= blockSize ) {
			return allocator;
		}
	}
	wsw::failWithRuntimeError( "Unreachable" );
}