#include "delayedexecutionsystem.h"
#include "cg_local.h"
#include "../common/wswalgorithm.h"

#include <new>

DelayedExecutionSystem::DelayedExecutionSystem() {
	try {
		for( unsigned allocatorNum  = 0; allocatorNum < kNumAllocators; ++allocatorNum ) {
			const unsigned step     = ( kMaxBlockSize - kMinBlockSize ) / kNumAllocators;
			const unsigned size     = sizeof( Callable ) + kMinBlockSize + step * ( allocatorNum + 1 );
			const unsigned capacity = 256;
			auto *const allocator   = new wsw::HeapBasedFreelistAllocator( size, capacity );
			assert( allocatorNum + 1 < kNumAllocators || size == kMaxBlockSize );
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
	// TODO: Don't bound by 1u (so we can use it for handling effects in the same frame while we've started the task system)?
	callable->timeoutAt = cg.time + wsw::max( 1u, timeout );
	m_heapOfCallables.push_back( callable );
	wsw::push_heap( m_heapOfCallables.begin(), m_heapOfCallables.end(), kCallableLess );
}

void DelayedExecutionSystem::destroyCallable( Callable *callable ) {
	auto *allocator = callable->allocator;
	callable->~Callable();
	allocator->free( callable );
}

void DelayedExecutionSystem::run() {
	// TODO: Enable parallel execution?
	while( !m_heapOfCallables.empty() ) {
		wsw::pop_heap( m_heapOfCallables.begin(), m_heapOfCallables.end() );
		if( m_heapOfCallables.back()->timeoutAt > cg.time ) {
			// Roll back the heap state
			wsw::push_heap( m_heapOfCallables.begin(), m_heapOfCallables.end() );
			break;
		}
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