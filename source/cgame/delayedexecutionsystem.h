#ifndef WSW_7616b4db_a476_495a_bb0a_0fdaadc45935_H
#define WSW_7616b4db_a476_495a_bb0a_0fdaadc45935_H

#include "../common/freelistallocator.h"
#include "../common/wswpodvector.h"
#include "../common/wswstaticvector.h"

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
		return lhs->timeoutAt < rhs->timeoutAt;
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