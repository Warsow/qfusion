#ifndef WSW_5755d5a6_44b7_4a82_bd65_68e10f8346b5_H
#define WSW_5755d5a6_44b7_4a82_bd65_68e10f8346b5_H

#include <cstdlib>
#include <cstdint>
#include <cassert>

#include <common/helpers/exceptions.h>

#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#define WSW_ASAN_LOCK( address, size ) ASAN_POISON_MEMORY_REGION( address, size )
#define WSW_ASAN_UNLOCK( address, size ) ASAN_UNPOISON_MEMORY_REGION( address, size )
#else
#define WSW_ASAN_LOCK( address, size ) (void)0
#define WSW_ASAN_UNLOCK( address, size ) (void)0
#endif

namespace wsw {

class FreelistAllocator {
protected:
	struct Header { Header *prev, *next; };
	static_assert( alignof( Header ) == sizeof( void * ) );

	Header m_sentinel { nullptr, nullptr };
	Header *m_freelist { nullptr };
	uint8_t *m_basePtr { nullptr };
	unsigned m_realChunkSize { 0 };
	unsigned m_capacity { 0 };
	unsigned m_alignment { 0 };

	[[nodiscard]]
	auto getHeader( void *p ) const -> Header * {
		const auto address = ( (intptr_t)p + (intptr_t)m_realChunkSize - sizeof( Header ) );
		assert( !( address % alignof( Header ) ) );
		return (Header *)address;
	}

	[[nodiscard]]
	auto getChunk( Header *h ) const -> uint8_t * {
		return (uint8_t *)( (intptr_t)h - (intptr_t)m_realChunkSize + sizeof( Header ) );
	}

	template <typename T>
	[[nodiscard]]
	static constexpr auto pad( T value, T alignment ) -> T {
		return value + ( alignment - value % alignment ) % alignment;
	}

	[[nodiscard]]
	static constexpr auto realChunkSize( unsigned userVisibleSize, unsigned alignment ) -> unsigned {
		static_assert( sizeof( Header ) == 2 * sizeof( void * ) );
		// We need to ensure that Header pointers are aligned properly too
		return (unsigned)pad( userVisibleSize + sizeof( Header ), alignment >= alignof( Header ) ? alignment : alignof( Header ) );
	}

	void lockMembers() {
		// Protect members that are most likely to be corrupted
		WSW_ASAN_LOCK( &m_sentinel, sizeof( m_sentinel ) );
		WSW_ASAN_LOCK( &m_freelist, sizeof( void * ) );
	}

	void unlockMembers() {
		WSW_ASAN_UNLOCK( &m_sentinel, sizeof( m_sentinel ) );
		WSW_ASAN_UNLOCK( &m_freelist, sizeof( void * ) );
	}

	void lockHeader( Header *h ) {
		if( h != &m_sentinel ) {
			WSW_ASAN_LOCK( h, sizeof( Header ) );
		}
	}
	void unlockHeader( Header *h ) {
		if( h != &m_sentinel ) {
			WSW_ASAN_UNLOCK( h, sizeof( Header ) );
		}
	}

	void lockFullChunkPointedByHeader( Header *h ) {
		WSW_ASAN_LOCK( getChunk( h ), m_realChunkSize );
	}
	void unlockFullChunkPointedByHeader( Header *h ) {
		WSW_ASAN_UNLOCK( getChunk( h ), m_realChunkSize );
	}

	void lockAllChunks() {
		WSW_ASAN_LOCK( m_basePtr, m_realChunkSize * m_capacity );
	}

	void unlockAllChunks() {
		WSW_ASAN_UNLOCK( m_basePtr, m_realChunkSize * m_capacity );
	}

	void set( void *p, unsigned chunkSize, unsigned capacity, unsigned alignment ) {
		assert( capacity );
		assert( alignment && !( alignment & ( alignment - 1 ) ) );
		assert( !( (uintptr_t)p % alignment ) );

		m_basePtr = (uint8_t *)p;
		m_realChunkSize = realChunkSize( chunkSize, alignment );
		m_capacity = capacity;
		m_alignment = alignment;

		fixLinks();

		lockMembers();
		lockAllChunks();
	}

	void fixLinks() {
		Header *first = getHeader( m_basePtr );
		Header *firstNext = getHeader( m_basePtr + m_realChunkSize );
		m_sentinel.next = first;
		first->prev = &m_sentinel;
		first->next = firstNext;
		m_freelist = first;

		Header *prev = first;
		Header *curr = firstNext;
		for( unsigned i = 1; i < m_capacity - 1; ++i ) {
			auto *const next = getHeader( m_basePtr + m_realChunkSize * ( i + 1 ) );
			curr->prev = prev;
			curr->next = next;
			prev = curr;
			curr = next;
		}

		Header *last = curr;
		last->prev = prev;
		last->next = &m_sentinel;
		m_sentinel.prev = last;
	}
public:
	~FreelistAllocator() {
		unlockMembers();
		unlockAllChunks();
	}

	void clear() {
		unlockMembers();
		unlockAllChunks();

		fixLinks();

		lockMembers();
		lockAllChunks();
	}

	[[nodiscard]]
	auto allocOrNull( unsigned *index = nullptr ) noexcept -> uint8_t * {
		unlockMembers();

		uint8_t *result = nullptr;
		if( m_freelist != &m_sentinel ) {
			Header *const curr = m_freelist;
			unlockFullChunkPointedByHeader( curr );

			Header *const next = m_freelist->next;
			Header *const prev = m_freelist->prev;

			unlockHeader( prev );
			unlockHeader( next );

			prev->next = next;
			next->prev = prev;
			// Prevent reusing
			curr->prev = curr->next = nullptr;
			m_freelist = next;
			assert( m_freelist->prev == &m_sentinel );

			result = getChunk( curr );
			assert( !( ( (uintptr_t)result ) % m_alignment ) );

			lockHeader( next );
			lockHeader( prev );
		}

		lockMembers();
		if( index && result ) {
			*index = slowIndexOf( result );
		}
		return result;
	}

	[[nodiscard]]
	auto allocOrThrow( unsigned *index = nullptr ) -> uint8_t * {
		if( auto *result = allocOrNull( index ) ) {
			return result;
		}
		wsw::failWithBadAlloc();
	}

	void free( void *p ) noexcept {
		assert( mayOwn( p ) );
		assert( hasValidOffset( p ) );

		unlockMembers();

		assert( p != m_freelist );
		assert( p != &m_sentinel );

		Header *const curr = getHeader( p );
		unlockHeader( m_freelist );

		assert( !curr->prev && !curr->next );
		assert( m_freelist->prev == &m_sentinel );

		Header *const prev = m_freelist->prev;
		Header *const next = m_freelist;
		unlockHeader( prev );
		unlockHeader( next );

		prev->next = curr;
		curr->prev = prev;
		next->prev = curr;
		curr->next = next;
		m_freelist = curr;
		assert( m_freelist->prev == &m_sentinel );

		lockHeader( next );
		lockHeader( prev );
		lockFullChunkPointedByHeader( curr );
		lockMembers();
	}

	[[nodiscard]]
	bool isFull() const {
		const_cast<FreelistAllocator *>(this)->unlockMembers();
		const bool result = m_freelist == &m_sentinel;
		const_cast<FreelistAllocator *>(this)->lockMembers();
		return result;
	}

	[[nodiscard]]
	auto capacity() const -> unsigned { return m_capacity; }

	[[nodiscard]]
	bool mayOwn( const void *p ) const {
		return (uintptr_t)( (const uint8_t *)p - m_basePtr ) < m_realChunkSize * m_capacity;
	}

	[[nodiscard]]
	bool hasValidOffset( const void *p ) const {
		return !( ( (const uint8_t *)p - m_basePtr ) % m_realChunkSize );
	}

	[[nodiscard]]
	auto slowIndexOf( const void *p ) const -> unsigned {
		assert( mayOwn( p ) && hasValidOffset( p ) );
		return (unsigned)( (const uint8_t *)p - m_basePtr ) / (unsigned)m_realChunkSize;
	}
};

template <size_t Size, size_t Capacity, size_t Alignment = 16>
class alignas( Alignment ) MemberBasedFreelistAllocator : public FreelistAllocator {
	static_assert( Alignment && !( Alignment & ( Alignment - 1 ) ) );
	alignas( Alignment ) uint8_t m_memberData[realChunkSize( Size, Alignment ) * Capacity];
public:
	MemberBasedFreelistAllocator() noexcept {
		set( m_memberData, Size, Capacity, Alignment );
	}
};

class HeapBasedFreelistAllocator : public FreelistAllocator {
	void *m_mallocData;
	unsigned m_alignmentBytes { 0 };
public:
	HeapBasedFreelistAllocator( unsigned size, unsigned capacity, unsigned alignment = 16u ) {
		assert( alignment && !( alignment & ( alignment - 1 ) ) );
		const bool isAlignedByDefault = ( sizeof( void * ) == 8u && alignment <= 16u ) || alignment <= 8u;
		if( isAlignedByDefault ) {
			m_mallocData = std::malloc( realChunkSize( size, alignment ) * capacity );
		} else {
			m_mallocData = std::malloc( realChunkSize( size, alignment ) * capacity + alignment );
		}
		if( !m_mallocData ) {
			wsw::failWithBadAlloc();
		}
		if( isAlignedByDefault ) {
			set( m_mallocData, size, capacity, alignment );
			m_alignmentBytes = 0;
		} else {
			auto *const basePtr = (uint8_t *)pad( (uintptr_t)m_mallocData, (uintptr_t)alignment );
			m_alignmentBytes = (unsigned)( (uintptr_t)basePtr - (uintptr_t)m_mallocData );
			assert( m_alignmentBytes < alignment );
			if( m_alignmentBytes ) {
				WSW_ASAN_LOCK( m_mallocData, m_alignmentBytes );
			}
			set( basePtr, size, capacity, alignment );
		}
	}

	~HeapBasedFreelistAllocator() {
		if( m_alignmentBytes ) {
			WSW_ASAN_UNLOCK( m_mallocData, m_alignmentBytes );
		}
		std::free( m_mallocData );
	}
};

}

#endif