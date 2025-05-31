#ifndef WSW_d5029961_ab38_45cc_b4ba_8a4516d44ebb_H
#define WSW_d5029961_ab38_45cc_b4ba_8a4516d44ebb_H

#include "../ailocal.h"

class alignas ( sizeof( void * ) )PoolBase {
	friend class PoolItem;

	char *basePtr;
	const char *tag;
	const uint16_t linksOffset;
	const uint16_t alignedChunkSize;

	static constexpr auto FREE_LIST = 0;
	static constexpr auto USED_LIST = 1;

	// The head of the free list initially points at the first element.
	// The head of the used list is not initially defined (is "null").
	int16_t listFirst[2] { 0, -1 };

#ifdef _DEBUG
	const char *ListName( int index ) {
		switch( index ) {
			case FREE_LIST: return "FREE";
			case USED_LIST: return "USED";
			default: AI_FailWith( "PoolBase::ListName()", "Illegal index %d\n", index );
		}
	}
#endif

	static uint16_t LinksOffset( uint16_t itemSize ) {
		uint16_t remainder = itemSize % alignof( uint16_t );
		if( !remainder ) {
			return itemSize;
		}
		return itemSize + alignof( uint16_t ) - remainder;
	}

	static uint16_t AlignedChunkSize( uint16_t itemSize ) {
		uint16_t totalSize = LinksOffset( itemSize ) + sizeof( ItemLinks );
		uint16_t remainder = totalSize % alignof( void * );
		if( !remainder ) {
			return totalSize;
		}
		return totalSize + alignof( void * ) - remainder;
	}

	class PoolItem &ItemAt( int16_t index ) {
		char *mem = ( basePtr + alignedChunkSize * index );
		assert( !( (uintptr_t)mem % sizeof( void * ) ) );
		return *(PoolItem *)mem;
	}

	int16_t IndexOf( const class PoolItem *item ) const {
		return (int16_t)( ( (const char *)item - basePtr ) / alignedChunkSize );
	}

	void Link( int16_t itemIndex, int16_t listIndex );
	void Unlink( int16_t itemIndex, int16_t listIndex );
protected:
	void *Alloc();
	void Free( class PoolItem *poolItem );

#ifndef _MSC_VER
	void Debug( const char *format, ... ) const __attribute__( ( format( printf, 2, 3 ) ) )
#else
	void Debug( _Printf_format_string_ const char *format, ... ) const
#endif
	{
		va_list va;
		va_start( va, format );
		AI_Debugv( tag, format, va );
		va_end( va );
	}

	// These links follow an item in-memory, not precede it.
	// This is to avoid wasting bytes on aligning an item after these links.
	// (An item is required to be 8-byte aligned on 64-bit systems
	// while the links alignment is less restrictive).
	struct alignas( 2 )ItemLinks {
		int16_t links[2];
		int16_t &Prev() { return links[0]; }
		int16_t &Next() { return links[1]; }
	};

	ItemLinks &ItemLinksAt( int16_t index ) {
		char *mem = ( basePtr + alignedChunkSize * index + linksOffset );
		assert( !( (uintptr_t)mem % alignof( ItemLinks ) ) );
		return *(ItemLinks *)mem;
	}
public:
	PoolBase( char *basePtr_, const char *tag_, uint16_t itemSize, uint16_t itemsCount );

	void Clear();
};

class alignas ( sizeof( void * ) )PoolItem {
	friend class PoolBase;
	PoolBase *pool;
public:
	explicit PoolItem( PoolBase * pool_ ) : pool( pool_ ) {}
	virtual ~PoolItem() = default;

	void DeleteSelf() {
		this->~PoolItem();
		pool->Free( this );
	}
};

template<class Item, unsigned N>
class alignas ( sizeof( void * ) )Pool : public PoolBase {
	// We have to introduce these intermediates instead of variables since we are limited to C++11 (not 14) standard.

	static constexpr unsigned OffsetRemainder() {
		return sizeof( Item ) % alignof( ItemLinks );
	}

	static constexpr unsigned ItemLinksOffset() {
		return OffsetRemainder() ? sizeof( Item ) + alignof( ItemLinks ) - OffsetRemainder() : sizeof( Item );
	}

	static constexpr unsigned TotalSize() {
		return ItemLinksOffset() + sizeof( ItemLinks );
	}

	static constexpr unsigned ChunkSizeRemainder() {
		return TotalSize() % sizeof( void * );
	}

	static constexpr unsigned ChunkSize() {
		return ChunkSizeRemainder() ? TotalSize() + sizeof( void * ) - ChunkSizeRemainder() : TotalSize();
	}

	alignas( alignof( void * ) ) char buffer[N * ChunkSize()];
public:
	explicit Pool( const char *tag_ )
		: PoolBase( buffer, tag_, sizeof( Item ), (uint16_t)N ) {
		static_assert( N <= std::numeric_limits<int16_t>::max(), "Links can't handle more than 2^15 elements in pool" );
	}

	template<typename... Args>
	Item *New( Args... args ) {
		if( void *mem = Alloc() ) {
			return new( mem )Item( this, args... );
		}
		return nullptr;
	}
};

#endif
