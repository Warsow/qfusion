#ifndef WSW_19b6f5dd_9f11_4325_9ea7_bd133094e464_H
#define WSW_19b6f5dd_9f11_4325_9ea7_bd133094e464_H

#include "../ailocal.h"

/**
 * Represents a bit vector that has a fixed size.
 */
class BitVector {
	friend class AasElementsMask;
public:
	/**
	 * Let's use 32-bit words to work fast on 32-bit processors.
	 */
	enum { WORD_SIZE = 32 };
	typedef uint32_t word_t;
private:
	word_t *const words;
	unsigned sizeInBits;

	BitVector( uint8_t *buffer, unsigned sizeInBytes )
		: words( (word_t *)buffer ), sizeInBits( sizeInBytes * 8 ) {
		assert( !( ( (uintptr_t)buffer ) % alignof( word_t ) ) );
	}

	void GetWordAndMask( int givenIndex, word_t *wordIndex, word_t *mask ) const {
		assert( givenIndex >= 0 );
		assert( givenIndex < (int)sizeInBits );
		*wordIndex = (word_t)givenIndex / WORD_SIZE;
		auto bitIndex = (word_t)givenIndex - *wordIndex * WORD_SIZE;
		*mask = ( 1u << bitIndex );
	}
public:
	void Clear() {
		memset( words, 0, sizeInBits / 8 );
	}

	bool IsSet( int index ) const {
		word_t wordIndex, mask;
		GetWordAndMask( index, &wordIndex, &mask );
		return ( words[wordIndex] & mask ) != 0;
	}

	/**
	 * This call follows "compare-and-swap" semantics
	 * trying to set a bit if it has not been already set.
	 * @return true if the index was not set.
	 */
	bool TrySet( int index ) {
		word_t wordIndex, mask;
		GetWordAndMask( index, &wordIndex, &mask );
		// Make the compiler hint to avoid fetching a word from memory twice
		auto *const wordPtr = &words[wordIndex];
		if( !( *wordPtr & mask ) ) {
			*wordPtr |= mask;
			return true;
		}
		return false;
	}

	void Set( int index, bool value ) {
		word_t wordIndex, mask;
		GetWordAndMask( index, &wordIndex, &mask );
		if( value ) {
			words[wordIndex] |= mask;
		} else {
			words[wordIndex] &= ~mask;
		}
	}
};

class AasElementsMask {
	friend class AiAasWorld;

	static BitVector *areasMask;
	static BitVector *facesMask;

	static bool *tmpAreasVisRow;
	static bool *blockedAreasTable;

	static int numAreas;

	/**
 	 * Managed by {@code AiAasWorld} as its initialization requires these masks.
   	 */
	static void Init( AiAasWorld *parent );
	/**
	 * Managed by {@code AiAasWorld} as it performs initialization of these masks.
	 */
	static void Shutdown();
public:
	/**
	 * Assuming {@code N} is the number of areas in the world,
	 * {@code N * TMP_ROW_REDUNDANCY_SCALE} elements are allocated for {@code TmpAreasVisRow()}
	 */
	static constexpr unsigned TMP_ROW_REDUNDANCY_SCALE = 8;

	static BitVector *AreasMask() { return areasMask; }
	static BitVector *FacesMask() { return facesMask; }

	static bool *TmpAreasVisRow( int instanceNum ) {
		assert( (unsigned)instanceNum < (unsigned)TMP_ROW_REDUNDANCY_SCALE );
		return tmpAreasVisRow + instanceNum * numAreas;
	}

	static bool *TmpAreasVisRow() { return tmpAreasVisRow; }
	static bool *BlockedAreasTable() { return blockedAreasTable; }
};

#endif