#ifndef QFUSION_AASAREASMASK_H
#define QFUSION_AASAREASMASK_H

#include "../ai_local.h"

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

	/**
 	 * Managed by {@code AiAasWorld} as its initialization requires these masks.
   	 */
	static void Init( AiAasWorld *parent );
	/**
	 * Managed by {@code AiAasWorld} as it performs initialization of these masks.
	 */
	static void Shutdown();
public:
	static BitVector *AreasMask() { return areasMask; }
	static BitVector *FacesMask() { return facesMask; }
};

#endif