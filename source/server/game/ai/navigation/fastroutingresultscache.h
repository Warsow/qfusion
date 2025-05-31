#ifndef WSW_77f50d5d_69bc_4776_ba4d_bd61e4a69ab2_H
#define WSW_77f50d5d_69bc_4776_ba4d_bd61e4a69ab2_H

#include "../ailocal.h"

class FastRoutingResultsCache {
public:
	// TODO: We can compactify it down to 16 bytes
	// if we remove some unused travel flag bits that occupy the high part of the key
	struct alignas( 4 )Node {
		UInt64Align4 key { 0 };

		// Compact indices of prev/next node in a hash bin
		// We have to use a single-element array due to signature of linking utility functions
		int16_t prev[1] { 0 };
		int16_t next[1] { 0 };
		uint16_t binIndex { 0 };

		uint16_t reachability { 0 };
		uint16_t travelTime { 0 };
	};

	static_assert( sizeof( Node ) <= 20, "The struct size assumptions are broken" );

	// Assuming that area nums are limited by 16 bits, all parameters can be composed in a single integer

	[[nodiscard]]
	static auto makeKey( int fromAreaNum, int toAreaNum, int travelFlags ) -> uint64_t {
		assert( fromAreaNum >= 0 && fromAreaNum <= 0xFFFF );
		assert( toAreaNum >= 0 && toAreaNum <= 0xFFFF );
		return ( ( (uint64_t)travelFlags ) << 32 ) | ( ( (uint64_t)fromAreaNum ) << 16 ) | ( (uint64_t)toAreaNum );
	}

	[[nodiscard]]
	static auto calcBinIndexForKey( uint64_t key ) -> uint16_t {
		// Convert a 64-bit key to 32-bit hash trying to preserve bits entropy.
		// The primary purpose of it is avoiding 64-bit division in modulo computation.
		// TODO: Ensure that compilers don't produce an actual division
		constexpr auto mask32 = ~( (uint32_t)0 );
		const auto loPart32   = (uint32_t)( key & mask32 );
		const auto hiPart32   = (uint32_t)( ( key >> 32 ) & mask32 );
		const auto hash       = loPart32 * 17 + hiPart32;
		return (uint16_t)( hash % kNumHashBins );
	}

	FastRoutingResultsCache() { reset(); }

	void reset();

	// The key and bin index must be computed by callers using makeKey() and calcBinIndexForKey().
	// This is a bit ugly but encourages efficient usage patterns.
	[[nodiscard]]
	auto getCachedResultForKey( uint16_t binIndex, uint64_t key ) const -> const Node *;
	[[nodiscard]]
	auto allocAndRegisterForKey( uint16_t binIndex, uint64_t key ) -> Node *;
private:
	static constexpr unsigned kMaxCachedResults = 512;
	static constexpr unsigned kNumHashBins      = 883;

	// A circular buffer of nodes
	mutable Node m_nodes[kMaxCachedResults];
	mutable unsigned m_cursorIndex { 0 };
	mutable int16_t m_hashBins[kNumHashBins];
};

#endif
