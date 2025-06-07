#ifndef WSW_e4a480ff_918a_4d13_b734_f93699e9346d_H
#define WSW_e4a480ff_918a_4d13_b734_f93699e9346d_H

#include <bit>
#include <cstdint>
#include <limits>

namespace wsw {

/// An implementation of the JSF generator.
/// This implementation is derived from the code in public domain
/// http://burtleburtle.net/bob/rand/smallprng.html
class RandomGenerator {
	// Initialize just to suppress Clang-Tidy warnings
	uint32_t m_a { 0 }, m_b { 0 }, m_c { 0 }, m_d { 0 };
public:
	[[nodiscard]]
	constexpr RandomGenerator() {
		setSeed( 17 );
	}

	[[nodiscard]]
	constexpr explicit RandomGenerator( uint32_t seed ) {
		setSeed( seed );
	}

	constexpr void setSeed( uint32_t seed ) {
		m_a = 0xF1EA5EED;
		m_b = m_c = m_d = seed;
		for( unsigned i = 0; i < 20; ++i ) {
			(void)next();
		}
	}

	[[nodiscard]]
	constexpr auto next() -> uint32_t {
		const uint32_t e = m_a - std::rotl( m_b, 27 );
		m_a = m_b ^ std::rotl( m_c, 17 );
		m_b = m_c + m_d;
		m_c = m_d + e;
		m_d = e + m_a;
		return m_d;
	}

	[[nodiscard]]
	constexpr auto nextBoundedFast( unsigned range ) -> uint32_t {
		// A biased sampling, acceptable in most cases
		constexpr double normalizer = 1.0 / std::numeric_limits<uint32_t>::max();
		return (uint32_t)( normalizer * (double)next() * (double)range );
	}

	[[nodiscard]]
	constexpr auto nextBounded( unsigned range ) -> uint32_t {
		// A public-domain implementation by D. Lemire.
		// Other implementations do not have clear/compatible licensing options.

		auto random32bit = (uint64_t)next();
		auto multiresult = (uint64_t)( random32bit * (uint64_t)range );
		auto leftover    = (uint32_t)multiresult;
		if( leftover < range ) {
			// Originally -range % range
			const uint32_t threshold = ( ~range + 1u ) % range;
			while( leftover < threshold ) {
				random32bit = next();
				multiresult = random32bit * range;
				leftover    = (uint32_t)multiresult;
			}
		}
		return (uint32_t)( multiresult >> 32 ); // [0, range)
	}

	[[nodiscard]]
	constexpr auto nextFloat() -> float {
		constexpr double normalizer = 1.0 / std::numeric_limits<uint32_t>::max();
		return (float)( normalizer * (double)next() );
	}

	[[nodiscard]]
	constexpr auto nextFloat( float min, float max ) -> float {
		assert( min <= max );
		const double doubleMin = min;
		const double doubleMax = max;
		constexpr double normalizer = 1.0 / std::numeric_limits<uint32_t>::max();
		return (float)( doubleMin + ( doubleMax - doubleMin ) * ( normalizer * (double)next() ) );
	}

	[[nodiscard]]
	constexpr bool tryWithChance( float chance ) {
		assert( chance >= 0.0f && chance <= 1.0f );
		return (double)next() <= (double)std::numeric_limits<uint32_t>::max() * (double)chance;
	}
};

}

#endif