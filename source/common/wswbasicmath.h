#ifndef WSW_519ad756_e10f_4b29_9c04_01f9a63308ba_H
#define WSW_519ad756_e10f_4b29_9c04_01f9a63308ba_H

#include "wswbasicarch.h"

namespace wsw {

template <typename T>
[[nodiscard]]
constexpr wsw_forceinline auto min( const T &a, const T &b ) -> T {
	return a < b ? a : b;
}

template <typename T>
[[nodiscard]]
constexpr wsw_forceinline auto max( const T &a, const T &b ) -> T {
	return a < b ? b : a;
}

template <typename T>
[[nodiscard]]
constexpr wsw_forceinline auto clamp( const T &v, const T &lo, const T &hi ) -> T {
	return min( max( v, lo ), hi );
}

template <typename T>
[[nodiscard]]
constexpr wsw_forceinline auto square( const T &v ) -> T {
	return v * v;
}

template <typename T>
[[nodiscard]]
constexpr wsw_forceinline auto cube( const T &v ) -> T {
	return v * v * v;
}

template <std::integral T>
[[nodiscard]]
constexpr wsw_forceinline bool isPowerOf2( const T &v ) {
	return ( v & ( v - 1 ) ) == 0;
}

template <std::unsigned_integral T>
[[nodiscard]]
constexpr wsw_forceinline auto ceilPowerOf2( const T &v ) -> T {
	// TODO: Use arch-specific code for non-std::is_constant_evaluated() contexts
	// (Handling boundary cases could be tricky)
	T result = 1;
	while( result < v ) {
		result = result << 1;
	}
	return result;
}

}

#endif
