#ifndef WSW_WSWTONUM_H
#define WSW_WSWTONUM_H

#include <cassert>
#include <cerrno>
#include <climits>
#include <optional>
#include <type_traits>
#include <common/types/stringview.h>
#include <common/types/staticstring.h>
#include <common/helpers/q_math.h>

namespace wsw {

/**
 * Tries to convert a given string to a number.
 * @tparam T a supplied number type
 * @param s a given string to convert
 * @param endPtr a nullable reference to write an address of a character after the number
 * @return {@code std::nullopt_t} on conversion errors, a wrapped value otherwise.
 * @note non-zero characters after a number are treated as errors unless {@code endPtr} is supplied.
*/
template <typename T>
[[nodiscard]]
// [[deprecated("Use toNum( const wsw::StringView & ) instead")]]
auto toNum( const char *s, const char **endPtr = nullptr ) -> std::optional<T> {
	constexpr bool isTypeParamIntegral = std::is_integral_v<T>;
	static_assert( isTypeParamIntegral || std::is_floating_point_v<T> );

	[[maybe_unused]]
	constexpr auto minVal = std::numeric_limits<T>::min();
	[[maybe_unused]]
	constexpr auto maxVal = std::numeric_limits<T>::max();

	errno = 0;
	char *tmp = nullptr;
	std::optional<T> result;
	if constexpr( isTypeParamIntegral ) {
		if constexpr( std::is_signed_v<T> ) {
			// MSVC: Can't be qualified by std::
			long long val = strtoll( s, &tmp, 10 );
			if( !val && !( tmp - s ) ) {
				return std::nullopt;
			}
			if( val < (long long)minVal || val > (long long)maxVal ) {
				return std::nullopt;
			}
			// Catch overflow for long long values
			if constexpr( sizeof( T ) == 8 ) {
				if( val == (long long)maxVal || val == (long long)minVal ) {
					if( errno == ERANGE ) {
						return std::nullopt;
					}
				}
			}
			result = std::make_optional( (T)val );
		} else {
			// MSVC: Can't be qualified by std::
			unsigned long long val = strtoull( s, &tmp, 10 );
			if( !val && !( tmp - s ) ) {
				return std::nullopt;
			}
			if( val > (unsigned long long)maxVal ) {
				return std::nullopt;
			}
			// Catch overflow for unsigned long long values
			if constexpr( sizeof( T ) == 8 ) {
				if( val == (unsigned long long)maxVal || val == (unsigned long long)minVal ) {
					if( errno == ERANGE ) {
						return std::nullopt;
					}
				}
			}
			result = std::make_optional( (T)val );
		}
	} else {
		constexpr bool isFloat = std::is_same_v<std::remove_cvref_t<T>, float>;
		constexpr bool isDouble = std::is_same_v<std::remove_cvref_t<T>, double>;
		constexpr bool isLongDouble = std::is_same_v<std::remove_cvref_t<T>, long double>;
		static_assert( isFloat || isDouble || isLongDouble, "Weird floating-point type" );

		T val, hugeVal;
		if constexpr( isFloat ) {
			val = std::strtof( s, &tmp );
			hugeVal = HUGE_VALF;
		}
		if constexpr( isDouble ) {
			val = std::strtod( s, &tmp );
			hugeVal = HUGE_VAL;
		}
		if constexpr( isLongDouble ) {
			val = std::strtold( s, &tmp );
			hugeVal = HUGE_VALL;
		}
		if( !val ) {
			// If not even a single digit was converted
			if( !( tmp - s ) ) {
				return std::nullopt;
			}
			// Try catching underflow
			if( errno == ERANGE ) {
				return std::nullopt;
			}
		}
		// Catch overflow
		if( ( val == +hugeVal || val == -hugeVal ) && errno == ERANGE ) {
			return std::nullopt;
		}
		result = std::make_optional( val );
	}

	// These conditions are put last just to avoid nesting/helpers introduction
	if( !endPtr ) {
		// We must stop at the last zero character unless the endPtr is explicitly specified
		if( *tmp != '\0' ) {
			return std::nullopt;
		}
	} else {
		*endPtr = tmp;
	}
	return result;
}

template <typename T>
[[nodiscard]]
auto toNum( const wsw::StringView &s, unsigned *stoppedAtIndex = nullptr ) -> std::optional<T> {
	// We would like to use std::from_chars but the support
	// for floating-point conversions is still incomplete.
	// However this should eventually be rewritten by using <charconv> stuff.

	if( s.isZeroTerminated() ) {
		if( !stoppedAtIndex ) {
			return wsw::toNum<T>( s.data() );
		}
		const char *endPtr = nullptr;
		if( auto maybeResult = wsw::toNum<T>( s.data(), &endPtr ) ) {
			*stoppedAtIndex = (unsigned)( endPtr - s.data() );
			return maybeResult;
		}
		return std::nullopt;
	}

	if( s.length() >= 256 ) {
		return std::nullopt;
	}

	wsw::StaticString<256> buffer( s );
	if( !stoppedAtIndex ) {
		return wsw::toNum<T>( buffer.data() );
	}
	const char *endPtr = nullptr;
	if( auto maybeResult = wsw::toNum<T>( s.data(), &endPtr) ) {
		*stoppedAtIndex = (unsigned)( endPtr - s.data() );
		return maybeResult;
	}
	return std::nullopt;
}

}

#endif
