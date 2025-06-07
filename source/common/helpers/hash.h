/*
Copyright (C) 2013 Victor Luchits

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/
#ifndef WSW_43714fdc_cde9_4d78_8ff3_abc86f2ec5d8_H
#define WSW_43714fdc_cde9_4d78_8ff3_abc86f2ec5d8_H

#include <common/helpers/q_arch.h>

#include <utility>

namespace wsw {

/**
 * An utility to get a case-insensitive hash code of a string along with its length
 * @param s a string that is supposed to be zero-terminated
 * @return a pair of a hash code and a length of a supplied string
 * @note Use with caution for performance-sensitive code.
 * Case insensitivity is only guaranteed for the ASCII character set.
 */
[[nodiscard]]
auto getHashAndLength( const char *s ) -> std::pair<uint32_t, size_t>;

/**
 * An utility to get a case-insensitive hash code of a string part specified by its length.
 * This is a "dual" (to some degree) version of {@code getHashAndLength()}
 * @param s an address of a string part
 * @param length a length of a given string part
 * @return a hash code of the given string part.
 * @note Use with caution for a performance-sensitive code.
 * Case insensitivity is only guaranteed for the ASCII character set.
 */
[[nodiscard]]
auto getHashForLength( const char *s, size_t length ) -> uint32_t;

/**
 * Adds a character to a string hash being built.
 * This is what {@code getHashAndLength()} and {@code getHashForLength()} rely upon.
 * This can be useful for some in-place hash computations that yield the same result as aforementioned routines.
 * @param hash a value of the hash so far
 * @param ch a character to add
 * @return a combined hash value
 */
[[nodiscard]]
inline auto nextHashStep( uint32_t hash, char ch ) -> uint32_t {
	auto val = (unsigned char)ch;
	// Use a fast lowercase conversion for ASCII letters.
	// First, cast to a signed integer to get a difference.
	// Second, convert the data as unsigned to promote negative values to huge ones and use a single branch.
	if( ( (unsigned)( (int)val - 'A' ) ) <= 'Z' - 'A' ) {
		val = ( val - 'A' ) + 'a';
	} else if( val == '\\' ) {
		val = '/';
	}
	return hash * 37u + val;
}

}

unsigned int COM_SuperFastHash( const unsigned char * data, size_t len, unsigned int hash );
unsigned int COM_SuperFastHash64BitInt( uint64_t data );

#endif
