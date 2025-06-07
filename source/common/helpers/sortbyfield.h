/*
Copyright (C) 2022-2024 Chasseur de bots

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

#ifndef WSW_71c96eea_c1cf_4f13_bb23_3d22d2b4b7e5_H
#define WSW_71c96eea_c1cf_4f13_bb23_3d22d2b4b7e5_H

#include <type_traits>
#include <cstdint>
#include <cstdlib>
#include <cassert>

namespace wsw {

namespace _details {

void sortStructsByFloatField( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );
void sortStructsByInt32Field( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );
void sortStructsByUInt32Field( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );

void sortStructsByFloatFieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );
void sortStructsByInt32FieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );
void sortStructsByUInt32FieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset );

}

// These subroutines act as lightweight specialized alternatives to std::sort()
// both in terms of compile time and in terms of reducing code bloat
// (template instantiations for different structs with the same size and sort field offset are shared)
// while preserving the same performance.

template <typename T, typename Field>
inline void sortByField( T *begin, T *end, Field T::*fieldPtr ) {
	static_assert( sizeof( T ) == 8, "Only 8-byte structs are supported so far" );
	static_assert( alignof( T ) == 4, "Only 4-byte aligned structs are supported so far" );
	static_assert( std::is_same_v<Field, float> || std::is_same_v<Field, int32_t> || std::is_same_v<Field, uint32_t> );

	const auto fieldRuntimeAddress = &( begin->*fieldPtr );
	const auto fieldOffset         = (uintptr_t)fieldRuntimeAddress - (uintptr_t)begin;
	assert( fieldOffset <= sizeof( T ) - sizeof( Field ) );

	// Call specialized implementations for each type

	if constexpr( std::is_same_v<Field, float> ) {
		wsw::_details::sortStructsByFloatField( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else if constexpr( std::is_same_v<Field, int32_t> ) {
		wsw::_details::sortStructsByInt32Field( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else if constexpr( std::is_same_v<Field, uint32_t> ) {
		wsw::_details::sortStructsByUInt32Field( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else {
		assert( false );
	}
}

template <typename T, typename Field>
inline void sortByFieldDescending( T *begin, T *end, Field T::*fieldPtr ) {
	static_assert( sizeof( T ) == 8, "Only 8-byte structs are supported so far" );
	static_assert( alignof( T ) == 4, "Only 4-byte aligned structs are supported so far" );
	static_assert( std::is_same_v<Field, float> || std::is_same_v<Field, int32_t> || std::is_same_v<Field, uint32_t> );

	const auto fieldRuntimeAddress = &( begin->*fieldPtr );
	const auto fieldOffset         = (uintptr_t)fieldRuntimeAddress - (uintptr_t)begin;
	assert( fieldOffset <= sizeof( T ) - sizeof( Field ) );

	// Call specialized implementations for each type

	if constexpr( std::is_same_v<Field, float> ) {
		wsw::_details::sortStructsByFloatFieldDescending( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else if constexpr( std::is_same_v<Field, int32_t> ) {
		wsw::_details::sortStructsByInt32FieldDescending( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else if constexpr( std::is_same_v<Field, uint32_t> ) {
		wsw::_details::sortStructsByUInt32FieldDescending( begin, end, sizeof( T ), alignof( T ), fieldOffset );
	} else {
		assert( false );
	}
}

}

#endif