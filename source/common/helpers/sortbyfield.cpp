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

#include "sortbyfield.h"
#include <common/helpers/wswbasicarch.h>

#include <algorithm>

namespace wsw::_details {

// This template gets used to actually generate specialized implementations

template <typename Field, size_t Size, size_t Alignment, bool Ascending>
wsw_forceinline void sortStructsByFieldImpl( void *begin, void *end, uintptr_t fieldOffset ) {
	struct alignas( Alignment ) Struct { char _contents[Size]; };
	static_assert( alignof( Struct ) == Alignment && sizeof( Struct ) == Size );

	assert( ( (uintptr_t)begin % Alignment ) == 0 );
	assert( ( (uintptr_t)end % Alignment ) == 0 );

	std::sort( (Struct *)begin, (Struct *)end, [fieldOffset]( const Struct &lhs, const Struct &rhs ) -> bool {
		const auto rawLeftStructBytes  = (const uint8_t *)std::addressof( lhs );
		const auto rawRightStructBytes = (const uint8_t *)std::addressof( rhs );
		const auto leftField           = *( (Field *)( rawLeftStructBytes + fieldOffset ) );
		const auto rightField          = *( (Field *)( rawRightStructBytes + fieldOffset ) );
		if constexpr( Ascending ) {
			return leftField < rightField;
		} else {
			return rightField < leftField;
		}
	});
}

void sortStructsByFloatField( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<float, 8, 4, true>( begin, end, fieldOffset );
}

void sortStructsByInt32Field( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<int32_t, 8, 4, true>( begin, end, fieldOffset );
}

void sortStructsByUInt32Field( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<uint32_t, 8, 4, true>( begin, end, fieldOffset );
}

void sortStructsByFloatFieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<float, 8, 4, false>( begin, end, fieldOffset );
}

void sortStructsByInt32FieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<int32_t, 8, 4, false>( begin, end, fieldOffset );
}

void sortStructsByUInt32FieldDescending( void *begin, void *end, size_t size, size_t alignment, uintptr_t fieldOffset ) {
	assert( size == 8 && alignment == 4 );
	sortStructsByFieldImpl<uint32_t, 8, 4, false>( begin, end, fieldOffset );
}

}