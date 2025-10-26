/*
Copyright (C) 2025 vvk2212, Chasseur de bots

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

#include "actiontape.h"
#include <common/helpers/wswbasicmath.h>
#include <common/helpers/exceptions.h>

#include <cstdint>
#include <typeinfo>

namespace wsw {

BaseActionTape::~BaseActionTape() {
	clear();
	std::free( m_basePtr );
}

auto BaseActionTape::allocActionMem( size_t requiredSize ) -> void * {
	if( m_sizeInBytes + requiredSize > m_capacityInBytes ) [[unlikely]] {
		size_t newCapacityInBytes;
		if( m_capacityInBytes < 1024 ) [[unlikely]] {
			newCapacityInBytes = 3072;
		} else {
			newCapacityInBytes = ( 3 * m_capacityInBytes ) / 2;
		}
		newCapacityInBytes = wsw::max<size_t>( newCapacityInBytes, m_capacityInBytes + requiredSize );
		// TODO: Just realloc if all callables (so far) are trivially relocatable
		auto *newBasePtr   = (uint8_t *)std::malloc( newCapacityInBytes );
		if( !newBasePtr ) [[unlikely]] {
			wsw::failWithBadAlloc();
		}

		// Everything below in this branch must not throw

		size_t offset = 0;
		while( offset < m_sizeInBytes ) {
			auto *const oldAction    = (BaseAction *)( m_basePtr + offset );
			auto *const newActionMem = newBasePtr + offset;
			oldAction->moveTo( newActionMem );
			auto *const newAction    = (BaseAction *)newActionMem;
			assert( oldAction->m_sizeOnTape == newAction->m_sizeOnTape );
			// TODO: Is it going to resolve the actual descendant type?
			assert( typeid( *oldAction ) == typeid( *oldAction ) );
			assert( oldAction->m_sizeOnTape > 0 );
			offset += oldAction->m_sizeOnTape;
			oldAction->~BaseAction();
		}
		assert( offset == m_sizeInBytes );

		std::free( m_basePtr );
		m_capacityInBytes = newCapacityInBytes;
		m_basePtr         = newBasePtr;
	}

	void *result = (uint8_t *)m_basePtr + m_sizeInBytes;
	m_sizeInBytes += requiredSize;
	assert( ( (uintptr_t)result ) % sizeof( void * ) == 0 );
	return result;
}

void BaseActionTape::clear() {
	size_t offset = 0;
	while( offset < m_sizeInBytes ) {
		auto *const action = (BaseAction *)( m_basePtr + offset );
		offset += action->m_sizeOnTape;
		action->~BaseAction();
	}
	assert( offset == m_sizeInBytes );
	m_sizeInBytes = 0;
}

}