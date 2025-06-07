/*
Copyright (C) 2024 Chasseur de bots

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

#ifndef WSW_92ce982b_5da9_4825_8e57_43a3a2fff11b_H
#define WSW_92ce982b_5da9_4825_8e57_43a3a2fff11b_H

#include <cassert>
#include <concepts>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <type_traits>

void memset16( void *__restrict dest, uint16_t value, size_t count );
void memset32( void *__restrict dest, uint32_t value, size_t count );
void memset64( void *__restrict dest, uint64_t value, size_t count );

namespace wsw {

class BasePodVector {
protected:
	~BasePodVector() { destroy(); }

	enum AllocGrowthPolicy { Optimal, Exact };

	// TODO: Add alignment argument
	void ensureCapacityInElems( size_t newCapacityInElems, size_t elemSize, AllocGrowthPolicy growthPolicy ) noexcept( false );

	void moveFromThat( BasePodVector *that ) {
		m_dataBytes             = that->m_dataBytes;
		m_capacityInElems       = that->m_capacityInElems;
		m_sizeInElems           = that->m_sizeInElems;
		that->m_dataBytes       = nullptr;
		that->m_capacityInElems = 0;
		that->m_sizeInElems     = 0;
	}

	void constructFromThat( const BasePodVector &that, size_t elemSize );
	void copyFromThat( const BasePodVector &that, size_t elemSize );

	void reserveImpl( size_t minDesiredCapacity, size_t elemSize );
	void expandImpl( size_t position, size_t elemSize, size_t numElems );
	void eraseImpl( size_t position, size_t elemSize, size_t numElems );
	void shrinkImpl( size_t elemSize );

	// TODO: Destroy must accept alignment
	void destroy() noexcept;

	// TODO: Check alignment
	uint8_t *m_dataBytes { nullptr };
	size_t m_capacityInElems { 0 };
	size_t m_sizeInElems { 0 };
};

template <typename T>
class PodVector final : BasePodVector {
	// TODO: We have to assume that std::optional/std::variant/std::pair of POD types are always trivially-relocatable.
	//static_assert( std::is_trivially_destructible_v<T> && std::is_trivially_copyable_v<T> );
	//static_assert( alignof( T ) <= alignof( void * ) );
public:
	using size_type       = size_t;
	using iterator        = T *;
	using const_iterator  = const T *;
	using reference       = T &;
	using const_reference = const T &;

	PodVector() = default;

	PodVector( std::initializer_list<T> elems ) : PodVector() {
		assign( elems.begin(), elems.size() );
	}

	PodVector( const T *elems, size_t numElems ) : PodVector() {
		assign( elems, numElems );
	}

	// std::string compatibility
	// TODO: We can't define class-scoped concepts so we have to copy-paste it
	template <typename Container>
		requires requires( const Container &c ) {
			{ c.data() } -> std::same_as<const T *>;
			{ c.size() } -> std::integral;
		}
	explicit PodVector( const Container &values ) : PodVector() {
		assign( values.data(), values.size() );
	}

	PodVector( const PodVector<T> &that ) {
		constructFromThat( that, sizeof( T ) );
	}

	[[maybe_unused]]
	auto operator=( const PodVector<T> &that ) -> PodVector<T> & {
		copyFromThat( that, sizeof( T ) );
		return *this;
	}

	PodVector( PodVector<T> &&that ) noexcept {
		moveFromThat( &that );
	}

	[[maybe_unused]]
	auto operator=( PodVector<T> &&that ) noexcept -> PodVector<T> & {
		if( m_capacityInElems ) [[unlikely]] {
			destroy();
		}
		moveFromThat( &that );
		return *this;
	}

	[[nodiscard]] auto data() -> T * { return ( T *)m_dataBytes; }
	[[nodiscard]] auto data() const -> const T * { return ( T *)m_dataBytes; }

	[[nodiscard]] auto begin() -> iterator { return data(); }
	[[nodiscard]] auto end() -> iterator { return data() + m_sizeInElems; }

	[[nodiscard]] auto begin() const -> const_iterator { return data(); }
	[[nodiscard]] auto end() const -> const_iterator { return data() + m_sizeInElems; }

	[[nodiscard]] auto cbegin() -> const_iterator { return data(); }
	[[nodiscard]] auto cend() -> const_iterator { return data() + m_sizeInElems; };

	[[nodiscard]] auto size() const -> size_type { return m_sizeInElems; }
	[[nodiscard]] bool empty() const { return m_sizeInElems == 0; }
	[[nodiscard]] bool isEmpty() const { return m_sizeInElems == 0; }

	[[nodiscard]]
	auto front() -> reference {
		assert( !empty() );
		return data()[0];
	}
	[[nodiscard]]
	auto front() const -> const_reference {
		assert( !empty() );
		return data()[0];
	}

	[[nodiscard]]
	auto back() -> reference {
		const size_t actualSize = size();
		assert( actualSize );
		return data()[actualSize - 1];
	}
	[[nodiscard]]
	auto back() const -> const_reference {
		const size_t actualSize = size();
		assert( actualSize );
		return data()[actualSize - 1];
	}

	[[nodiscard]]
	auto operator[]( size_type index ) -> reference {
		assert( index < size() );
		return data()[index];
	}
	[[nodiscard]]
	auto operator[]( size_type index ) const -> const_reference {
		assert( index < size() );
		return data()[index];
	}

	void clear() { m_sizeInElems = 0; }

	void reserve( size_t capacityInElems ) {
		reserveImpl( capacityInElems, sizeof( T ) );
	}

	void shrink_to_fit() {
		shrinkImpl( sizeof( T ) );
	}

	void resize( size_type newSize, const T &fillBy = T {} ) {
		if( newSize > m_sizeInElems ) {
			if( m_capacityInElems < newSize ) {
				ensureCapacityInElems( newSize, sizeof( T ), Optimal );
			}
			fill( m_sizeInElems, fillBy, newSize - m_sizeInElems );
		}
		m_sizeInElems = newSize;
	}

	void assign( size_type count, const T &fillBy ) {
		clear();
		resize( count, fillBy );
	}

	void assign( const T *rangeBegin, const T *rangeEnd ) {
		clear();
		assert( rangeEnd >= rangeBegin );
		insert( end(), rangeBegin, (size_t)( rangeEnd - rangeBegin ) );
	}

	void assign( const T *elems, size_type count ) {
		clear();
		insert( end(), elems, count );
	}

	// std::string compatibility
	template <typename Container>
		requires requires( const Container &c ) {
			{ c.data() } -> std::same_as<const T *>;
			{ c.size() } -> std::integral;
		}
	void assign( const Container &values ) {
		assign( values.data(), values.size() );
	}

	void insert( iterator position, const T *rangeBegin, const T *rangeEnd ) {
		assert( rangeEnd >= rangeBegin );
		insert( position, rangeBegin, (size_t)( rangeEnd - rangeBegin ) );
	}

	void insert( iterator position, const T *elems, size_type count ) {
		const auto index = convertIteratorToIndex( position );
		expandImpl( index, sizeof( T ), count );
		std::memcpy( m_dataBytes + sizeof( T ) * index, elems, sizeof( T ) * count );
		m_sizeInElems += count;
	}

	void insert( iterator position, const T &value ) {
		const auto index = convertIteratorToIndex( position );
		expandImpl( index, sizeof( T ), 1 );
		new( m_dataBytes + sizeof( T ) * index )T( value );
		m_sizeInElems++;
	}

	void insert( iterator position, size_type count, const T &value ) {
		const auto index = convertIteratorToIndex( position );
		expandImpl( index, sizeof( T ), count );
		fill( index, value, count );
		m_sizeInElems += count;
	}

	template <typename Container>
		requires requires( const Container &c ) {
			{ c.data() } -> std::same_as<const T *>;
			{ c.size() } -> std::integral;
		}
	void insert( iterator position, const Container &values ) {
		insert( position, values.data(), values.size() );
	}

	void append( const T *rangeBegin, const T *rangeEnd ) {
		assert( rangeEnd >= rangeBegin );
		append( rangeBegin, (size_type)( rangeEnd - rangeBegin ) );
	}

	void append( const T *elems, size_type count ) {
		insert( end(), elems, count );
	}

	// std::string compatibility
	template <typename Container>
		requires requires( const Container &c ) {
			{ c.data() } -> std::same_as<const T *>;
			{ c.size() } -> std::integral;
		}
	void append( const Container &values ) {
		append( values.data(), values.size() );
	}

	void erase( const_iterator rangeBegin, const_iterator rangeEnd ) {
		assert( rangeEnd >= rangeBegin );
		erase( begin() + ( rangeBegin - cbegin() ), (size_type)( rangeEnd - rangeBegin ) );
	}

	void erase( iterator position, size_type count = 1 ) {
		eraseImpl( convertIteratorToIndex( position ), sizeof( T ), count );
	}

	// Inline-friendly
	void push_back( const T &value ) {
		if( m_sizeInElems == m_capacityInElems ) [[unlikely]] {
			ensureCapacityInElems( m_sizeInElems + 1, sizeof( T ), Optimal );
		}
		// This could be cheaper than explicit memcpy() even if calling constructors is not required
		new( m_dataBytes + sizeof( T ) * m_sizeInElems )T( value );
		++m_sizeInElems;
	}

	// Inline-friendly
	void emplace_back( T &&value ) {
		push_back( value );
	}

	void append( const T &value ) {
		push_back( value );
	}

	void pop_back() {
		assert( m_sizeInElems );
		// Trivially destructible
		--m_sizeInElems;
	}
private:
	[[nodiscard]]
	auto convertIteratorToIndex( iterator it ) const -> size_type {
		assert( (uintptr_t)it >= (uintptr_t)m_dataBytes );
		assert( (uintptr_t)it <= (uintptr_t)m_dataBytes + sizeof( T ) * m_sizeInElems );
		return (size_type)( it - begin() );
	}

	void fill( size_type position, const T &fillBy, size_type count ) {
		uint8_t *__restrict writePtr = m_dataBytes + sizeof( T ) * position;
		if constexpr( sizeof( T ) == 1 && alignof( T ) == 1 ) {
			std::memset( writePtr, *( (uint8_t *)&fillBy ), count );
		} else if constexpr( sizeof( T ) == 2 && alignof( T ) == 2 ) {
			memset16( writePtr, *( (uint16_t *)&fillBy ), count );
		} else if constexpr( sizeof( T ) == 4 && alignof( T ) == 4 ) {
			memset32( writePtr, *( (uint32_t *)&fillBy ), count );
		} else if constexpr( sizeof( T ) == 8 && alignof( T ) == 8 ) {
			memset64( writePtr, *( (uint64_t *)&fillBy ), count );
		} else {
			size_t elemNum = 0;
			do {
				new( writePtr )T( fillBy );
				writePtr += sizeof( T );
			} while( ++elemNum < count );
		}
	}
};

}

#endif