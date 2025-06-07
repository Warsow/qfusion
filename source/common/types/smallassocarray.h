#ifndef WSW_0e010d8b_fcfd_448e_af29_aeee22d405f9_H
#define WSW_0e010d8b_fcfd_448e_af29_aeee22d405f9_H

#include "staticvector.h"
#include <common/helpers/exceptions.h>

#include <utility>
#include <cassert>

namespace wsw {

template <typename K, typename V, unsigned Capacity>
class SmallAssocArray {
	// TODO: We can use specializations for small/primitive POD types
	// that keep keys and values separate to utilize a SIMD lookup
	wsw::StaticVector<std::pair<K, V>, Capacity> m_entries;
public:
	class const_iterator {
		const std::pair<K, V> *m_p;
	public:
		explicit const_iterator( const std::pair<K, V> *p ) noexcept : m_p( p ) {}
		[[nodiscard]]
		auto operator*() const noexcept -> const std::pair<K, V> & { return *m_p; }
		[[nodiscard]]
		auto key() const noexcept -> const K & { return m_p->first; }
		[[nodiscard]]
		auto value() const noexcept -> const V & { return m_p->second; }
		[[nodiscard]]
		bool operator==( const const_iterator &that ) const noexcept { return m_p == that.m_p; }
		[[nodiscard]]
		bool operator!=( const const_iterator &that ) const noexcept { return m_p != that.m_p; }
		[[maybe_unused]]
		auto operator++() -> const_iterator { ++m_p; return *this; }
		[[nodiscard]]
		auto operator++(int) -> const_iterator { auto result( *this ); m_p++; return result; }
	};

	[[nodiscard]]
	auto find( const K &key ) const noexcept -> const_iterator {
		for( const auto &entry: m_entries ) {
			if( entry.first == key ) {
				return const_iterator( std::addressof( entry ) );
			}
		}
		return cend();
	}

	[[nodiscard]]
	bool contains( const K &key ) const noexcept { return find( key ) != cend(); }

	[[nodiscard]]
	auto operator[]( const K key ) const -> const V & {
		if( const auto it = find( key ); it != cend() ) {
			return it.value();
		}
		wsw::failWithOutOfRange( "Failed to find an entry by key" );
	}

	[[nodiscard]]
	auto insert( const K &key, const V &value ) -> std::pair<const_iterator, bool> {
		if( const auto it = find( key ); it != cend() ) {
			return { it, false };
		}
		const auto oldSize = m_entries.size();
		m_entries.push_back( { key, value } );
		return { const_iterator( m_entries.data() + oldSize ), true };
	}

	// Bits of STL compatibility
	[[maybe_unused]]
	auto insert_or_assign( const K &key, const V &value ) -> const_iterator {
		for( auto &entry: m_entries ) {
			if( entry.first == key ) {
				entry.second = value;
				return const_iterator( std::addressof( entry ) );
			}
		}
		const auto oldSize = m_entries.size();
		m_entries.push_back( { key, value } );
		return const_iterator( m_entries.data() + oldSize );
	}

	[[maybe_unused]]
	auto insertOrReplace( const K &key, const V &value ) -> const_iterator {
		return insert_or_assign( key, value );
	}

	void insertOrThrow( const K &key, const V &value ) {
		auto [_, success] = insert( key, value );
		if( !success ) {
			wsw::failWithLogicError( "This key is already present" );
		}
	}

	void erase( const const_iterator &iterator ) {
		assert( iterator != cend() );
		assert( (size_t)( iterator.m_p - m_entries.data() ) < (size_t)m_entries.size() );
		m_entries.erase( iterator.m_p );
	}

	[[nodiscard]]
	bool remove( const K &key ) {
		if( const auto it = find( key ); it != cend() ) {
			erase( it );
			return true;
		}
		return false;
	}

	void clear() { m_entries.clear(); }

	[[nodiscard]]
	bool empty() const { return m_entries.empty(); }
	[[nodiscard]]
	bool full() const { return m_entries.full(); }
	[[nodiscard]]
	bool isEmpty() const { return m_entries.empty(); }
	[[nodiscard]]
	bool isFull() const { return m_entries.full(); }
	[[nodiscard]]
	auto size() const -> unsigned { return m_entries.size(); }
	[[nodiscard]]
	static constexpr auto capacity() -> unsigned { return Capacity; }

	[[nodiscard]]
	auto begin() const noexcept -> const_iterator { return cbegin(); }
	[[nodiscard]]
	auto end() const noexcept -> const_iterator { return cend(); }
	[[nodiscard]]
	auto cbegin() const noexcept -> const_iterator { return const_iterator( m_entries.data() ); }
	[[nodiscard]]
	auto cend() const noexcept -> const_iterator { return const_iterator( m_entries.data() + m_entries.size() ); }
};

}

#endif