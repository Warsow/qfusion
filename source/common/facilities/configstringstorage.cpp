#include "configstringstorage.h"
#include <common/helpers/links.h>
#include <common/helpers/q_libc.h>

#include <cstddef>
#include <stdexcept>

namespace wsw {

inline auto ConfigStringStorage::getUnderlyingLocalStorage( const char *data ) -> ShortStringBlock * {
	const auto dataAddress = (uintptr_t)data;
	const auto minLocalAddress = (uintptr_t)( &m_localBlocks[0].buffer[0] );
	if( dataAddress - minLocalAddress > sizeof( m_localBlocks ) ) {
		return nullptr;
	}
	const auto blockAddress = ( (uintptr_t)dataAddress - offsetof( ShortStringBlock, buffer ) );
	assert( blockAddress % alignof( ShortStringBlock ) == 0 );
	return (ShortStringBlock *)blockAddress;
}

ConfigStringStorage::ConfigStringStorage() {
	std::memset( m_entries, 0, sizeof( m_entries ) );
	makeFreeListLinks();
}

ConfigStringStorage::~ConfigStringStorage() {
	freeEntries();
}

void ConfigStringStorage::freeEntries() {
	for( Entry &entry: m_entries ) {
		if( char *const data = entry.data ) {
			entry.data = nullptr;
			if( !getUnderlyingLocalStorage( data ) ) {
				Q_free( data );
			}
		}
	}
}

void ConfigStringStorage::makeFreeListLinks() {
	m_freeHead = nullptr;
	m_usedHead = nullptr;
	for( ShortStringBlock &s: m_localBlocks ) {
		wsw::link( std::addressof( s ), &m_freeHead );
	}
}

void ConfigStringStorage::assignEntryData( Entry *entry, char *data, size_t capacity, const wsw::StringView &string ) {
	assert( string.size() <= capacity );
	auto len = string.length();
	std::memcpy( data, string.data(), len );
	data[len] = '\0';

	entry->data = data;
	entry->len = len;
	entry->capacity = capacity;
}

void ConfigStringStorage::clear() {
	freeEntries();
	makeFreeListLinks();
}

void ConfigStringStorage::copyFrom( const ConfigStringStorage &from ) {
	// TODO: Optimize bounds checks
	for( unsigned i = 0; i < kMaxStrings; ++i ) {
		// TODO: Add a clearAt() call?
		setNoCheck( i, from.getNoCheck( i ).value_or( wsw::StringView() ) );
	}
}

auto ConfigStringStorage::getNoCheck( unsigned index ) const -> std::optional<wsw::StringView> {
	const auto &entry = m_entries[index];
	if( const char *data = entry.data ) {
		return wsw::StringView( data, entry.len, wsw::StringView::ZeroTerminated );
	}
	return std::nullopt;
}

void ConfigStringStorage::setNoCheck( unsigned index, const wsw::StringView &string ) {
	auto *const entry = &m_entries[index];
	const size_t len = string.length();

	if( !len ) {
		char *const data = m_entries[index].data;
		m_entries[index].data = nullptr;

		if( !data ) {
			return;
		}

		if( auto *block = getUnderlyingLocalStorage( data ) ) {
			wsw::unlink( block, &m_usedHead );
			wsw::link( block, &m_freeHead );
			return;
		}

		Q_free( data );
		return;
	}

	// There's no string at this index
	if( !entry->data ) {
		if( len <= kMaxShortStringSize && m_freeHead ) {
			auto *block = wsw::unlink( m_freeHead, &m_freeHead );
			wsw::link( block, &m_usedHead );
			assignEntryData( entry, block->buffer, kMaxShortStringSize, string );
			return;
		}
		assignEntryData( entry, (char *)( Q_malloc( len + 1 ) ), len, string );
		return;
	}

	// If a local storage was used for the string
	if( auto *block = getUnderlyingLocalStorage( entry->data ) ) {
		// The new string could fit the existing storage
		if( len <= kMaxShortStringSize ) {
			assignEntryData( entry, entry->data, kMaxShortStringSize, string );
			return;
		}
		wsw::unlink( block, &m_usedHead );
		wsw::link( block, &m_freeHead );
		assignEntryData( entry, (char *)( Q_malloc( len + 1 ) ), len, string );
		return;
	}

	// Check whether the buffer can and should be reused
	if( len < entry->capacity && 2 * len > entry->capacity && entry->capacity < 4096 ) {
		assignEntryData( entry, entry->data, entry->capacity, string );
	} else {
		// TODO: Avoid copying the old content while performing a realloc...
		assignEntryData( entry, (char *)Q_realloc( entry->data, len + 1 ), len, string );
	}
}

}