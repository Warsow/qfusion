#include "userinfo.h"

namespace wsw {

static const wsw::CharLookup kIllegalChars( wsw::StringView( "\\;\"" ) );

bool UserInfo::isValidKey( const wsw::StringView &key ) {
	return key.length() && key.length() < MAX_INFO_KEY && !key.containsAny( kIllegalChars );
}

bool UserInfo::isValidValue( const wsw::StringView &value ) {
	return value.length() < MAX_INFO_VALUE && !value.containsAny( kIllegalChars );
}

bool UserInfo::set( const wsw::HashedStringView &key, const wsw::StringView &value ) {
	if( isValidKey( key ) && isValidValue( value ) ) {
		if( auto it = m_keysAndValues.find( key ); it != m_keysAndValues.end() ) {
			auto *data = const_cast<char *>( it.value().data() );
			value.copyTo( data, MAX_INFO_VALUE );
			wsw::StringView ownedValue( data, value.length(), wsw::StringView::ZeroTerminated );
			// TODO: Use an insertion hint
			// TODO: Add a non-const iterator?
			m_keysAndValues.insertOrReplace( key, value );
			return true;
		}

		if( m_allocator.isFull() ) {
			return false;
		}

		assert( !m_keysAndValues.isFull() );

		// Allocate new data for the key and the value
		auto *data = (char *)m_allocator.allocOrNull();
		key.copyTo( data, MAX_INFO_KEY );
		const wsw::HashedStringView ownedKey( data, key.size(), wsw::StringView::ZeroTerminated );

		data += key.length() + 1;
		value.copyTo( data, MAX_INFO_VALUE );
		const wsw::HashedStringView ownedValue( data, value.size(), wsw::StringView::ZeroTerminated );

		// TODO: Use an insertion hint
		m_keysAndValues.insertOrThrow( ownedKey, ownedValue );
		return true;
	}

	return false;
}

void UserInfo::clear() {
	// The allocator .clear() call could be costly, check first
	if( !m_keysAndValues.empty() ) {
		m_keysAndValues.clear();
		m_allocator.clear();
	}
}

bool UserInfo::parse( const wsw::StringView &input ) {
	clear();
	if( !parse_( input.trim() ) ) {
		clear();
		return false;
	}
	return true;
}

bool UserInfo::parse_( const wsw::StringView &input ) {
	if( input.empty() ) {
		return true;
	}

	wsw::StringView view( input );
	if( !view.startsWith( '\\' ) ) {
		return false;
	}

	view = view.drop( 1 );
	for(;; ) {
		if( view.empty() ) {
			return true;
		}

		wsw::StringView key( view );
		const std::optional<unsigned> valueSlashIndex = view.indexOf( '\\' );
		if( !valueSlashIndex ) {
			return false;
		}
		key = key.take( *valueSlashIndex );
		if( !isValidKey( key ) ) {
			return false;
		}
		view = view.drop( *valueSlashIndex + 1 );

		wsw::StringView value( view );
		const std::optional<unsigned> nextKeySlashIndex = view.indexOf( '\\' );
		// There is a next pair
		if( nextKeySlashIndex ) {
			value = value.take( *nextKeySlashIndex );
			view = view.drop( *nextKeySlashIndex + 1 );
		} else {
			view = view.drop( view.length() );
		}
		if( !isValidValue( value ) ) {
			return false;
		}

		assert( !m_keysAndValues.isFull() );
		assert( !m_allocator.isFull() );

		if( !set( wsw::HashedStringView( key ), value ) ) {
			return false;
		}
	}
}

}

void Info_Print( char *s ) {
	char key[512];
	char value[512];
	char *o;
	int l;

	if( *s == '\\' ) {
		s++;
	}
	while( *s ) {
		o = key;
		while( *s && *s != '\\' )
			*o++ = *s++;

		l = o - key;
		if( l < 20 ) {
			memset( o, ' ', 20 - l );
			key[20] = 0;
		} else {
			*o = 0;
		}
		Com_Printf( "%s", key );

		if( !*s ) {
			Com_Printf( "MISSING VALUE\n" );
			return;
		}

		o = value;
		s++;
		while( *s && *s != '\\' )
			*o++ = *s++;
		*o = 0;

		if( *s ) {
			s++;
		}
		Com_Printf( "%s\n", value );
	}
}