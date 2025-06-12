#include "userinfo.h"

#include <common/helpers/q_libc.h>
#include <common/helpers/q_math.h>
#include <common/facilities/messagestreams.h>

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

/*
* Info_ValidateValue
*/
static bool Info_ValidateValue( const char *value ) {
	assert( value );

	if( !value ) {
		return false;
	}

	if( strlen( value ) >= MAX_INFO_VALUE ) {
		return false;
	}

	if( strchr( value, '\\' ) ) {
		return false;
	}

	if( strchr( value, ';' ) ) {
		return false;
	}

	if( strchr( value, '"' ) ) {
		return false;
	}

	return true;
}

/*
* Info_ValidateKey
*/
static bool Info_ValidateKey( const char *key ) {
	assert( key );

	if( !key ) {
		return false;
	}

	if( !key[0] ) {
		return false;
	}

	if( strlen( key ) >= MAX_INFO_KEY ) {
		return false;
	}

	if( strchr( key, '\\' ) ) {
		return false;
	}

	if( strchr( key, ';' ) ) {
		return false;
	}

	if( strchr( key, '"' ) ) {
		return false;
	}

	return true;
}

/*
* Info_Validate
*
* Some characters are illegal in info strings because they
* can mess up the server's parsing
*/
bool Info_Validate( const char *info ) {
	const char *p, *start;

	assert( info );

	if( !info ) {
		return false;
	}

	if( strlen( info ) >= MAX_INFO_STRING ) {
		return false;
	}

	if( strchr( info, '\"' ) ) {
		return false;
	}

	if( strchr( info, ';' ) ) {
		return false;
	}

	if( strchr( info, '"' ) ) {
		return false;
	}

	p = info;

	while( p && *p ) {
		if( *p++ != '\\' ) {
			return false;
		}

		start = p;
		p = strchr( start, '\\' );
		if( !p ) { // missing key
			return false;
		}
		if( p - start >= MAX_INFO_KEY ) { // too long
			return false;
		}

		p++; // skip the \ char

		start = p;
		p = strchr( start, '\\' );
		if( ( p && p - start >= MAX_INFO_KEY ) || ( !p && strlen( start ) >= MAX_INFO_KEY ) ) { // too long
			return false;
		}
	}

	return true;
}

/*
* Info_CleanValue
*
* Removes invalid characters from a userinfo value.
* Used internally by steamlib at least.
*/
void Info_CleanValue( const char *in, char *out, size_t outsize ) {
	size_t len = 0;
	int c;

	if( !outsize ) {
		return;
	}

	clamp_high( outsize, MAX_INFO_VALUE );

	while( ( len + 1 < outsize ) && ( ( ( c = *in ) != '\0' ) ) ) {
		in++;

		if( c == '\\' ) {
			continue;
		}
		if( c == ';' ) {
			continue;
		}
		if( c == '"' ) {
			continue;
		}

		out[len++] = c;
	}

	out[len] = '\0';
}

/*
* Info_FindKey
*
* Returns the pointer to the \ character if key is found
* Otherwise returns NULL
*/
static char *Info_FindKey( const char *info, const char *key ) {
	const char *p, *start;
	size_t key_len;

	assert( Info_Validate( info ) );
	assert( Info_ValidateKey( key ) );

	if( !Info_Validate( info ) || !Info_ValidateKey( key ) ) {
		return NULL;
	}

	p = info;
	key_len = strlen( key );

	while( p && *p ) {
		start = p;

		p++; // skip the \ char
		if( !strncmp( key, p, key_len ) && p[key_len] == '\\' ) {
			return (char *)start;
		}

		p = strchr( p, '\\' );
		if( !p ) {
			return NULL;
		}

		p++; // skip the \ char
		p = strchr( p, '\\' );
	}

	return NULL;
}

/*
* Info_ValueForKey
*
* Searches the string for the given
* key and returns the associated value, or NULL
*/
char *Info_ValueForKey( const char *info, const char *key ) {
	static char value[2][MAX_INFO_VALUE]; // use two buffers so compares work without stomping on each other
	static int valueindex;
	const char *p, *start;
	size_t len;

	assert( info && Info_Validate( info ) );
	assert( key && Info_ValidateKey( key ) );

	if( !Info_Validate( info ) || !Info_ValidateKey( key ) ) {
		return NULL;
	}

	valueindex ^= 1;

	p = Info_FindKey( info, key );
	if( !p ) {
		return NULL;
	}

	p++; // skip the \ char
	p = strchr( p, '\\' );
	if( !p ) {
		return NULL;
	}

	p++; // skip the \ char
	start = p;
	p = strchr( p, '\\' );
	if( !p ) {
		len = strlen( start );
	} else {
		len = p - start;
	}

	if( len >= MAX_INFO_VALUE ) {
		assert( false );
		return NULL;
	}
	strncpy( value[valueindex], start, len );
	value[valueindex][len] = 0;

	return value[valueindex];
}

/*
* Info_RemoveKey
*/
void Info_RemoveKey( char *info, const char *key ) {
	assert( info && Info_Validate( info ) );
	assert( key && Info_ValidateKey( key ) );

	if( !Info_Validate( info ) || !Info_ValidateKey( key ) ) {
		return;
	}

	while( 1 ) {
		char *start, *p;

		p = Info_FindKey( info, key );
		if( !p ) {
			return;
		}

		start = p;

		p++; // skip the \ char
		p = strchr( p, '\\' );
		if( p ) {
			p++; // skip the \ char
			p = strchr( p, '\\' );
		}

		if( !p ) {
			*start = 0;
		} else {
			// aiwa : fixed possible source and destination overlap with strcpy()
			memmove( start, p, strlen( p ) + 1 );
		}
	}
}

/*
* Info_SetValueForKey
*/
bool Info_SetValueForKey( char *info, const char *key, const char *value ) {
	char pair[MAX_INFO_KEY + MAX_INFO_VALUE + 1];

	assert( info && Info_Validate( info ) );
	assert( key && Info_ValidateKey( key ) );
	assert( value && Info_ValidateValue( value ) );

	if( !Info_Validate( info ) || !Info_ValidateKey( key ) || !Info_ValidateValue( value ) ) {
		return false;
	}

	Info_RemoveKey( info, key );

	Q_snprintfz( pair, sizeof( pair ), "\\%s\\%s", key, value );

	if( strlen( pair ) + strlen( info ) > MAX_INFO_STRING ) {
		return false;
	}

	Q_strncatz( info, pair, MAX_INFO_STRING );

	return true;
}