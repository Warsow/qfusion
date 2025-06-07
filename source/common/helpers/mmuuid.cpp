/*
Copyright (C) 2023 Chasseur de bots

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

#include "mmuuid.h"
#include <common/facilities/q_shared.h>
#include <common/types/staticstring.h>

auto Uuid_FromString( const wsw::StringView &string ) -> std::optional<mm_uuid_t> {
	if( string.length() == UUID_DATA_LENGTH ) {
		wsw::StaticString<UUID_DATA_LENGTH> buffer;
		buffer << string;
		mm_uuid_t uuid;
		if( Uuid_FromString( buffer.data(), &uuid ) ) {
			return uuid;
		}
	}
	return std::nullopt;
}

#ifndef _WIN32

#include <uuid/uuid.h>

mm_uuid_t *Uuid_FromString( const char *buffer, mm_uuid_t *dest ) {
	if( ::uuid_parse( buffer, (uint8_t *)dest ) < 0 ) {
		return nullptr;
	}
	return dest;
}

char *Uuid_ToString( char *buffer, mm_uuid_t uuid ) {
	::uuid_unparse( (uint8_t *)&uuid, buffer );
	return buffer;
}

mm_uuid_t mm_uuid_t::Random() {
	mm_uuid_t result;
	::uuid_generate( (uint8_t *)&result );
	return result;
}

#else

// It's better to avoid using platform formatting routines on Windows.
// A brief look at the API's provided (some allocations are involved)
// is sufficient to alienate a coder.

mm_uuid_t *Uuid_FromString( const char *buffer, mm_uuid_t *dest ) {
	unsigned long long groups[5];
	int expectedHyphenIndices[4] = { 8, 13, 18, 23 };
	char stub[1] = { '\0' };
	char *endptr = stub;

	if( !buffer ) {
		return NULL;
	}

	const char *currptr = buffer;
	for( int i = 0; i < 5; ++i ) {
		groups[i] = strtoull( currptr, &endptr, 16 );
		if( groups[i] == ULLONG_MAX && errno == ERANGE ) {
			return NULL;
		}
		if( *endptr != '-' ) {
			if( i != 4 && *endptr != '\0' ) {
				return NULL;
			}
		} else if( endptr - buffer != expectedHyphenIndices[i] ) {
			return NULL;
		}
		currptr = endptr + 1;
	}

	// If there are any trailing characters
	if( *endptr != '\0' ) {
		return NULL;
	}

	dest->hiPart = ( ( ( groups[0] << 16 ) | groups[1] ) << 16 ) | groups[2];
	dest->loPart = ( groups[3] << 48 ) | groups[4];
	return dest;
}

char *Uuid_ToString( char *buffer, const mm_uuid_t uuid ) {
	const char *format = "%08" PRIx64 "-%04" PRIx64 "-%04" PRIx64 "-%04" PRIx64 "-%012" PRIx64;
	uint64_t groups[5];
	groups[0] = ( uuid.hiPart >> 32 ) & 0xFFFFFFFFull;
	groups[1] = ( uuid.hiPart >> 16 ) & 0xFFFF;
	groups[2] = ( uuid.hiPart >> 00 ) & 0xFFFF;
	groups[3] = ( uuid.loPart >> 48 ) & 0xFFFF;
	groups[4] = ( uuid.loPart >> 00 ) & 0xFFFFFFFFFFFFull;
	Q_snprintfz( buffer, UUID_BUFFER_SIZE, format, groups[0], groups[1], groups[2], groups[3], groups[4] );
	return buffer;
}

#include <Objbase.h>

mm_uuid_t mm_uuid_t::Random() {
	static_assert( sizeof( mm_uuid_t ) == sizeof( GUID ), "" );
	mm_uuid_t result;
	(void)::CoCreateGuid( (GUID *)&result );
	return result;
}

#endif