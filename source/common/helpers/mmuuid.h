/*
Copyright (C) 1997-2001 Id Software, Inc.

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

// mm_common.h -- matchmaker definitions for client and server exe's (not modules)

#ifndef __MM_COMMON_H
#define __MM_COMMON_H

#include <optional>
#include <cstdint>

namespace wsw { class StringView; }

// We were thinking about using strings without much care about actual uuid representation,
// but string manipulation turned to be painful in the current codebase state,
// so its better to introduce this value-type.
struct mm_uuid_t {
	uint64_t hiPart;
	uint64_t loPart;

	mm_uuid_t(): hiPart( 0 ), loPart( 0 ) {}

	mm_uuid_t( uint64_t hiPart_, uint64_t loPart_ )
		: hiPart( hiPart_ ), loPart( loPart_ ) {}

	bool operator==( const mm_uuid_t &that ) const;
	bool operator!=( const mm_uuid_t &that ) const {
		return !( *this == that );
	}

	bool IsZero() const;
	bool IsFFFs() const;
	bool IsValidSessionId() const;

	char *ToString( char *buffer ) const;
	static mm_uuid_t *FromString( const char *buffer, mm_uuid_t *dest );
	static mm_uuid_t Random();
};

// Let pass non-modified parameters by value to reduce visual clutter
static inline bool Uuid_Compare( mm_uuid_t u1, mm_uuid_t u2 ) {
	return u1.hiPart == u2.hiPart && u1.loPart == u2.loPart;
}

static inline mm_uuid_t Uuid_ZeroUuid() {
	mm_uuid_t result = { 0, 0 };
	return result;
}

static inline mm_uuid_t Uuid_FFFsUuid() {
	mm_uuid_t result = { (uint64_t)-1, (uint64_t)-1 };
	return result;
}

#define UUID_DATA_LENGTH ( 36 )
#define UUID_BUFFER_SIZE ( UUID_DATA_LENGTH + 1 )

mm_uuid_t *Uuid_FromString( const char *buffer, mm_uuid_t *dest );

[[nodiscard]]
auto Uuid_FromString( const wsw::StringView &buffer ) -> std::optional<mm_uuid_t>;

char *Uuid_ToString( char *buffer, mm_uuid_t uuid );

static inline bool Uuid_IsValidSessionId( mm_uuid_t uuid ) {
	if( uuid.hiPart == 0 && uuid.loPart == 0 ) {
		return false;
	}
	if( uuid.hiPart == (uint64_t)-1 && uuid.loPart == (uint64_t)-1 ) {
		return false;
	}
	return true;
}

static inline bool Uuid_IsZeroUuid( mm_uuid_t uuid ) {
	return uuid.hiPart == 0 && uuid.loPart == 0;
}

static inline bool Uuid_IsFFFsUuid( mm_uuid_t uuid ) {
	return uuid.hiPart == (uint64_t)-1 && uuid.loPart == (uint64_t)-1;
}

inline bool mm_uuid_t::operator==( const mm_uuid_t &that ) const {
	return Uuid_Compare( *this, that );
}

inline bool mm_uuid_t::IsZero() const {
	return Uuid_IsZeroUuid( *this );
}

inline bool mm_uuid_t::IsFFFs() const {
	return Uuid_IsFFFsUuid( *this );
}

inline bool mm_uuid_t::IsValidSessionId() const {
	return Uuid_IsValidSessionId( *this );
}

inline char *mm_uuid_t::ToString( char *buffer ) const {
	Uuid_ToString( buffer, *this );
	return buffer;
}

inline mm_uuid_t *mm_uuid_t::FromString( const char *buffer, mm_uuid_t *dest ) {
	return Uuid_FromString( buffer, dest );
}

#endif
