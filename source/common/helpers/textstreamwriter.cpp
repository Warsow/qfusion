// TODO: Use dynamic dispatch to hide OutputMessageWriter type
#include <common/facilities/messagestreams.h>
#include "textstreamwriter.h"

#include <cstring>
#include <cstdio>
#include <charconv>
#include <cassert>

namespace wsw {

template <typename T>
wsw_forceinline void TextStreamWriter::writeFloatingPointValue( T value ) {
	static_assert( std::is_same_v<std::remove_cvref_t<T>, float> || std::is_same_v<std::remove_cvref_t<T>, double> );
	const size_t separatorLen  = ( hasPendingSeparator ? 1 : 0 );
	// TODO: Is it sufficient for edge cases?
	constexpr size_t numberLen = std::is_same_v<std::remove_cvref<T>, float> ? 32 : 96;
	const size_t sizeToReserve = separatorLen + numberLen + 1;
	if( char *bufferChars = m_stream->reserve( sizeToReserve ) ) [[likely]] {
		bufferChars[0] = separatorChar;
		int res;
		// Unfortunately, we have to fall back to snprintf due to
		// a lack of full std::to_chars/std::format() support
		if constexpr( std::is_same_v<std::remove_cvref_t<T>, double> ) {
			res = snprintf( bufferChars + separatorLen, numberLen + 1, "%lf", value );
		} else {
			res = snprintf( bufferChars + separatorLen, numberLen + 1, "%f", value );
		}
		if( res > 0 && (size_t)res <= numberLen ) {
			const size_t charsWritten = (size_t)res + separatorLen;
			m_stream->advance( charsWritten );
			hasPendingSeparator = usePendingSeparators;
		}
	}
}

template <typename T>
wsw_forceinline void TextStreamWriter::writeIntegralValue( T value ) {
	const size_t separatorLen  = ( hasPendingSeparator ? 1 : 0 );
	constexpr size_t numberLen = 32; // TODO???
	const size_t sizeToReserve = numberLen + separatorLen;
	if( char *const bufferChars = m_stream->reserve( sizeToReserve ) ) [[likely]] {
		bufferChars[0] = separatorChar;
		char *const toCharsBegin = bufferChars + separatorLen;
		char *const toCharsEnd  = bufferChars + separatorLen + numberLen;
		const auto [ptr, err] = std::to_chars( toCharsBegin, toCharsEnd, value );
		if( err == std::errc() ) [[likely]] {
			m_stream->advance( (size_t)( ptr - toCharsBegin ) + separatorLen );
			hasPendingSeparator = usePendingSeparators;
		}
	}
}

void TextStreamWriter::writeInt8( int8_t value ) {
	writeIntegralValue<int8_t>( value );
}

void TextStreamWriter::writeInt16( int16_t value ) {
	writeIntegralValue<int16_t>( value );
}

void TextStreamWriter::writeInt32( int32_t value ) {
	writeIntegralValue<int32_t>( value );
}

void TextStreamWriter::writeInt64( int64_t value ) {
	writeIntegralValue<int64_t>( value );
}

void TextStreamWriter::writeUInt8( uint8_t value ) {
	writeIntegralValue<uint8_t>( value );
}

void TextStreamWriter::writeUInt16( uint16_t value ) {
	writeIntegralValue<uint16_t>( value );
}

void TextStreamWriter::writeUInt32( uint32_t value ) {
	writeIntegralValue<uint32_t>( value );
}

void TextStreamWriter::writeUInt64( uint64_t value ) {
	writeIntegralValue<uint64_t>( value );
}

void TextStreamWriter::writeFloat( float value ) {
	writeFloatingPointValue<float>( value );
}

void TextStreamWriter::writeDouble( double value ) {
	writeFloatingPointValue<double>( value );
}

void TextStreamWriter::writeBool( bool value ) {
	size_t valueLen;
	const char *valueChars;
	if( value ) {
		valueLen = 4;
		valueChars = "true";
	} else {
		valueLen = 5;
		valueChars = "false";
	}
	const size_t separatorLen = ( hasPendingSeparator ? 1 : 0 );
	const size_t charsToWrite = separatorLen + valueLen;
	if( char *const bufferChars = m_stream->reserve( charsToWrite ) ) [[likely]] {
		bufferChars[0] = separatorChar;
		std::memcpy( bufferChars + separatorLen, valueChars, valueLen );
		m_stream->advance( charsToWrite );
		hasPendingSeparator = usePendingSeparators;
	}
}

void TextStreamWriter::writeChar( char value ) {
	const size_t separatorLen = ( hasPendingSeparator ? 1 : 0 );
	const size_t charsToWrite = separatorLen + 1;
	if( char *const bufferChars = m_stream->reserve( charsToWrite ) ) [[likely]] {
		bufferChars[0] = separatorChar;
		bufferChars[separatorLen] = value;
		m_stream->advance( charsToWrite );
		hasPendingSeparator = usePendingSeparators;
	}
}

void TextStreamWriter::writePtr( const void *value ) {
	const size_t separatorLen = ( hasPendingSeparator ? 1 : 0 );
	// The required length for 64-bit architectures is actually lesser
	// but this is too hard to handle in a robust fashion.
	const size_t addressCharsToReserve = 2 + ( ( sizeof( void * ) == 8 ) ? 16 : 8 );
	if( char *const bufferChars = m_stream->reserve( separatorLen + addressCharsToReserve ) ) [[likely]] {
		bufferChars[0] = separatorChar;
		// Let "%p" cut off unused address bits
		const int res = snprintf( bufferChars + separatorLen, addressCharsToReserve, "%p", value );
		assert( res > 0 );
		m_stream->advance( separatorLen + res );
		hasPendingSeparator = usePendingSeparators;
	}
}

void TextStreamWriter::writeChars( const char *chars, size_t numGivenChars ) {
	if( numGivenChars ) [[likely]] {
		const size_t separatorLen  = ( hasPendingSeparator ? 1 : 0 );
		const size_t charsToWrite  = separatorLen + numGivenChars;
		if( char *bufferChars = m_stream->reserve( charsToWrite ) ) [[likely]] {
			bufferChars[0] = separatorChar;
			std::memcpy( bufferChars + separatorLen, chars, numGivenChars );
			m_stream->advance( charsToWrite );
			hasPendingSeparator = usePendingSeparators;
		}
	}
}

void TextStreamWriter::writeQuotedChars( const char *chars, size_t numGivenChars ) {
	if( numGivenChars ) [[likely]] {
		const size_t separatorLen = ( hasPendingSeparator ? 1 : 0 );
		const size_t charsToWrite = separatorLen + numGivenChars + 2;
		if( char *bufferChars = m_stream->reserve( charsToWrite ) ) [[likely]] {
			bufferChars[0] = separatorChar;
			bufferChars[separatorLen] = quotesChar;
			std::memcpy( bufferChars + separatorLen + 1, chars, numGivenChars );
			bufferChars[separatorLen + 1 + numGivenChars] = quotesChar;
			m_stream->advance( charsToWrite );
			hasPendingSeparator = usePendingSeparators;
		}
	}
}

template <size_t N>
[[nodiscard]]
static auto writeConsoleColor( TextStreamWriter &writer, const char ( &color )[N] ) -> TextStreamWriter & {
	static_assert( N == 3 ); // Includes '\0'
	const bool hadSeparator    = writer.hasPendingSeparator;
	writer.hasPendingSeparator = false;
	writer.writeChars( color, 2 );
	writer.hasPendingSeparator = hadSeparator;
	return writer;
}

auto black( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_BLACK ); }
auto red( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_RED ); }
auto green( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_GREEN ); }
auto yellow( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_YELLOW ); }
auto blue( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_BLUE ); }
auto cyan( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_CYAN ); }
auto magenta( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_MAGENTA ); }
auto white( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_WHITE ); }
auto orange( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_ORANGE ); }
auto grey( TextStreamWriter &writer ) -> TextStreamWriter & { return writeConsoleColor( writer, S_COLOR_GREY ); }

}