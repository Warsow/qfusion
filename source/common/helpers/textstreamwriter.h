#ifndef WSW_9595d89d_d3d4_4235_b7a4_608538f78cc9_H
#define WSW_9595d89d_d3d4_4235_b7a4_608538f78cc9_H

#include <common/helpers/wswbasicarch.h>
#include <cstdint>
#include <cstdlib>
#include <type_traits>
#include <concepts>

namespace wsw {

class OutputMessageStream;

class TextStreamWriter {
protected:
	OutputMessageStream *const m_stream;

	// TODO: It's unclear what to do in case of errors.
	// Throwing an exception is definitely not an appropriate choice.
	// We can just set some hadError flag so callers that care can handle that.

	template <typename T>
	wsw_forceinline void writeFloatingPointValue( T value );
	template <typename T>
	wsw_forceinline void writeIntegralValue( T value );

public:
	// For now we use the exact type.
	// A supertype/interface should be introduced if there's a need in generalization.
	explicit TextStreamWriter( OutputMessageStream *stream ) : m_stream( stream ) {}

	TextStreamWriter( const TextStreamWriter & ) = delete;
	TextStreamWriter( TextStreamWriter && ) = delete;
	auto operator=( const TextStreamWriter & ) = delete;
	auto operator=( TextStreamWriter && ) = delete;

	// Note: these methods are made public to avoid hassle with declaring global operators as friends.

	void writeInt8( int8_t value );
	void writeInt16( int16_t value );
	void writeInt32( int32_t value );
	void writeInt64( int64_t value );
	void writeUInt8( uint8_t value );
	void writeUInt16( uint16_t value );
	void writeUInt32( uint32_t value );
	void writeUInt64( uint64_t value );
	void writeFloat( float value );
	void writeDouble( double value );

	void writeBool( bool value );
	void writeChar( char value );
	void writePtr( const void *value );
	void writeChars( const char *chars, size_t numGivenChars );
	void writeQuotedChars( const char *chars, size_t numGivenChars );

	char separatorChar { ' ' };
	char quotesChar { '\'' };
	bool hasPendingSeparator { false };
	bool usePendingSeparators { true };
};

// https://stackoverflow.com/a/74922953
struct StringLiteral {
private:
	[[nodiscard]]
	static consteval auto trimTrailingZeros( const char *s, size_t n ) -> size_t {
		while( n && s[n - 1] == '\0' ) {
			n--;
		}
		return n;
	}
public:
	template<class T, std::size_t N, std::enable_if_t<std::is_same_v<T, const char>>...>
	consteval StringLiteral( T ( &chars )[N] ) : data( chars ), length( trimTrailingZeros( chars, N ) ) {}

	const char *const data;
	const size_t length;
};

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, char value ) -> TextStreamWriter & {
	writer.writeChar( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, int8_t value ) -> TextStreamWriter & {
	writer.writeInt8( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, int16_t value ) -> TextStreamWriter & {
	writer.writeInt16( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, int32_t value ) -> TextStreamWriter & {
	writer.writeInt32( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, int64_t value ) -> TextStreamWriter & {
	writer.writeInt64( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, uint8_t value ) -> TextStreamWriter & {
	writer.writeUInt8( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, uint16_t value ) -> TextStreamWriter & {
	writer.writeUInt16( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, uint32_t value ) -> TextStreamWriter & {
	writer.writeUInt32( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, uint64_t value ) -> TextStreamWriter & {
	writer.writeUInt64( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, float value ) -> TextStreamWriter & {
	writer.writeFloat( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, double value ) -> TextStreamWriter & {
	writer.writeDouble( value ); return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, const StringLiteral &literal ) -> TextStreamWriter & {
	if( literal.length ) [[likely]] {
		writer.writeChars( literal.data, literal.length );
	}
	return writer;
}

[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, TextStreamWriter &(*fn)( TextStreamWriter & ) ) -> TextStreamWriter & {
	fn( writer );
	return writer;
}

template <typename Chars>
	requires
		requires( const Chars &ch ) {
		{ ch.data() } -> std::same_as<const char *>;
		{ ch.size() } -> std::integral;
	}
[[maybe_unused]]
wsw_forceinline auto operator<<( TextStreamWriter &writer, const Chars &chars ) -> TextStreamWriter & {
	writer.writeQuotedChars( chars.data(), chars.size() );
	return writer;
}

}

#endif
