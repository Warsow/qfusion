#ifndef WSW_WSWSTATICSTRING_H
#define WSW_WSWSTATICSTRING_H

#include "stringview.h"
#include <cinttypes>

namespace wsw {

template <unsigned N>
class StaticString {
private:
	unsigned m_len { 0 };
	char m_data[N + 1];
public:
	using size_type = unsigned;
	using value_type = char;
	using reference = char &;
	using const_reference = const char &;
	using pointer = char *;
	using const_pointer = const char *;
	using difference_type = ptrdiff_t;
	using iterator = char *;
	using const_iterator = const char *;

	static constexpr unsigned npos = ~0u;

	StaticString() {
		m_data[0] = '\0';
	}

#ifndef _MSC_VER
	StaticString( const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
	StaticString( _Printf_format_string_ const char *format, ... );
#endif

	template <typename Container>
	StaticString( const Container &container ) {
		assert( container.size() <= N );
		::memcpy( m_data, container.data(), container.size() );
		m_len = container.size();
		m_data[m_len] = '\0';
	}

	void clear() {
		m_data[0] = '\0';
		m_len = 0;
	}

	[[nodiscard]]
	auto size() const -> size_type { return m_len; }
	[[nodiscard]]
	auto length() const -> size_type { return m_len; }

	[[nodiscard]]
	auto data() -> char * { return m_data; }
	[[nodiscard]]
	auto data() const -> const char * { return m_data; }

	[[nodiscard]]
	static constexpr auto capacity() -> size_type {
		static_assert( N > 0, "Illegal chars buffer size" );
		return N;
	}

	template <typename Container>
	[[nodiscard]]
	bool equals( const Container &that ) const {
		if( that.size() != m_len ) {
			return false;
		}
		// Create an intermediate variable immediately so the type
		// of the container data is restricted to char * by the SFINAE principle
		const char *const thatData = that.data();
		return !memcmp( m_data, thatData, m_len );
	}

	template <typename Container>
	[[nodiscard]]
	bool equalsIgnoreCase( const Container &that ) const {
		if( that.size() != m_len ) {
			return false;
		}
		const char *const thatData = that.data();
		return !Q_strnicmp( m_data, thatData, m_len );
	}

	template<unsigned M>
	[[nodiscard]]
	bool operator==( const StaticString<M> &that ) const {
		return equals( that );
	}

	template<unsigned M>
	[[nodiscard]]
	bool operator!=( const StaticString<M> &that ) const {
		return !equals( that );
	}

	[[nodiscard]]
	bool equals( const wsw::StringView &view ) const {
		if( view.size() != this->m_len ) {
			return false;
		}
		return !std::memcmp( this->m_data, view.data(), this->m_len );
	}

	void assign( const char *chars, size_t numChars ) {
		assert( numChars < N );
		std::memcpy( m_data, chars, numChars );
		m_data[numChars] = '\0';
		m_len = (unsigned)numChars;
	}

	void assign( const wsw::StringView &view ) {
		assign( view.data(), view.size() );
	}

#ifndef _MSC_VER
	[[nodiscard]]
	bool assignf( const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
	[[nodiscard]]
	bool assignf( _Printf_format_string_ const char *format, ... );
#endif

	[[nodiscard]]
	bool assignfv( const char *format, va_list va );

	[[nodiscard]]
	auto asView() const -> wsw::StringView {
		return wsw::StringView( m_data, m_len, wsw::StringView::ZeroTerminated );
	}

	[[nodiscard]]
	auto begin() -> char * { return m_data; }
	[[nodiscard]]
	auto end() -> char * { return m_data + m_len; }

	[[nodiscard]]
	auto begin() const -> const char * { return m_data; }
	[[nodiscard]]
	auto end() const -> const char * { return m_data + m_len; }
	[[nodiscard]]
	auto cbegin() const -> const char * { return m_data; }
	[[nodiscard]]
	auto cend() const -> const char * { return m_data + m_len; }

	[[nodiscard]]
	auto front() -> char & {
		assert( m_len );
		return m_data[0];
	}

	[[nodiscard]]
	auto front() const -> const char & {
		assert( m_len );
		return m_data[0];
	};

	[[nodiscard]]
	auto back() -> char & {
		assert( m_len );
		return m_data[m_len - 1];
	}

	[[nodiscard]]
	auto back() const -> const char & {
		assert( m_len );
		return m_data[m_len - 1];
	}

	[[nodiscard]]
	auto maybeFront() const -> std::optional<char> {
		return m_len ? std::optional( m_data[0] ) : std::nullopt;
	}

	[[nodiscard]]
	auto maybeBack() const -> std::optional<char> {
		return m_len ? std::optional( m_data[m_len] ) : std::nullopt;
	}

	[[nodiscard]]
	auto operator[]( size_type index ) -> char & {
		assert( index < m_len );
		return m_data[index];
	}

	[[nodiscard]]
	auto operator[]( size_type index ) const -> const char & {
		assert( index < m_len );
		return m_data[index];
	}

	[[nodiscard]]
	auto maybeAt( size_type index ) const -> std::optional<char> {
		return index < m_len ? std::optional( m_data[index] ) : std::nullopt;
	}

	[[nodiscard]]
	bool empty() const { return !m_len; }

	[[nodiscard]]
	bool full() const { return m_len == N; }

	[[nodiscard]]
	auto indexOf( char ch ) const -> std::optional<size_type> {
		return asView().indexOf( ch );
	}

	[[nodiscard]]
	auto indexOf( const wsw::CharLookup &lookup ) const -> std::optional<size_type> {
		return asView().indexOf( lookup );
	}

	[[nodiscard]]
	auto indexOf( const wsw::StringView &view ) const -> std::optional<size_type> {
		return asView().indexOf( view );
	}

	[[nodiscard]]
	auto lastIndexOf( char ch ) const -> std::optional<size_type> {
		return asView().lastIndexOf( ch );
	}

	[[nodiscard]]
	auto lastIndexOf( const wsw::CharLookup &lookup ) const -> std::optional<size_type> {
		return asView().lastIndexOf( lookup );
	}

	[[nodiscard]]
	auto lastIndexOf( const wsw::StringView &view ) const -> std::optional<size_type> {
		return asView().lastIndexOf( view );
	}

	[[nodiscard]]
	bool contains( char ch ) const {
		return asView().contains( ch );
	}

	[[nodiscard]]
	bool containsAny( const wsw::StringView &chars ) const {
		return asView().containsAny( chars );
	}

	[[nodiscard]]
	bool containsOnly( const wsw::StringView &chars ) const {
		return asView().containsOnly( chars );
	}

	[[nodiscard]]
	bool startsWith( char ch ) const {
		return asView().startsWith( ch );
	}

	[[nodiscard]]
	bool endsWith( char ch ) const {
		return asView().endsWith( ch );
	}

	[[nodiscard]]
	bool startsWith( const wsw::StringView &prefix ) const {
		return asView().startsWith( prefix );
	}

	[[nodiscard]]
	bool endsWith( const wsw::StringView &suffix ) const {
		return asView().endsWith( suffix );
	}

	void copyTo( char *buffer, size_t bufferSize ) const {
		asView().copyTo( buffer, bufferSize );
	}

	template <size_t M>
	void copyTo( char buffer[M] ) const {
		static_assert( M > N );
		copyTo( buffer, M );
	}

	[[maybe_unused]]
	auto append( char ch ) -> decltype( *this ) {
		assert( !full() );
		m_data[m_len + 0] = ch;
		m_data[m_len + 1] = '\0';
		m_len++;
		return *this;
	}

	template <typename Container>
	[[maybe_unused]]
	auto append( const Container &chars ) -> decltype( *this ) {
		return append( chars.data(), chars.size() );
	}

	[[maybe_unused]]
	auto append( const char *data, size_t dataSize ) -> decltype( *this ) {
		assert( m_len + dataSize <= N );
		std::memmove( m_data + m_len, data, dataSize );
		m_len += dataSize;
		m_data[m_len] = '\0';
		return *this;
	}
#ifndef _MSC_VER
	[[nodiscard]]
	bool appendf( const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
	[[nodiscard]]
	bool appendf( _Printf_format_string_ const char *format, ... );
#endif

	[[nodiscard]]
	bool appendfv( const char *format, va_list va );

	void push_back( char ch ) {
		(void)append( ch );
	}

	template <typename Container>
	void push_back( const Container &chars ) {
		(void)append( chars );
	}

	void pop_back() {
		assert( !empty() );
		m_data[--m_len] = '\0';
	}

	template <typename Container>
	auto insert( size_type index, const Container &chars ) -> decltype( *this ) {
		assert( m_len + chars.size() <= N );
		assert( index <= m_len );
		if( !chars.size() ) {
			return *this;
		}
		if( const auto tailSize = (size_t)( m_len - index ) ) {
			std::memmove( m_data + index + chars.size(), m_data + index, tailSize );
		}
		std::memmove( m_data + index, chars.data(), chars.size() );
		m_len += chars.size();
		m_data[m_len] = '\0';
		return *this;
	}

	template <typename Container>
	auto insert( iterator pos, const Container &chars ) -> decltype( *this ) {
		assert( pos >= begin() && pos <= end() );
		return insert( (size_type)( pos - begin() ), chars );
	}

	template <typename Container>
	auto insert( const_iterator pos, const Container &chars ) -> decltype( *this ) {
		assert( pos >= cbegin() && pos <= cend() );
		return insert( (size_type)( pos - cbegin() ), chars );
	}

#ifndef _MSC_VER
	[[nodiscard]]
	bool insertf( size_type index, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
	[[nodiscard]]
	bool insertf( size_type index, _Printf_format_string_ const char *format, ... );
#endif

	[[nodiscard]]
	bool insertfv( size_type index, const char *format, va_list va );

	[[maybe_unused]]
	auto erase( size_type index, size_type count = npos ) -> decltype( *this ) {
		assert( index <= m_len );
		if( count >= m_len - index ) {
			m_len = index;
			m_data[index] = '\0';
			return *this;
		}
		std::memmove( m_data + index, m_data + index + count, m_len - index - count );
		m_len -= count;
		m_data[m_len] = '\0';
		return *this;
	}

	[[maybe_unused]]
	auto erase( iterator it ) -> decltype( *this ) {
		assert( it >= begin() && it <= end() );
		return erase( (size_type)( it - begin() ), npos );
	}

	[[maybe_unused]]
	auto erase( const_iterator it ) -> decltype( *this ) {
		assert( it >= cbegin() && it <= cend() );
		return erase( (size_type)( it - cbegin() ), npos );
	}

	void resize( size_type newLength, char fillByChar ) {
		assert( newLength <= capacity() );
		if( newLength > m_len ) {
			std::memset( m_data + m_len, fillByChar, newLength - m_len );
			m_len = newLength;
			m_data[m_len] = '\0';
		} else if( newLength < m_len ) {
			erase( newLength );
		}
	}

	[[maybe_unused]]
	auto operator<<( bool value ) -> decltype( *this ) {
		return appendf( "%s", ( value ? "true" : "false" ) );
	}

	[[maybe_unused]]
	auto operator<<( char value ) -> decltype( *this ) {
		return append( value );
	}

	[[maybe_unused]]
	auto operator<<( int value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%d", value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( unsigned value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%u", value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( int64_t value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%" PRId64, value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( uint64_t value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%" PRIu64, value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( float value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%f", value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( double value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%lf", value );
		assert( res );
		return *this;
	}

	[[maybe_unused]]
	auto operator<<( long double value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%Lf", value );
		assert( res );
		return *this;
	}

	auto operator<<( const char * ) = delete;

	[[maybe_unused]]
	auto operator<<( const void *value ) -> decltype( *this ) {
		[[maybe_unused]]
		bool res = appendf( "%p", value );
		assert( res );
		return *this;
	}

	template <typename Container>
	[[maybe_unused]]
	auto operator<<( const Container &container ) -> decltype( *this ) {
		return append( container );
	}
};

template <unsigned N>
StaticString<N>::StaticString( const char *format, ... ) {
	va_list va;
	va_start( va, format );
	[[maybe_unused]]
	auto result = Q_vsnprintfz( m_data, N + 1, format, va );
	va_end( va );

	assert( (unsigned)result < N + 1 );
	m_len = (size_type)result;
}

template <unsigned N>
bool StaticString<N>::assignf( const char *format, ... ) {
	va_list va;
	va_start( va, format );
	bool res = assignfv( format, va );
	va_end( va );
	return res;
}

template <unsigned N>
bool StaticString<N>::assignfv( const char *format, va_list va ) {
	char buffer[N + 1];
	const auto res = Q_vsnprintfz( buffer, N + 1, format, va );
	if( (unsigned)res > N ) {
		return false;
	}

	std::memcpy( m_data, buffer, (unsigned)res + 1 );
	assert( m_data[res] == '\0' );
	m_len = res;
	return true;
}

template <unsigned N>
bool StaticString<N>::appendf( const char *format, ... ) {
	va_list va;
	va_start( va, format );
	bool res = appendfv( format, va );
	va_end( va );
	return res;
}

template <unsigned N>
bool StaticString<N>::appendfv( const char *format, va_list va ) {
	char buffer[N + 1];

	const auto res = Q_vsnprintfz( buffer, N + 1, format, va );
	if( (unsigned)res > N - m_len ) {
		return false;
	}

	std::memcpy( m_data + m_len, buffer, res + 1 );
	m_len += res;
	assert( m_data[m_len] == '\0' );
	return true;
}

template <unsigned N>
bool StaticString<N>::insertf( size_type index, const char *format, ... ) {
	va_list va;
	va_start( va, format );
	bool res = insertfv( index, format, va );
	va_end( va );
	return res;
}

template <unsigned N>
bool StaticString<N>::insertfv( size_type index, const char *format, va_list va ) {
	char buffer[N + 1];

	const auto res = Q_vsnprintfz( buffer, N, format, va );
	if( (unsigned)res > N - m_len ) {
		return false;
	}

	insert( index, wsw::StringView( buffer, (unsigned)res ) );
	return true;
}

}

#endif
