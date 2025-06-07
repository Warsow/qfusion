#ifndef WSW_5d66d1d6_f1e7_4f33_84ee_835f7b833e2a_H
#define WSW_5d66d1d6_f1e7_4f33_84ee_835f7b833e2a_H

#include <common/helpers/q_arch.h>
#include <common/helpers/wswbasicmath.h>

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <limits>

namespace wsw {

enum CaseSensitivity { IgnoreCase, MatchCase };

[[nodiscard]]
auto getHashAndLength( const char *s ) -> std::pair<uint32_t, size_t>;
[[nodiscard]]
auto getHashForLength( const char *s, size_t length ) -> uint32_t;

// TODO: Make it consteval-only when this functionality has a reliable compiler support
[[nodiscard]]
inline constexpr auto strlen( const char *s ) -> size_t {
	const char *p = s;
	while( *p ) {
		p++;
	}
	return (size_t)( p - s );
}

class StringView;

class CharLookup {
	bool m_data[std::numeric_limits<unsigned char>::max()];
public:
	explicit CharLookup( const wsw::StringView &chars ) noexcept;

	template <typename P>
	explicit CharLookup( const P &p ) noexcept;

	[[nodiscard]]
	bool operator()( char ch ) const { return m_data[(unsigned char)ch]; }
};

class StringView {
protected:
	const char *m_s;
	size_t m_len: 31;
	bool m_terminated: 1;

	static constexpr unsigned kMaxLen = 1u << 31u;

	[[nodiscard]]
	static constexpr auto checkLen( size_t len ) -> size_t {
		assert( len < kMaxLen );
		return len;
	}

	[[nodiscard]]
	auto toOffset( const char *p ) const -> unsigned {
		// These casts are mandatory for 32-bit uintptr_t's
		assert( (uint64_t)(uintptr_t)( p - m_s ) < (uint64_t)kMaxLen );
		return (unsigned)( p - m_s );
	}

	template <typename Predicate>
	[[nodiscard]]
	auto lengthOfLeftSatisfyingSpan( Predicate &&predicate ) const -> size_t {
		const size_t len = m_len;
		const char *p    = m_s;
		const char *s    = m_s;
		for( ; (size_t)( p - s ) < len; ++p ) {
			if( !predicate( *p ) ) [[unlikely]] {
				break;
			}
		}
		return (size_t)( p - s );
	}

	template <typename Predicate>
	[[nodiscard]]
	auto lengthOfRightSatisfyingSpan( Predicate &&predicate ) const -> size_t {
		const size_t len = m_len;
		const char *p    = m_s + len;
		const char *s    = m_s;
		for( ;; ) {
			--p;
			if( p < s ) [[unlikely]] {
				return len;
			}
			if( !predicate( *p ) ) [[unlikely]] {
				return (size_t)( len - ( p - s ) - 1 );
			}
		}
	}
public:
	enum Terminated {
		Unspecified,
		ZeroTerminated,
	};

	constexpr StringView() noexcept
		: m_s( "" ), m_len( 0 ), m_terminated( ZeroTerminated ) {}

	constexpr explicit StringView( const char *s ) noexcept
		: m_s( s ), m_len( checkLen( wsw::strlen( s ) ) ), m_terminated( ZeroTerminated ) {}

	constexpr StringView( const char *s, size_t len, Terminated terminated = Unspecified ) noexcept
		: m_s( s ), m_len( checkLen( len ) ), m_terminated( terminated ) {
		assert( !m_terminated || !m_s[len] );
	}

	[[nodiscard]]
	bool isZeroTerminated() const { return m_terminated; }

	[[nodiscard]]
	auto data() const -> const char * { return m_s; }
	[[nodiscard]]
	auto size() const -> size_t { return m_len; }
	[[nodiscard]]
	auto length() const -> size_t { return m_len; }

	[[nodiscard]]
	bool empty() const { return !m_len; }

	[[nodiscard]]
	bool equals( const wsw::StringView &that, CaseSensitivity caseSensitivity = MatchCase ) const {
		if( m_len == that.m_len ) {
			if( caseSensitivity == MatchCase ) {
				return !std::memcmp( m_s, that.m_s, m_len );
			}
			return !Q_strnicmp( m_s, that.m_s, m_len );
		}
		return false;
	}

	[[nodiscard]]
	bool equalsIgnoreCase( const wsw::StringView &that ) const {
		return m_len == that.m_len && !Q_strnicmp( m_s, that.m_s, m_len );
	}

	[[nodiscard]]
	bool operator==( const wsw::StringView &that ) const { return equals( that ); }
	[[nodiscard]]
	bool operator!=( const wsw::StringView &that ) const { return !equals( that ); }

	[[nodiscard]]
	auto begin() const -> const char * { return m_s; }
	[[nodiscard]]
	auto end() const -> const char * { return m_s + m_len; }
	[[nodiscard]]
	auto cbegin() const -> const char * { return m_s; }
	[[nodiscard]]
	auto cend() const -> const char * { return m_s + m_len; }

	[[nodiscard]]
	auto front() const -> const char & {
		assert( m_len );
		return m_s[0];
	}

	[[nodiscard]]
	auto back() const -> const char & {
		assert( m_len );
		return m_s[0];
	}

	[[nodiscard]]
	auto maybeFront() const -> std::optional<char> {
		return m_len ? std::optional( m_s[0] ) : std::nullopt;
	}

	[[nodiscard]]
	auto maybeBack() const -> std::optional<char> {
		return m_len ? std::optional( m_s[m_len - 1] ) : std::nullopt;
	}

	[[nodiscard]]
	auto operator[]( size_t index ) const -> const char & {
		assert( index < m_len );
		return m_s[index];
	}

	[[nodiscard]]
	auto maybeAt( size_t index ) const -> std::optional<char> {
		return index < m_len ? std::optional( m_s[index] ) : std::nullopt;
	}

	[[nodiscard]]
	auto indexOf( char ch ) const -> std::optional<unsigned> {
		if( const auto *p = (char *)::memchr( m_s, ch, m_len ) ) {
			return toOffset( p );
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto lastIndexOf( char ch ) const -> std::optional<unsigned>;
	[[nodiscard]]
	auto indexOf( const wsw::StringView &that ) const -> std::optional<unsigned>;
	[[nodiscard]]
	auto lastIndexOf( const wsw::StringView &that ) const -> std::optional<unsigned>;
	[[nodiscard]]
	auto indexOf( const wsw::CharLookup &lookup ) const -> std::optional<unsigned>;
	[[nodiscard]]
	auto lastIndexOf( const wsw::CharLookup &lookup ) const -> std::optional<unsigned>;

	[[nodiscard]]
	bool contains( char ch ) const {
		return indexOf( ch ).has_value();
	}

	[[nodiscard]]
	bool contains( const wsw::StringView &that ) const {
		return indexOf( that ).has_value();
	}

	[[nodiscard]]
	bool containsAny( const wsw::StringView &chars ) const {
		return containsAny( CharLookup( chars ) );
	}

	[[nodiscard]]
	bool containsAny( const wsw::CharLookup &chars ) const;

	[[nodiscard]]
	bool containsOnly( const wsw::StringView &chars ) const {
		return containsOnly( CharLookup( chars ) );
	}

	[[nodiscard]]
	bool containsOnly( const wsw::CharLookup &chars ) const;

	[[nodiscard]]
	bool containsAll( const wsw::StringView &chars ) const {
		return chars.containsOnly( *this );
	}

	[[nodiscard]]
	bool startsWith( char ch ) const {
		return m_len && m_s[0] == ch;
	}

	[[nodiscard]]
	bool endsWith( char ch ) const {
		return m_len && m_s[m_len - 1] == ch;
	}

	[[nodiscard]]
	bool startsWith( const wsw::StringView &prefix, wsw::CaseSensitivity caseSensitivity = MatchCase ) const {
		if( prefix.length() <= m_len ) {
			if( caseSensitivity == MatchCase ) {
				return !std::memcmp( m_s, prefix.m_s, prefix.length() );
			}
			return !Q_strnicmp( m_s, prefix.m_s, prefix.length() );
		}
		return false;
	}

	[[nodiscard]]
	bool endsWith( const wsw::StringView &suffix, wsw::CaseSensitivity caseSensitivity = MatchCase ) const {
		if( suffix.length() <= m_len ) {
			if( caseSensitivity == MatchCase ) {
				return !std::memcmp( m_s + m_len - suffix.length(), suffix.m_s, suffix.length() );
			}
			return !::Q_strnicmp( m_s + m_len - suffix.length(), suffix.m_s, suffix.length() );
		}
		return false;
	}

	[[nodiscard]]
	auto trimLeft() const -> wsw::StringView;

	[[nodiscard]]
	auto trimLeft( char ch ) const -> wsw::StringView;

	[[nodiscard]]
	auto trimLeft( const wsw::StringView &chars ) const -> wsw::StringView;

	[[nodiscard]]
	auto trimRight() const -> wsw::StringView;

	[[nodiscard]]
	auto trimRight( char ch ) const -> wsw::StringView;

	[[nodiscard]]
	auto trimRight( const wsw::StringView &chars ) const -> wsw::StringView;

	[[nodiscard]]
	auto trim() const -> wsw::StringView {
		return trimLeft().trimRight();
	}

	[[nodiscard]]
	auto trim( char ch ) const -> wsw::StringView {
		return trimLeft( ch ).trimRight( ch );
	}

	[[nodiscard]]
	auto trim( const wsw::StringView &chars ) const -> wsw::StringView;

	[[nodiscard]]
	auto getCommonPrefixLength( const wsw::StringView &that, wsw::CaseSensitivity caseSensitivity = wsw::MatchCase ) const -> size_t;

	[[nodiscard]]
	auto take( size_t n ) const -> wsw::StringView {
		Terminated terminated = m_terminated && n >= m_len ? ZeroTerminated : Unspecified;
		return wsw::StringView( m_s, wsw::min( m_len, n ), terminated );
	}

	[[nodiscard]]
	auto takeExact( size_t n ) const -> std::optional<wsw::StringView> {
		if( n <= m_len ) {
			Terminated terminated = m_terminated && n == m_len ? ZeroTerminated : Unspecified;
			return wsw::StringView( m_s, n, terminated );
		}
		return std::nullopt;
	}

	template <typename Predicate>
	[[nodiscard]]
	auto takeWhile( Predicate &&predicate ) const -> wsw::StringView {
		const size_t len = lengthOfLeftSatisfyingSpan( std::forward<Predicate>( predicate ) );
		Terminated terminated = ( m_terminated && len == m_len ) ? ZeroTerminated : Unspecified;
		return wsw::StringView( m_s, len, terminated );
	}

	[[nodiscard]]
	auto drop( size_t n ) const -> wsw::StringView {
		size_t prefixLen = wsw::min( n, m_len );
		return wsw::StringView( m_s + prefixLen, m_len - prefixLen, (Terminated)m_terminated );
	}

	[[nodiscard]]
	auto dropExact( size_t n ) const -> std::optional<wsw::StringView> {
		if( n <= m_len ) {
			return wsw::StringView( m_s + n, m_len - n, (Terminated)m_terminated );
		}
		return std::nullopt;
	}

	template <typename Predicate>
	[[nodiscard]]
	auto dropWhile( Predicate &&predicate ) const -> wsw::StringView {
		const size_t len = lengthOfLeftSatisfyingSpan( std::forward<Predicate>( predicate ) );
		return wsw::StringView( m_s + len, m_len - len, (Terminated)m_terminated );
	}

	[[nodiscard]]
	auto takeRight( size_t n ) const -> wsw::StringView {
		size_t suffixLen = wsw::min( n, m_len );
		return wsw::StringView( m_s + m_len - suffixLen, suffixLen, (Terminated)m_terminated );
	}

	[[nodiscard]]
	auto takeRightExact( size_t n ) const -> std::optional<wsw::StringView> {
		if( n <= m_len ) {
			return wsw::StringView( m_s + m_len - n, n, (Terminated)m_terminated );
		}
		return std::nullopt;
	}

	template <typename Predicate>
	[[nodiscard]]
	auto takeRightWhile( Predicate &&predicate ) const -> wsw::StringView {
		const size_t len = lengthOfRightSatisfyingSpan( std::forward<Predicate>( predicate ) );
		return wsw::StringView( m_s + m_len - len, len, (Terminated)m_terminated );
	}

	[[nodiscard]]
	auto dropRight( size_t n ) const -> wsw::StringView {
		Terminated terminated = ( m_terminated && n == 0 ) ? ZeroTerminated : Unspecified;
		return wsw::StringView( m_s, m_len - wsw::min( n, m_len ), terminated );
	}

	[[nodiscard]]
	auto dropRightExact( size_t n ) const -> std::optional<wsw::StringView> {
		if( n <= m_len ) {
			Terminated terminated = m_terminated && n == 0 ? ZeroTerminated : Unspecified;
			return wsw::StringView( m_s, m_len - n, terminated );
		}
		return std::nullopt;
	}

	template <typename Predicate>
	[[nodiscard]]
	auto dropRightWhile( Predicate predicate ) const -> wsw::StringView {
		const size_t len      = lengthOfRightSatisfyingSpan( std::forward<Predicate>( predicate ) );
		Terminated terminated = ( m_terminated && len == 0 ) ? ZeroTerminated : Unspecified;
		return wsw::StringView( m_s, m_len - len, terminated );
	}

	[[nodiscard]]
	auto takeMid( size_t index, size_t n ) const -> wsw::StringView {
		assert( index < m_len );
		Terminated terminated = m_terminated && index + n >= m_len ? ZeroTerminated : Unspecified;
		return wsw::StringView( m_s + index, wsw::min( n, m_len - index ), terminated );
	}

	[[nodiscard]]
	auto takeMidExact( size_t index, size_t n ) const -> std::optional<wsw::StringView> {
		assert( index < m_len );
		if( index + n <= m_len ) {
			Terminated terminated = m_terminated && index + n == m_len ? ZeroTerminated : Unspecified;
			return wsw::StringView( m_s + index, n, terminated );
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto dropMid( size_t index, size_t n ) const -> std::pair<wsw::StringView, wsw::StringView> {
		assert( index < m_len );
		wsw::StringView left( m_s, index );
		const auto off2 = wsw::min( index + n, m_len );
		const auto len2 = m_len - off2;
		wsw::StringView right( m_s + off2, len2, m_terminated ? ZeroTerminated : Unspecified );
		assert( left.length() + wsw::min( n, m_len - index ) + right.length() == m_len );
		return std::make_pair( left, right );
	}

	[[nodiscard]]
	auto dropMidExact( size_t index, size_t n ) const -> std::optional<std::pair<wsw::StringView, wsw::StringView>> {
		assert( index < m_len );
		if( index + n > m_len ) {
			return std::nullopt;
		}
		wsw::StringView left( m_s, index );
		const auto off2 = index + n;
		const auto len2 = m_len - off2;
		wsw::StringView right( m_s + off2, len2, m_terminated ? ZeroTerminated : Unspecified );
		assert( left.length() + n + right.length() == m_len );
		return std::make_pair( left, right );
	}

	void copyTo( char *buffer, size_t bufferSize ) const {
		assert( bufferSize > m_len );
		::memcpy( buffer, m_s, m_len );
		buffer[m_len] = '\0';
	}

	template <size_t N>
	void copyTo( char buffer[N] ) const {
		copyTo( buffer, N );
	}
};

template <typename P>
inline CharLookup::CharLookup( const P &p ) noexcept {
	::memset( m_data, 0, sizeof( m_data ) );
	for( size_t i = 0; i < sizeof( m_data ) / sizeof( char ); ++i ) {
		if( p.operator()( (char)i ) ) {
			m_data[i] = true;
		}
	}
}

/**
 * An extension of {@code StringView} that stores a value of a case-insensitive hash code in addition.
 */
class HashedStringView : public StringView {
protected:
	uint32_t m_hash;
public:
	constexpr HashedStringView() : StringView(), m_hash( 0 ) {}

	explicit HashedStringView( const char *s ) : StringView( s ) {
		m_hash = getHashForLength( s, m_len );
	}

	HashedStringView( const char *s, size_t len, Terminated terminated = Unspecified )
		: StringView( s, len, terminated ) {
		m_hash = getHashForLength( s, len );
	}

	HashedStringView( const char *s, size_t len, uint32_t hash, Terminated terminated = Unspecified )
		: StringView( s, len, terminated ), m_hash( hash ) {}

	explicit HashedStringView( const wsw::StringView &that )
		: StringView( that.data(), that.size(), that.isZeroTerminated() ? ZeroTerminated : Unspecified ) {
		m_hash = getHashForLength( m_s, m_len );
	}

	[[nodiscard]]
	auto getHash() const -> uint32_t { return m_hash; }

	[[nodiscard]]
	bool equals( const wsw::HashedStringView &that ) const {
		return m_hash == that.m_hash && m_len == that.m_len && !::strncmp( m_s, that.m_s, m_len );
	}

	[[nodiscard]]
	bool equalsIgnoreCase( const wsw::HashedStringView &that ) const {
		return m_hash == that.m_hash && m_len == that.m_len && !Q_strnicmp( m_s, that.m_s, m_len );
	}

	[[nodiscard]]
	bool operator==( const wsw::HashedStringView &that ) const { return equals( that ); }
	[[nodiscard]]
	bool operator!=( const wsw::HashedStringView &that ) const { return !equals( that ); }
};

[[nodiscard]]
inline constexpr auto operator "" _asView( const char *s, std::size_t len ) -> wsw::StringView {
	return len ? wsw::StringView( s, len, wsw::StringView::ZeroTerminated ) : wsw::StringView();
}

[[nodiscard]]
inline auto operator "" _asHView( const char *s, std::size_t len ) -> wsw::HashedStringView {
	return len ? wsw::HashedStringView( s, len, wsw::StringView::ZeroTerminated ) : wsw::HashedStringView();
}

}

#endif
