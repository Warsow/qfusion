#include "stringview.h"

#include <span>
#include <cstring>
#include <algorithm>
#include <cctype>

namespace wsw {

CharLookup::CharLookup( const wsw::StringView &chars ) noexcept {
	std::memset( m_data, 0, sizeof( m_data ) );
	for( char ch: chars ) {
		m_data[(unsigned char)ch] = true;
	}
}

auto StringView::lastIndexOf( char ch ) const -> std::optional<unsigned> {
	const auto start = std::make_reverse_iterator( m_s + m_len ), end = std::make_reverse_iterator( m_s );
	if( const auto it = std::find( start, end, ch ); it != end ) {
		return toOffset( it.base() ) - 1u;
	}
	return std::nullopt;
}

auto StringView::indexOf( const wsw::StringView &that ) const -> std::optional<unsigned> {
	if( const char *p = std::search( m_s, m_s + m_len, that.m_s, that.m_s + that.m_len ); p != m_s + m_len ) {
		return toOffset( p );
	}
	return std::nullopt;
}

auto StringView::lastIndexOf( const wsw::StringView &that ) const -> std::optional<unsigned> {
	const auto start     = std::make_reverse_iterator( m_s + m_len );
	const auto end       = std::make_reverse_iterator( m_s );
	const auto thatStart = std::make_reverse_iterator( that.m_s + that.m_len );
	const auto thatEnd   = std::make_reverse_iterator( that.m_s );
	if( const auto it = std::search( start, end, thatStart, thatEnd ); it != end ) {
		return toOffset( it.base() ) - that.m_len;
	}
	return std::nullopt;
}

auto StringView::indexOf( const wsw::CharLookup &lookup ) const -> std::optional<unsigned> {
	for( unsigned i = 0; i < m_len; ++i ) {
		if( lookup( m_s[i] ) ) {
			return i;
		}
	}
	return std::nullopt;
}

auto StringView::lastIndexOf( const wsw::CharLookup &lookup ) const -> std::optional<unsigned> {
	auto iLast = m_len;
	for( unsigned i = m_len; i <= iLast; iLast = i, i-- ) {
		if( lookup( m_s[i] ) ) {
			return i;
		}
	}
	return std::nullopt;
}

bool StringView::containsAny( const wsw::CharLookup &chars ) const {
	return std::find_if( m_s, m_s + m_len, chars ) != m_s + m_len;
}

bool StringView::containsOnly( const wsw::CharLookup &chars ) const {
	return std::find_if_not( m_s, m_s + m_len, chars ) == m_s + m_len;
}

auto StringView::trimLeft() const -> wsw::StringView {
	const char *p = std::find_if( m_s, m_s + m_len, []( char arg ) { return !std::isspace( arg ); } );
	return wsw::StringView( p, m_len - ( p - m_s ), (Terminated)m_terminated );
}

auto StringView::trimLeft( char ch ) const -> wsw::StringView {
	const char *p = std::find_if( m_s, m_s + m_len, [=]( char arg ) { return arg != ch; });
	return wsw::StringView( p, m_len - ( p - m_s ), (Terminated)m_terminated );
}

auto StringView::trimLeft( const wsw::StringView &chars ) const -> wsw::StringView {
	CharLookup lookup( chars );
	const char *p = std::find_if_not( m_s, m_s + m_len, lookup );
	return wsw::StringView( p, m_len - ( p - m_s ), (Terminated)m_terminated );
}

auto StringView::trimRight() const -> wsw::StringView {
	auto start = std::make_reverse_iterator( m_s + m_len ), end = std::make_reverse_iterator( m_s );
	auto it = std::find_if( start, end, []( char arg ) { return !std::isspace( arg ); } );
	const char *p = it.base();
	Terminated terminated = ( m_terminated && p == m_s + m_len ) ? ZeroTerminated : Unspecified;
	return wsw::StringView( m_s, p - m_s, terminated );
}

auto StringView::trimRight( char ch ) const -> wsw::StringView {
	auto start = std::make_reverse_iterator( m_s + m_len ), end = std::make_reverse_iterator( m_s );
	auto it = std::find_if( start, end, [=]( char arg ) { return arg != ch; });
	const char *p = it.base();
	Terminated terminated = ( m_terminated && p == m_s + m_len ) ? ZeroTerminated : Unspecified;
	return wsw::StringView( m_s, p - m_s, terminated );
}

auto StringView::trimRight( const wsw::StringView &chars ) const -> wsw::StringView {
	CharLookup lookup( chars );
	auto begin = std::make_reverse_iterator( m_s + m_len ), end = std::make_reverse_iterator( m_s );
	auto it = std::find_if_not( begin, end, lookup );
	Terminated terminated = ( m_terminated && it == begin ) ? ZeroTerminated : Unspecified;
	return wsw::StringView( m_s, it.base() - m_s, terminated );
}

auto StringView::trim( const wsw::StringView &chars ) const -> wsw::StringView {
	CharLookup lookup( chars );
	const char *left = std::find_if_not( m_s, m_s + m_len, lookup );
	if( left == m_s + m_len ) {
		return wsw::StringView();
	}

	auto begin = std::make_reverse_iterator( m_s + m_len ), end = std::make_reverse_iterator( m_s );
	auto it = std::find_if_not( begin, end, lookup );
	const char *right = it.base();
	Terminated terminated = ( m_terminated && right == m_s + m_len ) ? ZeroTerminated : Unspecified;
	return wsw::StringView( left, right - left, terminated );
}

auto StringView::getCommonPrefixLength( const wsw::StringView &that, wsw::CaseSensitivity caseSensitivity ) const -> size_t {
	const size_t limit = wsw::min( length(), that.length() );
	if( caseSensitivity == MatchCase ) {
		// TODO: Optimize if needed
		for( size_t i = 0; i < limit; ++i ) {
			if( m_s[i] != that.m_s[i] ) {
				return i;
			}
		}
	} else {
		for( size_t i = 0; i < limit; ++i ) {
			if( std::toupper( m_s[i] ) != std::toupper( that.m_s[i] ) ) {
				return i;
			}
		}
	}
	return limit;
}

}