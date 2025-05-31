#include "wordsmatcher.h"
#include "local.h"

#include <numeric>
#include <vector>

namespace wsw::ui {

class MatrixAdapter {
	unsigned *const m_data;
	const size_t m_rows;
	const size_t m_cols;
public:
	MatrixAdapter( unsigned *data, size_t rows, size_t cols ): m_data( data ), m_rows( rows ), m_cols( cols ) {}

	[[nodiscard]]
	auto operator()( size_t i, size_t j ) -> unsigned & {
		const size_t index = i * m_cols + j;
		assert( index < m_rows * m_cols );
		return m_data[index];
	}
};

auto WordsMatcher::distance( wsw::StringView a, wsw::StringView b, unsigned maxDist ) -> std::optional<Match> {
	unsigned prefixLen = 0;
	for( unsigned i = 0, len = (unsigned)wsw::min( a.length(), b.length() ); i < len; ++i ) {
		if( a[i] != b[i] ) {
			prefixLen = i;
			break;
		}
	}

	a = a.drop( prefixLen );
	b = b.drop( prefixLen );

	unsigned suffixLen = 0;
	for( unsigned i = 0, len = wsw::min( a.length(), b.length() ); i < len; ++i ) {
		if( a[a.length() - 1 - i] != b[b.length() - 1 - i] ) {
			suffixLen = i;
			break;
		}
	}

	a = a.dropRight( suffixLen );
	b = b.dropRight( suffixLen );

	const unsigned rows = a.length() + 1;
	const unsigned cols = b.length() + 1;
	m_distanceBuffer.resize( rows * cols );
	std::fill( m_distanceBuffer.begin(), m_distanceBuffer.end(), 0 );

	MatrixAdapter d( m_distanceBuffer.data(), rows, cols );
	for( size_t i = 0; i < rows; ++i ) {
		d( i, 0 ) = (unsigned)i;
	}
	for( size_t j = 0; j < cols; ++j ) {
		d( 0, j ) = (unsigned)j;
	}

	for( size_t i = 1; i < rows; ++i ) {
		for( size_t j = 1; j < cols; ++j ) {
			const unsigned matchCost     = ( a[i - 1] != b[j - 1] ) ? 1 : 0;
			const unsigned matchDist     = d( i - 1, j - 1 ) + matchCost;
			const unsigned deletionDist  = d( i - 1, j ) + 1;
			const unsigned insertionDist = d( i, j - 1 ) + 1;
			unsigned cellDist = wsw::min( matchDist, wsw::min( deletionDist, insertionDist ) );
			// Its better to turn -funswitch-loops on...
			if( i > 1 && j > 1 ) [[likely]] {
				// Check the transposition case
				if( a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] ) {
					cellDist = wsw::min( cellDist, d( i - 2, j - 2 ) + 1 );
				}
			}
			d( i, j ) = cellDist;
		}
	}

	// TODO: Add early exits
	if( const unsigned dist = d( a.length(), b.length() ); dist < maxDist ) {
		return Match { .editDistance = dist, .commonLength = prefixLen + suffixLen };
	}

	return std::nullopt;
}

auto WordsMatcher::prepareInput( const wsw::StringView &rawInput ) -> wsw::StringView {
	m_inputBuffer.clear();
	m_inputBuffer.reserve( rawInput.size() );
	for( char ch: rawInput ) {
		m_inputBuffer.push_back( (char)std::tolower( ch ) );
	}
	return wsw::StringView( m_inputBuffer.data(), m_inputBuffer.size() );
}

auto WordsMatcher::matchByDistance( const wsw::StringView &input, const wsw::StringView &word, unsigned maxDist )
	-> std::optional<Match> {
	if( input.length() <= word.size() ) {
		return distance( word, input, maxDist );
	}

	unsigned bestMismatch = maxDist;
	unsigned resultLen = 0;
	const unsigned numMatchingAttempts = input.length() - word.length();
	for( unsigned i = 0; i < numMatchingAttempts; ++i ) {
		const wsw::StringView inputWindow( input.takeMid( i, word.length() ) );
		// Make sure we handle last windows correctly
		assert( inputWindow.length() == word.length() );
		if( const auto maybeMatch = distance( word, inputWindow, bestMismatch ) ) {
			auto [dist, len] = *maybeMatch;
			bestMismatch = dist;
			resultLen = len;
			if( !bestMismatch ) {
				return Match { 0u, resultLen };
			}
		}
	}

	if( bestMismatch != maxDist ) {
		return Match { bestMismatch, resultLen };
	}

	return std::nullopt;
}

WordsMatcher::WordsMatcher( const wsw::StringView &word ) {
	if( word.empty() ) {
		wsw::failWithLogicError( "Should not be used for empty words" );
	}

	m_stringDataBuffer.reserve( word.size() );
	for( char ch: word ) {
		m_stringDataBuffer.push_back( (char)std::tolower( ch ) );
	}

	const char *begin = m_stringDataBuffer.data();
	const char *end = m_stringDataBuffer.data() + m_stringDataBuffer.size();
	new( m_exactMatcherHolder.unsafe_grow_back() )std::boyer_moore_searcher( begin, end );
}

auto WordsMatcher::match( const wsw::StringView &rawInput, unsigned maxDist ) -> std::optional<Match> {
	const wsw::StringView input( prepareInput( rawInput ) );
	if( input.empty() ) {
		return std::nullopt;
	}

	std::boyer_moore_searcher<const char *> &matcher = m_exactMatcherHolder.front();
	if( matcher( input.begin(), input.end() ).first != input.end() ) {
		return Match { 0u, (unsigned)m_stringDataBuffer.size() };
	}

	// Only if a fuzzy match is allowed
	if( maxDist ) {
		const wsw::StringView word( m_stringDataBuffer.data(), m_stringDataBuffer.size() );
		if( auto maybeMatch = matchByDistance( input, word, maxDist ) ) {
			return *maybeMatch;
		}
	}

	return std::nullopt;
}

}
