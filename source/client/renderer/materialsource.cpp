#include "materiallocal.h"
#include <common/helpers/algorithm.h>

auto MaterialSource::preparePlaceholders() -> std::optional<Placeholders> {
	wsw::PodVector<PlaceholderSpan> buffer;

	auto [spans, numTokens] = getTokenSpans();
	const char *data = getCharData();

	for( unsigned i = 0; i < numTokens; ++i ) {
		const auto &tokenSpan = spans[i];
		findPlaceholdersInToken( wsw::StringView( data + tokenSpan.offset, tokenSpan.len ), i, buffer );
	}

	return buffer.empty() ? std::nullopt : std::optional( buffer );
}

void MaterialSource::findPlaceholdersInToken( const wsw::StringView &token, unsigned tokenNum,
											  wsw::PodVector<PlaceholderSpan> &buffer ) {
	size_t index = 0;

	for(;; ) {
		auto *p = wsw::find( token.data() + index, token.data() + token.size(), '$' );
		if( p == token.data() + token.size() ) {
			return;
		}
		index = p - token.data() + 1;
		auto start = index - 1;
		int num = 0;
		bool overflow = false;
		for(; index < token.length() && isdigit( token[index] ); index++ ) {
			if( !overflow ) {
				num = num * 10 + ( token[index] - '0' );
				if( num > std::numeric_limits<uint8_t>::max() ) {
					overflow = true;
				}
			}
		}
		// If it was just a single dollar character
		if( !num ) {
			continue;
		}
		if( overflow ) {
			// TODO: Show a warning
		}
		auto len = index - start;
		buffer.emplace_back( PlaceholderSpan { (uint32_t)tokenNum, (uint16_t)start, (uint8_t)len, (uint8_t)num } );
	}
}

bool MaterialSource::expandTemplate( const wsw::StringView *args, size_t numArgs, wsw::PodVector<char> &expansionBuffer, wsw::PodVector<TokenSpan> &resultingTokens ) {
	if( !m_triedPreparingPlaceholders ) {
		m_placeholders = preparePlaceholders();
		m_triedPreparingPlaceholders = true;
	}

	if( !m_placeholders ) {
		return false;
	}

	ExpansionState state { expansionBuffer, resultingTokens };
	const ExpansionParams params { args, numArgs, *m_placeholders };
	return expandTemplate( params, state );
}

[[nodiscard]]
auto MaterialSource::validateAndEstimateExpandedDataSize( const ExpansionParams &params ) const -> std::optional<unsigned> {
	const auto [args, numArgs, placeholders] = params;
	const auto [spans, numTokens] = getTokenSpans();

	unsigned lastTokenNum = ~0u;
	unsigned result = 0;
	for( const auto &p: placeholders ) {
		static_assert( std::is_unsigned<decltype( p.argNum )>::value );
		if( ( p.argNum - 1u ) >= numArgs ) {
			return std::nullopt;
		}
		result -= p.len;
		result += args[p.argNum - 1].length();
		if( p.tokenNum == lastTokenNum ) {
			continue;
		}
		assert( p.tokenNum < numTokens );
		result += spans[p.tokenNum].len;
		lastTokenNum = p.tokenNum;
	}

	// No placeholders met
	if( !result ) {
		return std::nullopt;
	}

	// Add a space for an extra character at the beginning
	return result + 1;
}

void MaterialSource::addTheRest( ExpansionState &state, size_t lastSpanNum, size_t currSpanNum ) const {
	const auto [spans, numSpans] = getTokenSpans();
	assert( lastSpanNum < numSpans );
	const auto &lastSpan = spans[lastSpanNum];

	const char *copySrc = getCharData() + lastSpan.offset + state.lastOffsetInSpan;
	assert( state.lastOffsetInSpan <= lastSpan.len );
	size_t copyLen = lastSpan.len - state.lastOffsetInSpan;
	state.expansionBuffer.append( copySrc, copyLen );

	assert( state.tokenStart );
	auto tokenLen = (uint32_t)( state.expansionBuffer.size() - state.tokenStart );
	auto tokenOffset = -(int32_t)state.tokenStart;
	state.resultingTokens.push_back( { tokenOffset, tokenLen, lastSpan.line } );

	for( size_t i = lastSpanNum + 1; i < currSpanNum; ++i ) {
		state.resultingTokens.push_back( spans[i] );
	}

	state.lastOffsetInSpan = 0;
	state.tokenStart = state.expansionBuffer.size();
}

bool MaterialSource::expandTemplate( const ExpansionParams &params, ExpansionState &state ) const {
	auto maybeSize = validateAndEstimateExpandedDataSize( params );
	if( !maybeSize ) {
		return false;
	}

	state.expansionBuffer.reserve( *maybeSize );
	state.expansionBuffer.push_back( ' ' );

	const char *const data = getCharData();
	const auto [spans, numSpans] = getTokenSpans();
	const auto [args, numArgs, placeholders] = params;

	size_t lastTokenNum = 0;
	// for each token that needs a substitution
	for( const auto &p: placeholders ) {
		if( lastTokenNum != p.tokenNum ) {
			addTheRest( state, lastTokenNum, p.tokenNum );
		}

		const auto &currSpan = spans[p.tokenNum];
		lastTokenNum = p.tokenNum;

		// Copy characters after offsetInLastToken and before placeholder offset
		assert( p.offset >= state.lastOffsetInSpan );
		size_t midLen = p.offset - state.lastOffsetInSpan;
		state.expansionBuffer.append( data + currSpan.offset + state.lastOffsetInSpan, midLen );
		state.lastOffsetInSpan += midLen + p.len;

		// Copy argument data
		static_assert( std::is_unsigned<decltype( p.argNum )>::value );
		assert( ( p.argNum - 1u ) < numArgs );
		const wsw::StringView &arg = args[p.argNum - 1];
		state.expansionBuffer.append( arg.data(), arg.size() );
	}

	addTheRest( state, lastTokenNum, numSpans );
	return true;
}