#include "gametypesmodel.h"

#include <common/wswfs.h>
#include <common/wswstringsplitter.h>
#include <common/wswtonum.h>

using wsw::operator""_asView;

namespace wsw::ui {

auto GametypeDefParser::readNextLine() -> std::optional<std::pair<wsw::StringView, unsigned>> {
	assert( m_reader );

	unsigned flags = None;
	for(;; ) {
		if( m_reader->isAtEof() ) {
			return std::make_pair( wsw::StringView(), flags | Eof );
		}

		auto res = m_reader->readToNewline( m_lineBuffer, kBufferSize );
		if( !res ) {
			return std::nullopt;
		}

		assert( !res->wasIncomplete && "Make sure the buffer can hold any line" );
		const auto bytesRead = res->bytesRead;
		if( !bytesRead ) {
			flags |= HadEmptyLines;
			continue;
		}

		assert( bytesRead < sizeof( m_lineBuffer ) );
		m_lineBuffer[bytesRead] = '\0';

		wsw::StringView view( m_lineBuffer, bytesRead, wsw::StringView::ZeroTerminated );
		return std::make_pair( view, flags );
	}
}

bool GametypeDefParser::expectSection( const wsw::StringView &heading ) {
	assert( !heading.empty() );

	auto res = readNextLine();
	if( !res ) {
		return false;
	}

	auto [view, flags] = *res;
	if( flags & Eof ) {
		return false;
	}

	if( !view.startsWith( '[' ) || !view.endsWith( ']' ) ) {
		return false;
	}

	return view.drop( 1 ).dropRight( 1 ).equalsIgnoreCase( heading );
}

bool GametypeDefParser::parseTitle() {
	if( !expectSection( "Title"_asView ) ) {
		return false;
	}
	const auto res = readNextLine();
	if( !res || ( res->second & Eof ) ) {
		return false;
	}
	const wsw::StringView view( res->first );
	if( view.size() > 32 ) {
		return false;
	}

	m_gametypeDef.m_titleSpanIndex = m_gametypeDef.m_stringDataStorage.add( view );
	return true;
}

static const wsw::StringView kNone( "None"_asView );
static const wsw::StringView kBased( "Based"_asView );
static const wsw::StringView kTeam( "Team"_asView );
static const wsw::StringView kRound( "Round"_asView );
static const wsw::StringView kRace( "Race"_asView );

bool GametypeDefParser::parseFlags() {
	if( !expectSection( "Flags"_asView ) ) {
		return false;
	}
	const auto res = readNextLine();
	if( !res || ( res->second & Eof ) ) {
		return false;
	}

	assert( m_gametypeDef.m_flags == None );

	const wsw::StringView view( res->first );
	wsw::StringSplitter splitter( view );
	while( const auto maybeToken = splitter.getNext() ) {
		const auto rawToken = *maybeToken;
		if( rawToken.equalsIgnoreCase( kNone ) ) {
			continue;
		}

		if( rawToken.equalsIgnoreCase( kRace ) ) {
			m_gametypeDef.m_flags |= GametypeDef::Race;
			continue;
		}

		// Allow the "-Based" suffix

		auto token = rawToken;
		if( token.takeRight( kBased.size() ).equalsIgnoreCase( kBased ) ) {
			token = token.dropRight( kBased.size() );
		}
		if( token.equalsIgnoreCase( kTeam ) ) {
			m_gametypeDef.m_flags |= GametypeDef::Team;
		} else if( token.equalsIgnoreCase( kRound ) ) {
			m_gametypeDef.m_flags |= GametypeDef::Round;
		} else {
			return false;
		}
	}
	return true;
}

static const std::pair<wsw::StringView, GametypeDef::BotConfig> kBotConfigTokens[] = {
	{ "None"_asView, GametypeDef::NoBots },
	{ "BestForMap"_asView, GametypeDef::BestNumBotsForMap },
	{ "FixedForMap"_asView, GametypeDef::FixedNumBotsForMap },
	{ "Script"_asView, GametypeDef::ScriptSpawnedBots }
};

bool GametypeDefParser::parseBotConfig() {
	if( !expectSection( "BotConfig"_asView ) ) {
		return false;
	}
	const auto res = readNextLine();
	if( !res || ( res->second & Eof ) ) {
		return false;
	}

	const auto view( res->first );
	if( const auto maybeNum = wsw::toNum<unsigned>( view ) ) {
		if( *maybeNum > 16 ) {
			return false;
		}
		m_gametypeDef.m_botConfig = GametypeDef::ExactNumBots;
		m_gametypeDef.m_exactNumBots = maybeNum;
		return true;
	}

	for( const auto &[token, value]: kBotConfigTokens ) {
		if( view.equalsIgnoreCase( token ) ) {
			m_gametypeDef.m_botConfig = value;
			return true;
		}
	}

	return false;
}

bool GametypeDefParser::parseMaps() {
	if( !expectSection( "Maps"_asView ) ) {
		return false;
	}

	const auto res = readNextLine();
	if( !res || ( res->second & Eof ) ) {
		return false;
	}

	wsw::StringSplitter splitter( res->first );
	while( const auto maybeToken = splitter.getNext() ) {
		const auto token = maybeToken->trim();
		const auto semicolonIndex = token.indexOf( ':' );
		if( semicolonIndex == std::nullopt ) {
			const auto botConfig = m_gametypeDef.m_botConfig;
			// The number of bots must be specified in this case
			if( botConfig == GametypeDef::FixedNumBotsForMap || botConfig == GametypeDef::BestNumBotsForMap ) {
				return false;
			}
			m_gametypeDef.addMap( token );
			continue;
		}

		if( *semicolonIndex == 0 ) {
			return false;
		}

		const auto rangePart = token.drop( *semicolonIndex + 1 );
		if( rangePart.empty() ) {
			return false;
		}

		// Caution! The (total) number of players is also displayed as a hint in the local game wizard.
		// Assume that we should keep at least a single player slot for the listen server owner.
		// Given the requirement that we should be able to actually start a spawned local game,
		// 2 or more players is the minimum for these parsed numeric values.

		const auto dashIndex = rangePart.indexOf( '-' );
		if( dashIndex == std::nullopt ) {
			auto maybeNum = wsw::toNum<unsigned>( rangePart );
			if( !maybeNum || *maybeNum < 2 || *maybeNum > 16 ) {
				return false;
			}
			m_gametypeDef.addMap( token.take( *semicolonIndex ), *maybeNum, *maybeNum );
			continue;
		}

		const auto minPartView = rangePart.take( *dashIndex );
		const auto maxPartView = rangePart.drop( *dashIndex + 1 );
		const auto maybeMin = wsw::toNum<unsigned>( minPartView );
		const auto maybeMax = wsw::toNum<unsigned>( maxPartView );
		if( !maybeMin || !maybeMax ) {
			return false;
		}
		if( *maybeMin >= *maybeMax ) {
			return false;
		}
		if( *maybeMin < 2 || *maybeMax < 2 ) {
			return false;
		}
		if( *maybeMin > 16 || *maybeMax > 16 ) {
			return false;
		}

		m_gametypeDef.addMap( token.take( *semicolonIndex ), *maybeMin, *maybeMax );
	}
	return true;
}

bool GametypeDefParser::parseDescription() {
	if( !expectSection( "Description"_asView ) ) {
		return false;
	}

	wsw::StaticString<kBufferSize> s;
	const auto off = s.size();

	bool hadLine = false;
	for(;; ) {
		const auto res = readNextLine();
		if( !res ) {
			return false;
		}
		if( res->second & Eof ) {
			assert( res->first.empty() );
			if( !hadLine ) {
				return false;
			}
			auto len = s.length() - off;
			if( !len ) {
				return false;
			}
			m_gametypeDef.m_descSpanIndex = m_gametypeDef.m_stringDataStorage.add( s.asView() );
			return true;
		}
		hadLine = true;
		if( res->second & HadEmptyLines ) {
			// TODO: Push a rich text paragraph start?
			if( off != s.size() ) {
				s.push_back( '\n' );
			}
		}
		s.append( res->first.data(), res->first.size() );
		s.push_back( '\n' );
	}
}

auto GametypeDefParser::exec_() -> std::optional<GametypeDef> {
	if( !m_reader ) {
		return std::nullopt;
	}
	if( m_reader->getInitialFileSize() >= kBufferSize ) {
		return std::nullopt;
	}
	if( !parseTitle() || !parseFlags() || !parseBotConfig() || !parseMaps() || !parseDescription() ) {
		return std::nullopt;
	}
	return std::move( m_gametypeDef );
}

}
