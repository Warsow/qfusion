#include "chat.h"
#include "g_local.h"
#include <common/helpers/singletonholder.h>
#include <common/types/stringview.h>
#include <common/types/vector.h>

#include <cstdint>
#include <sstream>

using wsw::operator""_asView;

ChatPrintHelper::ChatPrintHelper( const edict_t *sender, uint64_t sendCommandNum, const char *format, ... )
	: m_sender( sender ), m_sendCommandNum( sendCommandNum ) {
	va_list va;
	va_start( va, format );
	formatTextFromVarargs( format, va );
	va_end( va );

	finishSetup();
}

ChatPrintHelper::ChatPrintHelper( const char *format, ... ) : m_sender( nullptr ), m_sendCommandNum( 0 ) {
	va_list va;
	va_start( va, format );
	formatTextFromVarargs( format, va );
	va_end( va );

	finishSetup();
}

ChatPrintHelper::ChatPrintHelper( const edict_t *source, uint64_t sendCommandNum, const wsw::StringView &message )
	: m_sender( source ), m_sendCommandNum( sendCommandNum ) {
	const wsw::StringView view( message.length() <= kMaxTextLength ? message : message.take( kMaxTextLength ) );
	view.copyTo( m_buffer + kTextOffset, view.length() + 1 );
	m_messageLength = (int)view.length();

	finishSetup();
}

void ChatPrintHelper::finishSetup() {
	// Replace double quotes in the message
	char *p = m_buffer + kTextOffset;
	while( ( p = strchr( p, '\"' ) ) ) {
		*p = '\'';
	}

	assert( !m_senderNum );
	if( m_sender ) {
		m_senderNum = PLAYERNUM( m_sender ) + 1;
	}

	// Wrap the message in double quotes
	m_buffer[kTextOffset - 1] = '\"';
	m_buffer[kTextOffset + m_messageLength] = '\"';
	m_buffer[kTextOffset + m_messageLength + 1] = '\0';
}

void ChatPrintHelper::formatTextFromVarargs( const char *format, va_list va ) {
	std::fill( m_buffer, m_buffer + kTextOffset, ' ' );
	m_messageLength = Q_vsnprintfz( m_buffer + kTextOffset, sizeof( m_buffer ) - kTextOffset, format, va );
	if( m_messageLength < 0 ) {
		m_messageLength = sizeof( m_buffer ) - kTextOffset - 1;
	}
}

void ChatPrintHelper::printToServerConsole( bool teamOnly ) {
	if( !dedicated->integer ) {
		return;
	}

	if( m_hasPrintedToServerConsole || m_skipServerConsole ) {
		return;
	}

	m_hasPrintedToServerConsole = true;

	const char *msg = m_buffer + kTextOffset;
	assert( m_buffer[kTextOffset + m_messageLength] == '"' );
	// Truncate the double quote at the end of the buffer
	m_buffer[kTextOffset + m_messageLength] = '\0';

	if( !m_sender ) {
		G_Printf( S_COLOR_GREEN "console: %s\n", m_buffer );     // admin console
	} else if( const auto *client = m_sender->r.client ) {
		if( teamOnly ) {
			const char *team = client->ps.stats[STAT_TEAM] == TEAM_SPECTATOR ? "SPEC" : "TEAM";
			G_Printf( S_COLOR_YELLOW "[%s]" S_COLOR_WHITE "%s" S_COLOR_YELLOW ": %s\n", team, client->netname.data(), msg );
		} else {
			G_Printf( "%s" S_COLOR_GREEN ": %s\n", client->netname.data(), msg );
		}
	}

	// Restore the double quote at the end of the buffer
	m_buffer[kTextOffset + m_messageLength] = '"';
}

void ChatPrintHelper::dispatchWithFilter( const ChatHandlersChain *filter, bool teamOnly ) {
	printToServerConsole( true );

	if( !m_sender ) {
		const char *cmd = setupPrefixForOthers( teamOnly );
		SV_DispatchGameCmd( nullptr, cmd );
		return;
	}

	const char *cmd = nullptr;
	for( int i = 0; i < ggs->maxclients; i++ ) {
		edict_t *ent = game.edicts + 1 + i;
		if( !ent->r.inuse || !ent->r.client ) {
			continue;
		}
		if( teamOnly && ent->s.team != m_sender->s.team ) {
			continue;
		}
		if( G_GetClientState( i ) < CS_CONNECTED ) {
			continue;
		}
		if( filter && filter->ignores( ent, m_sender ) ) {
			filter->notifyOfIgnoredMessage( ent, m_sender );
			continue;
		}
		// Set up the needed prefix
		const char *pendingCmd = cmd;
		if( m_sender == ent ) {
			cmd = setupPrefixForSender( teamOnly );
			// Force setting up the prefix for others on the next iteration
			pendingCmd = nullptr;
		} else if( !cmd ) {
			cmd = setupPrefixForOthers( teamOnly );
			pendingCmd = cmd;
		}
		SV_DispatchGameCmd( ent, cmd );
		cmd = pendingCmd;
	}
}

void ChatPrintHelper::printTo( const edict_t *target, bool teamOnly ) {
	if( !target->r.inuse ) {
		return;
	}

	if( !target->r.client ) {
		return;
	}

	if( teamOnly && m_sender && m_sender->s.team != target->s.team ) {
		return;
	}

	if( G_GetClientState( PLAYERNUM( target ) ) < CS_SPAWNED ) {
		return;
	}

	printToServerConsole( teamOnly );
	const char *cmd = ( m_sender == target ) ? setupPrefixForSender( teamOnly ) : setupPrefixForOthers( teamOnly );
	SV_DispatchGameCmd( target, cmd );
}

auto ChatPrintHelper::setupPrefixForOthers( bool teamOnly ) -> const char * {
	const char *command = teamOnly ? "tch" : "ch";
	constexpr int limit = kTextOffset - 1;
	const int res = Q_snprintfz( m_buffer, limit, "%s %d", command, m_senderNum );
	assert( res > 0 && res < limit );
	std::fill( m_buffer + res, m_buffer + limit, ' ' );
	return m_buffer;
}

auto ChatPrintHelper::setupPrefixForSender( bool teamOnly ) -> const char * {
	const char *command = teamOnly ? "tcha" : "cha";
	constexpr const char *format = "%s %" PRIu64 " %d";
	constexpr int limit = kTextOffset - 1;
	const int res = Q_snprintfz( m_buffer, limit, format, command, m_sendCommandNum, m_senderNum );
	assert( res > 0 && res < limit );
	std::fill( m_buffer + res, m_buffer + limit, ' ' );
	return m_buffer;
}

void MuteFilter::mute( const edict_s *ent ) {
	m_muted[ENTNUM( ent ) - 1] = true;
}

void MuteFilter::unmute( const edict_s *ent ) {
	m_muted[ENTNUM( ent ) - 1] = false;
}

auto MuteFilter::handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> {
	if( m_muted[message.clientNum] ) {
		return MessageFault { message.clientCommandNum, MessageFault::Muted, 0 };
	}
	return std::nullopt;
}

bool RespectHandler::skipStatsForClient( const edict_s *ent ) const {
	const auto &entry = m_entries[ENTNUM( ent ) - 1];
	return entry.m_hasViolatedCodex || entry.m_hasIgnoredCodex;
}

void RespectHandler::addToReportStats( const edict_s *ent, RespectStats *reported ) {
	m_entries[ENTNUM( ent ) - 1].addToReportStats( reported );
}

void RespectHandler::onClientDisconnected( const edict_s *ent ) {
	m_entries[ENTNUM( ent ) - 1].onClientDisconnected();
}

void RespectHandler::onClientJoinedTeam( const edict_s *ent, int newTeam ) {
	m_entries[ENTNUM( ent ) - 1].onClientJoinedTeam( newTeam );
}

bool IgnoreFilter::ignores( const edict_s *target, const edict_s *source ) const {
	if( target == source ) {
		return false;
	}
	const ClientEntry &e = m_entries[PLAYERNUM( target )];
	if( e.ignoresEverybody ) {
		return true;
	}
	if( e.ignoresNotTeammates && ( target->s.team != source->s.team ) ) {
		return true;
	}
	return e.GetClientBit( PLAYERNUM( source ) );
}

RespectHandler::RespectHandler() {
	for( int i = 0; i < MAX_CLIENTS; ++i ) {
		m_entries[i].m_ent = game.edicts + i + 1;
	}
	reset();
}

void RespectHandler::reset() {
	for( ClientEntry &e: m_entries ) {
		e.reset();
	}

	m_matchStartedAt = -1;
	m_lastFrameMatchState = MATCH_STATE_NONE;
}

void RespectHandler::frame() {
	const auto matchState = GS_MatchState( *ggs );
	// This is not 100% correct but is sufficient for message checks
	if( matchState == MATCH_STATE_PLAYTIME ) {
		if( m_lastFrameMatchState != MATCH_STATE_PLAYTIME ) {
			m_matchStartedAt = level.time;
		}
	}

	if( !GS_RaceGametype( *ggs ) ) {
		for( int i = 0; i < ggs->maxclients; ++i ) {
			m_entries[i].checkBehaviour( m_matchStartedAt );
		}
	}

	m_lastFrameMatchState = matchState;
}

auto RespectHandler::handleMessage( const ChatMessage &message ) -> std::optional<MessageFault> {
	// Race is another world...
	if( GS_RaceGametype( *ggs ) ) {
		return std::nullopt;
	}

	const auto matchState = GS_MatchState( *ggs );
	// Ignore until countdown
	if( matchState < MATCH_STATE_COUNTDOWN ) {
		return std::nullopt;
	}

	assert( message.clientNum < std::size( m_entries ) );
	(void)m_entries[message.clientNum].handleMessage( message );
	// TODO we don't really detect faults with this handler
	return std::nullopt;
}

void RespectHandler::ClientEntry::reset() {
	m_warnedAt = 0;
	m_joinedMidGameAt = 0;
	std::fill( std::begin( m_lastSaidAt ), std::end( m_lastSaidAt ), 0 );
	std::fill( std::begin( m_numSaidTokens ), std::end( m_numSaidTokens ), 0 );
	m_hasCompletedMatchStartAction = false;
	m_hasCompletedMatchEndAction = false;
	m_hasTakenCountdownHint = false;
	m_hasTakenStartHint = false;
	m_hasTakenSecondStartHint = false;
	m_hasTakenFinalHint = false;
	m_hasIgnoredCodex = false;
	m_hasViolatedCodex = false;
}

bool RespectHandler::ClientEntry::handleMessage( const ChatMessage &message ) {
	// If has already violated or ignored the Codex
	if( m_hasViolatedCodex || m_hasIgnoredCodex ) {
		return false;
	}

	const auto matchState = GS_MatchState( *ggs );
	// Skip everything in warmup
	if( matchState < MATCH_STATE_COUNTDOWN ) {
		return false;
	}

	// Skip messages from spectators unless being post-match.
	if( matchState < MATCH_STATE_POSTMATCH && ( m_ent->s.team == TEAM_SPECTATOR ) ) {
		return false;
	}

	if( processMessageAsRespectTokensOnlyMessage( message.text ) ) {
		return false;
	}

	// Skip further tests for spectators (we might have saved post-match tokens that could be important)
	if( m_ent->s.team == TEAM_SPECTATOR ) {
		return false;
	}
	// Allow chatting (and just save respect tokens) in timeouts as well
	if( GS_MatchPaused( *ggs ) ) {
		return false;
	}

	bool displayingWarningRequested = false;
	bool respectViolationTriggered  = false;

	if( G_ISGHOSTING( m_ent ) ) {
		if( const int64_t millisSinceLastWarn = level.time - m_warnedAt; millisSinceLastWarn >= 2000 ) {
			// This is primarily for round-based gametypes. Just print a warning.
			// Fragged players waiting for a next round start
			// are not considered spectators while really they are.
			// Other gametypes should follow this behaviour to avoid misunderstanding rules.
			displayingWarningRequested = true;
		}
	} else {
		// We do not intercept this condition in RespectHandler::HandleMessage()
		// as we still need to collect last said tokens for clients using CheckForTokens()
		if( matchState <= MATCH_STATE_PLAYTIME ) {
			if( matchState < MATCH_STATE_PLAYTIME ) {
				displayingWarningRequested = true;
				// Don't set the timestamp
			} else {
				if( !m_warnedAt ) {
					m_warnedAt = level.time;
					displayingWarningRequested = true;
				} else {
					// Don't warn again for occasional flood
					if( const int64_t millisSinceLastWarn = level.time - m_warnedAt; millisSinceLastWarn > 2000 ) {
						// Allow speaking occasionally once per 5 minutes
						if( millisSinceLastWarn > 5 * 60 * 1000 ) {
							m_warnedAt = level.time;
							displayingWarningRequested = true;
						} else {
							m_hasViolatedCodex = true;
							respectViolationTriggered = true;
						}
					}
				}
			}
		}
	}

	assert( !( displayingWarningRequested && respectViolationTriggered ) );
	if( displayingWarningRequested ) {
		const wsw::StringView title( "Less talk, let's play!"_asView );
		const wsw::StringView desc( ""_asView );
		// Zero-size arrays are illegal for MSVC. We should switch to passing std::span eventually.
		const std::pair<wsw::StringView, wsw::StringView> actions[1];
		G_SendActionRequest( m_ent, "respectWarning"_asView, 3000, title, desc, actions, actions + 0 );
		// Continue the message handling
		return false;
	}

	if( respectViolationTriggered ) {
		// Print the message first
		ChatPrintHelper chatPrintHelper( m_ent, message.clientCommandNum, message.text );
		chatPrintHelper.printToEverybody( ChatHandlersChain::instance() );
		// Then announce
		announceMisconductBehaviour( "violated" );
		// Interrupt the message handling
		return true;
	}

	// Continue the message handling
	return false;
}

void RespectHandler::ClientEntry::announceMisconductBehaviour( const char *action ) {
	// Ignore bots.
	// We plan to add R&S bot behaviour but do not currently want to touch the game module
	if( m_ent->r.svflags & SVF_FAKECLIENT ) {
		return;
	}

	// We can't actually figure out printing that in non-triggering fashion
	(void)action;

	char message[256] = S_COLOR_YELLOW "'" S_COLOR_CYAN "Fair play" S_COLOR_YELLOW "' award lost";
	G_PrintMsg( m_ent, "%s!\n", message );
}

class RespectToken {
	const wsw::StringView m_name;
	const unsigned m_tokenNum;
	const wsw::PodVector<wsw::StringView> m_aliases;

	[[nodiscard]]
	auto tryMatchingByAlias( const char *p, const wsw::StringView &alias ) const -> std::optional<unsigned>;
public:
	RespectToken( const wsw::StringView &name, unsigned tokenNum, wsw::PodVector<wsw::StringView> &&aliases ) noexcept
		: m_name( name ), m_tokenNum( tokenNum ), m_aliases( aliases ) {
		assert( !m_aliases.empty() );
		assert( std::all_of( m_aliases.begin(), m_aliases.end(), []( const wsw::StringView &a ) { return a.isZeroTerminated(); } ) );
	}

	[[nodiscard]]
	auto getName() const -> const wsw::StringView & { return m_name; }
	[[nodiscard]]
	auto getNum() const -> unsigned { return m_tokenNum; }

	[[nodiscard]]
	auto getMatchedLength( const char *p ) const -> std::optional<unsigned>;
};

auto RespectToken::getMatchedLength( const char *p ) const -> std::optional<unsigned> {
	for( const wsw::StringView &alias: m_aliases ) {
		if( const std::optional<unsigned> maybeMatchedLen = tryMatchingByAlias( p, alias ) ) {
			return maybeMatchedLen;
		}
	}
	return std::nullopt;
}

auto RespectToken::tryMatchingByAlias( const char *p, const wsw::StringView &alias ) const -> std::optional<unsigned> {
	assert( alias.isZeroTerminated() );
	const char *const start = p;
	for( const char aliasChar: alias ) {
		assert( !::isalpha( aliasChar ) || ::islower( aliasChar ) );
		// Try finding a first character that is not a part of a color token
		for(; ; ) {
			const char charToMatch = *p++;
			if( ::tolower( charToMatch ) == aliasChar ) {
				break;
			}
			if( charToMatch != '^' ) {
				return std::nullopt;
			}
			const char nextCharToMatch = *p++;
			if( nextCharToMatch && !::isdigit( nextCharToMatch ) ) {
				return std::nullopt;
			}
		}
	}
	return (unsigned)( p - start );
}

class RespectTokensRegistry {
	static const std::array<RespectToken, 10> kTokens;

	static_assert( RespectHandler::kNumTokens == 10 );
public:
	static const unsigned kSayAtStartTokenNum;
	static const unsigned kSayAtEndTokenNum;

	[[nodiscard]]
	static auto matchByToken( const char **p ) -> std::optional<unsigned>;

	[[nodiscard]]
	static auto findByName( const wsw::StringView &name ) -> const RespectToken * {
		for( const RespectToken &token: kTokens ) {
			if( token.getName().equalsIgnoreCase( name ) ) {
				return std::addressof( token );
			}
		}
		return nullptr;
	}

	[[nodiscard]]
	static auto getTokenForNum( unsigned num ) -> const RespectToken & {
		const auto &result = kTokens[num];
		assert( result.getNum() == num );
		return result;
	}
};

const std::array<RespectToken, 10> RespectTokensRegistry::kTokens = {{
	{ "hi"_asView, 0, { "hi"_asView } },
	{ "bb"_asView, 1, { "bb"_asView } },
	{ "glhf"_asView, 2, { "glhf"_asView, "gl"_asView, "hf"_asView } },
	{ "gg"_asView, 3, { "ggs"_asView, "gg"_asView, "bgs"_asView, "bg"_asView } },
	{ "plz"_asView, 4, { "plz"_asView } },
	{ "tks"_asView, 5, { "tks"_asView } },
	{ "soz"_asView, 6, { "soz"_asView } },
	{ "n1"_asView, 7, { "n1"_asView } },
	{ "nt"_asView, 8, { "nt"_asView } },
	{ "lol"_asView, 9, { "lol"_asView } },
}};

const unsigned RespectTokensRegistry::kSayAtStartTokenNum = RespectTokensRegistry::findByName( "glhf"_asView )->getNum();
const unsigned RespectTokensRegistry::kSayAtEndTokenNum   = RespectTokensRegistry::findByName( "gg"_asView )->getNum();

auto RespectTokensRegistry::matchByToken( const char **p ) -> std::optional<unsigned> {
	for( const RespectToken &token: kTokens ) {
		if( const std::optional<unsigned> maybeMatchedLength = token.getMatchedLength( *p ) ) {
			*p += *maybeMatchedLength;
			return token.getNum();
		}
	}
	return std::nullopt;
}

/**
 * Tries to match a sequence like this: {@code ( Whitespace-Char* Color-Token? )* }
 * @param s an address of the supplied string. Gets modified on success.
 * @param numWhitespaceChars an address to write a number of whitespace characters met.
 * @return false if there were malformed color tokens (the only kind of failure possible).
 */
static bool StripUpToMaybeToken( const char **s, int *numWhitespaceChars ) {
	*numWhitespaceChars = 0;
	const char *p = *s;
	for(; ; ) {
		const char *const oldp = p;
		// Strip whitespace and punctuation except the circumflex that requires a special handling
		while( ::ispunct( *p ) && *p != '^' ) {
			p++;
		}
		*numWhitespaceChars += (int)( oldp - p );
		// Interrupt at the string end
		if( !*p ) {
			break;
		}
		// Try matching a single color token
		const char ch = *p;
		if( ch != '^' ) {
			break;
		}
		p++;
		const char nextCh = *p++;
		// Interrupt at an incomplete color token at the end with success
		if( !nextCh ) {
			break;
		}
		// A next character (if any) must be a digit
		if( !::isdigit( nextCh ) ) {
			return false;
		}
		// Go to a next color token (if any)
	}

	*s = p;
	return true;
}

bool RespectHandler::ClientEntry::processMessageAsRespectTokensOnlyMessage( const wsw::StringView &message ) {
	// Do not modify tokens count immediately
	// Either this routine fails completely or stats for all tokens get updated
	unsigned numFoundTokens[kNumTokens];
	std::fill( std::begin( numFoundTokens ), std::end( numFoundTokens ), 0 );

	bool expectPunctOrSpace = false;
	// TODO: Make everything work with string views directly
	wsw::StaticString<MAX_STRING_CHARS> buffer( message.data(), message.size() );
	const char *p = buffer.data();
	for(;; ) {
		int numWhitespaceChars = 0;
		// If there were malformed color tokens
		if( !StripUpToMaybeToken( &p, &numWhitespaceChars ) ) {
			return false;
		}
		// If we've reached the string end
		if( !*p ) {
			break;
		}
		// If we didn't advance a displayed position after a previously matched token
		if( expectPunctOrSpace && !numWhitespaceChars ) {
			return false;
		}
		const std::optional<unsigned> maybeTokenNum = RespectTokensRegistry::matchByToken( &p );
		if( maybeTokenNum == std::nullopt ) {
			return false;
		}
		numFoundTokens[*maybeTokenNum]++;
		// Expect a whitespace after just matched token
		// (punctuation characters are actually allowed as well).
		expectPunctOrSpace = true;
	}

	const int64_t levelTime = level.time;
	for( unsigned tokenNum = 0; tokenNum < kNumTokens; ++tokenNum ) {
		if( const unsigned numTokens = numFoundTokens[tokenNum] ) {
			m_numSaidTokens[tokenNum] += numTokens;
			m_lastSaidAt[tokenNum] = levelTime;
		}
	}

	return true;
}

void RespectHandler::ClientEntry::checkBehaviour( const int64_t matchStartTime ) {
	if( !m_ent->r.inuse ) {
		return;
	}

	if( !m_ent->r.client->stats.had_playtime ) {
		return;
	}

	if( m_hasViolatedCodex || m_hasIgnoredCodex ) {
		return;
	}

	if( m_hasCompletedMatchStartAction && m_hasCompletedMatchEndAction ) {
		return;
	}

	const auto levelTime     = level.time;
	const auto matchState    = GS_MatchState( *ggs );
	const auto startTokenNum = RespectTokensRegistry::kSayAtStartTokenNum;

	if( matchState == MATCH_STATE_COUNTDOWN ) {
		if( !m_hasCompletedMatchStartAction ) {
			// If has just said "glhf"
			if( levelTime - m_lastSaidAt[startTokenNum] < 64 ) {
				m_hasCompletedMatchStartAction = true;
			} else {
				if( !m_hasTakenCountdownHint ) {
					requestClientRespectAction( startTokenNum );
					m_hasTakenCountdownHint = true;
				}
			}
		}
	} else if( matchState == MATCH_STATE_PLAYTIME ) {
		if( !m_hasCompletedMatchStartAction ) {
			const auto lastActivityAt = m_ent->r.client->last_activity;
			if( !lastActivityAt || levelTime - lastActivityAt > 10000 ) {
				// Skip inactive clients considering their behaviour respectful
				m_hasCompletedMatchStartAction = true;
			} else if( levelTime - m_lastSaidAt[startTokenNum] < 64 ) {
				// Complete the start action
				m_hasCompletedMatchStartAction = true;
			} else {
				const int64_t countdownStartTime = wsw::max( m_joinedMidGameAt, matchStartTime );
				if( levelTime - countdownStartTime > 1500 ) {
					if( !m_hasTakenStartHint ) {
						requestClientRespectAction( startTokenNum );
						m_hasTakenStartHint = true;
					} else {
						// Wait for making a second hint
						if( levelTime - countdownStartTime > 1500 + 5000 ) {
							if( !m_hasTakenSecondStartHint ) {
								requestClientRespectAction( startTokenNum );
								m_hasTakenSecondStartHint = true;
							} else {
								// Consider that the user has ignored the R&S Codex
								if( levelTime - countdownStartTime > 10000 ) {
									// The misconduct behaviour is going to be detected inevitably.
									// This is just to prevent massive console spam at the same time.
									if( random() > 0.95f ) {
										m_hasIgnoredCodex = true;
										announceMisconductBehaviour( "ignored" );
									}
								}
							}
						}
					}
				}
			}
		}
	} else if( matchState == MATCH_STATE_POSTMATCH ) {
		if( !m_hasCompletedMatchEndAction ) {
			// A note: we do not distinguish players that became spectators mid-game
			// and players that have played till the match end.
			// They still have to say the mandatory token at the end with the single exception of becoming inactive.
			const auto lastActivityAt = m_ent->r.client->last_activity;
			if( !lastActivityAt || levelTime - lastActivityAt > 10000 ) {
				m_hasCompletedMatchEndAction = true;
			} else {
				const unsigned endTokenNum = RespectTokensRegistry::kSayAtEndTokenNum;
				if( levelTime - m_lastSaidAt[endTokenNum] < 64 ) {
					G_PlayerAward( m_ent, S_COLOR_CYAN "Fair play!" );
					G_PrintMsg( m_ent, "Your stats and awards have been confirmed!\n" );

					m_hasCompletedMatchEndAction = true;
				} else {
					if( !m_hasTakenFinalHint ) {
						requestClientRespectAction( endTokenNum );
						m_hasTakenFinalHint = true;
					}
				}
			}
		}
	}
}

void RespectHandler::ClientEntry::requestClientRespectAction( unsigned tokenNum ) {
	wsw::StringView rawTokenView( RespectTokensRegistry::getTokenForNum( tokenNum ).getName() );
	wsw::StaticString<8> token;
	token << rawTokenView;
	for( char &ch: token ) {
		ch = (char)std::toupper( ch );
	}

	const wsw::StringView yellow( S_COLOR_YELLOW ), white( S_COLOR_WHITE );

	wsw::StaticString<32> title;
	title << "Say "_asView << yellow << token << white << ", please!"_asView;

	// Client bindings for a 1st token start at 0-th offset in the numeric keys row.
	const unsigned keyNum = ( tokenNum + 1 ) % 10;

	wsw::StaticString<32> desc;
	desc << "Press "_asView << yellow << keyNum << white << " for that"_asView;

	wsw::StaticString<16> key( "%d", keyNum );

	wsw::StaticString<16> command;
	command << "say "_asView << rawTokenView;

	const std::pair<wsw::StringView, wsw::StringView> actions[1] { { key.asView(), command.asView() } };
	G_SendActionRequest( m_ent, "respectAction"_asView, 4000, title.asView(), desc.asView(), actions, actions + 1 );
}

void RespectHandler::ClientEntry::onClientDisconnected() {
	// We assume that Statsow has just saved stats (including respect status) for this client
	// TODO: Do something for non-authenticated clients
	reset();
}

void RespectHandler::ClientEntry::onClientJoinedTeam( int newTeam ) {
	if( GS_MatchState( *ggs ) == MATCH_STATE_PLAYTIME ) {
		// Invalidate the "saidAfter" flag possible set on a disconnection during a match
		// TODO: Does it still hold?
		if( newTeam == TEAM_SPECTATOR ) {
			m_hasCompletedMatchEndAction = false;
		} else {
			if( !m_joinedMidGameAt ) {
				m_joinedMidGameAt = level.time;
			}
		}
	}
}

void RespectHandler::ClientEntry::addToReportStats( RespectStats *reportedStats ) {
	if( !reportedStats->hasViolatedCodex ) {
		if( m_hasViolatedCodex ) {
			reportedStats->Clear();
			reportedStats->hasViolatedCodex = true;
			reportedStats->hasIgnoredCodex  = m_hasIgnoredCodex;
		} else if( !reportedStats->hasIgnoredCodex ) {
			if( m_hasIgnoredCodex ) {
				reportedStats->Clear();
				reportedStats->hasIgnoredCodex = true;
			} else {
				for( unsigned tokenNum = 0; tokenNum < kNumTokens; ++tokenNum ) {
					if( const auto numTokens = m_numSaidTokens[tokenNum]; numTokens > 0 ) {
						const auto &token = RespectTokensRegistry::getTokenForNum( tokenNum );
						assert( token.getName().isZeroTerminated() );
						reportedStats->AddToEntry( token.getName().data(), numTokens );
					}
				}
			}
		}
	}
}

void IgnoreFilter::handleIgnoreCommand( const edict_t *ent, bool ignore, const CmdArgs &cmdArgs ) {
	const int numArgs = wsw::min( Cmd_Argc(), MAX_CLIENTS );
	if( numArgs < 2 ) {
		printIgnoreCommandUsage( ent, ignore );
		return;
	}

	// Resetting of global flags should also unset individual bits

	ClientEntry &e = m_entries[PLAYERNUM( ent )];
	const char *prefix = Cmd_Argv( 1 );
	if( !Q_stricmp( prefix, "everybody" ) ) {
		if( e.ignoresEverybody == ignore ) {
			return;
		}
		e.ignoresEverybody = ignore;
		e.ignoresNotTeammates = false;
		if( !ignore ) {
			e.ignoredClientsMask = 0;
		}
		sendChangeFilterVarCommand( ent );
		return;
	}

	if( !Q_stricmp( prefix, "notteam" ) ) {
		if( e.ignoresNotTeammates == ignore ) {
			return;
		}
		e.ignoresNotTeammates = ignore;
		e.ignoresEverybody = false;
		if( !ignore ) {
			for( int i = 0; i <= ggs->maxclients; ++i ) {
				const edict_t *player = game.edicts + i + 1;
				if( player->r.inuse && player->s.team != ent->s.team ) {
					e.SetClientBit( i, false );
				}
			}
		}
		sendChangeFilterVarCommand( ent );
		return;
	}

	if( Q_stricmp( prefix, "players" ) != 0 ) {
		printIgnoreCommandUsage( ent, ignore );
		return;
	}

	uint64_t requestedMask = 0;
	static_assert( MAX_CLIENTS <= 64, "" );
	bool wereOnlyTeammates = true;
	// Convert player numbers first before applying a modification (don't apply changes partially)
	for( int i = 2; i < numArgs; ++i ) {
		const char *arg = Cmd_Argv( i );
		const edict_t *player = G_PlayerForText( arg );
		if( !player ) {
			G_PrintMsg( ent, "Failed to get a player for `%s`\n", arg );
			return;
		}
		if( player == ent ) {
			G_PrintMsg( ent, "You can't ignore yourself\n" );
			return;
		}
		wereOnlyTeammates &= ( player->s.team == ent->s.team );
		requestedMask |= ( ( (uint64_t)1 ) << PLAYERNUM( player ) );
	}

	// We think we should no longer keep these global flags
	// if a player affected by these flags was mentioned in "unignore" command
	if( !ignore && requestedMask ) {
		if( e.ignoresEverybody ) {
			e.ignoresEverybody = false;
			// Convert the global flag to the per-client mask
			e.ignoredClientsMask = ~( (uint64_t)0 );
			sendChangeFilterVarCommand( ent );
		} else if( e.ignoresNotTeammates && !wereOnlyTeammates ) {
			e.ignoresNotTeammates = false;
			// Convert the global flag to the per-client mask
			for( int i = 0; i < ggs->maxclients; ++i ) {
				const edict_t *clientEnt = game.edicts + i + 1;
				if( clientEnt->r.inuse && clientEnt->s.team != ent->s.team ) {
					e.SetClientBit( i, true );
				}
			}
			sendChangeFilterVarCommand( ent );
		}
	}

	if( ignore ) {
		e.ignoredClientsMask |= requestedMask;
	} else {
		e.ignoredClientsMask &= ~requestedMask;
	}
}

void IgnoreFilter::printIgnoreCommandUsage( const edict_t *ent, bool ignore ) {
	const char *usageFormat = "Usage: %s players <player1> [, <player2> ...] or %s everybody or %s notteam\n";
	const char *verb = ignore ? "ignore" : "unignore";
	G_PrintMsg( ent, usageFormat, verb, verb, verb );
}

void IgnoreFilter::sendChangeFilterVarCommand( const edict_t *ent ) {
	ClientEntry &e = m_entries[PLAYERNUM( ent )];
	int value = 0;
	if( e.ignoresEverybody ) {
		value = 1;
	} else if( e.ignoresNotTeammates ) {
		value = 2;
	}
	SV_DispatchGameCmd( ent, va( "ign setVar %d", value ) );
}

void IgnoreFilter::handleIgnoreListCommand( const edict_t *ent, const CmdArgs &cmdArgs ) {
	const edict_t *player = G_PlayerForText( Cmd_Argv( 1 ) );
	if( !player ) {
		if( Cmd_Argc() >= 2 ) {
			G_PrintMsg( ent, "Usage: ignorelist [<player>]\n" );
			return;
		}
		player = ent;
	}

	const char *action = S_COLOR_WHITE "You ignore";
	const char *pronoun = S_COLOR_WHITE "your";
	char buffer[64];
	if( player != ent ) {
		Q_snprintfz( buffer, sizeof( buffer ), S_COLOR_WHITE "%s" S_COLOR_WHITE " ignores", player->r.client->netname.data() );
		action = buffer;
		pronoun = "their";
	}

	const ClientEntry &e = m_entries[PLAYERNUM( player )];
	if( e.ignoresEverybody ) {
		G_PrintMsg( ent, "%s everybody\n", action );
		return;
	}

	if( !e.ignoredClientsMask ) {
		if( e.ignoresNotTeammates ) {
			G_PrintMsg( ent, "%s players not in %s team\n", action, pronoun );
		} else {
			G_PrintMsg( ent, "%s nobody\n", action );
		}
		return;
	}

	std::stringstream ss;
	ss << action;
	const char *separator = " ";
	bool wereTeammatesMet = false;
	for( int i = 0; i < ggs->maxclients; ++i ) {
		const auto *clientEnt = game.edicts + i + 1;
		if( clientEnt == player ) {
			continue;
		}
		if( !clientEnt->r.inuse ) {
			continue;
		}
		if( G_GetClientState( i ) < CS_SPAWNED ) {
			continue;
		}
		if( !e.GetClientBit( i ) ) {
			continue;
		}
		if( e.ignoresNotTeammates ) {
			if( clientEnt->s.team != player->s.team ) {
				continue;
			}
			wereTeammatesMet = true;
		}
		ss << S_COLOR_WHITE << separator << clientEnt->r.client->netname.data();
		separator = ", ";
	}

	if( e.ignoresNotTeammates ) {
		if( wereTeammatesMet ) {
			ss << S_COLOR_WHITE << " and";
		}
		ss << S_COLOR_WHITE " players not in " << pronoun << " team";
	}

	const auto str( ss.str() );
	G_PrintMsg( ent, "%s\n", str.c_str() );
}

void IgnoreFilter::reset() {
	for( ClientEntry &e: m_entries ) {
		e.Reset();
	}
}

void IgnoreFilter::notifyOfIgnoredMessage( const edict_t *target, const edict_t *source ) const {
	SV_DispatchGameCmd( target, va( "ign %d", PLAYERNUM( source ) + 1 ) );
}

void IgnoreFilter::onUserInfoChanged( const edict_t *user ) {
	ClientEntry &e = m_entries[PLAYERNUM( user )];
	const int flags = user->r.client->getChatFilterFlags();
	e.ignoresEverybody = ( flags & 1 ) != 0;
	e.ignoresNotTeammates = ( flags & 2 ) != 0;
}

static SingletonHolder<ChatHandlersChain> chatHandlersChainHolder;

void ChatHandlersChain::init() {
	::chatHandlersChainHolder.init();
}

void ChatHandlersChain::shutdown() {
	::chatHandlersChainHolder.shutdown();
}

ChatHandlersChain *ChatHandlersChain::instance() {
	return ::chatHandlersChainHolder.instance();
}

void ChatHandlersChain::reset() {
	m_muteFilter.reset();
	m_floodFilter.reset();
	m_respectHandler.reset();
	m_ignoreFilter.reset();
}

void ChatHandlersChain::resetForClient( int clientNum ) {
	m_muteFilter.resetForClient( clientNum );
	m_floodFilter.resetForClient( clientNum );
	m_respectHandler.resetForClient( clientNum );
	m_ignoreFilter.resetForClient( clientNum );
}

void ChatHandlersChain::onClientDisconnected( const edict_s *ent ) {
	// Reset everything except the respect handler which needs special treatment
	m_muteFilter.resetForClient( ENTNUM( ent ) - 1 );
	m_floodFilter.resetForClient( ENTNUM( ent ) - 1 );
	m_respectHandler.onClientDisconnected( ent );
	m_ignoreFilter.resetForClient( ENTNUM( ent ) - 1 );
}

auto ChatHandlersChain::handleMessage( const ChatMessage &message )
	-> std::optional<MessageFault> {
	// We want to call overridden methods directly just to avoid pointless virtual invocations.
	// Filters are applied in order of their priority.
	if( const auto maybeFault = m_muteFilter.handleMessage( message ) ) {
		return maybeFault;
	}
	if( const auto maybeFault = m_floodFilter.handleMessage( message ) ) {
		return maybeFault;
	}
	if( const auto maybeFault = m_respectHandler.handleMessage( message ) ) {
		return maybeFault;
	}

	ChatPrintHelper chatPrintHelper( PLAYERENT( message.clientNum ), message.clientCommandNum, message.text );
	chatPrintHelper.printToEverybody( ChatHandlersChain::instance() );
	return std::nullopt;
}