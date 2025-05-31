#include "baseteam.h"
#include "squadbasedteam.h"
#include "../bot.h"
#include <common/links.h>

AiBaseTeam *AiBaseTeam::teamsForNums[GS_MAX_TEAMS - 1];

AiBaseTeam::AiBaseTeam( int teamNum_ )
	: m_frameAffinityModulo( 4 ), m_frameAffinityOffset( teamNum_ != TEAM_BETA ? 0 : 2 ), teamNum( teamNum_ ) {}

void AiBaseTeam::Debug( const char *format, ... ) {
	// Cut it early to help optimizer to eliminate AI_Debugv call
#ifdef _DEBUG
	va_list va;
	va_start( va, format );
	AI_Debugv( GS_TeamName( ggs, teamNum ), format, va );
	va_end( va );
#endif
}

void AiBaseTeam::CheckTeamNum( int teamNum ) {
#ifndef PUBLIC_BUILD
	if( teamNum < TEAM_PLAYERS || teamNum >= GS_MAX_TEAMS ) {
		AI_FailWith( "AiBaseTeam", "GetTeamForNum(): Illegal team num %d\n", teamNum );
	}
#endif
}

AiBaseTeam **AiBaseTeam::TeamRefForNum( int teamNum ) {
	CheckTeamNum( teamNum );
	return &teamsForNums[teamNum - TEAM_PLAYERS];
}

AiBaseTeam *AiBaseTeam::GetTeamForNum( int teamNum ) {
	CheckTeamNum( teamNum );
	AiBaseTeam **teamRef = TeamRefForNum( teamNum );
	if( !*teamRef ) {
		AI_FailWith( "AiBaseTeam", "GetTeamForNum(): A team for num %d is not instantiated atm\n", teamNum );
	}
	return *teamRef;
}

AiBaseTeam *AiBaseTeam::ReplaceTeam( int teamNum, const std::type_info &desiredType ) {
	// Make sure this method is applied only for instantiation of descendants
	assert( typeid( AiBaseTeam ) != desiredType );

	AiBaseTeam **teamRef = TeamRefForNum( teamNum );
	if( !*teamRef ) {
		AI_FailWith( "AiBaseTeam::ReplaceTeam()", "A team for num %d has not been instantiated yet\n", teamNum );
	}

	// Destroy the existing AI team for the team slot
	AiBaseTeam *oldTeam = *teamRef;
	// Delegate further ops to the descendant factory
	*teamRef = AiSquadBasedTeam::InstantiateTeam( teamNum, desiredType );
	// Move an additional state (if any) from the old team
	( *teamRef )->TransferStateFrom( oldTeam );
	// Destroy the old team
	oldTeam->~AiBaseTeam();
	Q_free( oldTeam );

	return *teamRef;
}

void AiBaseTeam::AddBot( Bot *bot ) {
	Debug( "new bot %s has been added\n", bot->Nick() );

	// Link first
	wsw::link( bot, &teamBotsHead, Bot::TEAM_LINKS );
	// Acquire affinity after linking
	AcquireBotFrameAffinity( ENTNUM( bot->self ) );
	// Call subtype method (if any) last
	OnBotAdded( bot );
}

void AiBaseTeam::RemoveBot( Bot *bot ) {
	Debug( "bot %s has been removed\n", bot->Nick() );

	// Call subtype method (if any) first
	OnBotRemoved( bot );
	// Release affinity before linking
	ReleaseBotFrameAffinity( ENTNUM( bot->self ) );
	// Unlink last
	wsw::unlink( bot, &teamBotsHead, Bot::TEAM_LINKS );
}

void AiBaseTeam::TransferStateFrom( AiBaseTeam *that ) {
	// Transfer bots list
	this->teamBotsHead = that->teamBotsHead;
	that->teamBotsHead = nullptr;
}

void AiBaseTeam::AcquireBotFrameAffinity( int entNum ) {
	if( GS_TeamBasedGametype( *ggs ) ) {
		assert( m_frameAffinityModulo == 4 );
		// Older versions used to set think frames of every team bot one frame after the team thinks.
		// It was expected that the team logic is going to be quite computational expensive.
		// Actually it is not more expensive than logic of a single bot.
		// Lets distribute frames evenly (giving the team the same weight as for a bot).

		// 0 for ALPHA, 2 for BETA
		const auto teamOffset = 2 * (unsigned)( teamNum - TEAM_ALPHA );
		assert( teamOffset == 0 || teamOffset == 2 );
		unsigned chosenOffset = teamOffset;
		// If more bots think at teamOffset frames (counting the team as a "bot" too)
		if( m_affinityOffsetsInUse[teamOffset] + 1 > m_affinityOffsetsInUse[teamOffset + 1] ) {
			chosenOffset = teamOffset + 1;
		}
		m_affinityOffsetsInUse[chosenOffset]++;
		SetBotFrameAffinity( entNum, m_frameAffinityModulo, chosenOffset );
	} else {
		// Distribute bot think frames evenly for non-team gametypes
		unsigned chosenOffset = 0;
		for( unsigned i = 1; i < std::size( m_affinityOffsetsInUse ); ++i ) {
			if( m_affinityOffsetsInUse[chosenOffset] > m_affinityOffsetsInUse[i] ) {
				chosenOffset = i;
			}
		}
		m_affinityOffsetsInUse[chosenOffset]++;
		SetBotFrameAffinity( entNum, m_frameAffinityModulo, chosenOffset );
	}
}

void AiBaseTeam::ReleaseBotFrameAffinity( int entNum ) {
	const unsigned offset = m_botAffinityOffsets[entNum];
	m_botAffinityOffsets[entNum] = 0;
	m_affinityOffsetsInUse[offset]--;
}

void AiBaseTeam::SetBotFrameAffinity( int entNum, unsigned modulo, unsigned offset ) {
	m_botAffinityOffsets[entNum] = (uint8_t)offset;
	game.edicts[entNum].bot->m_frameAffinityModulo = modulo;
	game.edicts[entNum].bot->m_frameAffinityOffset = offset;
}

void AiBaseTeam::Init() {
#ifndef PUBLIC_BUILD
	for( int team = TEAM_PLAYERS; team < GS_MAX_TEAMS; ++team ) {
		if( *TeamRefForNum( team ) ) {
			AI_FailWith( "AiBaseTeam::Init()", "A team for num %d is already present / was not released", team );
		}
	}
#endif

	if( GS_TeamBasedGametype( *ggs ) ) {
		for( int team = TEAM_ALPHA; team < GS_MAX_TEAMS; ++team ) {
			CreateTeam( team );
		}
	} else {
		CreateTeam( TEAM_PLAYERS );
	}
}

void AiBaseTeam::Shutdown() {
	// Destroy all current teams (if any)
	for( int team = TEAM_PLAYERS; team < GS_MAX_TEAMS; ++team ) {
		ReleaseTeam( team );
	}
}

void AiBaseTeam::CreateTeam( int teamNum ) {
	AiBaseTeam **teamRef = TeamRefForNum( teamNum );
	// If there was an existing
	if( *teamRef ) {
		ReleaseTeam( teamNum );
	}
	// Set team pointer
	*teamRef = InstantiateTeam( teamNum );
	// TODO: Should we nofify bots? They should not use a cached team reference and always use GetTeamForNum()
}

void AiBaseTeam::ReleaseTeam( int teamNum ) {
	// Get the static cell that maybe holds the address of the team
	AiBaseTeam **teamToRef = TeamRefForNum( teamNum );
	// If there is no team address in this memory cell
	if( !*teamToRef ) {
		return;
	}

	// Destruct the team
	( *teamToRef )->~AiBaseTeam();
	// Free team memory
	Q_free( *teamToRef );
	// Nullify the static memory cell holding no longer valid address
	*teamToRef = nullptr;
}

AiBaseTeam *AiBaseTeam::InstantiateTeam( int teamNum ) {
	// Delegate construction to AiSquadBasedTeam
	if( GS_TeamBasedGametype( *ggs ) && !GS_IndividualGametype( *ggs ) ) {
		return AiSquadBasedTeam::InstantiateTeam( teamNum );
	}

	void *mem = Q_malloc( sizeof( AiBaseTeam ) );
	return new(mem)AiBaseTeam( teamNum );
}
