#include "manager.h"
#include "planning/planner.h"
#include "teamplay/baseteam.h"
#include "evolutionmanager.h"
#include "bot.h"
#include "combat/tacticalspotsregistry.h"
#include "../../qcommon/links.h"

#include <algorithm>

// Class static variable declaration
AiManager *AiManager::instance = nullptr;

// Actual instance location in memory
static wsw::StaticVector<AiManager, 1> instanceHolder;

void AiManager::Init( const char *gametype, const char *mapname ) {
	if( instance ) {
		AI_FailWith( "AiManager::Init()", "An instance is already present\n" );
	}

	AiBaseTeam::Init();

	new( instanceHolder.unsafe_grow_back() )AiManager( gametype, mapname );
	instance = &instanceHolder.front();

	BotEvolutionManager::Init();
}

void AiManager::Shutdown() {
	BotEvolutionManager::Shutdown();

	AiBaseTeam::Shutdown();

	if( instance ) {
		instance = nullptr;
	}

	instanceHolder.clear();
}

AiManager::AiManager( const char *gametype, const char *mapname ) {
	std::fill_n( teams, MAX_CLIENTS, TEAM_SPECTATOR );
}

void AiManager::notifyOfNavEntitySignaledAsReached( const NavEntity *navEntity ) {
	assert( navEntity );
	// find all bots which have this node as goal and tell them their goal is reached
	for( Bot *bot = botHandlesHead; bot; bot = bot->NextInAIList() ) {
		bot->notifyOfNavEntitySignaledAsReached( navEntity );
	}
}

void AiManager::notifyOfNavEntityRemoved( const NavEntity *navEntity ) {
	assert( navEntity );
	// find all bots which have this node as goal and tell them their goal is reached
	for( Bot *bot = botHandlesHead; bot; bot = bot->NextInAIList() ) {
		bot->notifyOfNavEntityRemoved( navEntity );
	}
}

void AiManager::OnBotJoinedTeam( edict_t *ent, int team ) {
	const int entNum = ENTNUM( ent );
	const int oldTeam = teams[entNum];
	if( oldTeam != team ) {
		if( oldTeam != TEAM_SPECTATOR ) {
			AiBaseTeam::GetTeamForNum( oldTeam )->RemoveBot( ent->bot );
		}
		if( team != TEAM_SPECTATOR ) {
			AiBaseTeam::GetTeamForNum( team )->AddBot( ent->bot );
		}
		teams[entNum] = team;
	}
}

void AiManager::OnBotDropped( edict_t *ent ) {
	const int entNum = ENTNUM( ent );
	const int oldTeam = teams[entNum];
	if( oldTeam != TEAM_SPECTATOR ) {
		AiBaseTeam::GetTeamForNum( oldTeam )->RemoveBot( ent->bot );
	}
	teams[entNum] = TEAM_SPECTATOR;
}

void AiManager::LinkAi( Ai *ai ) {
	if( Bot *bot = dynamic_cast<Bot *>( ai ) ) {
		wsw::link( bot, &botHandlesHead, Bot::AI_LINKS );
	}
}

void AiManager::UnlinkAi( Ai *ai ) {
	Bot *bot = dynamic_cast<Bot *>( ai );
	if( !bot ) {
		return;
	}

	wsw::unlink( bot, &botHandlesHead, Bot::AI_LINKS );

	// All links related to the unlinked AI become invalid.
	// Reset CPU quota cycling state to prevent use-after-free.
	globalCpuQuota.OnRemoved( bot );
	for( ThinkQuota &quota: thinkQuota ) {
		quota.OnRemoved( bot );
	}
}

void AiManager::RegisterEvent( const edict_t *ent, int event, int parm ) {
	for( Bot *bot = ent->bot; bot; bot = bot->NextInAIList() ) {
		bot->RegisterEvent( ent, event, parm );
	}
}

static struct { const char *name; const char *model; } botCharacters[] = {
	{ "Viciious",   "bigvic" },
	{ "Sid",        "bigvic" },
	{ "Pervert",    "bigvic" },
	{ "Sick",       "bigvic" },
	{ "Punk",       "bigvic" },

	{ "Black Sis",  "monada" },
	{ "Monada",     "monada" },
	{ "Afrodita",   "monada" },
	{ "Goddess",    "monada" },
	{ "Athena",     "monada" },

	{ "Silver",     "silverclas" },
	{ "Cathy",      "silverclaw" },
	{ "MishiMishi", "silverclaw" },
	{ "Lobita",     "silverclaw" },
	{ "SisterClaw", "silverclaw" },

	{ "Padpork",    "padpork" },
	{ "Jason",      "padpork" },
	{ "Lord Hog",   "padpork" },
	{ "Porkalator", "padpork" },
	{ "Babe",       "padpork" },

	{ "YYZ2112",    "bobot" },
	{ "01011001",   "bobot" },
	{ "Sector",     "bobot" },
	{ "%APPDATA%",  "bobot" },
	{ "P.E.#1",     "bobot" },
};

static constexpr int BOT_CHARACTERS_COUNT = sizeof( botCharacters ) / sizeof( botCharacters[0] );

void AiManager::CreateUserInfo( char *buffer, size_t bufferSize ) {
	// Try to avoid bad distribution, otherwise some bots are selected too often. Weights are prime numbers
	int characterIndex = ( (int)( 3 * random() + 11 * random() +  97 * random() + 997 * random() ) ) % BOT_CHARACTERS_COUNT;

	memset( buffer, 0, bufferSize );

	Info_SetValueForKey( buffer, "name", botCharacters[characterIndex].name );
	Info_SetValueForKey( buffer, "model", botCharacters[characterIndex].model );
	Info_SetValueForKey( buffer, "skin", "default" );
	Info_SetValueForKey( buffer, "hand", va( "%i", (int)( random() * 2.5 ) ) );
	const char *color = va( "%i %i %i", (uint8_t)( random() * 255 ), (uint8_t)( random() * 255 ), (uint8_t)( random() * 255 ) );
	Info_SetValueForKey( buffer, "color", color );
}

edict_t * AiManager::ConnectFakeClient() {
	char userInfo[MAX_INFO_STRING];
	static char fakeSocketType[] = "loopback";
	static char fakeIP[] = "127.0.0.1";
	CreateUserInfo( userInfo, sizeof( userInfo ) );
	int entNum = trap_FakeClientConnect( userInfo, fakeSocketType, fakeIP );
	if( entNum >= 1 ) {
		return game.edicts + entNum;
	}

	G_Printf( "AiManager::ConnectFakeClient(): Can't spawn a fake client\n" );
	return nullptr;
}

void AiManager::RespawnBot( edict_t *self ) {
	if( !self->bot ) {
		return;
	}

	BotEvolutionManager::Instance()->OnBotRespawned( self );

	self->bot->OnRespawn();
}

static void BOT_JoinPlayers( edict_t *self ) {
	G_Teams_JoinAnyTeam( self, true );
	self->think = nullptr;
}

bool AiManager::CheckCanSpawnBots() {
	if( level.spawnedTimeStamp + 5000 > game.realtime || !level.canSpawnEntities ) {
		return false;
	}

	if( AiAasWorld::instance()->isLoaded() && TacticalSpotsRegistry::Instance()->IsLoaded() ) {
		return true;
	}

	Com_Printf( "AI: Can't spawn bots without a valid navigation file\n" );
	if( g_numbots->integer ) {
		trap_Cvar_Set( "g_numbots", "0" );
	}

	return false;
}

float AiManager::MakeSkillForNewBot( const Client *client ) const {
	float skillLevel;

	// Always use the same skill for bots that are subject of evolution
	if( ai_evolution->integer ) {
		skillLevel = 0.75f;
	} else {
		skillLevel = ( trap_Cvar_Value( "sv_skilllevel" ) + random() ) / 3.0f;
		// Let the skill be not less than 10, so we can have nice-looking
		// two-digit skills (not talking about formatting here)
		Q_clamp( skillLevel, 0.10f, 0.99f );
	}

	G_Printf( "%s skill %i\n", client->netname.data(), (int)( skillLevel * 100 ) );
	return skillLevel;
}

void AiManager::SetupBotForEntity( edict_t *ent ) {
	if( ent->ai ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Entity AI has been already initialized\n" );
	}

	if( !( ent->r.svflags & SVF_FAKECLIENT ) ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Only fake clients are supported\n" );
	}

	auto *mem = (uint8_t *)Q_malloc( sizeof( Bot ) );
	ent->bot = new( mem )Bot( ent, MakeSkillForNewBot( ent->r.client ) );
	ent->ai = ent->bot;

	LinkAi( ent->bot );

	ent->think = nullptr;
	ent->nextThink = level.time + 1;
	ent->classname = "bot";
	ent->die = player_die;
}

void AiManager::TryJoiningTeam( edict_t *ent, const char *teamName ) {
	int team = GS_Teams_TeamFromName( teamName );
	if( team >= TEAM_PLAYERS && team <= TEAM_BETA ) {
		// Join specified team immediately
		G_Teams_JoinTeam( ent, team );
		return;
	}

	// Stay as spectator, give random time for joining
	ent->think = BOT_JoinPlayers;
	ent->nextThink = level.time + 500 + (unsigned)( random() * 2000 );
}

void AiManager::SpawnBot( const char *teamName ) {
	if( !CheckCanSpawnBots() ) {
		return;
	}

	edict_t *const ent = ConnectFakeClient();
	if( !ent ) {
		return;
	}

	SetupBotForEntity( ent );
	RespawnBot( ent );
	TryJoiningTeam( ent, teamName );
	SetupBotGoalsAndActions( ent );
	BotEvolutionManager::Instance()->OnBotConnected( ent );

	game.numBots++;
}

void AiManager::RemoveBot( const wsw::StringView &name ) {
	// Do not iterate over the linked list of bots since it is implicitly modified by these calls
	for( edict_t *ent = game.edicts + gs.maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( ent->r.client->netname.equalsIgnoreCase( name ) ) {
			trap_DropClient( ent, DROP_TYPE_GENERAL, nullptr );
			OnBotDropped( ent );
			G_FreeAI( ent );
			game.numBots--;
			return;
		}
	}
	G_Printf( "AiManager::RemoveBot(): A bot `%s` has not been found\n", name.data() );
}

void AiManager::AfterLevelScriptShutdown() {
	// Do not iterate over the linked list of bots since it is implicitly modified by these calls
	for( edict_t *ent = game.edicts + gs.maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( !ent->r.inuse || !ent->ai ) {
			continue;
		}

		trap_DropClient( ent, DROP_TYPE_GENERAL, nullptr );
		OnBotDropped( ent );
		G_FreeAI( ent );
		game.numBots--;
	}
}

void AiManager::BeforeLevelScriptShutdown() {
	BotEvolutionManager::Instance()->SaveEvolutionResults();
}

void AiManager::SetupBotGoalsAndActions( edict_t *ent ) {
	ent->bot->planningModule.RegisterBuiltinGoalsAndActions();
}

void AiManager::Frame() {
	globalCpuQuota.Update( botHandlesHead );
	thinkQuota[level.framenum % 4].Update( botHandlesHead );

	if( !GS_TeamBasedGametype() ) {
		AiBaseTeam::GetTeamForNum( TEAM_PLAYERS )->Update();
		return;
	}

	for( int team = TEAM_ALPHA; team < GS_MAX_TEAMS; ++team ) {
		AiBaseTeam::GetTeamForNum( team )->Update();
	}
}

void AiManager::FindHubAreas() {
	const auto *aasWorld = AiAasWorld::instance();
	if( !aasWorld->isLoaded() ) {
		return;
	}

	const auto aasAreas = aasWorld->getAreas();
	const auto aasReaches = aasWorld->getReaches();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	wsw::StaticVector<AreaAndScore, sizeof( hubAreas ) / sizeof( *hubAreas )> bestAreasHeap;
	for( size_t i = 1; i < aasAreaSettings.size(); ++i ) {
		const auto &areaSettings = aasAreaSettings[i];
		if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
			continue;
		}
		if( areaSettings.areaflags & AREA_DISABLED ) {
			continue;
		}
		if( areaSettings.contents & ( AREACONTENTS_DONOTENTER | AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_WATER ) ) {
			continue;
		}

		// Reject degenerate areas, pass only relatively large areas
		const auto &area = aasAreas[i];
		if( area.maxs[0] - area.mins[0] < 56.0f ) {
			continue;
		}
		if( area.maxs[1] - area.mins[1] < 56.0f ) {
			continue;
		}

		// Count as useful only several kinds of reachabilities
		int usefulReachCount = 0;
		int reachNum = areaSettings.firstreachablearea;
		int lastReachNum = areaSettings.firstreachablearea + areaSettings.numreachableareas - 1;
		while( reachNum <= lastReachNum ) {
			const auto &reach = aasReaches[reachNum];
			if( reach.traveltype == TRAVEL_WALK || reach.traveltype == TRAVEL_WALKOFFLEDGE ) {
				usefulReachCount++;
			}
			++reachNum;
		}

		// Reject early to avoid more expensive call to push_heap()
		if( !usefulReachCount ) {
			continue;
		}

		bestAreasHeap.push_back( AreaAndScore( (int)i, (float)usefulReachCount ) );
		std::push_heap( bestAreasHeap.begin(), bestAreasHeap.end() );

		// bestAreasHeap size should be always less than its capacity:
		// 1) to ensure that there is a free room for next area;
		// 2) to ensure that hubAreas capacity will not be exceeded.
		if( bestAreasHeap.full() ) {
			std::pop_heap( bestAreasHeap.begin(), bestAreasHeap.end() );
			bestAreasHeap.pop_back();
		}
	}

	std::sort( bestAreasHeap.begin(), bestAreasHeap.end() );

	for( unsigned i = 0; i < bestAreasHeap.size(); ++i ) {
		this->hubAreas[i] = bestAreasHeap[i].areaNum;
	}

	this->numHubAreas = (int)bestAreasHeap.size();
}

bool AiManager::IsAreaReachableFromHubAreas( int targetArea, float *score ) const {
	if( !targetArea ) {
		return false;
	}

	if( !this->numHubAreas ) {
		const_cast<AiManager *>( this )->FindHubAreas();
	}

	const auto *routeCache = AiAasRouteCache::Shared();
	int numReach = 0;
	float scoreSum = 0.0f;
	for( int i = 0; i < numHubAreas; ++i ) {
		if( routeCache->ReachabilityToGoalArea( hubAreas[i], targetArea, Bot::ALLOWED_TRAVEL_FLAGS ) ) {
			numReach++;
			// Give first (and best) areas greater score
			scoreSum += ( numHubAreas - i ) / (float)numHubAreas;
			// That's enough, stop wasting CPU cycles
			if( numReach == 4 ) {
				if( score ) {
					*score = scoreSum;
				}
				return true;
			}
		}
	}

	if ( score ) {
		*score = scoreSum;
	}

	return numReach > 0;
}

bool AiManager::GlobalQuota::Fits( const Bot *bot ) const {
	return !bot->IsGhosting();
}

bool AiManager::ThinkQuota::Fits( const Bot *bot ) const {
	// Only bots that have the same frame affinity fit
	return bot->frameAffinityOffset == affinityOffset;
}

void AiManager::Quota::Update( const Bot *aiHandlesHead ) {
	if( !owner ) {
		owner = aiHandlesHead;
		while( owner && !Fits( owner ) ) {
			owner = owner->NextInAIList();
		}
		return;
	}

	const auto *const oldOwner = owner;
	// Start from the next AI in list
	owner = owner->NextInAIList();
	// Scan all bots that are after the current owner in the list
	while( owner ) {
		// Stop on the first bot that fits this
		if( Fits( owner ) ) {
			break;
		}
		owner = owner->NextInAIList();
	}

	// If the scan has not reached the list end
	if( owner ) {
		return;
	}

	// Rewind to the list head
	owner = aiHandlesHead;

	// Scan all bots that is before the current owner in the list
	// Keep the current owner if there is no in-game bots before
	while( owner && owner != oldOwner ) {
		// Stop on the first bot that fits this
		if( Fits( owner ) ) {
			break;
		}
		owner = owner->NextInAIList();
	}

	// If the loop execution has not been interrupted by break,
	// quota owner remains the same as before this call.
	// This means a bot always gets a quota if there is no other active bots in game.
}

bool AiManager::TryGetExpensiveComputationQuota( const Bot *bot ) {
	return globalCpuQuota.TryAcquire( bot );
}

bool AiManager::TryGetExpensiveThinkCallQuota( const Bot *bot ) {
	return thinkQuota[level.framenum % 4].TryAcquire( bot );
}

bool AiManager::Quota::TryAcquire( const Bot *bot ) {
	if( bot != owner ) {
		return false;
	}

	auto levelTime = level.time;
	// Allow expensive computations only once per frame
	if( givenAt == levelTime ) {
		return false;
	}

	// Mark it
	givenAt = levelTime;
	return true;
}