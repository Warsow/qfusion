#include "manager.h"
#include "planning/planner.h"
#include "teamplay/baseteam.h"
#include "evolutionmanager.h"
#include "bot.h"
#include "combat/tacticalspotsregistry.h"
#include <common/helpers/links.h>
#include <common/types/stringview.h>
#include <common/helpers/algorithm.h>

using wsw::operator""_asView;
using wsw::operator""_asHView;

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

void AiManager::LinkBot( Bot *bot ) {
	wsw::link( bot, &botHandlesHead, Bot::AI_LINKS );
}

void AiManager::UnlinkBot( Bot *bot ) {
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

static constexpr unsigned kNumBotBreeds    = 5;
static constexpr unsigned kNumBotsPerBreed = 5;

static const struct { wsw::StringView model; wsw::StringView names[kNumBotsPerBreed]; } kBotBreedSpecs[kNumBotBreeds] {
	{ "bigvic"_asView, { "Viciious"_asView, "Sid"_asView, "Pervert"_asView, "Sick"_asView, "Punk"_asView } },
	{ "monada"_asView, { "Black Sis"_asView, "Monada"_asView, "Afrodita"_asView, "Goddess"_asView, "Athena"_asView } },
	{ "silverclaw"_asView, { "Silver"_asView, "Cathy"_asView, "MishiMishi"_asView, "Lobita"_asView, "SisterClaw"_asView } },
	{ "padpork"_asView, { "Padpork"_asView, "Jason"_asView, "Lord Hog"_asView, "Porkalator"_asView, "Babe"_asView } },
	{ "bobot"_asView, { "YYZ2112"_asView, "01011001"_asView, "Sector"_asView, "%APPDATA%"_asView, "P.E.#1"_asView } },
};

void AiManager::CreateUserInfo( char *buffer, size_t bufferSize ) {
	unsigned nameCounters[kNumBotBreeds][kNumBotsPerBreed];
	std::memset( nameCounters, 0, sizeof( nameCounters ) );
	wsw::StaticVector<std::pair<unsigned, unsigned>, kNumBotBreeds> breedCounters;
	for( unsigned i = 0; i < kNumBotBreeds; ++i ) {
		breedCounters.emplace_back( { i, 0 } );
	}

	for( int i = 1; PLAYERNUM( ( game.edicts + i ) ) < ggs->maxclients; i++ ) {
		const edict_t *ent = game.edicts + i;
		// Consider regular clients in disguise as well
		if( ent->r.inuse && G_GetClientState( PLAYERNUM( ent ) ) >= CS_SPAWNED ) {
			const wsw::StringView model = ent->r.client->m_userInfo.getOrThrow( "model"_asHView );
			const auto breedIt = wsw::find_if( std::begin( kBotBreedSpecs ), std::end( kBotBreedSpecs ), [=]( const auto &b ) {
				return model.equalsIgnoreCase( b.model );
			});
			// Some models are unavailable for bots
			if( breedIt != std::end( kBotBreedSpecs ) ) {
				const auto breedIndex = breedIt - std::begin( kBotBreedSpecs );
				breedCounters[breedIndex].second++;
				for( auto nameIt = std::begin( breedIt->names ); nameIt != std::end( breedIt->names ); ++nameIt ) {
					if( ent->r.client->colorlessNetname.asView().startsWith( *nameIt, wsw::IgnoreCase ) ) {
						nameCounters[breedIndex][nameIt - std::begin( breedIt->names )]++;
					}
				}
			}
		}
	}

	// Sort breeds so breeds with the lowest number of bots come first
	// TODO: Just use a heap?
	wsw::sortByField( breedCounters.begin(), breedCounters.end(), &std::pair<unsigned, unsigned>::second );
	// Take breeds with the lowest number of bots.
	// This should have an absolute priority so we quickly get a variety of creatures in-game.
	unsigned numBreedsForChoosing = 1;
	while( numBreedsForChoosing < breedCounters.size() && breedCounters[0].second == breedCounters[numBreedsForChoosing].second ) {
		numBreedsForChoosing++;
	}

	wsw::StaticVector<std::pair<std::pair<uint16_t, uint16_t>, unsigned>, kNumBotBreeds * kNumBotsPerBreed> namesWithCounters;
	for( unsigned breedNum = 0; breedNum < numBreedsForChoosing; ++breedNum ) {
		// The original breed index (not the index of the sorted entry)
		const unsigned breedIndex = breedCounters[breedNum].first;
		for( unsigned nameIndex = 0; nameIndex < kNumBotsPerBreed; ++nameIndex ) {
			namesWithCounters.push_back( { { breedIndex, nameIndex }, nameCounters[breedIndex][nameIndex] } );
		}
	}

	// Sort names so names with the lowest number of bots come first
	wsw::sortByField( namesWithCounters.begin(), namesWithCounters.end(), &std::pair<std::pair<uint16_t, uint16_t>, unsigned>::second );
	unsigned numNamesForChoosing = 1;
	while( numNamesForChoosing < namesWithCounters.size() && namesWithCounters[0].second == namesWithCounters[numNamesForChoosing].second ) {
		numNamesForChoosing++;
	}

	// TODO: Use game-global generator
	static wsw::RandomGenerator rg( time( nullptr ) );
	const auto [chosenBreedIndex, chosenNameIndex] = namesWithCounters[rg.nextBounded( numNamesForChoosing )].first;
	assert( chosenBreedIndex < kNumBotBreeds );
	assert( chosenNameIndex < kNumBotsPerBreed );

	memset( buffer, 0, bufferSize );

	Info_SetValueForKey( buffer, "name", kBotBreedSpecs[chosenBreedIndex].names[chosenNameIndex].data() );
	Info_SetValueForKey( buffer, "model", kBotBreedSpecs[chosenBreedIndex].model.data() );
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
	int entNum = SVC_FakeConnect( userInfo, fakeSocketType, fakeIP );
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

	aiWarning() << "Can't spawn bots without a valid navigation file";
	if( g_numbots->integer ) {
		Cvar_Set( "g_numbots", "0" );
	}

	return false;
}

float AiManager::MakeSkillForNewBot( const Client *client ) const {
	float skillLevel;

	// Always use the same skill for bots that are subject of evolution
	if( v_evolution.get() ) {
		skillLevel = 0.75f;
	} else {
		skillLevel = ( Cvar_Value( "sv_skilllevel" ) + random() ) / 3.0f;
		// Let the skill be not less than 10, so we can have nice-looking
		// two-digit skills (not talking about formatting here)
		Q_clamp( skillLevel, 0.10f, 0.99f );
	}

	aiNotice() << client->netname << "skill" << (int)( skillLevel * 100 );
	return skillLevel;
}

void AiManager::SetupBotForEntity( edict_t *ent ) {
	if( ent->bot ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Entity AI has been already initialized\n" );
	}

	if( !( ent->r.svflags & SVF_FAKECLIENT ) ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Only fake clients are supported\n" );
	}

	auto *mem = (uint8_t *)Q_malloc( sizeof( Bot ) );
	ent->bot = new( mem )Bot( ent, MakeSkillForNewBot( ent->r.client ) );

	LinkBot( ent->bot );

	ent->think = nullptr;
	ent->nextThink = level.time + 1;
	ent->classname = "bot";
	ent->die = player_die;
}

void AiManager::TryJoiningTeam( edict_t *ent, const char *teamName ) {
	int team = GS_Teams_TeamFromName( ggs, teamName );
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
	for( edict_t *ent = game.edicts + ggs->maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( ent->r.client->netname.equalsIgnoreCase( name ) ) {
			G_DropClient( ent, ReconnectBehaviour::DontReconnect );
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
	for( edict_t *ent = game.edicts + ggs->maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( !ent->r.inuse || !ent->bot ) {
			continue;
		}

		G_DropClient( ent, ReconnectBehaviour::DontReconnect );
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

void AiManager::Update() {
	globalCpuQuota.Update( botHandlesHead );
	thinkQuota[level.framenum % 4].Update( botHandlesHead );

	if( !GS_TeamBasedGametype( *ggs ) ) {
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
		wsw::push_heap( bestAreasHeap.begin(), bestAreasHeap.end() );

		// bestAreasHeap size should be always less than its capacity:
		// 1) to ensure that there is a free room for next area;
		// 2) to ensure that hubAreas capacity will not be exceeded.
		if( bestAreasHeap.full() ) {
			wsw::pop_heap( bestAreasHeap.begin(), bestAreasHeap.end() );
			bestAreasHeap.pop_back();
		}
	}

	wsw::sortByFieldDescending( bestAreasHeap.begin(), bestAreasHeap.end(), &AreaAndScore::score );
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
		// Note: We used to check reachability numbers which are not defined for route from area to the same area,
		// therefore the code used to produce wrong results for hub areas themselves.
		// Now we check the travel time.
		if( routeCache->FindRoute( hubAreas[i], targetArea, Bot::ALLOWED_TRAVEL_FLAGS ) ) {
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
	return !G_ISGHOSTING( bot->self );
}

bool AiManager::ThinkQuota::Fits( const Bot *bot ) const {
	// Only bots that have the same frame affinity fit
	return bot->m_frameAffinityOffset == affinityOffset;
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