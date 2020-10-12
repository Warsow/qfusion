#include "ai_manager.h"
#include "planning/Planner.h"
#include "teamplay/BaseTeam.h"
#include "bot_evolution_manager.h"
#include "bot.h"
#include "combat/TacticalSpotsRegistry.h"
#include "../../qcommon/links.h"

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

template <typename T, unsigned N>
T* AiManager::StringValueMap<T, N>::Get( const char *key ) {
	for( std::pair<const char *, T> &pair: keyValuePairs ) {
		if( !Q_stricmp( key, pair.first ) ) {
			return &pair.second;
		}
	}

	return nullptr;
}

template <typename T, unsigned N>
bool AiManager::StringValueMap<T, N>::Insert( const char *key, T &&value ) {
	for( std::pair<const char *, T> &pair: keyValuePairs )
		if( !Q_stricmp( key, pair.first ) ) {
			return false;
		}

	// Caller logic should not allow this. The following code is only an assertion for debug builds.
#ifdef _DEBUG
	if( IsFull() ) {
		AI_FailWith( "AiManager::StringValueMap::Insert()", "Capacity has been exceeded\n" );
	}
#endif

	keyValuePairs.emplace_back( std::make_pair( key, std::move( value ) ) );
	return true;
}

#define REGISTER_BUILTIN_GOAL( goal ) this->RegisterBuiltinGoal(#goal )
#define REGISTER_BUILTIN_ACTION( action ) this->RegisterBuiltinAction(#action )

AiManager::AiManager( const char *gametype, const char *mapname ) {
	std::fill_n( teams, MAX_CLIENTS, TEAM_SPECTATOR );

	REGISTER_BUILTIN_GOAL( GrabItemGoal );
	REGISTER_BUILTIN_GOAL( KillEnemyGoal );
	REGISTER_BUILTIN_GOAL( RunAwayGoal );
	REGISTER_BUILTIN_GOAL( ReactToHazardGoal );
	REGISTER_BUILTIN_GOAL( ReactToThreatGoal );
	REGISTER_BUILTIN_GOAL( ReactToEnemyLostGoal );
	REGISTER_BUILTIN_GOAL( AttackOutOfDespairGoal );
	REGISTER_BUILTIN_GOAL( RoamGoal );

	// Do not clear built-in goals later
	registeredGoals.MarkClearLimit();

	REGISTER_BUILTIN_ACTION( RunToNavEntityAction );
	REGISTER_BUILTIN_ACTION( PickupNavEntityAction );
	REGISTER_BUILTIN_ACTION( WaitForNavEntityAction );

	REGISTER_BUILTIN_ACTION( KillEnemyAction );
	REGISTER_BUILTIN_ACTION( AdvanceToGoodPositionAction );
	REGISTER_BUILTIN_ACTION( RetreatToGoodPositionAction );
	REGISTER_BUILTIN_ACTION( GotoAvailableGoodPositionAction );
	REGISTER_BUILTIN_ACTION( AttackFromCurrentPositionAction );
	REGISTER_BUILTIN_ACTION( AttackAdvancingToTargetAction );

	REGISTER_BUILTIN_ACTION( FleeToSpotAction );
	REGISTER_BUILTIN_ACTION( StartGotoCoverAction );
	REGISTER_BUILTIN_ACTION( TakeCoverAction );
	REGISTER_BUILTIN_ACTION( StartGotoRunAwayTeleportAction );
	REGISTER_BUILTIN_ACTION( DoRunAwayViaTeleportAction );
	REGISTER_BUILTIN_ACTION( StartGotoRunAwayJumppadAction );
	REGISTER_BUILTIN_ACTION( DoRunAwayViaJumppadAction );
	REGISTER_BUILTIN_ACTION( StartGotoRunAwayElevatorAction );
	REGISTER_BUILTIN_ACTION( DoRunAwayViaElevatorAction );
	REGISTER_BUILTIN_ACTION( StopRunningAwayAction );

	REGISTER_BUILTIN_ACTION( DodgeToSpotAction );

	REGISTER_BUILTIN_ACTION( TurnToThreatOriginAction );

	REGISTER_BUILTIN_ACTION( TurnToLostEnemyAction );
	REGISTER_BUILTIN_ACTION( StartLostEnemyPursuitAction );
	REGISTER_BUILTIN_ACTION( StopLostEnemyPursuitAction );

	// Do not clear builtin actions later
	registeredActions.MarkClearLimit();
}

void AiManager::NavEntityReachedBy( const NavEntity *navEntity, const Ai *grabber ) {
	if( !navEntity ) {
		return;
	}

	// find all bots which have this node as goal and tell them their goal is reached
	for( auto *aiHandle = aiHandlesListHead; aiHandle; aiHandle = aiHandle->Next() ) {
		aiHandle->aiRef->OnNavEntityReachedBy( navEntity, grabber );
	}
}

void AiManager::NavEntityReachedSignal( const edict_t *ent ) {
	if( !aiHandlesListHead ) {
		return;
	}

	// find all bots which have this node as goal and tell them their goal is reached
	for( auto *aiHandle = aiHandlesListHead; aiHandle; aiHandle = aiHandle->Next() ) {
		aiHandle->aiRef->OnEntityReachedSignal( ent );
	}
}

void AiManager::OnBotJoinedTeam( edict_t *ent, int team ) {
	const int entNum = ENTNUM( ent );
	const int oldTeam = teams[entNum];
	if( oldTeam != team ) {
		if( oldTeam != TEAM_SPECTATOR ) {
			AiBaseTeam::GetTeamForNum( oldTeam )->RemoveBot( ent->ai->botRef );
		}
		if( team != TEAM_SPECTATOR ) {
			AiBaseTeam::GetTeamForNum( team )->AddBot( ent->ai->botRef );
		}
		teams[entNum] = team;
	}
}

void AiManager::OnBotDropped( edict_t *ent ) {
	const int entNum = ENTNUM( ent );
	const int oldTeam = teams[entNum];
	if( oldTeam != TEAM_SPECTATOR ) {
		AiBaseTeam::GetTeamForNum( oldTeam )->RemoveBot( ent->ai->botRef );
	}
	teams[entNum] = TEAM_SPECTATOR;
}

void AiManager::LinkAi( ai_handle_t *aiHandle ) {
	wsw::link( aiHandle, &aiHandlesListHead, 0 );
}

void AiManager::UnlinkAi( ai_handle_t *aiHandle ) {
	wsw::unlink( aiHandle, &aiHandlesListHead, 0 );

	// All links related to the unlinked AI become invalid.
	// Reset CPU quota cycling state to prevent use-after-free.
	globalCpuQuota.OnRemoved( aiHandle );
	for( int i = 0; i < 4; ++i ) {
		thinkQuota->OnRemoved( aiHandle );
	}
}

void AiManager::RegisterEvent( const edict_t *ent, int event, int parm ) {
	for( auto *aiHandle = aiHandlesListHead; aiHandle; aiHandle = aiHandle->Next() ) {
		if( auto *bot = aiHandle->botRef ) {
			bot->RegisterEvent( ent, event, parm );
		}
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

	{ "Silver",     "silverclaw" },
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
	if( AI_GetType( self->ai ) != AI_ISBOT ) {
		return;
	}

	BotEvolutionManager::Instance()->OnBotRespawned( self );

	self->ai->botRef->OnRespawn();
}

static void BOT_JoinPlayers( edict_t *self ) {
	G_Teams_JoinAnyTeam( self, true );
	self->think = nullptr;
}

bool AiManager::CheckCanSpawnBots() {
	if( level.spawnedTimeStamp + 5000 > game.realtime || !level.canSpawnEntities ) {
		return false;
	}

	if( AiAasWorld::Instance()->IsLoaded() && TacticalSpotsRegistry::Instance()->IsLoaded() ) {
		return true;
	}

	Com_Printf( "AI: Can't spawn bots without a valid navigation file\n" );
	if( g_numbots->integer ) {
		trap_Cvar_Set( "g_numbots", "0" );
	}

	return false;
}

float AiManager::MakeSkillForNewBot( const gclient_t *client ) const {
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

	G_Printf( "%s skill %i\n", client->netname, (int)( skillLevel * 100 ) );
	return skillLevel;
}

void AiManager::SetupBotForEntity( edict_t *ent ) {
	if( ent->ai ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Entity AI has been already initialized\n" );
	}

	if( !( ent->r.svflags & SVF_FAKECLIENT ) ) {
		AI_FailWith( "AiManager::SetupBotForEntity()", "Only fake clients are supported\n" );
	}

	size_t memSize = sizeof( ai_handle_t ) + sizeof( Bot );
	size_t alignmentBytes = 0;
	if( sizeof( ai_handle_t ) % 16 ) {
		alignmentBytes = 16 - sizeof( ai_handle_t ) % 16;
	}
	memSize += alignmentBytes;

	auto *handleMem = (uint8_t *)Q_malloc( memSize );
	ent->ai = (ai_handle_t *)handleMem;
	ent->ai->type = AI_ISBOT;

	auto *botMem = handleMem + sizeof( ai_handle_t ) + alignmentBytes;
	ent->ai->botRef = new( botMem )Bot( ent, MakeSkillForNewBot( ent->r.client ) );
	ent->ai->aiRef = ent->ai->botRef;

	LinkAi( ent->ai );

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

void AiManager::RemoveBot( const char *name ) {
	// Do not iterate over the linked list of bots since it is implicitly modified by these calls
	for( edict_t *ent = game.edicts + gs.maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( !Q_stricmp( ent->r.client->netname, name ) ) {
			trap_DropClient( ent, DROP_TYPE_GENERAL, nullptr );
			OnBotDropped( ent );
			G_FreeAI( ent );
			game.numBots--;
			return;
		}
	}
	G_Printf( "AiManager::RemoveBot(): A bot `%s` has not been found\n", name );
}

void AiManager::AfterLevelScriptShutdown() {
	// Do not iterate over the linked list of bots since it is implicitly modified by these calls
	for( edict_t *ent = game.edicts + gs.maxclients; PLAYERNUM( ent ) >= 0; ent-- ) {
		if( !ent->r.inuse || AI_GetType( ent->ai ) != AI_ISBOT ) {
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

// We have to sanitize all input values since these methods are exported to scripts

void AiManager::RegisterScriptGoal( const char *goalName, void *factoryObject, unsigned updatePeriod ) {
	constexpr const char *tag = "AiManager::RegisterScriptGoal()";

	if( registeredGoals.IsFull() ) {
		G_Printf( S_COLOR_RED "%s: Can't register the %s goal (too many goals)\n", tag, goalName );
		return;
	}

	GoalProps goalProps( goalName, factoryObject, updatePeriod );
	// Ensure map key valid lifetime, use GoalProps::name
	if( !registeredGoals.Insert( goalProps.name, std::move( goalProps ) ) ) {
		G_Printf( S_COLOR_RED "%s: Goal %s is already registered\n", tag, goalName );
	}
}

void AiManager::RegisterScriptAction( const char *actionName, void *factoryObject ) {
	constexpr const char *tag = "AiManager::RegisterScriptAction()";

	if( registeredActions.IsFull() ) {
		G_Printf( S_COLOR_RED "%s: can't register the %s action (too many actions)\n", tag, actionName );
		return;
	}

	ActionProps actionProps( actionName, factoryObject );
	// Ensure map key valid lifetime, user ActionProps::name
	if( !registeredActions.Insert( actionProps.name, std::move( actionProps ) ) ) {
		G_Printf( S_COLOR_RED "%s: action %s is already registered\n", tag, actionName );
	}
}

void AiManager::AddApplicableAction( const char *goalName, const char *actionName ) {
	constexpr const char *tag = "AiManager::AddApplicableAction";

	ActionProps *actionProps = registeredActions.Get( actionName );
	if( !actionProps ) {
		G_Printf( S_COLOR_RED "%s: action %s has not been registered\n", tag, actionName );
		return;
	}

	GoalProps *goalProps = registeredGoals.Get( goalName );
	if( !goalProps ) {
		G_Printf( S_COLOR_RED "%s: goal %s has not been registered\n", tag, goalName );
		return;
	}

	if( !actionProps->factoryObject && !goalProps->factoryObject ) {
		const char *format = S_COLOR_RED
							 "%s: both goal %s and action %s are builtin "
							 "(builtin action/goal relations are hardcoded for performance)\n";
		G_Printf( format, tag, goalName, actionName );
		return;
	}

	if( goalProps->numApplicableActions == MAX_ACTIONS ) {
		G_Printf( S_COLOR_RED "%s: too many actions have already been registered\n", tag );
		return;
	}

	// Ensure that applicable action name has valid lifetime, use ActionProps::name
	goalProps->applicableActions[goalProps->numApplicableActions++] = actionProps->name;
}

void AiManager::RegisterBuiltinGoal( const char *goalName ) {
	constexpr const char *tag = "AiManager::RegisterBuiltinGoal()";

	if( registeredGoals.IsFull() ) {
		AI_FailWith( tag, "Too many registered goals" );
	}

	GoalProps goalProps( goalName, nullptr, 0 );
	// Ensure map key valid lifetime, user GoalProps::name
	if( !registeredGoals.Insert( goalProps.name, std::move( goalProps ) ) ) {
		AI_FailWith( tag, "The goal %s is already registered", goalName );
	}
}

void AiManager::RegisterBuiltinAction( const char *actionName ) {
	constexpr const char *tag = "AiManager::RegisterBuiltinAction()";

	if( registeredActions.IsFull() ) {
		AI_FailWith( tag, "Too many registered actions" );
	}

	ActionProps actionProps( actionName, nullptr );
	// Ensure map key valid lifetime, use ActionProps::name.
	if( !registeredActions.Insert( actionProps.name, std::move( actionProps ) ) ) {
		AI_FailWith( tag, "The action %s is already registered", actionName );
	}
}

void AiManager::SetupBotGoalsAndActions( edict_t *ent ) {
	Bot *const bot = ent->ai->botRef;

	constexpr const char *tag = "AiManager::SetupBotGoalsAndActions()";

	// This explicit call helps to avoid initialization order issues
	// when goals/actions and AI objects refer to each other.
	bot->planningModule.RegisterBuiltinGoalsAndActions();

	// Make sure all builtin goals and actions have been registered
	bool wereErrors = false;
	for( const auto *goal: bot->planningModule.Goals() ) {
		if( !registeredGoals.Get( goal->Name() ) ) {
			// Use G_Printf() as Debug() output may be turned off
			G_Printf( S_COLOR_RED "%s: Builtin goal %s has not been registered\n", tag, goal->Name() );
			wereErrors = true;
		}
	}
	for( const auto *action: bot->planningModule.Actions() ) {
		if( !registeredActions.Get( action->Name() ) ) {
			G_Printf( S_COLOR_RED "%s: Builtin action %s has not been registered\n", tag, action->Name() );
			wereErrors = true;
		}
	}
	if( wereErrors ) {
		AI_FailWith( tag, "There were errors\n" );
	}

	for( auto &goalPropsAndName: registeredGoals ) {
		GoalProps &goalProps = goalPropsAndName.second;
		// If the goal is builtin
		BotGoal *goal;
		if( !goalProps.factoryObject ) {
			goal = bot->GetGoalByName( goalProps.name );
		} else {
			goal = bot->InstantiateScriptGoal( goalProps.factoryObject, goalProps.name, goalProps.updatePeriod );
		}

		for( unsigned i = 0; i < goalProps.numApplicableActions; ++i ) {
			const char *actionName = goalProps.applicableActions[i];
			ActionProps *actionProps = registeredActions.Get( actionName );
			BotAction *action;
			// If the action is builtin
			if( !actionProps->factoryObject ) {
				action = bot->GetActionByName( actionProps->name );
			} else {
				action = bot->InstantiateScriptAction( actionProps->factoryObject, actionProps->name );
			}
			goal->AddExtraApplicableAction( action );
		}
	}
}

void AiManager::Frame() {
	globalCpuQuota.Update( aiHandlesListHead );
	thinkQuota[level.framenum % 4].Update( aiHandlesListHead );

	if( !GS_TeamBasedGametype() ) {
		AiBaseTeam::GetTeamForNum( TEAM_PLAYERS )->Update();
		return;
	}

	for( int team = TEAM_ALPHA; team < GS_MAX_TEAMS; ++team ) {
		AiBaseTeam::GetTeamForNum( team )->Update();
	}
}

void AiManager::FindHubAreas() {
	const auto *aasWorld = AiAasWorld::Instance();
	if( !aasWorld->IsLoaded() ) {
		return;
	}

	wsw::StaticVector<AreaAndScore, sizeof( hubAreas ) / sizeof( *hubAreas )> bestAreasHeap;
	for( int i = 1; i < aasWorld->NumAreas(); ++i ) {
		const auto &areaSettings = aasWorld->AreaSettings()[i];
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
		const auto &area = aasWorld->Areas()[i];
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
			const auto &reach = aasWorld->Reachabilities()[reachNum];
			if( reach.traveltype == TRAVEL_WALK || reach.traveltype == TRAVEL_WALKOFFLEDGE ) {
				usefulReachCount++;
			}
			++reachNum;
		}

		// Reject early to avoid more expensive call to push_heap()
		if( !usefulReachCount ) {
			continue;
		}

		bestAreasHeap.push_back( AreaAndScore( i, usefulReachCount ) );
		std::push_heap( bestAreasHeap.begin(), bestAreasHeap.end() );

		// bestAreasHeap size should be always less than its capacity:
		// 1) to ensure that there is a free room for next area;
		// 2) to ensure that hubAreas capacity will not be exceeded.
		if( bestAreasHeap.size() == bestAreasHeap.capacity() ) {
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

bool AiManager::GlobalQuota::Fits( const ai_handle_t *ai ) const {
	return !ai->aiRef->IsGhosting();
}

bool AiManager::ThinkQuota::Fits( const ai_handle_t *ai ) const {
	if( !ai->aiRef->IsGhosting() ) {
		return false;
	}
	// Only bots that have the same frame affinity fit
	return ai->botRef && ai->botRef->frameAffinityOffset == affinityOffset;
}

void AiManager::Quota::Update( const ai_handle_t *aiHandlesHead ) {
	if( !owner ) {
		owner = aiHandlesHead;
		while( owner && !Fits( owner ) ) {
			owner = owner->Next();
		}
		return;
	}

	const auto *const oldOwner = owner;
	// Start from the next AI in list
	owner = owner->Next();
	// Scan all bots that are after the current owner in the list
	while( owner ) {
		// Stop on the first bot that fits this
		if( Fits( owner ) ) {
			break;
		}
		owner = owner->Next();
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
		owner = owner->Next();
	}

	// If the loop execution has not been interrupted by break,
	// quota owner remains the same as before this call.
	// This means a bot always gets a quota if there is no other active bots in game.
}

bool AiManager::TryGetExpensiveComputationQuota( const Bot *bot ) {
	const edict_t *ent = game.edicts + bot->EntNum();
	return globalCpuQuota.TryAcquire( ent->ai );
}

bool AiManager::TryGetExpensiveThinkCallQuota( const Bot *bot ) {
	const edict_t *ent = game.edicts + bot->EntNum();
	return thinkQuota[level.framenum % 4].TryAcquire( ent->ai );
}

bool AiManager::Quota::TryAcquire( const ai_handle_t *ai ) {
	if( ai != owner ) {
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
