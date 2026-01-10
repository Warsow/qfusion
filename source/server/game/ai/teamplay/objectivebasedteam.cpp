#include "../groundtracecache.h"
#include "objectivebasedteam.h"
#include "teamplaylocal.h"
#include "../navigation/aasroutecache.h"
#include "../combat/advantageproblemsolver.h"
#include "../bot.h"
#include <common/helpers/algorithm.h>

template <typename Spot, unsigned N, typename ScriptSpot>
AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::SpotsContainer( const char *itemName_ )
	: itemName( itemName_ ) {
	Clear();
}

template <typename Spot, unsigned N, typename ScriptSpot>
void AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::Clear() {
	// Clear references in loookup by id table
	std::fill_n( spotsForId, N, nullptr );

	spots[0].prev[Spot::STORAGE_LIST] = nullptr;
	spots[0].next[Spot::STORAGE_LIST] = &spots[1];
	spots[0].ReleaseHelpers();

	for( unsigned i = 1; i < N - 1; ++i ) {
		spots[i].prev[Spot::STORAGE_LIST] = &spots[i - 1];
		spots[i].next[Spot::STORAGE_LIST] = &spots[i + 1];
		spots[i].ReleaseHelpers();
	}

	spots[N - 1].prev[Spot::STORAGE_LIST] = &spots[N - 2];
	spots[N - 1].next[Spot::STORAGE_LIST] = nullptr;
	spots[N - 1].ReleaseHelpers();

	freeSpotsHead = &spots[0];
	usedSpotsHead = nullptr;
	size = 0;
}

template <typename Spot, unsigned N, typename ScriptSpot>
Spot *AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::Add( const ScriptSpot &scriptSpot ) {
	if( !ValidateId( scriptSpot.id ) ) {
		return nullptr;
	}

	if( GetById( scriptSpot.id, true ) ) {
		G_Printf( S_COLOR_YELLOW "%s (id=%d) is already present\n", itemName, scriptSpot.id );
		return nullptr;
	}

	if( !freeSpotsHead ) {
		G_Printf( S_COLOR_YELLOW "Can't add %s (id=%d): too many %s's\n", itemName, scriptSpot.id, itemName );
		return nullptr;
	}

	Spot *spot = wsw::unlink( freeSpotsHead, &freeSpotsHead, Spot::STORAGE_LIST );
	// Set an address of the spot in the lookup by id table
	spotsForId[scriptSpot.id] = spot;
	// Construct a new spot in-place before linking based on the script-visible spot
	new( spot )Spot( scriptSpot );
	// Set valid links
	wsw::link( spot, &usedSpotsHead, Spot::STORAGE_LIST );
	size++;

	return spot;
}

template <typename Spot, unsigned N, typename ScriptSpot>
Spot *AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::Remove( int id ) {
	if( !ValidateId( id ) ) {
		return nullptr;
	}

	if( Spot *spot = GetById( id ) ) {
		wsw::unlink( spot, &usedSpotsHead, Spot::STORAGE_LIST );
		wsw::link( spot, &freeSpotsHead, Spot::STORAGE_LIST );
		spotsForId[id] = nullptr;
		size--;
		spot->ReleaseHelpers();
		return spot;
	}

	return nullptr;
}

template <typename Spot, unsigned N, typename ScriptSpot>
bool AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::ValidateId( int id, bool silentOnError ) {
	if( (unsigned)id < N ) {
		return true;
	}
	if( !silentOnError ) {
		const char *format = S_COLOR_YELLOW "Illegal %s id %d (must be within [0, %d) range)\n";
		G_Printf( format, itemName, id, N );
	}
	return false;
}

template <typename Spot, unsigned N, typename ScriptSpot>
Spot *AiObjectiveBasedTeam::SpotsContainer<Spot, N, ScriptSpot>::GetById( int id, bool silentOnNull ) {
	// As this method is internal, it must be called with already validated id
	assert( ValidateId( id ) );
	Spot *const result = spotsForId[id];
	if( !silentOnNull && !result ) {
		G_Printf( S_COLOR_YELLOW "%s (id=%d) cannot be found\n", itemName, id );
	}
	return result;
}

AiObjectiveBasedTeam::AiObjectiveBasedTeam( int team_ )
	: AiSquadBasedTeam( team_ )
	, defenceSpots( "DefenceSpot" )
	, offenseSpots( "OffenseSpot" ) {}

void AiObjectiveBasedTeam::ObjectiveSpotImpl::ReleaseHelpers() {
	for( edict_t *&ent: helperEnts ) {
		if( ent ) {
			AI_RemoveNavEntity( ent );
			G_FreeEdict( ent );
			ent = nullptr;
		}
	}
}

void AiObjectiveBasedTeam::ObjectiveSpotImpl::PrepareForAssignment() {
	botsListHead = nullptr;
	weight = 0.0f;
	currHelperEnt = 0;

	memset( thisEntityWeightsForBot, 0, sizeof( thisEntityWeightsForBot ) );
}

bool AiObjectiveBasedTeam::AddDefenceSpot( const AiDefenceSpot &scriptVisibleSpot ) {
	if( DefenceSpot *addedSpot = defenceSpots.Add( scriptVisibleSpot ) ) {
		EnableDefenceSpotAutoAlert( addedSpot );
		return true;
	}
	return false;
}

bool AiObjectiveBasedTeam::RemoveDefenceSpot( int id ) {
	if( DefenceSpot *removedSpot = defenceSpots.Remove( id ) ) {
		ResetAllBotsOrders();
		if( removedSpot->usesAutoAlert ) {
			DisableDefenceSpotAutoAlert( removedSpot );
		}
		return true;
	}
	return false;
}

bool AiObjectiveBasedTeam::AddOffenseSpot( const AiOffenseSpot &scriptVisibleSpot ) {
	return offenseSpots.Add( scriptVisibleSpot ) != nullptr;
}

bool AiObjectiveBasedTeam::RemoveOffenseSpot( int id ) {
	if( offenseSpots.Remove( id ) ) {
		ResetAllBotsOrders();
		return true;
	}
	return false;
}

void AiObjectiveBasedTeam::RemoveAllObjectiveSpots() {
	for( DefenceSpot *spot = defenceSpots.Head(); spot; spot = spot->Next() ) {
		if( spot->usesAutoAlert ) {
			DisableDefenceSpotAutoAlert( spot );
		}
	}

	defenceSpots.Clear();
	offenseSpots.Clear();

	ResetAllBotsOrders();
}

bool AiObjectiveBasedTeam::SetDefenceSpotAlert( int id, float alertLevel, unsigned timeoutPeriod ) {
	if( !defenceSpots.ValidateId( id ) ) {
		return false;
	}
	DefenceSpot *const spot = defenceSpots.GetById( id );
	if( !spot ) {
		return false;
	}

	Q_clamp( alertLevel, 0.0f, 1.0f );
	spot->alertLevel = alertLevel;
	spot->alertTimeoutAt = level.time + timeoutPeriod;
	return true;
}

AiAlertSpot AiObjectiveBasedTeam::DefenceSpot::ToAlertSpot() const {
	AiAlertSpot result( id, Vec3( entity->s.origin ), radius );
	result.regularEnemyInfluenceScale = regularEnemyAlertScale;
	result.carrierEnemyInfluenceScale = carrierEnemyAlertScale;
	return result;
}

void AiObjectiveBasedTeam::EnableDefenceSpotAutoAlert( DefenceSpot *defenceSpot ) {
	AiAlertSpot alertSpot( defenceSpot->ToAlertSpot() );
	// TODO: Track a list of all bots in AiBaseTeam
	for( int i = 1; i <= ggs->maxclients; ++i ) {
		edict_t *ent = game.edicts + i;
		if( !ent->bot ) {
			continue;
		}
		// If an entity is an AI, it is a client.
		if( ent->r.client->team != this->teamNum ) {
			continue;
		}
		auto callback = (AlertTracker::AlertCallback)( &AiObjectiveBasedTeam::OnAlertReported );
		ent->bot->EnableAutoAlert( alertSpot, callback, this );
	}
	defenceSpot->usesAutoAlert = true;
}

void AiObjectiveBasedTeam::DisableDefenceSpotAutoAlert( DefenceSpot *defenceSpot ) {
	for( int i = 1; i <= ggs->maxclients; ++i ) {
		edict_t *ent = game.edicts + i;
		if( !ent->bot ) {
			continue;
		}
		if( ent->r.client->team != this->teamNum ) {
			continue;
		}
		ent->bot->DisableAutoAlert( defenceSpot->id );
	}
	defenceSpot->usesAutoAlert = false;
}

void AiObjectiveBasedTeam::OnAlertReported( Bot *bot, int id, float alertLevel ) {
	DefenceSpot *const spot = defenceSpots.GetById( id );
	if( !spot ) {
		G_Printf( S_COLOR_YELLOW "AiObjectiveBasedTeam::OnAlertReported(): Can't find a DefenceSpot (id=%d)\n", id );
		return;
	}
	// Several bots in team may not realize real alert level
	// (in alert reporting "fair" bot vision is used, and bot may have missed other attackers)

	const float oldAlertLevel = spot->alertLevel;
	// If reported alert level is greater than the current one, always override the current level
	if( spot->alertLevel <= alertLevel ) {
		spot->alertLevel = alertLevel;
	}
	// Otherwise override the current level only when last report is dated and has almost expired
	else if( spot->alertTimeoutAt < level.time + 150 ) {
		spot->alertLevel = alertLevel;
	}

	// Keep alert state if an alert is present
	// Note: bots may (and usually do) report zero alert level.
	if( alertLevel > 0 ) {
		spot->alertTimeoutAt = level.time + 1000;
	}

	if( oldAlertLevel + 0.3f > alertLevel ) {
		return;
	}

	const char *const colorPrefix = alertLevel >= 0.5f ? S_COLOR_RED : S_COLOR_YELLOW;
	if( const auto *baseMessage = spot->alertMessage ) {
		G_Say_Team( bot->self, va( "%s%s!!!", colorPrefix, baseMessage ) );
	} else {
		G_Say_Team( bot->self, va( "%sAn enemy is incoming!!!", colorPrefix ) );
	}
}

void AiObjectiveBasedTeam::OnBotAdded( Bot *bot ) {
	AiSquadBasedTeam::OnBotAdded( bot );

	for( DefenceSpot *spot = defenceSpots.Head(); spot; spot = spot->Next() ) {
		if( !spot->usesAutoAlert ) {
			continue;
		}
		auto callback = (AlertTracker::AlertCallback)( &AiObjectiveBasedTeam::OnAlertReported );
		bot->EnableAutoAlert( spot->ToAlertSpot(), callback, this );
	}
}

void AiObjectiveBasedTeam::OnBotRemoved( Bot *bot ) {
	AiSquadBasedTeam::OnBotRemoved( bot );

	ResetBotOrders( bot );

	for( DefenceSpot *spot = defenceSpots.Head(); spot; spot = spot->Next() ) {
		if( spot->usesAutoAlert ) {
			bot->DisableAutoAlert( spot->id );
		}
	}
}

bool AiObjectiveBasedTeam::OverridesEntityWeights( const Bot *bot ) const {
	if( AiSquadBasedTeam::OverridesEntityWeights( bot ) ) {
		return true;
	}
	return bot->ObjectiveSpot();
}

using WeightsPair = const std::pair<float, float>;

WeightsPair *AiObjectiveBasedTeam::GetEntityWeights( const Bot *bot, const NavEntity *navEntity ) const {
	return ChooseWeights( GetOwnWeights( bot, navEntity ), AiSquadBasedTeam::GetEntityWeights( bot, navEntity ) );
}

inline WeightsPair *AiObjectiveBasedTeam::GetOwnWeights( const Bot *bot, const NavEntity *navEntity ) const {
	const AiObjectiveSpot *givenSpot = bot->ObjectiveSpot();
	if( !givenSpot ) {
		return nullptr;
	}

	if( const auto *weights = FindWeightsForBot( defenceSpots.Head(), givenSpot, bot, navEntity ) ) {
		return weights;
	}

	if( const auto *weights = FindWeightsForBot( offenseSpots.Head(), givenSpot, bot, navEntity ) ) {
		return weights;
	}

	// This is perfectly reachable as arbitrary nav entities can be supplied
	return nullptr;
}

WeightsPair *AiObjectiveBasedTeam::FindWeightsForBot( const ObjectiveSpotImpl *spotsChainHead,
													  const AiObjectiveSpot *givenSpot,
													  const Bot *bot,
													  const NavEntity *navEntity ) const {
	for( const ObjectiveSpotImpl *spot = spotsChainHead; spot; spot = spot->Next() ) {
		if( static_cast<const AiObjectiveSpot *>( spot->underlying ) == givenSpot ) {
			if( const auto *weights = spot->FindWeightsForBot( bot, navEntity ) ) {
				return weights;
			}
		}
	}

	return nullptr;
}

WeightsPair *AiObjectiveBasedTeam::ObjectiveSpotImpl::FindWeightsForBot( const Bot *bot, const NavEntity *nav ) const {
	if( nav->IsBasedOnEntity( underlying->entity ) ) {
		return &thisEntityWeightsForBot[bot->ClientNum()][0];
	}

	for( int i = 0; i < MAX_HELPER_ENTS; ++i ) {
		if( nav->IsBasedOnEntity( helperEnts[i] ) ) {
			return &thisEntityWeightsForBot[bot->ClientNum()][i + 1];
		}
	}

	return nullptr;
}

void AiObjectiveBasedTeam::Update() {
	// Call super method first, it contains an obligatory logic
	AiSquadBasedTeam::Update();

	if( PermitsDistributedUpdateThisFrame() ) {
		Candidates candidates;
		FindAllCandidates( candidates );

		// First reset all candidates statuses to default values
		for( auto &botAndScore: candidates ) {
			ResetBotOrders( botAndScore.bot );
		}

		// Defenders must be assigned first
		AssignBots( defenceSpots.Head(), defenceSpots.size, candidates );
		AssignBots( offenseSpots.Head(), offenseSpots.size, candidates );
	}
}

void AiObjectiveBasedTeam::ResetBotOrders( Bot *bot ) {
	bot->SetObjectiveSpot( nullptr );
	// We modify base offensiveness. Set it to default
	bot->SetBaseOffensiveness( 0.5f );
}

void AiObjectiveBasedTeam::ResetAllBotsOrders() {
	// TODO: Utilize AiManager Ai/Bot list?
	for( int i = 0; i <= ggs->maxclients; ++i ) {
		edict_t *ent = game.edicts + i;
		if( !ent->r.inuse || !ent->bot ) {
			continue;
		}
		ResetBotOrders( ent->bot );
	}
}

void AiObjectiveBasedTeam::FindAllCandidates( Candidates &candidates ) {
	for( int i = 0; i <= ggs->maxclients; ++i ) {
		edict_t *ent = game.edicts + i;
		if( !ent->r.inuse || !ent->bot ) {
			continue;
		}
		// If an entity is an AI, it is a client too.
		if( G_ISGHOSTING( ent ) ) {
			continue;
		}
		if( ent->r.client->team != this->teamNum ) {
			continue;
		}

		candidates.push_back( BotAndScore( ent->bot ) );
	}
}

/**
 * Unfortunately we can't (?) inherit from self as a template type in C++,
 * so we can't use a self-type for generic {@code next}, {@code prev} links in a superclass
 * (and thus write a generic code that operates on these links in a superclass) like this:
 * <pre>
 * 	  template <typename T> struct ObjectiveSpotImpl {
 * 	  	T *prev[], *next[];
 * 	  };
 * 	  ...
 * 	  struct DefenceSpot: public ObjectiveSpotImpl<DefenceSpot>
 * 	  ...
 * </pre>
 * This free (non-member) template is a workaround to quickly get an implementation for an exact descendant type.
 */
template <typename Spot>
Spot *SortByWeightImpl( Spot *knownSpotsHead ) {
	// Best-weight spots should be first after sorting
	auto less = []( const Spot &lhs, const Spot &rhs ) { return lhs.weight > rhs.weight; };
	return ::SortLinkedList( knownSpotsHead, Spot::STORAGE_LIST, Spot::SORTED_LIST, less );
}

AiObjectiveBasedTeam::DefenceSpot *AiObjectiveBasedTeam::DefenceSpot::SortByWeight( ObjectiveSpotImpl *head ) {
	return SortByWeightImpl( dynamic_cast<DefenceSpot *>( head ) );
}

AiObjectiveBasedTeam::OffenseSpot *AiObjectiveBasedTeam::OffenseSpot::SortByWeight( ObjectiveSpotImpl *head ) {
	return SortByWeightImpl( dynamic_cast<OffenseSpot *>( head ) );
}

void AiObjectiveBasedTeam::AssignBots( ObjectiveSpotImpl *spotsListHead, int numSpots, Candidates &candidates ) {
	if( !spotsListHead ) {
		return;
	}

	// Compute raw score of bots as for this objective.
	// This is a call to an overridden method, so we need some instance (even if its unused).
	spotsListHead->ComputeRawScores( candidates );

	const float weightNormalizationScale = 1.0f / (float)numSpots;
	for( ObjectiveSpotImpl *spot = spotsListHead; spot; spot = spot->Next() ) {
		spot->PrepareForAssignment();
		spot->weight *= weightNormalizationScale;
	}

	ObjectiveSpotImpl *const sortedSpotsHead = spotsListHead->SortByWeight( spotsListHead );

	for( ObjectiveSpotImpl *spot = sortedSpotsHead; spot; spot = spot->NextSorted() ) {
		if( candidates.empty() ) {
			break;
		}

		// Note: don't interrupt at zero spot weight.
		// We have to assign MinAssignedBots() even for zero-weight spots.

		// Compute effective bot defender scores for this spot
		spot->ComputeEffectiveScores( candidates );
		// Sort candidates so best candidates are last
		wsw::sortPodNonSpeedCritical( candidates.begin(), candidates.end() );

		// Reserve minimal number of assigned bots
		unsigned numBotsToAssign = spot->MinAssignedBots();
		// Be greedy and try to assign a number of all other bots scaled by the spot weight
		numBotsToAssign += std::round( ( candidates.size() - spot->MinAssignedBots() ) * spot->weight );
		// Ensure the value is withing the defined bounds
		Q_clamp( numBotsToAssign, spot->MinAssignedBots(), spot->MaxAssignedBots() );

		clamp_high( numBotsToAssign, candidates.size() );
		// Best candidates are at the end of candidates vector
		for( unsigned j = 0; j < numBotsToAssign; ++j ) {
			spot->Link( candidates.back().bot );
			candidates.pop_back();
		}
	}

	// Update status of ALL spots (the loop above might be interrupted early)
	for( ObjectiveSpotImpl *spot = spotsListHead; spot; spot = spot->Next() ) {
		spot->UpdateBotsStatus();
	}
}

void AiObjectiveBasedTeam::DefenceSpot::ComputeRawScores( Candidates &candidates ) {
	const float armorProtection = g_armor_protection->value;
	const float armorDegradation = g_armor_degradation->value;
	const auto *const gameEdicts = game.edicts;
	for( auto &botAndScore: candidates ) {
		const Bot *bot = botAndScore.bot;
		const auto *const botEnt = gameEdicts + bot->EntNum();
		// Prefer attacking having powerups
		if( ::HasPowerups( botEnt ) ) {
			botAndScore.rawScore = 0.001f;
			continue;
		}

		float resistanceScore = DamageToKill( botEnt, armorProtection, armorDegradation );
		float weaponScore = 0.0f;
		const auto *inventory = botAndScore.bot->Inventory();
		for( int weapon = WEAP_GUNBLADE + 1; weapon < WEAP_TOTAL; ++weapon ) {
			if( !inventory[weapon] ) {
				continue;
			}

			const auto *weaponDef = GS_GetWeaponDef( ggs, weapon );

			if( weaponDef->firedef.ammo_id != AMMO_NONE && weaponDef->firedef.ammo_max ) {
				weaponScore += inventory[weaponDef->firedef.ammo_id] / (float)weaponDef->firedef.ammo_max;
			} else {
				weaponScore += 1.0f;
			}

			if( weaponDef->firedef_weak.ammo_id != AMMO_NONE && weaponDef->firedef_weak.ammo_max ) {
				weaponScore += inventory[weaponDef->firedef_weak.ammo_id] / (float)weaponDef->firedef_weak.ammo_max;
			} else {
				weaponScore += 1.0f;
			}

			// TODO: Modify by weapon tier
		}

		weaponScore /= ( WEAP_TOTAL - WEAP_GUNBLADE - 1 );
		weaponScore = sqrtf( weaponScore + 0.001f );

		botAndScore.rawScore = resistanceScore * weaponScore * bot->PlayerDefenciveAbilitiesRating();
	}
}

inline void AiObjectiveBasedTeam::ObjectiveSpotImpl::Link( Bot *bot ) {
	wsw::link( bot, &botsListHead, Bot::OBJECTIVE_LINKS );
}

void AiObjectiveBasedTeam::ObjectiveSpotImpl::ComputeEffectiveScores( Candidates &candidates ) {
	const float *const spotOrigin = this->Origin();
	for( auto &botAndScore: candidates ) {
		const float squareDistance = DistanceSquared( botAndScore.bot->Origin(), spotOrigin );
		const float inverseDistance = 1.0f / std::sqrt( squareDistance + 0.001f );
		botAndScore.effectiveScore = botAndScore.rawScore * inverseDistance;
	}
}

void AiObjectiveBasedTeam::OffenseSpot::ComputeRawScores( Candidates &candidates ) {
	const float armorProtection = g_armor_protection->value;
	const float armorDegradation = g_armor_degradation->value;
	const auto *const gameEdicts = game.edicts;
	for( auto &botAndScore: candidates ) {
		const auto *const bot = botAndScore.bot;
		const auto *const botEnt = gameEdicts + bot->EntNum();
		float score = DamageToKill( botEnt, armorProtection, armorDegradation );
		// Consider quad and shell equally valuable
		if( ::HasShell( botEnt ) ) {
			score *= QUAD_DAMAGE_SCALE;
		}
		if( ::HasQuad( botEnt ) ) {
			score *= QUAD_DAMAGE_SCALE;
		}
		score *= bot->PlayerOffenciveAbilitiesRating();
		botAndScore.rawScore = score;
	}
}

int AiObjectiveBasedTeam::DefenceSpot::FindSpots( vec3_t spots[MAX_HELPER_ENTS], int numRequestedSpots ) {
	// Try finding tactical spots around target
	AdvantageProblemSolver::OriginParams originParams( underlying->entity, this->radius, AiAasRouteCache::Shared() );
	AdvantageProblemSolver::ProblemParams problemParams( underlying->entity );
	problemParams.setMinHeightAdvantageOverOrigin( 24.0f );
	problemParams.setAdvantageOverOriginForMaxScore( 1000.0f );
	problemParams.setMinHeightAdvantageOverEntity( 24.0f );
	problemParams.setAdvantageOverEntityForMaxScore( 1000.0f );
	problemParams.setSpotProximityThreshold( 192.0f );
	problemParams.setOriginWeightFalloffDistanceRatio( 0.5f );
	problemParams.setMinSpotDistanceToEntity( 192.0f );
	problemParams.setCheckToAndBackReach( true );
	problemParams.setMaxFeasibleTravelTimeMillis( 7500 );
	problemParams.setOptimizeAggressively( true );
	AdvantageProblemSolver solver( originParams, problemParams );
	return solver.findMany( spots, wsw::min( (int)MAX_HELPER_ENTS, numRequestedSpots ) );
}

void AiObjectiveBasedTeam::DefenceSpot::UpdateBotsStatusForAlert() {
	float squareDistances[MAX_CLIENTS];
	ComputeBotDistances( squareDistances );

	const int numRequestedSpots = SuggestNumberOfTacticalSpots( squareDistances, this->radius );

	vec3_t spots[MAX_HELPER_ENTS];
	const int numSpots = FindSpots( spots, numRequestedSpots );

	int spotNum = 0;
	const float *const spotOrigin = this->entity->s.origin;
	const float squareSpotRadius = this->radius * this->radius;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		const float squareDistanceToSpot = DistanceSquared( bot->Origin(), spotOrigin );
		if( squareDistanceToSpot > squareSpotRadius ) {
			SetWeightsForBot( bot, 12.0f );
			bot->SetBaseOffensiveness( 0.25f );
			continue;
		}

		if( spotNum < numSpots ) {
			if( auto *ent = AllocHelperEnt( spots[spotNum] ) ) {
				SetWeightsForBot( bot, ent, 3.0f );
				bot->SetBaseOffensiveness( 1.0f );
				spotNum++;
				continue;
			}
		}

		bot->SetBaseOffensiveness( 1.0f );
		if( squareDistanceToSpot < 72.0f * 72.0f ) {
			bot->SetObjectiveSpot( nullptr );
			continue;
		}

		SetWeightsForBot( bot, 9.0f );
	}
}

bool AiObjectiveBasedTeam::DefenceSpot::IsVisibleForDefenders() {
	const auto *const pvsCache = EntitiesPvsCache::Instance();
	auto *const spotOrigin = const_cast<float *>( this->entity->s.origin );
	auto *const gameEdicts = game.edicts;
	edict_t *const ignore = gameEdicts + ENTNUM( this->entity );
	trace_t trace;

	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		const auto *const botEnt = gameEdicts + bot->EntNum();
		Vec3 botToSpot( spotOrigin );
		botToSpot -= bot->Origin();
		// Don't normalize and use a dot product sign for a coarse test
		if( bot->EntityPhysicsState()->ForwardDir().dot( botToSpot ) < 0 ) {
			continue;
		}

		if( !pvsCache->AreInPvs( botEnt, this->entity ) ) {
			continue;
		}

		Vec3 viewOrigin( bot->Origin() );
		viewOrigin.z() += botEnt->viewheight;
		G_Trace( &trace, viewOrigin.data(), nullptr, nullptr, spotOrigin, ignore, MASK_AISOLID );
		if( trace.fraction != 1.0f ) {
			continue;
		}

		return true;
	}

	return false;
}

Bot *AiObjectiveBasedTeam::ObjectiveSpotImpl::FindBestByTravelTimeBot() {
	const int spotAreaNum = AiAasWorld::instance()->findAreaNum( underlying->entity );
	int bestTravelTime = std::numeric_limits<int>::max();
	Bot *bestBot = nullptr;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		int fromAreaNums[2] = { 0, 0 };
		const int numFromAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( fromAreaNums );
		const int travelTime = bot->RouteCache()->FindRoute( fromAreaNums, numFromAreas, spotAreaNum, bot->TravelFlags() );
		if( travelTime && travelTime < bestTravelTime ) {
			bestTravelTime = travelTime;
			bestBot = bot;
		}
	}
	return bestBot;
}

Bot *AiObjectiveBasedTeam::ObjectiveSpotImpl::FindClosestByDistanceBot( float *squareDistances ) {
	const float *const __restrict spotOrigin = underlying->entity->s.origin;

	float scratchpad[MAX_CLIENTS];
	if( !squareDistances ) {
		squareDistances = scratchpad;
	}

	Bot *bot = botsListHead;
	Bot *bestBot = bot;
	float bestDistance = DistanceSquared( bot->Origin(), spotOrigin );
	squareDistances[bot->ClientNum()] = bestDistance;
	for( bot = bot->NextInObjective(); bot; bot = bot->NextInObjective() ) {
		float distance = DistanceSquared( bot->Origin(), spotOrigin );
		squareDistances[bot->ClientNum()] = distance;
		if( distance < bestDistance ) {
			bestBot = bot;
			bestDistance = distance;
		}
	}
	return bestBot;
}

int AiObjectiveBasedTeam::ObjectiveSpotImpl::SuggestNumberOfTacticalSpots( const float *squareDistances,
																		   float maxDistance,
																		   const Bot *excludedBot ) {
	int numRequestedSpots = 0;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		if( bot == excludedBot ) {
			continue;
		}
		if( squareDistances[bot->ClientNum()] > maxDistance * maxDistance ) {
			continue;
		}
		numRequestedSpots++;
	}
	return numRequestedSpots;
}

void AiObjectiveBasedTeam::ObjectiveSpotImpl::ComputeBotDistances( float *squareDistances ) {
	const float *__restrict origin = this->Origin();
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		const float *__restrict botOrigin = bot->Origin();
		squareDistances[bot->ClientNum()] = DistanceSquared( origin, botOrigin );
	}
}

void AiObjectiveBasedTeam::DefenceSpot::UpdateBotsStatus() {
	if( this->alertLevel ) {
		UpdateBotsStatusForAlert();
		return;
	}

	Bot *alreadyAssignedBot = nullptr;
	if( !IsVisibleForDefenders() ) {
		if( ( alreadyAssignedBot = FindBestByTravelTimeBot() ) ) {
			// Force the bot to check the spot status
			alreadyAssignedBot->SetBaseOffensiveness( 0.5f );
			SetWeightsForBot( alreadyAssignedBot, 15.0f );
		}
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto aasFloorClusters = aasWorld->areaFloorClusterNums();
	const auto spotFloorClusterNum = aasFloorClusters[aasWorld->findAreaNum( this->entity )];
	const float *spotOrigin = this->entity->s.origin;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		if( bot == alreadyAssignedBot ) {
			continue;
		}

		const float squareDistanceToSpot = DistanceSquared( bot->Origin(), spotOrigin );

		// If the bot is fairly close to the spot, stop running to it and enrage bots to attack everybody
		if( squareDistanceToSpot < ( 0.33f * this->radius ) * ( 0.33f * this->radius ) ) {
			SetWeightsForBot( bot, 0.0f );
			bot->SetBaseOffensiveness( 1.0f );
			continue;
		}

		// If the bot is farther than 2/3 of the spot radius
		// return to the spot with high priority and regular offensiveness
		if( squareDistanceToSpot < ( 0.66f * this->radius ) * ( 0.66f * this->radius ) ) {
			SetWeightsForBot( bot, 3.0f );
			bot->SetBaseOffensiveness( 0.5f );
			continue;
		}

		// The bot is within 2/3 of the radius.
		// Check whether the bot is in the same floor cluster
		const int botFloorClusterNum = aasFloorClusters[bot->EntityPhysicsState()->DroppedToFloorAasAreaNum()];
		if( spotFloorClusterNum && botFloorClusterNum && botFloorClusterNum == spotFloorClusterNum ) {
			// Set the spot as a nav target but with a low weight
			// (the bot should be able to return to it quickly being in the same floor cluster)
			SetWeightsForBot( bot, 0.5f );
			// Raise offensiveness compared to default.
			// As the bot is in the same cluster, the bot can prefer attacking enemies
			// more than return to the spot for the same reasons.
			bot->SetBaseOffensiveness( 0.85f );
			continue;
		}

		// Return to the spot having middle interest
		SetWeightsForBot( bot, 1.0f );
		// Raise offensiveness
		bot->SetBaseOffensiveness( 0.70f );
	}
}

inline void AiObjectiveBasedTeam::ObjectiveSpotImpl::SetWeightsForBot( Bot *bot, float navWeight, float goalWeight ) {
	if( navWeight <= 0.0f ) {
		bot->SetObjectiveSpot( nullptr );
		return;
	}

	// Set the goal weight to nav weight if not specified
	if( goalWeight <= 0.0f ) {
		goalWeight = navWeight;
	}

	thisEntityWeightsForBot[bot->ClientNum()][0] = std::make_pair( navWeight, goalWeight );
	bot->SetObjectiveSpot( underlying );
}

void AiObjectiveBasedTeam::ObjectiveSpotImpl::SetWeightsForBot( Bot *bot, const edict_t *helperEnt,
																float navWeight, float goalWeight ) {
	if( navWeight <= 0.0f ) {
		bot->SetObjectiveSpot( nullptr );
		return;
	}

	// Set the goal weight to nav weight if not specified
	if( goalWeight <= 0.0f ) {
		goalWeight = navWeight;
	}

	for( int i = 0; i < MAX_HELPER_ENTS; ++i ) {
		if( helperEnt == helperEnts[i] ) {
			thisEntityWeightsForBot[bot->ClientNum()][i + 1] = std::make_pair( navWeight, goalWeight );
			break;
		}
	}

	bot->SetObjectiveSpot( underlying );
}

void AiObjectiveBasedTeam::OffenseSpot::SetDefaultSpotWeightsForBots() {
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		// If bot is not in squad, set an offence spot weight to a value of an ordinary valuable item.
		// Thus bots will not attack alone and will grab some items instead in order to prepare to attack.
		SetWeightsForBot( bot, bot->IsInSquad() ? 9.0f : 3.0f );
		bot->SetBaseOffensiveness( 0.0f );
	}
}

int AiObjectiveBasedTeam::OffenseSpot::FindSpots( vec3_t spots[MAX_HELPER_ENTS], int numRequestedSpots ) {
	// Try finding tactical spots around target
	AdvantageProblemSolver::OriginParams originParams( underlying->entity, 768.0f, AiAasRouteCache::Shared() );
	AdvantageProblemSolver::ProblemParams problemParams( underlying->entity );
	problemParams.setSpotProximityThreshold( 96.0f );
	problemParams.setOriginWeightFalloffDistanceRatio( 0.25f );

	AdvantageProblemSolver solver( originParams, problemParams );
	return solver.findMany( spots, wsw::min( (int)MAX_HELPER_ENTS, numRequestedSpots ) );
}

void AiObjectiveBasedTeam::OffenseSpot::UpdateBotsStatus() {
	if( !botsListHead ) {
		return;
	}

	if( const auto *navEntity = NavEntitiesRegistry::Instance()->NavEntityForEntity( underlying->entity ) ) {
		if( navEntity->ShouldBeReachedInGroup() ) {
			// Everybody should stay on the spot.
			// Just set the spot as a nav target for every bot.
			SetDefaultSpotWeightsForBots();
			return;
		}
	}

	float squareDistances[MAX_CLIENTS];
	Bot *closestBot = FindClosestByDistanceBot( squareDistances );

	// If even the closest bot is not sufficiently close to target
	if( squareDistances[closestBot->ClientNum()] > 72.0f * 72.0f ) {
		SetDefaultSpotWeightsForBots();
		return;
	}

	// Find how many spots do we need.
	constexpr float rushDistanceThreshold = 768.0f;
	const int numRequestedSpots = SuggestNumberOfTacticalSpots( squareDistances, 768.0f, closestBot );

	// Nobody else is at least "rush distance threshold" close to the target
	if( !numRequestedSpots ) {
		SetDefaultSpotWeightsForBots();
		return;
	}

	vec3_t spots[MAX_HELPER_ENTS];
	const int numSpots = FindSpots( spots, numRequestedSpots );

	int spotNum = 0;
	for( Bot *bot = botsListHead; bot; bot = bot->NextInObjective() ) {
		if( bot == closestBot ) {
			// Keep staying here
			SetWeightsForBot( bot, 12.0f );
			bot->SetBaseOffensiveness( 0.0f );
			continue;
		}

		if( squareDistances[bot->ClientNum()] > rushDistanceThreshold * rushDistanceThreshold ) {
			// Rush to the spot
			SetWeightsForBot( bot, 9.0f );
			bot->SetBaseOffensiveness( 0.0f );
			continue;
		}

		// Go to an advantage spot if possible
		if( spotNum < numSpots ) {
			if( edict_t *ent = AllocHelperEnt( spots[spotNum] ) ) {
				SetWeightsForBot( bot, ent, 9.0f );
				bot->SetBaseOffensiveness( 0.5f );
				spotNum++;
				continue;
			}
		}

		// If the bot can see the target from this origin, just attack enemies while teammates are completing objective
		if( !EntitiesPvsCache::Instance()->AreInPvs( game.edicts + bot->EntNum(), underlying->entity ) ) {
			// Keep advancing to the spot
			SetWeightsForBot( bot, 9.0f );
			bot->SetBaseOffensiveness( 0.25f );
			continue;
		}

		trace_t trace;
		// TODO: Use raycast in a floor cluster if possible
		// TODO: Check against entities like wbomb1 explosion target
		SolidWorldTrace( &trace, bot->Origin(), underlying->entity->s.origin );
		if( trace.fraction == 1.0f ) {
			bot->SetObjectiveSpot( nullptr );
			bot->SetBaseOffensiveness( 1.0f );
			continue;
		}

		// Keep advancing to the spot
		SetWeightsForBot( bot, 9.0f );
		bot->SetBaseOffensiveness( 0.25f );
	}
}

edict_t *AiObjectiveBasedTeam::ObjectiveSpotImpl::AllocHelperEnt( const vec3_t origin ) {
	if( currHelperEnt == MAX_HELPER_ENTS ) {
		return nullptr;
	}

	edict_t *ent = helperEnts[currHelperEnt];
	if( ent ) {
		VectorCopy( origin, ent->s.origin );
		currHelperEnt++;
		return ent;
	}

	ent = helperEnts[currHelperEnt] = G_Spawn();
	ent->classname = "objective_helper_ent";
	VectorCopy( origin, ent->s.origin );
	// Hack! We try to avoid Add/RemoveNavEntity() calls and reuse existing nav entities instead.
	// Use MOVABLE flag so the area num gets updated automatically.
	auto flags = NavEntityFlags::REACH_AT_RADIUS | NavEntityFlags::MOVABLE;
	// Provide actual area num at spawn (even if its going to be checked next frame)
	int areaNum = AiAasWorld::instance()->findAreaNum( origin );
	NavEntitiesRegistry::Instance()->AddNavEntity( ent, areaNum, flags );

	currHelperEnt++;
	return ent;
}