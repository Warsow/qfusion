#include "evolutionmanager.h"
#include "bot.h"

#include <random>

void *GT_asGetScriptBotEvolutionManager();

void GENERIC_asSetupConnectingBotWeights( void *scriptEvolutionManager, edict_t *ent );
void GENERIC_asSetupRespawningBotWeights( void *scriptEvolutionManager, edict_t *ent );

void GENERIC_asSaveEvolutionResults( void *scriptEvolutionManager );

BotEvolutionManager *BotEvolutionManager::instance = nullptr;

class DefaultBotEvolutionManager : public BotEvolutionManager
{
	BotWeightConfig referenceConfig;

	// Returns score of the reference config (greater than zero if it has been found).
	float DefaultEvolutionScore( const edict_t *ent ) const;

	void LoadReferenceWeightConfig();

public:
	DefaultBotEvolutionManager()
		: referenceConfig( nullptr ) {
		LoadReferenceWeightConfig();
	}

	~DefaultBotEvolutionManager() {}

	void OnBotConnected( edict_t *ent ) override;
	void OnBotRespawned( edict_t *ent ) override {}
	void SaveEvolutionResults() override;
};

class ScriptBotEvolutionManager : public BotEvolutionManager
{
	void *scriptObject;

public:
	ScriptBotEvolutionManager( void *scriptObject_ )
		: scriptObject( scriptObject_ ) {}

	~ScriptBotEvolutionManager() {}

	void OnBotConnected( edict_t *ent ) override {
		GENERIC_asSetupConnectingBotWeights( scriptObject, ent );
	}
	void OnBotRespawned( edict_t *ent ) override {
		GENERIC_asSetupRespawningBotWeights( scriptObject, ent );
	}
	void SaveEvolutionResults() override {
		GENERIC_asSaveEvolutionResults( scriptObject );
	}
};

static wsw::StaticVector<DefaultBotEvolutionManager, 1> defaultEvolutionManagerInstanceHolder;
static wsw::StaticVector<ScriptBotEvolutionManager, 1> scriptEvolutionManagerInstanceHolder;

void BotEvolutionManager::Init() {
	if( instance ) {
		AI_FailWith( "BotEvolutionManager::Init()", "An instance is already present" );
	}

	if( v_evolution.get() ) {
		if( void *scriptObject = GT_asGetScriptBotEvolutionManager() ) {
			void *mem = scriptEvolutionManagerInstanceHolder.unsafe_grow_back();
			instance = new(mem)ScriptBotEvolutionManager( scriptObject );
		}
	}

	if( !instance ) {
		instance = new ( defaultEvolutionManagerInstanceHolder.unsafe_grow_back() )DefaultBotEvolutionManager;
	}
}

void BotEvolutionManager::Shutdown() {
	if( !instance ) {
		return;
	}

	// Do not get an address of StaticVector::back() even if it seems legal,
	// an assertion in back() gets triggered in debug mode for an empty vector.
	// Do not assume that begin() returns a pointer and not a generic iterator,
	// get an address of iterator value explicitly.
	if( instance == &( *scriptEvolutionManagerInstanceHolder.begin() ) ) {
		scriptEvolutionManagerInstanceHolder.clear();
	} else {
		defaultEvolutionManagerInstanceHolder.clear();
	}

	instance = nullptr;
}

void DefaultBotEvolutionManager::LoadReferenceWeightConfig() {
	char gametype[MAX_CONFIGSTRING_CHARS];
	Q_snprintfz( gametype, MAX_CONFIGSTRING_CHARS, "%s%s", ( GS_Instagib( *ggs ) ? "i" : "" ), g_gametype->string );

	const char *mapname = level.mapname;
	if( referenceConfig.Load( va( "ai/%s_%s.weights", gametype, mapname ) ) ) {
		return;
	}

	constexpr const char *tag = "DefaultBotEvolutionManager::LoadReferenceWeightConfig()";
	G_Printf( S_COLOR_YELLOW "%s: Can't load AI weight config for gametype %s and map %s\n", tag, gametype, mapname );
	if( referenceConfig.Load( va( "ai/%s.weights", gametype ) ) ) {
		G_Printf( S_COLOR_YELLOW "%s: Using generic AI weight config for gametype %s\n", tag, gametype );
		return;
	}

	G_Printf( S_COLOR_YELLOW "%s: Can't load generic AI weight config for gametype %s\n", tag, gametype );
	if( referenceConfig.Load( "ai/generic.weights" ) ) {
		G_Printf( S_COLOR_YELLOW "%s: Using generic AI weight config...\n", tag );
		return;
	}

	G_Printf( S_COLOR_YELLOW "%s: Can't load generic AI weight config. Default weight values will be used\n", tag );
	referenceConfig.ResetToDefaultValues();
}

static wsw::StaticVector<std::pair<std::mt19937, std::normal_distribution<float> >, 1> randomHolder;

static inline float GetNextGaussianRandom( float stdDev ) {
	if( randomHolder.empty() ) {
		std::mt19937 generator;
		// This is very important, otherwise there will not be any randomness for evolution
		generator.seed( (unsigned long)clock() );
		std::normal_distribution<float> distribution( 0, 1 );
		randomHolder.emplace_back( std::make_pair( generator, distribution ) );
	}

	auto &generator = randomHolder.back().first;
	auto &distribution = randomHolder.back().second;
	return stdDev * distribution( generator );
}

void CopyWeightConfigRandomizing( const AiWeightConfigVarGroup *from, AiWeightConfigVarGroup *to, float stdDev ) {
	auto groupsIterator( ZipItemChains( from->GroupsListHead(), to->GroupsListHead(), "Randomizing child groups" ) );
	for(; groupsIterator.HasNext(); groupsIterator.Next() ) {
		CopyWeightConfigRandomizing( groupsIterator.First(), groupsIterator.Second(), stdDev );
	}

	auto varsIterator( ZipItemChains( from->VarsListHead(), to->VarsListHead(), "Randomizing child vars" ) );
	for(; varsIterator.HasNext(); varsIterator.Next() ) {
		float value, minValue, maxValue, defaultValue;
		varsIterator.First()->GetValueProps( &value, &minValue, &maxValue, &defaultValue );
		// Retrieve a normally-distributed random with mean at 0 (stdDev is assumed to be in (0, 1] range).
		float gaussianRandom = GetNextGaussianRandom( stdDev );
		// Adjust actual value by a length of the value domain (one half for each of positive/negative parts)
		gaussianRandom *= 0.5f * ( maxValue - minValue );
		// Apply the gaussian random to the value. For gaussian random = 0 value remains the same
		value += gaussianRandom;
		// Respect the value domain
		Q_clamp( value, minValue, maxValue );
		// Set the mutated var value
		varsIterator.Second()->SetValue( value );
	}
}

void DefaultBotEvolutionManager::OnBotConnected( edict_t *ent ) {
	if( v_evolution.get() ) {
		ent->bot->WeightConfig().CopyValues( referenceConfig );
		return;
	}

	int numBotsInGame = 0;
	for( int i = 0; i < ggs->maxclients; ++i ) {
		if( G_GetClientState( i ) < CS_SPAWNED ) {
			continue;
		}

		const edict_t *clientEnt = game.edicts + i + 1;
		if( !clientEnt->bot ) {
			continue;
		}

		numBotsInGame++;
	}

	float botNumRatio = numBotsInGame / (float)g_numbots->integer;
	if( botNumRatio <= 0.5f ) {
		ent->bot->WeightConfig().CopyValues( referenceConfig );
		return;
	}

	CopyWeightConfigRandomizing( (AiWeightConfigVarGroup *)&referenceConfig,
								 (AiWeightConfigVarGroup *)&ent->bot->WeightConfig(),
								 0.25f * ( botNumRatio - 0.5f ) );
}

float DefaultBotEvolutionManager::DefaultEvolutionScore( const edict_t *ent ) const {
	if( !ent->bot ) {
		return 0.0f;
	}

	const score_stats_t &stats = ent->r.client->stats;
	if( !stats.had_playtime ) {
		return 0.0f;
	}

	// We need some common action utility measurement unit. Give each action a weight in health/damage units.

	float damageScore = stats.GetEntry( "damage_given" );
	if( auto damageTaken = stats.GetEntry( "damage_taken" ) ) {
		damageScore *= damageScore / (float)damageTaken;
	}

	// Add 100 "damage" points for frags
	float killsScore = 100 * stats.GetEntry( "frags" );
	if( auto deaths = stats.GetEntry( "deaths" ) ) {
		killsScore *= ( killsScore / 100 ) / (float)deaths;
	}

	float healthScore = stats.GetEntry( "health_taken" ) + ( 1 / 0.66f ) * stats.GetEntry( "armor_taken" );
	// Add extra reward for MH/armors pickup
	healthScore += 2 * 100 * ( stats.GetEntry( "mh_taken" ) + stats.GetEntry( "uh_taken" ) );
	float armorScore = 100 * stats.GetEntry( "ra_taken" );
	armorScore += 75 * stats.GetEntry( "ya_taken" );
	armorScore += 50 * stats.GetEntry( "ga_taken" );
	healthScore += 2 * ( 1 / 0.66f ) * armorScore;

	float powerupsScore = stats.GetEntry( "quads_taken" );
	powerupsScore += stats.GetEntry( "shells_taken" );
	powerupsScore += stats.GetEntry( "regens_taken" );
	powerupsScore *= 500;

	float flagsScore = 500 * stats.GetEntry( "flags_capped" );
	float bombsScore = 200 * stats.GetEntry( "bombs_planted" ) + 300 * stats.GetEntry( "bombs_defused" );

	return damageScore + killsScore + healthScore + powerupsScore + flagsScore + bombsScore;
}

void DefaultBotEvolutionManager::SaveEvolutionResults() {
	if( !v_evolution.get() ) {
		return;
	}

	const edict_t *bestEnt = nullptr;
	float bestScore = 0.0f;
	float bestReferenceScore = 0.0f;
	unsigned numRatedBots = 0;

	for( int i = 1; i <= ggs->maxclients; ++i ) {
		edict_t *ent = game.edicts + i;
		float score = DefaultEvolutionScore( ent );
		if( score <= 0.0f ) {
			continue;
		}

		numRatedBots++;
		if( score > bestScore ) {
			bestScore = score;
			bestEnt = ent;
		}

		// Do this cheap test first, AiWeightConfig::operator==() is expensive
		if( score > bestReferenceScore ) {
			if( ent->bot->WeightConfig() == referenceConfig ) {
				bestReferenceScore = score;
			}
		}
	}

	constexpr const char *tag = "DefaultBotEvolutionManager::SaveEvolutionResults()";
	if( numRatedBots < ( GS_IndividualGametype( *ggs ) ? 2u : 3u ) ) {
		G_Printf( S_COLOR_YELLOW "%s: There were too few (%d) rated bots. No results to save.\n", tag, numRatedBots );
		return;
	}

	if( bestReferenceScore == 0.0f ) {
		G_Printf( S_COLOR_YELLOW "%s: Looks like the reference weight config was not contested. No results to save.\n", tag );
		return;
	}

	const char *fileName = va( "ai/%s%s_%s.weights", ( GS_Instagib( *ggs ) ? "i" : "" ), g_gametype->string, level.mapname );
	if( !bestEnt->bot->WeightConfig().Save( fileName ) ) {
		G_Printf( S_COLOR_RED "%s: Can't save weights file `%s`\n", tag, fileName );
	}
}
