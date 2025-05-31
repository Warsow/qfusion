#ifndef WSW_d69f420e_1b43_400b_8929_814d4626366c_H
#define WSW_d69f420e_1b43_400b_8929_814d4626366c_H

#include "planning/planner.h"
#include "component.h"
#include "planning/goalentities.h"
#include <common/wswstaticvector.h>

class Bot;

class AiManager {
	static const unsigned MAX_ACTIONS = AiPlanner::MAX_ACTIONS;
	static const unsigned MAX_GOALS = AiPlanner::MAX_GOALS;

protected:
	AiManager( const char *gametype, const char *mapname );

	int teams[MAX_CLIENTS];
	Bot *botHandlesHead { nullptr };

	struct Quota {
		int64_t givenAt { 0 };
		const Bot *owner { nullptr };

		virtual bool Fits( const Bot *ai ) const = 0;

		bool TryAcquire( const Bot *ai );
		void Update( const Bot *botHandlesHead );

		void OnRemoved( const Bot *bot ) {
			if( bot == owner ) {
				owner = nullptr;
			}
		}
	};

	struct GlobalQuota final : public Quota {
		bool Fits( const Bot *bot ) const override;
	};

	struct ThinkQuota final : public Quota {
		const unsigned affinityOffset;
		explicit ThinkQuota( unsigned affinityOffset_ ): affinityOffset( affinityOffset_ ) {}
		bool Fits( const Bot *bot ) const override;
	};

	GlobalQuota globalCpuQuota;
	ThinkQuota thinkQuota[4] = {
		ThinkQuota( 0 ), ThinkQuota( 1 ), ThinkQuota( 2 ), ThinkQuota( 3 )
	};

	int hubAreas[16];
	int numHubAreas { 0 };

	static AiManager *instance;

	bool CheckCanSpawnBots();
	void CreateUserInfo( char *buffer, size_t bufferSize );
	edict_t * ConnectFakeClient();
	float MakeSkillForNewBot( const Client *client ) const;
	void SetupBotForEntity( edict_t *ent );
	void TryJoiningTeam( edict_t *ent, const char *teamName );

	void RegisterBuiltinGoal( const char *goalName );
	void RegisterBuiltinAction( const char *actionName );

	void SetupBotGoalsAndActions( edict_t *ent );

	void FindHubAreas();
public:
	void Update();

	void LinkBot( Bot *bot );
	void UnlinkBot( Bot *bot );

	void OnBotDropped( edict_t *ent );

	static AiManager *Instance() { return instance; }

	static void Init( const char *gametype, const char *mapname );
	static void Shutdown();

	void notifyOfNavEntitySignaledAsReached( const NavEntity *navEntity );
	void notifyOfNavEntityRemoved( const NavEntity *navEntity );

	void OnBotJoinedTeam( edict_t *ent, int team );

	void RegisterEvent( const edict_t *ent, int event, int parm );

	void SpawnBot( const char *teamName );
	void RespawnBot( edict_t *ent );
	void RemoveBot( const wsw::StringView &name );
	void AfterLevelScriptShutdown();
	void BeforeLevelScriptShutdown();

	bool IsAreaReachableFromHubAreas( int targetArea, float *score = nullptr ) const;

	/**
	 * Allows cycling rights to perform CPU-consuming operations among bots.
	 * This is similar to checking ent == level.think_client_entity
	 * but counts only bots making cycling and thus frametimes more even.
	 * These calls have semantics similar to "compare and swap":
	 * If somebody has already requested an operation, returns false.
	 * Otherwise, sets some internal lock and returns true.
	 * @note Subsequent calls in the same frame fail even for the same client
	 * (only a single expensive operation is allowed per frame globally).
	 */
	bool TryGetExpensiveComputationQuota( const Bot *bot );

	/**
	 * Similar to {@code TryGetExpensiveComputationQuota()}
	 * but tracks bots that have different think frame offset separately.
	 * @note This quota is independent from the global one.
	 */
	bool TryGetExpensiveThinkCallQuota( const Bot *bot );
};

#endif
