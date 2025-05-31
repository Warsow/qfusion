#ifndef WSW_607da844_a2a6_4c8f_b6e1_9120aa212c87_H
#define WSW_607da844_a2a6_4c8f_b6e1_9120aa212c87_H

#include "planner.h"

class BotPlanningModule;
class SelectedEnemy;
class BotWeightConfig;

class BotGoal : public AiGoal {
	wsw::StaticVector<AiAction *, AiPlanner::MAX_ACTIONS> extraApplicableActions;
public:
	BotGoal( BotPlanningModule *module_, const char *name_, int debugColor_, unsigned updatePeriod_ );

	void AddExtraApplicableAction( AiAction *action ) {
		extraApplicableActions.push_back( action );
	}
protected:
	BotPlanningModule *const module;

	float DamageToBeKilled() const;

	PlannerNode *ApplyExtraActions( PlannerNode *firstTransition, const WorldState &worldState );

	Bot *Self() { return (Bot *)self; }
	const Bot *Self() const { return (Bot *)self; }

	const std::optional<struct SelectedNavEntity> &getSelectedNavEntity() const;
	const std::optional<SelectedEnemy> &getSelectedEnemy() const;
	const class BotWeightConfig &WeightConfig() const;
};

class GrabItemGoal : public BotGoal {
public:
	explicit GrabItemGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "GrabItemGoal", COLOR_RGB( 0, 255, 0 ), 950 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class KillEnemyGoal : public BotGoal {
	float additionalWeight { 0.0f };
public:
	explicit KillEnemyGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "KillEnemyGoal", COLOR_RGB( 255, 0, 0 ), 1250 ) {}

	void SetAdditionalWeight( float weight ) {
		this->additionalWeight = weight;
	}

	float GetAndResetAdditionalWeight() {
		float result = wsw::max( 0.0f, additionalWeight );
		this->additionalWeight = 0;
		return result;
	}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class RunAwayGoal : public BotGoal {
public:
	explicit RunAwayGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "RunAwayGoal", COLOR_RGB( 0, 0, 255 ), 950 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class AttackOutOfDespairGoal : public BotGoal {
	float oldOffensiveness { 1.0f };
public:
	explicit AttackOutOfDespairGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "AttackOutOfDespairGoal", COLOR_RGB( 192, 192, 0 ), 750 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;

	void OnPlanBuildingStarted() override;
	void OnPlanBuildingCompleted( const AiActionRecord *planHead ) override;
};

class ReactToHazardGoal : public BotGoal {
public:
	explicit ReactToHazardGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "ReactToHazardGoal", COLOR_RGB( 192, 0, 192 ), 750 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class ReactToThreatGoal : public BotGoal {
public:
	explicit ReactToThreatGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "ReactToThreatGoal", COLOR_RGB( 255, 0, 128 ), 350 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class ReactToEnemyLostGoal : public BotGoal {
	void ModifyWeightForTurningBack( const WorldState &currWorldState, const Vec3 &enemyOrigin );
	void ModifyWeightForPursuit( const WorldState &currWorldState, const Vec3 &enemyOrigin );
	bool HuntEnemiesLeftInMinority( const WorldState &currWorldState ) const;
	int FindNumPlayersAlive( int team ) const;
public:
	explicit ReactToEnemyLostGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "ReactToEnemyLostGoal", COLOR_RGB( 0, 192, 192 ), 950 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

class RoamGoal : public BotGoal {
public:
	explicit RoamGoal( BotPlanningModule *module_ )
		: BotGoal( module_, "RoamGoal", COLOR_RGB( 0, 0, 80 ), 400 ) {}

	void UpdateWeight( const WorldState &currWorldState ) override;
	bool IsSatisfiedBy( const WorldState &worldState ) const override;
	PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) override;
};

#endif
