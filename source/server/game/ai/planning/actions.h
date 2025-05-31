#ifndef WSW_ea299e83_b6a8_432c_9c13_98d1fa0d0def_H
#define WSW_ea299e83_b6a8_432c_9c13_98d1fa0d0def_H

#include "planner.h"
#include "itemsselector.h"

constexpr const float GOAL_PICKUP_ACTION_RADIUS = 72.0f;
constexpr const float TACTICAL_SPOT_RADIUS = 40.0f;

class Bot;

class BotPlanningModule;

[[nodiscard]]
inline bool isSpecifiedAndTrue( const std::optional<bool> &var ) {
	return var && *var;
}
[[nodiscard]]
inline bool isUnspecifiedOrFalse( const std::optional<bool> &var ) {
	return !var || !*var;
}

class BotActionRecord : public AiActionRecord {
protected:
	Bot *Self() { return (Bot *)self; }
	const Bot *Self() const { return (const Bot *)self; }
public:
	BotActionRecord( PoolBase *pool_, Bot *self_, const char *name_ );

	void Activate() override;
	void Deactivate() override;
};

class BotAction : public AiAction {
protected:
	using DualOrigin = std::pair<Vec3, Vec3>;

	BotPlanningModule *const module;
	Bot *Self() { return (Bot *)self; }
	const Bot *Self() const { return (const Bot *)self; }
public:
	BotAction( BotPlanningModule *module_, const char *name_ );

	float DamageToBeKilled() const;
	float DamageToKill() const;
	float KillToBeKilledDamageRatio() const;

	inline const class BotWeightConfig &WeightConfig() const;
};

class RunToNavEntityActionRecord : public BotActionRecord {
	const SelectedNavEntity m_selectedNavEntity;

	bool ShouldUseSneakyBehaviour( const WorldState &currWorldState ) const;
	bool IsInPhsForEnemyTeam() const;
public:
	RunToNavEntityActionRecord( PoolBase *pool_, Bot *self_, const SelectedNavEntity &selectedNavEntity )
		: BotActionRecord( pool_, self_, "RunToNavEntityActionRecord" ), m_selectedNavEntity( selectedNavEntity ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

#define DECLARE_ACTION( actionName, poolSize )                                    \
	class actionName final : public BotAction {                                   \
		Pool<actionName ## Record, poolSize> pool;                                \
	public:                                                                       \
		explicit actionName( BotPlanningModule * module_ )                        \
			: BotAction( module_, #actionName )                                   \
			, pool( "Pool<" #actionName "Record>" ) {}                            \
		PlannerNode *TryApply( const WorldState &worldState ) override;           \
	}

#define DECLARE_INHERITED_ACTION( actionName, baseActionName, poolSize )          \
	class actionName : public baseActionName {                                    \
		Pool<actionName ## Record, poolSize> pool;                                \
public:                                                                           \
		actionName( BotPlanningModule * module_ )                                 \
			: baseActionName( module_, #actionName )                              \
			, pool( "Pool<" #actionName "Record>" ) {}                            \
		PlannerNode *TryApply( const WorldState &worldState ) override;           \
	}

DECLARE_ACTION( RunToNavEntityAction, 3 );

class PickupNavEntityActionRecord : public BotActionRecord {
	const SelectedNavEntity m_selectedNavEntity;
public:
	PickupNavEntityActionRecord( PoolBase *pool_, Bot *self_, const SelectedNavEntity &selectedNavEntity )
		: BotActionRecord( pool_, self_, "PickupNavEntityActionRecord" ), m_selectedNavEntity( selectedNavEntity ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( PickupNavEntityAction, 3 );

class WaitForNavEntityActionRecord : public BotActionRecord {
	const SelectedNavEntity m_selectedNavEntity;
public:
	WaitForNavEntityActionRecord( PoolBase *pool_, Bot *self_, const SelectedNavEntity &selectedNavEntity )
		: BotActionRecord( pool_, self_, "WaitForNavEntityActionRecord" ), m_selectedNavEntity( selectedNavEntity ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( WaitForNavEntityAction, 3 );

// A dummy action that always terminates actions chain but should not actually gets reached.
// This action is used to avoid direct world state satisfaction by temporary actions
// (that leads to premature planning termination).
class DummyActionRecord : public BotActionRecord {
public:
	DummyActionRecord( PoolBase *pool_, Bot *self_, const char *name_ )
		: BotActionRecord( pool_, self_, name_ ) {}

	void Activate() override { BotActionRecord::Activate(); }
	void Deactivate() override { BotActionRecord::Deactivate(); }

	Status UpdateStatus( const WorldState &currWorldState ) override {
		Debug( "This is a dummy action, should move to next one or replan\n" );
		return COMPLETED;
	}
};

#define DECLARE_DUMMY_ACTION_RECORD( recordName )               \
class recordName : public DummyActionRecord  {                  \
public:                                                         \
	recordName( PoolBase * pool_, Bot *self_ )                  \
		: DummyActionRecord( pool_, self_, #recordName ) {}     \
};

DECLARE_DUMMY_ACTION_RECORD( KillEnemyActionRecord )
DECLARE_ACTION( KillEnemyAction, 5 );

class RunAwayActionRecord : public BotActionRecord {
protected:
	NavSpot navSpot { Vec3( 0, 0, 0 ), 0.0f, NavTargetFlags::NONE };
	const unsigned selectedEnemyInstanceId;

public:
	RunAwayActionRecord( PoolBase *pool_,
						 Bot *self_,
						 const char *name_,
						 const Vec3 &navTargetOrigin,
						 unsigned selectedEnemyInstanceId_ )
		: BotActionRecord( pool_, self_, name_ ),
		selectedEnemyInstanceId( selectedEnemyInstanceId_ ) {
		navSpot.Set( navTargetOrigin, 32.0f, NavTargetFlags::REACH_ON_RADIUS );
	}
};

#define DECLARE_RUN_AWAY_ACTION_RECORD( recordName )                                                                 \
class recordName : public RunAwayActionRecord {                                                                      \
public:                                                                                                              \
	recordName( PoolBase * pool_, Bot *self_, const Vec3 &tacticalSpotOrigin, unsigned selectedEnemyInstanceId_ )  \
		: RunAwayActionRecord( pool_, self_, #recordName, tacticalSpotOrigin, selectedEnemyInstanceId_ ) {}        \
	void Activate() override;                                                                                        \
	void Deactivate() override;                                                                                      \
	Status UpdateStatus( const WorldState &currWorldState ) override;                                                \
}

class RunAwayAction : public BotAction {
protected:
	bool checkCommonPreconditionsForStartingRunningAway( const WorldState &worldState ) const;
	bool CheckMiddleRangeKDDamageRatio( const WorldState &worldState ) const;
	bool CheckCloseRangeKDDamageRatio( const WorldState &worldState ) const;
public:
	RunAwayAction( BotPlanningModule *module_, const char *name_ )
		: BotAction( module_, name_ ) {}
};

class FleeToSpotActionRecord : public BotActionRecord {
	NavSpot navSpot { NavSpot::Dummy() };
public:
	FleeToSpotActionRecord( PoolBase *pool_, Bot *self_, const Vec3 &destination )
		: BotActionRecord( pool_, self_, "FleeToSpotActionRecord" ) {
		navSpot.Set( destination, GOAL_PICKUP_ACTION_RADIUS, NavTargetFlags::REACH_ON_RADIUS );
	}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( FleeToSpotAction, 5 );

DECLARE_DUMMY_ACTION_RECORD( StartGotoCoverActionRecord );
DECLARE_INHERITED_ACTION( StartGotoCoverAction, RunAwayAction, 5 );

DECLARE_RUN_AWAY_ACTION_RECORD( TakeCoverActionRecord );
DECLARE_INHERITED_ACTION( TakeCoverAction, RunAwayAction, 5 );

DECLARE_DUMMY_ACTION_RECORD( StartGotoRunAwayTeleportActionRecord );
DECLARE_INHERITED_ACTION( StartGotoRunAwayTeleportAction, RunAwayAction, 5 );

DECLARE_RUN_AWAY_ACTION_RECORD( DoRunAwayViaTeleportActionRecord );
DECLARE_INHERITED_ACTION( DoRunAwayViaTeleportAction, RunAwayAction, 5 );

DECLARE_DUMMY_ACTION_RECORD( StartGotoRunAwayJumppadActionRecord );
DECLARE_INHERITED_ACTION( StartGotoRunAwayJumppadAction, RunAwayAction, 5 );

DECLARE_RUN_AWAY_ACTION_RECORD( DoRunAwayViaJumppadActionRecord );
DECLARE_INHERITED_ACTION( DoRunAwayViaJumppadAction, RunAwayAction, 5 );

DECLARE_DUMMY_ACTION_RECORD( StartGotoRunAwayElevatorActionRecord );
DECLARE_INHERITED_ACTION( StartGotoRunAwayElevatorAction, RunAwayAction, 5 );

DECLARE_RUN_AWAY_ACTION_RECORD( DoRunAwayViaElevatorActionRecord );
DECLARE_INHERITED_ACTION( DoRunAwayViaElevatorAction, RunAwayAction, 5 );

DECLARE_DUMMY_ACTION_RECORD( StopRunningAwayActionRecord );
DECLARE_INHERITED_ACTION( StopRunningAwayAction, RunAwayAction, 5 );

#undef DEFINE_ACTION
#undef DEFINE_INHERITED_ACTION
#undef DEFINE_DUMMY_ACTION_RECORD
#undef DEFINE_COMBAT_ACTION_RECORD
#undef DEFINE_RUN_AWAY_ACTION_RECORD

class DodgeToSpotActionRecord : public BotActionRecord {
	NavSpot navSpot { NavSpot::Dummy() };
	int64_t timeoutAt { std::numeric_limits<int>::max() };
public:
	DodgeToSpotActionRecord( PoolBase *pool_, Bot *self_, const Vec3 &spotOrigin )
		: BotActionRecord( pool_, self_, "DodgeToSpotActionRecord" )
		, navSpot( spotOrigin, 16.0f, NavTargetFlags::REACH_ON_RADIUS ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( DodgeToSpotAction, 1 );

class TurnToThreatOriginActionRecord : public BotActionRecord {
	Vec3 threatPossibleOrigin;
public:
	TurnToThreatOriginActionRecord( PoolBase *pool_, Bot *self_, const Vec3 &threatPossibleOrigin_ )
		: BotActionRecord( pool_, self_, "TurnToThreatOriginActionRecord" ),
		threatPossibleOrigin( threatPossibleOrigin_ ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( TurnToThreatOriginAction, 1 );

class TurnToLostEnemyActionRecord : public BotActionRecord {
	Vec3 lastSeenEnemyOrigin;
public:
	TurnToLostEnemyActionRecord( PoolBase *pool_, Bot *self_, const Vec3 &lastSeenEnemyOrigin_ )
		: BotActionRecord( pool_, self_, "TurnToLostEnemyActionRecord" ),
		lastSeenEnemyOrigin( lastSeenEnemyOrigin_ ) {}

	void Activate() override;
	void Deactivate() override;
	Status UpdateStatus( const WorldState &currWorldState ) override;
};

DECLARE_ACTION( TurnToLostEnemyAction, 1 );

class StartLostEnemyPursuitActionRecord : public DummyActionRecord {
public:
	StartLostEnemyPursuitActionRecord( PoolBase *pool_, Bot *self_ )
		: DummyActionRecord( pool_, self_, "StartLostEnemyPursuitActionRecord" ) {}
};

DECLARE_ACTION( StartLostEnemyPursuitAction, 1 );

class StopLostEnemyPursuitActionRecord : public DummyActionRecord {
public:
	StopLostEnemyPursuitActionRecord( PoolBase *pool_, Bot *self_ )
		: DummyActionRecord( pool_, self_, "StopLostEnemyPursuitActionRecord" ) {}
};

DECLARE_ACTION( StopLostEnemyPursuitAction, 1 );

#endif
