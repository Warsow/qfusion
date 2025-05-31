#ifndef WSW_e014c832_c2a4_46e0_84fa_6db073ba5bf7_H
#define WSW_e014c832_c2a4_46e0_84fa_6db073ba5bf7_H

#include "actions.h"
#include "botplanner.h"
#include "goals.h"
#include "itemsselector.h"
#include "roamingmanager.h"
#include "tacticalspotscache.h"

class BotPlanningModule {
	friend class Bot;
	friend class BotPlanner;
	friend class BotAction;
	friend class BotGoal;
	friend class GrabItemGoal;
	friend class KillEnemyGoal;
	friend class RunAwayGoal;
	friend class ReactToHazardGoal;
	friend class ReactToThreatGoal;
	friend class ReactToEnemyLostGoal;
	friend class AttackOutOfDespairGoal;
	friend class RoamGoal;
	friend class BotTacticalSpotsCache;
	friend class WorldState;

	friend class StartGotoRunAwayElevatorAction;
	friend class StartGotoRunAwayTeleportAction;
	friend class StartGotoRunAwayJumppadAction;

	friend class StartGotoCoverAction;
	friend class DodgeToSpotAction;

	Bot *const bot;

	BotPlanner planner;

	GrabItemGoal grabItemGoal;
	KillEnemyGoal killEnemyGoal;
	RunAwayGoal runAwayGoal;
	ReactToHazardGoal reactToHazardGoal;
	ReactToThreatGoal reactToThreatGoal;
	ReactToEnemyLostGoal reactToEnemyLostGoal;
	AttackOutOfDespairGoal attackOutOfDespairGoal;
	RoamGoal roamGoal;

	RunToNavEntityAction runToNavEntityAction;
	PickupNavEntityAction pickupNavEntityAction;
	WaitForNavEntityAction waitForNavEntityAction;

	FleeToSpotAction fleeToSpotAction;
	StartGotoCoverAction startGotoCoverAction;
	TakeCoverAction takeCoverAction;

	StartGotoRunAwayTeleportAction startGotoRunAwayTeleportAction;
	DoRunAwayViaTeleportAction doRunAwayViaTeleportAction;
	StartGotoRunAwayJumppadAction startGotoRunAwayJumppadAction;
	DoRunAwayViaJumppadAction doRunAwayViaJumppadAction;
	StartGotoRunAwayElevatorAction startGotoRunAwayElevatorAction;
	DoRunAwayViaElevatorAction doRunAwayViaElevatorAction;
	StopRunningAwayAction stopRunningAwayAction;

	DodgeToSpotAction dodgeToSpotAction;

	TurnToThreatOriginAction turnToThreatOriginAction;

	TurnToLostEnemyAction turnToLostEnemyAction;
	StartLostEnemyPursuitAction startLostEnemyPursuitAction;
	StopLostEnemyPursuitAction stopLostEnemyPursuitAction;

	BotTacticalSpotsCache tacticalSpotsCache;
	BotItemsSelector itemsSelector;
	BotRoamingManager roamingManager;

	void RegisterBuiltinGoal( BotGoal &goal ) {
		planner.goals.push_back( &goal );
	}

	void RegisterBuiltinAction( BotAction &action ) {
		planner.actions.push_back( &action );
	}
public:
	explicit BotPlanningModule( Bot *bot_ );

	void RegisterBuiltinGoalsAndActions();

	bool ShouldAimPrecisely() const {
		// Try shooting immediately if "attacking out of despair"
		return planner.activeGoal != &attackOutOfDespairGoal;
	}

	bool IsReactingToHazard() const {
		return planner.activeGoal == &reactToHazardGoal;
	}

	void ClearGoalAndPlan() { planner.ClearGoalAndPlan(); }

	const WorldState &CachedWorldState() const { return planner.cachedWorldState; }

	void CheckTargetProximity() { return roamingManager.CheckSpotsProximity(); }

	void OnMovementToNavEntityBlocked( const NavEntity *navEntity ) {
		roamingManager.DisableSpotsInRadius( navEntity->Origin(), 144.0f );
		itemsSelector.MarkAsDisabled( *navEntity, 4000 );
	}

	void ClearOverriddenEntityWeights() {
		itemsSelector.ClearOverriddenEntityWeights();
	}

	void OverrideEntityWeight( const edict_t *ent, float weight ) {
		itemsSelector.OverrideEntityWeight( ent, weight );
	}

	std::optional<SelectedNavEntity> SuggestGoalNavEntity( const NavEntity *currNavEntity ) {
		return itemsSelector.SuggestGoalNavEntity( currNavEntity );
	}

	bool IsTopTierItem( const NavTarget *navTarget ) const {
		return itemsSelector.IsTopTierItem( navTarget );
	}

	bool IsPerformingPursuit() const;
};

#endif
