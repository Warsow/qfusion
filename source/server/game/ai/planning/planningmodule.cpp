#include "planningmodule.h"
#include "planninglocal.h"

BotPlanningModule::BotPlanningModule( Bot *bot_ )
	: bot( bot_ )
	, planner( bot_, this )
	, grabItemGoal( this )
	, killEnemyGoal( this )
	, runAwayGoal( this )
	, reactToHazardGoal( this )
	, reactToThreatGoal( this )
	, reactToEnemyLostGoal( this )
	, attackOutOfDespairGoal( this )
	, roamGoal( this )
	, runToNavEntityAction( this )
	, pickupNavEntityAction( this )
	, waitForNavEntityAction( this )
	, fleeToSpotAction( this )
	, startGotoCoverAction( this )
	, takeCoverAction( this )
	, startGotoRunAwayTeleportAction( this )
	, doRunAwayViaTeleportAction( this )
	, startGotoRunAwayJumppadAction( this )
	, doRunAwayViaJumppadAction( this )
	, startGotoRunAwayElevatorAction( this )
	, doRunAwayViaElevatorAction( this )
	, stopRunningAwayAction( this )
	, dodgeToSpotAction( this )
	, turnToThreatOriginAction( this )
	, turnToLostEnemyAction( this )
	, startLostEnemyPursuitAction( this )
	, stopLostEnemyPursuitAction( this )
	, tacticalSpotsCache( bot_ )
	, itemsSelector( bot_ )
	, roamingManager( bot_ ) {}

void BotPlanningModule::RegisterBuiltinGoalsAndActions() {
	RegisterBuiltinGoal( grabItemGoal );
	RegisterBuiltinGoal( killEnemyGoal );
	RegisterBuiltinGoal( runAwayGoal );
	RegisterBuiltinGoal( reactToHazardGoal );
	RegisterBuiltinGoal( reactToThreatGoal );
	RegisterBuiltinGoal( reactToEnemyLostGoal );
	RegisterBuiltinGoal( attackOutOfDespairGoal );
	RegisterBuiltinGoal( roamGoal );

	RegisterBuiltinAction( runToNavEntityAction );
	RegisterBuiltinAction( pickupNavEntityAction );
	RegisterBuiltinAction( waitForNavEntityAction );

	RegisterBuiltinAction( fleeToSpotAction );
	RegisterBuiltinAction( startGotoCoverAction );
	RegisterBuiltinAction( takeCoverAction );

	RegisterBuiltinAction( startGotoRunAwayTeleportAction );
	RegisterBuiltinAction( doRunAwayViaTeleportAction );
	RegisterBuiltinAction( startGotoRunAwayJumppadAction );
	RegisterBuiltinAction( doRunAwayViaJumppadAction );
	RegisterBuiltinAction( startGotoRunAwayElevatorAction );
	RegisterBuiltinAction( doRunAwayViaElevatorAction );
	RegisterBuiltinAction( stopRunningAwayAction );

	RegisterBuiltinAction( dodgeToSpotAction );

	RegisterBuiltinAction( turnToThreatOriginAction );

	RegisterBuiltinAction( turnToLostEnemyAction );
	RegisterBuiltinAction( startLostEnemyPursuitAction );
	RegisterBuiltinAction( stopLostEnemyPursuitAction );
}

bool BotPlanningModule::IsPerformingPursuit() const {
	// These dynamic casts are quite bad but this is not invoked on a hot code path
	if( dynamic_cast<const ReactToEnemyLostGoal *>( planner.activeGoal ) ) {
		if( !dynamic_cast<const TurnToLostEnemyActionRecord *>( planner.planHead ) ) {
			return true;
		}
	}
	return false;
}