#include "planninglocal.h"
#include "../bot.h"

void FleeToSpotActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
	Self()->SetNavTarget( &navSpot );
}

void FleeToSpotActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status FleeToSpotActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy();
	if( selectedEnemy && selectedEnemy->CouldBeHitIfBotTurns() ) {
		Self()->GetMiscTactics().PreferAttackRatherThanRun();
	} else {
		Self()->GetMiscTactics().PreferRunRatherThanAttack();
	}

	// It really gets invalidated on goal reevaluation

	if( ( navSpot.Origin() - Self()->Origin() ).fastLength() <= GOAL_PICKUP_ACTION_RADIUS ) {
		return COMPLETED;
	}

	return VALID;
}

PlannerNode *FleeToSpotAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin );
	if( !navTargetOrigin ) {
		Debug( "Nav target is absent in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( botOrigin.fastDistanceTo( *navTargetOrigin ) <= GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is too close to the nav target\n" );
		return nullptr;
	}

	if( const std::optional<SelectedNavEntity> &maybeSelectedNavEntity = Self()->GetSelectedNavEntity() ) {
		const Vec3 navEntityOrigin = maybeSelectedNavEntity->navEntity->Origin();
		if( ( navEntityOrigin - *navTargetOrigin ).squareLength() < 1.0f ) {
			Debug( "Action is not applicable for goal entities (there are specialized actions for these ones)\n" );
			return nullptr;
		}
	}

	// As a contrary to combat actions, illegal travel time (when the destination is not reachable for AAS) is allowed.
	// Combat actions require simple kinds of movement to keep crosshair on enemy.
	// Thus tactical spot should be reachable in common way for combat actions.
	// In case of retreating, some other kinds of movement AAS is not aware of might be used.
	int travelTimeMillis = Self()->CheckTravelTimeMillis( botOrigin, *navTargetOrigin );
	// If the travel time is 0, set it to maximum allowed AAS travel time
	// (AAS stores time as seconds^-2 in a short value)
	if( !travelTimeMillis ) {
		travelTimeMillis = 10 * std::numeric_limits<short>::max();
	}

	FleeToSpotActionRecord *record = pool.New( Self(), *navTargetOrigin );
	PlannerNode *const plannerNode = newNodeForRecord( record, worldState, (float)travelTimeMillis );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::BotOrigin, *navTargetOrigin );
	// Since bot origin has been moved, tactical spots should be recomputed
	// TODO plannerNode.WorldState().ResetTacticalSpots();

	return plannerNode;
}