#include "planninglocal.h"
#include "../bot.h"

void PickupNavEntityActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->GetMiscTactics().shouldMoveCarefully = true;
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
	assert( m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) );
	const NavEntity *const navEntity = m_selectedNavEntity.navEntity;
	Self()->SetCampingSpot( AiCampingSpot( navEntity->Origin(), GOAL_PICKUP_ACTION_RADIUS, 0.5f ) );
	Self()->SetNavTarget( navEntity );
}

void PickupNavEntityActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
	Self()->ResetCampingSpot();
}

AiActionRecord::Status PickupNavEntityActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	if( isSpecifiedAndTrue( currWorldState.getBool( WorldState::HasPickedGoalItem ) ) ) {
		Debug( "Goal item has been just picked up\n" );
		return COMPLETED;
	}

	if( !m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) ) {
		Debug( "The actual selected nav entity differs from the stored one\n" );
		return INVALID;
	}
	if( m_selectedNavEntity.navEntity->SpawnTime() - level.time > 0 ) {
		Debug( "The nav entity requires waiting for it\n" );
		return INVALID;
	}

	const Vec3 navEntityOrigin( m_selectedNavEntity.navEntity->Origin() );
	if( navEntityOrigin.squareDistanceTo( Self()->Origin() ) > wsw::square( GOAL_PICKUP_ACTION_RADIUS ) ) {
		Debug( "The nav entity is too far from the bot to pickup it\n" );
		return INVALID;
	}

	return VALID;
}

PlannerNode *PickupNavEntityAction::TryApply( const WorldState &worldState ) {
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasPickedGoalItem ) ) ) {
		Debug( "Bot has just picked a goal item in the given world state\n" );
		return nullptr;
	}

	const std::optional<Vec3> navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin );
	if( !navTargetOrigin ) {
		Debug( "The nav target origin is unspecified in the current world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( navTargetOrigin->squareDistanceTo( botOrigin ) > wsw::square( GOAL_PICKUP_ACTION_RADIUS ) ) {
		Debug( "Distance to goal item nav target is too large to pick up an item in the given world state\n" );
		return nullptr;
	}

	if( worldState.getUInt( WorldState::GoalItemWaitTime ) ) {
		Debug( "Goal item wait time is specified in the given world state\n" );
		return nullptr;
	}

	const std::optional<SelectedNavEntity> &maybeSelectedNavEntity = Self()->GetSelectedNavEntity();
	const SelectedNavEntity &selectedNavEntity = maybeSelectedNavEntity.value();
	PickupNavEntityActionRecord *record = pool.New( Self(), selectedNavEntity );

	PlannerNode *plannerNode = newNodeForRecord( record, worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setBool( WorldState::HasPickedGoalItem, true );
	plannerNode->worldState.setVec3( WorldState::BotOrigin, *navTargetOrigin );

	// plannerNode.WorldState().ResetTacticalSpots();

	return plannerNode;
}