#include "planninglocal.h"
#include "../bot.h"

void WaitForNavEntityActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->GetMiscTactics().shouldMoveCarefully = true;
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
	assert( m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) );
	const NavEntity *navEntity = m_selectedNavEntity.navEntity;
	Self()->SetNavTarget( navEntity );
	Self()->SetCampingSpot( AiCampingSpot( navEntity->Origin(), GOAL_PICKUP_ACTION_RADIUS, 0.5f ) );
}

void WaitForNavEntityActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetCampingSpot();
	Self()->ResetNavTarget();
}

AiActionRecord::Status WaitForNavEntityActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	if( isSpecifiedAndTrue( currWorldState.getBool( WorldState::HasPickedGoalItem ) ) ) {
		Debug( "Goal item has been just picked up\n" );
		return COMPLETED;
	}

	if( !m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) ) {
		Debug( "The actual selected nav entity differs from the stored one\n" );
		return INVALID;
	}

	const NavEntity *navEntity = m_selectedNavEntity.navEntity;
	// Wait duration is too long (more than it was estimated)
	const auto waitDuration = (uint64_t)( navEntity->SpawnTime() - level.time );
	if( waitDuration > navEntity->MaxWaitDuration() ) {
		constexpr auto *format = "Wait duration %" PRIu64 " is too long "
								 "(the maximum allowed value for a nav entity is %" PRIu64 ")\n";
		Debug( format, waitDuration, navEntity->MaxWaitDuration() );
		return INVALID;
	}

	if( navEntity->Origin().SquareDistanceTo( Self()->Origin() ) > wsw::square( GOAL_PICKUP_ACTION_RADIUS ) ) {
		Debug( "Distance to the item is too large to wait for it\n" );
		return INVALID;
	}

	if( isSpecifiedAndTrue( currWorldState.getBool( WorldState::HasThreateningEnemy ) ) ) {
		Debug( "The bot has a threatening enemy\n" );
		return INVALID;
	}

	return VALID;
}

PlannerNode *WaitForNavEntityAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> navTargetOrigin( worldState.getVec3( WorldState::NavTargetOrigin ) );
	if( !navTargetOrigin ) {
		Debug( "Nav target is ignored in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( navTargetOrigin->SquareDistanceTo( botOrigin ) > wsw::square( GOAL_PICKUP_ACTION_RADIUS ) ) {
		Debug( "Distance to goal item nav target is too large to wait for an item in the given world state\n" );
		return nullptr;
	}

	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasPickedGoalItem ) ) ) {
		Debug( "Bot has just picked a goal item in the given world state\n" );
		return nullptr;
	}

	const std::optional<unsigned> waitTime = worldState.getUInt( WorldState::GoalItemWaitTime );
	if( !waitTime ) {
		Debug( "Goal item wait time is not specified in the given world state\n" );
		return nullptr;
	}

	const std::optional<SelectedNavEntity> &maybeSelectedNavEntity = Self()->GetSelectedNavEntity();
	const SelectedNavEntity &selectedNavEntity = maybeSelectedNavEntity.value();
	WaitForNavEntityActionRecord *record = pool.New( Self(), selectedNavEntity );

	PlannerNode *plannerNode = newNodeForRecord( record, worldState, (float)*waitTime );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::BotOrigin, *navTargetOrigin );
	plannerNode->worldState.setBool( WorldState::HasPickedGoalItem, true );

	return plannerNode;
}