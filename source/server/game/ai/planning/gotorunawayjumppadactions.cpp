#include "planninglocal.h"
#include "../bot.h"

PlannerNode *StartGotoRunAwayJumppadAction::TryApply( const WorldState &worldState ) {
	if( !checkCommonPreconditionsForStartingRunningAway( worldState ) ) {
		return nullptr;
	}

	if( worldState.getVec3( WorldState::PendingJumppadDest ) ) {
		Debug( "The pending origin is already present in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	const Vec3 enemyOrigin = worldState.getVec3( WorldState::EnemyOrigin ).value();
	std::optional<DualOrigin> maybeFromTo = module->tacticalSpotsCache.getRunAwayJumppadOrigin( botOrigin, enemyOrigin );
	if( !maybeFromTo ) {
		Debug( "Failed to find a (cached) runaway jumppad\n" );
		return nullptr;
	}

	PlannerNode *plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::NavTargetOrigin, maybeFromTo->first );
	plannerNode->worldState.setVec3( WorldState::PendingJumppadDest, maybeFromTo->second );

	return plannerNode;
}

void DoRunAwayViaJumppadActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->SetNavTarget( &navSpot );
}

void DoRunAwayViaJumppadActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status DoRunAwayViaJumppadActionRecord::UpdateStatus( const WorldState &currWorldState )  {
	if( level.time - Self()->LastTeleportTouchTime() < 64 ) {
		return COMPLETED;
	}

	if( const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy() ) {
		if( selectedEnemyInstanceId != selectedEnemy->InstanceId() ) {
			Debug( "New enemies have been selected\n" );
			return INVALID;
		}
	} else {
		Debug( "Selected enemies have been invalidated\n" );
		return INVALID;
	}

	// Use the same radius as for goal items pickups
	// (running actions for picking up an item and running away might be shared)
	if( ( navSpot.Origin() - Self()->Origin() ).fastLength() > GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is too far from the jumppad trigger\n" );
		return INVALID;
	}

	return VALID;
}

PlannerNode *DoRunAwayViaJumppadAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> pendingOrigin = worldState.getVec3( WorldState::PendingJumppadDest );
	if( !pendingOrigin ) {
		Debug( "The pending jumppad dest is missing in the current world state\n" );
		return nullptr;
	}

	const Vec3 navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin ).value();
	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();

	if( botOrigin.fastDistanceTo( navTargetOrigin ) > GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is too far from the nav target (jumppad origin)" );
		return nullptr;
	}

	const Vec3 &jumppadOrigin = navTargetOrigin;
	// Use distance from jumppad origin to target as an estimation for travel time millis
	const float cost = ( jumppadOrigin - *pendingOrigin ).fastLength();

	unsigned selectedEnemyInstanceId = Self()->GetSelectedEnemy().value().InstanceId();
	DoRunAwayViaJumppadActionRecord *record = pool.New( Self(), jumppadOrigin, selectedEnemyInstanceId );
	
	PlannerNode *const plannerNode = newNodeForRecord( record, worldState, cost );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::BotOrigin, *pendingOrigin );
	plannerNode->worldState.clearVec3( WorldState::PendingJumppadDest );

	plannerNode->worldState.setBool( WorldState::IsRunningAway, true );
	plannerNode->worldState.setBool( WorldState::CanHitEnemy, false );
	plannerNode->worldState.setBool( WorldState::EnemyCanHit, false );

	return plannerNode;
}

