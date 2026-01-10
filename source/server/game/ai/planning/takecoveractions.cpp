#include "planninglocal.h"
#include "../bot.h"

PlannerNode *StartGotoCoverAction::TryApply( const WorldState &worldState ) {
	if( !checkCommonPreconditionsForStartingRunningAway( worldState ) ) {
		return nullptr;
	}

	if( isSpecifiedAndTrue( worldState.getBool( WorldState::IsRunningAway ) ) ) {
		return nullptr;
	}

	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasRunAway ) ) ) {
		return nullptr;
	}

	if( worldState.getVec3( WorldState::PendingCoverSpot ) ) {
		Debug( "Pending origin is already present in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( botOrigin.fastDistanceTo( Self()->Origin() ) > 1.0f ) {
		Debug( "This action is only applicable to the actual bot origin\n" );
		return nullptr;
	}

	const Vec3 enemyOrigin = worldState.getVec3( WorldState::EnemyOrigin ).value();

	const std::optional<Vec3> maybeSpotOrigin = module->tacticalSpotsCache.getCoverSpot( botOrigin, enemyOrigin );
	if( !maybeSpotOrigin ) {
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::NavTargetOrigin, *maybeSpotOrigin );
	plannerNode->worldState.setVec3( WorldState::PendingCoverSpot, *maybeSpotOrigin );

	return plannerNode;
}

void TakeCoverActionRecord::Activate() {
	BotActionRecord::Activate();
	// Since bot should be already close to the nav target, give (a defencive) aiming a higher priority
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
	Self()->SetNavTarget( &navSpot );
}

void TakeCoverActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status TakeCoverActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	static_assert( GOAL_PICKUP_ACTION_RADIUS > TACTICAL_SPOT_RADIUS );

	float distanceToActionNavTarget = ( navSpot.Origin() - Self()->Origin() ).squareLength();
	if( distanceToActionNavTarget > GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is too far from nav target\n" );
		return INVALID;
	}

	return ( distanceToActionNavTarget < TACTICAL_SPOT_RADIUS ) ? COMPLETED : VALID;
}

PlannerNode *TakeCoverAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin );
	if( !navTargetOrigin ) {
		return nullptr;
	}

	const std::optional<Vec3> pendingOrigin = worldState.getVec3( WorldState::PendingCoverSpot );
	if( !pendingOrigin ) {
		return nullptr;
	}

	if( pendingOrigin->squareDistanceTo( *navTargetOrigin ) > 1.0f ) {
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( botOrigin.squareDistanceTo( *navTargetOrigin ) > TACTICAL_SPOT_RADIUS ) {
		Debug( "Bot is too far from the nav target (pending cover spot)\n" );
		return nullptr;
	}

	const unsigned selectedEnemyInstanceId = Self()->GetSelectedEnemy().value().InstanceId();
	TakeCoverActionRecord *record = pool.New( Self(), *navTargetOrigin, selectedEnemyInstanceId );
	
	PlannerNode *const plannerNode = newNodeForRecord( record, worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	// Bot origin var remains the same (it is close to nav target)

	plannerNode->worldState.clearVec3( WorldState::PendingCoverSpot );

	plannerNode->worldState.setBool( WorldState::IsRunningAway, false );
	plannerNode->worldState.setBool( WorldState::HasRunAway, true );
	plannerNode->worldState.setBool( WorldState::CanHitEnemy, false );
	plannerNode->worldState.setBool( WorldState::EnemyCanHit, false );

	return plannerNode;
}