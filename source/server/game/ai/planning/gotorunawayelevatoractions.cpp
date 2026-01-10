#include "planninglocal.h"
#include "../bot.h"
#include "../groundtracecache.h"

PlannerNode *StartGotoRunAwayElevatorAction::TryApply( const WorldState &worldState ) {
	if( !checkCommonPreconditionsForStartingRunningAway( worldState ) ) {
		return nullptr;
	}

	if( worldState.getVec3( WorldState::PendingElevatorDest ) ) {
		Debug( "Pending origin is already present in the given world state\n" );
		return nullptr;
	}

	const Vec3 enemyOrigin = worldState.getVec3( WorldState::EnemyOrigin ).value();
	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	std::optional<DualOrigin> maybeFromTo = module->tacticalSpotsCache.getRunAwayElevatorOrigin( botOrigin, enemyOrigin );
	if( !maybeFromTo ) {
		Debug( "Failed to find a (cached) runaway elevator\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::NavTargetOrigin, maybeFromTo->first );
	plannerNode->worldState.setVec3( WorldState::PendingElevatorDest, maybeFromTo->second );

	return plannerNode;
}

void DoRunAwayViaElevatorActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->SetNavTarget( &navSpot );
}

void DoRunAwayViaElevatorActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status DoRunAwayViaElevatorActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	// Checking of this action record differs from other run away action record.
	// We want the bot to stand on a platform until it finishes its movement.

	// We do not want to invalidate an action due to being a bit in air above the platform, don't check self->groundentity
	trace_t selfTrace;
	const edict_t *ent = game.edicts + Self()->EntNum();
	AiGroundTraceCache::Instance()->GetGroundTrace( ent, 64.0f, &selfTrace );

	if( selfTrace.fraction == 1.0f ) {
		Debug( "Bot is too high above the ground (if any)\n" );
		return INVALID;
	}
	if( selfTrace.ent <= ggs->maxclients || game.edicts[selfTrace.ent].use != Use_Plat ) {
		Debug( "Bot is not above a platform\n" );
		return INVALID;
	}

	// If there are no valid enemies, just keep standing on the platform
	if( const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy() ) {
		trace_t enemyTrace;
		AiGroundTraceCache::Instance()->GetGroundTrace( selectedEnemy->Ent(), 128.0f, &enemyTrace );
		if( enemyTrace.fraction != 1.0f && enemyTrace.ent == selfTrace.ent ) {
			Debug( "Enemy is on the same platform!\n" );
			return INVALID;
		}
	}

	if( game.edicts[selfTrace.ent].moveinfo.state == STATE_TOP ) {
		return COMPLETED;
	}

	return VALID;
}

PlannerNode *DoRunAwayViaElevatorAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> pendingOrigin( worldState.getVec3( WorldState::PendingElevatorDest ) );
	if( !pendingOrigin ) {
		Debug( "The pending elevator dest origin is missing in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin       = worldState.getVec3( WorldState::BotOrigin ).value();
	const Vec3 navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin ).value();

	if( botOrigin.fastDistanceTo( navTargetOrigin ) > GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is too far from the nav target (elevator origin)\n" );
		return nullptr;
	}

	const Vec3 &elevatorOrigin = navTargetOrigin;
	unsigned selectedEnemyInstanceId = Self()->GetSelectedEnemy().value().InstanceId();

	const float elevatorDistance = ( elevatorOrigin - *pendingOrigin ).fastLength();
	// Assume that elevator speed is 400 units per second
	const float speedInUnitsPerMillis = 400 / 1000.0f;
	const float cost = elevatorDistance * Q_Rcp( speedInUnitsPerMillis );

	DoRunAwayViaElevatorActionRecord *record = pool.New( Self(), elevatorOrigin, selectedEnemyInstanceId );
	
	PlannerNode *const plannerNode = newNodeForRecord( record, worldState, cost );
	if( !plannerNode ) {
		return nullptr;
	}

	// Set bot origin to the elevator destination
	plannerNode->worldState.setVec3( WorldState::BotOrigin, *pendingOrigin );
	// Reset pending origin
	plannerNode->worldState.clearVec3( WorldState::PendingElevatorDest );

	plannerNode->worldState.setBool( WorldState::IsRunningAway, true );
	plannerNode->worldState.setBool( WorldState::CanHitEnemy, false );
	plannerNode->worldState.setBool( WorldState::EnemyCanHit, false );

	return plannerNode;
}