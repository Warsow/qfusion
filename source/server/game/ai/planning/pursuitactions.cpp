#include "planninglocal.h"
#include "../bot.h"

PlannerNode *StartLostEnemyPursuitAction::TryApply( const WorldState &worldState ) {
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::IsReactingToEnemyLost ) ) ) {
		Debug( "Bot is already reacting to enemy lost in the given world state\n" );
		return nullptr;
	}
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasReactedToEnemyLost ) ) ) {
		Debug( "Bot has already reacted to enemy lost in the given world state\n" );
		return nullptr;
	}

	const std::optional<Vec3> lostEnemyOrigin = worldState.getVec3( WorldState::LostEnemyLastSeenOrigin );
	if( !lostEnemyOrigin ) {
		Debug( "Lost enemy origin is ignored in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	const float distanceToEnemy = botOrigin.fastDistanceTo( *lostEnemyOrigin );
	if( distanceToEnemy < GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Bot is already close to the last seen enemy origin\n" );
		return nullptr;
	}

	// Vary pursuit max distance threshold depending on offensiveness.
	// Never pursue enemies farther than LG range (otherwise a poor bot behaviour is observed).
	const float maxDistanceThreshold = 96.0f + ( kLasergunRange - 96.0f ) * Self()->GetEffectiveOffensiveness();
	if( distanceToEnemy > maxDistanceThreshold ) {
		Debug( "The enemy is way too far for pursuing it\n" );
		return nullptr;
	}

	if( ( botOrigin - Self()->Origin() ).squareLength() > 1.0f ) {
		Debug( "The action can be applied only to the current bot origin\n" );
		return nullptr;
	}

	// TODO: Query in lazy fashion?
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::MightSeeLostEnemyAfterTurn ) ) ) {
		Debug( "Bot might see lost enemy after turn, pursuit would be pointless\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setBool( WorldState::IsReactingToEnemyLost, true );
	plannerNode->worldState.setVec3( WorldState::NavTargetOrigin, *lostEnemyOrigin );

	return plannerNode;
}

PlannerNode *StopLostEnemyPursuitAction::TryApply( const WorldState &worldState ) {
	if( isUnspecifiedOrFalse( worldState.getBool( WorldState::IsReactingToEnemyLost ) ) ) {
		Debug( "Bot is not reacting to enemy lost in the given world state\n" );
		return nullptr;
	}
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasReactedToEnemyLost ) ) ) {
		Debug( "Bot has already reacted to enemy lost in the given world state\n" );
		return nullptr;
	}

	const std::optional<Vec3> lostEnemyOrigin = worldState.getVec3( WorldState::LostEnemyLastSeenOrigin );
	if( !lostEnemyOrigin ) {
		Debug( "Lost enemy origin is ignored in the given world state\n" );
		return nullptr;
	}

	const std::optional<Vec3> navTargetOrigin = worldState.getVec3( WorldState::NavTargetOrigin );
	if( !navTargetOrigin ) {
		Debug( "Nav target origin is ignored in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();

	if( lostEnemyOrigin->squareDistanceTo( *navTargetOrigin ) > 1.0f ) {
		Debug( "The lost enemy origin does not match nav target in the given world state\n" );
		return nullptr;
	}

	if( botOrigin.squareDistanceTo( *navTargetOrigin ) > wsw::square( GOAL_PICKUP_ACTION_RADIUS ) ) {
		Debug( "The bot is way too far from nav target in the given world state\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.clearVec3( WorldState::NavTargetOrigin );
	plannerNode->worldState.setBool( WorldState::HasReactedToEnemyLost, true );
	plannerNode->worldState.setBool( WorldState::IsReactingToEnemyLost, false );

	return plannerNode;
}