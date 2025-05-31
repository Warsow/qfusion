#include "planninglocal.h"
#include "../bot.h"

PlannerNode *KillEnemyAction::TryApply( const WorldState &worldState ) {
	if( !worldState.getVec3( WorldState::EnemyOrigin ) ) {
		Debug( "Enemy is ignored in the given world state\n" );
		return nullptr;
	}
	if( !isSpecifiedAndTrue( worldState.getBool( WorldState::HasPositionalAdvantage ) ) ) {
		Debug( "Bot does not have positional advantage in the given world state\n" );
		return nullptr;
	}
	if( !isSpecifiedAndTrue( worldState.getBool( WorldState::CanHitEnemy ) ) ) {
		Debug( "Bot can't hit enemy in the given world state\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self() ), worldState, 1.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	const unsigned stateDistinctionId = Self()->NextSimilarWorldStateInstanceId();

	plannerNode->worldState.setBool( WorldState::HasJustKilledEnemy, true );
	plannerNode->worldState.setUInt( WorldState::SimilarWorldStateInstanceId, stateDistinctionId );

	return plannerNode;
}