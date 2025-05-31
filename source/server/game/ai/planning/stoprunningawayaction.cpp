#include "planninglocal.h"
#include "../bot.h"

PlannerNode *StopRunningAwayAction::TryApply( const WorldState &worldState ) {
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::IsRunningAway ) ) ) {
		Debug( "Bot is not running away in the given world state\n" );
		return nullptr;
	}
	if( !isSpecifiedAndTrue( worldState.getBool( WorldState::HasRunAway ) ) ) {
		Debug( "Bot has already run away in the given world state\n" );
		return nullptr;
	}
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::EnemyCanHit ) ) ) {
		Debug( "Enemy still can hit in the given world state\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode( newNodeForRecord( pool.New( Self() ), worldState, 1.0f ) );
	if( !plannerNode ) {
		return nullptr;
	}

	const unsigned stateDistinctionId = Self()->NextSimilarWorldStateInstanceId();

	plannerNode->worldState.setBool( WorldState::IsRunningAway, false );
	plannerNode->worldState.setBool( WorldState::HasRunAway, true );
	plannerNode->worldState.setUInt( WorldState::SimilarWorldStateInstanceId, stateDistinctionId );

	return plannerNode;
}