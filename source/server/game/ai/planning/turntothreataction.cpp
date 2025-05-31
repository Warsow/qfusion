#include "planninglocal.h"
#include "../bot.h"

void TurnToThreatOriginActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->SetPendingLookAtPoint( AiPendingLookAtPoint( threatPossibleOrigin, 3.0f ), 350 );
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
}

void TurnToThreatOriginActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetPendingLookAtPoint();
}

AiActionRecord::Status TurnToThreatOriginActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	const edict_t *ent = game.edicts + Self()->EntNum();

	vec3_t lookDir;
	AngleVectors( ent->s.angles, lookDir, nullptr, nullptr );

	Vec3 toThreatDir( threatPossibleOrigin );
	toThreatDir -= ent->s.origin;
	if( !toThreatDir.normalizeFast() ) {
		return COMPLETED;
	}

	if( toThreatDir.Dot( lookDir ) > Self()->FovDotFactor() ) {
		return COMPLETED;
	}

	return Self()->HasPendingLookAtPoint() ? VALID : INVALID;
}

PlannerNode *TurnToThreatOriginAction::TryApply( const WorldState &worldState ) {
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasReactedToThreat ) ) ) {
		Debug( "Bot has already reacted to threat in the given world state\n" );
		return nullptr;
	}

	const std::optional<Vec3> threatOrigin = worldState.getVec3( WorldState::ThreatPossibleOrigin );
	if( !threatOrigin ) {
		Debug( "Threat possible origin is ignored in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( botOrigin.FastDistanceTo( Self()->Origin() ) > 1.0f ) {
		Debug( "The action can be applied only to the current bot origin\n" );
		return nullptr;
	}

	PlannerNode *const plannerNode = newNodeForRecord( pool.New( Self(), *threatOrigin ), worldState, 500.0f );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setBool( WorldState::HasReactedToThreat, true );
	// If a bot has reacted to threat, he can't hit current enemy (if any)
	plannerNode->worldState.setBool( WorldState::CanHitEnemy, false );

	return plannerNode;
}