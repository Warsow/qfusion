#include "planninglocal.h"
#include "../bot.h"

void DodgeToSpotActionRecord::Activate() {
	BotActionRecord::Activate();
	Self()->SetNavTarget( &navSpot );
	timeoutAt = level.time + Hazard::TIMEOUT;
	Self()->GetMiscTactics().PreferAttackRatherThanRun();
}

void DodgeToSpotActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status DodgeToSpotActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	// If the bot has reached the spot, consider the action completed
	// (use a low threshold because dodging is a precise movement)
	if( ( navSpot.Origin() - Self()->Origin() ).squareLength() < 16 * 16 ) {
		return COMPLETED;
	}

	// Return INVALID if has not reached the spot when the action timed out
	return timeoutAt > level.time ? VALID : INVALID;
}

PlannerNode *DodgeToSpotAction::TryApply( const WorldState &worldState ) {
	if( !worldState.getFloat( WorldState::PotentialHazardDamage ) ) {
		Debug( "Potential hazard damage is ignored in the given world state\n" );
		return nullptr;
	}

	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasReactedToHazard ) ) ) {
		Debug( "Has already reacted to hazard in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	const float *actualOrigin = Self()->Origin();
	if( botOrigin.distanceTo( actualOrigin ) >= 1.0f ) {
		Debug( "The action can be applied only to the current bot origin\n" );
		return nullptr;
	}

	const Hazard *hazard = Self()->PrimaryHazard();
	assert( hazard && hazard->IsValid() );

	std::optional<Vec3> spotOrigin = module->tacticalSpotsCache.getDodgeHazardSpot( botOrigin, hazard->hitPoint,
																					hazard->direction,
																					hazard->splashRadius > 0 );

	if( !spotOrigin ) {
		Debug( "Failed to find a dodge hazard spot for the given world state\n" );
		return nullptr;
	}

	const int travelTimeMillis = Self()->CheckTravelTimeMillis( botOrigin, *spotOrigin );
	if( !travelTimeMillis ) {
		Debug( "Warning: can't find travel time from the bot origin to the spot origin in the given world state\n" );
		return nullptr;
	}
	
	DodgeToSpotActionRecord *record = pool.New( Self(), *spotOrigin );

	PlannerNode *const plannerNode = newNodeForRecord( record, worldState, (float)travelTimeMillis );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::BotOrigin, *spotOrigin );
	plannerNode->worldState.setBool( WorldState::HasReactedToHazard, true );
	plannerNode->worldState.clearFloat( WorldState::PotentialHazardDamage );

	return plannerNode;
}
