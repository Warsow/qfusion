#include "planninglocal.h"
#include "../bot.h"

bool RunAwayAction::checkCommonPreconditionsForStartingRunningAway( const WorldState &worldState ) const {
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasRunAway ) ) ) {
		Debug( "Bot has already run away in the given world state\n" );
		return false;
	}
	if( isSpecifiedAndTrue( worldState.getBool( WorldState::IsRunningAway ) ) ) {
		Debug( "Bot is already running away in the given world state\n" );
		return false;
	}

	const std::optional<Vec3> botOrigin = worldState.getVec3( WorldState::BotOrigin );
	if( !botOrigin ) {
		Debug( "Weird world state - missing bot origin\n" );
		return false;
	}

	const std::optional<Vec3> enemyOriginVar = worldState.getVec3( WorldState::EnemyOrigin );
	if( !enemyOriginVar ) {
		Debug( "Enemy is ignored in the given world state\n" );
		return false;
	}

	if( botOrigin->squareDistanceTo( Self()->Origin() ) > 1.0f ) {
		Debug( "This action is only applicable to the current world state\n" );
		return false;
	}

	const float offensiveness = Self()->GetEffectiveOffensiveness();
	if( offensiveness == 1.0f ) {
		Debug( "The effective offensiveness does not permit running away\n" );
		return false;
	}

	const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy();
	assert( selectedEnemy );
	if( selectedEnemy->HasQuad() ) {
		if( const auto *inventory = Self()->Inventory(); !inventory[POWERUP_QUAD] && !inventory[POWERUP_SHELL] ) {
			return true;
		}
	}

	if( isSpecifiedAndTrue( worldState.getBool( WorldState::HasThreateningEnemy ) ) && DamageToBeKilled() < 50 ) {
		return true;
	}

	const float distanceToEnemy = botOrigin->fastDistanceTo( *enemyOriginVar );
	if( distanceToEnemy > kLasergunRange ) {
		/*
		 * TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		if( !worldState.EnemyHasGoodSniperRangeWeaponsVar() && !worldState.EnemyHasGoodFarRangeWeaponsVar() ) {
			Debug( "Enemy does not have good sniper range weapons and thus taking cover makes no sense\n" );
			return false;
		}*/
		if( DamageToBeKilled() > 80 ) {
			Debug( "Bot can resist more than 80 damage units on sniper range and thus taking cover makes no sense\n" );
			return false;
		}
		return true;
	}

	if( distanceToEnemy > 0.33f * kLasergunRange ) {
		if( !CheckMiddleRangeKDDamageRatio( worldState ) ) {
			Debug( "The additional checks do not suggest running away on the middle range\n" );
			return false;
		}
		return true;
	}

	if( !CheckCloseRangeKDDamageRatio( worldState ) ) {
		Debug( "The additional checks do not suggest running away on the close range\n" );
		return false;
	}
	return true;
}

bool RunAwayAction::CheckMiddleRangeKDDamageRatio( const WorldState &worldState ) const {
	float offensiveness = Self()->GetEffectiveOffensiveness();
	/* TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if( worldState.HasThreateningEnemyVar() ) {
		if( worldState.HasGoodMiddleRangeWeaponsVar() ) {
			if( worldState.KillToBeKilledDamageRatio() < 1.0f + 1.0f * offensiveness ) {
				return false;
			}
		} else {
			if( worldState.KillToBeKilledDamageRatio() < 0.75f + 0.5f * offensiveness ) {
				return false;
			}
		}
		return true;
	}

	if( worldState.HasGoodMiddleRangeWeaponsVar() ) {
		if( worldState.KillToBeKilledDamageRatio() < 1.5f + 3.0f * offensiveness ) {
			return false;
		}
	}*/

	return KillToBeKilledDamageRatio() > 1.5f + 1.5f * offensiveness;
}

bool RunAwayAction::CheckCloseRangeKDDamageRatio( const WorldState &worldState ) const {
	float offensiveness = Self()->GetEffectiveOffensiveness();
	/* TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if( worldState.HasThreateningEnemyVar() ) {
		if( worldState.HasGoodCloseRangeWeaponsVar() ) {
			if( worldState.KillToBeKilledDamageRatio() < 1.0f + 1.0f * offensiveness ) {
				return false;
			}
		} else {
			if( worldState.KillToBeKilledDamageRatio() < 0.5f + 0.5f * offensiveness ) {
				return false;
			}
		}
		return true;
	}*/

	return KillToBeKilledDamageRatio() > 2.0f + 1.0f * offensiveness;
}