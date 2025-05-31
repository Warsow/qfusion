#include "weaponselector.h"
#include "../bot.h"
#include "../planning/planninglocal.h" // ???

void BotWeaponSelector::Frame() {
	if( nextFastWeaponSwitchActionCheckAt > level.time ) {
		return;
	}

	if( !bot->m_selectedEnemy ) {
		return;
	}

	// Disallow "fast weapon switch actions" while a bot has quad.
	// The weapon balance and usage is completely different for a quad bearer.
	if( bot->self->r.client->ps.inventory[POWERUP_QUAD] ) {
		return;
	}

	if( checkFastWeaponSwitchAction() ) {
		nextFastWeaponSwitchActionCheckAt = level.time + 750;
	}
}

void BotWeaponSelector::Think() {
	if( bot->weaponsUsageModule.GetSelectedWeapons().AreValid() ) {
		return;
	}

	if( !bot->m_selectedEnemy ) {
		return;
	}

	if( weaponChoiceRandomTimeoutAt <= level.time ) {
		weaponChoiceRandom = random();
		weaponChoiceRandomTimeoutAt = level.time + 2000;
	}

	selectWeapon();
}

bool BotWeaponSelector::checkFastWeaponSwitchAction() {
	if( game.edicts[bot->EntNum()].r.client->ps.stats[STAT_WEAPON_TIME] >= 64 ) {
		return false;
	}

	// Easy bots do not perform fast weapon switch actions
	if( bot->Skill() < 0.33f ) {
		return false;
	}

	// Disallow in this case.
	// We don't care if the actual var string is invalid (this allows us to efficiently disable the action).
	if( sv_cheats->integer && !v_forceWeapon.get().empty() ) {
		return false;
	}

	if( bot->Skill() < 0.66f ) {
		// Mid-skill bots do these actions in non-think frames occasionally
		if( !bot->PermitsDistributedUpdateThisFrame() && random() > bot->Skill() ) {
			return false;
		}
	}

	if( auto maybeChosenWeapon = suggestFinishWeapon() ) {
		setSelectedWeapons( WeaponsToSelect::bultinOnly( *maybeChosenWeapon ), 100u );
		return true;
	}

	return false;
}

// TODO: Lift this to Bot?
bool BotWeaponSelector::hasWeakOrStrong( int weapon ) const {
	assert( weapon >= WEAP_NONE && weapon < WEAP_TOTAL );
	const auto *inventory = bot->Inventory();
	if( !inventory[weapon] ) {
		return false;
	}
	const int weaponShift = weapon - WEAP_GUNBLADE;
	return ( inventory[AMMO_GUNBLADE + weaponShift] | inventory[AMMO_WEAK_GUNBLADE + weaponShift] ) != 0;
}

void BotWeaponSelector::selectWeapon() {
	const float distanceToEnemy = bot->GetSelectedEnemy()->ActualOrigin().FastDistanceTo( bot->Origin() );
	const auto timeout = weaponChoicePeriod;
	// Use instagib selection code for quad bearers as well
	// TODO: Select script weapon too
	if( GS_Instagib( *ggs ) || bot->self->r.client->ps.inventory[POWERUP_QUAD] ) {
		if( auto maybeBuiltinWeapon = suggestInstagibWeapon( distanceToEnemy ) ) {
			setSelectedWeapons( WeaponsToSelect::bultinOnly( *maybeBuiltinWeapon ), timeout );
		}
		// TODO: Report failure/replan
		return;
	}

	std::optional<int> maybeBuiltinWeapon;
	if( sv_cheats->integer ) {
		if( const wsw::StringView &forceWeapon = v_forceWeapon.get(); !forceWeapon.empty() ) {
			assert( forceWeapon.isZeroTerminated() );
			if( const gsitem_t *item = GS_FindItemByName( ggs, forceWeapon.data() ) ) {
				if( item->tag >= WEAP_GUNBLADE && item->tag < WEAP_TOTAL ) {
					const auto *const inventory = bot->Inventory();
					if( inventory[item->tag] && ( inventory[item->weakammo_tag] + inventory[item->ammo_tag] > 0 ) ) {
						maybeBuiltinWeapon = item->tag;
					}
				}
			}
		}
	}

	if( !maybeBuiltinWeapon ) {
		if( distanceToEnemy > 2.0f * kLasergunRange ) {
			maybeBuiltinWeapon = suggestSniperRangeWeapon( distanceToEnemy );
		} else if( distanceToEnemy > kLasergunRange ) {
			maybeBuiltinWeapon = suggestFarRangeWeapon( distanceToEnemy );
		} else if( distanceToEnemy > 0.33f * kLasergunRange ) {
			maybeBuiltinWeapon = suggestMiddleRangeWeapon( distanceToEnemy );
		} else {
			maybeBuiltinWeapon = suggestCloseRangeWeapon( distanceToEnemy );
		}
	}

	// TODO: Report failure/replan
	if( !maybeBuiltinWeapon ) {
		return;
	}

	const auto bultinNum = *maybeBuiltinWeapon;
	if( auto maybeScriptWeaponAndTier = suggestScriptWeapon( distanceToEnemy ) ) {
		auto [scriptNum, tier] = *maybeScriptWeaponAndTier;
		if( tier >= BuiltinWeaponTier( *maybeBuiltinWeapon ) ) {
			setSelectedWeapons( WeaponsToSelect::builtinAndPrimaryScript( bultinNum, scriptNum ), timeout );
		} else {
			setSelectedWeapons( WeaponsToSelect::builtinAndSecondaryScript( bultinNum, scriptNum ), timeout );
		}
	} else {
		setSelectedWeapons( WeaponsToSelect::bultinOnly( bultinNum ), timeout );
	}
}

auto BotWeaponSelector::suggestFarOrSniperPositionalCombatWeapon( bool hasEB, bool hasMG ) -> std::optional<int> {
	if( bot->WillAdvance() || bot->WillRetreat() ) {
		return std::nullopt;
	}

	const float dodgeThreshold = DEFAULT_DASHSPEED + 10.0f;
	if( bot->EntityPhysicsState()->Speed() > dodgeThreshold ) {
		return std::nullopt;
	}

	if( bot->m_selectedEnemy->ActualVelocity().SquaredLength() > dodgeThreshold * dodgeThreshold ) {
		return std::nullopt;
	}

	auto enemyWeapon = bot->m_selectedEnemy->PendingWeapon();
	// Try to counter enemy EB/MG by a complementary weapon
	if( enemyWeapon == WEAP_MACHINEGUN ) {
		if( hasEB ) {
			return WEAP_ELECTROBOLT;
		}
	} else if( enemyWeapon == WEAP_ELECTROBOLT ) {
		if( hasMG ) {
			return WEAP_MACHINEGUN;
		}
	}

	return std::nullopt;
}

static constexpr float kSwitchToMGForFinishingHP = 40.0f;

auto BotWeaponSelector::suggestSniperRangeWeapon( float distanceToEnemy ) -> std::optional<int> {
	const bool hasMG = hasWeakOrStrong( WEAP_MACHINEGUN );
	const float damageToKill = bot->GetSelectedEnemy()->DamageToKill();
	if( damageToKill < kSwitchToMGForFinishingHP ) {
		if( hasMG ) {
			return WEAP_MACHINEGUN;
		}
	} else if( damageToKill < 0.5 * kSwitchToMGForFinishingHP ) {
		if( hasWeakOrStrong( WEAP_RIOTGUN ) ) {
			return WEAP_RIOTGUN;
		}
	}

	const bool hasEB = hasWeakOrStrong( WEAP_ELECTROBOLT );
	const bool hasIG = hasWeakOrStrong( WEAP_INSTAGUN );
	if( auto maybeWeapon = suggestFarOrSniperPositionalCombatWeapon( hasEB, hasMG ) ) {
		return maybeWeapon;
	}

	if( hasIG ) {
		return WEAP_INSTAGUN;
	}
	if( hasEB ) {
		return WEAP_ELECTROBOLT;
	}
	if( hasMG ) {
		return WEAP_MACHINEGUN;
	}

	return std::nullopt;
}

auto BotWeaponSelector::suggestFarRangeWeapon( float distanceToEnemy ) -> std::optional<int> {
	const bool hasMG = hasWeakOrStrong( WEAP_MACHINEGUN );
	if( bot->GetSelectedEnemy()->DamageToKill() < kSwitchToMGForFinishingHP ) {
		if( hasMG ) {
			return WEAP_MACHINEGUN;
		}
		if( hasWeakOrStrong( WEAP_RIOTGUN ) ) {
			return WEAP_RIOTGUN;
		}
	}

	const bool hasIG = hasWeakOrStrong( WEAP_INSTAGUN );
	const bool hasEB = hasWeakOrStrong( WEAP_ELECTROBOLT );
	if( auto maybeWeapon = suggestFarOrSniperPositionalCombatWeapon( hasEB, hasMG ) ) {
		return maybeWeapon;
	}

	const bool hasPG = hasWeakOrStrong( WEAP_PLASMAGUN );
	const bool hasSW = hasWeakOrStrong( WEAP_SHOCKWAVE );
	if( bot->WillRetreat() ) {
		if( hasPG ) {
			return WEAP_PLASMAGUN;
		}
		if( hasSW ) {
			return WEAP_SHOCKWAVE;
		}
	}

	if( hasIG ) {
		return WEAP_INSTAGUN;
	}
	if( hasEB ) {
		return WEAP_ELECTROBOLT;
	}
	if( hasMG ) {
		return WEAP_MACHINEGUN;
	}

	if( bot->WillAdvance() ) {
		if( hasPG ) {
			return WEAP_PLASMAGUN;
		}
		if( hasSW ) {
			return WEAP_SHOCKWAVE;
		}
		if( hasWeakOrStrong( WEAP_ROCKETLAUNCHER ) ) {
			return WEAP_ROCKETLAUNCHER;
		}
	}

	// Using gunblade is fine in this case
	if( bot->WillRetreat() ) {
		const auto *inventory = bot->Inventory();
		return inventory[WEAP_GUNBLADE] && inventory[AMMO_GUNBLADE];
	}

	return std::nullopt;
}

auto BotWeaponSelector::suggestMiddleRangeWeapon( float distanceToEnemy ) -> std::optional<int> {
	const bool hasLG = hasWeakOrStrong( WEAP_LASERGUN );
	if( hasLG && bot->IsInSquad() ) {
		return WEAP_LASERGUN;
	}

	const bool hasRL = hasWeakOrStrong( WEAP_ROCKETLAUNCHER );
	if( distanceToEnemy < 0.5f * kLasergunRange || bot->WillAdvance() ) {
		if( hasRL ) {
			return WEAP_ROCKETLAUNCHER;
		}
	}

	if( distanceToEnemy > 0.5f * kLasergunRange || bot->WillRetreat() ) {
		if( hasLG ) {
			return WEAP_LASERGUN;
		}
	}

	// Drop any randomness of choice in fighting against enemy LG
	if( weaponChoiceRandom > 0.5f || bot->m_selectedEnemy->PendingWeapon() == WEAP_LASERGUN ) {
		if ( hasLG ) {
			return WEAP_LASERGUN;
		}
		if ( hasRL ) {
			return WEAP_ROCKETLAUNCHER;
		}
	}

	const bool hasPG = hasWeakOrStrong( WEAP_PLASMAGUN );
	if( hasPG && bot->WillAdvance() ) {
		return WEAP_PLASMAGUN;
	}

	const bool hasSW = hasWeakOrStrong( WEAP_SHOCKWAVE );
	if( hasSW && bot->WillRetreat() ) {
		return WEAP_SHOCKWAVE;
	}

	if( hasLG ) {
		return WEAP_LASERGUN;
	}
	if( hasRL ) {
		return WEAP_ROCKETLAUNCHER;
	}
	if( hasSW ) {
		return WEAP_SHOCKWAVE;
	}
	if( hasPG ) {
		return WEAP_PLASMAGUN;
	}

	const bool hasMG = hasWeakOrStrong( WEAP_MACHINEGUN );
	if( hasMG && bot->WillRetreat() ) {
		return WEAP_MACHINEGUN;
	}
	const bool hasRG = hasWeakOrStrong( WEAP_RIOTGUN );
	if( hasRG && bot->WillAdvance() ) {
		return WEAP_RIOTGUN;
	}

	const bool hasIG = hasWeakOrStrong( WEAP_INSTAGUN );
	const bool hasEB = hasWeakOrStrong( WEAP_ELECTROBOLT );
	if( distanceToEnemy > 0.5f * kLasergunRange && bot->WillRetreat() ) {
		if( hasIG ) {
			return WEAP_INSTAGUN;
		}
		if( hasEB ) {
			return WEAP_ELECTROBOLT;
		}
	}

	if( hasRG ) {
		return WEAP_RIOTGUN;
	}
	if( hasMG ) {
		return WEAP_MACHINEGUN;
	}
	if( hasIG ) {
		return WEAP_INSTAGUN;
	}
	if( hasEB ) {
		return WEAP_ELECTROBOLT;
	}

	if( distanceToEnemy > 0.5f * kLasergunRange && bot->WillRetreat() ) {
		if( hasWeakOrStrong( WEAP_GRENADELAUNCHER ) ) {
			return WEAP_GRENADELAUNCHER;
		}
	}

	const auto *inventory = bot->Inventory();
	if( inventory[WEAP_GUNBLADE] && inventory[AMMO_GUNBLADE] ) {
		return WEAP_GUNBLADE;
	}

	return std::nullopt;
}

auto BotWeaponSelector::suggestCloseRangeWeapon( float distanceToEnemy ) -> std::optional<int> {
	// TODO: Modify by powerups
	const float damageToBeKilled = DamageToKill( bot->Health(), bot->Armor() );

	bool tryAvoidingSelfDamage = false;
	if( GS_SelfDamage( *ggs ) && distanceToEnemy < 150.0f ) {
		if( damageToBeKilled < 100 || !( level.gametype.spawnableItemsMask & IT_HEALTH ) ) {
			tryAvoidingSelfDamage = true;
		}
	}

	const bool hasLG = hasWeakOrStrong( WEAP_LASERGUN );
	const bool hasPG = hasWeakOrStrong( WEAP_PLASMAGUN );
	const bool hasRG = hasWeakOrStrong( WEAP_RIOTGUN );
	if( tryAvoidingSelfDamage ) {
		if( hasLG ) {
			return WEAP_LASERGUN;
		}
		if( hasPG && distanceToEnemy > 72.0f ) {
			return WEAP_PLASMAGUN;
		}
		if( hasRG ) {
			return WEAP_RIOTGUN;
		}
	}

	if( hasWeakOrStrong( WEAP_ROCKETLAUNCHER ) ) {
		return WEAP_ROCKETLAUNCHER;
	}
	if( hasWeakOrStrong( WEAP_SHOCKWAVE ) ) {
		return WEAP_SHOCKWAVE;
	}

	if( hasLG ) {
		return WEAP_LASERGUN;
	}
	if( hasPG ) {
		return WEAP_PLASMAGUN;
	}
	if( hasRG ) {
		return WEAP_RIOTGUN;
	}

	if( hasWeakOrStrong( WEAP_MACHINEGUN ) ) {
		return WEAP_MACHINEGUN;
	}

	const auto *inventory = bot->Inventory();
	const bool hasGB = inventory[WEAP_GUNBLADE] && inventory[AMMO_GUNBLADE];
	if( hasGB ) {
		if( !tryAvoidingSelfDamage ) {
			return WEAP_GUNBLADE;
		}
		if( distanceToEnemy ) {
			return WEAP_GUNBLADE;
		}
	}

	if( hasWeakOrStrong( WEAP_INSTAGUN ) ) {
		return WEAP_INSTAGUN;
	}
	if( hasWeakOrStrong( WEAP_ELECTROBOLT ) ) {
		return WEAP_ELECTROBOLT;
	}

	if( !tryAvoidingSelfDamage && hasWeakOrStrong( WEAP_GRENADELAUNCHER ) ) {
		return WEAP_GRENADELAUNCHER;
	}

	if( hasGB ) {
		return WEAP_GUNBLADE;
	}

	return std::nullopt;
}

auto BotWeaponSelector::suggestScriptWeapon( float distanceToEnemy ) -> std::optional<std::pair<int, int>> {
	const auto &scriptWeaponDefs = bot->weaponsUsageModule.scriptWeaponDefs;
	const auto &scriptWeaponCooldown = bot->weaponsUsageModule.scriptWeaponCooldown;

	int effectiveTier = 0;
	float bestScore = 0.000001f;
	int bestWeaponNum = -1;

	for( unsigned i = 0; i < scriptWeaponDefs.size(); ++i ) {
		const auto &weaponDef = scriptWeaponDefs[i];
		int cooldown = scriptWeaponCooldown[i];
		if( cooldown >= 1000 ) {
			continue;
		}

		if( distanceToEnemy > weaponDef.maxRange ) {
			continue;
		}
		if( distanceToEnemy < weaponDef.minRange ) {
			continue;
		}

		float score = 1.0f;

		score *= 1.0f - BoundedFraction( cooldown, 1000.0f );

		if( GS_SelfDamage( *ggs ) ) {
			float estimatedSelfDamage = 0.0f;
			estimatedSelfDamage = weaponDef.maxSelfDamage;
			estimatedSelfDamage *= ( 1.0f - BoundedFraction( distanceToEnemy, weaponDef.splashRadius ) );
			if( estimatedSelfDamage > 100.0f ) {
				continue;
			}
			if( distanceToEnemy < estimatedSelfDamage ) {
				continue;
			}
			score *= 1.0f - BoundedFraction( estimatedSelfDamage, 100.0f );
		}

		// We assume that maximum ordinary tier is 3
		score *= weaponDef.tier / 3.0f;

		// Treat points in +/- 192 units of best range as in best range too
		float bestRangeLowerBounds = weaponDef.bestRange - wsw::min( 192.0f, weaponDef.bestRange - weaponDef.minRange );
		float bestRangeUpperBounds = weaponDef.bestRange + wsw::min( 192.0f, weaponDef.maxRange - weaponDef.bestRange );

		if( distanceToEnemy < bestRangeLowerBounds ) {
			score *= distanceToEnemy / bestRangeLowerBounds;
		} else if( distanceToEnemy > bestRangeUpperBounds ) {
			score *= ( distanceToEnemy - bestRangeUpperBounds ) / ( weaponDef.maxRange - bestRangeLowerBounds );
		}

		if( score > bestScore ) {
			bestScore = score;
			bestWeaponNum = (int)i;
			effectiveTier = (int)( score * 3.0f + 0.99f );
		}
	}

	return bestWeaponNum < 0 ? std::nullopt : std::make_optional( std::make_pair( bestWeaponNum, effectiveTier ) );
}

auto BotWeaponSelector::suggestInstagibWeapon( float distanceToEnemy ) -> std::optional<int> {
	const bool hasMG = hasWeakOrStrong( WEAP_MACHINEGUN );
	const bool hasRG = hasWeakOrStrong( WEAP_RIOTGUN );
	const bool hasPG = hasWeakOrStrong( WEAP_PLASMAGUN );
	const bool hasSW = hasWeakOrStrong( WEAP_SHOCKWAVE );
	const bool hasIG = hasWeakOrStrong( WEAP_INSTAGUN );
	const bool hasEB = hasWeakOrStrong( WEAP_ELECTROBOLT );
	const auto *const inventory = bot->Inventory();
	if( distanceToEnemy > kLasergunRange ) {
		if( hasMG ) {
			return WEAP_MACHINEGUN;
		}
		if( hasRG ) {
			return WEAP_RIOTGUN;
		}

		if( distanceToEnemy < 2.0f * kLasergunRange && bot->WillAdvance() ) {
			if( hasPG ) {
				return WEAP_PLASMAGUN;
			}
			if( inventory[WEAP_GUNBLADE] && inventory[AMMO_GUNBLADE] ) {
				return WEAP_GUNBLADE;
			}
			if( hasSW ) {
				return WEAP_SHOCKWAVE;
			}
		}

		if( hasIG ) {
			return WEAP_INSTAGUN;
		}
		if( hasEB ) {
			return WEAP_ELECTROBOLT;
		}

		return std::nullopt;
	}

	if( hasWeakOrStrong( WEAP_LASERGUN ) ) {
		return WEAP_LASERGUN;
	}
	if( hasPG ) {
		return WEAP_PLASMAGUN;
	}
	if( hasMG ) {
		return WEAP_MACHINEGUN;
	}
	if( hasRG ) {
		return WEAP_RIOTGUN;
	}

	const bool hasRL = hasWeakOrStrong( WEAP_ROCKETLAUNCHER );
	if( distanceToEnemy > 0.5f * kLasergunRange || !GS_SelfDamage( *ggs ) ) {
		if ( hasSW ) {
			return WEAP_SHOCKWAVE;
		}
		if ( hasRL ) {
			return WEAP_ROCKETLAUNCHER;
		}
	}

	if( inventory[WEAP_GUNBLADE] && inventory[AMMO_GUNBLADE] ) {
		return WEAP_GUNBLADE;
	}

	if( hasIG ) {
		return WEAP_INSTAGUN;
	}
	if( hasEB ) {
		return WEAP_ELECTROBOLT;
	}

	return std::nullopt;
}

auto BotWeaponSelector::suggestFinishWeapon() -> std::optional<int> {
	const auto &selectedEnemy = bot->GetSelectedEnemy();
	if( !selectedEnemy ) {
		return std::nullopt;
	}

	const float damageToKill = selectedEnemy->DamageToKill();
	if( damageToKill > 50 ) {
		return std::nullopt;
	}

	const float damageToBeKilled = DamageToKill( (float)bot->Health(), (float)bot->Armor() );
	const float distanceToEnemy  = selectedEnemy->ActualOrigin().FastDistanceTo( bot->Origin() );

	const auto *const inventory  = bot->Inventory();
	if( distanceToEnemy < 0.33f * kLasergunRange ) {
		if( inventory[WEAP_GUNBLADE] && inventory[AMMO_WEAK_GUNBLADE] ) {
			if( damageToBeKilled > 0 && distanceToEnemy > 1.0f && distanceToEnemy < 64.0f ) {
				Vec3 dirToEnemy( selectedEnemy->ActualOrigin() );
				dirToEnemy *= Q_Rcp( distanceToEnemy );
				Vec3 lookDir( bot->EntityPhysicsState()->ForwardDir() );
				if ( lookDir.Dot( dirToEnemy ) > 0.7f ) {
					return WEAP_GUNBLADE;
				}
			}
		}

		if( damageToKill < 30 ) {
			if( hasWeakOrStrong( WEAP_LASERGUN ) ) {
				return WEAP_LASERGUN;
			}
		}

		if( inventory[WEAP_RIOTGUN] && inventory[AMMO_SHELLS] ) {
			return WEAP_RIOTGUN;
		}

		if( !GS_SelfDamage( *ggs ) ) {
			bool canRefillHealth = level.gametype.spawnableItemsMask & IT_HEALTH;
			if( canRefillHealth && ( damageToBeKilled > 150.0f || inventory[POWERUP_SHELL] ) ) {
				if ( hasWeakOrStrong( WEAP_ROCKETLAUNCHER ) ) {
					return WEAP_ROCKETLAUNCHER;
				}
			}
		}

		if( inventory[WEAP_RIOTGUN] && inventory[AMMO_WEAK_SHELLS] ) {
			return WEAP_RIOTGUN;
		}

		return std::nullopt;
	}

	if( distanceToEnemy > 0.33f * kLasergunRange && distanceToEnemy < kLasergunRange ) {
		if( damageToBeKilled > 75 ) {
			if( hasWeakOrStrong( WEAP_LASERGUN ) ) {
				return WEAP_LASERGUN;
			}
		}
		return std::nullopt;
	}

	if( damageToKill < 30 ) {
		if( hasWeakOrStrong( WEAP_RIOTGUN ) ) {
			return WEAP_RIOTGUN;
		}
	}

	return std::nullopt;
}
