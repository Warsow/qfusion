#include "itemsselector.h"
#include "../bot.h"
#include "../teamplay/objectivebasedteam.h"

#include <algorithm>

void BotItemsSelector::UpdateInternalItemAndGoalWeights() {
	memset( internalEntityWeights, 0, sizeof( internalEntityWeights ) );
	memset( internalPickupGoalWeights, 0, sizeof( internalPickupGoalWeights ) );

	const edict_t *self = game.edicts + bot->EntNum();

	const auto *const botTeam = AiBaseTeam::GetTeamForNum( self->s.team );
	const auto levelTime = level.time;
	auto *const navEntitiesRegistry = NavEntitiesRegistry::Instance();

	// Check whether we can skip not so cheap AiBaseTeam::GetEntityWeights() calls
	if( !botTeam->OverridesEntityWeights( bot ) ) {
		for( const NavEntity *goalEnt = navEntitiesRegistry->Head(); goalEnt; goalEnt = goalEnt->Next()) {
			float &navWeight = internalEntityWeights[goalEnt->Id()];
			float &goalWeight = internalPickupGoalWeights[goalEnt->Id()];
			if( disabledForSelectionUntil[goalEnt->Id()] >= levelTime ) {
				continue;
			}

			if( !goalEnt->Item() ) {
				continue;
			}

			ItemAndGoalWeights weights = ComputeItemWeights( goalEnt->Item() );
			std::tie( navWeight, goalWeight ) = std::make_pair( weights.itemWeight, weights.goalWeight );
		}
		return;
	}

	for( const NavEntity *goalEnt = navEntitiesRegistry->Head(); goalEnt; goalEnt = goalEnt->Next()) {
		float &navWeight = internalEntityWeights[goalEnt->Id()];
		float &goalWeight = internalPickupGoalWeights[goalEnt->Id()];
		// Do not even try to compute a weight for the disabled item
		if( disabledForSelectionUntil[goalEnt->Id()] >= levelTime ) {
			continue;
		}

		if( goalEnt->Item() ) {
			ItemAndGoalWeights weights = ComputeItemWeights( goalEnt->Item() );
			std::tie( navWeight, goalWeight ) = std::make_pair( weights.itemWeight, weights.goalWeight );
			continue;
		}

		const auto *navAndGoalWeight = botTeam->GetEntityWeights( bot, goalEnt );
		if( navAndGoalWeight ) {
			std::tie( navWeight, goalWeight ) = *navAndGoalWeight;
			continue;
		}
	}
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputeItemWeights( const gsitem_t *item ) const {
	switch( item->type ) {
		case IT_WEAPON: return ComputeWeaponWeights( item );
		case IT_AMMO: return ComputeAmmoWeights( item );
		case IT_HEALTH: return ComputeHealthWeights( item );
		case IT_ARMOR: return ComputeArmorWeights( item );
		case IT_POWERUP: return ComputePowerupWeights( item );
	}

	// Collect ammo packs too.
	// Checking an actual pack contents might sound better, but:
	// 1) It complicates the item selection code that is likely to be reworked anyway.
	// 2) It adds some degree of cheating (a bot knows exact pack contents in this case)
	if( item->tag == AMMO_PACK || item->tag == AMMO_PACK_STRONG || item->tag == AMMO_PACK_WEAK ) {
		// These weights are relatively large for this kind of item,
		// but we guess ammo packs are valuable in gametypes where they might be dropped.
		return ItemAndGoalWeights( 0.75f, 0.75f );
	}

	return ItemAndGoalWeights( 0, 0 );
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputeWeaponWeights( const gsitem_t *item ) const {
	const auto *const inventory = game.edicts[bot->EntNum()].r.client->ps.inventory;

	if( inventory[item->tag] ) {
		// TODO: Precache
		const gsitem_t *ammo = GS_FindItemByTag( item->ammo_tag );
		if( inventory[ammo->tag] >= ammo->inventory_max ) {
			return ItemAndGoalWeights( 0, 0 );
		}

		float ammoQuantityFactor = 1.0f - inventory[ammo->tag] * Q_Rcp( ammo->inventory_max );
		if( ammoQuantityFactor > 0 ) {
			ammoQuantityFactor = Q_Sqrt( ammoQuantityFactor );
		}

		switch( item->tag ) {
			case WEAP_ELECTROBOLT:
				return ItemAndGoalWeights( ammoQuantityFactor, 0.5f * ammoQuantityFactor );
			case WEAP_LASERGUN:
				return ItemAndGoalWeights( ammoQuantityFactor * 1.1f, 0.6f * ammoQuantityFactor );
			case WEAP_PLASMAGUN:
				return ItemAndGoalWeights( ammoQuantityFactor * 1.1f, 0.6f * ammoQuantityFactor );
			case WEAP_ROCKETLAUNCHER:
				return ItemAndGoalWeights( ammoQuantityFactor, 0.5f * ammoQuantityFactor );
			default:
				return ItemAndGoalWeights( 0.75f * ammoQuantityFactor, 0.75f * ammoQuantityFactor );
		}
	}

	// We may consider plasmagun in a bot's hand as a top tier weapon too
	const int topTierWeapons[4] = { WEAP_ELECTROBOLT, WEAP_LASERGUN, WEAP_ROCKETLAUNCHER, WEAP_PLASMAGUN };

	// TODO: Precompute
	float topTierWeaponGreed = 0.0f;
	for( int i = 0; i < 4; ++i ) {
		if( !inventory[topTierWeapons[i]] ) {
			topTierWeaponGreed += 1.0f;
		}
	}

	const bool hasOnlyGunblade = bot->HasOnlyGunblade();
	for( int i = 0; i < 4; ++i ) {
		if( topTierWeapons[i] == item->tag ) {
			float weight = ( hasOnlyGunblade ? 2.0f : 0.9f ) + ( topTierWeaponGreed - 1.0f ) / 3.0f;
			return ItemAndGoalWeights( weight, weight );
		}
	}

	return hasOnlyGunblade ? ItemAndGoalWeights( 1.5f, 2.0f ) : ItemAndGoalWeights( 0.75f, 0.75f );
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputeAmmoWeights( const gsitem_t *item ) const {
	const auto *const inventory = game.edicts[bot->EntNum()].r.client->ps.inventory;

	if( inventory[item->tag] < item->inventory_max ) {
		float quantityFactor = 1.0f - inventory[item->tag] * Q_Rcp( item->inventory_max );
		if( quantityFactor > 0 ) {
			quantityFactor = Q_Sqrt( quantityFactor );
		}

		for( int weapon = WEAP_GUNBLADE; weapon < WEAP_TOTAL; weapon++ ) {
			// TODO: Preache
			const gsitem_t *weaponItem = GS_FindItemByTag( weapon );
			if( weaponItem->ammo_tag == item->tag ) {
				if( inventory[weaponItem->tag] ) {
					switch( weaponItem->tag ) {
						case WEAP_ELECTROBOLT:
							return ItemAndGoalWeights( quantityFactor, quantityFactor );
						case WEAP_LASERGUN:
							return ItemAndGoalWeights( quantityFactor * 1.1f, quantityFactor );
						case WEAP_PLASMAGUN:
							return ItemAndGoalWeights( quantityFactor * 1.1f, quantityFactor );
						case WEAP_ROCKETLAUNCHER:
							return ItemAndGoalWeights( quantityFactor, quantityFactor );
						default:
							return ItemAndGoalWeights( 0.5f * quantityFactor, quantityFactor );
					}
				}
				return ItemAndGoalWeights( quantityFactor * 0.33f, quantityFactor * 0.5f );
			}
		}
	}
	return ItemAndGoalWeights( 0.0, 0.0f );
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputeHealthWeights( const gsitem_t *item ) const {
	if( item->tag == HEALTH_MEGA || item->tag == HEALTH_ULTRA ) {
		return ItemAndGoalWeights( 2.5f, 1.5f );
	}

	const edict_t *self = game.edicts + bot->EntNum();

	// Always set low goal weight for small health bubbles
	if( item->tag == HEALTH_SMALL ) {
		return ItemAndGoalWeights( 0.2f + 0.3f * ( 1.0f - self->health * Q_Rcp( self->max_health ) ), 0.05f );
	}

	float healthFactor = wsw::max( 0.0f, 1.0f - self->health * Q_Rcp( self->max_health ) );
	return ItemAndGoalWeights( healthFactor, healthFactor );
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputeArmorWeights( const gsitem_t *item ) const {
	float currArmor = game.edicts[bot->EntNum()].r.client->armor;
	switch( item->tag ) {
		case ARMOR_RA:
			return currArmor < 150.0f ? ItemAndGoalWeights( 2.0f, 1.0f ) : ItemAndGoalWeights( 0, 0 );
		case ARMOR_YA:
			return currArmor < 125.0f ? ItemAndGoalWeights( 1.7f, 1.0f ) : ItemAndGoalWeights( 0, 0 );
		case ARMOR_GA:
			return currArmor < 100.0f ? ItemAndGoalWeights( 1.4f, 1.0f ) : ItemAndGoalWeights( 0, 0 );
		case ARMOR_SHARD:
		{
			// Always set low goal weight for shards
			if( currArmor < 25 || currArmor >= 150 ) {
				return ItemAndGoalWeights( 0.4f, 0.10f );
			}
			return ItemAndGoalWeights( 0.25f, 0.05f );
		}
	}
	return ItemAndGoalWeights( 0, 0 );
}

BotItemsSelector::ItemAndGoalWeights BotItemsSelector::ComputePowerupWeights( const gsitem_t *item ) const {
	return ItemAndGoalWeights( 3.5f, 2.00f );
}

constexpr float MOVE_TIME_WEIGHT = 1.0f;
constexpr float WAIT_TIME_WEIGHT = 3.5f;

struct NavEntityAndWeight {
	const NavEntity *goal;
	float weight;
	inline NavEntityAndWeight( const NavEntity *goal_, float weight_ ) : goal( goal_ ), weight( weight_ ) {}
	// For sorting in descending by weight order operator < is negated
	inline bool operator<( const NavEntityAndWeight &that ) const { return weight > that.weight; }
};

std::optional<SelectedNavEntity> BotItemsSelector::SuggestGoalNavEntity( const NavEntity *currSelectedNavEntity ) {
	UpdateInternalItemAndGoalWeights();

	wsw::StaticVector<NavEntityAndWeight, MAX_NAVENTS> rawWeightCandidates;
	const auto levelTime = level.time;
	auto *navEntitiesRegistry = NavEntitiesRegistry::Instance();
	const auto *routeCache = bot->RouteCache();
	for( const NavEntity *navEnt = navEntitiesRegistry->Head(); navEnt; navEnt = navEnt->Next() ) {
		if( navEnt->IsDisabled() ) {
			continue;
		}

		// We cannot just set a zero internal weight for a temporarily disabled nav entity
		// (it might be overridden by an external weight, and we should not modify external weights
		// as script users expect them remaining the same unless explicitly changed via script API)
		if( disabledForSelectionUntil[navEnt->Id()] >= levelTime ) {
			continue;
		}

		// Since movable goals have been introduced (and clients qualify as movable goals), prevent picking itself as a goal.
		if( navEnt->Id() == bot->EntNum() ) {
			continue;
		}

		if( navEnt->Item() && !G_Gametype_CanPickUpItem( navEnt->Item() ) ) {
			continue;
		}

		// Reject an entity quickly if it looks like blocked by an enemy that is close to the entity.
		// Note than passing this test does not guarantee that entire path to the entity is not blocked by enemies.
		if( routeCache->AreaDisabled( navEnt->AasAreaNum() ) ) {
			continue;
		}

		// This is a coarse and cheap test, helps to reject recently picked armors and powerups
		int64_t spawnTime = navEnt->SpawnTime();
		// A feasible spawn time (non-zero) always >= level.time.
		if( !spawnTime || spawnTime - level.time > 15000 ) {
			continue;
		}

		float weight = GetEntityWeight( navEnt->Id() );
		if( weight > 0 ) {
			rawWeightCandidates.push_back( NavEntityAndWeight( navEnt, weight ) );
		}
	}

	// Make sure the candidates list is not empty and thus we can access the best candidate
	if( rawWeightCandidates.empty() ) {
		Debug( "Can't find a feasible long-term goal nav. entity\n" );
		return std::nullopt;
	}

	// Sort all pre-selected candidates by their raw weights
	std::sort( rawWeightCandidates.begin(), rawWeightCandidates.end() );

	int fromAreaNums[2] = { 0, 0 };
	const auto &entityPhysicsState = bot->EntityPhysicsState();
	const int numFromAreas = entityPhysicsState->PrepareRoutingStartAreas( fromAreaNums );

	// Pick the best raw weight nav entity.
	// This nav entity is not necessarily the best final nav entity
	// by the final weight that is influenced by routing cost,
	// but the best raw weight means the high importance of it.
	// The picked entity must be reachable from the current location
	auto rawCandidatesIter = rawWeightCandidates.begin();
	const auto rawCandidatesEnd = rawWeightCandidates.end();
	const NavEntity *rawBestNavEnt = ( *rawCandidatesIter ).goal;
	int rawBestAreaNum = rawBestNavEnt->AasAreaNum();
	unsigned botToBestRawEntMoveDuration = 0;
	for(;; ) {
		botToBestRawEntMoveDuration = 10U * routeCache->PreferredRouteToGoalArea( fromAreaNums, numFromAreas, rawBestAreaNum );
		if( botToBestRawEntMoveDuration ) {
			break;
		}
		++rawCandidatesIter;
		if( rawCandidatesIter == rawCandidatesEnd ) {
			Debug( "Can't find a feasible long-term goal nav. entity\n" );
			return std::nullopt;
		}
		rawBestNavEnt = ( *rawCandidatesIter ).goal;
		rawBestAreaNum = rawBestNavEnt->AasAreaNum();
	}

	// Try checking whether the bot is in some floor cluster to give a greater weight for items in the same cluster
	int currFloorClusterNum = 0;
	const std::span<const uint16_t> aasFloorClusterNums = AiAasWorld::instance()->areaFloorClusterNums();
	if( aasFloorClusterNums[entityPhysicsState->CurrAasAreaNum()] ) {
		currFloorClusterNum = aasFloorClusterNums[entityPhysicsState->CurrAasAreaNum()];
	} else if( aasFloorClusterNums[entityPhysicsState->DroppedToFloorAasAreaNum()] ) {
		currFloorClusterNum = aasFloorClusterNums[entityPhysicsState->DroppedToFloorAasAreaNum()];
	}

	const NavEntity *currGoalNavEntity = currSelectedNavEntity;
	float currGoalEntWeight = 0.0f;
	float currGoalEntCost = 0.0f;
	const NavEntity *bestNavEnt = nullptr;
	float bestWeight = 0.000001f;
	float bestNavEntCost = 0.0f;

	const auto startCandidatesIter = rawCandidatesIter;
	// Start from the first (and best) reachable nav entity.
	// (This entity cannot be selected right now as there are additional tests).
	// Test no more than 16 next entities to prevent performance drops.
	for(; rawCandidatesIter - startCandidatesIter < 16 && rawCandidatesIter != rawCandidatesEnd; ++rawCandidatesIter ) {
		const NavEntity *navEnt = ( *rawCandidatesIter ).goal;
		float weight = ( *rawCandidatesIter ).weight;

		const unsigned botToCandidateMoveDuration =
			routeCache->PreferredRouteToGoalArea( fromAreaNums, numFromAreas, navEnt->AasAreaNum() ) * 10U;

		// AAS functions return 0 as a "none" value, 1 as a lowest feasible value
		if( !botToCandidateMoveDuration ) {
			continue;
		}

		if( navEnt->IsDroppedEntity() ) {
			// Do not pick an entity that is likely to dispose before it may be reached
			if( navEnt->Timeout() <= level.time + botToCandidateMoveDuration ) {
				continue;
			}
		}

		int64_t spawnTime = navEnt->SpawnTime();
		// The entity is not spawned and respawn time is unknown
		if( !spawnTime ) {
			continue;
		}

		// Entity origin may be reached at this time
		int64_t reachTime = level.time + botToCandidateMoveDuration;
		unsigned waitDuration = 1;
		if( reachTime < spawnTime ) {
			waitDuration = (unsigned)( spawnTime - reachTime );
		}

		if( waitDuration > navEnt->MaxWaitDuration() ) {
			continue;
		}

		bool isShortRangeReachable = false;
		// If the bot is inside a floor cluster
		if( currFloorClusterNum ) {
			// Increase weight for nav entities in the same floor cluster if the entity is fairly close to it,
			// are spawned, is visible and is reachable by just by walking (to cut off entities behind gaps).
			// Note: do not try making weights depend of velocity/view dir as it is prone to jitter.
			if( currFloorClusterNum == aasFloorClusterNums[navEnt->AasAreaNum()] ) {
				if( IsShortRangeReachable( navEnt, fromAreaNums, numFromAreas ) ) {
					isShortRangeReachable = true;
					weight *= 2.0f;
				}
			}
		}

		// Check the travel time from the nav entity to the best raw weight nav entity
		const unsigned candidateToRawBestEntMoveDuration =
			routeCache->PreferredRouteToGoalArea( navEnt->AasAreaNum(), rawBestNavEnt->AasAreaNum() ) * 10U;

		// If the best raw weight nav entity is not reachable from the entity
		if( !candidateToRawBestEntMoveDuration ) {
			continue;
		}

		// Take into account not only travel time to candidate, but also travel time from candidate to the raw best nav ent.
		// If moving to the current candidate leads to increased travel time to the best raw nav entity.
		// compared to the travel time to it from the current origin, consider it as a penalty.
		// No penalty is applied for nav entities that are closer to the best raw nav entity than the bot.
		// This bots are forced to advance to "best" map regions to complete objectives (capture flags, etc)
		// while still grabbing stuff that is short-range reachable.
		unsigned moveDurationPenalty = 0;
		// Don't apply penalty for short-range reachable nav entities
		if( !isShortRangeReachable ) {
			if( candidateToRawBestEntMoveDuration > botToBestRawEntMoveDuration ) {
				moveDurationPenalty = candidateToRawBestEntMoveDuration - botToBestRawEntMoveDuration;
			}
		}

		const float moveCost = MOVE_TIME_WEIGHT * ( botToCandidateMoveDuration + moveDurationPenalty );
		const float cost = moveCost + WAIT_TIME_WEIGHT * waitDuration;

		weight = ( 1000 * weight ) / ( 0.001f + cost * navEnt->CostInfluence() );

		// Store current weight of the current goal entity
		if( currGoalNavEntity == navEnt ) {
			currGoalEntWeight = weight;
			// Waiting time is handled by the planner for wait actions separately.
			currGoalEntCost = moveCost;
		}

		if( weight <= bestWeight ) {
			continue;
		}

		bestNavEnt = navEnt;
		bestWeight = weight;
		// Waiting time is handled by the planner for wait actions separately.
		bestNavEntCost = moveCost;
	}

	if( !bestNavEnt ) {
		Debug( "Can't find a feasible long-term goal nav. entity\n" );
		return std::nullopt;
	}

	// If it is time to pick a new goal (not just re-evaluate current one), do not be too sticky to the current goal
	const float currToBestWeightThreshold = currGoalNavEntity != nullptr ? 0.6f : 0.8f;

	if( currGoalNavEntity && currGoalNavEntity == bestNavEnt ) {
		constexpr const char *format = "The current goal entity %s is kept as still having best weight %.2f\n";
		Debug( format, currGoalNavEntity->Name(), bestWeight );
		return Select( bestNavEnt, bestNavEntCost, 4000u );
	}

	if( currGoalEntWeight > 0 && currGoalEntWeight / bestWeight > currToBestWeightThreshold ) {
		constexpr const char *format = "The current goal entity %s is kept as having weight %.2f good enough\n";
		// If currGoalEntWeight > 0, currLongTermGoalEnt is guaranteed to be non-null
		Debug( format, currGoalNavEntity->Name(), currGoalEntWeight );
		return Select( currGoalNavEntity, currGoalEntCost, 2500u );
	}

	if( currGoalNavEntity ) {
		const char *format = "Suggested %s weighted %.2f as a long-term goal instead of %s weighted now as %.2f\n";
		Debug( format, bestNavEnt->Name(), bestWeight, currGoalNavEntity->Name(), currGoalEntWeight );
	} else {
		Debug( "Suggested %s weighted %.2f as a new long-term goal\n", bestNavEnt->Name(), bestWeight );
	}

	return Select( bestNavEnt, bestNavEntCost, 2500u );
}

void BotItemsSelector::Debug( const char *format, ... ) {
	char tagBuffer[64];
	va_list va;
	va_start( va, format );
	AI_Debugv( va_r( tagBuffer, sizeof( tagBuffer ), "ItemsSelector(%s)", bot->Nick() ), format, va );
	va_end( va );
}

bool BotItemsSelector::IsShortRangeReachable( const NavEntity *navEnt, const int *fromAreaNums, int numFromAreas ) const {
	if( navEnt->Origin().SquareDistanceTo( bot->Origin() ) > 256.0f * 256.0f ) {
		return false;
	}

	if( !navEnt->IsSpawnedNow() ) {
		return false;
	}

	const auto *ent = game.edicts + navEnt->Id();
	const auto *self = game.edicts + bot->EntNum();
	if( !EntitiesPvsCache::Instance()->AreInPvs( self, ent ) ) {
		return false;
	}

	Vec3 viewOrigin( self->s.origin );
	viewOrigin.Z() += self->viewheight;
	trace_t trace;

	SolidWorldTrace( &trace, viewOrigin.Data(), ent->s.origin );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	const int travelFlags = TFL_WALK | TFL_AIR;
	const auto *routeCache = self->bot->routeCache;
	for( int i = 0; i < numFromAreas; ++i ) {
		if( routeCache->TravelTimeToGoalArea( fromAreaNums[i], navEnt->AasAreaNum(), travelFlags ) ) {
			return true;
		}
	}

	return false;
}
