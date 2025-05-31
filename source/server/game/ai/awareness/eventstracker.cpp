#include "eventstracker.h"
#include <common/smallassocarray.h>
#include "../bot.h"

void EventsTracker::TryGuessingBeamOwnersOrigins( const EntNumsVector &dangerousEntsNums, float failureChance ) {
	const edict_t *const gameEdicts = game.edicts;
	for( auto entNum: dangerousEntsNums ) {
		if( m_rng.tryWithChance( 1.0f - failureChance ) ) {
			const edict_t *owner = &gameEdicts[gameEdicts[entNum].s.ownerNum];
			if( !m_bot->IsDefinitelyNotAFeasibleEnemy( owner ) ) {
				if( CanDistinguishGenericEnemySoundsFromTeammates( owner ) ) {
					m_bot->OnEnemyOriginGuessed( owner, 128 );
				}
			}
		}
	}
}

void EventsTracker::TryGuessingProjectileOwnersOrigins( const EntNumsVector &dangerousEntNums, float failureChance ) {
	wsw::SmallAssocArray<int, std::pair<int, bool>, 3> projectileToWeaponTable;
	(void)projectileToWeaponTable.insert( ET_GRENADE, { WEAP_GRENADELAUNCHER, true } );
	(void)projectileToWeaponTable.insert( ET_ROCKET, { WEAP_ROCKETLAUNCHER, false } );
	(void)projectileToWeaponTable.insert( ET_PLASMA, { WEAP_PLASMAGUN, false } );

	const edict_t *const gameEdicts = game.edicts;
	const int64_t levelTime         = level.time;
	for( auto entNum: dangerousEntNums ) {
		if( failureChance == 0.0f || m_rng.tryWithChance( 1.0f - failureChance ) ) {
			const edict_t *const projectile = &gameEdicts[entNum];
			const edict_t *const owner      = &gameEdicts[projectile->s.ownerNum];
			if( !m_bot->IsDefinitelyNotAFeasibleEnemy( owner ) ) {
				if( projectile->s.linearMovement ) {
					// This test is expensive, do it after cheaper ones have succeeded.
					if( CanDistinguishGenericEnemySoundsFromTeammates( projectile->s.linearMovementBegin ) ) {
						m_bot->OnEnemyOriginGuessed( owner, 256, projectile->s.linearMovementBegin );
					}
				} else {
					const auto it = projectileToWeaponTable.find( projectile->s.type );
					if( it != projectileToWeaponTable.end() ) {
						const auto *weaponDef  = GS_GetWeaponDef( ggs, it.value().first );
						const auto *fireDef    = it.value().second ? &weaponDef->firedef : &weaponDef->firedef_weak;
						// Assume that we may legibly guess on the first half of lifetime
						if( projectile->nextThink > levelTime + fireDef->timeout / 2 ) {
							// This test is expensive, do it after cheaper ones have succeeded.
							// TODO: Keep the spawn origin?
							if( CanDistinguishGenericEnemySoundsFromTeammates( owner ) ) {
								// Use the exact enemy origin as a guessed one
								m_bot->OnEnemyOriginGuessed( owner, 384 );
							}
						}
					}
				}
			}
		}
	}
}

void EventsTracker::ComputeTeammatesVisData( const vec3_t forwardDir, float fovDotFactor ) {
	assert( GS_TeamBasedGametype( *ggs ) );

	m_areAllTeammatesInFov = true;
	m_teammatesVisData.clear();

	const edict_t *botEnt   = game.edicts + m_bot->EntNum();
	const int numTeammates  = ::teamlist[botEnt->s.team].numplayers;
	const int *teammateNums = ::teamlist[botEnt->s.team].playerIndices;
	const auto *gameEdicts  = game.edicts;

	for( int i = 0; i < numTeammates; ++i ) {
		const edict_t *teammate = gameEdicts + teammateNums[i];
		if( teammate != botEnt && !G_ISGHOSTING( teammate ) ) {
			Vec3 dir( Vec3( teammate->s.origin ) - botEnt->s.origin );
			float dot, distance;
			if( const auto maybeDistance = dir.normalizeFast( { .minAcceptableLength = 48.0f } ) ) {
				dot      = dir.Dot( forwardDir );
				distance = *maybeDistance;
			} else {
				dot      = 1.0f;
				distance = 0.0f;
			}
			if( dot < fovDotFactor ) {
				m_areAllTeammatesInFov = false;
			}
			m_teammatesVisData.emplace_back( TeammateVisDataEntry {
				.botViewDirDotDirToTeammate = dot,
				.distanceToBot              = distance,
				.playerNum                  = (uint8_t)teammateNums[i],
				.visStatus                  = TeammateVisStatus::NotTested
			});
		}
	}
}

bool EventsTracker::CanDistinguishGenericEnemySoundsFromTeammates( const GuessedEnemy &guessedEnemy ) {
	if( !GS_TeamBasedGametype( *ggs ) ) {
		return true;
	}

	Vec3 toEnemyDir( Vec3( guessedEnemy.m_origin ) - m_bot->Origin() );
	const auto maybeSignificantDistanceToEnemy = toEnemyDir.normalizeFast( { .minAcceptableLength = 48.0f } );
	if( !maybeSignificantDistanceToEnemy ) {
		return true;
	}

	const float distanceToEnemy = *maybeSignificantDistanceToEnemy;
	const auto *gameEdicts      = game.edicts;
	const Vec3 forwardDir       = m_bot->EntityPhysicsState()->ForwardDir();
	const float fovDotFactor    = m_bot->FovDotFactor();
	const edict_t *botEnt       = game.edicts + m_bot->EntNum();
	const bool canShowMinimap   = GS_CanShowMinimap( *ggs );

	// Compute vis data lazily
	if( m_teammatesVisDataComputedAt != level.time ) {
		m_teammatesVisDataComputedAt = level.time;
		ComputeTeammatesVisData( forwardDir.Data(), fovDotFactor );
	}

	const float botViewDitDotToEnemyDir = forwardDir.Dot( toEnemyDir );

	// If the bot can't see the guessed enemy origin
	if( botViewDitDotToEnemyDir < fovDotFactor ) {
		// Assume that everything not in fov is performed by enemies
		if( m_areAllTeammatesInFov ) {
			return true;
		}

		// Cannot "look at the minimap" in this case
		if( !canShowMinimap ) {
			return false;
		}

		// Try using a minimap to make the distinction.
		for( const TeammateVisDataEntry &teammateEntry: m_teammatesVisData ) {
			// If this teammate is in not in fov
			if( teammateEntry.botViewDirDotDirToTeammate < fovDotFactor ) {
				const edict_t *teammate = gameEdicts + teammateEntry.playerNum + 1;
				// If this teammate is way too close to the guessed origin
				if( DistanceSquared( teammate->s.origin, guessedEnemy.m_origin ) < wsw::square( 300.0f ) ) {
					// Check whether there's nothing solid in-between (we assume walls) that can help the distinction
					if( guessedEnemy.AreInPvsWith( botEnt ) ) {
						trace_t trace;
						SolidWorldTrace( &trace, teammate->s.origin, guessedEnemy.m_origin );
						if( trace.fraction == 1.0f ) {
							return false;
						}
					}
				}
			}
		}

		return true;
	}

	const auto *pvsCache = EntitiesPvsCache::Instance();
	for( TeammateVisDataEntry &teammateEntry: m_teammatesVisData ) {
		const edict_t *teammate = gameEdicts + teammateEntry.playerNum + 1;
		// The bot can't see or hear the teammate. Try using a minimap to make the distinction.
		if( teammateEntry.botViewDirDotDirToTeammate < fovDotFactor ) {
			// A mate is way too close to the guessed origin.
			if( DistanceSquared( teammate->s.origin, guessedEnemy.m_origin ) < wsw::square( 300.0f ) ) {
				if( !canShowMinimap ) {
					return false;
				}
				// Check whether there is a wall that helps to make the distinction.
				if( guessedEnemy.AreInPvsWith( botEnt ) ) {
					trace_t trace;
					SolidWorldTrace( &trace, teammate->s.origin, guessedEnemy.m_origin );
					if( trace.fraction == 1.0f ) {
						return false;
					}
				}
			}
		} else if( std::abs( teammateEntry.botViewDirDotDirToTeammate - botViewDitDotToEnemyDir ) < 0.1f ) {
			// Consider that it's guaranteed that the enemy cannot be distinguished from a teammate
			return false;
		} else {
			// Test teammate visibility status lazily
			if( teammateEntry.visStatus == TeammateVisStatus::NotTested ) {
				teammateEntry.visStatus = TeammateVisStatus::Invisible;
				if( pvsCache->AreInPvs( botEnt, teammate ) ) {
					Vec3 viewOrigin( botEnt->s.origin );
					viewOrigin.Z() += (float)botEnt->viewheight;
					trace_t trace;
					SolidWorldTrace( &trace, viewOrigin.Data(), teammate->s.origin );
					if( trace.fraction == 1.0f ) {
						teammateEntry.visStatus = TeammateVisStatus::Visible;
					}
				}
			}

			// Can't say much if the teammate is not visible.
			// We can test visibility of the guessed origin but its way too expensive.
			// (there might be redundant computations overlapping with the normal bot vision).
			if( teammateEntry.visStatus == TeammateVisStatus::Invisible ) {
				return false;
			}

			if( distanceToEnemy > 2048.0f ) {
				if( teammateEntry.distanceToBot < distanceToEnemy ) {
					return false;
				}
			}
		}
	}

	return true;
}

bool EventsTracker::GuessedEnemyEntity::AreInPvsWith( const edict_t *botEnt ) const {
	return EntitiesPvsCache::Instance()->AreInPvs( botEnt, m_ent );
}

bool EventsTracker::GuessedEnemyOrigin::AreInPvsWith( const edict_t *botEnt ) const {
	if( botEnt->r.num_clusters < 0 ) {
		return SV_InPVS( botEnt->s.origin, m_origin );
	}

	// Compute leafs for the own origin lazily
	if( !m_numLeafs ) {
		const vec3_t mins { -4, -4, -4 };
		const vec3_t maxs { +4, +4, +4 };
		[[maybe_unused]] int topNode = 0;
		m_numLeafs = SV_BoxLeafnums( mins, maxs, m_leafNums, 4, &topNode );
		// Filter out solid leafs
		for( int i = 0; i < m_numLeafs; ) {
			if( SV_LeafCluster( m_leafNums[i] ) >= 0 ) {
				i++;
			} else {
				m_numLeafs--;
				m_leafNums[i] = m_leafNums[m_numLeafs];
			}
		}
	}

	const int *botLeafNums = botEnt->r.leafnums;
	for( int i = 0, end = botEnt->r.num_clusters; i < end; ++i ) {
		for( int j = 0; j < m_numLeafs; ++j ) {
			if( SV_LeafsInPVS( botLeafNums[i], m_leafNums[j] ) ) {
				return true;
			}
		}
	}

	return false;
}

void EventsTracker::HandlePlayerSexedSoundEvent( const edict_t *player, float distanceThreshold ) {
	assert( player->s.type == ET_PLAYER );

	if( DistanceSquared( player->s.origin, m_bot->Origin() ) < wsw::square( distanceThreshold ) ) {
		if( !m_bot->IsDefinitelyNotAFeasibleEnemy( player ) ) {
			// TODO: Not all events have different sounds for teammates and enemies
			AddPendingGuessedEnemyOrigin( player, player->s.origin );
		}
	}
}

void EventsTracker::HandleGenericPlayerEntityEvent( const edict_t *player, float distanceThreshold ) {
	assert( player->s.type == ET_PLAYER );

	if( DistanceSquared( player->s.origin, m_bot->Origin() ) < wsw::square( distanceThreshold ) ) {
		if( !m_bot->IsDefinitelyNotAFeasibleEnemy( player ) ) {
			if( CanDistinguishGenericEnemySoundsFromTeammates( player ) ) {
				AddPendingGuessedEnemyOrigin( player, player->s.origin );
			}
		}
	}
}

void EventsTracker::AddPendingGuessedEnemyOrigin( const edict_t *enemy, const vec_t *origin ) {
	// Try finding an existing entry for this enemy
	for( PendingGuessedEnemyOrigin &pendingGuessedOrigin: m_guessedEnemyOriginsQueue ) {
		if( pendingGuessedOrigin.entNum == ENTNUM( enemy ) ) {
			// Just overwrite by the most recent one guess.
			// Hacks: Don't let overwriting by a more coarse guess.
			// TODO: Use more sophisticated checks?
			const float actualToOldOrigin = DistanceSquared( enemy->s.origin, pendingGuessedOrigin.origin.Data() );
			const float actualToNewOrigin = DistanceSquared( enemy->s.origin, origin );
			if( actualToOldOrigin > actualToNewOrigin ) {
				pendingGuessedOrigin.origin.Set( origin );
			}
			return;
		}
	}

	if( m_guessedEnemyOriginsQueue.full() ) {
		m_guessedEnemyOriginsQueue.pop_back();
	}
	m_guessedEnemyOriginsQueue.emplace_front( { .origin = Vec3( origin ), .entNum = ENTNUM( enemy ) } );
}

void EventsTracker::HandleJumppadEvent( const edict_t *player, float ) {
	assert( player->s.type == ET_PLAYER );

	if( DistanceSquared( player->s.origin, m_bot->Origin() ) < wsw::square( 1024.0f ) ) {
		if( !m_bot->IsDefinitelyNotAFeasibleEnemy( player ) ) {
			if( CanDistinguishGenericEnemySoundsFromTeammates( player ) ) {
				// TODO: Make sure we can distinguish it from enemy sounds
				// Keep reporting its origin not longer than 5000 micros
				m_jumppadUserTrackingTimeoutAt[PLAYERNUM( player )] = level.time + 5000;
			}
		}
	}
}

void EventsTracker::RegisterEvent( const edict_t *ent, int event, int parm ) {
	( this->*m_eventHandlers[event] )( ent, this->m_eventHandlingParams[event] );
}

EventsTracker::EventsTracker( Bot *bot ) : m_bot( bot ) {
	for( int eventNum = 0; eventNum < MAX_EVENTS; ++eventNum ) {
		SetEventHandler( eventNum, &EventsTracker::HandleEventNoOp );
	}

	// Note: radius values are a bit lower than it is expected for a human perception,
	// but otherwise bots behave way too hectic detecting all minor events

	for( int eventNum : { EV_FIREWEAPON, EV_SMOOTHREFIREWEAPON } ) {
		SetEventHandler( eventNum, &EventsTracker::HandleGenericPlayerEntityEvent, 1024.0f + 512.0f );
	}

	SetEventHandler( EV_WEAPONACTIVATE, &EventsTracker::HandleGenericPlayerEntityEvent, 1024.0f );
	SetEventHandler( EV_NOAMMOCLICK, &EventsTracker::HandleGenericPlayerEntityEvent, 768.0f );

	for( int eventNum : { EV_DASH, EV_WALLJUMP, EV_DOUBLEJUMP, EV_JUMP } ) {
		SetEventHandler( eventNum, &EventsTracker::HandlePlayerSexedSoundEvent, 1024.0f + 256.0f );
	}

	SetEventHandler( EV_WALLJUMP_FAILED, &EventsTracker::HandlePlayerSexedSoundEvent, 768.0f );

	SetEventHandler( EV_JUMP_PAD, &EventsTracker::HandleJumppadEvent );

	SetEventHandler( EV_FALL, &EventsTracker::HandleGenericPlayerEntityEvent, 1024.0f );
}

void EventsTracker::Update() {
	const edict_t *gameEdicts = game.edicts;
	const int64_t levelTime   = level.time;

	// Try detecting landing events every frame
	for( int clientNum = 0, maxClients = ggs->maxclients; clientNum < maxClients; ++clientNum ) {
		if( m_jumppadUserTrackingTimeoutAt[clientNum] > levelTime ) {
			if( gameEdicts[clientNum + 1].groundentity ) {
				m_jumppadUserTrackingTimeoutAt[clientNum] = 0;
			}
		}
	}

	if( m_bot->PermitsDistributedUpdateThisFrame() ) {
		for( int clientNum = 0, maxClients = ggs->maxclients; clientNum < maxClients; ++clientNum ) {
			if( m_jumppadUserTrackingTimeoutAt[clientNum] > levelTime ) {
				const edict_t *ent = gameEdicts + clientNum + 1;
				// Check whether a player cannot be longer a valid entity
				if( m_bot->IsDefinitelyNotAFeasibleEnemy( ent ) ) {
					m_jumppadUserTrackingTimeoutAt[clientNum] = 0;
				} else {
					m_bot->OnEnemyOriginGuessed( ent, 384 );
				}
			}
		}

		// We have to validate detected enemies since the events queue
		// has been accumulated during the Think() frames cycle
		// and might contain outdated info (e.g. a player has changed its team).
		for( const auto &[origin, entNum]: m_guessedEnemyOriginsQueue ) {
			const edict_t *ent = gameEdicts + entNum;
			if( !m_bot->IsDefinitelyNotAFeasibleEnemy( ent ) ) {
				m_bot->OnEnemyOriginGuessed( ent, 96, origin.Data() );
			}
		}

		m_guessedEnemyOriginsQueue.clear();
	}
}

