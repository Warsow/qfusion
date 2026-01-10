#include "bot.h"
#include "navigation/aasworld.h"
#include "teamplay/objectivebasedteam.h"
#include "../g_gametypes.h"
#include "manager.h"
#include <array>

#ifndef _MSC_VER
// Allow getting an address of not initialized yet field movementModule.movementState.entityPhysicsState.
// Saving this address for further use is legal, the field is not going to be used right now.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
#endif

Bot::Bot( edict_t *self_, float skillLevel_ )
	: self( self_ )
	, planner( &planningModule.planner )
	, routeCache( AiAasRouteCache::NewInstance() )
	, aasWorld( AiAasWorld::instance() )
	, entityPhysicsState( &m_movementSubsystem.movementState.entityPhysicsState )
	, blockedTimeoutAt( level.time + BLOCKED_TIMEOUT )
	, skillLevel( skillLevel_ )
	, m_movementSubsystem( this )
	, awarenessModule( this )
	, planningModule( this )
	, weightConfig( self_ )
	, weaponsUsageModule( this ) {
	self->r.client->movestyle = GS_CLASSICBUNNY;
	SetTag( "%s", self->r.client->netname.data() );
	angularViewSpeed[YAW]   = skillLevel_ > 0.33f ? DEFAULT_YAW_SPEED * 1.5f : DEFAULT_YAW_SPEED;
	angularViewSpeed[PITCH] = skillLevel_ > 0.33f ? DEFAULT_PITCH_SPEED * 1.2f : DEFAULT_PITCH_SPEED;
	angularViewSpeed[ROLL]  = 0.0f;
	static_assert( sizeof( attitude[0] ) == 1 && sizeof( attitude ) == MAX_EDICTS );
	std::memset( attitude, -1, sizeof( attitude ) );
}

#ifndef _MSC_VER
#pragma GCC diagnostic pop
#endif

Bot::~Bot() {
	AiAasRouteCache::ReleaseInstance( routeCache );
}

void Bot::OnAttachedToSquad( AiSquad *squad_ ) {
	if( !squad_ ) {
		FailWith( "Bot::OnAttachedToSquad(): Attempt to attach to a null squad" );
	}
	if( this->squad ) {
		if( this->squad == squad_ ) {
			FailWith( "Bot::OnAttachedToSquad(%p): Was already attached to the squad\n", squad_ );
		} else {
			FailWith( "Bot::OnAttachedToSquad(%p): Was attached to another squad %p\n", squad_, this->squad );
		}
	}

	this->squad = squad_;
	awarenessModule.OnAttachedToSquad( squad_ );
	ForcePlanBuilding();
}

void Bot::OnDetachedFromSquad( AiSquad *squad_ ) {
	if( squad_ != this->squad ) {
		if( this->squad ) {
			FailWith( "OnDetachedFromSquad(%p): Was attached to squad %p\n", squad_, this->squad );
		} else {
			FailWith( "OnDetachedFromSquad(%p): Was not attached to a squad\n", squad_ );
		}
	}

	this->squad = nullptr;
	awarenessModule.OnDetachedFromSquad( squad_ );
	ForcePlanBuilding();
}

int Bot::DefenceSpotId() const {
	// This call is used only for scripts compatibility so this is not that bad
	if( dynamic_cast<AiDefenceSpot *>( objectiveSpot ) ) {
		return objectiveSpot->id;
	}
	return -1;
}

int Bot::OffenseSpotId() const {
	// This call is used only for scripts compatibility so this is not that bad
	if( dynamic_cast<AiOffenseSpot *>( objectiveSpot ) ) {
		return objectiveSpot->id;
	}
	return -1;
}

void Bot::notifyOfNavEntitySignaledAsReached( const NavEntity *navEntity ) {
	if( navTarget && navTarget->IsBasedOnNavEntity( navEntity ) ) {
		ResetNavTarget();
	}
	if( m_selectedNavEntity && m_selectedNavEntity->navEntity == navEntity ) {
		m_selectedNavEntity = std::nullopt;
	}
}

void Bot::notifyOfNavEntityRemoved( const NavEntity *navEntity ) {
	if( navTarget && navTarget->IsBasedOnNavEntity( navEntity ) ) {
		ResetNavTarget();
	}
	if( m_selectedNavEntity && m_selectedNavEntity->navEntity == navEntity ) {
		m_selectedNavEntity = std::nullopt;
	}
}

void Bot::TouchedEntity( edict_t *ent ) {
	if( CanHandleNavTargetTouch( ent ) ) {
		// Clear goal area num to ensure bot will not repeatedly try to reach that area even if he has no goals.
		// Usually it gets overwritten in this or next frame, when bot picks up next goal,
		// but sometimes there are no other goals to pick up.
		ResetNavTarget();
		m_selectedNavEntity = std::nullopt;
	} else {
		TouchedOtherEntity( ent );
	}
}

void Bot::TouchedOtherEntity( const edict_t *entity ) {
	if( !entity->classname ) {
		return;
	}

	// Cut off string comparisons by doing these cheap tests first

	// Only triggers are interesting for following code
	if( entity->r.solid != SOLID_TRIGGER ) {
		return;
	}
	// Items should be handled by TouchedNavEntity() or skipped (if it is not a current nav entity)
	if( entity->item ) {
		return;
	}

	if( !Q_stricmp( entity->classname, "trigger_push" ) ) {
		m_lastTouchedJumppadAt = level.time;
		m_movementSubsystem.ActivateJumppadState( entity );
		return;
	}

	if( !Q_stricmp( entity->classname, "trigger_teleport" ) ) {
		m_lastTouchedTeleportAt = level.time;
		return;
	}

	if( !Q_stricmp( entity->classname, "trigger_platform" ) ) {
		m_lastTouchedElevatorAt = level.time;
		m_movementSubsystem.ActivateElevatorState( entity );
		return;
	}
}

void Bot::CheckTargetProximity() {
	planningModule.CheckTargetProximity();

	if( !NavTargetAasAreaNum() ) {
		return;
	}

	if( !IsCloseToNavTarget( 128.0f ) ) {
		return;
	}

	// Save the origin for the roaming manager to avoid its occasional modification in the code below
	if( !TryReachNavTargetByProximity() ) {
		return;
	}

	navTarget           = nullptr;
	m_selectedNavEntity = std::nullopt;
}

bool Bot::CanHandleNavTargetTouch( const edict_t *ent ) {
	if( !ent ) {
		return false;
	}

	if( !navTarget ) {
		return false;
	}

	if( !navTarget->IsBasedOnEntity( ent ) ) {
		return false;
	}

	if( !navTarget->ShouldBeReachedAtTouch() ) {
		return false;
	}

	return true;
}

bool Bot::TryReachNavTargetByProximity() {
	if( !navTarget ) {
		return false;
	}

	if( !navTarget->ShouldBeReachedAtRadius() ) {
		return false;
	}

	if( ( navTarget->Origin() - self->s.origin ).squareLength() < wsw::square( navTarget->RadiusOrDefault( 40.0f ) ) ) {
		return true;
	}

	return false;
}

void Bot::OnPain( const edict_t *enemy, float kick, int damage ) {
	if( enemy != self ) {
		awarenessModule.OnPain( enemy, kick, damage );
	}
}

void Bot::OnKnockback( const edict_t *attacker, const vec3_t basedir, int kick, int dflags ) {
	if( kick ) {
		lastKnockbackAt = level.time;
		VectorCopy( basedir, lastKnockbackBaseDir );
		if( attacker == self ) {
			lastOwnKnockbackKick = kick;
			lastOwnKnockbackAt = level.time;
		}
	}
}

void Bot::OnEnemyDamaged( const edict_t *enemy, int damage ) {
	if( enemy != self ) {
		awarenessModule.OnEnemyDamaged( enemy, damage );
	}
}

void Bot::OnEnemyOriginGuessed( const edict_t *enemy, unsigned millisSinceLastSeen, const float *guessedOrigin ) {
	if( !guessedOrigin ) {
		guessedOrigin = enemy->s.origin;
	}
	awarenessModule.OnEnemyOriginGuessed( enemy, millisSinceLastSeen, guessedOrigin );
}

const std::optional<SelectedNavEntity> &Bot::GetOrUpdateSelectedNavEntity() {
	const NavEntity *currNavEntity = nullptr;
	if( m_selectedNavEntity ) {
		if( m_selectedNavEntity->timeoutAt < level.time ) {
			return m_selectedNavEntity;
		}
		currNavEntity = m_selectedNavEntity->navEntity;
	}

	// Force an update using the currently selected nav entity
	// (it's OK if it's not valid) as a reference info for selection
	ForceSetNavEntity( planningModule.SuggestGoalNavEntity( currNavEntity ) );
	// Return the modified selected nav entity
	return m_selectedNavEntity;
}

void Bot::ForceSetNavEntity( const std::optional<SelectedNavEntity> &selectedNavEntity ) {
	// Use direct access to the field to skip assertion
	m_selectedNavEntity = selectedNavEntity;

	if( m_selectedNavEntity ) {
		lastItemSelectedAt = level.time;
	} else {
		// Edge detection
		if( lastItemSelectedAt >= noItemAvailableSince ) {
			noItemAvailableSince = level.time;
		}
	}
}

void Bot::ChangeWeapons( const SelectedWeapons &selectedWeapons_ ) {
	if( selectedWeapons_.BuiltinFireDef() != nullptr ) {
		self->r.client->ps.stats[STAT_PENDING_WEAPON] = selectedWeapons_.BuiltinWeaponNum();
	}
	if( selectedWeapons_.ScriptFireDef() != nullptr ) {
		GT_asSelectScriptWeapon( self->r.client, selectedWeapons_.ScriptWeaponNum() );
	}
}

void Bot::OnBlockedTimeout() {
	self->health = 0;
	blockedTimeoutAt = level.time + BLOCKED_TIMEOUT;
	self->die( self, self, self, 100000, vec3_origin );
	G_Killed( self, self, self, 999, vec3_origin, MOD_SUICIDE );
	self->nextThink = level.time + 1;
}

void Bot::OnRespawn() {
	VectorClear( self->r.client->ps.pmove.delta_angles );
	self->r.client->last_activity = level.time;

	m_selectedEnemy = std::nullopt;
	m_lostEnemy     = std::nullopt;

	planningModule.ClearGoalAndPlan();
	m_movementSubsystem.Reset();
	blockedTimeoutAt = level.time + BLOCKED_TIMEOUT;
	navTarget = nullptr;
	m_selectedNavEntity = std::nullopt;
}

void Bot::Update() {
	// We should update weapons status each frame since script weapons may be changed each frame.
	// These statuses are used by firing methods, so actual weapon statuses are required.
	weaponsUsageModule.UpdateScriptWeaponsStatus();

	const int weakAmmoShift   = (int)AMMO_GUNBLADE - (int)WEAP_GUNBLADE;
	const int strongAmmoShift = (int)AMMO_WEAK_GUNBLADE - (int)WEAP_GUNBLADE;
	const auto *inventory     = self->r.client->ps.inventory;

	hasOnlyGunblade = true;
	for( int weapon = WEAP_GUNBLADE + 1; weapon < WEAP_TOTAL; ++weapon ) {
		if( inventory[weapon] && ( inventory[weapon + strongAmmoShift] || inventory[weapon + weakAmmoShift] ) ) {
			hasOnlyGunblade = false;
			break;
		}
	}

	m_pendingClientThinkInput = std::nullopt;

	if( !G_ISGHOSTING( self ) ) {
		entityPhysicsState->UpdateFromEntity( self );
	}

	if( level.spawnedTimeStamp + 5000 > game.realtime || !level.canSpawnEntities ) {
		self->nextThink = level.time + game.snapFrameTime;
	}

	if( G_ISGHOSTING( self ) ) {
		m_selectedEnemy = std::nullopt;
		m_lostEnemy     = std::nullopt;

		planningModule.ClearGoalAndPlan();

		m_movementSubsystem.Reset();

		navTarget           = nullptr;
		m_selectedNavEntity = std::nullopt;
		blockedTimeoutAt    = level.time + BLOCKED_TIMEOUT;

		// wait 3 seconds after entering the level
		if( self->r.client->levelTimestamp + 3000 < level.time && level.canSpawnEntities ) {
			bool trySpawning;
			if( self->r.client->team == TEAM_SPECTATOR ) {
				trySpawning = false;
				if( !self->r.client->queueTimeStamp && self == level.think_client_entity ) {
					G_Teams_JoinAnyTeam( self, false );
					if( self->r.client->team != TEAM_SPECTATOR ) {
						trySpawning = true;
					}
				}
			} else {
				// ask for respawn if the minimum bot respawning time passed
				trySpawning = level.time > self->deathTimeStamp + 3000;
			}

			if( trySpawning ) {
				m_pendingClientThinkInput = BotInput {};
				m_pendingClientThinkInput->isUcmdSet = true;
				m_pendingClientThinkInput->SetAttackButton( true );
			}
		}
	} else {
		//get ready if in the game
		if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP && !IsReady() && self->r.client->teamStateTimestamp + 4000 < level.time ) {
			G_Match_Ready( self, {} );
		}

		awarenessModule.Update();
		// Awareness stuff must be up-to date for planning.
		planner->Update();

		weaponsUsageModule.Frame( planningModule.CachedWorldState() );

		m_pendingClientThinkInput = BotInput {};

		// Might modify botInput
		m_movementSubsystem.Frame( std::addressof( *m_pendingClientThinkInput ) );

		CheckTargetProximity();

		// Might modify botInput
		if( ShouldAttack() ) {
			weaponsUsageModule.TryFire( std::addressof( *m_pendingClientThinkInput ) );
		}

		// Apply modified botInput
		m_movementSubsystem.ApplyInput( std::addressof( *m_pendingClientThinkInput ) );
	}

	assert( !m_selectedEnemy || !m_selectedEnemy->ShouldInvalidate() );

	if( !G_ISGHOSTING( self ) ) {
		if( PermitsDistributedUpdateThisFrame() ) {
			// TODO: Check whether we are camping/holding a spot
			if( !self->groundentity ) {
				blockedTimeoutAt = level.time + BLOCKED_TIMEOUT;
			} else if( self->groundentity->use == Use_Plat && VectorLengthSquared( self->groundentity->velocity ) > wsw::square( 1 ) ) {
				blockedTimeoutAt = level.time + BLOCKED_TIMEOUT;
			} else if( VectorLengthSquared( self->velocity ) > wsw::square( 30 ) ) {
				blockedTimeoutAt = level.time + BLOCKED_TIMEOUT;
			} else {
				// if completely stuck somewhere
				if( blockedTimeoutAt < level.time ) {
					OnBlockedTimeout();
				}
			}
			// TODO: Let the weapons usage module decide?
			if( CanChangeWeapons() ) {
				weaponsUsageModule.Think( planningModule.CachedWorldState() );
				ChangeWeapons( weaponsUsageModule.GetSelectedWeapons() );
			}
		}
	}

	if( m_pendingClientThinkInput ) {
		usercmd_t ucmd {};

		m_pendingClientThinkInput->CopyToUcmd( &ucmd );

		for( int i = 0; i < 3; i++ ) {
			ucmd.angles[i] = (short)ANGLE2SHORT( self->s.angles[i] ) - self->r.client->ps.pmove.delta_angles[i];
		}

		VectorSet( self->r.client->ps.pmove.delta_angles, 0, 0, 0 );

		// set approximate ping and show values
		ucmd.msec            = (uint8_t)game.frametime;
		ucmd.serverTimeStamp = game.serverTime;

		G_ClientThink( self, &ucmd, 0 );

		m_pendingClientThinkInput = std::nullopt;
	}

	self->nextThink = level.time + 1;
}

void Bot::OnMovementToNavTargetBlocked() {
	if( m_selectedNavEntity ) {
		// If a new nav target is set in blocked state, the bot remains blocked
		// for few millis since the ground acceleration is finite.
		// Prevent classifying just set nav targets as ones that have led to blocking.
		if( level.time - lastBlockedNavTargetReportedAt > 400 ) {
			lastBlockedNavTargetReportedAt = level.time;

			planningModule.OnMovementToNavEntityBlocked( m_selectedNavEntity->navEntity );

			planningModule.ClearGoalAndPlan();
			m_selectedNavEntity = std::nullopt;
		}
	}
}

bool Bot::NavTargetWorthRushing() const {
	if( ShouldBeSilent() || ShouldMoveCarefully() ) {
		return false;
	}

	if( ShouldRushHeadless() ) {
		return true;
	}

	if( !GS_SelfDamage( *ggs ) ) {
		return true;
	}

	// Force insta-jumps regardless of GS_SelfDamage( *ggs ) value
	if( GS_Instagib( *ggs ) && g_instajump->integer ) {
		// Check whether the bot really has an IG.
		const auto *inventory = self->r.client->ps.inventory;
		if( inventory[WEAP_INSTAGUN] && inventory[AMMO_INSTAS] ) {
			return true;
		}
	}

	// If the bot cannot refill health
	if( !( level.gametype.spawnableItemsMask & IT_HEALTH ) ) {
		// TODO: Allow it at the end of round. How to detect a round state in the native code?
		return false;
	}

	// Force jumps for pursuing enemies
	if( planningModule.IsPerformingPursuit() ) {
		return true;
	}

	// Don't jump if there's no pressure from enemies
	if( m_selectedEnemy == std::nullopt ) {
		// Duel-like gametypes are an exception
		if( !( GS_TeamBasedGametype( *ggs ) && GS_IndividualGametype( *ggs ) ) ) {
			return false;
		}
	}

	if( planningModule.IsTopTierItem( navTarget ) ) {
		return true;
	}

	return HasOnlyGunblade() && ( navTarget && navTarget->IsTopTierWeapon() );
}

int Bot::GetWeaponsForWeaponJumping( int *weaponNumsBuffer ) {
	// TODO: Implement more sophisticated logic
	if( ShouldBeSilent() || ShouldMoveCarefully() ) {
		return 0;
	}

	int numSuitableWeapons = 0;
	const auto *inventory = self->r.client->ps.inventory;

	if( g_instajump->integer ) {
		if( inventory[WEAP_INSTAGUN] && inventory[AMMO_INSTAS] ) {
			weaponNumsBuffer[numSuitableWeapons++] = WEAP_INSTAGUN;
		}
	}

	// We have decided to avoid using Shockwave...
	std::array<int, 2> rlPriorityWeapons = { { WEAP_ROCKETLAUNCHER, WEAP_GUNBLADE } };
	std::array<int, 2> gbPriorityWeapons = { { WEAP_GUNBLADE, WEAP_ROCKETLAUNCHER } };
	const std::array<int, 2> *weaponsList;

	if( g_allow_selfdamage->integer ) {
		weaponsList = &gbPriorityWeapons;
		float damageToKill = DamageToKill( self, g_armor_protection->value, g_armor_degradation->value );
		if( HasQuad( self ) ) {
			damageToKill *= 1.0f / QUAD_DAMAGE_SCALE;
		}
		if( HasShell( self ) ) {
			damageToKill *= QUAD_DAMAGE_SCALE;
			weaponsList = &rlPriorityWeapons;
		}

		for( int weapon: *weaponsList ) {
			if( inventory[weapon] && inventory[AMMO_GUNBLADE + ( weapon - WEAP_GUNBLADE )] ) {
				const auto &firedef = GS_GetWeaponDef( ggs, weapon )->firedef;
				if( firedef.damage * firedef.selfdamage + 15 < damageToKill ) {
					weaponNumsBuffer[numSuitableWeapons++] = weapon;
				}
			}
		}
	} else {
		// Prefer RL as it is very likely to be the CA gametype and high knockback is expected
		weaponsList = &rlPriorityWeapons;
		if( inventory[AMMO_ROCKETS] < 5 ) {
			// Save RL ammo in this case
			weaponsList = &gbPriorityWeapons;
		}
		for( int weapon: *weaponsList ) {
			if( inventory[weapon] && inventory[AMMO_GUNBLADE + ( weapon - WEAP_GUNBLADE )] ) {
				weaponNumsBuffer[numSuitableWeapons++] = weapon;
			}
		}
	}

	return numSuitableWeapons;
}

bool Bot::ShouldSkinBunnyInFavorOfCombatMovement() const {
	// Return a feasible value for this case
	if( m_selectedEnemy == std::nullopt ) {
		return false;
	}

	// Self-descriptive...
	if( ShouldRushHeadless() ) {
		return false;
	}

	// Prepare to avoid/dodge an EB/IG shot
	if( m_selectedEnemy->IsAboutToHitEBorIG() ) {
		return true;
	}

	// Prepare to avoid/dodge beams
	if( m_selectedEnemy->IsAboutToHitLGorPG() ) {
		return true;
	}

	// As its fairly rarely gets really detected, always return true in this case
	// (we tried first to apply an additional distance cutoff)
	return m_selectedEnemy->IsAboutToHitRLorSW();
}

bool Bot::IsCombatDashingAllowed() const {
	// Should not be called with this enemies state but lets return a feasible value for this case
	if( m_selectedEnemy == std::nullopt ) {
		return false;
	}

	// Avoid RL/EB shots
	if( m_selectedEnemy->IsAboutToHitRLorSW() || m_selectedEnemy->IsAboutToHitEBorIG() ) {
		return true;
	}

	// AD-AD spam vs a quad is pointless, the bot should flee away
	if( m_selectedEnemy->HasQuad() ) {
		return true;
	}

	if( const auto *hazard = PrimaryHazard() ) {
		// Always dash avoiding projectiles
		if( hazard->IsSplashLike() ) {
			return true;
		}
	}

	// Allow dashing for gaining speed to change a position
	return WillAdvance() || WillRetreat();
}

bool Bot::IsCombatCrouchingAllowed() const {
	if( m_selectedEnemy == std::nullopt ) {
		return false;
	}

	// If they're with EB and IG and are about to hit me
	if( m_selectedEnemy->IsAboutToHitEBorIG() ) {
		// TODO: Isn't that mutually exclusive?
		if( !m_selectedEnemy->IsAboutToHitRLorSW() && !m_selectedEnemy->IsAboutToHitLGorPG() ) {
			return true;
		}
	}

	return false;
}

float Bot::GetEffectiveOffensiveness() const {
	if( squad ) {
		return squad->IsSupporter( self ) ? 1.0f : 0.0f;
	}
	if( GS_MatchState( *ggs ) <= MATCH_STATE_WARMUP ) {
		return 1.0f;
	}
	if( m_selectedEnemy && m_selectedEnemy->IsACarrier() ) {
		return 0.75f;
	}
	return baseOffensiveness;
}

void Bot::SetAttitude( const edict_t *ent, int attitude_ ) {
	const int entNum       = ent->s.number;
	const auto oldAttitude = this->attitude[entNum];
	this->attitude[entNum] = (int8_t)attitude_;

	if( oldAttitude != attitude_ ) {
		// TODO: Invalidate enemy selection
		// OnAttitudeChanged( ent, oldAttitude, attitude_ );
	}
}

bool Bot::TryGetExtraComputationQuota() const {
	return MillisInBlockedState() < 100 && AiManager::Instance()->TryGetExpensiveComputationQuota( this );
}

bool Bot::TryGetVitalComputationQuota() const {
	return AiManager::Instance()->TryGetExpensiveComputationQuota( this );
}

bool Bot::TryGetExpensiveThinkCallQuota() const {
	return AiManager::Instance()->TryGetExpensiveThinkCallQuota( this );
}

float Bot::GetChangedAngle( float oldAngle, float desiredAngle, unsigned frameTime,
						   float angularSpeedMultiplier, int angleIndex ) const {
	float maxAngularMove = angularSpeedMultiplier * angularViewSpeed[angleIndex] * ( 1e-3f * frameTime );
	float angularMove = AngleNormalize180( desiredAngle - oldAngle );
	if( angularMove < -maxAngularMove ) {
		angularMove = -maxAngularMove;
	} else if( angularMove > maxAngularMove ) {
		angularMove = maxAngularMove;
	}

	return AngleNormalize180( oldAngle + angularMove );
}

Vec3 Bot::GetNewViewAngles( const vec3_t oldAngles, const Vec3 &desiredDirection,
						    unsigned frameTime, float angularSpeedMultiplier ) const {
	vec3_t newAngles, desiredAngles;
	VecToAngles( desiredDirection.data(), desiredAngles );
	assert( desiredAngles[ROLL] == 0.0f );

	for( auto angleNum: { YAW, PITCH } ) {
		// Normalize180 angles so they can be compared
		newAngles[angleNum]     = AngleNormalize180( oldAngles[angleNum] );
		desiredAngles[angleNum] = AngleNormalize180( desiredAngles[angleNum] );
		if( newAngles[angleNum] != desiredAngles[angleNum] ) {
			newAngles[angleNum] = GetChangedAngle( newAngles[angleNum], desiredAngles[angleNum],
												   frameTime, angularSpeedMultiplier, angleNum );
		}
	}

	newAngles[ROLL] = 0.0f;
	return Vec3( newAngles );
}

int Bot::CheckTravelTimeMillis( const Vec3& from, const Vec3 &to, bool allowUnreachable ) {
	// We try to use the same checks the TacticalSpotsRegistry performs to find spots.
	// If a spot is not reachable, it is an bug,
	// because a reachability must have been checked by the spots registry first in a few preceeding calls.

	int fromAreaNum;
	if( ( from - self->s.origin ).squareLength() < wsw::square( 4.0f ) ) {
		fromAreaNum = aasWorld->findAreaNum( self );
	} else {
		fromAreaNum = aasWorld->findAreaNum( from );
	}

	if( !fromAreaNum ) {
		if( allowUnreachable ) {
			return 0;
		}

		FailWith( "CheckTravelTimeMillis(): Can't find `from` AAS area" );
	}

	const int toAreaNum = aasWorld->findAreaNum( to.data() );
	if( !toAreaNum ) {
		if( allowUnreachable ) {
			return 0;
		}

		FailWith( "CheckTravelTimeMillis(): Can't find `to` AAS area" );
	}

	if( int aasTravelTime = routeCache->FindRoute( fromAreaNum, toAreaNum, TravelFlags() ) ) {
		return 10 * aasTravelTime;
	}

	if( allowUnreachable ) {
		return 0;
	}

	FailWith( "CheckTravelTimeMillis(): Can't find travel time %d->%d\n", fromAreaNum, toAreaNum );
}

bool Bot::IsDefinitelyNotAFeasibleEnemy( const edict_t *ent ) const {
	if( !ent->r.inuse ) {
		return true;
	}
	// Skip non-clients that do not have positive intrinsic entity weight
	if( !ent->r.client && ent->aiIntrinsicEnemyWeight <= 0.0f ) {
		return true;
	}
	// Skip ghosting entities
	if( G_ISGHOSTING( ent ) ) {
		return true;
	}
	// Skip chatting or notarget entities except carriers
	if( ( ent->flags & ( FL_NOTARGET | FL_BUSY ) ) && !( ent->s.effects & EF_CARRIER ) ) {
		return true;
	}
	// Skip teammates. Note that team overrides attitude
	if( GS_TeamBasedGametype( *ggs ) && ent->s.team == self->s.team ) {
		return true;
	}
	// Skip entities that has a non-negative bot attitude.
	// Note that by default all entities have negative attitude.
	if( attitude[ENTNUM( ent )] >= 0 ) {
		return true;
	}

	return self == ent;
}