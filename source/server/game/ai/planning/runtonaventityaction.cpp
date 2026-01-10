#include "planninglocal.h"

void RunToNavEntityActionRecord::Activate() {
	BotActionRecord::Activate();
	assert( m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) );
	// Set the nav target first as it gets used by further calls
	Self()->SetNavTarget( m_selectedNavEntity.navEntity );
	// Attack if view angles needed for movement fit aiming
	Self()->GetMiscTactics().PreferRunRatherThanAttack();
	// TODO: It's better to supply the cached world state via Activate() method argument
	Self()->GetMiscTactics().shouldBeSilent = ShouldUseSneakyBehaviour( Self()->CachedWorldState() );
}

void RunToNavEntityActionRecord::Deactivate() {
	BotActionRecord::Deactivate();
	Self()->ResetNavTarget();
}

AiActionRecord::Status RunToNavEntityActionRecord::UpdateStatus( const WorldState &currWorldState ) {
	if( !m_selectedNavEntity.isSame( Self()->GetSelectedNavEntity() ) ) {
		Debug( "The actual selected nav entity differs from the stored one\n" );
		return INVALID;
	}

	if( m_selectedNavEntity.navEntity->Origin().fastDistanceTo( Self()->Origin() ) <= GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Nav target pickup distance has been reached\n" );
		return COMPLETED;
	}

	// Update upon each check
	Self()->GetMiscTactics().shouldBeSilent = ShouldUseSneakyBehaviour( currWorldState );
	return VALID;
}

bool RunToNavEntityActionRecord::ShouldUseSneakyBehaviour( const WorldState &currWorldState ) const {
	const NavEntity *const navEntity = m_selectedNavEntity.navEntity;

	// Hack for following a sneaky movement of a leader (if any).
	if( !navEntity->IsClient() ) {
		return false;
	}

	const edict_t *const targetEnt = game.edicts + navEntity->EntityId();
	const edict_t *const botEnt = game.edicts + Self()->EntNum();

	// Make sure the target is in the same team
	if( botEnt->s.team < TEAM_ALPHA || targetEnt->s.team != botEnt->s.team ) {
		return false;
	}

	// Disallow following a sneaky behaviour of a bot leader.
	// Bots still have significant troubles with movement.
	// Bot movement troubles produce many false positives.
	if( targetEnt->r.svflags & SVF_FAKECLIENT ) {
		return false;
	}

	// If there's a defined enemy origin (and a defined enemy consequently)
	if( currWorldState.getVec3( WorldState::EnemyOrigin ) ) {
		// TODO: Lift testing of a value within the "Ignore" monad

		// Interrupt sneaking immediately if there's a threatening enemy
		if( isSpecifiedAndTrue( currWorldState.getBool( WorldState::HasThreateningEnemy ) ) ) {
			return false;
		}
		// Don't try being sneaky if the bot can hit an enemy
		// (we can try doing that but bot behaviour is rather poor)
		if( isSpecifiedAndTrue( currWorldState.getBool( WorldState::CanHitEnemy ) ) ) {
			return false;
		}
		// Interrupt sneaking immediately if an enemy can hit
		if( !isSpecifiedAndTrue( currWorldState.getBool( WorldState::EnemyCanHit ) ) ) {
			return false;
		}
	}

	const float *const velocity = targetEnt->velocity;
	// Check whether the leader seems to be using a sneaky movement
	const float maxSpeedThreshold = GS_DefaultPlayerSpeed( *ggs ) * 1.5f;
	const float squareVelocity2D = velocity[0] * velocity[0] + velocity[1] * velocity[1];
	if( squareVelocity2D > maxSpeedThreshold * maxSpeedThreshold ) {
		return false;
	}

	// The bot should be relatively close to the target client.
	// The threshold value is chosen having bomb gametype in mind to prevent a site rush disclosure
	const float maxDistanceThreshold = 1024.0f + 512.0f;
	const float squareDistanceToTarget = DistanceSquared( botEnt->s.origin, targetEnt->s.origin );
	if( squareDistanceToTarget > maxDistanceThreshold * maxDistanceThreshold ) {
		return false;
	}

	// Check whether the leader is in PVS for the bot.
	// Note: either retrieval of this value is cheap as it's cached
	// or the newly computed result is going to be useful.
	if( !EntitiesPvsCache::Instance()->AreInPvs( botEnt, targetEnt ) ) {
		return false;
	}

	// Skip further tests if the bot is fairly close to the target client and follow the sneaky behaviour of the client.
	if( squareDistanceToTarget < 128 * 128 ) {
		return true;
	}

	// Feel free to run/jump/dash in this case
	if( !IsInPhsForEnemyTeam() ) {
		return false;
	}

	// Skip further tests if the client seems to be walking (having a walk key held).
	// Holding a walk key pressed is a way to look at bot teammates without forcing them rushing.
	// TODO: Avoid these magic numbers (there's no numeric constant for this currently)
	if( squareVelocity2D < 200 * 200 ) {
		return true;
	}

	// Check whether the bot is approximately in the client's fov
	// and the client looks approximately at the bot
	// (do not confuse this with assistance tests that are much more strict).

	vec3_t targetLookDir;
	AngleVectors( targetEnt->s.angles, targetLookDir, nullptr, nullptr );
	Vec3 targetToBotDir( Q_RSqrt( squareDistanceToTarget ) * Vec3( botEnt->s.origin ) - targetEnt->s.origin );
	// If the bot not in the center of the leader view
	if( targetToBotDir.dot( targetLookDir ) < 0.7f ) {
		return true;
	}

	// Check whether the bot looks approximately at the leader as well.
	// This allows to behave realistically "understanding" a leader intent.
	if( targetToBotDir.dot( Self()->EntityPhysicsState()->ForwardDir() ) > 0.3f ) {
		return true;
	}

	trace_t trace;
	Vec3 traceStart( Vec3( targetEnt->s.origin ) + Vec3( 0, 0, targetEnt->viewheight ) );
	// Do an approximate test whether the bot is behind a wall or an obstacle
	SolidWorldTrace( &trace, traceStart.data(), botEnt->s.origin );
	if( trace.fraction != 1.0f ) {
		Vec3 traceEnd( Vec3( botEnt->s.origin ) + Vec3( 0, 0, playerbox_stand_maxs[2] ) );
		SolidWorldTrace( &trace, traceStart.data(), traceEnd.data() );
		if( trace.fraction != 1.0f ) {
			return false;
		}
	}

	// Don't be sneaky. Hurry up to follow the leader.
	return false;
}

bool RunToNavEntityActionRecord::IsInPhsForEnemyTeam() const {
	const edict_t *botEnt = game.edicts + Self()->EntNum();
	// This method is only allowed to be called in team-based gametypes
	assert( botEnt->s.team == TEAM_ALPHA || botEnt->s.team == TEAM_BETA );

	const auto &enemyTeamList = ::teamlist[( botEnt->s.team == TEAM_ALPHA ) ? TEAM_BETA : TEAM_ALPHA];
	// These values are entity numbers and not client ones (contrary to one might guess by the name)
	const auto *enemyEntIndices = enemyTeamList.playerIndices;
	const auto *gameEdicts = game.edicts;

	// There is actually no potentially-hearable set.
	// Sounds are transmitted just being in a sufficient range.
	// Try finding an enemy client in-game that is fairly close to hear the bot.
	// This is coarse and does not match real attenuation formulae but is fine for driving a bot behaviour.
	// This is some kind of cheating as this can reveal nearby enemies for a human player in a bot team.
	// However playing with bots is not a competitive environment
	// and the distance threshold is fairly high to spot an exact nearby enemy position.
	for( int i = 0, end = enemyTeamList.numplayers; i < end; ++i ) {
		const auto *enemyEnt = gameEdicts + enemyEntIndices[i];
		// Sane team-based gametypes should disallow free-fly spectating
		if( G_ISGHOSTING( enemyEnt ) ) {
			continue;
		}
		// Assume player sounds can be heard at this range
		if( DistanceSquared( botEnt->s.origin, enemyEnt->s.origin ) < 1500 * 1500 ) {
			return true;
		}
	}

	return false;
}

PlannerNode *RunToNavEntityAction::TryApply( const WorldState &worldState ) {
	const std::optional<Vec3> navTargetOriginVar = worldState.getVec3( WorldState::NavTargetOrigin );
	if( !navTargetOriginVar ) {
		Debug( "Nav target is ignored in the given world state\n" );
		return nullptr;
	}

	const Vec3 botOrigin = worldState.getVec3( WorldState::BotOrigin ).value();
	if( botOrigin.fastDistanceTo( *navTargetOriginVar ) <= GOAL_PICKUP_ACTION_RADIUS ) {
		Debug( "Distance to goal item nav target is too low in the given world state\n" );
		return nullptr;
	}

	if( ( botOrigin - Self()->Origin() ).squareLength() > 1.0f ) {
		Debug( "Selected goal item is valid only for current bot origin\n" );
		return nullptr;
	}

	const std::optional<SelectedNavEntity> maybeSelectedNavEntity = Self()->GetSelectedNavEntity();
	const SelectedNavEntity &selectedNavEntity = maybeSelectedNavEntity.value();
	RunToNavEntityActionRecord *record = pool.New( Self(), selectedNavEntity );

	// TODO: The cost is totally wrong, supply travel time millis !!!!!!!!!!!!!!!!
	PlannerNode *plannerNode = newNodeForRecord( record, worldState, selectedNavEntity.cost );
	if( !plannerNode ) {
		return nullptr;
	}

	plannerNode->worldState.setVec3( WorldState::BotOrigin, *navTargetOriginVar );
	// TODO plannerNode.WorldState().ResetTacticalSpots();

	return plannerNode;
}