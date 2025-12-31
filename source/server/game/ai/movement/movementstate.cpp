#include "movementstate.h"
#include "movementlocal.h"

void AiEntityPhysicsState::UpdateAreaNums() {
	const AiAasWorld *const __restrict aasWorld = AiAasWorld::instance();
	this->currAasAreaNum = (uint16_t)aasWorld->findAreaNum( Origin() );
	// Use a computation shortcut when entity is on ground
	if( this->groundEntNum >= 0 ) {
		SetHeightOverGround( 0 );
		const Vec3 droppedOrigin( origin[0], origin[1], origin[2] + playerbox_stand_mins[2] + 8.0f );
		if( !( this->droppedToFloorAasAreaNum = (uint16_t)aasWorld->findAreaNum( droppedOrigin ) ) ) {
			this->droppedToFloorAasAreaNum = this->currAasAreaNum;
		}
	} else if( aasWorld->isAreaGrounded( this->currAasAreaNum ) ) {
		const float areaMinsZ = aasWorld->getAreas()[this->currAasAreaNum].mins[2];
		const float selfZ = Self()->s.origin[2];
		SetHeightOverGround( ( selfZ - areaMinsZ ) + playerbox_stand_mins[2] );
		this->droppedToFloorAasAreaNum = this->currAasAreaNum;
	} else {
		// Try dropping the origin to floor
		const edict_t *ent = Self();
		const Vec3 traceEnd( origin[0], origin[1], origin[2] - GROUND_TRACE_DEPTH );
		// TODO: We can replace this inefficient G_Trace() call
		// by clipping against nearby solid entities which could be cached
		trace_t trace;
		G_Trace( &trace, this->origin, ent->r.mins, ent->r.maxs, traceEnd.Data(), ent, MASK_PLAYERSOLID );
		// Check not only whether there is a hit but test whether is it really a ground (and not a wall or obstacle)
		if( ( trace.fraction != 1.0f ) && ( origin[2] - trace.endpos[2] ) > -playerbox_stand_mins[2] ) {
			SetHeightOverGround( ( trace.fraction * GROUND_TRACE_DEPTH ) + playerbox_stand_mins[2] );
			const Vec3 droppedOrigin( trace.endpos[0], trace.endpos[1], trace.endpos[2] + 8.0f );
			if( !( this->droppedToFloorAasAreaNum = (uint16_t)aasWorld->findAreaNum( droppedOrigin ) ) ) {
				this->droppedToFloorAasAreaNum = this->currAasAreaNum;
			}
		} else {
			SetHeightOverGround( std::numeric_limits<float>::infinity() );
			this->droppedToFloorAasAreaNum = this->currAasAreaNum;
		}
	}
}

void AiEntityPhysicsState::UpdateFromEntity( const edict_t *ent ) {
	VectorCopy( ent->s.origin, this->origin );
	SetVelocity( ent->velocity );
	this->waterType = ent->watertype;
	this->waterLevel = ( decltype( this->waterLevel ) )ent->waterlevel;
	// TODO: Get rid of packing
	this->angles[0] = ANGLE2SHORT( ent->s.angles[0] );
	this->angles[1] = ANGLE2SHORT( ent->s.angles[1] );
	this->angles[2] = ANGLE2SHORT( ent->s.angles[2] );
	vec3_t forward, right;
	AngleVectors( ent->s.angles, forward, right, nullptr );
	SetPackedDir( forward, this->forwardDir );
	SetPackedDir( right, this->rightDir );
	this->groundEntNum = -1;
	if( ent->groundentity ) {
		this->groundEntNum = ( decltype( this->groundEntNum ) )( ENTNUM( ent->groundentity ) );
	}
	this->selfEntNum = ( decltype( this->selfEntNum ) )ENTNUM( ent );
	// Compute lazily on demand in this case
	SetGroundNormalZ( 0 );

	UpdateAreaNums();
}

void AiEntityPhysicsState::UpdateFromPMove( const pmove_t *pmove ) {
	VectorCopy( pmove->playerState->pmove.origin, this->origin );
	SetVelocity( pmove->playerState->pmove.velocity );
	this->waterType = pmove->watertype;
	this->waterLevel = ( decltype( this->waterLevel ) )pmove->waterlevel;
	// TODO: Get rid of packing
	this->angles[0] = ANGLE2SHORT( pmove->playerState->viewangles[0] );
	this->angles[1] = ANGLE2SHORT( pmove->playerState->viewangles[1] );
	this->angles[2] = ANGLE2SHORT( pmove->playerState->viewangles[2] );
	SetPackedDir( pmove->forward, this->forwardDir );
	SetPackedDir( pmove->right, this->rightDir );
#if 0
	[[maybe_unused]] vec3_t forward, right;
		AngleVectors( pmove->playerState->viewangles, forward, right, nullptr );
		assert( DotProduct( pmove->forward, forward ) > 0.999f );
		assert( DotProduct( pmove->right, right ) > 0.999f );
#endif
	this->groundEntNum = ( decltype( this->groundEntNum ) )pmove->groundentity;
	this->selfEntNum = ( decltype( this->selfEntNum ) )( pmove->playerState->playerNum + 1 );
	SetGroundNormalZ( pmove->groundentity >= 0 ? pmove->groundplane.normal[2] : 0 );

	UpdateAreaNums();
}

void AiEntityPhysicsState::SetSpeed( const vec3_t velocity_ ) {
	float squareSpeed2D = velocity_[0] * velocity_[0] + velocity_[1] * velocity_[1];
	float squareSpeed   = squareSpeed2D + velocity_[2] * velocity_[2];
	this->speed         = Q_Sqrt( squareSpeed );
	this->speed2D       = Q_Sqrt( squareSpeed2D );
}

// Returns number of start areas to use in routing
int AiEntityPhysicsState::PrepareRoutingStartAreas( int *areaNums ) const {
	areaNums[0] = areaNums[1] = 0;

	int numAreas = 0;

	if( int areaNum = CurrAasAreaNum() ) {
		areaNums[numAreas++] = areaNum;
	}

	if( int areaNum = DroppedToFloorAasAreaNum() ) {
		if( numAreas ) {
			if( areaNums[0] != areaNum ) {
				areaNums[numAreas++] = areaNum;
			}
		} else {
			areaNums[numAreas++] = areaNum;
		}
	}

	return numAreas;
}

void AiEntityPhysicsState::SetHeightOverGround( float heightOverGround_ ) {
	if( heightOverGround_ >= 0.0f ) {
		if( heightOverGround_ <= GROUND_TRACE_DEPTH ) {
			this->heightOverGround = ( uint16_t )( heightOverGround_ * 256 );
		} else {
			this->heightOverGround = ( uint16_t )( ( GROUND_TRACE_DEPTH + 1 ) * 256 + 1 );
		}
	} else {
		this->heightOverGround = 0;
	}
}

float AiEntityPhysicsState::GetGroundNormalZ() const {
	if( groundNormalZ != 0 ) {
		return groundNormalZ / std::numeric_limits<int16_t>::max();
	}
	if( groundEntNum < 0 ) {
		return 0;
	}

	// In worst case that is rarely gets triggered the bot is on ground
	// but the ground normal has not been computed yet, and was not initially available.
	// Compute it right now following PMove() implementation.
	// This lazy approach really helps reducing amount of expensive trace calls.
	trace_t trace;
	auto *start = const_cast<float *>( Origin() );
	Vec3 end( Origin() );
	end.Z() -= 0.25f;
	edict_t *self = game.edicts + selfEntNum;
	G_Trace( &trace, start, playerbox_stand_mins, playerbox_stand_maxs, end.Data(), self, MASK_PLAYERSOLID );
	if( trace.fraction != 1.0f ) {
		groundNormalZ = (int16_t)( trace.plane.normal[2] / std::numeric_limits<int16_t>::max() );
	}
	return groundNormalZ;
}
