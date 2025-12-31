#ifndef WSW_8d257c89_c2f5_4704_bbac_c272921e7fda_H
#define WSW_8d257c89_c2f5_4704_bbac_c272921e7fda_H

#include "botinput.h"

class PredictionContext;

class alignas ( 4 )AiEntityPhysicsState {
	// Fields of this class are packed to allow cheap copying of class instances in bot movement prediction code
	static constexpr float GROUND_TRACE_DEPTH = 128.0f;
	// These fields are accessed way too often, so packing benefits does not outweigh unpacking performance loss.
	vec3_t origin;
	vec3_t velocity;
	float speed;
	float speed2D;
	// Unpacking of these fields is much cheaper than calling AngleVectors() that uses the expensive fsincos instruction
	// 12 bytes totally
	int16_t forwardDir[3];
	int16_t rightDir[3];

	static void SetPackedDir( const vec3_t dir, int16_t *result ) {
		// Do not multiply by the exact 2 ^ 15 value, leave some space for vector components slightly > 1.0f
		result[0] = (signed short)( dir[0] * 30000 );
		result[1] = (signed short)( dir[1] * 30000 );
		result[2] = (signed short)( dir[2] * 30000 );
	}
	static Vec3 UnpackedDir( const int16_t *packedDir ) {
		float scale = 1.0f / 30000;
		return Vec3( scale * packedDir[0], scale * packedDir[1], scale * packedDir[2] );
	}

public:
	// CONTENTS flags, cannot be compressed
	int waterType;
private:
	int16_t angles[3];
	static_assert( MAX_EDICTS < ( 1 << 15 ), "Fields bits count assumes 2^15 as game entities count limit" );
	// Use a signed type for indicating an absent ground entity by a negative value
	int16_t groundEntNum;
	uint16_t selfEntNum;
	// This needs some precision (can be used to restore trace fraction if needed), so its packed into 2 bytes
	uint16_t heightOverGround;

	void SetHeightOverGround( float heightOverGround_ );

	mutable int16_t groundNormalZ;

	void SetGroundNormalZ( float value ) {
		this->groundNormalZ = (int16_t)( value * std::numeric_limits<int16_t>::max() );
	}
private:
	uint16_t currAasAreaNum;
	uint16_t droppedToFloorAasAreaNum;

	void SetSpeed( const vec3_t velocity_ );

	void UpdateAreaNums();

public:
	uint8_t waterLevel;

	AiEntityPhysicsState()
		: speed( 0 )
		, speed2D( 0 )
		, waterType( 0 )
		, groundEntNum( 0 )
		, selfEntNum( 0 )
		, heightOverGround( 0 )
		, groundNormalZ( 0 )
		, currAasAreaNum( 0 )
		, droppedToFloorAasAreaNum( 0 )
		, waterLevel( 0 ) {}

	void UpdateFromEntity( const edict_t *ent );
	void UpdateFromPMove( const pmove_t *pmove );

	float HeightOverGround() const {
		if( heightOverGround <= GROUND_TRACE_DEPTH * 256 ) {
			return heightOverGround / 256.0f;
		}
		return std::numeric_limits<float>::infinity();
	}

	// If true, reachability checks do not make sense, wait for landing.
	bool IsHighAboveGround() const {
		return heightOverGround > GROUND_TRACE_DEPTH * 256;
	}

	const edict_t *GroundEntity() const {
		return groundEntNum >= 0 ? game.edicts + groundEntNum : nullptr;
	}
	const edict_t *Self() const { return game.edicts + selfEntNum; }

	Vec3 Angles() const {
		return Vec3( (float)SHORT2ANGLE( angles[0] ), (float)SHORT2ANGLE( angles[1] ), (float)SHORT2ANGLE( angles[2] ) );
	}

	int CurrAasAreaNum() const { return (int)currAasAreaNum; }
	int DroppedToFloorAasAreaNum() const { return (int)droppedToFloorAasAreaNum; }

	// Do not expose origin/velocity directly.
	// These accessors help to trace access to origin, and packing is yet an open question.
	// A bug have already been spotted using this access tracing.

	const float *Origin() const { return origin; }
	void SetOrigin( const vec3_t origin_ ) { VectorCopy( origin_, this->origin ); }
	void SetOrigin( const Vec3 &origin_ ) { SetOrigin( origin_.Data() ); }

	const float *Velocity() const { return velocity; }
	void SetVelocity( const vec3_t velocity_ ) {
		VectorCopy( velocity_, this->velocity );
		SetSpeed( velocity_ );
	}
	void SetVelocity( const Vec3 &velocity_ ) { SetVelocity( velocity_.Data() ); }

	float Speed() const { return speed; }
	float Speed2D() const { return speed2D; }
	// These getters are provided for compatibility with the other code
	float SquareSpeed() const {
		float unpackedSpeed = Speed();
		return unpackedSpeed * unpackedSpeed;
	}
	float SquareSpeed2D() const {
		float unpackedSpeed2D = Speed2D();
		return unpackedSpeed2D * unpackedSpeed2D;
	}

	Vec3 ForwardDir() const { return UnpackedDir( forwardDir ); }
	Vec3 RightDir() const { return UnpackedDir( rightDir ); }

	// Returns number of start areas to use in routing
	int PrepareRoutingStartAreas( int *areaNums ) const;

	float GetGroundNormalZ() const;
};

class alignas ( 2 )AiCampingSpot {
	// Fields of this class are packed to allow cheap copying of class instances in bot movement prediction code
	friend class Bot;
	friend class CampingSpotState;

	int16_t origin[3];
	int16_t lookAtPoint[3];
	uint16_t radius;
	uint8_t alertness;
	AiCampingSpot() : radius( 32 ), alertness( 255 ), hasLookAtPoint( false ) {}

public:
	bool hasLookAtPoint;

	float Radius() const { return radius; }
	float Alertness() const { return alertness / 256.0f; }
	Vec3 Origin() const { return GetUnpacked4uVec( origin ); }
	Vec3 LookAtPoint() const { return GetUnpacked4uVec( lookAtPoint ); }

	// Warning! This does not set hasLookAtPoint, only used to store a vector in (initially unsused) lookAtPoint field
	// This behaviour is used when lookAtPoint is controlled manually by an external code.
	void SetLookAtPoint( const Vec3 &lookAtPoint_ ) { SetPacked4uVec( lookAtPoint_, lookAtPoint ); }

	AiCampingSpot( const Vec3 &origin_, float radius_, float alertness_ = 0.75f )
		: radius( (uint16_t)( radius_ ) ), alertness( (uint8_t)( alertness_ * 255 ) ), hasLookAtPoint( false )
	{
		SetPacked4uVec( origin_, origin );
	}

	AiCampingSpot( const vec3_t &origin_, float radius_, float alertness_ = 0.75f )
		: radius( (uint16_t)radius_ ), alertness( (uint8_t)( alertness_ * 255 ) ), hasLookAtPoint( false )
	{
		SetPacked4uVec( origin_, origin );
	}

	AiCampingSpot( const vec3_t &origin_, const vec3_t &lookAtPoint_, float radius_, float alertness_ = 0.75f )
		: radius( (uint16_t)radius_ ), alertness( (uint8_t)( alertness_ * 255 ) ), hasLookAtPoint( true )
	{
		SetPacked4uVec( origin_, origin );
		SetPacked4uVec( lookAtPoint_, lookAtPoint );
	}

	AiCampingSpot( const Vec3 &origin_, const Vec3 &lookAtPoint_, float radius_, float alertness_ = 0.75f )
		: radius( (uint16_t)radius_ ), alertness( (uint8_t)( alertness_ * 255 ) ), hasLookAtPoint( true )
	{
		SetPacked4uVec( origin_, origin );
		SetPacked4uVec( lookAtPoint_, lookAtPoint );
	}
};

class alignas ( 2 )AiPendingLookAtPoint {
	friend class MovementSubsystem;

	int16_t origin[3];
	// Floating point values greater than 1.0f are allowed (unless they are significantly greater than 1.0f);
	uint16_t turnSpeedMultiplier;

	AiPendingLookAtPoint() {
		// Shut an analyzer up
		turnSpeedMultiplier = 16;
	}

public:
	Vec3 Origin() const { return GetUnpacked4uVec( origin ); }
	float TurnSpeedMultiplier() const { return turnSpeedMultiplier / 16.0f; };

	AiPendingLookAtPoint( const vec3_t origin_, float turnSpeedMultiplier_ )
		: turnSpeedMultiplier( (uint16_t)( wsw::min( 255.0f, turnSpeedMultiplier_ * 16.0f ) ) )
	{
		SetPacked4uVec( origin_, origin );
	}

	AiPendingLookAtPoint( const Vec3 &origin_, float turnSpeedMultiplier_ )
		: turnSpeedMultiplier(  (uint16_t)( wsw::min( 255.0f, turnSpeedMultiplier_ * 16.0f ) ) )
	{
		SetPacked4uVec( origin_, origin );
	}
};

struct MovementState {
	AiEntityPhysicsState entityPhysicsState;
	// A current input rotation kind that is used in this state.
	// This value is saved to prevent choice jitter trying to apply an input rotation.
	// (The current input rotation kind has a bit less restrictive application conditions).
	InputRotation inputRotation { InputRotation::NONE };
};

#endif
