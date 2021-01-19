#ifndef WSW_0a44f827_8554_4545_9e9f_76cda68b5bdd_H
#define WSW_0a44f827_8554_4545_9e9f_76cda68b5bdd_H

#include "movementscript.h"

/*
class JumpOverBarrierScript: public MovementScript {
	vec3_t start { 0, 0, 0 };
	vec3_t top { 0, 0, 0 };
	bool hasReachedStart { false };
	bool allowWalljumping { false };
public:
	explicit JumpOverBarrierScript( const Bot *bot_, BotMovementModule *module_ )
		: MovementScript( bot_, module_, COLOR_RGB( 128, 0, 128 ) ) {}

	void Activate( const vec3_t start_, const vec3_t top_, bool allowWalljumping_ = true ) {
		VectorCopy( start_, start );
		VectorCopy( top_, top );
		hasReachedStart = false;
		allowWalljumping = allowWalljumping_;
		MovementScript::Activate();
	}

	bool TryDeactivate( MovementPredictionContext *context = nullptr ) override;

	void SetupMovement( MovementPredictionContext *context ) override;
};*/

#endif
