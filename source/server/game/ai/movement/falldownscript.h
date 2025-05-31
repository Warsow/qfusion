#ifndef WSW_413f6f70_a16a_44e1_b088_a54019523663_H
#define WSW_413f6f70_a16a_44e1_b088_a54019523663_H

#include "movementscript.h"

class FallDownScript: public MovementScript {
	vec3_t startOrigin { 0, 0, 0 };
	vec3_t targetOrigin { 0, 0, 0 };
	unsigned timeout { 0 };
	float reachRadius { 0.0f };
public:
	explicit FallDownScript( const Bot *bot_, MovementSubsystem *subsystem )
		: MovementScript( bot_, subsystem, COLOR_RGB( 128, 0, 0 ) ) {}

	// Note: It is expected that bot origin Z should be <= target origin Z
	// after completion of the fallback, so target Z matters a lot!
	// Timeout is variable and should be set according to estimated sum of traveling to the ledge and falling
	void Activate( const vec3_t startOrigin_, const vec3_t targetOrigin_, unsigned timeout_, float reachRadius_ = 32.0f ) {
		VectorCopy( startOrigin_, this->startOrigin );
		VectorCopy( targetOrigin_, this->targetOrigin );
		this->timeout = timeout_;
		this->reachRadius = reachRadius_;
		MovementScript::Activate();
	}

	bool TryDeactivate( PredictionContext *context = nullptr ) override;

	void SetupMovement( PredictionContext *context ) override;
};

#endif
