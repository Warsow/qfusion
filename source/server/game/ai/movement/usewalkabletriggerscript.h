#ifndef WSW_514b316f_16bd_445d_b50b_d8cb341777e8_H
#define WSW_514b316f_16bd_445d_b50b_d8cb341777e8_H

#include "genericgroundmovementscript.h"

class UseWalkableTriggerScript: public GenericGroundMovementScript {
	const edict_t *trigger { nullptr };

	void GetSteeringTarget( vec3_t target ) override;
public:
	explicit UseWalkableTriggerScript( const Bot *bot_, MovementSubsystem *subsystem )
		: GenericGroundMovementScript( bot_, subsystem, COLOR_RGB( 192, 0, 192 ) ) {}

	void Activate( const edict_t *trigger_ ) {
		this->trigger = trigger_;
		GenericGroundMovementScript::Activate();
	}

	bool TryDeactivate( PredictionContext *context = nullptr ) override;
};

#endif
