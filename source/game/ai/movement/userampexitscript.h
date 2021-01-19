#ifndef WSW_8f6ea35a_cfc5_4b00_b6ca_b220465583a0_H
#define WSW_8f6ea35a_cfc5_4b00_b6ca_b220465583a0_H

#include "genericgroundmovementscript.h"

/*
class UseRampExitScript: public GenericGroundMovementScript {
	int rampAreaNum { 0 };
	int exitAreaNum { 0 };

	void GetSteeringTarget( vec3_t target ) override {
		return GetAreaMidGroundPoint( exitAreaNum, target );
	}
public:
	UseRampExitScript( const Bot *bot_, BotMovementModule *module_ )
		: GenericGroundMovementScript( bot_, module_, COLOR_RGB( 192, 0, 0 ) ) {}

	void Activate( int rampAreaNum_, int exitAreaNum_ ) {
		this->rampAreaNum = rampAreaNum_;
		this->exitAreaNum = exitAreaNum_;
		GenericGroundMovementScript::Activate();
	}

	bool TryDeactivate( MovementPredictionContext *context = nullptr ) override;
};
*/

const int *TryFindBestInclinedFloorExitArea( MovementPredictionContext *context,
											 int rampAreaNum,
											 int forbiddenAreaNum = 0 );

#endif
