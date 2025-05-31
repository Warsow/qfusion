#ifndef WSW_8918cd6a_85ef_4864_bcd2_4ef944792615_H
#define WSW_8918cd6a_85ef_4864_bcd2_4ef944792615_H

#include "genericgroundmovementscript.h"

class UseStairsExitScript: public GenericGroundMovementScript {
	int stairsClusterNum { 0 };
	int exitAreaNum { 0 };

	void GetSteeringTarget( vec3_t target ) override {
		GetAreaMidGroundPoint( exitAreaNum, target );
	}
public:
	UseStairsExitScript( const Bot *bot_, MovementSubsystem *subsystem )
		: GenericGroundMovementScript( bot_, subsystem, COLOR_RGB( 0, 0, 192 ) ) {}

	void Activate( int stairsClusterNum_, int stairsExitAreaNum_ ) {
		this->stairsClusterNum = stairsClusterNum_;
		this->exitAreaNum = stairsExitAreaNum_;
		GenericGroundMovementScript::Activate();
	}

	bool TryDeactivate( PredictionContext *context = nullptr ) override;
};

const uint16_t *TryFindBestStairsExitArea( PredictionContext *context, int stairsClusterNum );

#endif
