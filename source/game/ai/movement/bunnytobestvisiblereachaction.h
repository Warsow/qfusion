#ifndef WSW_3a0dbfd3_81a9_4542_bf8b_ef1e60f50d7b_H
#define WSW_3a0dbfd3_81a9_4542_bf8b_ef1e60f50d7b_H

#include "bunnyhopaction.h"

class BunnyToBestVisibleReachAction: public BunnyHopAction {
public:
	explicit BunnyToBestVisibleReachAction( BaseScript2 *script )
		: BunnyHopAction( script, "BunnyToStairsOrRampExitAction", COLOR_RGB( 96, 0, 255 ) ) {}

	void PlanPredictionStep( MovementPredictionContext *context ) override;
};

#endif
