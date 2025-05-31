#ifndef WSW_275299a7_5b3f_40c5_b8bf_d8ca21b22341_H
#define WSW_275299a7_5b3f_40c5_b8bf_d8ca21b22341_H

#include "movementscript.h"
#include "../navigation/aasroutecache.h"

class GenericGroundMovementScript: public MovementScript {
protected:
	bool allowRunning { true };
	bool allowDashing { true };
	bool allowAirAccel { true };
	bool allowCrouchSliding { true };

	virtual void GetSteeringTarget( vec3_t target ) = 0;

	bool ShouldSkipTests( PredictionContext *context = nullptr );

	int GetCurrBotAreas( int *areaNums, PredictionContext *context = nullptr );

	bool TestActualWalkability( int targetAreaNum, const vec3_t targetOrigin,
								PredictionContext *context = nullptr );

	void GetAreaMidGroundPoint( int areaNum, vec3_t target ) {
		const auto &area = AiAasWorld::instance()->getAreas()[areaNum];
		VectorCopy( area.center, target );
		target[2] = area.mins[2] + 1.0f - playerbox_stand_mins[2];
	}

	bool SetupForKeptPointInFov( PredictionContext *context,
								 const float *steeringTarget,
								 const float *keptInFovPoint );
public:
	static constexpr auto TRAVEL_FLAGS = TFL_WALK | TFL_AIR | TFL_WALKOFFLEDGE;

	GenericGroundMovementScript( const Bot *bot_, MovementSubsystem *subsystem, int debugColor_ )
		: MovementScript( bot_, subsystem, debugColor_ ) {}

	void SetupMovement( PredictionContext *context ) override;

	bool TryDeactivate( PredictionContext *context ) override;
};

#endif
