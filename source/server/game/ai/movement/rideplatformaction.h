#ifndef WSW_bbfd3c31_cdfd_4dd8_882f_b8854c95351e_H
#define WSW_bbfd3c31_cdfd_4dd8_882f_b8854c95351e_H

#include "baseaction.h"

class RidePlatformAction : public BaseAction {
	friend class MovementSubsystem;

public:
	explicit RidePlatformAction( MovementSubsystem *subsystem ) :
		BaseAction( subsystem, "RidePlatformAction", COLOR_RGB( 128, 128, 0 ) ) {}
	void PlanPredictionStep( PredictionContext *context ) override;
	void CheckPredictionStepResults( PredictionContext *context ) override;

	void BeforePlanning() override;

	void OnApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
private:
	unsigned m_currTestedAreaIndex { 0 };
	const edict_t *m_foundPlatform { nullptr };
	wsw::StaticVector<int, 24> m_exitAreas;

	// TODO: These should be separate stages of script
	enum Stage { StageWait, StageEnter, StageExit } m_stage { StageWait };

	const edict_t *GetPlatform( PredictionContext *context ) const;
	bool DetermineStageAndProperties( PredictionContext *context, const edict_t *platform );
	void FindSuitableAreasInBox( const Vec3 &boxMins, const Vec3 &boxMaxs, wsw::StaticVector<int, 24> *foundAreas );
	int FindNavTargetReachableAreas( std::span<const int> givenAreas, int navTargetAreaNum, wsw::StaticVector<int, 24> *foundAreas );

	void SetupEnterPlatformMovement( PredictionContext *context );
	void SetupExitPlatformMovement( PredictionContext *context );
	void SetupRidePlatformMovement( PredictionContext *context );
};

#endif
