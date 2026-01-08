/*
Copyright (C) 2026 vvk2212, Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef WSW_7c18a3ec_2ac4_4d7d_9a93_86be3b387050_H
#define WSW_7c18a3ec_2ac4_4d7d_9a93_86be3b387050_H

#include "baseaction.h"

class WalkToPointAction : public BaseAction {
public:
	explicit WalkToPointAction( MovementSubsystem *subsystem ) :
		BaseAction( subsystem, "WalkToPointAction", COLOR_RGB( 0, 128, 0 ) ) {}

	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
	[[nodiscard]]
	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;

	void setTargetPoint( const Vec3 &targetPoint ) { m_targetPoint = targetPoint; }
	void setAllowToReachThePointInAir( bool allow ) { m_allowToReachThePointInAir = true; }
	void setWalkProximityThreshold( float distance ) { m_walkProximityThreshold = distance; }
	void setMaxAllowed2DSpeedAtTargetPoint( float speed ) { m_maxAllowed2DSpeedAtTargetPoint = speed; }
private:
	void beforePlanning() override;
	void afterPlanning() override;

	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
									   unsigned stoppedAtFrameIndex ) override;

	void bumpDashDistance();
	void bumpJumpDistance();

	static constexpr float kBaseMinDistanceFromTargetToDash { 108.0f };
	static constexpr float kBaseMinDistanceFromTargetToJump { 96.0f };

	Vec3 m_targetPoint { 0.0f, 0.0f, 0.0f };
	float m_minDistanceFromTargetToJump { 0.0f };
	float m_minDistanceFromTargetToDash { 0.0f };
	float m_distanceFromStartToTarget { 0.0f };
	float m_walkProximityThreshold { 8.0f };
	float m_maxAllowed2DSpeedAtTargetPoint { DEFAULT_PLAYERSPEED_STANDARD + 1.0f };
	bool m_allowToReachThePointInAir { false };
	bool m_isDashingAllowed { false };
	bool m_isJumpingAllowed { false };
	bool m_hasJumped { false };
	bool m_hasDashedOrWalljumped { false };
	bool m_isDisabledForPlanning { false };
};

class WalkToPointScript : public PredictingAndCachingMovementScript {
public:
	explicit WalkToPointScript( MovementSubsystem *movementSubsystem )
		: PredictingAndCachingMovementScript( movementSubsystem ), m_walkToPointAction( movementSubsystem ) {
		m_movementActions = m_storageOfActionPtrs;
	}

	[[nodiscard]]
	bool produceBotInput( BotInput *input ) override;

	void setTargetPoint( const Vec3 &targetPoint ) {
		m_predictedMovementActions.clear();
		m_targetPoint = targetPoint;
	}
private:
	WalkToPointAction m_walkToPointAction;
	BaseAction *m_storageOfActionPtrs[1] { &m_walkToPointAction };

	Vec3 m_targetPoint { 0.0f, 0.0f, 0.0f };
};

class JumpToPointAction : public BaseAction {
public:
	explicit JumpToPointAction( MovementSubsystem *subsystem ) :
		BaseAction( subsystem, "JumpToPointAction", COLOR_RGB( 192, 192, 32 ) ) {}

	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
	[[nodiscard]]
	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;

	void setTarget( const Vec3 &targetPoint, int targetAreaNum ) {
		assert( targetAreaNum >= 0 );
		m_targetPoint   = targetPoint;
		m_targetAreaNum = targetAreaNum;
	}
private:
	void beforePlanning() override;
	void afterPlanning() override;

	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
									   unsigned stoppedAtFrameIndex ) override;

	static constexpr unsigned kMaxAttempts = 4;

	Vec3 m_startPoint { 0.0f, 0.0f, 0.0f };
	Vec3 m_targetPoint { 0.0f, 0.0f, 0.0f };
	int m_targetAreaNum { 0 };
	unsigned m_attemptNum { 0 };
	bool m_hasJumped { false };
	bool m_isDisabledForPlanning { false };
};

class SwitchingActionsForStateScript : public PredictingAndCachingMovementScript {
public:
	explicit SwitchingActionsForStateScript( MovementSubsystem *movementSubsystem )
		: PredictingAndCachingMovementScript( movementSubsystem ) {
		m_movementActions = m_storageOfActionPtrs;
	}
protected:
	void selectActiveAction( BaseAction *action );
private:
	BaseAction *m_storageOfActionPtrs[1] { nullptr };
};

class TraverseJumpReachScript : public SwitchingActionsForStateScript {
public:
	explicit TraverseJumpReachScript( MovementSubsystem *movementSubsystem )
		: SwitchingActionsForStateScript( movementSubsystem )
		, m_walkToPointAction( movementSubsystem )
		, m_jumpToPointAction( movementSubsystem ) {}

	[[nodiscard]]
	bool produceBotInput( BotInput *input ) override;

	void setTargetReachNum( int reachNum ) {
		m_predictedMovementActions.clear();
		m_targetReachNum = reachNum;
	}
private:
	WalkToPointAction m_walkToPointAction;
	JumpToPointAction m_jumpToPointAction;
	int m_targetReachNum { 0 };
};

class LandOnPointAction : public BaseAction {
public:
	explicit LandOnPointAction( MovementSubsystem *subsystem ) :
		BaseAction( subsystem, "LandOnPointAction", COLOR_RGB( 0, 192, 192 ) ) {}

	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
	[[nodiscard]]
	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;

	void setTarget( const Vec3 &targetPoint, int targetAreaNum ) {
		assert( targetAreaNum >= 0 );
		m_targetPoint   = targetPoint;
		m_targetAreaNum = targetAreaNum;
	}

	[[nodiscard]]
	auto getTargetAreaNum() -> int { return m_targetAreaNum; }
private:
	void beforePlanning() override;
	void afterPlanning() override;

	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
									   unsigned stoppedAtFrameIndex ) override;

	Vec3 m_targetPoint { 0.0f, 0.0f, 0.0f };
	int m_targetAreaNum { 0 };
	unsigned m_attemptNum { 0 };
	bool m_isDisabledForPlanning { false };
	bool m_tryUsingDirectionKeys { false };
	bool m_tryUsingCheatingCorrection { false };
};

class TraverseWalkOffLedgeReachScript : public SwitchingActionsForStateScript {
public:
	explicit TraverseWalkOffLedgeReachScript( MovementSubsystem *movementSubsystem )
		: SwitchingActionsForStateScript( movementSubsystem )
		, m_walkToPointAction( movementSubsystem )
		, m_landOnPointAction( movementSubsystem ) {}

	[[nodiscard]]
	bool produceBotInput( BotInput *input ) override;

	void setTargetReachNum( int reachNum ) {
		assert( reachNum > 0 );
		m_predictedMovementActions.clear();
		m_targetReachNum = reachNum;
	}

private:
	int m_targetReachNum { 0 };

	WalkToPointAction m_walkToPointAction;
	LandOnPointAction m_landOnPointAction;
};

class ClimbOntoBarrierAction : public BaseAction {
public:
	explicit ClimbOntoBarrierAction( MovementSubsystem *subsystem )
		: BaseAction( subsystem, "ClimbOntoBarrierAction", COLOR_RGB( 255, 0, 64 ) ) {}

	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
	[[nodiscard]]
	auto checkPredictionStepResults( PredictionContext *context ) -> PredictionResult override;

	void setTargetReachNum( int reachNum ) { m_targetReachNum = reachNum; }
private:
	void beforePlanning() override;
	void afterPlanning() override;

	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
									   unsigned stoppedAtFrameIndex ) override;

	int m_targetReachNum { 0 };
	bool m_isDisabledForPlanning { false };
};

class TraverseBarrierJumpReachScript : public SwitchingActionsForStateScript {
public:
	explicit TraverseBarrierJumpReachScript( MovementSubsystem *movementSubsystem )
		: SwitchingActionsForStateScript( movementSubsystem )
		, m_walkToPointAction( movementSubsystem )
		, m_climbOntoBarrierAction( movementSubsystem ) {}

	[[nodiscard]]
	bool produceBotInput( BotInput *input ) override;

	void setTargetReachNum( int reachNum ) {
		m_predictedMovementActions.clear();
		m_targetReachNum = reachNum;
	}

private:
	WalkToPointAction m_walkToPointAction;
	ClimbOntoBarrierAction m_climbOntoBarrierAction;
	int m_targetReachNum { 0 };
};

class JumppadScript : public SwitchingActionsForStateScript {
public:
	explicit JumppadScript( MovementSubsystem *movementSubsystem )
		: SwitchingActionsForStateScript( movementSubsystem ), m_landOnPointAction( movementSubsystem ) {
		m_timeoutAt = std::numeric_limits<decltype( m_timeoutAt )>::max();
	}

	[[nodiscard]]
	bool produceBotInput( BotInput *input ) override;

	void setTarget( int triggerEntNum, int reachNum ) {
		assert( triggerEntNum > 0 );
		assert( reachNum >= 0 );
		m_triggerEntNum  = triggerEntNum;
		m_targetReachNum = reachNum;
	}
private:
	[[nodiscard]]
	bool reuseCachedPathForLastGoodArea( BotInput *input );
	[[nodiscard]]
	bool tryLandingOnArea( int areaNum, BotInput *input );
	[[nodiscard]]
	bool tryLandingOnAreas( std::span<const uint16_t> areaNums, int skipAreaNum, BotInput *input );
	[[nodiscard]]
	bool setupFreeflyMovement( BotInput *input, const float *targetTriggerOrigin, const AiEntityPhysicsState & );
	[[nodiscard]]
	bool setupRestartTriggerMovement( BotInput *input, const AiEntityPhysicsState & );
	[[nodiscard]]
	bool setupNonLandingMovement( BotInput *input, const float *triggerTargetOrigin, const AiEntityPhysicsState & );

	LandOnPointAction m_landOnPointAction;

	int m_triggerEntNum { 0 };
	int m_targetReachNum { 0 };
	int m_lastGoodLandingAreaNum { 0 };
};

#endif
