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
private:
	void beforePlanning() override;
	void afterPlanning() override;

	void onApplicationSequenceStarted( PredictionContext *context ) override;
	void onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
									   unsigned stoppedAtFrameIndex ) override;

	void bumpDashDistance();
	void bumpJumpDistance();

	Vec3 m_targetPoint { 0.0f, 0.0f, 0.0f };
	float m_minDistanceFromTargetToJump { 0.0f };
	float m_minDistanceFromTargetToDash { 0.0f };
	float m_distanceFromStartToTarget { 0.0f };
	bool m_isDashingAllowed { false };
	bool m_isJumpingAllowed { false };
	bool m_hasJumped { false };
	bool m_hasDashedOrWalljumped { false };
	bool m_isDisabledForPlanning { false };
};

#endif
