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

#ifndef WSW_37cd33f1_ae6e_4017_8864_edb1800013b2_H
#define WSW_37cd33f1_ae6e_4017_8864_edb1800013b2_H

#include "movementscript.h"
#include "walktopointaction.h"

class WalkToPointScript : public PredictingAndCachingMovementScript {
public:
	explicit WalkToPointScript( MovementSubsystem *movementSubsystem )
		: PredictingAndCachingMovementScript( movementSubsystem ), m_walkToPointAction( movementSubsystem ) {
		m_movementActions = m_storageOfActionPtrs;
	}

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

#endif