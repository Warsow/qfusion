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

#include "walktopointscript.h"
#include "movementsubsystem.h"
#include "../bot.h"

bool WalkToPointScript::produceBotInput( BotInput *input ) {
	const Vec3 botMins( Vec3( playerbox_stand_mins ) + m_subsystem->movementState.entityPhysicsState.Origin() );
	const Vec3 botMaxs( Vec3( playerbox_stand_maxs ) + m_subsystem->movementState.entityPhysicsState.Origin() );
	// TODO: Invalidate explicitly?
	if( BoundsAndSphereIntersect( botMins.Data(), botMaxs.Data(), m_targetPoint.Data(), 1.0f ) ) {
		return false;
	}

	m_walkToPointAction.setTargetPoint( m_targetPoint );

	if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
		const auto lastRecordTimestamp = m_predictedMovementActions.back().timestamp;
		// game.realtime is the baseline for predicted entries
		assert( lastRecordTimestamp >= game.realtime );
		// TODO: Should we specify timeout in realtime as well?
		m_timeoutAt = level.time + ( lastRecordTimestamp - game.realtime ) + 1;
		return true;
	}

	return false;
}