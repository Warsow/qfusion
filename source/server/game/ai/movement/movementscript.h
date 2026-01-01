/*
Copyright (C) 2017-2025 vvk2212, Chasseur de bots

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

#ifndef WSW_1fb31275_631b_4bf0_badc_faa105cf4bf4_H
#define WSW_1fb31275_631b_4bf0_badc_faa105cf4bf4_H

#include "movementstate.h"

class BaseAction;
class MovementSubsystem;

struct MovementActionRecord {
	BotInput botInput;

private:
	int16_t modifiedVelocity[3];

public:
	int8_t pendingWeapon : 7;
	bool hasModifiedVelocity : 1;

	MovementActionRecord()
		: pendingWeapon( -1 ),
		  hasModifiedVelocity( false ) {}

	void Clear() {
		botInput.Clear();
		pendingWeapon = -1;
		hasModifiedVelocity = false;
	}

	void SetModifiedVelocity( const Vec3 &velocity ) {
		SetModifiedVelocity( velocity.Data() );
	}

	void SetModifiedVelocity( const vec3_t velocity ) {
		for( int i = 0; i < 3; ++i ) {
			int snappedVelocityComponent = (int)( velocity[i] * 16.0f );
			if( snappedVelocityComponent > std::numeric_limits<signed short>::max() ) {
				snappedVelocityComponent = std::numeric_limits<signed short>::max();
			} else if( snappedVelocityComponent < std::numeric_limits<signed short>::min() ) {
				snappedVelocityComponent = std::numeric_limits<signed short>::min();
			}
			modifiedVelocity[i] = (signed short)snappedVelocityComponent;
		}
		hasModifiedVelocity = true;
	}

	Vec3 ModifiedVelocity() const {
		assert( hasModifiedVelocity );
		float scale = 1.0f / 16.0f;
		return Vec3( scale * modifiedVelocity[0], scale * modifiedVelocity[1], scale * modifiedVelocity[2] );
	}
};

struct PredictedMovementAction {
	AiEntityPhysicsState entityPhysicsState;
	MovementActionRecord record;
	BaseAction *action;
	int64_t timestamp;
	unsigned stepMillis;

	PredictedMovementAction()
		: action( nullptr ),
		  timestamp( 0 ),
		  stepMillis( 0 ) {}
};

struct MovementPredictionConstants {
	enum SequenceStopReason : uint8_t {
		UNSPECIFIED, // An empty initial value, should be replaced by SWITCHED on actual use
		SUCCEEDED,   // The sequence has been completed successfully
		SWITCHED,    // The action cannot be applied in the current environment, another action is suggested
		DISABLED,    // The action is disabled for application, another action is suggested
		FAILED       // A prediction step has lead to a failure
	};

	enum class PredictionResult : unsigned {
		Continue,
		Abort,
		Restart,
		Complete,
	};

	static constexpr unsigned MAX_SAVED_LANDING_AREAS = 16;

	// Note: We have deliberately lowered this value
	// to prevent fruitless prediction frames that lead to an overflow anyway
	// once much more stricter bunnying checks are implemented
	static constexpr unsigned MAX_PREDICTED_STATES = 32;
};

class MovementScript {
public:
	explicit MovementScript( MovementSubsystem *movementSubsystem ) : m_subsystem( movementSubsystem ) {}

	virtual ~MovementScript() = default;

	[[nodiscard]]
	auto getTimeoutAt() const -> int64_t { return m_timeoutAt; };

	[[nodiscard]]
	virtual bool produceBotInput( BotInput *botInput ) = 0;

	void bumpTimeout() {
		// TODO: std::optional<int64_t>?
		if( m_timeoutAt != std::numeric_limits<int64_t>::max() ) {
			m_timeoutAt = level.time + 3000;
		}
	};
protected:
	int64_t m_timeoutAt { 0 };
	MovementSubsystem *const m_subsystem;
};

class PredictingMovementScript : public MovementPredictionConstants, public MovementScript {
public:
	explicit PredictingMovementScript( MovementSubsystem *movementSubsystem )
		: MovementScript( movementSubsystem ) {}

	void onInterceptedPredictedEvent( int ev, int parm );
	void onInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin );
protected:
	PredictionContext *m_activeContext { nullptr };
};

class PredictingAndCachingMovementScript : public PredictingMovementScript {
public:
	explicit PredictingAndCachingMovementScript( MovementSubsystem *movementSubsystem )
		: PredictingMovementScript( movementSubsystem ) {}
private:
	void Debug( const char *format, ... ) const;

	bool produceBotInput( BotInput *input ) override;

	[[nodiscard]]
	auto getCachedActionAndRecordForCurrTime( MovementActionRecord *record_ ) -> BaseAction *;

	[[nodiscard]]
	auto tryCheckingAndLerpingActions( PredictedMovementAction *prevAction,
									   PredictedMovementAction *nextAction,
									   MovementActionRecord *record_ ) -> BaseAction *;

	[[nodiscard]]
	auto lerpActionRecords( PredictedMovementAction *prevAction,
							PredictedMovementAction *nextAction,
							MovementActionRecord *record_ ) -> BaseAction *;

	[[nodiscard]]
	bool checkPredictedOrigin( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );
	[[nodiscard]]
	bool checkPredictedVelocity( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );
	[[nodiscard]]
	bool checkPredictedAngles( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );

	wsw::StaticVector<PredictedMovementAction, MAX_PREDICTED_STATES> m_predictedMovementActions;
protected:
	std::span<BaseAction *> m_movementActions;
};

#endif