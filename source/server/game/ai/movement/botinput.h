#ifndef WSW_4de78118_59e2_4a53_bd69_65112cf15a99_H
#define WSW_4de78118_59e2_4a53_bd69_65112cf15a99_H

#include "../ailocal.h"

class alignas ( 4 )BotInput {
	friend class PredictionContext;
	friend class PredictingAndCachingMovementScript;
	// Todo: Pack since it is required to be normalized now?
	Vec3 intendedLookDir { 0, 0, 0 };
	// A copy of self->s.angles for modification
	// We do not want to do deeply hidden angles update in the aiming functions,
	// the BotInput should be only mutable thing in the related code.
	// Should be copied back to self->s.angles if it has been modified when the BotInput gets applied.
	Vec3 alreadyComputedAngles { 0, 0, 0 };
	uint8_t turnSpeedMultiplier { 16 };
	// Unfortunately, we can't use bitfield member initializers with pre-20 language standards
	signed ucmdForwardMove : 2;
	signed ucmdSideMove : 2;
	signed ucmdUpMove : 2;
	bool attackButton : 1;
	bool specialButton : 1;
	bool walkButton : 1;

public:
	bool fireScriptWeapon : 1;
	bool isUcmdSet : 1;
	bool isLookDirSet : 1;
	bool hasAlreadyComputedAngles : 1;
	bool canOverrideUcmd : 1;
	bool shouldOverrideUcmd : 1;
	bool canOverrideLookVec : 1;
	bool shouldOverrideLookVec : 1;
	bool canOverridePitch : 1;

	BotInput() {
		// We have to put this code block to the constructor too in order to suppress Clang-Tidy warnings
		ucmdForwardMove = ucmdSideMove = ucmdUpMove = 0;
		attackButton = specialButton = walkButton = false;
		fireScriptWeapon = false;
		isUcmdSet = isLookDirSet = false;
		hasAlreadyComputedAngles = false;
		canOverrideUcmd = shouldOverrideUcmd = false;
		canOverrideLookVec = shouldOverrideLookVec = canOverridePitch = false;
	}

	void Clear() {
		turnSpeedMultiplier = 16;
		ucmdForwardMove = ucmdSideMove = ucmdUpMove = 0;
		attackButton = specialButton = walkButton = false;
		fireScriptWeapon = false;
		isUcmdSet = isLookDirSet = false;
		hasAlreadyComputedAngles = false;
		canOverrideUcmd = shouldOverrideUcmd = false;
		canOverrideLookVec = shouldOverrideLookVec = canOverridePitch = false;
	}

	// Button accessors are kept for backward compatibility with existing bot movement code
	void SetAttackButton( bool isSet ) { attackButton = isSet; }
	void SetSpecialButton( bool isSet ) { specialButton = isSet; }
	void SetWalkButton( bool isSet ) { walkButton = isSet; }

	bool IsAttackButtonSet() const { return attackButton; }
	bool IsSpecialButtonSet() const { return specialButton; }
	bool IsWalkButtonSet() const { return walkButton; }

	int ForwardMovement() const { return ucmdForwardMove; }
	int RightMovement() const { return ucmdSideMove; }
	int UpMovement() const { return ucmdUpMove; }

	bool IsCrouching() const { return UpMovement() < 0; }

	void SetForwardMovement( int movement ) { ucmdForwardMove = movement; }
	void SetRightMovement( int movement ) { ucmdSideMove = movement; }
	void SetUpMovement( int movement ) { ucmdUpMove = movement; }

	void ClearMovementDirections() {
		ucmdForwardMove = 0;
		ucmdSideMove = 0;
		ucmdUpMove = 0;
	}

	void ClearButtons() {
		attackButton = false;
		specialButton = false;
		walkButton = false;
	}

	float TurnSpeedMultiplier() const { return turnSpeedMultiplier / 16.0f; }
	void SetTurnSpeedMultiplier( float value ) {
		turnSpeedMultiplier = ( decltype( turnSpeedMultiplier ) )( value * 16.0f );
	}

	void CopyToUcmd( usercmd_t *ucmd ) const {
		ucmd->forwardmove = 127 * ForwardMovement();
		ucmd->sidemove = 127 * RightMovement();
		ucmd->upmove = 127 * UpMovement();

		ucmd->buttons = 0;
		if( attackButton ) {
			ucmd->buttons |= BUTTON_ATTACK;
		}
		if( specialButton ) {
			ucmd->buttons |= BUTTON_SPECIAL;
		}
		if( walkButton ) {
			ucmd->buttons |= BUTTON_WALK;
		}
	}

	void SetAlreadyComputedAngles( const Vec3 &angles ) {
		alreadyComputedAngles = angles;
		hasAlreadyComputedAngles = true;
	}

	const Vec3 &AlreadyComputedAngles() const {
		return alreadyComputedAngles;
	}

	void SetIntendedLookDir( const Vec3 &intendedLookVec, bool alreadyNormalized = false ) {
		SetIntendedLookDir( intendedLookVec.Data(), alreadyNormalized );
	}

	void SetIntendedLookDir( const vec3_t intendedLookVec, bool alreadyNormalized = false ) {
		this->intendedLookDir.Set( intendedLookVec );
		if( !alreadyNormalized ) {
			this->intendedLookDir.normalizeFastOrThrow();
		}
#ifndef PUBLIC_BUILD
		else if( fabsf( this->intendedLookDir.LengthFast() - 1.0f ) > 0.1f ) {
			AI_FailWith( "BotInput::SetIntendedLookDir()", "The argument is claimed to be normalized but it isn't\n" );
		}
#endif
		this->isLookDirSet = true;
	}

	const Vec3 &IntendedLookDir() const {
		return intendedLookDir;
	}
};

#endif
