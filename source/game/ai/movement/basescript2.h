#ifndef WSW_0ca0eb25_a743_4f04_ae37_7bea05022ec8_H
#define WSW_0ca0eb25_a743_4f04_ae37_7bea05022ec8_H

#include "movementlocal.h"

class BaseMovementAction;

class BaseScript2 {
	friend class MovementPredictionContext;
	friend class BaseMovementAction;
	friend class BunnyToBestFloorClusterPointAction;
protected:
	BotMovementModule *const m_module;
	MovementPredictionContext::PredictedPath *const m_cachedPath;

	// TODO: This is not needed except for loop detection in dev mode
	wsw::StaticVector<BaseMovementAction *, 5> m_actions;

	[[nodiscard]]
	auto tryCheckAndLerpActions( PathElem *prevAction, PathElem *nextAction, MovementActionRecord *record_ ) -> BaseMovementAction *;
	[[nodiscard]]
	auto lerpActionRecords( PathElem *prevAction, PathElem *nextAction, MovementActionRecord *record_ ) -> BaseMovementAction *;

	[[nodiscard]]
	bool checkPredictedOrigin( PathElem *prevAction, PathElem *nextAction, float frac );
	[[nodiscard]]
	bool checkPredictedVelocity( PathElem *prevAction, PathElem *nextAction, float frac );
	[[nodiscard]]
	bool checkPredictedAngles( PathElem *prevAction, PathElem *nextAction, float frac );

	[[nodiscard]]
	auto getCachedActionAndRecordForCurrTime( MovementActionRecord *record_ ) -> BaseMovementAction *;

	void debug( const char *format, ... ) const;
public:
	explicit BaseScript2( BotMovementModule *module );

	virtual ~BaseScript2() = default;

	[[nodiscard]]
	virtual auto getActionAndRecordForCurrGameState( MovementActionRecord *record ) -> BaseMovementAction *;
};

class BunnyHopScript2 : public BaseScript2 {
	BunnyToStairsOrRampExitAction m_bunnyToStairsOrRampExitAction { this };
	BunnyTestingNextReachDirsAction m_bunnyTestingNextReachDirsAction { this };
	BunnyToBestVisibleReachAction m_bunnyToBestVisibleReachAction { this };
	BunnyToBestFloorClusterPointAction m_bunnyToBestFloorClusterPointAction { this };
	BunnyTestingMultipleTurnsAction m_bunnyTestingMultipleTurnsAction { this };
public:
	explicit BunnyHopScript2( BotMovementModule *module ) : BaseScript2( module ) {}
};

/*
class CombatScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class PlantScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class PlatformScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class SwimScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class JumppadScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class WeaponJumpScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class FallbackDispatcherScript : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class JumpOverGapScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class JumpOverBarrierScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class FallDownScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class WalkToSpotOrTriggerScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class WalkToRampExitScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};

class WalkToStairsExitScript2 : public BaseScript2 {
	[[nodiscard]]
	bool checkValidInCurrGameState() const override;
};*/

#endif
