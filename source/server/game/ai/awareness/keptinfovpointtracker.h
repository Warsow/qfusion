#ifndef WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H
#define WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H

#include "enemiestracker.h"

class Bot;
class BotAwarenessModule;
class SelectedEnemy;

class AiPendingLookAtPoint {
	float origin[3] { 0.0f, 0.0f, 0.0f };
	// Floating point values greater than 1.0f are allowed (unless they are significantly greater than 1.0f);
	float turnSpeedMultiplier { 1.0f };

public:
	AiPendingLookAtPoint() {}

	Vec3 Origin() const { return Vec3( origin ); }
	float TurnSpeedMultiplier() const { return turnSpeedMultiplier; };

	AiPendingLookAtPoint( const vec3_t origin_, float turnSpeedMultiplier_ )
		: turnSpeedMultiplier( turnSpeedMultiplier_ )
	{
		VectorCopy( origin_, origin );
	}

	AiPendingLookAtPoint( const Vec3 &origin_, float turnSpeedMultiplier_ )
		: turnSpeedMultiplier( turnSpeedMultiplier_ )
	{
		origin_.copyTo( origin );
	}
};

/**
 * A helper class that encapsulates details of "kept in fov" point maintenance.
 * A "kept in fov point" is an origin a bot tries to keep looking at while moving.
 */
class KeptInFovPointTracker {
	Bot *const m_bot;
	BotAwarenessModule *const m_awarenessModule;

	struct PendingLookAtPointState {
		AiPendingLookAtPoint pendingLookAtPoint;
		int64_t timeoutAt { 0 };
	} m_pendingLookAtPointState;

	// TODO: Select/respect the desired turn speed multiplier
	std::optional<Vec3> m_point;

	[[nodiscard]]
	auto selectCurrentPoint() -> std::optional<Vec3>;

	[[nodiscard]]
	bool isPointInPvs( const Vec3 &point ) const;

	[[nodiscard]]
	auto selectPointBasedOnEnemies( const SelectedEnemy &selectedEnemy ) -> std::optional<Vec3>;
	[[nodiscard]]
	auto selectPointBasedOnLostOrHiddenEnemy( const TrackedEnemy *enemy ) -> std::optional<Vec3>;
public:
	KeptInFovPointTracker( Bot *bot, BotAwarenessModule *awarenessModule )
		: m_bot( bot ), m_awarenessModule( awarenessModule ) {}

	void setPendingLookAtPoint( const AiPendingLookAtPoint &pendingLookAtPoint, unsigned timeoutPeriod = 1000 ) {
		m_pendingLookAtPointState.pendingLookAtPoint = pendingLookAtPoint;
		m_pendingLookAtPointState.timeoutAt          = level.time + timeoutPeriod;
	}

	void resetPendingLookAtPoint() { m_pendingLookAtPointState.timeoutAt = 0; }

	[[nodiscard]]
	bool hasPendingLookAtPoint() const { return m_pendingLookAtPointState.timeoutAt > level.time; }

	void update();

	[[nodiscard]]
	auto getActivePoint() const -> const std::optional<Vec3> & { return m_point; }
};

#endif
