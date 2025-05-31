#ifndef WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H
#define WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H

#include "enemiestracker.h"

class Bot;
class BotAwarenessModule;
class SelectedEnemy;

/**
 * A helper class that encapsulates details of "kept in fov" point maintenance.
 * A "kept in fov point" is an origin a bot tries to keep looking at while moving.
 */
class KeptInFovPointTracker {
	Bot *const m_bot;
	BotAwarenessModule *const m_awarenessModule;

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

	void update();

	[[nodiscard]]
	auto getActivePoint() const -> const std::optional<Vec3> & { return m_point; }
};

#endif
