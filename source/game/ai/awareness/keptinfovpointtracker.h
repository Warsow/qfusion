#ifndef WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H
#define WSW_373346d9_e782_47e7_bb1a_b44a9cc7ef75_H

#include "enemiestracker.h"
#include "../selection.h"

class Bot;
class BotAwarenessModule;
class SelectedEnemies;

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
	auto selectPointBasedOnEnemies( const SelectedEnemies &selectedEnemies ) -> std::optional<Vec3>;
	[[nodiscard]]
	auto selectPointBasedOnLostOrHiddenEnemy( const TrackedEnemy *enemy ) -> std::optional<Vec3>;
public:
	KeptInFovPointTracker( Bot *bot, BotAwarenessModule *awarenessModule )
		: m_bot( bot ), m_awarenessModule( awarenessModule ) {}

	void update();

	[[nodiscard]]
	auto getActivePoint() const -> const float * { return m_point ? m_point->Data() : nullptr; };
};

#endif
