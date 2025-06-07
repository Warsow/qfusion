#ifndef WSW_367671cf_02ef_4de7_9728_36bbe2aeab04_H
#define WSW_367671cf_02ef_4de7_9728_36bbe2aeab04_H

#include <common/types/staticvector.h>
#include "ailocal.h"

#include <bitset>

template <typename> class SingletonHolder;

namespace wsw::ai {

class ClassifiedEntitiesCache {
	template <typename> friend class SingletonHolder;
public:
	static constexpr unsigned kMaxClassTriggerEnts = 32;
private:
	wsw::StaticVector<uint16_t, kMaxClassTriggerEnts> m_allTeleporters;
	wsw::StaticVector<uint16_t, kMaxClassTriggerEnts> m_allJumppads;
	wsw::StaticVector<uint16_t, kMaxClassTriggerEnts> m_allPlatformTriggers;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_allOtherTriggers;
	std::bitset<MAX_EDICTS> m_persistentEntitiesMask;
	bool m_hasRetrievedPersistentEntities { false };

	wsw::StaticVector<uint16_t, MAX_EDICTS> m_rockets;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_grenades;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_plasmas;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_blasts;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_lasers;
	wsw::StaticVector<uint16_t, MAX_EDICTS> m_waves;

	void retrievePersistentEntities();
public:
	static void init();
	static void shutdown();
	[[nodiscard]]
	static auto instance() -> ClassifiedEntitiesCache *;

	void update();

	[[nodiscard]]
	auto getAllPersistentMapTeleporters() const -> std::span<const uint16_t> {
		return { m_allTeleporters.begin(), m_allTeleporters.end() };
	}
	[[nodiscard]]
	auto getAllPersistentMapJumppads() const -> std::span<const uint16_t> {
		return { m_allJumppads.begin(), m_allJumppads.end() };
	}
	[[nodiscard]]
	auto getAllPersistentMapPlatformTriggers() const -> std::span<const uint16_t> {
		return { m_allPlatformTriggers.begin(), m_allPlatformTriggers.end() };
	}
	[[nodiscard]]
	auto getAllOtherTriggersInThisFrame() const -> std::span<const uint16_t> {
		return { m_allOtherTriggers.begin(), m_allOtherTriggers.end() };
	}

	[[nodiscard]]
	auto getAllRockets() const -> std::span<const uint16_t> { return { m_rockets.begin(), m_rockets.end() }; }
	[[nodiscard]]
	auto getAllGrenades() const -> std::span<const uint16_t> { return { m_grenades.begin(), m_grenades.end() }; }
	[[nodiscard]]
	auto getAllPlasmas() const -> std::span<const uint16_t> { return { m_plasmas.begin(), m_plasmas.end() }; }
	[[nodiscard]]
	auto getAllBlasts() const -> std::span<const uint16_t> { return { m_blasts.begin(), m_blasts.end() }; }
	[[nodiscard]]
	auto getAllLasers() const -> std::span<const uint16_t> { return { m_lasers.begin(), m_lasers.end() }; }
	[[nodiscard]]
	auto getAllWaves() const -> std::span<const uint16_t> { return { m_waves.begin(), m_waves.end() }; }
};

}

#endif