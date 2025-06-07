#ifndef WSW_76bfcd28_24f1_4001_b4b4_d7abbc68722e_H
#define WSW_76bfcd28_24f1_4001_b4b4_d7abbc68722e_H

#include "../ailocal.h"
#include <common/types/staticvector.h>

#include "awarenesslocal.h"

class HazardsDetector {
	friend class BotAwarenessModule;
	friend class HazardsSelector;

	void Clear();

	static const auto MAX_NONCLIENT_ENTITIES = MAX_EDICTS - MAX_CLIENTS;
	using EntsAndDistancesVector = wsw::StaticVector<EntAndDistance, MAX_NONCLIENT_ENTITIES>;
	using EntNumsVector = wsw::StaticVector<uint16_t, MAX_NONCLIENT_ENTITIES>;

	static constexpr float kWaveDetectionRadius = 450.0f;

	const Bot *const bot;

	EntsAndDistancesVector maybeVisibleDangerousRockets;
	EntNumsVector visibleDangerousRockets;
	EntsAndDistancesVector maybeVisibleDangerousWaves;
	EntNumsVector visibleDangerousWaves;
	EntsAndDistancesVector maybeVisibleDangerousPlasmas;
	EntNumsVector visibleDangerousPlasmas;
	EntsAndDistancesVector maybeVisibleDangerousBlasts;
	EntNumsVector visibleDangerousBlasts;
	EntsAndDistancesVector maybeVisibleDangerousGrenades;
	EntNumsVector visibleDangerousGrenades;
	EntsAndDistancesVector maybeVisibleDangerousLasers;
	EntNumsVector visibleDangerousLasers;

	// Note: Entities of the same team are not included in "other" entities vector
	// to avoid redundant visibility checks.
	// Guessing enemy origins is the sole utility of "other" entities vectors so far.

	EntsAndDistancesVector maybeVisibleOtherRockets;
	EntNumsVector visibleOtherRockets;
	EntsAndDistancesVector maybeVisibleOtherWaves;
	EntNumsVector visibleOtherWaves;
	EntsAndDistancesVector maybeVisibleOtherPlasmas;
	EntNumsVector visibleOtherPlasmas;
	EntsAndDistancesVector maybeVisibleOtherBlasts;
	EntNumsVector visibleOtherBlasts;
	EntsAndDistancesVector maybeVisibleOtherGrenades;
	EntNumsVector visibleOtherGrenades;
	EntsAndDistancesVector maybeVisibleOtherLasers;
	EntNumsVector visibleOtherLasers;

	explicit HazardsDetector( const Bot *bot_ ) : bot( bot_ ) {}

	void Exec();
};

#endif
