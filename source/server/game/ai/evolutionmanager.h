#ifndef WSW_0645cf41_7bb6_4a65_83c8b79c7ce9b1d7_H
#define WSW_0645cf41_7bb6_4a65_83c8b79c7ce9b1d7_H

#include "botweightconfig.h"

class BotEvolutionManager
{
	static BotEvolutionManager *instance;

protected:
	BotEvolutionManager() {}

public:
	virtual ~BotEvolutionManager() {};

	static void Init();
	static void Shutdown();

	static inline BotEvolutionManager *Instance() { return instance; }

	virtual void OnBotConnected( edict_t *ent ) {};
	virtual void OnBotRespawned( edict_t *ent ) {};

	virtual void SaveEvolutionResults() {};
};

#endif
