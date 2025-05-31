#ifndef WSW_a69d3211_8dbc_4f4e_8ea9_55a9636ced1c_H
#define WSW_a69d3211_8dbc_4f4e_8ea9_55a9636ced1c_H

#include <stdarg.h>
#include "planner.h"
#include "../awareness/enemiestracker.h"
#include "itemsselector.h"
#include "../combat/weaponselector.h"
#include "actions.h"
#include "goals.h"

struct Hazard;

class BotPlanner : public AiPlanner {
	friend class BotPlanningModule;
	friend class BotItemsSelector;

	Bot *const bot;
	BotPlanningModule *const module;

	void PrepareCurrWorldState( WorldState *worldState ) override;

	bool ShouldSkipPlanning() const override;

	void BeforePlanning() override;
public:
	BotPlanner() = delete;
	// Disable copying and moving
	BotPlanner( BotPlanner &&that ) = delete;

	// A WorldState cached from the moment of last world state update
	WorldState cachedWorldState;

	BotPlanner( Bot *bot_, BotPlanningModule *module_ );
};

#endif
