#ifndef WSW_a69d3211_8dbc_4f4e_8ea9_55a9636ced1c_H
#define WSW_a69d3211_8dbc_4f4e_8ea9_55a9636ced1c_H

#include <stdarg.h>
#include "../baseai.h"
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

	const int *Inventory() const;

	template <int Weapon>
	int AmmoReadyToFireCount() const {
		if( !Inventory()[Weapon] ) {
			return 0;
		}
		return Inventory()[WeaponAmmo < Weapon > ::strongAmmoTag] + Inventory()[WeaponAmmo < Weapon > ::weakAmmoTag];
	}

	int ShellsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_RIOTGUN>(); }
	int GrenadesReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_GRENADELAUNCHER>(); }
	int RocketsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_ROCKETLAUNCHER>(); }
	int PlasmasReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_PLASMAGUN>(); }
	int BulletsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_MACHINEGUN>(); }
	int LasersReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_LASERGUN>(); }
	int BoltsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_ELECTROBOLT>(); }
	int WavesReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_SHOCKWAVE>(); }
	int InstasReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_INSTAGUN>(); }

	bool FindDodgeHazardSpot( const Hazard &hazard, vec3_t spotOrigin );

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
