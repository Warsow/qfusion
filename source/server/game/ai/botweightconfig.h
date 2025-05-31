#ifndef WSW_94e6f90b_5059_45b3_bb10_597e4f412589_H
#define WSW_94e6f90b_5059_45b3_bb10_597e4f412589_H

#include "weightconfig.h"

// Var names might seem to be way too shortened, but its silly to replace each minor numeric constant by a 30 chars name

class BotWeightConfig : public AiWeightConfig {
	using Var = AiNativeWeightConfigVar;
public:
	class NativeGoalsGroup : public AiNativeWeightConfigVarGroup {
public:
		struct GrabItemGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.00f, 0.50f, 0.00f };
			Var selectedGoalWeightScale { this, "SelectedGoalWeightScale", 0.75f, 1.25f, 1.00f };

			explicit GrabItemGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "GrabItem" ) {}
		} grabItem { this };

		struct KillEnemyGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.00f, 0.50f, 0.00f };
			Var offCoeff { this, "OffCoeff", 0.50f, 3.00f, 1.75f };
			Var nmyThreatCoeff { this, "NmyThreatCoeff", 1.10f, 2.00f, 1.25f };

			explicit KillEnemyGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "KillEnemy" ) {}
		} killEnemy { this };

		struct RunAwayGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.00f, 0.50f, 0.00f };
			Var offCoeff { this, "OffCoeff", 1.00f, 3.00f, 1.75f };
			Var nmyThreatCoeff { this, "NmyThreatCoeff", 1.10f, 3.00f, 1.50f };

			explicit RunAwayGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "RunAway" ) {}
		} runAway { this };

		struct ReactToHazardGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.50f, 1.00f, 0.75f };
			Var dmgFracCoeff { this, "DmgFracCoeff", 0.50f, 5.00f, 2.00f };
			Var weightBound { this, "WeightBound", 1.00f, 5.00f, 2.00f };

			explicit ReactToHazardGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "ReactToHazard" ) {}
		} reactToHazard { this };

		struct ReactToThreatGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.50f, 1.00f, 0.50f };
			Var dmgFracCoeff { this, "DmgFracCoeff", 1.00f, 5.00f, 3.00f };
			Var weightBound { this, "WeightBound", 1.00f, 3.00f, 1.75f };
			Var offCoeff { this, "OffCoeff", 0.00f, 2.00f, 1.00f };

			explicit ReactToThreatGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "ReactToThreat" ) {}
		} reactToThreat { this };

		struct ReactToEnemyLostGroup : public AiNativeWeightConfigVarGroup {
			Var baseWeight { this, "BaseWeight", 0.50f, 1.00f, 0.50f };
			Var offCoeff { this, "OffCoeff", 0.50f, 6.00f, 2.00f };

			explicit ReactToEnemyLostGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "ReactToEnemyLost" ) {}
		} reactToEnemyLost { this };

		struct AttackOutOfDespairGroup : public AiNativeWeightConfigVarGroup {
			Var nmyFireDelayThreshold { this, "NmyFireDelayThreshold", 200, 800, 600 };
			Var baseWeight { this, "BaseWeight", 0.25f, 1.00f, 0.50f };
			Var nmyThreatExtraWeight { this, "NmyThreatExtraWeight", 0.10f, 1.00f, 0.50f };
			Var dmgFracCoeff { this, "DmgFracCoeff", 0.50f, 1.75f, 1.25f };
			Var dmgUpperBound { this, "DmgUpperBound", 50.0f, 200.0f, 100.0f };

			explicit AttackOutOfDespairGroup( AiWeightConfigVarGroup *parent )
				: AiNativeWeightConfigVarGroup( parent, "AttackOutOfDespair" ) {}
		} attackOutOfDespair { this };

		explicit NativeGoalsGroup( AiWeightConfigVarGroup *parent )
			: AiNativeWeightConfigVarGroup( parent, "NativeGoals" ) {}
	} nativeGoals;

	// We have decided to remove "native actions group" as weights tweaking on this level
	// just adds more jitter and produces fruitless combinations of weights
	// making convergence to a good solution impossible.

	// There are systems though, like "enemies tracker", "weapons selector", etc
	// that might benefit from this kind of weights optimization.
	// There should be an option to freeze goal weights and interbreed only weights for the suggested groups.
	// This is doable via using scripted evolution manager.

	explicit BotWeightConfig( const edict_t *owner )
		: AiWeightConfig( owner ),
		nativeGoals( Root() ) {
		RegisterInScript();
	}
};

#endif
