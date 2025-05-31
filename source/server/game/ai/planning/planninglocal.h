#ifndef WSW_09ba68bc_04d2_46ea_aa2e_be5b4ecfb29b_H
#define WSW_09ba68bc_04d2_46ea_aa2e_be5b4ecfb29b_H

#include "../bot.h"

float DamageToKill( float health, float armor, float armorProtection, float armorDegradation );

inline float DamageToKill( float health, float armor ) {
	return DamageToKill( health, armor, g_armor_protection->value, g_armor_degradation->value );
}

inline PlannerNode::PlannerNode( PoolBase *pool, Bot *self ) : PoolItem( pool ) {}

inline const BotWeightConfig &BotAction::WeightConfig() const {
	return Self()->WeightConfig();
}

#endif
