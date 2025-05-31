#include "actions.h"
#include "planninglocal.h"
#include "../bot.h"
#include "../groundtracecache.h"
#include "../combat/tacticalspotsregistry.h"

BotActionRecord::BotActionRecord( PoolBase *pool_, Bot *self_, const char *name_ )
	: AiActionRecord( pool_, self_, name_ ) {}

BotAction::BotAction( BotPlanningModule *module_, const char *name_ )
	: AiAction( module_->bot, name_ ), module( module_ ) {}

float BotAction::DamageToBeKilled() const {
	const edict_t *botEnt = game.edicts + Self()->EntNum();
	const float health    = HEALTH_TO_INT( botEnt->health );
	const float armor     = botEnt->r.client->ps.stats[STAT_ARMOR];
	float damageToBeKilled = ::DamageToKill( health, armor );
	if( botEnt->r.client->ps.inventory[POWERUP_SHELL] ) {
		damageToBeKilled *= QUAD_DAMAGE_SCALE;
	}
	const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy();
	if( selectedEnemy && selectedEnemy->HasQuad() ) {
		damageToBeKilled *= 1.0f / QUAD_DAMAGE_SCALE;
	}
	return damageToBeKilled;
}

float BotAction::DamageToKill() const {
	const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy();
	return selectedEnemy ? selectedEnemy->DamageToKill() : 0.0f;
}

float BotAction::KillToBeKilledDamageRatio() const {
	return DamageToKill() / DamageToBeKilled();
}

float BotGoal::DamageToBeKilled() const {
	const edict_t *botEnt = game.edicts + Self()->EntNum();
	const float health    = HEALTH_TO_INT( botEnt->health );
	const float armor     = botEnt->r.client->ps.stats[STAT_ARMOR];
	float damageToBeKilled = ::DamageToKill( health, armor );
	if( botEnt->r.client->ps.inventory[POWERUP_SHELL] ) {
		damageToBeKilled *= QUAD_DAMAGE_SCALE;
	}
	const std::optional<SelectedEnemy> &selectedEnemy = Self()->GetSelectedEnemy();
	if( selectedEnemy && selectedEnemy->HasQuad() ) {
		damageToBeKilled *= 1.0f / QUAD_DAMAGE_SCALE;
	}
	return damageToBeKilled;
}

void BotActionRecord::Activate() {
	AiActionRecord::Activate();
	Self()->GetMiscTactics().Clear();
}

void BotActionRecord::Deactivate() {
	AiActionRecord::Deactivate();
	Self()->GetMiscTactics().Clear();
}

