#include "basescript2.h"

BaseScript2::BaseScript2( BotMovementModule *module )
	: m_module( module )
	, m_cachedPath( &module->m_sharedCachedPath ) {}

[[nodiscard]]
auto BaseScript2::getActionAndRecordForCurrGameState( MovementActionRecord *record ) -> BaseMovementAction * {
	return nullptr;
}
