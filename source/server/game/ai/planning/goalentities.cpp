#include "goalentities.h"
#include <common/helpers/links.h>

float NavEntity::CostInfluence() const {
	// Usually these kinds of nav entities are CTF flags or bomb spots,
	// so these entities should be least affected by cost.
	if( !ent->item ) {
		return 0.5f;
	}

	// Make this local copy to shorten lines.
	// Cast a enum value to int in comparisons to avoid analyzer warnings
	// (different enum types are used for comparison).
	const int itemTag = ent->item->tag;

	// Cost influence is not a weight.
	// Cost influence separates different classes of items, not items itself.
	// For two items of the same cost class costs should match and weights may (and usually) no.
	// We add extra conditions just because MH and a health bubble,
	// RA and an armor shard are not really in the same class.
	switch( ent->item->type ) {
		case IT_POWERUP:
			return 0.6f;
		case IT_ARMOR:
			return ( itemTag != ARMOR_SHARD ) ? 0.7f : 0.9f;
		case IT_HEALTH:
			return ( itemTag == HEALTH_MEGA || itemTag == HEALTH_ULTRA ) ? 0.7f : 0.9f;
		case IT_WEAPON:
			return 0.8f;
		case IT_AMMO:
			return 1.0f;
	}

	// Return a default value for malformed (e.g. in custom GT scripts) item type/tags.
	return 0.5f;
}

bool NavEntity::IsTopTierItem( const float *overriddenEntityWeights ) const {
	if( !ent->r.inuse ) {
		return false;
	}

	if( !ent->item ) {
		return overriddenEntityWeights && overriddenEntityWeights[ENTNUM( ent )] >= 2.0f;
	}

	// Make these local copies to shorten lines.
	const auto itemType = ent->item->type;

	// Cast a enum value to int in comparisons to avoid analyzer warnings
	// (different enum types are used for comparison).
	const int itemTag = ent->item->tag;

	if( itemType == IT_POWERUP ) {
		return true;
	}

	if( itemType == IT_HEALTH ) {
		switch( itemTag ) {
			case HEALTH_MEGA:
			case HEALTH_ULTRA:
				return true;
		}	
	}

	if( itemType == IT_ARMOR ) {
		switch( itemTag ) {
			case ARMOR_RA:
			case ARMOR_YA:
				return true;
		}	
	}

	return false;
}

bool NavEntity::IsTopTierWeapon() const {
	const auto *item = ent->item;
	if( !item ) {
		return false;
	}

	if( item->type != IT_WEAPON ) {
		return false;
	}

	// Consider all other weapons top-tier ones
	static_assert( WEAP_NONE < WEAP_GUNBLADE, "" );
	static_assert( WEAP_GUNBLADE < WEAP_MACHINEGUN, "" );
	static_assert( WEAP_MACHINEGUN < WEAP_RIOTGUN, "" );
	static_assert( WEAP_RIOTGUN < WEAP_GRENADELAUNCHER, "" );
	return item->tag > WEAP_GRENADELAUNCHER;
}

int64_t NavEntity::SpawnTime() const {
	if( !ent->r.inuse ) {
		return 0;
	}

	if( ent->r.solid == SOLID_TRIGGER ) {
		return level.time;
	}

	// This means the nav entity is spawned by a script
	// (only items are hardcoded nav. entities)
	// Let the script manage the entity, do not prevent any action with the entity
	if( !ent->item ) {
		return level.time;
	}

	if( !ent->classname ) {
		return 0;
	}

	// MH needs special handling
	// If MH owner is sent, exact MH spawn time can't be predicted
	// Otherwise fallback to the generic spawn prediction code below
	// Check owner first to cut off string comparison early in negative case
	if( ent->r.owner && !Q_stricmp( "item_health_mega", ent->classname ) ) {
		return 0;
	}

	return ent->nextThink;
}

uint64_t NavEntity::MaxWaitDuration() const {
	if( !ent->item || ShouldBeReachedOnEvent() ) {
		return std::numeric_limits<uint64_t>::max();
	}

	// Make these local copies to shorten lines.
	const auto itemType = ent->item->type;

	// Cast a enum value to int in comparisons to avoid analyzer warnings
	// (different enum types are used for comparison).
	const int itemTag = ent->item->tag;

	if( itemType == IT_POWERUP ) {
		return 5000;
	}

	if( itemType == IT_HEALTH && ( itemTag == HEALTH_MEGA || itemTag == HEALTH_ULTRA ) ) {
		return 3000;
	}

	if( itemType == IT_ARMOR ) {
		switch( itemTag ) {
			case ARMOR_RA:
				return 3000;
			case ARMOR_YA:
				return 2000;
			case ARMOR_GA:
				return 1500;
		}
	}

	return 1000;
}

int64_t NavEntity::Timeout() const {
	if( IsDroppedEntity() ) {
		return ent->nextThink;
	}
	return std::numeric_limits<int64_t>::max();
}

static wsw::StaticVector<NavEntitiesRegistry, 1> instanceHolder;
NavEntitiesRegistry *NavEntitiesRegistry::instance = nullptr;

void NavEntitiesRegistry::Init() {
	assert( !instance );
	instance = new( instanceHolder.unsafe_grow_back() )NavEntitiesRegistry;
}

void NavEntitiesRegistry::Shutdown() {
	if( instance ) {
		instanceHolder.clear();
		instance = nullptr;
	}
}

NavEntitiesRegistry::NavEntitiesRegistry() {
	const size_t lookupTableSize = sizeof( NavEntity * ) * MAX_EDICTS;
	const size_t storageSize = STORAGE_STRIDE * MAX_NAVENTS;
	static_assert( lookupTableSize, "The assumption on alignment of remaining bytes is broken" );

	auto *const mem = (uint8_t *)Q_malloc( storageSize + lookupTableSize );
	entityToNavEntity = (NavEntity **)mem;
	navEntsStorage = mem + lookupTableSize;

	memset( entityToNavEntity, 0, lookupTableSize );

	// Always add nav entities for clients
	// (despite requiring special handling in Update() they are expected to be always present)
	for( int clientEnt = 1; clientEnt <= ggs->maxclients; ++clientEnt ) {
		AddNavEntity( game.edicts + clientEnt, 0, NavEntityFlags::REACH_ON_EVENT | NavEntityFlags::MOVABLE );
	}
}

void NavEntitiesRegistry::Update() {
	const auto *aasWorld = AiAasWorld::instance();
	for( auto *navEnt = activeNavEntsHead; navEnt; navEnt = navEnt->Next() ) {
		if( ( navEnt->flags & NavEntityFlags::MOVABLE ) != NavEntityFlags::NONE ) {
			// If it has been moved
			if( navEnt->origin != Vec3( navEnt->ent->s.origin ) ) {
				navEnt->origin.set( navEnt->ent->s.origin );
				navEnt->aasAreaNum = aasWorld->findAreaNum( navEnt->ent->s.origin );
			}
		}
	}
}

NavEntity::NavEntity( const edict_t *ent_, int aasAreaNum_, NavEntityFlags flags_ )
	: NavTarget( aasAreaNum_, ent_->s.origin )
	, ent( ent_ )
	, entityId( (int)( ent_ - game.edicts ) )
	, flags( flags_ ) {
	// Try providing sane debug messages
	const char *const className = ent->classname ? ent->classname : "";

	if( ( flags & NavEntityFlags::MOVABLE ) != NavEntityFlags::NONE ) {
		if( ENTNUM( ent ) <= ggs->maxclients ) {
			Q_snprintfz( name, NavEntity::MAX_NAME_LEN, "%s(ent#=%d)@client", className, ENTNUM( ent ) );
		} else {
			Q_snprintfz( name, NavEntity::MAX_NAME_LEN, "%s(ent#=%d)@movable", className, ENTNUM( ent ) );
		}
		return;
	}

	auto x = (int)ent->s.origin[0];
	auto y = (int)ent->s.origin[1];
	auto z = (int)ent->s.origin[2];
	Q_snprintfz( name, NavEntity::MAX_NAME_LEN, "%s(ent#=%d)@{%d %d %d}", className, ENTNUM( ent ), x, y, z );
}

NavEntity *NavEntitiesRegistry::AddNavEntity( edict_t *ent, int aasAreaNum, NavEntityFlags flags ) {
	assert( ent );
	const auto num = (int)( ent - game.edicts );
	assert( !entityToNavEntity[num]);

	void *const mem = navEntsStorage + num * STORAGE_STRIDE;
	auto *const navEntity = new( mem )NavEntity( ent, aasAreaNum, flags );

	entityToNavEntity[num] = navEntity;
	wsw::link( navEntity, &activeNavEntsHead );

	return navEntity;
}

void NavEntitiesRegistry::RemoveNavEntity( NavEntity *navEntity ) {
	assert( navEntity );
	const auto num = (int)( navEntity->ent - game.edicts );
	assert( entityToNavEntity[num] );

	wsw::unlink( navEntity, &activeNavEntsHead );
	entityToNavEntity[num] = nullptr;

	navEntity->~NavEntity();
}
