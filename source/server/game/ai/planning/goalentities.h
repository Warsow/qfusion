#ifndef WSW_62ef46e7_ee5e_4416_975c_b09cfb45ce73_H
#define WSW_62ef46e7_ee5e_4416_975c_b09cfb45ce73_H

#include "../ailocal.h"
#include <common/wswstaticvector.h>
#include "../vec3.h"

enum class NavEntityFlags : unsigned {
	NONE = 0x0,
	REACH_AT_TOUCH = 0x1,
	REACH_AT_RADIUS = 0x2,
	REACH_ON_EVENT = 0x4,
	REACH_IN_GROUP = 0x8,
	DROPPED_ENTITY = 0x10,
	NOTIFY_SCRIPT = 0x20,
	MOVABLE = 0x40
};

inline NavEntityFlags operator|( const NavEntityFlags &lhs, const NavEntityFlags &rhs ) {
	return (NavEntityFlags)( (unsigned)lhs | (unsigned)( rhs ) );
}
inline NavEntityFlags operator&( const NavEntityFlags &lhs, const NavEntityFlags &rhs ) {
	return (NavEntityFlags)( (unsigned)lhs & (unsigned)( rhs ) );
}

enum class NavTargetFlags : unsigned {
	NONE = 0x0,
	REACH_ON_RADIUS = 0x1,
	REACH_ON_EVENT = 0x2,
	REACH_IN_GROUP = 0x4,
	TACTICAL_SPOT = 0x8
};

inline NavTargetFlags operator|( const NavTargetFlags &lhs, const NavTargetFlags &rhs ) {
	return (NavTargetFlags)( (unsigned)lhs | (unsigned)rhs );
}
inline NavTargetFlags operator&( const NavTargetFlags &lhs, const NavTargetFlags &rhs ) {
	return (NavTargetFlags)( (unsigned)lhs & (unsigned)rhs );
}

class NavEntity;

/**
 * This is a base subclass for things that may be navigation targets for bot movement subsystem.
 * Common and often-used fields are lifted here to avoid dynamic dispatch overhead
 * (as they are accessed during bot movement planning).
 * Descendants must set these fields properly.
 */
class NavTarget {
protected:
	int aasAreaNum;
	Vec3 origin;

	NavTarget( int aasAreaNum_, const Vec3 &origin_ )
		: aasAreaNum( aasAreaNum_ ), origin( origin_ ) {}

	NavTarget( int aasAreaNum_, const vec3_t origin_ )
		: aasAreaNum( aasAreaNum_ ), origin( origin_ ) {}

	virtual ~NavTarget() = default;
public:
	int AasAreaNum() const { return aasAreaNum; }
	const Vec3 &Origin() const { return origin; }

	virtual const char *Name() const = 0;

	virtual bool IsBasedOnEntity( const edict_t *ent ) const { return false; }
	virtual bool IsBasedOnNavEntity( const NavEntity *navEntity ) const { return false; }

	virtual float RadiusOrDefault( float defaultValue ) const { return defaultValue; }
	virtual bool IsTopTierItem( const float *externalEntityWeights ) const { return false; }
	virtual bool IsTopTierWeapon() const { return false; }
	virtual bool ShouldBeReachedAtTouch() const = 0;
	virtual bool ShouldBeReachedAtRadius() const = 0;
	virtual bool ShouldBeReachedOnEvent() const = 0;
};

// A NavEntity is based on some entity (edict_t) plus some attributes.
// All NavEntities are global for all Ai beings.
// A Goal is often based on a NavEntity
class NavEntity final : public NavTarget {
	friend class NavEntitiesRegistry;
	template <typename T> friend auto wsw::link( T *, T ** ) -> T *;
	template <typename T> friend auto wsw::unlink( T *, T ** ) -> T *;

	// An entity this goal is based on
	const edict_t *const ent;

	NavEntity *prev { nullptr };
	NavEntity *next { nullptr };

	int entityId;
	NavEntityFlags flags { NavEntityFlags::NONE };

	static constexpr unsigned MAX_NAME_LEN = 64;
	char name[MAX_NAME_LEN];

	bool IsFlagSet( NavEntityFlags flag ) const {
		return NavEntityFlags::NONE != ( this->flags & flag );
	}

	NavEntity( const edict_t *ent_, int aasAreaNum_, NavEntityFlags flags_ );

	NavEntity *Next() { return next; }
public:
	NavEntityFlags Flags() const { return flags; }
	int Id() const { return entityId; }
	int EntityId() const { return entityId; }
	/**
	 * A cost influence defines how base entity weight is affected by cost (move duration and wait time).
	 * A cost influence is a positive float number usually in 0.5-1.0 range.
	 * Lesser cost influence means that an entity weight is less affected by distance.
	 */
	float CostInfluence() const;

	const gsitem_t *Item() const { return ent->item; }

	bool IsEnabled() const { return ent->r.inuse; }
	bool IsDisabled() const { return !IsEnabled(); }

	bool IsBasedOnEntity( const edict_t *e ) const override { return this->ent == e; }
	bool IsBasedOnNavEntity( const NavEntity *navEntity ) const override { return navEntity == this; }
	bool IsClient() const { return ent->r.client != nullptr; }
	bool IsSpawnedNow() const { return ent->r.solid != SOLID_NOT; }

	bool IsDroppedEntity() const { return IsFlagSet( NavEntityFlags::DROPPED_ENTITY ); }

	bool ShouldBeReachedInGroup() const { return IsFlagSet( NavEntityFlags::REACH_IN_GROUP ); }

	uint64_t MaxWaitDuration() const;

	bool IsTopTierItem( const float *overriddenEntityWeights ) const override;
	bool IsTopTierWeapon() const override;

	const char *Name() const override { return name; }

	void NotifyTouchedByBot( const edict_t *botEnt ) const {
		if( ShouldNotifyScript() ) {
			GT_asBotTouchedGoal( botEnt->bot, ent );
		}
	}

	void NotifyBotReachedRadius( const edict_t *botEnt ) const {
		if( ShouldNotifyScript() ) {
			GT_asBotReachedGoalRadius( botEnt->bot, ent );
		}
	}

	int64_t Timeout() const;

	bool ShouldBeReachedAtTouch() const override { return IsFlagSet( NavEntityFlags::REACH_AT_TOUCH ); }
	bool ShouldBeReachedAtRadius() const override { return IsFlagSet( NavEntityFlags::REACH_AT_RADIUS ); }
	bool ShouldBeReachedOnEvent() const override { return IsFlagSet( NavEntityFlags::REACH_ON_EVENT ); }

	bool ShouldNotifyScript() const { return IsFlagSet( NavEntityFlags::NOTIFY_SCRIPT ); }

	const NavEntity *Next() const { return next; }

	/**
	 * @return level.time if the item is already spawned, zero if spawn time is unknown,
	 * a valid spawn time if the item is not spawned and spawn time could be predicted
	 */
	int64_t SpawnTime() const;
};

class NavSpot final : public NavTarget {
	NavTargetFlags flags;
	float radius;

	bool IsFlagSet( NavTargetFlags flag ) const {
		return NavTargetFlags::NONE != ( this->flags & flag );
	}
public:
	static NavSpot Dummy() {
		return NavSpot( Vec3( 0, 0, 0 ), 0.0f, NavTargetFlags::NONE );
	}

	NavSpot( const Vec3 &origin_, float radius_, NavTargetFlags flags_ )
		: NavTarget( AiAasWorld::instance()->findAreaNum( origin_ ), origin_ )
		, flags( flags_ | NavTargetFlags::REACH_ON_RADIUS ), radius( radius_ ) {}

	void Set( const Vec3 &origin_, float radius_, NavTargetFlags flags_ ) {
		NavTarget::origin = origin_;
		NavTarget::aasAreaNum = AiAasWorld::instance()->findAreaNum( origin_ );
		this->radius = radius_;
		this->flags = flags_ | NavTargetFlags::REACH_ON_RADIUS;
	}

	const char *Name() const override { return "???"; }

	float RadiusOrDefault( float ) const override { return radius; }

	bool ShouldBeReachedAtTouch() const override { return false; }
	bool ShouldBeReachedAtRadius() const override { return IsFlagSet( NavTargetFlags::REACH_ON_RADIUS ); }
	bool ShouldBeReachedOnEvent() const override { return IsFlagSet( NavTargetFlags::REACH_ON_EVENT ); }
};

class NavEntitiesRegistry {
	friend void AI_CommonFrame();

	NavEntity **entityToNavEntity;

	static constexpr size_t STORAGE_STRIDE =
		( sizeof( NavEntity ) % 8 ? sizeof( NavEntity ) + 8 - sizeof( NavEntity ) % 8 : sizeof( NavEntity ) );

	uint8_t *navEntsStorage;

	NavEntity *activeNavEntsHead { nullptr };

	NavEntitiesRegistry();

	static NavEntitiesRegistry *instance;

	void Update();
public:
	static void Init();
	static void Shutdown();

	/**
	 * @note null results are allowed as a temporary hack for initialization order issues.
	 * Otherwise intrusive changes to /game code are required.
	 */
	static NavEntitiesRegistry *Instance() { return instance; }

	~NavEntitiesRegistry() {
		// The storage follows in the same memory block
		Q_free( entityToNavEntity );
	}

	const NavEntity *Head() const { return activeNavEntsHead; }

	NavEntity *AddNavEntity( edict_t *ent, int aasAreaNum, NavEntityFlags flags );
	void RemoveNavEntity( NavEntity *navEntity );

	NavEntity *NavEntityForEntity( const edict_t *ent ) {
		if( !ent ) {
			return nullptr;
		}
		return entityToNavEntity[ENTNUM( ent )];
	}
};

#endif
