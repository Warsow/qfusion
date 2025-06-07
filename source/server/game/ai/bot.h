#ifndef WSW_da942a7a_e77d_4eaf_9ad8_e1b4b6fc5240_H
#define WSW_da942a7a_e77d_4eaf_9ad8_e1b4b6fc5240_H

#include <common/types/staticvector.h>
#include <common/helpers/links.h>
#include "awareness/awarenessmodule.h"
#include "planning/planningmodule.h"
#include "vec3.h"

#include "movement/movementsubsystem.h"
#include "combat/weaponsusagemodule.h"
#include "planning/tacticalspotscache.h"
#include "awareness/awarenessmodule.h"
#include "planning/roamingmanager.h"
#include "botweightconfig.h"

#include "planning/goals.h"
#include "planning/actions.h"

class AiSquad;
class EnemiesTracker;

/**
 * This can be represented as an enum but feels better in the following form.
 * Many values that affect bot behaviour already are not boolean
 * (such as nav targets and special movement states like camping spots),
 * and thus controlling a bot by a single flags field already is not possible.
 * This struct is likely to be extended by non-boolean values later.
 */
struct SelectedMiscTactics {
	bool willAdvance;
	bool willRetreat;

	bool shouldBeSilent;
	bool shouldMoveCarefully;

	bool shouldAttack;
	bool shouldKeepXhairOnEnemy;

	bool willAttackMelee;
	bool shouldRushHeadless;

	SelectedMiscTactics() { Clear(); };

	void Clear() {
		willAdvance = false;
		willRetreat = false;

		shouldBeSilent = false;
		shouldMoveCarefully = false;

		shouldAttack = false;
		shouldKeepXhairOnEnemy = false;

		willAttackMelee = false;
		shouldRushHeadless = false;
	}

	void PreferAttackRatherThanRun() {
		shouldAttack = true;
		shouldKeepXhairOnEnemy = true;
	}

	void PreferRunRatherThanAttack() {
		shouldAttack = true;
		shouldKeepXhairOnEnemy = false;
	}
};

struct AiObjectiveSpot;
struct AiDefenceSpot;
struct AiOffenseSpot;

class Bot: public AiComponent {
	friend class AiManager;
	friend class BotEvolutionManager;
	friend class AiBaseTeam;
	friend class AiSquadBasedTeam;
	friend class AiObjectiveBasedTeam;
	friend class AiPlanner;
	friend class BotPlanner;
	friend class AiAction;
	friend class AiGoal;
	friend class AiSquad;
	friend class SquadsBuilder;
	friend class EnemiesTracker;
	friend class PathBlockingTracker;
	friend class BotAwarenessModule;
	friend class BotFireTargetCache;
	friend class BotItemsSelector;
	friend class BotWeaponSelector;
	friend class BotWeaponsUsageModule;
	friend class BotRoamingManager;
	friend class TacticalSpotsRegistry;
	friend class BotTacticalSpotsCache;
	friend class RoamGoal;
	friend class WorldState;

	friend class MovementSubsystem;
	friend class PredictionContext;
	friend class CorrectWeaponJumpAction;

	friend class CachedTravelTimesMatrix;

	template <typename T> friend auto wsw::link( T *, T **, int ) -> T *;
	template <typename T> friend auto wsw::unlink( T *, T **, int ) -> T *;
public:
	static constexpr auto ALLOWED_TRAVEL_FLAGS =
		TFL_WALK | TFL_WALKOFFLEDGE | TFL_JUMP | TFL_STRAFEJUMP | TFL_AIR | TFL_TELEPORT | TFL_JUMPPAD |
		TFL_WATER | TFL_WATERJUMP | TFL_SWIM | TFL_LADDER | TFL_ELEVATOR | TFL_BARRIERJUMP;

	Bot( edict_t *self_, float skillLevel_ );

	~Bot() override;

	// For backward compatibility with dated code that should be rewritten
	const edict_t *Self() const { return self; }
	edict_t *Self() { return self; }

	// Should be preferred instead of use of Self() that is deprecated and will be removed
	int EntNum() const { return ENTNUM( self ); }

	int ClientNum() const { return ENTNUM( self ) - 1; }

	const player_state_t *PlayerState() const { return &self->r.client->ps; }
	player_state_t *PlayerState() { return &self->r.client->ps; }

	const float *Origin() const { return self->s.origin; }
	const float *Velocity() const { return self->velocity; }

	float Skill() const { return skillLevel; }
	bool IsReady() const { return level.ready[PLAYERNUM( self )]; }

	void TouchedEntity( edict_t *ent );

	void OnPain( const edict_t *enemy, float kick, int damage );
	void OnKnockback( const edict_t *attacker, const vec3_t basedir, int kick, int dflags );
	void OnEnemyDamaged( const edict_t *enemy, int damage );
	void OnEnemyOriginGuessed( const edict_t *enemy, unsigned millisSinceLastSeen, const float *guessedOrigin = nullptr );

	void RegisterEvent( const edict_t *ent, int event, int parm ) {
		awarenessModule.RegisterEvent( ent, event, parm );
	}

	void OnAttachedToSquad( AiSquad *squad_ );
	void OnDetachedFromSquad( AiSquad *squad_ );

	inline bool IsInSquad() const { return squad != nullptr; }

	/**
	 * Returns a timestamp of last attack (being hit) by an attacker.
	 * @note bots forget attack stats in a dozen of seconds.
	 * @param attacker an entity that (maybe) initiated an attack.
	 * @return a timestamp of last attack or 0 if a record of such event can't be found.
	 */
	int64_t LastAttackedByTime( const edict_t *attacker ) {
		return awarenessModule.LastAttackedByTime( attacker );
	}
	/**
	 * Returns a timestamp of last selection of a target by the bot.
	 * @note bots forget attack stats in a dozen of seconds.
	 * @param target an entity that (maybe) was selected as a target.
	 * @return a timestamp of last selection as a target or 0 if a record of such event can't be found.
	 */
	int64_t LastTargetTime( const edict_t *target ) {
		return awarenessModule.LastTargetTime( target );
	}

	void OnEnemyRemoved( const TrackedEnemy *enemy ) {
		awarenessModule.OnEnemyRemoved( enemy );
	}

	void OnHurtByNewThreat( const edict_t *newThreat, const AiComponent *threatDetector ) {
		awarenessModule.OnHurtByNewThreat( newThreat, threatDetector );
	}

	float GetBaseOffensiveness() const { return baseOffensiveness; }

	float GetEffectiveOffensiveness() const;

	void SetBaseOffensiveness( float baseOffensiveness_ ) {
		this->baseOffensiveness = baseOffensiveness_;
		Q_clamp( this->baseOffensiveness, 0.0f, 1.0f );
	}

	void ClearOverriddenEntityWeights() {
		planningModule.ClearOverriddenEntityWeights();
	}

	void OverrideEntityWeight( const edict_t *ent, float weight ) {
		planningModule.OverrideEntityWeight( ent, weight );
	}

	const int *Inventory() const { return self->r.client->ps.inventory; }

	void EnableAutoAlert( const AiAlertSpot &alertSpot,
						  AlertTracker::AlertCallback callback,
						  AiComponent *receiver ) {
		awarenessModule.EnableAutoAlert( alertSpot, callback, receiver );
	}

	void DisableAutoAlert( int id ) {
		awarenessModule.DisableAutoAlert( id );
	}

	int Health() const {
		return self->r.client->ps.stats[STAT_HEALTH];
	}
	int Armor() const {
		return self->r.client->ps.stats[STAT_ARMOR];
	}

	bool CanAndWouldDropHealth() const {
		return GT_asBotWouldDropHealth( self->r.client );
	}

	void DropHealth() {
		GT_asBotDropHealth( self->r.client );
	}

	bool CanAndWouldDropArmor() const {
		return GT_asBotWouldDropArmor( self->r.client );
	}

	void DropArmor() {
		GT_asBotDropArmor( self->r.client );
	}

	float PlayerDefenciveAbilitiesRating() const {
		return GT_asPlayerDefenciveAbilitiesRating( self->r.client );
	}

	float PlayerOffenciveAbilitiesRating() const {
		return GT_asPlayerOffensiveAbilitiesRating( self->r.client );
	}

	const AiObjectiveSpot *ObjectiveSpot() const {
		return objectiveSpot;
	}

	void SetObjectiveSpot( AiObjectiveSpot *spot ) {
		objectiveSpot = spot;
	}

	int DefenceSpotId() const;
	int OffenseSpotId() const;

	/**
	 * Returns a field of view of the bot in degrees (dependent of skill level).
	 */
	float Fov() const { return 75.0f + 50.0f * Skill(); }
	/**
	 * Returns a value based on {@code Fov()} that is ready to be used
	 * in comparison of dot products of normalized vectors to determine visibility.
	 */
	float FovDotFactor() const { return cosf( (float)DEG2RAD( Fov() / 2 ) ); }

	const WorldState &CachedWorldState() const {
		return planningModule.CachedWorldState();
	}

	const BotWeightConfig &WeightConfig() const { return weightConfig; }
	BotWeightConfig &WeightConfig() { return weightConfig; }

	void OnInterceptedPredictedEvent( int ev, int parm ) {
		m_movementSubsystem.OnInterceptedPredictedEvent( ev, parm );
	}

	void OnInterceptedPMoveTouchTriggers( pmove_t *pm, const vec3_t previousOrigin ) {
		m_movementSubsystem.OnInterceptedPMoveTouchTriggers( pm, previousOrigin );
	}

	const AiEntityPhysicsState *EntityPhysicsState() const {
		return entityPhysicsState;
	}

	// The movement code should use this method if there really are no
	// feasible ways to continue traveling to the nav target.
	void OnMovementToNavTargetBlocked();

	void notifyOfNavEntitySignaledAsReached( const NavEntity *navEntity );
	void notifyOfNavEntityRemoved( const NavEntity *navEntity );

	int NavTargetAasAreaNum() const {
		return navTarget ? navTarget->AasAreaNum() : 0;
	}

	Vec3 NavTargetOrigin() const {
		if( !navTarget ) {
			AI_FailWith( "Ai::NavTargetOrigin()", "Nav target is not present\n" );
		}
		return navTarget->Origin();
	}

	float NavTargetRadius() const {
		if( !navTarget ) {
			AI_FailWith( "Ai::NavTargetRadius()", "Nav target is not present\n" );
		}
		return navTarget->RadiusOrDefault( 40.0f );
	}

	bool IsNavTargetBasedOnEntity( const edict_t *ent ) const {
		return navTarget && navTarget->IsBasedOnEntity( ent );
	}

	void SetNavTarget( const NavTarget *navTarget_ ) {
		this->navTarget = navTarget_;
	}

	void SetNavTarget( const Vec3 &navTargetOrigin, float reachRadius ) {
		localNavSpotStorage.Set( navTargetOrigin, reachRadius, NavTargetFlags::REACH_ON_RADIUS );
		this->navTarget = &localNavSpotStorage;
	}

	void ResetNavTarget() {
		this->navTarget = nullptr;
	}

	bool IsCloseToNavTarget( float proximityThreshold ) const {
		return DistanceSquared( self->s.origin, navTarget->Origin().Data() ) < proximityThreshold * proximityThreshold;
	}

	unsigned MillisInBlockedState() const {
		int64_t diff = BLOCKED_TIMEOUT - ( blockedTimeoutAt - level.time );
		return diff >= 0 ? (unsigned)diff : 0;
	}

	bool IsBlocked() const {
		// Blocking is checked in Think() frames (usually every 64 millis),
		// so the blockedTimeoutAt value might be a bit outdated
		return MillisInBlockedState() > 64 + 16;
	}

	int TravelFlags() const { return travelFlags; }

	// Exposed for native and script actions
	int CheckTravelTimeMillis( const Vec3 &from, const Vec3 &to, bool allowUnreachable = true );

	// Helps to reject non-feasible enemies quickly.
	// A false result does not guarantee that enemy is feasible.
	// A true result guarantees that enemy is not feasible.
	bool IsDefinitelyNotAFeasibleEnemy( const edict_t *ent ) const;

	void SetAttitude( const edict_t *ent, int attitude );

	// These methods are exposed mostly for script interface
	unsigned NextSimilarWorldStateInstanceId() {
		return ++similarWorldStateInstanceId;
	}

	int64_t LastTriggerTouchTime() const {
		return wsw::max( m_lastTouchedJumppadAt, wsw::max( m_lastTouchedTeleportAt, m_lastTouchedElevatorAt ) );
	}

	int64_t LastTeleportTouchTime() const { return m_lastTouchedTeleportAt; }
	int64_t LastJumppadTouchTime() const { return m_lastTouchedJumppadAt; }

	int64_t LastKnockbackAt() const { return lastKnockbackAt; }

	void ForceSetNavEntity( const std::optional<SelectedNavEntity> &selectedNavEntity );

	void ForcePlanBuilding() {
		planner->ClearGoalAndPlan();
	}

	void SetCampingSpot( const AiCampingSpot &campingSpot ) {
		m_movementSubsystem.SetCampingSpot( campingSpot );
	}
	void ResetCampingSpot() {
		m_movementSubsystem.ResetCampingSpot();
	}
	bool HasActiveCampingSpot() const {
		return m_movementSubsystem.HasActiveCampingSpot();
	}
	void SetPendingLookAtPoint( const AiPendingLookAtPoint &lookAtPoint, unsigned timeoutPeriod ) {
		return m_movementSubsystem.SetPendingLookAtPoint( lookAtPoint, timeoutPeriod );
	}
	void ResetPendingLookAtPoint() {
		m_movementSubsystem.ResetPendingLookAtPoint();
	}
	bool HasPendingLookAtPoint() const {
		return m_movementSubsystem.HasPendingLookAtPoint();
	}

	bool CanInterruptMovement() const {
		return m_movementSubsystem.CanInterruptMovement();
	}

	const std::optional<SelectedNavEntity> &GetSelectedNavEntity() const {
		return m_selectedNavEntity;
	}

	bool NavTargetWorthRushing() const;

	bool NavTargetWorthWeaponJumping() const {
		// TODO: Implement more sophisticated logic for this and another methods
		return NavTargetWorthRushing();
	}

	bool IsNavTargetATopTierItem() const {
		return planningModule.IsTopTierItem( navTarget );
	}

	// Returns a number of weapons the logic allows to be used for weapon jumping.
	// The buffer is assumed to be capable to store all implemented weapons.
	int GetWeaponsForWeaponJumping( int *weaponNumsBuffer );

	const std::optional<SelectedNavEntity> &GetOrUpdateSelectedNavEntity();

	const std::optional<SelectedEnemy> &GetSelectedEnemy() const { return m_selectedEnemy; }

	const Hazard *PrimaryHazard() const {
		return awarenessModule.PrimaryHazard();
	}

	SelectedMiscTactics &GetMiscTactics() { return selectedTactics; }
	const SelectedMiscTactics &GetMiscTactics() const { return selectedTactics; }

	const AiAasRouteCache *RouteCache() const { return routeCache; }

	const TrackedEnemy *TrackedEnemiesHead() const {
		return awarenessModule.TrackedEnemiesHead();
	}

	const BotAwarenessModule::HurtEvent *ActiveHurtEvent() const {
		return awarenessModule.GetValidHurtEvent();
	}

	const std::optional<Vec3> &GetKeptInFovPoint() const {
		return awarenessModule.GetKeptInFovPoint();
	}

	bool WillAdvance() const { return selectedTactics.willAdvance; }
	bool WillRetreat() const { return selectedTactics.willRetreat; }

	bool ShouldBeSilent() const { return selectedTactics.shouldBeSilent; }
	bool ShouldMoveCarefully() const { return selectedTactics.shouldMoveCarefully; }

	bool ShouldAttack() const { return selectedTactics.shouldAttack; }
	bool ShouldKeepXhairOnEnemy() const { return selectedTactics.shouldKeepXhairOnEnemy; }

	bool WillAttackMelee() const { return selectedTactics.willAttackMelee; }
	bool ShouldRushHeadless() const { return selectedTactics.shouldRushHeadless; }

	/**
	 * A hint for the weapon usage module.
	 * If true, bot should wait for better match of a "crosshair" and an enemy,
	 * otherwise shoot immediately if there is such opportunity.
	 */
	bool ShouldAimPrecisely() const {
		return ShouldKeepXhairOnEnemy() && planningModule.ShouldAimPrecisely();
	}

	bool IsReactingToHazard() const {
		return planningModule.IsReactingToHazard();
	}

	// Whether the bot should stop bunnying even if it could produce
	// good predicted results and concentrate on combat/dodging
	bool ShouldSkinBunnyInFavorOfCombatMovement() const;
	// Whether it is allowed to dash right now
	bool IsCombatDashingAllowed() const;
	// Whether it is allowed to crouch right now
	bool IsCombatCrouchingAllowed() const;

	/**
	 * A wrapper over {@code AiManager::TryGetExpensiveComputationQuota()}.
	 * Should be used for situations where computations are not
	 * necessary for a bot lifecycle (but could improve behaviour).
	 */
	bool TryGetExtraComputationQuota() const;

	/**
	 * A wrapper over {@code AiManager::TryGetExpensiveComputationQuota()}.
	 * Should be used for situations where computations are mandatory
	 * for a bot lifecycle (e.g. a bot is blocked and has to suicide otherwise having no solution).
	 */
	bool TryGetVitalComputationQuota() const;

	/**
	 * A wrapper over {@code AiManager::TryGetExpensiveThinkCallQuota()}.
	 * Provided for consistency.
	 */
	bool TryGetExpensiveThinkCallQuota() const;

	/**
	 * Gets whether the bot has only Gunblade for making attacks.
	 * @note weapons without actual ammo are ignored.
	 */
	bool HasOnlyGunblade() const { return hasOnlyGunblade; }

	void Update();

	bool PermitsDistributedUpdateThisFrame() const {
		// Don't even try this kind of updates during ghosting frames
		assert( !G_ISGHOSTING( self ) );
		// Ensure that the controlling team has already set the affinity properly
		assert( m_frameAffinityModulo && m_frameAffinityOffset < m_frameAffinityModulo && wsw::isPowerOf2( m_frameAffinityModulo ) );
		return ( level.framenum & ( m_frameAffinityModulo - 1 ) ) == m_frameAffinityOffset;
	}
private:
	const char *Nick() const {
		return self->r.client ? self->r.client->netname.data() : self->classname;
	}

	bool CanHandleNavTargetTouch( const edict_t *ent );
	bool TryReachNavTargetByProximity();
	void TouchedOtherEntity( const edict_t *entity );

	bool IsPrimaryAimEnemy( const edict_t *enemy ) const {
		return m_selectedEnemy && m_selectedEnemy->IsBasedOn( enemy );
	}

	inline bool ShouldUseRoamSpotAsNavTarget() const {
		const std::optional<SelectedNavEntity> &maybeSelectedNavEntity = GetSelectedNavEntity();
		return ( maybeSelectedNavEntity == std::nullopt ) && ( level.time - noItemAvailableSince > 3000 );
	}

	bool CanChangeWeapons() const {
		return m_movementSubsystem.CanChangeWeapons();
	}

	void ChangeWeapons( const SelectedWeapons &selectedWeapons_ );

	void OnBlockedTimeout();

	void OnRespawn();

	void CheckTargetProximity();

	float GetChangedAngle( float oldAngle, float desiredAngle, unsigned frameTime,
						   float angularSpeedMultiplier, int angleIndex ) const;

	// This function produces very basic but reliable results.
	// Imitation of human-like aiming should be a burden of callers that prepare the desiredDirection.
	Vec3 GetNewViewAngles( const vec3_t oldAngles, const Vec3 &desiredDirection, unsigned frameTime, float angularSpeedMultiplier ) const;

	edict_t *const self;
	// Must be set in a subclass constructor. A subclass manages memory for its planner
	// (it either has it as an intrusive member of allocates it on heap)
	// and provides a reference to it to this base class via this pointer.
	AiPlanner *planner { nullptr };
	// Must be set in a subclass constructor.
	// A subclass should decide whether a shared or separated route cache should be used.
	// A subclass should destroy the cache instance if necessary.
	AiAasRouteCache *routeCache { nullptr };
	// A cached reference to an AAS world, set by this class
	AiAasWorld *aasWorld;
	// Must be set in a subclass constructor. Can be arbitrary changed later.
	// Can point to external (predicted) entity physics state during movement planning.
	AiEntityPhysicsState *entityPhysicsState { nullptr };

	// Currently, we don't support modifying flags in runtime
	const int travelFlags { ALLOWED_TRAVEL_FLAGS };

	int64_t blockedTimeoutAt;

	unsigned m_frameAffinityModulo { 0 };
	unsigned m_frameAffinityOffset { 0 };

	vec3_t angularViewSpeed { 0.0f, 0.0f, 0.0f };

	// An actually used nav target, be it a nav entity or a spot
	const NavTarget *navTarget { nullptr };
	NavSpot localNavSpotStorage { NavSpot::Dummy() };

	// Negative  = enemy
	// Zero      = ignore (don't attack)
	// Positive  = allies (might be treated as potential squad mates)
	// All entities have a negative attitude by default.
	// The default MayNotBeFeasibleEnemy() gives attitude the lowest priority,
	// teams in team-based gametypes, aiIntrinsicEntityWeight (for non-clients) are tested first.
	int8_t attitude[MAX_EDICTS];

	AiSquad *squad { nullptr };
	const float skillLevel;
	float baseOffensiveness { 0.5f };

	unsigned similarWorldStateInstanceId { 0 };

	// TODO: Move to the AwarenessModule?
	std::optional<SelectedEnemy> m_selectedEnemy;
	std::optional<SelectedEnemy> m_lostEnemy;
	SelectedMiscTactics selectedTactics;
	std::optional<SelectedNavEntity> m_selectedNavEntity;

	// Put the movement subsystem at the object beginning so the relative offset is small
	MovementSubsystem m_movementSubsystem;
	BotAwarenessModule awarenessModule;

	// Put planning module and weight config together
	BotPlanningModule planningModule;
	BotWeightConfig weightConfig;

	BotWeaponsUsageModule weaponsUsageModule;

	std::optional<BotInput> m_pendingClientThinkInput;

	static constexpr float DEFAULT_YAW_SPEED = 330.0f;
	static constexpr float DEFAULT_PITCH_SPEED = 170.0f;

	static constexpr unsigned BLOCKED_TIMEOUT = 15000;

	/**
	 * {@code next[]} and {@code prev[]} links below are addressed by these indices
	 */
	enum { SQUAD_LINKS, TMP_LINKS, TEAM_LINKS, OBJECTIVE_LINKS, AI_LINKS };

	Bot *next[5] { nullptr, nullptr, nullptr, nullptr, nullptr };
	Bot *prev[5] { nullptr, nullptr, nullptr, nullptr, nullptr };

	Bot *NextInSquad() { return next[SQUAD_LINKS]; };
	const Bot *NextInSquad() const { return next[SQUAD_LINKS]; }

	Bot *NextInTmpList() { return next[TMP_LINKS]; }
	const Bot *NextInTmpList() const { return next[TMP_LINKS]; }

	Bot *NextInBotsTeam() { return next[TEAM_LINKS]; }
	const Bot *NextInBotsTeam() const { return next[TEAM_LINKS]; }

	Bot *NextInObjective() { return next[OBJECTIVE_LINKS]; }
	const Bot *NextInObjective() const { return next[OBJECTIVE_LINKS]; }

	Bot *NextInAIList() { return next[AI_LINKS]; }
	const Bot *NextInAIList() const { return next[AI_LINKS]; }

	AiObjectiveSpot *objectiveSpot { nullptr };

	int64_t m_lastTouchedTeleportAt { 0 };
	int64_t m_lastTouchedJumppadAt { 0 };
	int64_t m_lastTouchedElevatorAt { 0 };

	int64_t lastKnockbackAt { 0 };
	int64_t lastOwnKnockbackAt { 0 };
	int lastOwnKnockbackKick { 0 };
	vec3_t lastKnockbackBaseDir;

	int64_t lastItemSelectedAt { 0 };
	int64_t noItemAvailableSince { 0 };

	int64_t lastBlockedNavTargetReportedAt { 0 };

	/**
	 * This value should be updated at the beginning of every frame.
	 * Making it lazy is not good for performance reasons (the result might be accessed in tight loops).
	 */
	bool hasOnlyGunblade { false };
};

#endif
