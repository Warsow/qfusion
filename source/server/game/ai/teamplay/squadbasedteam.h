#ifndef WSW_b3833165_a50a_4f39_93f2_063b6de83c94_H
#define WSW_b3833165_a50a_4f39_93f2_063b6de83c94_H

#include "baseteam.h"
#include "../awareness/enemiestracker.h"
#include "../navigation/aasroutecache.h"
#include "../navigation/aasworld.h"
#include <common/types/staticvector.h>

#include <utility>

class Bot;

class AiSquadBasedTeam;

class AiSquad {
	friend class AiSquadBasedTeam;
	friend class SquadsBuilder;

	template <typename T> friend auto wsw::link( T *, T ** ) -> T *;
	template <typename T> friend auto wsw::unlink( T *, T ** ) -> T *;
public:
	static constexpr unsigned MAX_SQUAD_SIZE = 3;
private:
	AiSquad *next { nullptr };
	AiSquad *prev { nullptr };

	AiSquadBasedTeam *const parent;
	Bot *botsListHead { nullptr };

	int numBots { 0 };
	int squadNum { -1 };

	bool isValid { false };

	// If bots can see at least a single teammate
	bool canFightTogether { false };
	// If bots can move in a single group
	bool canMoveTogether { false };

	/**
	 * If a connectivity of squad members is violated
	 * (bots can't neither fight, nor move together)
	 * and is not restored to this moment, the squad should be invalidated.
	 */
	int64_t brokenConnectivityTimeoutAt { false };

	bool botsDetached { false };

	AiSquad *NextInList() { return next; }

	bool CheckCanFightTogether() const;
	bool CheckCanMoveTogether() const;

	int GetBotFloorCluster( Bot *bot ) const;

	int64_t lastDroppedByBotTimestamps[MAX_CLIENTS];
	int64_t lastDroppedForBotTimestamps[MAX_CLIENTS];

	void CheckMembersInventory();

	/**
	 * Fills an array of best weapon tiers for squad bots.
	 * @param maxBotWeaponTiers an array for results addressed by a client number.
	 * @return lowest best weapon tier among all squad bots.
	 */
	int FindBotWeaponsTiers( int maxBotWeaponTiers[MAX_CLIENTS] ) const;

	/**
	 * Finds a lowest health among squad bots.
	 */
	int FindLowestBotHealth() const;
	/**
	 * Finds a lowest armor among squad bots.
	 */
	int FindLowestBotArmor() const;
	/**
	 * Fills an array mask of possible health suppliers among squad bots.
	 * @param wouldSupplyHealth a mask addressed by a client number.
	 * @return true if at least a single bot can and would drop health.
	 */
	bool FindHealthSuppliers( bool wouldSupplyHealth[MAX_CLIENTS] ) const;
	/**
	 * Fills an array mask of possible armor suppliers among squad bots.
	 * @param wouldSupplyArmor a mask addressed by a client number.
	 * @return true if at least a single bot can and would drop armor.
	 */
	bool FindArmorSuppliers( bool wouldSupplyArmor[MAX_CLIENTS] ) const;

	bool ShouldNotDropItemsNow() const;

	// maxBotWeaponTiers, wouldSupplyHealth, wouldSupplyArmor are global for all bots.
	// Potential suppliers are selected for a single bot, best (nearest) suppliers first.
	// Potential suppliers should be checked then against global capabilities mentioned above.
	void FindSupplierCandidates( Bot *consumer, Bot **suppliersListHead ) const;

	bool RequestWeaponAndAmmoDrop( Bot *consumer, const int *maxBotWeaponTiers, Bot *suppliersListHead );
	bool RequestHealthDrop( Bot *consumer, bool wouldSupplyHealth[MAX_CLIENTS], Bot *suppliersListHead );
	bool RequestArmorDrop( Bot *consumer, bool wouldSupplyArmor[MAX_CLIENTS], Bot *suppliersListHead );

	bool RequestDrop( Bot *consumer, bool wouldSupply[MAX_CLIENTS], Bot *suppliersListHead, void ( Bot::*dropFunc )() );

	edict_t *TryDropAmmo( Bot *consumer, Bot *supplier, int weapon );
	edict_t *TryDropWeapon( Bot *consumer, Bot *supplier, int weapon, const int *maxBotWeaponTiers );

	// Hack! To be able to access bot's private methods, define this entity physics callback as a (static) member
	static void SetDroppedEntityAsBotGoal( edict_t *ent );
protected:
	explicit AiSquad( AiSquadBasedTeam *parent_ );
public:
	/**
	 * Gets a zero-based index of squad that allows address arrays by squad index
	 * (squads are arranged in a linked list and thus do not have natural indices).
	 */
	int Num() const { return squadNum; }

	/**
	 * Gets whether the squad has been invalidated
	 */
	bool IsValid() const { return isValid; }

	/**
	 * Must be called before a first {@code AddBot()} call
	 */
	void PrepareToAddBots();

	/**
	 * Links a bot to a squad.
	 */
	void AddBot( Bot *bot );

	/**
	 * Checks whether a bot can be attached to an existing squad.
	 * Returns zero if it cannot be attached.
	 * Returns a positive value otherwise. The greater the value is the more suitable is the attachment.
	 */
	float GetScoreForBotAttachment( const Bot *bot ) const;

	/**
	 * Marks the squad as invalid. Detaches bots if necessary.
	 * @note duplicated calls are allowed and have no effect.
	 */
	void Invalidate() {
		DetachBots();
		isValid = false;
	}

	/**
	 * Detaches bots (if any)
	 * @note duplicated calls are allowed and have no effect.
	 */
	void DetachBots();

	/**
	 * Checks whether the bot is not a carrier but a supporter for a carrier.
	 * (there is a carrier in squad and the bot is not that carrier).
	 * @note the squad must be valid at the moment of this call.
	 */
	bool IsSupporter( const edict_t *bot ) const;

	void Update();
};

class AiSquadBasedTeam : public AiBaseTeam {
	friend class AiBaseTeam;
	friend class AiSquad;
	friend class SquadsBuilder;

	wsw::StaticVector<AiSquad, MAX_CLIENTS> squads;

	AiSquad *usedSquadsHead { nullptr };
	AiSquad *freeSquadsHead { nullptr };

	/**
	 * A list of bots that are not in squad.
	 */
	Bot *orphanBotsHead { nullptr };

	class PlayerAssistanceTracker {
		friend class AiSquadBasedTeam;

		const AiSquadBasedTeam *const parent;

		int8_t *influenceScores;
		mutable std::pair<float, float> tmpWeights;

		int assistedClientNum[MAX_CLIENTS];
		int assistanceMillisLeft[MAX_CLIENTS];

		explicit PlayerAssistanceTracker( const AiSquadBasedTeam *parent_ );

		~PlayerAssistanceTracker() {
			Q_free( influenceScores );
		}

		void Think() {
			UpdateInfluence();
			DrainAndPick();
		}

		void UpdateInfluence();
		void DrainAndPick();

		inline bool OverridesEntityWeights( const Bot *bot ) const;

		const std::pair<float, float> *GetEntityWeights( const Bot *bot, const NavEntity *navEntity ) const;
	};

	PlayerAssistanceTracker assistanceTracker;
protected:
	void OnBotAdded( Bot *bot ) override;
	void OnBotRemoved( Bot *bot ) override;

	/**
	 * Should be overridden completely if you want to modify squad clustering logic
	 */
	virtual void SetupSquads();

	/**
	 * Gets a new squad for bots addition.
	 */
	AiSquad *AllocSquad();

	void AddToOrphansList( Bot *bot );

	void TransferStateFrom( AiBaseTeam *that ) override;

	static AiSquadBasedTeam *InstantiateTeam( int team );
	static AiSquadBasedTeam *InstantiateTeam( int teamNum, const std::type_info &desiredType );
public:
	explicit AiSquadBasedTeam( int team_ );

	~AiSquadBasedTeam() override;

	virtual void Update();

	bool OverridesEntityWeights( const Bot *bot ) const override;
	const std::pair<float, float> *GetEntityWeights( const Bot *bot, const NavEntity *navEntity ) const override;
};

#endif
