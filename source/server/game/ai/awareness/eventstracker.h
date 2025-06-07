#ifndef WSW_c12756b8_723a_43bd_b7dc_ddea74468e39_H
#define WSW_c12756b8_723a_43bd_b7dc_ddea74468e39_H

#include "../planning/planner.h"
#include <common/types/staticdeque.h>
#include <common/helpers/randomgenerator.h>
#include "awarenesslocal.h"

class EventsTracker : public AiComponent {
	friend class BotAwarenessModule;
	friend class HazardsDetector;

	Bot *const m_bot;

	enum class TeammateVisStatus : uint8_t { NotTested, Invisible, Visible };

	struct TeammateVisDataEntry {
		float botViewDirDotDirToTeammate;
		float distanceToBot;
		uint8_t playerNum;
		TeammateVisStatus visStatus;
	};

	int64_t m_teammatesVisDataComputedAt { 0 };
	wsw::StaticVector<TeammateVisDataEntry, MAX_CLIENTS> m_teammatesVisData;
	bool m_areAllTeammatesInFov { false };

	struct PendingGuessedEnemyOrigin {
		Vec3 origin;
		int entNum;
	};

	wsw::StaticDeque<PendingGuessedEnemyOrigin, 16> m_guessedEnemyOriginsQueue;
	wsw::RandomGenerator m_rng;

	int64_t m_jumppadUserTrackingTimeoutAt[MAX_CLIENTS] {};

	// The failure chance is specified mainly to throttle excessive plasma spam
	void TryGuessingBeamOwnersOrigins( const EntNumsVector &dangerousEntsNums, float failureChance = 0.0f );
	void TryGuessingProjectileOwnersOrigins( const EntNumsVector &dangerousEntNums, float failureChance = 0.0f );

	void ComputeTeammatesVisData( const vec3_t forwardDir, float fovDotFactor );

	// We introduce a common wrapper superclass for either edict_t or vec3_t
	// to avoid excessive branching in the call below that that leads to unmaintainable code.
	// Virtual calls are not so expensive as one might think (they are predicted on a sane arch).
	struct GuessedEnemy {
		const vec3_t m_origin;
		explicit GuessedEnemy( const float *origin ) : m_origin { origin[0], origin[1], origin[2] } {}
		[[nodiscard]]
		virtual bool AreInPvsWith( const edict_t *botEnt ) const = 0;
	};

	struct GuessedEnemyEntity final: public GuessedEnemy {
		const edict_t *const m_ent;
		explicit GuessedEnemyEntity( const edict_t *ent ) : GuessedEnemy( ent->s.origin ), m_ent( ent ) {}
		bool AreInPvsWith( const edict_t *botEnt ) const override;
	};

	struct GuessedEnemyOrigin final: public EventsTracker::GuessedEnemy {
		mutable int m_leafNums[4] {}, m_numLeafs { 0 };
		explicit GuessedEnemyOrigin( const float *origin ) : GuessedEnemy( origin ) {}
		bool AreInPvsWith( const edict_t *botEnt ) const override;
	};

	bool CanDistinguishGenericEnemySoundsFromTeammates( const edict_t *enemy ) {
		return CanDistinguishGenericEnemySoundsFromTeammates( GuessedEnemyEntity( enemy ) );
	}

	bool CanDistinguishGenericEnemySoundsFromTeammates( const vec3_t specifiedOrigin ) {
		return CanDistinguishGenericEnemySoundsFromTeammates( GuessedEnemyOrigin( specifiedOrigin ) );
	}

	bool CanDistinguishGenericEnemySoundsFromTeammates( const GuessedEnemy &guessedEnemy );

	void AddPendingGuessedEnemyOrigin( const edict_t *enemy, const vec3_t origin );

	void HandlePlayerSexedSoundEvent( const edict_t *player, float distanceThreshold );
	void HandleGenericPlayerEntityEvent( const edict_t *player, float distanceThreshold );
	void HandleEventNoOp( const edict_t *, float ) {}
	void HandleJumppadEvent( const edict_t *player, float );

	// We are not sure what code a switch statement produces.
	// Event handling code is quite performance-sensitive since its is called for each bot for each event.
	// So we set up a lookup table manually.
	typedef void ( EventsTracker::*EventHandler )( const edict_t *, float );
	EventHandler m_eventHandlers[MAX_EVENTS];
	float m_eventHandlingParams[MAX_EVENTS];

	void SetEventHandler( int event, EventHandler handler, float param = 0.0f ) {
		m_eventHandlers[event] = handler;
		m_eventHandlingParams[event] = param;
	}

public:
	explicit EventsTracker( Bot *bot );

	void RegisterEvent( const edict_t *ent, int event, int parm );

	void Update();
};

#endif
