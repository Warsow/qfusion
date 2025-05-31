/*
Copyright (C) 1997-2001 Id Software, Inc.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef __G_GAMETYPE_H__
#define __G_GAMETYPE_H__

#include <cassert>
#include <cstring>
#include <cstdlib>
#include <utility>

#include <common/mmcommon.h>

//g_gametypes.c
extern cvar_t *g_warmup_timelimit;
extern cvar_t *g_postmatch_timelimit;
extern cvar_t *g_countdown_time;
extern cvar_t *g_match_extendedtime;
extern cvar_t *g_votable_gametypes;
extern cvar_t *g_gametype; // only for use in function that deal with changing gametype, use GS_Gametype()
extern cvar_t *g_gametypes_list;


#define G_CHALLENGERS_MIN_JOINTEAM_MAPTIME  9000 // must wait 10 seconds before joining
#define GAMETYPE_PROJECT_EXTENSION          ".gt"
#define CHAR_GAMETYPE_SEPARATOR             ';'

#define MAX_RACE_CHECKPOINTS    32

struct LoggedAward {
	const wsw::StringView name;
	int count;

	explicit LoggedAward( const wsw::StringView &name_, int count_ = 1 )
		: name( name_ ), count( count_ ) {}
};

struct LoggedFrag {
	/**
	 * A session id of an attacker. May be {@code Uuid_ZeroUuid()} if an attacker is missing.
	 */
	const mm_uuid_t attacker;
	/**
	 * A session id of a victim (must be always valid)
	 */
	const mm_uuid_t victim;
	/**
	 * A time in millis since the match start
	 */
	const int time;
	/**
	 * An index of a weapon used, zero if none. There's no distinction between weak and strong ammo.
	 */
	const int weapon;

	LoggedFrag( const mm_uuid_t &attacker_, const mm_uuid_t &victim_, int time_, int weapon_ )
		: attacker( attacker_ ), victim( victim_ ), time( time_ ), weapon( weapon_ ){
		assert( weapon >= 0 && weapon < WEAP_TOTAL );
	}
};

struct alignas( 8 )RaceRun {
	/**
	 * A nickname is a fallback identifier for race runs.
	 * (Playing race at ranked servers being anonymous is allowed and stats are collected as well for these players).
	 * A nickname is empty if there' s a valid session id.
	 */
	char nickname[32] { '\0' };
	/**
	 * A session id of a player (that prefers using the login system).
	 */
	mm_uuid_t clientSessionId;
	/**
	 * A real-world timestamp that corresponds to the run completion moment.
	 */
	int64_t utcTimestamp { 0 };
	/**
	 * A number of sectors to complete in the run.
	 * The range [0, numSectors) defines valid sector values for {@code StatsowFacade::SetSectorTime()}
	 */
	const int numSectors;
	/**
	 * An array of sector times that is allocated within this object
	 * and is capable to store {@code numSectors + 1} elements (the last one is for the final time).
	 */
	uint32_t *const times;

	/**
	 * Creates a new run object given an external storage for sector times.
	 * @param client_ a client the run should belong to
	 * @param numSectors_ a fixed number of sectors to complete
	 * @param times_ a storage for sector times capable of storing {@code numSectors + 1} elements
	 * @note it is assumed that the times array is allocated within
	 * this run object and thus does not need its own memory management.
	 */
	RaceRun( const struct Client *client_, int numSectors_, uint32_t *times_ );

	void SaveNickname( const struct Client *client );
};

class QueryObject;
class StatsowFacade;

namespace wsw {
template <typename T> auto link( T*, T** ) -> T *;
template <typename T> auto unlink( T*, T** ) -> T *;
}

class RunStatusQuery {
	friend class StatsowFacade;

	template <typename T> friend auto wsw::link( T*, T** ) -> T *;
	template <typename T> friend auto wsw::unlink( T*, T** ) -> T *;

	const int64_t createdAt;
	int64_t nextRetryAt { 0 };
	RunStatusQuery *prev { nullptr };
	RunStatusQuery *next { nullptr };
	StatsowFacade *const parent;
	QueryObject *query;
	// For debugging purposes
	const mm_uuid_t runId;
	int outcome { 0 };
	int worldRank { -1 };
	int personalRank { -1 };

	RunStatusQuery( StatsowFacade *parent_, QueryObject *query_, const mm_uuid_t &runId_ );

	~RunStatusQuery();

	void CheckReadyForAccess( const char *tag ) const;
	void CheckValidForAccess( const char *tag ) const;

	void Update( int64_t millisNow );

	void ScheduleRetry( int64_t millisNow );

	int GetQueryField( const char *fieldName );
public:
	void DeleteSelf();

	bool IsReady() const {
		return outcome != 0;
	}

	bool HasFailed() const {
		CheckReadyForAccess( "RunStatusQuery::HasFailed()" );
		return outcome < 0;
	}

	int WorldRank() const {
		CheckValidForAccess( "RunStatusQuery::WorldRank()" );
		return worldRank;
	}

	int PersonalRank() const {
		CheckValidForAccess( "RunStatusQuery::PersonalRank()" );
		return personalRank;
	}
};

class GVariousStats {
	struct Node {
		Node *nextInList;
		Node *nextInBin;
		char *key;
		int64_t value;
		uint32_t keyHash;
		uint32_t keyLength;
	};

	Node *listHead;
	// Initialize bins lazily since some objects are global and are constructed before game imports are set up.
	mutable Node **bins;
	unsigned numHashBins;

	const Node *GetNode( unsigned binIndex, const char *key, uint32_t hash, uint32_t length ) const;
	void LinkNewNode( unsigned binIndex, const char *key, uint32_t hash, uint32_t length, int64_t value );
protected:
	void MoveFields( GVariousStats &&that ) {
		this->numHashBins = that.numHashBins;
		this->bins = that.bins, that.bins = nullptr;
		this->listHead = that.listHead, that.listHead = nullptr;
	}
public:
	explicit GVariousStats( unsigned numHashBins_ )
		: listHead( nullptr ), bins( nullptr ), numHashBins( numHashBins_ ) {}

	~GVariousStats();

	GVariousStats( const GVariousStats & ) = delete;
	GVariousStats &operator=( const GVariousStats & ) = delete;

	GVariousStats( GVariousStats &&that ) {
		MoveFields( std::move( that ) );
	}

	GVariousStats &operator=( GVariousStats &&that ) {
		Clear();
		MoveFields( std::move( that ) );
		return *this;
	}

	void Clear();

	int64_t GetEntry( const char *key, int64_t defaultValue = 0 ) const;
	void SetEntry( const char *key, int64_t value );
	void AddToEntry( const char *key, int64_t delta );

	template<typename T>
	inline void AddToEntry( const std::pair<const char *, T> &keyAndValue ) {
		AddToEntry( keyAndValue.first, keyAndValue.second );
	}

	class const_iterator {
		friend class GVariousStats;

		const Node *currNode;
		explicit const_iterator( const Node *node ): currNode( node ) {}
	public:
		bool operator==( const const_iterator &that ) const {
			return currNode == that.currNode;
		}

		bool operator!=( const const_iterator &that ) const { return !( *this == that ); }

		const_iterator &operator++() {
			currNode = currNode->nextInList;
			return *this;
		}

		const_iterator operator++(int) {
			const_iterator result( currNode );
			currNode = currNode->nextInList;
			return result;
		}

		const std::pair<const char *, int64_t> operator*() const {
			return std::make_pair( currNode->key, currNode->value );
		};
	};

	const_iterator begin() const { return const_iterator( listHead ); }
	const_iterator end() const { return const_iterator( nullptr ); }
};

template <typename T, size_t N> inline void MoveArray( T ( &dest )[N], T ( &src )[N] ) {
	memcpy( dest, src, N * sizeof( T ) );
	memset( src, 0, N * sizeof( T ) );
}

void *Q_malloc( size_t size );
void *Q_realloc( void *buf, size_t newsize );
void Q_free( void *buf );
char *Q_strdup( const char *str );

template <typename T>
class StatsSequence {
	struct Chunk {
		Chunk *next { nullptr };
		unsigned numItems { 0 };
		/**
		 * Keep this value instead of basing on the chunk size of a parent
		 * as chunks could be transferred from other sequence
		 * that does not obligatory has the same chunk size.
		 */
		unsigned itemsLeft { 0 };
		uint8_t *data { nullptr };

		~Chunk() {
			auto *items = (T *)data;
			for( unsigned i = 0; i < numItems; ++i ) {
				items[i].~T();
			}
		}

		bool IsFull() const {
			return !itemsLeft;
		}

		void *UnsafeGrowBack() {
			assert( itemsLeft );
			void *result = data + ( numItems * sizeof( T ) );
			numItems++;
			itemsLeft--;
			return result;
		}
	};

	Chunk *headChunk { nullptr };
	Chunk *tailChunk { nullptr };
	const unsigned elemsPerChunk;
	unsigned totalNumItems { 0 };

	Chunk *AllocChunk() {
		// TODO: Something weird was with operations priority so we have separated statements
		size_t memSize = sizeof( Chunk );
		memSize += ( sizeof( T ) + 8 ) & 7;
		memSize += elemsPerChunk * sizeof( T );
		auto *mem = (uint8_t *)Q_malloc( memSize );
		auto *result = new( mem )Chunk;
		result->itemsLeft = elemsPerChunk;
		result->data = mem;
		result->data += sizeof( Chunk );
		result->data += ( sizeof( T ) + 8 ) & 7;
		return result;
	}

	/**
	 * @warning assumes default field values of `this`.
	 */
	void MoveFieldsFrom( StatsSequence &&that ) noexcept {
		std::swap( this->headChunk, that.headChunk );
		std::swap( this->tailChunk, that.tailChunk );
		std::swap( this->totalNumItems, that.totalNumItems );
	}
public:
	explicit StatsSequence( unsigned elemsPerChunk_ = 32 )
		: elemsPerChunk( elemsPerChunk_ ) {
		// Sanity check
		assert( elemsPerChunk_ >= 1 && elemsPerChunk_ < ( 1u << 16u ) );
	}

	StatsSequence( const StatsSequence & ) = delete;
	StatsSequence &operator=( const StatsSequence & ) = delete;

	StatsSequence( StatsSequence &&that ) noexcept
		: elemsPerChunk( that.elemsPerChunk ) {
		MoveFieldsFrom( std::move( that ) );
	}

	StatsSequence &operator=( StatsSequence &&that ) noexcept {
		Clear();
		MoveFieldsFrom( std::move( that ) );
		return *this;
	}

	~StatsSequence() {
		Clear();
	}

	void Clear() {
		Chunk *nextChunk;
		for( Chunk *chunk = headChunk; chunk; chunk = nextChunk ) {
			nextChunk = chunk->next;
			chunk->~Chunk();
			Q_free( chunk );
		}
		headChunk = nullptr;
		tailChunk = nullptr;
		totalNumItems = 0;
	}

	/**
	 * Provided for STL structural compatibility.
	 */
	unsigned size() const { return totalNumItems; }
	/**
	 * Provided for STL structural compatibility.
	 */
	bool empty() const { return !totalNumItems; };

	template <typename... Args>
	T *New( Args... args ) {
		if( !tailChunk ) {
			headChunk = tailChunk = AllocChunk();
		} else if( tailChunk->IsFull() ) {
			Chunk *chunk = AllocChunk();
			tailChunk->next = chunk;
			tailChunk = chunk;
		}
		totalNumItems++;
		return new( tailChunk->UnsafeGrowBack() )T( args ... );
	}

	void MergeWith( StatsSequence<T> &&that ) noexcept {
		if( !tailChunk ) {
			this->headChunk = that.headChunk;
			this->tailChunk = that.tailChunk;
			this->totalNumItems = that.totalNumItems;
		} else {
			this->tailChunk->next = that.headChunk;
			this->tailChunk = that.tailChunk;
			this->totalNumItems += that.totalNumItems;
		}
		that.totalNumItems = 0;
		that.headChunk = that.tailChunk = nullptr;
	}

	template <typename Impl>
	class BaseIterator {
	protected:
		Chunk *currChunk;
		unsigned currElem;

		BaseIterator( Chunk *currChunk_, unsigned currElem_ )
			: currChunk( currChunk_ ), currElem( currElem_ ) {}
	public:
		bool operator==( const Impl &that ) const {
			return currChunk == that.currChunk && currElem == that.currElem;
		}

		bool operator!=( const Impl &that ) const {
			return !( *this == that );
		}

		Impl &operator++() {
			assert( currChunk );
			if( currElem + 1 < currChunk->numItems ) {
				currElem++;
				return static_cast<Impl &>( *this );
			}
			currChunk = currChunk->next;
			currElem = 0;
			return static_cast<Impl &>( *this );
		}
	};

	class iterator : public BaseIterator<iterator> {
		template <typename> friend class StatsSequence;

		iterator( Chunk *currChunk_, unsigned currElem_ )
			: BaseIterator<iterator>( currChunk_, currElem_ ) {}
	public:
		const T &operator *() const {
			assert( this->currChunk );
			return *( (T *)this->currChunk->data + this->currElem );
		}

		T &operator *() {
			assert( this->currChunk );
			return *( (T *)this->currChunk->data + this->currElem );
		}
	};

	class const_iterator : public BaseIterator<const_iterator> {
		template <typename> friend class StatsSequence;

		const_iterator( Chunk *currChunk_, unsigned currElem_ )
			: BaseIterator<const_iterator>( currChunk_, currElem_ ) {}

	public:
		const T &operator *() const {
			assert( this->currChunk );
			return *( (T *)this->currChunk->data + this->currElem );
		}
	};

	/**
	 * Provided for STL structural compatibility.
	 */
	const_iterator cbegin() const {
		return { headChunk, 0 };
	}
	/**
	 * Provided for STL structural compatibility.
	 */
	const_iterator cend() const {
		return { nullptr, 0 };
	}
	/**
	 * Provided for STL structural compatibility.
	 */
	const_iterator begin() const { return cbegin(); }
	/**
	 * Provided for STL structural compatibility.
	 */
	const_iterator end() const { return cend(); }
	/**
	 * Provided for STL structural compatibility.
	 */
	iterator begin() { return { headChunk, 0 }; }
	/**
	 * Provided for STL structural compatibility.
	 */
	iterator end() { return { nullptr, 0 }; }
};

typedef struct score_stats_s: public GVariousStats {
	score_stats_s(): GVariousStats( 271 ) {
		Clear();
	}

	int score;
	int awards;

	void Clear() {
		GVariousStats::Clear();

		score = 0;
		awards = 0;

		memset( accuracy_shots, 0, sizeof( accuracy_shots ) );
		memset( accuracy_hits, 0, sizeof( accuracy_hits ) );
		memset( accuracy_hits_direct, 0, sizeof( accuracy_hits_direct ) );
		memset( accuracy_hits_air, 0, sizeof( accuracy_hits_air ) );
		memset( accuracy_damage, 0, sizeof( accuracy_damage ) );
		memset( accuracy_frags, 0, sizeof( accuracy_frags ) );

		had_playtime = false;

		awardsSequence.Clear();
	}

	// These getters serve an utilty. We might think of precomputing handles (key/length)
	// for strings and use ones instead of plain string for faster access.

	void AddDeath() { AddToEntry( "deaths", 1 ); }
	void AddFrag() { AddToEntry( "frags", 1 ); }
	void AddSuicide() { AddToEntry( "suicides", 1 ); }
	void AddTeamFrag() { AddToEntry( "team_frags", 1 ); }
	void AddRound() { AddToEntry( "rounds", 1 ); }

	int accuracy_shots[AMMO_TOTAL - AMMO_GUNBLADE];
	int accuracy_hits[AMMO_TOTAL - AMMO_GUNBLADE];
	int accuracy_hits_direct[AMMO_TOTAL - AMMO_GUNBLADE];
	int accuracy_hits_air[AMMO_TOTAL - AMMO_GUNBLADE];
	int accuracy_damage[AMMO_TOTAL - AMMO_GUNBLADE];
	int accuracy_frags[AMMO_TOTAL - AMMO_GUNBLADE];

	void AddDamageGiven( float damage ) {
		AddToEntry( "damage_given", (int)damage );
	}
	void AddDamageTaken( float damage ) {
		AddToEntry( "damage_taken", (int)damage );
	}
	void AddTeamDamageGiven( float damage ) {
		AddToEntry( "team_damage_given", (int)damage );
	}
	void AddTeamDamageTaken( float damage ) {
		AddToEntry( "team_damage_taken", (int)damage );
	}

	bool had_playtime;

	StatsSequence<LoggedAward> awardsSequence;

	struct RaceRun *currentRun;

	score_stats_s( const score_stats_s &that ) = delete;
	score_stats_s &operator=( const score_stats_s &that ) = delete;

	score_stats_s( score_stats_s &&that )
		: GVariousStats( std::move( that ) ) {
		MoveFields( std::move( that ) );
	}

	score_stats_s &operator=( score_stats_s &&that ) {
		Clear();
		MoveFields( std::move( that ) );
		return *this;
	}
private:
	void MoveFields( score_stats_s &&that ) {
		GVariousStats::MoveFields( std::move( that ) );

		this->score = that.score, that.score = 0;
		this->awards = that.awards, that.awards = 0;

		MoveArray( this->accuracy_shots, that.accuracy_shots );
		MoveArray( this->accuracy_hits, that.accuracy_hits );
		MoveArray( this->accuracy_hits_direct, that.accuracy_hits_direct );
		MoveArray( this->accuracy_hits_air, that.accuracy_hits_air );
		MoveArray( this->accuracy_damage, that.accuracy_damage );
		MoveArray( this->accuracy_frags, that.accuracy_frags );

		this->had_playtime = that.had_playtime, that.had_playtime = false;

		this->currentRun = that.currentRun;
		that.currentRun = nullptr;

		this->awardsSequence = std::move( that.awardsSequence );
	}
} score_stats_t;

// this is only really used to create the script objects
typedef struct {
	bool dummy;
} match_t;

typedef struct {
	match_t match;

	void *initFunc;
	void *spawnFunc;
	void *matchStateStartedFunc;
	void *matchStateFinishedFunc;
	void *thinkRulesFunc;
	void *playerRespawnFunc;
	void *scoreEventFunc;
	void *updateScoreboardFunc;
	void *selectSpawnPointFunc;
	void *clientCommandFunc;
	void *shutdownFunc;

	int spawnableItemsMask;
	int respawnableItemsMask;
	int dropableItemsMask;
	int pickableItemsMask;

	bool isTeamBased;
	bool isRace;
	bool isTutorial;
	bool inverseScore;
	bool hasChallengersQueue;
	bool hasChallengersRoulette;
	int maxPlayersPerTeam;

	// default item respawn time
	int ammo_respawn;
	int armor_respawn;
	int weapon_respawn;
	int health_respawn;
	int powerup_respawn;
	int megahealth_respawn;
	int ultrahealth_respawn;

	// few default settings
	bool readyAnnouncementEnabled;
	bool scoreAnnouncementEnabled;
	bool countdownEnabled;
	bool matchAbortDisabled;
	bool shootingDisabled;
	bool infiniteAmmo;
	bool canForceModels;
	bool canShowMinimap;
	bool teamOnlyMinimap;
	bool customDeadBodyCam;
	bool removeInactivePlayers;
	bool disableObituaries;

	int spawnpointRadius;

	bool mmCompatible;

	int numBots;
	bool dummyBots;

	int forceTeamHumans;
	int forceTeamBots;
} gametype_descriptor_t;

typedef struct g_teamlist_s {
	int playerIndices[MAX_CLIENTS];
	int numplayers;
	score_stats_t stats;
	int ping;
	bool locked;
	int invited[MAX_CLIENTS];
	bool has_coach;

	void Clear() {
		memset( playerIndices, 0, sizeof( playerIndices ) );
		numplayers = 0;
		stats.Clear();
		ping = 0;
		locked = false;;
		memset( invited, 0, sizeof( invited ) );
		has_coach = false;
	}
} g_teamlist_t;

extern g_teamlist_t teamlist[GS_MAX_TEAMS];

//clock
extern char clockstring[16];

//
//	matches management
//
bool G_Match_Tied( void );
bool G_Match_CheckExtendPlayTime( void );
void G_Match_RemoveProjectiles( edict_t *owner );
void G_Match_CleanUpPlayerStats( edict_t *ent );
void G_Match_FreeBodyQueue( void );
void G_Match_LaunchState( int matchState );

//
//	teams
//
void G_Teams_Init( void );

void G_Teams_ExecuteChallengersQueue( void );
void G_Teams_AdvanceChallengersQueue( void );

void G_Match_Autorecord_Start( void );
void G_Match_Autorecord_AltStart( void );
void G_Match_Autorecord_Stop( void );
void G_Match_Autorecord_Cancel( void );
bool G_Match_ScorelimitHit( void );
bool G_Match_SuddenDeathFinished( void );
bool G_Match_TimelimitHit( void );

//coach
void G_Teams_Coach( edict_t *ent, const CmdArgs & );
void G_Teams_CoachLockTeam( edict_t *ent, const CmdArgs & );
void G_Teams_CoachUnLockTeam( edict_t *ent, const CmdArgs & );

bool G_Gametype_Exists( const char *name );

#endif //  __G_GAMETYPE_H__
