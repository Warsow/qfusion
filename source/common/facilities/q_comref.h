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

#ifndef GAME_QCOMREF_H
#define GAME_QCOMREF_H

#include <common/helpers/q_arch.h>

//
// button bits
//
#define BUTTON_ATTACK               1
#define BUTTON_WALK                 2
#define BUTTON_SPECIAL              4
#define BUTTON_USE                  8
#define BUTTON_ZOOM                 16
#define BUTTON_BUSYICON             32
#define BUTTON_ANY                  128     // any key whatsoever

enum {
	KEYICON_FORWARD = 0,
	KEYICON_BACKWARD,
	KEYICON_LEFT,
	KEYICON_RIGHT,
	KEYICON_FIRE,
	KEYICON_JUMP,
	KEYICON_CROUCH,
	KEYICON_SPECIAL,
	KEYICON_TOTAL
};

// user command communications
#define CMD_BACKUP  64  // allow a lot of command backups for very fast systems
#define CMD_MASK    ( CMD_BACKUP - 1 )

// usercmd_t is sent to the server each client frame
typedef struct usercmd_s {
	uint8_t msec;
	uint32_t buttons;
	int64_t serverTimeStamp;
	int16_t angles[3];
	int8_t forwardmove, sidemove, upmove;
} usercmd_t;

#define MAX_PM_STATS 16

enum {
	PM_STAT_FEATURES,
	PM_STAT_NOUSERCONTROL,
	PM_STAT_KNOCKBACK,
	PM_STAT_CROUCHTIME,
	PM_STAT_ZOOMTIME,
	PM_STAT_DASHTIME,
	PM_STAT_WJTIME,
	PM_STAT_NOAUTOATTACK,
	PM_STAT_STUN,
	PM_STAT_MAXSPEED,
	PM_STAT_JUMPSPEED,
	PM_STAT_DASHSPEED,
	PM_STAT_FWDTIME,
	PM_STAT_CROUCHSLIDETIME,

	PM_STAT_SIZE = MAX_PM_STATS
};

// pmove_state_t is the information necessary for client side movement
// prediction
typedef enum {
	// can accelerate and turn
	PM_NORMAL,
	PM_SPECTATOR,

	// no acceleration or turning
	PM_GIB,         // different bounding box
	PM_FREEZE,
	PM_CHASECAM     // same as freeze, but so client knows it's in chasecam
} pmtype_t;

// pmove->pm_flags
#define PMF_WALLJUMPCOUNT   ( 1 << 0 )
#define PMF_JUMP_HELD       ( 1 << 1 )
#define PMF_ON_GROUND       ( 1 << 2 )
#define PMF_TIME_WATERJUMP  ( 1 << 3 )   // pm_time is waterjump
#define PMF_TIME_LAND       ( 1 << 4 )  // pm_time is time before rejump
#define PMF_TIME_TELEPORT   ( 1 << 5 )  // pm_time is non-moving time
#define PMF_NO_PREDICTION   ( 1 << 6 )  // temporarily disables prediction (used for grappling hook)
#define PMF_DASHING         ( 1 << 7 ) // Dashing flag
#define PMF_SPECIAL_HELD    ( 1 << 8 ) // Special flag
#define PMF_WALLJUMPING     ( 1 << 9 ) // WJ starting flag
#define PMF_DOUBLEJUMPED    ( 1 << 10 ) // DJ stat flag
#define PMF_JUMPPAD_TIME    ( 1 << 11 )    // temporarily disables fall damage
#define PMF_CROUCH_SLIDING  ( 1 << 12 )    // Crouch slide flag

typedef struct {
	int pm_type;

	float origin[3];
	float velocity[3];

	int pm_flags;               // ducked, jump_held, etc
	int pm_time;                // each unit = 8 ms
	int unused;                 // kept for 2.5 netcode compatibility
	short stats[PM_STAT_SIZE];  // Kurim : timers for knockback, stun, doublejump, walljump
	int gravity;
	short delta_angles[3];      // add to command angles to get view direction
	                            // changed by spawns, rotating objects, and teleporters
} pmove_state_t;

#define MAXTOUCH    32

#define PM_CROUCHSLIDE 1500
#define PM_CROUCHSLIDE_FADE 500


//==========================================================
//
//  ELEMENTS COMMUNICATED ACROSS THE NET
//
//==========================================================


// note that Q_rint was causing problems here
// (spawn looking straight up\down at delta_angles wrapping)

#define ANGLE2SHORT( x )    ( (int)( ( x ) * 65536 / 360 ) & 65535 )
#define SHORT2ANGLE( x )    ( ( x ) * ( 360.0 / 65536 ) )

#define ANGLE2BYTE( x )     ( (int)( ( x ) * 256 / 360 ) & 255 )
#define BYTE2ANGLE( x )     ( ( x ) * ( 360.0 / 256 ) )

#define MAX_GAMECOMMANDS    64     // command names for command completion
#define MAX_WEAPONDEFS      MAX_ITEMS
#define MAX_HELPMESSAGES    64

//
// config strings are a general means of communication from
// the server to all connected clients.
// Each config string can be at most MAX_QPATH characters.
//
#define CS_HOSTNAME         0
#define CS_MAXCLIENTS       2
#define CS_MODMANIFEST      3

#define SERVER_PROTECTED_CONFIGSTRINGS 5

#define CS_MESSAGE          5
#define CS_MAPNAME          6
#define CS_AUDIOTRACK       7
#define CS_SKYBOX           8
#define CS_STATNUMS         9
#define CS_POWERUPEFFECTS   10
#define CS_GAMETYPETITLE    11
#define CS_GAMETYPENAME     12
#define CS_GAMETYPEVERSION  13
#define CS_GAMETYPEAUTHOR   14
#define CS_AUTORECORDSTATE  15

#define CS_SCOREBOARD_ASSETS 16
#define CS_SCOREBOARD_SCHEMA 17

#define CS_ACTIVE_CALLVOTE  18
#define CS_CALLVOTE_GROUPS  19

#define CS_TEAM_SPECTATOR_NAME  20
#define CS_TEAM_PLAYERS_NAME    21
#define CS_TEAM_ALPHA_NAME      22
#define CS_TEAM_BETA_NAME       23

#define CS_MATCHNAME        26
#define CS_MATCHSCORE       27
#define CS_MATCHUUID        28

#define CS_GAMETYPE_OPTIONS_TITLE 29

#define CS_WORLDMODEL       30
#define CS_MAPCHECKSUM      31      // for catching cheater maps

//precache stuff begins here
#define CS_MODELS           32
#define CS_SOUNDS           ( CS_MODELS + MAX_MODELS )
#define CS_IMAGES           ( CS_SOUNDS + MAX_SOUNDS )
#define CS_SKINFILES        ( CS_IMAGES + MAX_IMAGES )
#define CS_LIGHTS           ( CS_SKINFILES + MAX_SKINFILES )
#define CS_ITEMS            ( CS_LIGHTS + MAX_LIGHTSTYLES )
#define CS_PLAYERINFOS      ( CS_ITEMS + MAX_ITEMS )
#define CS_GAMECOMMANDS     ( CS_PLAYERINFOS + MAX_CLIENTS )
#define CS_WEAPONDEFS       ( CS_GAMECOMMANDS + MAX_GAMECOMMANDS )
#define CS_GENERAL          ( CS_WEAPONDEFS + MAX_WEAPONDEFS )
#define CS_CALLVOTEINFOS    ( CS_GENERAL + MAX_GENERAL )
#define CS_GAMETYPE_OPTIONS ( CS_CALLVOTEINFOS + MAX_CALLVOTEINFOS )
#define CS_MMPLAYERINFOS    ( CS_GAMETYPE_OPTIONS + MAX_GAMETYPE_OPTIONS )
#define CS_HELPMESSAGES     ( CS_MMPLAYERINFOS + MAX_MMPLAYERINFOS ) // for localizable messages, that got a special place on the HUD

#define MAX_CONFIGSTRINGS   ( CS_HELPMESSAGES + MAX_HELPMESSAGES )

#ifdef Status
#undef Status
#endif

namespace wsw {

class ScoreboardShared {
protected:
	enum ColumnKind {
		Nickname,
		Clan,
		Score,
		Status,
		Ping,
		Number,
		Glyph,
		Icon,
	};

	// We prefer declaring these as plain constants due to name clash with Q_ENUM in the client scoreboard
	static constexpr unsigned kPowerupBitQuad  = 0x1;
	static constexpr unsigned kPowerupBitShell = 0x2;
	static constexpr unsigned kPowerupBitRegen = 0x4;

	// Don't imply that player indices are client numbers (they have the same range but are sorted by score)
	static constexpr unsigned kMaxPlayers = MAX_CLIENTS;
	static constexpr unsigned kMaxColumns = 10;
	static constexpr unsigned kMaxTitleLen = 12;
	static constexpr unsigned kTitleDataLimit = 64 + kMaxColumns;
	static constexpr unsigned kMaxAssets = 16;
	static constexpr unsigned kAssetDataLimit = 256 + kMaxAssets;
	static constexpr unsigned kMaxShortSlots = 7;
};

}

//==============================================

// infoservers cvar is shared by client and server. This ensures both have the same default string
#ifndef PUBLIC_BUILD
#define DEFAULT_INFO_SERVERS_IPS          "warsow.net dpmaster.deathmask.net"
#else
#define DEFAULT_INFO_SERVERS_IPS          "warsow.net"
#endif

#define DEFAULT_PLAYERMODEL                 "viciious"
#define DEFAULT_TEAMPLAYERS_MODEL          "bobot"
#define DEFAULT_TEAMALPHA_MODEL            "bigvic"
#define DEFAULT_TEAMBETA_MODEL             "padpork"

#define DEFAULT_PLAYERSKIN                  "default"

// entity_state_t is the information conveyed from the server
// in an update message about entities that the client will
// need to render in some way

// edict->svflags
#define SVF_NOCLIENT            0x00000001      // don't send entity to clients, even if it has effects
#define SVF_PORTAL              0x00000002      // merge PVS at old_origin
#define SVF_TRANSMITORIGIN2     0x00000008      // always send old_origin (beams, etc)
#define SVF_SOUNDCULL           0x00000010      // distance culling
#define SVF_FAKECLIENT          0x00000020      // do not try to send anything to this client
#define SVF_BROADCAST           0x00000040      // always transmit
#define SVF_CORPSE              0x00000080      // treat as CONTENTS_CORPSE for collision
#define SVF_PROJECTILE          0x00000100      // sets s.solid to SOLID_NOT for prediction
#define SVF_ONLYTEAM            0x00000200      // this entity is only transmited to clients with the same ent->s.team value
#define SVF_FORCEOWNER          0x00000400      // this entity forces the entity at s.ownerNum to be included in the snapshot
#define SVF_ONLYOWNER           0x00000800      // this entity is only transmitted to its owner
#define SVF_FORCETEAM           0x00001000      // this entity is always transmitted to clients with the same ent->s.team value

// edict->solid values
typedef enum {
	SOLID_NOT,              // no interaction with other objects
	SOLID_TRIGGER,          // only touch when inside, after moving
	SOLID_YES               // touch on edge
} solid_t;

#define SOLID_BMODEL    31  // special value for bmodel

// entity_state_t->event values
// entity events are for effects that take place relative
// to an existing entities origin. Very network efficient.

#define EVENT_ENTITIES_START    96 // entity types above this index will get event treatment
#define ISEVENTENTITY( x ) ( ( (entity_state_t *)x )->type >= EVENT_ENTITIES_START )

//==============================================

// primitive encoding types for network messages

typedef enum {
	WIRE_BOOL,					// a of value of 'true' is represented by a single bit in the header

	WIRE_FIXED_INT8,				// 8-bit integer
	WIRE_FIXED_INT16,			// 16-bit integer
	WIRE_FIXED_INT32,			// 32-bit integer
	WIRE_FIXED_INT64,			// 64-bit integer

	WIRE_FLOAT,					// 32-bit floating point value
	WIRE_HALF_FLOAT,				// 16-bit floating point value

	WIRE_ANGLE,					// 32-bit float angle value, normalized to [0..360], transmitted at half-precision

	WIRE_BASE128,				// base-128 encoded unsigned integer
	WIRE_UBASE128				// base-128 encoded signed integer
} wireType_t;

//==============================================

typedef struct entity_state_s {
	int number;                         // edict index

	unsigned int svflags;

	int type;                           // ET_GENERIC, ET_BEAM, etc

	// for client side prediction, 8*(bits 0-4) is x/y radius
	// 8*(bits 5-9) is z down distance, 8(bits10-15) is z up
	// GClip_LinkEntity sets this properly
	int solid;

	vec3_t origin;
	vec3_t angles;
	vec3_t origin2;                 // ET_BEAM, ET_PORTALSURFACE, ET_EVENT specific

	unsigned int modelindex;
	unsigned int modelindex2;

	int bodyOwner;                  // ET_PLAYER specific, for dead bodies
	int channel;                    // ET_SOUNDEVENT

	int frame;
	int ownerNum;                   // ET_EVENT specific

	unsigned int effects;

	// impulse events -- muzzle flashes, footsteps, etc
	// events only go out for a single frame, they
	// are automatically cleared each frame
	int events[2];
	int eventParms[2];

	int counterNum;                 // ET_GENERIC
	int skinnum;                    // for ET_PLAYER
	int itemNum;                    // for ET_ITEM
	int firemode;                   // for weapon events
	int damage;                     // EV_BLOOD
	int targetNum;                  // ET_EVENT specific
	int colorRGBA;                  // ET_BEAM, ET_EVENT specific
	int range;                      // ET_LASERBEAM, ET_CURVELASERBEAM specific

	bool linearMovement;
	vec3_t linearMovementVelocity;      // this is transmitted instead of origin when linearProjectile is true
	vec3_t linearMovementEnd;           // the end movement point for brush models
	vec3_t linearMovementBegin;			// the starting movement point for brush models
	unsigned int linearMovementDuration;
	int64_t linearMovementTimeStamp;
	int64_t linearMovementPrevServerTime;

	float attenuation;                  // should be <= 255/16.0 as this is sent as byte

	// server will use this for sound culling in case
	// the entity has an event attached to it (along with
	// PVS culling)

	int weapon;                         // WEAP_ for players
	bool teleported;

	int sound;                          // for looping sounds, to guarantee shutoff

	int light;							// constant light glow

	int team;                           // team in the game
} entity_state_t;

//==============================================

typedef enum {
	CA_UNINITIALIZED,
	CA_DISCONNECTED,                    // not talking to a server
	CA_GETTING_TICKET,                  // getting a session ticket for matchmaking
	CA_CONNECTING,                      // sending request packets to the server
	CA_HANDSHAKE,                       // netchan_t established, waiting for svc_serverdata
	CA_CONNECTED,                       // connection established, game module not loaded
	CA_LOADING,                         // loading game module
	CA_ACTIVE,                          // game views should be displayed
} connstate_t;

enum class ReconnectBehaviour : unsigned {
	DontReconnect = 1,
	Autoreconnect,
	OfUserChoice,
	RequestPassword,
};

// TODO: We need supporting this at language level
constexpr ReconnectBehaviour kReconnectBehaviourValues[] = {
	ReconnectBehaviour::DontReconnect, ReconnectBehaviour::Autoreconnect,
	ReconnectBehaviour::OfUserChoice, ReconnectBehaviour::RequestPassword
};

enum class ConnectionDropStage : unsigned {
	EstablishingFailed = 1,
	FunctioningError,
	TerminatedByServer,
};

constexpr ConnectionDropStage kConnectionDropStageValues[] = {
	ConnectionDropStage::EstablishingFailed, ConnectionDropStage::FunctioningError, ConnectionDropStage::TerminatedByServer
};

typedef enum {
	MM_LOGIN_STATE_LOGGED_OUT,
	MM_LOGIN_STATE_IN_PROGRESS,
	MM_LOGIN_STATE_LOGGED_IN
} mmstate_t;

typedef enum {
	DOWNLOADTYPE_NONE,
	DOWNLOADTYPE_SERVER,
	DOWNLOADTYPE_WEB
} downloadtype_t;

//==============================================

typedef enum {
	HTTP_METHOD_BAD = -1,
	HTTP_METHOD_NONE = 0,
	HTTP_METHOD_GET  = 1,
	HTTP_METHOD_POST = 2,
	HTTP_METHOD_PUT  = 3,
	HTTP_METHOD_HEAD = 4,
} http_query_method_t;

typedef enum {
	HTTP_RESP_NONE = 0,
	HTTP_RESP_OK = 200,
	HTTP_RESP_PARTIAL_CONTENT = 206,
	HTTP_RESP_BAD_REQUEST = 400,
	HTTP_RESP_FORBIDDEN = 403,
	HTTP_RESP_NOT_FOUND = 404,
	HTTP_RESP_REQUEST_TOO_LARGE = 413,
	HTTP_RESP_REQUESTED_RANGE_NOT_SATISFIABLE = 416,
	HTTP_RESP_SERVICE_UNAVAILABLE = 503,
} http_response_code_t;

struct MessageFault {
	enum Kind { Muted = 1, Flood };
	static constexpr auto kMinKind = Muted;
	static constexpr auto kMaxKind = Flood;
	const uint64_t clientCommandNum;
	const Kind kind;
	const unsigned timeout;
};

//==============================================

#define MAX_GAME_STATS  64

typedef struct {
	int64_t stats[MAX_GAME_STATS];
} game_state_t;

#include <optional>

struct ReplicatedScoreboardData final : public wsw::ScoreboardShared {
private:
	[[nodiscard]]
	static auto packWord( unsigned currentPacked, unsigned word, unsigned mask, unsigned shift ) -> unsigned {
		assert( !( mask & ( mask + 1 ) ) );
		assert( shift < 32u );
		assert( !( word & ~mask ) );
		word = word << shift;
		mask = mask << shift;
		return ( currentPacked & ~mask ) | word;
	}

	[[nodiscard]]
	static auto unpackWord( unsigned packed, unsigned mask, unsigned shift ) -> unsigned {
		assert( !( mask & ( mask + 1 ) ) );
		assert( shift < 32u );
		return ( packed >> shift ) & mask;
	}

	// Player num occupies first 5 bits
	static constexpr unsigned kPlayerNumShift = 0u;
	static constexpr unsigned kPlayerNumMask = ( ( 1u << 5u ) - 1u );
	// Health starts from the 8th bit and occupies 10 bits
	static constexpr unsigned kHealthShift = 5u;
	static constexpr unsigned kHealthMask = ( ( 1u << 10u ) - 1u );
	// Weapon occupies 4 next bits to the left
	static constexpr unsigned kWeaponShift = kHealthShift + 10u;
	static constexpr unsigned kWeaponMask = ( ( 1u << 4u ) - 1u );
	// Armor occupies 10 next bits to the left
	static constexpr unsigned kArmorShift = kWeaponShift + 4u;
	static constexpr unsigned kArmorMask = ( ( 1u << 10u ) - 1u );
	// Powerup statuses occupy leftmost 3 bits
	static constexpr unsigned kPowerupsShift = kArmorShift + 10u;
	static constexpr unsigned kPowerupsMask = ( ( 1u << 3u ) - 1u );
	static_assert( kPowerupsShift + 3u <= 32u );

	void setPlayerFlagBit( unsigned playerIndex, unsigned flagBit, bool set ) {
		assert( (unsigned)playerIndex < (unsigned)kMaxPlayers );
		assert( flagBit == 1 || flagBit == 2 );
		const uint64_t bitMask = ( (uint64_t)flagBit ) << ( 2 * playerIndex );
		if( set ) {
			playersFlagsMask |= bitMask;
		} else {
			playersFlagsMask &= ~bitMask;
		}
	}

	[[nodiscard]]
	bool testPlayerFlagBit( unsigned playerIndex, unsigned flagBit ) const {
		assert( (unsigned)playerIndex < (unsigned)kMaxPlayers );
		assert( flagBit == 1 || flagBit == 2 );
		return ( ( playersFlagsMask >> ( 2 * playerIndex ) ) & flagBit ) != 0;
	}

	static constexpr unsigned kFlagBitConnected = 0x1;
	static constexpr unsigned kFlagBitGhosting  = 0x2;
public:
	// Transmitted separately for historical reason, and lately - for reasons described in the locations remark.
	uint64_t playersTeamMask;
	// See locations remark
	uint64_t playersFlagsMask;
	int alphaScore;
	int betaScore;
	int scores[kMaxPlayers];
	short values[kMaxShortSlots][kMaxPlayers];
	uint32_t packedPlayerSpecificData[kMaxPlayers];
	// The challengers queue. Entity numbers are written (this means valid entries are non-zero and start from 1).
	uint8_t challengersQueue[kMaxPlayers];
	// Let the low-level client code access it without parsing high-level schema
	uint8_t pingSlot;

	using wsw::ScoreboardShared::kMaxShortSlots;

	void setPlayerScore( unsigned playerIndex, int value ) {
		assert( playerIndex < (unsigned)kMaxPlayers );
		scores[playerIndex] = value;
	}

	[[nodiscard]]
	auto getPlayerScore( unsigned playerIndex ) const -> int {
		assert( playerIndex < (unsigned)kMaxPlayers );
		return scores[playerIndex];
	}

	void setPlayerShort( unsigned playerIndex, unsigned slot, int16_t value ) {
		assert( playerIndex < (unsigned)kMaxPlayers );
		assert( slot < (unsigned)kMaxShortSlots );
		values[slot][playerIndex] = value;
	}

	[[nodiscard]]
	auto getPlayerShort( unsigned playerIndex, unsigned slot ) const -> int16_t {
		assert( playerIndex < (unsigned)kMaxPlayers );
		assert( slot < (unsigned)kMaxShortSlots );
		return values[slot][playerIndex];
	}

	void setPlayerTeam( unsigned playerIndex, int team ) {
		assert( playerIndex < (unsigned)kMaxPlayers );
		assert( team >= 0 && team <= 3 );
		playersTeamMask |= ( (uint64_t)team << ( 2 * playerIndex ) );
	}

	[[nodiscard]]
	auto getPlayerTeam( unsigned playerIndex ) const -> int {
		assert( playerIndex < (unsigned)kMaxPlayers );
		return (int)( ( playersTeamMask >> ( 2 * playerIndex ) ) & 0x3u );
	}

	void setPlayerNum( unsigned playerIndex, unsigned num ) {
		assert( playerIndex < (unsigned)kMaxPlayers && num < MAX_CLIENTS );
		auto &packed = packedPlayerSpecificData[playerIndex];
		packed = packWord( packed, num, kPlayerNumMask, kPlayerNumShift );
	}

	[[nodiscard]]
	auto getPlayerNum( unsigned playerIndex ) const -> unsigned {
		assert( playerIndex < (unsigned)kMaxPlayers );
		return unpackWord( packedPlayerSpecificData[playerIndex], kPlayerNumMask, kPlayerNumShift );
	}

	void setPlayerHealth( unsigned playerIndex, int health ) {
		assert( playerIndex < (unsigned)MAX_CLIENTS && (unsigned)health < 1024u );
		auto &packed = packedPlayerSpecificData[playerIndex];
		packed = packWord( packed, (unsigned)health, kHealthMask, kHealthShift );
	}

	[[nodiscard]]
	auto getPlayerHealth( unsigned playerIndex ) const -> unsigned {
		assert( playerIndex < (unsigned)MAX_CLIENTS );
		return (uint16_t)unpackWord( packedPlayerSpecificData[playerIndex], kHealthMask, kHealthShift );
	}

	void setPlayerArmor( unsigned playerIndex, int armor ) {
		assert( playerIndex < (unsigned)MAX_CLIENTS && (unsigned)armor < 1024u );
		auto &packed = packedPlayerSpecificData[playerIndex];
		packed = packWord( packed, (unsigned)armor, kArmorMask, kArmorShift );
	}

	[[nodiscard]]
	auto getPlayerArmor( unsigned playerIndex ) const -> unsigned {
		assert( playerIndex < (unsigned)MAX_CLIENTS );
		return (uint16_t)unpackWord( packedPlayerSpecificData[playerIndex], kArmorMask, kArmorShift );
	}

	void setPlayerWeapon( unsigned playerIndex, int weapon ) {
		assert( playerIndex < (unsigned)MAX_CLIENTS && (unsigned)weapon < 16u );
		auto &packed = packedPlayerSpecificData[playerIndex];
		packed = packWord( packed, (unsigned)weapon, kWeaponMask, kWeaponShift );
	}

	[[nodiscard]]
	auto getPlayerWeapon( unsigned playerIndex ) const -> unsigned {
		assert( playerIndex < (unsigned)MAX_CLIENTS );
		return (uint8_t)unpackWord( packedPlayerSpecificData[playerIndex], kWeaponMask, kWeaponShift );
	}

	void setPlayerPowerupsBits( unsigned playerIndex, unsigned powerupBits ) {
		assert( playerIndex < (unsigned)MAX_CLIENTS && powerupBits < 8 );
		auto &packed = packedPlayerSpecificData[playerIndex];
		packed = packWord( packed, powerupBits, kPowerupsMask, kPowerupsShift );
	}

	[[nodiscard]]
	auto getPlayerPowerupBits( unsigned playerIndex ) const -> unsigned {
		assert( playerIndex < (unsigned)MAX_CLIENTS );
		return unpackWord( packedPlayerSpecificData[playerIndex], kPowerupsMask, kPowerupsShift );
	}

	void shadowPrivateData( unsigned playerIndex ) {
		setPlayerHealth( playerIndex, 0 );
		setPlayerArmor( playerIndex, 0 );
		setPlayerWeapon( playerIndex, 0 );
	}

	void setPlayerConnected( unsigned playerIndex, bool connected ) {
		setPlayerFlagBit( playerIndex, kFlagBitConnected, connected );
	}

	[[nodiscard]]
	bool isPlayerConnected( unsigned playerIndex ) const {
		return testPlayerFlagBit( playerIndex, kFlagBitConnected );
	}

	void setPlayerGhosting( unsigned playerIndex, bool ghosting ) {
		setPlayerFlagBit( playerIndex, kFlagBitGhosting, ghosting );
	}

	[[nodiscard]]
	bool isPlayerGhosting( unsigned playerIndex ) const {
		return testPlayerFlagBit( playerIndex, kFlagBitGhosting );
	}

	[[nodiscard]]
	auto getClientNumOfChallenger( unsigned indexInQueue ) const -> std::optional<unsigned> {
		assert( indexInQueue < (unsigned)MAX_CLIENTS );
		const unsigned maybeNum = challengersQueue[indexInQueue];
		return maybeNum ? std::optional( maybeNum - 1u ) : std::nullopt;
	}

	void copyThatRow( unsigned destRow, const ReplicatedScoreboardData &that, unsigned thatSrcRow ) {
		scores[destRow] = that.scores[thatSrcRow];
		// TODO: This could be a memmove but it's more clear written this way
		for( unsigned slot = 0; slot < kMaxShortSlots; ++slot ) {
			setPlayerShort( destRow, slot, that.getPlayerShort( thatSrcRow, slot ) );
			assert( getPlayerShort( destRow, slot ) == that.getPlayerShort( thatSrcRow, slot ) );
		}
		packedPlayerSpecificData[destRow] = that.packedPlayerSpecificData[thatSrcRow];
	}
};

//==============================================

#define MAX_PARSE_GAMECOMMANDS  256

typedef struct {
	bool all;
	uint8_t targets[MAX_CLIENTS / 8];
	size_t commandOffset;           // offset of the data in gamecommandsData
} gcommand_t;

//==============================================

// player_state_t is the information needed in addition to pmove_state_t
// to rendered a view.  There will only be 10 player_state_t sent each second,
// but the number of pmove_state_t changes will be relative to client
// frame rates
#define PS_MAX_STATS            64

constexpr const unsigned kNumAccuracySlots = 10;

typedef struct {
	pmove_state_t pmove;        // for prediction

	// these fields do not need to be communicated bit-precise

	vec3_t viewangles;          // for fixed views

	int event[2], eventParm[2];
	unsigned int POVnum;        // entity number of the player in POV
	unsigned int playerNum;     // client number
	float viewheight;
	float fov;                  // horizontal field of view (unused)

	int inventory[MAX_ITEMS];
	short stats[PS_MAX_STATS];  // fast status bar updates
	uint32_t plrkeys;           // infos on the pressed keys of chased player (self if not chasing)
	uint8_t weaponState;

	uint8_t strongAccuracy[kNumAccuracySlots];
	uint8_t weakAccuracy[kNumAccuracySlots];
} player_state_t;

typedef struct {
	// state (in / out)
	player_state_t *playerState;

	// command (in)
	usercmd_t cmd;

	// A hint (in)
	bool skipCollision;
	// A flag for disabling occasional ladder usage for bots without intrusive changes to bot code (in)
	bool skipLadders;

	bool snapInitially;

	// results (out)
	int numtouch;
	int touchents[MAXTOUCH];
	float step;                 // used for smoothing the player view

	vec3_t mins, maxs;          // bounding box size

	vec3_t forward, right;

	int groundentity;
	cplane_t groundplane;       // valid if groundentity >= 0
	int groundsurfFlags;        // valid if groundentity >= 0
	int groundcontents;         // valid if groundentity >= 0
	int watertype;
	int waterlevel;

	int contentmask;

	bool ladder;
} pmove_t;

#endif // GAME_QCOMREF_H
