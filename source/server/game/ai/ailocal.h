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
--------------------------------------------------------------
The ACE Bot is a product of Steve Yeager, and is available from
the ACE Bot homepage, at http://www.axionfx.com/ace.

This program is a modification of the ACE Bot, and is therefore
in NO WAY supported by Steve Yeager.
*/

#ifndef WSW_6e462ea5_4ee8_482b_a00a_74ab3719bacf_H
#define WSW_6e462ea5_4ee8_482b_a00a_74ab3719bacf_H

#include "../g_local.h"
#include <common/facilities/configvars.h>
#include <common/facilities/q_collision.h>

#include <utility>
#include <stdarg.h>

// Platform states:
constexpr auto STATE_TOP    = 0;
constexpr auto STATE_BOTTOM = 1;
constexpr auto STATE_UP     = 2;
constexpr auto STATE_DOWN   = 3;

constexpr auto MAX_NAVENTS = MAX_EDICTS;

constexpr auto AI_STEPSIZE          = STEPSIZE; // 18
constexpr auto AI_JUMPABLE_HEIGHT   = 50;
constexpr auto AI_JUMPABLE_DISTANCE = 360;
constexpr auto AI_WATERJUMP_HEIGHT  = 24;
constexpr auto AI_MIN_RJ_HEIGHT     = 128;
constexpr auto AI_MAX_RJ_HEIGHT     = 512;
constexpr auto AI_GOAL_SR_RADIUS    = 200;
constexpr auto AI_GOAL_SR_LR_RADIUS = 600;

constexpr int AI_GOAL_SR_MILLIS    = 750;
constexpr int AI_GOAL_SR_LR_MILLIS = 1500;

constexpr auto MASK_AISOLID = CONTENTS_SOLID | CONTENTS_PLAYERCLIP | CONTENTS_BODY | CONTENTS_MONSTERCLIP;

typedef enum {
	AI_WEAPON_AIM_TYPE_INSTANT_HIT,
	AI_WEAPON_AIM_TYPE_PREDICTION,
	AI_WEAPON_AIM_TYPE_PREDICTION_EXPLOSIVE,
	AI_WEAPON_AIM_TYPE_DROP
} ai_weapon_aim_type;

ai_weapon_aim_type BuiltinWeaponAimType( int builtinWeapon, int fireMode );

inline bool IsBuiltinWeaponContinuousFire( int builtinWeapon ) {
	return builtinWeapon == WEAP_LASERGUN || builtinWeapon == WEAP_PLASMAGUN || builtinWeapon == WEAP_MACHINEGUN;
}

int BuiltinWeaponTier( int builtinWeapon );
int FindBestWeaponTier( const Client *client );

bool GT_asBotWouldDropHealth( const Client *client );
void GT_asBotDropHealth( Client *client );
bool GT_asBotWouldDropArmor( const Client *client );
void GT_asBotDropArmor( Client *client );

void GT_asBotTouchedGoal( const Bot *bot, const edict_t *goalEnt );
void GT_asBotReachedGoalRadius( const Bot *bot, const edict_t *goalEnt );

// These functions return a score in range [0, 1].
// Default score should be 0.5f, and it should be returned
// when a GT script does not provide these function counterparts.
// Note that offence and defence score are not complementary but independent.
float GT_asPlayerOffensiveAbilitiesRating( const Client *client );
float GT_asPlayerDefenciveAbilitiesRating( const Client *client );

struct AiScriptWeaponDef {
	int weaponNum;
	int tier;
	float minRange;
	float maxRange;
	float bestRange;
	float projectileSpeed;
	float splashRadius;
	float maxSelfDamage;
	ai_weapon_aim_type aimType;
	bool isContinuousFire;
};

int GT_asGetScriptWeaponsNum( const Client *client );
bool GT_asGetScriptWeaponDef( const Client *client, int scriptWeaponNum, AiScriptWeaponDef *weaponDef );
int GT_asGetScriptWeaponCooldown( const Client *client, int scriptWeaponNum );
bool GT_asSelectScriptWeapon( Client *client, int scriptWeaponNum );
bool GT_asFireScriptWeapon( Client *client, int scriptWeaponNum );

#include "navigation/aasworld.h"
#include "vec3.h"

#ifndef _MSC_VER
void AI_Debug( const char *tag, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
void AI_Debugv( const char *tag, const char *format, va_list va );
void AI_FailWith( const char *tag, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) ) __attribute__( ( noreturn ) );
void AI_FailWithv( const char *tag, const char *format, va_list va ) __attribute__( ( noreturn ) );
#else
void AI_Debug( const char *tag, _Printf_format_string_ const char *format, ... );
void AI_Debugv( const char *tag, const char *format, va_list va );
__declspec( noreturn ) void AI_FailWith( const char *tag, _Printf_format_string_ const char *format, ... );
__declspec( noreturn ) void AI_FailWithv( const char *tag, const char *format, va_list va );
#endif

inline float Clamp( float value ) {
	Q_clamp( value, 0.0f, 1.0f );
	return value;
}

inline float Clamp( float value, float minValue, float maxValue ) {
	Q_clamp( value, minValue, maxValue );
	return value;
}

inline float BoundedFraction( float value, float bound ) {
	return wsw::min( value, bound ) / bound;
}

inline unsigned From0UpToMax( unsigned maxValue, float ratio ) {
//#ifdef _DEBUG
	if( ratio < 0 || ratio > 1 ) {
		AI_FailWith( "From0UpToMax()", "ratio %f is out of valid [0,1] bounds", ratio );
	}
//#endif
	return (unsigned)( maxValue * ratio );
}

inline unsigned From1UpToMax( unsigned maxValue, float ratio ) {
//#ifdef _DEBUG
	if( !maxValue ) {
		AI_FailWith( "From1UpToMax()", "maxValue is 0" );
	}
//#endif
	return 1 + From0UpToMax( maxValue - 1, ratio );
}

inline void SetPacked4uVec( const vec3_t vec, int16_t *packed ) {
	packed[0] = (int16_t)( vec[0] / 4.0f );
	packed[1] = (int16_t)( vec[1] / 4.0f );
	packed[2] = (int16_t)( vec[2] / 4.0f );
}

inline void SetPacked4uVec( const Vec3 &vec, int16_t *packed ) {
	SetPacked4uVec( vec.Data(), packed );
}

inline Vec3 GetUnpacked4uVec( const int16_t *packed ) {
	return Vec3( packed[0] * 4, packed[1] * 4, packed[2] * 4 );
}

inline const char *Nick( const edict_t *ent ) {
	if( !ent ) {
		return "???";
	}
	if( ent->r.client ) {
		return ent->r.client->netname.data();
	}
	return ent->classname;
}

inline const char *WeapName( int weapon ) {
	return GS_GetWeaponDef( ggs, weapon )->name;
}

//----------------------------------------------------------

[[nodiscard]]
inline auto getTriggerOrigin( const edict_t *trigger ) -> Vec3 {
	// Triggers are usually based on brush models, hence their origins are usually all-zero
	return 0.5f * ( Vec3( trigger->r.absmin ) + Vec3( trigger->r.absmax ) );
}

//game
//----------------------------------------------------------
void Use_Plat( edict_t *ent, edict_t *other, edict_t *activator );

void AITools_DrawLine( const vec3_t origin, const vec3_t dest );
void AITools_DrawColorLine( const vec3_t origin, const vec3_t dest, int color, int parm );

// A cheaper version of G_Trace() that does not check against entities
inline void StaticWorldTrace( trace_t *trace, const vec3_t from, const vec3_t to, int contentsMask,
							  const vec3_t mins = vec3_origin, const vec3_t maxs = vec3_origin, int topNodeHint = 0 ) {
	assert( from );
	float *from_ = const_cast<float *>( from );
	assert( to );
	float *to_ = const_cast<float *>( to );
	assert( mins );
	float *mins_ = const_cast<float *>( mins );
	assert( maxs );
	float *maxs_ = const_cast<float *>( maxs );
	SV_TransformedBoxTrace( trace, from_, to_, mins_, maxs_, nullptr, contentsMask, nullptr, nullptr, topNodeHint );
}

// This shorthand is for backward compatibility and some degree of convenience
inline void SolidWorldTrace( trace_t *trace, const vec3_t from, const vec3_t to,
							 const vec3_t mins = vec3_origin, const vec3_t maxs = vec3_origin ) {
	StaticWorldTrace( trace, from, to, MASK_SOLID, mins, maxs );
}

struct EntAndScore {
	int entNum;
	float score;
	EntAndScore(): entNum( 0 ), score( 0.0f ) {}
	EntAndScore( int entNum_, float score_ ) : entNum( entNum_ ), score( score_ ) {}
	bool operator<( const EntAndScore &that ) const { return score > that.score; }
};

struct AreaAndScore {
	int areaNum;
	float score;
	AreaAndScore(): areaNum( 0 ), score( 0.0f ) {}
	AreaAndScore( int areaNum_, float score_ ) : areaNum( areaNum_ ), score( score_ ) {}
	bool operator<( const AreaAndScore &that ) const { return score > that.score; }
};

// This is a compact storage for 64-bit values.
// If an int64_t field is used in an array of tiny structs,
// a substantial amount of space can be lost for alignment.
class alignas ( 4 )Int64Align4 {
	uint32_t parts[2];

	inline void SetParts( int64_t value ) {
		parts[0] = (uint32_t)( ( (uint64_t)value >> 32 ) & 0xFFFFFFFFu );
		parts[1] = (uint32_t)( ( (uint64_t)value >> 00 ) & 0xFFFFFFFFu );
	}
public:
	operator int64_t() const {
		return (int64_t)( ( (uint64_t)parts[0] << 32 ) | parts[1] );
	}

	Int64Align4 operator=( int64_t value ) {
		SetParts( value );
		return *this;
	}

	Int64Align4() {}

	Int64Align4( int64_t value ) {
		SetParts( value );
	}
};

class alignas ( 4 )UInt64Align4 {
	uint32_t parts[2];

	inline void SetParts( uint64_t value ) {
		parts[0] = (uint32_t)( ( (uint64_t)value >> 32 ) & 0xFFFFFFFFu );
		parts[1] = (uint32_t)( ( (uint64_t)value >> 00 ) & 0xFFFFFFFFu );
	}
public:
	operator uint64_t() const {
		return (uint64_t)( ( (uint64_t)parts[0] << 32 ) | parts[1] );
	}

	UInt64Align4 operator=( uint64_t value ) {
		SetParts( value );
		return *this;
	}

	UInt64Align4() {}

	UInt64Align4( uint64_t value ) {
		SetParts( value );
	}
};

class alignas( 2 )Int32Align2 {
	uint16_t parts[2];

	inline void SetParts( int32_t value ) {
		uint32_t u = (uint32_t)value;
		parts[0] = (uint16_t)( ( u >> 16u ) & 0xFFFFu );
		parts[1] = (uint16_t)( u & 0xFFFFu );
	}
public:
	operator int32_t() const {
		return (int32_t)( ( (uint32_t)parts[0] ) << 16u | parts[1] );
	}

	Int32Align2 operator=( int32_t value ) {
		SetParts( value );
		return *this;
	}

	Int32Align2() {}

	Int32Align2( int32_t value ) {
		SetParts( value );
	}
};

class alignas( 2 )FloatAlign2 {
	uint16_t parts[2];

	inline void SetParts( float value ) {
		uint32_t *p = (uint32_t *)&value;
		parts[0] = (uint16_t)( ( *p >> 16u ) & 0xFFFFu );
		parts[1] = (uint16_t)( *p & 0xFFFFu );
	}
public:
	operator float() const {
		uint32_t v = ( (uint32_t)parts[0] ) << 16u | parts[1];
		return *( (float *)&v );
	}

	FloatAlign2 operator=( float value ) {
		SetParts( value );
		return *this;
	}

	FloatAlign2() {}

	FloatAlign2( float value ) {
		SetParts( value );
	}
};

extern const BoolConfigVar v_evolution;
extern const BoolConfigVar v_debugOutput;
extern const BoolConfigVar v_shareRoutingCache;
extern const StringConfigVar v_forceWeapon;

#define aiDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::AI, wsw::MessageCategory::Debug ) ).getWriter()
#define aiNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::AI, wsw::MessageCategory::Notice ) ).getWriter()
#define aiWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::AI, wsw::MessageCategory::Warning ) ).getWriter()
#define aiError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::AI, wsw::MessageCategory::Error ) ).getWriter()

#endif