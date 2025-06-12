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

// gs_weapondefs.c	-	hard coded weapon definitions

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include "q_comref.h"
#include "q_collision.h"
#include "gs_public.h"

#define INSTANT 0

#define WEAPONDOWN_FRAMETIME 50
#define WEAPONUP_FRAMETIME 50
#define DEFAULT_BULLET_SPREAD 350

static std::initializer_list<gs_weapon_definition_t> kDefaultWeaponDefs =
{
	{
		"no weapon",
		WEAP_NONE,
		{
			FIRE_MODE_STRONG,               // fire mode
			AMMO_NONE,                      // ammo tag
			0,                              // ammo usage per shot
			0,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			0,                              // reload frametime
			0,                              // cooldown frametime
			0,                              // projectile timeout
			false,                          // smooth refire

			//damages
			0,                              // damage
			0,                              // selfdamage ratio
			0,                              // knockback
			0,                              // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			0,                              // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep


			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			0,                              // max amount
			0                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,                 // fire mode
			AMMO_NONE,                      // ammo tag
			0,                              // ammo usage per shot
			0,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			0,                              // reload frametime
			0,                              // cooldown frametime
			0,                              // projectile timeout
			false,                          // smooth refire

			//damages
			0,                              // damage
			0,                              // selfdamage ratio
			0,                              // knockback
			0,                              // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			0,                              // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			0,                              // max amount
			0                               // low ammo threshold
		},
	},

	{
		"Gunblade",
		WEAP_GUNBLADE,
		{
			FIRE_MODE_STRONG,
			AMMO_GUNBLADE,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			600,                            // reload frametime
			0,                              // cooldown frametime
			5000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			30,                             // damage
			1.0,                            // selfdamage ratio
			90,                             // knockback
			0,                              // stun
			80,                             // splash radius
			0,                              // splash fraction
			8,                              // splash minimum damage
			10,                             // splash minimum knockback

			//projectile def
			2500,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			1,                              // max amount
			0                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_GUNBLADE,
			0,                              // ammo usage per shot
			0,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			600,                            // reload frametime
			0,                              // cooldown frametime
			64,                             // projectile timeout  / projectile range for instant weapons
			false,                          // smooth refire

			//damages
			50,                             // damage
			0,                              // selfdamage ratio
			50,                             // knockback
			0,                              // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			0,                              // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			0,                              // max amount
			0                               // low ammo threshold
		},
	},

	{
		"Machinegun",
		WEAP_MACHINEGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_BULLETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			50,                            // reload frametime
			0,                              // cooldown frametime
			6000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			8,                              // damage
			0,                              // selfdamage ratio
			10,                             // knockback
			50,                             // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			250,                            // spread
			250,                            // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                             // weapon pickup amount
			100,                            // pickup amount
			100,                            // max amount
			20                              // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_BULLETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			80,                             // reload frametime
			0,                              // cooldown frametime
			6000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			8,                              // damage
			0,                              // selfdamage ratio
			10,                             // knockback
			50,                             // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			150,                             // spread
			150,                             // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			100,                              // weapon pickup amount
			0,                              // pickup amount
			100,                              // max amount
			0                               // low ammo threshold
		},
	},

	{
		"Riotgun",
		WEAP_RIOTGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_SHELLS,
			1,                              // ammo usage per shot
			20,                             // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			800,                            // reload frametime
			0,                              // cooldown frametime
			8192,                           // projectile timeout / projectile range for instant weapons
			false,                          // smooth refire

			//damages
			5,                              // damage
			0,                              // selfdamage ratio (rg cant selfdamage)
			7,                              // knockback
			85,                             // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			160,                            // spread
			90,                             // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			10,                             // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_SHELLS,
			1,                              // ammo usage per shot
			10,                             // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			600,                            // reload frametime
			0,                              // cooldown frametime
			8192,                           // projectile timeout / projectile range for instant weapons
			false,                          // smooth refire

			//damages
			4,                              // damage
			0,                              // selfdamage ratio (rg cant selfdamage)
			5,                              // knockback
			85,                             // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			160,                            // spread
			90,                             // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                             // weapon pickup amount
			0,                              // pickup amount
			10,                             // max amount
			6                               // low ammo threshold
		},
	},

	{
		"Grenade Launcher",
		WEAP_GRENADELAUNCHER,
		{
			FIRE_MODE_STRONG,
			AMMO_GRENADES,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			800,                            // reload frametime
			0,                              // cooldown frametime
			1250,                           // projectile timeout
			false,                          // smooth refire

			//damages
			80,                             // damage
			1.00,                           // selfdamage ratio
			100,                                // knockback
			1250,                           // stun
			125,                            // splash radius
			0,                              // splash fraction
			15,                             // splash minimum damage
			35,                             // splash minimum knockback

			//projectile def
			1000,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			10,                             // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_GRENADES,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			800,                            // reload frametime
			0,                              // cooldown frametime
			1250,                           // projectile timeout
			false,                          // smooth refire

			//damages
			80,                             // damage
			1.00,                           // selfdamage ratio
			100,                                // knockback
			1250,                           // stun
			135,                            // splash radius
			0,                              // splash fraction
			15,                             // splash minimum damage
			35,                             // splash minimum knockback

			//projectile def
			1000,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                             // weapon pickup amount
			0,                              // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},
	},

	{
		"Rocket Launcher",
		WEAP_ROCKETLAUNCHER,
		{
			FIRE_MODE_STRONG,
			AMMO_ROCKETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			900,                            // reload frametime
			0,                              // cooldown frametime
			10000,                          // projectile timeout
			false,                          // smooth refire

			//damages
			90,                             // damage
			1.00,                           // selfdamage ratio
			100,                                // knockback
			1250,                           // stun
			100,                            // splash radius
			0,                              // splash fraction
			10,                             // splash minimum damage
			35,                             // splash minimum knockback

			//projectile def
			1150,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			10,                             // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_ROCKETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			900,                            // reload frametime
			0,                              // cooldown frametime
			10000,                          // projectile timeout
			false,                          // smooth refire

			//damages
			90,                             // damage
			1.00,                           // selfdamage ratio
			100,                                // knockback
			1250,                           // stun
			100,                            // splash radius
			0,                              // splash fraction
			10,                             // splash minimum damage
			35,                             // splash minimum knockback

			//projectile def
			1150,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                              // weapon pickup amount
			0,                              // pickup amount
			10,                              // max amount
			0                               // low ammo threshold
		},
	},

	{
		"Plasmagun",
		WEAP_PLASMAGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_PLASMA,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			100,                            // reload frametime
			0,                              // cooldown frametime
			5000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			15,                             // damage
			0.5,                            // selfdamage ratio
			20,                             // knockback
			200,                            // stun
			45,                             // splash radius
			0,                              // splash fraction
			5,                              // splash minimum damage
			1,                              // splash minimum knockback

			//projectile def
			2500,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                             // weapon pickup amount
			100,                            // pickup amount
			100,                            // max amount
			20                              // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_PLASMA,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			100,                            // reload frametime
			0,                              // cooldown frametime
			5000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			15,                             // damage
			0.5,                            // selfdamage ratio
			20,                             // knockback
			200,                            // stun
			45,                             // splash radius
			0,                              // splash fraction
			5,                              // splash minimum damage
			1,                              // splash minimum knockback

			//projectile def
			2500,                           // speed
			175,                            // spread
			175,                            // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			100,                              // weapon pickup amount
			0,                              // pickup amount
			100,                              // max amount
			0                               // low ammo threshold
		},
	},

	{
		"Lasergun",
		WEAP_LASERGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_LASERS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			50,                             // reload frametime
			0,                              // cooldown frametime
			(unsigned)kLasergunRange,       // projectile timeout / projectile range for instant weapons
			true,                           // smooth refire

			//damages
			7,                              // damage
			0,                              // selfdamage ratio (lg cant damage)
			12,                             // knockback
			300,                            // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                             // weapon pickup amount
			100,                            // pickup amount
			100,                            // max amount
			20                              // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_LASERS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			50,                             // reload frametime
			0,                              // cooldown frametime
			(unsigned)kLasergunRange,       // projectile timeout / projectile range for instant weapons
			true,                           // smooth refire

			//damages
			7,                              // damage
			0,                              // selfdamage ratio (lg cant damage)
			12,                             // knockback
			300,                            // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			100,                              // weapon pickup amount
			0,                              // pickup amount
			100,                            // max amount
			20                              // low ammo threshold
		},
	},

	{
		"Electrobolt",
		WEAP_ELECTROBOLT,
		{
			FIRE_MODE_STRONG,
			AMMO_BOLTS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			1250,                           // reload frametime
			0,                              // cooldown frametime
			900,                            // min damage range
			false,                          // smooth refire

			//damages
			70,                             // damage
			0,                              // selfdamage ratio
			80,                             // knockback
			1000,                           // stun
			0,                              // splash radius
			0,                              // splash fraction
			75,                             // minimum damage
			35,                             // minimum knockback

			//projectile def
			INSTANT,                        // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			10,                             // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_BOLTS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			1250,                           // reload frametime
			0,                              // cooldown frametime
			900,                            // min damage range
			false,                          // smooth refire

			//damages
			70,                             // damage
			0,                              // selfdamage ratio
			160,                             // knockback
			1000,                           // stun
			0,                              // splash radius
			0,                              // splash fraction
			75,                             // minimum damage
			35,                             // minimum knockback

			//projectile def
			7500,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                              // weapon pickup amount
			0,                              // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},
	},

	{
		"Shockwave",
		WEAP_SHOCKWAVE,
		{
			FIRE_MODE_STRONG,
			AMMO_WAVES,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			300,                            // weapon up frametime (a noticeable delay)
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			950,                            // reload frametime (same as RL)
			0,                              // cooldown frametime
			10000,                          // projectile timeout
			false,                          // smooth refire

			//damages
			55,                             // damage
			1.0,                            // selfdamage ratio
			90,                             // knockback (as gunblade)
			500,                            // stun
			90,                             // splash radius
			0,                              // splash fraction
			55,                             // minimum damage
			35,                             // minimum knockback

			//projectile def
			775,                            // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			10,                             // pickup amount
			10,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_WAVES,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			300,                            // weapon up frametime (a noticeable delay)
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			950,                            // reload frametime (same as RL)
			0,                              // cooldown frametime
			10000,                          // projectile timeout
			false,                          // smooth refire

			//damages
			55,                             // damage
			0,                              // selfdamage ratio
			90,                             // knockback
			500,                            // stun
			90,                             // splash radius
			0,                              // splash fraction
			55,                             // minimum damage
			35,                             // minimum knockback

			//projectile def
			775,                            // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                              // weapon pickup amount
			0,                              // pickup amount
			10,                              // max amount
			3                               // low ammo threshold
		},
	},

	{
		"Instagun",
		WEAP_INSTAGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_INSTAS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			1300,                           // reload frametime
			0,                              // cooldown frametime
			8024,                           // range
			false,                          // smooth refire

			//damages
			200,                            // damage
			0.1,                            // selfdamage ratio (ig cant damage)
			95,                             // knockback
			1000,                           // stun
			80,                             // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                              // weapon pickup amount
			10,                              // pickup amount
			10,                              // max amount
			0                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_INSTAS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			1300,                           // reload frametime
			0,                              // cooldown frametime
			8024,                           // range
			false,                          // smooth refire

			//damages
			200,                            // damage
			0.1,                            // selfdamage ratio (ig cant damage)
			95,                             // knockback
			1000,                           // stun
			80,                             // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			INSTANT,                        // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			10,                              // weapon pickup amount
			10,                              // pickup amount
			10,                             // max amount
			0                               // low ammo threshold
		},
	},
};

static std::initializer_list<gs_weapon_definition_t> kDefaultRaceWeaponDefs =
{
	{NULL},

	{
		"Gunblade",
		WEAP_GUNBLADE,
		{
			FIRE_MODE_STRONG,
			AMMO_GUNBLADE,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			600,                            // reload frametime
			0,                              // cooldown frametime
			5000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			35,                             // damage
			1.0,                            // selfdamage ratio
			60,                             // knockback (race specific)
			0,                              // stun
			80,                             // splash radius
			1.3,                            // splash fraction (race specific)
			8,                              // splash minimum damage
			10,                             // splash minimum knockback

			//projectile def
			3000,                           // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			1,                              // max amount
			0                               // low ammo threshold
		},

		{
			FIRE_MODE_WEAK,
			AMMO_WEAK_GUNBLADE,
			0,                              // ammo usage per shot
			0,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			600,                            // reload frametime
			0,                              // cooldown frametime
			64,                             // projectile timeout  / projectile range for instant weapons
			false,                          // smooth refire

			//damages
			50,                             // damage
			0,                              // selfdamage ratio
			50,                             // knockback
			0,                              // stun
			0,                              // splash radius
			0,                              // splash fraction
			0,                              // splash minimum damage
			0,                              // splash minimum knockback

			//projectile def
			0,                              // speed
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep

			//ammo
			0,                              // weapon pickup amount
			0,                              // pickup amount
			0,                              // max amount
			0                               // low ammo threshold
		},
	},

	{NULL},

	{NULL},

	{
		"Grenade Launcher",
		WEAP_GRENADELAUNCHER,
		{
			FIRE_MODE_STRONG,
			AMMO_GRENADES,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			800,                            // reload frametime
			0,                              // cooldown frametime
			1650,                           // projectile timeout (race specific)
			false,                          // smooth refire

			//damages
			80,                             // damage
			1.00,                           // selfdamage ratio
			120,                                // knockback (race specific)
			1250,                           // stun
			170,                            // splash radius (race specific)
			2.5,                            // splash fraction (race specific)
			15,                             // splash minimum damage
			1,                              // splash minimum knockback (race specific)

			//projectile def
			800,                            // speed (race specific)
			0,                              // spread
			0,                              // v_spread
			1.25,                           // friction (race specific)
			1.22,                           // gravity (race specific)
			24,                             // prestep (race specific)

			//ammo
			10,                             // weapon pickup amount
			10,                             // pickup amount
			20,                             // max amount
			3                               // low ammo threshold
		},

		{
			 FIRE_MODE_STRONG,
			 AMMO_GRENADES,
			 1,                              // ammo usage per shot
			 1,                              // projectiles fired each shot

			 //timings (in msecs)
			 WEAPONUP_FRAMETIME,             // weapon up frametime
			 WEAPONDOWN_FRAMETIME,           // weapon down frametime
			 800,                            // reload frametime
			 0,                              // cooldown frametime
			 1650,                           // projectile timeout (race specific)
			 false,                          // smooth refire

			 //damages
			 80,                             // damage
			 1.00,                           // selfdamage ratio
			 120,                                // knockback (race specific)
			 1250,                           // stun
			 170,                            // splash radius (race specific)
			 2.5,                            // splash fraction (race specific)
			 15,                             // splash minimum damage
			 1,                              // splash minimum knockback (race specific)

			 //projectile def
			 800,                            // speed (race specific)
			 0,                              // spread
			 0,                              // v_spread
			 1.25,                           // friction (race specific)
			 1.22,                           // gravity (race specific)
			 24,                             // prestep (race specific)

			 //ammo
			 10,                             // weapon pickup amount
			 10,                             // pickup amount
			 20,                             // max amount
			 3                               // low ammo threshold
		 },
	},

	{
		"Rocket Launcher",
		WEAP_ROCKETLAUNCHER,
		{
			FIRE_MODE_STRONG,
			AMMO_ROCKETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			850,                            // reload frametime (race specific)
			0,                              // cooldown frametime
			100000,                          // projectile timeout (race specific)
			false,                          // smooth refire

			//damages
			80,                             // damage
			1.00,                           // selfdamage ratio
			108,                                // knockback (race specific)
			1250,                           // stun
			120,                            // splash radius (race specific)
			1,                              // splash fraction (race specific)
			15,                             // splash minimum damage
			1,                              // splash minimum knockback (race specific)

			//projectile def
			950,                            // speed (race specific)
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			10,                             // prestep (race specific)

			//ammo
			5,                              // weapon pickup amount
			10,                             // pickup amount
			20,                             // max amount
			3                               // low ammo threshold
		},

		{
			FIRE_MODE_STRONG,
			AMMO_ROCKETS,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			850,                            // reload frametime (race specific)
			0,                              // cooldown frametime
			100000,                          // projectile timeout (race specific)
			false,                          // smooth refire

			//damages
			80,                             // damage
			1.00,                           // selfdamage ratio
			108,                                // knockback (race specific)
			1250,                           // stun
			120,                            // splash radius (race specific)
			1,                              // splash fraction (race specific)
			15,                             // splash minimum damage
			1,                              // splash minimum knockback (race specific)

			//projectile def
			950,                            // speed (race specific)
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			10,                             // prestep (race specific)

			//ammo
			5,                              // weapon pickup amount
			10,                             // pickup amount
			20,                             // max amount
			3                               // low ammo threshold
		},
	},

	{
		"Plasmagun",
		WEAP_PLASMAGUN,
		{
			FIRE_MODE_STRONG,
			AMMO_PLASMA,
			1,                              // ammo usage per shot
			1,                              // projectiles fired each shot

			//timings (in msecs)
			WEAPONUP_FRAMETIME,             // weapon up frametime
			WEAPONDOWN_FRAMETIME,           // weapon down frametime
			100,                            // reload frametime
			0,                              // cooldown frametime
			5000,                           // projectile timeout
			false,                          // smooth refire

			//damages
			15,                             // damage
			0.5,                            // selfdamage ratio
			24,                             // knockback (race specific)
			200,                            // stun
			40,                             // splash radius (race specific)
			1,                              // splash fraction (race specific)
			5,                              // splash minimum damage (race specific)
			1,                              // splash minimum knockback

			//projectile def
			1700,                           // speed (race specific)
			0,                              // spread
			0,                              // v_spread
			0,                              // friction
			0,                              // gravity
			0,                              // prestep (race specific)

			//ammo
			50,                             // weapon pickup amount
			100,                            // pickup amount
			150,                            // max amount
			20                              // low ammo threshold
		},

		{
			 FIRE_MODE_STRONG,
			 AMMO_PLASMA,
			 1,                              // ammo usage per shot
			 1,                              // projectiles fired each shot

			 //timings (in msecs)
			 WEAPONUP_FRAMETIME,             // weapon up frametime
			 WEAPONDOWN_FRAMETIME,           // weapon down frametime
			 100,                            // reload frametime
			 0,                              // cooldown frametime
			 5000,                           // projectile timeout
			 false,                          // smooth refire

			 //damages
			 15,                             // damage
			 0.5,                            // selfdamage ratio
			 24,                             // knockback (race specific)
			 200,                            // stun
			 40,                             // splash radius (race specific)
			 1,                              // splash fraction (race specific)
			 5,                              // splash minimum damage (race specific)
			 1,                              // splash minimum knockback

			 //projectile def
			 1700,                           // speed (race specific)
			 0,                              // spread
			 0,                              // v_spread
			 0,                              // friction
			 0,                              // gravity
			 0,                              // prestep (race specific)

			 //ammo
			 50,                             // weapon pickup amount
			 100,                            // pickup amount
			 150,                            // max amount
			 20                              // low ammo threshold
		 },
	},

	{NULL},

	{NULL},

	{NULL},

	{NULL},
};

/*
* GS_GetWeaponDef
*/
const gs_weapon_definition_t *GS_GetWeaponDef( const gs_state_t *gs, int weapon ) {
	if( gs->weaponDefs.empty() ) [[unlikely]] {
		gs->weaponDefs.assign( kDefaultWeaponDefs.begin(), kDefaultWeaponDefs.end() );
		gs->raceWeaponDefs.assign( kDefaultRaceWeaponDefs.begin(), kDefaultRaceWeaponDefs.end() );
	}

	assert( weapon >= 0 && weapon < WEAP_TOTAL );
	assert( gs->weaponDefs.size() == WEAP_TOTAL && gs->raceWeaponDefs.size() == WEAP_TOTAL );
	if( GS_RaceGametype( *gs ) && gs->raceWeaponDefs[weapon].name != NULL ){
		return &gs->raceWeaponDefs[weapon];
	} else {
		return &gs->weaponDefs[weapon];
	}
}

/*
* GS_GetWeaponDefExt
*/
gs_weapon_definition_t *GS_GetWeaponDefExt( gs_state_t *gs, int weapon, bool race ) {
	if( gs->weaponDefs.empty() ) [[unlikely]] {
		gs->weaponDefs.assign( kDefaultWeaponDefs.begin(), kDefaultWeaponDefs.end() );
		gs->raceWeaponDefs.assign( kDefaultRaceWeaponDefs.begin(), kDefaultRaceWeaponDefs.end() );
	}

	if( race && gs->raceWeaponDefs[weapon].name != NULL ) {
		return &gs->raceWeaponDefs[weapon];
	} else {
		return &gs->weaponDefs[weapon];
	}
}

/*
* GS_InitWeapons
*/
void GS_InitWeapons( gs_state_t *gs ) {
	gs->weaponDefs.assign( kDefaultWeaponDefs.begin(), kDefaultWeaponDefs.end() );
	gs->raceWeaponDefs.assign( kDefaultRaceWeaponDefs.begin(), kDefaultRaceWeaponDefs.end() );

	for( int i = WEAP_GUNBLADE; i < WEAP_TOTAL; i++ ) {
		const gsitem_t *item = GS_FindItemByTag( gs, i );
		const gs_weapon_definition_t *weapondef = GS_GetWeaponDef( gs, i );

		assert( item && weapondef );

		// hack : use the firedef pickup counts on items
		if( item->weakammo_tag ) {
			// Note: The behavior of const_cast is well-defined in this case
			if( auto *weakAmmoItem = const_cast<gsitem_t *>( GS_FindItemByTag( gs, item->weakammo_tag ) ) ) {
				weakAmmoItem->quantity = weapondef->firedef_weak.ammo_pickup;
				weakAmmoItem->inventory_max = weapondef->firedef_weak.ammo_max;
			}
		}

		if( item->ammo_tag ) {
			if( auto *ammoItem = const_cast<gsitem_t *>( GS_FindItemByTag( gs, item->ammo_tag ) ) ) {
				ammoItem->quantity = weapondef->firedef.ammo_pickup;
				ammoItem->inventory_max = weapondef->firedef.ammo_max;
			}
		}
	}
}
