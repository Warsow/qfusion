/*
Copyright (C) 2009-2019 Chasseur de bots

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

///*****************************************************************
/// NEW MAP ENTITY DEFINITIONS
///*****************************************************************

/**
 * Cgg - defrag support
 * target_init are meant to reset the player hp, armor and inventory.
 * spawnflags can be used to limit the effects of the target to certain types of items :
 *   - spawnflag 1 prevents the armor from being removed.
 *   - spawnflag 2 prevents the hp from being reset.
 *   - spawnflag 4 prevents the weapons and ammo from being removed.
 *   - spawnflag 8 prevents the powerups from being removed.
 *   - spawnflag 16 used to prevent the removal of the holdable items (namely the
 *     medkit and teleport) from the player inventory.
 */
void target_init_use( Entity @self, Entity @other, Entity @activator )
{
	if ( @activator.client == null )
		return;

	// armor
	if ( ( self.spawnFlags & 1 ) == 0 )
		activator.client.armor = 0;

	// health
	if ( ( self.spawnFlags & 2 ) == 0 )
	{
		activator.health = activator.maxHealth;
	}

	// weapons
	if ( ( self.spawnFlags & 4 ) == 0 )
	{
		for ( int i = WEAP_GUNBLADE; i < WEAP_TOTAL; i++ )
		{
			activator.client.inventorySetCount( i, 0 );
		}

		for ( int i = AMMO_WEAK_GUNBLADE; i < AMMO_TOTAL; i++ )
		{
			activator.client.inventorySetCount( i, 0 );
		}

		activator.client.inventorySetCount( WEAP_GUNBLADE, 1 );
		activator.client.selectWeapon( WEAP_GUNBLADE );
	}

	// powerups
	if ( ( self.spawnFlags & 8 ) == 0 )
	{
		for ( int i = POWERUP_QUAD; i < POWERUP_TOTAL; i++ )
			activator.client.inventorySetCount( i, 0 );
	}
}

// doesn't need to do anything at all, just sit there, waiting
void target_init( Entity @self )
{
	@self.use = target_init_use;
}

void target_checkpoint_use( Entity @self, Entity @other, Entity @activator )
{
	if ( @activator.client == null )
		return;

	Player @player = RACE_GetPlayer( activator.client );

	if ( !player.inRace )
		return;

	if ( player.touchCheckPoint( self.count ) )
		self.useTargets( activator );
}

void target_checkpoint( Entity @self )
{
	self.count = numCheckpoints;
	@self.use = target_checkpoint_use;
	numCheckpoints++;
}

void target_stoptimer_use( Entity @self, Entity @other, Entity @activator )
{
	if ( @activator.client == null )
		return;

	Player @player = RACE_GetPlayer( activator.client );

	if ( !player.inRace )
		return;

	player.completeRace();

	self.useTargets( activator );
}

// This sucks: some defrag maps have the entity classname with pseudo camel notation
// and classname->function is case sensitive

void target_stoptimer( Entity @self )
{
	@self.use = target_stoptimer_use;
}

void target_stopTimer( Entity @self )
{
	target_stoptimer( self );
}

void target_starttimer_use( Entity @self, Entity @other, Entity @activator )
{
	if ( @activator.client == null )
		return;

	Player @player = RACE_GetPlayer( activator.client );

	if ( player.inRace )
		return;

	if ( player.startRace() )
	{
		if ( !player.heardGo )
		{
			int soundIndex = G_SoundIndex( "sounds/announcer/countdown/go0" + (1 + (rand() & 1)) );
			G_AnnouncerSound( activator.client, soundIndex, GS_MAX_TEAMS, false, null );
			player.heardGo = true;
		}

		self.useTargets( activator );
	}
}

// doesn't need to do anything at all, just sit there, waiting
void target_starttimer( Entity @ent )
{
	@ent.use = target_starttimer_use;
}

void target_startTimer( Entity @ent )
{
	target_starttimer( ent );
}
