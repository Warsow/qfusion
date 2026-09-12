/*
Copyright (C) 2009-2010 Chasseur de bots

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

const int PRIMARY_BIT_RL = 0x1;
const int PRIMARY_BIT_LG = 0x2;
const int PRIMARY_BIT_EB = 0x4;

const int SECONDARY_NONE = -1; // used for pending test
const int SECONDARY_MIN = 0;
const int SECONDARY_PG = 0;
const int SECONDARY_RG = 1;
const int SECONDARY_MG = 2;
const int SECONDARY_GL = 3;
const int SECONDARY_GB = 4;
const int SECONDARY_MAX = 4;

const int AMMO_EB = 15;
const int AMMO_RL = 15;
const int AMMO_LG = 180;
const int AMMO_PG = 140;
const int AMMO_RG = 15;
const int AMMO_MG = 150;
const int AMMO_GL = 10;
const int AMMO_GB = 1; // might as well spawn with it fully charged

const float PLAYER_ARMOR = 100.0f;

cPlayer@[] players( maxClients ); // array of handles
bool playersInitialized = false;

class cPlayer
{
	Client @client;

	int weapPrimary;
	int weapSecondary;

	int killsThisRound; // int to avoid mismatch and honestly, could anyone but me get 2 trillion kills

	uint arms; // hopefully 2...
	uint defuses;

	bool dueToSpawn; // used for respawning during countdown

	bool isCarrier;

	uint oneVS;	// 1vs this many enemies on this round
	
	cPlayer( Client @player )
	{
		@this.client = @player;

		this.weapPrimary = PRIMARY_BIT_RL | PRIMARY_BIT_EB;
		this.weapSecondary = SECONDARY_MIN;

		this.arms = 0;
		this.defuses = 0;

		this.dueToSpawn = false;

		this.isCarrier = false;
		
		this.oneVS = 0;

		@players[player.playerNum] = @this;
	}

	void giveInventory()
	{
		this.client.inventoryClear();

		if ( gametype.isInstagib )
		{
			this.client.inventoryGiveItem( WEAP_INSTAGUN );

			this.client.inventorySetCount( AMMO_INSTAS, 1 );
			this.client.inventorySetCount( AMMO_WEAK_INSTAS, 1 );

			this.client.selectWeapon( -1 );

			return;
		}

		this.client.inventorySetCount( WEAP_GUNBLADE, 1 );
		this.client.armor = PLAYER_ARMOR;

		// XXX: old bomb would set the player's model depending on their
		//      primary weapon but i don't see the point

		// it dies if you don't cast...
		if( ( int( this.weapPrimary ) & PRIMARY_BIT_RL ) != 0 )
		{
			this.client.inventoryGiveItem( WEAP_ROCKETLAUNCHER );
			this.client.inventorySetCount( AMMO_ROCKETS, AMMO_RL );
			this.client.inventorySetCount( AMMO_WEAK_ROCKETS, 0 );
		}
		if( ( int( this.weapPrimary ) & PRIMARY_BIT_LG ) != 0 )
		{
			this.client.inventoryGiveItem( WEAP_LASERGUN );
			this.client.inventorySetCount( AMMO_LASERS, AMMO_LG );
			this.client.inventorySetCount( AMMO_WEAK_LASERS, 0 );
		}
		if( ( int( this.weapPrimary ) & PRIMARY_BIT_EB ) != 0 )
		{
			this.client.inventoryGiveItem( WEAP_ELECTROBOLT );
			this.client.inventorySetCount( AMMO_BOLTS, AMMO_EB );
			this.client.inventorySetCount( AMMO_WEAK_BOLTS, 0 );
		}

		switch ( int( this.weapSecondary ) )
		{
			case SECONDARY_PG:
				this.client.inventoryGiveItem( WEAP_PLASMAGUN );

				this.client.inventorySetCount( AMMO_PLASMA, AMMO_PG );

				this.client.inventorySetCount( AMMO_WEAK_PLASMA, 0 );

				break;

			case SECONDARY_RG:
				this.client.inventoryGiveItem( WEAP_RIOTGUN );

				this.client.inventorySetCount( AMMO_SHELLS, AMMO_RG );

				this.client.inventorySetCount( AMMO_WEAK_SHELLS, 0 );

				break;

			case SECONDARY_MG:
				this.client.inventoryGiveItem( WEAP_MACHINEGUN );

				this.client.inventorySetCount( AMMO_BULLETS, AMMO_MG );

				this.client.inventorySetCount( AMMO_WEAK_BULLETS, 0 );

				break;

			case SECONDARY_GL:
				this.client.inventoryGiveItem( WEAP_GRENADELAUNCHER );

				this.client.inventorySetCount( AMMO_GRENADES, AMMO_GL );

				this.client.inventorySetCount( AMMO_WEAK_GRENADES, 0 );

				break;

			case SECONDARY_GB:
				this.client.inventorySetCount( AMMO_GUNBLADE, AMMO_GB );

				break;

			default:
				assert( false, "player.as giveInventory: bad secondary weapon" );

				break;
		}

		this.client.selectWeapon( -1 );
	}

	void sendOptionsStatus()
	{
		String command = "optionsstatus \"" + ( isCarrier ? "1" : "0" );
		if ( !gametype.isInstagib )
		{
			command += " " + this.weapPrimary;
			command += " " + this.weapSecondary;
		}
		command += "\"";
		this.client.execServerCommand( command );
	}

	bool setCarrier( bool requested )
	{
		if ( this.isCarrier != requested )
		{
			this.isCarrier = requested;
			this.sendOptionsStatus();
			return true;
		}
		return false;
	}

	bool isValidPrimary( int weapon )
	{
		if( ( weapon & ~( PRIMARY_BIT_RL | PRIMARY_BIT_LG | PRIMARY_BIT_EB ) ) == 0 )
		{
			// TODO: Is there popcnt()?
			int setBitsCount = 0;
			if( ( weapon & PRIMARY_BIT_RL ) != 0 )
				setBitsCount++;
			if( ( weapon & PRIMARY_BIT_LG ) != 0 )
				setBitsCount++;
			if( ( weapon & PRIMARY_BIT_EB ) != 0 )
				setBitsCount++;
			if ( setBitsCount == 2 )
				return true;
		}
		return false;
	}

	bool selectPrimary( int weapon )
	{
		assert( isValidPrimary( weapon ), "Illegal primary weapon value" );
		if ( this.weapPrimary != weapon )
		{
			this.weapPrimary = weapon;
			this.sendOptionsStatus();
			return true;
		}
		return false;
	}

	bool isValidSecondary( int weapon )
	{
		return weapon >= SECONDARY_MIN && weapon <= SECONDARY_MAX;
	}

	bool selectSecondary( int weapon )
	{
		assert( isValidSecondary( weapon ), "Illegal secondary weapon value" );
		if ( this.weapSecondary != weapon )
		{
			this.weapSecondary = weapon;
			this.sendOptionsStatus();
			return true;
		}
		return false;
	}

	void selectRandomBotWeapons()
	{
		// Prefer EB + LG
		if ( random() < 0.7f )
		{
			this.weapPrimary = PRIMARY_BIT_LG | PRIMARY_BIT_EB;
			// Choose RG to compensate lack of RL
			if ( random() < 0.7f )
				this.weapSecondary = SECONDARY_RG;
			else if ( random() < 0.7f )
				this.weapSecondary = SECONDARY_PG;
			else
				this.weapSecondary = SECONDARY_GL;
		}
		// Otherwise prefer EB + RL
		else if ( random() < 0.7f )
		{
			this.weapPrimary = PRIMARY_BIT_RL | PRIMARY_BIT_EB;
			// Choose PG to compensate lack of continous fire weapons
			if ( random() < 0.7f )
				this.weapSecondary = SECONDARY_PG;
			else
				this.weapSecondary = SECONDARY_MG;
		}
		// RL + LG
		else
		{
			this.weapPrimary = PRIMARY_BIT_RL | PRIMARY_BIT_LG;
			// Choose MG to compensate lack of long-range weapons
			this.weapSecondary = SECONDARY_MG;
		}
	}
}

// since i am using an array of handles this must
// be done to avoid null references if there are players
// already on the server
void playersInit()
{
	// do initial setup (that doesn't spawn any entities, but needs clients to be created) only once, not every round
	if( !playersInitialized )
	{
		for ( int i = 0; i < maxClients; i++ )
		{
			Client @client = @G_GetClient( i );

			if ( client.state() >= CS_CONNECTING )
			{
				cPlayer( @client );
			}
		}

		playersInitialized = true;
	}
}

// using a global counter would be faster
uint getCarrierCount( int teamNum )
{
	uint count = 0;

	Team @team = @G_GetTeam( teamNum );

	for ( int i = 0; @team.ent( i ) != null; i++ )
	{
		Client @client = @team.ent( i ).client; // stupid AS...
		cPlayer @player = @playerFromClient( @client );

		if ( player.isCarrier )
		{
			count++;
		}
	}

	return count;
}

void resetKillCounters()
{
	for ( int i = 0; i < maxClients; i++ )
	{
		if ( @players[i] != null )
		{
			players[i].killsThisRound = 0;
			players[i].oneVS = 0;
		}
	}
}

cPlayer @playerFromClient( Client @client )
{
	cPlayer @player = @players[client.playerNum];

	// XXX: as of 0.18 this check shouldn't be needed as playersInit works
	if ( @player == null )
	{
		assert( false, "player.as playerFromClient: no player exists for client - state: " + client.state() );

		return cPlayer( @client );
	}

	return @player;
}

void team_CTF_genericSpawnpoint( Entity @ent, int team )
{
	ent.team = team;

	Trace trace;

	Vec3 start, end;
	Vec3 mins( -16, -16, -24 ), maxs( 16, 16, 40 );

	start = end = ent.origin;

	start.z += 16;
	end.z -= 1024;

	trace.doTrace( start, mins, maxs, end, ent.entNum, MASK_SOLID );

	if ( trace.startSolid )
	{
		G_Print( ent.classname + " starts inside solid, removing...\n" );

		ent.freeEntity();

		return;
	}

	if ( ent.spawnFlags & 1 == 0 )
	{
		// move it 1 unit away from the plane

		ent.origin = trace.endPos + trace.planeNormal;
	}
}

void team_CTF_alphaspawn( Entity @ent )
{
	team_CTF_genericSpawnpoint( ent, defendingTeam );
}

void team_CTF_betaspawn( Entity @ent )
{
	team_CTF_genericSpawnpoint( ent, attackingTeam );
}

void team_CTF_alphaplayer( Entity @ent )
{
	team_CTF_genericSpawnpoint( ent, defendingTeam );
}

void team_CTF_betaplayer( Entity @ent )
{
	team_CTF_genericSpawnpoint( ent, attackingTeam );
}
