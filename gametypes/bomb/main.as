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

// XXX: i waive all rights to this code and the warsow dev team can do
//      what they like with it etc cba to paste gpl

// XXX: should all timers be instant in ibomb?

// XXX: i've marked various things to fix in the future
//      with "FIXME type" where type is one of
//          enum:  should be changed when enums work with 64bit AS
//
//          pure:  should be changed when pk3s are packaged correctly so the
//                 unpure pk3 doesn't get marked as pure and break custom xhairs etc

// TODO: organise this crap
//       maybe move to constants.as

const uint BOMB_MAX_PLANT_SPEED = 400; // can't plant above this speed

const uint BOMB_DROP_RETAKE_DELAY = 1000; // time (ms) after dropping before you can retake it

// jit cries if i use COLOR_RGBA so readability can go suck it
//                                     RED            GREEN          BLUE          ALPHA
const int BOMB_LIGHT_INACTIVE = int( ( 255 << 0 ) | ( 255 << 8 ) | ( 0 << 16 ) | ( 128 << 24 ) ); // yellow
const int BOMB_LIGHT_ARMED    = int( ( 255 << 0 ) | (   0 << 8 ) | ( 0 << 16 ) | ( 128 << 24 ) ); // red

// min cos(ang) between ground and up to plant
// 0.90 gives ~26 degrees max slope
// setting this too low breaks the roofplant @ alley b
const float BOMB_MIN_DOT_GROUND = 0.90f;

const Vec3 VEC_UP( 0, 0, 1 ); // this must have length 1! don't change this unless +z is no longer up...

const float BOMB_ARM_DEFUSE_RADIUS = 100.0f; // it makes no sense for the two to be different

const uint BOMB_SPRITE_RESIZE_TIME = 300; // time taken to expand/shrink sprite/decal

const float BOMB_BEEP_FRACTION = 1.0f / 12.0f; // fraction of time left between beeps
const uint BOMB_BEEP_MAX = 5000;               // max time (ms) between beeps
const uint BOMB_BEEP_MIN = 200;                // min time (ms) between beeps

const uint BOMB_HURRYUP_TIME = 12000;

const uint BOMB_AUTODROP_DISTANCE = 400; // distance from indicator to drop (only some maps)

const uint BOMB_THROW_SPEED = 300; // speed at which the bomb is thrown with drop

const uint BOMB_EXPLOSION_EFFECT_RADIUS = 256;

const uint BOMB_DEAD_CAMERA_DIST = 256;

const uint IMPRESSIVE_KILLS = 3; // how many kills before they are deemed impressive

const uint POINTS_DEFUSE = 0;

const int INITIAL_ATTACKERS = TEAM_ALPHA;
const int INITIAL_DEFENDERS = TEAM_BETA;

const uint SITE_EXPLOSION_POINTS   = 30;
const uint SITE_AVERAGE_EXPLOSIONS = 20;

const uint SITE_EXPLOSION_MAX_DELAY = 1500; // XXX THIS MUST BE BIGGER THAN BOMB_SPRITE_RESIZE_TIME OR EVERYTHING DIES FIXME?

const float SITE_EXPLOSION_PROBABILITY = float( SITE_AVERAGE_EXPLOSIONS ) / float( SITE_EXPLOSION_POINTS );
const float SITE_EXPLOSION_MAX_DIST    = 512.0f;

// jit cries if i use const
Vec3 BOMB_MINS( -16, -16, -8 );
Vec3 BOMB_MAXS(  16,  16, 48 ); // same size as player i guess

// Bots
cBombSite @BOMB_BOTS_SITE = null;

// cvars
Cvar cvarRoundTime(       "g_bomb_roundtime",       "60", CVAR_ARCHIVE );
Cvar cvarExplodeTime(     "g_bomb_bombtimer",       "30", CVAR_ARCHIVE );
Cvar cvarArmTime(         "g_bomb_armtime",         "4",  CVAR_ARCHIVE );
Cvar cvarDefuseTime(      "g_bomb_defusetime",      "7",  CVAR_ARCHIVE );
Cvar cvarEnableCarriers(  "g_bomb_carriers",        "1",  CVAR_ARCHIVE );
Cvar cvarSpawnProtection( "g_bomb_spawnprotection", "3",  CVAR_ARCHIVE );

// read from this later
Cvar cvarScoreLimit( "g_scorelimit", "16", CVAR_ARCHIVE );

// why doesn't AS have char arithmetic...
const String[] SITE_LETTERS = { 'A', 'B' };

const int COUNTDOWN_MAX = 6; // was 4, but this gives people more time to change weapons

// TODO: Add different assets
const int hudIconBomb = 1;

// this should really kill the program
// but i'm mostly using it as an indicator that it's about to die anyway
void assert( const bool test, const String msg )
{
	if ( !test )
	{
		G_Print( S_COLOR_RED + "assert failed [" + gametype.name + " " + gametype.version + "]: " + msg + "\n" );
	}
}

void setTeamProgressAndAnim( int teamNum, int progress, int anim )
{
	assert( teamNum == TEAM_ALPHA || teamNum == TEAM_BETA, "Illegal team" );
	Team @team = @G_GetTeam( teamNum );

	for ( int i = 0; @team.ent( i ) != null; i++ )
	{
		Entity @ent = @team.ent( i );
		if ( ent.team == teamNum )
		{
			ent.client.setHUDStat( STAT_INDICATOR_3_PROGRESS, progress );
			ent.client.setHUDStat( STAT_INDICATOR_3_ANIM, anim );
		}
	}
}

bool GT_Command( Client @client, const String &cmdString, const String &argsString, int argc )
{
	if ( cmdString == "drop" )
	{
		for ( int i = 0; i < argc; i++ )
		{
			String token = argsString.getToken( i );

			if ( token.len() == 0 )
			{
				break;
			}

			if ( token == "bomb" || token == "flag" )
			{
				if ( @client.getEnt() == @bombCarrier )
				{
					bombDrop( BOMBDROP_NORMAL );
				}
			}
		}

		return true;
	}

	if ( cmdString == "carrier" )
	{
		cPlayer @player = @playerFromClient( @client );
		const String token = argsString.getToken( 0 );
		if ( token.len() != 0 )
		{
			const bool requestedStatus = token.toInt() != 0;
			G_Print( "Token " + token + " \n" );
			if ( player.setCarrier( requestedStatus ) )
			{
				if ( player.isCarrier )
				{
					G_PrintMsg( @client.getEnt(), "You are now a bomb carrier!\n" );
				}
				else
				{
					G_PrintMsg( @client.getEnt(), "You are no longer a bomb carrier.\n" );
				}
			}
		}
		else
		{
			// We've decided to disable toggling the status by default.
			G_PrintMsg( @client.getEnt(), "Malformed command (the argument is missing)\n" );
		}

		return true;
	}

	if ( cmdString == "requestoptionsstatus" )
	{
		playerFromClient( @client ).sendOptionsStatus();
		return true;
	}

	if ( cmdString == "primary" )
	{
		const int value = argsString.getToken( 0 ).toInt();
		cPlayer @player = @playerFromClient( @client );
		if ( player.isValidPrimary( value ) )
		{
			if ( player.selectPrimary( value ) )
			{
				if ( roundState == ROUNDSTATE_PRE )
					player.giveInventory();
			}
		}
		else
		{
			G_PrintMsg( @client.getEnt(), "Malformed command\n" );
		}
		return true;
	}

	if ( cmdString == "secondary" )
	{
		const int value = argsString.getToken( 0 ).toInt();
		cPlayer @player = @playerFromClient( @client );
		if ( player.isValidSecondary( value ) )
		{
			if ( player.selectSecondary( value ) )
			{
				if ( roundState == ROUNDSTATE_PRE )
					player.giveInventory();
			}
		}
		else
		{
			G_PrintMsg( @client.getEnt(), "Malformed command\n" );
		}
		return true;
	}

	if ( cmdString == "gametype" )
	{
		String response = "";
		Cvar fs_game( "fs_game", "", 0 );
		String manifest = gametype.manifest;

		response += "\n";
		response += "Gametype " + gametype.name + " : " + gametype.title + "\n";
		response += "----------------\n";
		response += "Version: " + gametype.version + "\n";
		response += "Author: " + gametype.author + "\n";
		response += "Mod: " + fs_game.string + ( !manifest.empty() ? " (manifest: " + manifest + ")" : "" ) + "\n";
		response += "----------------\n";

		G_PrintMsg( client.getEnt(), response );

		return true;
	}

	if ( cmdString == "cvarinfo" )
	{
		GENERIC_CheatVarResponse( @client, cmdString, argsString, argc );

		return true;
	}

	if ( cmdString == "callvotevalidate" )
	{
		String votename = argsString.getToken( 0 );

		client.printMessage( "Unknown callvote " + votename + "\n" );

		return false;

	}

	if ( cmdString == "callvotepassed" )
	{
		String votename = argsString.getToken( 0 );

		return true;
	}

	return false;
}

Entity @GT_SelectSpawnPoint( Entity @self )
{
	if ( self.team == attackingTeam )
	{
		return GENERIC_SelectBestRandomSpawnPoint( @self, "team_CTF_betaspawn" );
	}

	return GENERIC_SelectBestRandomSpawnPoint( @self, "team_CTF_alphaspawn" );
}

void GT_updateScore( Client @client )
{
	cPlayer @player = @playerFromClient( @client );
	Stats @stats = @client.stats;

	if ( gametype.isInstagib )
	{
		client.stats.setScore(
			stats.getEntry( "frags" ) - stats.getEntry( "team_frags" )
			+ player.defuses * POINTS_DEFUSE
		);

		return;
	}

	// 2 * teamDamage because totalDamage includes it
	client.stats.setScore( int(
		( stats.getEntry( "frags" ) - stats.getEntry( "team_frags" ) ) * 0.5
		+ ( stats.getEntry( "damage_given" ) - 2 * stats.getEntry( "team_damage_given" ) ) * 0.01
		+ player.defuses * POINTS_DEFUSE )
	);
}

// Some game actions trigger score events. These are events not related to killing
// oponents, like capturing a flag
void GT_ScoreEvent( Client @client, const String &score_event, const String &args )
{
	// XXX: i think this can be called but if the client then
	//      doesn't connect (ie hits escape/crashes) then the
	//      disconnect score event doesn't fire, which leaves
	//      us with a teeny tiny bit of wasted memory
	if ( score_event == "connect" )
	{
		// why not
		cPlayer( @client );

		return;
	}

	if ( score_event == "dmg" )
	{
		// this does happen when you shoot corpses or something
		if ( @client == null )
		{
			return;
		}

		GT_updateScore( @client );

		// spawn protection

		if ( match.getState() != MATCH_STATE_PLAYTIME || roundState != ROUNDSTATE_ROUND )
		{
			return;
		}

		int protectTime = cvarSpawnProtection.integer;

		if ( protectTime <= 0 )
		{
			return;
		}

		int elapsedTime = int( ( levelTime - roundStartTime ) * 0.001f );

		if ( elapsedTime > protectTime )
		{
			return;
		}

		Entity @attacker = @client.getEnt();
		Entity @victim = @G_GetEntity( args.getToken( 0 ).toInt() );

		if ( @victim == null || @victim == @attacker || victim.team != attacker.team
			|| attacker.isGhosting() || attacker.health < 0 // becase every rg pellet counts as a dmg event...
		)
		{
			return;
		}

		// WE'VE MADE IT THIS FAR

		float damage = args.getToken( 1 ).toFloat();

		assert( damage > 0, "main.as GT_ScoreEvent: damage < 0" );

		int protectTimeO3 = protectTime / 3; // protectTime over 3

		// TODO: smooth damage scaling?
		if ( elapsedTime < protectTimeO3 )
		{
			damage *= 2.0f; // double damage in first third of spawn protection
		}
		else if ( elapsedTime > protectTimeO3 * 2 )
		{
			damage /= 2.0f; // half damage in last third of spawn protection
		}

		attacker.health -= damage;

		int newArmor = int( client.armor - damage * 0.5 );

		if ( newArmor < 0 )
		{
			newArmor = 0;
		}

		client.armor = float( newArmor );

		// HUMILIATION
		if ( attacker.health < 0 )
		{
			if ( @attacker == @bombCarrier )
			{
				bombDrop( BOMBDROP_KILLED );
			}

			G_PrintMsg( null, client.name + S_COLOR_RED + " was punished for teamdamage!\n" );

			G_CenterPrintMsg( attacker, S_COLOR_RED + "Teamdamage punishment!" );

			attacker.explosionEffect( 128 ); // :)
		}

		return;
	}

	if ( score_event == "kill" )
	{
		Entity @attacker = null;
		
		if ( @client != null )
		{
			@attacker = @client.getEnt();

			GT_updateScore( @client );
		}

		Entity @victim = @G_GetEntity( args.getToken( 0 ).toInt() );
		Entity @inflictor = @G_GetEntity( args.getToken( 1 ).toInt() );

		playerKilled( @victim, @attacker, @inflictor );

		return;
	}
	
	if ( score_event == "disconnect" )
	{
		// clean up
		@players[client.playerNum] = null;

		return;
	}
	
	if( score_event == "rebalance" || score_event == "shuffle" )
	{
		// end round when in match
		if ( ( @client == null ) && ( match.getState() == MATCH_STATE_PLAYTIME ) )
		{
			roundNewState( ROUNDSTATE_FINISHED );
		}	
	}	
}

// a player is being respawned. This can happen from several ways, as dying, changing team,
// being moved to ghost state, be placed in respawn queue, being spawned from spawn queue, etc
void GT_PlayerRespawn( Entity @ent, int old_team, int new_team )
{
	Client @client = @ent.client;
	cPlayer @player = @playerFromClient( @client );

	int matchState = match.getState();

	if ( new_team != old_team )
	{
		if ( bombState == BOMBSTATE_CARRIED && @ent == @bombCarrier )
		{
			bombDrop( BOMBDROP_TEAM );
		}

		if ( !gametype.isInstagib )
		{
			if ( @client.getBot() != null )
			{
				player.selectRandomBotWeapons();
			}
		}

		if ( matchState == MATCH_STATE_PLAYTIME )
		{
			if ( roundState == ROUNDSTATE_ROUND )
			{
				if ( old_team != TEAM_SPECTATOR )
				{
					checkPlayersAlive( old_team );
				}
			}
			else if ( roundState == ROUNDSTATE_PRE && new_team != TEAM_SPECTATOR )
			{
				// respawn during countdown
				// mark for respawning next frame because
				// respawning this frame doesn't work

				player.dueToSpawn = true;
			}
		}
	}
	else if ( roundState == ROUNDSTATE_PRE )
	{
		disableMovementFor( @client );
	}

	if ( ent.isGhosting() )
	{
		ent.svflags &= ~SVF_FORCETEAM;
		GENERIC_ClearQuickMenu( @client );
		return;
	}

	player.giveInventory();
	
	ent.svflags |= SVF_FORCETEAM;
	ent.respawnEffect();
}

// Thinking function. Called each frame
void GT_ThinkRules()
{
	// XXX: old bomb would let the current round finish before doing this
	if ( match.timeLimitHit() || match.suddenDeathFinished() )
	{
		match.launchState( match.getState() + 1 );
	}

	for ( int t = 0; t < GS_MAX_TEAMS; t++ )
	{
		Team @team = @G_GetTeam( t );

		for ( int i = 0; @team.ent( i ) != null; i++ )
		{
			Client @client = @team.ent( i ).client;
			cPlayer @player = @playerFromClient( @client );

			// this should only happen when match state is playtime
			// but i put this up here since i'm calling playerFromClient
			if ( player.dueToSpawn )
			{
				client.respawn( false );

				player.dueToSpawn = false;

				continue;
			}
		}
	}

    // Clear the state for robustness purposes
	for( int i = 0; i < maxClients; ++i )
	{
		Client @client = @G_GetClient( i );
		client.setHUDStat( STAT_INDICATOR_1_ENABLED, 0 );
		client.setHUDStat( STAT_INDICATOR_2_ENABLED, 0 );
		client.setHUDStat( STAT_INDICATOR_3_ENABLED, 1 );
		client.setHUDStat( STAT_INDICATOR_3_ANIM, HUD_INDICATOR_NO_ANIM );
		client.setHUDStat( STAT_INDICATOR_3_ICON, hudIconBomb );
		client.setHUDStat( STAT_INDICATOR_3_PROGRESS, 0 );
	}

	if ( match.getState() < MATCH_STATE_PLAYTIME )
	{
		return;
	}

	roundThink();

	uint aliveAlpha = playersAliveOnTeam( TEAM_ALPHA );
	uint aliveBeta  = playersAliveOnTeam( TEAM_BETA );

	// i guess you could speed this up...
	if ( bombState == BOMBSTATE_ARMED )
	{
		uint aliveOff = TEAM_ALPHA == attackingTeam ? aliveAlpha : aliveBeta;

		if ( aliveOff == 0 )
		{
			Team @team = @G_GetTeam( attackingTeam );

			for ( int i = 0; @team.ent( i ) != null; i++ )
			{
				bombLookAt( @team.ent( i ) );
			}
		}
	}
	
	if ( bombState == BOMBSTATE_CARRIED )
	{
		bombCarrierLastPos = bombCarrier.origin;
		bombCarrierLastVel = bombCarrier.velocity;
	}

	GENERIC_Think();

	BOMB_UpdateBotsExtraGoals();
}

// The game has detected the end of the match state, but it
// doesn't advance it before calling this function.
// This function must give permission to move into the next
// state by returning true.
bool GT_MatchStateFinished( int incomingMatchState )
{
	if ( match.getState() <= MATCH_STATE_WARMUP && incomingMatchState > MATCH_STATE_WARMUP && incomingMatchState < MATCH_STATE_POSTMATCH )
	{
		match.startAutorecord();
	}

	if ( match.getState() == MATCH_STATE_POSTMATCH )
	{
		match.stopAutorecord();
	}

	return true;
}

// the match state has just moved into a new state. Here is the
void GT_MatchStateStarted()
{
	switch ( match.getState() )
	{
		case MATCH_STATE_WARMUP:
			GENERIC_SetUpWarmup();

			attackingTeam = INITIAL_ATTACKERS;
			defendingTeam = INITIAL_DEFENDERS;

			for ( int t = TEAM_PLAYERS; t < GS_MAX_TEAMS; t++ )
			{
				gametype.setTeamSpawnsystem( t, SPAWNSYSTEM_INSTANT, 0, 0, false );
			}

			break;

		case MATCH_STATE_COUNTDOWN:		
			// XXX: old bomb had its own function to do pretty much
			//      exactly the same thing
			//      the only difference i can see is that bomb's
			//      didn't respawn all items, but that doesn't
			//      matter cause there aren't any
			GENERIC_SetUpCountdown();

			break;

		case MATCH_STATE_PLAYTIME:
			newGame();

			break;

		case MATCH_STATE_POSTMATCH:
			endGame();

			break;

		default:
			// do nothing

			break;
	}
}

// the gametype is shutting down cause of a match restart or map change
void GT_Shutdown()
{
}

// The map entities have just been spawned. The level is initialized for
// playing, but nothing has yet started.
void GT_SpawnGametype()
{
	bombInit();

	playersInit();
}

// Important: This function is called before any entity is spawned, and
// spawning entities from it is forbidden. ifyou want to make any entity
// spawning at initialization do it in GT_SpawnGametype, which is called
// right after the map entities spawning.
void GT_InitGametype()
{
	gametype.title = "Bomb and Defuse";
	gametype.version = "1.0";
	gametype.author = "Warsow Development Team";

	gametype.spawnableItemsMask = 0;
	gametype.respawnableItemsMask = 0;
	gametype.dropableItemsMask = 0; // XXX: old bomb lets you drop ammo
	gametype.pickableItemsMask = 0;

	gametype.isTeamBased = true;
	gametype.isRace = false;
	gametype.hasChallengersQueue = false;
	gametype.maxPlayersPerTeam = 0;

	gametype.ammoRespawn = 20;
	gametype.armorRespawn = 25;
	gametype.weaponRespawn = 15;
	gametype.healthRespawn = 25;
	gametype.powerupRespawn = 90;
	gametype.megahealthRespawn = 20;
	gametype.ultrahealthRespawn = 60;

	gametype.readyAnnouncementEnabled = false;
	gametype.scoreAnnouncementEnabled = true;
	gametype.countdownEnabled = false;
	gametype.mathAbortDisabled = false;
	gametype.shootingDisabled = false;
	gametype.infiniteAmmo = false;
	gametype.canForceModels = true;
	gametype.canShowMinimap = true;
	gametype.teamOnlyMinimap = true;
	gametype.removeInactivePlayers = true;

	gametype.mmCompatible = true;
	
	gametype.spawnpointRadius = 256;

	// set spawnsystem type to instant while players join
	for ( int t = TEAM_PLAYERS; t < GS_MAX_TEAMS; t++ )
	{
		gametype.setTeamSpawnsystem( t, SPAWNSYSTEM_INSTANT, 0, 0, false );
	}

	scoreboard.beginDefiningSchema();
	scoreboard.endDefiningSchema();

    // TODO: Make proper different assets
	G_ConfigString( CS_GENERAL + hudIconBomb - 1, "gfx/bomb/carriericon" );

	// add commands
	G_RegisterCommand( "drop" );
	G_RegisterCommand( "carrier" );

	G_RegisterCommand( "gametype" );

	G_RegisterCommand( "requestoptionsstatus" );

	String carrierOption = "Be a preferred bomb carrier | Exactly1OfList | carrier |";
	carrierOption += "Off, gfx/hud/icons/vsay/no, On, gfx/bomb/carriericon_base";
	G_ConfigString( CS_GAMETYPE_OPTIONS, carrierOption );
	if( !gametype.isInstagib )
	{
		G_RegisterCommand( "primary" );
		String primaryOption = "Primary weapon | Exactly2OfList | primary |";
		primaryOption += " RL, gfx/hud/icons/weapon/rocket";
		primaryOption += ",LG, gfx/hud/icons/weapon/laser";
		primaryOption += ",EB, gfx/hud/icons/weapon/electro";
		G_ConfigString( CS_GAMETYPE_OPTIONS + 1, primaryOption );

		G_RegisterCommand( "secondary" );
		String secondaryOption = "Secondary weapon | Exactly1OfList | secondary |";
		secondaryOption += " PG, gfx/hud/icons/weapon/plasma";
		secondaryOption += ",RG, gfx/hud/icons/weapon/riot";
		secondaryOption += ",MG, gfx/hud/icons/weapon/machinegun";
		secondaryOption += ",GL, gfx/hud/icons/weapon/grenade";
		// TODO: Unicode * - there's no such glyph in our base font
		secondaryOption += ",GB*, gfx/hud/icons/weapon/gunblade_blast";
		G_ConfigString( CS_GAMETYPE_OPTIONS + 2, secondaryOption );
	}

	// add callvotes
	// XXX: vic says no to callvotes
	//G_RegisterCallvote( "roundtime", "> 0", "integer", "How long the attacking team gets to plant and arm the bomb (seconds)" );
	//G_RegisterCallvote( "bombtimer", "> 0", "integer", "The bomb's fuse length (seconds)" );

	mediaInit();

	/*if(!G_FileExists("configs/server/gametypes/" + gametype.name + ".cfg")) {
		String config;
		// the config file doesn't exist or it's empty, create it
		config = "// '" + gametype.title + "' gametype configuration file\n"
			+ "// This config will be executed each time the gametype is started\n"
			+ "\n// " + gametype.title + " specific settings\n"
			+ "set ftag_allowPowerups \"1\"\n"
			+ "set ftag_powerupDrop \"1\"\n"
			+ "\n// map rotation\n"
			+ "set g_maplist \"wdm1 wdm2 wdm4 wdm5 wdm6 wdm7 wdm9 wdm10 wdm11 wdm12 wdm13 wdm14 wdm15 wdm16 wdm17\" // list of maps in automatic rotation\n"
			+ "set g_maprotation \"1\"   // 0 = same map, 1 = in order, 2 = random\n"
			+ "\n// game settings\n"
			+ "set g_scorelimit \"15\"\n"
			+ "set g_timelimit \"0\"\n"
			+ "set g_warmup_enabled \"1\"\n"
			+ "set g_warmup_timelimit \"1.5\"\n"
			+ "set g_match_extendedtime \"0\"\n"
			+ "set g_allow_falldamage \"1\"\n"
			+ "set g_allow_selfdamage \"1\"\n"
			+ "set g_allow_teamdamage \"1\"\n"
			+ "set g_allow_stun \"1\"\n"
			+ "set g_teams_maxplayers \"0\"\n"
			+ "set g_teams_allow_uneven \"0\"\n"
			+ "set g_countdown_time \"5\"\n"
			+ "set g_maxtimeouts \"3\" // -1 = unlimited\n"
			+ "\necho \"" + gametype.name + ".cfg executed\"\n";

		G_WriteFile("configs/server/gametypes/" + gametype.name + ".cfg", config);
		G_Print("Created default config file for '" + gametype.name + "'\n");
		G_CmdExecute("exec configs/server/gametypes/" + gametype.name + ".cfg silent");
	}*/

	G_CmdExecute( "exec configs/server/gametypes/bomb.cfg silent" ); // TODO XXX FIXME

	G_Print( "Gametype '" + gametype.title + "' initialized\n" );
}
