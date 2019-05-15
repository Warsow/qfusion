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

int numCheckpoints = 0;
bool demoRecording = false;
const int MAX_RECORDS = 30;
const int HUD_RECORDS = 3;

String levelRecordPlayerName;

enum eMenuItems
{
	MI_EMPTY,
	MI_RESTART_RACE,
	MI_ENTER_PRACTICE,
	MI_LEAVE_PRACTICE,
	MI_NOCLIP_ON,
	MI_NOCLIP_OFF,
	MI_SAVE_POSITION,
	MI_LOAD_POSITION,
	MI_CLEAR_POSITION
};

array<const String @> menuItems = { 
	'"" ""',
	'"Restart race" "racerestart"',
	'"Enter practice mode" "practicemode" ',
	'"Leave practice mode" "practicemode" ',
	'"Enable noclip mode" "noclip" ',
	'"Disable noclip mode" "noclip" ',
	'"Save position" "position save" ',
	'"Load saved position" "position load" ',
	'"Clear saved position" "position clear" '
};

///*****************************************************************
/// LOCAL FUNCTIONS
///*****************************************************************

String RACE_TimeToString( uint time )
{
	// convert times to printable form
	String minsString, secsString, millString;
	uint min, sec, milli;

	milli = time;
	min = milli / 60000;
	milli -= min * 60000;
	sec = milli / 1000;
	milli -= sec * 1000;

	if ( min == 0 )
		minsString = "00";
	else if ( min < 10 )
		minsString = "0" + min;
	else
		minsString = min;

	if ( sec == 0 )
		secsString = "00";
	else if ( sec < 10 )
		secsString = "0" + sec;
	else
		secsString = sec;

	if ( milli == 0 )
		millString = "000";
	else if ( milli < 10 )
		millString = "00" + milli;
	else if ( milli < 100 )
		millString = "0" + milli;
	else
		millString = milli;

	return minsString + ":" + secsString + "." + millString;
}

String RACE_TimeDiffString( uint time, uint reference, bool clean )
{
	if ( reference == 0 && clean )
		return "";

	if ( reference == 0 )
		return S_COLOR_WHITE + "--:--.---";

	if ( time == reference )
		return S_COLOR_YELLOW + "+-" + RACE_TimeToString( 0 );

	if ( time < reference )
		return S_COLOR_GREEN + "-" + RACE_TimeToString( reference - time );

	return S_COLOR_RED + "+" + RACE_TimeToString( time - reference );
}

void RACE_UpdateHUDTopScores()
{
	for ( int i = 0; i < HUD_RECORDS; i++ )
	{
		// Somehow it is not shown the first time if it isn't initialized like this
		G_ConfigString( CS_GENERAL + i, "" );

		RecordTime @record = localRecordsStorage.findRecordByRank( i );
		if ( @record == null )
			continue;

		String timeString = RACE_TimeToString( record.finishTime );
		G_ConfigString( CS_GENERAL + i, "#" + ( i + 1 ) + " - " + record.playerName + " - " + timeString );
	}
}

// a player has just died. The script is warned about it so it can account scores
void RACE_playerKilled( Entity @target, Entity @attacker, Entity @inflicter )
{
	if ( @target == null || @target.client == null )
		return;

	RACE_GetPlayer( target.client ).cancelRace();
}

void RACE_SetUpMatch()
{
	int i, j;
	Entity @ent;
	Team @team;

	gametype.shootingDisabled = false;
	gametype.readyAnnouncementEnabled = false;
	gametype.scoreAnnouncementEnabled = false;
	gametype.countdownEnabled = true;

	gametype.pickableItemsMask = gametype.spawnableItemsMask;
	gametype.dropableItemsMask = gametype.spawnableItemsMask;

	// clear player stats and scores, team scores

	for ( i = TEAM_PLAYERS; i < GS_MAX_TEAMS; i++ )
	{
		@team = G_GetTeam( i );
		team.stats.clear();
	}

	G_RemoveDeadBodies();
}

///*****************************************************************
/// MODULE SCRIPT CALLS
///*****************************************************************

bool GT_Command( Client @client, const String &cmdString, const String &argsString, int argc )
{
	return commandHandlersRegistry.handle( client, cmdString, argsString, argc );
}

// select a spawning point for a player
Entity @GT_SelectSpawnPoint( Entity @self )
{
	return GENERIC_SelectBestRandomSpawnPoint( self, "info_player_deathmatch" );
}

String @GT_ScoreboardMessage( uint maxlen )
{
	String scoreboardMessage = "";
	String entry;
	Team @team;
	Entity @ent;
	Player @player;
	int i, playerID;
	String racing;
	//int readyIcon;

	@team = G_GetTeam( TEAM_PLAYERS );

	// &t = team tab, team tag, team score (doesn't apply), team ping (doesn't apply)
	entry = "&t " + int( TEAM_PLAYERS ) + " 0 " + team.ping + " ";
	if ( scoreboardMessage.length() + entry.length() < maxlen )
		scoreboardMessage += entry;

	// "Name Time Ping Racing"
	for ( i = 0; @team.ent( i ) != null; i++ )
	{
		@ent = team.ent( i );

		@player = RACE_GetPlayer( ent.client );
		if ( player.practicing )
			racing = S_COLOR_CYAN + "No";
		else if ( player.inRace )
			racing = S_COLOR_GREEN + "Yes";
		else
			racing = S_COLOR_RED + "No";

		playerID = ( ent.isGhosting() && ( match.getState() == MATCH_STATE_PLAYTIME ) ) ? -( ent.playerNum + 1 ) : ent.playerNum;
		entry = "&p " + playerID + " " + ent.client.clanName + " "
				+ player.bestFinishTime + " "
				+ ent.client.ping + " " + racing + " ";

		if ( scoreboardMessage.length() + entry.length() < maxlen )
			scoreboardMessage += entry;
	}

	return scoreboardMessage;
}

// Some game actions trigger score events. These are events not related to killing
// oponents, like capturing a flag
// Warning: client can be null
void GT_ScoreEvent( Client @client, const String &score_event, const String &args )
{
	if ( score_event == "dmg" )
	{
	}
	else if ( score_event == "kill" )
	{
		Entity @attacker = null;

		if ( @client != null )
			@attacker = client.getEnt();

		int arg1 = args.getToken( 0 ).toInt();
		int arg2 = args.getToken( 1 ).toInt();

		// target, attacker, inflictor
		RACE_playerKilled( G_GetEntity( arg1 ), attacker, G_GetEntity( arg2 ) );
	}
	else if ( score_event == "award" )
	{
	}
	else if ( score_event == "enterGame" )
	{
		if ( @client != null )
		{
			RACE_GetPlayer( client ).clear();
			RACE_UpdateHUDTopScores();
		}

		// ch : begin fetching records over interweb
		// MM_FetchRaceRecords( client.getEnt() );
	}
	else if ( score_event == "userinfochanged" )
	{
		if ( @client != null )
		{
			String login = client.getUserInfoKey( "cl_mm_login" );
			if ( login != "" )
			{
				RecordTime @record = localRecordsStorage.findRecordByLogin( login );
				if ( @record != null )
				{
					RACE_GetPlayer( client ).takeTimesFromRecord( record );
				}
			}
		}
	}
}

// a player is being respawned. This can happen from several ways, as dying, changing team,
// being moved to ghost state, be placed in respawn queue, being spawned from spawn queue, etc
void GT_PlayerRespawn( Entity @ent, int old_team, int new_team )
{
	Player @player = RACE_GetPlayer( ent.client );
	player.cancelRace();

	player.setQuickMenu();

	if ( ent.isGhosting() )
		return;

	// set player movement to pass through other players
	ent.client.pmoveFeatures = ent.client.pmoveFeatures | PMFEAT_GHOSTMOVE;

	if ( gametype.isInstagib )
		ent.client.inventoryGiveItem( WEAP_INSTAGUN );
	else
		ent.client.inventorySetCount( WEAP_GUNBLADE, 1 );

	// select rocket launcher if available
	if ( ent.client.canSelectWeapon( WEAP_ROCKETLAUNCHER ) )
		ent.client.selectWeapon( WEAP_ROCKETLAUNCHER );
	else
		ent.client.selectWeapon( -1 ); // auto-select best weapon in the inventory

	player.loadPosition( false );

	// add a teleportation effect
	ent.respawnEffect();

	if ( !player.practicing && !player.heardReady )
	{
		int soundIndex = G_SoundIndex( "sounds/announcer/countdown/ready0" + (1 + (rand() & 1)) );
		G_AnnouncerSound( ent.client, soundIndex, GS_MAX_TEAMS, false, null );
		player.heardReady = true;
	}
}

// Thinking function. Called each frame
void GT_ThinkRules()
{
	if ( match.scoreLimitHit() || match.timeLimitHit() || match.suddenDeathFinished() )
		match.launchState( match.getState() + 1 );

	if ( match.getState() >= MATCH_STATE_POSTMATCH )
		return;

	GENERIC_Think();

	if ( match.getState() == MATCH_STATE_PLAYTIME )
	{
		// if there is no player in TEAM_PLAYERS finish the match and restart
		if ( G_GetTeam( TEAM_PLAYERS ).numPlayers == 0 && demoRecording )
		{
			match.stopAutorecord();
			demoRecording = false;
		}
		else if ( !demoRecording && G_GetTeam( TEAM_PLAYERS ).numPlayers > 0 )
		{
			match.startAutorecord();
			demoRecording = true;
		}
	}

	// set all clients race stats
	Client @client;
	Player @player;

	for ( int i = 0; i < maxClients; i++ )
	{
		@client = G_GetClient( i );
		if ( client.state() < CS_SPAWNED )
			continue;

		// disable gunblade autoattack
		client.pmoveFeatures = client.pmoveFeatures & ~PMFEAT_GUNBLADEAUTOATTACK;

		// always clear all before setting
		client.setHUDStat( STAT_PROGRESS_SELF, 0 );
		client.setHUDStat( STAT_PROGRESS_OTHER, 0 );
		client.setHUDStat( STAT_IMAGE_SELF, 0 );
		client.setHUDStat( STAT_IMAGE_OTHER, 0 );
		client.setHUDStat( STAT_PROGRESS_ALPHA, 0 );
		client.setHUDStat( STAT_PROGRESS_BETA, 0 );
		client.setHUDStat( STAT_IMAGE_ALPHA, 0 );
		client.setHUDStat( STAT_IMAGE_BETA, 0 );
		client.setHUDStat( STAT_MESSAGE_SELF, 0 );
		client.setHUDStat( STAT_MESSAGE_OTHER, 0 );
		client.setHUDStat( STAT_MESSAGE_ALPHA, 0 );
		client.setHUDStat( STAT_MESSAGE_BETA, 0 );

		// all stats are set to 0 each frame, so it's only needed to set a stat if it's going to get a value
		@player = RACE_GetPlayer( client );
		if ( player.inRace )
			client.setHUDStat( STAT_TIME_SELF, player.raceTime() / 100 );

		client.setHUDStat( STAT_TIME_BEST, player.bestFinishTime / 100 );

		RecordTime @record = localRecordsStorage.findRecordByRank( 0 );
		if ( @record != null )
			client.setHUDStat( STAT_TIME_RECORD, record.finishTime / 100 );
		else
			client.setHUDStat( STAT_TIME_RECORD, 0 );

		client.setHUDStat( STAT_TIME_ALPHA, -9999 );
		client.setHUDStat( STAT_TIME_BETA, -9999 );

		if ( @record != null )
			client.setHUDStat( STAT_MESSAGE_OTHER, CS_GENERAL );

		@record = localRecordsStorage.findRecordByRank( 1 );
		if ( @record != null )
			client.setHUDStat( STAT_MESSAGE_ALPHA, CS_GENERAL + 1 );

		@record = localRecordsStorage.findRecordByRank( 2 );
		if ( @record != null )
			client.setHUDStat( STAT_MESSAGE_BETA, CS_GENERAL + 2 );

		player.checkContestedRecordStatus();

		// Add a play time at this map if a client is racing/practicing and is running relatively fast.
		// Note: unauthorized clients get rejected at the native engine code level.
		if( !player.inRace && !player.practicing )
			continue;

		Vec3 velocity2D( client.getEnt().velocity );
		velocity2D.z = 0;
		if( velocity2D.length() < 500 )
			continue;

		client.addToRacePlayTime( frameTime );
	}
}

// The game has detected the end of the match state, but it
// doesn't advance it before calling this function.
// This function must give permission to move into the next
// state by returning true.
bool GT_MatchStateFinished( int incomingMatchState )
{
	if ( match.getState() == MATCH_STATE_POSTMATCH )
	{
		match.stopAutorecord();
		demoRecording = false;

		localRecordsStorage.save();

		if ( randmap_passed != "" )
			G_CmdExecute( "map " + randmap_passed );
	}

	return true;
}

// the match state has just moved into a new state. Here is the
// place to set up the new state rules
void GT_MatchStateStarted()
{
	// hettoo : skip warmup and countdown
	if ( match.getState() < MATCH_STATE_PLAYTIME )
	{
		match.launchState( MATCH_STATE_PLAYTIME );
		return;
	}

	switch ( match.getState() )
	{
	case MATCH_STATE_PLAYTIME:
		RACE_SetUpMatch();
		break;

	case MATCH_STATE_POSTMATCH:
		gametype.pickableItemsMask = 0;
		gametype.dropableItemsMask = 0;
		GENERIC_SetUpEndMatch();
		break;

	default:
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
	//G_Print( "numCheckPoints: " + numCheckpoints + "\n" );
	localRecordsStorage.load();
	RACE_UpdateHUDTopScores();
}

// Important: This function is called before any entity is spawned, and
// spawning entities from it is forbidden. If you want to make any entity
// spawning at initialization do it in GT_SpawnGametype, which is called
// right after the map entities spawning.

void GT_InitGametype()
{
	gametype.title = "Race";
	gametype.version = "1.02";
	gametype.author = "Warsow Development Team";

	// if the gametype doesn't have a config file, create it
	if ( !G_FileExists( "configs/server/gametypes/" + gametype.name + ".cfg" ) )
	{
		String config;

		// the config file doesn't exist or it's empty, create it
		config = "// '" + gametype.title + "' gametype configuration file\n"
				 + "// This config will be executed each time the gametype is started\n"
				 + "\n\n// map rotation\n"
				 + "set g_maplist \"\" // list of maps in automatic rotation\n"
				 + "set g_maprotation \"0\"   // 0 = same map, 1 = in order, 2 = random\n"
				 + "\n// game settings\n"
				 + "set g_scorelimit \"0\"\n"
				 + "set g_timelimit \"0\"\n"
				 + "set g_warmup_timelimit \"0\"\n"
				 + "set g_match_extendedtime \"0\"\n"
				 + "set g_allow_falldamage \"0\"\n"
				 + "set g_allow_selfdamage \"0\"\n"
				 + "set g_allow_teamdamage \"0\"\n"
				 + "set g_allow_stun \"0\"\n"
				 + "set g_teams_maxplayers \"0\"\n"
				 + "set g_teams_allow_uneven \"0\"\n"
				 + "set g_countdown_time \"5\"\n"
				 + "set g_maxtimeouts \"0\" // -1 = unlimited\n"
				 + "set g_challengers_queue \"0\"\n"
				 + "\necho " + gametype.name + ".cfg executed\n";

		G_WriteFile( "configs/server/gametypes/" + gametype.name + ".cfg", config );
		G_Print( "Created default config file for '" + gametype.name + "'\n" );
		G_CmdExecute( "exec configs/server/gametypes/" + gametype.name + ".cfg silent" );
	}

	gametype.spawnableItemsMask = ( IT_AMMO | IT_WEAPON | IT_POWERUP );
	if ( gametype.isInstagib )
		gametype.spawnableItemsMask &= ~uint( G_INSTAGIB_NEGATE_ITEMMASK );

	gametype.respawnableItemsMask = gametype.spawnableItemsMask;
	gametype.dropableItemsMask = 0;
	gametype.pickableItemsMask = ( gametype.spawnableItemsMask | gametype.dropableItemsMask );

	gametype.isTeamBased = false;
	gametype.isRace = true;
	gametype.hasChallengersQueue = false;
	gametype.maxPlayersPerTeam = 0;

	gametype.ammoRespawn = 1;
	gametype.armorRespawn = 1;
	gametype.weaponRespawn = 1;
	gametype.healthRespawn = 1;
	gametype.powerupRespawn = 1;
	gametype.megahealthRespawn = 1;
	gametype.ultrahealthRespawn = 1;

	gametype.readyAnnouncementEnabled = false;
	gametype.scoreAnnouncementEnabled = false;
	gametype.countdownEnabled = false;
	gametype.mathAbortDisabled = true;
	gametype.shootingDisabled = false;
	gametype.infiniteAmmo = true;
	gametype.canForceModels = true;
	gametype.canShowMinimap = false;
	gametype.teamOnlyMinimap = true;

	gametype.mmCompatible = true;

	gametype.spawnpointRadius = 0;

	if ( gametype.isInstagib )
		gametype.spawnpointRadius *= 2;

	gametype.inverseScore = true;

	// set spawnsystem type
	for ( int team = TEAM_PLAYERS; team < GS_MAX_TEAMS; team++ )
		gametype.setTeamSpawnsystem( team, SPAWNSYSTEM_INSTANT, 0, 0, false );

	// define the scoreboard layout
	G_ConfigString( CS_SCB_PLAYERTAB_LAYOUT, "%n 112 %s 52 %t 96 %l 48 %s 52" );
	G_ConfigString( CS_SCB_PLAYERTAB_TITLES, "Name Clan Time Ping Racing" );

	// add commands
	RACE_RegisterCommands();

	// add votes
	G_RegisterCallvote( "randmap", "<* | pattern>", "string", "Changes to a random map" );

	demoRecording = false;

	G_Print( "Gametype '" + gametype.title + "' initialized\n" );
}
