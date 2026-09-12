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

class CommandHandlersRegistry
{
	private array<CommandHandler @> handlers;

	void register( CommandHandler @handler )
	{
		handlers.insertLast( handler );
		handler.registerGameCommand();
	}

	bool handle( Client @client, const String &cmdString, const String &argsString, int argc )
	{
		if( @client == null )
			return false;

		for( uint i = 0; i < handlers.size(); ++i )
		{
			CommandHandler @handler = handlers[i];
			if ( handler.canHandle( cmdString ) )
				return handler.handle( client, argsString, argc );
		}

		G_PrintMsg( client.getEnt(), "Unknown command " + cmdString );
		return false;
	}
}

abstract class CommandHandler
{
	const String @name;

	CommandHandler( const String @name )
	{
		@this.name = @name;
	}

	private void shouldBeUnreachable()
	{
		// Force a crash... TODO: Enable rasing exceptions and throw something
		Entity @bogusEntity = null;
		bogusEntity.origin = Vec3( 0, 0, 0 );
	}

	void registerGameCommand()
	{
		shouldBeUnreachable();
	}

	bool canHandle( const String &cmdString )
	{
		return name == cmdString;
	}

	bool handle( Client @client, const String &argsString, int argc )
	{
		shouldBeUnreachable();
		return false;
	}
}

mixin class RegistersGameCommand
{
	void registerGameCommand() override
	{
		G_RegisterCommand( name );
	}
}

mixin class UsesExistingCommand
{
	void registerGameCommand() override
	{
		// Just to override the CommandHandler::registerGameCommand()
		// default implementation that should be unreachable
	}
}

class GametypeCommandHandler : CommandHandler, RegistersGameCommand
{
	GametypeCommandHandler()
	{
		super( "gametype" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		Cvar fs_game( "fs_game", "", 0 );
		String manifest = gametype.manifest;

		String response = "";
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
};

class CVarInfoHandler : CommandHandler, UsesExistingCommand
{
	CVarInfoHandler()
	{
		super( "cvarinfo" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		GENERIC_CheatVarResponse( client, this.name, argsString, argc );
		return true;
	}
}

String randmap;
String randmap_passed = "";
int64 randmap_time = 0;

class CallvoteValidateHandler : CommandHandler, UsesExistingCommand
{
	CallvoteValidateHandler()
	{
		super( "callvotevalidate" );
	}

	array<const String @> collectMapsForPattern( const String &pattern, const String &current )
	{
		array<const String @> result;
		String lowercaseMap;

		const String lowercaseCurrent = current.tolower();
		const String lowercasePattern = pattern.tolower();

		if ( pattern == "" || pattern == "*" )
		{
			for ( int i = 0; ; i++ )
			{
				const String @map = ML_GetMapByNum( i );
				if ( @map == null )
					break;

				lowercaseMap = map.tolower();
				if ( lowercaseMap == lowercaseCurrent )
					continue;

				result.insertLast( map );
			}

			return result;
		}

		for ( int i = 0; ; i++ )
		{
			const String @map = ML_GetMapByNum( i );
			if ( @map == null )
				break;

			lowercaseMap = map.tolower();
			if( lowercaseMap == lowercaseCurrent )
				continue;

			bool match = false;
			// TODO: This should be a native method
			for ( uint p = 0; p < lowercaseMap.length(); p++ )
			{
				uint eq = 0;
				while ( eq < lowercasePattern.length() && p + eq < lowercaseMap.length() )
				{
					if ( lowercaseMap[p + eq] != lowercasePattern[eq] )
						break;

					eq++;
				}
				if ( eq == lowercasePattern.length() )
				{
					match = true;
					break;
				}
			}

			if ( match )
				result.insertLast( map );
		}

		return result;
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		String votename = argsString.getToken( 0 );
		if ( votename != "randmap" )
		{
			client.printMessage( "Unknown callvote " + votename + "\n" );
			return false;
		}

		Cvar mapname( "mapname", "", 0 );
		array<const String @> maps = collectMapsForPattern( argsString.getToken( 1 ), mapname.string );

		if ( maps.length() == 0 )
		{
			client.printMessage( "No matching maps\n" );
			return false;
		}

		if ( levelTime - randmap_time < 80 )
		{
			String message;
			message += S_COLOR_YELLOW + "Chosen map: " + S_COLOR_WHITE + randmap;
			message += S_COLOR_YELLOW + " (out of " + S_COLOR_WHITE + maps.length();
			message += S_COLOR_YELLOW + " matches)\n";
			G_PrintMsg( null, message );
			return true;
		}

		randmap_time = levelTime;
		randmap = maps[rand() % maps.length()];

		return true;
	}
}

class CallvotePassedHandler : CommandHandler, UsesExistingCommand
{
	CallvotePassedHandler()
	{
		super( "callvotepassed" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		String votename = argsString.getToken( 0 );

		if ( votename == "randmap" )
		{
			randmap_passed = randmap;
			match.launchState( MATCH_STATE_POSTMATCH );
		}

		return true;
	}
}

// Warning: mixing in RegistersGameCommand prevents "kill" command registration
class RaceRestartHandler : CommandHandler
{
	RaceRestartHandler()
	{
		super( "racerestart" );
	}

	void registerGameCommand() override
	{
		G_RegisterCommand( "racerestart" );
		G_RegisterCommand( "kill" );
	}

	bool canHandle( const String &cmdString ) override
	{
		// The super method tries to match the string against 'racerestart'
		return CommandHandler::canHandle( cmdString ) || ( cmdString == "kill" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		if ( @client == null )
			return true;

		Player @player = RACE_GetPlayer( client );
		if ( player.inRace )
			player.cancelRace();

		if ( client.team == TEAM_SPECTATOR && !gametype.isTeamBased )
			client.team = TEAM_PLAYERS;

		client.respawn( false );

		return true;
	}
}

class PracticeModeHandler : CommandHandler, RegistersGameCommand
{
	PracticeModeHandler()
	{
		super( "practicemode" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		RACE_GetPlayer( client ).togglePracticeMode();
		return true;
	}
}

class NoClipHandler: CommandHandler, RegistersGameCommand
{
	NoClipHandler()
	{
		super( "noclip" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		Player @player = RACE_GetPlayer( client );
		return player.toggleNoclip();
	}
}

class PositionHandler : CommandHandler, RegistersGameCommand
{
	PositionHandler()
	{
		super( "position" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		String action = argsString.getToken( 0 );
		if ( action == "save" )
		{
			return RACE_GetPlayer( client ).savePosition();
		}

		if ( action == "load" )
		{
			return RACE_GetPlayer( client ).loadPosition( true );
		}

		if ( action == "speed" && argsString.getToken( 1 ) != "" )
		{
			Position @position = RACE_GetPlayer( client ).savedPosition();
			String speed = argsString.getToken( 1 );
			if ( speed.locate( "+", 0 ) == 0 )
				position.speed += speed.substr( 1 ).toFloat();
			else if ( speed.locate( "-", 0 ) == 0 )
				position.speed -= speed.substr( 1 ).toFloat();
			else
				position.speed = speed.toFloat();
		}
		else if ( action == "clear" )
		{
			return RACE_GetPlayer( client ).clearPosition();
		}
		else
		{
			G_PrintMsg( client.getEnt(), "position <save | load | speed <value> | clear>\n" );
			return false;
		}

		return true;
	}
}

class TopCommandHandler : CommandHandler, RegistersGameCommand
{
	TopCommandHandler()
	{
		super( "top" );
	}

	bool handle( Client @client, const String &argsString, int argc ) override
	{
		RecordTime @top = localRecordsStorage.findRecordByRank( 0 );
		if ( @top == null )
		{
			client.printMessage( S_COLOR_RED + "No records yet.\n" );
			return true;
		}

		Table table( "r r r l l" );
		for ( int i = MAX_RECORDS - 1; i >= 0; i-- )
		{
			RecordTime @record = localRecordsStorage.findRecordByRank( i );
			if ( @record == null )
				continue;

			table.addCell( ( i + 1 ) + "." );
			table.addCell( S_COLOR_GREEN + RACE_TimeToString( record.finishTime ) );
			table.addCell( S_COLOR_YELLOW + "[+" + RACE_TimeToString( record.finishTime - top.finishTime ) + "]" );
			table.addCell( S_COLOR_WHITE + record.playerName );
			if ( record.login != "" )
				table.addCell( "(" + S_COLOR_YELLOW + record.login + S_COLOR_WHITE + ")" );
			else
				table.addCell( "" );
		}

		uint rows = table.numRows();
		for ( uint i = 0; i < rows; i++ )
			client.printMessage( table.getRow( i ) + "\n" );

		return true;
	}
}

CommandHandlersRegistry commandHandlersRegistry;

void RACE_RegisterCommands()
{
	commandHandlersRegistry.register( @GametypeCommandHandler() );
	commandHandlersRegistry.register( @RaceRestartHandler() );
	commandHandlersRegistry.register( @PracticeModeHandler() );
	commandHandlersRegistry.register( @NoClipHandler() );
	commandHandlersRegistry.register( @PositionHandler() );
	commandHandlersRegistry.register( @TopCommandHandler() );

	commandHandlersRegistry.register( @CallvoteValidateHandler() );
	commandHandlersRegistry.register( @CallvotePassedHandler() );
	commandHandlersRegistry.register( @CVarInfoHandler() );
}

//
