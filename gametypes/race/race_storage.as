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

class RecordTime
{
	uint position;
	uint[] sectorTimes;
	uint finishTime;
	String playerName;
	String login;

	RecordTime()
	{
		this.position = uint( -1 );
		this.finishTime = 0;

		this.sectorTimes.resize( numCheckpoints );
		for ( int i = 0; i < numCheckpoints; ++i )
			this.sectorTimes[i] = 0;
	}

	~RecordTime() {}

	void clear()
	{
		this.playerName = "";
		this.login = "";
		this.finishTime = 0;

		for ( int i = 0; i < numCheckpoints; i++ )
			this.sectorTimes[i] = 0;
	}

	void Copy( RecordTime &other )
	{
		this.finishTime = other.finishTime;
		this.playerName = other.playerName;
		this.login = other.login;
		for ( int i = 0; i < numCheckpoints; i++ )
			this.sectorTimes[i] = other.sectorTimes[i];
	}

	void Store( Client @client )
	{
		Player @player = RACE_GetPlayer( client );
		this.finishTime = player.finishTime;
		this.playerName = client.name;
		this.login = client.getUserInfoKey( "cl_mm_login" );
		for ( int i = 0; i < numCheckpoints; i++ )
			this.sectorTimes[i] = player.sectorTimes[i];
	}
}

/**
 * Manages storage and retrieval of (local) race records.
 */
class LocalRecordsStorage
{
	private array<RecordTime @> records;

	/**
	 * Given a map name creates a full path in the game filesystem
	 * for a file that should serve as a local storage on a disk.
	 * @param mapName a name of the map. The letters case does not matter.
	 * @return a path for the file in the game filesystem.
	 */
	private String @createFullPath( const String &mapName )
	{
		return "race/local/" + mapName.tolower() + ".txt";
	}

	/**
	 * Loads local race records from a disk.
	 */
	void load()
	{
		Cvar mapName( "mapname", "", 0 );

		const String fileContents = G_LoadFile( createFullPath( mapName.string ) );
		if ( fileContents.length() == 0 )
			return;

		parseFileContents( fileContents );
	}

	private void parseFileContents( String &contents )
	{
		String timeToken, loginToken, nameToken, sectorToken;
		int count = 0;

		for (;; )
		{
			timeToken = contents.getToken( count++ );
			if ( timeToken.length() == 0 )
			{
				break;
			}

			uint sep = timeToken.locate( "|", 0 );
			if ( sep == timeToken.length() )
			{
				loginToken = "";
			}
			else
			{
				loginToken = timeToken.substr( sep + 1 );
				timeToken = timeToken.substr( 0, sep );
			}

			nameToken = contents.getToken( count++ );
			if ( nameToken.length() == 0 )
			{
				break;
			}

			// check if he already has a score
			if ( loginToken != "" )
			{
				if ( @findRecordByLogin( loginToken ) != null )
					continue;
			}
			else
			{
				if ( @findRecordByName( nameToken ) != null )
					continue;
			}

			sectorToken = contents.getToken( count++ );
			if ( sectorToken.length() == 0 )
			{
				break;
			}

			RecordTime @newRecord = this.grow();

			// store this one
			int numSectors = sectorToken.toInt();
			for ( int j = 0; j < numSectors; j++ )
			{
				sectorToken = contents.getToken( count++ );
				if ( sectorToken.length() == 0 )
				{
					break;
				}
				newRecord.sectorTimes[j] = uint( sectorToken.toInt() );
			}

			newRecord.finishTime = uint( timeToken.toInt() );
			newRecord.playerName = nameToken;
			newRecord.login = loginToken;
		}
	}

	/**
	 * Tries to retrieve a local record by a player name.
	 * @param name a player name. The letters case does not matter.
	 * @return a reference to a found record or null if not found.
	 */
	RecordTime @findRecordByName( const String &name )
	{
		String cleanName = name.removeColorTokens().tolower();
		for ( uint i = 0; i < records.size(); ++i )
		{
			RecordTime @record = @records[i];
			if ( record.playerName.removeColorTokens().tolower() == cleanName )
			{
				return @record;
			}
		}

		return null;
	}

	/**
	 * Tries to retrieve a local record by a MM login.
	 * @param login a MM login. The letters case is significant.
	 * @return a reference to a found record or null if not found.
	 */
	RecordTime @findRecordByLogin( const String &login )
	{
		for ( uint i = 0; i < records.size(); ++i )
		{
			RecordTime @record = records[i];
			if ( record.login == login )
			{
				return @record;
			}
		}

		return null;
	}

	/**
	 * Tries to retrieve a local record by a numeric rank.
	 * @param rank a rank to look a record for. Must be non-negative.
	 * @return a reference to a found record or null if not found.
	 */
	RecordTime @findRecordByRank( int rank )
	{
		if( rank < int( records.size() ) )
		{
			return @records[rank];
		}

		return null;
	}

	/**
	 * Tries to make a string representation of a local rank for a finish time.
	 * @param finishTime a final time of a completed run.
	 * @return a reference to a string or null if it cannot be defined.
	 */
	const String @getFinalRankAsString( uint finishTime ) const
	{
		for ( uint i = 0; i < records.size(); ++i )
		{
			if ( finishTime <= records[i].finishTime )
			{
				return "" + ( i + 1 );
			}
		}

		return null;
	}

	/**
	 * Tries to make a string representation of a local rank for a sector time.
	 * @param sectorTime a racing time of the sector.
	 * @param sectorNum a number of the sector.
	 * @return a reference to a string or null if it cannot be defined.
	 */
	const String @getSectorRankAsString( uint sectorTime, int sectorNum ) const
	{
		for ( uint i = 0; i < records.size(); ++i )
		{
			if ( sectorTime <= records[i].sectorTimes[sectorNum] )
			{
				return "" + ( i + 1 );
			}
		}

		return null;
	}

	/**
	 * Gets a currently best local finish time.
	 * @return a best local time or zero if there's no records.
	 */
	uint getBestTime() const
	{
		if ( records.size() > 0 )
		{
			return records[0].finishTime;
		}
		return 0;
	}

	/**
	 * Saves local race records to a disk.
	 */
	void save()
	{
		Cvar mapName( "mapname", "", 0 );

		String fileContents = "//" + mapName.string.tolower() + " top scores\n\n";
		for ( uint i = 0; i < records.size(); ++i )
		{
			addToFileContents( records[i], fileContents );
		}

		G_WriteFile( createFullPath( mapName.string ), fileContents );
	}

	private void addToFileContents( const RecordTime &record, String &contents )
	{
		contents += "\"" + int( record.finishTime );
		// optionally storing it in a token with another value provides backwards compatibility
		if ( record.login != "" )
		{
			contents += "|" + record.login;
		}

		contents += "\" \"" + record.playerName + "\" ";

		// add the sectors
		contents += "\"" + numCheckpoints + "\" ";

		for ( int j = 0; j < numCheckpoints; j++ )
		{
			contents += "\"" + record.sectorTimes[j] + "\" ";
		}

		contents += "\n";
	}

	private RecordTime @grow()
	{
		RecordTime @record = @RecordTime();
		record.position = records.size();
		records.insertLast( @record );
		return record;
	}

	// We prefer to keep these methods separate and call these methods separately for clarity.
	// We should really try using linked lists...

	private void remove( RecordTime @record )
	{
		records.removeAt( record.position );

		// TODO: Patch positions starting from the removed record
		patchRecordPositions();
	}

	private void expandAtPosition( uint index )
	{
		records.insertAt( index, @RecordTime() );

		// TODO: Patch positions starting from next position
		patchRecordPositions();
	}

	private void patchRecordPositions()
	{
		for ( uint i = 0; i < records.size(); ++i )
		{
			records[i].position = i;
		}
	}

	/**
	 * Registers a just completed race run if it's necessary for the records tracking.
	 * @param runner a player that has just completed a race run.
	 */
	void registerCompletedRun( Player @runner )
	{
		// See if the runner improved one of the top scores
		uint top = 0;
		for (; top < records.size(); ++top )
		{
			if ( runner.finishTime <= records[top].finishTime )
				break;
		}

		// If not found
		if ( top == records.size() )
		{
			// Prevent infinite growth of the top list
			if ( top == MAX_RECORDS )
				return;
		}

		if ( top == 0 )
		{
			runner.client.addAward( S_COLOR_GREEN + "Server record!" );
			// TODO: Announce that the record is contested against a world record in this case.
			G_PrintMsg( null, runner.client.name + S_COLOR_YELLOW + " set a new server record: "
					+ S_COLOR_WHITE + RACE_TimeToString( runner.finishTime ) + "\n" );
		}

		String login = runner.client.getUserInfoKey( "cl_mm_login" );

		RecordTime @existingRecord;
		if ( login != "" )
		{
			@existingRecord = findRecordByLogin( login );
		}
		else
		{
			@existingRecord = findRecordByName( runner.client.name );
		}

		if ( @existingRecord != null )
		{
			// The runner already has a better time, don't save the new run
			if ( existingRecord.finishTime <= runner.finishTime )
			{
				return;
			}

			// TODO: All of this is much simpler if a linked list is used.
			// We do not really need addressing of records by index
			// except few first records and this should be fine with lists too.

			// Remove the existing record
			this.remove( @existingRecord );
		}

		// Make sure we have a clean addressable cell at `top`
		this.expandAtPosition( top );
		// Store the record at `top`
		records[top].Store( runner.client );

		RACE_UpdateHUDTopScores();

		// Save the current state to a disk
		this.save();
	}
}

/**
 * A global singleton instance of the {@code LocalRecordsStorage}.
 */
LocalRecordsStorage localRecordsStorage;
