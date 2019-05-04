class RecordTime
{
    bool saved;
    uint[] sectorTimes;
    uint finishTime;
    String playerName;
    String login;
    bool arraysSetUp;

    void setupArrays( int size )
    {
        this.sectorTimes.resize( size );

        for ( int i = 0; i < numCheckpoints; i++ )
            this.sectorTimes[i] = 0;

        this.arraysSetUp = true;
    }

    RecordTime()
    {
        this.saved = false;
        this.arraysSetUp = false;
        this.finishTime = 0;
    }

    ~RecordTime() {}

    void clear()
    {
        this.saved = false;
        this.playerName = "";
        this.login = "";
        this.finishTime = 0;

        for ( int i = 0; i < numCheckpoints; i++ )
            this.sectorTimes[i] = 0;
    }

    void Copy( RecordTime &other )
    {
        if ( !this.arraysSetUp )
            return;

        this.saved = other.saved;
        this.finishTime = other.finishTime;
        this.playerName = other.playerName;
        this.login = other.login;
        for ( int i = 0; i < numCheckpoints; i++ )
            this.sectorTimes[i] = other.sectorTimes[i];
    }

    void Store( Client @client )
    {
        if ( !this.arraysSetUp )
            return;

        Player @player = RACE_GetPlayer( client );

        this.saved = true;
        this.finishTime = player.finishTime;
        this.playerName = client.name;
        if ( client.getUserInfoKey( "cl_mm_session" ).toInt() > 0 )
            this.login = client.getUserInfoKey( "cl_mm_login" );
        else
            this.login = "";
        for ( int i = 0; i < numCheckpoints; i++ )
            this.sectorTimes[i] = player.sectorTimes[i];
    }
}

RecordTime[] levelRecords( MAX_RECORDS );

void RACE_WriteTopScores()
{
    String topScores;
    Cvar mapNameVar( "mapname", "", 0 );
    String mapName = mapNameVar.string.tolower();

    topScores = "//" + mapName + " top scores\n\n";

    for ( int i = 0; i < MAX_RECORDS; i++ )
    {
    	if ( !levelRecords[i].saved )
    		continue;

    	if ( levelRecords[i].playerName.length() == 0 )
    		continue;

        topScores += "\"" + int( levelRecords[i].finishTime );
        if ( levelRecords[i].login != "" )
            topScores += "|" + levelRecords[i].login; // optionally storing it in a token with another value provides backwards compatibility
        topScores += "\" \"" + levelRecords[i].playerName + "\" ";

        // add the sectors
        topScores += "\"" + numCheckpoints+ "\" ";

        for ( int j = 0; j < numCheckpoints; j++ )
            topScores += "\"" + int( levelRecords[i].sectorTimes[j] ) + "\" ";

        topScores += "\n";
    }

    G_WriteFile( "topscores/race/" + mapName + ".txt", topScores );
}

void RACE_LoadTopScores()
{
    Cvar mapNameVar( "mapname", "", 0 );
    String mapName = mapNameVar.string.tolower();

    const String fileContents = G_LoadFile( "topscores/race/" + mapName + ".txt" );
    if ( fileContents.length() == 0 )
    	return;

    String timeToken, loginToken, nameToken, sectorToken;
    int count = 0;
    uint sep;

    int i = 0;
    while ( i < MAX_RECORDS )
    {
        timeToken = fileContents.getToken( count++ );
        if ( timeToken.length() == 0 )
            break;

        sep = timeToken.locate( "|", 0 );
        if ( sep == timeToken.length() )
        {
            loginToken = "";
        }
        else
        {
            loginToken = timeToken.substr( sep + 1 );
            timeToken = timeToken.substr( 0, sep );
        }

        nameToken = fileContents.getToken( count++ );
        if ( nameToken.length() == 0 )
            break;

        sectorToken = fileContents.getToken( count++ );
        if ( sectorToken.length() == 0 )
            break;

        int numSectors = sectorToken.toInt();

        // store this one
        for ( int j = 0; j < numSectors; j++ )
        {
            sectorToken = fileContents.getToken( count++ );
            if ( sectorToken.length() == 0 )
                break;

            levelRecords[i].sectorTimes[j] = uint( sectorToken.toInt() );
        }

        // check if he already has a score
        String cleanName = nameToken.removeColorTokens().tolower();
        bool exists = false;
        for ( int j = 0; j < i; j++ )
        {
            if ( ( loginToken != "" && levelRecords[j].login == loginToken )
                    || levelRecords[j].playerName.removeColorTokens().tolower() == cleanName )
            {
                exists = true;
                break;
            }
        }
        if ( exists )
        {
            levelRecords[i].clear();
            continue;
        }

        levelRecords[i].saved = true;
        levelRecords[i].finishTime = uint( timeToken.toInt() );
        levelRecords[i].playerName = nameToken;
        levelRecords[i].login = loginToken;

        i++;
    }

    RACE_UpdateHUDTopScores();
}
