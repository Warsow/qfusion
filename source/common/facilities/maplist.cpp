/*
Copyright (C) 2007 Will Franklin

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

// MAPLIST FUNCTIONS

#include <common/common.h>
#include "maplist.h"
#include <common/types/q_trie.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>

#include <common/helpers/stringsplitter.h>
#include <common/types/stringview.h>
#include <common/types/staticstring.h>
#include <common/facilities/wswfs.h>

using wsw::operator""_asView;

#define MLIST_CACHE "cache/mapcache.txt"
#define MLIST_NULL  ""

#define MLIST_TRIE_CASING TRIE_CASE_INSENSITIVE

#define MLIST_UNKNOWN_MAPNAME   "@#$"

typedef struct mapinfo_s {
	char *filename, *fullname;
	struct mapinfo_s *next;
} mapinfo_t;

static mapinfo_t *maplist;
static trie_t *mlist_filenames_trie = NULL, *mlist_fullnames_trie = NULL;

static bool ml_flush = true;
static bool ml_initialized = false;

static void ML_BuildCache( void );
static void ML_InitFromCache( void );
static void ML_InitFromMaps( void );
static void ML_GetFullnameFromMap( const char *filename, char *fullname, size_t len );
static bool ML_FilenameExistsExt( const char *filename, bool quick );

/*
* ML_AddMap
* Handles assigning memory for map and adding it to the list
* in alphabetical order
*/
static void ML_AddMap( const char *filename, const char *fullname ) {
	mapinfo_t *map;
	char *buffer;
	char fullname_[MAX_QPATH];

	if( !ML_ValidateFilename( filename ) ) {
		return;
	}

	if( !strcmp( filename, "ui" ) ) {
		return;
	}

	if( !fullname ) {
		ML_GetFullnameFromMap( filename, fullname_, sizeof( fullname_ ) );
		fullname = fullname_;
	}

	if( !ML_ValidateFullname( fullname ) && *fullname ) { // allow empty fullnames
		return;
	}

	if( !strcmp( fullname, "ui" ) ) {
		return;
	}

	ml_flush = true;    // tell everyone that maplist has changed
	buffer = ( char* )Q_malloc( sizeof( mapinfo_t ) + strlen( filename ) + 1 + strlen( fullname ) + 1 );

	map = ( mapinfo_t * )buffer;
	buffer += sizeof( mapinfo_t );

	map->filename = buffer;
	strcpy( map->filename, filename );
	COM_StripExtension( map->filename );
	buffer += strlen( filename ) + 1;

	map->fullname = buffer;
	strcpy( map->fullname, fullname );
	COM_RemoveColorTokens( map->fullname );
	Q_strlwr( map->fullname );

	Trie_Insert( mlist_filenames_trie, map->filename, map );
	Trie_Insert( mlist_fullnames_trie, map->fullname, map );

	map->next = maplist;
	maplist = map;
}

/*
* ML_BuildCache
* Write the map data to a cache file
*/
static void ML_BuildCache( void ) {
	int filenum;
	mapinfo_t *map;
	struct trie_dump_s *dump;

	if( !ml_initialized ) {
		return;
	}

	if( FS_FOpenFile( MLIST_CACHE, &filenum, FS_WRITE | FS_CACHE ) != -1 ) {
		unsigned int i;

		Trie_Dump( mlist_filenames_trie, "", TRIE_DUMP_VALUES, &dump );
		for( i = 0; i < dump->size; ++i ) {
			map = ( mapinfo_t * )( dump->key_value_vector[i].value );
			FS_Printf( filenum, "%s\r\n%s\r\n", map->filename, map->fullname );
		}
		Trie_FreeDump( dump );

		FS_FCloseFile( filenum );
	}
}

/*
* ML_InitFromCache
* Fills map list array from cache, much faster
*/
static void ML_InitFromCache( wsw::fs::BufferedReader &reader ) {
	if( ml_initialized ) {
		return;
	}

	wsw::fs::SearchResultHolder searchResultHolder;
	const auto maybeCallResult = searchResultHolder.findDirFiles( "maps"_asView, ".bsp"_asView );
	if( !maybeCallResult ) {
		return;
	}

	wsw::PodVector<wsw::StringView> existingFileNames;
	existingFileNames.reserve( maybeCallResult->getNumFiles() );
	wsw::PodVector<char> existingFileNameData;
	existingFileNameData.reserve( maybeCallResult->getNumFiles() * MAX_QPATH + 1 );

	for( const wsw::StringView &fileName: *maybeCallResult ) {
		const size_t oldDataLen = existingFileNameData.size();
		existingFileNameData.append( fileName.data(), fileName.size() );
		existingFileNameData.append( '\0' );

		char *cleanName = existingFileNameData.data() + oldDataLen;
		COM_StripExtension( cleanName );
		const size_t cleanNameLen = std::strlen( cleanName );

		existingFileNameData.erase( existingFileNameData.begin() + oldDataLen + cleanNameLen, existingFileNameData.end() );
		existingFileNameData.append( '\0' );

		existingFileNames.append( wsw::StringView( cleanName, cleanNameLen, wsw::StringView::ZeroTerminated ) );
	}

	constexpr size_t kBufferSize = 1024;
	char fileNameBuffer[kBufferSize];
	char fullNameBuffer[kBufferSize];
	char *buffers[2] = { &fileNameBuffer[0], &fullNameBuffer[0] };
	unsigned turn = 0;

	for(;; ) {
		if( reader.isAtEof() ) {
			break;
		}

		auto readResult = reader.readToNewline( buffers[turn], kBufferSize );
		// The buffer must be sufficient to read any line
		if( !readResult ) {
			break;
		}
		if( readResult->wasIncomplete ) {
			break;
		}

		assert( readResult->bytesRead < kBufferSize );
		buffers[turn][readResult->bytesRead] = '\0';

		const auto oldTurn = turn;
		turn = ( turn + 1 ) % 2;
		if( !oldTurn ) {
			COM_SanitizeFilePath( fileNameBuffer );
			continue;
		}

		// well, if we've got a map with an unknown fullname, load it from map
		if( !strcmp( fullNameBuffer, MLIST_UNKNOWN_MAPNAME ) ) {
			ML_AddMap( fileNameBuffer, nullptr );
		} else {
			ML_AddMap( fileNameBuffer, fullNameBuffer );
		}
	}

	// we've now loaded the mapcache, but there may be files which
	// have been added to maps directory, and the mapcache isnt aware
	// these will be left over in our directory list
	for( const wsw::StringView &fileName: existingFileNames ) {
		assert( fileName.isZeroTerminated() );
		ML_AddMap( fileName.data(), nullptr );
	}
}

/*
* ML_InitFromMaps
* Fills map list array from each map file. Very slow
* and should only be called if cache doesnt exist
*/
static void ML_InitFromMaps( void ) {
	wsw::fs::SearchResultHolder searchResultHolder;
	if( auto callResult = searchResultHolder.findDirFiles( "maps"_asView, ".bsp"_asView ) ) {
		char buffer[MAX_QPATH];
		for( const wsw::StringView &fileName: *callResult ) {
			fileName.copyTo( buffer, sizeof( buffer ) );
			COM_SanitizeFilePath( buffer );
			COM_StripExtension( buffer );

			ML_AddMap( buffer, nullptr );
		}
	}
}

static int ML_PatternMatchesMap( void *map, void *pattern ) {
	assert( map );
	return !pattern || Com_GlobMatch( (const char *) pattern, ( (mapinfo_t *) map )->filename, false );
}

/*
* ML_MapListCmd
* Handler for console command "maplist"
*/
static void ML_MapListCmd( const CmdArgs &cmdArgs ) {
	const char *pattern;
	mapinfo_t *map;
	int argc = Cmd_Argc();
	unsigned int i;
	struct trie_dump_s *dump = NULL;

	if( argc > 2 ) {
		Com_Printf( "Usage: %s [rebuild]\n", Cmd_Argv( 0 ) );
		return;
	}

	pattern = ( argc == 2 ? Cmd_Argv( 1 ) : NULL );
	if( pattern && !*pattern ) {
		pattern = NULL;
	}

	if( pattern ) {
		if( !strcmp( pattern, "rebuild" ) ) {
			Com_Printf( "Rebuilding map list...\n" );
			ML_Restart( true );
			return;
		}

		if( !strcmp( pattern, "update" ) ) {
			Com_Printf( "Updating map list...\n" );
			ML_Update();
			return;
		}
	}

	Trie_DumpIf( mlist_filenames_trie, "", TRIE_DUMP_VALUES, ML_PatternMatchesMap, (void *)pattern, &dump );
	for( i = 0; i < dump->size; i++ ) {
		map = ( mapinfo_t * )( dump->key_value_vector[i].value );
		Com_Printf( "%s: %s\n", map->filename, map->fullname );
	}
	Trie_FreeDump( dump );

	Com_Printf( "%d map(s) %s\n", i, pattern ? "matching" : "total" );
}

CompletionResult ML_CompleteBuildList( const wsw::StringView &partial ) {
	wsw::PodVector<char> ztPartial( partial.data(), partial.size() );
	ztPartial.append( '\0' );

	assert( mlist_filenames_trie );

	struct trie_dump_s *dump = nullptr;
	Trie_Dump( mlist_filenames_trie, ztPartial.data(), TRIE_DUMP_VALUES, &dump );

	CompletionResult result;

	for( unsigned i = 0; i < dump->size; ++i ) {
		result.add( wsw::StringView( ( (const mapinfo_t *) ( dump->key_value_vector[i].value ))->filename ) );
	}

	Trie_FreeDump( dump );

	return result;
}

/*
* ML_Init
* Initialize map list. Check if cache file exists, if not create it
*/
void ML_Init( void ) {
	if( ml_initialized ) {
		return;
	}

	Trie_Create( MLIST_TRIE_CASING, &mlist_filenames_trie );
	Trie_Create( MLIST_TRIE_CASING, &mlist_fullnames_trie );

	Cmd_AddClientAndServerCommand( "maplist", ML_MapListCmd );

	if( auto maybeReader = wsw::fs::openAsBufferedReader( wsw::StringView( MLIST_CACHE ), wsw::fs::UseCacheFS ) ) {
		ML_InitFromCache( *maybeReader );
	} else {
		ML_InitFromMaps();
	}

	ml_initialized = true;
	ml_flush = true;
}

/*
* ML_Shutdown
* Free map list memory
*/
void ML_Shutdown( void ) {
	mapinfo_t *map;

	if( !ml_initialized ) {
		return;
	}

	ML_BuildCache();

	ml_initialized = false;

	Cmd_RemoveClientAndServerCommand( "maplist" );

	Trie_Destroy( mlist_filenames_trie );
	Trie_Destroy( mlist_fullnames_trie );

	while( maplist ) {
		map = maplist;
		maplist = map->next;
		Q_free( map );
	}

	ml_flush = true;
}

/*
* ML_Restart
* Restart map list stuff
*/
void ML_Restart( bool forcemaps ) {
	ML_Shutdown();
	if( forcemaps ) {
		int filenum;
		if( FS_FOpenFile( MLIST_CACHE, &filenum, FS_WRITE | FS_CACHE ) != -1 ) {
			FS_FCloseFile( filenum );
		}
	}
	FS_Rescan();
	ML_Init();
}

/*
* ML_Update
*/
bool ML_Update( void ) {
	if( !FS_Rescan() ) {
		return false;
	}

	wsw::fs::SearchResultHolder searchResultHolder;
	if( auto callResult = searchResultHolder.findDirFiles( "maps"_asView, ".bsp"_asView ) ) {
		for( const wsw::StringView &fileName: *callResult ) {
			char buffer[MAX_QPATH];
			fileName.copyTo( buffer, sizeof( buffer ) );
			COM_StripExtension( buffer );
			// don't check for existance of each file itself, as we've just got the fresh list
			if( !ML_FilenameExistsExt( buffer, true ) ) {
				ML_AddMap( buffer, MLIST_UNKNOWN_MAPNAME );
			}
		}
	}

	return true;
}

/*
* ML_GetFilename
* Returns the filename for the map with the corresponding fullname
*/
const char *ML_GetFilenameExt( const char *fullname, bool recursive ) {
	mapinfo_t *map;
	trie_error_t err;
	char *fullname2;

	if( !ml_initialized ) {
		return MLIST_NULL;
	}

	if( !ML_ValidateFullname( fullname ) ) {
		return MLIST_NULL;
	}

	fullname2 = ( char* )Q_malloc( strlen( fullname ) + 1 );
	strcpy( fullname2, fullname );
	Q_strlwr( fullname2 );

	err = Trie_Find( mlist_fullnames_trie, fullname2, TRIE_EXACT_MATCH, (void **)&map );
	Q_free( fullname2 );

	if( err == TRIE_OK ) {
		return map->filename;
	}

	// we should technically never get here, but
	// maybe the mapper has changed the fullname of the map
	// or the user has tampered with the mapcache
	// we need to reload the whole cache from file if we get here
	/*
	if( !recursive )
	{
	ML_Restart( true );
	return ML_GetFilenameExt( fullname, true );
	}
	*/
	return MLIST_NULL;
}

/*
* ML_GetFilename
* Returns the filename for the map with the corresponding fullname
*/
const char *ML_GetFilename( const char *fullname ) {
	return ML_GetFilenameExt( fullname, false );
}

/*
* ML_FilenameExists
* Checks to see if a filename is present in the map list
*/
static bool ML_FilenameExistsExt( const char *filename, bool quick ) {
	mapinfo_t *map;
	char *filepath;

	if( !ml_initialized ) {
		return false;
	}

	filepath = va( "maps/%s.bsp", filename );
	COM_SanitizeFilePath( filepath );

	if( !ML_ValidateFilename( filename ) ) {
		return false;
	}

	if( Trie_Find( mlist_filenames_trie, filename, TRIE_EXACT_MATCH, (void **)&map ) == TRIE_OK ) {
		if( quick || FS_FOpenFile( filepath, NULL, FS_READ ) != -1 ) {
			return true;
		}
	}

	return false;
}

/*
* ML_FilenameExists
* Checks to see if a filename is present in the map list
*/
bool ML_FilenameExists( const char *filename ) {
	return ML_FilenameExistsExt( filename, false );
}

/*
* ML_GetFullname
* Returns the fullname for the map with the corresponding filename
*/
const char *ML_GetFullname( const char *filename ) {
	mapinfo_t *map;
	//char *filepath;

	if( !ml_initialized ) {
		return MLIST_NULL;
	}

	if( !ML_ValidateFilename( filename ) ) {
		return MLIST_NULL;
	}

	/*
	filepath = va( "maps/%s.bsp", filename );
	COM_SanitizeFilePath( filepath );

	if( FS_FOpenFile( filepath, NULL, FS_READ ) == -1 )
	    return MLIST_NULL;
	*/

	if( Trie_Find( mlist_filenames_trie, filename, TRIE_EXACT_MATCH, (void **)&map ) == TRIE_OK ) {
		return map->fullname;
	}

	// we should never get down here!
	assert( false );

	return MLIST_NULL;
}

/*
* ML_GetFullnameFromMap
* Get fullname of map from file or worldspawn (slow)
*/
static void ML_GetFullnameFromMap( const char *filename, char *fullname, size_t len ) {
	char *buffer;

	*fullname = '\0';

	// Try and load fullname from a file
	FS_LoadFile( va( "maps/%s.txt", filename ), ( void ** )&buffer, NULL, 0 );
	if( buffer ) {
		char *line = buffer;
		Q_strncpyz( fullname, COM_Parse( &line ), len );
		FS_FreeFile( buffer );
		return;
	}

	// Try and load fullname from worldspawn
	CM_LoadMapMessage( va( "maps/%s.bsp", filename ), fullname, len );
	COM_RemoveColorTokens( fullname );
}

/*
* ML_ValidateFilename
* Checks that the filename provided is valid
*/
bool ML_ValidateFilename( const char *filename ) {
	const char *extension;

	if( !filename || !*filename ) {
		return false;
	}

	extension = COM_FileExtension( filename );
	if( !extension ) {
		if( strlen( "maps/" ) + strlen( filename ) + strlen( ".bsp" ) >= MAX_QPATH ) {
			return false;
		}
	} else {
		if( Q_stricmp( extension, ".bsp" ) ) {
			return false;
		}
		if( strlen( "maps/" ) + strlen( filename ) >= MAX_QPATH ) {
			return false;
		}
	}

	if( !COM_ValidateRelativeFilename( filename ) || strchr( filename, '/' ) ) {
		return false;
	}

	return true;
}

/*
* ML_ValidateFullname
* Checks that the fullname provided is valid
*/
bool ML_ValidateFullname( const char *fullname ) {
	if( !fullname || !*fullname ) {
		return false;
	}

	if( strlen( fullname ) >= MAX_CONFIGSTRING_CHARS ) {
		return false;
	}

	return true;
}

static int lastNum = 0;
static trie_dump_s *dump = nullptr;

static bool ML_CheckDumpCache( int num ) {
	if( !ml_initialized ) {
		return false;
	}

	if( ml_flush || lastNum > num ) {
		if( dump ) {
			Trie_FreeDump( dump );
			dump = nullptr;
		}
		ml_flush = false;
	}

	if( !dump ) {
		lastNum = 0;
		Trie_Dump( mlist_filenames_trie, "", TRIE_DUMP_VALUES, &dump );
	}

	return true;
}

auto ML_GetListSize() -> size_t {
	if( !ML_CheckDumpCache( std::numeric_limits<int>::max() ) ) {
		return 0;
	}
	assert( dump );
	return dump->size;
}

auto ML_GetMapByNum( int num ) -> std::optional<MapNamesPair> {
	if( !ML_CheckDumpCache( num ) ) {
		return std::nullopt;
	}

	assert( dump );
	for( ; lastNum < num && lastNum < (int)dump->size; lastNum++ )
		;

	if( lastNum == (int)dump->size ) {
		return std::nullopt;
	}

	const auto *mapInfo = ( mapinfo_t * )( dump->key_value_vector[num].value );
	if( strcmp( mapInfo->fullname, MLIST_UNKNOWN_MAPNAME ) != 0 ) {
		MapNamesPair result { wsw::StringView( mapInfo->filename ), wsw::StringView( mapInfo->fullname ) };
		return result;
	}

	MapNamesPair result { wsw::StringView( mapInfo->filename ), std::nullopt };
	return result;
}
