#ifndef WSW_MAPLIST_H
#define WSW_MAPLIST_H

#include <common/facilities/cmdcompat.h>

void ML_Init( void );
void ML_Shutdown( void );
void ML_Restart( bool forcemaps );
bool ML_Update( void );

const char *ML_GetFilenameExt( const char *fullname, bool recursive );
const char *ML_GetFilename( const char *fullname );
const char *ML_GetFullname( const char *filename );

struct MapNamesPair {
	const wsw::StringView fileName;
	const std::optional<wsw::StringView> fullName;
};

auto ML_GetListSize() -> size_t;
auto ML_GetMapByNum( int num ) -> std::optional<MapNamesPair>;

bool ML_FilenameExists( const char *filename );

bool ML_ValidateFilename( const char *filename );
bool ML_ValidateFullname( const char *fullname );

CompletionResult ML_CompleteBuildList( const wsw::StringView &partial );

#endif
