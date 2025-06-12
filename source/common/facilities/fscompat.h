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

#ifndef WSW_0d62aa41_3d65_437b_97fe_981d9e3c8bca_H
#define WSW_0d62aa41_3d65_437b_97fe_981d9e3c8bca_H

#define FS_NOTIFY_NEWPAKS   0x01

// directory searching
#define SFF_ARCH    0x01
#define SFF_HIDDEN  0x02
#define SFF_RDONLY  0x04
#define SFF_SUBDIR  0x08
#define SFF_SYSTEM  0x10

#define FS_READ             0
#define FS_WRITE            1
#define FS_APPEND           2
#define FS_NOSIZE           0x80    // FS_NOSIZE bit tells that we're not interested in real size of the file
// it is merely a hint a proper file size may still be returned by FS_Open
#define FS_GZ               0x100   // compress on write and decompress on read automatically
// doesn't work for pk3 files
#define FS_UPDATE           0x200
#define FS_CACHE            0x800

#define FS_RWA_MASK         ( FS_READ | FS_WRITE | FS_APPEND )

#define FS_SEEK_CUR         0
#define FS_SEEK_SET         1
#define FS_SEEK_END         2

void        FS_Init( void );
int         FS_Rescan( void );
void        FS_Frame( void );
void        FS_Shutdown( void );

#include <common/types/stringview.h>

static inline const wsw::StringView kDataDirectory( "basewsw" );

int         FS_GetExplicitPurePakList( char ***paknames );
bool        FS_IsExplicitPurePak( const char *pakname, bool *wrongver );

// handling of absolute filenames
// only to be used if necessary (library not supporting custom file handling functions etc.)
const char *FS_WriteDirectory( void );
const char *FS_CacheDirectory( void );
const char *FS_DownloadsDirectory( void );
const char *FS_RuntimeDirectory( void );
void        FS_CreateAbsolutePath( const char *path );
const char *FS_AbsoluteNameForFile( const char *filename );
const char *FS_AbsoluteNameForBaseFile( const char *filename );

// // game and base files
// file streaming
int     FS_FOpenFile( const char *filename, int *filenum, int mode );
int     FS_FOpenBaseFile( const char *filename, int *filenum, int mode );
int     FS_FOpenAbsoluteFile( const char *filename, int *filenum, int mode );
void    FS_FCloseFile( int file );

int     FS_Read( void *buffer, size_t len, int file );
int     FS_Print( int file, const char *msg );

#ifndef _MSC_VER
int FS_Printf( int file, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
int FS_Printf( int file, _Printf_format_string_ const char *format, ... );
#endif

int     FS_Write( const void *buffer, size_t len, int file );
int     FS_Tell( int file );
int     FS_Seek( int file, int offset, int whence );
int     FS_Eof( int file );
int     FS_Flush( int file );
int     FS_FileNo( int file, size_t *offset );

void    FS_SetCompressionLevel( int file, int level );
int     FS_GetCompressionLevel( int file );

// file loading
int     FS_LoadFileExt( const char *path, int flags, void **buffer, void *stack, size_t stackSize, const char *filename, int fileline );
int     FS_LoadBaseFileExt( const char *path, int flags, void **buffer, void *stack, size_t stackSize, const char *filename, int fileline );
void    FS_FreeFile( void *buffer );
void    FS_FreeBaseFile( void *buffer );
#define FS_LoadFile( path,buffer,stack,stacksize ) FS_LoadFileExt( path,0,buffer,stack,stacksize,__FILE__,__LINE__ )
#define FS_LoadBaseFile( path,buffer,stack,stacksize ) FS_LoadBaseFileExt( path,0,buffer,stack,stacksize,__FILE__,__LINE__ )
#define FS_LoadCacheFile( path,buffer,stack,stacksize ) FS_LoadFileExt( path,FS_CACHE,buffer,stack,stacksize,__FILE__,__LINE__ )

/**
* Maps an existing file on disk for reading.
* Does *not* work for compressed virtual files.
*
* @return mapped pointer to data on disk or NULL if mapping failed or passed size is 0.
*/
void    *FS_MMapBaseFile( int file, size_t size, size_t offset );
void    FS_UnMMapBaseFile( int file, void *data );

int     FS_GetNotifications( void );
int     FS_RemoveNotifications( int bitmask );

// util functions
bool    FS_CopyFile( const char *src, const char *dst );
bool    FS_CopyBaseFile( const char *src, const char *dst );
bool    FS_ExtractFile( const char *src, const char *dst );
bool    FS_MoveFile( const char *src, const char *dst );
bool    FS_MoveBaseFile( const char *src, const char *dst );
bool    FS_MoveCacheFile( const char *src, const char *dst );
bool    FS_RemoveFile( const char *filename );
bool    FS_RemoveBaseFile( const char *filename );
bool    FS_RemoveAbsoluteFile( const char *filename );
bool    FS_RemoveDirectory( const char *dirname );
bool    FS_RemoveBaseDirectory( const char *dirname );
bool    FS_RemoveAbsoluteDirectory( const char *dirname );
unsigned    FS_ChecksumAbsoluteFile( const char *filename );
unsigned    FS_ChecksumBaseFile( const char *filename, bool ignorePakChecksum );
bool    FS_CheckPakExtension( const char *filename );
bool    FS_PakFileExists( const char *packfilename );

time_t      FS_FileMTime( const char *filename );
time_t      FS_BaseFileMTime( const char *filename );

// // only for game files
const char *FS_FirstExtension( const char *filename, const char *extensions[], int num_extensions );
const char *FS_PakNameForFile( const char *filename );
bool    FS_IsPureFile( const char *pakname );
const char *FS_FileManifest( const char *filename );
const char *FS_BaseNameForFile( const char *filename );

int         FS_GetFileList( const char *dir, const char *extension, char *buf, size_t bufsize, int start, int end );
int         FS_GetFileListExt( const char *dir, const char *extension, char *buf, size_t *bufsize, int start, int end );

// // only for base files
bool    FS_IsPakValid( const char *filename, unsigned *checksum );
bool    FS_AddPurePak( unsigned checksum );
void    FS_RemovePurePaks( void );

typedef struct purelist_s {
	char *filename;
	unsigned checksum;
	struct purelist_s *next;
} purelist_t;

void Com_AddPakToPureList( purelist_t **purelist, const char *pakname, const unsigned checksum );
unsigned Com_CountPureListFiles( purelist_t *purelist );
purelist_t *Com_FindPakInPureList( purelist_t *purelist, const char *pakname );
void Com_FreePureList( purelist_t **purelist );

char *COM_SanitizeFilePath( char *filename );
bool COM_ValidateFilename( const char *filename );
bool COM_ValidateRelativeFilename( const char *filename );
void COM_StripExtension( char *filename );
const char *COM_FileExtension( const char *in );
void COM_DefaultExtension( char *path, const char *extension, size_t size );
void COM_ReplaceExtension( char *path, const char *extension, size_t size );
const char *COM_FileBase( const char *in );
void COM_StripFilename( char *filename );
int COM_FilePathLength( const char *in );

#endif