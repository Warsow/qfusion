/*
Copyright (C) 2002-2003 Victor Luchits

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

#include "cg_local.h"
#include "../client/snd_public.h"
#include "../qcommon/qcommon.h"
#include "../qcommon/wswstaticstring.h"
#include "../client/client.h"
#include "mediacache.h"

#include <algorithm>

MediaCache::CachedSound::CachedSound( MediaCache *parent, const wsw::StringView &name )
	: MediaCache::CachedHandle<sfx_s>( name ) {
	parent->link( this, &parent->m_sounds );
}

MediaCache::CachedModel::CachedModel( MediaCache *parent, const wsw::StringView &name )
	: MediaCache::CachedHandle<model_s>( name ) {
	parent->link( this, &parent->m_models );
}

MediaCache::CachedMaterial::CachedMaterial( MediaCache *parent, const wsw::StringView &name )
	: MediaCache::CachedHandle<shader_s>( name ) {
	parent->link( this, &parent->m_materials );
}

MediaCache::CachedSoundsArray::CachedSoundsArray( MediaCache *parent, const char *format, unsigned indexShift )
	: CachedHandlesArray<sfx_s>( parent, format, indexShift ) {
	parent->link( this, &parent->m_soundsArrays );
}

// Make sure it's defined here so the inline setup of fields does not get replicated over inclusion places
MediaCache::MediaCache() {}

void MediaCache::registerSounds() {
	for( CachedSound *sound = m_sounds; sound; sound = (CachedSound *)sound->m_next ) {
		registerSound( sound );
	}
	for( CachedSoundsArray *array = m_soundsArrays; array; array = (CachedSoundsArray *)array->m_next ) {
		registerSoundsArray( array );
	}
}

void MediaCache::registerModels() {
	for( CachedModel *model = m_models; model; model = (CachedModel *)model->m_next ) {
		registerModel( model );
	}
}

void MediaCache::registerMaterials() {
	for( CachedMaterial *material = m_materials; material; material = (CachedMaterial *)material->m_next ) {
		registerMaterial( material );
	}
}

void MediaCache::registerSound( CachedSound *sound ) {
	if( !sound->m_handle ) {
		assert( sound->m_name.isZeroTerminated() );
		sound->m_handle = SoundSystem::instance()->registerSound( sound->m_name.data() );
	}
}

void MediaCache::registerSoundsArray( MediaCache::CachedSoundsArray *array ) {
	const size_t oldStorageSize = m_handlesArraysDataStorage.size();

	for( unsigned i = array->m_indexShift; i < 10; ++i ) {
		char buffer[MAX_QPATH];
		const char *name = va_r( buffer, sizeof( buffer ), array->m_format, i );
		if( sfx_s *sfx = SoundSystem::instance()->registerSound( name ) ) {
			m_handlesArraysDataStorage.push_back( sfx );
		} else {
			break;
		}
	}

	const size_t newStorageSize = m_handlesArraysDataStorage.size();
	if( newStorageSize <= oldStorageSize +1 ) {
		wsw::StaticString<MAX_QPATH> buffer;
		buffer << wsw::StringView( array->m_format ).take( MAX_QPATH );
		std::replace( buffer.begin(), buffer.end(), '%', '$');
		if( oldStorageSize == newStorageSize ) {
			Com_Printf( S_COLOR_RED "Failed to find any sound for pattern %s ($ for percent here)\n", buffer.data() );
		} else {
			Com_Printf( S_COLOR_RED "Too few sounds for pattern %s ($ for percent here)\n", buffer.data() );
		}
	}

	array->m_handlesOffset = (uint16_t)oldStorageSize;
	array->m_numHandles    = (uint16_t)( newStorageSize - oldStorageSize );
}

void MediaCache::registerModel( CachedModel *model ) {
	if( !model->m_handle ) {
		assert( model->m_name.isZeroTerminated() );
		model->m_handle = CG_RegisterModel( model->m_name.data() );
	}
}

void MediaCache::registerMaterial( CachedMaterial *material ) {
	if( !material->m_handle ) {
		assert( material->m_name.isZeroTerminated() );
		material->m_handle = R_RegisterPic( material->m_name.data() );
	}
}

/*
* CG_RegisterModel
*/
struct model_s *CG_RegisterModel( const char *name ) {
	struct model_s *model;

	model = R_RegisterModel( name );

	// precache bones
	if( R_SkeletalGetNumBones( model, NULL ) ) {
		CG_SkeletonForModel( model );
	}

	return model;
}

void CG_RegisterLevelMinimap( void ) {
	size_t i;
	int file;
	char minimap[MAX_QPATH];

	cgs.shaderMiniMap = NULL;

	const char *name = cgs.configStrings.getMapName()->data();

	for( i = 0; i < NUM_IMAGE_EXTENSIONS; i++ ) {
		Q_snprintfz( minimap, sizeof( minimap ), "minimaps/%s%s", name, IMAGE_EXTENSIONS[i] );
		file = FS_FOpenFile( minimap, NULL, FS_READ );
		if( file != -1 ) {
			cgs.shaderMiniMap = R_RegisterPic( minimap );
			break;
		}
	}
}

/*
* CG_RegisterFonts
*/
void CG_RegisterFonts( void ) {
	cvar_t *con_fontSystemFamily = Cvar_Get( "con_fontSystemFamily", DEFAULT_SYSTEM_FONT_FAMILY, CVAR_ARCHIVE );
	cvar_t *con_fontSystemMonoFamily = Cvar_Get( "con_fontSystemMonoFamily", DEFAULT_SYSTEM_FONT_FAMILY_MONO, CVAR_ARCHIVE );
	cvar_t *con_fontSystemSmallSize = Cvar_Get( "con_fontSystemSmallSize", STR_TOSTR( DEFAULT_SYSTEM_FONT_SMALL_SIZE ), CVAR_ARCHIVE );
	cvar_t *con_fontSystemMediumSize = Cvar_Get( "con_fontSystemMediumSize", STR_TOSTR( DEFAULT_SYSTEM_FONT_MEDIUM_SIZE ), CVAR_ARCHIVE );
	cvar_t *con_fontSystemBigSize = Cvar_Get( "con_fontSystemBigSize", STR_TOSTR( DEFAULT_SYSTEM_FONT_BIG_SIZE ), CVAR_ARCHIVE );

	// register system fonts
	Q_strncpyz( cgs.fontSystemFamily, con_fontSystemFamily->string, sizeof( cgs.fontSystemFamily ) );
	Q_strncpyz( cgs.fontSystemMonoFamily, con_fontSystemMonoFamily->string, sizeof( cgs.fontSystemMonoFamily ) );
	if( con_fontSystemSmallSize->integer <= 0 ) {
		Cvar_Set( con_fontSystemSmallSize->name, con_fontSystemSmallSize->dvalue );
	}
	if( con_fontSystemMediumSize->integer <= 0 ) {
		Cvar_Set( con_fontSystemMediumSize->name, con_fontSystemMediumSize->dvalue );
	}
	if( con_fontSystemBigSize->integer <= 0 ) {
		Cvar_Set( con_fontSystemBigSize->name, con_fontSystemBigSize->dvalue );
	}

	float scale = ( float )( cgs.vidHeight ) / 600.0f;

	cgs.fontSystemSmallSize = ceilf( con_fontSystemSmallSize->integer * scale );
	cgs.fontSystemSmall = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemSmallSize );
	if( !cgs.fontSystemSmall ) {
		Q_strncpyz( cgs.fontSystemFamily, DEFAULT_SYSTEM_FONT_FAMILY, sizeof( cgs.fontSystemFamily ) );
		cgs.fontSystemSmallSize = ceilf( DEFAULT_SYSTEM_FONT_SMALL_SIZE * scale );

		cgs.fontSystemSmall = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemSmallSize );
		if( !cgs.fontSystemSmall ) {
			CG_Error( "Couldn't load default font \"%s\"", cgs.fontSystemFamily );
		}
	}

	cgs.fontSystemMediumSize = ceilf( con_fontSystemMediumSize->integer * scale );
	cgs.fontSystemMedium = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemMediumSize );
	if( !cgs.fontSystemMedium ) {
		cgs.fontSystemMediumSize = ceilf( DEFAULT_SYSTEM_FONT_MEDIUM_SIZE * scale );
		cgs.fontSystemMedium = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemMediumSize );
	}

	cgs.fontSystemBigSize = ceilf( con_fontSystemBigSize->integer * scale );
	cgs.fontSystemBig = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemBigSize );
	if( !cgs.fontSystemBig ) {
		cgs.fontSystemBigSize = ceilf( DEFAULT_SYSTEM_FONT_BIG_SIZE * scale );
		cgs.fontSystemBig = SCR_RegisterFont( cgs.fontSystemFamily, QFONT_STYLE_NONE, cgs.fontSystemBigSize );
	}
}
