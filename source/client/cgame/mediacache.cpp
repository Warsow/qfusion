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
#include <common/wswfs.h>
#include <client/client.h>
#include "mediacache.h"

MediaCache::CachedSound::CachedSound( MediaCache *parent, const SoundSetProps &props )
	: m_props( props ) {
	parent->link( this, &parent->m_sounds );
}

MediaCache::CachedModel::CachedModel( MediaCache *parent, const wsw::StringView &name )
	: m_name( name ) {
	parent->link( this, &parent->m_models );
}

MediaCache::CachedMaterial::CachedMaterial( MediaCache *parent, const wsw::StringView &name )
	: m_name( name ) {
	parent->link( this, &parent->m_materials );
}

// Make sure it's defined here so the inline setup of fields does not get replicated over inclusion places
MediaCache::MediaCache() {}

void MediaCache::registerSounds() {
	for( CachedSound *sound = m_sounds; sound; sound = (CachedSound *)sound->m_next ) {
		registerSound( sound );
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
		sound->m_handle = SoundSystem::instance()->registerSound( sound->m_props );
	}
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

void CG_RegisterFonts() {
	// TODO: Just use int explicitly and get rid of ceil?
	const float scale = VID_GetPixelRatio();

	cgs.fontPlayerNameTiny = SCR_RegisterFont( DEFAULT_SYSTEM_FONT_FAMILY, QFONT_STYLE_NONE, (unsigned)ceilf( 13 * scale ) );
	if( !cgs.fontPlayerNameTiny ) {
		CG_Error( "Couldn't load default font \"%s\"", DEFAULT_SYSTEM_FONT_FAMILY );
	}

	cgs.fontPlayerNameSmall = SCR_RegisterFont( DEFAULT_SYSTEM_FONT_FAMILY, QFONT_STYLE_NONE, (unsigned)ceilf( 15 * scale ) );
	if( !cgs.fontPlayerNameSmall ) {
		CG_Error( "Couldn't load default font \"%s\"", DEFAULT_SYSTEM_FONT_FAMILY );
	}

	cgs.fontPlayerNameLarge = SCR_RegisterFont( DEFAULT_SYSTEM_FONT_FAMILY, QFONT_STYLE_NONE, (unsigned)ceilf( 24 * scale ) );
	if( !cgs.fontPlayerNameLarge ) {
		CG_Error( "Couldn't load default font \"%s\"", DEFAULT_SYSTEM_FONT_FAMILY );
	}
}

class CrosshairMaterialCache {
protected:
	const wsw::StringView m_pathPrefix;

	mutable wsw::StringSpanStorage<unsigned, unsigned> m_knownImageFiles;
	struct CacheEntry {
		struct Bin {
			shader_s *material { nullptr };
			unsigned cachedRequestedSize { 0 };
			std::pair<unsigned, unsigned> cachedActualSize { 0, 0 };
		} bins[2];
	};
	mutable wsw::StaticVector<CacheEntry, 16> m_cacheEntries;
public:
	explicit CrosshairMaterialCache( const wsw::StringView &pathPrefix ) noexcept : m_pathPrefix( pathPrefix ) {}

	[[nodiscard]]
	auto getMaterial( const wsw::StringView &name, bool isForMiniview, unsigned size ) -> std::optional<std::tuple<shader_s *, unsigned, unsigned>> {
		assert( !name.empty() && size > 1u );
		CacheEntry *foundEntry = nullptr;
		// TODO: Isn't this design fragile?
		assert( m_cacheEntries.size() == m_knownImageFiles.size() );
		for( unsigned i = 0; i < m_cacheEntries.size(); ++i ) {
			if( m_knownImageFiles[i].equalsIgnoreCase( name ) ) {
				foundEntry = std::addressof( m_cacheEntries[i] );
				break;
			}
		}
		if( foundEntry ) {
			CacheEntry::Bin *const bin = &foundEntry->bins[isForMiniview ? 1 : 0];
			if( foundEntry->bins[isForMiniview].cachedRequestedSize != size ) {
				wsw::StaticString<256> filePath;
				makeCrosshairFilePath( &filePath, m_pathPrefix, name );
				ImageOptions options {
					.desiredSize         = std::make_pair( size, size ),
					.borderWidth         = 1,
					.fitSizeForCrispness = true,
					.useOutlineEffect    = true,
				};
				R_UpdateExplicitlyManaged2DMaterialImage( bin->material, filePath.data(), options );
				bin->cachedRequestedSize = size;
				if( bin->material ) {
					bin->cachedActualSize = R_GetShaderDimensions( bin->material ).value();
				}
			}
			if( bin->material ) {
				return std::make_tuple( bin->material, bin->cachedActualSize.first, bin->cachedActualSize.second );
			}
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto getFileSpans() const -> const wsw::StringSpanStorage<unsigned, unsigned> & {
		if( m_knownImageFiles.empty() ) {
			wsw::fs::SearchResultHolder searchResultHolder;
			const wsw::StringView extension( ".svg" );
			if( const auto maybeSearchResult = searchResultHolder.findDirFiles( m_pathPrefix, extension ) ) {
				m_knownImageFiles.reserveSpans( m_cacheEntries.capacity() );
				for( const wsw::StringView &fileName: *maybeSearchResult ) {
					if( m_knownImageFiles.size() > m_cacheEntries.capacity() ) {
						cgWarning() << "Too many crosshair image files in" << m_pathPrefix;
						break;
					}
					if( fileName.endsWith( extension ) ) {
						m_knownImageFiles.add( fileName.dropRight( extension.size() ) );
					} else {
						m_knownImageFiles.add( fileName );
					}
				}
			}
			if( m_knownImageFiles.empty() ) {
				cgWarning() << "Failed to find crosshair files in" << m_pathPrefix;
			}
		}
		return m_knownImageFiles;
	}

	void initMaterials() {
		assert( m_cacheEntries.empty() );
		for( unsigned i = 0, maxEntries = getFileSpans().size(); i < maxEntries; ++i ) {
			m_cacheEntries.emplace_back( CacheEntry {
				.bins = {
					CacheEntry::Bin { .material = R_CreateExplicitlyManaged2DMaterial() },
					CacheEntry::Bin { .material = R_CreateExplicitlyManaged2DMaterial() },
				},
			});
		}
	}

	void destroyMaterials() {
		for( CacheEntry &entry: m_cacheEntries ) {
			for( CacheEntry::Bin &bin: entry.bins ) {
				R_ReleaseExplicitlyManaged2DMaterial( bin.material );
			}
		}
		m_cacheEntries.clear();
	}
};

static CrosshairMaterialCache g_regularCrosshairsMaterialCache( kRegularCrosshairsDirName );
static CrosshairMaterialCache g_strongCrosshairsMaterialCache( kStrongCrosshairsDirName );

auto getRegularCrosshairFiles() -> const wsw::StringSpanStorage<unsigned, unsigned> & {
	return g_regularCrosshairsMaterialCache.getFileSpans();
}

auto getStrongCrosshairFiles() -> const wsw::StringSpanStorage<unsigned, unsigned> & {
	return g_strongCrosshairsMaterialCache.getFileSpans();
}

auto getRegularCrosshairMaterial( const wsw::StringView &name, bool isForMiniview, unsigned size )
	-> std::optional<std::tuple<shader_s *, unsigned, unsigned>> {
	return g_regularCrosshairsMaterialCache.getMaterial( name, isForMiniview, size );
}

auto getStrongCrosshairMaterial( const wsw::StringView &name, bool isForMiniview, unsigned size )
	-> std::optional<std::tuple<shader_s *, unsigned, unsigned>> {
	return g_strongCrosshairsMaterialCache.getMaterial( name, isForMiniview, size );
}

void CG_InitCrosshairs() {
	g_regularCrosshairsMaterialCache.initMaterials();
	g_strongCrosshairsMaterialCache.initMaterials();
}

void CG_ShutdownCrosshairs() {
	g_regularCrosshairsMaterialCache.destroyMaterials();
	g_strongCrosshairsMaterialCache.destroyMaterials();
}