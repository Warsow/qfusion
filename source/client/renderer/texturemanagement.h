/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2013 Victor Luchits
Copyright (C) 2025 Chasseur de bots

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

#ifndef WSW_72be7cf6_e5f9_4680_994e_3759b733465a_H
#define WSW_72be7cf6_e5f9_4680_994e_3759b733465a_H

#include "glimp.h"

#include <common/types/podbuffer.h>
#include <common/types/staticstring.h>
#include <common/helpers/freelistallocator.h>

enum {
	IT_NONE
	,IT_CLAMP           = 1 << 0
	,IT_NOMIPMAP        = 1 << 1
	,IT_NOPICMIP        = 1 << 2
	,IT_CUBEMAP         = 1 << 4
	,IT_FLIPX           = 1 << 5
	,IT_FLIPY           = 1 << 6
	,IT_FLIPDIAGONAL    = 1 << 7      // when used alone, equals to rotating 90 CW and flipping X; with FLIPX|Y, 90 CCW and flipping X
	,IT_NOCOMPRESS      = 1 << 8
	,IT_DEPTH           = 1 << 9
	,IT_NORMALMAP       = 1 << 10
	,IT_FRAMEBUFFER     = 1 << 11
	,IT_DEPTHRB         = 1 << 12     // framebuffer has a depth renderbuffer
	,IT_NOFILTERING     = 1 << 13
	,IT_ALPHAMASK       = 1 << 14     // image only contains an alpha mask
	,IT_BGRA            = 1 << 15
	,IT_SYNC            = 1 << 16     // load image synchronously
	,IT_DEPTHCOMPARE    = 1 << 17
	,IT_ARRAY           = 1 << 18
	,IT_3D              = 0
	,IT_STENCIL         = 1 << 20     // for IT_DEPTH or IT_DEPTHRB textures, whether there's stencil
	,IT_NO_DATA_SYNC    = 1 << 21     // owned by the drawing thread, do not sync in the frontend thread
	,IT_FLOAT           = 1 << 22
	,IT_SRGB            = 1 << 23
	,IT_CUSTOMFILTERING = 1 << 24

};

/**
 * These flags don't effect the actual usage and purpose of the image.
 * They are ignored when searching for an image.
 * The loader threads may modify these flags (but no other flags),
 * so they must not be used for anything that has a long-term effect.
 */
#define IT_LOADFLAGS        ( IT_ALPHAMASK | IT_BGRA | IT_SYNC | IT_SRGB )

#define IT_SPECIAL          ( IT_CLAMP | IT_NOMIPMAP | IT_NOPICMIP | IT_NOCOMPRESS )

/**
 * Image usage tags, to allow certain images to be freed separately.
 */
enum {
	IMAGE_TAG_GENERIC   = 1 << 0      // Images that don't fall into any other category.
	,IMAGE_TAG_BUILTIN  = 1 << 1      // Internal ref images that must not be released.
	,IMAGE_TAG_WORLD    = 1 << 2      // World textures.
};

enum class BuiltinTexNum {
	No,
	White,
	WhiteCubemap,
	Black,
	Grey,
	BlankBump,
	Particle,
	Corona,
	Portal0
};

class Texture {
public:
	enum Links { ListLinks, BinLinks };

	Texture *prev[2] { nullptr, nullptr };
	Texture *next[2] { nullptr, nullptr };

	Texture *nextInList() { return next[ListLinks]; }
	Texture *nextInBin() { return next[BinLinks]; }

	int flags { 0 };
	GLuint texnum { 0 };              // gl texture binding
	GLuint target { 0 };
	int width { -1 }, height { -1 };  // source image
	int layers { -1 };                // texture array size
	int samples { -1 };
	int tags { 0 };                   // usage tags of the image, illegal by default so we're forced to set proper ones
};

class FontMask : public Texture {
public:
	explicit FontMask( GLuint handle, unsigned width, unsigned height, int flags ) {
		this->texnum = handle;
		this->target = GL_TEXTURE_2D;
		this->width = width;
		this->height = height;
		this->samples = 1;
		this->flags = flags;
	}
};

class Lightmap : public Texture {
	friend class TextureFactory;
	Lightmap( GLuint handle, unsigned w, unsigned h, unsigned samples, int flags ) {
		assert( samples == 1 || samples == 3 );
		this->texnum = handle;
		this->target = GL_TEXTURE_2D;
		this->width = w;
		this->height = h;
		this->samples = samples;
		this->flags = flags;
	}
};

class LightmapArray : public Texture {
	friend class TextureFactory;
	LightmapArray( GLuint handle, unsigned w, unsigned h, unsigned samples, unsigned layers, int flags ) {
		assert( samples == 1 || samples == 3 );
		this->texnum = handle;
		this->width = w;
		this->height = h;
		this->target = GL_TEXTURE_2D_ARRAY;
		this->samples = samples;
		this->layers = layers;
		this->flags = flags;
	}
};

class Raw2DTexture : public Texture {
	friend class TextureFactory;
	explicit Raw2DTexture( GLuint handle ) {
		this->texnum = handle;
		this->target = GL_TEXTURE_2D;
		this->flags = IT_CLAMP;
	}
};

struct BitmapProps {
	uint16_t width { 0 }, height { 0 }, samples { 0 };
	[[nodiscard]]
	bool operator!=( const BitmapProps &that ) const {
		return width != that.width || height != that.height || samples != that.samples;
	}
};

class MaterialTexture : public Texture {
	const wsw::HashedStringView m_name;
protected:
	MaterialTexture( const wsw::HashedStringView &name, GLuint handle, GLuint target, BitmapProps bitmapProps, int flags )
		: m_name( name ) {
		this->texnum = handle;
		this->target = target;
		this->width = bitmapProps.width;
		this->height = bitmapProps.height;
		this->samples = bitmapProps.samples;
		this->flags = flags;
	}
public:
	int registrationSequence { 0 };
	unsigned binIndex { ~0u };

	[[nodiscard]]
	auto getName() const -> const wsw::HashedStringView & { return m_name; }
};

class Material2DTexture : public MaterialTexture {
	friend class TextureFactory;
	Material2DTexture( const wsw::HashedStringView &name, GLuint handle, BitmapProps bitmapProps, int flags )
		: MaterialTexture( name, handle, GL_TEXTURE_2D, bitmapProps, flags ) {}
};

class MaterialCubemap : public MaterialTexture {
	friend class TextureFactory;
	MaterialCubemap( const wsw::HashedStringView &name, GLuint handle, BitmapProps bitmapProps, int flags )
		: MaterialTexture( name, handle, GL_TEXTURE_CUBE_MAP, bitmapProps, flags ) {}
};

class RenderTarget;

class RenderTargetTexture : public Texture {
public:
	RenderTarget *attachedToRenderTarget { nullptr };
};

class RenderTargetDepthBuffer {
public:
	RenderTarget *attachedToRenderTarget { nullptr };
	GLuint rboId { 0 };
};

class RenderTarget {
public:
	RenderTargetTexture *attachedTexture;
	RenderTargetDepthBuffer *attachedDepthBuffer;
	GLuint fboId { 0 };
};

class RenderTargetComponents {
public:
	RenderTarget *renderTarget { nullptr };
	RenderTargetTexture *texture { nullptr };
	RenderTargetDepthBuffer *depthBuffer { nullptr };
};

class TextureCache;
class ImageBuffer;

class TextureManagementShared {
	static wsw::StaticString<64> s_cleanNameBuffer;
protected:
	static constexpr auto kMaxPortalRenderTargets = 4u;

	// This terminology is valid only for 2D textures but it's is trivial/convenient to use
	enum TextureFilter {
		Nearest,
		Bilinear,
		Trilinear
	};

	static const std::pair<wsw::StringView, TextureFilter> kTextureFilterNames[3];
	static const std::pair<GLuint, GLuint> kTextureFilterGLValues[3];

	[[nodiscard]]
	static auto findFilterByName( const wsw::StringView &name ) -> std::optional<TextureFilter>;

	[[nodiscard]]
	static auto makeCleanName( const wsw::StringView &rawName, const wsw::StringView &suffix )
	-> std::optional<wsw::StringView>;

	void bindToModify( Texture *texture ) { bindToModify( texture->target, texture->texnum ); }
	void unbindModified( Texture *texture ) { unbindModified( texture->target, texture->texnum ); }

	void bindToModify( GLenum target, GLuint texture );
	void unbindModified( GLenum target, GLuint texture );

	void applyFilter( Texture *texture, GLuint minify, GLuint magnify );
	void applyAniso( Texture *texture, int aniso );
};

class TextureHistogram {
public:
	TextureHistogram( const TextureHistogram & ) = delete;
	auto operator=( const TextureHistogram & ) = delete;
	TextureHistogram( TextureHistogram && ) = delete;
	auto operator=( TextureHistogram && ) = delete;

	TextureHistogram();
	~TextureHistogram();

	void addTexelColor( unsigned color );
	void clear();
	[[nodiscard]]
	auto findDominantColor() const -> std::optional<unsigned>;
private:
	// Hide the heavyweight STL stuff in the private part
	void *m_priv;
};

class TextureFactory : TextureManagementShared {
private:
	friend class TextureCache;

	static constexpr auto kMaxMaterialTextures = 1024u;
	static constexpr auto kMaxMaterialCubemaps = 64u;
	static constexpr auto kMaxRaw2DTextures = 32u;
	static constexpr auto kMaxFontMasks = 32u;
	static constexpr auto kMaxLightmaps = 32u;

	static constexpr auto kMaxBuiltinTextures = 16u;

	wsw::HeapBasedFreelistAllocator m_materialTexturesAllocator { sizeof( Material2DTexture ), kMaxMaterialTextures };
	wsw::HeapBasedFreelistAllocator m_materialCubemapsAllocator { sizeof( MaterialCubemap ), kMaxMaterialCubemaps };
	wsw::HeapBasedFreelistAllocator m_raw2DTexturesAllocator { sizeof( Raw2DTexture ), kMaxRaw2DTextures };
	wsw::HeapBasedFreelistAllocator m_fontMasksAllocator { sizeof( FontMask ), kMaxFontMasks };
	wsw::HeapBasedFreelistAllocator m_lightmapsAllocator { wsw::max( sizeof( Lightmap ), sizeof( LightmapArray ) ), kMaxLightmaps };
	wsw::HeapBasedFreelistAllocator m_builtinTexturesAllocator { sizeof( Texture ), kMaxBuiltinTextures };

	wsw::MemberBasedFreelistAllocator<sizeof( RenderTargetTexture ), kMaxPortalRenderTargets> m_renderTargetTexturesAllocator;
	wsw::MemberBasedFreelistAllocator<sizeof( RenderTarget ), kMaxPortalRenderTargets> m_renderTargetsAllocator;
	wsw::MemberBasedFreelistAllocator<sizeof( RenderTargetDepthBuffer ), 2> m_renderTargetDepthBufferAllocator;

	static constexpr unsigned kMaxNameLen = 63;
	static constexpr unsigned kNameDataStride = kMaxNameLen + 1;

	PodBuffer<char> m_nameDataStorage;

	TextureFilter m_textureFilter { Trilinear };
	int m_anisoLevel { 1 };

	/**
	 * The name is a little reference to {@code java.lang.String::intern()}
	 */
	[[nodiscard]]
	auto internTextureName( unsigned storageIndex, const wsw::HashedStringView &name ) -> wsw::HashedStringView;

	[[nodiscard]]
	auto loadTextureDataFromFile( const wsw::StringView &name, ImageBuffer *readBuffer,
								  ImageBuffer *dataBuffer, ImageBuffer *conversionBuffer,
								  const ImageOptions &imageOptions )
	-> std::optional<std::pair<uint8_t*, BitmapProps>>;

	[[nodiscard]]
	bool tryUpdatingFilterOrAniso( TextureFilter filter, int givenAniso, int *anisoToApply,
								   bool *doApplyFilter, bool *doApplyAniso );

	void setupWrapMode( GLuint target, unsigned flags );
	void setupFilterMode( GLuint target, unsigned flags );

	// TODO: Return a RAII wrapper?
	[[nodiscard]]
	auto generateHandle( const wsw::StringView &label ) -> GLuint;

	struct Builtin2DTextureData {
		const uint8_t *bytes { nullptr };
		unsigned width { 0 }, height { 0 };
		unsigned samples { 0 };
		unsigned flags { 0 };
		bool nearestFilteringOnly {false };
	};

	[[nodiscard]]
	auto createBuiltin2DTexture( const Builtin2DTextureData &data ) -> Texture *;
	[[nodiscard]]
	auto createSolidColorBuiltinTexture( const float *color ) -> Texture *;
	[[nodiscard]]
	auto createBuiltinNoTextureTexture() -> Texture *;
	[[nodiscard]]
	auto createBuiltinBlankNormalmap() -> Texture *;
	[[nodiscard]]
	auto createBuiltinWhiteCubemap() -> Texture *;
	[[nodiscard]]
	auto createBuiltinParticleTexture() -> Texture *;
	[[nodiscard]]
	auto createBuiltinCoronaTexture() -> Texture *;
	[[nodiscard]]
	auto createUITextureHandleWrapper() -> Texture *;

	void releaseBuiltinTexture( Texture *texture );

	void releaseMaterialTexture( Material2DTexture *texture );
	void releaseMaterialCubemap( MaterialCubemap *cubemap );

	[[nodiscard]]
	auto createRenderTargetTexture( unsigned width, unsigned height ) -> RenderTargetTexture *;
	[[nodiscard]]
	auto createRenderTargetDepthBuffer( unsigned width, unsigned height ) -> RenderTargetDepthBuffer *;
	[[nodiscard]]
	auto createRenderTarget() -> RenderTarget *;

	void releaseRenderTargetTexture( RenderTargetTexture * );
	void releaseRenderTargetDepthBuffer( RenderTargetDepthBuffer * );
	void releaseRenderTarget( RenderTarget * );
public:
	TextureFactory();

	[[nodiscard]]
	auto loadMaterialTexture( const wsw::HashedStringView &name, unsigned flags ) -> Material2DTexture *;

	[[nodiscard]]
	auto loadMaterialCubemap( const wsw::HashedStringView &name, unsigned flags ) -> MaterialCubemap *;

	[[nodiscard]]
	bool addTextureColorsToHistogram( const wsw::StringView &path, TextureHistogram *histogram );

	[[nodiscard]]
	auto createFontMask( unsigned w, unsigned h, const uint8_t *data ) -> Texture *;

	[[nodiscard]]
	auto createLightmap( unsigned w, unsigned h, unsigned samples, const uint8_t *data ) -> Texture *;

	[[nodiscard]]
	auto createLightmapArray( unsigned w, unsigned h, unsigned numLayers, unsigned samples ) -> Texture *;

	void replaceLightmapLayer( Texture *texture, unsigned layer, const uint8_t *data );

	void replaceFontMaskSamples( Texture *texture, unsigned x, unsigned y, unsigned width, unsigned height, const uint8_t *data );

	[[nodiscard]]
	auto asMaterial2DTexture( Texture *texture ) -> Material2DTexture * {
		return m_materialTexturesAllocator.mayOwn( texture ) ? (Material2DTexture *)texture : nullptr;
	}
	[[nodiscard]]
	auto asMaterialCubemap( Texture *texture ) -> MaterialCubemap * {
		return m_materialCubemapsAllocator.mayOwn( texture ) ? (MaterialCubemap *)texture : nullptr;
	}

	[[nodiscard]]
	auto createRaw2DTexture() -> Raw2DTexture *;
	void releaseRaw2DTexture( Raw2DTexture *texture );
	[[nodiscard]]
	bool updateRaw2DTexture( Raw2DTexture *texture, const wsw::StringView &name, const ImageOptions &options );
};

class TextureCache : TextureManagementShared {
	TextureFactory m_factory;

	Texture *m_builtinTextures[(size_t)BuiltinTexNum::Portal0] {};
	Texture *m_menuTextureWrapper { nullptr };
	Texture *m_hudTextureWrapper { nullptr };

	struct PortalRenderTargetComponents : public RenderTargetComponents {
		unsigned drawSceneFrameNum { 0 };
	};

	wsw::StaticVector<PortalRenderTargetComponents, kMaxPortalRenderTargets> m_portalRenderTargets;
	RenderTargetDepthBuffer *m_portalRenderTargetDepthBuffer { nullptr };

	wsw::StaticVector<RenderTargetComponents, 1> m_miniviewRenderTarget;
	RenderTargetDepthBuffer *m_miniviewRenderTargetDepthBuffer { nullptr };

	static constexpr unsigned kNumHashBins = 101;

	Material2DTexture *m_materialTextureBins[kNumHashBins] { nullptr };
	MaterialCubemap *m_materialCubemapBins[kNumHashBins] { nullptr };

	Material2DTexture *m_materialTexturesHead { nullptr };
	MaterialCubemap *m_materialCubemapsHead { nullptr };

	template <typename T>
	[[nodiscard]]
	auto findCachedTextureInBin( T *binHead, const wsw::HashedStringView &name, unsigned flags ) -> T *;

	template <typename T, typename Method>
	[[nodiscard]]
	auto getTexture(  const wsw::StringView &name, const wsw::StringView &suffix,
					  unsigned flags, T **listHead, T **bins, Method methodOfFactory ) -> T *;

	void applyFilterOrAnisoInList( Texture *listHead, TextureFilter filter,
								   int aniso, bool applyFilter, bool applyAniso );
	[[nodiscard]]
	auto wrapTextureHandle( GLuint externalTexNum, Texture *reuse ) -> Texture *;

	[[nodiscard]]
	auto createRenderTargetAndTexture( unsigned width, unsigned height ) -> std::optional<std::pair<RenderTarget *, RenderTargetTexture *>>;
public:
	TextureCache();
	~TextureCache();

	static void init();
	static void shutdown();

	static auto instance() -> TextureCache *;
	// FIXME this is a grand hack for being compatible with the existing code. It must eventually be removed.
	[[nodiscard]]
	static auto maybeInstance() -> TextureCache *;

	void applyFilter( const wsw::StringView &name, int anisoLevel );

	[[nodiscard]]
	auto getBuiltinTexture( BuiltinTexNum number ) -> Texture * {
		assert( number < BuiltinTexNum::Portal0 );
		Texture *result = m_builtinTextures[(unsigned)number];
		assert( result );
		return result;
	}

	[[nodiscard]]
	auto getUnderlyingFactory() -> TextureFactory * { return &m_factory; }

	[[nodiscard]]
	auto getMaterial2DTexture( const wsw::StringView &name, const wsw::StringView &suffix,
							   unsigned tags ) -> Material2DTexture *;

	[[nodiscard]]
	auto getMaterial2DTexture( const wsw::StringView &name, unsigned flags ) -> Material2DTexture * {
		return getMaterial2DTexture( name, wsw::StringView(), flags );
	}

	[[nodiscard]]
	auto getMaterialCubemap( const wsw::StringView &name, unsigned flags ) -> MaterialCubemap *;

	// Hacks for sky
	[[nodiscard]]
	bool addTextureColorsToHistogram( const wsw::StringView &name, const wsw::StringView &suffix, TextureHistogram *histogram );

	[[nodiscard]]
	auto noTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::No ); }
	[[nodiscard]]
	auto whiteTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::White ); }
	[[nodiscard]]
	auto greyTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::Grey ); }
	[[nodiscard]]
	auto blackTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::Black ); }
	[[nodiscard]]
	auto blankNormalmap() -> Texture * { return getBuiltinTexture( BuiltinTexNum::BlankBump ); }
	[[nodiscard]]
	auto whiteCubemapTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::WhiteCubemap ); }
	[[nodiscard]]
	auto particleTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::Particle ); }
	[[nodiscard]]
	auto coronaTexture() -> Texture * { return getBuiltinTexture( BuiltinTexNum::Corona ); }

	[[nodiscard]]
	auto wrapMenuTextureHandle( GLuint externalTexNum ) -> Texture * { return wrapTextureHandle( externalTexNum, m_menuTextureWrapper ); }
	[[nodiscard]]
	auto wrapHudTextureHandle( GLuint externalTexNum ) -> Texture * { return wrapTextureHandle( externalTexNum, m_hudTextureWrapper ); }

	void createPrimaryRenderTargetAttachments() {}
	void releasePrimaryRenderTargetAttachments() {}

	void touchTexture( Texture *texture, unsigned tags );

	void freeUnusedWorldTextures();
	void freeAllUnusedTextures();

	[[nodiscard]]
	auto getPortalRenderTarget( unsigned drawSceneFrameNum ) -> RenderTargetComponents *;

	[[nodiscard]]
	auto getMiniviewRenderTarget() -> RenderTargetComponents *;
};

#endif