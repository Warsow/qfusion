/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2013 Victor Luchits

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
#ifndef R_LOCAL_H
#define R_LOCAL_H

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/qfiles.h>
#include <common/facilities/bsp.h>
#include <common/helpers/patch.h>
#include <common/facilities/messagestreams.h>

#ifdef ALIGN
#undef ALIGN
#endif

#define ALIGN( x, a ) ( ( ( x ) + ( ( size_t )( a ) - 1 ) ) & ~( ( size_t )( a ) - 1 ) )

typedef struct mempool_s mempool_t;
typedef struct cinematics_s cinematics_t;
typedef struct qthread_s qthread_t;
typedef struct qmutex_s qmutex_t;
typedef struct qbufPipe_s qbufPipe_t;

typedef unsigned short elem_t;

typedef vec_t instancePoint_t[8]; // quaternion for rotation + xyz pos + uniform scale

#define NUM_CUSTOMCOLORS        16

#include "ref.h"
#include "refmath.h"
#include "vattribs.h"

typedef struct superLightStyle_s {
	vattribmask_t vattribs;
	int lightmapNum[MAX_LIGHTMAPS];
	int lightmapStyles[MAX_LIGHTMAPS];
	int vertexStyles[MAX_LIGHTMAPS];
	float stOffset[MAX_LIGHTMAPS][2];
} superLightStyle_t;

#include "glimp.h"


enum drawSurfaceType_t : unsigned {
	ST_NONE,
	ST_BSP,
	ST_ALIAS,
	ST_SKELETAL,
	ST_SPRITE,
	ST_QUAD_POLY,
	ST_DYNAMIC_MESH,
	ST_PARTICLE,
	ST_CORONA,
	ST_NULLMODEL,

	ST_MAX_TYPES,
};

struct DynamicMeshDrawSurface {
	// TODO: Do we really need to keep temporaries of algorithms here
	unsigned requestedNumVertices;
	unsigned requestedNumIndices;
	unsigned actualNumVertices;
	unsigned actualNumIndices;
	unsigned verticesOffset;
	unsigned indicesOffset;
	const struct DynamicMesh *dynamicMesh;
	char scratchpad[32];
};

struct MergedBspSurface {
	struct shader_s *shader;
	struct mfog_s *fog;
	struct mesh_vbo_s *vbo;
	struct superLightStyle_s *superLightStyle;
	instancePoint_t *instances;
	unsigned numVerts, numElems;
	unsigned firstVboVert, firstVboElem;
	unsigned firstWorldSurface, numWorldSurfaces;
	unsigned numInstances;
	unsigned numLightmaps;
};

struct MultiDrawElemSpan {
	const GLsizei *counts;
	const GLvoid **indices;
	GLsizei numDraws;
};
struct portalSurface_s;

typedef struct {
	MergedBspSurface *mergedBspSurf;
	portalSurface_s *portalSurface;
	MultiDrawElemSpan mdSpan;
	unsigned dlightBits;
	float portalDistance;
} drawSurfaceBSP_t;

typedef struct {
	struct maliasmesh_s *mesh;
	struct model_s *model;
} drawSurfaceAlias_t;

typedef struct {
	struct mskmesh_s *mesh;
	struct model_s *model;
} drawSurfaceSkeletal_t;

#include <common/types/podvector.h>
#include <common/types/staticstring.h>
#include <common/helpers/freelistallocator.h>
#include <common/types/podbuffer.h>

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

struct BitmapProps {
	uint16_t width { 0 }, height { 0 }, samples { 0 };
	[[nodiscard]]
	bool operator!=( const BitmapProps &that ) const {
		return width != that.width || height != that.height || samples != that.samples;
	}
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

class ImageBuffer {
	static inline ImageBuffer *s_head;

	ImageBuffer *m_next { nullptr };
	uint8_t *m_data { nullptr };
	size_t m_capacity { 0 };
public:
	ImageBuffer() noexcept {
		m_next = s_head;
		s_head = this;
	}

	~ImageBuffer() {
		::free( m_data );
	}

	[[nodiscard]]
	auto reserveAndGet( size_t size ) -> uint8_t * {
		if( m_capacity < size ) {
			// TODO: Use something like ::mmap()/VirtualAlloc() for this kind of allocations
			if( auto *newData = (uint8_t *)::realloc( m_data, size ) ) {
				m_data = newData;
				m_capacity = size;
			} else {
				wsw::failWithBadAlloc();
			}
		}
		return m_data;
	}

	[[nodiscard]]
	auto reserveForCubemapAndGet( size_t sideSize ) -> uint8_t ** {
		if( auto rem = sideSize % 4 ) {
			sideSize += 4 - rem;
		}
		size_t headerSize = 6 * sizeof( uint8_t );
		uint8_t *rawData = reserveAndGet( headerSize + 6 * sideSize );
		auto *const header = (uint8_t **)rawData;
		rawData += headerSize;
		for( unsigned i = 0; i < 6; ++i ) {
			header[i] = rawData;
			rawData += sideSize;
		}
		return header;
	}

	static void freeAllBuffers() {
		for( ImageBuffer *buffer = s_head; buffer; buffer = buffer->m_next ) {
			::free( buffer->m_data );
			buffer->m_data = nullptr;
			buffer->m_capacity = 0;
		}
	}
};

struct shader_s;
struct mfog_s;
struct portalSurface_s;

#define MIN_RENDER_MESHES           2048

typedef struct mesh_s {
	unsigned short numVerts;
	unsigned short numElems;

	elem_t              *elems;

	vec4_t              *xyzArray;
	vec4_t              *normalsArray;
	vec4_t              *sVectorsArray;
	vec2_t              *stArray;
	vec2_t              *lmstArray[MAX_LIGHTMAPS];
	byte_vec4_t         *lmlayersArray[( MAX_LIGHTMAPS + 3 ) / 4];
	byte_vec4_t         *colorsArray[MAX_LIGHTMAPS];

	uint8_t             *blendIndices;
	uint8_t             *blendWeights;
} mesh_t;

typedef struct {
	uint64_t sortKey;
	const void *drawSurf;
	unsigned distKey;
	unsigned surfType;
	// Currently works just in addition to common mergeability checks.
	unsigned mergeabilitySeparator;
} sortedDrawSurf_t;

struct FrontendToBackendShared;

#include "shader.h"

enum {
	RB_VBO_STREAM_COMPACT       = -2, // bind RB_VBO_STREAM instead
	RB_VBO_STREAM               = -1,
	RB_VBO_NONE                 = 0,
	RB_VBO_NUM_STREAMS          = -RB_VBO_STREAM_COMPACT
};

//===================================================================

struct shader_s;
struct mfog_s;
struct superLightStyle_s;
struct portalSurface_s;
struct refScreenTexSet_s;

// core
void RB_Init();
void RB_Shutdown();
void RB_SetTime( int64_t time );
void RB_BeginFrame();
void RB_EndFrame();
void RB_BeginRegistration();
void RB_EndRegistration();

void RB_LoadCameraMatrix( const mat4_t m );
void RB_LoadObjectMatrix( const mat4_t m );
void RB_LoadProjectionMatrix( const mat4_t m );

void RB_DepthRange( float depthmin, float depthmax );
void RB_GetDepthRange( float* depthmin, float *depthmax );
//void RB_DepthOffset( bool enable );
void RB_SaveDepthRange();
void RB_RestoreDepthRange();

void RB_FlipFrontFace();
void RB_Scissor( int x, int y, int w, int h );
void RB_GetScissor( int *x, int *y, int *w, int *h );
void RB_Viewport( int x, int y, int w, int h );
void RB_Clear( int bits, float r, float g, float b, float a );
void RB_SetZClip( float zNear, float zFar );

void RB_BindFrameBufferObject( RenderTargetComponents *renderTargetComponents );

void RB_BindVBO( int id, int primitive );

void RB_AddDynamicMesh( const entity_t *entity, const shader_t *shader,
						const struct mfog_s *fog, const struct portalSurface_s *portalSurface, unsigned int shadowBits,
						const struct mesh_s *mesh, int primitive, float x_offset, float y_offset );
void RB_FlushDynamicMeshes( void );

enum : unsigned {
	UPLOAD_GROUP_DYNAMIC_MESH = 0,
	UPLOAD_GROUP_BATCHED_MESH = 1,
};

constexpr auto MAX_UPLOAD_VBO_VERTICES = (1 << 16) - 1;
constexpr auto MAX_UPLOAD_VBO_INDICES  = 6 * MAX_UPLOAD_VBO_VERTICES;

int RB_VBOIdForFrameUploads( unsigned group );

void R_BeginFrameUploads( unsigned group );
void R_SetFrameUploadMeshSubdata( unsigned group, unsigned verticesOffset, unsigned indicesOffset, const mesh_t *mesh );
void R_EndFrameUploads( unsigned group );

struct VertElemSpan {
	unsigned firstVert;
	unsigned numVerts;
	unsigned firstElem;
	unsigned numElems;
};

using DrawCallData = std::variant<VertElemSpan, MultiDrawElemSpan>;

void RB_DrawElements( const FrontendToBackendShared *fsh, int firstVert, int numVerts, int firstElem, int numElems );

void RB_DrawElements( const FrontendToBackendShared *fsh, const DrawCallData &drawCallData );

void RB_FlushTextureCache( void );

// shader
void RB_BindShader( const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable, const shader_s *shader, const mfog_s *fog );
void RB_SetLightstyle( const struct superLightStyle_s *lightStyle );
void RB_SetDlightBits( unsigned int dlightBits );
void RB_SetBonesData( int numBones, dualquat_t *dualQuats, int maxWeights );
void RB_SetPortalSurface( const struct portalSurface_s *portalSurface );
void RB_SetRenderFlags( int flags );
void RB_SetLightParams( float minLight, bool noWorldLight, float hdrExposure = 1.0f );
void RB_SetShaderStateMask( int ANDmask, int ORmask );
void RB_SetCamera( const vec3_t cameraOrigin, const mat3_t cameraAxis );
bool RB_EnableWireframe( bool enable );

/*
==============================================================================

BRUSH MODELS

==============================================================================
*/


//
// in memory representation
//
typedef struct {
	float radius;

	unsigned int firstModelSurface;
	unsigned int numModelSurfaces;

	unsigned int firstModelDrawSurface;
	unsigned int numModelDrawSurfaces;

	vec3_t mins, maxs;
} mmodel_t;

typedef struct mfog_s {
	shader_t        *shader;
	cplane_t        *visibleplane;
	vec3_t mins, maxs;
} mfog_t;

typedef struct mshaderref_s {
	char name[MAX_QPATH];
	int flags;
	int contents;
	shader_t        *shaders[NUM_SHADER_TYPES_BSP];
} mshaderref_t;

#define MAX_REF_SCENES   16 // max scenes rendered per frame
#define MAX_REF_CAMERAS  32 // max cameras rendered per frame

typedef struct msurface_s {

	vec4_t plane;

	float mins[8], maxs[8];

	unsigned facetype, flags;
	unsigned firstDrawSurfVert, firstDrawSurfElem;
	unsigned numInstances;
	unsigned mergedSurfNum;

	mutable unsigned traceFrame;

	mesh_t mesh;

	instancePoint_t *instances;

	shader_t *shader;
	mfog_t *fog;

	struct superLightStyle_s *superLightStyle;
} msurface_t;

typedef struct mnode_s {
	cplane_t        plane;

	// TODO: We can really use short indices!
	int32_t        children[2];
} mnode_t;

typedef struct mleaf_s {
	// leaf specific
	int cluster, area;

	float mins[8];
	float maxs[8];                      // for bounding box culling

	unsigned *visSurfaces;
	unsigned *fragmentSurfaces;

	// visSurfaces gets reassigned...
	void *surfDataToFree;

	unsigned numVisSurfaces;
	unsigned numFragmentSurfaces;
} mleaf_t;

typedef struct {
	uint8_t ambient[MAX_LIGHTMAPS][3];
	uint8_t diffuse[MAX_LIGHTMAPS][3];
	uint8_t styles[MAX_LIGHTMAPS];
	uint8_t direction[2];
} mgridlight_t;

typedef struct {
	int texNum;
	int texLayer;
	float texMatrix[2][2];
} mlightmapRect_t;

struct OccluderBoundsEntry {
	float mins[4], maxs[4];
};

struct OccluderDataEntry {
	unsigned numVertices;
	vec4_t plane;
	vec3_t innerPolyPoint;
	vec3_t data[7];
};

typedef struct mbrushmodel_s {
	const bspFormatDesc_t *format;

	dvis_t          *pvs;

	unsigned int numsubmodels;
	mmodel_t        *submodels;
	struct model_s  *inlines;

	unsigned int numModelSurfaces;
	unsigned int firstModelSurface;

	unsigned int numModelMergedSurfaces;
	unsigned int firstModelMergedSurface;

	msurface_t      *modelSurfaces;

	unsigned int numplanes;
	cplane_t        *planes;

	unsigned int numleafs;              // number of visible leafs, not counting 0
	mleaf_t         *leafs;
	mleaf_t         **visleafs;
	unsigned int numvisleafs;

	unsigned int numnodes;
	mnode_t         *nodes;

	unsigned int numsurfaces;
	msurface_t      *surfaces;

	unsigned int numlightgridelems;
	mgridlight_t    *lightgrid;

	unsigned int numlightarrayelems;
	int             *lightarray;

	unsigned int numfogs;
	mfog_t          *fogs;
	mfog_t          *globalfog;

	/*unsigned*/ int numareas;

	vec3_t gridSize;
	vec3_t gridMins;
	int gridBounds[4];

	unsigned int numMergedSurfaces;
	MergedBspSurface *mergedSurfaces;

	unsigned int numLightmapImages;
	Texture **lightmapImages;

	unsigned int numSuperLightStyles;
	struct superLightStyle_s *superLightStyles;

	OccluderBoundsEntry *occluderBoundsEntries;
	OccluderDataEntry *occluderDataEntries;
	unsigned numOccluders;

	struct shader_s *skyShader;
} mbrushmodel_t;

/*
==============================================================================

ALIAS MODELS

==============================================================================
*/

//
// in memory representation
//
typedef struct {
	short point[3];
	uint8_t latlong[2];                     // use bytes to keep 8-byte alignment
} maliasvertex_t;

typedef struct {
	vec3_t mins, maxs;
	vec3_t scale;
	vec3_t translate;
	float radius;
} maliasframe_t;

typedef struct {
	char name[MD3_MAX_PATH];
	quat_t quat;
	vec3_t origin;
} maliastag_t;

typedef struct {
	char name[MD3_MAX_PATH];
	shader_t        *shader;
} maliasskin_t;

typedef struct maliasmesh_s {
	char name[MD3_MAX_PATH];

	int numverts;
	maliasvertex_t *vertexes;
	vec2_t          *stArray;

	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec4_t          *sVectorsArray;

	int numtris;
	elem_t          *elems;

	int numskins;
	maliasskin_t    *skins;

	struct mesh_vbo_s *vbo;
} maliasmesh_t;

typedef struct maliasmodel_s {
	int numframes;
	maliasframe_t   *frames;

	int numtags;
	maliastag_t     *tags;

	int nummeshes;
	maliasmesh_t    *meshes;
	drawSurfaceAlias_t *drawSurfs;

	int numskins;
	maliasskin_t    *skins;

	int numverts;             // sum of numverts for all meshes
	int numtris;             // sum of numtris for all meshes
} maliasmodel_t;

/*
==============================================================================

SKELETAL MODELS

==============================================================================
*/

//
// in memory representation
//
#define SKM_MAX_WEIGHTS     4

//
// in memory representation
//
typedef struct {
	char            *name;
	shader_t        *shader;
} mskskin_t;

typedef struct {
	uint8_t indices[SKM_MAX_WEIGHTS];
	uint8_t weights[SKM_MAX_WEIGHTS];
} mskblend_t;

typedef struct mskmesh_s {
	char            *name;

	uint8_t         *blendIndices;
	uint8_t         *blendWeights;

	unsigned int numverts;
	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec2_t          *stArray;
	vec4_t          *sVectorsArray;

	unsigned int    *vertexBlends;  // [0..numbones-1] reference directly to bones
	// [numbones..numbones+numblendweights-1] reference to model blendweights

	unsigned int maxWeights;        // the maximum number of bones, affecting a single vertex in the mesh

	unsigned int numtris;
	elem_t          *elems;

	mskskin_t skin;

	struct mesh_vbo_s *vbo;
} mskmesh_t;

typedef struct {
	char            *name;
	signed int parent;
	unsigned int flags;
} mskbone_t;

typedef struct {
	vec3_t mins, maxs;
	float radius;
	bonepose_t      *boneposes;
} mskframe_t;

typedef struct mskmodel_s {
	unsigned int numbones;
	mskbone_t       *bones;

	unsigned int nummeshes;
	mskmesh_t       *meshes;
	drawSurfaceSkeletal_t *drawSurfs;

	unsigned int numtris;
	elem_t          *elems;

	unsigned int numverts;
	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec2_t          *stArray;
	vec4_t          *sVectorsArray;
	uint8_t         *blendIndices;
	uint8_t         *blendWeights;

	unsigned int numblends;
	mskblend_t      *blends;
	unsigned int    *vertexBlends;  // [0..numbones-1] reference directly to bones
	// [numbones..numbones+numblendweights-1] reference to blendweights

	unsigned int numframes;
	mskframe_t      *frames;
	bonepose_t      *invbaseposes;
	void            *stringsDataToFree;
} mskmodel_t;

//===================================================================

//
// Whole model
//

typedef enum { mod_bad = -1, mod_free, mod_brush, mod_alias, mod_skeletal } modtype_t;
typedef void ( *mod_touch_t )( struct model_s *model );

#define MOD_MAX_LODS    4

typedef struct model_s {
	char            *name;
	int registrationSequence;
	mod_touch_t touch;          // touching a model updates registration sequence, images and VBO's

	modtype_t type;

	//
	// volume occupied by the model graphics
	//
	vec3_t mins, maxs;
	float radius;

	//
	// memory representation pointer
	//
	void            *extradata;

	int lodnum;                 // LOD index, 0 for parent model, 1..MOD_MAX_LODS for LOD models
	int numlods;
	struct model_s  *lods[MOD_MAX_LODS];

	mempool_t       *mempool;
} model_t;

//============================================================================

extern model_t *r_prevworldmodel;

void        R_InitModels( void );
void        R_ShutdownModels( void );
void        R_FreeUnusedModels( void );

void        Mod_ClearAll( void );
model_t     *Mod_ForName( const char *name, bool crash );
mleaf_t     *Mod_PointInLeaf( float *p, model_t *model );
uint8_t     *Mod_ClusterPVS( int cluster, model_t *model );

void        Mod_StripLODSuffix( char *name );

//#include "program.h"

#ifdef CGAMEGETLIGHTORIGIN
#define SHADOW_MAPPING          2
#else
#define SHADOW_MAPPING          1
#endif

#define SUBDIVISIONS_MIN        3
#define SUBDIVISIONS_MAX        16
#define SUBDIVISIONS_DEFAULT    5
#define MAX_PORTAL_SURFACES     32
#define MAX_PORTAL_TEXTURES     64

#define NUM_BLOOM_LODS          4

#define BACKFACE_EPSILON        4

#define ON_EPSILON              0.1         // point on plane side epsilon

#define Z_NEAR                  4.0f
#define Z_BIAS                  64.0f

#define SIDE_FRONT              0
#define SIDE_BACK               1
#define SIDE_ON                 2

#define RF_BIT( x )               ( 1ULL << ( x ) )

#define RF_NONE                 0x0
#define RF_MIRRORVIEW           RF_BIT( 0 )
#define RF_PORTALVIEW           RF_BIT( 1 )
#define RF_ENVVIEW              RF_BIT( 2 )
#define RF_SHADOWMAPVIEW        RF_BIT( 3 )
#define RF_DRAWFLAT             RF_BIT( 5 )
#define RF_CLIPPLANE            RF_BIT( 6 )
#define RF_NOOCCLUSIONCULLING   RF_BIT( 7 )
#define RF_LIGHTMAP             RF_BIT( 8 )
#define RF_SOFT_PARTICLES       RF_BIT( 9 )
#define RF_PORTAL_CAPTURE       RF_BIT( 10 )
#define RF_SHADOWMAPVIEW_RGB    RF_BIT( 11 )
#define RF_DRAWBRIGHT           RF_BIT( 12 )

#define RF_CUBEMAPVIEW          ( RF_ENVVIEW )
#define RF_NONVIEWERREF         ( RF_PORTALVIEW | RF_MIRRORVIEW | RF_ENVVIEW | RF_SHADOWMAPVIEW )

#define MAX_REF_ENTITIES        ( MAX_ENTITIES + 48 ) // must not exceed 2048 because of sort key packing

typedef struct portalSurface_s {
	vec4_t mins, maxs;
	const entity_t *entity;
	cplane_t plane, untransformed_plane;
	const shader_t *shader;
	Texture *texures[2];            // front and back portalmaps
	void *statesForCamera[2];
} portalSurface_t;

//====================================================

// globals shared by the frontend and the backend
// the backend should never attempt modifying any of these
typedef struct {
	// any asset (model, shader, texture, etc) with has not been registered
	// or "touched" during the last registration sequence will be freed
	int registrationSequence;
	bool registrationOpen;

	// bumped each time R_RegisterWorldModel is called
	int worldModelSequence;

	float sinTableByte[256];

	model_t         *worldModel;
	mbrushmodel_t   *worldBrushModel;

	struct mesh_vbo_s *nullVBO;
	struct mesh_vbo_s *postProcessingVBO;

	vec3_t wallColor, floorColor;

	shader_t        *envShader;
	shader_t        *whiteShader;
	shader_t        *emptyFogShader;

	byte_vec4_t customColors[NUM_CUSTOMCOLORS];
} r_shared_t;

// global frontend variables are stored here
// the backend should never attempt reading or modifying them
typedef struct {
	bool in2D;
	int width2D, height2D;

	int frameBufferWidth, frameBufferHeight;

	int swapInterval;

	struct {
		unsigned average;        // updates 4 times per second
		int64_t time, oldTime;
		unsigned count, oldCount;
	} frameTime;

	volatile bool dataSync;   // call R_Finish

	char speedsMsg[2048];
	msurface_t      *debugSurface;
} r_globals_t;

extern r_shared_t rsh;
extern r_globals_t rf;

extern lightstyle_t lightStyles[MAX_LIGHTSTYLES];

constexpr const unsigned kWorldEntNumber = 0;

#include <common/facilities/configvars.h>

extern BoolConfigVar v_fullbright, v_lightmap, v_drawEntities, v_drawWorld, v_lerpModels, v_drawElements;
extern IntConfigVar v_showTris;

extern BoolConfigVar v_detailTextures;

extern IntConfigVar v_dynamicLight, v_subdivisions;

extern BoolConfigVar v_fastSky, v_portalOnly, v_portalMaps;
extern IntConfigVar v_portalMaps_maxTexSize;

extern BoolConfigVar v_lighting_deluxeMapping, v_lighting_specular, v_lighting_vertexLight;
extern BoolConfigVar v_lighting_packLightmaps, v_lighting_grayscale;
extern IntConfigVar v_lighting_maxLmBlockSize;
extern FloatConfigVar v_lighting_glossIntensity, v_lighting_glossExponent;
extern FloatConfigVar v_lighting_ambientScale, v_lighting_directedScale;
extern FloatConfigVar v_lighting_intensity;
extern IntConfigVar v_lighting_maxGlslDlights;

extern IntConfigVar v_offsetMapping;
extern FloatConfigVar v_offsetMapping_scale;
extern BoolConfigVar v_offsetMapping_reliefMapping;

extern FloatConfigVar v_outlinesWorld, v_outlinesScale, v_outlinesCutoff;

extern BoolConfigVar v_softParticles;
extern FloatConfigVar v_softParticles_scale;

extern BoolConfigVar v_hdr;
extern FloatConfigVar v_hdrGamma, v_hdrExposure;

extern IntConfigVar v_lodBias;
extern FloatConfigVar v_lodScale;

extern FloatConfigVar v_gamma;

extern StringConfigVar v_textureFilter;
extern IntConfigVar v_anisoLevel;
extern BoolConfigVar v_textureCompression;

// TODO: should we allow configuring it?
extern IntConfigVar v_stencilBits;

extern BoolConfigVar v_drawFlat;
extern ColorConfigVar v_wallColor, v_floorColor;

extern IntConfigVar v_swapInterval;

extern BoolConfigVar v_showShaderCache;

void R_NormToLatLong( const vec_t *normal, uint8_t latlong[2] );
void R_LatLongToNorm( const uint8_t latlong[2], vec3_t out );
void R_LatLongToNorm4( const uint8_t latlong[2], vec4_t out );

#define R_LinearFloatFromsRGBFloat( c ) ( ( ( c ) <= 0.04045f ) ? ( c ) * ( 1.0f / 12.92f ) : (float)pow( ( ( c ) + 0.055f ) * ( 1.0f / 1.055f ), 2.4f ) )
#define R_sRGBFloatFromLinearFloat( c ) ( ( ( c ) < 0.0031308f ) ? ( c ) * 12.92f : 1.055f * (float)pow( ( c ), 1.0f / 2.4f ) - 0.055f )
#define R_LinearFloatFromsRGB( c ) Image_LinearFloatFromsRGBFloat( ( c ) * ( 1.0f / 255.0f ) )
#define R_sRGBFloatFromLinear( c ) Image_sRGBFloatFromLinearFloat( ( c ) * ( 1.0f / 255.0f ) )

//
// r_alias.c
//
model_t *R_AliasModelLOD( const entity_t *e, const float *viewOrigin, float fovLodScale, float viewLodScale );
void R_AliasModelLerpBBox( const entity_t *e, const model_t *mod, vec3_t mins, vec3_t maxs );
bool R_AliasModelLerpTag( orientation_t *orient, const maliasmodel_t *aliasmodel, int framenum, int oldframenum,
							 float lerpfrac, const char *name );
void        R_AliasModelFrameBounds( const model_t *mod, int frame, vec3_t mins, vec3_t maxs );


//
// r_light.c
//
#define MAX_SUPER_STYLES    128

void        R_LightForOrigin( const vec3_t origin, vec3_t dir, vec4_t ambient, vec4_t diffuse, float radius, bool noWorldLight );
void        R_InitLightStyles( model_t *mod );
superLightStyle_t   *R_AddSuperLightStyle( model_t *mod, const int *lightmaps,
										   const uint8_t *lightmapStyles, const uint8_t *vertexStyles, mlightmapRect_t **lmRects );
void        R_SortSuperLightStyles( model_t *mod );
void        R_TouchLightmapImages( model_t *mod );

//
// r_main.c
//

int         R_LoadFile_( const char *path, int flags, void **buffer, const char *filename, int fileline );
void        R_FreeFile_( void *buffer, const char *filename, int fileline );

#define     R_LoadFile( path,buffer ) R_LoadFile_( path,0,buffer,__FILE__,__LINE__ )
#define     R_LoadCacheFile( path,buffer ) R_LoadFile_( path,FS_CACHE,buffer,__FILE__,__LINE__ )
#define     R_FreeFile( buffer ) R_FreeFile_( buffer,__FILE__,__LINE__ )

void        R_BeginFrame( bool forceClear, int swapInterval );
void        R_EndFrame( void );
int         R_SetSwapInterval( int swapInterval, int oldSwapInterval );
void        R_SetGamma( float gamma );
void        R_SetWallFloorColors( int wallColor, int floorColor );
void        R_Set2DMode( bool enable );
void        R_Flush( void );

/**
 * Calls R_Finish if data sync was previously deferred.
 */
void        R_DataSync( void );

/**
 * Defer R_DataSync call at the start/end of the next frame.
 */
void        R_DeferDataSync( void );

int         R_LODForSphere( const vec3_t origin, float radius, const float *viewOrigin, float fovLodScale, float viewLodScale );

void        R_InitCustomColors( void );
int         R_GetCustomColor( int num );
void        R_ShutdownCustomColors( void );

void R_BuildTangentVectors( int numVertexes, vec4_t *xyzArray, vec4_t *normalsArray, vec2_t *stArray,
							int numTris, elem_t *elems, vec4_t *sVectorsArray );

struct ParticleDrawSurface { uint16_t aggregateIndex; uint16_t particleIndex; };

struct FrontendToBackendShared {
	const Scene::DynamicLight *dynamicLights;
	const Scene::ParticlesAggregate *particleAggregates;
	std::span<const uint16_t> visibleProgramLightIndices;
	std::span<const uint16_t> allVisibleLightIndices;
	const VertElemSpan *batchedVertElemSpans;
	const void *preparedSpriteMeshes;
	unsigned preparedSpriteMeshStride;
	// TODO: How do i supply mesh
	vec3_t viewOrigin;
	mat3_t viewAxis;
	unsigned renderFlags;
	unsigned cameraId;
	unsigned sceneIndex;
};

void R_SubmitAliasSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceAlias_t *drawSurf );
void R_SubmitSkeletalSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceSkeletal_t *drawSurf );
void R_SubmitDynamicMeshToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const DynamicMeshDrawSurface *drawSurf );
void R_SubmitBSPSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf );
void R_SubmitNullSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * );

void R_SubmitSpriteSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, unsigned meshOffset );
void R_SubmitBatchedSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, unsigned vertElemSpanOffset );

//
// r_poly.c
//
bool    R_SurfPotentiallyFragmented( const msurface_t *surf );

//
// r_register.c
//

void        R_ModelBounds( const model_s *model, vec3_t mins, vec3_t maxs );
void        R_ModelFrameBounds( const model_s *model, int frame, vec3_t mins, vec3_t maxs );
void        R_RegisterWorldModel( const char *model );
model_s    *R_RegisterModel( const char *name );

int         R_SkeletalGetBoneInfo( const model_s *mod, int bonenum, char *name, size_t name_size, int *flags );
void        R_SkeletalGetBonePose( const model_s *mod, int bonenum, int frame, bonepose_t *bonepose );
int         R_SkeletalGetNumBones( const model_s *mod, int *numFrames );

shader_s    *R_RegisterShader( const char *name, int type );
shader_s    *R_RegisterPic( const char *name );
shader_s    *R_RegisterRawAlphaMask( const char *name, int width, int height, const uint8_t *data );
shader_s    *R_RegisterSkin( const char *name );
shader_s    *R_RegisterLinearPic( const char *name );

void        R_ReplaceRawSubPic( shader_s *shader, int x, int y, int width, int height, const uint8_t *data );

Skin *R_RegisterSkinFile( const char *name );

shader_s *R_CreateExplicitlyManaged2DMaterial();
void R_ReleaseExplicitlyManaged2DMaterial( shader_s *material );
bool R_UpdateExplicitlyManaged2DMaterialImage( shader_s *material, const char *name, const ImageOptions &options );

[[nodiscard]]
auto R_GetShaderDimensions( const shader_s *shader ) -> std::optional<std::pair<unsigned, unsigned>>;

void        R_BeginRegistration_();
void        R_EndRegistration_();

// By default, we reserve a core for the sound backend.
// Also, a core is always implicitly reserved for the main thread.
struct SuggestNumWorkerThreadsArgs { unsigned numExcludedCores { 1 }; };
[[nodiscard]]
auto suggestNumExtraWorkerThreads( const SuggestNumWorkerThreadsArgs &args ) -> unsigned;

void        R_Shutdown_( bool verbose );

void R_BrushModelBBox( const entity_t *e, vec3_t mins, vec3_t maxs, bool *rotated = nullptr );

struct skmcacheentry_s;

//
// r_skm.c
//
void R_AddSkeletalModelCache( const entity_t *e, const model_t *mod, unsigned sceneIndex );
model_t *R_SkeletalModelLOD( const entity_t *e, const float *viewOrigin, float fovLodScale, float viewLodScale );
skmcacheentry_s *R_GetSkeletalCache( int entNum, int lodNum, unsigned sceneIndex );
dualquat_t *R_GetSkeletalBones( skmcacheentry_s *cache );
bool R_SkeletalRenderAsFrame0( skmcacheentry_s *cache );
void R_SkeletalModelFrameBounds( const model_t *mod, int frame, vec3_t mins, vec3_t maxs );
void R_SkeletalModelLerpBBox( const entity_t *e, const model_t *mod, vec3_t mins, vec3_t maxs );
bool R_SkeletalModelLerpTag( orientation_t *orient, const mskmodel_t *skmodel, int oldframenum, int framenum, float lerpfrac, const char *name );

void        R_InitSkeletalCache( void );
void        R_ClearSkeletalCache( void );
void        R_ShutdownSkeletalCache( void );

msurface_t *R_TransformedTraceLine( VisualTrace *tr, const vec3_t start, const vec3_t end, const model_s *model,
									const float *modelOrigin, const float *modelAxis, int surfumask );

//
// r_vbo.c
//

typedef enum {
	VBO_TAG_NONE,
	VBO_TAG_WORLD,
	VBO_TAG_MODEL,
	VBO_TAG_STREAM
} vbo_tag_t;

typedef struct mesh_vbo_s {
	unsigned int index;
	int registrationSequence;
	vbo_tag_t tag;

	unsigned int vertexId;
	unsigned int elemId;
	void                *owner;
	unsigned int visframe;

	unsigned int numVerts;
	unsigned int numElems;

	size_t vertexSize;
	size_t arrayBufferSize;
	size_t elemBufferSize;

	vattribmask_t vertexAttribs;
	vattribmask_t halfFloatAttribs;

	size_t normalsOffset;
	size_t sVectorsOffset;
	size_t stOffset;
	size_t lmstOffset[( MAX_LIGHTMAPS + 1 ) / 2];
	size_t lmstSize[( MAX_LIGHTMAPS + 1 ) / 2];
	size_t lmlayersOffset[( MAX_LIGHTMAPS + 3 ) / 4];
	size_t colorsOffset[MAX_LIGHTMAPS];
	size_t bonesIndicesOffset;
	size_t bonesWeightsOffset;
	size_t spritePointsOffset;              // autosprite or autosprite2 centre + radius
	size_t instancesOffset;
} mesh_vbo_t;

void        R_InitVBO( void );
mesh_vbo_t *R_CreateMeshVBO( void *owner, int numVerts, int numElems, int numInstances,
							 vattribmask_t vattribs, vbo_tag_t tag, vattribmask_t halfFloatVattribs );
void        R_ReleaseMeshVBO( mesh_vbo_t *vbo );
void        R_TouchMeshVBO( mesh_vbo_t *vbo );
mesh_vbo_t *R_GetVBOByIndex( int index );
int         R_GetNumberOfActiveVBOs( void );
vattribmask_t R_FillVBOVertexDataBuffer( mesh_vbo_t *vbo, vattribmask_t vattribs, const mesh_t *mesh, void *outData );
void        R_UploadVBOVertexRawData( mesh_vbo_t *vbo, int vertsOffset, int numVerts, const void *data );
vattribmask_t R_UploadVBOVertexData( mesh_vbo_t *vbo, int vertsOffset, vattribmask_t vattribs, const mesh_t *mesh );
void        R_UploadVBOElemData( mesh_vbo_t *vbo, int vertsOffset, int elemsOffset, const mesh_t *mesh );
vattribmask_t R_UploadVBOInstancesData( mesh_vbo_t *vbo, int instOffset, int numInstances, instancePoint_t *instances );
void        R_FreeVBOsByTag( vbo_tag_t tag );
void        R_FreeUnusedVBOs( void );
void        R_ShutdownVBO( void );

typedef struct {
	int overbrightBits;                     // map specific overbright bits

	float ambient[3];
	byte_vec4_t outlineColor;
	byte_vec4_t environmentColor;
	float averageLightingIntensity;

	bool lightmapsPacking;
	bool lightmapArrays;                    // true if using array textures for lightmaps
	int maxLightmapSize;                    // biggest dimension of the largest lightmap
	bool deluxeMaps;                        // true if there are valid deluxemaps in the .bsp
	bool deluxeMappingEnabled;              // true if deluxeMaps is true and r_lighting_deluxemaps->integer != 0

	bool forceClear;

	bool skipSky;

	bool forceWorldOutlines;
} mapconfig_t;

extern mapconfig_t mapConfig;

struct VisualTrace;

[[nodiscard]]
inline bool startsWithUtf8Bom( const char *data, size_t length ) {
	if( length > 2 ) {
		// The data must be cast to an unsigned type first, otherwise a comparison gets elided by a compiler
		const auto *p = (const uint8_t *)data;
		if( ( p[0] == 0xEFu ) & ( p[1] == 0xBBu ) & ( p[2] == 0xBFu ) ) {
			return true;
		}
	}
	return false;
}

// TODO: This is arch-dependent...
[[nodiscard]]
wsw_forceinline bool doOverlapTestFor14Dops( const float *mins1, const float *maxs1, const float *mins2, const float *maxs2 ) {
	// TODO: Inline into call sites/reduce redundant loads

	const __m128 xmmMins1_03 = _mm_loadu_ps( mins1 + 0 ), xmmMaxs1_03 = _mm_loadu_ps( maxs1 + 0 );
	const __m128 xmmMins2_03 = _mm_loadu_ps( mins2 + 0 ), xmmMaxs2_03 = _mm_loadu_ps( maxs2 + 0 );
	const __m128 xmmMins1_47 = _mm_loadu_ps( mins1 + 4 ), xmmMaxs1_47 = _mm_loadu_ps( maxs1 + 4 );
	const __m128 xmmMins2_47 = _mm_loadu_ps( mins2 + 4 ), xmmMaxs2_47 = _mm_loadu_ps( maxs2 + 4 );

	// Mins1 [0..3] > Maxs2[0..3]
	__m128 cmp1 = _mm_cmpgt_ps( xmmMins1_03, xmmMaxs2_03 );
	// Mins2 [0..3] > Maxs1[0..3]
	__m128 cmp2 = _mm_cmpgt_ps( xmmMins2_03, xmmMaxs1_03 );
	// Mins1 [4..7] > Maxs2[4..7]
	__m128 cmp3 = _mm_cmpgt_ps( xmmMins1_47, xmmMaxs2_47 );
	// Mins2 [4..7] > Maxs1[4..7]
	__m128 cmp4 = _mm_cmpgt_ps( xmmMins2_47, xmmMaxs1_47 );
	// Zero if no comparison was successful
	return _mm_movemask_ps( _mm_or_ps( _mm_or_ps( cmp1, cmp2 ), _mm_or_ps( cmp3, cmp4 ) ) ) == 0;
}

[[nodiscard]]
auto findLightsThatAffectBounds( const Scene::DynamicLight *lights, std::span<const uint16_t> lightIndicesSpan,
								 const float *mins, const float *maxs, uint16_t *affectingLightIndices ) -> unsigned;

constexpr uint64_t kSortKeyFogOffset = 0;
constexpr uint64_t kSortKeyFogLength = 5;

constexpr uint64_t kSortKeyPortalOffset = kSortKeyFogOffset + kSortKeyFogLength;
constexpr uint64_t kSortKeyPortalLength = 5;

constexpr uint64_t kSortKeyParamsOffset = kSortKeyPortalOffset + kSortKeyPortalLength;
constexpr uint64_t kSortKeyParamsLength = 15;

constexpr uint64_t kSortKeyEntityOffset = kSortKeyParamsOffset + kSortKeyParamsLength;
constexpr uint64_t kSortKeyEntityLength = 11;

constexpr uint64_t kSortKeyShaderOffset = kSortKeyEntityOffset + kSortKeyEntityLength;
constexpr uint64_t kSortKeyShaderLength = 11;

inline uint64_t R_PackSortKey( unsigned shaderNum, int fogNum, int portalNum, unsigned entNum, int paramsNum ) {
	assert( shaderNum < ( 1 << kSortKeyShaderLength ) );
	assert( fogNum >= -1 && fogNum + 1 < (int)( 1 << kSortKeyFogLength ) );
	assert( portalNum >= -1 && portalNum + 1 < (int)( 1 << kSortKeyPortalLength ) );
	assert( entNum < ( 1 << kSortKeyEntityLength ) );
	assert( paramsNum >= -1 && paramsNum + 1 < (int)( 1 << kSortKeyParamsLength ) );

	return ( ( (uint64_t)shaderNum ) << kSortKeyShaderOffset ) |
		   ( ( (uint64_t)entNum ) << kSortKeyEntityOffset ) |
		   ( ( (uint64_t)( paramsNum + 1 ) ) << kSortKeyParamsOffset ) |
		   ( ( (uint64_t)( portalNum + 1 ) ) << kSortKeyPortalOffset ) |
		   ( ( (uint64_t)( fogNum + 1 ) ) << kSortKeyFogOffset );
}

inline void R_UnpackSortKey( uint64_t sortKey, unsigned *shaderNum, int *fogNum,
							 int *portalNum, unsigned *entNum, int *paramsNum ) {


	*shaderNum = ( sortKey >> kSortKeyShaderOffset ) & ( ( 1 << kSortKeyShaderLength ) - 1 );
	*entNum    = ( sortKey >> kSortKeyEntityOffset ) & ( ( 1 << kSortKeyEntityLength ) - 1 );
	*paramsNum = -1 + (int)( ( sortKey >> kSortKeyParamsOffset ) & ( ( 1 << kSortKeyParamsLength ) - 1 ) );
	*portalNum = -1 + (int)( ( sortKey >> kSortKeyPortalOffset ) & ( ( 1 << kSortKeyPortalLength ) - 1 ) );
	*fogNum    = -1 + (int)( ( sortKey >> kSortKeyFogOffset )    & ( ( 1 << kSortKeyFogLength ) - 1 ) );
}

#define rDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Debug ) ).getWriter()
#define rNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Notice ) ).getWriter()
#define rWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Warning ) ).getWriter()
#define rError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Error ) ).getWriter()

#endif // R_LOCAL_H
