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

#include <common/helpers/singletonholder.h>
#include <common/helpers/links.h>
#include "local.h"
#include "texturemanagement.h"

using wsw::operator""_asView;

static SingletonHolder<TextureCache> g_instanceHolder;
static bool g_initialized;

wsw::StaticString<64> TextureManagementShared::s_cleanNameBuffer;

void TextureCache::init() {
	g_instanceHolder.init();
	g_initialized = true;
}

void TextureCache::shutdown() {
	g_instanceHolder.shutdown();
	g_initialized = false;
}

auto TextureCache::instance() -> TextureCache * {
	return g_instanceHolder.instance();
}

auto TextureCache::maybeInstance() -> TextureCache * {
	if( g_initialized ) {
		return g_instanceHolder.instance();
	}
	return nullptr;
}

TextureCache::TextureCache() {
	std::fill( std::begin( m_builtinTextures ), std::end( m_builtinTextures ), nullptr );

	m_builtinTextures[(unsigned)BuiltinTexNum::No]           = m_factory.createBuiltinNoTextureTexture();
	m_builtinTextures[(unsigned)BuiltinTexNum::White]        = m_factory.createSolidColorBuiltinTexture( colorWhite );
	m_builtinTextures[(unsigned)BuiltinTexNum::WhiteCubemap] = m_factory.createBuiltinWhiteCubemap();
	m_builtinTextures[(unsigned)BuiltinTexNum::Black]        = m_factory.createSolidColorBuiltinTexture( colorBlack );
	m_builtinTextures[(unsigned)BuiltinTexNum::Grey]         = m_factory.createSolidColorBuiltinTexture( colorMdGrey );
	m_builtinTextures[(unsigned)BuiltinTexNum::BlankBump]    = m_factory.createBuiltinBlankNormalmap();
	m_builtinTextures[(unsigned)BuiltinTexNum::Particle]     = m_factory.createBuiltinParticleTexture();
	m_builtinTextures[(unsigned)BuiltinTexNum::Corona]       = m_factory.createBuiltinCoronaTexture();

	m_menuTextureWrapper = m_factory.createUITextureHandleWrapper();
	m_hudTextureWrapper  = m_factory.createUITextureHandleWrapper();

	// TODO: Don't store it as a member?
	m_miniviewRenderTargetDepthBuffer = m_factory.createRenderTargetDepthBuffer( glConfig.width, glConfig.height );
	if( m_miniviewRenderTargetDepthBuffer ) {
		auto maybeRenderTargetAndRexture = createRenderTargetAndTexture( glConfig.width, glConfig.height );
		if( !maybeRenderTargetAndRexture ) {
			// TODO: Eliminate the duplication with the code below
			wsw::failWithRuntimeError( "Failed to create miniview render target" );
		}
		auto *const components = new( m_miniviewRenderTarget.unsafe_grow_back() )RenderTargetComponents;
		components->renderTarget = maybeRenderTargetAndRexture->first;
		components->texture      = maybeRenderTargetAndRexture->second;
		components->depthBuffer  = m_miniviewRenderTargetDepthBuffer;

		// Setup attachments TODO: Fix state management
		R_BindRenderTarget( components );
		R_BindRenderTarget( nullptr );
	} else {
		// TODO: Eliminate this duplication
		wsw::failWithRuntimeError( "Failed to create miniview render target" );
	}

	const auto portalBufferSide = wsw::min<unsigned>( 1 << Q_log2( wsw::max( 1, v_portalMaps_maxTexSize.get() ) ),
													  wsw::min( 1024, glConfig.maxRenderbufferSize ) );

	// TODO: Should we use separate depth buffers?
	m_portalRenderTargetDepthBuffer = m_factory.createRenderTargetDepthBuffer( portalBufferSide, portalBufferSide );
	if( m_portalRenderTargetDepthBuffer ) {
		do {
			auto maybeRenderTargetAndTexture = createRenderTargetAndTexture( portalBufferSide, portalBufferSide );
			if( !maybeRenderTargetAndTexture ) {
				break;
			}
			auto *const components   = new( m_portalRenderTargets.unsafe_grow_back() )PortalRenderTargetComponents;
			components->renderTarget = maybeRenderTargetAndTexture->first;
			components->texture      = maybeRenderTargetAndTexture->second;
			components->depthBuffer  = m_portalRenderTargetDepthBuffer;
		} while( !m_portalRenderTargets.full() );
	}
}

TextureCache::~TextureCache() {
	m_factory.releaseBuiltinTexture( m_menuTextureWrapper );
	m_factory.releaseBuiltinTexture( m_hudTextureWrapper );
	for( Texture *texture: m_builtinTextures ) {
		m_factory.releaseBuiltinTexture( texture );
	}

	wsw::StaticVector<RenderTargetComponents *, kMaxPortalRenderTargets + 1> allRenderTargets;
	for( RenderTargetComponents &components: m_portalRenderTargets ) {
		allRenderTargets.push_back( std::addressof( components ) );
	}
	allRenderTargets.push_back( std::addressof( m_miniviewRenderTarget.front() ) );

	for( RenderTargetComponents *components: allRenderTargets ) {
		if( RenderTarget *attachmentTarget = components->texture->attachedToRenderTarget ) {
			qglBindFramebuffer( GL_FRAMEBUFFER, attachmentTarget->fboId );
			qglFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0 );
			components->texture->attachedToRenderTarget = nullptr;
			attachmentTarget->attachedTexture           = nullptr;
			m_factory.releaseRenderTargetTexture( components->texture );
		}
	}

	for( RenderTargetDepthBuffer *depthBuffer: { m_portalRenderTargetDepthBuffer, m_miniviewRenderTargetDepthBuffer } ) {
		if( depthBuffer ) {
			if( RenderTarget *attachmentTarget = depthBuffer->attachedToRenderTarget ) {
				qglBindFramebuffer( GL_FRAMEBUFFER, attachmentTarget->fboId );
				qglFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, 0 );
				depthBuffer->attachedToRenderTarget  = nullptr;
				attachmentTarget->attachedDepthBuffer = nullptr;
			}
			m_factory.releaseRenderTargetDepthBuffer( depthBuffer );
		}
	}

	for( RenderTargetComponents &components: m_portalRenderTargets ) {
		m_factory.releaseRenderTarget( components.renderTarget );
	}
	for( RenderTargetComponents &components: m_miniviewRenderTarget ) {
		m_factory.releaseRenderTarget( components.renderTarget );
	}

	qglBindFramebuffer( GL_FRAMEBUFFER, 0 );
}

auto TextureCache::createRenderTargetAndTexture( unsigned width, unsigned height )
	-> std::optional<std::pair<RenderTarget *, RenderTargetTexture *>> {
	RenderTarget *renderTarget = m_factory.createRenderTarget();
	if( !renderTarget ) {
		return std::nullopt;
	}
	RenderTargetTexture *texture = m_factory.createRenderTargetTexture( width, height );
	if( !texture ) {
		m_factory.releaseRenderTarget( renderTarget );
		return std::nullopt;
	}
	return std::make_pair( renderTarget, texture );
}

auto TextureManagementShared::makeCleanName( const wsw::StringView &rawName, const wsw::StringView &suffix )
	-> std::optional<wsw::StringView> {
	wsw::StringView name( rawName );
	if( name.startsWith( '/' ) || name.startsWith( '\\' ) ) {
		name = name.drop( 1 );
	}
	if( name.empty() || name.length() + suffix.length() >= s_cleanNameBuffer.capacity() ) {
		return std::nullopt;
	}

	int lastDotIndex = -1, lastSlashIndex = -1;
	s_cleanNameBuffer.clear();
	for( int i = 0; i < (int)name.size(); ++i ) {
		const char ch = name[i];
		if( ch == '.' ) {
			lastDotIndex = i;
		}
		if( ch == '\\' ) {
			s_cleanNameBuffer.append( '/' );
		} else {
			s_cleanNameBuffer.append( ch );
		}
		if( s_cleanNameBuffer.back() == '/' ) {
			lastSlashIndex = i;
		}
	}

	if( lastDotIndex >= 0 && lastDotIndex < lastSlashIndex ) {
		s_cleanNameBuffer.erase( (unsigned)lastDotIndex );
	}
	s_cleanNameBuffer.append( suffix );
	return s_cleanNameBuffer.asView();
}

template <typename T>
auto TextureCache::findCachedTextureInBin( T *binHead, const wsw::HashedStringView &name, unsigned flags ) -> T * {
	for( T *texture = binHead; texture; texture = (T *)texture->next[Texture::BinLinks] ) {
		if( texture->getName().equalsIgnoreCase( name ) ) {
			if( ( (unsigned)texture->flags & ~IT_LOADFLAGS ) == ( flags & ~IT_LOADFLAGS ) ) {
				return texture;
			}
		}
	}

	return nullptr;
}

template <typename T, typename Method>
auto TextureCache::getTexture( const wsw::StringView &name, const wsw::StringView &suffix,
							   unsigned flags, T **listHead, T **bins, Method methodOfFactory ) -> T * {
	T *texture = nullptr;
	if( const auto maybeCleanName = makeCleanName( name, suffix ) ) {
		const wsw::HashedStringView hashedCleanName( *maybeCleanName );
		const auto binIndex = hashedCleanName.getHash() % kNumHashBins;
		if( !( texture = findCachedTextureInBin( bins[binIndex], hashedCleanName, flags ) ) ) {
			if( ( texture = ( &m_factory->*methodOfFactory )( hashedCleanName, flags ) ) ) {
				wsw::link( texture, &bins[binIndex], Texture::BinLinks );
				wsw::link( texture, listHead, Texture::ListLinks );
				texture->binIndex = binIndex;
			}
		}
	}
	return texture;
}

auto TextureCache::getMaterial2DTexture( const wsw::StringView &name, const wsw::StringView &suffix, unsigned flags ) -> Material2DTexture * {
	return getTexture( name, suffix, flags, &m_materialTexturesHead, m_materialTextureBins, &TextureFactory::loadMaterialTexture );
}

auto TextureCache::getMaterialCubemap( const wsw::StringView &name, unsigned flags ) -> MaterialCubemap * {
	return getTexture( name, ""_asView, flags, &m_materialCubemapsHead, m_materialCubemapBins, &TextureFactory::loadMaterialCubemap );
}

bool TextureCache::addTextureColorsToHistogram( const wsw::StringView &name, const wsw::StringView &suffix, TextureHistogram *histogram ) {
	if( const auto maybeCleanName = makeCleanName( name, suffix ) ) {
		return m_factory.addTextureColorsToHistogram( *maybeCleanName, histogram );
	}
	return false;
}

auto TextureCache::wrapTextureHandle( GLuint externalHandle, Texture *reuse ) -> Texture * {
	assert( reuse );
	reuse->texnum  = externalHandle;
	reuse->width   = glConfig.width;
	reuse->height  = glConfig.height;
	reuse->samples = 4;
	return reuse;
}

const std::pair<wsw::StringView, TextureManagementShared::TextureFilter> TextureManagementShared::kTextureFilterNames[3] {
	{ "nearest"_asView, Nearest },
	{ "bilinear"_asView, Bilinear },
	{ "trilinear"_asView, Trilinear }
};

const std::pair<GLuint, GLuint> TextureManagementShared::kTextureFilterGLValues[3] {
	// We have decided to apply mipmap filtering for the "nearest" filter.
	{ GL_NEAREST_MIPMAP_LINEAR, GL_NEAREST },
	{ GL_LINEAR_MIPMAP_NEAREST, GL_LINEAR },
	{ GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR }
};

auto TextureManagementShared::findFilterByName( const wsw::StringView &name ) -> std::optional<TextureFilter> {
	for( const auto &[filterName, filter]: kTextureFilterNames ) {
		if( name.equalsIgnoreCase( filterName ) ) {
			return filter;
		}
	}
	return std::nullopt;
}

void TextureManagementShared::bindToModify( GLenum target, GLuint texture ) {
	qglBindTexture( target, texture );
	assert( qglGetError() == GL_NO_ERROR );
}

void TextureManagementShared::unbindModified( GLenum target, GLuint texture ) {
	// TODO: Check whether it's actively bound (it must be) for debugging purposes
	qglBindTexture( target, 0 );
	assert( qglGetError() == GL_NO_ERROR );
}

void TextureManagementShared::applyFilter( Texture *texture, GLuint minify, GLuint magnify ) {
	// TODO: Check whether it's bound for modifying
	// TODO: What about usign the bindless API?
	if( texture->flags & ( IT_NOFILTERING | IT_DEPTH | IT_CUSTOMFILTERING ) ) {
		return;
	}
	GLuint minifyToApply = ( texture->flags & IT_NOMIPMAP ) ? magnify : minify;
	qglTexParameteri( texture->target, GL_TEXTURE_MIN_FILTER, minifyToApply );
	qglTexParameteri( texture->target, GL_TEXTURE_MAG_FILTER, magnify );
}

void TextureManagementShared::applyAniso( Texture *texture, int level ) {
	assert( glConfig.ext.texture_filter_anisotropic );
	assert( level >= 1 && level <= glConfig.maxTextureFilterAnisotropic );
	if( ( texture->flags & ( IT_NOFILTERING | IT_DEPTH | IT_NOMIPMAP | IT_CUSTOMFILTERING ) ) ) {
		return;
	}
	qglTexParameteri( texture->target, GL_TEXTURE_MAX_ANISOTROPY_EXT, level );
}

void TextureCache::applyFilterOrAnisoInList( Texture *listHead, TextureFilter filter,
											 int aniso, bool applyFilter, bool applyAniso ) {
	assert( (unsigned)filter < std::size( kTextureFilterGLValues ) );
	const auto [minify, magnify] = kTextureFilterGLValues[(unsigned)filter];

	Texture *lastTexture = nullptr;
	for( Texture *texture = listHead; texture; texture = texture->nextInList() ) {
		// There are individual checks in each apply() subroutine.
		// We add another early check to avoid doing a fruitless bind.
		if( texture->flags & ( IT_NOFILTERING | IT_DEPTH | IT_CUSTOMFILTERING ) ) {
			continue;
		}
		bindToModify( texture );
		lastTexture = texture;
		if( applyFilter ) {
			TextureManagementShared::applyFilter( texture, minify, magnify );
		}
		if( applyAniso ) {
			TextureManagementShared::applyAniso( texture, aniso );
		}
	}
	if( lastTexture ) {
		unbindModified( lastTexture );
	}
}

void TextureCache::applyFilter( const wsw::StringView &name, int anisoLevel ) {
	const auto maybeFilter = findFilterByName( name );
	if( !maybeFilter ) {
		rWarning() << "Failed to find a texture filter by name" << name;
		return;
	}

	int anisoToApply = 1;
	const TextureFilter filter = *maybeFilter;
	bool doApplyFilter = false, doApplyAniso = false;
	if( !m_factory.tryUpdatingFilterOrAniso( filter, anisoLevel, &anisoToApply, &doApplyFilter, &doApplyAniso ) ) {
		return;
	}

	applyFilterOrAnisoInList( m_materialTexturesHead, filter, anisoToApply, doApplyFilter, doApplyAniso );
	applyFilterOrAnisoInList( m_materialCubemapsHead, filter, anisoToApply, doApplyFilter, doApplyAniso );
}

void TextureCache::touchTexture( Texture *texture, unsigned tags ) {
	assert( tags );
	assert( ( tags & ~( IMAGE_TAG_GENERIC | IMAGE_TAG_BUILTIN | IMAGE_TAG_WORLD ) ) == 0 );
	assert( ( texture->tags & ~( IMAGE_TAG_GENERIC | IMAGE_TAG_BUILTIN | IMAGE_TAG_WORLD ) ) == 0 );
	texture->tags |= tags;
	MaterialTexture *materialTexture = m_factory.asMaterial2DTexture( texture );
	if( !materialTexture ) {
		materialTexture = m_factory.asMaterialCubemap( texture );
	}
	if( materialTexture ) {
		materialTexture->registrationSequence = rsh.registrationSequence;
	}
}

void TextureCache::freeUnusedWorldTextures() {
	const int registrationSequence = rsh.registrationSequence;
	Material2DTexture *nextTexture = nullptr;
	for( Material2DTexture *texture = m_materialTexturesHead; texture; texture = nextTexture ) {
		nextTexture = (Material2DTexture *)texture->nextInList();
		if( ( texture->tags & IMAGE_TAG_WORLD ) && ( texture->registrationSequence != registrationSequence ) ) {
			// TODO: Could something better be done with these casts?
			wsw::unlink( (Texture *)texture, (Texture **)&m_materialTexturesHead, Texture::ListLinks );
			wsw::unlink( (Texture *)texture, (Texture **)&m_materialTextureBins[texture->binIndex], Texture::BinLinks );
			m_factory.releaseMaterialTexture( texture );
		}
	}
	MaterialCubemap *nextCubemap = nullptr;
	for( MaterialCubemap *texture = m_materialCubemapsHead; texture; texture = nextCubemap ) {
		nextCubemap = (MaterialCubemap *)texture->nextInList();
		if( ( texture->tags & IMAGE_TAG_WORLD ) && ( texture->registrationSequence != registrationSequence ) ) {
			wsw::unlink( (Texture *)texture, (Texture **)&m_materialCubemapsHead, Texture::ListLinks );
			wsw::unlink( (Texture *)texture, (Texture **)&m_materialCubemapBins[texture->binIndex], Texture::BinLinks );
			m_factory.releaseMaterialCubemap( texture );
		}
	}
}

void TextureCache::freeAllUnusedTextures() {
	freeUnusedWorldTextures();
}

auto TextureCache::getPortalRenderTarget( unsigned drawSceneFrameNum ) -> RenderTargetComponents * {
	for( PortalRenderTargetComponents &components : m_portalRenderTargets ) {
		if( components.drawSceneFrameNum != drawSceneFrameNum ) {
			components.drawSceneFrameNum = drawSceneFrameNum;
			return std::addressof( components );
		}
	}

	return nullptr;
}

auto TextureCache::getMiniviewRenderTarget() -> RenderTargetComponents * {
	return m_miniviewRenderTarget.data();
}

// TODO: Is there a more appropritate place to put this subroutine
void R_BindRenderTarget( RenderTargetComponents *components ) {
	if( components ) {
		RenderTarget            *const renderTarget           = components->renderTarget;
		RenderTargetTexture     *const oldAttachedTexture     = components->renderTarget->attachedTexture;
		RenderTargetDepthBuffer *const oldAttachedDepthBuffer = components->renderTarget->attachedDepthBuffer;
		RenderTargetTexture     *const newTexture             = components->texture;
		RenderTargetDepthBuffer *const newDepthBuffer         = components->depthBuffer;

		bool hasChanges = false;
		qglBindFramebuffer( GL_FRAMEBUFFER, renderTarget->fboId );
		if( oldAttachedTexture != newTexture ) {
			if( oldAttachedTexture ) {
				oldAttachedTexture->attachedToRenderTarget = nullptr;
			}
			if( RenderTarget *oldTarget = newTexture->attachedToRenderTarget ) {
				assert( oldTarget != renderTarget );
				// TODO: Do we have to bind it and call detach?
				oldTarget->attachedTexture = nullptr;
			}
			renderTarget->attachedTexture      = newTexture;
			newTexture->attachedToRenderTarget = renderTarget;
			qglFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, newTexture->texnum, 0 );
			hasChanges = true;
		}
		if( oldAttachedDepthBuffer != newDepthBuffer ) {
			if( oldAttachedDepthBuffer ) {
				oldAttachedDepthBuffer->attachedToRenderTarget = nullptr;
			}
			if( RenderTarget *oldTarget = newDepthBuffer->attachedToRenderTarget ) {
				assert( oldTarget != renderTarget );
				// TODO: Do we have to bind it and call detach?
				oldTarget->attachedDepthBuffer = nullptr;
			}
			renderTarget->attachedDepthBuffer      = newDepthBuffer;
			newDepthBuffer->attachedToRenderTarget = renderTarget;
			qglFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, newDepthBuffer->rboId );
			hasChanges = true;
		}
		if( hasChanges ) {
			// TODO: What to do in this case
			if( qglCheckFramebufferStatus( GL_FRAMEBUFFER ) != GL_FRAMEBUFFER_COMPLETE ) {
				// Just make sure that the status of attachments remains correct
				assert( renderTarget->attachedTexture == newTexture );
				assert( renderTarget->attachedDepthBuffer == newDepthBuffer );
				qglFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0 );
				qglFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, 0 );
				renderTarget->attachedTexture          = nullptr;
				renderTarget->attachedDepthBuffer      = nullptr;
				newTexture->attachedToRenderTarget     = nullptr;
				newDepthBuffer->attachedToRenderTarget = nullptr;
			}
		}
	} else {
		qglBindFramebuffer( GL_FRAMEBUFFER, 0 );
	}
}