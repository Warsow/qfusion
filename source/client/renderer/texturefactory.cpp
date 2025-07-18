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

#include "local.h"
#include <common/helpers/hash.h>
#include <common/helpers/links.h>
#include <common/facilities/wswfs.h>
#include <common/helpers/singletonholder.h>
#include <client/imageloading.h>

#ifdef DEBUG_NOISE
#include <common/helpers/noise.h>
#endif

using wsw::operator""_asView;
using wsw::operator""_asHView;

#include <tuple>
#include <memory>
#include <utility>
#include <unordered_map>

TextureFactory::TextureFactory() {
	// Cubemap names are put after material ones in the same chunk
	m_nameDataStorage.reserve( ( kMaxMaterialTextures + kMaxMaterialCubemaps ) * kNameDataStride );

	qglPixelStorei( GL_PACK_ALIGNMENT, 1 );
	qglPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
}

[[nodiscard]]
bool TextureFactory::tryUpdatingFilterOrAniso( TextureFilter filter, int givenAniso, int *anisoToApply,
											   bool *doApplyFilter, bool *doApplyAniso ) {
	*doApplyFilter = m_textureFilter != filter;

	if( glConfig.ext.texture_filter_anisotropic ) {
		*anisoToApply = wsw::clamp( givenAniso, 1, glConfig.maxTextureFilterAnisotropic );
		// TODO: The extension spec allows specifying aniso with non-trilinear filtering modes.
		// Should we really allow doing that?
		*doApplyAniso = m_anisoLevel != *anisoToApply;
	} else {
		*anisoToApply = 1;
		*doApplyAniso = false;
	}

	m_textureFilter = filter;
	m_anisoLevel    = *anisoToApply;
	return *doApplyFilter || *doApplyAniso;
}

auto TextureFactory::generateHandle( const wsw::StringView &label ) -> GLuint {
	assert( qglGetError() == GL_NO_ERROR );

	GLuint handle = 0;
	qglGenTextures( 1, &handle );
	// TODO
	//if( qglObjectLabel ) {
	//	qglObjectLabel( GL_TEXTURE, handle, wsw::min( label.length(), (size_t)glConfig.maxObjectLabelLen ), label.data() );
	//}

	assert( qglGetError() == GL_NO_ERROR );
	return handle;
}

void TextureFactory::setupFilterMode( GLuint target, unsigned flags ) {
	if( flags & IT_NOFILTERING ) {
		qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
		qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
	} else if( flags & IT_DEPTH ) {
		qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
		qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
		if( glConfig.ext.texture_filter_anisotropic ) {
			qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1 );
		}
	// TODO: Reverse the condition, reorder branches
	} else if( !( flags & IT_NOMIPMAP ) ) {
		auto [minify, magnify] = kTextureFilterGLValues[m_textureFilter];
		if( flags & IT_CUBEMAP ) {
			// Turn mipmap filtering off for cubemaps.
			// There's no visual difference for real kinds of surfaces.
			// This should aid texturing performance especially on a low-end hardware.
			if( minify == GL_LINEAR_MIPMAP_LINEAR ) {
				minify = GL_LINEAR_MIPMAP_NEAREST;
			} else if( minify == GL_NEAREST_MIPMAP_LINEAR ) {
				minify = GL_NEAREST_MIPMAP_NEAREST;
			}
		}
		qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, minify );
		qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, magnify );
		if( glConfig.ext.texture_filter_anisotropic ) {
			qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, m_anisoLevel );
		}
	} else {
		const GLuint magnify = kTextureFilterGLValues[m_textureFilter].second;
		assert( magnify == GL_LINEAR || magnify == GL_NEAREST );
		// TODO: Use magnify for both??
		qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, magnify );
		qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, magnify );
		if( glConfig.ext.texture_filter_anisotropic ) {
			qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1 );
		}
	}
}

void TextureFactory::setupWrapMode( GLuint target, unsigned flags ) {
	const int wrap = ( flags & IT_CLAMP ) ? GL_CLAMP_TO_EDGE : GL_REPEAT;
	qglTexParameteri( target, GL_TEXTURE_WRAP_S, wrap );
	qglTexParameteri( target, GL_TEXTURE_WRAP_T, wrap );
}

static const GLint kSwizzleMaskIdentity[] = { GL_RED, GL_GREEN, GL_BLUE, GL_ALPHA };
static const GLint kSwizzleMaskAlpha[] = { GL_ZERO, GL_ZERO, GL_ZERO, GL_RED };
static const GLint kSwizzleMaskLuminance[] = { GL_RED, GL_RED, GL_RED, GL_ALPHA };
static const GLint kSwizzleMaskLuminanceAlpha[] = { GL_RED, GL_RED, GL_RED, GL_GREEN };

[[nodiscard]]
static auto getTextureInternalFormat( int samples, int flags ) -> std::pair<GLuint, const GLint *> {
	// TODO: Unset this flag in LDR modes
	const bool sRGB = false;//( flags & IT_SRGB ) != 0;

	if( !( flags & IT_NOCOMPRESS ) && r_texturecompression->integer ) {
		if( samples == 4 ) {
			return { sRGB ? GL_COMPRESSED_SRGB_ALPHA : GL_COMPRESSED_RGBA, kSwizzleMaskIdentity };
		}
		if( samples == 3 ) {
			return { sRGB ? GL_COMPRESSED_SRGB :  GL_COMPRESSED_RGB, kSwizzleMaskIdentity };
		}
		if( samples == 2 ) {
			return { sRGB ? GL_COMPRESSED_SLUMINANCE_ALPHA : GL_COMPRESSED_RG, kSwizzleMaskLuminanceAlpha };
		}
		if( ( samples == 1 ) && !( flags & IT_ALPHAMASK ) ) {
			return { sRGB ? GL_COMPRESSED_SLUMINANCE : GL_COMPRESSED_RED, kSwizzleMaskLuminance };
		}
		// TODO: is the combination left illegal?
		assert( 0 );
	}

	if( samples == 3 ) {
		return { sRGB ? GL_SRGB8 : GL_RGB8, kSwizzleMaskIdentity };
	}

	if( samples == 2 ) {
		return { sRGB ? GL_RG16F : GL_RG, kSwizzleMaskLuminanceAlpha };
	}

	if( samples == 1 ) {
		const GLint *mask = ( flags & IT_ALPHAMASK ) ? kSwizzleMaskAlpha : kSwizzleMaskLuminance;
		return { sRGB ? GL_R16F : GL_R8, mask };
	}

	return { sRGB ? GL_SRGB8_ALPHA8 : GL_RGBA8, kSwizzleMaskIdentity };
}

struct GLTexImageFormats {
	const GLint *swizzleMask;
	GLint internalFormat;
	GLenum format;
	GLenum type;
};

[[nodiscard]]
static auto getDepthTexImageFormats( int flags ) -> GLTexImageFormats {
	assert( flags & IT_DEPTH );

	GLint internalFormat;
	GLenum format, type;
	if( flags & IT_STENCIL ) {
		internalFormat = format = GL_DEPTH_STENCIL;
		type = GL_UNSIGNED_INT_24_8;
	} else {
		internalFormat = format = GL_DEPTH_COMPONENT;
		type = GL_UNSIGNED_INT;
	}

	return { kSwizzleMaskIdentity, internalFormat, format, type };
}

[[nodiscard]]
static auto getFramebufferTexImageFormats( int flags, int samples ) -> GLTexImageFormats {
	assert( flags & IT_FRAMEBUFFER );

	GLint internalFormat;
	GLenum format, type;

	if( flags & IT_FLOAT ) {
		type = GL_FLOAT;
		if( samples == 4 ) {
			std::tie( format, internalFormat ) = std::make_pair( GL_RGBA, GL_RGBA16F );
		} else {
			std::tie( format, internalFormat ) = std::make_pair( GL_RGB, GL_RGB16F );
		}
	} else {
		type = GL_UNSIGNED_BYTE;
		if( samples == 4 ) {
			std::tie( format, internalFormat ) = std::make_pair( GL_RGBA, GL_RGBA8 );
		} else {
			std::tie( format, internalFormat ) = std::make_pair( GL_RGB, GL_RGB8 );
		}
	}

	return { kSwizzleMaskIdentity, internalFormat, format, type };
}

static const std::pair<unsigned, GLint> kFloatInternalFormatForSamples[4] {
	{ 4, GL_RGBA16F },
	{ 3, GL_RGB16F },
	{ 2, GL_RG16F },
	{ 1, GL_RG16F }
};

[[nodiscard]]
auto getRegularTexImageFormats( unsigned flags, unsigned samples ) -> GLTexImageFormats {
	const GLint *swizzleMask = kSwizzleMaskIdentity;
	GLenum format, type;
	GLint internalFormat;

	if( samples == 4 ) {
		format = ( flags & IT_BGRA ? GL_BGRA_EXT : GL_RGBA );
	} else if( samples == 3 ) {
		format = ( flags & IT_BGRA ? GL_BGR_EXT : GL_RGB );
	} else if( samples == 2 ) {
		format = GL_RG;
		swizzleMask = kSwizzleMaskLuminanceAlpha;
	} else if( flags & IT_ALPHAMASK ) {
		format = GL_RED;
		swizzleMask = kSwizzleMaskAlpha;
	} else {
		format = GL_RED;
		swizzleMask = kSwizzleMaskLuminance;
	}

	if( flags & IT_FLOAT ) {
		assert( samples >= 1 && samples <= 4 );
		const auto [tableSamples, tableFormat] = kFloatInternalFormatForSamples[samples - 1];
		assert( tableSamples == samples );
		// TODO: No swizzles?
		std::tie( type, internalFormat ) = std::make_pair( GL_FLOAT, tableFormat );
	} else {
		type = GL_UNSIGNED_BYTE;
		std::tie( internalFormat, swizzleMask ) = getTextureInternalFormat( samples, flags );
	}

	return { swizzleMask, internalFormat, format, type };
}

struct alignas( 1 ) Rgba {
	uint8_t r, g, b, a;
};

static_assert( alignof( Rgba ) == 1 && sizeof( Rgba ) == 4 );

static void applyOutlineEffectRgba( Rgba *__restrict dest, const Rgba *__restrict src,
									unsigned width, unsigned height ) {
	// Sanity checks
	assert( width > 0 && height > 0 && width < ( 1u << 16 ) && height < ( 1u << 16 ) );

	for( unsigned line = 0; line < height; ++line ) {
		for( unsigned column = 0; column < width; ++column ) {
			float accumAlpha = 0.0f;
			if( line > 0 ) {
				accumAlpha += (float)src[width * ( line - 1 ) + column].a;
			}
			if( line + 1 < height ) {
				accumAlpha += (float)src[width * ( line + 1 ) + column].a;
			}
			if( column > 0 ) {
				accumAlpha += (float)src[width * line + ( column - 1 )].a;
			}
			if( column + 1 < width ) {
				accumAlpha += (float)src[width * line + ( column + 1)].a;
			}
			const Rgba blendDest { 0, 0, 0, (uint8_t)wsw::min( 255.0f, accumAlpha ) };
			const Rgba &blendSrc = src[width * line + column];
			const auto srcFrac   = (float)blendSrc.a * ( 1.0f / 255.0f );
			const auto destFrac  = 1.0f - srcFrac;
			dest[line * width + column] = Rgba {
				(uint8_t)( srcFrac * (float)blendSrc.r + destFrac * (float)blendDest.r ),
				(uint8_t)( srcFrac * (float)blendSrc.g + destFrac * (float)blendDest.g ),
				(uint8_t)( srcFrac * (float)blendSrc.b + destFrac * (float)blendDest.b ),
				(uint8_t)( srcFrac * (float)blendSrc.a + destFrac * (float)blendDest.a )
			};
		}
	}
}

static ImageBuffer readFileBuffer;
static ImageBuffer cubemapBuffer[6];
static ImageBuffer loadingBuffer;
static ImageBuffer conversionBuffer;

auto TextureFactory::loadTextureDataFromFile( const wsw::StringView &name,
											  ImageBuffer *readBuffer,
											  ImageBuffer *dataBuffer,
											  ImageBuffer *conversionBuffer,
											  const ImageOptions &options )
											-> std::optional<std::pair<uint8_t *, BitmapProps>> {
	assert( name.isZeroTerminated() );
	assert( std::size( IMAGE_EXTENSIONS ) == 4 );
	// TODO: Adopt the sane FS interface over the codebase
	const wsw::StringView extensions[4] = {
		wsw::StringView( IMAGE_EXTENSIONS[0] ),
		wsw::StringView( IMAGE_EXTENSIONS[1] ),
		wsw::StringView( IMAGE_EXTENSIONS[2] ),
		wsw::StringView( IMAGE_EXTENSIONS[3] )
	};

	const auto maybePartsPair = wsw::fs::findFirstExtension( name, std::begin( extensions ), std::end( extensions ) );
	if( !maybePartsPair ) {
		return std::nullopt;
	}

	wsw::StaticString<MAX_QPATH> path;
	path << maybePartsPair->first << maybePartsPair->second;
	auto maybeHandle = wsw::fs::openAsReadHandle( path.asView() );
	if( !maybeHandle ) {
		return std::nullopt;
	}

	const bool isSvg = maybePartsPair->second.equalsIgnoreCase( ".svg"_asView );

	constexpr size_t kMaxSaneBitmapDataSize = 2048 * 2048 * 4;
	// In case of regular images, consider that the data size cannot be greater than 2048x2048 RGBA + some header bytes
	const size_t maxSaneImageDataSize = isSvg ? 1024 * 1024 : ( kMaxSaneBitmapDataSize + 8192 );
	const size_t fileSize = maybeHandle->getInitialFileSize();
	if( fileSize > maxSaneImageDataSize ) {
		return std::nullopt;
	}

	uint8_t *const fileBufferBytes = readBuffer->reserveAndGet( fileSize );
	if( !maybeHandle->readExact( fileBufferBytes, fileSize ) ) {
		return std::nullopt;
	}

	unsigned width = 0, height = 0, samples = 0;
	size_t imageDataSize = 0;
	uint8_t *bytes = nullptr;
	if( isSvg ) {
		// It's only gets used in this case.
		// TODO: Redesign supplying of desired texture parameters
		// so it can be meaningful in case of regular images.
		if( !options.desiredSize ) {
			return std::nullopt;
		}
		samples = 4;
		const auto [desiredWidth, desiredHeight] = *options.desiredSize;
		const auto bufferDataSize = (size_t)desiredWidth * (size_t)desiredHeight * (size_t)samples;
		bytes = conversionBuffer->reserveAndGet( bufferDataSize );
		const auto maybeSize = wsw::rasterizeSvg( fileBufferBytes, fileSize, bytes, bufferDataSize, options );
		if( !maybeSize ) {
			return std::nullopt;
		}
		std::tie( width, height ) = *maybeSize;
		imageDataSize = (size_t)width * (size_t)height * (size_t)samples;
		assert( imageDataSize <= bufferDataSize );
	} else {
		bytes = wsw::decodeImageData( fileBufferBytes, fileSize, &width, &height, &samples );
		if( !bytes ) {
			return std::nullopt;
		}
		assert( width > 0 && height > 0 && samples > 0 );
		imageDataSize = (size_t)width * (size_t)height * (size_t)samples;
	}

	assert( imageDataSize );
	// Sanity check (disallow huge bogus allocations)
	if( imageDataSize > kMaxSaneBitmapDataSize ) {
		return std::nullopt;
	}

	uint8_t *const imageData = dataBuffer->reserveAndGet( imageDataSize );
	if( options.useOutlineEffect ) {
		if( samples == 4 && width && height ) {
			applyOutlineEffectRgba( (Rgba *)imageData, (const Rgba *)bytes, (unsigned)width, (unsigned)height );
		} else {
			return std::nullopt;
		}
	} else {
		std::memcpy( imageData, bytes, imageDataSize );
	}

	if( !isSvg ) {
		// TODO: Use custom allocators for decoding buffers
		free( bytes );
	}

	return std::make_pair( imageData, BitmapProps { (uint16_t)width, (uint16_t)height, (uint16_t)samples } );
}

auto TextureFactory::internTextureName( unsigned storageIndex,
										const wsw::HashedStringView &name ) -> wsw::HashedStringView {
	assert( name.length() <= kMaxNameLen );
	char *const data = m_nameDataStorage.get() + kNameDataStride * storageIndex;
	std::memcpy( data, name.data(), name.length() );
	data[name.length()] = '\0';
	return wsw::HashedStringView( data, name.length(), name.getHash(), wsw::StringView::ZeroTerminated );
}

auto TextureFactory::createRaw2DTexture() -> Raw2DTexture * {
	if( m_raw2DTexturesAllocator.isFull() ) {
		return nullptr;
	}

	const GLuint handle = generateHandle( wsw::StringView() );
	const GLenum target = GL_TEXTURE_2D;
	// TODO: Skip?
	bindToModify( target, handle );
	unbindModified( target, handle );

	return new( m_raw2DTexturesAllocator.allocOrNull() )Raw2DTexture( handle );
}

void TextureFactory::releaseRaw2DTexture( Raw2DTexture *texture ) {
	if( texture ) {
		qglDeleteTextures( 1, &texture->texnum );
		m_raw2DTexturesAllocator.free( texture );
	}
}

bool TextureFactory::updateRaw2DTexture( Raw2DTexture *texture, const wsw::StringView &name,
										 const ImageOptions &options ) {
	const auto maybeCleanName = makeCleanName( name, wsw::StringView() );
	if( !maybeCleanName ) {
		return false;
	}

	auto maybeFileData = loadTextureDataFromFile( *maybeCleanName, &::readFileBuffer, &::loadingBuffer,
												  &::conversionBuffer, options );
	if( !maybeFileData ) {
		return false;
	}

	auto [fileBytes, bitmapProps] = *maybeFileData;

	qglPixelStorei( GL_UNPACK_ALIGNMENT, 1 );

	const GLenum target = GL_TEXTURE_2D;

	bindToModify( target, texture->texnum );

	auto [swizzleMask, internalFormat, format, type] =
		getRegularTexImageFormats( texture->flags, bitmapProps.samples );
	setupFilterMode( target, texture->flags );
	setupWrapMode( target, texture->flags );

	if( swizzleMask != kSwizzleMaskIdentity ) {
		qglTexParameteriv( target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
	}

	qglTexImage2D( target, 0, internalFormat, bitmapProps.width, bitmapProps.height, 0, format, type, fileBytes );

	if( true || !( texture->flags & IT_NOMIPMAP ) ) {
		qglGenerateMipmap( target );
	}

	unbindModified( target, texture->texnum );

	texture->width = bitmapProps.width;
	texture->height = bitmapProps.height;
	texture->samples = bitmapProps.samples;

	return true;
}

auto TextureFactory::loadMaterialTexture( const wsw::HashedStringView &name, unsigned flags ) -> Material2DTexture * {
	if( m_materialTexturesAllocator.isFull() ) {
		return nullptr;
	}

	auto maybeFileData = loadTextureDataFromFile( name, &::readFileBuffer, &::loadingBuffer,
												  &::conversionBuffer, ImageOptions {} );
	if( !maybeFileData ) {
		return nullptr;
	}

	auto [fileBytes, bitmapProps] = *maybeFileData;

	qglPixelStorei( GL_UNPACK_ALIGNMENT, 1 );

	const GLuint handle = generateHandle( name );
	const GLenum target = GL_TEXTURE_2D;

	bindToModify( target, handle );

	const auto [swizzleMask, internalFormat, format, type] = getRegularTexImageFormats( flags, bitmapProps.samples );
	setupFilterMode( target, flags );
	setupWrapMode( target, flags );

	if( swizzleMask != kSwizzleMaskIdentity ) {
		qglTexParameteriv( target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
	}

	qglTexImage2D( target, 0, internalFormat, bitmapProps.width, bitmapProps.height, 0, format, type, fileBytes );

	if( true || !( flags & IT_NOMIPMAP ) ) {
		qglGenerateMipmap( target );
	}

	unbindModified( target, handle );

	unsigned index = 0;
	void *mem = m_materialTexturesAllocator.allocOrNull( &index );
	const wsw::HashedStringView ownedName( internTextureName( index, name ) );
	return new( mem )Material2DTexture( ownedName, handle, bitmapProps, flags );
}

auto TextureFactory::loadMaterialCubemap( const wsw::HashedStringView &name, unsigned flags ) -> MaterialCubemap * {
	if( m_materialCubemapsAllocator.isFull() ) {
		return nullptr;
	}

	const char signLetters[2] { 'p', 'n' };
	const char axisLetters[3] { 'x', 'y', 'z' };

	wsw::StaticString<MAX_QPATH> nameBuffer;
	nameBuffer << name << '_';
	const auto prefixLen = nameBuffer.size();

	const void *dataOfSides[6];
	BitmapProps firstProps { 0, 0, 0 };
	for( unsigned i = 0; i < 6; ++i ) {
		nameBuffer.erase( prefixLen );
		nameBuffer.append( signLetters[i % 2] );
		nameBuffer.append( axisLetters[i / 2] );
		const auto maybeData = loadTextureDataFromFile( nameBuffer.asView(), &::loadingBuffer,
														&::cubemapBuffer[i], &::conversionBuffer, ImageOptions {} );
		if( !maybeData ) {
			return nullptr;
		}
		const auto &[bytes, props] = *maybeData;
		if( i ) {
			if( props != firstProps ) {
				return nullptr;
			}
		} else {
			firstProps = props;
		}
		dataOfSides[i] = bytes;
	}

	const GLenum target = GL_TEXTURE_CUBE_MAP;
	const GLuint handle = generateHandle( name );

	bindToModify( target, handle );

	const auto [swizzleMask, internalFormat, format, type] = getRegularTexImageFormats( (int)flags, firstProps.samples );

	for( unsigned i = 0; i < 6; ++i ) {
		const GLenum sideTarget = GL_TEXTURE_CUBE_MAP_POSITIVE_X + i;
		if( swizzleMask != kSwizzleMaskIdentity ) {
			qglTexParameteriv( sideTarget, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
		}
		qglTexImage2D( sideTarget, 0, internalFormat, firstProps.width, firstProps.height, 0, format, type, dataOfSides[i] );
	}

	setupFilterMode( target, flags );
	setupWrapMode( target, flags );

	qglGenerateMipmap( target );

	unbindModified( target, handle );

	unsigned index = 0;
	void *const mem = m_materialCubemapsAllocator.allocOrNull( &index );
	const wsw::HashedStringView ownedName( internTextureName( kMaxRaw2DTextures + index, name ) );
	return new( mem )MaterialCubemap( ownedName, handle, firstProps, flags );
}

auto TextureFactory::createFontMask( unsigned w, unsigned h, const uint8_t *data ) -> Texture * {
	const auto flags = IT_NOFILTERING | IT_NOMIPMAP | IT_ALPHAMASK;
	const GLenum target = GL_TEXTURE_2D;
	const GLuint handle = generateHandle( wsw::StringView() );

	bindToModify( target, handle );

	const auto [swizzleMask, internalFormat, format, type] = getRegularTexImageFormats( flags, 1 );
	if( swizzleMask != kSwizzleMaskIdentity ) {
		qglTexParameteriv( target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
	}

	qglTexImage2D( target, 0, internalFormat, w, h, 0, format, type, data );

	setupFilterMode( target, flags );
	setupWrapMode( target, flags );

	unbindModified( target, handle );

	return new( m_fontMasksAllocator.allocOrNull() )FontMask( handle, w, h, flags );
}

static auto getLightmapFormatsForSamples( unsigned samples ) -> std::pair<GLint, GLenum> {
	assert( samples == 1 || samples == 3 );
	if( samples == 3 ) {
		return std::make_pair( GL_RGB8, GL_RGB );
	}
	return std::make_pair( GL_R8, GL_RED );
}

auto TextureFactory::createLightmap( unsigned w, unsigned h, unsigned samples, const uint8_t *data ) -> Texture * {
	if( m_lightmapsAllocator.isFull() ) {
		return nullptr;
	}

	const auto [internalFormat, format] = getLightmapFormatsForSamples( samples );
	const GLenum target = GL_TEXTURE_2D;
	const GLuint handle = generateHandle( wsw::StringView() );
	bindToModify( target, handle );

	qglTexImage2D( target, 0, internalFormat, w, h, 0, format, GL_UNSIGNED_BYTE, data );

	const unsigned flags = IT_CLAMP | IT_NOMIPMAP | IT_CUSTOMFILTERING;
	setupWrapMode( target, flags );

	qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
	qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
	if( glConfig.ext.texture_filter_anisotropic ) {
		qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1 );
	}

	unbindModified( target, handle );

	// TODO: Is lifetime management explicit?
	return new( m_lightmapsAllocator.allocOrNull() )Lightmap( handle, w, h, samples, flags );
}

auto TextureFactory::createLightmapArray( unsigned w, unsigned h, unsigned numLayers, unsigned samples ) -> Texture * {
	if( m_lightmapsAllocator.isFull() ) {
		return nullptr;
	}

	const auto [internalFormat, format] = getLightmapFormatsForSamples( samples );
	const GLenum target = GL_TEXTURE_2D;
	const GLuint handle = generateHandle( wsw::StringView() );
	bindToModify( target, handle );

	const unsigned flags = IT_CLAMP | IT_NOMIPMAP | IT_CUSTOMFILTERING;
	qglTexImage3D( target, 0, internalFormat, w, h, numLayers, 0, format, GL_UNSIGNED_BYTE, nullptr );
	setupWrapMode( target, flags );

	qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
	qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
	if( glConfig.ext.texture_filter_anisotropic ) {
		qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1 );
	}

	unbindModified( target, 0 );

	return new( m_lightmapsAllocator.allocOrNull() )LightmapArray( handle, w, h, samples, numLayers, flags );
}

void TextureFactory::replaceLightmapLayer( Texture *texture, unsigned layer, const uint8_t *data ) {
	assert( texture );
	assert( layer < (unsigned)texture->layers );

	// TODO: Store these properties in a LightmapTextureArray instance
	const auto [internalFormat, format] = getLightmapFormatsForSamples( texture->samples );

	bindToModify( texture );

	qglTexSubImage3D( texture->target, 0, 0, 0, layer, texture->width, texture->height, 1, format, GL_UNSIGNED_BYTE, data );

	unbindModified( texture );
}

// TODO: Pass a reference to some "rectangle" type instead of these parameters
void TextureFactory::replaceFontMaskSamples( Texture *texture, unsigned x, unsigned y, unsigned w, unsigned h, const uint8_t *data ) {
	const GLenum target = texture->target;
	assert( target == GL_TEXTURE_2D );

	bindToModify( target, texture->texnum );

	// TODO: We must know statically
	const auto [swizzleMask, internalFormat, format, type] = getRegularTexImageFormats( IT_ALPHAMASK | IT_NOMIPMAP | IT_NOFILTERING, 1 );
	if( swizzleMask != kSwizzleMaskIdentity ) {
		qglTexParameteriv( target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
	}

	qglTexSubImage2D( target, 0, x, y, w, h, format, type, data );

	unbindModified( target, 0 );

	assert( qglGetError() == GL_NO_ERROR );
}

[[nodiscard]]
auto TextureFactory::createBuiltin2DTexture( const Builtin2DTextureData &data ) -> Texture * {
	assert( !m_builtinTexturesAllocator.isFull() );

	assert( qglGetError() == GL_NO_ERROR );

	const GLenum target = GL_TEXTURE_2D;
	const GLuint handle = generateHandle( wsw::StringView() );

	bindToModify( target, handle );

	const auto [swizzleMask, internalFormat, format, type] = getRegularTexImageFormats( data.flags, data.samples );
	if( swizzleMask != kSwizzleMaskIdentity ) {
		qglTexParameteriv( target, GL_TEXTURE_SWIZZLE_RGBA, swizzleMask );
	}

	qglTexImage2D( target, 0, internalFormat, data.width, data.height, 0, format, type, data.bytes );

	setupWrapMode( target, data.flags );
	if( data.flags & IT_CUSTOMFILTERING ) {
		assert( data.nearestFilteringOnly );
		qglTexParameteri( target, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR );
		qglTexParameteri( target, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
		if( glConfig.ext.texture_filter_anisotropic ) {
			qglTexParameteri( target, GL_TEXTURE_MAX_ANISOTROPY_EXT, 1 );
		}
	} else {
		assert( !data.nearestFilteringOnly );
		setupFilterMode( target, data.flags );
	}

	// !!!!!!!!
	qglGenerateMipmap( target );

	unbindModified( target, handle );

	auto *const texture = (Texture *)m_builtinTexturesAllocator.allocOrNull();
	texture->texnum = handle;
	texture->target = GL_TEXTURE_2D;
	texture->width = data.width;
	texture->height = data.height;
	texture->flags = data.flags;
	texture->samples = data.samples;
	texture->tags = IMAGE_TAG_BUILTIN;
	return texture;
}

[[nodiscard]]
auto TextureFactory::createSolidColorBuiltinTexture( const float *color ) -> Texture * {
	auto *bytes = ::loadingBuffer.reserveAndGet( 3 );
	bytes[0] = (uint8_t)( 255 * color[0] );
	bytes[1] = (uint8_t)( 255 * color[1] );
	bytes[2] = (uint8_t)( 255 * color[2] );
	Builtin2DTextureData data;
	data.bytes = bytes;
	data.width = data.height = 1;
	data.samples = 3;
	data.flags = IT_NOPICMIP | IT_NOCOMPRESS;
	return createBuiltin2DTexture( data );
}

[[nodiscard]]
auto TextureFactory::createBuiltinNoTextureTexture() -> Texture * {
#ifdef DEBUG_NOISE
	constexpr const unsigned side = 1024;
#else
	constexpr const unsigned side = 8;

	const uint8_t wswPurple[] { 53, 34, 69 };
	const uint8_t wswOrange[] { 95, 39, 9 };
	uint8_t markPurple[3], markOrange[3];
	const uint8_t red[3] = { 128, 0, 0 };
	for( int i = 0; i < 3; ++i ) {
		markPurple[i] = (uint8_t)( ( 3 * wswPurple[i] + red[i] ) / 4 );
		markOrange[i] = (uint8_t)( ( 3 * wswOrange[i] + red[i] ) / 4 );
	}
#endif

	ptrdiff_t offset = 0;
	uint8_t *const __restrict p = loadingBuffer.reserveAndGet( side * side * 3 );
	for( unsigned pixNum = 0; pixNum < side * side; ++pixNum ) {
		const unsigned x = pixNum % side;
		const unsigned y = pixNum / side;

#ifdef DEBUG_NOISE
		const float noiseX = 25.0f * (float)x / (float)side;
		const float noiseY = 25.0f * (float)y / (float)side;
#if 1
		//const float noiseValue = calcSimplexNoise2D( noiseX, noiseY );
		//const float noiseValue = calcSimplexNoise3D( noiseX, noiseY, 0.0f );
		//const float noiseValue = calcVoronoiNoiseLinear( noiseX, noiseY, 0.0f );
		const float noiseValue = calcVoronoiNoiseSquared( noiseX, noiseY, 0.0f );
		const auto noiseByte   = (uint8_t)( 255.0f * noiseValue );

		p[offset + 0] = noiseByte;
		p[offset + 1] = noiseByte;
		p[offset + 2] = noiseByte;
#else
		const Vec3 curl = calcSimplexNoiseCurl( noiseX, noiseY, 0.0f );
		p[offset + 0]   = (uint8_t)( 255.0f * ( 0.5f + 0.5f * curl.X() ) );
		p[offset + 1]   = (uint8_t)( 255.0f * ( 0.5f + 0.5f * curl.Y() ) );
		p[offset + 2]   = (uint8_t)( 255.0f * ( 0.5f + 0.5f * curl.Z() ) );
#endif
#else
		const unsigned xySum = x + y;
		const uint8_t *__restrict color;
		if( !xySum ) {
			color = markPurple;
		} else if( xySum == 1 ) {
			color = markOrange;
		} else if( xySum % 2 ) {
			color = wswOrange;
		} else {
			color = wswPurple;
		}
		p[offset + 0] = color[0];
		p[offset + 1] = color[1];
		p[offset + 2] = color[2];
#endif

		// Increment after addressing to avoid AGI
		offset += 3;
	}

	Builtin2DTextureData data;
	data.width = data.height = side;
#ifdef DEBUG_NOISE
	data.flags = IT_SRGB;
	data.nearestFilteringOnly = false;
#else
	data.flags = IT_SRGB | IT_CUSTOMFILTERING;
	data.nearestFilteringOnly = true;
#endif
	data.samples = 3;
	data.bytes   = p;
	return createBuiltin2DTexture( data );
}

[[nodiscard]]
auto TextureFactory::createBuiltinBlankNormalmap() -> Texture * {
	auto *const bytes = ::loadingBuffer.reserveAndGet( 4 );
	bytes[0] = bytes[1] = 128;
	bytes[2] = 255;
	bytes[3] = 128; // Displacement height

	Builtin2DTextureData data;
	data.width = data.height = 1;
	data.samples = 4;
	data.flags = IT_NOPICMIP | IT_NOCOMPRESS;
	data.bytes = bytes;
	return createBuiltin2DTexture( data );
}

[[nodiscard]]
auto TextureFactory::createBuiltinWhiteCubemap() -> Texture * {
	const GLenum target = GL_TEXTURE_CUBE_MAP;
	const GLuint handle = generateHandle( wsw::StringView() );

	bindToModify( target, handle );

	const auto flags = IT_NOMIPMAP | IT_CUBEMAP;
	const uint8_t data[3] = { 255, 255, 255 };
	for( unsigned i = 0; i < 6; ++i ) {
		qglTexImage2D( GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB8, 1, 1, 0, GL_RGB, GL_UNSIGNED_BYTE, data );
	}

	setupWrapMode( target, flags );
	unbindModified( target, handle );

	Texture *const texture = (Texture *)m_builtinTexturesAllocator.allocOrNull();
	texture->target = target;
	texture->texnum = handle;
	texture->width = 1;
	texture->height = 1;
	texture->layers = 0;
	texture->samples = 3;
	texture->flags = flags;
	texture->tags = IMAGE_TAG_BUILTIN;
	return texture;
}

[[nodiscard]]
auto TextureFactory::createBuiltinParticleTexture() -> Texture * {
	constexpr unsigned side = 16;

	uint8_t *const __restrict p = loadingBuffer.reserveAndGet( side * side * 4 );

	ptrdiff_t offset = 0;
	constexpr int halfSide = (int)side / 2;
	for( int pixNum = 0; pixNum < (int)( side * side ); ++pixNum ) {
		const int x = pixNum % (int)side;
		const int y = pixNum / (int)side;
		const int dx = x - halfSide;
		const int dy = y - halfSide;
		const uint8_t value = ( dx * dx + dy * dy < halfSide * halfSide ) ? 255 : 0;
		p[offset + 0] = value;
		p[offset + 1] = value;
		p[offset + 2] = value;
		p[offset + 3] = value;
		offset += 4;
	}

	Builtin2DTextureData data;
	data.width = data.height = side;
	data.samples = 4;
	data.flags = IT_NOPICMIP | IT_NOMIPMAP | IT_SRGB;
	data.bytes = p;
	return createBuiltin2DTexture( data );
}

[[nodiscard]]
auto TextureFactory::createBuiltinCoronaTexture() -> Texture * {
	constexpr unsigned side = 32;

	uint8_t *const __restrict p = ::loadingBuffer.reserveAndGet( side * side * 4 );

	ptrdiff_t offset = 0;
	constexpr int halfSide = (int)side / 2;
	constexpr float invHalfSide = 1.0f / (float)halfSide;
	for( int pixNum = 0; pixNum < (int)( side * side ); ++pixNum ) {
		const int x = pixNum % (int)side;
		const int y = pixNum / (int)side;

		const auto unitDX = (float)( x - halfSide ) * invHalfSide;
		const auto unitDY = (float)( y - halfSide ) * invHalfSide;

		float frac = 1.0f - Q_Sqrt( unitDX * unitDX + unitDY * unitDY );
		frac = wsw::clamp( frac, 0.0f, 1.0f );
		frac = frac * frac;

		const auto value = (uint8_t)( 128.0f * frac );
		p[offset + 0] = value;
		p[offset + 1] = value;
		p[offset + 2] = value;
		p[offset + 3] = value;
		offset += 4;
	}

	Builtin2DTextureData data;
	data.width = data.height = side;
	data.flags = IT_SPECIAL | IT_SRGB;
	data.samples = 4;
	data.bytes = p;
	return createBuiltin2DTexture( data );
}

[[nodiscard]]
auto TextureFactory::createUITextureHandleWrapper() -> Texture * {
	assert( !m_builtinTexturesAllocator.isFull() );
	auto *const texture = (Texture *)m_builtinTexturesAllocator.allocOrNull();
	texture->width = 0;
	texture->height = 0;
	texture->tags = IMAGE_TAG_BUILTIN;
	texture->flags = IT_SPECIAL;
	texture->samples = 0;
	texture->texnum = 0;
	texture->target = GL_TEXTURE_2D;
	return texture;
}

void TextureFactory::releaseBuiltinTexture( Texture *texture ) {
	if( texture ) {
		qglDeleteTextures( 1, &texture->texnum );
		m_builtinTexturesAllocator.free( texture );
	}
}

void TextureFactory::releaseMaterialTexture( Material2DTexture *texture ) {
	qglDeleteTextures( 1, &texture->texnum );
	m_materialTexturesAllocator.free( texture );
}

void TextureFactory::releaseMaterialCubemap( MaterialCubemap *cubemap ) {
	qglDeleteTextures( 1, &cubemap->texnum );
	m_materialCubemapsAllocator.free( cubemap );
}

auto TextureFactory::createRenderTargetTexture( unsigned width, unsigned height ) -> RenderTargetTexture * {
	if( void *const mem = m_renderTargetTexturesAllocator.allocOrNull() ) {
		(void)qglGetError();

		GLuint id = 0;
		qglGenTextures( 1, &id );
		bindToModify( GL_TEXTURE_2D, id );
		// TODO: Add floating-point formats if needed
		// TODO: We don't need sRGB rendering for portals
		qglTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr );
		// TODO: Add a separate enumeration
		setupFilterMode( GL_TEXTURE_2D, IT_DEPTH );
		setupWrapMode( GL_TEXTURE_2D, IT_CLAMP );
		unbindModified( GL_TEXTURE_2D, id );

		if( qglGetError() == GL_NO_ERROR ) {
			auto *const texture = new( mem )RenderTargetTexture;
			texture->texnum     = id;
			texture->width      = width;
			texture->height     = height;
			texture->samples    = 4;
			texture->tags       = IMAGE_TAG_BUILTIN;
			texture->layers     = 0;
			texture->target     = GL_TEXTURE_2D;

			return texture;
		} else {
			if( id ) {
				qglDeleteTextures( 1, &id );
			}
			m_renderTargetTexturesAllocator.free( mem );
			return nullptr;
		}
	}
	return nullptr;
}

auto TextureFactory::createRenderTargetDepthBuffer( unsigned width, unsigned height ) -> RenderTargetDepthBuffer * {
	if( void *const mem = m_renderTargetDepthBufferAllocator.allocOrNull() ) {
		(void)qglGetError();

		GLuint id = 0;
		qglGenRenderbuffers( 1, &id );
		qglBindRenderbuffer( GL_RENDERBUFFER, id );
		qglRenderbufferStorage( GL_RENDERBUFFER, GL_DEPTH_COMPONENT, width, height );
		qglBindRenderbuffer( GL_RENDERBUFFER, 0 );
		if( qglGetError() == GL_NO_ERROR ) {
			auto *const buffer = new( mem )RenderTargetDepthBuffer;
			buffer->rboId      = id;
			return buffer;
		} else {
			if( id ) {
				qglDeleteRenderbuffers( 1, &id );
			}
			m_renderTargetDepthBufferAllocator.free( mem );
			return nullptr;
		}
	}
	return nullptr;
}

auto TextureFactory::createRenderTarget() -> RenderTarget * {
	if( void *const mem = m_renderTargetsAllocator.allocOrNull() ) {
		(void)qglGetError();

		GLuint id = 0;
		qglGenFramebuffers( 1, &id );
		if( qglGetError() == GL_NO_ERROR ) {
			auto *const target = new( mem )RenderTarget;
			target->fboId      = id;
			return target;
		} else {
			if( id ) {
				qglDeleteFramebuffers( 1, &id );
			}
			m_renderTargetsAllocator.free( mem );
			return nullptr;
		}
	}
	return nullptr;
}

void TextureFactory::releaseRenderTargetTexture( RenderTargetTexture *texture ) {
	if( texture ) {
		assert( !texture->attachedToRenderTarget );
		qglDeleteTextures( 1, &texture->texnum );
		m_renderTargetTexturesAllocator.free( texture );
	}
}

void TextureFactory::releaseRenderTargetDepthBuffer( RenderTargetDepthBuffer *renderTargetDepthBuffer ) {
	if( renderTargetDepthBuffer ) {
		assert( !renderTargetDepthBuffer->attachedToRenderTarget );
		qglDeleteRenderbuffers( 1, &renderTargetDepthBuffer->rboId );
		m_renderTargetDepthBufferAllocator.free( renderTargetDepthBuffer );
	}
}

void TextureFactory::releaseRenderTarget( RenderTarget *renderTarget ) {
	if( renderTarget ) {
		assert( !renderTarget->attachedTexture );
		assert( !renderTarget->attachedDepthBuffer );
		qglDeleteFramebuffers( 1, &renderTarget->fboId );
		m_renderTargetsAllocator.free( renderTarget );
	}
}

bool TextureFactory::addTextureColorsToHistogram( const wsw::StringView &path, TextureHistogram *__restrict histogram ) {
	auto maybeFileData = loadTextureDataFromFile( path, &::readFileBuffer, &::loadingBuffer,
												  &::conversionBuffer, ImageOptions {} );
	if( maybeFileData ) {
		auto [fileBytes, bitmapProps] = *maybeFileData;
		if( bitmapProps.samples == 1 || bitmapProps.samples == 3 || bitmapProps.samples == 4 ) {
			// TODO: Account for aspect ratio?
			constexpr unsigned scaledSide = 32;
			// Downscale the image so it does not blow up the histogram
			// Note that we do scaling for dry-run as well for maintaining 100% consistency with the real behavior
			if( uint8_t *const scaledData = wsw::scaleImageData( fileBytes, bitmapProps.width, bitmapProps.height,
																 scaledSide, scaledSide, bitmapProps.samples ) ) {
				if( histogram ) {
					if( bitmapProps.samples == 1 ) {
						for( unsigned i = 0; i < scaledSide * scaledSide; ++i ) {
							histogram->addTexelColor( COLOR_RGB( (unsigned)scaledData[i], (unsigned)scaledData[i],
																 (unsigned)scaledData[i] ) );
						}
					} else if( bitmapProps.samples == 3 ) {
						for( unsigned i = 0; i < 3 * scaledSide * scaledSide; i += 3 ) {
							histogram->addTexelColor( COLOR_RGB( (unsigned)scaledData[i + 0], (unsigned)scaledData[i + 1],
																 (unsigned)scaledData[i + 2] ) );
						}
					} else {
						for( unsigned i = 0; i < 4 * scaledSide * scaledSide; i += 4 ) {
							histogram->addTexelColor( COLOR_RGBA( (unsigned)scaledData[i + 0], (unsigned)scaledData[i + 1],
																  (unsigned)scaledData[i + 2], (unsigned)scaledData[i + 3] ) );
						}
					}
				}
				::free( scaledData );
				return true;
			}
		}
	}

	return false;
}

TextureHistogram::TextureHistogram() {
	m_priv = new std::unordered_map<unsigned, unsigned>;
}

TextureHistogram::~TextureHistogram() {
	delete (std::unordered_map<unsigned, unsigned> *)m_priv;
}

void TextureHistogram::clear() {
	( (std::unordered_map<unsigned, unsigned> *)m_priv )->clear();
}

void TextureHistogram::addTexelColor( unsigned color ) {
	( *( (std::unordered_map<unsigned, unsigned> *)m_priv ) )[color]++;
}

auto TextureHistogram::findDominantColor() const -> std::optional<unsigned> {
	double accum[4] { 0.0f, 0.0f, 0.0f, 0.0f };
	double totalWeights = 0.0;
	for( auto [color, count] : *( (std::unordered_map<unsigned, unsigned> *)m_priv ) ) {
		assert( count > 0 );
		const int r = COLOR_R( color );
		const int g = COLOR_G( color );
		const int b = COLOR_B( color );
		// CBA to roll correct saturation computations
		const double saturationLikeFrac = (double)( wsw::min( 255, std::abs( r - g ) + std::abs( r - b ) + std::abs( g - b ) ) ) / 255.0;
		const double weight = count * ( 0.1 + 0.9 * saturationLikeFrac );
		accum[0] += weight * r;
		accum[1] += weight * g;
		accum[2] += weight * b;
		accum[3] += weight * COLOR_A( color );
		totalWeights += weight;
	}
	if( totalWeights > 0.0 ) {
		const double scale = 1.0 / (double)totalWeights;
		return COLOR_RGBA( (unsigned)( scale * accum[0] ), (unsigned)( scale * accum[1] ),
						   (unsigned)( scale * accum[2] ), (unsigned)( scale * accum[3] ) );
	}
	return std::nullopt;
}