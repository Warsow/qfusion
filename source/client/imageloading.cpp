#include "imageloading.h"
#include <common/helpers/exceptions.h>
#include "renderer/ref.h"

#include <QSvgRenderer>
#include <QPainter>
#include <QImage>

#include <cstring>

#define STB_IMAGE_STATIC

#define STBI_NO_LINEAR
#define STBI_NO_STDIO
#define STBI_NO_HDR
#define STBI_NO_BMP
#define STBI_NO_PSD
#define STBI_NO_GIF
#define STBI_NO_PIC
#define STBI_NO_PNM

#define STB_IMAGE_IMPLEMENTATION
#include "../../third-party/stb/stb_image.h"
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "../../third-party/stb/stb_image_write.h"
#define STB_IMAGE_RESIZE_IMPLEMENTATION
#include "../../third-party/stb/stb_image_resize2.h"

namespace wsw {

[[nodiscard]]
static auto estimateCrispness( const QImage &image ) -> float {
	if( !image.size().isValid() || image.size().isEmpty() ) {
		return 0.0f;
	}

	assert( image.format() == QImage::Format_RGBA8888 );
	const auto *__restrict data = image.constBits();
	const unsigned dataSize = image.sizeInBytes();

	unsigned numAlphaTransitionPixels = 0;
	for( unsigned i = 0; i + 3 < dataSize; ++i ) {
		const auto alpha = data[i + 3];
		numAlphaTransitionPixels += ( alpha != 0 ) & ( alpha != 255 );
	}

	const auto totalNumPixels = (unsigned)image.width() * (unsigned)image.height();
	assert( totalNumPixels == dataSize / 4 );
	return (float)( totalNumPixels - numAlphaTransitionPixels ) / (float)( numAlphaTransitionPixels );
}

[[nodiscard]]
static auto rasterizeSvg( QSvgRenderer *renderer, int w, int h, int border ) -> QImage {
	assert( border >= 0 && w > 2 * border && h > 2 * border );
	QImage image( w, h, QImage::Format_RGBA8888 );
	image.fill( Qt::transparent );
	QPainter painter( &image );
	painter.setRenderHint( QPainter::Antialiasing );
	renderer->render( &painter, QRect( border, border, w - 2 * border, h - 2 * border ) );
	return image;
}

[[nodiscard]]
auto rasterizeSvg( const QByteArray &data, const ImageOptions &options ) -> QImage {
	assert( options.desiredSize );
	const auto desiredWidth  = (int)options.desiredSize->first;
	const auto desiredHeight = (int)options.desiredSize->second;
	const auto borderWidth   = (int)options.borderWidth;
	assert( desiredWidth > 2 * borderWidth && desiredHeight > 2 * borderWidth );

	QSvgRenderer renderer( data );
	if( !renderer.isValid() ) {
		return QImage();
	}

	QImage image( rasterizeSvg( &renderer, desiredWidth, desiredHeight, borderWidth ) );
	if( desiredWidth < 2 || desiredHeight < 2 ) {
		return image;
	}

	if( !options.fitSizeForCrispness ) {
		return image;
	}

	QImage altImage( rasterizeSvg( &renderer, desiredWidth - 1, desiredHeight - 1, borderWidth ) );
	return estimateCrispness( image ) > estimateCrispness( altImage ) ? image : altImage;
}

// The dest is assumed to accept ARGB8 pixels
// TODO: Allow caching parsed SVG data
// TODO: Use spans for supplying arguments
[[nodiscard]]
auto rasterizeSvg( const void *rawSvgData, size_t rawSvgDataSize,
				   void *dest, size_t destCapacity, const ImageOptions &options )
	-> std::optional<std::pair<unsigned, unsigned>> {
	if( !options.desiredSize ) {
		wsw::failWithLogicError( "The desired size must be specified" );
	}

	const auto [desiredWidth, desiredHeight] = *options.desiredSize;
	const size_t expectedSize = 4 * desiredWidth * desiredHeight;
	if( destCapacity < expectedSize ) {
		wsw::failWithOutOfRange( "The dest buffer has an insufficient capacity" );
	}

	const QByteArray data( QByteArray::fromRawData( (const char *)rawSvgData, (int)rawSvgDataSize ) );
	const QImage image( rasterizeSvg( data, options ) );
	if( image.isNull() ) {
		return std::nullopt;
	}

	assert( ( (size_t)image.sizeInBytes() == expectedSize ) || options.fitSizeForCrispness );
	std::memcpy( dest, image.constBits(), image.sizeInBytes() );
	return std::make_pair( (unsigned)image.width(), (unsigned)image.height() );
}

[[nodiscard]]
auto decodeImageData( const void *rawImageData, size_t rawImageDataSize, unsigned *width, unsigned *height,
					  unsigned *samples, std::optional<unsigned> requestedSamples ) -> uint8_t * {
	int intWidth = 0, intHeight = 0, intSamples = 0;
	// No in-place loading wtf?
	// How does this thing get broadly suggested in the internets?
	void *bytes  = stbi_load_from_memory( (const stbi_uc *)rawImageData, (int)rawImageDataSize, &intWidth,
										  &intHeight, &intSamples, requestedSamples.value_or( 0 ) );
	if( bytes ) {
		if( width ) {
			*width = (unsigned)intWidth;
		}
		if( height ) {
			*height = (unsigned)intHeight;
		}
		if( samples ) {
			*samples = (unsigned)intSamples;
		}
		return (uint8_t *)bytes;
	}
	return nullptr;
}

[[nodiscard]]
auto scaleImageData( const void *srcImageData, unsigned srcWidth, unsigned srcHeight,
					 unsigned desiredWidth, unsigned desiredHeight, int samples ) -> uint8_t * {
	if( samples == 1 || samples == 3 || samples == 4 ) {
		return stbir_resize_uint8_linear( (const uint8_t *)srcImageData, (int)srcWidth, (int)srcHeight, 0, nullptr,
										  (int)desiredWidth, (int)desiredHeight, 0, (stbir_pixel_layout)samples );
	}
	return nullptr;
}

}