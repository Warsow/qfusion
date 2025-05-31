#include "wswimageprovider.h"

#include <QPainter>
#include <QtConcurrent>
#include <QSvgRenderer>
#include <QThreadPool>
#include <QImageReader>
#include <QBuffer>
#include <QUrl>
#include <QUrlQuery>

#include "local.h"
#include <common/wswfs.h>
#include <client/imageloading.h>
#include <client/renderer/ref.h>

namespace wsw::ui {

WswImageProvider::WswImageProvider() {
	// Don't spawn an excessive number of threads
	m_threadPool.setMaxThreadCount( 1 );
	m_threadPool.setExpiryTimeout( 3000 );
}

class WswImageRunnable : public QRunnable {
	WswImageResponse *const m_response;
public:
	explicit WswImageRunnable( WswImageResponse *response ) : m_response( response ) {}
	void run() override;
};

auto WswImageProvider::requestImageResponse( const QString &id, const QSize &requestedSize ) -> WswImageResponse * {
	WswImageResponse::Options options = WswImageResponse::NoOptions;
	QString name = id;
	// Only when the URL parsing is really needed
	if( id.contains( '?' ) ) {
		if( QUrl url( id ); url.isValid() && url.hasQuery() ) {
			QUrlQuery query( url.query() );
			if( const QString &grayscaleValue = query.queryItemValue( "grayscale" ); !grayscaleValue.isEmpty() ) {
				if( grayscaleValue.compare( "true", Qt::CaseInsensitive ) == 0 ) {
					options = (WswImageResponse::Options)( options | WswImageResponse::Grayscale );
				} else if( grayscaleValue.compare( "false", Qt::CaseInsensitive ) == 0 ) {
					uiWarning() << "Illegal \"grayscale\" query option value";
				}
			}
			name = url.path();
		} else {
			uiWarning() << "Failed to parse the supplied argument with '?' character as an URL";
		}
	}

	auto *response = new WswImageResponse( name, requestedSize, options );
	auto *runnable = new WswImageRunnable( response );

	// Listen to the ready() signal. Emit finished() in this calling thread
	QObject::connect( response, &WswImageResponse::ready, response, &WswImageResponse::finished, Qt::QueuedConnection );

	m_threadPool.start( runnable );
	return response;
}

void WswImageRunnable::run() {
	m_response->exec();
	Q_EMIT m_response->ready();
}

auto WswImageResponse::convertToGrayscale( QImage &&image ) -> QImage {
	QImage converted;

	const QImage::Format format = image.format();
	if( format == QImage::Format_RGB888 ) {
		image.convertTo( QImage::Format_Grayscale8 );
		image.convertTo( QImage::Format_RGB888 );
		converted = image;
	} else if( format == QImage::Format_RGBA8888 ) {
		const int width  = image.width();
		const int height = image.height();
		assert( width > 0 && height > 0 );
		converted = QImage( width, height, format );
		for( int lineNum = 0; lineNum < height; ++lineNum ) {
			const uint8_t *const __restrict srcLine = image.constScanLine( lineNum );
			uint8_t *const __restrict dstLine       = converted.scanLine( lineNum );
			int byteNum = 0;
			do {
				const int value      = qGray( srcLine[byteNum + 0], srcLine[byteNum + 1], srcLine[byteNum + 2] );
				dstLine[byteNum + 0] = (uint8_t)value;
				dstLine[byteNum + 1] = (uint8_t)value;
				dstLine[byteNum + 2] = (uint8_t)value;
				dstLine[byteNum + 3] = srcLine[byteNum + 3];
				byteNum += 4;
			} while( byteNum < 4 * width );
		}
	} else {
		uiWarning() << "Failed to convert" << m_name << "to greyscale: unsupported image format";
	}

	return converted;
}

static const wsw::StringView kExtensions[] = { ".svg"_asView, ".tga"_asView, ".png"_asView, ".webp"_asView, ".jpg"_asView };

void WswImageResponse::exec() {
	wsw::StringView nameView( m_name.data(), (size_t)m_name.size(), wsw::StringView::ZeroTerminated );
	if( const auto maybeBaseAndExt = wsw::fs::findFirstExtension( nameView, kExtensions, std::end( kExtensions ) ) ) {
		const auto [base, ext] = *maybeBaseAndExt;

		wsw::StaticString<MAX_QPATH> path;
		path << base << ext;

		if( auto fileHandle = wsw::fs::openAsReadHandle( path.asView() ) ) {
			QByteArray fileData( (int)fileHandle->getInitialFileSize(), Qt::Uninitialized );
			if( fileHandle->read( fileData.data(), fileData.size() ) == std::optional( fileData.size() ) ) {
				QImage result;
				if( ext.equalsIgnoreCase( ".svg"_asView ) ) {
					// No resize needed
					result = loadSvg( fileData );
				} else {
					if( result = loadOther( fileData, ext ); !result.isNull() ) {
						if( m_requestedSize.isValid() ) {
							result = result.scaled( m_requestedSize );
							if( result.isNull() ) {
								uiWarning() << "Failed to scale" << m_name << "to" << m_requestedSize;
							}
						}
					}
				}
				if( !result.isNull() ) {
					if( m_options & Grayscale ) {
						m_image = convertToGrayscale( std::move( result ) );
					} else {
						m_image = result;
					}
				}
			} else {
				uiWarning() << "Failed to read" << path;
			}
		} else {
			uiWarning() << "Failed to open" << path;
		}
	} else {
		uiWarning() << "Failed to find a first extension for" << m_name;
	}
}

auto WswImageResponse::loadSvg( const QByteArray &fileData ) -> QImage {
	QImage result;
	if( m_requestedSize.isValid() && !m_requestedSize.isEmpty() ) {
		ImageOptions imageOptions;
		imageOptions.setDesiredSize( m_requestedSize.width(), m_requestedSize.height() );
		imageOptions.fitSizeForCrispness = true;
		result = rasterizeSvg( fileData, imageOptions );
		if( result.isNull() ) {
			uiWarning() << "Failed to parse SVG for %s\n" << m_name;
		}
	} else {
		uiWarning() << "A valid size must be specified for loading an SVG image %s\n" << m_name;
	}
	return result;
}

auto WswImageResponse::loadOther( const QByteArray &fileData, const wsw::StringView &ext ) -> QImage {
	QImage result;

	unsigned w = 0, h = 0, chans = 0;
	if( uint8_t *bytes = wsw::decodeImageData( fileData.constData(), (size_t)fileData.length(), &w, &h, &chans ) ) {
		if( chans == 3 || chans == 4 ) {
			const QImage::Format format = ( chans == 3 ) ? QImage::Format_RGB888 : QImage::Format_RGBA8888;
			result = QImage( bytes, (int)w, (int)h, format, ::free, bytes );
			if( result.isNull() ) {
				uiWarning() << "Failed to create an image from decoded image data for" << m_name << wsw::StringView( ext );
				::free( bytes );
			}
		} else {
			uiWarning() << "Weird number of" << m_name << ext << "image channels" << chans;
			::free( bytes );
		}
	} else {
		uiWarning() << "Failed to decode" << m_name << ext << "from data";
	}

	return result;
}

auto WswImageResponse::textureFactory() const -> QQuickTextureFactory * {
	if( m_image.isNull() ) {
		return nullptr;
	}
	return QQuickTextureFactory::textureFactoryForImage( m_image );
}

}
