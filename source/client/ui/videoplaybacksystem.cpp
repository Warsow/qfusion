/*
Copyright (C) 2023 Chasseur de Bots

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

// The video playback parts are derived from a reference code distributed under following terms

/*
Copyright(c) 2019 Dominic Szablewski

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files(the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and / or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions :
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include "videoplaybacksystem.h"

#include <QThread>
#include <QImage>
#include <QVideoSurfaceFormat>

#include <common/helpers/links.h>
#include <common/helpers/singletonholder.h>
#include <common/helpers/textstreamwriterextras.h>
#include "local.h"

#define PL_MPEG_IMPLEMENTATION
#include "../../../third-party/pl_mpeg/pl_mpeg.h"

#include <QDebug>

namespace wsw::ui {

constexpr QVideoFrame::PixelFormat kDesiredPixelFormat = QVideoFrame::Format_BGRA32;

void VideoSource::setFilePath( const QByteArray &filePath ) {
	if( m_filePath != filePath ) {
		m_filePath = filePath;

		detachDecoder();
		stopVideoSurface();

		Status status = m_status;
		if( filePath.isEmpty() ) {
			status = Idle;
		} else if( VideoDecoder *const decoder = m_playbackSystem->createDecoderForPath( this, filePath ) ) {
			m_decoder = decoder;

			connect( this, &VideoSource::updateRequested, m_decoder, &VideoDecoder::onUpdateRequested, Qt::QueuedConnection );
			connect( this, &VideoSource::deleteDecoder, m_decoder, &VideoDecoder::deleteLater, Qt::QueuedConnection );
			connect( m_decoder, &VideoDecoder::frameAvailable, this, &VideoSource::onFrameAvailable, Qt::QueuedConnection );

			status = Running;
		} else {
			uiWarning() << "Failed to open the video file" << filePath;
			status = Error;
		}

		Q_EMIT filePathChanged( filePath );
		applyStatus( status );
	}
}

void VideoSource::setVideoSurface( QAbstractVideoSurface *videoSurface ) {
	if( m_videoSurface != videoSurface ) {
		m_videoSurface = videoSurface;
		Status status = m_status;
		if( videoSurface ) {
			bool supportsDesiredFormat = false;
			for( const QVideoFrame::PixelFormat &pixelFormat: videoSurface->supportedPixelFormats() ) {
				if( pixelFormat == kDesiredPixelFormat ) {
					supportsDesiredFormat = true;
				}
			}
			if( !supportsDesiredFormat ) {
				status = Error;
			} else {
				if( !videoSurface->start( QVideoSurfaceFormat( QSize(), kDesiredPixelFormat ) ) ) {
					status = Error;
				}
			}
		} else {
			status = Idle;
		}
		Q_EMIT videoSurfaceChanged( videoSurface );
		applyStatus( status );
	}
}

VideoSource::VideoSource() {
	VideoPlaybackSystem::instance()->registerSource( this );
}

VideoSource::~VideoSource() {
	if( m_decoder ) {
		Q_EMIT deleteDecoder();
		m_decoder = nullptr;
	}
	stopVideoSurface();
	VideoPlaybackSystem::instance()->unregisterSource( this );
}

void VideoSource::onFrameAvailable( const QVideoFrame &frame ) {
	if( m_videoSurface ) {
		Status status = m_status;
		if( frame.isValid() ) {
			if( !m_videoSurface->isFormatSupported( QVideoSurfaceFormat( frame.size(), frame.pixelFormat() ) ) ) {
				status = Error;
			} else {
				if( !m_videoSurface->present( frame ) ) {
					status = Error;
				}
			}
		} else {
			status = Error;
		}
		applyStatus( status );
	} else {
		applyStatus( Idle );
	}
}

void VideoSource::detachDecoder() {
	if( m_decoder ) {
		Q_EMIT deleteDecoder();
		disconnect( m_decoder );
		m_decoder = nullptr;
	}
}

void VideoSource::stopVideoSurface() {
	if( m_videoSurface ) {
		m_videoSurface->stop();
	}
}

void VideoSource::applyStatus( Status status ) {
	if( m_status != status ) {
		switch( status ) {
			case Idle: uiDebug() << "Video source status=Idle filePath=" << m_filePath; break;
			case Running: uiDebug() << "Video source status=Running filePath=" << m_filePath; break;
			case Error: uiDebug() << "Video source status=Error filePath=" << m_filePath; break;
		}
		if( status != Running ) {
			detachDecoder();
			stopVideoSurface();
			m_filePath.clear();
		}
		m_status = status;
		Q_EMIT statusChanged( status );
	}
}

VideoDecoder::~VideoDecoder() noexcept {
	if( m_plm ) {
		plm_destroy( m_plm );
	}
	if( m_plmBuffer ) {
		plm_buffer_destroy( m_plmBuffer );
	}
}

void VideoDecoder::onUpdateRequested( int64_t timestamp ) {
	// Note: This condition is not reachable for looping videos
	// TODO: Check decoding errors here?
	if( !plm_has_ended( m_plm ) ) {
		if( const int64_t diffMillis = timestamp - m_lastUpdateTimestamp; diffMillis > 0 ) {
			const double frameSeconds = wsw::min( (double)diffMillis * 1e-3, 1.0 / 30.0 );
			// May call the decoder callback, which in turn emits frameAvailable()
			plm_decode( m_plm, frameSeconds );
		}
	} else {
		// Signal termination
		Q_EMIT frameAvailable( QVideoFrame() );
	}

	m_lastUpdateTimestamp = timestamp;
}

void VideoDecoder::fillBufferCallback( plm_buffer_t *buffer, void *userData ) {
	plm_buffer_discard_read_bytes( buffer );

	const size_t oldBufferSize = plm_buffer_get_size( buffer );
	// Should not happen?
	if( oldBufferSize >= kPlmBufferInitialCapacity ) {
		uiWarning() << "Won't fill the buffer over the initial capacity";
		return;
	}

	const size_t totalBytesToRead = kPlmBufferInitialCapacity - oldBufferSize;
	assert( totalBytesToRead <= kPlmBufferInitialCapacity );

	// TODO: Can't we read directly into the PLM buffer
	size_t totalBytesRead = 0;
	auto *const decoder   = (VideoDecoder *)userData;
	while( totalBytesRead < totalBytesToRead ) {
		// Read exactly the number of bytes needed.
		// While the buffer can grow internally at its own, this is not a desired behavior.
		size_t bytesToReadThisStep = std::size( decoder->m_readFileBuffer );
		if( totalBytesRead + bytesToReadThisStep > totalBytesToRead ) {
			bytesToReadThisStep = totalBytesToRead - totalBytesRead;
			assert( bytesToReadThisStep < std::size( decoder->m_readFileBuffer ) );
		}
		assert( decoder->m_fileHandle != std::nullopt );
		if( decoder->m_fileHandle->isAtEof() ) {
			if( !decoder->m_fileHandle->rewind() ) {
				uiWarning() << "Failed to rewind the video file";
				// TODO: What do we do with reading errors?
				break;
			}
		}
		const std::optional<size_t> bytesRead = decoder->m_fileHandle->read( decoder->m_readFileBuffer, bytesToReadThisStep );
		// TODO: Same as above
		if( bytesRead == std::nullopt ) {
			uiWarning() << "Failed to read the video file";
			break;
		}
		totalBytesRead += *bytesRead;
		plm_buffer_write( buffer, decoder->m_readFileBuffer, *bytesRead );
	}

	assert( plm_buffer_get_size( buffer ) <= kPlmBufferInitialCapacity );
}

void VideoDecoder::decodeVideoCallback( plm_t *plm, void *opaqueFrame, void *userData ) {
	auto *const decoder = (VideoDecoder *)userData;
	auto *const frame   = (plm_frame_t *)opaqueFrame;

	const auto sizeInBytes = (int)( frame->width * frame->height * 4 );
	const auto lineStride  = (int)( frame->width * 4 );
	const QSize frameSize( (int)frame->width, (int)frame->height );

	// TODO: How to send 3 planes without this slow conversion
	QVideoFrame qFrame( sizeInBytes, frameSize, lineStride, kDesiredPixelFormat );
	qFrame.map( QAbstractVideoBuffer::WriteOnly );
	// Clear alpha (wtf?)
	memset( qFrame.bits(), 0, sizeInBytes );
	// Note: Converting to ARGB intentionally (wtf?). Only this setup works.
	plm_frame_to_argb( frame, qFrame.bits(), lineStride );
	qFrame.unmap();

	Q_EMIT decoder->frameAvailable( qFrame );
}

static SingletonHolder<VideoPlaybackSystem> g_instanceHolder;

void VideoPlaybackSystem::init() {
	g_instanceHolder.init();
}

void VideoPlaybackSystem::shutdown() {
	g_instanceHolder.shutdown();
}

auto VideoPlaybackSystem::instance() -> VideoPlaybackSystem * {
	return g_instanceHolder.instance();
}

void VideoPlaybackSystem::registerSource( VideoSource *source ) {
	wsw::link( source, &m_sourcesHead );
	source->m_playbackSystem = this;
}

void VideoPlaybackSystem::unregisterSource( VideoSource *source ) {
	wsw::unlink( source, &m_sourcesHead );
}

auto VideoPlaybackSystem::createDecoderForPath( VideoSource *source, const QByteArray &filePath ) -> VideoDecoder * {
	const wsw::StringView pathView( filePath.data(), (size_t)filePath.size() );
	if( !pathView.endsWith( wsw::StringView( ".mpeg" ), wsw::IgnoreCase ) ) {
		uiError() << "The video path" << filePath << "must have a .mpeg extension";
		return nullptr;
	}

	std::optional<wsw::fs::ReadHandle> maybeHandle = wsw::fs::openAsReadHandle( pathView );
	if( !maybeHandle ) {
		uiError() << "Failed to open the video file" << filePath;
		return nullptr;
	}

	std::unique_ptr<VideoDecoder> decoder( new VideoDecoder );
	// TODO: PLM error handling leaves a lot to be desired. We hope of never running into OOM.
	decoder->m_plmBuffer = plm_buffer_create_with_capacity( VideoDecoder::kPlmBufferInitialCapacity );
	plm_buffer_set_load_callback( decoder->m_plmBuffer, &VideoDecoder::fillBufferCallback, decoder.get() );
	decoder->m_fileHandle = std::move( maybeHandle );
	decoder->m_plm = plm_create_with_buffer( decoder->m_plmBuffer, 0 );

	// Try figuring out whether we've managed to load valid data using retrieval of these properties

	const int width        = plm_get_width( decoder->m_plm );
	const int height       = plm_get_height( decoder->m_plm );
	const double framerate = plm_get_framerate( decoder->m_plm );

	bool hadValidationErrors = false;
	if( width < 192 || width > 4096 ) {
		uiError() << "The width of the video" << width << "is out of expected bounds";
		hadValidationErrors = true;
	}
	if( height < 192 || height > 4096 ) {
		uiError() << "The height of the video" << height << "is out of expected bounds";
		hadValidationErrors = true;
	}
	if( framerate < 15.0 || framerate > 90.0 ) {
		uiError() << "The framerate of the video" << framerate << "is out of expected bounds";
		hadValidationErrors = true;
	}
	if( hadValidationErrors ) {
		uiError() << "The video file" << filePath << "seems to be corrupt or having unsupported format";
		return nullptr;
	}

	uiDebug() << wsw::named( "width", width ) << wsw::named( "height", height ) << wsw::named( "framerate", framerate );

	plm_set_video_decode_callback( decoder->m_plm, (plm_video_decode_callback)&VideoDecoder::decodeVideoCallback, decoder.get() );

	plm_set_loop( decoder->m_plm, 1 );
	plm_set_audio_enabled( decoder->m_plm, 0 );
	plm_set_video_enabled( decoder->m_plm, 1 );

	// TODO: Protect from failure
	if( !m_decoderThread ) {
		m_decoderThread = new QThread;
		m_decoderThread->start( QThread::NormalPriority );
	}

	decoder->moveToThread( m_decoderThread );
	connect( decoder.get(), &QObject::destroyed, this, &VideoPlaybackSystem::decoderDestroyed, Qt::QueuedConnection );
	assert( m_numActiveDecoders >= 0 );
	m_numActiveDecoders++;
	return decoder.release();
}

void VideoPlaybackSystem::decoderDestroyed( QObject * ) {
	assert( m_numActiveDecoders > 0 );
	m_numActiveDecoders--;
}

void VideoPlaybackSystem::update( int64_t timestamp ) {
	if( m_sourcesHead ) {
		for( VideoSource *source = m_sourcesHead; source; source = source->next ) {
			Q_EMIT source->updateRequested( timestamp );
		}
		m_lastActivityTimestamp = timestamp;
	} else {
		if( m_decoderThread ) {
			if( m_lastActivityTimestamp + 10'000 < timestamp ) {
				// TODO: Just check? This relies upon timings, unlikely to fail though
				assert( !m_numActiveDecoders );
				m_decoderThread->quit();
				m_decoderThread = nullptr;
			}
		}
	}
}

VideoPlaybackSystem::~VideoPlaybackSystem() {
	if( m_decoderThread ) {
		m_decoderThread->quit();
		m_decoderThread->wait();
	}
}

}