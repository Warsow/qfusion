#ifndef WSW_1094ec95_82ea_4513_8646_70d16650429e_H
#define WSW_1094ec95_82ea_4513_8646_70d16650429e_H

#include <QQuickAsyncImageProvider>
#include <QThreadPool>
#include <QRunnable>

#include <common/types/staticstring.h>

namespace wsw::ui {

class WswImageResponse : public QQuickImageResponse {
	Q_OBJECT

	friend class WswImageProvider;
public:
	enum Options { NoOptions = 0x0, Grayscale = 0x1 };

	WswImageResponse( const QString &name, const QSize &requestedSize, Options options = NoOptions )
		: m_name( name.toUtf8() ), m_requestedSize( requestedSize ), m_options( options ) {}

	Q_SIGNAL void ready();

	void exec();

	[[nodiscard]]
	auto textureFactory() const -> QQuickTextureFactory * override;

private:
	const QByteArray m_name;
	const QSize m_requestedSize;

	QImage m_image;
	const Options m_options;

	[[nodiscard]]
	auto loadSvg( const QByteArray &fileData ) -> QImage;
	[[nodiscard]]
	auto loadOther( const QByteArray &fileData, const wsw::StringView &ext ) -> QImage;
	[[nodiscard]]
	auto convertToGrayscale( QImage &&image ) -> QImage;
};

class WswImageProvider : public QQuickAsyncImageProvider {
	QThreadPool m_threadPool;
public:
	WswImageProvider();

	auto requestImageResponse( const QString &id, const QSize &requestedSize ) -> WswImageResponse * override;
};

}

#endif
