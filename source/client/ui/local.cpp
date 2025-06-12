#include "local.h"
#include <client/renderer/ref.h>

#include <QColor>
#include <QImage>
#include <QPainter>
#include <QSvgRenderer>

namespace wsw::ui {

class HtmlColorNamesCache {
	QByteArray names[10];
public:
	auto getColorName( int colorNum ) -> const QByteArray & {
		assert( (unsigned)colorNum < 10u );
		if( !names[colorNum].isEmpty() ) {
			return names[colorNum];
		}
		const float *rawColor = color_table[colorNum];
		names[colorNum] = QColor::fromRgbF( rawColor[0], rawColor[1], rawColor[2] ).name( QColor::HexRgb ).toLatin1();
		return names[colorNum];
	}
};

static HtmlColorNamesCache htmlColorNamesCache;

static const QLatin1String kFontOpeningTagPrefix( "<font color=\"" );
static const QLatin1String kFontOpeningTagSuffix( "\">" );
static const QLatin1String kFontClosingTag( "</font>" );

// Can't be written as a regular ASCII literal with a 5-character length
static const char kEntityCharsToEscapeData[5] { '>', '<', '&', '\"', (char)0xA0 };
static const wsw::CharLookup kEntityCharsToEscapeLookup( wsw::StringView( kEntityCharsToEscapeData, 5 ) );

static const QLatin1String kEntityGt( "&gt;" );
static const QLatin1String kEntityLt( "&lt;" );
static const QLatin1String kEntityAmp( "&amp;" );
static const QLatin1String kEntityQuot( "&quot;" );
static const QLatin1String kEntityNbsp( "&nbsp;" );

static void appendUtf8View( QString *__restrict dest, const wsw::StringView &__restrict view ) {
	bool hasHiChars = true;
	// Try saving allocations
	if( view.length() < 64 ) {
		hasHiChars = false;
		for( const char ch : view ) {
			if( (unsigned char)ch > 127u ) {
				hasHiChars = true;
				break;
			}
		}
	}
	if( hasHiChars ) {
		dest->append( QString::fromUtf8( view.data(), (int)view.size() ) );
	} else {
		dest->append( QLatin1String( view.data(), (int)view.size() ) );
	}
}

static void appendEscapingEntities( QString *__restrict dest, const wsw::StringView &__restrict src ) {
	wsw::StringView sv( src );
	for(;; ) {
		const std::optional<unsigned> maybeIndex = sv.indexOf( kEntityCharsToEscapeLookup );
		if( !maybeIndex ) {
			appendUtf8View( dest, sv );
			return;
		}

		const auto index = *maybeIndex;
		appendUtf8View( dest, sv.take( index ) );
		const char ch = sv[index];
		sv = sv.drop( index + 1 );

		// The subset of actually supported entities is tiny
		// https://github.com/qt/qtdeclarative/blob/dev/src/quick/util/qquickstyledtext.cpp
		// void QQuickStyledTextPrivate::parseEntity(const QChar *&ch, const QString &textIn, QString &textOut)

		// TODO: Aren't &quot; and &nbsp; stripped by the engine?
		// Still, some content could be supplied by external sources like info servers.
		if( ch == '>' ) {
			dest->append( kEntityGt );
		} else if( ch == '<' ) {
			dest->append( kEntityLt );
		} else if( ch == '&' ) {
			dest->append( kEntityAmp );
		} else if( ch == '"' ) {
			dest->append( kEntityQuot );
		} else if( (int)ch == 0xA0 ) {
			dest->append( kEntityNbsp );
		}
	}
}

auto toStyledText( const wsw::StringView &text ) -> QString {
	QString result;
	result.reserve( (int)text.size() + 1 );

	bool hadColorToken = false;
	wsw::StringView sv( text );
	for(;; ) {
		const std::optional<unsigned> maybeIndex = sv.indexOf( '^' );
		if( !maybeIndex ) {
			appendEscapingEntities( &result, sv );
			if( hadColorToken ) {
				result.append( kFontClosingTag );
			}
			return result;
		}

		const auto index = *maybeIndex;
		appendEscapingEntities( &result, sv.take( index ) );
		sv = sv.drop( index + 1 );
		if( sv.empty() ) {
			if( hadColorToken ) {
				result.append( kFontClosingTag );
			}
			return result;
		}

		if( sv.front() < '0' || sv.front() > '9' ) {
			if( sv.front() == '^' ) {
				result.append( '^' );
				sv = sv.drop( 1 );
			}
			continue;
		}

		if( hadColorToken ) {
			result.append( kFontClosingTag );
		}

		result.append( kFontOpeningTagPrefix );
		result.append( htmlColorNamesCache.getColorName( sv.front() - '0' ) );
		result.append( kFontOpeningTagSuffix );

		sv = sv.drop( 1 );
		hadColorToken = true;
	}
}

auto wrapInColorTags( const wsw::StringView &text, int rgb ) -> QString {
	const int r = COLOR_R( rgb ), g = COLOR_G( rgb ), b = COLOR_B( rgb );
	constexpr const char *digits = "0123456789ABCDEF";
	const char colorBuffer[7] {
		'#', digits[r / 16], digits[r % 16], digits[g / 16], digits[g % 16], digits[b / 16], digits[b % 16]
	};

	auto sizeToReserve = (int)text.size();
	sizeToReserve += kFontOpeningTagPrefix.size() + kFontOpeningTagSuffix.size() + kFontClosingTag.size();
	sizeToReserve += sizeof( colorBuffer );

	QString result;
	result.reserve( sizeToReserve );

	result.append( kFontOpeningTagPrefix );
	result.append( QLatin1String( colorBuffer, (int)std::size( colorBuffer ) ) );
	result.append( kFontOpeningTagSuffix );
	appendEscapingEntities( &result, text );
	result.append( kFontClosingTag );

	return result;
}

auto formatPing( int ping ) -> QByteArray {
	ping = wsw::clamp( ping, 0, 999 );
	int colorNum;
	if( ping < 50 ) {
		colorNum = 2;
	} else if( ping < 100 ) {
		colorNum = 3;
	} else if( ping < 150 ) {
		colorNum = 8;
	} else {
		colorNum = 1;
	}
	const QByteArray &colorName = htmlColorNamesCache.getColorName( colorNum );
	const int totalTagsSize = kFontOpeningTagPrefix.size() + kFontOpeningTagSuffix.size() + kFontClosingTag.size();
	QByteArray result;
	result.reserve( totalTagsSize + colorName.size() + 3 );
	result.setNum( ping );
	result.prepend( kFontOpeningTagSuffix.data(), kFontOpeningTagSuffix.size() );
	result.prepend( colorName );
	result.prepend( kFontOpeningTagPrefix.data(), kFontOpeningTagPrefix.size() );
	result.append( kFontClosingTag.data(), kFontClosingTag.size() );
	return result;
}

}

auto operator<<( wsw::TextStreamWriter &writer, const QString &string ) -> wsw::TextStreamWriter & {
	writer << string.toUtf8();
	return writer;
}

auto operator<<( wsw::TextStreamWriter &writer, const QSize &size ) -> wsw::TextStreamWriter & {
	writer << size.width();
	writer.hasPendingSeparator = false;
	writer << 'x';
	writer.hasPendingSeparator = false;
	writer << size.height();
	return writer;
}

auto operator<<( wsw::TextStreamWriter &writer, const QObject *object ) -> wsw::TextStreamWriter & {
	const char *objectName    = "QObject";
	const void *objectAddress = object;
	if( object ) {
		objectName = object->metaObject()->className();
	}
	writer.writeChars( objectName, std::strlen( objectName ) );
	writer.hasPendingSeparator = false;
	writer.writeChar( '(' );
	writer.hasPendingSeparator = false;
	writer.writePtr( objectAddress );
	writer.hasPendingSeparator = false;
	writer.writeChar( ')' );
	return writer;
}