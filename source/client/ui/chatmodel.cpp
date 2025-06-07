#include "chatmodel.h"

#include <QQmlEngine>

#include "local.h"
#include <client/client.h>
#include <common/helpers/algorithm.h>

namespace wsw::ui {

[[nodiscard]]
static inline auto asQByteArray( const wsw::StringView &view ) -> QByteArray {
	return QByteArray::fromRawData( view.data(), (int)view.size() );
}

auto CompactChatModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Timestamp, "timestamp" }, { Name, "name" }, { Text, "text" } };
}

auto CompactChatModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_proxy->m_lineRefs.size();
}

auto CompactChatModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < (unsigned)m_proxy->m_lineRefs.size() ) {
			const Line *const line = m_proxy->m_lineRefs[row];
			switch( role ) {
				case Text: return toStyledText( line->getText() );
				case Name: return toStyledText( line->getName() );
				case Timestamp: return asQByteArray( line->getTimestamp() );
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

void CompactChatModel::beginRemoveOldestLine( Line *line ) {
	const int lastRow = m_proxy->m_lineRefs.size() - 1;
	assert( m_proxy->m_lineRefs[lastRow] == line );
	beginRemoveRows( QModelIndex(), lastRow, lastRow );
}

auto RichChatModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ RegularMessageText, "regularMessageText" },
		{ SectionName, "sectionName" },
		{ SectionTimestamp, "sectionTimestamp" }
	};
}

auto RichChatModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto RichChatModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			const auto [regularLine, sectionLine] = m_entries[row];
			switch( role ) {
				case RegularMessageText: return regularLine ? toStyledText( regularLine->getText() ) : QVariant();
				case SectionName: return sectionLine ? toStyledText( sectionLine->getName( ) ) : QVariant();
				case SectionTimestamp: return sectionLine ? asQByteArray( sectionLine->getTimestamp() ) : QVariant();
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

/**
 * We pass the allowed difference as a template parameter to avoid confusion with absolute minute argument values
 */
template <int N>
[[nodiscard]]
static bool isWithinNMinutes( int minute1, int minute2 ) {
	// TODO: Account for leap seconds (lol)?
	assert( (unsigned)minute1 < 60u );
	assert( (unsigned)minute2 < 60u );
	if( minute1 == minute2 ) {
		return true;
	}
	const int minMinute = wsw::min( minute1, minute2 );
	const int maxMinute = wsw::max( minute1, minute2 );
	if( maxMinute - minMinute < N + 1 ) {
		return true;
	}
	return ( minMinute + 60 ) - maxMinute < N + 1;
}

void RichChatModel::beginClear() {
	beginResetModel();
}

void RichChatModel::endClear() {
	m_entries.clear();
	endResetModel();
}

void RichChatModel::beginRemoveOldestLine( Line *line ) {
	assert( line );
	// For every shared line 1 or 2 entries are created.
	// We choose removal of these entries as the simplest approach.
	// This leaves next group messages (if any) without their corresponding section but we can accept that.
	const auto numEntries = (int)m_entries.size();
	if( m_entries.back().sectionLine == line ) {
		assert( m_entries.size() >= 2 );
		assert( m_entries[numEntries - 2].regularLine == line );
		beginRemoveRows( QModelIndex(), numEntries - 2, numEntries - 1 );
		m_entries.pop_back();
		m_entries.pop_back();
	} else if( m_entries.back().regularLine == line ) {
		beginRemoveRows( QModelIndex(), numEntries - 1, numEntries - 1 );
		m_entries.pop_back();
	} else {
		wsw::failWithLogicError( "unreachable: the shared line must have its RichChatModel entry counterparts" );
	}
}

void RichChatModel::endRemoveOldestLine() {
	endRemoveRows();
}

void RichChatModel::beginAddingLine( Line *line, const QDate &date, int timeHours, int timeMinutes ) {
	if( canAddToCurrGroup( line, date, timeHours, timeMinutes ) ) {
		beginInsertRows( QModelIndex(), 0, 0 );
		m_lastMessageMinute = timeMinutes;
		m_totalGroupLengthSoFar += line->getText().length();
		m_entries.push_front( { line, nullptr } );
	} else {
		beginInsertRows( QModelIndex(), 0, 1 );
		m_currHeadingDate = date;
		m_currHeadingHour = timeHours;
		m_currHeadingMinute = m_lastMessageMinute = timeMinutes;
		m_totalGroupLengthSoFar = line->getText().length();
		m_entries.push_front( { nullptr, line } );
		m_entries.push_front( { line, nullptr } );
	}
	m_lastMessageName.assign( line->getName() );
}

void RichChatModel::endAddingLine() {
	endInsertRows();
}

bool RichChatModel::canAddToCurrGroup( const Line *line, const QDate &date, int timeHours, int timeMinutes ) {
	if( line->getText().length() + m_totalGroupLengthSoFar < 1000 ) {
		if( line->getName().equalsIgnoreCase( m_lastMessageName.asView() ) ) {
			if( date == m_currHeadingDate && timeHours == m_currHeadingHour ) {
				if( isWithinNMinutes<4>( timeMinutes, m_currHeadingMinute ) ) {
					if( isWithinNMinutes<2>( timeMinutes, m_lastMessageMinute ) ) {
						return true;
					}
				}
			}
		}
	}
	return false;
}

auto ChatProxy::getCompactModel() -> QObject * {
	if( !m_hasSetCompactModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_compactModel, QQmlEngine::CppOwnership );
		m_hasSetCompactModelOwnership = true;
	}
	return &m_compactModel;
}

auto ChatProxy::getRichModel() -> QObject * {
	if( !m_hasSetRichModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_richModel, QQmlEngine::CppOwnership );
		m_hasSetRichModelOwnership = true;
	}
	return &m_richModel;
}

void ChatProxy::clear() {
	for( ChatModel *model: m_childModels ) {
		model->beginClear();
	}

	// This call performs a shallow cleanup but the following one actually does it
	m_lineRefs.clear();
	m_linesAllocator.clear();

	for( ChatModel *model: m_childModels ) {
		model->endClear();
	}

	m_pendingCommandNums.clear();
}

bool ChatProxy::removeFromPendingCommands( uint64_t commandNum ) {
	// TODO: Use some kind of a specialized set
	auto it = wsw::find( m_pendingCommandNums.begin(), m_pendingCommandNums.end(), commandNum );
	if( it != m_pendingCommandNums.end() ) {
		*it = m_pendingCommandNums.back();
		m_pendingCommandNums.pop_back();
		return true;
	}
	return false;
}

void ChatProxy::addReceivedMessage( const wsw::cl::ChatMessage &message, int64_t frameTimestamp ) {
	if( message.sendCommandNum ) {
		(void)removeFromPendingCommands( *message.sendCommandNum );
	}

	m_wasInTheSameFrame = ( m_lastMessageFrameTimestamp == frameTimestamp );
	if( !m_wasInTheSameFrame ) {
		m_lastMessageFrameTimestamp = frameTimestamp;
		const QDateTime dateTime( QDateTime::currentDateTime() );
		m_lastMessageQtDate = dateTime.date();
		const QTime time( dateTime.time() );
		m_lastMessageTimeHours = time.hour();
		m_lastMessageTimeMinutes = time.minute();
		(void)m_lastMessageFormattedTime.assignf( "%d:%02d", m_lastMessageTimeHours, m_lastMessageTimeMinutes );
	}

	if( m_lineRefs.size() == kMaxLines ) {
		Line *const oldestLine = m_lineRefs.back();
		for( ChatModel *model: m_childModels ) {
			model->beginRemoveOldestLine( oldestLine );
		}

		m_lineRefs.pop_back();
		m_linesAllocator.free( oldestLine );

		for( ChatModel *model: m_childModels ) {
			model->endRemoveOldestLine();
		}
	}

	const wsw::StringView &name = message.name, &text = message.text;

	assert( !m_linesAllocator.isFull() );
	uint8_t *const mem = m_linesAllocator.allocOrNull();
	auto *const line = new( mem )Line;

	line->basePtr = (char *)( line + 1 );

	char *writePtr = line->basePtr;
	std::memcpy( writePtr, name.data(), name.size() );
	writePtr[name.size()] = '\0';
	line->nameLen = (uint8_t)name.length();

	writePtr += name.length() + 1;
	std::memcpy( writePtr, m_lastMessageFormattedTime.data(), m_lastMessageFormattedTime.size() );
	writePtr[m_lastMessageFormattedTime.size()] = '\0';
	line->timestampLen = m_lastMessageFormattedTime.length();

	writePtr += m_lastMessageFormattedTime.length() + 1;
	std::memcpy( writePtr, text.data(), text.size() );
	line->textLen = text.length();
	writePtr[text.length()] = '\0';

	for( ChatModel *model: m_childModels ) {
		model->beginAddingLine( line, m_lastMessageQtDate, m_lastMessageTimeHours, m_lastMessageTimeMinutes );
	}

	m_lineRefs.push_front( line );

	for( ChatModel *model: m_childModels ) {
		model->endAddingLine();
	}
}

void ChatProxy::sendMessage( const QString &text ) {
	// TODO: This is quite inefficient
	// TODO: Must be unicode-aware
	const QString clearText( text.trimmed().replace( '\r', ' ' ).replace( '\n', ' ' ).constData() );
	if( !clearText.isEmpty() ) {
		const QByteArray &utf8Text = clearText.toUtf8();
		const wsw::StringView textView( utf8Text.data(), (unsigned)utf8Text.size() );
		if( m_kind == TeamChat ) {
			Con_SendTeamChatMessage( textView );
		} else {
			Con_SendCommonChatMessage( textView );
		}
	}
}

void ChatProxy::handleMessageFault( const MessageFault &fault ) {
	// If the command is missing this is totally fine
	if( removeFromPendingCommands( fault.clientCommandNum ) ) {
		if( fault.kind == MessageFault::Muted ) {
			Q_EMIT muted();
		} else if( fault.kind == MessageFault::Flood ) {
			Q_EMIT floodDetected();
		} else {
			wsw::failWithLogicError( "Unreachable" );
		}
	}
}

}