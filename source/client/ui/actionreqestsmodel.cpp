#include "actionrequestmodel.h"
#include "local.h"
#include <client/client.h>

namespace wsw::ui {

auto ActionRequestsModel::Entry::getMatchingAction( int key ) const -> std::optional<wsw::StringView> {
	for( unsigned i = 0; i < numActions; ++i ) {
		const auto [commandSpan, actionKey] = actions[i];
		if( key == actionKey ) {
			const auto [off, len] = commandSpan;
			return wsw::StringView( actionsDataBuffer.data() + off, len, wsw::StringView::ZeroTerminated );
		}
	}
	return std::nullopt;
}

auto ActionRequestsModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Title, "title" }, { Desc, "desc" }, { ExpectsInput, "expectsInput" } };
}

auto ActionRequestsModel::rowCount( const QModelIndex & ) const -> int {
	return m_entries.size();
}

auto ActionRequestsModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case Title: return m_entries[row].title;
				case Desc: return m_entries[row].desc;
				case ExpectsInput: return m_entries[row].numActions != 0;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

void ActionRequestsModel::removeAt( unsigned index ) {
	beginRemoveRows( QModelIndex(), (int)index, (int)index );
	m_entries.erase( m_entries.begin() + index );
	endRemoveRows();
}

void ActionRequestsModel::update() {
	// TODO: Use some common frame time
	const auto currTime = Sys_Milliseconds();
	for( unsigned i = 0; i < m_entries.size(); ) {
		if( m_entries[i].timeoutAt < currTime ) {
			removeAt( i );
		} else {
			i++;
		}
	}
}

void ActionRequestsModel::touch( const wsw::StringView &tag, unsigned timeout,
								 const wsw::StringView &title, const wsw::StringView &desc,
								 const std::pair<wsw::StringView, int> *actionsBegin,
								 const std::pair<wsw::StringView, int> *actionsEnd ) {
	const auto currTime = Sys_Milliseconds();
	for( Entry &entry: m_entries ) {
		if( entry.tag.equalsIgnoreCase( tag ) ) {
			// Update the desc if needed
			if( entry.desc.compare( QLatin1String( desc.data(), (int)desc.size() ), Qt::CaseSensitive ) != 0 ) {
				entry.desc = toStyledText( desc );
				const auto row = (int)( std::addressof( entry ) - m_entries.begin() );
				const QModelIndex modelIndex( index( row, 0 ) );
				Q_EMIT dataChanged( modelIndex, modelIndex );
			}
			entry.timeoutAt = currTime + timeout;
			return;
		}
	}

	if( m_entries.full() ) {
		return;
	}

	beginInsertRows( QModelIndex(), (int)m_entries.size(), (int)m_entries.size() );

	auto *const entry = new( m_entries.unsafe_grow_back() )Entry;
	entry->timeoutAt = currTime + timeout;
	entry->tag.assign( tag );
	entry->title = toStyledText( title );
	entry->desc = toStyledText( desc );

	entry->numActions = (unsigned)( actionsEnd - actionsBegin );
	if( entry->numActions >= std::size( entry->actions ) ) {
		entry->numActions = std::size( entry->actions );
	}
	for( unsigned i = 0; i < entry->numActions; ++i ) {
		const auto [command, key] = *( actionsBegin + i );
		const auto off = (unsigned)entry->actionsDataBuffer.size();
		assert( command.isZeroTerminated() );
		entry->actionsDataBuffer.append( command.data(), command.size() + 1 );
		entry->actions[i] = { { off, entry->actionsDataBuffer.size() - 1 }, key };
	}

	endInsertRows();
}

void ActionRequestsModel::removeByTag( const wsw::StringView &tag ) {
	for( unsigned i = 0; i < m_entries.size(); ) {
		if( m_entries[i].tag.equalsIgnoreCase( tag ) ) {
			removeAt( i );
		} else {
			i++;
		}
	}
}

void ActionRequestsModel::clear() {
	beginResetModel();
	m_entries.clear();
	endResetModel();
}

bool ActionRequestsModel::handleKeyEvent( int quakeKey ) {
	for( const auto &entry: m_entries ) {
		if( const auto maybeCommand = entry.getMatchingAction( quakeKey ) ) {
			assert( maybeCommand->isZeroTerminated() );
			CL_Cbuf_AppendCommand( maybeCommand->data() );
			CL_Cbuf_AppendCommand( "\n" );
			removeAt( (unsigned)( std::addressof( entry ) - m_entries.begin() ) );
			return true;
		}
	}
	return false;
}

}