#include "callvotesmodel.h"
#include <client/client.h>
#include <common/helpers/base64.h>
#include <common/helpers/compression.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/stringsplitter.h>
#include "local.h"

#include <QJsonObject>
#include <QQmlEngine>

using wsw::operator""_asView;

namespace wsw::ui {

auto CallvotesListModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Name, "name" },
		{ Desc, "desc" },
		{ Flags, "flags" },
		{ Group, "group" },
		{ ArgsKind, "argsKind" },
		{ ArgsHandle, "argsHandle" },
		{ Current, "current" }
	};
}

auto CallvotesListModel::rowCount( const QModelIndex & ) const -> int  {
	return m_displayedEntryNums.size();
}

auto CallvotesListModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < (unsigned)m_displayedEntryNums.size() ) {
			const auto num = m_displayedEntryNums[row];
			switch( role ) {
				case Name: return m_proxy->getEntry( num ).name;
				case Desc: return m_proxy->getEntry( num ).desc;
				case Flags: return m_proxy->getEntry( num ).flags;
				case Group: return m_proxy->getEntry( num ).group;
				case ArgsKind: return m_proxy->getEntry( num ).kind;
				case ArgsHandle: return m_proxy->getEntry( num ).argsHandle;
				case Current: return m_proxy->getEntry( num ).current;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

auto CallvotesGroupsModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Name, "name" }, { Group, "group" } };
}

auto CallvotesGroupsModel::rowCount( const QModelIndex & ) const -> int {
	return m_roleIndices.size();
}

auto CallvotesGroupsModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < m_roleIndices.size() ) {
			switch( role ) {
				// TODO: Convert other strings from plain bytes on demand too
				case Name: return toStyledText( m_proxy->m_groupDataStorage[m_roleIndices[row] * 2 + 1] );
				case Group: return m_roleIndices[row];
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

void CallvotesListModel::beginReloading() {
	beginResetModel();

	m_parentEntryNums.clear();
	m_displayedEntryNums.clear();
}

void CallvotesListModel::addNum( int num ) {
	m_parentEntryNums.push_back( num );
}

static const wsw::StringView kAll( "all"_asView );

void CallvotesListModel::endReloading() {
	static_assert( CallvotesModelProxy::kMaxGroups < 32 );
	unsigned presentGroupsMask = 0;

	// Build groups
	m_groupsModel.m_roleIndices.clear();
	// Push a dummy group for "all"
	assert( m_proxy->m_groupDataStorage[0].equalsIgnoreCase( kAll ) );
	m_groupsModel.m_roleIndices.push_back( 0 );

	for( const int entryNum: m_parentEntryNums ) {
		const unsigned group = m_proxy->getEntry( entryNum ).group;
		assert( group && group < (unsigned)CallvotesModelProxy::kMaxGroups );
		const unsigned groupBit = 1u << group;
		if( !( presentGroupsMask & groupBit ) ) {
			m_groupsModel.m_roleIndices.push_back( group );
			presentGroupsMask |= groupBit;
		}
	}

	endResetModel();
}

void CallvotesListModel::notifyOfChangesAtNum( int num ) {
	const auto it = wsw::find( m_displayedEntryNums.begin(), m_displayedEntryNums.end(), num );
	if( it != m_displayedEntryNums.end() ) {
		const auto index = (int)( it - m_displayedEntryNums.begin() );
		QModelIndex modelIndex( createIndex( index, 0 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex, kRoleCurrentChangeset );
		Q_EMIT currentChanged( index, m_proxy->getEntry( num ).current );
	}
}

void CallvotesListModel::setGroupFilter( int group ) {
	beginResetModel();
	m_displayedEntryNums.clear();
	if( group ) {
		for( int num: m_parentEntryNums ) {
			if( (int)m_proxy->getEntry( num ).group == group ) {
				m_displayedEntryNums.push_back( num );
			}
		}
	} else {
		for( int num: m_parentEntryNums ) {
			m_displayedEntryNums.push_back( num );
		}
	}
	endResetModel();
}

auto CallvotesListModel::getGroupsModel() -> QObject * {
	if( !m_hasSetGroupModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_groupsModel, QQmlEngine::CppOwnership );
		m_hasSetGroupModelOwnership = true;
	}
	return &m_groupsModel;
}

auto CallvotesListModel::getOptionsList( int handle ) const -> QJsonArray {
	assert( (unsigned)( handle - 1 ) < (unsigned)m_proxy->m_options.size() );
	[[maybe_unused]] const auto &[options, storedHandle] = m_proxy->m_options[handle - 1];
	assert( handle == storedHandle );

	QJsonArray result;
	for( const auto &[off, len]: options.spans ) {
		QString s( QString::fromUtf8( options.content.data() + off, len ) );
		result.append( QJsonObject {{"name", s}, {"value", s}});
	}

	return result;
}

static const std::pair<wsw::StringView, CallvotesListModel::Kind> kArgKindNames[] {
	{ "boolean"_asView, CallvotesListModel::Boolean },
	{ "number"_asView, CallvotesListModel::Number },
	{ "player"_asView, CallvotesListModel::Player },
	{ "minutes"_asView, CallvotesListModel::Minutes },
	{ "maplist"_asView, CallvotesListModel::MapList },
	{ "options"_asView, CallvotesListModel::Options }
};

void CallvotesModelProxy::reload() {
	m_regularModel.beginReloading();
	m_operatorModel.beginReloading();

	m_entries.clear();
	m_options.clear();
	m_groupDataStorage.clear();

	if( const auto maybeError = tryReloading() ) {
		m_entries.clear();
		m_options.clear();
		m_groupDataStorage.clear();
		uiError() << wsw::StringView( maybeError->data() );
	}

	m_regularModel.endReloading();
	m_operatorModel.endReloading();
}

auto CallvotesModelProxy::tryReloading() -> std::optional<wsw::StringView> {
	using Storage = wsw::ConfigStringStorage;
	const Storage &storage = ::cl.configStrings;
	static_assert( (unsigned)Storage::CallvoteFields::Name == 0, "The name is assumed to come first" );

	const auto maybeGroupsString = storage.getCallvoteGroups();
	if( !maybeGroupsString ) {
		return "The groups config string is missing"_asView;
	}
	if( !tryParsingCallvoteGroups( *maybeGroupsString ) ) {
		return "Failed to parse callvote groups"_asView;
	}

	unsigned num = 0;
	for(;; num ++ ) {
		const auto maybeName = storage.getCallvoteName( num );
		if( !maybeName ) {
			// Stop at this
			return std::nullopt;
		}

		const auto maybeDesc = storage.getCallvoteDesc( num );
		const auto maybeStatus = storage.getCallvoteStatus( num );

		// Very unlikely
		if( !maybeDesc || !maybeStatus ) {
			return "A desc or a status are missing"_asView;
		}

		wsw::StringSplitter splitter( *maybeStatus );
		const auto maybeAllowedOps = splitter.getNext();
		// Very unlikely
		if( !maybeAllowedOps ) {
			return "A permitted ops mask is missing"_asView;
		}

		const bool isVotingEnabled = maybeAllowedOps->contains( 'v' );
		const bool isOpcallEnabled = maybeAllowedOps->contains( 'o' );
		// Very unlikely
		if( !isVotingEnabled && !isOpcallEnabled ) {
			return "A permitted ops mask is empty"_asView;
		}

		const auto maybeGroupTag = storage.getCallvoteGroup( num );
		if( !maybeGroupTag ) {
			return "A group tag is not specified"_asView;
		}
		const auto maybeGroupNum = findGroupByTag( *maybeGroupTag );
		if( !maybeGroupNum ) {
			return "Failed to find a group by tag"_asView;
		}

		const auto current = splitter.getNext().value_or( wsw::StringView() );

		const auto maybeArgKindAndHandle = addArgs( storage.getCallvoteArgs( num ) );
		// Very unlikely
		if( !maybeArgKindAndHandle ) {
			return "Failed to parse args"_asView;
		}

		const auto [kind, maybeArgsHandle] = *maybeArgKindAndHandle;
		assert( !maybeArgsHandle || *maybeArgsHandle > 0 );

		unsigned flags = 0;
		if( isVotingEnabled ) {
			flags |= CallvotesListModel::Regular;
		}
		if( isOpcallEnabled ) {
			flags |= CallvotesListModel::Operator;
		}

		m_operatorModel.addNum( (int)m_entries.size() );
		if( isVotingEnabled ) {
			m_regularModel.addNum( (int)m_entries.size() );
		}

		Entry entry {
			QString::fromUtf8( maybeName->data(), maybeName->size() ),
			QString::fromUtf8( maybeDesc->data(), maybeDesc->size() ),
			QString::fromUtf8( current.data(), current.size() ),
			flags,
			*maybeGroupNum,
			kind,
			maybeArgsHandle.value_or( 0 )
		};

		m_entries.emplace_back( std::move( entry ) );
	}
}

auto CallvotesModelProxy::findGroupByTag( const wsw::StringView &tag ) const -> std::optional<unsigned> {
	assert( !m_groupDataStorage.empty() && !( m_groupDataStorage.size() % 2 ) );
	// Respective group names are addressed by even indices
	for( unsigned i = 0; i < m_groupDataStorage.size(); i += 2 ) {
		if( tag.equalsIgnoreCase( m_groupDataStorage[i] ) ) {
			return i / 2;
		}
	}
	return std::nullopt;
}

auto CallvotesModelProxy::addArgs( const std::optional<wsw::StringView> &maybeArgs )
	-> std::optional<std::pair<CallvotesListModel::Kind, std::optional<int>>> {
	if( !maybeArgs ) {
		return std::make_pair( CallvotesListModel::NoArgs, std::nullopt );
	}

	wsw::StringSplitter splitter( *maybeArgs );

	const auto maybeHeadToken = splitter.getNext();
	// Malformed args, should not happen
	if( !maybeHeadToken ) {
		return std::nullopt;
	}

	auto foundKind = CallvotesListModel::NoArgs;
	for( const auto &[name, kind] : kArgKindNames ) {
		assert( kind != CallvotesListModel::NoArgs );
		if( maybeHeadToken->equalsIgnoreCase( name ) ) {
			foundKind = kind;
			break;
		}
	}

	if( foundKind == CallvotesListModel::NoArgs ) {
		return std::nullopt;
	}
	if( foundKind != CallvotesListModel::Options && foundKind != CallvotesListModel::MapList ) {
		return std::make_pair( foundKind, std::nullopt );
	}

	const auto maybeDataToken = splitter.getNext();
	if( !maybeDataToken ) {
		return std::nullopt;
	}

	// Check for illegal trailing tokens
	if( splitter.getNext() ) {
		return std::nullopt;
	}

	if( const auto maybeArgsHandle = parseAndAddOptions( *maybeDataToken ) ) {
		return std::make_pair( foundKind, maybeArgsHandle );
	}

	return std::nullopt;
}

auto CallvotesModelProxy::parseAndAddOptions( const wsw::StringView &encodedOptions ) -> std::optional<int> {
	size_t zippedDataLen = 0;
	auto *decoded = base64_decode( (const unsigned char *)encodedOptions.data(), encodedOptions.size(), &zippedDataLen );
	if( !decoded ) {
		return std::nullopt;
	}

	wsw::PodVector<char> content;
	content.resize( 1u << 15u );

	uLong unpackedDataLen = content.size();
	const auto zlibResult = qzuncompress( (Bytef *)content.data(), &unpackedDataLen, decoded, zippedDataLen );
	Q_free( decoded );
	if( zlibResult != Z_OK ) {
		return std::nullopt;
	}

	content.resize( unpackedDataLen );

	wsw::PodVector<std::pair<uint16_t, uint16_t>> spans;
	wsw::StringSplitter splitter( wsw::StringView( content.data(), content.size() ) );
	while( auto maybeToken = splitter.getNext() ) {
		const auto token = *maybeToken;
		const auto rawOffset = token.data() - content.data();

		// This allows us a further retrieval of zero-terminated tokens
		if( const auto tokenEndIndex = rawOffset + token.length(); tokenEndIndex < content.size() ) {
			content[tokenEndIndex] = '\0';
		}

		assert( rawOffset < (int)std::numeric_limits<uint16_t>::max() );
		assert( token.size() < (size_t)std::numeric_limits<uint16_t>::max() );
		spans.emplace_back( std::make_pair( (uint16_t)rawOffset, (uint16_t)token.size() ) );
	}

	const int handle = (int)m_options.size() + 1;
	OptionTokens tokens { std::move( content ), std::move( spans ) };
	m_options.emplace_back( std::make_pair( std::move( tokens ), handle ) );
	return handle;
}

void CallvotesModelProxy::handleConfigString( unsigned configStringNum, const wsw::StringView &string ) {
	assert( (unsigned)( configStringNum - CS_CALLVOTEINFOS < MAX_CALLVOTEINFOS ) );
	// Only vote status updates are expected
	using Storage = wsw::ConfigStringStorage;
	if( configStringNum % Storage::kNumCallvoteFields != (unsigned)Storage::CallvoteFields::Status ) {
		return;
	}

	const auto entryNum = (int)( configStringNum - CS_CALLVOTEINFOS ) / Storage::kNumCallvoteFields;
	// Should not happen
	if( (unsigned)entryNum >= (unsigned)m_entries.size() ) {
		return;
	}

	wsw::StringSplitter splitter( string );
	// Skip flags
	if( !splitter.getNext() ) {
		return;
	}

	const auto maybeCurrent = splitter.getNext();
	if( !maybeCurrent ) {
		return;
	}

	// TODO: Avoid reallocation?
	m_entries[entryNum].current = QString::fromUtf8( maybeCurrent->data(), maybeCurrent->size() );

	// TODO: Also emit some global signal that should be useful for updating values in an open popup
	m_regularModel.notifyOfChangesAtNum( entryNum );
	m_operatorModel.notifyOfChangesAtNum( entryNum );
}

bool CallvotesModelProxy::tryParsingCallvoteGroups( const wsw::StringView &groups ) {
	if( groups.length() > m_groupDataStorage.charsCapacity() ) {
		return false;
	}

	unsigned lastNum = 0;
	wsw::StringSplitter splitter( groups );
	while( const auto maybeTokenAndNum = splitter.getNextWithNum( ',' ) ) {
		auto [token, num] = *maybeTokenAndNum;
		lastNum = num;
		token = token.trim();
		if( token.empty() ) {
			return false;
		}
		if( !m_groupDataStorage.canAdd( token ) ) {
			return false;
		}
		if( num % 2 ) {
			// Just add the displayed name
			m_groupDataStorage.add( token );
			continue;
		}
		for( unsigned existingGroupIndex = 0; existingGroupIndex < m_groupDataStorage.size(); existingGroupIndex += 2 ) {
			if( m_groupDataStorage[existingGroupIndex].equalsIgnoreCase( token ) ) {
				return false;
			}
		}
		// The first token must equal the "all" group
		if( !num && !token.equalsIgnoreCase( kAll ) ) {
			return false;
		}
		m_groupDataStorage.add( token );
	}

	// Make sure there were some pairs and the last pair is complete
	return lastNum && ( lastNum % 2 );
}

}