#include "gametypeoptionsmodel.h"
#include "local.h"
#include <client/client.h>
#include <common/wswstringsplitter.h>
#include <common/wswtonum.h>

using wsw::operator""_asView;

namespace wsw::ui {

auto GametypeOptionsModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ (int)Role::Title, "title" },
		{ (int)Role::Kind, "kind" },
		{ (int)Role::NumItems, "numItems" },
		{ (int)Role::SelectionLimit, "selectionLimit" },
		{ (int)Role::Current, "current" }
	};
}

auto GametypeOptionsModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_allowedOptions.size();
}

auto GametypeOptionsModel::data( const QModelIndex &modelIndex, int role ) const -> QVariant {
 	if( modelIndex.isValid() ) {
 		if( const int row = modelIndex.row(); (unsigned)row < (unsigned)m_allowedOptions.size() ) {
			const unsigned entryIndex = m_allowedOptions[row].entryIndex;
			assert( entryIndex < (unsigned)m_allOptionEntries.size() );
			const OptionEntry &entry = m_allOptionEntries[entryIndex];
			if( (Role)role == Role::Current ) {
				QVariantList result;
				if( entry.selectionLimit == 1 ) {
					result.append( m_allowedOptions[row].currentValue );
				} else {
					for( unsigned index = 0; index < entry.numItems; ++index ) {
						if( (unsigned)m_allowedOptions[row].currentValue & ( 1u << index ) ) {
							result.append( index );
						}
					}
				}
				return result;
			} else {
 				switch( (Role)role ) {
 					case Role::Title: return getString( entry.titleSpanIndex );
 					case Role::Kind: return entry.kind;
 					case Role::NumItems: return entry.numItems;
					case Role::SelectionLimit: return entry.selectionLimit;
 					default: break;
 				}
 			}
 		}
 	}
 	return {};
}

auto GametypeOptionsModel::addString( const wsw::StringView &string ) -> unsigned {
	return m_stringDataStorage.add( string );
}

auto GametypeOptionsModel::getString( unsigned stringSpanIndex ) const -> QByteArray {
	const wsw::StringView view( m_stringDataStorage[stringSpanIndex] );
	return QByteArray::fromRawData( view.data(), (int)view.size() );
}

auto GametypeOptionsModel::getSelectableEntry( int optionRow, int indexInRow ) const -> const SelectableItemEntry & {
	assert( (unsigned)optionRow < (unsigned)m_allowedOptions.size() );
	const auto index = m_allowedOptions[optionRow].entryIndex;
	assert( (unsigned)index < (unsigned)m_allOptionEntries.size() );
	assert( m_allOptionEntries[index].kind != Boolean );
	[[maybe_unused]] auto [listOff, listLen] = m_allOptionEntries[index].selectableItemsSpan;
	assert( (unsigned)listOff + (unsigned)listLen <= (unsigned)m_selectableItemEntries.size() );
	assert( (unsigned)indexInRow < (unsigned)listLen );
	return m_selectableItemEntries[listOff + (unsigned)indexInRow];
}

auto GametypeOptionsModel::getSelectorItemTitle( int optionRow, int indexInRow ) const -> QByteArray {
	return getString( getSelectableEntry( optionRow, indexInRow ).titleSpanIndex );
}

static const QByteArray kImagePrefix( "image://wsw/" );

auto GametypeOptionsModel::getSelectorItemIcon( int optionIndex, int chosenIndex ) const -> QByteArray {
	return kImagePrefix + getString( getSelectableEntry( optionIndex, chosenIndex ).iconSpanIndex );
}

void GametypeOptionsModel::select( int optionRowIndex, const QVariantList &selectedItemIndices ) {
	assert( (unsigned)optionRowIndex < m_allowedOptions.size() );

	OptionRow *const optionRow     = std::addressof( m_allowedOptions[optionRowIndex] );
	const OptionEntry &optionEntry = m_allOptionEntries[optionRow->entryIndex];

	int selectedNumericValue;
	assert( optionEntry.kind == ExactlyNOfList );
	assert( optionEntry.selectionLimit == (unsigned)selectedItemIndices.size() );
	if( optionEntry.selectionLimit == 1 ) {
		assert( selectedItemIndices.size() == 1 );
		selectedNumericValue = selectedItemIndices.front().toInt();
		assert( selectedNumericValue >= 0 && selectedNumericValue < (int)optionEntry.numItems );
	} else {
		selectedNumericValue = 0;
		for( const QVariant &selectedIndexVariant: selectedItemIndices ) {
			const int selectedIndex = selectedIndexVariant.toInt();
			const int selectedBit   = 1 << selectedIndex;
			assert( !( selectedNumericValue & selectedBit ) );
			selectedNumericValue |= selectedBit;
		}
	}

	if( optionRow->currentValue != selectedNumericValue ) {
		optionRow->currentValue = selectedNumericValue;
		const QModelIndex modelIndex( this->index( optionRowIndex ) );
		Q_EMIT dataChanged( modelIndex, modelIndex, kCurrentRoleAsVector );

		assert( (unsigned)optionRowIndex < (unsigned)m_allowedOptions.size() );
		const auto index = m_allowedOptions[optionRowIndex].entryIndex;
		assert( (unsigned)index < (unsigned)m_allOptionEntries.size() );

		const wsw::StringView commandName( m_stringDataStorage[m_allOptionEntries[index].commandSpanIndex] );
		wsw::StaticString<64> commandText;
		static_assert( commandText.capacity() > kMaxCommandLen + 24 );
		commandText << commandName << ' ' << selectedNumericValue;
		CL_Cbuf_AppendCommand( commandText.data() );
	}
}

bool GametypeOptionsModel::parseEntryParts( const wsw::StringView &string, wsw::StaticVector<wsw::StringView, 4> &parts ) {
	parts.clear();
	wsw::StringSplitter splitter( string );
	while( const std::optional<wsw::StringView> &maybeToken = splitter.getNext( '|' ) ) {
		if( parts.size() == parts.capacity() ) {
			uiError() << "Got too many parts while parsing '|'-separated option parts";
			return false;
		}
		const wsw::StringView token( maybeToken->trim() );
		if( token.empty() ) {
			uiError() << "Got an empty part while parsing '|'-separated option parts parts";
			return false;
		}
		parts.push_back( token );
	}
	if( parts.size() != parts.capacity() ) {
		uiError() << "Got an insufficient number of parts while parsing '|'-separated option parts";
		return false;
	}
	return true;
}

void GametypeOptionsModel::clear() {
	m_allOptionEntries.clear();
	m_selectableItemEntries.clear();
	m_allowedOptions.clear();
	m_stringDataStorage.clear();
}

void GametypeOptionsModel::reload() {
	const bool wasAvailable = isAvailable();
	beginResetModel();
	clear();

	// If we can request option entries, it makes sense to parse available options
	const wsw::StringView command( "requestoptionsstatus"_asView );
	if( CL_Cmd_Exists( command ) ) {
		if( !doReloadFromConfigStrings() ) {
			clear();
		} else {
			// Having parsed options, request actual values
			if( isAvailable() ) {
				CL_Cbuf_AppendCommand( command );
			} else {
				uiWarning() << "Has" << command << "command while no option entries are defined";
			}
		}
	}

	if( const bool available = isAvailable(); available != wasAvailable ) {
		Q_EMIT availableChanged( available );
	}

	endResetModel();
}

static const QString kLoadouts( "Loadouts" );

bool GametypeOptionsModel::doReloadFromConfigStrings() {
	static_assert( MAX_GAMETYPE_OPTIONS == kMaxOptions );
	unsigned configStringNum = CS_GAMETYPE_OPTIONS;
	for(; configStringNum < CS_GAMETYPE_OPTIONS + MAX_GAMETYPE_OPTIONS; ++configStringNum ) {
		if( const std::optional<wsw::StringView> maybeString = ::cl.configStrings.get( configStringNum ) ) {
			if( !parseConfigString( *maybeString ) ) {
				uiError() << "Failed to parse the option configstring" << *maybeString;
				return false;
			}
		} else {
			// Interrupt at empty config string (getting this is perfectly valid)
			break;
		}
	}

	if( const std::optional<wsw::StringView> &maybeTitle = ::cl.configStrings.get( CS_GAMETYPE_OPTIONS_TITLE ) ) {
		QString title( QString::fromLatin1( maybeTitle->data(), (int)maybeTitle->size() ) );
		if( m_tabTitle != title ) {
			m_tabTitle = title;
			Q_EMIT tabTitleChanged( m_tabTitle );
		}
	} else {
		if( m_tabTitle != kLoadouts ) {
			m_tabTitle = kLoadouts;
			Q_EMIT tabTitleChanged( m_tabTitle );
		}
	}

	return true;
}

bool GametypeOptionsModel::parseConfigString( const wsw::StringView &configString ) {
	wsw::StaticVector<wsw::StringView, 4> parts;
	// Expect 5 parts separated by the "|" character
	if( !parseEntryParts( configString, parts ) ) {
		uiError() << "Failed to parse '|'-separated option parts";
		return false;
	}

	// The first part is the option name.
	// The second part describes the kind of the option.
	// The third part contains the command.
	// The fourth part contains an encoded list for the ExactlyNOfList option kind
	const wsw::StringView &titlePart = parts[0], &kindPart = parts[1], &commandPart = parts[2], &valuesPart = parts[3];
	if( commandPart.length() > kMaxCommandLen ) {
		uiError() << "The length of the option command" << commandPart << "exceeds the maximum allowed length" << kMaxCommandLen;
		return false;
	}

	for( const char ch: commandPart ) {
		if( ch == ';' || std::isspace( ch ) ) {
			uiError() << "The command could not be a valid command (illegal characters)";
			return false;
		}
	}

	if( kindPart.equalsIgnoreCase( "Boolean"_asView ) ) {
		// The values part must be a placeholder in this case
		if( valuesPart.equals( wsw::StringView( "-" ) ) ) {
			m_allOptionEntries.emplace_back( OptionEntry {
				.titleSpanIndex   = addString( titlePart ),
				.commandSpanIndex = addString( commandPart ),
				.kind             = Kind::Boolean,
			});
			return true;
		} else {
			uiError() << "The fourth part of a boolean option configstring must be the '-' placeholder";
			return false;
		}
	}

	const wsw::StringView ofListSuffix = "OfList"_asView;
	if( kindPart.endsWith( ofListSuffix, wsw::IgnoreCase ) ) {
		const wsw::StringView &rulesPrefix = kindPart.take( kindPart.length() - ofListSuffix.length() );
		const wsw::StringView &exactlyPrefix = "Exactly"_asView;
		if( !rulesPrefix.startsWith( exactlyPrefix ) ) {
			uiError() << "The prefix for an option list selection rules is currently limited to" << exactlyPrefix;
			return false;
		}
		const wsw::StringView &rulesNumber = rulesPrefix.drop( exactlyPrefix.length() );
		if( rulesNumber.length() != 1 || ( rulesNumber[0] <= '0' || rulesNumber[0] >= '9' ) ) {
			uiError() << "The number in an option list selection rules must be limited to a single non-zero digit";
			return false;
		}
		const std::optional<std::pair<unsigned, unsigned>> &maybeListItemsSpan = addOptionListItems( valuesPart );
		if( !maybeListItemsSpan ) {
			uiError() << "Failed to add option list items";
			return false;
		}
		const auto [_, numItems] = *maybeListItemsSpan;
		const auto selectionLimit = (unsigned)( rulesNumber[0] - '0' );
		if( numItems <= selectionLimit ) {
			uiError() << "Too few option list items" << numItems << "for a selection limit" << selectionLimit;
			return false;
		}
		m_allOptionEntries.emplace_back( OptionEntry {
			.titleSpanIndex      = addString( titlePart ),
			.commandSpanIndex    = addString( commandPart ),
			.kind                = Kind::ExactlyNOfList,
			.numItems            = numItems,
			.selectionLimit      = selectionLimit,
			.selectableItemsSpan = *maybeListItemsSpan,
		});
		return true;
	}

	uiError() << "Illegal option kind" << kindPart;
	return false;
}

auto GametypeOptionsModel::addOptionListItems( const wsw::StringView &string ) -> std::optional<std::pair<unsigned, unsigned>> {
	// Mark the beginning of the added span
	const auto oldListItemsSize = m_selectableItemEntries.size();

	// The items list consists of pairs (title, icon path).
	// The transmitted list is flattened and elements are separated by ","

	unsigned tmpSpans[2];
	unsigned lastTokenNum = 0;
	wsw::StringSplitter splitter( string );
	while( const std::optional<std::pair<wsw::StringView, unsigned>> maybeTokenAndNum = splitter.getNextWithNum( ',' ) ) {
		auto [token, tokenNum] = *maybeTokenAndNum;
		token = token.trim();
		if( token.empty() ) {
			uiError() << "Got an empty token in an options list";
			return std::nullopt;
		}

		const auto spanNum = tokenNum % 2;
		lastTokenNum = tokenNum;
		// TODO: Sanitize the displayed name...

		const bool isOptionName = spanNum == 0;
		if( isOptionName ) {
			if( token.length() > kMaxOptionLen ) {
				uiError() << "The length of the option name" << token.length() << "exceeds the maximum allowed length" << kMaxOptionLen;
				return std::nullopt;
			}
		} else {
			if( token.length() >= MAX_QPATH ) {
				uiError() << "The option icon path cannot be a valid FS path (too long)";
				return std::nullopt;
			}
		}

		tmpSpans[spanNum] = addString( token );

		if( !isOptionName ) {
			if( m_selectableItemEntries.size() - oldListItemsSize > 6 ) {
				uiError() << "Too many options in the list, 6 is the limit";
				return std::nullopt;
			}
			// Complete bulding of the pair
			m_selectableItemEntries.push_back( SelectableItemEntry { tmpSpans[0], tmpSpans[1] } );
		}
	}

	// Check for empty or incomplete
	if( !lastTokenNum || !( lastTokenNum % 2 ) ) {
		uiError() << "The options list is malformed (the number of tokens is empty or uneven)";
		return std::nullopt;
	}

	return std::make_pair( oldListItemsSize, m_selectableItemEntries.size() - oldListItemsSize );
}

bool GametypeOptionsModel::validateBooleanOptionValue( int rawValue ) {
	if( rawValue != 0 && rawValue != 1 ) {
		uiError() << "The value" << rawValue << "is not valid for a boolean option";
		return false;
	}
	return true;
}

bool GametypeOptionsModel::validateExactlyNOfListOptionValue( int rawValue, unsigned selectionLimit, unsigned numItems ) {
	assert( selectionLimit && selectionLimit < numItems && numItems < 32 );
	if( selectionLimit == 1 ) {
		if( (unsigned)rawValue >= numItems ) {
			uiError() << "The value" << rawValue << "is not a valid index for a list option";
			return false;
		}
	} else {
		if( (unsigned)std::popcount( (unsigned)rawValue ) != selectionLimit ) {
			uiError() << "The value" << rawValue << "does not have an expected number of set bits for an option" << selectionLimit;
			return false;
		}
		const unsigned allowedBitMask    = ( 1u << numItems ) - 1u;
		const unsigned disallowedBitMask = ~allowedBitMask;
		if( disallowedBitMask & (unsigned)rawValue ) {
			uiError() << "The value" << rawValue << "has set bits in illegal positions for an option";
			return false;
		}
	}
	return true;
}

void GametypeOptionsModel::handleOptionsStatusCommand( const wsw::StringView &status ) {
	wsw::StaticVector<OptionRow, kMaxOptions> allowedOptions;

	// Options status is transmitted as a list of current option values.
	// Negative current values indicate disallowed options.

	// TODO: What to do in case of errors? Disable the loadouts page?

	wsw::StringSplitter splitter( status );
	while( const std::optional<std::pair<wsw::StringView, unsigned>> maybeTokenAndIndex = splitter.getNextWithNum() ) {
		const auto [token, index] = *maybeTokenAndIndex;
		assert( m_allOptionEntries.size() <= kMaxOptions );
		if( index >= m_allOptionEntries.size() ) {
			uiError() << "Illegal option entry index" << index;
			return;
		}
		const auto maybeValue = wsw::toNum<int>( token );
		if( !maybeValue ) {
			uiError() << "Failed to parse option value from token" << token;
			return;
		}
		// The maximum number of items in list is very limited, so negative values can never be a valid mask
		if( const int value = *maybeValue; value >= 0 ) {
			const OptionEntry &entry = m_allOptionEntries[index];
			if( entry.kind == Boolean ) {
				if( !validateBooleanOptionValue( value ) ) {
					return;
				}
			} else if( entry.kind == ExactlyNOfList ) {
				const unsigned numItems = entry.selectableItemsSpan.second;
				if( !validateExactlyNOfListOptionValue( value, entry.selectionLimit, numItems ) ) {
					return;
				}
			}
			allowedOptions.push_back( { (unsigned)index, value } );
		}
	}

	bool hasHandledUpdatesGracefully = false;
	// TODO: We can avoid doing a full reset in some other cases as well
	if( m_allowedOptions.size() == allowedOptions.size() ) {
		const auto compareIndices = []( const auto &a, const auto &b ) { return a.entryIndex == b.entryIndex; };
		// If indices are the same, only current values are changed
		if( std::equal( allowedOptions.begin(), allowedOptions.end(), m_allowedOptions.begin(), compareIndices ) ) {
			for( unsigned i = 0; i < m_allowedOptions.size(); ++i ) {
				if( m_allowedOptions[i].currentValue != allowedOptions[i].currentValue ) {
					m_allowedOptions[i].currentValue = allowedOptions[i].currentValue;
					const QModelIndex modelIndex( index( (int)i ) );
					Q_EMIT dataChanged( modelIndex, modelIndex, kCurrentRoleAsVector );
				}
			}
			hasHandledUpdatesGracefully = true;
		}
	}

	if( !hasHandledUpdatesGracefully ) {
		beginResetModel();
		m_allowedOptions.clear();
		m_allowedOptions.insert( m_allowedOptions.end(), allowedOptions.begin(), allowedOptions.end() );
		endResetModel();
	}
}

}