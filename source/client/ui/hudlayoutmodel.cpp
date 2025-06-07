#include "hudlayoutmodel.h"
#include "local.h"
#include <common/helpers/algorithm.h>
#include <common/facilities/wswfs.h>
#include <common/helpers/tonum.h>
#include <common/types/staticvector.h>
#include <common/helpers/stringsplitter.h>

#include <QPointF>
#include <QSizeF>
#include <QQmlEngine>

using wsw::operator""_asView;

namespace wsw::ui {

auto HudEditorLayoutModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ ItemKind, "kind" },
		{ Origin, "origin" },
		{ Size, "size" },
		{ Name, "name" },
		{ Color, "color" },
		{ Draggable, "draggable" },
		{ SelfAnchors, "selfAnchors" },
		{ AnchorItemAnchors, "anchorItemAnchors" },
		{ DisplayedAnchors, "displayedAnchors" },
		{ SelfAnchors, "selfAnchors" },
		{ AnchorItemAnchors, "anchorItemAnchors" },
		{ AnchorItemIndex, "anchorItemIndex" }
	};
}

auto HudEditorLayoutModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto HudEditorLayoutModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( int row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case ItemKind: return m_entries[row].kind;
				case Origin: return m_entries[row].rectangle.topLeft();
				case Size: return m_entries[row].rectangle.size();
				case Name: return QByteArray::fromRawData( m_entries[row].name.data(), m_entries[row].name.size() );
				case Color: return m_entries[row].color;
				case Draggable: return isDraggable( row );
				case DisplayedAnchors: return m_entries[row].displayedAnchors;
				case DisplayedAnchorItemIndex: return m_entries[row].getQmlAnchorItem();
				case SelfAnchors: return m_entries[row].selfAnchors;
				case AnchorItemAnchors: return m_entries[row].anchorItemAnchors;
				case AnchorItemIndex: return m_entries[row].realAnchorItem.toRawValue();
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

bool HudEditorLayoutModel::isDraggable( int index ) const {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	// This is not that bad as properties are cached/retrieved on demand
	// and maintaining a bidirectional mapping is extremely error-prone.
	for( unsigned i = 0; i < m_entries.size(); ++i ) {
		if( i != (unsigned)index ) {
			const Entry &entry = m_entries[i];
			if( entry.realAnchorItem.isOtherItem() && entry.realAnchorItem.toItemIndex() == index ) {
				return false;
			}
			if( const auto maybeDisplayedItem = entry.displayedAnchorItem ) {
				if( maybeDisplayedItem->isOtherItem() && maybeDisplayedItem->toItemIndex() == index ) {
					return false;
				}
			}
		}
	}
	return true;
}

void HudEditorLayoutModel::notifyOfDisplayedAnchorsUpdateAtIndex( int index ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	const QModelIndex modelIndex( createIndex( index, 0 ) );
	Q_EMIT dataChanged( modelIndex, modelIndex, kDisplayedAnchorRoles );
}

void HudEditorLayoutModel::notifyOfOriginUpdateAtIndex( int index ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	const QModelIndex modelIndex( createIndex( index, 0 ) );
	Q_EMIT dataChanged( modelIndex, modelIndex, kOriginRoleAsVector );
}

void HudEditorLayoutModel::notifyOfFullUpdateAtIndex( int index ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	const QModelIndex modelIndex( createIndex( index, 0 ) );
	Q_EMIT dataChanged( modelIndex, modelIndex );
}

bool HudLayoutModel::load( const QByteArray &fileName ) {
	return load( wsw::StringView( fileName.data(), fileName.size() ) );
}

bool HudLayoutModel::load( const wsw::StringView &fileName ) {
	wsw::StaticString<MAX_QPATH> pathBuffer;
	if( const auto maybePath = makeFilePath( &pathBuffer, fileName ) ) {
		if( auto maybeHandle = wsw::fs::openAsReadHandle( *maybePath ) ) {
			char dataBuffer[4096];
			if( const size_t size = maybeHandle->getInitialFileSize(); size && size < sizeof( dataBuffer ) ) {
				if( maybeHandle->readExact( dataBuffer, size ) ) {
					if( auto maybeLoadedEntries = deserialize( wsw::StringView( dataBuffer, size ) ) ) {
						if( !maybeLoadedEntries->empty() ) {
							return acceptDeserializedEntries( std::move( *maybeLoadedEntries ) );
						}
					}
				}
			}
		}
	} else {
		uiNotice() << "Failed to make file path";
	}
	return false;
}

static const wsw::StringView kHudsExtension( ".hud"_asView );
static const wsw::StringView kRegularHudsDir( "huds/regular"_asView );
static const wsw::StringView kMiniviewHudsDir( "huds/miniview"_asView );

void HudEditorModel::reloadExistingHuds() {
	wsw::StringView dir, suffix;
	if( m_flavor != HudLayoutModel::Miniview ) {
		dir    = kRegularHudsDir;
		suffix = ".regular"_asView;
	} else {
		dir    = kMiniviewHudsDir;
		suffix = ".miniview"_asView;
	}

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeResult = searchResultHolder.findDirFiles( dir, kHudsExtension ) ) {
		QJsonArray huds;
		for( const wsw::StringView &fileName : *maybeResult ) {
			if( const auto maybeName = wsw::fs::stripExtension( fileName ) ) {
				// The fs can't handle multi-dot extensions (?), here's the workaround
				if( maybeName->endsWith( suffix, wsw::IgnoreCase ) ) {
					const wsw::StringView actualName = maybeName->dropRight( suffix.length() );
					if( actualName.length() <= HudLayoutModel::kMaxHudNameLength ) {
						// Check whether the hud file really contains a valid hud.
						// We use a separate scratchpad model to avoid modifying the actual model.
						if( m_scratchpadLayoutModel.load( actualName ) ) {
							huds.append( QString::fromLatin1( actualName.data(), (int)actualName.size() ).toLower() );
						}
					} else {
						uiWarning() << "The name of file" << fileName << "is too long for a valid hud";
					}
				} else {
					uiWarning() << "The name of file" << fileName << R"(must end with a valud suffix, ".regular" or ".miniview")";
				}
			}
		}
		// Don't hold no longer useful temporary data in memory
		m_scratchpadLayoutModel.beginResetModel();
		m_scratchpadLayoutModel.m_entries.clear();
		m_scratchpadLayoutModel.endResetModel();
		if( huds.empty() ) {
			uiWarning() << "Failed to find any HUD for" << suffix.drop( 1 ) << "flavor";
		}
		if( huds != m_existingHuds ) {
			m_existingHuds = huds;
			Q_EMIT existingHudsChanged( huds );
		}
	}
}

bool HudEditorModel::load( const QByteArray &fileName ) {
	return m_layoutModel.load( fileName );
}

bool HudEditorModel::save( const QByteArray &fileName ) {
	const wsw::StringView fileNameView( fileName.data(), fileName.size() );
	wsw::StaticString<MAX_QPATH> pathBuffer;
	bool hasWrittenSuccessfully = false;
	if( const auto maybePath = m_layoutModel.makeFilePath( &pathBuffer, fileNameView ) ) {
		if( auto maybeHandle = wsw::fs::openAsWriteHandle( *maybePath ) ) {
			wsw::StaticString<4096> dataBuffer;
			if( m_layoutModel.serialize( &dataBuffer ) ) {
				if( maybeHandle->write( dataBuffer.data(), dataBuffer.size() ) ) {
					hasWrittenSuccessfully = true;
				}
			}
		}
	}
	// Make sure that the file handle has been closed to the moment of reloading
	if( hasWrittenSuccessfully ) {
		reloadExistingHuds();
		Q_EMIT hudUpdated( fileName, m_flavor );
	}
	return hasWrittenSuccessfully;
}

auto HudLayoutModel::makeFilePath( wsw::StaticString<MAX_QPATH> *buffer,
								   const wsw::StringView &baseFileName ) const -> std::optional<wsw::StringView> {
	if( baseFileName.length() > kMaxHudNameLength ) {
		return std::nullopt;
	}
	if( baseFileName.length() > wsw::min( 16u, buffer->capacity() ) ) {
		return std::nullopt;
	}
	if( baseFileName.contains( '/' ) || baseFileName.contains( '\\' ) || baseFileName.contains( '.' ) ) {
		return std::nullopt;
	}
	for( char ch: baseFileName ) {
		if( !::isalnum( ch ) ) {
			return std::nullopt;
		}
	}
	buffer->clear();
	if( m_flavor != Miniview ) {
		( *buffer ) << kRegularHudsDir << '/' << baseFileName << ".regular"_asView << kHudsExtension;
	} else {
		( *buffer ) << kMiniviewHudsDir << '/' << baseFileName << ".miniview"_asView << kHudsExtension;
	}
	return buffer->asView();
}

static const wsw::StringView kKind( "Kind"_asView );
static const wsw::StringView kAnchoredTo( "AnchoredTo"_asView );
static const wsw::StringView kSelfAnchors( "SelfAnchors"_asView );
static const wsw::StringView kOtherAnchors( "OtherAnchors"_asView );

static const wsw::StringView *const kOrderedKeywords[] { &kKind, &kAnchoredTo, &kSelfAnchors, &kOtherAnchors };

static const wsw::StringView kField( "Field"_asView );

static const wsw::StringView kVersionToken( "Version"_asView );
static const unsigned kCurrentHudVersion = 1;

bool HudEditorLayoutModel::serialize( wsw::StaticString<4096> *buffer_ ) {
	if( m_entries.size() >= 32 ) {
		wsw::failWithLogicError( "Too many entries, this shouldn't happen" );
	}

	if( m_entries.empty() ) {
		uiWarning() << "Cannot serialize an empty HUD";
		return false;
	}

	wsw::StaticString<32> selfAnchorsString, otherAnchorsString;
	auto &buffer = *buffer_;

	const QMetaEnum kindMeta( QMetaEnum::fromType<Kind>() );
	// `<<` throws on overflow
	try {
		buffer.clear();
		buffer << kVersionToken << ' ' << kCurrentHudVersion << "\r\n"_asView;
		for( const Entry &entry: m_entries ) {
			if( !entry.realAnchorItem.isToolbox() ) {
				buffer << kKind << ' ' << wsw::StringView( kindMeta.valueToKey( entry.kind ) ) << ' ';
				buffer << kAnchoredTo << ' ';
				if( entry.realAnchorItem.isField() ) {
					buffer << kField << ' ';
				} else {
					buffer << wsw::StringView( kindMeta.valueToKey( m_entries[entry.realAnchorItem.toItemIndex()].kind ) ) << ' ';
				}
				writeAnchor( &selfAnchorsString, entry.selfAnchors );
				writeAnchor( &otherAnchorsString, entry.anchorItemAnchors );
				buffer << kSelfAnchors << ' ' << selfAnchorsString << ' ';
				buffer << kOtherAnchors << ' ' << otherAnchorsString << "\r\n"_asView;
			}
		}
		return true;
	} catch( ... ) {
		return false;
	}
}

void HudEditorLayoutModel::writeAnchor( wsw::StaticString<32> *buffer, int anchor ) {
	const auto [first, second] = getAnchorNames( anchor );
	buffer->clear();
	( *buffer ) << first << '|' << second;
}

// -Wdeprecated-enum-enum-conversion
[[nodiscard]]
constexpr auto operator|( HudLayoutModel::HorizontalAnchorBits lhs, HudLayoutModel::VerticalAnchorBits rhs ) -> int {
	return (int)lhs | (int)rhs;
}
[[nodiscard]]
constexpr auto operator|( HudLayoutModel::VerticalAnchorBits lhs, HudLayoutModel::HorizontalAnchorBits rhs ) -> int {
	return (int)lhs | (int)rhs;
}

// Pairs are arranged in their priority order

const HudLayoutModel::AnchorPair HudLayoutModel::kMatchingItemAndItemAnchorPairs[] {
	// Sides
	{ Top | HCenter, Bottom | HCenter },
	{ Bottom | HCenter, Top | HCenter },
	{ VCenter | Left, VCenter | Right },
	{ VCenter | Right, VCenter | Left },

	// Pairs of opposite corners
	{ Top | Left, Bottom | Left }, { Top | Right, Bottom | Left },
	{ Bottom | Left, Top | Right }, { Bottom | Right, Top | Left },

	// Corner-corner pairs that don't lead to overlapping
	{ Top | Left, Bottom | Left }, { Top | Left, Top | Right },
	{ Top | Right, Bottom | Right }, { Top | Right, Top | Left },
	{ Bottom | Left, Top | Left }, { Bottom | Left, Bottom | Right },
	{ Bottom | Right, Top | Right }, { Bottom | Right, Bottom | Left },

	// Corners to mid points of sides
	{ Top | Left, Bottom | HCenter }, { Top | Right, Bottom | HCenter },
	{ Bottom | Left, Top | HCenter }, { Bottom | Right, Top | HCenter },

	// Mid points of sides to corners
	{ Top | HCenter, Bottom | Left }, { Top | HCenter, Bottom | Right },
	{ Bottom | HCenter, Top | Left }, { Bottom | HCenter, Top | Right }
};

const HudLayoutModel::AnchorPair HudLayoutModel::kMatchingItemAndFieldAnchorPairs[] {
	{ VCenter | HCenter, VCenter | HCenter },

	// Centers of same contacting sides
	{ Top | HCenter, Top | HCenter },
	{ Bottom | HCenter, Bottom | HCenter },
	{ VCenter | Left, VCenter | Left },
	{ VCenter | Right, VCenter | Right },

	// Same corners
	{ Top | Left, Top | Left },
	{ Top | Right, Top | Right },
	{ Bottom | Left, Bottom | Left },
	{ Bottom | Right, Bottom | Right },

	// Top or bottom to the vertical center field line
	{ Top | Left, VCenter | Left },
	{ Top | Right, VCenter | Right },
	{ Bottom | Left, VCenter | Left },
	{ Bottom | Right, VCenter | Right },

	// Left or right to the horizontal center field line
	{ Top | Left, Top | HCenter },
	{ Top | Right, Top | HCenter },
	{ Bottom | Left, Bottom | HCenter },
	{ Bottom | Right, Bottom | HCenter },

	// A side mid point to the central point
	{ Top | HCenter, VCenter | HCenter },
	{ Bottom | HCenter, VCenter | HCenter },
	{ VCenter | Left, VCenter | HCenter },
	{ VCenter | Right, VCenter | HCenter },

	// A corner to the central point
	{ Top | Left, VCenter | HCenter },
	{ Top | Right, VCenter | HCenter },
	{ Bottom | Left, VCenter | HCenter },
	{ Bottom | Right, VCenter | HCenter }
};

// TODO: Share the static instance over the codebase
static const wsw::CharLookup kNewlineChars( "\r\n"_asView );

auto HudLayoutModel::deserialize( const wsw::StringView &data ) -> std::optional<wsw::PodVector<FileEntry>> {
	wsw::PodVector<FileEntry> entries;
	wsw::StringSplitter lineSplitter( data );
	std::optional<unsigned> parsedVersion;
	while( const auto maybeNextLine = lineSplitter.getNext( kNewlineChars ) ) {
		if( parsedVersion ) {
			// TODO: Dispatch by version if multiple versions are supported
			if( std::optional<FileEntry> maybeEntry = parseEntry( *maybeNextLine ) ) {
				entries.push_back( *maybeEntry );
			} else {
				uiWarning() << "Failed to parse expected entry line" << *maybeNextLine;
				return std::nullopt;
			}
		} else {
			parsedVersion = parseVersion( *maybeNextLine );
			if( !parsedVersion ) {
				uiWarning() << "Failed to parse expected version line" << *maybeNextLine;
				return std::nullopt;
			}
			if( parsedVersion != std::optional( kCurrentHudVersion ) ) {
				uiWarning() << "Unsupported HUD file version" << *parsedVersion;
				return std::nullopt;
			}
		}
	}

	if( !parsedVersion ) {
		uiWarning() << "Has not managed to find the HUD file version";
		return std::nullopt;
	}
	if( entries.empty() ) {
		uiWarning() << "Has not managed to collect HUD item entries";
		return std::nullopt;
	}

	const QMetaEnum metaKinds( QMetaEnum::fromType<Kind>() );
	assert( metaKinds.keyCount() <= 32 );

	[[maybe_unused]] const auto isAValidKindValue = [&]( int kindValue ) -> bool {
		return kindValue > 0 && kindValue < 32 && metaKinds.valueToKey( kindValue ) != nullptr;
	};

	// Validate self-kinds
	unsigned presentKindsMask = 0;
	for( const FileEntry &entry: entries ) {
		const int kind = entry.selfKind;
		// Must be valid in parsed entries
		assert( isAValidKindValue( kind ) );

		const EditorProps &props = kEditorPropsForKind[kind - 1];
		if( m_flavor == Miniview ) {
			assert( props.kind == kind );
			if( !props.allowInMiniviewHud ) {
				uiWarning() << "An element of kind" << props.name << "is disallowed in miniview HUDs";
				return std::nullopt;
			}
		}

		const unsigned kindBit = 1u << kind;
		if( presentKindsMask & kindBit ) {
			uiWarning() << "An element of kind" << props.name << "appears multiple times";
			return std::nullopt;
		}

		presentKindsMask |= kindBit;
	}

	// Validate other item kinds
	for( const FileEntry &entry: entries ) {
		if( entry.otherKind ) {
			assert( isAValidKindValue( *entry.otherKind ) );
			if( !( presentKindsMask & ( 1u << *entry.otherKind ) ) ) {
				const EditorProps &selfProps = kEditorPropsForKind[entry.selfKind - 1];
				const EditorProps &otherProps = kEditorPropsForKind[*entry.otherKind - 1];
				uiWarning() << "An element of kind" << selfProps.name << "is anchored to an item"
					<< otherProps.name << "which is not present in this HUD";
				return std::nullopt;
			}
		}
	}

	// Reject illegal anchors
	// TODO: Check mutual intersections of items (this requires applying a layout manually)
	// TODO: Check indirect anchor loops (graph cycles)
	for( const FileEntry &entry: entries ) {
		[[maybe_unused]] const AnchorPair *validAnchorsBegin = nullptr;
		[[maybe_unused]] const AnchorPair *validAnchorsEnd   = nullptr;
		[[maybe_unused]] const EditorProps &selfProps        = kEditorPropsForKind[entry.selfKind - 1];
		// Disallow non-existing anchor items
		if( entry.otherKind ) {
			if( entry.selfKind == *entry.otherKind ) {
				uiWarning() << "An element of kind" << selfProps.name << "is anchored to itself";
				return std::nullopt;
			}
			const auto it = wsw::find_if( entries.begin(), entries.end(), [&]( const FileEntry &e ) {
				return e.selfKind == *entry.otherKind;
			});
			assert( it != entries.end() );
			// Try detecting direct (1-1) anchor loops'
			if( it->otherKind == std::optional( entry.selfKind ) ) {
				const EditorProps &otherProps = kEditorPropsForKind[*entry.otherKind - 1];
				uiWarning() << "An element of kind" << selfProps.name << "is in a direct anchoring loop with" << otherProps.name;
				return std::nullopt;
			}
			validAnchorsBegin = std::begin( kMatchingItemAndItemAnchorPairs );
			validAnchorsEnd = std::end( kMatchingItemAndItemAnchorPairs );
		} else {
			validAnchorsBegin = std::begin( kMatchingItemAndFieldAnchorPairs );
			validAnchorsEnd   = std::end( kMatchingItemAndFieldAnchorPairs );
		}
		bool hasValidAnchorPair = false;
		for( const AnchorPair *it = validAnchorsBegin; it != validAnchorsEnd; ++it ) {
			if( it->selfAnchors == entry.selfAnchors && it->otherAnchors == entry.otherAnchors ) {
				hasValidAnchorPair = true;
				break;
			}
		}
		if( !hasValidAnchorPair ) {
			uiWarning() << "Anchors of the element of kind" << selfProps.name << "are invalid/disallowed";
			return std::nullopt;
		}
	}

	return entries;
}

auto HudLayoutModel::parseEntry( const wsw::StringView &line ) -> std::optional<FileEntry> {
	std::optional<int> anchors[2];
	std::optional<Kind> selfKind;
	std::optional<Kind> otherKind;

	unsigned lastTokenNum = 0;
	wsw::StringSplitter splitter( line );
	while( const std::optional<std::pair<wsw::StringView, unsigned>> maybeNextTokenAndNum = splitter.getNextWithNum() ) {
		const auto [token, num] = *maybeNextTokenAndNum;
		lastTokenNum = num;
		if( num % 2 ) {
			if( num == 1 ) {
				if( const std::optional<Kind> maybeKind = parseKind( token ) ) {
					selfKind = maybeKind;
				} else {
					uiWarning() << "Failed to parse the token" << token << "as" << kKind;
					return std::nullopt;
				}
			} else if( num == 3 ) {
				if( const std::optional<Kind> maybeKind = parseKind( token ) ) {
					otherKind = maybeKind;
				} else {
					if( !kField.equalsIgnoreCase( token ) ) {
						uiWarning() << "Failed to parse the token" << token << "as" << kAnchoredTo << "kind";
						return std::nullopt;
					}
				}
			} else if( num == 5 || num == 7 ) {
				const unsigned anchorValueIndex = ( num / 2 ) - 2;
				assert( anchorValueIndex == 0 || anchorValueIndex == 1 );
				if( const std::optional<int> maybeAnchors = parseAnchors( token ) ) {
					anchors[anchorValueIndex] = maybeAnchors;
				} else {
					uiWarning() << "Failed to parse the token" << token << "as" << ( num == 5 ? kSelfAnchors : kOtherAnchors );
					return std::nullopt;
				}
			} else {
				uiWarning() << "Too many tokens in the line";
				return std::nullopt;
			}
		} else {
			// 0, 2, 4, 6
			const unsigned keywordIndex = num / 2;
			if( keywordIndex >= std::size( kOrderedKeywords ) ) {
				uiWarning() << "Too many tokens in the line";
				return std::nullopt;
			}
			if( !kOrderedKeywords[keywordIndex]->equalsIgnoreCase( token ) ) {
				uiWarning() << "A token" << token << "does not match the expected keyword";
				return std::nullopt;
			}
		}
	}

	if( lastTokenNum + 1 == 2 * std::size( kOrderedKeywords ) ) {
		return FileEntry {
			.selfKind     = selfKind.value(),
			.selfAnchors  = anchors[0].value(),
			.otherAnchors = anchors[1].value(),
			.otherKind    = otherKind,
		};
	} else {
		uiWarning() << "Insufficient number of tokens in the line";
		return std::nullopt;
	}
}

auto HudLayoutModel::parseVersion( const wsw::StringView &line ) -> std::optional<unsigned> {
	std::optional<unsigned> result;
	wsw::StringSplitter splitter( line );
	while( const auto maybeTokenAndNum = splitter.getNextWithNum() ) {
		const auto [token, num] = *maybeTokenAndNum;
		if( num == 0 ) {
			if( !token.equalsIgnoreCase( kVersionToken ) ) {
				return std::nullopt;
			}
		} else if( num == 1 ) {
			result = wsw::toNum<unsigned>( token );
			if( !result ) {
				return std::nullopt;
			}
		} else {
			return std::nullopt;
		}
	}
	return result;
}

auto HudLayoutModel::parseAnchors( const wsw::StringView &token ) -> std::optional<int> {
	wsw::StringSplitter splitter( token );
	// Expect exactly 2 tokens
	if( const auto maybeFirst = splitter.getNext( '|' ) ) {
		if( const auto maybeSecond = splitter.getNext( '|' ) ) {
			if( !splitter.getNext() ) {
				return parseAnchors( maybeFirst->trim(), maybeSecond->trim() );
			}
		}
	}
	return std::nullopt;
}

auto HudLayoutModel::parseAnchors( const wsw::StringView &first, const wsw::StringView &second ) -> std::optional<int> {
	std::optional<int> horizontal, vertical;
	if( !( vertical = parseVerticalAnchors( first ) ) ) {
		if( !( horizontal = parseHorizontalAnchors( first ) ) ) {
			return std::nullopt;
		}
	}
	if( vertical ) {
		if( !( horizontal = parseHorizontalAnchors( second ) ) ) {
			return std::nullopt;
		}
	} else {
		if( !( vertical = parseVerticalAnchors( second ) ) )  {
			return std::nullopt;
		}
	}
	return horizontal.value() | vertical.value();
}

static const wsw::StringView kTop( "Top"_asView );
static const wsw::StringView kBottom( "Bottom"_asView );
static const wsw::StringView kVCenter( "VCenter"_asView );

static const wsw::StringView kLeft( "Left"_asView );
static const wsw::StringView kRight( "Right"_asView );
static const wsw::StringView kHCenter( "HCenter"_asView );

auto HudLayoutModel::parseHorizontalAnchors( const wsw::StringView &keyword ) -> std::optional<int> {
	const std::pair<wsw::StringView, int> mapping[] { { kTop, Top }, { kBottom, Bottom }, { kVCenter, VCenter } };
	for( const auto &[word, value] : mapping ) {
		if( word.equalsIgnoreCase( keyword ) ) {
			return value;
		}
	}
	return std::nullopt;
}

auto HudLayoutModel::parseVerticalAnchors( const wsw::StringView &keyword ) -> std::optional<int> {
	const std::pair<wsw::StringView, int> mapping[] { { kLeft, Left }, { kRight, Right }, { kHCenter, HCenter } };
	for( const auto &[word, value] : mapping ) {
		if( word.equalsIgnoreCase( keyword ) ) {
			return value;
		}
	}
	return std::nullopt;
}

auto HudLayoutModel::parseKind( const wsw::StringView &token ) -> std::optional<Kind> {
	const QMetaEnum kindMeta( QMetaEnum::fromType<Kind>() );
	// Implement a case-insensitive comparison manually
	for( int i = 0, end = kindMeta.keyCount(); i < end; ++i ) {
		if( token.equalsIgnoreCase( wsw::StringView( kindMeta.key( i ) ) ) ) {
			return (Kind)kindMeta.value( i );
		}
	}
	return std::nullopt;
}

auto HudEditorToolboxModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Kind, "kind" }, { Name, "name" }, { Size, "size" },
		{ Color, "color" }, { DisplayedAnchors, "displayedAnchors" },
	};
}

auto HudEditorToolboxModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto HudEditorToolboxModel::data( const QModelIndex &modelIndex, int role ) const -> QVariant {
	if( modelIndex.isValid() ) {
		if( const int row = modelIndex.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case Kind: return m_entries[row].kind;
				case Name: return m_entries[row].name.data();
				case Size: return m_entries[row].rectangle.size();
				case Color: return m_entries[row].color;
				case DisplayedAnchors: return m_entries[row].displayedAnchors;
				default:;
			}
		}
	}
	return QVariant();
}

auto HudEditorToolboxModel::findEntryByKind( int kind ) const -> const Entry * {
	for( const Entry &entry: m_entries ) {
		if( entry.kind == kind ) {
			return std::addressof( entry );
		}
	}
	return nullptr;
}

void HudEditorToolboxModel::setDisplayedAnchorsForKind( int kind, int /*anchors*/ ) {
	const Entry *entry = findEntryByKind( kind );
	assert( entry );
	const auto indexOfEntry = entry - m_entries.data();
	// TODO: This is wrong for the mini model
	//assert( (unsigned)( kind - 1 ) < m_entries.size() );
	// TODO: Why were we assinging the kind
	//m_entries[kind - 1].kind = kind;
	const QModelIndex modelIndex( createIndex( (int)indexOfEntry, 0 ) );
	// TODO: Use a better granularity for supplied roles
	Q_EMIT dataChanged( modelIndex, modelIndex, kMutableRoles );
}

bool HudEditorLayoutModel::acceptDeserializedEntries( wsw::PodVector<FileEntry> &&fileEntries ) {
	const QMetaEnum metaKinds( QMetaEnum::fromType<Kind>() );

	// Discover what kinds are missing from the layout
	unsigned presentKindsMask = 0;
	assert( metaKinds.keyCount() < 32 );

	wsw::PodVector<Entry> entries;
	for( FileEntry &fileEntry: fileEntries ) {
		const auto &props = kEditorPropsForKind[fileEntry.selfKind - 1];
		assert( fileEntry.selfKind == props.kind );
		assert( m_flavor != Miniview || props.allowInMiniviewHud );
		Entry entry;
		entry.kind = fileEntry.selfKind;
		entry.selfAnchors = fileEntry.selfAnchors;
		entry.anchorItemAnchors = fileEntry.otherAnchors;
		if( fileEntry.otherKind ) {
			const auto it = wsw::find_if( fileEntries.begin(), fileEntries.end(), [&]( const FileEntry &e ) {
				return e.selfKind == *fileEntry.otherKind;
			});
			assert( it != fileEntries.end() );
			entry.realAnchorItem = AnchorItem::forItem( (unsigned)( it - fileEntries.begin() ) );
		} else {
			entry.realAnchorItem = AnchorItem::forField();
		}
		entry.rectangle.setSize( props.size );
		entry.color = props.color;
		entry.name = props.name;
		entries.push_back( entry );

		// These assertions hold for valid deserialized entries
		const auto kind = (unsigned)entry.kind;
		assert( kind && kind < 32 );
		assert( metaKinds.valueToKey( kind ) );
		assert( !( presentKindsMask & ( 1u << kind ) ) );
		presentKindsMask |= ( 1u << kind );
	}

	// Add toolbox entries for missing item kinds
	for( int i = 1; i < metaKinds.keyCount() + 1; ++i ) {
		if( !( presentKindsMask & ( 1u << (unsigned)i ) ) ) {
			const auto kind = (Kind)i;
			const auto props = kEditorPropsForKind[kind - 1];
			if( m_flavor != Miniview || props.allowInMiniviewHud ) {
				Entry entry;
				entry.kind = kind;
				entry.selfAnchors = VCenter | HCenter;
				entry.anchorItemAnchors = VCenter | HCenter;
				entry.realAnchorItem = AnchorItem::forToolbox();
				entry.rectangle.setSize( props.size );
				entry.color = props.color;
				entry.name = props.name;
				entries.push_back( entry );
			}
		}
	}

	beginResetModel();
	std::swap( m_entries, entries );
	endResetModel();
	return true;
}

const HudLayoutModel::EditorProps HudLayoutModel::kEditorPropsForKind[] {
	EditorProps {
		.name = "Health"_asView, .kind = HealthBar, .size = QSize( 144, 32 ),
		.color = QColor::fromRgbF( 1.0, 0.5, 1.0 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Armor"_asView, .kind = ArmorBar, .size = QSize( 144, 32 ),
		.color = QColor::fromRgbF( 1.0, 0.3, 0.0 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Inventory"_asView, .kind = InventoryBar, .size = QSize( 256, 48 ),
		.color = QColor::fromRgbF( 1.0, 0.8, 0.0 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Weapon status"_asView, .kind = WeaponStatus, .size = QSize( 96, 96 ),
		.color = QColor::fromRgbF( 1.0, 0.5, 0.0 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Match time"_asView, .kind = MatchTime, .size = QSize( 128, 64 ),
		.color = QColor::fromRgbF( 0.7, 0.7, 0.7 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Alpha score"_asView, .kind = AlphaScore, .size = QSize( 128, 56 ),
		.color = QColor::fromRgbF( 1.0, 0.0, 0.0 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Beta score"_asView, .kind = BetaScore, .size = QSize( 128, 56 ),
		.color = QColor::fromRgbF( 0.0, 1.0, 0.0 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Chat"_asView, .kind = Chat, .size = QSize( 256, 72 ),
		.color = QColor::fromRgbF( 0.7, 1.0, 0.3 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Team info"_asView, .kind = TeamInfo, .size = QSize( 192, 128 ),
		.color = QColor::fromRgbF( 0.0, 0.3, 0.7 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Frags feed"_asView, .kind = FragsFeed, .size = QSize( 144, 108 ),
		.color = QColor::fromRgbF( 0.3, 0.0, 0.7 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Message feed"_asView, .kind = MessageFeed, .size = QSize( 256, 64 ),
		.color = QColor::fromRgbF( 0.0, 0.7, 0.7 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Awards area"_asView, .kind = AwardsArea, .size = QSize( 192, 64 ),
		.color = QColor::fromRgbF( 0.0, 0.7, 0.9 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Status message"_asView, .kind = StatusMessage, .size = QSize( 192, 32 ),
		.color = QColor::fromRgbF( 0.3, 0.9, 0.7 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Objective status"_asView, .kind = ObjectiveStatus, .size = QSize( 96, 64 ),
		.color = QColor::fromRgbF( 0.9, 0.6, 0.3 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "POV Nickname"_asView, .kind = PovNickname, .size = QSize( 128, 36 ),
		.color = QColor::fromRgbF( 0.4, 0.8, 0.4 ), .allowInMiniviewHud = true,
	},
	EditorProps {
		.name = "Miniview\npane 1"_asView, .kind = MiniviewPane1, .size = QSize( 128, 128 ),
		.color = QColor::fromRgbF( 0.5, 1.0, 0.6 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Miniview\npane 2"_asView, .kind = MiniviewPane2, .size = QSize( 128, 128 ),
		.color = QColor::fromRgbF( 0.5, 1.0, 0.6 ), .allowInMiniviewHud = false,
	},
	EditorProps {
		.name = "Perf pane"_asView, .kind = PerfPane, .size = QSize( 128, 72 ),
		.color = QColor::fromRgbF( 0.7, 0.7, 0.5 ), .allowInMiniviewHud = false,
	}
};

void HudEditorModel::setFieldAreaSize( qreal width, qreal height ) {
	if( const QSizeF size( width, height); size != m_fieldAreaSize ) {
		m_layoutModel.beginResetModel();
		m_toolboxModel.beginResetModel();
		m_fieldAreaSize = size;
		m_toolboxModel.endResetModel();
		m_layoutModel.endResetModel();
	}
}

void HudEditorModel::setDragAreaSize( qreal width, qreal height ) {
	if( const QSizeF size( width, height ); size != m_dragAreaSize ) {
		m_layoutModel.beginResetModel();
		m_toolboxModel.beginResetModel();
		m_dragAreaSize = size;
		m_toolboxModel.endResetModel();
		m_layoutModel.endResetModel();
	}
}

auto HudEditorModel::getLayoutModel() -> QObject * {
	if( !m_hasSetLayoutModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_layoutModel, QQmlEngine::CppOwnership );
		m_hasSetLayoutModelOwnership = true;
	}
	return &m_layoutModel;
}

auto HudEditorModel::getToolboxModel() -> QObject * {
	if( !m_hasSetToolboxModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_toolboxModel, QQmlEngine::CppOwnership );
		m_hasSetToolboxModelOwnership = true;
	}
	return &m_toolboxModel;
}

void HudEditorModel::trackDragging( int index, qreal x, qreal y ) {
	auto &entries = m_layoutModel.m_entries;
	assert( (unsigned)index < (unsigned)entries.size() );
	entries[index].pendingOrigin = QPointF( x, y );
	updateMarkers( index );
}

void HudEditorModel::finishDragging( int index ) {
	auto &entries = m_layoutModel.m_entries;
	assert( (unsigned)index < (unsigned)entries.size() );
	auto &dragged = entries[index];

	if( dragged.rectangle.topLeft() != dragged.pendingOrigin ) {
		dragged.rectangle.moveTopLeft( dragged.pendingOrigin );
	}

	if( dragged.displayedAnchorItem ) {
		const AnchorItem anchorItem = *dragged.displayedAnchorItem;
		assert( dragged.displayedAnchors );
		dragged.realAnchorItem = anchorItem;
		dragged.selfAnchors = dragged.displayedAnchors;
		if( anchorItem.isOtherItem() ) {
			dragged.anchorItemAnchors = entries[anchorItem.toItemIndex()].displayedAnchors;
		} else if( anchorItem.isField() ) {
			dragged.anchorItemAnchors = m_displayedFieldAnchors;
		} else {
			dragged.anchorItemAnchors = HudLayoutModel::VCenter | HudLayoutModel::HCenter;
		}
	}

	m_layoutModel.notifyOfFullUpdateAtIndex( index );
}

void HudEditorModel::clearDisplayedMarkers( int index ) {
	updateMarkers( index );

	auto &entries = m_layoutModel.m_entries;
	auto &dragged = entries[index];
	if( dragged.displayedAnchorItem ) {
		const auto oldAnchorItem = *dragged.displayedAnchorItem;
		if( oldAnchorItem.isOtherItem() ) {
			const int itemIndex = oldAnchorItem.toItemIndex();
			entries[itemIndex].displayedAnchors = 0;
			assert( entries[itemIndex].displayedAnchorItem == std::nullopt );
			m_layoutModel.notifyOfDisplayedAnchorsUpdateAtIndex( itemIndex );
		} else if( oldAnchorItem.isField() ) {
			setDisplayedFieldAnchors( 0 );
		} else {
			assert( oldAnchorItem.isToolbox() && dragged.kind );
			m_toolboxModel.setDisplayedAnchorsForKind( dragged.kind, 0 );
		}
	}

	entries[index].displayedAnchors = 0;
	entries[index].displayedAnchorItem = std::nullopt;
	m_layoutModel.notifyOfDisplayedAnchorsUpdateAtIndex( index );
}

void HudEditorModel::updateElementPosition( int index, qreal x, qreal y ) {
	auto &entries = m_layoutModel.m_entries;
	assert( (unsigned)index < (unsigned)entries.size() );
	auto &entry = entries[index];
	if( const QPointF point( x, y ); entry.rectangle.topLeft() != point ) {
		entry.rectangle.moveTopLeft( point );
		m_layoutModel.notifyOfOriginUpdateAtIndex( index );
	}
}

Q_INVOKABLE void HudEditorModel::updatePlaceholderPosition( int index, qreal x, qreal y ) {
	auto &entries = m_toolboxModel.m_entries;
	assert( (unsigned)index < (unsigned)entries.size() );
	auto &entry = entries[index];
	if( const QPointF point( x, y ); entry.rectangle.topLeft() != point ) {
		entry.rectangle.moveTopLeft( point );
		// There's no need to dispatch updates in this case
	}
}

class SmallIntSet {
	wsw::StaticVector<int, 3> m_nums;
public:
	[[nodiscard]]
	auto begin() const { return m_nums.begin(); }
	[[nodiscard]]
	auto end() const { return m_nums.end(); }
	void add( int num ) {
		if( !wsw::contains( m_nums, num ) ) {
			m_nums.push_back( num );
		}
	}
};

void HudEditorModel::updateMarkers( int draggedIndex ) {
	// It's more convenient to decompose results
	std::optional<AnchorItem> anchorItem;
	std::optional<AnchorPair> anchorsPair;
	if( const auto maybeItemAndAnchors = getMatchingAnchorItem( draggedIndex ) ) {
		anchorItem = maybeItemAndAnchors->first;
		anchorsPair = maybeItemAndAnchors->second;
	}

	SmallIntSet modifiedRows;
	std::optional<int> updatedToolboxAnchors;

	auto &entries = m_layoutModel.m_entries;
	HudEditorLayoutModel::Entry &dragged = entries[draggedIndex];
	if( dragged.displayedAnchorItem != anchorItem ) {
		// Clear flags of a (maybe) old item
		if( dragged.displayedAnchorItem ) {
			const AnchorItem oldDisplayedAnchorItem = *dragged.displayedAnchorItem;
			if( oldDisplayedAnchorItem.isOtherItem() ) {
				const int itemIndex = oldDisplayedAnchorItem.toItemIndex();
				entries[itemIndex].displayedAnchors = 0;
				entries[itemIndex].displayedAnchorItem = std::nullopt;
				modifiedRows.add( itemIndex );
			} else if( oldDisplayedAnchorItem.isField() ) {
				setDisplayedFieldAnchors( 0 );
			} else {
				assert( oldDisplayedAnchorItem.isToolbox() );
				updatedToolboxAnchors = 0;
			}
		}
		dragged.displayedAnchorItem = anchorItem;
		modifiedRows.add( draggedIndex );
	}

	if( dragged.displayedAnchorItem ) {
		assert( anchorsPair );
		const AnchorItem currDisplayedAnchorItem = *dragged.displayedAnchorItem;
		if( currDisplayedAnchorItem.isOtherItem() ) {
			const int currItemIndex = currDisplayedAnchorItem.toItemIndex();
			if( entries[currItemIndex].displayedAnchors != anchorsPair->otherAnchors ) {
				entries[currItemIndex].displayedAnchors = anchorsPair->otherAnchors;
				modifiedRows.add( currItemIndex );
			}
		} else if( currDisplayedAnchorItem.isField() ) {
			setDisplayedFieldAnchors( anchorsPair->otherAnchors );
		} else {
			assert( currDisplayedAnchorItem.isToolbox() );
			assert( anchorsPair->otherAnchors == ( HudLayoutModel::VCenter | HudLayoutModel::HCenter ) );
			updatedToolboxAnchors = anchorsPair->otherAnchors;
		}
	}

	const int selfAnchors = anchorsPair ? anchorsPair->selfAnchors : 0;
	if( dragged.displayedAnchors != selfAnchors ) {
		dragged.displayedAnchors = selfAnchors;
		modifiedRows.add( draggedIndex );
	}

	for( int row: modifiedRows ) {
		m_layoutModel.notifyOfDisplayedAnchorsUpdateAtIndex( row );
	}

	if( updatedToolboxAnchors != std::nullopt ) {
		m_toolboxModel.setDisplayedAnchorsForKind( dragged.kind, *updatedToolboxAnchors );
	}
}

[[nodiscard]]
static inline bool isClose( const QPointF &pt1, const QPointF &pt2 ) {
	const QPointF diff( pt1 - pt2 );
	// Units are device-independent so this is correct
	return diff.x() * diff.x() + diff.y() * diff.y() < 8.0 * 8.0;
}

template <typename Range>
auto HudEditorModel::getMatchingAnchors( const QRectF &draggedRectangle, const QRectF &otherRectangle,
										 const Range &range )
	-> std::optional<AnchorPair> {
	for( auto it = std::begin( range ); it != std::end( range ); ++it ) {
		const auto &[selfAnchors, otherAnchors] = *it;
		const QPointF selfPoint( getPointForAnchors( draggedRectangle, selfAnchors ) );
		const QPointF otherPoint( getPointForAnchors( otherRectangle, otherAnchors ) );
		if( isClose( selfPoint, otherPoint ) ) {
			return AnchorPair { selfAnchors, otherAnchors };
		}
	}
	return std::nullopt;
}

auto HudEditorModel::getMatchingEntryAnchors( const QRectF &draggedRectangle, const QRectF &otherEntryRectangle )
	-> std::optional<AnchorPair> {
	return getMatchingAnchors( draggedRectangle, otherEntryRectangle, HudLayoutModel::kMatchingItemAndItemAnchorPairs );
}

auto HudEditorModel::getMatchingFieldAnchors( const QRectF &draggedRectangle, const QRectF &fieldRectangle )
	-> std::optional<AnchorPair> {
	return getMatchingAnchors( draggedRectangle, fieldRectangle, HudLayoutModel::kMatchingItemAndFieldAnchorPairs );
}

auto HudEditorModel::getMatchingAnchorItem( int draggedIndex ) const
	-> std::optional<std::pair<AnchorItem, AnchorPair>> {
	const auto &itemEntries = m_layoutModel.m_entries;
	assert( (unsigned)draggedIndex < (unsigned)itemEntries.size() );
	QRectF draggedRectangle( itemEntries[draggedIndex].rectangle );
	draggedRectangle.moveTopLeft( itemEntries[draggedIndex].pendingOrigin );
	const QRectF validAreaRectangle( 0, 0, m_fieldAreaSize.width(), m_fieldAreaSize.height() );
	if( !draggedRectangle.intersects( validAreaRectangle ) ) {
		const auto draggedItemKind = itemEntries[draggedIndex].kind;
		const auto entry           = m_toolboxModel.findEntryByKind( draggedItemKind );
		assert( entry );
		const auto &toolboxEntryRectangle = entry->rectangle;
		const auto centerAnchors = HudLayoutModel::VCenter | HudLayoutModel::HCenter;
		const QPointF draggedItemCenter( getPointForAnchors( draggedRectangle, centerAnchors ) );
		const QPointF toolboxPlaceholderCenter( getPointForAnchors( toolboxEntryRectangle, centerAnchors ) );
		if( isClose( draggedItemCenter, toolboxPlaceholderCenter ) ) {
			return std::make_pair( AnchorItem::forToolbox(), AnchorPair { centerAnchors, centerAnchors } );
		}
		return std::nullopt;
	}

	for( unsigned i = 0; i < itemEntries.size(); ++i ) {
		if( i != (unsigned)draggedIndex ) {
			if( const auto maybeAnchors = getMatchingEntryAnchors( draggedRectangle, itemEntries[i].rectangle ) ) {
				if( isAnchorDefinedPositionValid( draggedIndex, (int)i, *maybeAnchors ) ) {
					const auto entryAnchorItem = itemEntries[i].realAnchorItem;
					if( !entryAnchorItem.isOtherItem() || entryAnchorItem.toItemIndex() != draggedIndex ) {
						return std::make_pair( AnchorItem::forItem( i ), *maybeAnchors );
					}
				}
			}
		}
	}

	assert( m_fieldAreaSize.isValid() );
	const QRectF fieldRectangle( 0, 0, m_fieldAreaSize.width(), m_fieldAreaSize.height() );
	if( const auto maybeAnchors = getMatchingFieldAnchors( draggedRectangle, fieldRectangle ) ) {
		if( isAnchorDefinedPositionValid( draggedIndex, std::nullopt, *maybeAnchors ) ) {
			return std::make_pair( AnchorItem::forField(), *maybeAnchors );
		}
	}

	return std::nullopt;
}

bool HudEditorModel::isAnchorDefinedPositionValid( int draggedIndex, const std::optional<int> &otherIndex,
												   const AnchorPair &anchors ) const {
	const auto &entries = m_layoutModel.m_entries;
	assert( (unsigned)draggedIndex < (unsigned)entries.size() );
	const auto &dragged = entries[draggedIndex];

	QPointF anchorPoint;
	if( otherIndex ) {
		assert( (unsigned)*otherIndex < (unsigned)entries.size() && *otherIndex != draggedIndex );
		anchorPoint = getPointForAnchors( entries[*otherIndex].rectangle, anchors.otherAnchors );
	} else {
		const QRectF fieldRectangle( 0, 0, m_fieldAreaSize.width(), m_fieldAreaSize.height() );
		anchorPoint = getPointForAnchors( fieldRectangle, anchors.otherAnchors );
	}

	// Apply an anchor-defined position to the dragged item rectangle
	QRectF predictedRectangle( dragged.rectangle );
	// Align center first as this moves along both axes
	if( anchors.selfAnchors & ( HudLayoutModel::VCenter | HudLayoutModel::HCenter ) ) {
		predictedRectangle.moveCenter( anchorPoint );
	}

	if( anchors.selfAnchors & HudLayoutModel::Left ) {
		predictedRectangle.moveLeft( anchorPoint.x() );
	} else if( anchors.selfAnchors & HudLayoutModel::Right ) {
		predictedRectangle.moveRight( anchorPoint.x() );
	}

	if( anchors.selfAnchors & HudLayoutModel::Top ) {
		predictedRectangle.moveTop( anchorPoint.y() );
	} else if( anchors.selfAnchors & HudLayoutModel::Bottom ) {
		predictedRectangle.moveBottom( anchorPoint.y() );
	}

	// Try shrinking a bit to mitigate f.p. comparison issues
	if( predictedRectangle.width() > 2.0 && predictedRectangle.height() > 2.0 ) {
		const QPointF oldCenter( predictedRectangle.center() );
		predictedRectangle.setWidth( predictedRectangle.width() - 0.5 );
		predictedRectangle.setHeight( predictedRectangle.height() - 0.5 );
		predictedRectangle.moveCenter( oldCenter );
	}

	const int secondCmpIndex = otherIndex ? *otherIndex : draggedIndex;
	for( unsigned i = 0; i < entries.size(); ++i ) {
		if( i != (unsigned)draggedIndex && i != (unsigned)secondCmpIndex ) {
			if( entries[i].rectangle.intersects( predictedRectangle ) ) {
				return false;
			}
		}
	}

	return true;
}

static constexpr auto kHorizontalBitsMask = 0x7;
static constexpr auto kVerticalBitsMask = 0x7 << 3;

auto HudEditorModel::getPointForAnchors( const QRectF &r, int anchors ) -> QPointF {
	qreal x;
	switch( anchors & kHorizontalBitsMask ) {
		case HudLayoutModel::Left: x = r.left(); break;
		case HudLayoutModel::HCenter: x = 0.5 * ( r.left() + r.right() ); break;
		case HudLayoutModel::Right: x = r.right(); break;
		default: wsw::failWithInvalidArgument( "Invalid X anchor bits" );
	}
	qreal y;
	switch( anchors & kVerticalBitsMask ) {
		case HudLayoutModel::Top: y = r.top(); break;
		case HudLayoutModel::VCenter: y = 0.5 * ( r.top() + r.bottom() ); break;
		case HudLayoutModel::Bottom: y = r.bottom(); break;
		default: wsw::failWithInvalidArgument( "Invalid Y anchor bits" );
	}
	return QPointF( x, y );
}

[[nodiscard]]
static inline auto getHorizontalAnchorName( int bits ) -> wsw::StringView {
	switch( bits & kHorizontalBitsMask ) {
		case HudLayoutModel::Left: return kLeft;
		case HudLayoutModel::HCenter: return kHCenter;
		case HudLayoutModel::Right: return kRight;
		default: return "None"_asView;
	}
}

[[nodiscard]]
static inline auto getVerticalAnchorName( int bits ) -> wsw::StringView {
	switch( bits & kVerticalBitsMask ) {
		case HudLayoutModel::Top: return kTop;
		case HudLayoutModel::VCenter: return kVCenter;
		case HudLayoutModel::Bottom: return kBottom;
		default: return "None"_asView;
	}
}

auto HudLayoutModel::getAnchorNames( int anchors ) -> std::pair<wsw::StringView, wsw::StringView> {
	return { getVerticalAnchorName( anchors ), getHorizontalAnchorName( anchors ) };
}

auto HudLayoutModel::getFlagsForKind( Kind kind ) -> Flags {
	switch( kind ) {
		case HealthBar: return AlivePovOnly;
		case ArmorBar: return AlivePovOnly;
		case InventoryBar: return AlivePovOnly;
		case WeaponStatus: return AlivePovOnly;
		case MatchTime: return NoFlags;
		case AlphaScore: return (Flags)( TeamBasedOnly | AllowPostmatch );
		case BetaScore: return (Flags)( TeamBasedOnly | AllowPostmatch );
		case Chat: return NoFlags;
		case TeamInfo: return (Flags)( TeamBasedOnly | PovOnly );
		case FragsFeed: return NoFlags;
		case MessageFeed: return NoFlags;
		case AwardsArea: return AllowPostmatch;
		case StatusMessage: return NoFlags;
		case ObjectiveStatus: return NoFlags;
		case PovNickname: return ChasePovOnly;
		case MiniviewPane1: return NoFlags;
		case MiniviewPane2: return NoFlags;
		case PerfPane: return NoFlags;
		default: wsw::failWithLogicError( "unreachable" );
	}
}

auto HudLayoutModel::getShownItemBitsForKind( Kind kind ) -> ShownItemBits {
	switch( kind ) {
		case HealthBar: return NoShownItemBits;
		case ArmorBar: return NoShownItemBits;
		case InventoryBar: return NoShownItemBits;
		case WeaponStatus: return NoShownItemBits;
		case MatchTime: return NoShownItemBits;
		case AlphaScore: return NoShownItemBits;
		case BetaScore: return NoShownItemBits;
		case Chat: return NoShownItemBits;
		case TeamInfo: return NoShownItemBits;
		case FragsFeed: return ShowFragsFeed;
		case MessageFeed: return ShowMessageFeed;
		case AwardsArea: return ShowAwards;
		case StatusMessage: return NoShownItemBits;
		case ObjectiveStatus: return NoShownItemBits;
		case PovNickname: return NoShownItemBits;
		// TODO: Should there be some specific bit for miniviews?
		case MiniviewPane1: return NoShownItemBits;
		case MiniviewPane2: return NoShownItemBits;
		case PerfPane: return (ShownItemBits)( ShowFps | ShowNet );
		default: wsw::failWithLogicError( "unreachable" );
	}
}

HudEditorModel::HudEditorModel( HudLayoutModel::Flavor flavor )
	: m_layoutModel( flavor ), m_scratchpadLayoutModel( flavor ), m_flavor( flavor ) {
	reloadExistingHuds();

	m_toolboxModel.beginResetModel();

	for( const auto &props: HudEditorLayoutModel::kEditorPropsForKind ) {
		if( m_flavor != HudLayoutModel::Miniview || props.allowInMiniviewHud ) {
			m_toolboxModel.m_entries.emplace_back( HudEditorToolboxModel::Entry {
				.rectangle = QRectF( 0, 0, props.size.width(), props.size.height() ),
				.name      = props.name,
				.color     = props.color,
				.kind      = props.kind,
			});
		}
	}

	m_toolboxModel.endResetModel();
}

auto InGameHudLayoutModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ ItemKind, "kind" },
		{ Flags, "flags" },
		{ SelfAnchors, "selfAnchors" },
		{ AnchorItemAnchors, "anchorItemAnchors" },
		{ AnchorItemIndex, "anchorItemIndex" },
		{ IndividualMask, "individualMask" },
		{ IsHidden, "isHidden" },
	};
}

auto InGameHudLayoutModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto InGameHudLayoutModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( int row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case ItemKind: return m_entries[row].kind;
				case Flags: return getFlagsForKind( m_entries[row].kind );
				case SelfAnchors: return m_entries[row].selfAnchors;
				case AnchorItemAnchors: return m_entries[row].otherAnchors;
				case AnchorItemIndex: return m_entries[row].anchorItem.toRawValue();
				case IndividualMask: return getShownItemBitsForKind( m_entries[row].kind );
				case IsHidden: return m_entries[row].isHidden;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

bool InGameHudLayoutModel::load( const wsw::StringView &fileName ) {
	const bool result = HudLayoutModel::load( fileName );
	if( result ) {
		const QLatin1String operableBytes( fileName.data(), (int)fileName.size() );
		if( m_fileName.compare( operableBytes, Qt::CaseInsensitive ) != 0 ) {
			m_fileName.clear();
			m_fileName.append( operableBytes );
			Q_EMIT nameChanged( m_fileName );
		}
	}
	return result;
}

bool InGameHudLayoutModel::acceptDeserializedEntries( wsw::PodVector<FileEntry> &&fileEntries ) {
	const QMetaEnum metaKinds( QMetaEnum::fromType<Kind>() );

	// Discover what kinds are missing from the layout
	unsigned presentKindsMask = 0;
	assert( metaKinds.keyCount() < 32 );

	const bool wasEmpty = m_entries.empty();
	if( wasEmpty ) {
		beginResetModel();
	} else {
		m_entries.clear();
	}

	// Items are linked by index, not by kind.
	// The sorting breaks original indices, hence we have to retranslate anchor item indices upon sorting.
	// Note that this is not related to the serialized form where we link by kind.
	assert( fileEntries.size() <= (size_t)metaKinds.keyCount() );
	auto *kindsForOriginalIndices = (Kind *)alloca( sizeof( Kind ) * fileEntries.size() );

	for( const FileEntry &fileEntry: fileEntries ) {
		assert( fileEntry.selfKind && fileEntry.selfKind < std::size( kEditorPropsForKind ) + 1 );
		AnchorItem anchorItem = AnchorItem::forToolbox();
		if( fileEntry.otherKind ) {
			const auto it = wsw::find_if( fileEntries.begin(), fileEntries.end(), [&]( const FileEntry &e ) {
				return e.selfKind == *fileEntry.otherKind;
			});
			assert( it != fileEntries.end() );
			anchorItem = AnchorItem::forItem( (unsigned)( it - fileEntries.begin() ) );
		}
		m_entries.emplace_back( Entry {
			.kind            = fileEntry.selfKind,
			.selfAnchors     = fileEntry.selfAnchors,
			.otherAnchors    = fileEntry.otherAnchors,
			.anchorItem      = anchorItem,
		});

		const auto kind = (unsigned)fileEntry.selfKind;
		assert( kind && kind < 32 );
		assert( metaKinds.valueToKey( kind ) );
		assert( !( presentKindsMask & ( 1u << kind ) ) );
		presentKindsMask |= ( 1u << kind );
		const auto index = m_entries.size() - 1;
		kindsForOriginalIndices[index] = fileEntry.selfKind;
	}

	// Add hidden entries for non-present kinds
	for( int kind = 1; kind < metaKinds.keyCount() + 1; ++kind ) {
		if( !( presentKindsMask & ( 1u << (unsigned)kind ) ) ) {
			if( m_flavor != Miniview || kEditorPropsForKind[kind - 1].allowInMiniviewHud ) {
				m_entries.emplace_back( Entry {
					.kind         = (Kind)kind,
					.selfAnchors  = HCenter | VCenter,
					.otherAnchors = HCenter | VCenter,
					.anchorItem   = AnchorItem::forField(),
					.isHidden     = true,
				});
			}
		}
	}

	wsw::sortPodNonSpeedCritical( m_entries.begin(), m_entries.end(), []( const Entry &a, const Entry &b ) {
		return a.kind < b.kind;
	});

	// Patch item indices after sorting
	for( Entry &entry: m_entries ) {
		// If it was in the file layout
		if( presentKindsMask & ( 1u << (unsigned)entry.kind ) ) {
			// If it needs index patching
			if( entry.anchorItem.isOtherItem() ) {
				const int unpatchedIndex  = entry.anchorItem.toItemIndex();
				const Kind anchorItemKind = kindsForOriginalIndices[unpatchedIndex];
				const auto it = wsw::find_if( m_entries.begin(), m_entries.end(), [&]( const Entry &e ) {
					return e.kind == anchorItemKind;
				});
				const auto newActualIndex = (unsigned)( it - m_entries.begin() );
				assert( newActualIndex < m_entries.size() );
				entry.anchorItem = AnchorItem::forItem( newActualIndex );
			}
		}
	}

	if( wasEmpty ) {
		endResetModel();
	} else {
		Q_EMIT arrangingItemsDisabled();
		const QModelIndex topLeft( this->index( 0 ) );
		const QModelIndex bottomRight( this->index( (int)( m_entries.size() - 1 ) ) );
		Q_EMIT dataChanged( topLeft, bottomRight, kAllRolesExceptKind );
		Q_EMIT arrangingItemsEnabled();
	}

	return true;
}

}