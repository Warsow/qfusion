/*
Copyright (C) 2013 Victor Luchits

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

#include <common/facilities/q_collision.h>
#include <common/helpers/glob.h>
#include <common/facilities/qfiles.h>
#include <common/facilities/messagestreams.h>
#include "bsp.h"
#include <common/helpers/algorithm.h>
#include <common/types/stringview.h>
#include <common/helpers/stringsplitter.h>
#include <common/types/podvector.h>
#include <common/types/vector.h>
#include <common/helpers/tonum.h>
#include <common/facilities/wswfs.h>

#include <cctype>
#include <optional>
#include <string_view>
#include <memory>
#include <unordered_map>

/*
==============================================================

BSP FORMATS

==============================================================
*/

static const int mod_IBSPQ3Versions[] = { Q3BSPVERSION, RTCWBSPVERSION, 0 };
static const int mod_RBSPQ3Versions[] = { RBSPVERSION, 0 };
static const int mod_FBSPQ3Versions[] = { QFBSPVERSION, 0 };

const bspFormatDesc_t q3BSPFormats[] =
{
	{ QFBSPHEADER, mod_FBSPQ3Versions, QF_LIGHTMAP_WIDTH, QF_LIGHTMAP_HEIGHT, BSP_RAVEN, LUMP_ENTITIES },
	{ IDBSPHEADER, mod_IBSPQ3Versions, LIGHTMAP_WIDTH, LIGHTMAP_HEIGHT, BSP_NONE, LUMP_ENTITIES },
	{ RBSPHEADER, mod_RBSPQ3Versions, LIGHTMAP_WIDTH, LIGHTMAP_HEIGHT, BSP_RAVEN, LUMP_ENTITIES },

	// trailing NULL
	{ NULL, NULL, 0, 0, 0, 0 }
};

/*
* Com_FindBSPFormat
*/
const bspFormatDesc_t *Q_FindBSPFormat( const bspFormatDesc_t *formats, const char *header, int version ) {
	int j;
	const bspFormatDesc_t *bspFormat;

	// check whether any of passed formats matches the header/version combo
	for( bspFormat = formats; bspFormat->header; bspFormat++ ) {
		if( strlen( bspFormat->header ) && strncmp( header, bspFormat->header, strlen( bspFormat->header ) ) ) {
			continue;
		}

		// check versions listed for this header
		for( j = 0; bspFormat->versions[j]; j++ ) {
			if( version == bspFormat->versions[j] ) {
				break;
			}
		}

		// found a match
		if( bspFormat->versions[j] ) {
			return bspFormat;
		}
	}

	return NULL;
}

/*
* Com_FindFormatDescriptor
*/
const modelFormatDescr_t *Q_FindFormatDescriptor( const modelFormatDescr_t *formats, const uint8_t *buf, const bspFormatDesc_t **bspFormat ) {
	int i;
	const modelFormatDescr_t *descr;

	// search for a matching header
	for( i = 0, descr = formats; descr->header; i++, descr++ ) {
		if( descr->header[0] == '*' ) {
			const char *header;
			int version;

			header = ( const char * )buf;
			version = LittleLong( *( (int *)( (uint8_t *)buf + descr->headerLen ) ) );

			// check whether any of specified formats matches the header/version combo
			*bspFormat = Q_FindBSPFormat( descr->bspFormats, header, version );
			if( *bspFormat ) {
				return descr;
			}
		} else {
			if( !strncmp( (const char *)buf, descr->header, descr->headerLen ) ) {
				return descr;
			}
		}
	}

	return NULL;
}

using wsw::operator""_asView;

// Using TreeMaps/HashMaps with strings as keys is an anti-pattern.
// This is a hack to get things done.
// TODO: Replace by a sane trie implementation.

namespace std {
	template<>
	struct hash<wsw::StringView> {
		auto operator()( const wsw::StringView &s ) const noexcept -> std::size_t {
			std::string_view stdView( s.data(), s.size() );
			std::hash<std::string_view> hash;
			return hash.operator()( stdView );
		}
	};
}

class SurfExtraFlagsCache {
public:
	void loadIfNeeded();

	[[nodiscard]]
	auto getExtraFlagsForMaterial( const wsw::StringView &material ) -> unsigned;
private:
	struct ClassEntry {
		// TODO: Store as Either/Variant
		wsw::StringView simpleName;
		wsw::StringView pattern;
		unsigned flags;
	};

	struct CacheEntry {
		std::unique_ptr<char[]> name;
		unsigned flags;
	};

	[[nodiscard]]
	static auto loadDataFromFile( const wsw::StringView &filePath ) -> std::optional<wsw::PodVector<char>>;

	// The return type should've been Either<ErrorString, Pair<String, UInt>>
	[[nodiscard]]
	static auto tryParsingLine( const wsw::StringView &lineToken, const wsw::CharLookup &separators )
	-> std::pair<const char *, std::optional<std::pair<wsw::StringView, unsigned>>>;

	[[maybe_unused]]
	static auto putZeroByteAfterStringView( wsw::PodVector<char> *fileData, const wsw::StringView &view ) -> const char *;

	bool m_triedLoading { false };

	wsw::PodVector<char> m_rawFileData;
	wsw::PodVector<ClassEntry> m_classEntries;
	std::unordered_map<wsw::StringView, CacheEntry> m_lookupCache;
};

auto SurfExtraFlagsCache::putZeroByteAfterStringView( wsw::PodVector<char> *fileData,
													  const wsw::StringView &view ) -> const char * {
	const auto viewOffset = (size_t)( view.data() - fileData->data() );
	assert( viewOffset + view.length() < fileData->size() );
	( *fileData )[viewOffset + view.length()] = '\0';
	return fileData->data() + viewOffset;
}

void SurfExtraFlagsCache::loadIfNeeded() {
	if( m_triedLoading ) {
		return;
	}

	m_triedLoading = true;

	// TODO: Allow multiple files, allow overriding non-pure content
	// TODO: Add sanity limits
	if( std::optional<wsw::PodVector<char>> maybeFileData = loadDataFromFile( "maps/custom_surf_params.txt"_asView ) ) {
		const wsw::CharLookup lineSeparators( "\0\r\n"_asView );
		const wsw::CharLookup tokenSeparators( []( char ch ) { return ::isspace( ch ); } );
		wsw::PodVector<char> fileData( std::move( *maybeFileData ) );
		wsw::StringSplitter lineSplitter( wsw::StringView( fileData.data(), fileData.size() ) );
		while( const auto maybeLineTokenAndLineNum = lineSplitter.getNextWithNum( lineSeparators ) ) {
			auto [lineToken, lineNum] = *maybeLineTokenAndLineNum;
			// Trim other whitespaces now
			if( const wsw::StringView line = lineToken.trim(); !line.empty() ) {
				const auto [maybeError, maybeResults] = tryParsingLine( line, tokenSeparators );
				if( maybeResults ) {
					const auto [parsedString, parsedFlags] = *maybeResults;
					putZeroByteAfterStringView( &fileData, parsedString );
					wsw::StringView ztString( parsedString.data(), parsedString.size(), wsw::StringView::ZeroTerminated );
					wsw::StringView simpleName, pattern;
					// TODO: Check for bogus patterns
					// TODO: Delegate checks to tryParsingLine
					if( ztString.contains( '*' ) ) {
						pattern = ztString;
					} else {
						simpleName = ztString;
					}
					m_classEntries.emplace_back( ClassEntry {
						.simpleName = simpleName,
						.pattern    = pattern,
						.flags      = parsedFlags
					});
				} else if( maybeError ) {
					const char *ztLine = putZeroByteAfterStringView( &fileData, line );
					Com_Printf( S_COLOR_YELLOW "Failed to parse line %d `%s`: %s\n", (int)lineNum, ztLine, maybeError );
				}
			}
		}
		m_rawFileData = std::move( fileData );
	}
}

static const std::pair<wsw::StringView, unsigned> kSurfImpactMaterialNames[] {
	{ "stone"_asView,  (unsigned)SurfImpactMaterial::Stone },
	{ "stucco"_asView, (unsigned)SurfImpactMaterial::Stucco },
	{ "wood"_asView,   (unsigned)SurfImpactMaterial::Wood },
	{ "dirt"_asView,   (unsigned)SurfImpactMaterial::Dirt },
	{ "sand"_asView,   (unsigned)SurfImpactMaterial::Sand },
	{ "metal"_asView,  (unsigned)SurfImpactMaterial::Metal },
	{ "glass"_asView,  (unsigned)SurfImpactMaterial::Glass }
};

auto SurfExtraFlagsCache::tryParsingLine( const wsw::StringView &line, const wsw::CharLookup &separators )
-> std::pair<const char *, std::optional<std::pair<wsw::StringView, unsigned>>> {
	wsw::StringSplitter tokenSplitter( line );

	wsw::StringView parsedString;
	unsigned parsedFlags     = 0;
	unsigned numParsedTokens = 0;
	const char *error        = nullptr;
	while( const std::optional<wsw::StringView> maybeToken = tokenSplitter.getNext() ) {
		if( const wsw::StringView token = maybeToken->trim(); !token.empty() ) {
			if( numParsedTokens == 3 ) {
				error = "Extra trailing tokens";
				break;
			} else if( token.equals( "//"_asView ) ) {
				if( numParsedTokens != 3 ) {
					error = "Insufficient number of non-commented tokens";
				}
				break;
			} else {
				if( numParsedTokens == 0 ) {
					const auto cmp = [&]( const std::pair<wsw::StringView, unsigned> &pair ) -> bool {
						return pair.first.equalsIgnoreCase( token );
					};
					const auto end = std::end( kSurfImpactMaterialNames );
					if( const auto it = wsw::find_if( std::begin( kSurfImpactMaterialNames ), end, cmp ); it != end ) {
						parsedFlags |= ( it->second << kSurfImpactMaterialShift );
					} else {
						error = "Unknown flags bit name";
						break;
					}
				} else if( numParsedTokens == 1 ) {
					if( const auto maybeNum = wsw::toNum<unsigned>( token ); maybeNum && *maybeNum <= 3 ) {
						parsedFlags |= *maybeNum << kSurfImpactMaterialParamShift;
					} else if( token != "-"_asView ) {
						error = "Illegal parameter, must be in [0,3] range or be a dash";
						break;
					}
				} else if( numParsedTokens == 2 ) {
					parsedString = token;
				}
				numParsedTokens++;
			}
		}
	}

	if( error ) {
		return { error, std::nullopt };
	}

	if( numParsedTokens == 0 ) {
		return { nullptr, std::nullopt };
	}

	assert( !parsedString.empty() && parsedFlags != 0 && numParsedTokens == 3 );
	return { nullptr, std::make_optional( std::make_pair( parsedString, parsedFlags ) ) };
}

auto SurfExtraFlagsCache::loadDataFromFile( const wsw::StringView &filePath ) -> std::optional<wsw::PodVector<char>> {
	if( std::optional<wsw::fs::ReadHandle> maybeHandle = wsw::fs::openAsReadHandle( filePath ) ) {
		if( const size_t size = maybeHandle->getInitialFileSize() ) {
			wsw::PodVector<char> result;
			result.resize( size + 1 );
			if( maybeHandle->readExact( result.data(), size ) ) {
				result[size] = '\0';
				return result;
			}
		}
	}

	return std::nullopt;
}

auto SurfExtraFlagsCache::getExtraFlagsForMaterial( const wsw::StringView &material ) -> unsigned {
	// Postpone loading to the first use (we would like to have logging systems initialized)
	loadIfNeeded();

	if( auto it = m_lookupCache.find( material ); it != m_lookupCache.end() ) {
		return it->second.flags;
	}

	unsigned flags = 0;
	for( const ClassEntry &entry: m_classEntries ) {
		if( !entry.simpleName.empty() ) {
			if( entry.simpleName.equalsIgnoreCase( material ) ) {
				flags = entry.flags;
				break;
			}
		} else {
			assert( !entry.pattern.empty() && entry.pattern.isZeroTerminated() && material.isZeroTerminated() );
			if( glob_match( entry.pattern.data(), material.data(), 0 ) ) {
				flags = entry.flags;
				break;
			}
		}
	}

	// There's no lightweight heap-allocated string without a redundant local buffer, we have to use char arrays
	const unsigned nameLength = material.length();
	std::unique_ptr<char[]> name( new char[nameLength + 1] );
	char *const nameData = name.get();
	material.copyTo( nameData, nameLength + 1 );

	// Be cautious with moves, the key data should point to the value buffer
	CacheEntry entry { .name = std::move( name ), .flags = flags };
	m_lookupCache.insert( std::make_pair( wsw::StringView( nameData, nameLength ), std::move( entry ) ) );

	return flags;
}

static SurfExtraFlagsCache surfExtraFlagsCache;

namespace wsw::bsp {

auto getExtraFlagsForMaterial( const wsw::StringView &material ) -> uint32_t {
	return ::surfExtraFlagsCache.getExtraFlagsForMaterial( material );
}

}