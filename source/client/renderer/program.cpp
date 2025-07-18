/*
Copyright (C) 2007 Victor Luchits
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

// r_program.c - OpenGL Shading Language support

#include "local.h"
#include "program.h"
#include <common/helpers/links.h>
#include <common/helpers/memspecbuilder.h>
#include <common/helpers/singletonholder.h>
#include <common/facilities/wswfs.h>
#include <common/helpers/parsecompat.h>
#include <common/helpers/stringsplitter.h>
#include <common/helpers/textstreamwriterextras.h>

#include <cctype>

using wsw::operator""_asView;

class ProgramSourceBuilder;

class ProgramSourceFileCache {
	friend class ProgramSourceLoader;
public:

	// Use opaque handles for file entries of builtins
	using KeyOfBuiltin = uintptr_t;

	KeyOfBuiltin deformAutoparticle { 0 };
	KeyOfBuiltin deformAutosprite { 0 };
	KeyOfBuiltin deformAutosprite2 { 0 };
	KeyOfBuiltin deformPrologue { 0 };
	KeyOfBuiltin deformEpilogue { 0 };

	KeyOfBuiltin constants { 0 };
	KeyOfBuiltin instancedTransforms { 0 };
	KeyOfBuiltin macros { 0 };
	KeyOfBuiltin macros120 { 0 };
	KeyOfBuiltin macros130 { 0 };
	KeyOfBuiltin math { 0 };
	KeyOfBuiltin quatTransform { 0 };
	KeyOfBuiltin transformVerts { 0 };
	KeyOfBuiltin uniforms { 0 };
	KeyOfBuiltin waveFuncs { 0 };

	ProgramSourceFileCache();
	~ProgramSourceFileCache();

	void precacheBuiltins();
	void addBuiltinSourceLines( KeyOfBuiltin key, ProgramSourceBuilder *sourceBuilder ) const;
private:

	struct Span {
		unsigned offset { 0 };
		unsigned length { 0 };
	};

	struct RegularInclude {
		Span fileNameSpan;
	};
	struct ConditionalInclude {
		Span fileNameSpan;
		Span conditionSpan;
	};

	using Include = std::variant<RegularInclude, ConditionalInclude>;

	// Either a direct line span an or index of a span in includes
	using LineSpan = std::variant<Span, unsigned>;

	struct FileEntry {
		FileEntry *nextInList { nullptr };
		FileEntry *nextInBin { nullptr };
		const char *stringData { nullptr };
		const LineSpan *lineData { nullptr };
		const Include *includeData { nullptr };
		unsigned stringDataSize { 0 };
		unsigned numLines { 0 };
		unsigned numIncludes { 0 };
		// Name comes first in the string data
		unsigned nameLength { 0 };
		uint32_t nameHash { 0 };

		[[nodiscard]]
		auto getName() const -> wsw::StringView {
			return getStringForSpan( { 0, nameLength } );
		}

		[[nodiscard]]
		auto getStringForSpan( const Span &span ) const -> wsw::StringView {
			assert( span.offset + span.length < stringDataSize );
			return { stringData + span.offset, span.length };
		}

		[[nodiscard]]
		auto getLineForIndex( unsigned index ) const -> const LineSpan & {
			assert( index < numLines );
			return lineData[index];
		}

		[[nodiscard]]
		auto getIncludeForIndex( unsigned index ) const -> const Include & {
			assert( index < numIncludes );
			return includeData[index];
		}
	};

	[[nodiscard]]
	auto getFileEntryForFile( const wsw::StringView &fileName ) -> const FileEntry *;

	[[nodiscard]]
	auto loadFileEntryFromFile( const wsw::StringView &fileName ) -> FileEntry *;

	[[nodiscard]]
	static bool parseFileContents( const wsw::StringView &fileName, wsw::PodVector<char> *stringData,
								   wsw::PodVector<LineSpan> *lineEntries, wsw::PodVector<Include> *includes );

	enum ParsingIncludeResult {
		ParsingFailure,
		NoIncludes,
		HasIncludes,
	};

	[[nodiscard]]
	static auto parseIncludes( const wsw::StringView &lineToken, char *stringsBasePtr,
							   wsw::PodVector<Include> *includes ) -> ParsingIncludeResult;

	[[nodiscard]]
	auto loadBuiltin( const wsw::StringView &name ) -> KeyOfBuiltin;

	wsw::PodVector<LineSpan> m_tmpLineEntries;
	wsw::PodVector<char> m_tmpStringData;
	wsw::PodVector<Include> m_tmpIncludesData;

	FileEntry *m_fileEntriesHead { nullptr };
	FileEntry *m_fileEntryHashBins[97];
};

// Kept during the entire program lifecycle
static ProgramSourceFileCache g_programSourceFileCache;

ProgramSourceFileCache::ProgramSourceFileCache() {
	std::fill( std::begin( m_fileEntryHashBins ), std::end( m_fileEntryHashBins ), nullptr );
}

ProgramSourceFileCache::~ProgramSourceFileCache() {
	for( FileEntry *entry = m_fileEntriesHead, *nextEntry; entry; entry = nextEntry ) {
		nextEntry = entry->nextInList;
		::free( entry );
	}
}

void ProgramSourceFileCache::precacheBuiltins() {
	bool succeeded = true;

	succeeded &= ( ( deformAutoparticle  = loadBuiltin( "deformAutoparticle"_asView ) ) != 0 );
	succeeded &= ( ( deformAutosprite    = loadBuiltin( "deformAutosprite"_asView ) ) != 0 );
	succeeded &= ( ( deformAutosprite2   = loadBuiltin( "deformAutosprite2"_asView ) ) != 0 );
	succeeded &= ( ( deformPrologue      = loadBuiltin( "deformPrologue"_asView ) ) != 0 );
	succeeded &= ( ( deformEpilogue      = loadBuiltin( "deformEpilogue"_asView ) ) != 0 );

	succeeded &= ( ( constants           = loadBuiltin( "constants"_asView ) ) != 0 );
	succeeded &= ( ( instancedTransforms = loadBuiltin( "instancedTransforms"_asView ) ) != 0 );
	succeeded &= ( ( macros              = loadBuiltin( "macros"_asView ) ) != 0 );
	succeeded &= ( ( macros120           = loadBuiltin( "macros120"_asView ) ) != 0 );
	succeeded &= ( ( macros130           = loadBuiltin( "macros130"_asView ) ) != 0 );
	succeeded &= ( ( math                = loadBuiltin( "math"_asView ) ) != 0 );
	succeeded &= ( ( quatTransform       = loadBuiltin( "quatTransform"_asView ) ) != 0 );
	succeeded &= ( ( transformVerts      = loadBuiltin( "transformVerts"_asView ) ) != 0 );
	succeeded &= ( ( uniforms            = loadBuiltin( "uniforms"_asView ) ) != 0 );
	succeeded &= ( ( waveFuncs           = loadBuiltin( "waveFuncs"_asView ) ) != 0 );

	if( !succeeded ) {
		Com_Error( ERR_FATAL, "Failed to initialize GLSL builtins\n" );
	}
}

auto ProgramSourceFileCache::getFileEntryForFile( const wsw::StringView &fileName ) -> const FileEntry * {
	// This hash function is case-insensitive
	const uint32_t nameHash = wsw::getHashForLength( fileName.data(), fileName.size() );
	const uint32_t binIndex = nameHash % std::size( m_fileEntryHashBins );

	for( const FileEntry *entry = m_fileEntryHashBins[binIndex]; entry; entry = entry->nextInBin ) {
		if( entry->nameHash == nameHash && entry->getName().equalsIgnoreCase( fileName ) ) {
			return entry;
		}
	}

	if( FileEntry *const entry = loadFileEntryFromFile( fileName ) ) {
		assert( entry->getName().equals( fileName ) );
		entry->nameHash               = nameHash;
		entry->nextInBin              = m_fileEntryHashBins[binIndex];
		m_fileEntryHashBins[binIndex] = entry;
		entry->nextInList             = m_fileEntriesHead;
		m_fileEntriesHead             = entry;
		return entry;
	}

	return nullptr;
}

auto ProgramSourceFileCache::loadFileEntryFromFile( const wsw::StringView &fileName ) -> FileEntry * {
	m_tmpStringData.clear();
	m_tmpLineEntries.clear();
	m_tmpIncludesData.clear();

	bool parsingResult;
	try {
		parsingResult = parseFileContents( fileName, &m_tmpStringData, &m_tmpLineEntries, &m_tmpIncludesData );
	} catch( ... ) {
		parsingResult = false;
	}

	if( !parsingResult ) {
		return nullptr;
	}

	wsw::MemSpecBuilder memSpecBuilder { wsw::MemSpecBuilder::initiallyEmpty() };
	(void)memSpecBuilder.add<FileEntry>();

	const auto stringsSpec  = memSpecBuilder.add<char>( m_tmpStringData.size() );
	const auto linesSpec    = memSpecBuilder.add<LineSpan>( m_tmpLineEntries.size() );
	const auto includesSpec = memSpecBuilder.add<Include>( m_tmpIncludesData.size() );

	void *const mem = ::malloc( memSpecBuilder.sizeSoFar() );
	if( !mem ) {
		return nullptr;
	}

	auto *const entry = new( mem )FileEntry;
	entry->nameLength = fileName.length();

	std::copy( m_tmpStringData.begin(), m_tmpStringData.end(), stringsSpec.get( mem ) );
	entry->stringData     = stringsSpec.get( mem );
	entry->stringDataSize = m_tmpStringData.size();

	std::copy( m_tmpLineEntries.begin(), m_tmpLineEntries.end(), linesSpec.get( mem ) );
	entry->lineData = linesSpec.get( mem );
	entry->numLines = m_tmpLineEntries.size();

	std::copy( m_tmpIncludesData.begin(), m_tmpIncludesData.end(), includesSpec.get( mem ) );
	entry->includeData = includesSpec.get( mem );
	entry->numIncludes = m_tmpIncludesData.size();

	return entry;
}

// TODO: This should be shared at the top level...
static const wsw::CharLookup kLineSeparatorChars( wsw::StringView( "\r\n" ) );

bool ProgramSourceFileCache::parseFileContents( const wsw::StringView &fileName, wsw::PodVector<char> *stringData,
												wsw::PodVector<LineSpan> *lineEntries, wsw::PodVector<Include> *includes ) {
	auto maybeFileHandle = wsw::fs::openAsReadHandle( fileName );
	if( !maybeFileHandle ) {
		return false;
	}

	const size_t rawFileSize = maybeFileHandle->getInitialFileSize();
	if( rawFileSize > ( 1 << 20 ) ) {
		rError() << "Bogus source file size" << rawFileSize << "for" << fileName;
		return false;
	}

	stringData->clear();

	// Put the name first in an optimistic fashion
	stringData->insert( stringData->end(), fileName.data(), fileName.data() + fileName.size() );
	stringData->push_back( '\0' );

	const auto fileStringDataOffset = stringData->size();

	// Allocate an extra space for 2 extra characters at the file end
	stringData->resize( stringData->size() + rawFileSize + 2 );

	if( !maybeFileHandle->readExact( stringData->data() + fileStringDataOffset, rawFileSize ) ) {
		rError() << "Failed to read the source file" << fileName;
		return false;
	}

	// Get rid of comments, otherwise we fail on compiling FXAA
	const auto strippedStringDataSize = (size_t)COM_Compress( stringData->data() + fileStringDataOffset );
	// Let the freed space be used for further additions
	stringData->resize( fileStringDataOffset + strippedStringDataSize + 2 );

	// Ensure the trailing \n for GLSL (the file could miss it)
	stringData->operator[]( fileStringDataOffset + strippedStringDataSize + 0 ) = '\n';
	// Ensure that the read data is zero-terminated
	stringData->operator[]( fileStringDataOffset + strippedStringDataSize + 1 ) = '\0';

	const size_t bomShift = startsWithUtf8Bom( stringData->data() + fileStringDataOffset, strippedStringDataSize ) ? 3 : 0;
	wsw::StringSplitter lineSplitter( { stringData->data() + fileStringDataOffset + bomShift, strippedStringDataSize } );
	while( const std::optional<wsw::StringView> &maybeLineToken = lineSplitter.getNext( kLineSeparatorChars ) ) {
		const wsw::StringView lineToken = *maybeLineToken;
		const unsigned oldIncludesSize  = includes->size();

		const ParsingIncludeResult parseIncludeResult = parseIncludes( lineToken, stringData->data(), includes );
		if( parseIncludeResult == ParsingFailure ) {
			return false;
		}
		if( parseIncludeResult == HasIncludes ) {
			assert( includes->size() == oldIncludesSize + 1 );
			lineEntries->push_back( oldIncludesSize );
			continue;
		}

		assert( lineToken.data() >= stringData->data() );
		assert( lineToken.data() < stringData->data() + stringData->size() );
		const ptrdiff_t offset = lineToken.data() - stringData->data();
		assert( (size_t)offset < stringData->size() );

		// Convert CR to LF
		if( stringData->operator[]( offset + lineToken.length() ) == '\r') {
			stringData->operator[]( offset + lineToken.length() ) = '\n';
		}

		assert( stringData->operator[]( offset + lineToken.length() ) == '\n' );
		lineEntries->push_back( Span { .offset = (unsigned)offset, .length = (unsigned)( lineToken.length() + 1 ) } );
	}

	return true;
}

static void sanitizeFilePath( char *chars, unsigned offset, unsigned length ) {
	for( unsigned i = offset; i < offset + length; ++i ) {
		if( chars[i] == '\\' ) {
			chars[i] = '/';
		}
	}
}

auto ProgramSourceFileCache::parseIncludes( const wsw::StringView &lineToken, char *stringsBasePtr,
											wsw::PodVector<Include> *includes ) -> ParsingIncludeResult {
	wsw::StringView lineLeftover = lineToken.trimLeft();
	if( !lineLeftover.startsWith( '#' ) ) {
		return NoIncludes;
	}

	const auto isATokenCharacter = []( char ch ) {
		return !isspace( ch ) && ch != '(' && ch != ')';
	};
	const auto isAFileNameCharacter = []( char ch ) {
		return !isspace( ch ) && ch != '"';
	};

	const wsw::StringView firstLineToken  = lineLeftover.takeWhile(isATokenCharacter);

	bool hasACondition = false;
	if( !firstLineToken.equalsIgnoreCase( "#include"_asView ) ) {
		if( !firstLineToken.equalsIgnoreCase( "#include_if"_asView ) ) {
			return NoIncludes;
		}
		hasACondition = true;
	}

	lineLeftover = lineLeftover.drop( firstLineToken.size() ).trimLeft();

	wsw::StringView condition;
	if( hasACondition ) {
		if( !lineLeftover.startsWith( '(' ) ) {
			return ParsingFailure;
		}
		lineLeftover = lineLeftover.drop( 1 ).trimLeft();

		condition = lineLeftover.takeWhile( isATokenCharacter );
		if( condition.empty() ) {
			return ParsingFailure;
		}

		lineLeftover = lineLeftover.drop( condition.size() ).trimLeft();
		if( !lineLeftover.startsWith( ')' ) ) {
			return ParsingFailure;
		}
		lineLeftover = lineLeftover.drop( 1 ).trimLeft();
	}

	if( !lineLeftover.startsWith( '"' ) ) {
		return ParsingFailure;
	}
	lineLeftover = lineLeftover.drop( 1 );

	const wsw::StringView &fileName = lineLeftover.takeWhile( isAFileNameCharacter );
	if( fileName.empty() ) {
		return ParsingFailure;
	}

	lineLeftover = lineLeftover.drop( fileName.length() );

	if( !lineLeftover.startsWith( '"' ) ) {
		return ParsingFailure;
	}

	lineLeftover = lineLeftover.drop( 1 ).trim();
	if( !lineLeftover.empty() ) {
		return ParsingFailure;
	}

	const Span fileNameSpan { (unsigned)( fileName.data() - stringsBasePtr ), (unsigned)fileName.size() };
	::sanitizeFilePath( stringsBasePtr, fileNameSpan.offset, fileNameSpan.length );

	if( hasACondition ) {
		const Span conditionSpan { (unsigned)( condition.data() - stringsBasePtr ), (unsigned)condition.size() };
		::sanitizeFilePath( stringsBasePtr, fileNameSpan.offset, fileNameSpan.length );
		includes->emplace_back( ConditionalInclude { .fileNameSpan = fileNameSpan, .conditionSpan = conditionSpan } );
	} else {
		includes->emplace_back( RegularInclude { .fileNameSpan = fileNameSpan } );
	}

	return HasIncludes;
}

auto ProgramSourceFileCache::loadBuiltin( const wsw::StringView &name ) -> KeyOfBuiltin {
	wsw::StaticString<MAX_QPATH> filePath;
	filePath << "glsl/builtin/"_asView << name << ".glsl"_asView;

	return (KeyOfBuiltin)getFileEntryForFile( filePath.asView() );
}

class ProgramSourceLoader {
public:
	ProgramSourceLoader( ProgramSourceFileCache *cache, wsw::PodVector<const char *> *lines, wsw::PodVector<int> *lengths )
		: m_sourceFileCache( cache ), m_lines( lines ), m_lengths( lengths ) {}

	[[nodiscard]]
	bool load( const wsw::StringView &rootFileName, uint64_t features, unsigned programType );

private:
	[[nodiscard]]
	bool loadRecursively( const wsw::StringView &fileName, int depth );

	[[nodiscard]]
	auto getRootFileDir() const -> wsw::StringView;

	[[nodiscard]]
	bool checkIncludeCondition( const wsw::StringView &condition ) const;

	ProgramSourceFileCache *const m_sourceFileCache;

	wsw::PodVector<const char *> *const m_lines;
	wsw::PodVector<int> *const m_lengths;

	mutable wsw::StaticString<64> m_cachedRootFileDir;
	wsw::StringView m_rootFileName;
	uint64_t m_features { 0 };
	unsigned m_programType { 0 };
};

bool ProgramSourceLoader::load( const wsw::StringView &rootFileName, uint64_t features, unsigned programType ) {
	assert( m_lines->size() == m_lengths->size() );
	const auto oldSize = m_lines->size();

	m_rootFileName = rootFileName;
	m_cachedRootFileDir.clear();

	m_features    = features;
	m_programType = programType;

	const bool loadResult = loadRecursively( rootFileName, 1 );
	if( !loadResult ) {
		m_lines->resize( oldSize );
		m_lengths->resize( oldSize );
	}

	assert( m_lines->size() == m_lengths->size() );
	return loadResult;
}

bool ProgramSourceLoader::loadRecursively( const wsw::StringView &fileName, int depth ) {
	if( const ProgramSourceFileCache::FileEntry *fileEntry = m_sourceFileCache->getFileEntryForFile( fileName ) ) {
		for( unsigned lineIndex = 0; lineIndex < fileEntry->numLines; ++lineIndex ) {
			const ProgramSourceFileCache::LineSpan &spanForLine = fileEntry->lineData[lineIndex];
			if( const auto *regularLineSpan = std::get_if<ProgramSourceFileCache::Span>( &spanForLine ) ) [[likely]] {
				m_lines->push_back( fileEntry->stringData + regularLineSpan->offset );
				m_lengths->push_back( (int)regularLineSpan->length );
				continue;
			}

			if( depth + 1 >= 16 ) {
				rWarning() << "Too many nested includes";
				return false;
			}

			const auto *includeIndex = std::get_if<unsigned>( &spanForLine );
			const ProgramSourceFileCache::Include &include = fileEntry->includeData[*includeIndex];
			ProgramSourceFileCache::Span includeFileNameSpan;

			// std::variant interface sucks
			if( const auto *conditional = std::get_if<ProgramSourceFileCache::ConditionalInclude>( &include ) ) {
				if( !checkIncludeCondition( fileEntry->getStringForSpan( conditional->conditionSpan ) ) ) {
					continue;
				}
				includeFileNameSpan  = conditional->fileNameSpan;
			} else if( const auto *regular = std::get_if<ProgramSourceFileCache::RegularInclude>( &include ) ) {
				includeFileNameSpan = regular->fileNameSpan;
			}

			const wsw::StringView &includeFileName = fileEntry->getStringForSpan( includeFileNameSpan );

			// Prepare the file name
			wsw::StaticString<MAX_QPATH> buffer;
			if( includeFileName.startsWith( '/' ) ) {
				buffer << getRootFileDir();
				assert( buffer.endsWith( '/' ) );
				buffer << includeFileName;
			} else {
				buffer << fileName;
				if( const auto maybeLastSlashIndex = buffer.lastIndexOf( '/' ) ) {
					buffer.erase( *maybeLastSlashIndex + 1 );
				}
				buffer << includeFileName;
			}

			if( !loadRecursively( buffer.asView(), depth + 1 ) ) {
				return false;
			}
		}

		return true;
	}

	return false;
}

auto ProgramSourceLoader::getRootFileDir() const -> wsw::StringView {
	if( m_cachedRootFileDir.empty() ) {
		assert( !m_rootFileName.empty() );
		wsw::StringView dir = m_rootFileName;
		if( const auto maybeLastSlashIndex = m_rootFileName.lastIndexOf( '/' ) ) {
			dir = m_rootFileName.take( *maybeLastSlashIndex + 1 );
		}
		m_cachedRootFileDir.assign( dir );
	}
	return m_cachedRootFileDir.asView();
}

// TODO: Should decision-making be performed at this level?
bool ProgramSourceLoader::checkIncludeCondition( const wsw::StringView &condition ) const {
	if( ( m_features & GLSL_SHADER_COMMON_FOG ) && condition.equalsIgnoreCase( "APPLY_FOG"_asView ) ) {
		return true;
	}
	if( ( m_features & GLSL_SHADER_COMMON_DLIGHTS ) && condition.equalsIgnoreCase( "NUM_DLIGHTS"_asView ) ) {
		return true;
	}
	if( ( m_features & GLSL_SHADER_COMMON_GREYSCALE ) && condition.equalsIgnoreCase( "APPLY_GREYSCALE"_asView ) ) {
		return true;
	}
	if( m_programType == GLSL_PROGRAM_TYPE_Q3A_SHADER ) {
		if( ( m_features & GLSL_SHADER_Q3_LIGHTSTYLE ) && condition.equalsIgnoreCase( "NUM_LIGHTMAPS"_asView ) ) {
			return true;
		}
	}
	if( m_programType == GLSL_PROGRAM_TYPE_MATERIAL ) {
		if( ( m_features & GLSL_SHADER_MATERIAL_LIGHTSTYLE ) ) {
			if( condition.equalsIgnoreCase( "NUM_LIGHTMAPS"_asView ) ) {
				return true;
			}
		}
		if( ( m_features & ( GLSL_SHADER_MATERIAL_OFFSETMAPPING | GLSL_SHADER_MATERIAL_RELIEFMAPPING ) ) ) {
			if( condition.equalsIgnoreCase( "APPLY_OFFSETMAPPING"_asView ) ) {
				return true;
			}
		}
		if( ( m_features & GLSL_SHADER_MATERIAL_CELSHADING ) ) {
			if( condition.equalsIgnoreCase( "APPLY_CELSHADING"_asView ) ) {
				return true;
			}
		}
		if( ( m_features & GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT ) ) {
			if( condition.equalsIgnoreCase( "APPLY_DIRECTIONAL_LIGHT"_asView ) ) {
				return true;
			}
		}
	}
	return false;
}

#define MAX_GLSL_PROGRAMS           1024
#define GLSL_PROGRAMS_HASH_SIZE     256

class ProgramSourceBuilder;

class ShaderProgramCache {
	template <typename> friend class SingletonHolder;
public:
	[[nodiscard]]
	auto getProgramForParams( int type, const wsw::StringView &maybeRequestedName, uint64_t features = 0,
							  const DeformSig &deformSig = DeformSig(),
							  std::span<const deformv_t> deforms = {} ) -> int;

	[[nodiscard]]
	auto getProgramForParams( int type, const char *maybeName, uint64_t features = 0,
							  const DeformSig &deformSig = DeformSig(),
							  std::span<const deformv_t> deforms = {} ) -> int {
		return getProgramForParams( type, wsw::StringView( maybeName ? maybeName : "" ), features, deformSig, deforms );
	}

	[[nodiscard]]
	auto getProgramById( int id ) -> ShaderProgram * {
		assert( id > 0 && id < MAX_GLSL_PROGRAMS + 1 );
		return m_programForIndex[id - 1];
	}
private:
	static constexpr unsigned kExtraTrailingProgramBytesSize = 16;
	static constexpr unsigned kAllocatorNameLengthLimit      = 32;

	ShaderProgramCache();
	~ShaderProgramCache();

	[[nodiscard]]
	auto createProgramFromSource( const wsw::StringView &name, int type, uint64_t features, std::span<const deformv_t> deforms )
		-> std::optional<std::tuple<GLuint, GLuint, GLuint>>;

	static void destroyProgramObjects( GLuint programId, GLuint vertexShaderId, GLuint fragmentShaderId );

	[[nodiscard]]
	bool loadShaderSources( const wsw::StringView &name, int type, uint64_t features, std::span<const deformv_t> deforms,
							GLuint vertexShaderId, GLuint fragmentShaderId );

	void loadSourceOfFeatures( int type, uint64_t features, ProgramSourceBuilder *sourceBuilder );

	struct SpanOfSourceStrings {
		std::span<const char *> strings;
		std::span<int> lengths;
	};

	[[nodiscard]]
	bool loadVertexShaderSource( GLuint id, const wsw::StringView &name, int type, uint64_t features,
								 std::span<const deformv_t> deforms,
								 const SpanOfSourceStrings &featureStrings,
								 const wsw::StringView &version, const wsw::StringView &maxBones );

	[[nodiscard]]
	bool loadFragmentShaderSource( GLuint id, const wsw::StringView &name, int type, uint64_t features,
								   const SpanOfSourceStrings &featureStrings,
								   const wsw::StringView &version, const wsw::StringView &maxBones );

	[[nodiscard]]
	bool compileShader( GLuint id, const char *kind, std::span<const char *> strings, std::span<int> lengths );

	[[nodiscard]]
	bool bindAttributeLocations( GLuint programId );

	[[nodiscard]]
	bool linkProgram( GLuint programId, GLuint vertexShaderId, GLuint fragmentShaderId );
	
	static void setupUniformsAndLocations( ShaderProgram *program );

	wsw::HeapBasedFreelistAllocator m_programsAllocator;
	wsw::HeapBasedFreelistAllocator m_namesAllocator;

	wsw::PodVector<const char *> m_tmpCommonStrings;
	wsw::PodVector<int> m_tmpCommonOffsets;

	wsw::PodVector<const char *> m_tmpShaderStrings;
	wsw::PodVector<int> m_tmpShaderLengths;

	ShaderProgram *m_programListHead { nullptr };
	ShaderProgram *m_programForIndex[MAX_GLSL_PROGRAMS];
	ShaderProgram *m_hashBinsForType[GLSL_PROGRAM_TYPE_MAXTYPE][GLSL_PROGRAMS_HASH_SIZE];
};

static SingletonHolder<ShaderProgramCache> g_programCacheInstanceHolder;
static bool g_programCacheInstanceHolderInitialized;

void RP_Init() {
	if( !g_programCacheInstanceHolderInitialized ) {
		g_programSourceFileCache.precacheBuiltins();
		g_programCacheInstanceHolder.init();
		g_programCacheInstanceHolderInitialized = true;
	}
}

void RP_Shutdown() {
	assert( g_programCacheInstanceHolderInitialized );
	qglUseProgram( 0 );
	g_programCacheInstanceHolder.shutdown();
	g_programCacheInstanceHolderInitialized = false;
}

ShaderProgramCache::ShaderProgramCache()
	: m_programsAllocator( sizeof( ShaderProgram ) + kExtraTrailingProgramBytesSize, MAX_GLSL_PROGRAMS )
	, m_namesAllocator( kAllocatorNameLengthLimit + 1, MAX_GLSL_PROGRAMS ) {
	// TODO: The allocators initialization is not exception-safe

	std::memset( m_programForIndex, 0, sizeof( m_programForIndex ) );
	std::memset( m_hashBinsForType, 0, sizeof( m_hashBinsForType ) );

	// Register basic programs

	bool succeeded = true;
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_MATERIAL, DEFAULT_GLSL_MATERIAL_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_MATERIAL, DEFAULT_GLSL_MATERIAL_PROGRAM, GLSL_SHADER_COMMON_BONE_TRANSFORMS1 );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_DISTORTION, DEFAULT_GLSL_DISTORTION_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_RGB_SHADOW, DEFAULT_GLSL_RGB_SHADOW_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_SHADOWMAP, DEFAULT_GLSL_SHADOWMAP_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_OUTLINE, DEFAULT_GLSL_OUTLINE_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_Q3A_SHADER, DEFAULT_GLSL_Q3A_SHADER_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_CELSHADE, DEFAULT_GLSL_CELSHADE_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_FOG, DEFAULT_GLSL_FOG_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_FXAA, DEFAULT_GLSL_FXAA_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_YUV, DEFAULT_GLSL_YUV_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_COLOR_CORRECTION, DEFAULT_GLSL_COLORCORRECTION_PROGRAM );
	succeeded &= (bool)getProgramForParams( GLSL_PROGRAM_TYPE_KAWASE_BLUR, DEFAULT_GLSL_KAWASE_BLUR_PROGRAM );

	if( !succeeded ) {
		Com_Error( ERR_FATAL, "Failed to precache basic GLSL programs\n" );
	}
}

ShaderProgramCache::~ShaderProgramCache() {
	for( ShaderProgram *program = m_programListHead, *next = nullptr; program; program = next ) {
		next = program->nextInList;

		destroyProgramObjects( program->programId, program->vertexShaderId, program->fragmentShaderId );

		if( program->deformSigDataToFree ) {
			Q_free( program->deformSigDataToFree );
		}
		if( program->nameDataToFree ) {
			if( m_namesAllocator.mayOwn( program->nameDataToFree ) ) {
				m_namesAllocator.free( program->nameDataToFree );
			} else {
				Q_free( program->nameDataToFree );
			}
		}

		m_programsAllocator.free( program );
	}
}

static const glsl_feature_t glsl_features_empty[] =
{
	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_material[] =
{
	{ GLSL_SHADER_COMMON_GREYSCALE, "#define APPLY_GREYSCALE\n", "_grey" },

	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX, "#define APPLY_RGB_ONE_MINUS_VERTEX\n", "_c1-v" },
	{ GLSL_SHADER_COMMON_RGB_GEN_CONST, "#define APPLY_RGB_CONST\n", "_cc" },
	{ GLSL_SHADER_COMMON_RGB_GEN_VERTEX, "#define APPLY_RGB_VERTEX\n", "_cv" },
	{ GLSL_SHADER_COMMON_RGB_DISTANCERAMP, "#define APPLY_RGB_DISTANCERAMP\n", "_rgb_dr" },

	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX, "#define APPLY_ALPHA_ONE_MINUS_VERTEX\n", "_a1-v" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX, "#define APPLY_ALPHA_VERTEX\n", "_av" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_CONST, "#define APPLY_ALPHA_CONST\n", "_ac" },
	{ GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP, "#define APPLY_ALPHA_DISTANCERAMP\n", "_alpha_dr" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n#define APPLY_FOG_IN 1\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_COMMON_DLIGHTS_16, "#define NUM_DLIGHTS 16\n", "_dl16" },
	{ GLSL_SHADER_COMMON_DLIGHTS_12, "#define NUM_DLIGHTS 12\n", "_dl12" },
	{ GLSL_SHADER_COMMON_DLIGHTS_8, "#define NUM_DLIGHTS 8\n", "_dl8" },
	{ GLSL_SHADER_COMMON_DLIGHTS_4, "#define NUM_DLIGHTS 4\n", "_dl4" },

	{ GLSL_SHADER_COMMON_DRAWFLAT, "#define APPLY_DRAWFLAT\n", "_flat" },

	{ GLSL_SHADER_COMMON_AUTOSPRITE, "#define APPLY_AUTOSPRITE\n", "" },
	{ GLSL_SHADER_COMMON_AUTOSPRITE2, "#define APPLY_AUTOSPRITE2\n", "" },
	{ GLSL_SHADER_COMMON_AUTOPARTICLE, "#define APPLY_AUTOSPRITE\n#define APPLY_AUTOPARTICLE\n", "" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n"
	  "#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_COMMON_AFUNC_GE128, "#define QF_ALPHATEST(a) { if ((a) < 0.5) discard; }\n", "_afunc_ge128" },
	{ GLSL_SHADER_COMMON_AFUNC_LT128, "#define QF_ALPHATEST(a) { if ((a) >= 0.5) discard; }\n", "_afunc_lt128" },
	{ GLSL_SHADER_COMMON_AFUNC_GT0, "#define QF_ALPHATEST(a) { if ((a) <= 0.0) discard; }\n", "_afunc_gt0" },

	{ GLSL_SHADER_COMMON_TC_MOD, "#define APPLY_TC_MOD\n", "_tc_mod" },

	{ GLSL_SHADER_MATERIAL_LIGHTSTYLE3, "#define NUM_LIGHTMAPS 4\n#define qf_lmvec01 vec4\n#define qf_lmvec23 vec4\n", "_ls3" },
	{ GLSL_SHADER_MATERIAL_LIGHTSTYLE2, "#define NUM_LIGHTMAPS 3\n#define qf_lmvec01 vec4\n#define qf_lmvec23 vec2\n", "_ls2" },
	{ GLSL_SHADER_MATERIAL_LIGHTSTYLE1, "#define NUM_LIGHTMAPS 2\n#define qf_lmvec01 vec4\n", "_ls1" },
	{ GLSL_SHADER_MATERIAL_LIGHTSTYLE0, "#define NUM_LIGHTMAPS 1\n#define qf_lmvec01 vec2\n", "_ls0" },
	{ GLSL_SHADER_MATERIAL_LIGHTMAP_ARRAYS, "#define LIGHTMAP_ARRAYS\n", "_lmarray" },
	{ GLSL_SHADER_MATERIAL_FB_LIGHTMAP, "#define APPLY_FBLIGHTMAP\n", "_fb" },
	{ GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT, "#define APPLY_DIRECTIONAL_LIGHT\n", "_dirlight" },

	{ GLSL_SHADER_MATERIAL_SPECULAR, "#define APPLY_SPECULAR\n", "_gloss" },
	{ GLSL_SHADER_MATERIAL_OFFSETMAPPING, "#define APPLY_OFFSETMAPPING\n", "_offmap" },
	{ GLSL_SHADER_MATERIAL_RELIEFMAPPING, "#define APPLY_RELIEFMAPPING\n", "_relmap" },
	{ GLSL_SHADER_MATERIAL_AMBIENT_COMPENSATION, "#define APPLY_AMBIENT_COMPENSATION\n", "_amb" },
	{ GLSL_SHADER_MATERIAL_DECAL, "#define APPLY_DECAL\n", "_decal" },
	{ GLSL_SHADER_MATERIAL_DECAL_ADD, "#define APPLY_DECAL_ADD\n", "_add" },
	{ GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY, "#define APPLY_BASETEX_ALPHA_ONLY\n", "_alpha" },
	{ GLSL_SHADER_MATERIAL_CELSHADING, "#define APPLY_CELSHADING\n", "_cel" },
	{ GLSL_SHADER_MATERIAL_HALFLAMBERT, "#define APPLY_HALFLAMBERT\n", "_lambert" },

	{ GLSL_SHADER_MATERIAL_ENTITY_DECAL, "#define APPLY_ENTITY_DECAL\n", "_decal2" },
	{ GLSL_SHADER_MATERIAL_ENTITY_DECAL_ADD, "#define APPLY_ENTITY_DECAL_ADD\n", "_decal2_add" },

	// doesn't make sense without APPLY_DIRECTIONAL_LIGHT
	{ GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_MIX, "#define APPLY_DIRECTIONAL_LIGHT_MIX\n", "_mix" },
	{ GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_FROM_NORMAL, "#define APPLY_DIRECTIONAL_LIGHT_FROM_NORMAL\n", "_normlight" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_distortion[] =
{
	{ GLSL_SHADER_COMMON_GREYSCALE, "#define APPLY_GREYSCALE\n", "_grey" },

	{ GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX, "#define APPLY_RGB_ONE_MINUS_VERTEX\n", "_c1-v" },
	{ GLSL_SHADER_COMMON_RGB_GEN_CONST, "#define APPLY_RGB_CONST\n", "_cc" },
	{ GLSL_SHADER_COMMON_RGB_GEN_VERTEX, "#define APPLY_RGB_VERTEX\n", "_cv" },
	{ GLSL_SHADER_COMMON_RGB_DISTANCERAMP, "#define APPLY_RGB_DISTANCERAMP\n", "_rgb_dr" },

	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX, "#define APPLY_ALPHA_ONE_MINUS_VERTEX\n", "_a1-v" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_CONST, "#define APPLY_ALPHA_CONST\n", "_ac" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX, "#define APPLY_ALPHA_VERTEX\n", "_av" },
	{ GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP, "#define APPLY_ALPHA_DISTANCERAMP\n", "_alpha_dr" },

	{ GLSL_SHADER_COMMON_AUTOSPRITE, "#define APPLY_AUTOSPRITE\n", "" },
	{ GLSL_SHADER_COMMON_AUTOSPRITE2, "#define APPLY_AUTOSPRITE2\n", "" },
	{ GLSL_SHADER_COMMON_AUTOPARTICLE, "#define APPLY_AUTOSPRITE\n#define APPLY_AUTOPARTICLE\n", "" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n"
	  "#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n#define APPLY_FOG_IN 1\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_DISTORTION_DUDV, "#define APPLY_DUDV\n", "_dudv" },
	{ GLSL_SHADER_DISTORTION_EYEDOT, "#define APPLY_EYEDOT\n", "_eyedot" },
	{ GLSL_SHADER_DISTORTION_DISTORTION_ALPHA, "#define APPLY_DISTORTION_ALPHA\n", "_alpha" },
	{ GLSL_SHADER_DISTORTION_REFLECTION, "#define APPLY_REFLECTION\n", "_refl" },
	{ GLSL_SHADER_DISTORTION_REFRACTION, "#define APPLY_REFRACTION\n", "_refr" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_rgbshadow[] =
{
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_RGBSHADOW_24BIT, "#define APPLY_RGB_SHADOW_24BIT\n", "_rgb24" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_shadowmap[] =
{
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_outline[] =
{
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_RGB_GEN_CONST, "#define APPLY_RGB_CONST\n", "_cc" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_CONST, "#define APPLY_ALPHA_CONST\n", "_ac" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n#define APPLY_FOG_IN 1\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_OUTLINE_OUTLINES_CUTOFF, "#define APPLY_OUTLINES_CUTOFF\n", "_outcut" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_q3a[] =
{
	{ GLSL_SHADER_COMMON_GREYSCALE, "#define APPLY_GREYSCALE\n", "_grey" },

	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX, "#define APPLY_RGB_ONE_MINUS_VERTEX\n", "_c1-v" },
	{ GLSL_SHADER_COMMON_RGB_GEN_CONST, "#define APPLY_RGB_CONST\n", "_cc" },
	{ GLSL_SHADER_COMMON_RGB_GEN_VERTEX, "#define APPLY_RGB_VERTEX\n", "_cv" },
	{ GLSL_SHADER_COMMON_RGB_DISTANCERAMP, "#define APPLY_RGB_DISTANCERAMP\n", "_rgb_dr" },

	{ GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX, "#define APPLY_ALPHA_ONE_MINUS_VERTEX\n", "_a1-v" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_CONST, "#define APPLY_ALPHA_CONST\n", "_ac" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX, "#define APPLY_ALPHA_VERTEX\n", "_av" },
	{ GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP, "#define APPLY_ALPHA_DISTANCERAMP\n", "_alpha_dr" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n#define APPLY_FOG_IN 1\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_COMMON_DLIGHTS_16, "#define NUM_DLIGHTS 16\n", "_dl16" },
	{ GLSL_SHADER_COMMON_DLIGHTS_12, "#define NUM_DLIGHTS 12\n", "_dl12" },
	{ GLSL_SHADER_COMMON_DLIGHTS_8, "#define NUM_DLIGHTS 8\n", "_dl8" },
	{ GLSL_SHADER_COMMON_DLIGHTS_4, "#define NUM_DLIGHTS 4\n", "_dl4" },

	{ GLSL_SHADER_COMMON_DRAWFLAT, "#define APPLY_DRAWFLAT\n", "_flat" },

	{ GLSL_SHADER_COMMON_AUTOSPRITE, "#define APPLY_AUTOSPRITE\n", "" },
	{ GLSL_SHADER_COMMON_AUTOSPRITE2, "#define APPLY_AUTOSPRITE2\n", "" },
	{ GLSL_SHADER_COMMON_AUTOPARTICLE, "#define APPLY_AUTOSPRITE\n#define APPLY_AUTOPARTICLE\n", "" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_COMMON_SOFT_PARTICLE, "#define APPLY_SOFT_PARTICLE\n", "_sp" },

	{ GLSL_SHADER_COMMON_AFUNC_GE128, "#define QF_ALPHATEST(a) { if ((a) < 0.5) discard; }\n", "_afunc_ge128" },
	{ GLSL_SHADER_COMMON_AFUNC_LT128, "#define QF_ALPHATEST(a) { if ((a) >= 0.5) discard; }\n", "_afunc_lt128" },
	{ GLSL_SHADER_COMMON_AFUNC_GT0, "#define QF_ALPHATEST(a) { if ((a) <= 0.0) discard; }\n", "_afunc_gt0" },

	{ GLSL_SHADER_COMMON_TC_MOD, "#define APPLY_TC_MOD\n", "_tc_mod" },

	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_Q3_TC_GEN_CELSHADE, "#define APPLY_TC_GEN_CELSHADE\n", "_tc_cel" },
	{ GLSL_SHADER_Q3_TC_GEN_PROJECTION, "#define APPLY_TC_GEN_PROJECTION\n", "_tc_proj" },
	{ GLSL_SHADER_Q3_TC_GEN_REFLECTION, "#define APPLY_TC_GEN_REFLECTION\n", "_tc_refl" },
	{ GLSL_SHADER_Q3_TC_GEN_ENV, "#define APPLY_TC_GEN_ENV\n", "_tc_env" },
	{ GLSL_SHADER_Q3_TC_GEN_VECTOR, "#define APPLY_TC_GEN_VECTOR\n", "_tc_vec" },
	{ GLSL_SHADER_Q3_TC_GEN_SURROUND, "#define APPLY_TC_GEN_SURROUND\n", "_tc_surr" },

	{ GLSL_SHADER_Q3_LIGHTSTYLE3, "#define NUM_LIGHTMAPS 4\n#define qf_lmvec01 vec4\n#define qf_lmvec23 vec4\n", "_ls3" },
	{ GLSL_SHADER_Q3_LIGHTSTYLE2, "#define NUM_LIGHTMAPS 3\n#define qf_lmvec01 vec4\n#define qf_lmvec23 vec2\n", "_ls2" },
	{ GLSL_SHADER_Q3_LIGHTSTYLE1, "#define NUM_LIGHTMAPS 2\n#define qf_lmvec01 vec4\n", "_ls1" },
	{ GLSL_SHADER_Q3_LIGHTSTYLE0, "#define NUM_LIGHTMAPS 1\n#define qf_lmvec01 vec2\n", "_ls0" },

	{ GLSL_SHADER_Q3_LIGHTMAP_ARRAYS, "#define LIGHTMAP_ARRAYS\n", "_lmarray" },

	{ GLSL_SHADER_Q3_ALPHA_MASK, "#define APPLY_ALPHA_MASK\n", "_alpha_mask" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_celshade[] =
{
	{ GLSL_SHADER_COMMON_GREYSCALE, "#define APPLY_GREYSCALE\n", "_grey" },

	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_AUTOSPRITE, "#define APPLY_AUTOSPRITE\n", "" },
	{ GLSL_SHADER_COMMON_AUTOSPRITE2, "#define APPLY_AUTOSPRITE2\n", "" },
	{ GLSL_SHADER_COMMON_AUTOPARTICLE, "#define APPLY_AUTOSPRITE\n#define APPLY_AUTOPARTICLE\n", "" },

	{ GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX, "#define APPLY_RGB_ONE_MINUS_VERTEX\n", "_c1-v" },
	{ GLSL_SHADER_COMMON_RGB_GEN_CONST, "#define APPLY_RGB_CONST\n", "_cc" },
	{ GLSL_SHADER_COMMON_RGB_GEN_VERTEX, "#define APPLY_RGB_VERTEX\n", "_cv" },

	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX, "#define APPLY_ALPHA_ONE_MINUS_VERTEX\n", "_a1-v" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX, "#define APPLY_ALPHA_VERTEX\n", "_av" },
	{ GLSL_SHADER_COMMON_ALPHA_GEN_CONST, "#define APPLY_ALPHA_CONST\n", "_ac" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n#define APPLY_FOG_IN 1\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ GLSL_SHADER_COMMON_AFUNC_GE128, "#define QF_ALPHATEST(a) { if ((a) < 0.5) discard; }\n", "_afunc_ge128" },
	{ GLSL_SHADER_COMMON_AFUNC_LT128, "#define QF_ALPHATEST(a) { if ((a) >= 0.5) discard; }\n", "_afunc_lt128" },
	{ GLSL_SHADER_COMMON_AFUNC_GT0, "#define QF_ALPHATEST(a) { if ((a) <= 0.0) discard; }\n", "_afunc_gt0" },

	{ GLSL_SHADER_CELSHADE_DIFFUSE, "#define APPLY_DIFFUSE\n", "_diff" },
	{ GLSL_SHADER_CELSHADE_DECAL, "#define APPLY_DECAL\n", "_decal" },
	{ GLSL_SHADER_CELSHADE_DECAL_ADD, "#define APPLY_DECAL_ADD\n", "_decal" },
	{ GLSL_SHADER_CELSHADE_ENTITY_DECAL, "#define APPLY_ENTITY_DECAL\n", "_edecal" },
	{ GLSL_SHADER_CELSHADE_ENTITY_DECAL_ADD, "#define APPLY_ENTITY_DECAL_ADD\n", "_add" },
	{ GLSL_SHADER_CELSHADE_STRIPES, "#define APPLY_STRIPES\n", "_stripes" },
	{ GLSL_SHADER_CELSHADE_STRIPES_ADD, "#define APPLY_STRIPES_ADD\n", "_stripes_add" },
	{ GLSL_SHADER_CELSHADE_CEL_LIGHT, "#define APPLY_CEL_LIGHT\n", "_light" },
	{ GLSL_SHADER_CELSHADE_CEL_LIGHT_ADD, "#define APPLY_CEL_LIGHT_ADD\n", "_add" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_fog[] =
{
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS4, "#define QF_NUM_BONE_INFLUENCES 4\n", "_bones4" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS3, "#define QF_NUM_BONE_INFLUENCES 3\n", "_bones3" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS2, "#define QF_NUM_BONE_INFLUENCES 2\n", "_bones2" },
	{ GLSL_SHADER_COMMON_BONE_TRANSFORMS1, "#define QF_NUM_BONE_INFLUENCES 1\n", "_bones1" },

	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_COMMON_FOG, "#define APPLY_FOG\n", "_fog" },
	{ GLSL_SHADER_COMMON_FOG_RGB, "#define APPLY_FOG_COLOR\n", "_rgb" },

	{ GLSL_SHADER_COMMON_AUTOSPRITE, "#define APPLY_AUTOSPRITE\n", "" },
	{ GLSL_SHADER_COMMON_AUTOSPRITE2, "#define APPLY_AUTOSPRITE2\n", "" },
	{ GLSL_SHADER_COMMON_AUTOPARTICLE, "#define APPLY_AUTOSPRITE\n#define APPLY_AUTOPARTICLE\n", "" },

	{ GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n", "_instanced" },
	{ GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS, "#define APPLY_INSTANCED_TRANSFORMS\n#define APPLY_INSTANCED_ATTRIB_TRANSFORMS\n", "_instanced_va" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_fxaa[] =
{
	{ GLSL_SHADER_FXAA_FXAA3, "#define APPLY_FXAA3\n", "_fxaa3" },

	{ 0, NULL, NULL }
};

static const glsl_feature_t glsl_features_colcorrection[] =
{
	{ GLSL_SHADER_COMMON_SRGB2LINEAR, "#define APPLY_SRGB2LINEAR\n", "_srgb" },
	{ GLSL_SHADER_COMMON_LINEAR2SRB, "#define APPLY_LINEAR2SRGB\n", "_linear" },

	{ GLSL_SHADER_COLOR_CORRECTION_LUT, "#define APPLY_LUT\n", "_lut" },
	{ GLSL_SHADER_COLOR_CORRECTION_HDR, "#define APPLY_HDR\n", "_hdr" },
	{ GLSL_SHADER_COLOR_CORRECTION_OVERBRIGHT, "#define APPLY_OVEBRIGHT\n", "_obloom" },
	{ GLSL_SHADER_COLOR_CORRECTION_BLOOM, "#define APPLY_BLOOM\n", "_bloom" },

	{ 0, NULL, NULL }
};


static const glsl_feature_t * const glsl_programtypes_features[] =
{
	// GLSL_PROGRAM_TYPE_NONE
	NULL,
	// GLSL_PROGRAM_TYPE_MATERIAL
	glsl_features_material,
	// GLSL_PROGRAM_TYPE_DISTORTION
	glsl_features_distortion,
	// GLSL_PROGRAM_TYPE_RGB_SHADOW
	glsl_features_rgbshadow,
	// GLSL_PROGRAM_TYPE_SHADOWMAP
	glsl_features_shadowmap,
	// GLSL_PROGRAM_TYPE_OUTLINE
	glsl_features_outline,
	// GLSL_PROGRAM_TYPE_UNUSED
	glsl_features_empty,
	// GLSL_PROGRAM_TYPE_Q3A_SHADER
	glsl_features_q3a,
	// GLSL_PROGRAM_TYPE_CELSHADE
	glsl_features_celshade,
	// GLSL_PROGRAM_TYPE_FOG
	glsl_features_fog,
	// GLSL_PROGRAM_TYPE_FXAA
	glsl_features_fxaa,
	// GLSL_PROGRAM_TYPE_YUV
	glsl_features_empty,
	// GLSL_PROGRAM_TYPE_COLOR_CORRECTION
	glsl_features_colcorrection,
	// GLSL_PROGRAM_TYPE_KAWASE_BLUR
	glsl_features_empty,
};

// ======================================================================================

#ifndef STR_HELPER
#define STR_HELPER( s )                 # s
#define STR_TOSTR( x )                  STR_HELPER( x )
#endif

#define QF_GLSL_VERSION120 "#version 120\n"
#define QF_GLSL_VERSION130 "#version 130\n"
#define QF_GLSL_VERSION140 "#version 140\n"

#define QF_GLSL_ENABLE_ARB_GPU_SHADER5 "#extension GL_ARB_gpu_shader5 : enable\n"
#define QF_GLSL_ENABLE_ARB_DRAW_INSTANCED "#extension GL_ARB_draw_instanced : enable\n"
#define QF_GLSL_ENABLE_EXT_TEXTURE_ARRAY "#extension GL_EXT_texture_array : enable\n"

class ProgramSourceBuilder {
public:
	ProgramSourceBuilder( wsw::PodVector<const char *> *strings, wsw::PodVector<int> *lengths )
		: m_strings( strings ), m_lengths( lengths ) {}

	void addAll( std::span<const char *> strings, std::span<int> lengths ) {
		assert( strings.size() == lengths.size() );
		assert( m_strings->size() == m_lengths->size() );
		m_strings->reserve( m_strings->size() + strings.size() );
		m_lengths->reserve( m_lengths->size() + lengths.size() );
		for( size_t i = 0; i < strings.size(); ++i ) {
			m_strings->push_back( strings[i] );
			m_lengths->push_back( lengths[i] );
		}
		assert( m_strings->size() == m_lengths->size() );
	}

	[[nodiscard]]
	auto size() const -> unsigned {
		assert( m_strings->size() == m_lengths->size() );
		return m_strings->size();
	}

	void resetToSize( size_t sizeToSet ) {
		assert( m_strings->size() == m_lengths->size() );
		assert( sizeToSet <= m_strings->size() );
		m_strings->resize( sizeToSet );
		m_lengths->resize( sizeToSet );
	}

	[[maybe_unused]]
	auto add( const char *string ) -> unsigned {
		const size_t result = m_lengths->size();
		m_strings->push_back( string );
		m_lengths->push_back( (int)std::strlen( string ) );
		return result;
	}

	[[maybe_unused]]
	auto add( const char *string, size_t length ) -> unsigned {
		const size_t result = m_lengths->size();
		m_strings->push_back( string );
		m_lengths->push_back( (int)length );
		return result;
	}

	// A wrapper for convenience of invocation
	void addAll( ProgramSourceFileCache::KeyOfBuiltin key, const ProgramSourceFileCache &sourceCache ) {
		sourceCache.addBuiltinSourceLines( key, this );
	}

private:
	static_assert( std::is_same_v<GLchar, char> );
	wsw::PodVector<const char *> *const m_strings;
	static_assert( std::is_same_v<int, GLint> );
	wsw::PodVector<int> *const m_lengths;
};

void ProgramSourceFileCache::addBuiltinSourceLines( KeyOfBuiltin key, ProgramSourceBuilder *sourceBuilder ) const {
	const auto *const fileEntry = (FileEntry *)key;

	for( unsigned lineIndex = 0; lineIndex < fileEntry->numLines; ++lineIndex ) {
		const Span &lineSpan = std::get<Span>( fileEntry->lineData[lineIndex] );
		const wsw::StringView &string = fileEntry->getStringForSpan( lineSpan );
		sourceBuilder->add( string.data(), string.length() );
	}
}

static_assert( SHADER_FUNC_SIN == 1 );
static_assert( SHADER_FUNC_TRIANGLE == 2 );
static_assert( SHADER_FUNC_SQUARE == 3 );
static_assert( SHADER_FUNC_SAWTOOTH == 4 );
static_assert( SHADER_FUNC_INVERSESAWTOOTH == 5 );

static constexpr const char *const kDeformFuncNames[] = {
	"WAVE_SIN",
	"WAVE_TRIANGLE",
	"WAVE_SQUARE",
	"WAVE_SAWTOOTH",
	"WAVE_INVERSESAWTOOTH"
};

using DeformStringBuffer = wsw::StaticString<128 * MAX_SHADER_DEFORMVS>;

[[nodiscard]]
static bool addWaveDeformSourceLines( const deformv_t &deform, DeformStringBuffer *buffer, ProgramSourceBuilder *builder ) {
	const auto func      = deform.func;
	const auto funcIndex = func.type - 1;
	if( (size_t)funcIndex >= std::size( kDeformFuncNames ) ) {
		return false;
	}
	const auto oldBufferSize = buffer->size();
	if( !buffer->appendf( "Position.xyz += "
						  "%s(u_QF_ShaderTime,%f,%f,%f+%f*(Position.x+Position.y+Position.z),%f) * Normal.xyz;\n",
						  kDeformFuncNames[funcIndex], func.args[0], func.args[1], func.args[2],
						  func.args[3] != 0.0f ? deform.args[0] : 0.0, func.args[3] ) ) {
		return false;
	}
	builder->add( buffer->data() + oldBufferSize, buffer->size() - oldBufferSize );
	return true;
}

[[nodiscard]]
static bool addMoveDeformSourceLines( const deformv_t &deform, DeformStringBuffer *buffer, ProgramSourceBuilder *builder ) {
	const auto func      = deform.func;
	const auto funcIndex = func.type - 1;
	if( (size_t)funcIndex >= std::size( kDeformFuncNames ) ) {
		return false;
	}
	const auto oldBufferSize = buffer->size();
	if( !buffer->appendf( "Position.xyz += "
						  "%s(u_QF_ShaderTime,%f,%f,%f,%f) * vec3(%f, %f, %f);\n",
						  kDeformFuncNames[funcIndex], func.args[0], func.args[1], func.args[2], func.args[3],
						  deform.args[0], deform.args[1], deform.args[2] ) ) {
		return false;
	}
	builder->add( buffer->data() + oldBufferSize, buffer->size() - oldBufferSize );
	return true;
}

[[nodiscard]]
static auto addBulgeDeformSourceLines( const deformv_t &deform, DeformStringBuffer *buffer, ProgramSourceBuilder *builder ) {
	const auto oldBufferSize1 = buffer->size();
	if( !buffer->appendf( "t = sin(TexCoord.s * %f + u_QF_ShaderTime * %f);\n",
						  deform.args[0], deform.args[2] ) ) {
		return false;
	}
	builder->add( buffer->data() + oldBufferSize1, buffer->size() - oldBufferSize1 );
	const auto oldBufferSize2 = buffer->size();
	if( !buffer->appendf( "Position.xyz += max (-1.0 + %f, t) * %f * Normal.xyz;\n",
						  deform.args[3], deform.args[1] ) ) {
		return false;
	}
	builder->add( buffer->data() + oldBufferSize2, buffer->size() - oldBufferSize2 );
	return true;
}

[[nodiscard]]
static bool addDeformSourceLines( std::span<const deformv_t> deforms, DeformStringBuffer *buffer, ProgramSourceBuilder *builder ) {
	if( deforms.empty() ) {
		return true;
	}

	assert( deforms.size() <= MAX_SHADER_DEFORMVS );

	buffer->clear();

	builder->addAll( g_programSourceFileCache.deformPrologue, g_programSourceFileCache );

	for( const deformv_t &deform: deforms ) {
		switch( deform.type ) {
			case DEFORMV_WAVE:
				if( !addWaveDeformSourceLines( deform, buffer, builder ) ) {
					return false;
				}
				break;
			case DEFORMV_MOVE:
				if( !addMoveDeformSourceLines( deform, buffer, builder ) ) {
					return false;
				}
			case DEFORMV_BULGE:
				if( !addBulgeDeformSourceLines( deform, buffer, builder ) ) {
					return false;
				}
				break;
			case DEFORMV_AUTOSPRITE:
				builder->addAll( g_programSourceFileCache.deformAutosprite, g_programSourceFileCache );
				break;
			case DEFORMV_AUTOPARTICLE:
				builder->addAll( g_programSourceFileCache.deformAutoparticle, g_programSourceFileCache );
				break;
			case DEFORMV_AUTOSPRITE2:
				builder->addAll( g_programSourceFileCache.deformAutosprite2, g_programSourceFileCache );
				break;
			default:
				return false;
		}
	}

	builder->addAll( g_programSourceFileCache.deformEpilogue, g_programSourceFileCache );
	return true;
}

auto ShaderProgramCache::getProgramForParams( int type, const wsw::StringView &maybeRequestedName, uint64_t features,
											  const DeformSig &deformSig, std::span<const deformv_t> deforms ) -> int {
	assert( qglGetError() == GL_NO_ERROR );
	if( type <= GLSL_PROGRAM_TYPE_NONE || type >= GLSL_PROGRAM_TYPE_MAXTYPE ) {
		return 0;
	}

	// TODO: Shuffle bits better
	const auto featureWord1 = (uint32_t)( features >> 32 );
	const auto featureWord2 = (uint32_t)( features & 0xFFFF'FFFFu );
	const auto featureHash  = featureWord1 ^ featureWord2;
	const auto binIndex     = featureHash % GLSL_PROGRAMS_HASH_SIZE;

	for( ShaderProgram *program = m_hashBinsForType[type][binIndex]; program; program = program->nextInHashBin ) {
		if( program->features == features && program->deformSig == deformSig ) {
			return (int)( program->index + 1 );
		}
	}

	wsw::StringView requestedNameToUse = maybeRequestedName;
	if( requestedNameToUse.empty() ) {
		for( const ShaderProgram *program = m_programListHead; program; program = program->nextInList ) {
			if( program->type == type && !program->features ) {
				assert( !program->name.empty() );
				requestedNameToUse = program->name;
				break;
			}
		}
		if( requestedNameToUse.empty() ) {
			rError() << "Failed to find an existing program for the type" << wsw::xfmt( type );
			return 0;
		}
	}

	assert( requestedNameToUse.isZeroTerminated() );

	if( m_programsAllocator.isFull() ) {
		rError() << "Failed to create a program" << requestedNameToUse << ": too many programs";
		return 0;
	}

	const auto maybeObjectIds = createProgramFromSource( requestedNameToUse, type, features, deforms );
	if( !maybeObjectIds ) {
		rError() << "Failed to create a program" << requestedNameToUse << "from source";
		return 0;
	}

	unsigned programIndex = 0;
	void *const mem        = m_programsAllocator.allocOrNull( &programIndex );
	auto *const program    = new( mem )ShaderProgram;

	char *nameData;
	if( requestedNameToUse.size() > kAllocatorNameLengthLimit ) {
		nameData = (char *)Q_malloc( requestedNameToUse.size() + 1 );
	} else {
		assert( !m_namesAllocator.isFull() );
		nameData = (char *)m_namesAllocator.allocOrNull();
	}

	requestedNameToUse.copyTo( nameData, requestedNameToUse.size() + 1 );
	program->name           = wsw::StringView( nameData, requestedNameToUse.size(), wsw::StringView::ZeroTerminated );
	program->nameDataToFree = nameData;

	program->deformSig = deformSig;
	if( deformSig.data && deformSig.len ) {
		const size_t deformSigDataSize = sizeof( int ) * deformSig.len;
		int *storedDeformSigData;
		if( deformSigDataSize > kExtraTrailingProgramBytesSize ) {
			storedDeformSigData          = (int *)Q_malloc( deformSigDataSize );
			program->deformSigDataToFree = storedDeformSigData;
		} else {
			storedDeformSigData = (int *)( program + 1 );
		}
		std::memcpy( storedDeformSigData, deformSig.data, deformSigDataSize );
		program->deformSig.data = storedDeformSigData;
	}

	program->programId        = std::get<0>( *maybeObjectIds );
	program->vertexShaderId   = std::get<1>( *maybeObjectIds );
	program->fragmentShaderId = std::get<2>( *maybeObjectIds );

	program->type     = type;
	program->features = features;

	program->nextInHashBin            = m_hashBinsForType[type][binIndex];
	m_hashBinsForType[type][binIndex] = program;

	program->nextInList = m_programListHead;
	m_programListHead   = program;

	program->index                  = programIndex;
	m_programForIndex[programIndex] = program;

	qglUseProgram( program->programId );
	setupUniformsAndLocations( program );
	assert( qglGetError() == GL_NO_ERROR );

	return (int)( programIndex + 1 );
}

auto ShaderProgramCache::createProgramFromSource( const wsw::StringView &name, int type, uint64_t features,
												  std::span<const deformv_t> deforms )
												  -> std::optional<std::tuple<GLuint, GLuint, GLuint>> {
	GLuint programId        = 0;
	GLuint vertexShaderId   = 0;
	GLuint fragmentShaderId = 0;

	bool succeeded = false;

	try {
		// This is a "structured-goto" that allows early exits
		do {
			if( !( programId = qglCreateProgram() ) ) {
				break;
			}
			if( !( vertexShaderId = qglCreateShader( GL_VERTEX_SHADER ) ) ) {
				break;
			}
			if( !( fragmentShaderId = qglCreateShader( GL_FRAGMENT_SHADER ) ) ) {
				break;
			}
			if( !bindAttributeLocations( programId ) ) {
				break;
			}
			if( !loadShaderSources( name, type, features, deforms, vertexShaderId, fragmentShaderId ) ) {
				break;
			}
			if( !linkProgram( programId, vertexShaderId, fragmentShaderId ) ) {
				break;
			}
			succeeded = true;
		} while( false );
	} catch( std::exception &ex ) {
		rError() << "Caught an exception while trying to create a program:" << wsw::StringView( ex.what() );
		succeeded = false;
	} catch( ... ) {
		succeeded = false;
	}

	if( !succeeded ) {
		destroyProgramObjects( programId, vertexShaderId, fragmentShaderId );
		return std::nullopt;
	}

	return std::make_tuple( programId, vertexShaderId, fragmentShaderId );
}

void ShaderProgramCache::destroyProgramObjects( GLuint programId, GLuint vertexShaderId, GLuint fragmentShaderId ) {
	if( programId ) {
		GLsizei numAttachedShaders = 0;
		GLuint attachedShaders[2] { 0, 0 };
		qglGetAttachedShaders( programId, 2, &numAttachedShaders, attachedShaders );
		for( GLsizei i = 0; i < numAttachedShaders; ++i ) {
			qglDetachShader( programId, attachedShaders[i] );
		}
	}
	if( fragmentShaderId ) {
		qglDeleteShader( fragmentShaderId );
	}
	if( vertexShaderId ) {
		qglDeleteShader( vertexShaderId );
	}
	if( programId ) {
		qglDeleteProgram( programId );
	}
}

bool ShaderProgramCache::loadShaderSources( const wsw::StringView &name, int type, uint64_t features,
											std::span<const deformv_t> deforms,
											GLuint vertexShaderId, GLuint fragmentShaderId ) {
	assert( !name.empty() && name.isZeroTerminated() );

	wsw::StaticString<64> shaderVersion;
	shaderVersion << "#define QF_GLSL_VERSION "_asView << glConfig.shadingLanguageVersion << "\n"_asView;

	wsw::StaticString<64> maxBones;
	maxBones << "#define MAX_UNIFORM_BONES "_asView << glConfig.maxGLSLBones << "\n"_asView;

	m_tmpCommonStrings.clear();
	m_tmpCommonOffsets.clear();

	ProgramSourceBuilder commonStringsBuilder( &m_tmpCommonStrings, &m_tmpCommonOffsets );

	const unsigned offsetOfFeatureStrings = commonStringsBuilder.size();
	loadSourceOfFeatures( type, features, &commonStringsBuilder );
	const unsigned lengthOfFeatureStrings = commonStringsBuilder.size() - offsetOfFeatureStrings;

	const SpanOfSourceStrings spanOfFeatureStrings {
		{ m_tmpCommonStrings.data() + offsetOfFeatureStrings, lengthOfFeatureStrings },
		{ m_tmpCommonOffsets.data() + offsetOfFeatureStrings, lengthOfFeatureStrings },
	};

	Com_DPrintf( "Registering GLSL program %s for features 0x%" PRIu64 "\n", name.data(), features );

	if( !loadVertexShaderSource( vertexShaderId, name, type, features, deforms,
								 spanOfFeatureStrings, shaderVersion.asView(), maxBones.asView() ) ) {
		return false;
	}

	if( !loadFragmentShaderSource( fragmentShaderId, name, type, features,
								   spanOfFeatureStrings, shaderVersion.asView(), maxBones.asView() ) ) {
		return false;
	}

	return true;
}

void ShaderProgramCache::loadSourceOfFeatures( int type, uint64_t features, ProgramSourceBuilder *sourceBuilder ) {

	if( const glsl_feature_t *const type_features = glsl_programtypes_features[type] ) {
		uint64_t unsatisfiedFeatures = features;
		int featureRowNum            = 0;
		for(;; ) {
			if( !unsatisfiedFeatures ) {
				break;
			}
			const glsl_feature_t &featureRow = type_features[featureRowNum];
			if( !featureRow.featureBit ) {
				break;
			}
			if( ( featureRow.featureBit & unsatisfiedFeatures ) == featureRow.featureBit ) {
				sourceBuilder->add( featureRow.define );
				unsatisfiedFeatures &= ~featureRow.featureBit;
			}
			featureRowNum++;
		}
	}
}

bool ShaderProgramCache::loadVertexShaderSource( GLuint id, const wsw::StringView &name, int type, uint64_t features,
												 std::span<const deformv_t> deforms,
												 const SpanOfSourceStrings &featureStrings,
												 const wsw::StringView &shaderVersion, const wsw::StringView &maxBones ) {
	assert( !name.empty() && name.isZeroTerminated() );

	m_tmpShaderStrings.clear();
	m_tmpShaderLengths.clear();

	ProgramSourceBuilder sourceBuilder( &m_tmpShaderStrings, &m_tmpShaderLengths );

	if( glConfig.shadingLanguageVersion >= 140 ) {
		sourceBuilder.add( QF_GLSL_VERSION140 );
	} else if( glConfig.shadingLanguageVersion >= 130 ) {
		sourceBuilder.add( QF_GLSL_VERSION130 );
	} else {
		sourceBuilder.add( QF_GLSL_VERSION120 );
	}

	if( glConfig.ext.gpu_shader5 ) {
		sourceBuilder.add( QF_GLSL_ENABLE_ARB_GPU_SHADER5 );
	}

	if( glConfig.shadingLanguageVersion < 400 ) {
		sourceBuilder.add( QF_GLSL_ENABLE_ARB_DRAW_INSTANCED );
	}

	sourceBuilder.add( shaderVersion.data() );
	sourceBuilder.add( "#define VERTEX_SHADER\n" );

	sourceBuilder.addAll( g_programSourceFileCache.macros, g_programSourceFileCache );
	if( glConfig.shadingLanguageVersion >= 130 ) {
		sourceBuilder.addAll( g_programSourceFileCache.macros130, g_programSourceFileCache );
	} else {
		sourceBuilder.addAll( g_programSourceFileCache.macros120, g_programSourceFileCache );
	}

	sourceBuilder.addAll( g_programSourceFileCache.constants, g_programSourceFileCache );
	sourceBuilder.add( maxBones.data() );
	sourceBuilder.addAll( g_programSourceFileCache.uniforms, g_programSourceFileCache );

	sourceBuilder.addAll( g_programSourceFileCache.waveFuncs, g_programSourceFileCache );
	sourceBuilder.addAll( g_programSourceFileCache.math, g_programSourceFileCache );

	sourceBuilder.addAll( featureStrings.strings, featureStrings.lengths );

	DeformStringBuffer deformStringBuffer;
	if( !addDeformSourceLines( deforms, &deformStringBuffer, &sourceBuilder ) ) {
		return false;
	}

	if( features & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		sourceBuilder.add( "qf_attribute vec4 a_BonesIndices, a_BonesWeights;\n" );
		sourceBuilder.add( "uniform vec4 u_DualQuats[MAX_UNIFORM_BONES*2];\n" );

		sourceBuilder.addAll( g_programSourceFileCache.quatTransform, g_programSourceFileCache );
		sourceBuilder.add( "#define QF_DUAL_QUAT_TRANSFORM_TANGENT\n");
		sourceBuilder.addAll( g_programSourceFileCache.quatTransform, g_programSourceFileCache );
		sourceBuilder.add( "#undef QF_DUAL_QUAT_TRANSFORM_TANGENT\n");
	}

	if( features & ( GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS | GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS ) ) {
		sourceBuilder.addAll( g_programSourceFileCache.instancedTransforms, g_programSourceFileCache );
	}

	sourceBuilder.addAll( g_programSourceFileCache.transformVerts, g_programSourceFileCache );

	char fileName[1024];
	Q_snprintfz( fileName, sizeof( fileName ), "glsl/%s.vert.glsl", name.data() );

	ProgramSourceLoader vertexShaderLoader( &g_programSourceFileCache, &m_tmpShaderStrings, &m_tmpShaderLengths );
	if( !vertexShaderLoader.load( wsw::StringView( fileName ), features, type ) ) {
		rError() << "Failed to load the source of" << wsw::StringView( fileName );
		return false;
	}

	if( !compileShader( id, "vertex", m_tmpShaderStrings, m_tmpShaderLengths ) ) {
		rError() << "Failed to compile" << wsw::StringView( fileName );
		return false;
	}

	return true;
}

bool ShaderProgramCache::loadFragmentShaderSource( GLuint id, const wsw::StringView &name, int type, uint64_t features,
												   const SpanOfSourceStrings &featureStrings,
												   const wsw::StringView &shaderVersion, const wsw::StringView &maxBones ) {
	assert( !name.empty() && name.isZeroTerminated() );

	m_tmpShaderStrings.clear();
	m_tmpShaderLengths.clear();

	ProgramSourceBuilder sourceBuilder( &m_tmpShaderStrings, &m_tmpShaderLengths );
	if( glConfig.shadingLanguageVersion >= 140 ) {
		sourceBuilder.add( QF_GLSL_VERSION140 );
	} else if( glConfig.shadingLanguageVersion >= 130 ) {
		sourceBuilder.add( QF_GLSL_VERSION130 );
	} else {
		sourceBuilder.add( QF_GLSL_VERSION120 );
	}

	if( glConfig.ext.gpu_shader5 ) {
		sourceBuilder.add( QF_GLSL_ENABLE_ARB_GPU_SHADER5 );
	}

	sourceBuilder.add( shaderVersion.data() );
	sourceBuilder.add( "#define FRAGMENT_SHADER\n" );

	sourceBuilder.addAll( g_programSourceFileCache.macros, g_programSourceFileCache );
	if( glConfig.shadingLanguageVersion >= 130 ) {
		sourceBuilder.addAll( g_programSourceFileCache.macros130, g_programSourceFileCache );
	} else {
		sourceBuilder.addAll( g_programSourceFileCache.macros120, g_programSourceFileCache );
	}

	sourceBuilder.addAll( g_programSourceFileCache.constants, g_programSourceFileCache );
	sourceBuilder.add( maxBones.data() );
	sourceBuilder.addAll( g_programSourceFileCache.uniforms, g_programSourceFileCache );

	sourceBuilder.addAll( g_programSourceFileCache.math, g_programSourceFileCache );

	sourceBuilder.addAll( featureStrings.strings, featureStrings.lengths );

	char fileName[1024];
	Q_snprintfz( fileName, sizeof( fileName ), "glsl/%s.frag.glsl", name.data() );

	ProgramSourceLoader fragmentShaderLoader( &g_programSourceFileCache, &m_tmpShaderStrings, &m_tmpShaderLengths );
	if( !fragmentShaderLoader.load( wsw::StringView( fileName ), features, type ) ) {
		rError() << "Failed to load source of", wsw::StringView( fileName );
		return false;
	}

	if( !compileShader( id, "fragment", m_tmpShaderStrings, m_tmpShaderLengths ) ) {
		rError() << "Failed to compile %s\n", wsw::StringView( fileName );
		return false;
	}

	return true;
}

bool ShaderProgramCache::compileShader( GLuint id, const char *kind, std::span<const char *> strings,
										std::span<int> lengths ) {
	assert( strings.size() == lengths.size() );
	qglShaderSource( id, (GLsizei)strings.size(), strings.data(), lengths.data() );
	qglCompileShader( id );

	GLint compileStatus = 0;
	qglGetShaderiv( id, GL_COMPILE_STATUS, &compileStatus );
	if( compileStatus != GL_TRUE ) {
		char log[1024];
		GLsizei logLength = 0;
		qglGetShaderInfoLog( id, (GLsizei)sizeof( log ), &logLength, log );
		log[logLength] = log[sizeof( log ) - 1] = '\0';
		// TODO: Split log lines
		rError() << "Failed to compile a" << wsw::unquoted( wsw::StringView( kind ) )
			<< "shader" << wsw::unquoted( wsw::StringView( log, logLength ) );
		return false;
	}

	return true;
}

bool ShaderProgramCache::linkProgram( GLuint programId, GLuint vertexShaderId, GLuint fragmentShaderId ) {
	qglAttachShader( programId, vertexShaderId );
	qglAttachShader( programId, fragmentShaderId );

	GLint linkStatus = 0;
	qglLinkProgram( programId );
	qglGetProgramiv( programId, GL_LINK_STATUS, &linkStatus );
	if( linkStatus != GL_TRUE ) {
		char log[1024];
		GLsizei logLength = 0;
		qglGetProgramInfoLog( programId, (GLsizei)sizeof( log ), &logLength, log );
		log[logLength] = log[sizeof( log ) - 1] = '\0';
		// TODO: Split log lines
		rError() << "Failed to link a program:" << wsw::unquoted( wsw::StringView( log, logLength ) );
		return false;
	}

	return true;
}

int RP_RegisterProgram( int type, const char *name, const DeformSig &deformSig, const deformv_t *deforms, int numDeforms, uint64_t features ) {
	return g_programCacheInstanceHolder.instance()->getProgramForParams( type, name, features, deformSig, { deforms, (size_t)numDeforms } );
}

int RP_GetProgramObject( int elem ) {
	if( ShaderProgram *program = g_programCacheInstanceHolder.instance()->getProgramById( elem ) ) {
		return (int)program->programId;
	}
	return 0;
}

/*
* RP_UpdateShaderUniforms
*/
void RP_UpdateShaderUniforms( int elem,
							  float shaderTime,
							  const vec3_t entOrigin, const vec3_t entDist, const uint8_t *entityColor,
							  const uint8_t *constColor, const float *rgbGenFuncArgs, const float *alphaGenFuncArgs,
							  const mat4_t texMatrix, float colorMod ) {
	GLfloat m[9];
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( entOrigin ) {
		if( program->loc.EntityOrigin >= 0 ) {
			qglUniform3fv( program->loc.EntityOrigin, 1, entOrigin );
		}
		if( program->loc.builtin.EntityOrigin >= 0 ) {
			qglUniform3fv( program->loc.builtin.EntityOrigin, 1, entOrigin );
		}
	}

	if( program->loc.EntityDist >= 0 && entDist ) {
		qglUniform3fv( program->loc.EntityDist, 1, entDist );
	}
	if( program->loc.EntityColor >= 0 && entityColor ) {
		qglUniform4f( program->loc.EntityColor, entityColor[0] * 1.0 / 255.0, entityColor[1] * 1.0 / 255.0, entityColor[2] * 1.0 / 255.0, entityColor[3] * 1.0 / 255.0 );
	}

	if( program->loc.ShaderTime >= 0 ) {
		qglUniform1f( program->loc.ShaderTime, shaderTime );
	}
	if( program->loc.builtin.ShaderTime >= 0 ) {
		qglUniform1f( program->loc.builtin.ShaderTime, shaderTime );
	}

	if( program->loc.ConstColor >= 0 && constColor ) {
		qglUniform4f( program->loc.ConstColor, constColor[0] * 1.0 / 255.0, constColor[1] * 1.0 / 255.0, constColor[2] * 1.0 / 255.0, constColor[3] * 1.0 / 255.0 );
	}
	if( program->loc.RGBGenFuncArgs >= 0 && rgbGenFuncArgs ) {
		qglUniform4fv( program->loc.RGBGenFuncArgs, 1, rgbGenFuncArgs );
	}
	if( program->loc.AlphaGenFuncArgs >= 0 && alphaGenFuncArgs ) {
		qglUniform4fv( program->loc.AlphaGenFuncArgs, 1, alphaGenFuncArgs );
	}

	// FIXME: this looks shit...
	if( program->loc.TextureMatrix >= 0 ) {
		m[0] = texMatrix[0], m[1] = texMatrix[4];
		m[2] = texMatrix[1], m[3] = texMatrix[5];
		m[4] = texMatrix[12], m[5] = texMatrix[13];

		qglUniform4fv( program->loc.TextureMatrix, 2, m );
	}

	if( program->loc.LightingIntensity >= 0 ) {
		qglUniform1f( program->loc.LightingIntensity, 1.0 );
	}
	if( program->loc.ColorMod >= 0 ) {
		qglUniform1f( program->loc.ColorMod, colorMod );
	}
}

/*
* RP_UpdateViewUniforms
*/
void RP_UpdateViewUniforms( int elem,
							const mat4_t modelviewMatrix, const mat4_t modelviewProjectionMatrix,
							const vec3_t viewOrigin, const mat3_t viewAxis,
							const float mirrorSide,
							int viewport[4],
							float zNear, float zFar ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.ModelViewMatrix >= 0 ) {
		qglUniformMatrix4fv( program->loc.ModelViewMatrix, 1, GL_FALSE, modelviewMatrix );
	}
	if( program->loc.ModelViewProjectionMatrix >= 0 ) {
		qglUniformMatrix4fv( program->loc.ModelViewProjectionMatrix, 1, GL_FALSE, modelviewProjectionMatrix );
	}

	if( program->loc.ZRange >= 0 ) {
		qglUniform2f( program->loc.ZRange, zNear, zFar );
	}

	if( viewOrigin ) {
		if( program->loc.ViewOrigin >= 0 ) {
			qglUniform3fv( program->loc.ViewOrigin, 1, viewOrigin );
		}
		if( program->loc.builtin.ViewOrigin >= 0 ) {
			qglUniform3fv( program->loc.builtin.ViewOrigin, 1, viewOrigin );
		}
	}

	if( viewAxis ) {
		if( program->loc.ViewAxis >= 0 ) {
			qglUniformMatrix3fv( program->loc.ViewAxis, 1, GL_FALSE, viewAxis );
		}
		if( program->loc.builtin.ViewAxis >= 0 ) {
			qglUniformMatrix3fv( program->loc.builtin.ViewAxis, 1, GL_FALSE, viewAxis );
		}
	}

	if( program->loc.Viewport >= 0 ) {
		qglUniform4iv( program->loc.Viewport, 1, viewport );
	}

	if( program->loc.MirrorSide >= 0 ) {
		qglUniform1f( program->loc.MirrorSide, mirrorSide );
	}
	if( program->loc.builtin.MirrorSide >= 0 ) {
		qglUniform1f( program->loc.builtin.MirrorSide, mirrorSide );
	}
}

/*
* RP_UpdateBlendMixUniform
*
* The first component corresponds to RGB, the second to ALPHA.
* Whenever the program needs to scale source colors, the mask needs
* to be used in the following manner:
* color *= mix(myhalf4(1.0), myhalf4(scale), u_BlendMix.xxxy);
*/
void RP_UpdateBlendMixUniform( int elem, vec2_t blendMix ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.BlendMix >= 0 ) {
		qglUniform2fv( program->loc.BlendMix, 1, blendMix );
	}
}

/*
* RP_UpdateSoftParticlesUniforms
*/
void RP_UpdateSoftParticlesUniforms( int elem, float scale ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.SoftParticlesScale >= 0 ) {
		qglUniform1f( program->loc.SoftParticlesScale, scale );
	}
}

/*
* RP_UpdateDiffuseLightUniforms
*/
void RP_UpdateDiffuseLightUniforms( int elem,
									const vec3_t lightDir, const vec4_t lightAmbient, const vec4_t lightDiffuse ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.LightDir >= 0 && lightDir ) {
		qglUniform3fv( program->loc.LightDir, 1, lightDir );
	}
	if( program->loc.LightAmbient >= 0 && lightAmbient ) {
		qglUniform3f( program->loc.LightAmbient, lightAmbient[0], lightAmbient[1], lightAmbient[2] );
	}
	if( program->loc.LightDiffuse >= 0 && lightDiffuse ) {
		qglUniform3f( program->loc.LightDiffuse, lightDiffuse[0], lightDiffuse[1], lightDiffuse[2] );
	}
	if( program->loc.LightingIntensity >= 0 ) {
		qglUniform1f( program->loc.LightingIntensity, r_lighting_intensity->value );
	}
}

/*
* RP_UpdateMaterialUniforms
*/
void RP_UpdateMaterialUniforms( int elem,
								float offsetmappingScale, float glossIntensity, float glossExponent ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.GlossFactors >= 0 ) {
		qglUniform2f( program->loc.GlossFactors, glossIntensity, glossExponent );
	}
	if( program->loc.OffsetMappingScale >= 0 ) {
		qglUniform1f( program->loc.OffsetMappingScale, offsetmappingScale );
	}
}

/*
* RP_UpdateDistortionUniforms
*/
void RP_UpdateDistortionUniforms( int elem, bool frontPlane ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.FrontPlane >= 0 ) {
		qglUniform1f( program->loc.FrontPlane, frontPlane ? 1 : -1 );
	}
}

/*
* RP_UpdateTextureUniforms
*/
void RP_UpdateTextureUniforms( int elem, int TexWidth, int TexHeight ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.TextureParams >= 0 ) {
		qglUniform4f( program->loc.TextureParams, TexWidth, TexHeight,
						 TexWidth ? 1.0 / TexWidth : 1.0, TexHeight ? 1.0 / TexHeight : 1.0 );
	}
}

/*
* RP_UpdateOutlineUniforms
*/
void RP_UpdateOutlineUniforms( int elem, float projDistance ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.OutlineHeight >= 0 ) {
		qglUniform1f( program->loc.OutlineHeight, projDistance );
	}
	if( program->loc.OutlineCutOff >= 0 ) {
		qglUniform1f( program->loc.OutlineCutOff, wsw::max( 0.0f, r_outlines_cutoff->value ) );
	}
}

/*
* RP_UpdateFogUniforms
*/
void RP_UpdateFogUniforms( int elem, byte_vec4_t color, float clearDist, float opaqueDist, cplane_t *fogPlane, cplane_t *eyePlane, float eyeDist ) {
	GLfloat fog_color[3] = { 0, 0, 0 };
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	VectorScale( color, ( 1.0 / 255.0 ), fog_color );

	if( program->loc.Fog.Color >= 0 ) {
		qglUniform3fv( program->loc.Fog.Color, 1, fog_color );
	}
	if( program->loc.Fog.ScaleAndEyeDist >= 0 ) {
		qglUniform2f( program->loc.Fog.ScaleAndEyeDist, 1.0 / ( opaqueDist - clearDist ), eyeDist );
	}
	if( program->loc.Fog.Plane >= 0 ) {
		qglUniform4f( program->loc.Fog.Plane, fogPlane->normal[0], fogPlane->normal[1], fogPlane->normal[2], fogPlane->dist );
	}
	if( program->loc.Fog.EyePlane >= 0 ) {
		qglUniform4f( program->loc.Fog.EyePlane, eyePlane->normal[0], eyePlane->normal[1], eyePlane->normal[2], eyePlane->dist );
	}
}

void RP_UpdateDynamicLightsUniforms( const FrontendToBackendShared *fsh,
									 int elem, const superLightStyle_t *superLightStyle,
									 const vec3_t entOrigin, const mat3_t entAxis, unsigned int dlightbits ) {
	int i, n, c;
	vec3_t dlorigin, tvec;
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );
	bool identityAxis = Matrix3_Compare( entAxis, axis_identity );
	vec4_t shaderColor[4];

	if( superLightStyle ) {
		GLfloat rgb[3];
		static float deluxemapOffset[( MAX_LIGHTMAPS + 3 ) & ( ~3 )];

		for( i = 0; i < MAX_LIGHTMAPS && superLightStyle->lightmapStyles[i] != 255; i++ ) {
			VectorCopy( lightStyles[superLightStyle->lightmapStyles[i]].rgb, rgb );

			if( program->loc.LightstyleColor[i] >= 0 ) {
				qglUniform3fv( program->loc.LightstyleColor[i], 1, rgb );
			}
			if( program->loc.DeluxemapOffset >= 0 ) {
				deluxemapOffset[i] = superLightStyle->stOffset[i][0];
			}
		}

		if( i && ( program->loc.DeluxemapOffset >= 0 ) ) {
			qglUniform4fv( program->loc.DeluxemapOffset, ( i + 3 ) / 4, deluxemapOffset );
		}
	}

	if( dlightbits ) {
		memset( shaderColor, 0, sizeof( vec4_t ) * 3 );
		Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
		n = 0;

		for( i = 0; i < (int)fsh->visibleProgramLightIndices.size(); ++i ) {
			const unsigned lightBit = 1 << i;
			if( !( dlightbits & lightBit ) ) {
				continue;
			}

			dlightbits &= ~lightBit;

			if( program->loc.DynamicLightsPosition[n] < 0 ) {
				break;
			}

			const auto *const light = fsh->dynamicLights + fsh->visibleProgramLightIndices[i];
			assert( light->hasProgramLight && light->programRadius >= 1.0f );

			VectorSubtract( light->origin, entOrigin, dlorigin );
			if( !identityAxis ) {
				VectorCopy( dlorigin, tvec );
				Matrix3_TransformVector( entAxis, tvec, dlorigin );
			}

			qglUniform3fv( program->loc.DynamicLightsPosition[n], 1, dlorigin );

			c = n & 3;
			shaderColor[0][c] = light->color[0];
			shaderColor[1][c] = light->color[1];
			shaderColor[2][c] = light->color[2];
			shaderColor[3][c] = Q_Rcp( light->programRadius );

			// DynamicLightsDiffuseAndInvRadius is transposed for SIMD, but it's still 4x4
			if( c == 3 ) {
				qglUniform4fv( program->loc.DynamicLightsDiffuseAndInvRadius[n >> 2], 4, shaderColor[0] );
				memset( shaderColor, 0, sizeof( vec4_t ) * 3 );
				Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
			}

			n++;

			dlightbits &= ~lightBit;
			if( !dlightbits ) {
				break;
			}
		}

		if( n & 3 ) {
			qglUniform4fv( program->loc.DynamicLightsDiffuseAndInvRadius[n >> 2], 4, shaderColor[0] );
			memset( shaderColor, 0, sizeof( vec4_t ) * 3 ); // to set to zero for the remaining lights
			Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
			n = ALIGN( n, 4 );
		}

		if( program->loc.NumDynamicLights >= 0 ) {
			qglUniform1i( program->loc.NumDynamicLights, n );
		}

		for( ; n < MAX_DLIGHTS; n += 4 ) {
			if( program->loc.DynamicLightsPosition[n] < 0 ) {
				break;
			}
			qglUniform4fv( program->loc.DynamicLightsDiffuseAndInvRadius[n >> 2], 4, shaderColor[0] );
		}
	}
}

/*
* RP_UpdateTexGenUniforms
*/
void RP_UpdateTexGenUniforms( int elem, const mat4_t reflectionMatrix, const mat4_t vectorMatrix ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.ReflectionTexMatrix >= 0 ) {
		mat3_t m;
		memcpy( &m[0], &reflectionMatrix[0], 3 * sizeof( vec_t ) );
		memcpy( &m[3], &reflectionMatrix[4], 3 * sizeof( vec_t ) );
		memcpy( &m[6], &reflectionMatrix[8], 3 * sizeof( vec_t ) );
		qglUniformMatrix3fv( program->loc.ReflectionTexMatrix, 1, GL_FALSE, m );
	}
	if( program->loc.VectorTexMatrix >= 0 ) {
		qglUniformMatrix4fv( program->loc.VectorTexMatrix, 1, GL_FALSE, vectorMatrix );
	}
}

/*
* RP_UpdateBonesUniforms
*
* Set uniform values for animation dual quaternions
*/
void RP_UpdateBonesUniforms( int elem, unsigned int numBones, dualquat_t *animDualQuat ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( numBones > glConfig.maxGLSLBones ) {
		return;
	}
	if( program->loc.DualQuats < 0 ) {
		return;
	}
	qglUniform4fv( program->loc.DualQuats, numBones * 2, &animDualQuat[0][0] );
}

/*
* RP_UpdateInstancesUniforms
*
* Set uniform values for instance points (quaternion + xyz + scale)
*/
void RP_UpdateInstancesUniforms( int elem, unsigned int numInstances, instancePoint_t *instances ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( numInstances > MAX_GLSL_UNIFORM_INSTANCES ) {
		numInstances = MAX_GLSL_UNIFORM_INSTANCES;
	}
	if( program->loc.InstancePoints < 0 ) {
		return;
	}
	qglUniform4fv( program->loc.InstancePoints, numInstances * 2, &instances[0][0] );
}

/*
* RP_UpdateColorCorrectionUniforms
*/
void RP_UpdateColorCorrectionUniforms( int elem, float hdrGamma, float hdrExposure ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.hdrGamma >= 0 ) {
		qglUniform1f( program->loc.hdrGamma, hdrGamma );
	}
	if( program->loc.hdrExposure >= 0 ) {
		qglUniform1f( program->loc.hdrExposure, hdrExposure );
	}
}

/*
* RP_UpdateDrawFlatUniforms
*/
void RP_UpdateDrawFlatUniforms( int elem, const vec3_t wallColor, const vec3_t floorColor ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.WallColor >= 0 ) {
		qglUniform3f( program->loc.WallColor, wallColor[0], wallColor[1], wallColor[2] );
	}
	if( program->loc.FloorColor >= 0 ) {
		qglUniform3f( program->loc.FloorColor, floorColor[0], floorColor[1], floorColor[2] );
	}
}

/*
* RP_UpdateKawaseUniforms
*/
void RP_UpdateKawaseUniforms( int elem, int TexWidth, int TexHeight, int iteration ) {
	ShaderProgram *const program = g_programCacheInstanceHolder.instance()->getProgramById( elem );

	if( program->loc.TextureParams >= 0 ) {
		qglUniform4f( program->loc.TextureParams,
						 TexWidth ? 1.0 / TexWidth : 1.0, TexHeight ? 1.0 / TexHeight : 1.0, (float)iteration, 1.0 );
	}
}

void ShaderProgramCache::setupUniformsAndLocations( ShaderProgram *program ) {
	char tmp[1024];
	unsigned int i;
	int locBaseTexture,
		locNormalmapTexture,
		locGlossTexture,
		locDecalTexture,
		locEntityDecalTexture,
		locLightmapTexture[MAX_LIGHTMAPS],
		locDuDvMapTexture,
		locReflectionTexture,
		locRefractionTexture,
		locCelShadeTexture,
		locCelLightTexture,
		locDiffuseTexture,
		locStripesTexture,
		locDepthTexture,
		locYUVTextureY,
		locYUVTextureU,
		locYUVTextureV,
		locColorLUT
	;

	memset( &program->loc, -1, sizeof( program->loc ) );

	program->loc.ModelViewMatrix = qglGetUniformLocation( program->programId, "u_ModelViewMatrix" );
	program->loc.ModelViewProjectionMatrix = qglGetUniformLocation( program->programId, "u_ModelViewProjectionMatrix" );

	program->loc.ZRange = qglGetUniformLocation( program->programId, "u_ZRange" );

	program->loc.ViewOrigin = qglGetUniformLocation( program->programId, "u_ViewOrigin" );
	program->loc.ViewAxis = qglGetUniformLocation( program->programId, "u_ViewAxis" );

	program->loc.MirrorSide = qglGetUniformLocation( program->programId, "u_MirrorSide" );

	program->loc.Viewport = qglGetUniformLocation( program->programId, "u_Viewport" );

	program->loc.LightDir = qglGetUniformLocation( program->programId, "u_LightDir" );
	program->loc.LightAmbient = qglGetUniformLocation( program->programId, "u_LightAmbient" );
	program->loc.LightDiffuse = qglGetUniformLocation( program->programId, "u_LightDiffuse" );
	program->loc.LightingIntensity = qglGetUniformLocation( program->programId, "u_LightingIntensity" );

	program->loc.TextureMatrix = qglGetUniformLocation( program->programId, "u_TextureMatrix" );

	locBaseTexture = qglGetUniformLocation( program->programId, "u_BaseTexture" );
	locNormalmapTexture = qglGetUniformLocation( program->programId, "u_NormalmapTexture" );
	locGlossTexture = qglGetUniformLocation( program->programId, "u_GlossTexture" );
	locDecalTexture = qglGetUniformLocation( program->programId, "u_DecalTexture" );
	locEntityDecalTexture = qglGetUniformLocation( program->programId, "u_EntityDecalTexture" );

	locDuDvMapTexture = qglGetUniformLocation( program->programId, "u_DuDvMapTexture" );
	locReflectionTexture = qglGetUniformLocation( program->programId, "u_ReflectionTexture" );
	locRefractionTexture = qglGetUniformLocation( program->programId, "u_RefractionTexture" );

	locCelShadeTexture = qglGetUniformLocation( program->programId, "u_CelShadeTexture" );
	locCelLightTexture = qglGetUniformLocation( program->programId, "u_CelLightTexture" );
	locDiffuseTexture = qglGetUniformLocation( program->programId, "u_DiffuseTexture" );
	locStripesTexture = qglGetUniformLocation( program->programId, "u_StripesTexture" );

	locDepthTexture = qglGetUniformLocation( program->programId, "u_DepthTexture" );

	locYUVTextureY = qglGetUniformLocation( program->programId, "u_YUVTextureY" );
	locYUVTextureU = qglGetUniformLocation( program->programId, "u_YUVTextureU" );
	locYUVTextureV = qglGetUniformLocation( program->programId, "u_YUVTextureV" );

	locColorLUT = qglGetUniformLocation( program->programId, "u_ColorLUT" );

	program->loc.DeluxemapOffset = qglGetUniformLocation( program->programId, "u_DeluxemapOffset" );

	for( i = 0; i < MAX_LIGHTMAPS; i++ ) {
		// arrays of samplers are broken on ARM Mali so get u_LightmapTexture%i instead of u_LightmapTexture[%i]
		locLightmapTexture[i] = qglGetUniformLocation( program->programId,
														  va_r( tmp, sizeof( tmp ), "u_LightmapTexture%i", i ) );

		if( locLightmapTexture[i] < 0 ) {
			break;
		}

		program->loc.LightstyleColor[i] = qglGetUniformLocation( program->programId,
																	va_r( tmp, sizeof( tmp ), "u_LightstyleColor[%i]", i ) );
	}

	program->loc.GlossFactors = qglGetUniformLocation( program->programId, "u_GlossFactors" );

	program->loc.OffsetMappingScale = qglGetUniformLocation( program->programId, "u_OffsetMappingScale" );

	program->loc.OutlineHeight = qglGetUniformLocation( program->programId, "u_OutlineHeight" );
	program->loc.OutlineCutOff = qglGetUniformLocation( program->programId, "u_OutlineCutOff" );

	program->loc.FrontPlane = qglGetUniformLocation( program->programId, "u_FrontPlane" );

	program->loc.TextureParams = qglGetUniformLocation( program->programId, "u_TextureParams" );

	program->loc.EntityDist = qglGetUniformLocation( program->programId, "u_EntityDist" );
	program->loc.EntityOrigin = qglGetUniformLocation( program->programId, "u_EntityOrigin" );
	program->loc.EntityColor = qglGetUniformLocation( program->programId, "u_EntityColor" );
	program->loc.ConstColor = qglGetUniformLocation( program->programId, "u_ConstColor" );
	program->loc.RGBGenFuncArgs = qglGetUniformLocation( program->programId, "u_RGBGenFuncArgs" );
	program->loc.AlphaGenFuncArgs = qglGetUniformLocation( program->programId, "u_AlphaGenFuncArgs" );

	program->loc.Fog.Plane = qglGetUniformLocation( program->programId, "u_FogPlane" );
	program->loc.Fog.Color = qglGetUniformLocation( program->programId, "u_FogColor" );
	program->loc.Fog.ScaleAndEyeDist = qglGetUniformLocation( program->programId, "u_FogScaleAndEyeDist" );
	program->loc.Fog.EyePlane = qglGetUniformLocation( program->programId, "u_FogEyePlane" );

	program->loc.ShaderTime = qglGetUniformLocation( program->programId, "u_ShaderTime" );

	program->loc.ReflectionTexMatrix = qglGetUniformLocation( program->programId, "u_ReflectionTexMatrix" );
	program->loc.VectorTexMatrix = qglGetUniformLocation( program->programId, "u_VectorTexMatrix" );

	program->loc.builtin.ViewOrigin = qglGetUniformLocation( program->programId, "u_QF_ViewOrigin" );
	program->loc.builtin.ViewAxis = qglGetUniformLocation( program->programId, "u_QF_ViewAxis" );
	program->loc.builtin.MirrorSide = qglGetUniformLocation( program->programId, "u_QF_MirrorSide" );
	program->loc.builtin.EntityOrigin = qglGetUniformLocation( program->programId, "u_QF_EntityOrigin" );
	program->loc.builtin.ShaderTime = qglGetUniformLocation( program->programId, "u_QF_ShaderTime" );

	// dynamic lights
	for( i = 0; i < MAX_DLIGHTS; i++ ) {
		program->loc.DynamicLightsPosition[i] = qglGetUniformLocation( program->programId,
																		  va_r( tmp, sizeof( tmp ), "u_DlightPosition[%i]", i ) );

		if( !( i & 3 ) ) {
			// 4x4 transposed, so we can index it with `i`
			program->loc.DynamicLightsDiffuseAndInvRadius[i >> 2] =
				qglGetUniformLocation( program->programId, va_r( tmp, sizeof( tmp ), "u_DlightDiffuseAndInvRadius[%i]", i ) );
		}
	}
	program->loc.NumDynamicLights = qglGetUniformLocation( program->programId, "u_NumDynamicLights" );

	program->loc.BlendMix = qglGetUniformLocation( program->programId, "u_BlendMix" );
	program->loc.ColorMod = qglGetUniformLocation( program->programId, "u_ColorMod" );

	program->loc.SoftParticlesScale = qglGetUniformLocation( program->programId, "u_SoftParticlesScale" );

	program->loc.DualQuats = qglGetUniformLocation( program->programId, "u_DualQuats" );

	program->loc.InstancePoints = qglGetUniformLocation( program->programId, "u_InstancePoints" );

	program->loc.WallColor = qglGetUniformLocation( program->programId, "u_WallColor" );
	program->loc.FloorColor = qglGetUniformLocation( program->programId, "u_FloorColor" );

	program->loc.hdrGamma = qglGetUniformLocation( program->programId, "u_HDRGamma" );
	program->loc.hdrExposure = qglGetUniformLocation( program->programId, "u_HDRExposure" );

	if( locBaseTexture >= 0 ) {
		qglUniform1i( locBaseTexture, 0 );
	}
	if( locDuDvMapTexture >= 0 ) {
		qglUniform1i( locDuDvMapTexture, 0 );
	}

	if( locNormalmapTexture >= 0 ) {
		qglUniform1i( locNormalmapTexture, 1 );
	}
	if( locGlossTexture >= 0 ) {
		qglUniform1i( locGlossTexture, 2 );
	}
	if( locDecalTexture >= 0 ) {
		qglUniform1i( locDecalTexture, 3 );
	}
	if( locEntityDecalTexture >= 0 ) {
		qglUniform1i( locEntityDecalTexture, 4 );
	}

	if( locReflectionTexture >= 0 ) {
		qglUniform1i( locReflectionTexture, 2 );
	}
	if( locRefractionTexture >= 0 ) {
		qglUniform1i( locRefractionTexture, 3 );
	}

	if( locCelShadeTexture >= 0 ) {
		qglUniform1i( locCelShadeTexture, 1 );
	}
	if( locDiffuseTexture >= 0 ) {
		qglUniform1i( locDiffuseTexture, 2 );
	}
	if( locStripesTexture >= 0 ) {
		qglUniform1i( locStripesTexture, 5 );
	}
	if( locCelLightTexture >= 0 ) {
		qglUniform1i( locCelLightTexture, 6 );
	}

	if( locDepthTexture >= 0 ) {
		qglUniform1i( locDepthTexture, 3 );
	}

	for( i = 0; i < MAX_LIGHTMAPS && locLightmapTexture[i] >= 0; i++ )
		qglUniform1i( locLightmapTexture[i], i + 4 );

	if( locYUVTextureY >= 0 ) {
		qglUniform1i( locYUVTextureY, 0 );
	}
	if( locYUVTextureU >= 0 ) {
		qglUniform1i( locYUVTextureU, 1 );
	}
	if( locYUVTextureV >= 0 ) {
		qglUniform1i( locYUVTextureV, 2 );
	}

	if( locColorLUT >= 0 ) {
		qglUniform1i( locColorLUT, 1 );
	}

	// TODO: Flush GL errors, if any?
	(void)qglGetError();
}

bool ShaderProgramCache::bindAttributeLocations( GLuint programId ) {
	qglBindAttribLocation( programId, VATTRIB_POSITION, "a_Position" );
	qglBindAttribLocation( programId, VATTRIB_SVECTOR, "a_SVector" );
	qglBindAttribLocation( programId, VATTRIB_NORMAL, "a_Normal" );
	qglBindAttribLocation( programId, VATTRIB_COLOR0, "a_Color" );
	qglBindAttribLocation( programId, VATTRIB_TEXCOORDS, "a_TexCoord" );

	qglBindAttribLocation( programId, VATTRIB_SPRITEPOINT, "a_SpritePoint" );
	qglBindAttribLocation( programId, VATTRIB_SVECTOR, "a_SpriteRightUpAxis" );

	qglBindAttribLocation( programId, VATTRIB_BONESINDICES, "a_BonesIndices" );
	qglBindAttribLocation( programId, VATTRIB_BONESWEIGHTS, "a_BonesWeights" );

	qglBindAttribLocation( programId, VATTRIB_LMCOORDS01, "a_LightmapCoord01" );
	qglBindAttribLocation( programId, VATTRIB_LMCOORDS23, "a_LightmapCoord23" );

	qglBindAttribLocation( programId, VATTRIB_LMLAYERS0123, "a_LightmapLayer0123" );

	qglBindAttribLocation( programId, VATTRIB_INSTANCE_QUAT, "a_InstanceQuat" );
	qglBindAttribLocation( programId, VATTRIB_INSTANCE_XYZS, "a_InstancePosAndScale" );

	if( glConfig.shadingLanguageVersion >= 130 ) {
		qglBindFragDataLocation( programId, 0, "qf_FragColor" );
		qglBindFragDataLocation( programId, 1, "qf_BrightColor" );
	}

	return true;
}
