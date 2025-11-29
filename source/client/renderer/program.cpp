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
#include <common/helpers/freelistallocator.h>
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
	auto findProgramForParams( int type, const wsw::StringView &maybeRequestedName, uint64_t features = 0,
							   const DeformSig &deformSig = DeformSig(),
							   std::span<const deformv_t> deforms = {} ) -> int;

	[[nodiscard]]
	auto findProgramForParams( int type, const char *maybeName, uint64_t features = 0,
							   const DeformSig &deformSig = DeformSig(),
							   std::span<const deformv_t> deforms = {} ) -> int {
		return findProgramForParams( type, wsw::StringView( maybeName ? maybeName : "" ), features, deformSig, deforms );
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
	auto findProgramInBin( int type, unsigned binIndex, uint64_t features, const DeformSig &deformSig ) const -> int;

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
								 const wsw::StringView &version,
								 const std::span<const wsw::StringView> &additionalConstants );

	[[nodiscard]]
	bool loadFragmentShaderSource( GLuint id, const wsw::StringView &name, int type, uint64_t features,
								   const SpanOfSourceStrings &featureStrings,
								   const wsw::StringView &version,
								   const std::span<const wsw::StringView> &additionalConstants );

	[[nodiscard]]
	bool compileShader( GLuint id, const char *kind, std::span<const char *> strings, std::span<int> lengths );

	[[nodiscard]]
	bool bindAttributeLocations( GLuint programId );

	[[nodiscard]]
	bool linkProgram( GLuint programId, GLuint vertexShaderId, GLuint fragmentShaderId );
	
	static void setupUniformsAndLocations( GLuint programId );

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

	void addAll( std::span<const wsw::StringView> strings ) {
		for( const wsw::StringView &string: strings ) {
			add( string.data(), string.size() );
		}
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

[[nodiscard]]
static inline auto getBinIndexForFeatures( uint64_t features ) -> unsigned {
	// TODO: Shuffle bits better
	const auto featureWord1 = (uint32_t)( features >> 32 );
	const auto featureWord2 = (uint32_t)( features & 0xFFFF'FFFFu );
	const auto featureHash  = featureWord1 ^ featureWord2;
	const auto binIndex     = featureHash % GLSL_PROGRAMS_HASH_SIZE;
	return binIndex;
}

auto ShaderProgramCache::findProgramInBin( int type, unsigned binIndex, uint64_t features, const DeformSig &deformSig ) const -> int {
	for( ShaderProgram *program = m_hashBinsForType[type][binIndex]; program; program = program->nextInHashBin ) {
		if( program->features == features && program->deformSig == deformSig ) {
			return (int)( program->index + 1 );
		}
	}
	return 0;
}

auto ShaderProgramCache::findProgramForParams( int type, const wsw::StringView &maybeRequestedName, uint64_t features,
											   const DeformSig &deformSig, std::span<const deformv_t> deforms ) -> int {
	//assert( qglGetError() == GL_NO_ERROR );
	// TODO: Convert to an assertion?
	if( type <= GLSL_PROGRAM_TYPE_NONE || type >= GLSL_PROGRAM_TYPE_MAXTYPE ) {
		return 0;
	}

	return findProgramInBin( type, getBinIndexForFeatures( features ), features, deformSig );
}

auto ShaderProgramCache::getProgramForParams( int type, const wsw::StringView &maybeRequestedName, uint64_t features,
											  const DeformSig &deformSig, std::span<const deformv_t> deforms ) -> int {
	//assert( qglGetError() == GL_NO_ERROR );
	// TODO: Convert to an assertion?
	if( type <= GLSL_PROGRAM_TYPE_NONE || type >= GLSL_PROGRAM_TYPE_MAXTYPE ) {
		return 0;
	}

	const unsigned binIndex = getBinIndexForFeatures( features );

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
	setupUniformsAndLocations( program->programId );
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
	maxBones << "#define MAX_UNIFORM_BONES "_asView << MAX_GLSL_UNIFORM_BONES << "\n"_asView;

	wsw::StaticString<64> maxDlights;
	maxDlights << "#define MAX_DLIGHTS "_asView << MAX_DLIGHTS << "\n"_asView;

	std::initializer_list<wsw::StringView> additionalConstants { maxBones.asView(), maxDlights.asView() };

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
								 spanOfFeatureStrings, shaderVersion.asView(), additionalConstants ) ) {
		return false;
	}

	if( !loadFragmentShaderSource( fragmentShaderId, name, type, features,
								   spanOfFeatureStrings, shaderVersion.asView(), additionalConstants ) ) {
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
												 const wsw::StringView &shaderVersion,
												 const std::span<const wsw::StringView> &additionalConstants ) {
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
	sourceBuilder.addAll( additionalConstants );
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
		sourceBuilder.add( "layout (std140) uniform BonesBlock { vec4 u_DualQuats[MAX_UNIFORM_BONES*2]; };\n" );

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
												   const wsw::StringView &shaderVersion,
												   const std::span<const wsw::StringView> &additionalConstants ) {
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
	sourceBuilder.addAll( additionalConstants );
	sourceBuilder.addAll( g_programSourceFileCache.uniforms, g_programSourceFileCache );

	sourceBuilder.addAll( g_programSourceFileCache.math, g_programSourceFileCache );

	sourceBuilder.addAll( featureStrings.strings, featureStrings.lengths );

	char fileName[1024];
	Q_snprintfz( fileName, sizeof( fileName ), "glsl/%s.frag.glsl", name.data() );

	ProgramSourceLoader fragmentShaderLoader( &g_programSourceFileCache, &m_tmpShaderStrings, &m_tmpShaderLengths );
	if( !fragmentShaderLoader.load( wsw::StringView( fileName ), features, type ) ) {
		rError() << "Failed to load source of" << wsw::StringView( fileName );
		return false;
	}

	if( !compileShader( id, "fragment", m_tmpShaderStrings, m_tmpShaderLengths ) ) {
		rError() << "Failed to compile" << wsw::StringView( fileName );
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

int RP_GetProgram( int type, const shader_s *materialToGetDeforms, uint64_t features ) {
	std::span<const deformv_t> deforms = { materialToGetDeforms->deforms, materialToGetDeforms->numdeforms };
	return g_programCacheInstanceHolder.instance()->getProgramForParams( type, nullptr, features, materialToGetDeforms->deformSig, deforms );
}

int RP_FindProgram( int type, const shader_s *materialToGetDeforms, uint64_t features ) {
	std::span<const deformv_t> deforms = { materialToGetDeforms->deforms, materialToGetDeforms->numdeforms };
	return g_programCacheInstanceHolder.instance()->findProgramForParams( type, nullptr, features, materialToGetDeforms->deformSig, deforms );
}

int RP_GetProgramObject( int elem ) {
	if( ShaderProgram *program = g_programCacheInstanceHolder.instance()->getProgramById( elem ) ) {
		return (int)program->programId;
	}
	return 0;
}

struct UniformBlock {
	enum class Binding : unsigned {
		DeformBuiltin,
		View,
		Shader,
		DiffuseLight,
		DeluxeMap,
		Material,
		DynamicLight,
		Outline,
		Bones,
		Fog,
		TextureParams,
		ColorCorrection,
		DrawFlat,
		SoftParticles,
		BlendMix,
		TexGen,
		Distortion,
	};

	// Ensure that we can address the array of streams by a binding value
	static_assert( (unsigned)Binding::Distortion + 1 == MAX_UNIFORM_BINDINGS );

	struct alignas( 16 ) DeformBuiltin {
		static constexpr unsigned kBinding = (unsigned)Binding::DeformBuiltin;

		alignas( 16 ) float viewOrigin[3];
		// mat3x3
		alignas( 16 ) float viewAxis[12];
		alignas( 16 ) float entityOrigin[3];

		// Follows a vec3, no alignment is required
		float shaderTime;
		float mirrorSide;
	};

	struct alignas( 16 ) View {
		static constexpr unsigned kBinding = (unsigned)Binding::View;

		alignas( 16 ) float modelViewMatrix[16];
		alignas( 16 ) float modelViewProjectionMatrix[16];
		alignas( 16 ) float viewOrigin[3];
		// mat3x3
		alignas( 16 ) float viewAxis[12];
		alignas( 16 ) int32_t viewport[4];

		alignas( 8 ) float zRange[2];
		// Follows a vec2, so no alignment is required
		float mirrorSide;
	};

	struct alignas( 16 ) Shader {
		static constexpr unsigned kBinding = (unsigned)Binding::Shader;

		alignas( 16 ) float entityOrigin[3];
		alignas( 16 ) float entityDist[3];
		alignas( 16 ) float entityColor[4];
		alignas( 16 ) float constColor[4];
		alignas( 16 ) float rgbGenFuncArgs[4];
		alignas( 16 ) float alphaGenFuncArgs[4];
		alignas( 16 ) float textureMatrix[8];

		float colorMod;
		float shaderTime;
	};

	struct alignas( 16 ) DiffuseLight {
		static constexpr unsigned kBinding = (unsigned)Binding::DiffuseLight;

		alignas( 16 ) float lightDir[3];
		alignas( 16 ) float lightAmbient[3];
		alignas( 16 ) float lightDiffuse[3];
	};

	struct alignas( 16 ) Material {
		static constexpr unsigned kBinding = (unsigned)Binding::Material;

		alignas( 8 ) float glossFactors[2];
		float offsetMappingScale;
	};

	struct alignas( 16 ) DeluxeMap {
		static constexpr unsigned kBinding = (unsigned)Binding::DeluxeMap;

		alignas( 16 ) float deluxeMapOffset[4];
		alignas( 16 ) float lightstyleColor[16];
	};

	struct alignas( 16 ) DynamicLight {
		static constexpr unsigned kBinding = (unsigned)Binding::DynamicLight;

		alignas( 16 ) float dynamicLightsPosition[4 * MAX_DLIGHTS];
		// One 4-component vector per light, grouped by 4 and transposed
		alignas( 16 ) float dynamicLightsDiffuseAndInvRadius[4 * MAX_DLIGHTS];

		// TODO: Should we use dynamic branching at all?
		alignas( 16 ) int32_t numDynamicLights;
	};

	struct alignas( 16 ) Outline {
		static constexpr unsigned kBinding = (unsigned)Binding::Outline;

		alignas( 16 ) float outlineHeight;
		float outlineCutoff;
	};

	struct alignas( 16 ) Bones {
		static constexpr unsigned kBinding = (unsigned)Binding::Bones;

		alignas( 16 ) float dualQuats[8 * MAX_GLSL_UNIFORM_BONES];
	};

	struct alignas( 16 ) Fog {
		static constexpr unsigned kBinding = (unsigned)Binding::Fog;

		alignas( 16 ) float eyePlane[4];
		alignas( 16 ) float plane[4];
		alignas( 16 ) float color[3];
		alignas( 16 ) float scaleAndEyeDist[2];
	};

	struct alignas( 16 ) TextureParams {
		static constexpr unsigned kBinding = (unsigned)Binding::TextureParams;

		// width, height, rcpWidth, rcpHeight
		// TODO: Do we need this in modern GL versions?
		alignas( 16 ) float textureParams[4];
	};

	struct alignas( 16 ) ColorCorrection {
		static constexpr unsigned kBinding = (unsigned)Binding::ColorCorrection;

		alignas( 16 ) float hdrGamma;
		float hdrExposure;
	};

	struct alignas( 16 ) DrawFlat {
		static constexpr unsigned kBinding = (unsigned)Binding::DrawFlat;

		alignas( 16 ) float wallColor[3];
		alignas( 16 ) float floorColor[3];
	};

	struct alignas( 16 ) SoftParticles {
		static constexpr unsigned kBinding = (unsigned)Binding::SoftParticles;

		alignas( 16 ) float softParticlesScale;
	};

	struct alignas( 16 ) BlendMix {
		static constexpr unsigned kBinding = (unsigned)Binding::BlendMix;

		alignas( 16 ) float blendMix[2];
	};

	struct alignas( 16 ) TexGen {
		static constexpr unsigned kBinding = (unsigned)Binding::TexGen;

		// mat3x3
		alignas( 16 ) float reflectionTexMatrix[12];
		alignas( 16 ) float vectorTexMatrix[16];
	};

	struct alignas( 16 ) Distortion {
		static constexpr unsigned kBinding = (unsigned)Binding::Distortion;

		alignas( 16 ) float frontPlane;
	};
};

template <typename Block>
auto allocUniformBlock( SimulatedBackendState *backendState ) -> Block * {
	return (Block *)RB_GetTmpUniformBlock( backendState, Block::kBinding, sizeof( Block ) );
}

template <typename Block>
void commitUniformBlock( SimulatedBackendState *backendState, Block *block ) {
	RB_CommitUniformBlock( backendState, Block::kBinding, block, sizeof( Block ) );
}

static inline void copyMat3ToStd140Layout( const mat3_t from, float *to ) {
	VectorCopy( from + 0, to + 0 );
	VectorCopy( from + 3, to + 4 );
	VectorCopy( from + 6, to + 8 );
}

static inline void copyMat4ToStd140Layout( const mat4_t from, float *to ) {
	Matrix4_Copy( from, to );
}

void RP_UpdateShaderUniforms( SimulatedBackendState *backendState, float shaderTime,
							  const vec3_t entOrigin, const vec3_t entDist, const uint8_t *entityColor,
							  const uint8_t *constColor, const float *rgbGenFuncArgs, const float *alphaGenFuncArgs,
							  const mat4_t texMatrix, float colorMod ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::Shader>( backendState );

	if( entOrigin ) {
		VectorCopy( entOrigin, block->entityOrigin );
	}

	if( entDist ) {
		VectorCopy( entDist, block->entityDist );
	}
	if( entityColor ) {
		Vector4Set( block->entityColor, entityColor[0] * 1.0 / 255.0, entityColor[1] * 1.0 / 255.0, entityColor[2] * 1.0 / 255.0, entityColor[3] * 1.0 / 255.0 );
	}

	block->shaderTime = shaderTime;

	if( constColor ) {
		Vector4Set( block->constColor, constColor[0] * 1.0 / 255.0, constColor[1] * 1.0 / 255.0, constColor[2] * 1.0 / 255.0, constColor[3] * 1.0 / 255.0 );
	}
	if( rgbGenFuncArgs ) {
		Vector4Copy( rgbGenFuncArgs, block->rgbGenFuncArgs );
	}
	if( alphaGenFuncArgs ) {
		Vector4Copy( alphaGenFuncArgs, block->alphaGenFuncArgs );
	}

	block->textureMatrix[0] = texMatrix[0], block->textureMatrix[1] = texMatrix[4];
	block->textureMatrix[2] = texMatrix[1], block->textureMatrix[3] = texMatrix[5];
	block->textureMatrix[4] = texMatrix[12], block->textureMatrix[5] = texMatrix[13];

	block->colorMod = colorMod;

	commitUniformBlock( backendState, block );
}

void RP_UpdateViewUniforms( SimulatedBackendState *backendState, const mat4_t modelviewMatrix, const mat4_t modelviewProjectionMatrix,
							const vec3_t viewOrigin, const mat3_t viewAxis,
							float mirrorSide,
							const int *viewport,
							float zNear, float zFar ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::View>( backendState );

	copyMat4ToStd140Layout( modelviewMatrix, block->modelViewMatrix );
	copyMat4ToStd140Layout( modelviewProjectionMatrix, block->modelViewProjectionMatrix );

	block->zRange[0] = zNear, block->zRange[1] = zFar;

	if( viewOrigin ) {
		VectorCopy( viewOrigin, block->viewOrigin );
	}

	if( viewAxis ) {
		copyMat3ToStd140Layout( viewAxis, block->viewAxis );
	}

	Vector4Copy( viewport, block->viewport );

	block->mirrorSide = mirrorSide;

	commitUniformBlock( backendState, block );
}

void RP_UpdateDeformBuiltinUniforms( SimulatedBackendState *backendState, float shaderTime, const vec3_t viewOrigin,
									 const mat3_t viewAxis, const vec3_t entOrigin, float mirrorSide ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::DeformBuiltin>( backendState );

	if( viewOrigin ) {
		VectorCopy( viewOrigin, block->viewOrigin );
	}
	if( viewAxis ) {
		copyMat3ToStd140Layout( viewAxis, block->viewAxis );
	}
	if( entOrigin ) {
		VectorCopy( entOrigin, block->entityOrigin );
	}

	block->shaderTime = shaderTime;
	block->mirrorSide = mirrorSide;

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateBlendMixUniform
*
* The first component corresponds to RGB, the second to ALPHA.
* Whenever the program needs to scale source colors, the mask needs
* to be used in the following manner:
* color *= mix(myhalf4(1.0), myhalf4(scale), u_BlendMix.xxxy);
*/
void RP_UpdateBlendMixUniforms( SimulatedBackendState *backendState, vec2_t blendMix ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::BlendMix>( backendState );

	Vector2Copy( blendMix, block->blendMix );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateSoftParticlesUniforms
*/
void RP_UpdateSoftParticlesUniforms( SimulatedBackendState *backendState, float scale ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::SoftParticles>( backendState );

	block->softParticlesScale = scale;

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateDiffuseLightUniforms
*/
void RP_UpdateDiffuseLightUniforms( SimulatedBackendState *backendState, const vec3_t lightDir, const vec4_t lightAmbient, const vec4_t lightDiffuse ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::DiffuseLight>( backendState );

	if( lightDir ) {
		VectorCopy( lightDir, block->lightDir );
	}
	if( lightAmbient ) {
		// TODO: Is alpha unused
		VectorCopy( lightAmbient, block->lightAmbient );
	}
	if( lightDiffuse ) {
		// TODO: Is alpha unused
		VectorCopy( lightDiffuse, block->lightDiffuse );
	}

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateMaterialUniforms
*/
void RP_UpdateMaterialUniforms( SimulatedBackendState *backendState, float offsetmappingScale, float glossIntensity, float glossExponent ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::Material>( backendState );

	block->glossFactors[0] = glossIntensity;
	block->glossFactors[1] = glossExponent;
	block->offsetMappingScale = offsetmappingScale;

	commitUniformBlock( backendState, block );
}

void RP_UpdateDistortionUniforms( SimulatedBackendState *backendState, bool frontPlane ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::Distortion>( backendState );

	block->frontPlane = frontPlane ? +1.0f : -1.0f;

	commitUniformBlock( backendState, block );
}

void RP_UpdateTextureUniforms( SimulatedBackendState *backendState, int TexWidth, int TexHeight ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::TextureParams>( backendState );

	Vector4Set( block->textureParams, TexWidth, TexHeight, TexWidth ? 1.0 / TexWidth : 1.0, TexHeight ? 1.0 / TexHeight : 1.0 );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateOutlineUniforms
*/
void RP_UpdateOutlineUniforms( SimulatedBackendState *backendState, float projDistance ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::Outline>( backendState );

	block->outlineHeight = projDistance;
	block->outlineCutoff = wsw::max( 0.0f, v_outlinesCutoff.get() );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateFogUniforms
*/
void RP_UpdateFogUniforms( SimulatedBackendState *backendState, byte_vec4_t color, float clearDist, float opaqueDist,
						   cplane_t *fogPlane, cplane_t *eyePlane, float eyeDist ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::Fog>( backendState );

	VectorScale( color, ( 1.0 / 255.0 ), block->color );
	Vector2Set( block->scaleAndEyeDist, 1.0 / ( opaqueDist - clearDist ), eyeDist );
	Vector4Set( block->plane, fogPlane->normal[0], fogPlane->normal[1], fogPlane->normal[2], fogPlane->dist );
	Vector4Set( block->eyePlane, eyePlane->normal[0], eyePlane->normal[1], eyePlane->normal[2], eyePlane->dist );

	commitUniformBlock( backendState, block );
}

void RP_UpdateDynamicLightsUniforms( SimulatedBackendState *backendState, const FrontendToBackendShared *fsh,
									 const superLightStyle_t *superLightStyle,
									 const vec3_t entOrigin, const mat3_t entAxis, unsigned int dlightbits ) {

	if( superLightStyle ) {
		float deluxemapOffset[( MAX_LIGHTMAPS + 3 ) & ( ~3 )];
		static_assert( MAX_LIGHTMAPS <= 4 );

		auto *const __restrict block = allocUniformBlock<UniformBlock::DeluxeMap>( backendState );

		int i = 0;
		for( ; i < MAX_LIGHTMAPS && superLightStyle->lightmapStyles[i] != 255; i++ ) {
			VectorCopy( lightStyles[superLightStyle->lightmapStyles[i]].rgb, &block->lightstyleColor[4 * i] );

			deluxemapOffset[i] = superLightStyle->stOffset[i][0];
		}

		if( i ) {
			assert( ( ( i + 3 ) / 4 ) == 1 );
			//qglUniform4fv( program->loc.DeluxemapOffset, ( i + 3 ) / 4, deluxemapOffset );
			Vector4Copy( deluxemapOffset, block->deluxeMapOffset );
		}

		commitUniformBlock( backendState, block );
	}

	if( dlightbits ) {
		const bool identityAxis = Matrix3_Compare( entAxis, axis_identity );

		auto *const __restrict block = allocUniformBlock<UniformBlock::DynamicLight>( backendState );

		vec4_t shaderColor[4];
		memset( shaderColor, 0, sizeof( vec4_t ) * 3 );
		Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
		int numAddedLights = 0;
		unsigned offsetInFloadElems = 0;

		for( unsigned i = 0; i < fsh->visibleProgramLightIndices.size(); ++i ) {
			const unsigned lightBit = 1 << i;
			if( !( dlightbits & lightBit ) ) {
				continue;
			}

			dlightbits &= ~lightBit;

			const auto *const light = fsh->dynamicLights + fsh->visibleProgramLightIndices[i];
			assert( light->hasProgramLight && light->programRadius >= 1.0f );

			vec3_t dlorigin;
			VectorSubtract( light->origin, entOrigin, dlorigin );
			if( !identityAxis ) {
				vec3_t tvec;
				VectorCopy( dlorigin, tvec );
				Matrix3_TransformVector( entAxis, tvec, dlorigin );
			}

			// Each origin element is laid out as vec4
			VectorCopy( dlorigin, &block->dynamicLightsPosition[4 * i] );
			//qglUniform3fv( program->loc.DynamicLightsPosition[n], 1, dlorigin );

			const int component       = numAddedLights & 3;
			shaderColor[0][component] = light->color[0];
			shaderColor[1][component] = light->color[1];
			shaderColor[2][component] = light->color[2];
			shaderColor[3][component] = Q_Rcp( light->programRadius );

			// DynamicLightsDiffuseAndInvRadius is transposed for SIMD, but it's still 4x4
			if( component == 3 ) {
				memcpy( block->dynamicLightsDiffuseAndInvRadius + offsetInFloadElems, shaderColor, 4 * sizeof( vec4_t ) );
				//qglUniform4fv( program->loc.DynamicLightsDiffuseAndInvRadius[n >> 2], 4, shaderColor[0] );
				memset( shaderColor, 0, sizeof( vec4_t ) * 3 );
				Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
				// We've copied 16 float values
				offsetInFloadElems += 16;
			}

			numAddedLights++;

			dlightbits &= ~lightBit;
			if( !dlightbits ) {
				break;
			}
		}

		if( numAddedLights & 3 ) {
			memcpy( block->dynamicLightsDiffuseAndInvRadius + offsetInFloadElems, shaderColor, 4 * sizeof( vec4_t ) );
			//qglUniform4fv( program->loc.DynamicLightsDiffuseAndInvRadius[n >> 2], 4, shaderColor[0] );
			memset( shaderColor, 0, sizeof( vec4_t ) * 3 ); // to set to zero for the remaining lights
			Vector4Set( shaderColor[3], 1.0f, 1.0f, 1.0f, 1.0f );
			numAddedLights = ALIGN( numAddedLights, 4 );
			// We've copied 16 float values
			offsetInFloadElems += 16;
		}

		for( ; numAddedLights < MAX_DLIGHTS; numAddedLights += 4 ) {
			memcpy( block->dynamicLightsDiffuseAndInvRadius + offsetInFloadElems, shaderColor, 4 * sizeof( vec4_t ) );
			offsetInFloadElems += 16;
		}

		block->numDynamicLights = numAddedLights;

		commitUniformBlock( backendState, block );
	}
}

/*
* RP_UpdateTexGenUniforms
*/
void RP_UpdateTexGenUniforms( SimulatedBackendState *backendState, const mat4_t reflectionMatrix, const mat4_t vectorMatrix ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::TexGen>( backendState );

	mat3_t m;
	VectorCopy( reflectionMatrix + 0, m + 0 );
	VectorCopy( reflectionMatrix + 4, m + 3 );
	VectorCopy( reflectionMatrix + 8, m + 6 );
	copyMat3ToStd140Layout( m, block->reflectionTexMatrix );
	copyMat4ToStd140Layout( vectorMatrix, block->vectorTexMatrix );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateBonesUniforms
*
* Set uniform values for animation dual quaternions
*/
void RP_UpdateBonesUniforms( SimulatedBackendState *backendState, unsigned numBones, dualquat_t *animDualQuat ) {
	assert( numBones <= MAX_GLSL_UNIFORM_BONES );

	auto *const __restrict block = allocUniformBlock<UniformBlock::Bones>( backendState );

	// TODO: We don't need delta-comparison for anim data as it's unique for all models, do we need?

	memcpy( block->dualQuats, animDualQuat, 8 * sizeof( float ) * numBones );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateColorCorrectionUniforms
*/
void RP_UpdateColorCorrectionUniforms( SimulatedBackendState *backendState, float hdrGamma, float hdrExposure ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::ColorCorrection>( backendState );

	block->hdrGamma = hdrGamma;
	block->hdrExposure = hdrExposure;

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateDrawFlatUniforms
*/
void RP_UpdateDrawFlatUniforms( SimulatedBackendState *backendState, const vec3_t wallColor, const vec3_t floorColor ) {
	auto *const __restrict block = allocUniformBlock<UniformBlock::DrawFlat>( backendState );

	VectorCopy( wallColor, block->wallColor );
	VectorCopy( floorColor, block->floorColor );

	commitUniformBlock( backendState, block );
}

/*
* RP_UpdateKawaseUniforms
*/
void RP_UpdateKawaseUniforms( SimulatedBackendState *backendState, int TexWidth, int TexHeight, int iteration ) {
	auto *const block = allocUniformBlock<UniformBlock::TextureParams>( backendState );

	Vector4Set( block->textureParams, TexWidth ? 1.0 / TexWidth : 1.0, TexHeight ? 1.0 / TexHeight : 1.0, (float)iteration, 1.0 );

	commitUniformBlock( backendState, block );
}

void ShaderProgramCache::setupUniformsAndLocations( GLuint programId ) {
	const auto bindSamplerUniform = []( GLuint programId, const char *name, int value ) {
		if( const int location = qglGetUniformLocation( programId, name ); location >= 0 ) {
			qglUniform1i( location, value );
		}
	};

	bindSamplerUniform( programId, "u_BaseTexture", 0 );
	bindSamplerUniform( programId, "u_NormalmapTexture", 1 );
	bindSamplerUniform( programId, "u_GlossTexture", 2 );
	bindSamplerUniform( programId, "u_DecalTexture", 3 );
	bindSamplerUniform( programId, "u_EntityDecalTexture", 4 );

	bindSamplerUniform( programId, "u_DuDvMapTexture", 0 );
	bindSamplerUniform( programId, "u_ReflectionTexture", 2 );
	bindSamplerUniform( programId, "u_RefractionTexture", 3 );

	bindSamplerUniform( programId, "u_CelShadeTexture", 1 );
	bindSamplerUniform( programId, "u_CelLightTexture", 6 );
	bindSamplerUniform( programId, "u_DiffuseTexture", 2 );
	bindSamplerUniform( programId, "u_StripesTexture", 5 );

	bindSamplerUniform( programId, "u_DepthTexture", 3 );

	bindSamplerUniform( programId, "u_YUVTextureY", 0 );
	bindSamplerUniform( programId, "u_YUVTextureU", 1 );
	bindSamplerUniform( programId, "u_YUVTextureV", 2 );

	bindSamplerUniform( programId, "u_ColorLUT", 1 );

	if( const int location = qglGetUniformLocation( programId, "u_LightingIntensity" ); location >= 0 ) {
		qglUniform1f( location, 1.0f );
	}

	for( int i = 0; i < MAX_LIGHTMAPS; i++ ) {
		char tmp[1024];
		// arrays of samplers are broken on ARM Mali so get u_LightmapTexture%i instead of u_LightmapTexture[%i]
		const char *name = va_r( tmp, sizeof( tmp ), "u_LightmapTexture%i", i );
		if( const int location = qglGetUniformLocation( programId, name ); location >= 0 ) {
			qglUniform1i( location, i + 4 );
		} else {
			break;
		}
	}

	const auto bindBlock = []( GLuint programId, const char *name, unsigned binding ) {
		if( const GLuint index = qglGetUniformBlockIndex( programId, name ); index != GL_INVALID_INDEX ) {
			qglUniformBlockBinding( programId, index, binding );
		}
	};

	bindBlock( programId, "DeformBuiltinBlock", UniformBlock::DeformBuiltin::kBinding );
	bindBlock( programId, "ViewBlock", UniformBlock::View::kBinding );
	bindBlock( programId, "ShaderBlock", UniformBlock::Shader::kBinding );
	bindBlock( programId, "DiffuseLightBlock", UniformBlock::DiffuseLight::kBinding );
	bindBlock( programId, "DeluxeMapBlock", UniformBlock::DeluxeMap::kBinding );
	bindBlock( programId, "DynamicLightBlock", UniformBlock::DynamicLight::kBinding );
	bindBlock( programId, "MaterialBlock", UniformBlock::Material::kBinding );
	bindBlock( programId, "OutlineBlock", UniformBlock::Outline::kBinding );
	bindBlock( programId, "FogBlock", UniformBlock::Fog::kBinding );
	bindBlock( programId, "TextureParamsBlock", UniformBlock::TextureParams::kBinding );
	bindBlock( programId, "ColorCorrectionBlock", UniformBlock::ColorCorrection::kBinding );
	bindBlock( programId, "DrawFlatBlock", UniformBlock::DrawFlat::kBinding );
	bindBlock( programId, "SoftParticlesBlock", UniformBlock::SoftParticles::kBinding );
	bindBlock( programId, "BlendMixBlock", UniformBlock::BlendMix::kBinding );
	bindBlock( programId, "TexGenBlock", UniformBlock::TexGen::kBinding );
	bindBlock( programId, "DistortionBlock", UniformBlock::Distortion::kBinding );
	bindBlock( programId, "BonesBlock", UniformBlock::Bones::kBinding );

	// TODO: Flush GL errors, if any?
	(void)qglGetError();
}

void RP_GetSizeOfUniformBlocks( unsigned *sizeOfBlocks ) {
	std::memset( sizeOfBlocks, 0, sizeof( *sizeOfBlocks ) * MAX_UNIFORM_BINDINGS );

	sizeOfBlocks[UniformBlock::DeformBuiltin::kBinding]   = sizeof( UniformBlock::DeformBuiltin );
	sizeOfBlocks[UniformBlock::View::kBinding]            = sizeof( UniformBlock::View );
	sizeOfBlocks[UniformBlock::Shader::kBinding]          = sizeof( UniformBlock::Shader );
	sizeOfBlocks[UniformBlock::DiffuseLight::kBinding]    = sizeof( UniformBlock::DiffuseLight );
	sizeOfBlocks[UniformBlock::DeluxeMap::kBinding]       = sizeof( UniformBlock::DeluxeMap );
	sizeOfBlocks[UniformBlock::DynamicLight::kBinding]    = sizeof( UniformBlock::DynamicLight );
	sizeOfBlocks[UniformBlock::Material::kBinding]        = sizeof( UniformBlock::Material );
	sizeOfBlocks[UniformBlock::Outline::kBinding]         = sizeof( UniformBlock::Outline );
	sizeOfBlocks[UniformBlock::Fog::kBinding]             = sizeof( UniformBlock::Fog );
	sizeOfBlocks[UniformBlock::TextureParams::kBinding]   = sizeof( UniformBlock::TextureParams );
	sizeOfBlocks[UniformBlock::ColorCorrection::kBinding] = sizeof( UniformBlock::ColorCorrection );
	sizeOfBlocks[UniformBlock::DrawFlat::kBinding]        = sizeof( UniformBlock::DrawFlat );
	sizeOfBlocks[UniformBlock::SoftParticles::kBinding]   = sizeof( UniformBlock::SoftParticles );
	sizeOfBlocks[UniformBlock::BlendMix::kBinding]        = sizeof( UniformBlock::BlendMix );
	sizeOfBlocks[UniformBlock::TexGen::kBinding]          = sizeof( UniformBlock::TexGen );
	sizeOfBlocks[UniformBlock::Distortion::kBinding]      = sizeof( UniformBlock::Distortion );
	sizeOfBlocks[UniformBlock::Bones::kBinding]           = sizeof( UniformBlock::Bones );

	for( unsigned i = 0; i < MAX_UNIFORM_BINDINGS; ++i ) {
		assert( sizeOfBlocks[i] );
		// TODO: Use generic alignment facilities
		if( const auto rem = sizeOfBlocks[i] % 16 ) {
			sizeOfBlocks[i] += 16 - rem;
		}
	}
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
