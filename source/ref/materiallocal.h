#ifndef WSW_0778b312_c4f6_4400_bcec_132900009562_H
#define WSW_0778b312_c4f6_4400_bcec_132900009562_H

#include "../common/q_shared.h"
#include "../common/memspecbuilder.h"
#include "../common/wswtonum.h"
#include "../common/common.h"

#include "vattribs.h"
#include "shader.h"
#include "glimp.h"
#include "../common/wswstaticvector.h"
#include "../common/stringspanstorage.h"
#include "../common/freelistallocator.h"

#include <optional>
#include <span>

using wsw::operator""_asView;

enum class PassKey {
	RgbGen,
	BlendFunc,
	DepthFunc,
	DepthWrite,
	AlphaFunc,
	TCMod,
	Map,
	AnimMap,
	CubeMap,
	ShadeCubeMap,
	ClampMap,
	AnimClampMap,
	TimelineMap,
	TimelineClampMap,
	TimelineMap2,
	TimelineClampMap2,
	Material,
	Distortion,
	CelShade,
	TCGen,
	AlphaGen,
	Detail,
	Grayscale,
	Skip,
};

enum class Deform {
	Wave = DEFORMV_WAVE,
	Bulge = DEFORMV_BULGE,
	Move = DEFORMV_MOVE,
	Autosprite = DEFORMV_AUTOSPRITE,
	Autosprite2 = DEFORMV_AUTOSPRITE2,
	Autoparticle = DEFORMV_AUTOPARTICLE,
};

enum class Func {
	Sin = SHADER_FUNC_SIN,
	Triangle = SHADER_FUNC_TRIANGLE,
	Square = SHADER_FUNC_SQUARE,
	Sawtooth = SHADER_FUNC_SAWTOOTH,
	InvSawtooth = SHADER_FUNC_INVERSESAWTOOTH,
	Noize = SHADER_FUNC_NOISE,
	DistanceRamp = SHADER_FUNC_RAMP,
};

enum class IntConditionVar {
	MaxTextureSize,
	MaxTextureCubemapSize,
	MaxTextureUnits,
};

enum class BoolConditionVar {
	TextureCubeMap,
	Glsl,
	DeluxeMaps,
	PortalMaps,
};

enum class LogicOp {
	And,
	Or
};

enum class CmpOp {
	LS,
	LE,
	GT,
	GE,
	NE,
	EQ
};

enum class CullMode {
	None,
	Back,
	Front
};

enum class SortMode {
	Portal = SHADER_SORT_PORTAL,
	Sky = SHADER_SORT_SKY,
	Opaque = SHADER_SORT_OPAQUE,
	Banner = SHADER_SORT_BANNER,
	Underwater = SHADER_SORT_UNDERWATER,
	Additive = SHADER_SORT_ADDITIVE,
	Nearest = SHADER_SORT_NEAREST
};

enum class MaterialKey {
	Cull,
	SkyParams,
	SkyParams2,
	SkyParamsSides,
	FogParams,
	NoMipMaps,
	NoPicMip,
	NoCompress,
	NoFiltering,
	SmallestMipSize,
	PolygonOffset,
	StencilTest,
	Sort,
	DeformVertexes,
	Portal,
	EntityMergable,
	If,
	EndIf,
	OffsetMappingScale,
	GlossExponent,
	GlossIntensity,
	Template,
	Skip,
	SoftParticle,
	ForceWorldOutlines,
};

enum class RgbGen {
	Identity,
	Wave,
	ColorWave,
	Custom,
	CustomWave,
	Entity,
	EntityWave,
	OneMinusEntity,
	Vertex,
	OneMinusVertex,
	LightingDiffuse,
	ExactVertex,
	Const
};

enum class AlphaGen {
	Vertex = ALPHA_GEN_VERTEX,
	OneMinusVertex = ALPHA_GEN_ONE_MINUS_VERTEX,
	Entity = ALPHA_GEN_ENTITY,
	Wave = ALPHA_GEN_WAVE,
	Const = ALPHA_GEN_CONST,
};

enum class SrcBlend {
	Zero = GLSTATE_SRCBLEND_ZERO,
	One = GLSTATE_SRCBLEND_ONE,
	DstColor = GLSTATE_SRCBLEND_DST_COLOR,
	OneMinusDstColor = GLSTATE_SRCBLEND_ONE_MINUS_DST_COLOR,
	SrcAlpha = GLSTATE_SRCBLEND_SRC_ALPHA,
	DstAlpha = GLSTATE_SRCBLEND_DST_ALPHA,
	OneMinusSrcAlpha = GLSTATE_SRCBLEND_ONE_MINUS_SRC_ALPHA,
	OneMinusDstAlpha = GLSTATE_SRCBLEND_ONE_MINUS_DST_ALPHA
};

enum class DstBlend {
	Zero = GLSTATE_DSTBLEND_ZERO,
	One = GLSTATE_DSTBLEND_ONE,
	SrcColor = GLSTATE_DSTBLEND_SRC_COLOR,
	OneMinusSrcColor = GLSTATE_DSTBLEND_ONE_MINUS_SRC_COLOR,
	SrcAlpha = GLSTATE_DSTBLEND_SRC_ALPHA,
	OneMinusSrcAlpha = GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA,
	DstAlpha = GLSTATE_DSTBLEND_DST_ALPHA,
	OneMinusDstAlpha = GLSTATE_DSTBLEND_ONE_MINUS_DST_ALPHA
};

enum class UnaryBlendFunc {
	Blend = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA,
	Filter = GLSTATE_SRCBLEND_DST_COLOR | GLSTATE_DSTBLEND_ZERO,
	Add = GLSTATE_SRCBLEND_ONE | GLSTATE_DSTBLEND_ONE
};

enum class AlphaFunc {
	GT0,
	LT128,
	GE128
};

enum class DepthFunc {
	GT,
	EQ
};

enum class TCMod {
	Rotate = TC_MOD_ROTATE,
	Scale = TC_MOD_SCALE,
	Scroll = TC_MOD_SCROLL,
	Stretch = TC_MOD_STRETCH,
	Transform = TC_MOD_TRANSFORM,
	Turb = TC_MOD_TURB
};

enum class TCGen {
	Base = TC_GEN_BASE,
	Lightmap = TC_GEN_LIGHTMAP,
	Environment = TC_GEN_ENVIRONMENT,
	Vector = TC_GEN_VECTOR,
	Reflection = TC_GEN_REFLECTION,
	Celshade = TC_GEN_REFLECTION_CELSHADE,
	Surround = TC_GEN_SURROUND
};

enum class SkySide {
	Right,
	Back,
	Left,
	Front,
	Up,
	Down
};

class TokenSplitter {
	const char *const m_data;
	size_t m_dataSize;
	ptrdiff_t m_offset { 0 };

	[[nodiscard]]
	auto tryMatching1Or2CharsToken( const char *tokenStart ) const -> std::optional<unsigned>;

	[[nodiscard]]
	static bool mustCloseTokenAtChar( char ch, char nextCh );
public:
	TokenSplitter( const char *data, size_t dataSize )
		: m_data( data ), m_dataSize( dataSize ) {}

	[[nodiscard]]
	bool isAtEof() const {
		if( (ptrdiff_t)m_offset >= (ptrdiff_t)m_dataSize ) {
			return true;
		}
		// Protect against bogus files
		return m_data[m_offset] == '\0';
	}

	[[nodiscard]]
	auto fetchNextTokenInLine() -> std::optional<std::pair<uint32_t, uint32_t>>;
};

struct TokenSpan {
	int32_t offset;
	uint32_t len;
	uint32_t line;
};

class TokenStream {
	// Initialize just to suppress a lint warning
	const char *m_data[2] { nullptr, nullptr };

	const TokenSpan *const m_tokenSpans;
	const int m_numTokens;

	int m_currToken { 0 };
	int m_currLine { 0 };

	[[nodiscard]]
	auto getView( int offset, unsigned len ) const -> wsw::StringView {
		// Use either a data or an alt data as a base ptr based on the offset sign
		const char *p = this->m_data[offset < 0] + std::abs( offset );
		assert( p );
		return wsw::StringView( p, len );
	}
public:
	TokenStream( const char *data, const TokenSpan *tokenSpans, int numTokens, const char *altData = nullptr )
		: m_tokenSpans( tokenSpans ), m_numTokens( numTokens ) {
		m_data[0] = data;
		m_data[1] = altData;
	}

	[[nodiscard]]
	bool isAtEof() const { return m_currToken >= m_numTokens; }

	[[nodiscard]]
	auto getCurrTokenNum() const -> int { return m_currToken; }

	void setCurrTokenNum( int num ) {
		assert( (unsigned)num <= (unsigned)m_numTokens );
		m_currToken = num;
		if( num < m_numTokens ) {
			m_currLine = m_tokenSpans[num].line;
		} else {
			m_currLine = std::numeric_limits<int>::max();
		}
	}

	[[nodiscard]]
	auto getNextTokenInLine() -> std::optional<wsw::StringView> {
		if( m_currToken >= m_numTokens ) {
			return std::nullopt;
		}
		const auto &[off, len, line] = m_tokenSpans[m_currToken];
		if( ( decltype( m_currLine ) )line != m_currLine ) {
			return std::nullopt;
		}
		m_currToken++;
		return std::optional( getView( off, len ) );
	}

	[[nodiscard]]
	auto getNextToken() -> std::optional<wsw::StringView> {
		if( m_currToken >= m_numTokens ) {
			return std::nullopt;
		}
		const auto &[off, len, line] = m_tokenSpans[m_currToken++];
		m_currLine = line;
		return std::optional( getView( off, len ) );
	}

	[[maybe_unused]]
	bool unGetToken() {
		assert( m_currToken <= m_numTokens );
		if( m_currToken == 0 ) {
			return false;
		}
		m_currToken = m_currToken - 1;
		m_currLine = m_tokenSpans[m_currToken].line;
		return true;
	}
};

class MaterialLexer {
	TokenStream *m_stream { nullptr };

	template <typename T>
	[[nodiscard]]
	auto getNumber() -> std::optional<T> {
		if( const auto maybeToken = getNextTokenInLine() ) {
			if( const auto maybeNumber = wsw::toNum<T>( *maybeToken ) ) {
				return maybeNumber;
			}
			unGetToken();
		}
		return std::nullopt;
	}

	template <typename T>
	[[nodiscard]]
	auto getNumberOr( T defaultValue ) -> T {
		if( const auto maybeToken = getNextTokenInLine() ) {
			if( const auto maybeNumber = wsw::toNum<T>( *maybeToken ) ) {
				return *maybeNumber;
			}
			unGetToken();
		}
		return defaultValue;
	}

	[[nodiscard]]
	bool parseVector( float *dest, size_t numElems );
	void parseVectorOrFill( float *dest, size_t numElems, float defaultValue );
public:
	explicit MaterialLexer( TokenStream *tokenStream ) : m_stream( tokenStream ) {}

	[[nodiscard]]
	auto getNextToken() -> std::optional<wsw::StringView> { return m_stream->getNextToken(); }
	[[nodiscard]]
	auto getNextTokenInLine() -> std::optional<wsw::StringView> { return m_stream->getNextTokenInLine(); }
	[[maybe_unused]]
	bool unGetToken() { return m_stream->unGetToken(); }
	[[nodiscard]]
	auto getCurrTokenNum() const -> int { return m_stream->getCurrTokenNum(); }

	[[nodiscard]]
	auto getPassKey() -> std::optional<PassKey>;
	[[nodiscard]]
	auto getDeform() -> std::optional<Deform>;
	[[nodiscard]]
	auto getFunc() -> std::optional<Func>;
	[[nodiscard]]
	auto getIntConditionVar() -> std::optional<IntConditionVar>;
	[[nodiscard]]
	auto getBoolConditionVar() -> std::optional<BoolConditionVar>;
	[[nodiscard]]
	auto getLogicOp() -> std::optional<LogicOp>;
	[[nodiscard]]
	auto getCmpOp() -> std::optional<CmpOp>;
	[[nodiscard]]
	auto getCullMode() -> std::optional<CullMode>;
	[[nodiscard]]
	auto getSortMode() -> std::optional<SortMode>;
	[[nodiscard]]
	auto getMaterialKey() -> std::optional<MaterialKey>;
	[[nodiscard]]
	auto getRgbGen() -> std::optional<RgbGen>;
	[[nodiscard]]
	auto getAlphaGen() -> std::optional<AlphaGen>;
	[[nodiscard]]
	auto getSrcBlend() -> std::optional<SrcBlend>;
	[[nodiscard]]
	auto getDstBlend() -> std::optional<DstBlend>;
	[[nodiscard]]
	auto getUnaryBlendFunc() -> std::optional<UnaryBlendFunc>;
	[[nodiscard]]
	auto getAlphaFunc() -> std::optional<AlphaFunc>;
	[[nodiscard]]
	auto getDepthFunc() -> std::optional<DepthFunc>;
	[[nodiscard]]
	auto getTCMod() -> std::optional<TCMod>;
	[[nodiscard]]
	auto getTCGen() -> std::optional<TCGen>;
	[[nodiscard]]
	auto getSkySide() -> std::optional<SkySide>;

	[[nodiscard]]
	bool skipToEndOfLine();

	[[nodiscard]]
	auto getFloat() -> std::optional<float> { return getNumber<float>(); }
	[[nodiscard]]
	auto getInt() -> std::optional<int> { return getNumber<int>(); }
	[[nodiscard]]
	auto getFloatOr( float defaultValue ) -> float { return getNumberOr<float>( defaultValue ); }
	[[nodiscard]]
	auto getIntOr( int defaultValue ) -> int { return getNumberOr<int>( defaultValue ); }

	template <size_t N>
	[[nodiscard]]
	bool getVector( float *dest ) {
		static_assert( N && N <= 8 );
		if constexpr( N == 1 ) {
			if( const auto number = getFloat() ) {
				*dest = *number;
				return true;
			}
			return false;
		}
		// Make sure it rolls back offset properly on failure like everything else does
		const auto oldTokenNum = m_stream->getCurrTokenNum();
		if( parseVector( dest, N ) ) {
			return true;
		}
		m_stream->setCurrTokenNum( oldTokenNum );
		return false;
	}

	template <size_t N>
	void getVectorOrFill( float *dest, float defaultValue ) {
		static_assert( N && N < 8 );
		if constexpr( N == 1 ) {
			*dest = getFloatOr( defaultValue );
		} else {
			parseVectorOrFill( dest, N, defaultValue );
		}
	}

	[[nodiscard]]
	auto getBool() -> std::optional<bool>;
};


struct PlaceholderSpan {
	uint32_t tokenNum;
	uint16_t offset;
	uint8_t len;
	uint8_t argNum;
};

struct MaterialFileContents {
	MaterialFileContents *next {nullptr };
	const char *data { nullptr };
	size_t dataSize { 0 };
	TokenSpan *spans { nullptr };
	unsigned numSpans { 0 };
};

class MaterialSource {
	friend class MaterialCache;
	friend class MaterialSourceTest;

	using Placeholders = wsw::PodVector<PlaceholderSpan>;

	std::optional<Placeholders> m_placeholders;

	MaterialSource *m_nextInList { nullptr };
	MaterialSource *m_nextInBin { nullptr };

	wsw::HashedStringView m_name;

	const MaterialSource *m_firstInSameMemChunk { nullptr };
	const MaterialFileContents *m_fileContents {nullptr };
	unsigned m_tokenSpansOffset { ~0u };
	unsigned m_numTokens { ~0u };
	bool m_triedPreparingPlaceholders { false };

	struct ExpansionParams {
		const wsw::StringView *args;
		const size_t numArgs;
		const Placeholders &placeholders;
	};

	struct ExpansionState {
		wsw::PodVector<char> &expansionBuffer;
		wsw::PodVector<TokenSpan> &resultingTokens;
		size_t tokenStart { 1 };
		size_t lastOffsetInSpan { 0 };
	};

	[[nodiscard]]
	bool expandTemplate( const ExpansionParams &params, ExpansionState &state ) const;

	void addTheRest( ExpansionState &state, size_t lastSpanNum, size_t currSpanNum ) const;

	[[nodiscard]]
	auto validateAndEstimateExpandedDataSize( const ExpansionParams &params ) const -> std::optional<unsigned>;
public:
	[[nodiscard]]
	auto getName() const -> const wsw::HashedStringView & { return m_name; }

	[[nodiscard]]
	auto getCharData() const -> const char * { return m_fileContents->data; }

	// TODO: std::span
	[[nodiscard]]
	auto getTokenSpans() const -> std::pair<const TokenSpan *, unsigned> {
		return std::make_pair( m_fileContents->spans + m_tokenSpansOffset, m_numTokens );
	}

	[[nodiscard]]
	auto preparePlaceholders() -> std::optional<Placeholders>;

	static void findPlaceholdersInToken( const wsw::StringView &token, unsigned tokenNum,
										 wsw::PodVector<PlaceholderSpan> &spans );

	[[nodiscard]]
	bool expandTemplate( const wsw::StringView *args, size_t numArgs,
						 wsw::PodVector<char> &expansionBuffer,
						 wsw::PodVector<TokenSpan> &resultingTokens );
};

// MSVC: Keep it defined as struct for now
struct Skin {
	friend class MaterialCache;
private:
	wsw::StringSpanStorage<uint16_t, uint16_t> m_stringDataStorage;
	wsw::StaticVector<std::pair<shader_s *, unsigned>, 8> m_meshPartMaterials;
	unsigned m_registrationSequence { 0 };
public:
	Skin *prev { nullptr }, *next { nullptr };
	[[nodiscard]]
	auto getName() const -> wsw::StringView { return m_stringDataStorage.back(); }
};

class MaterialCache;

struct ImageOptions;

class MaterialFactory {
	friend class MaterialParser;
	friend class MaterialCache;

	MaterialCache *const m_materialCache;
	wsw::PodVector<char> m_expansionBuffer;
	wsw::PodVector<TokenSpan> m_templateTokenSpans;
	wsw::StaticVector<TokenStream, 1> m_templateTokenStreamHolder;
	wsw::StaticVector<MaterialLexer, 1> m_templateLexerHolder;
	wsw::StaticVector<TokenStream, 1> m_primaryTokenStreamHolder;

	[[nodiscard]]
	auto findImage( const wsw::StringView &name, int flags ) -> Texture *;
	void loadMaterial( Texture **images, const wsw::StringView &fullName, int flags );

	[[nodiscard]]
	auto expandTemplate( const wsw::StringView &name, const wsw::StringView *args, size_t numArgs ) -> MaterialLexer *;

	explicit MaterialFactory( MaterialCache *materialCache ) : m_materialCache( materialCache ) {}
public:
	[[nodiscard]]
	auto initMaterial( int type, const wsw::HashedStringView &cleanName, wsw::MemSpecBuilder memSpec ) -> shader_t *;
	[[nodiscard]]
	auto newMaterial( int type, const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefaultMaterial( int type, const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefaultVertexMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefaultDeluxeMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefaultCoronaMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefaultDiffuseMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newDefault2DLikeMaterial( int type, const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newOpaqueEnvMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;
	[[nodiscard]]
	auto newFogMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name ) -> shader_t *;

	void destroyMaterial( shader_s *material );

	// TODO: Split this functionality into MaterialCache and MaterialLoader?

	// Results bypass caching and require a manual lifetime management.
	// It's recommended to implement custom domain-specific caching on top of it where it's possible.
	[[nodiscard]]
	auto create2DMaterialBypassingCache() -> shader_t *;
	void release2DMaterialBypassingCache( shader_t *material );
	bool update2DMaterialImageBypassingCache( shader_t *material, const wsw::StringView &name,
											  const ImageOptions &options );
};

class MaterialCache {
	friend class MaterialParser;
	friend class MaterialSource;

	MaterialFactory m_factory { this };

	MaterialFileContents *m_fileContentsHead { nullptr };

	enum { kNumBins = 307 };

	MaterialSource *m_sourcesHead { nullptr };
	MaterialSource *m_sourceBins[kNumBins] { nullptr };

	shader_t *m_materialsHead { nullptr };
	shader_t *m_materialBins[kNumBins] {};
	shader_t *m_materialById[MAX_SHADERS] {};

	Skin *m_skinsHead { nullptr };

	wsw::PodVector<char> m_pathNameBuffer;
	wsw::PodVector<char> m_cleanNameBuffer;
	wsw::PodVector<char> m_fileContentsBuffer;

	wsw::PodVector<TokenSpan> m_fileTokenSpans;

	wsw::PodVector<wsw::StringView> m_fileMaterialNames;
	wsw::PodVector<std::pair<unsigned, unsigned>> m_fileSourceSpans;

	wsw::StaticVector<uint16_t, MAX_SHADERS> m_freeMaterialIds;

	wsw::MemberBasedFreelistAllocator<sizeof( Skin ), 16> m_skinsAllocator;

	[[nodiscard]]
	auto loadFileContents( const wsw::StringView &fileName ) -> MaterialFileContents *;
	[[nodiscard]]
	auto readRawContents( const wsw::StringView &fileName ) -> const wsw::PodVector<char> *;

	void loadDirContents( const wsw::StringView &dir );

	void addFileContents( const wsw::StringView &fileName );

	[[nodiscard]]
	bool tryAddingFileContents( const MaterialFileContents *contents );

	void unlinkAndFree( shader_t *material );

	[[nodiscard]]
	auto getNextMaterialId() -> unsigned;
	[[nodiscard]]
	auto makeCleanName( const wsw::StringView &name ) -> wsw::HashedStringView;

	[[nodiscard]]
	auto findSkinByName( const wsw::StringView &name ) -> Skin *;
	[[nodiscard]]
	auto parseSkinFileData( const wsw::StringView &name, const wsw::StringView &fileData ) -> Skin *;
	[[nodiscard]]
	bool parseSkinFileData( Skin *skin, const wsw::StringView &fileData );
	[[nodiscard]]
	auto readSkinFileData( const wsw::StringView &name, char *buffer, size_t bufferSize ) ->
		std::optional<wsw::StringView>;
public:
	MaterialCache();
	~MaterialCache();

	static void init();
	static void shutdown();

	[[nodiscard]]
	static auto instance() -> MaterialCache *;

	[[nodiscard]]
	auto getUnderlyingFactory() -> MaterialFactory * { return &m_factory; }

	[[nodiscard]]
	auto findSourceByName( const wsw::StringView &name ) -> MaterialSource * {
		return findSourceByName( wsw::HashedStringView( name ) );
	}

	[[nodiscard]]
	auto findSourceByName( const wsw::HashedStringView &name ) -> MaterialSource *;

	void freeUnusedMaterialsByType( const std::span<shaderType_e> &types );

	void freeUnusedObjects();

	void touchMaterialsByName( const wsw::StringView &name );

	void touchMaterial( shader_s *material );

	[[nodiscard]]
	auto getMaterialById( int id ) -> shader_t * { return m_materialById[id]; }

	[[nodiscard]]
	auto loadMaterial( const wsw::StringView &name, int type, bool forceDefault ) -> shader_t *;

	[[nodiscard]]
	auto loadDefaultMaterial( const wsw::StringView &name, int type ) -> shader_t *;

	[[nodiscard]]
	auto registerSkin( const wsw::StringView &name ) -> Skin *;

	[[nodiscard]]
	auto findMeshMaterialInSkin( const Skin *skin, const wsw::StringView &meshName ) -> shader_t *;
};

struct shader_s;
struct shaderpass_s;
struct shaderfunc_s;
class Texture;

class MaterialParser {
	friend class ParserTestWrapper;

	MaterialFactory *const m_materialFactory;
	MaterialLexer m_defaultLexer;
	MaterialLexer *m_lexer;

	const wsw::StringView m_name;
	const wsw::HashedStringView m_cleanName;

	wsw::StaticVector<int, 256> m_deformSig;
	wsw::StaticVector<shaderpass_t, MAX_SHADER_PASSES> m_passes;
	wsw::StaticVector<deformv_t, MAX_SHADER_DEFORMVS> m_deforms;
	wsw::StaticVector<tcmod_t, MAX_SHADER_PASSES * MAX_SHADER_TCMODS> m_tcMods;

	int m_sort { 0 };
	int m_flags { SHADER_CULL_FRONT };
	shaderType_e m_type { (shaderType_e)0 };

	std::optional<int> m_minMipSize;

	uint8_t m_fog_color[4] { 0, 0, 0, 0 };
	float m_fog_dist { 0.0f };
	float m_fog_clearDist { 0.0f };

	uint8_t m_skyColor[4] { 0, 0, 0, 0 };

	float m_glossIntensity { 0.0f };
	float m_glossExponent { 0.0f };
	float m_offsetMappingScale { 0.0f };

	float m_portalDistance { 0.0f };

	int m_conditionalBlockDepth { 0 };

	bool m_noPicMip { false };
	bool m_noMipMaps { false };
	bool m_noCompress { false };
	bool m_noFiltering { false };

	bool m_hasLightmapPass { false };

	bool m_allowUnknownEntries { true };

	bool m_strict { false };

	[[nodiscard]]
	auto currPass() -> shaderpass_t * {
		assert( !m_passes.empty() );
		return &m_passes.back();
	}

	[[nodiscard]]
	auto tryAddingPassTCMod( TCMod modType ) -> tcmod_t *;
	[[nodiscard]]
	auto tryAddingDeform( Deform deformType ) -> deformv_t *;

	[[nodiscard]]
	bool parsePass();
	[[nodiscard]]
	bool parsePassKey();
	[[nodiscard]]
	bool parseKey();

	[[nodiscard]]
	bool parseRgbGen();
	[[nodiscard]]
	bool parseBlendFunc();
	[[nodiscard]]
	bool parseDepthFunc();
	[[nodiscard]]
	bool parseDepthWrite();
	[[nodiscard]]
	bool parseAlphaFunc();
	[[nodiscard]]
	bool parseTCMod();
	[[nodiscard]]
	bool parseMap();
	[[nodiscard]]
	bool parseAnimMap();
	[[nodiscard]]
	bool parseCubeMap();
	[[nodiscard]]
	bool parseShadeCubeMap();
	[[nodiscard]]
	bool parseSurroundMap();
	[[nodiscard]]
	bool parseClampMap();
	[[nodiscard]]
	bool parseAnimClampMap();
	[[nodiscard]]
	bool parseTimelineMap();
	[[nodiscard]]
	bool parseTimelineClampMap();
	[[nodiscard]]
	bool parseTimelineMap2();
	[[nodiscard]]
	bool parseTimelineClampMap2();
	[[nodiscard]]
	bool parseMaterial();
	[[nodiscard]]
	bool parseDistortion();
	[[nodiscard]]
	bool parseCelshade();
	[[nodiscard]]
	bool parseTCGen();
	[[nodiscard]]
	bool parseAlphaGen();
	[[nodiscard]]
	bool parseDetail();
	[[nodiscard]]
	bool parseGrayscale();
	[[nodiscard]]
	bool parseSkip();

	[[nodiscard]]
	bool parseAlphaGenPortal();

	[[nodiscard]]
	bool parseMapExt( int addFlags );
	[[nodiscard]]
	bool tryMatchingPortalMap( const wsw::StringView &texNameToken );
	[[nodiscard]]
	bool tryMatchingLightMap( const wsw::StringView &texNameToken );

	[[nodiscard]]
	bool parseAnimMapExt( int addFlags );
	[[nodiscard]]
	bool parseTimelineMapExt( int addFlags );
	[[nodiscard]]
	bool parseTimelineMap2Ext( int addFlags );
	[[nodiscard]]
	bool parseCubeMapExt( int addFlags, int tcGen );

	[[nodiscard]]
	bool parseCull();
	[[nodiscard]]
	bool parseSkyParms();
	[[nodiscard]]
	bool parseSkyParms2();
	[[nodiscard]]
	bool parseSkyParmsSides();
	[[nodiscard]]
	bool parseFogParams();
	[[nodiscard]]
	bool parseNoMipmaps();
	[[nodiscard]]
	bool parseNoPicmip();
	[[nodiscard]]
	bool parseNoCompress();
	[[nodiscard]]
	bool parseNofiltering();
	[[nodiscard]]
	bool parseSmallestMipSize();
	[[nodiscard]]
	bool parsePolygonOffset();
	[[nodiscard]]
	bool parseStencilTest();
	[[nodiscard]]
	bool parseEntityMergable();
	[[nodiscard]]
	bool parseSort();
	[[nodiscard]]
	bool parseDeformVertexes();
	[[nodiscard]]
	bool parsePortal();
	[[nodiscard]]
	bool parseIf();
	[[nodiscard]]
	bool parseEndIf();
	[[nodiscard]]
	bool parseOffsetMappingScale();
	[[nodiscard]]
	bool parseGlossExponent();
	[[nodiscard]]
	bool parseGlossIntensity();
	[[nodiscard]]
	bool parseTemplate();
	[[nodiscard]]
	bool parseSoftParticle();
	[[nodiscard]]
	bool parseForceWorldOutlines();

	[[nodiscard]]
	auto parseCondition() -> std::optional<bool>;
	[[nodiscard]]
	bool skipConditionalBlock();
	[[nodiscard]]
	static auto getIntConditionVarValue( IntConditionVar var ) -> int;
	[[nodiscard]]
	static auto getBoolConditionVarValue( BoolConditionVar var ) -> bool;

	[[nodiscard]]
	bool parseDeformWave();
	[[nodiscard]]
	bool parseDeformBulge();
	[[nodiscard]]
	bool parseDeformMove();

	[[nodiscard]]
	bool parseFunc( shaderfunc_s *func );

	struct SkyParmOptions { bool custom { false }, underscore { false }; };
	[[nodiscard]]
	bool parseSkyParmsWithOptions( const SkyParmOptions &options );
	[[nodiscard]]
	bool parseCustomSkySides( class TextureHistogram *rn );
	[[nodiscard]]
	bool parseSkySides( class TextureHistogram *histogram, bool useUnderscore );

	template <typename... Args>
	[[nodiscard]]
	bool addToDeformSignature( Args ... args ) {
		return _addToDeformSignature( args... );
	}

	template <typename... Args>
	[[nodiscard]]
	bool _addToDeformSignature( int arg, Args... rest ) {
		return tryAddingToSignature( arg ) && _addToDeformSignature( rest... );
	}

	template <typename... Args>
	[[nodiscard]]
	bool _addToDeformSignature( unsigned arg, Args... rest ) {
		return tryAddingToSignature( (int)arg ) && _addToDeformSignature( rest... );
	}

	template <typename... Args>
	[[nodiscard]]
	bool _addToDeformSignature( float arg, Args... rest ) {
		union { float f; int32_t i; } u;
		u.f = arg;
		return tryAddingToSignature( u.i ) && _addToDeformSignature( rest... );
	}

	[[nodiscard]]
	bool tryAddingToSignature( int value ) {
		if( !m_deformSig.full() ) {
			m_deformSig.push_back( value );
			return true;
		}
		return false;
	}

	[[nodiscard]]
	static bool _addToDeformSignature() { return true; }

	int getImageFlags();

	[[nodiscard]]
	auto findImage( const wsw::StringView &name, int flags ) -> Texture * {
		return m_materialFactory->findImage( name, flags );
	}

	void fixLightmapsForVertexLight();
	void fixFlagsAndSortingOrder();

	[[nodiscard]]
	bool parse();
	[[nodiscard]]
	auto build() -> shader_t *;

	[[nodiscard]]
	auto buildVertexAttribs() -> int;
	[[nodiscard]]
	static auto getDeformVertexAttribs( const deformv_t &deform ) -> int;
	[[nodiscard]]
	static auto getPassVertexAttribs( const shaderpass_t &pass ) -> int;
	[[nodiscard]]
	static auto getRgbGenVertexAttribs( const shaderpass_t &pass, const colorgen_t &gen ) -> int;
	[[nodiscard]]
	static auto getAlphaGenVertexAttribs( const colorgen_t &gen ) -> int;
	[[nodiscard]]
	static auto getTCGenVertexAttribs( unsigned gen ) -> int;
public:
	MaterialParser( MaterialFactory *materialFactory,
					TokenStream *mainTokenStream,
					const wsw::StringView &name,
					const wsw::HashedStringView &cleanName,
					shaderType_e type );

	[[nodiscard]]
    auto exec() -> shader_t *;
};

const wsw::StringView kNormSuffix( "_norm" );
const wsw::StringView kGlossSuffix( "_gloss" );
const wsw::StringView kDecalSuffix( "_decal" );
const wsw::StringView kAddSuffix( "_add" );

class MaterialIfEvaluator {
public:
	static constexpr size_t Capacity = 32;
private:
	enum class Tag: uint8_t {
		Value,
		UnaryNot,
		LogicOp,
		CmpOp,
		LParen,
		RParen
	};

	struct alignas( 8 )TapeEntry {
		uint8_t data[7];
		Tag tag;
	};

	TapeEntry m_tape[Capacity];
	int m_numEntries { 0 };
	int m_tapeCursor { 0 };
	bool m_hadError { false };

#pragma pack( push, 2 )
	struct Value {
		union {
			int32_t i;
			bool b;
		} u;

		bool isBool;
		bool isInputValue { false };

		explicit Value( bool b ) : isBool( true ) {
			u.b = b;
		}
		explicit Value( int32_t i ) : isBool( false ) {
			u.i = i;
		}
		operator bool() const {
			return isBool ? u.b : u.i;
		}
		operator int() const {
			return isBool ? u.b : u.i;
		}
	};

	static_assert( alignof( Value ) <= 8 );
	static_assert( sizeof( Value ) == 6 );
#pragma pack( pop )

	[[maybe_unused]]
	auto makeEntry( Tag tag ) -> TapeEntry * {
		assert( (unsigned)m_numEntries < (unsigned)Capacity );
		auto *e = &m_tape[m_numEntries++];
		e->tag = tag;
		return e;
	}

	[[nodiscard]]
	auto nextTokenTag() -> std::optional<Tag> {
		return ( m_tapeCursor < m_numEntries ) ? std::optional( m_tape[m_tapeCursor++].tag ) : std::nullopt;
	}

	void ungetToken() {
		assert( m_tapeCursor >= 0 );
		m_tapeCursor = m_tapeCursor ? m_tapeCursor - 1 : 0;
	}

	[[nodiscard]]
	auto lastEntry() const -> const TapeEntry & {
		assert( m_tapeCursor - 1 < m_numEntries );
		return m_tape[m_tapeCursor - 1];
	}

	template <typename T>
	[[nodiscard]]
	auto lastEntryAs() const -> T {
		return *( ( const T *)lastEntry().data );
	}

	[[nodiscard]]
	auto lastTag() const -> Tag {
		assert( m_tapeCursor - 1 < m_numEntries );
		return m_tape[m_tapeCursor - 1].tag;
	}

	[[nodiscard]]
	auto lastValue() -> std::optional<Value> {
		return ( lastTag() == Tag::Value ) ? std::optional( lastEntryAs<Value>() ) : std::nullopt;
	}

	[[nodiscard]]
	auto lastLogicOp() -> std::optional<LogicOp> {
		return ( lastTag() == Tag::LogicOp ) ? std::optional( lastEntryAs<LogicOp>() ) : std::nullopt;
	}

	[[nodiscard]]
	auto lastCmpOp() -> std::optional<CmpOp> {
		return ( lastTag() == Tag::CmpOp ) ? std::optional( lastEntryAs<CmpOp>() ) : std::nullopt;
	}

#ifndef _MSC_VER
	std::nullopt_t withError( const char *fmt, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
	void warn( const char *fmt, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
	auto withError( _Printf_format_string_ const char *fmt, ... ) -> std::nullopt_t;
	void warn( _Printf_format_string_ const char *fmt, ... );
#endif

	void warnBooleanToIntegerConversion( const Value &value, const char *desc, const char *context );
	void warnIntegerToBooleanConversion( const Value &value, const char *desc, const char *context );

	[[nodiscard]]
	auto evalUnaryExpr() -> std::optional<Value>;
	[[nodiscard]]
	auto evalCmpExpr() -> std::optional<Value>;
	[[nodiscard]]
	auto evalLogicExpr() -> std::optional<Value>;

	[[nodiscard]]
	auto evalExpr() -> std::optional<Value> { return evalLogicExpr(); }
public:

	void addInt( int value ) {
		auto *v = new( makeEntry( Tag::Value ) )Value( value );
		v->isInputValue = true;
	}

	void addBool( bool value ) {
		auto *v = new( makeEntry( Tag::Value ) )Value( value );
		v->isInputValue = true;
	}

	void addLogicOp( LogicOp op ) {
		new( makeEntry( Tag::LogicOp ) )LogicOp( op );
	}

	void addCmpOp( CmpOp op ) {
		new( makeEntry( Tag::CmpOp ) )CmpOp( op );
	}

	void addUnaryNot() { makeEntry( Tag::UnaryNot ); }

	void addLeftParen() { makeEntry( Tag::LParen ); }
	void addRightParen() { makeEntry( Tag::RParen ); }

	[[nodiscard]]
	auto exec() -> std::optional<bool>;
};

#endif
