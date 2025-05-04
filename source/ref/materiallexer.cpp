/*
Copyright (C) 1999 Stephen C. Taylor
Copyright (C) 2002-2007 Victor Luchits

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

#include "materiallocal.h"

#include "../common/links.h"
#include "../common/enumtokenmatcher.h"

using wsw::operator""_asView;

class DeformTokenMatcher : public wsw::EnumTokenMatcher<Deform, DeformTokenMatcher> {
public:
	DeformTokenMatcher() : EnumTokenMatcher({
		{ "Wave"_asView, Deform::Wave },
		{ "Bulge"_asView, Deform::Bulge },
		{ "Move"_asView, Deform::Move },
		{ "Autosprite"_asView, Deform::Autosprite },
		{ "Autosprite2"_asView, Deform::Autosprite2 },
		{ "Autoparticle"_asView, Deform::Autoparticle },
	}) {}
};

class FuncTokenMatcher : public wsw::EnumTokenMatcher<Func, FuncTokenMatcher> {
public:
	FuncTokenMatcher() : EnumTokenMatcher({
		{ "Sin"_asView, Func::Sin },
		{ "Triangle"_asView, Func::Triangle },
		{ "Square"_asView, Func::Square },
		{ "Sawtooth"_asView, Func::Sawtooth },
		{ "InvSawtooth"_asView, Func::InvSawtooth },
		{ "InverseSawtooth"_asView, Func::InvSawtooth },
		{ "Noize"_asView, Func::Noize },
		{ "DistanceRamp"_asView, Func::DistanceRamp },
	}) {}
};

class PassKeyMatcher : public wsw::EnumTokenMatcher<PassKey, PassKeyMatcher> {
public:
	PassKeyMatcher() : EnumTokenMatcher({
		{ "RgbGen"_asView, PassKey::RgbGen },
		{ "BlendFunc"_asView, PassKey::BlendFunc },
		{ "DepthFunc"_asView, PassKey::DepthFunc },
		{ "DepthWrite"_asView, PassKey::DepthWrite },
		{ "AlphaFunc"_asView, PassKey::AlphaFunc },
		{ "TCMod"_asView, PassKey::TCMod },
		{ "Map"_asView, PassKey::Map },
		{ "AnimMap"_asView, PassKey::AnimMap },
		{ "CubeMap"_asView, PassKey::CubeMap },
		{ "ShadeCubeMap"_asView, PassKey::ShadeCubeMap },
		{ "ClampMap"_asView, PassKey::ClampMap },
		{ "AnimClampMap"_asView, PassKey::AnimClampMap },
		{ "TimelineMap"_asView, PassKey::TimelineMap },
		{ "TimelineClampMap"_asView, PassKey::TimelineClampMap },
		{ "TimelineMap2"_asView, PassKey::TimelineMap2 },
		{ "TimelineClampMap2"_asView, PassKey::TimelineClampMap2 },
		{ "Material"_asView, PassKey::Material },
		{ "Distortion"_asView, PassKey::Distortion },
		{ "CelShade"_asView, PassKey::CelShade },
		{ "TCGen"_asView, PassKey::TCGen },
		{ "AlphaGen"_asView, PassKey::AlphaGen },
		{ "Detail"_asView, PassKey::Detail },
		{ "Greyscale"_asView, PassKey::Grayscale },
		{ "Grayscale"_asView, PassKey::Grayscale },
		{ "Skip"_asView, PassKey::Skip },
	}) {}
};

class IntConditionVarMatcher : public wsw::EnumTokenMatcher<IntConditionVar, IntConditionVarMatcher> {
public:
	IntConditionVarMatcher() : EnumTokenMatcher({
		{ "MaxTextureSize"_asView, IntConditionVar::MaxTextureSize },
		{ "MaxTextureCubemapSize"_asView, IntConditionVar::MaxTextureCubemapSize },
		{ "MaxTextureUnits"_asView, IntConditionVar::MaxTextureUnits },
	}) {}
};

class BoolConditionVarMatcher : public wsw::EnumTokenMatcher<BoolConditionVar, BoolConditionVarMatcher> {
public:
	BoolConditionVarMatcher() : EnumTokenMatcher({
		{ "TextureCubeMap"_asView, BoolConditionVar::TextureCubeMap },
		{ "Glsl"_asView, BoolConditionVar::Glsl },
		{ "Deluxe"_asView, BoolConditionVar::DeluxeMaps },
		{ "DeluxeMaps"_asView, BoolConditionVar::DeluxeMaps },
		{ "PortalMaps"_asView, BoolConditionVar::PortalMaps },
	}) {}
};

class LogicOpMatcher : public wsw::EnumTokenMatcher<LogicOp, LogicOpMatcher> {
public:
	LogicOpMatcher() : EnumTokenMatcher({
		{ "&&"_asView, LogicOp::And },
		{ "||"_asView, LogicOp::Or },
	}) {}
};

class CmpOpMatcher : public wsw::EnumTokenMatcher<CmpOp, CmpOpMatcher> {
public:
	CmpOpMatcher() : EnumTokenMatcher({
		{ "<"_asView, CmpOp::LS },
		{ "<="_asView, CmpOp::LE },
		{ ">"_asView, CmpOp::GT },
		{ ">="_asView, CmpOp::GE },
		{ "!="_asView, CmpOp::NE },
		{ "=="_asView, CmpOp::EQ },
	}) {}
};

class CullModeMatcher : public wsw::EnumTokenMatcher<CullMode, CullModeMatcher> {
public:
	CullModeMatcher() : EnumTokenMatcher({
		{ "None"_asView, CullMode::None },
		{ "Disable"_asView, CullMode::None },
		{ "Twosided"_asView, CullMode::None },

		{ "Back"_asView, CullMode::Back },
		{ "Backside"_asView, CullMode::Back },
		{ "Backsided"_asView, CullMode::Back },

		{ "Front"_asView, CullMode::Front },
	}) {}
};

class SortModeMatcher : public wsw::EnumTokenMatcher<SortMode, SortModeMatcher> {
public:
	SortModeMatcher() : EnumTokenMatcher({
		{ "Portal"_asView, SortMode::Portal },
		{ "Sky"_asView, SortMode::Sky },
		{ "Opaque"_asView, SortMode::Opaque },
		{ "Banner"_asView, SortMode::Banner },
		{ "Underwater"_asView, SortMode::Underwater },
		{ "Additive"_asView, SortMode::Additive },
		{ "Nearest"_asView, SortMode::Nearest },
	}) {}
};

class MaterialKeyMatcher : public wsw::EnumTokenMatcher<MaterialKey, MaterialKeyMatcher> {
public:
	MaterialKeyMatcher() : EnumTokenMatcher({
		{ "Cull"_asView, MaterialKey::Cull },
		{ "SkyParms"_asView, MaterialKey::SkyParams },
		{ "SkyParms2"_asView, MaterialKey::SkyParams2 },
		{ "SkyParmsSides"_asView, MaterialKey::SkyParamsSides },
		{ "FogParms"_asView, MaterialKey::FogParams },
		{ "FogParams"_asView, MaterialKey::FogParams },
		{ "NoMipMaps"_asView, MaterialKey::NoMipMaps },
		{ "NoPicMip"_asView, MaterialKey::NoPicMip },
		{ "NoCompress"_asView, MaterialKey::NoCompress },
		{ "NoFiltering"_asView, MaterialKey::NoFiltering },
		{ "SmallestMipSize"_asView, MaterialKey::SmallestMipSize },
		{ "PolygonOffset"_asView, MaterialKey::PolygonOffset },
		{ "StencilTest"_asView, MaterialKey::StencilTest },
		{ "Sort"_asView, MaterialKey::Sort },
		{ "DeformVertexes"_asView, MaterialKey::DeformVertexes },
		{ "Portal"_asView, MaterialKey::Portal },
		{ "EntityMergable"_asView, MaterialKey::EntityMergable },
		{ "If"_asView, MaterialKey::If },
		{ "EndIf"_asView, MaterialKey::EndIf },
		{ "OffsetMappingScale"_asView, MaterialKey::OffsetMappingScale },
		{ "GlossExponent"_asView, MaterialKey::GlossExponent },
		{ "GlossIntensity"_asView, MaterialKey::GlossIntensity },
		{ "Template"_asView, MaterialKey::Template },
		{ "Skip"_asView, MaterialKey::Skip },
		{ "SoftParticle"_asView, MaterialKey::SoftParticle },
		{ "ForceWorldOutlines"_asView, MaterialKey::ForceWorldOutlines },
	}) {}
};

class RgbGenMatcher : public wsw::EnumTokenMatcher<RgbGen, RgbGenMatcher> {
public:
	RgbGenMatcher() : EnumTokenMatcher({
		{ "Identity"_asView, RgbGen::Identity },
		{ "IdentityLighting"_asView, RgbGen::Identity },
		{ "LightingIdentity"_asView, RgbGen::Identity },
		{ "Wave"_asView, RgbGen::Wave },
		{ "ColorWave"_asView, RgbGen::ColorWave },
		{ "Custom"_asView, RgbGen::Custom },
		{ "CustomWave"_asView, RgbGen::Custom },
		{ "TeamColor"_asView, RgbGen::Custom },
		{ "CustomColorWave"_asView, RgbGen::Custom },
		{ "TeamColorWave"_asView, RgbGen::CustomWave },
		{ "Entity"_asView, RgbGen::Entity },
		{ "EntityWave"_asView, RgbGen::EntityWave },
		{ "OneMinusEntity"_asView, RgbGen::OneMinusEntity },
		{ "Vertex"_asView, RgbGen::Vertex },
		{ "OneMinusVertex"_asView, RgbGen::OneMinusVertex },
		{ "LightingDiffuse"_asView, RgbGen::LightingDiffuse },
		{ "ExactVertex"_asView, RgbGen::ExactVertex },
		{ "Const"_asView, RgbGen::Const },
		{ "Constant"_asView, RgbGen::Const },
	}) {}
};

class AlphaGenMatcher : public wsw::EnumTokenMatcher<AlphaGen, AlphaGenMatcher> {
public:
	AlphaGenMatcher() : EnumTokenMatcher({
		{ "Vertex"_asView, AlphaGen::Vertex },
		{ "OneMinusVertex"_asView, AlphaGen::OneMinusVertex },
		{ "Entity"_asView, AlphaGen::Entity },
		{ "Wave"_asView, AlphaGen::Wave },
		{ "Const"_asView, AlphaGen::Const },
		{ "Constant"_asView, AlphaGen::Const },
	}) {}
};

class SrcBlendMatcher : public wsw::EnumTokenMatcher<SrcBlend, SrcBlendMatcher> {
public:
	SrcBlendMatcher() : EnumTokenMatcher({
		{ "GL_zero"_asView, SrcBlend::Zero },
		{ "GL_one"_asView, SrcBlend::One },
		{ "GL_dst_color"_asView, SrcBlend::DstColor },
		{ "GL_one_minus_dst_color"_asView, SrcBlend::OneMinusDstColor },
		{ "GL_src_alpha"_asView, SrcBlend::SrcAlpha },
		{ "GL_one_minus_src_alpha"_asView, SrcBlend::OneMinusSrcAlpha },
		{ "GL_dst_alpha"_asView, SrcBlend::DstAlpha },
		{ "GL_one_minus_dst_alpha"_asView, SrcBlend::OneMinusDstAlpha },
	}) {}
};

class DstBlendMatcher : public wsw::EnumTokenMatcher<DstBlend, DstBlendMatcher> {
public:
	DstBlendMatcher() : EnumTokenMatcher({
		{ "GL_zero"_asView, DstBlend::Zero },
		{ "GL_one"_asView, DstBlend::One },
		{ "GL_src_color"_asView, DstBlend::SrcColor },
		{ "GL_one_minus_src_color"_asView, DstBlend::OneMinusSrcColor },
		{ "GL_src_alpha"_asView, DstBlend::SrcAlpha },
		{ "GL_one_minus_src_alpha"_asView, DstBlend::OneMinusSrcAlpha },
		{ "GL_dst_alpha"_asView, DstBlend::DstAlpha },
		{ "GL_one_minus_dst_alpha"_asView, DstBlend::OneMinusDstAlpha },
	}) {}
};

class UnaryBlendFuncMatcher : public wsw::EnumTokenMatcher<UnaryBlendFunc, UnaryBlendFuncMatcher> {
public:
	UnaryBlendFuncMatcher() : EnumTokenMatcher({
		{ "Blend"_asView, UnaryBlendFunc::Blend },
		{ "Filter"_asView, UnaryBlendFunc::Filter },
		{ "Add"_asView, UnaryBlendFunc::Add },
	}) {}
};

class AlphaFuncMatcher : public wsw::EnumTokenMatcher<AlphaFunc, AlphaFuncMatcher> {
public:
	AlphaFuncMatcher() : EnumTokenMatcher({
		{ "Gt0"_asView, AlphaFunc::GT0 },
		{ "Lt128"_asView, AlphaFunc::LT128 },
		{ "Ge128"_asView, AlphaFunc::GE128 },
	}) {}
};

class TCModMatcher : public wsw::EnumTokenMatcher<TCMod, TCModMatcher> {
public:
	TCModMatcher() : EnumTokenMatcher({
		{ "Rotate"_asView, TCMod::Rotate },
		{ "Scale"_asView, TCMod::Scale },
		{ "Scroll"_asView, TCMod::Scroll },
		{ "Stretch"_asView, TCMod::Stretch },
		{ "Transform"_asView, TCMod::Transform },
		{ "Turb"_asView, TCMod::Turb },
	}) {}
};

class TCGenMatcher : public wsw::EnumTokenMatcher<TCGen, TCGenMatcher> {
public:
	TCGenMatcher() : EnumTokenMatcher({
		{ "Base"_asView, TCGen::Base },
		{ "Lightmap"_asView, TCGen::Lightmap },
		{ "Environment"_asView, TCGen::Environment },
		{ "Vector"_asView, TCGen::Vector },
		{ "Reflection"_asView, TCGen::Reflection },
		{ "Celshade"_asView, TCGen::Celshade },
		{ "Surround"_asView, TCGen::Surround },
	}) {}
};

class SkySideMatcher : public wsw::EnumTokenMatcher<SkySide, SkySideMatcher> {
public:
	SkySideMatcher() : EnumTokenMatcher({
		{ "Rt"_asView, SkySide::Right },
		{ "Bk"_asView, SkySide::Back },
		{ "Lf"_asView, SkySide::Left },
		{ "Rt"_asView, SkySide::Right },
		{ "Up"_asView, SkySide::Up },
		{ "Dn"_asView, SkySide::Down },
	}) {}
};

#define IMPLEMENT_GET_ENUM_METHOD( type, method, Matcher ) \
auto MaterialLexer::method() -> std::optional<type> {\
	if( const auto token = getNextTokenInLine() ) {\
		if( const auto func = Matcher::instance().match( *token ) ) {\
			return func;\
		}\
		unGetToken();\
	}\
	return std::nullopt;\
}

IMPLEMENT_GET_ENUM_METHOD( Func, getFunc, FuncTokenMatcher )
IMPLEMENT_GET_ENUM_METHOD( Deform, getDeform, DeformTokenMatcher )
IMPLEMENT_GET_ENUM_METHOD( PassKey, getPassKey, PassKeyMatcher )
IMPLEMENT_GET_ENUM_METHOD( IntConditionVar, getIntConditionVar, IntConditionVarMatcher )
IMPLEMENT_GET_ENUM_METHOD( BoolConditionVar, getBoolConditionVar, BoolConditionVarMatcher )
IMPLEMENT_GET_ENUM_METHOD( LogicOp, getLogicOp, LogicOpMatcher )
IMPLEMENT_GET_ENUM_METHOD( CmpOp, getCmpOp, CmpOpMatcher )
IMPLEMENT_GET_ENUM_METHOD( CullMode, getCullMode, CullModeMatcher )
IMPLEMENT_GET_ENUM_METHOD( SortMode, getSortMode, SortModeMatcher )
IMPLEMENT_GET_ENUM_METHOD( MaterialKey, getMaterialKey, MaterialKeyMatcher )
IMPLEMENT_GET_ENUM_METHOD( RgbGen, getRgbGen, RgbGenMatcher )
IMPLEMENT_GET_ENUM_METHOD( AlphaGen, getAlphaGen, AlphaGenMatcher )
IMPLEMENT_GET_ENUM_METHOD( SrcBlend, getSrcBlend, SrcBlendMatcher )
IMPLEMENT_GET_ENUM_METHOD( DstBlend, getDstBlend, DstBlendMatcher )
IMPLEMENT_GET_ENUM_METHOD( UnaryBlendFunc, getUnaryBlendFunc, UnaryBlendFuncMatcher )
IMPLEMENT_GET_ENUM_METHOD( AlphaFunc, getAlphaFunc, AlphaFuncMatcher )
IMPLEMENT_GET_ENUM_METHOD( TCMod, getTCMod, TCModMatcher )
IMPLEMENT_GET_ENUM_METHOD( TCGen, getTCGen, TCGenMatcher )
IMPLEMENT_GET_ENUM_METHOD( SkySide, getSkySide, SkySideMatcher )

static const wsw::StringView kEqualLiteral( "equal" );

[[nodiscard]]
auto MaterialLexer::getDepthFunc() -> std::optional<DepthFunc> {
	// Keep the original behaviour
	if( const auto maybeToken = getNextTokenInLine() ) {
		return maybeToken->equalsIgnoreCase( kEqualLiteral ) ? DepthFunc::EQ : DepthFunc::GT;
	}
	return std::nullopt;
}

static const wsw::StringView kTrueLiteral( "true" );
static const wsw::StringView kFalseLiteral( "false" );

auto MaterialLexer::getBool() -> std::optional<bool> {
	if( const auto maybeToken = getNextToken() ) {
		if( kTrueLiteral.equalsIgnoreCase( *maybeToken ) ) {
			return true;
		}
		if( kFalseLiteral.equalsIgnoreCase( *maybeToken ) ) {
			return false;
		}
		unGetToken();
	}
	return std::nullopt;
}

bool MaterialLexer::parseVector( float *dest, size_t numElems ) {
	assert( numElems > 1 && numElems <= 8 );
	float scratchpad[8];

	bool hadParenAtStart = false;
	if( const auto maybeFirstToken = getNextTokenInLine() ) {
		auto token = *maybeFirstToken;
		if( token.equals( wsw::StringView( "(" ) ) ) {
			hadParenAtStart = true;
		} else if( !unGetToken() ) {
			return false;
		}
	}

	for( size_t i = 0; i < numElems; ++i ) {
		if( const auto maybeFloat = getFloat() ) {
			scratchpad[i] = *maybeFloat;
		} else {
			return false;
		}
	}

	// Modify the dest array if and only if parsing has succeeded

	if( !hadParenAtStart ) {
		std::copy( scratchpad, scratchpad + numElems, dest );
		return true;
	}

	if( const auto maybeNextToken = getNextTokenInLine() ) {
		const auto token = *maybeNextToken;
		if( token.equals( wsw::StringView( ")" ) ) ) {
			std::copy( scratchpad, scratchpad + numElems, dest );
			return true;
		}
	}

	return false;
}

void MaterialLexer::parseVectorOrFill( float *dest, size_t numElems, float defaultValue ) {
	assert( numElems > 1 && numElems <= 8 );

	bool hadParenAtStart = false;
	if( const auto maybeFirstToken = getNextTokenInLine() ) {
		if( ( *maybeFirstToken ).equals( wsw::StringView( "(" ) ) ) {
			hadParenAtStart = true;
		} else {
			unGetToken();
		}
	}

	size_t i = 0;
	for(; i < numElems; ++i ) {
		if( const auto maybeFloat = getFloat() ) {
			dest[i] = *maybeFloat;
		} else {
			break;
		}
	}

	std::fill( dest + i, dest + numElems, defaultValue );

	if( hadParenAtStart ) {
		if( const auto maybeNextToken = getNextTokenInLine() ) {
			if( !( *maybeNextToken ).equals( wsw::StringView( ")" ) ) ) {
				unGetToken();
			}
		}
	}
}

bool MaterialLexer::skipToEndOfLine() {
	// Could be optimized but it gets called rarely (TODO: Really?)
	for(;; ) {
		if( getNextTokenInLine() == std::nullopt ) {
			return true;
		}
	}
}

template <typename Predicate>
class CharLookupTable {
	bool values[256];
public:
	CharLookupTable() noexcept {
		memset( values, 0, sizeof( values ) );

		const Predicate predicate;
		for( int i = 0; i < 256; ++i ) {
			if( predicate( (uint8_t)i ) ) {
				values[i] = true;
			}
		}
	}

	bool operator()( char ch ) const {
		return values[(uint8_t)ch];
	}
};

struct IsSpace {
	bool operator()( uint8_t ch ) const {
		for( uint8_t spaceCh : { ' ', '\f', '\n', '\r', '\t', '\v' } ) {
			if( spaceCh == ch ) {
				return true;
			}
		}
		return false;
	}
};

static CharLookupTable<IsSpace> isSpace;

struct IsNewlineChar {
	bool operator()( uint8_t ch ) const {
		return ch == (uint8_t)'\n' || ch == (uint8_t)'\r';
	}
};

static CharLookupTable<IsNewlineChar> isNewlineChar;

struct IsValidNonNewlineChar {
	bool operator()( uint8_t ch ) const {
		return ch != (uint8_t)'\0' && ch != (uint8_t)'\n' && ch != (uint8_t)'\r';
	}
};

static CharLookupTable<IsValidNonNewlineChar> isValidNonNewlineChar;

struct IsLastStringLiteralChar {
	bool operator()( uint8_t ch ) const {
		return ch == (uint8_t)'"' || ch == (uint8_t)'\0';
	}
};

static CharLookupTable<IsLastStringLiteralChar> isLastStringLiteralChar;

auto TokenSplitter::fetchNextTokenInLine() -> std::optional<std::pair<unsigned, unsigned>> {
	const char *__restrict p = m_data + m_offset;

start:
	// Strip whitespace characters until a non-whitespace one or a newline character is met
	for(;; p++ ) {
		if( !isSpace( *p ) ) {
			break;
		}
		if( !isNewlineChar( *p ) ) {
			continue;
		}
		// Strip newline characters
		p++;
		while( isNewlineChar( *p ) ) {
			p++;
		}
		m_offset = p - m_data;
		return std::nullopt;
	}

	if( !*p ) {
		m_offset = p - m_data;
		return std::nullopt;
	}

	if( *p == '/' ) {
		if( p[1] == '/' ) {
			// Skip till end of line
			while( isValidNonNewlineChar( *p ) ) {
				p++;
			}
			// Strip newline at the end
			while( isNewlineChar( *p ) ) {
				p++;
			}
			m_offset = p - m_data;
			return std::nullopt;
		}

		if( p[1] == '*' ) {
			bool metNewline = false;
			// Skip till "*/" is met
			for(;; p++ ) {
				if( !*p ) {
					m_offset = p - m_data;
					return std::nullopt;
				}
				// TODO: Should we mark newlines met?
				if( *p == '*' ) {
					if( p[1] == '/' ) {
						p += 2;
						m_offset = p - m_data;
						break;
					}
				}
				metNewline |= isNewlineChar( *p );
			}
			if( metNewline ) {
				m_offset = p - m_data;
				return std::nullopt;
			}
			// We may just recurse but this can lead to an overflow at bogus files with tons of comments
			goto start;
		}
	}

	if( *p == '"' ) {
		p++;
		const char *tokenStart = p;
		for(;; p++ ) {
			// TODO: What if '\n', '\r' (as a single byte) are met inside a string?
			if( isLastStringLiteralChar( *p ) ) {
				m_offset = p - m_data + 1;
				// What if a string is empty?
				return std::make_pair( tokenStart - m_data, p - tokenStart );
			}
		}
	}

	if( const auto maybeSpanLen = tryMatching1Or2CharsToken( p ) ) {
		const auto len = *maybeSpanLen;
		m_offset = ( p - m_data ) + len;
		return std::make_pair( p - m_data, len );
	}

	const char *tokenStart = p;
	for(;; p++ ) {
		if( !mustCloseTokenAtChar( p[0], p[1] ) ) {
			continue;
		}
		m_offset = p - m_data;
		auto len = p - tokenStart;
		assert( len >= 0 );
		return std::make_pair( tokenStart - m_data, len );
	}
}

auto TokenSplitter::tryMatching1Or2CharsToken( const char *tokenStart ) const -> std::optional<unsigned> {
	const char ch = tokenStart[0];

	if( ch == '{' || ch == '}' || ch == '(' || ch == ')' ) {
		return 1;
	}

	if( ch == '<' || ch == '>' || ch == '!' ) {
		return ( tokenStart[1] == '=' ) ? 2 : 1;
	}

	if( ch == '=' && tokenStart[1] == '=' ) {
		return 2;
	}

	return std::nullopt;
}

struct CloseTokenAt1Char {
	bool operator()( char ch ) const {
		if( isSpace( ch ) ) {
			return true;
		}
		if( ch == '\0' || ch == '"' ) {
			return true;
		}
		if( ch == '{' || ch == '}' || ch == '(' || ch == ')' ) {
			return true;
		}
		if( ch == '>' || ch == '<' || ch == '!' ) {
			return true;
		}
		return false;
	}
};

static CharLookupTable<CloseTokenAt1Char> closeTokenAt1Char;

bool TokenSplitter::mustCloseTokenAtChar( char ch, char nextCh ) {
	if( closeTokenAt1Char( ch ) ) {
		return true;
	}

	if( ch == '/' && ( nextCh == '/' || nextCh == '*' ) ) {
		return true;
	}

	return ch == '=' && nextCh == '=';
}