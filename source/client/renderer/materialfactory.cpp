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
#include "local.h"
#include "program.h"

using wsw::operator""_asView;

auto MaterialFactory::create2DMaterialBypassingCache() -> shader_t * {
	auto *texture = TextureCache::instance()->getUnderlyingFactory()->createRaw2DTexture();
	if( !texture ) {
		return nullptr;
	}

	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();

	shader_t *material = initMaterial( SHADER_TYPE_2D, wsw::HashedStringView(), memSpec );

	material->flags = 0;
	material->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	material->sort = SHADER_SORT_ADDITIVE;
	material->numpasses = 1;
	material->passes = passSpec.get( material );

	auto *pass = &material->passes[0];
	pass->flags = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
	pass->rgbgen.type = RGB_GEN_VERTEX;
	pass->alphagen.type = ALPHA_GEN_VERTEX;
	pass->tcgen = TC_GEN_BASE;
	pass->images[0] = texture;

	return material;
}

void MaterialFactory::release2DMaterialBypassingCache( shader_t *material ) {
	if( material ) {
		auto *image = material->passes[0].images[0];
		assert( image );
		TextureCache::instance()->getUnderlyingFactory()->releaseRaw2DTexture( (Raw2DTexture *)image );
		material->~shader_t();
		Q_free( material );
	}
}

bool MaterialFactory::update2DMaterialImageBypassingCache( shader_t *material, const wsw::StringView &name, const ImageOptions &options ) {
	if( material ) {
		assert( material->type == SHADER_TYPE_2D );
		assert( material->numpasses == 1 );
		assert( material->passes[0].images[0] );
		auto *texture = material->passes[0].images[0];
		return TextureCache::instance()->getUnderlyingFactory()->updateRaw2DTexture( (Raw2DTexture *)texture, name, options );
	}
	return false;
}

auto MaterialFactory::initMaterial( int type, const wsw::HashedStringView &cleanName, wsw::MemSpecBuilder memSpec )
	-> shader_t * {
	assert( memSpec.sizeSoFar() >= sizeof( shader_t ) );
	auto nameSpec = memSpec.add<char>( cleanName.size() + 1 );

	void *const mem = Q_malloc( memSpec.sizeSoFar() );
	auto *const s = new( mem )shader_t;
	s->type = (decltype( s->type ) )type;

	if( type >= SHADER_TYPE_BSP_MIN && type <= SHADER_TYPE_BSP_MAX ) {
		s->imagetags = IMAGE_TAG_WORLD;
	} else {
		s->imagetags = IMAGE_TAG_GENERIC;
	}

	// set defaults
	s->flags = SHADER_CULL_FRONT;
	s->vattribs = 0;
	s->glossIntensity = 0;
	s->glossExponent = 0;
	s->offsetmappingScale = 1;

	char *nameData = nameSpec.get( s );
	cleanName.copyTo( nameData, cleanName.size() + 1 );
	s->name = wsw::HashedStringView( nameData, cleanName.size(), cleanName.getHash(), wsw::StringView::ZeroTerminated );

	return s;
}

auto MaterialFactory::newDefaultVertexMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();

	shader_t *s = initMaterial( SHADER_TYPE_VERTEX, cleanName, memSpec );

	// vertex lighting
	s->flags = SHADER_DEPTHWRITE | SHADER_CULL_FRONT;
	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	s->sort = SHADER_SORT_OPAQUE;
	s->numpasses = 1;
	auto *pass = s->passes = passSpec.get( s );

	pass->flags = GLSTATE_DEPTHWRITE;
	pass->tcgen = TC_GEN_BASE;
	pass->rgbgen.type = RGB_GEN_VERTEX;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->images[0] = findImage( name, IT_SRGB );

	return s;
}

auto MaterialFactory::newDefaultDeluxeMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();
	shader_t *s = initMaterial( SHADER_TYPE_DELUXEMAP, cleanName, memSpec );

	// deluxemapping

	Texture *images[3] { nullptr, nullptr, nullptr };
	// TODO: Name or clean name?
	loadMaterial( images, name, 0 );

	s->flags = SHADER_DEPTHWRITE | SHADER_CULL_FRONT | SHADER_LIGHTMAP;
	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_LMCOORDS0_BIT | VATTRIB_NORMAL_BIT | VATTRIB_SVECTOR_BIT;
	s->sort = SHADER_SORT_OPAQUE;
	s->numpasses = 1;
	s->passes = passSpec.get( s );

	auto *pass = &s->passes[0];
	pass->flags = GLSTATE_DEPTHWRITE;
	pass->tcgen = TC_GEN_BASE;
	pass->rgbgen.type = RGB_GEN_IDENTITY;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->program_type = GLSL_PROGRAM_TYPE_MATERIAL;
	pass->images[0] = findImage( name, IT_SRGB );
	pass->images[1] = images[0]; // normalmap
	pass->images[2] = images[1]; // glossmap
	pass->images[3] = images[2]; // decalmap

	return s;
}

auto MaterialFactory::newDefaultCoronaMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();

	shader_t *s = initMaterial( SHADER_TYPE_CORONA, cleanName, memSpec );

	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	s->sort = SHADER_SORT_ADDITIVE;
	s->numpasses = 1;
	s->passes = passSpec.get( s );
	s->flags = SHADER_SOFT_PARTICLE;

	auto *pass = &s->passes[0];
	pass->flags = GLSTATE_SRCBLEND_ONE | GLSTATE_DSTBLEND_ONE;
	pass->rgbgen.type = RGB_GEN_VERTEX;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->tcgen = TC_GEN_BASE;
	pass->images[0] = findImage( wsw::StringView( "*corona" ), IT_SPECIAL );

	return s;
}

auto MaterialFactory::newDefaultDiffuseMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	auto passSpec = memSpec.add<shaderpass_t>();

	shader_s *s = initMaterial( SHADER_TYPE_DIFFUSE, cleanName, memSpec );

	Texture *materialImages[3];

	// load material images
	// TODO: Name or clean name?
	loadMaterial( materialImages, cleanName, 0 );

	s->flags = SHADER_DEPTHWRITE | SHADER_CULL_FRONT;
	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_NORMAL_BIT;
	s->sort = SHADER_SORT_OPAQUE;
	s->numpasses = 1;
	s->passes = passSpec.get( s );

	auto *pass = &s->passes[0];
	pass->flags = GLSTATE_DEPTHWRITE;
	pass->rgbgen.type = RGB_GEN_IDENTITY;
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->tcgen = TC_GEN_BASE;
	pass->program_type = GLSL_PROGRAM_TYPE_MATERIAL;
	pass->images[0] = findImage( name, IT_SRGB );
	pass->images[1] = materialImages[0]; // normalmap
	pass->images[2] = materialImages[1]; // glossmap
	pass->images[3] = materialImages[2]; // decalmap
	s->vattribs |= VATTRIB_SVECTOR_BIT | VATTRIB_NORMAL_BIT;

	return s;
}

auto MaterialFactory::newDefault2DLikeMaterial( int type,
											  const wsw::HashedStringView &cleanName,
											  const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();

	shader_t *s = initMaterial( type, cleanName, memSpec );

	s->flags = 0;
	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT | VATTRIB_COLOR0_BIT;
	s->sort = SHADER_SORT_ADDITIVE;
	s->numpasses = 1;
	s->passes = passSpec.get( s );

	auto *pass = &s->passes[0];
	pass->flags = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
	pass->rgbgen.type = RGB_GEN_VERTEX;
	pass->alphagen.type = ALPHA_GEN_VERTEX;
	pass->tcgen = TC_GEN_BASE;
	if( type == SHADER_TYPE_2D_LINEAR ) {
		pass->images[0] = findImage( name, s->flags | IT_SPECIAL | IT_SYNC );
	} else if( type != SHADER_TYPE_2D_RAW ) {
		pass->images[0] = findImage( name, s->flags | IT_SPECIAL | IT_SYNC | IT_SRGB );
	}

	return s;
}

auto MaterialFactory::newOpaqueEnvMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_s * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );
	const auto passSpec = memSpec.add<shaderpass_t>();

	auto *s = initMaterial( SHADER_TYPE_OPAQUE_ENV, cleanName, memSpec );

	s->vattribs = VATTRIB_POSITION_BIT;
	s->sort = SHADER_SORT_OPAQUE;
	s->flags = SHADER_CULL_FRONT | SHADER_DEPTHWRITE;
	s->numpasses = 1;
	s->passes = passSpec.get( s );

	auto *pass = &s->passes[0];
	pass->flags = GLSTATE_DEPTHWRITE;
	pass->rgbgen.type = RGB_GEN_ENVIRONMENT;
	VectorClear( pass->rgbgen.args );
	pass->alphagen.type = ALPHA_GEN_IDENTITY;
	pass->tcgen = TC_GEN_NONE;
	pass->images[0] = TextureCache::instance()->whiteTexture();

	return s;
}

auto MaterialFactory::newFogMaterial( const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	wsw::MemSpecBuilder memSpec( wsw::MemSpecBuilder::withInitialSizeOf<shader_t>() );

	auto *s = initMaterial( SHADER_TYPE_FOG, cleanName, memSpec );
	s->vattribs = VATTRIB_POSITION_BIT | VATTRIB_TEXCOORDS_BIT;
	s->sort = SHADER_SORT_FOG;
	s->flags = SHADER_CULL_FRONT;
	s->numpasses = 0;
	return s;
}

void MaterialFactory::destroyMaterial( shader_s *material ) {
	if( material ) {
		material->~shader_s();
		Q_free( material );
	}
}

class BuiltinTexMatcher {
	// We have to compose tokens based on a supplied name. Do that without touching heap.
	struct Chunk {
		char data[16];
	};

	wsw::StaticVector<Chunk, 12> stringChunks;

	using TexNumberViews = wsw::StaticVector<std::pair<wsw::StringView, BuiltinTexNum>, 6>;

	TexNumberViews longTexNumbers;
	TexNumberViews shortTexNumbers;

	[[nodiscard]]
	auto makeView( const char *prefix, const char *name, const char *suffix ) -> wsw::StringView {
		char *data = stringChunks.unsafe_grow_back()->data;
		Q_snprintfz( data, sizeof( Chunk::data ), "%s%s%s", prefix, name, suffix );
		return wsw::StringView( data );
	}

	void add( const char *name, BuiltinTexNum texNumber ) noexcept {
		assert( std::strlen( name ) < sizeof( Chunk::data ) );

		longTexNumbers.emplace_back( std::make_pair( makeView( "$", name, "image" ), texNumber ) );
		shortTexNumbers.emplace_back( std::make_pair( makeView( "*", name, "" ), texNumber ) );
	}

	[[nodiscard]]
	static auto matchInList( const wsw::StringView &image, const TexNumberViews &views )
		-> std::optional<BuiltinTexNum> {
		for( const auto &[name, texNum] : views ) {
			if( name.equalsIgnoreCase( image ) ) {
				return std::optional( texNum );
			}
		}
		return std::nullopt;
	}
public:
	BuiltinTexMatcher() noexcept {
		add( "white", BuiltinTexNum::White );
		add( "black", BuiltinTexNum::Black );
		add( "grey", BuiltinTexNum::Grey );
		add( "blankbump", BuiltinTexNum::BlankBump );
		add( "particle", BuiltinTexNum::Particle );
		add( "corona", BuiltinTexNum::Corona );
	}

	[[nodiscard]]
	auto match( const wsw::StringView &image ) -> std::optional<BuiltinTexNum> {
		// Try matching long tokens (they're more likely to be met in wsw assets)
		if( const auto num = matchInList( image, longTexNumbers ) ) {
			return num;
		}
		return matchInList( image, shortTexNumbers );
	}
};

static BuiltinTexMatcher builtinTexMatcher;

static const wsw::StringView kLightmapPrefix( "*lm" );

auto MaterialFactory::findImage( const wsw::StringView &name, int flags ) -> Texture * {
	if( const auto maybeBuiltinTexNum = builtinTexMatcher.match( name ) ) {
		return TextureCache::instance()->getBuiltinTexture( *maybeBuiltinTexNum );
	}

	if( kLightmapPrefix.equalsIgnoreCase( name.take( 3 ) ) ) {
		return TextureCache::instance()->whiteTexture();
	}

	Texture *texture;
	auto *const textureCache = TextureCache::instance();
	if( flags & IT_CUBEMAP ) {
		if( !( texture = textureCache->getMaterialCubemap( name, flags ) ) ) {
			texture = textureCache->whiteCubemapTexture();
		}
	} else {
		if( !( texture = textureCache->getMaterial2DTexture( name, flags ) ) ) {
			texture = textureCache->noTexture();
		}
	}

	return texture;
}

void MaterialFactory::loadMaterial( Texture **images, const wsw::StringView &fullName, int addFlags ) {
	// set defaults
	images[0] = images[1] = images[2] = nullptr;

	auto *const textureCache = TextureCache::instance();
	// load normalmap image
	images[0] = textureCache->getMaterial2DTexture( fullName, kNormSuffix, ( addFlags | IT_NORMALMAP ) );

	// load glossmap image
	if( v_lighting_specular.get() ) {
		images[1] = textureCache->getMaterial2DTexture( fullName, kGlossSuffix, addFlags );
	}

	images[2] = textureCache->getMaterial2DTexture( fullName, kDecalSuffix, addFlags );
	if( !images[2] ) {
		images[2] = textureCache->getMaterial2DTexture( fullName, kAddSuffix, addFlags );
	}
}

auto MaterialFactory::expandTemplate( const wsw::StringView &name, const wsw::StringView *args, size_t numArgs )
	-> MaterialLexer * {
	if( MaterialSource *const source = m_materialCache->findSourceByName( name ) ) {
		m_expansionBuffer.clear();
		m_templateTokenSpans.clear();
		if( source->expandTemplate( args, numArgs, m_expansionBuffer, m_templateTokenSpans ) ) {
			if( !m_templateLexerHolder.empty() ) {
				m_templateLexerHolder.pop_back();
				m_templateTokenStreamHolder.pop_back();
			}

			void *streamMem = m_templateTokenStreamHolder.unsafe_grow_back();
			new( streamMem )TokenStream( source->getCharData(), m_templateTokenSpans.data(),
										 (int)m_templateTokenSpans.size(), m_expansionBuffer.data() );

			void *lexerMem = m_templateLexerHolder.unsafe_grow_back();
			new( lexerMem )MaterialLexer( m_templateTokenStreamHolder.begin() );

			return m_templateLexerHolder.begin();
		}
	}
	return nullptr;
}

auto MaterialFactory::newMaterial( int type, const wsw::HashedStringView &cleanName,
								   const wsw::StringView &name ) -> shader_t * {
	if( MaterialSource *const source = m_materialCache->findSourceByName( cleanName ) ) {
		if( !m_primaryTokenStreamHolder.empty() ) {
			m_primaryTokenStreamHolder.pop_back();
		}
		void *const mem = m_primaryTokenStreamHolder.unsafe_grow_back();
		const auto &[spans, numTokens] = source->getTokenSpans();
		auto *const stream = new( mem )TokenStream( source->getCharData(), spans, (int)numTokens );
		MaterialParser parser( this, stream, name, cleanName, (shaderType_e)type );
		return parser.exec();
	}
	return nullptr;
}

auto MaterialFactory::newDefaultMaterial( int type, const wsw::HashedStringView &cleanName, const wsw::StringView &name )
	-> shader_t * {
	switch( type ) {
		case SHADER_TYPE_VERTEX:
			return newDefaultVertexMaterial( cleanName, name );
		case SHADER_TYPE_DELUXEMAP:
			return newDefaultDeluxeMaterial( cleanName, name );
		case SHADER_TYPE_CORONA:
			return newDefaultCoronaMaterial( cleanName, name );
		case SHADER_TYPE_DIFFUSE:
			return newDefaultDiffuseMaterial( cleanName, name );
		case SHADER_TYPE_2D:
		case SHADER_TYPE_2D_RAW:
		case SHADER_TYPE_VIDEO:
		case SHADER_TYPE_2D_LINEAR:
			return newDefault2DLikeMaterial( type, cleanName, name );
		case SHADER_TYPE_OPAQUE_ENV:
			return newOpaqueEnvMaterial( cleanName, name );
		case SHADER_TYPE_FOG:
			return newFogMaterial( cleanName, name );
	}

	return nullptr;
}