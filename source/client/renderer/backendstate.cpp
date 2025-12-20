/*
Copyright (C) 2011 Victor Luchits

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

#include "local.h"
#include "program.h"
#include "backendactiontape.h"
#include "backendstate.h"
#include "buffermanagement.h"
#include "texturemanagement.h"
#include "uploadmanager.h"

#include <common/helpers/noise.h>

static const shaderpass_t kBuiltinFogPass {
	.flags        = GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA,
	.rgbgen       = { .type = RGB_GEN_FOG },
	.alphagen     = { .type = ALPHA_GEN_IDENTITY },
	.tcgen        = TC_GEN_FOG,
	.program_type = GLSL_PROGRAM_TYPE_FOG,
};

static const shaderpass_t kBuiltinShadowmapPass {
	.flags        = GLSTATE_DEPTHFUNC_EQ | GLSTATE_SRCBLEND_ZERO | GLSTATE_DSTBLEND_SRC_COLOR,
	.rgbgen       = { .type = RGB_GEN_IDENTITY },
	.alphagen     = { .type = ALPHA_GEN_IDENTITY },
	.tcgen        = TC_GEN_NONE,
	.program_type = GLSL_PROGRAM_TYPE_SHADOWMAP,
};

static const shaderpass_t kBuiltinOutlinePass {
	.flags        = GLSTATE_DEPTHWRITE,
	.rgbgen       = { .type = RGB_GEN_OUTLINE },
	.alphagen     = { .type = ALPHA_GEN_OUTLINE },
	.tcgen        = TC_GEN_NONE,
	.program_type = GLSL_PROGRAM_TYPE_OUTLINE,
};

SimulatedBackendState::SimulatedBackendState( UploadManager *uploadManager, unsigned uniformUploadCategory,
											  BackendActionTape *actionTape, int width, int height )
	: m_rhiState( actionTape, width, height ), m_uploadManager( uploadManager ), m_actionTape( actionTape ) {

	m_uniformState.sliceId = m_uploadManager->beginUniformUploads( &m_uniformState.currentOffsets, uniformUploadCategory );
	memcpy( &m_uniformState.initialOffsets, &m_uniformState.currentOffsets, sizeof( UniformBlockOffsets ) );

	std::memset( &m_globalState, 0, sizeof( m_globalState ) );
	std::memset( &m_drawState, 0, sizeof( m_drawState ) );
	std::memset( &m_materialState, 0, sizeof( m_materialState ) );
	std::memset( &m_programState, 0, sizeof( m_programState ) );

	Vector4Set( m_globalState.nullEnt.shaderRGBA, 1, 1, 1, 1 );
	m_globalState.nullEnt.scale = 1.0f;
	VectorClear( m_globalState.nullEnt.origin );
	Matrix3_Identity( m_globalState.nullEnt.axis );

	actionTape->append( []( RuntimeBackendState * ) { R_SetDefaultGLState(); } );
}

void SimulatedBackendState::setTime( int64_t requestTime, int64_t globalTime ) {
	m_globalState.time               = requestTime;
	m_globalState.nullEnt.shaderTime = globalTime;
}

void SimulatedBackendState::setDepthRange( float depthmin, float depthmax ) {
	m_rhiState.setDepthRange( depthmin, depthmax );
}

void SimulatedBackendState::getDepthRange( float *depthmin, float *depthmax ) {
	m_rhiState.getDepthRange( depthmin, depthmax );
}

void SimulatedBackendState::saveDepthRange() {
	m_rhiState.saveDepthRange();
}

void SimulatedBackendState::restoreDepthRange() {
	m_rhiState.restoreDepthRange();
}

void SimulatedBackendState::loadCameraMatrix( const mat4_t m ) {
	Matrix4_Copy( m, m_globalState.cameraMatrix );
}

void SimulatedBackendState::loadObjectMatrix( const mat4_t m ) {
	Matrix4_Copy( m, m_globalState.objectMatrix );
	Matrix4_MultiplyFast( m_globalState.cameraMatrix, m, m_globalState.modelviewMatrix );
	Matrix4_Multiply( m_globalState.projectionMatrix, m_globalState.modelviewMatrix, m_globalState.modelviewProjectionMatrix );
}

void SimulatedBackendState::getObjectMatrix( float *m ) {
	Matrix4_Copy( m_globalState.objectMatrix, m );
}

void SimulatedBackendState::loadProjectionMatrix( const mat4_t m ) {
	Matrix4_Copy( m, m_globalState.projectionMatrix );
	Matrix4_Multiply( m, m_globalState.modelviewMatrix, m_globalState.modelviewProjectionMatrix );
}

void SimulatedBackendState::transformForWorld() {
	loadObjectMatrix( mat4x4_identity );
}

void SimulatedBackendState::translateForEntity( const entity_t *e ) {
	mat4_t objectMatrix;

	Matrix4_Identity( objectMatrix );

	objectMatrix[0] = e->scale;
	objectMatrix[5] = e->scale;
	objectMatrix[10] = e->scale;
	objectMatrix[12] = e->origin[0];
	objectMatrix[13] = e->origin[1];
	objectMatrix[14] = e->origin[2];

	loadObjectMatrix( objectMatrix );
}

void SimulatedBackendState::transformForEntity( const entity_t *e ) {
	assert( e->rtype == RT_MODEL && e->number != kWorldEntNumber );

	mat4_t objectMatrix;

	if( e->scale != 1.0f ) {
		objectMatrix[0] = e->axis[0] * e->scale;
		objectMatrix[1] = e->axis[1] * e->scale;
		objectMatrix[2] = e->axis[2] * e->scale;
		objectMatrix[4] = e->axis[3] * e->scale;
		objectMatrix[5] = e->axis[4] * e->scale;
		objectMatrix[6] = e->axis[5] * e->scale;
		objectMatrix[8] = e->axis[6] * e->scale;
		objectMatrix[9] = e->axis[7] * e->scale;
		objectMatrix[10] = e->axis[8] * e->scale;
	} else {
		objectMatrix[0] = e->axis[0];
		objectMatrix[1] = e->axis[1];
		objectMatrix[2] = e->axis[2];
		objectMatrix[4] = e->axis[3];
		objectMatrix[5] = e->axis[4];
		objectMatrix[6] = e->axis[5];
		objectMatrix[8] = e->axis[6];
		objectMatrix[9] = e->axis[7];
		objectMatrix[10] = e->axis[8];
	}

	objectMatrix[3] = 0;
	objectMatrix[7] = 0;
	objectMatrix[11] = 0;
	objectMatrix[12] = e->origin[0];
	objectMatrix[13] = e->origin[1];
	objectMatrix[14] = e->origin[2];
	objectMatrix[15] = 1.0;

	loadObjectMatrix( objectMatrix );
}

void SimulatedBackendState::flipFrontFace() {
	m_rhiState.flipFrontFace();
}

void SimulatedBackendState::setScissor( int x, int y, int w, int h ) {
	m_rhiState.setScissor( x, y, w, h );
}

void SimulatedBackendState::getScissor( int *x, int *y, int *w, int *h ) {
	m_rhiState.getScissor( x, y, w, h );
}

void SimulatedBackendState::setViewport( int x, int y, int w, int h ) {
	m_rhiState.setViewport( x, y, w, h );
}

void SimulatedBackendState::clear( int bits, float r, float g, float b, float a ) {
	unsigned state = m_rhiState.getState();

	if( bits & GL_DEPTH_BUFFER_BIT ) {
		state |= GLSTATE_DEPTHWRITE;
	}

	if( bits & GL_STENCIL_BUFFER_BIT ) {
		m_actionTape->clearStencil( 128 );
	}

	if( bits & GL_COLOR_BUFFER_BIT ) {
		state = ( state & ~GLSTATE_NO_COLORWRITE ) | GLSTATE_ALPHAWRITE;
		m_actionTape->clearColor( r, g, b, a );
	}

	m_rhiState.setState( state );

	m_rhiState.applyScissor();

	m_actionTape->clear( bits );

	m_rhiState.setDepthRange( 0.0f, 1.0f );
}

void SimulatedBackendState::setCamera( const vec3_t cameraOrigin, const mat3_t cameraAxis ) {
	VectorCopy( cameraOrigin, m_globalState.cameraOrigin );
	Matrix3_Copy( cameraAxis, m_globalState.cameraAxis );
}

void SimulatedBackendState::setRenderFlags( int flags ) {
	m_globalState.renderFlags = flags;
}

bool SimulatedBackendState::enableWireframe( bool enable ) {
	const bool oldVal = m_globalState.wireframe;

	if( m_globalState.wireframe != enable ) {
		m_globalState.wireframe = enable;

		if( enable ) {
			setShaderStateMask( 0, GLSTATE_NO_DEPTH_TEST );
			m_actionTape->polygonMode( GL_FRONT_AND_BACK, GL_LINE );
		} else {
			setShaderStateMask( ~0, 0 );
			m_actionTape->polygonMode( GL_FRONT_AND_BACK, GL_FILL );
		}
	}

	return oldVal;
}

bool SimulatedBackendState::shouldApplyDrawflatInCurrentState() const {
	return m_materialState.currentModelType == mod_brush &&
		   m_globalState.renderFlags & ( RF_DRAWFLAT | RF_DRAWBRIGHT ) &&
		   !( m_materialState.currentShader->flags & SHADER_NODRAWFLAT );
}

void SimulatedBackendState::updateRequiredVertexAttribs() {
	vattribmask_t vattribs = m_materialState.currentShader->vattribs;
	if( m_drawState.superLightStyle ) {
		vattribs |= m_drawState.superLightStyle->vattribs;
	}
	if( m_drawState.bonesData.numBones ) {
		vattribs |= VATTRIB_BONES_BITS;
	}
	if( m_materialState.currentEntity->outlineHeight != 0.0f ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( shouldApplyDrawflatInCurrentState() ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( m_drawState.currentShadowBits && ( m_materialState.currentModelType == mod_brush ) ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	m_drawState.currentVAttribs = vattribs;
}

void SimulatedBackendState::bindShader( const entity_t *e, const ShaderParams *overrideParams,
										const ShaderParamsTable *paramsTable, const shader_t *shader,
										const mfog_t *fog, const portalSurface_s *portalSurface ) {
	m_materialState.currentShader = shader;
	m_materialState.fog           = fog;
	m_materialState.texFog        = nullptr;
	m_materialState.colorFog      = nullptr;

	m_drawState.doneDepthPass     = false;
	m_drawState.dirtyUniformState = true;

	m_materialState.currentEntity = e ? e : &m_globalState.nullEnt;

	if( m_materialState.currentEntity->rtype == RT_MODEL && m_materialState.currentEntity->model ) {
		m_materialState.currentModelType = m_materialState.currentEntity->model->type;
	} else {
		m_materialState.currentModelType = mod_bad;
	}

	m_drawState.currentDlightBits = 0;
	m_drawState.currentShadowBits = 0;
	m_drawState.superLightStyle   = nullptr;

	m_drawState.bonesData.numBones   = 0;
	m_drawState.bonesData.maxWeights = 0;

	m_materialState.currentPortalSurface = portalSurface;

	if( !e ) {
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			m_materialState.currentShaderTime = 1e-3 * (double)materialParams->shaderTime;
			m_materialState.currentShaderFrac = materialParams->shaderFrac;
		} else {
			m_materialState.currentShaderTime = 1e-3 * (double)m_globalState.nullEnt.shaderTime;
			m_materialState.currentShaderFrac = 0.0f;
		}
		m_materialState.alphaHack    = false;
		m_materialState.greyscale    = false;
		m_materialState.noDepthTest  = false;
		m_materialState.noColorWrite = false;
		m_materialState.depthEqual   = false;
	} else {
		Vector4Copy( m_materialState.currentEntity->shaderRGBA, m_materialState.entityColor );
		Vector4Copy( m_materialState.currentEntity->outlineColor, m_materialState.entityOutlineColor );
		int64_t givenShaderTime;
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			givenShaderTime = materialParams->shaderTime;
			m_materialState.currentShaderFrac = materialParams->shaderFrac;
		} else {
			givenShaderTime = m_materialState.currentEntity->shaderTime;
			m_materialState.currentShaderFrac = 0.0f;
		}
		if( givenShaderTime > m_globalState.time ) {
			m_materialState.currentShaderTime = 0;
		} else {
			m_materialState.currentShaderTime = 1e-3 * (double)( m_globalState.time - givenShaderTime );
		}
		m_materialState.alphaHack    = ( e->renderfx & RF_ALPHAHACK ) != 0;
		m_materialState.hackedAlpha  = (float)e->shaderRGBA[3] / 255.0f;
		m_materialState.greyscale    = ( e->renderfx & RF_GREYSCALE ) != 0;
		m_materialState.noDepthTest  = ( e->renderfx & RF_NODEPTHTEST ) != 0 && e->rtype == RT_SPRITE;
		m_materialState.noColorWrite = ( e->renderfx & RF_NOCOLORWRITE ) != 0;
		m_materialState.depthEqual   = m_materialState.alphaHack && ( e->renderfx & RF_WEAPONMODEL ) != 0;
	}

	if( fog && fog->shader && !m_materialState.noColorWrite ) {
		// should we fog the geometry with alpha texture or scale colors?
		if( !m_materialState.alphaHack && Shader_UseTextureFog( shader ) ) {
			m_materialState.texFog = fog;
		} else {
			// use scaling of colors
			m_materialState.colorFog = fog;
		}
	}

	updateRequiredVertexAttribs();
}

void SimulatedBackendState::setLightstyle( const superLightStyle_t *lightStyle ) {
	assert( m_materialState.currentShader );
	m_drawState.superLightStyle = lightStyle;
	m_drawState.dirtyUniformState = true;

	updateRequiredVertexAttribs();
}

void SimulatedBackendState::setDlightBits( unsigned dlightBits ) {
	assert( m_materialState.currentShader );
	m_drawState.currentDlightBits = dlightBits;
	m_drawState.dirtyUniformState = true;
}

void SimulatedBackendState::setBonesData( int numBones, const dualquat_t *dualQuats, int maxWeights ) {
	assert( m_materialState.currentShader );

	if( numBones > MAX_GLSL_UNIFORM_BONES ) {
		numBones = MAX_GLSL_UNIFORM_BONES;
	}
	if( maxWeights > 4 ) {
		maxWeights = 4;
	}

	m_drawState.bonesData.numBones = numBones;
	memcpy( m_drawState.bonesData.dualQuats, dualQuats, numBones * sizeof( *dualQuats ) );
	m_drawState.bonesData.maxWeights = maxWeights;

	m_drawState.dirtyUniformState = true;

	updateRequiredVertexAttribs();
}

void SimulatedBackendState::setZClip( float zNear, float zFar ) {
	m_globalState.zNear = zNear;
	m_globalState.zFar  = zFar;
}

void SimulatedBackendState::setLightParams( float minLight, bool noWorldLight, float hdrExposure ) {
	m_globalState.minLight     = minLight;
	m_globalState.noWorldLight = noWorldLight;
	m_globalState.hdrExposure  = hdrExposure;
}

void SimulatedBackendState::setShaderStateMask( unsigned ANDmask, unsigned ORmask ) {
	m_globalState.shaderStateANDmask = ANDmask;
	m_globalState.shaderStateORmask  = ORmask;
}

void SimulatedBackendState::drawMeshVerts( const DrawMeshVertSpan *vertSpan, int primitive ) {
	m_rhiState.applyScissor();

	if( const auto *mdSpan = std::get_if<MultiDrawElemSpan>( vertSpan ) ) {
		m_rhiState.multiDrawElements( primitive, mdSpan->counts, GL_UNSIGNED_SHORT, mdSpan->indices, mdSpan->numDraws );
	} else if( const auto *vertElemSpan = std::get_if<VertElemSpan>( vertSpan ) ) {
		const unsigned numVerts  = vertElemSpan->numVerts;
		const unsigned numElems  = vertElemSpan->numElems;
		const unsigned firstVert = vertElemSpan->firstVert;
		const unsigned firstElem = vertElemSpan->firstElem;

		m_rhiState.drawRangeElements( primitive, firstVert, firstVert + numVerts - 1, (int)numElems,
									  GL_UNSIGNED_SHORT, (GLvoid *)( firstElem * sizeof( elem_t ) ) );
	} else {
		assert( false );
	}
}

void SimulatedBackendState::bindMeshBuffer( const MeshBuffer *buffer ) {
	if( buffer ) {
		m_rhiState.bindVertexBuffer( buffer->vboId );
		m_rhiState.bindIndexBuffer( buffer->iboId );
	} else {
		m_rhiState.bindVertexBuffer( 0 );
		m_rhiState.bindIndexBuffer( 0 );
	}
}

void SimulatedBackendState::bindRenderTarget( RenderTargetComponents *components ) {
	m_rhiState.bindRenderTarget( components );
}

void SimulatedBackendState::bindExistingProgram( int program ) {
	assert( program > 0 );
	if( m_programState.boundProgram != program ) {
		m_drawState.dirtyUniformState = true;
		m_programState.boundProgram = program;
		m_actionTape->bindProgram( program );
	}
}

void SimulatedBackendState::setupProgram( int type, uint64_t features ) {
	const shader_s *materialToGetDeforms = m_materialState.currentShader;
	// Deforms bypass BackendState program cache
	// (TODO: Why, were comparisons that expensive?)
	if( materialToGetDeforms->numdeforms ) [[unlikely]] {
		// Try to find an existing program in the global program cache
		const int program = RP_FindProgram( type, materialToGetDeforms, features );
		// TODO: Make a distinction between programs that failed to be created and programs that haven't been created yet
		if( program ) {
			bindExistingProgram( program );
		} else {
			m_drawState.dirtyUniformState = true;
			// Invalidate bound program in the simulated backend state
			m_programState.boundProgram = -1;
			// Create the program during the tape execution
			m_actionTape->createAndBindProgram( type, materialToGetDeforms, features );
		}
	} else {
		// Perform a fast lookup by type and features in the first-level cache
		if( m_programState.cachedFastLookupProgramType == type && m_programState.cachedFastLookupProgramFeatures == features ) {
			assert( m_programState.cachedFastLookupProgram > 0 );
			bindExistingProgram( m_programState.cachedFastLookupProgram );
		} else {
			// Try to find an existing prorgam in the global program cache
			const int program = RP_FindProgram( type, materialToGetDeforms, features );
			// TODO: Make a distinction between programs that failed to be created and programs that haven't been created yet
			if( program ) {
				bindExistingProgram( program );
				// Save it in the first-level cache
				m_programState.cachedFastLookupProgram         = program;
				m_programState.cachedFastLookupProgramType     = type;
				m_programState.cachedFastLookupProgramFeatures = features;
			} else {
				// Force the state invalidation as we don't know the exact program that is getting used
				// TODO: Do we need additional flags, like "can reset dirty uniform state?" - looks like we can check boundProgram < 0
				m_drawState.dirtyUniformState = true;
				// Invalidate lookup cache in the simulated backend state (TODO: Is it needed?)
				m_programState.cachedFastLookupProgram        = -1;
				m_programState.cachedFastLookupProgramType    = -1;
				m_programState.cachedFastLookupProgramFeatures = 0;
				// Invalidate bound program in the simulated backend state
				m_programState.boundProgram = -1;
				// Create the program during the tape execution
				m_actionTape->createAndBindProgram( type, materialToGetDeforms, features );
			}
		}
	}
}

void SimulatedBackendState::bindUniformBufferRange( unsigned binding, GLuint bufferId, unsigned offset, unsigned size ) {
	m_actionTape->bindBufferRange( GL_UNIFORM_BUFFER, binding, bufferId, offset, size );
}

static inline float RB_FastSin( float t ) {
	return std::sin( t * (float)M_TWOPI );
}

[[nodiscard]]
static auto calcFuncValue( unsigned func, float arg ) -> float {
	const float modArg  = std::fmod( arg, 1.0f );
	const float argFrac = modArg + ( modArg >= 0.0f ? 0.0f : 1.0f );
	assert( argFrac >= 0.0f && argFrac <= 1.0f );
	switch( func ) {
		case SHADER_FUNC_SIN:
			return RB_FastSin( argFrac );
		case SHADER_FUNC_TRIANGLE: {
			if( argFrac < 0.25f ) {
				return argFrac * 4.0f;
			} else if( argFrac < 0.75f ) {
				return 2.0f - 4.0f * argFrac;
			} else {
				return ( argFrac - 0.75f ) * 4.0f - 1.0f;
			}
		}
		case SHADER_FUNC_SQUARE:
			return argFrac < 0.5f ? +1.0f : -1.0f;
		case SHADER_FUNC_SAWTOOTH:
			return argFrac;
		case SHADER_FUNC_INVERSESAWTOOTH:
			return 1.0f - argFrac;
		default:
			return RB_FastSin( argFrac );
	}
}

auto SimulatedBackendState::transformFogPlanes( const mfog_t *fog, vec3_t fogNormal, float *fogDist,
												vec3_t vpnNormal, float *vpnDist ) const -> float {
	const entity_t *e = m_materialState.currentEntity;

	assert( fog );
	assert( fogNormal && fogDist );
	assert( vpnNormal && vpnDist );

	const cplane_t *fogPlane = fog->visibleplane;
	const shader_s *fogShader = fog->shader;

	// distance to fog
	const float dist = PlaneDiff( m_globalState.cameraOrigin, fog->visibleplane );
	const float scale = e->scale;

	vec3_t viewtofog;
	if( e->rtype == RT_MODEL ) {
		VectorCopy( e->origin, viewtofog );
	} else {
		VectorClear( viewtofog );
	}

	// some math tricks to take entity's rotation matrix into account
	// for fog texture coordinates calculations:
	// M is rotation matrix, v is vertex, t is transform vector
	// n is plane's normal, d is plane's dist, r is view origin
	// (M*v + t)*n - d = (M*n)*v - ((d - t*n))
	// (M*v + t - r)*n = (M*n)*v - ((r - t)*n)
	Matrix3_TransformVector( e->axis, fogPlane->normal, fogNormal );
	VectorScale( fogNormal, scale, fogNormal );
	*fogDist = ( fogPlane->dist - DotProduct( viewtofog, fogPlane->normal ) );

	Matrix3_TransformVector( e->axis, m_globalState.cameraAxis, vpnNormal );
	VectorScale( vpnNormal, scale, vpnNormal );
	*vpnDist = ( ( m_globalState.cameraOrigin[0] - viewtofog[0] ) * m_globalState.cameraAxis[AXIS_FORWARD + 0] +
				 ( m_globalState.cameraOrigin[1] - viewtofog[1] ) * m_globalState.cameraAxis[AXIS_FORWARD + 1] +
				 ( m_globalState.cameraOrigin[2] - viewtofog[2] ) * m_globalState.cameraAxis[AXIS_FORWARD + 2] ) +
			   fogShader->fog_clearDist;

	return dist;
}

void SimulatedBackendState::buildTCCelshadeMatrix( mat4_t matrix ) const {
	const entity_t *e = m_materialState.currentEntity;
	if( e->model && !( m_globalState.renderFlags & RF_SHADOWMAPVIEW ) ) {
		vec3_t dir;
		R_LightForOrigin( e->lightingOrigin, dir, nullptr, nullptr, e->model->radius * e->scale, m_globalState.noWorldLight );

		mat4_t m;
		Matrix4_Identity( m );

		// rotate direction
		Matrix3_TransformVector( e->axis, dir, &m[0] );
		VectorNormalize( &m[0] );

		MakeNormalVectors( &m[0], &m[4], &m[8] );
		Matrix4_Transpose( m, matrix );
	}
}

void SimulatedBackendState::buildTCModMatrix( const shaderpass_t *pass, mat4_t result ) const {
	const double materialTime = m_materialState.currentShaderTime;
	for( unsigned tcmodNum = 0; tcmodNum < pass->numtcmods; ++tcmodNum ) {
		const tcmod_t *const tcmod = pass->tcmods + tcmodNum;
		switch( tcmod->type ) {
			case TC_MOD_ROTATE: {
				const float arg  = tcmod->args[0] * materialTime;
				const float sint = RB_FastSin( arg );
				const float cost = RB_FastSin( arg + 0.25f );
				mat4_t m1, m2;
				m2[0] =  cost, m2[1] = sint, m2[12] =  0.5f * ( sint - cost + 1 );
				m2[4] = -sint, m2[5] = cost, m2[13] = -0.5f * ( sint + cost - 1 );
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
			} break;
			case TC_MOD_SCALE: {
				Matrix4_Scale2D( result, tcmod->args[0], tcmod->args[1] );
			} break;
			case TC_MOD_TURB: {
				const float t1 = 0.25f;
				const float t2 = tcmod->args[2] + materialTime * tcmod->args[3];
				Matrix4_Scale2D( result,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 ) + tcmod->args[0] ) * t1,
								 1 + ( tcmod->args[1] * RB_FastSin( t2 + 0.25 ) + tcmod->args[0] ) * t1 );
			} break;
			case TC_MOD_STRETCH: {
				float t2 = tcmod->args[3] + materialTime * tcmod->args[4];
				float t1 = calcFuncValue( (unsigned)tcmod->args[0], t2 ) * tcmod->args[2] + tcmod->args[1];
				t1 = t1 ? 1.0f / t1 : 1.0f;
				t2 = 0.5f - 0.5f * t1;
				Matrix4_Stretch2D( result, t1, t2 );
			} break;
			case TC_MOD_SCROLL: {
				float t1 = tcmod->args[0] * materialTime;
				float t2 = tcmod->args[1] * materialTime;
				if( pass->program_type != GLSL_PROGRAM_TYPE_DISTORTION ) { // HACK HACK HACK
					t1 = t1 - std::floor( t1 );
					t2 = t2 - std::floor( t2 );
				}
				Matrix4_Translate2D( result, t1, t2 );
			} break;
			case TC_MOD_TRANSFORM: {
				mat4_t m1, m2;
				m2[0] = tcmod->args[0], m2[1] = tcmod->args[2], m2[12] = tcmod->args[4],
				m2[5] = tcmod->args[1], m2[4] = tcmod->args[3], m2[13] = tcmod->args[5];
				Matrix4_Copy2D( result, m1 );
				Matrix4_Multiply2D( m2, m1, result );
			} break;
			default:
				break;
		}
	}
}

[[nodiscard]]
static auto evaluateFunc( const shaderfunc_t *func, double materialTime ) -> float {
	float value;
	if( func->type == SHADER_FUNC_NOISE ) {
		// TODO: Not sure if this is suitable. There's no sufficient data to test it.
		// Vanilla material scripts do not use the noise function. Looks like we shouldn't care.
		const auto arg = (float)( ( materialTime + func->args[2] ) * func->args[3] );
		value = calcSimplexNoise2D( 0.0f, arg );
	} else {
		const auto arg = (float)( func->args[2] + materialTime * func->args[3] );
		value = calcFuncValue( func->type, arg );
	}
	return value * func->args[1] + func->args[0];
}

auto SimulatedBackendState::getPassRgb( const shaderpass_t *pass, int *rgb ) const -> float {
	float a = 0.0f;
	double temp = 0.0f;
	vec3_t v { 0.0f, 0.0f, 0.0f };
	float colorMod = 1.0f;

	VectorSet( rgb, 255, 255, 255 );

	const shaderfunc_t *rgbgenfunc = &pass->rgbgen.func;
	switch( pass->rgbgen.type ) {
		case RGB_GEN_IDENTITY:
			break;
		case RGB_GEN_CONST:
			rgb[0] = ( int )( pass->rgbgen.args[0] * 255.0f );
			rgb[1] = ( int )( pass->rgbgen.args[1] * 255.0f );
			rgb[2] = ( int )( pass->rgbgen.args[2] * 255.0f );
			break;
		case RGB_GEN_ENTITYWAVE:
		case RGB_GEN_WAVE:
		case RGB_GEN_CUSTOMWAVE:
			if( rgbgenfunc->type == SHADER_FUNC_NONE ) {
				temp = 1;
			} else if( rgbgenfunc->type == SHADER_FUNC_RAMP ) {
				break;
			} else if( rgbgenfunc->args[1] == 0 ) {
				temp = rgbgenfunc->args[0];
			} else {
				temp = evaluateFunc( rgbgenfunc, m_materialState.currentShaderTime );
			}

			if( pass->rgbgen.type == RGB_GEN_ENTITYWAVE ) {
				VectorSet( v,
						   m_materialState.entityColor[0] * ( 1.0 / 255.0 ),
						   m_materialState.entityColor[1] * ( 1.0 / 255.0 ),
						   m_materialState.entityColor[2] * ( 1.0 / 255.0 ) );
			} else if( pass->rgbgen.type == RGB_GEN_CUSTOMWAVE ) {
				int c = R_GetCustomColor( (int)pass->rgbgen.args[0] );
				VectorSet( v,
						   COLOR_R( c ) * ( 1.0 / 255.0 ),
						   COLOR_G( c ) * ( 1.0 / 255.0 ),
						   COLOR_B( c ) * ( 1.0 / 255.0 ) );
			} else {
				VectorCopy( pass->rgbgen.args, v );
			}

			a = v[0]; rgb[0] = ( int )( a * 255.0f );
			a = v[1]; rgb[1] = ( int )( a * 255.0f );
			a = v[2]; rgb[2] = ( int )( a * 255.0f );
			colorMod = (float)temp;
			break;
		case RGB_GEN_OUTLINE:
			rgb[0] = m_materialState.entityOutlineColor[0];
			rgb[1] = m_materialState.entityOutlineColor[1];
			rgb[2] = m_materialState.entityOutlineColor[2];
			break;
		case RGB_GEN_ONE_MINUS_ENTITY:
			rgb[0] = 255 - m_materialState.entityColor[0];
			rgb[1] = 255 - m_materialState.entityColor[1];
			rgb[2] = 255 - m_materialState.entityColor[2];
			break;
		case RGB_GEN_FOG:
			rgb[0] = m_materialState.texFog->shader->fog_color[0];
			rgb[1] = m_materialState.texFog->shader->fog_color[1];
			rgb[2] = m_materialState.texFog->shader->fog_color[2];
			break;
		case RGB_GEN_ENVIRONMENT:
			rgb[0] = mapConfig.environmentColor[0];
			rgb[1] = mapConfig.environmentColor[1];
			rgb[2] = mapConfig.environmentColor[2];
			break;
		default:
			break;
	}

	return colorMod;
}

auto SimulatedBackendState::getPassAlpha( const shaderpass_t *pass ) const -> int {
	int result = 255;
	float a = 0.0f;

	const shaderfunc_t *alphagenfunc = &pass->alphagen.func;
	switch( pass->alphagen.type ) {
		case ALPHA_GEN_IDENTITY:
			break;
		case ALPHA_GEN_CONST:
			result = ( int )( pass->alphagen.args[0] * 255.0f );
			break;
		case ALPHA_GEN_WAVE:
			if( !alphagenfunc || alphagenfunc->type == SHADER_FUNC_NONE ) {
				a = 1;
			} else if( alphagenfunc->type == SHADER_FUNC_RAMP ) {
				break;
			} else {
				a = evaluateFunc( alphagenfunc, m_materialState.currentShaderTime );
			}

			result = ( int )( a * 255.0f );
			break;
		case ALPHA_GEN_ENTITY:
			result = m_materialState.entityColor[3];
			break;
		case ALPHA_GEN_OUTLINE:
			result = m_materialState.entityOutlineColor[3];
		default:
			break;
	}

	return result;
}

auto SimulatedBackendState::getPassColor( const shaderpass_t *pass, byte_vec4_t rgba_ ) const -> float {
	int rgba[4];

	const float colorMod = getPassRgb( pass, &rgba[0] );
	rgba[3] = getPassAlpha( pass );

	for( int i = 0; i < 4; i++ ) {
		rgba_[i] = wsw::clamp( rgba[i], 0, 255 );
	}

	return colorMod;
}

auto SimulatedBackendState::getPassTexture( const shaderpass_t *pass ) -> Texture * {
	if( pass->anim_numframes ) {
		Texture *res;
		if( pass->anim_fps > 0.0f ) {
			res = pass->images[(int)( pass->anim_fps * m_materialState.currentShaderTime ) % pass->anim_numframes];
		} else {
			assert( m_materialState.currentShaderFrac >= 0.0f && m_materialState.currentShaderFrac <= 1.0f );
			assert( pass->timelineFracs[0] == 0.0f );
			res = pass->images[0];
			for( unsigned i = 1; i < pass->anim_numframes; ++i ) {
				assert( pass->timelineFracs[i] >= 0.0f && pass->timelineFracs[i] < 1.0f );
				if( pass->timelineFracs[i] < m_materialState.currentShaderFrac ) {
					res = pass->images[i];
				} else {
					break;
				}
			}
		}
		assert( res );
		return res;
	}

	auto *const textureCache = TextureCache::instance();
	if( pass->flags & SHADERPASS_PORTALMAP ) {
		if( const auto *surface = m_materialState.currentPortalSurface ) {
			if( surface->texures[0] ) {
				return surface->texures[0];
			} else {
				return textureCache->blackTexture();
			}
		} else {
			return textureCache->blackTexture();
		}
	}

	Texture *const tex = pass->images[0];
	return tex ? tex : textureCache->noTexture();
}

auto SimulatedBackendState::getProgramFeaturesForRgbaGen( const colorgen_t *rgbgen, const colorgen_t *alphagen ) const -> uint64_t {
	uint64_t programFeatures = 0;
	int identity             = 0;

	switch( rgbgen->type ) {
		case RGB_GEN_VERTEX:
		case RGB_GEN_EXACT_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_VERTEX;
			break;
		case RGB_GEN_ONE_MINUS_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_ONE_MINUS_VERTEX;
			break;
		case RGB_GEN_WAVE:
		case RGB_GEN_CUSTOMWAVE:
		case RGB_GEN_ENTITYWAVE:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_CONST;
			if( rgbgen->func.type == SHADER_FUNC_RAMP ) {
				programFeatures |= GLSL_SHADER_COMMON_RGB_DISTANCERAMP;
			}
			break;
		case RGB_GEN_IDENTITY:
			identity++;
		default:
			programFeatures |= GLSL_SHADER_COMMON_RGB_GEN_CONST;
			break;
	}

	switch( alphagen->type ) {
		case ALPHA_GEN_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_VERTEX;
			break;
		case ALPHA_GEN_ONE_MINUS_VERTEX:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_ONE_MINUS_VERTEX;
			break;
		case ALPHA_GEN_WAVE:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_CONST;
			if( alphagen->func.type == SHADER_FUNC_RAMP ) {
				programFeatures |= GLSL_SHADER_COMMON_ALPHA_DISTANCERAMP;
			}
			break;
		case ALPHA_GEN_IDENTITY:
			identity++;
		default:
			programFeatures |= GLSL_SHADER_COMMON_ALPHA_GEN_CONST;
			break;
	}

	if( identity == 2 && !m_materialState.alphaHack ) {
		return 0;
	}

	return programFeatures;
}

auto SimulatedBackendState::getProgramFeaturesForBoneTransforms() const -> uint64_t {
	// check whether the current model is actually sketetal
	if( m_materialState.currentModelType != mod_skeletal ) {
		return 0;
	}
	// base pose sketetal models aren't animated and rendered as-is
	if( !m_drawState.bonesData.numBones ) {
		return 0;
	}
	return m_drawState.bonesData.maxWeights * GLSL_SHADER_COMMON_BONE_TRANSFORMS1;
}

auto SimulatedBackendState::getProgramFeaturesForDlightBits( unsigned dlightBits ) const -> uint64_t {
	assert( dlightBits );

	int numDlights     = Q_bitcount( dlightBits );
	const int varValue = v_lighting_maxGlslDlights.get();
	// TODO: This should be some kind of OptIntVar
	if( varValue && numDlights > varValue ) {
		numDlights = varValue;
	}

	if( numDlights <= 4 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_4;
	}
	if( numDlights <= 8 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_8;
	}
	if( numDlights <= 12 ) {
		return GLSL_SHADER_COMMON_DLIGHTS_12;
	}
	return GLSL_SHADER_COMMON_DLIGHTS_16;
}

auto SimulatedBackendState::getProgramFeaturesForAutosprite() const -> uint64_t {
	uint64_t programFeatures = 0;
	if( ( m_drawState.currentVAttribs & VATTRIB_AUTOSPRITE2_BIT ) == VATTRIB_AUTOSPRITE2_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE2;
	} else if( ( m_drawState.currentVAttribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		programFeatures |= GLSL_SHADER_COMMON_AUTOSPRITE;
	}
	return programFeatures;
}

auto SimulatedBackendState::getProgramFeaturesForInstancedArrays() const -> uint64_t {
	uint64_t programFeatures = 0;
	if( ( m_drawState.currentVAttribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_ATTRIB_TRANSFORMS;
	} else if( /*rb.drawMeshVertSpan.numInstances*/ false ) {
		programFeatures |= GLSL_SHADER_COMMON_INSTANCED_TRANSFORMS;
	}
	return programFeatures;
}

auto SimulatedBackendState::getProgramFeaturesForFog( const shaderpass_t *pass, const mfog_t *fog ) const -> uint64_t {
	uint64_t programFeatures = 0;
	if( fog ) {
		programFeatures |= GLSL_SHADER_COMMON_FOG;
		if( fog == m_materialState.colorFog ) {
			programFeatures |= GLSL_SHADER_COMMON_FOG_RGB;
		}
	}
	return programFeatures;
}

auto SimulatedBackendState::getProgramFeaturesForAlphaTest( const shaderpass_t *pass ) const -> uint64_t {
	switch( pass->flags & SHADERPASS_ALPHAFUNC ) {
		case SHADERPASS_AFUNC_GT0:
			return GLSL_SHADER_COMMON_AFUNC_GT0;
		case SHADERPASS_AFUNC_LT128:
			return GLSL_SHADER_COMMON_AFUNC_LT128;
		case SHADERPASS_AFUNC_GE128:
			return GLSL_SHADER_COMMON_AFUNC_GE128;
	}
	return 0;
}

auto SimulatedBackendState::getProgramFeaturesForTCMod( const shaderpass_t *pass ) const -> uint64_t {
	if( pass->numtcmods ) {
		return GLSL_SHADER_COMMON_TC_MOD;
	}
	return 0;
}

auto SimulatedBackendState::getProgramFeaturesForTCGen( int tcgen, float *tcgenVec,
														mat4_t texMatrix, mat4_t genVectors ) const -> uint64_t {
	uint64_t programFeatures = 0;

	Matrix4_Identity( texMatrix );

	switch( tcgen ) {
		case TC_GEN_ENVIRONMENT:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_ENV;
			break;
		case TC_GEN_VECTOR:
			Matrix4_Identity( genVectors );
			Vector4Copy( &tcgenVec[0], &genVectors[0] );
			Vector4Copy( &tcgenVec[4], &genVectors[4] );
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_VECTOR;
			break;
		case TC_GEN_PROJECTION:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_PROJECTION;
			break;
		case TC_GEN_REFLECTION_CELSHADE:
			buildTCCelshadeMatrix( texMatrix );
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_CELSHADE;
			break;
		case TC_GEN_REFLECTION:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_REFLECTION;
			break;
		case TC_GEN_SURROUND:
			programFeatures |= GLSL_SHADER_Q3_TC_GEN_SURROUND;
			break;
		default:
			break;
	}

	return programFeatures;
}

auto SimulatedBackendState::getProgramFeaturesForSrgb( const shaderpass_t *pass ) const -> uint64_t {
	// don't do srgb<->linear conversions at all, used for blitting framebuffers
	if( pass->flags & SHADERPASS_NOSRGB ) {
		return 0;
	}

	uint64_t programFeatures = 0;
	if( glConfig.sSRGB ) {
		programFeatures |= GLSL_SHADER_COMMON_SRGB2LINEAR;

		// ok, so we're getting sRGB linear input while rendering to
		// default framebuffer, so we need to go back from linear to sRGB
		programFeatures |= GLSL_SHADER_COMMON_LINEAR2SRB;
	}

	return programFeatures;
}

void SimulatedBackendState::updateCommonUniforms( const shaderpass_t *pass, mat4_t texMatrix ) {
	const entity_t *e = m_materialState.currentEntity;

	// the logic here should match R_TransformForEntity
	vec3_t entDist, entOrigin;
	if( e->rtype != RT_MODEL ) {
		VectorClear( entOrigin );
		VectorCopy( m_globalState.cameraOrigin, entDist );
	} else {
		VectorCopy( e->origin, entOrigin );
		vec3_t tmp;
		VectorSubtract( m_globalState.cameraOrigin, e->origin, tmp );
		Matrix3_TransformVector( e->axis, tmp, entDist );
	}

	// calculate constant color
	byte_vec4_t constColor;
	const float colorMod = getPassColor( pass, constColor );

	// apply modifications to texture coordinates
	if( pass->numtcmods ) {
		buildTCModMatrix( pass, texMatrix );
	}

	const float mirrorSide = ( m_globalState.renderFlags & RF_MIRRORVIEW ) ? -1 : +1;
	RP_UpdateViewUniforms( this, m_globalState.modelviewMatrix,
						   m_globalState.modelviewProjectionMatrix,
						   m_globalState.cameraOrigin, m_globalState.cameraAxis, mirrorSide,
						   m_rhiState.getViewport(), m_globalState.zNear, m_globalState.zFar );

	vec2_t blendMix { 0, 0 };
	if( m_rhiState.isAlphaBlendingEnabled() ) {
		blendMix[1] = 1;
		if( m_materialState.alphaHack ) {
			constColor[3] *= m_materialState.hackedAlpha;
		}
	} else {
		blendMix[0] = 1;
		if( m_materialState.alphaHack ) {
			VectorScale( constColor, m_materialState.hackedAlpha, constColor );
		}
	}

	RP_UpdateShaderUniforms( this, m_materialState.currentShaderTime,
							 entOrigin, entDist, m_materialState.entityColor,
							 constColor,
							 pass->rgbgen.func.type != SHADER_FUNC_NONE ? pass->rgbgen.func.args : pass->rgbgen.args,
							 pass->alphagen.func.type != SHADER_FUNC_NONE ? pass->alphagen.func.args : pass->alphagen.args,
							 texMatrix, colorMod );

	RP_UpdateDeformBuiltinUniforms( this, m_materialState.currentShaderTime, m_globalState.cameraOrigin,
									m_globalState.cameraAxis, entOrigin, mirrorSide );

	RP_UpdateBlendMixUniforms( this, blendMix );

	RP_UpdateSoftParticlesUniforms( this, v_softParticles_scale.get() );
}

void SimulatedBackendState::updateFogUniforms( const mfog_t *fog ) {
	assert( fog );

	cplane_t fogPlane, vpnPlane;
	const float dist = transformFogPlanes( fog, fogPlane.normal, &fogPlane.dist, vpnPlane.normal, &vpnPlane.dist );

	RP_UpdateFogUniforms( this, fog->shader->fog_color, fog->shader->fog_clearDist,
						  fog->shader->fog_dist, &fogPlane, &vpnPlane, dist );
}

void SimulatedBackendState::renderMeshUsingQ3AProgram( const FrontendToBackendShared *fsh,
													   const DrawMeshVertSpan *vertSpan, int primitive,
													   const shaderpass_t *pass, uint64_t programFeatures ) {
	const bool isWorldSurface = m_materialState.currentModelType == mod_brush;
	const unsigned rgbgen     = pass->rgbgen.type;
	const entity_t *e         = m_materialState.currentEntity;

	bool isLightmapped = false;
	const superLightStyle_t *lightStyle = nullptr;
	// lightmapped surface pass
	if( isWorldSurface ) {
		const superLightStyle_t *drawStateLightStyle = m_drawState.superLightStyle;
		if( drawStateLightStyle && drawStateLightStyle->lightmapNum[0] >= 0 ) {
			if( m_materialState.currentShader->flags & SHADER_LIGHTMAP ) {
				if( ( pass->flags & GLSTATE_BLEND_ADD ) != GLSTATE_BLEND_ADD ) {
					if( ( pass->flags & ( GLSTATE_SRCBLEND_SRC_ALPHA ) ) != GLSTATE_SRCBLEND_SRC_ALPHA ) {
						// TODO: Replace multiple || by the bitmask trick
						if( rgbgen == RGB_GEN_IDENTITY || rgbgen == RGB_GEN_CONST || rgbgen == RGB_GEN_WAVE
							|| rgbgen == RGB_GEN_CUSTOMWAVE || rgbgen == RGB_GEN_VERTEX
							|| rgbgen == RGB_GEN_ONE_MINUS_VERTEX || rgbgen == RGB_GEN_EXACT_VERTEX ) {
							lightStyle = drawStateLightStyle;
							isLightmapped = true;
						}
					}
				}
			}
		}
	}

	bool isWorldVertexLight = false;
	// vertex-lit world surface
	if( isWorldSurface ) {
		if ( rgbgen == RGB_GEN_VERTEX || rgbgen == RGB_GEN_EXACT_VERTEX ) {
			if( m_drawState.superLightStyle ) {
				isWorldVertexLight = true;
			}
		}
	}

	const mfog_t *fog = m_materialState.fog;
	// possibly apply the fog inline
	if( fog == m_materialState.texFog ) {
		if( m_drawState.currentShadowBits ) {
			fog = nullptr;
		} else if( m_materialState.currentShader->numpasses == 1 || ( isLightmapped && m_materialState.currentShader->numpasses == 2 ) ) {
			// TODO: Modifying the material state!
			m_materialState.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}

	programFeatures |= getProgramFeaturesForFog( pass, fog );

	vec3_t lightDir;
	vec4_t lightAmbient, lightDiffuse;
	// diffuse lighting for entities
	if( !isWorldSurface && rgbgen == RGB_GEN_LIGHTING_DIFFUSE && !( e->flags & RF_FULLBRIGHT ) ) {
		vec3_t temp = { 0.1f, 0.2f, 0.7f };
		float radius = 1;

		if( e->number != kWorldEntNumber && e->model != nullptr ) {
			radius = e->model->radius;
		}

		// get weighted incoming direction of world and dynamic lights
		R_LightForOrigin( e->lightingOrigin, temp, lightAmbient, lightDiffuse, radius * e->scale, m_globalState.noWorldLight );

		if( e->flags & RF_MINLIGHT ) {
			if( lightAmbient[0] <= 0.1f || lightAmbient[1] <= 0.1f || lightAmbient[2] <= 0.1f ) {
				VectorSet( lightAmbient, 0.1f, 0.1f, 0.1f );
			}
		}

		// rotate direction
		Matrix3_TransformVector( e->axis, temp, lightDir );
	} else {
		VectorSet( lightDir, 0, 0, 0 );
		Vector4Set( lightAmbient, 1, 1, 1, 1 );
		Vector4Set( lightDiffuse, 1, 1, 1, 1 );
	}

	const Texture *texture = getPassTexture( pass );
	if( isLightmapped || isWorldVertexLight ) {
		// add dynamic lights
		if( m_drawState.currentDlightBits ) {
			programFeatures |= getProgramFeaturesForDlightBits( m_drawState.currentDlightBits );
		}
		if( shouldApplyDrawflatInCurrentState() ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT;
		}
		if( m_globalState.renderFlags & RF_LIGHTMAP ) {
			texture = TextureCache::instance()->whiteTexture();
		}
	}

	if( texture->flags & IT_ALPHAMASK ) {
		programFeatures |= GLSL_SHADER_Q3_ALPHA_MASK;
	}

	m_rhiState.bindTexture( 0, texture );

	programFeatures |= getProgramFeaturesForRgbaGen( &pass->rgbgen, &pass->alphagen );

	mat4_t texMatrix, genVectors;
	programFeatures |= getProgramFeaturesForTCGen( pass->tcgen, pass->tcgenVec, texMatrix, genVectors );

	// set shaderpass state (blending, depthwrite, etc)
	unsigned passStateFlags = pass->flags;

	// possibly force depthwrite and give up blending when doing a lightmapped pass
	if( ( isLightmapped || isWorldVertexLight ) && !m_drawState.doneDepthPass ) {
		if( !( passStateFlags & GLSTATE_DEPTHWRITE ) && ( m_materialState.currentShader->flags & SHADER_DEPTHWRITE ) ) {
			if( !( pass->flags & SHADERPASS_ALPHAFUNC ) ) {
				passStateFlags &= ~GLSTATE_BLEND_MASK;
			}
			passStateFlags |= GLSTATE_DEPTHWRITE;
		}
	}

	setPassStateFlags( passStateFlags );

	if( isLightmapped ) {
		int i = 0;
		// bind lightmap textures and set program's features for lightstyles
		while( i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255 ) {
			m_rhiState.bindTexture( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] );
			++i;
		}
		programFeatures |= ( i * GLSL_SHADER_Q3_LIGHTSTYLE0 );
		if( mapConfig.lightmapArrays ) {
			programFeatures |= GLSL_SHADER_Q3_LIGHTMAP_ARRAYS;
		}
	}

	setupProgram( GLSL_PROGRAM_TYPE_Q3A_SHADER, programFeatures );

	updateCommonUniforms( pass, texMatrix );

	RP_UpdateTexGenUniforms( this, texMatrix, genVectors );

	if( isWorldSurface || rgbgen == RGB_GEN_LIGHTING_DIFFUSE ) {
		RP_UpdateDiffuseLightUniforms( this, lightDir, lightAmbient, lightDiffuse );
	}

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		updateFogUniforms( fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( this, m_drawState.bonesData.numBones, m_drawState.bonesData.dualQuats );
	}

	// dynamic lights
	if( isLightmapped || isWorldVertexLight ) {
		RP_UpdateDynamicLightsUniforms( this, fsh, lightStyle, e->origin, e->axis, m_drawState.currentDlightBits );
	}

	// r_drawflat
	if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
		if( m_globalState.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
			RP_UpdateDrawFlatUniforms( this, colorWhite, colorWhite );
		} else {
			RP_UpdateDrawFlatUniforms( this, rsh.wallColor, rsh.floorColor );
		}
	}

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingMaterialProgram( const FrontendToBackendShared *fsh,
															const DrawMeshVertSpan *vertSpan, int primitive,
															const shaderpass_t *pass, uint64_t programFeatures ) {
	if( m_materialState.currentModelType == mod_brush && !mapConfig.deluxeMappingEnabled ) {
		renderMeshUsingQ3AProgram( fsh, vertSpan, primitive, pass, programFeatures );
		return;
	}

	TextureCache *const textureCache = TextureCache::instance();

	const Texture *const baseTexture           = getPassTexture( pass );
	const Texture *normalMapTexture            = pass->images[1] ? pass->images[1] : textureCache->blankNormalmap();
	const Texture *const glossMapTexture       = pass->images[2];
	const Texture *decalMapTexture             = pass->images[3];
	const Texture *const entityDecalMapTexture = pass->images[4];

	// use blank image if the normalmap is too tiny due to high picmip value
	if( !normalMapTexture || ( normalMapTexture->width < 2 || normalMapTexture->height < 2 ) ) {
		normalMapTexture = textureCache->blankNormalmap();
	}

	float offsetmappingScale = 0.0f;
	if( normalMapTexture->samples == 4 ) {
		offsetmappingScale = v_offsetMapping_scale.get() * m_materialState.currentShader->offsetmappingScale;
	}

	float glossIntensity = m_materialState.currentShader->glossIntensity;
	if( glossIntensity <= 0.0f ) {
		glossIntensity = v_lighting_glossIntensity.get();
	}

	float glossExponent = m_materialState.currentShader->glossExponent;
	if( glossExponent <= 0.0f ) {
		glossExponent = v_lighting_glossExponent.get();
	}

	const bool applyDecal = decalMapTexture != nullptr;

	const mfog_t *fog = m_materialState.fog;
	// possibly apply the "texture" fog inline
	if( fog == m_materialState.texFog ) {
		if( ( m_materialState.currentShader->numpasses == 1 ) && !m_drawState.currentShadowBits ) {
			// TODO: Modifying the material state!
			m_materialState.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}

	programFeatures |= getProgramFeaturesForFog( pass, fog );

	if( m_materialState.currentModelType == mod_brush ) {
		// brush models
		if( !( v_offsetMapping.get() & 1 ) ) {
			offsetmappingScale = 0;
		}
		if( m_globalState.renderFlags & RF_LIGHTMAP ) {
			programFeatures |= GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
		if( shouldApplyDrawflatInCurrentState() ) {
			programFeatures |= GLSL_SHADER_COMMON_DRAWFLAT | GLSL_SHADER_MATERIAL_BASETEX_ALPHA_ONLY;
		}
	} else if( m_materialState.currentModelType == mod_bad ) {
		// polys
		if( !( v_offsetMapping.get() & 2 ) ) {
			offsetmappingScale = 0;
		}
	} else {
		// regular models
		if( !( v_offsetMapping.get() & 4 ) ) {
			offsetmappingScale = 0;
		}
	#ifdef CELSHADEDMATERIAL
		programFeatures |= GLSL_SHADER_MATERIAL_CELSHADING;
	#endif
	#ifdef HALFLAMBERTLIGHTING
		programFeatures |= GLSL_SHADER_MATERIAL_HALFLAMBERT;
	#endif
	}

	// add dynamic lights
	if( m_drawState.currentDlightBits ) {
		programFeatures |= getProgramFeaturesForDlightBits( m_drawState.currentDlightBits );
	}

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	m_rhiState.bindTexture( 0, baseTexture );

	programFeatures |= getProgramFeaturesForRgbaGen( &pass->rgbgen, &pass->alphagen );

	setPassStateFlags( pass->flags );

	// we only send S-vectors to GPU and recalc T-vectors as cross product
	// in vertex shader
	m_rhiState.bindTexture( 1, normalMapTexture );

	if( glossMapTexture && glossIntensity > 0.0f ) {
		programFeatures |= GLSL_SHADER_MATERIAL_SPECULAR;
		m_rhiState.bindTexture( 2, glossMapTexture );
	}

	if( applyDecal ) {
		programFeatures |= GLSL_SHADER_MATERIAL_DECAL;

		if( m_globalState.renderFlags & RF_LIGHTMAP ) {
			decalMapTexture = textureCache->blackTexture();
			programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
		} else {
			// if no alpha, use additive blending
			if( decalMapTexture->samples & 1 ) {
				programFeatures |= GLSL_SHADER_MATERIAL_DECAL_ADD;
			}
		}

		m_rhiState.bindTexture( 3, decalMapTexture );
	}

	if( entityDecalMapTexture ) {
		programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL;

		// if no alpha, use additive blending
		if( entityDecalMapTexture->samples & 1 ) {
			programFeatures |= GLSL_SHADER_MATERIAL_ENTITY_DECAL_ADD;
		}

		m_rhiState.bindTexture( 4, entityDecalMapTexture );
	}

	if( offsetmappingScale > 0 ) {
		programFeatures |= v_offsetMapping_reliefMapping.get() ?
						   GLSL_SHADER_MATERIAL_RELIEFMAPPING : GLSL_SHADER_MATERIAL_OFFSETMAPPING;
	}

	vec3_t lightDir { 0.0f, 0.0f, 0.0f };
	vec4_t ambient { 0.0f, 0.0f, 0.0f, 0.0f };
	vec4_t diffuse { 0.0f, 0.0f, 0.0f, 0.0f };
	const superLightStyle_t *lightStyle = nullptr;
	if( m_materialState.currentModelType == mod_brush ) {
		// world surface
		if( m_drawState.superLightStyle && m_drawState.superLightStyle->lightmapNum[0] >= 0 ) {
			lightStyle = m_drawState.superLightStyle;

			int i = 0;
			// bind lightmap textures and set program's features for lightstyles
			while( i < MAX_LIGHTMAPS && lightStyle->lightmapStyles[i] != 255 ) {
				m_rhiState.bindTexture( i + 4, rsh.worldBrushModel->lightmapImages[lightStyle->lightmapNum[i]] );
				++i;
			}

			programFeatures |= ( i * GLSL_SHADER_MATERIAL_LIGHTSTYLE0 );

			if( mapConfig.lightmapArrays ) {
				programFeatures |= GLSL_SHADER_MATERIAL_LIGHTMAP_ARRAYS;
			}

			if( i == 1 ) {
				vec_t *rgb = lightStyles[lightStyle->lightmapStyles[0]].rgb;

				// GLSL_SHADER_MATERIAL_FB_LIGHTMAP indicates that there's no need to renormalize
				// the lighting vector for specular (saves 3 adds, 3 muls and 1 normalize per pixel)
				if( rgb[0] == 1 && rgb[1] == 1 && rgb[2] == 1 ) {
					programFeatures |= GLSL_SHADER_MATERIAL_FB_LIGHTMAP;
				}
			}

			if( !VectorCompare( mapConfig.ambient, vec3_origin ) ) {
				VectorCopy( mapConfig.ambient, ambient );
				programFeatures |= GLSL_SHADER_MATERIAL_AMBIENT_COMPENSATION;
			}
		} else {
			// vertex lighting
			VectorSet( lightDir, 0.1f, 0.2f, 0.7f );
			VectorSet( ambient, m_globalState.minLight, m_globalState.minLight, m_globalState.minLight );
			VectorSet( diffuse, m_globalState.minLight, m_globalState.minLight, m_globalState.minLight );

			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT | GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_MIX;
		}
	} else {
		programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT;

		if( m_materialState.currentModelType == mod_bad ) {
			programFeatures |= GLSL_SHADER_MATERIAL_DIRECTIONAL_LIGHT_FROM_NORMAL;

			VectorSet( lightDir, 0, 0, 0 );
			Vector4Set( ambient, 0, 0, 0, 0 );
			Vector4Set( diffuse, 1, 1, 1, 1 );
		} else {
			const entity_t *e = m_materialState.currentEntity;

			if( e->flags & RF_FULLBRIGHT ) {
				Vector4Set( ambient, 1, 1, 1, 1 );
				Vector4Set( diffuse, 1, 1, 1, 1 );
			} else {
				vec3_t temp;
				if( e->model && e->number != kWorldEntNumber ) {
					// get weighted incoming direction of world and dynamic lights
					R_LightForOrigin( e->lightingOrigin, temp, ambient, diffuse,
									  e->model->radius * e->scale, m_globalState.noWorldLight );
				} else {
					VectorSet( temp, 0.1f, 0.2f, 0.7f );
				}

				if( e->flags & RF_MINLIGHT ) {
					float minLight = m_globalState.minLight;
					float ambientL = VectorLength( ambient );

					if( ambientL < minLight ) {
						if( ambientL < 0.001 ) {
							VectorSet( ambient, 1, 1, 1 );
						}
						VectorNormalize( ambient );
						VectorScale( ambient, minLight, ambient );
					}
				}

				// rotate direction
				Matrix3_TransformVector( e->axis, temp, lightDir );
			}
		}
	}

	setupProgram( GLSL_PROGRAM_TYPE_MATERIAL, programFeatures );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateMaterialUniforms( this, offsetmappingScale, glossIntensity, glossExponent );
	RP_UpdateDiffuseLightUniforms( this, lightDir, ambient, diffuse );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		updateFogUniforms( fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( this, m_drawState.bonesData.numBones, m_drawState.bonesData.dualQuats );
	}

	// dynamic lights
	RP_UpdateDynamicLightsUniforms( this, fsh, lightStyle, m_materialState.currentEntity->origin,
									m_materialState.currentEntity->axis, m_drawState.currentDlightBits );

	// r_drawflat
	if( programFeatures & GLSL_SHADER_COMMON_DRAWFLAT ) {
		if( m_globalState.renderFlags & RF_DRAWBRIGHT ) [[unlikely]] {
			RP_UpdateDrawFlatUniforms( this, colorWhite, colorWhite );
		} else {
			RP_UpdateDrawFlatUniforms( this, rsh.wallColor, rsh.floorColor );
		}
	}

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingDistortionProgram( const FrontendToBackendShared *,
															  const DrawMeshVertSpan *vertSpan, int primitive,
															  const shaderpass_t *pass, uint64_t programFeatures ) {
	const portalSurface_s *const portalSurface = m_materialState.currentPortalSurface;
	// TODO: Is this condition reachable?
	if( !portalSurface ) {
		return;
	}

	TextureCache *const textureCache     = TextureCache::instance();
	Texture *const blankTexture          = textureCache->greyTexture();
	Texture *const blankNormalMapTexture = textureCache->blankNormalmap();

	const Texture *portalTextures[2];
	int width = 1, height = 1;
	for( int i = 0; i < 2; i++ ) {
		portalTextures[i] = portalSurface->texures[i];
		if( !portalTextures[i] ) {
			portalTextures[i] = blankTexture;
		} else {
			width  = portalTextures[i]->width;
			height = portalTextures[i]->height;
		}
	}

	const Texture *const dudvMapTexture   = pass->images[0] ? pass->images[0] : blankNormalMapTexture;
	const Texture *const normalMapTexture = pass->images[1] ? pass->images[1] : blankNormalMapTexture;
	if( dudvMapTexture != blankNormalMapTexture ) {
		programFeatures |= GLSL_SHADER_DISTORTION_DUDV;
	}

	if( portalTextures[0] == blankTexture && portalTextures[1] == blankTexture ) {
		// Let it be actually drawn
		programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
	} else {
		if( portalTextures[0] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFLECTION;
		}
		if( portalTextures[1] != blankTexture ) {
			programFeatures |= GLSL_SHADER_DISTORTION_REFRACTION;
		}
	}

	const bool frontPlane = PlaneDiff( m_globalState.cameraOrigin, &portalSurface->untransformed_plane ) > 0;
	if( frontPlane ) {
		if( pass->alphagen.type != ALPHA_GEN_IDENTITY ) {
			programFeatures |= GLSL_SHADER_DISTORTION_DISTORTION_ALPHA;
		}
	}

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	m_rhiState.bindTexture( 0, dudvMapTexture );

	programFeatures |= getProgramFeaturesForRgbaGen( &pass->rgbgen, &pass->alphagen );
	programFeatures |= getProgramFeaturesForFog( pass, m_materialState.fog );

	setPassStateFlags( pass->flags );

	if( normalMapTexture != blankNormalMapTexture ) {
		// eyeDot
		programFeatures |= GLSL_SHADER_DISTORTION_EYEDOT;
		m_rhiState.bindTexture( 1, normalMapTexture );
	}

	m_rhiState.bindTexture( 2, portalTextures[0] );           // reflection
	m_rhiState.bindTexture( 3, portalTextures[1] );           // refraction

	setupProgram( GLSL_PROGRAM_TYPE_DISTORTION, programFeatures );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateDistortionUniforms( this, frontPlane );
	RP_UpdateTextureUniforms( this, width, height );

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingOutlineProgram( const FrontendToBackendShared *,
														   const DrawMeshVertSpan *vertSpan, int primitive,
														   const shaderpass_t *pass, uint64_t programFeatures ) {
	if( m_materialState.currentModelType == mod_brush ) {
		programFeatures |= GLSL_SHADER_OUTLINE_OUTLINES_CUTOFF;
	}

	programFeatures |= getProgramFeaturesForRgbaGen( &pass->rgbgen, &pass->alphagen );
	programFeatures |= getProgramFeaturesForFog( pass, m_materialState.fog );

	setupProgram( GLSL_PROGRAM_TYPE_OUTLINE, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	const auto faceCull = m_rhiState.getCull();
	m_rhiState.setCull( GL_BACK );

	setPassStateFlags( pass->flags );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateOutlineUniforms( this, m_materialState.currentEntity->outlineHeight * v_outlinesScale.get() );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		updateFogUniforms( m_materialState.fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( this, m_drawState.bonesData.numBones, m_drawState.bonesData.dualQuats );
	}

	drawMeshVerts( vertSpan, primitive );

	m_rhiState.setCull( faceCull );
}

auto SimulatedBackendState::bindCelshadeTexture( int tmu, const Texture *texture, uint64_t feature,
												 bool canAdd, const Texture *replacement ) -> uint64_t {
	const Texture *textureToUse = nullptr;
	uint64_t resultFeatures     = 0;

	// Note: Preserving the orignal logic here
	if( texture ) {
		if( !( m_globalState.renderFlags & RF_SHADOWMAPVIEW ) ) [[likely]] {
			textureToUse = texture;
			resultFeatures |= feature;
			if( canAdd && ( textureToUse->samples & 1 ) ) {
				resultFeatures |= ( feature << 1 );
			}
		} else {
			if( texture->flags & IT_CUBEMAP ) {
				textureToUse = TextureCache::instance()->whiteCubemapTexture();
			} else {
				textureToUse = TextureCache::instance()->whiteTexture();
			}
		}
	}

	if( textureToUse ) {
		m_rhiState.bindTexture( tmu, textureToUse );
	}

	return resultFeatures;
}

void SimulatedBackendState::renderMeshUsingCelshadeProgram( const FrontendToBackendShared *,
															const DrawMeshVertSpan *vertSpan, int primitive,
															const shaderpass_t *pass, uint64_t programFeatures ) {
	TextureCache *const textureCache = TextureCache::instance();

	const Texture *const baseTexture        = pass->images[0];
	const Texture *const shadeTexture       = pass->images[1];
	const Texture *const diffuseTexture     = pass->images[2];
	const Texture *const decalTexture       = pass->images[3];
	const Texture *const entityDecalTexture = pass->images[4];
	const Texture *const stripesTexture     = pass->images[5];
	const Texture *const lightTexture       = pass->images[6];

	m_rhiState.bindTexture( 0, baseTexture );

	const mfog_t *fog = m_materialState.fog;
	// possibly apply the "texture" fog inline
	if( fog == m_materialState.texFog ) {
		if( ( m_materialState.currentShader->numpasses == 1 ) && !m_drawState.currentShadowBits ) {
			// TODO: Modifying the material state
			m_materialState.texFog = nullptr;
		} else {
			fog = nullptr;
		}
	}

	programFeatures |= getProgramFeaturesForFog( pass, fog );
	programFeatures |= getProgramFeaturesForRgbaGen( &pass->rgbgen, &pass->alphagen );

	setPassStateFlags( pass->flags );

	Texture *const whiteTexture = textureCache->whiteTexture();
	assert( whiteTexture && whiteTexture->target == GL_TEXTURE_2D );
	Texture *const whiteCubemapTexture = textureCache->whiteCubemapTexture();
	assert( whiteCubemapTexture && whiteCubemapTexture->target == GL_TEXTURE_CUBE_MAP );

	programFeatures |= bindCelshadeTexture( 1, shadeTexture, 0, false, whiteCubemapTexture );
	programFeatures |= bindCelshadeTexture( 2, diffuseTexture, GLSL_SHADER_CELSHADE_DIFFUSE, false, nullptr );
	programFeatures |= bindCelshadeTexture( 3, decalTexture, GLSL_SHADER_CELSHADE_DECAL, true, nullptr );
	programFeatures |= bindCelshadeTexture( 4, entityDecalTexture, GLSL_SHADER_CELSHADE_ENTITY_DECAL, true, whiteTexture );
	programFeatures |= bindCelshadeTexture( 5, stripesTexture, GLSL_SHADER_CELSHADE_STRIPES, true, nullptr );
	programFeatures |= bindCelshadeTexture( 6, lightTexture, GLSL_SHADER_CELSHADE_CEL_LIGHT, true, nullptr );

	setupProgram( GLSL_PROGRAM_TYPE_CELSHADE, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	mat4_t reflectionMatrix;
	buildTCCelshadeMatrix( reflectionMatrix );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateTexGenUniforms( this, reflectionMatrix, texMatrix );

	if( programFeatures & GLSL_SHADER_COMMON_FOG ) {
		updateFogUniforms( fog );
	}

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( this, m_drawState.bonesData.numBones, m_drawState.bonesData.dualQuats );
	}

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingFogProgram( const FrontendToBackendShared *,
													   const DrawMeshVertSpan *vertSpan, int primitive,
													   const shaderpass_t *pass, uint64_t programFeatures ) {
	const mfog_t *fog = m_materialState.fog;

	programFeatures |= GLSL_SHADER_COMMON_FOG;

	setPassStateFlags( pass->flags );

	setupProgram( GLSL_PROGRAM_TYPE_FOG, programFeatures );

	mat4_t texMatrix = { 0 };
	updateCommonUniforms( pass, texMatrix );
	updateFogUniforms( fog );

	// submit animation data
	if( programFeatures & GLSL_SHADER_COMMON_BONE_TRANSFORMS ) {
		RP_UpdateBonesUniforms( this, m_drawState.bonesData.numBones, m_drawState.bonesData.dualQuats );
	}

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingFxaaProgram( const FrontendToBackendShared *,
														const DrawMeshVertSpan *vertSpan, int primitive,
														const shaderpass_t *pass, uint64_t programFeatures ) {
	setPassStateFlags( pass->flags );

	const Texture *image = pass->images[0];
	m_rhiState.bindTexture( 0, image );

	if( glConfig.ext.gpu_shader5 ) {
		programFeatures |= GLSL_SHADER_FXAA_FXAA3;
	}

	setupProgram( GLSL_PROGRAM_TYPE_FXAA, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateTextureUniforms( this, image->width, image->height );

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingYuvProgram( const FrontendToBackendShared *,
													   const DrawMeshVertSpan *vertSpan, int primitive,
													   const shaderpass_t *pass, uint64_t programFeatures ) {
	setPassStateFlags( pass->flags );

	m_rhiState.bindTexture( 0, pass->images[0] );
	m_rhiState.bindTexture( 1, pass->images[1] );
	m_rhiState.bindTexture( 2, pass->images[2] );

	setupProgram( GLSL_PROGRAM_TYPE_YUV, programFeatures );

	// TODO: Should we set it to identity?
	mat4_t texMatrix = { 0 };
	updateCommonUniforms( pass, texMatrix );

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingColorCorrectionProgram( const FrontendToBackendShared *,
																   const DrawMeshVertSpan *vertSpan, int primitive,
																   const shaderpass_t *pass, uint64_t programFeatures ) {
	programFeatures &= ~GLSL_SHADER_COMMON_SRGB2LINEAR;
	if( pass->images[1] ) { // lut
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_LUT;
	}
	if( pass->images[2] ) { // output bloom
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_OVERBRIGHT;
	}
	if( pass->images[3] ) { // apply bloom
		programFeatures |= GLSL_SHADER_COLOR_CORRECTION_BLOOM;
	}

	if( pass->images[0]->flags & IT_FLOAT ) {
		if( glConfig.sSRGB ) {
			programFeatures |= GLSL_SHADER_COMMON_SRGB2LINEAR;
		}
		if( v_hdr.get() ) {
			programFeatures |= GLSL_SHADER_COLOR_CORRECTION_HDR;
		}
	}

	setPassStateFlags( pass->flags );

	m_rhiState.bindTexture( 0, pass->images[0] );
	if( pass->images[1] ) {
		m_rhiState.bindTexture( 1, pass->images[1] );
	}
	for( int i = 0; i < NUM_BLOOM_LODS; i++ ) {
		if( pass->images[3 + i] ) {
			m_rhiState.bindTexture( 2 + i, pass->images[3 + i] );
		}
	}

	setupProgram( GLSL_PROGRAM_TYPE_COLOR_CORRECTION, programFeatures );

	mat4_t texMatrix;
	Matrix4_Identity( texMatrix );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateColorCorrectionUniforms( this, v_hdrGamma.get(), m_globalState.hdrExposure );

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingKawaseProgram( const FrontendToBackendShared *fsh,
														  const DrawMeshVertSpan *vertSpan, int primitive,
														  const shaderpass_t *pass, uint64_t programFeatures ) {
	setPassStateFlags( pass->flags );

	m_rhiState.bindTexture( 0, pass->images[0] );

	setupProgram( GLSL_PROGRAM_TYPE_KAWASE_BLUR, programFeatures );

	mat4_t texMatrix = { 0 };
	Matrix4_Identity( texMatrix );

	updateCommonUniforms( pass, texMatrix );
	RP_UpdateKawaseUniforms( this, pass->images[0]->width, pass->images[0]->height, pass->anim_numframes );

	drawMeshVerts( vertSpan, primitive );
}

void SimulatedBackendState::renderMeshUsingAppropriateProgram( const FrontendToBackendShared *fsh,
															   const DrawMeshVertSpan *vertSpan, int primitive,
															   const shaderpass_t *pass, unsigned programType ) {
	uint64_t features = 0;

	if( m_materialState.greyscale || ( pass->flags & SHADERPASS_GREYSCALE ) ) {
		features |= GLSL_SHADER_COMMON_GREYSCALE;
	}

	features |= getProgramFeaturesForBoneTransforms();
	features |= getProgramFeaturesForAutosprite();
	features |= getProgramFeaturesForInstancedArrays();
	features |= getProgramFeaturesForAlphaTest( pass );
	features |= getProgramFeaturesForTCMod( pass );
	features |= getProgramFeaturesForSrgb( pass );

	switch( programType ) {
		case GLSL_PROGRAM_TYPE_MATERIAL:
			return renderMeshUsingMaterialProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_DISTORTION:
			return renderMeshUsingDistortionProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_RGB_SHADOW:
			[[fallthrough]];
		case GLSL_PROGRAM_TYPE_SHADOWMAP:
			return;
		case GLSL_PROGRAM_TYPE_OUTLINE:
			return renderMeshUsingOutlineProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_Q3A_SHADER:
			return renderMeshUsingQ3AProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_CELSHADE:
			return renderMeshUsingCelshadeProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_FOG:
			return renderMeshUsingFogProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_FXAA:
			return renderMeshUsingFxaaProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_YUV:
			return renderMeshUsingYuvProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_COLOR_CORRECTION:
			return renderMeshUsingColorCorrectionProgram( fsh, vertSpan, primitive, pass, features );
		case GLSL_PROGRAM_TYPE_KAWASE_BLUR:
			return renderMeshUsingKawaseProgram( fsh, vertSpan, primitive, pass, features );
		default:
			wsw::failWithLogicError( "Unreachable" );
	}
}

void SimulatedBackendState::renderPass( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan,
										int primitive, const shaderpass_t *pass ) {
	// for depth texture we render light's view to, ignore passes that do not write into depth buffer
	if( !( m_globalState.renderFlags & RF_SHADOWMAPVIEW ) || ( pass->flags & GLSTATE_DEPTHWRITE ) ) [[likely]] {
		const unsigned programType = pass->program_type ? pass->program_type : GLSL_PROGRAM_TYPE_Q3A_SHADER;
		renderMeshUsingAppropriateProgram( fsh, vertSpan, primitive, pass, programType );

		if( m_drawState.dirtyUniformState ) {
			m_drawState.donePassesTotal = 0;
			m_drawState.dirtyUniformState = false;
		}

		if( m_rhiState.getState() & GLSTATE_DEPTHWRITE ) {
			m_drawState.doneDepthPass = true;
		}

		m_drawState.donePassesTotal++;
	}
}

void SimulatedBackendState::updateCurrentShaderState() {
	const unsigned shaderFlags = m_materialState.currentShader->flags;

	// Face culling
	if( m_materialState.currentEntity->rtype == RT_SPRITE ) {
		m_rhiState.setCull( 0 );
	} else if( shaderFlags & SHADER_CULL_FRONT ) {
		m_rhiState.setCull( GL_FRONT );
	} else if( shaderFlags & SHADER_CULL_BACK ) {
		m_rhiState.setCull( GL_BACK );
	} else {
		m_rhiState.setCull( 0 );
	}

	unsigned stateFlags = 0;

	if( shaderFlags & SHADER_POLYGONOFFSET ) {
		stateFlags |= GLSTATE_OFFSET_FILL;
	}
	if( shaderFlags & SHADER_STENCILTEST ) {
		stateFlags |= GLSTATE_STENCIL_TEST;
	}

	if( m_materialState.noDepthTest ) {
		stateFlags |= GLSTATE_NO_DEPTH_TEST;
	}

	m_drawState.currentShaderState = ( stateFlags & m_globalState.shaderStateANDmask ) | m_globalState.shaderStateORmask;
}

void SimulatedBackendState::setPassStateFlags( unsigned passStateFlags ) {
	passStateFlags |= m_drawState.currentShaderState;
	if( m_materialState.alphaHack ) {
		if( !( passStateFlags & GLSTATE_BLEND_MASK ) ) {
			// force alpha blending
			passStateFlags = ( passStateFlags & ~GLSTATE_DEPTHWRITE ) | GLSTATE_SRCBLEND_SRC_ALPHA | GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA;
		}
	}
	if( m_materialState.noColorWrite ) {
		passStateFlags |= GLSTATE_NO_COLORWRITE;
	}
	if( m_materialState.depthEqual && ( passStateFlags & GLSTATE_DEPTHWRITE ) ) {
		passStateFlags |= GLSTATE_DEPTHFUNC_EQ;
	}

	m_rhiState.setState( passStateFlags );
}

bool SimulatedBackendState::tryExecutingSinglePassReusingBoundState( const DrawMeshVertSpan *vertSpan, int primitive ) {
	// reuse current GLSL state (same program bound, same uniform values)
	if( !m_drawState.dirtyUniformState && m_drawState.donePassesTotal == 1 && m_programState.boundProgram >= 0 ) {
		drawMeshVerts( vertSpan, primitive );
		return true;
	}
	return false;
}

auto SimulatedBackendState::getCurrWireframeColor() const -> const float * {
	if( v_showTris.get() != 2 ) {
		return colorWhite;
	}
	if( m_materialState.currentModelType == mod_brush ) {
		return colorBlack;
	}
	if( m_materialState.currentModelType != mod_bad ) {
		return colorRed;
	}
	if( m_materialState.currentEntity && m_materialState.currentEntity->number != kWorldEntNumber ) {
		return colorBlue;
	}
	return colorGreen;
}

void SimulatedBackendState::drawWireframeMesh( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive ) {
	if( !tryExecutingSinglePassReusingBoundState( vertSpan, primitive ) ) {
		const shaderpass_t *referencePass;
		if( !m_materialState.currentShader->numpasses ) {
			// happens on fog volumes
			referencePass = &kBuiltinFogPass;
		} else {
			referencePass = &m_materialState.currentShader->passes[0];
		}

		shaderpass_t wireframePass  = *referencePass;

		const float *const wireColor = getCurrWireframeColor();
		wireframePass.rgbgen.type = RGB_GEN_CONST;
		VectorCopy( wireColor, wireframePass.rgbgen.args );
		wireframePass.alphagen.type = ALPHA_GEN_CONST;
		VectorSet( wireframePass.alphagen.args, wireColor[3], wireColor[3], wireColor[3] );

		wireframePass.flags          = 0;
		wireframePass.images[0]      = TextureCache::instance()->whiteTexture();
		wireframePass.anim_fps       = 0;
		wireframePass.anim_numframes = 0;
		wireframePass.program_type   = GLSL_PROGRAM_TYPE_Q3A_SHADER;

		m_drawState.currentShadowBits = 0;
		m_drawState.currentDlightBits = 0;

		// TODO: Modifying the material state
		m_materialState.colorFog    = nullptr;
		m_materialState.texFog      = nullptr;
		m_drawState.superLightStyle = nullptr;

		updateCurrentShaderState();

		renderPass( fsh, vertSpan, primitive, &wireframePass );
	}
}

void SimulatedBackendState::drawShadedMesh( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive ) {
	if( !tryExecutingSinglePassReusingBoundState( vertSpan, primitive ) ) {
		const unsigned sort      = m_materialState.currentShader->sort;
		const unsigned numPasses = m_materialState.currentShader->numpasses;

		bool addGLSLOutline = false;
		// TODO: Show outlines in mirrors
		if( m_materialState.currentEntity->outlineHeight > 0 ) {
			if( !( m_materialState.currentEntity->renderfx & RF_VIEWERMODEL ) ) [[likely]] {
				if( !( m_globalState.renderFlags & ( RF_CLIPPLANE | RF_SHADOWMAPVIEW ) ) ) [[likely]] {
					if( sort == SHADER_SORT_OPAQUE && ( m_materialState.currentShader->flags & SHADER_CULL_FRONT ) ) {
						addGLSLOutline = true;
					}
				}
			}
		}

		updateCurrentShaderState();

		for( unsigned passNum = 0; passNum < numPasses; ++passNum ) {
			const shaderpass_t *pass = m_materialState.currentShader->passes + passNum;
			if( !( pass->flags & SHADERPASS_DETAIL ) || v_detailTextures.get() ) {
				if( !( pass->flags & SHADERPASS_LIGHTMAP ) ) [[likely]] {
					renderPass( fsh, vertSpan, primitive, pass );
				}
			}
		}

		if( m_drawState.currentShadowBits && sort >= SHADER_SORT_OPAQUE && sort <= SHADER_SORT_ALPHATEST ) {
			renderPass( fsh, vertSpan, primitive, &kBuiltinShadowmapPass );
		}

		if( addGLSLOutline ) {
			renderPass( fsh, vertSpan, primitive, &kBuiltinOutlinePass );
		}

		if( m_materialState.texFog && m_materialState.texFog->shader ) {
			shaderpass_t fogPass = kBuiltinFogPass;

			fogPass.images[0] = TextureCache::instance()->whiteTexture();
			if( !numPasses || m_materialState.currentShader->fog_dist != 0.0f ) {
				fogPass.flags &= ~GLSTATE_DEPTHFUNC_EQ;
			} else {
				fogPass.flags |= GLSTATE_DEPTHFUNC_EQ;
			}

			renderPass( fsh, vertSpan, primitive, &fogPass );
		}
	}
}

void SimulatedBackendState::drawMesh( const FrontendToBackendShared *fsh, const MeshBuffer *buffer,
									  const VboSpanLayout *layout, const DrawMeshVertSpan *drawMeshVertSpan, int primitive ) {
	assert( buffer && layout );

	bindMeshBuffer( buffer );

	m_drawState.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	assert( m_materialState.currentShader );

	m_rhiState.enableVertexAttribs( m_drawState.currentVAttribs, layout );

	if( m_globalState.wireframe ) {
		drawWireframeMesh( fsh, drawMeshVertSpan, primitive );
	} else {
		drawShadedMesh( fsh, drawMeshVertSpan, primitive );
	}
}
