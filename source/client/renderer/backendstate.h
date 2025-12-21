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
#ifndef R_BACKEND_LOCAL_H
#define R_BACKEND_LOCAL_H

#include "backendrhistate.h"
#include "local.h"

struct RuntimeBackendState {
	GLuint programId { 0 };
};

class UploadManager;
struct portalSurface_s;
struct mfog_s;
struct FrontendToBackendShared;

class SimulatedBackendState {
public:
	SimulatedBackendState( UploadManager *uploadManager, unsigned uniformUploadCategory,
						   BackendActionTape *actionTape, int width, int height );

	void loadCameraMatrix( const mat4_t m );
	void getObjectMatrix( float *m );
	void loadObjectMatrix( const mat4_t m );
	void loadProjectionMatrix( const mat4_t m );

	void transformForWorld();
	void translateForEntity( const entity_t *e );
	void transformForEntity( const entity_t *e );

	void setTime( int64_t requestTime, int64_t globalTime );

	void setDepthRange( float depthmin, float depthmax );
	void getDepthRange( float *depthmin, float *depthmax );
	void saveDepthRange();
	void restoreDepthRange();

	void flipFrontFace();
	void setScissor( int x, int y, int w, int h );
	void getScissor( int *x, int *y, int *w, int *h );
	void setViewport( int x, int y, int w, int h );
	void clear( int bits, float r, float g, float b, float a );
	void setZClip( float zNear, float zFar );

	void bindShader( const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
					 const shader_s *shader, const mfog_s *fog, const portalSurface_s *portalSurface );
	void setLightstyle( const struct superLightStyle_s *lightStyle );
	void setDlightBits( unsigned dlightBits );
	void setBonesData( int numBones, const dualquat_t *dualQuats, int maxWeights );
	void setRenderFlags( int flags );
	void setLightParams( float minLight, bool noWorldLight, float hdrExposure = 1.0f );
	void setShaderStateMask( unsigned ANDmask, unsigned ORmask );
	void setCamera( const vec3_t cameraOrigin, const mat3_t cameraAxis );
	bool enableWireframe( bool enable );

	void bindMeshBuffer( const MeshBuffer *buffer );
	void bindRenderTarget( RenderTargetComponents *components );

	void bindUniformBlock( unsigned binding, GLuint bufferId, unsigned offset, unsigned size );

	[[nodiscard]]
	auto getUploadManager() -> UploadManager * { return m_uploadManager; }

	[[nodiscard]]
	auto getUniformSliceId() const -> unsigned { return m_uniformSliceId; };

	void drawMesh( const FrontendToBackendShared *fsh, const MeshBuffer *buffer, const VboSpanLayout *layout, const DrawMeshVertSpan *vertSpan, int primitive );
private:
	[[nodiscard]]
	bool shouldApplyDrawflatInCurrentState() const;

	[[nodiscard]]
	auto transformFogPlanes( const mfog_t *fog, vec3_t fogNormal, float *fogDist, vec3_t vpnNormal, float *vpnDist ) const -> float;

	void buildTCModMatrix( const shaderpass_t *pass, mat4_t result ) const;
	void buildTCCelshadeMatrix( mat4_t matrix ) const;

	void updateCommonUniforms( const shaderpass_t *pass, mat4_t texMatrix );
	void updateFogUniforms( const mfog_t *fog );

	[[nodiscard]]
	auto getProgramFeaturesForRgbaGen( const colorgen_t *rgbgen, const colorgen_t *alphagen ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForBoneTransforms() const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForDlightBits( unsigned dlightBits ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForAutosprite() const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForInstancedArrays() const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForFog( const shaderpass_t *pass, const mfog_t *fog ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForAlphaTest( const shaderpass_t *pass ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForTCMod( const shaderpass_t *pass ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForTCGen( int tcgen, float *tcgenVec, mat4_t texMatrix, mat4_t genVectors ) const -> uint64_t;
	[[nodiscard]]
	auto getProgramFeaturesForSrgb( const shaderpass_t *pass ) const -> uint64_t;

	[[nodiscard]]
	auto getPassColor( const shaderpass_t *pass, byte_vec4_t rgba_ ) const -> float;
	[[nodiscard]]
	auto getPassRgb( const shaderpass_t *pass, int *rgb ) const -> float;
	[[nodiscard]]
	auto getPassAlpha( const shaderpass_t *pass ) const -> int;

	[[nodiscard]]
	auto getPassTexture( const shaderpass_t *pass ) -> Texture *;

	[[nodiscard]]
	auto bindCelshadeTexture( int tmu, const Texture *texture, uint64_t feature, bool canAdd, const Texture *replacement ) -> uint64_t;

	void bindExistingProgram( int program );
	void setupProgram( int type, uint64_t features );

	void setPassStateFlags( unsigned passStateFlags );
	void updateCurrentShaderState();
	void updateRequiredVertexAttribs();

	[[nodiscard]]
	// -> &'static
	auto getCurrWireframeColor() const -> const float *;

	void renderMeshUsingQ3AProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
									int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingMaterialProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
										 int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingOutlineProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
										int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingCelshadeProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
										 int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingDistortionProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
										   int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingFogProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
									int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingYuvProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
									int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingFxaaProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
									 int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingColorCorrectionProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
												int primitive, const shaderpass_t *pass, uint64_t programFeatures );
	void renderMeshUsingKawaseProgram( const FrontendToBackendShared *, const DrawMeshVertSpan *vertSpan,
									   int primitive, const shaderpass_t *pass, uint64_t programFeatures );

	void renderMeshUsingAppropriateProgram( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan,
											int primitive, const shaderpass_t *pass, unsigned programType );

	void renderPass( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive, const shaderpass_t *pass );

	void drawMeshVerts( const DrawMeshVertSpan *vertSpan, int primitive );

	void drawWireframeMesh( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive );
	void drawShadedMesh( const FrontendToBackendShared *fsh, const DrawMeshVertSpan *vertSpan, int primitive );

	[[nodiscard]]
	bool tryExecutingSinglePassReusingBoundState( const DrawMeshVertSpan *vertSpan, int primitive );

	struct {
		entity_t nullEnt;
		int64_t time;

		mat4_t cameraMatrix;
		mat4_t objectMatrix;
		mat4_t modelviewMatrix;
		mat4_t projectionMatrix;
		mat4_t modelviewProjectionMatrix;
		float zNear, zFar;

		int renderFlags;

		vec3_t cameraOrigin;
		mat3_t cameraAxis;

		float minLight;
		float hdrExposure;
		bool noWorldLight;

		// TODO: Should it be a tracked state? We don't really perform frequent switching to/from wireframe
		bool wireframe;

		unsigned shaderStateORmask;
		unsigned shaderStateANDmask;
	} m_globalState;

	// Gets modified only by RB_BindShader() call
	struct {
		const shader_t *currentShader;
		double currentShaderTime;
		float currentShaderFrac;

		const entity_t *currentEntity;
		modtype_t currentModelType;
		const portalSurface_t *currentPortalSurface;

		uint8_t entityColor[4];
		uint8_t entityOutlineColor[4];

		const mfog_t *fog, *texFog, *colorFog;

		bool greyscale;
		bool alphaHack;
		bool noDepthTest;
		bool noColorWrite;
		bool depthEqual;
		float hackedAlpha;
	} m_materialState;

	// Gets modified by RB_BindShader() and some consequent calls, including drawing passes
	struct {
		vattribmask_t currentVAttribs;
		unsigned currentDlightBits;
		unsigned currentShadowBits;
		struct {
			unsigned int numBones;
			dualquat_t dualQuats[MAX_GLSL_UNIFORM_BONES];
			unsigned int maxWeights;
		} bonesData;
		const superLightStyle_t *superLightStyle;
		unsigned currentShaderState;
		bool dirtyUniformState;
		bool doneDepthPass;
		int donePassesTotal;
	} m_drawState;

	struct {
		int boundProgram;

		int cachedFastLookupProgram;
		int cachedFastLookupProgramType;
		uint64_t cachedFastLookupProgramFeatures;
	} m_programState;

	unsigned m_uniformSliceId { ~0u };

	SimulatedRhiState m_rhiState;
	UploadManager *const m_uploadManager;
	BackendActionTape *const m_actionTape;
};

#endif // R_BACKEND_LOCAL_H
