/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2013 Victor Luchits

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

#ifndef WSW_63ccf348_3b16_4f9c_9a49_cd5849918618_H
#define WSW_63ccf348_3b16_4f9c_9a49_cd5849918618_H

#include <span>

#include <common/podbufferholder.h>
#include <common/wswfunction.h>
#include <common/tasksystem.h>

struct alignas( 32 )Frustum {
	alignas( 32 ) float planeX[8];
	alignas( 32 ) float planeY[8];
	alignas( 32 ) float planeZ[8];
	alignas( 32 ) float planeD[8];
	alignas( 32 ) uint32_t xBlendMasks[8];
	alignas( 32 ) uint32_t yBlendMasks[8];
	alignas( 32 ) uint32_t zBlendMasks[8];

	void setPlaneComponentsAtIndex( unsigned index, const float *n, float d );

	// Call after setting up 4th+ planes
	void fillComponentTails( unsigned numPlanesSoFar );

	void setupFor4Planes( const float *viewOrigin, const mat3_t viewAxis, float fovX, float fovY );
};

#define SHOW_OCCLUDED( stateForCamera, v1, v2, color ) do { /*addDebugLine( stateForCamera, v1, v2, color );*/ } while( 0 )
//#define SHOW_OCCLUDERS
//#define SHOW_OCCLUDERS_FRUSTA

//#define DEBUG_OCCLUDERS
#ifdef DEBUG_OCCLUDERS
#define SHOW_OCCLUDERS_FRUSTA
#endif

namespace wsw::ref {

class alignas( 32 ) Frontend {
	friend struct BatchedMeshBuilder;

public:
	Frontend();
	~Frontend();

	static void init();
	static void shutdown();

	[[nodiscard]]
	static auto instance() -> Frontend *;

	// TODO: See the remark along with m_taskSystem definition
	[[nodiscard]]
	auto getTaskSystem() -> TaskSystem * { return &m_taskSystem; }

	void beginDrawingScenes();

	[[nodiscard]]
	auto createDrawSceneRequest( const refdef_t &refdef ) -> DrawSceneRequest *;

	[[nodiscard]]
	auto beginProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests ) -> TaskHandle;
	[[nodiscard]]
	auto endProcessingDrawSceneRequests( std::span<DrawSceneRequest *> requests, std::span<const TaskHandle> dependencies ) -> TaskHandle;

	void commitProcessedDrawSceneRequest( DrawSceneRequest *request );

	void endDrawingScenes();

	[[nodiscard]]
	auto getMiniviewRenderTarget() -> RenderTargetComponents *;

	void initVolatileAssets();

	void destroyVolatileAssets();

	void set2DMode( bool enable );
	void set2DScissor( int x, int y, int w, int h );

	void dynLightDirForOrigin( const float *origin, float radius, vec3_t dir, vec3_t diffuseLocal, vec3_t ambientLocal );

private:
	struct SortedOccluder {
		unsigned occluderNum;
		float score;
		[[nodiscard]]
		bool operator<( const SortedOccluder &that ) const { return score > that.score; }
	};

	struct MergedSurfSpan {
		MultiDrawElemSpan mdSpan;
		int firstSurface;
		int lastSurface;
		unsigned subspansOffset;
		unsigned numSubspans;
	};

	struct alignas( 16 ) VisTestedModel {
		vec4_t absMins, absMaxs;
		// TODO: Pass lod number?
		const model_t *selectedLod;
		unsigned indexInEntitiesGroup;
	};

	// Make sure it can be supplied to the generic culling subroutine
	static_assert( offsetof( VisTestedModel, absMins ) + 4 * sizeof( float ) == offsetof( VisTestedModel, absMaxs ) );

	static constexpr unsigned kMaxOccluderFrusta = 64;

	static constexpr unsigned kMaxLightsInScene       = 1024;
	static constexpr unsigned kMaxProgramLightsInView = 32;

	struct StateForCamera;

	struct PrepareBatchedSurfWorkload {
		std::span<const sortedDrawSurf_t> batchSpan;
		Scene *scene;
		StateForCamera *stateForCamera;
		unsigned vertSpanOffset;
	};

	// ST_SPRITE surfaces need special handling as they go through the legacy dynamic mesh path
	struct PrepareSpriteSurfWorkload {
		std::span<const sortedDrawSurf_t> batchSpan;
		StateForCamera *stateForCamera;
		unsigned firstMeshOffset;
	};

	// A "mesh" and the respective data storage
	struct PreparedSpriteMesh {
		mesh_t mesh;
		vec4_t positions[4];
		vec4_t normals[4];
		vec2_t texCoords[4];
		byte_vec4_t colors[4];
		elem_t indices[6];
	};

	struct DebugLine {
		float p1[3];
		float p2[3];
		int color;
	};

	struct alignas( alignof( Frustum ) ) StateForCamera {
		alignas( alignof( Frustum ) )Frustum frustum;
		alignas( alignof( Frustum ) )Frustum occluderFrusta[kMaxOccluderFrusta];
		// TODO: Can we share/cache dops for different camera states
		alignas( 16 ) struct { float mins[8], maxs[8]; } lightBoundingDops[kMaxProgramLightsInView];

		unsigned numVisibleProgramLights { 0 };
		unsigned numAllVisibleLights { 0 };
		uint16_t visibleProgramLightIndices[kMaxProgramLightsInView];
		uint16_t allVisibleLightIndices[kMaxLightsInScene];
		uint16_t visibleCoronaLightIndices[kMaxLightsInScene];

		unsigned renderFlags { 0 };

		unsigned cameraId { ~0u };
		unsigned sceneIndex { ~0u };

		int viewCluster { -1 };
		int viewArea { -1 };

		int scissor[4] { 0, 0, 0, 0 };
		int viewport[4] { 0, 0, 0, 0 };

		vec3_t viewOrigin;
		mat3_t viewAxis;
		float farClip;

		vec3_t lodOrigin;
		vec3_t pvsOrigin;

		mat4_t cameraMatrix;
		mat4_t projectionMatrix;
		mat4_t cameraProjectionMatrix;                  // cameraMatrix * projectionMatrix

		float fovLodScale { 1.0f };
		float viewLodScale { 1.0f };

		unsigned numPortalSurfaces;

		portalSurface_t portalSurfaces[MAX_PORTAL_SURFACES];

		refdef_t refdef;

		wsw::PodVector<ShaderParams> *shaderParamsStorage;
		wsw::PodVector<ShaderParams::Material> *materialParamsStorage;

		ShaderParamsTable shaderParamsTable;

		// TODO: We don't really need a growable vector, preallocate at it start
		wsw::PodVector<sortedDrawSurf_t> *sortList;
		// Same here, we can't use PodBufferHolder yet for wsw::Function<>
		// TODO: Use something less wasteful wrt storage than wsw::Function<>
		wsw::PodVector<wsw::Function<void( FrontendToBackendShared *)>> *drawActionsList;

		wsw::PodVector<PrepareBatchedSurfWorkload> *preparePolysWorkload;
		wsw::PodVector<PrepareBatchedSurfWorkload> *prepareCoronasWorkload;
		wsw::PodVector<PrepareBatchedSurfWorkload> *prepareParticlesWorkload;

		// Results of preparation get stored in this buffer
		wsw::PodVector<VertElemSpan> *batchedSurfVertSpans;

		wsw::PodVector<PrepareSpriteSurfWorkload> *prepareSpritesWorkload;
		wsw::PodVector<PreparedSpriteMesh> *preparedSpriteMeshes;

		PodBufferHolder<unsigned> *visibleLeavesBuffer;
		PodBufferHolder<unsigned> *visibleOccludersBuffer;
		PodBufferHolder<SortedOccluder> *sortedOccludersBuffer;

		// TODO: Merge these two? Keeping it separate can be more cache-friendly though
		PodBufferHolder<drawSurfaceBSP_t> *bspDrawSurfacesBuffer;
		PodBufferHolder<MergedSurfSpan> *drawSurfSurfSpansBuffer;

		PodBufferHolder<uint8_t> *leafSurfTableBuffer;
		PodBufferHolder<unsigned> *leafSurfNumsBuffer;

		PodBufferHolder<uint8_t> *surfVisTableBuffer;
		PodBufferHolder<GLsizei> *drawSurfMultiDrawCountsBuffer;
		PodBufferHolder<const GLvoid *> *drawSurfMultiDrawIndicesBuffer;
		// For lights
		PodBufferHolder<unsigned> *drawSurfSurfSubspansBuffer;

		unsigned drawSurfSurfSubspansOffset { 0 };
		unsigned drawSurfMultiDrawDataOffset { 0 };

		PodBufferHolder<VisTestedModel> *visTestedModelsBuffer;
		PodBufferHolder<uint32_t> *leafLightBitsOfSurfacesBuffer;

		ParticleDrawSurface *particleDrawSurfaces;
		DynamicMeshDrawSurface *dynamicMeshDrawSurfaces;

		// A table of pairs (span offset, span length)
		std::pair<unsigned, unsigned> *lightSpansForParticleAggregates;
		PodBufferHolder<uint16_t> *lightIndicesForParticleAggregates;

		wsw::PodVector<DebugLine> *debugLines;

		unsigned numDynamicMeshDrawSurfaces { 0 };

		// Saved intermediate results of the world occlusion stage in addition to respective filled buffers
		std::span<const unsigned> leavesInFrustumAndPvs;
		std::span<const unsigned> surfsInFrustumAndPvs;
		unsigned numOccluderFrusta { 0 };
		bool drawWorld { false };
		bool useOcclusionCulling { false };
		bool useWorldBspOcclusionCulling { false };
		bool canAddLightsToParticles { false };

		// Includes stateForSkyPortalCamera
		wsw::StaticVector<StateForCamera *, MAX_REF_CAMERAS> portalCameraStates;
		StateForCamera *stateForSkyPortalCamera { nullptr };
	};

	static_assert( sizeof( StateForCamera ) % alignof( Frustum ) == 0 );

	[[nodiscard]]
	auto getFogForBounds( const StateForCamera *stateForCamera, const float *mins, const float *maxs ) -> mfog_t *;
	[[nodiscard]]
	auto getFogForSphere( const StateForCamera *stateForCamera, const vec3_t centre, const float radius ) -> mfog_t *;

	void bindRenderTargetAndViewport( RenderTargetComponents *renderTargetComponents, const StateForCamera *stateForCamera );

	[[nodiscard]]
	auto getDefaultFarClip( const refdef_t *fd ) const -> float;

	struct CameraOverrideParams {
		const float *pvsOrigin { nullptr };
		const float *lodOrigin { nullptr };
		unsigned renderFlagsToAdd { 0 };
		unsigned renderFlagsToClear { 0 };
	};

	[[nodiscard]]
	auto setupStateForCamera( const refdef_t *fd, unsigned sceneIndex,
							  std::optional<CameraOverrideParams> overrideParams = std::nullopt ) -> StateForCamera *;

	[[nodiscard]]
	static auto coBeginProcessingDrawSceneRequests( CoroTask::StartInfo si, Frontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask;
	[[nodiscard]]
	static auto coEndProcessingDrawSceneRequests( CoroTask::StartInfo si, Frontend *self, std::span<DrawSceneRequest *> requests ) -> CoroTask;

	[[nodiscard]]
	auto beginPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
												  bool areCamerasPortalCameras ) -> TaskHandle;
	[[nodiscard]]
	auto endPreparingRenderingFromTheseCameras( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
												std::span<const TaskHandle> dependencies,
												bool areCamerasPortalCameras ) -> TaskHandle;

	[[nodiscard]]
	static auto coBeginPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, Frontend *self,
														   std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														   bool areCamerasPortalCameras ) -> CoroTask;
	[[nodiscard]]
	static auto coEndPreparingRenderingFromTheseCameras( CoroTask::StartInfo si, Frontend *self,
														 std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
														 bool areCamerasPortalCameras ) -> CoroTask;

	void performPreparedRenderingFromThisCamera( Scene *scene, StateForCamera *stateForCamera );

	[[nodiscard]]
	bool updatePortalSurfaceUsingMesh( StateForCamera *stateForCamera, portalSurface_t *portalSurface, const mesh_t *mesh,
									   const float *mins, const float *maxs, const shader_t *shader );

	void collectVisiblePolys( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta );

	void addVisibleWorldSurfacesToSortList( StateForCamera *stateForCamera, Scene *scene );

	void collectVisibleEntities( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta );

	[[nodiscard]]
	auto collectVisibleLights( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta )
		-> std::pair<std::span<const uint16_t>, std::span<const uint16_t>>;

	void calcSubspansOfMergedSurfSpans( StateForCamera *stateForCamera );

	void processWorldPortalSurfaces( StateForCamera *stateForCamera, Scene *scene, bool isCameraAPortalCamera );

	[[nodiscard]]
	auto cullNullModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> nullModelEntities,
								std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices,
								VisTestedModel *tmpModels ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto cullAliasModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> aliasModelEntities,
								 std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
								 VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto cullSkeletalModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> skeletalModelEntities,
									std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
									VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto cullBrushModelEntities( StateForCamera *stateForCamera, std::span<const entity_t> brushModelEntities,
								 std::span<const Frustum> occluderFrusta, uint16_t *tmpIndicesBuffer,
								 VisTestedModel *selectedModelsBuffer ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto cullSpriteEntities( StateForCamera *stateForCamera, std::span<const entity_t> spriteEntities,
							 std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices, uint16_t *tmpIndices2,
							 VisTestedModel *tmpModels ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto cullLights( StateForCamera *stateForCamera, std::span<const Scene::DynamicLight> lights,
					 std::span<const Frustum> occluderFrusta, uint16_t *tmpAllLightIndices,
					 uint16_t *tmpCoronaLightIndices, uint16_t *tmpProgramLightIndices )
					 	-> std::tuple<std::span<const uint16_t>, std::span<const uint16_t>, std::span<const uint16_t>>;

	void collectVisibleParticles( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta );

	[[nodiscard]]
	auto cullParticleAggregates( StateForCamera *stateForCamera, std::span<const Scene::ParticlesAggregate> aggregates,
								 std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	void collectVisibleDynamicMeshes( StateForCamera *stateForCamera, Scene *scene, std::span<const Frustum> frusta,
									  std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices );

	[[nodiscard]]
	auto cullCompoundDynamicMeshes( StateForCamera *stateForCamera, std::span<const Scene::CompoundDynamicMesh> meshes,
									std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	// TODO: Check why spans can't be supplied
	[[nodiscard]]
	auto cullDynamicMeshes( StateForCamera *stateForCamera, const DynamicMesh **meshes, unsigned numMeshes,
							std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	// TODO: Check why spans can't be supplied
	// TODO: We can avoid supplying tmpModels argument if a generic sphere culling subroutine is available
	[[nodiscard]]
	auto cullQuadPolys( StateForCamera *stateForCamera, QuadPoly **polys, unsigned numPolys,
						std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices,
						VisTestedModel *tmpModels ) -> std::span<const uint16_t>;

	void addAliasModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *aliasModelEntities,
										  std::span<const VisTestedModel> models, std::span<const uint16_t> indices );
	void addSkeletalModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *skeletalModelEntities,
											 std::span<const VisTestedModel> models, std::span<const uint16_t> indices );

	void addNullModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *nullModelEntities,
										 std::span<const uint16_t> indices );
	void addBrushModelEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *brushModelEntities,
										  std::span<const VisTestedModel> models,
										  std::span<const uint16_t> indices, std::span<const Scene::DynamicLight> lights );

	void addSpriteEntitiesToSortList( StateForCamera *stateForCamera, const entity_t *spriteEntities,
									  std::span<const uint16_t> indices );

	void addParticlesToSortList( StateForCamera *stateForCamera, const entity_t *particleEntity,
								 const Scene::ParticlesAggregate *particles, std::span<const uint16_t> aggregateIndices );

	void addDynamicMeshesToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity, const DynamicMesh **meshes,
									 std::span<const uint16_t> indicesOfMeshes, std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices );

	void addCompoundDynamicMeshesToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity,
											 const Scene::CompoundDynamicMesh *meshes, std::span<const uint16_t> indicesOfMeshes,
											 std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices );

	void addDynamicMeshToSortList( StateForCamera *stateForCamera, const entity_t *meshEntity, const DynamicMesh *mesh,
								   float distance, std::pair<unsigned, unsigned> *offsetsOfVerticesAndIndices );

	void addCoronaLightsToSortList( StateForCamera *stateForCamera, const entity_t *polyEntity, const Scene::DynamicLight *lights,
									std::span<const uint16_t> indices );

	void addDebugLine( StateForCamera *stateForCamera, const float *p1, const float *p2, int color = COLOR_RGB( 255, 255, 255 ) );

	void submitDebugStuffToBackend( StateForCamera *stateForCamera, Scene *scene );

	// The template parameter is needed just to make instatiation of methods in different translation units correct

	enum : unsigned { Sse2 = 1, Sse41 = 2, Avx = 3 };

	template <unsigned Arch>
	[[nodiscard]]
	auto collectVisibleWorldLeavesArch( StateForCamera *stateForCamera ) -> std::span<const unsigned>;

	template <unsigned Arch>
	[[nodiscard]]
	auto collectVisibleOccludersArch( StateForCamera *stateForCamera ) -> std::span<const unsigned>;

	template <unsigned Arch>
	[[nodiscard]]
	auto buildFrustaOfOccludersArch( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
		-> std::span<const Frustum>;

	template <unsigned Arch>
	void cullSurfacesByOccludersArch( StateForCamera *stateForCamera,
									  std::span<const unsigned> indicesOfSurfaces,
									  std::span<const Frustum> occluderFrusta,
									  MergedSurfSpan *mergedSurfSpans,
									  uint8_t *surfVisTable );

	template <unsigned Arch>
	[[nodiscard]]
	auto cullEntriesWithBoundsArch( StateForCamera *stateForCamera, const void *entries,
									unsigned numEntries, unsigned boundsFieldOffset,
									unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
									std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	template <unsigned Arch>
	[[nodiscard]]
	auto cullEntryPtrsWithBoundsArch( StateForCamera *stateForCamera, const void **entryPtrs,
									  unsigned numEntries, unsigned boundsFieldOffset,
								      const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
								      uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto collectVisibleWorldLeavesSse2( StateForCamera *stateForCamera ) -> std::span<const unsigned>;
	[[nodiscard]]
	auto collectVisibleOccludersSse2( StateForCamera *stateForCamera ) -> std::span<const unsigned>;
	[[nodiscard]]
	auto buildFrustaOfOccludersSse2( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
		-> std::span<const Frustum>;

	void cullSurfacesByOccludersSse2( StateForCamera *stateForCamera,
									  std::span<const unsigned> indicesOfSurfaces,
									  std::span<const Frustum> occluderFrusta,
									  MergedSurfSpan *mergedSurfSpans,
									  uint8_t *surfVisTable );

	[[nodiscard]]
	auto cullEntriesWithBoundsSse2( StateForCamera *stateForCamera, const void *entries,
									unsigned numEntries, unsigned boundsFieldOffset,
									unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
									std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	// Allows supplying an array of pointers instead of a contignuous array
	[[nodiscard]]
	auto cullEntryPtrsWithBoundsSse2( StateForCamera *stateForCamera, const void **entryPtrs,
									  unsigned numEntries, unsigned boundsFieldOffset,
									  const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
									  uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	[[nodiscard]]
	auto collectVisibleWorldLeavesSse41( StateForCamera *stateForCamera ) -> std::span<const unsigned>;
	[[nodiscard]]
	auto collectVisibleOccludersSse41( StateForCamera *stateForCamera ) -> std::span<const unsigned>;
	[[nodiscard]]
	auto buildFrustaOfOccludersSse41( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
		-> std::span<const Frustum>;

	void cullSurfacesByOccludersSse41( StateForCamera *stateForCamera,
									   std::span<const unsigned> indicesOfSurfaces,
									   std::span<const Frustum> occluderFrusta,
									   MergedSurfSpan *mergedSurfSpans,
									   uint8_t *surfVisTable );

	[[nodiscard]]
	auto cullEntriesWithBoundsSse41( StateForCamera *stateForCamera, const void *entries,
									 unsigned numEntries, unsigned boundsFieldOffset,
									 unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
									 std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	// Allows supplying an array of pointers instead of a contignuous array
	[[nodiscard]]
	auto cullEntryPtrsWithBoundsSse41( StateForCamera *stateForCamera, const void **entryPtrs,
									   unsigned numEntries, unsigned boundsFieldOffset,
									   const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
									   uint16_t *tmpIndices ) -> std::span<const uint16_t>;


	void cullSurfacesByOccludersAvx( StateForCamera *stateForCamera,
									 std::span<const unsigned> indicesOfSurfaces,
									 std::span<const Frustum> occluderFrusta,
									 MergedSurfSpan *mergedSurfSpans,
									 uint8_t *surfVisTable );

	[[nodiscard]]
	auto collectVisibleWorldLeaves( StateForCamera *stateForCamera ) -> std::span<const unsigned>;

	[[nodiscard]]
	auto collectVisibleOccluders( StateForCamera *stateForCamera ) -> std::span<const unsigned>;

	[[nodiscard]]
	auto calcOccluderScores( StateForCamera *stateForCamera, std::span<const unsigned> visibleOccluders ) -> TaskHandle;

	[[nodiscard]]
	auto pruneAndSortOccludersByScores( StateForCamera *stateForCamera, std::span<const unsigned> visibleOccluders )
		-> std::span<const SortedOccluder>;

#if defined( IMPLEMENT_collectVisibleOccluders ) || defined( IMPLEMENT_buildFrustaOfOccluders )
#if defined( SHOW_OCCLUDERS_FRUSTA ) || defined( DEBUG_OCCLUDERS )
	// This subroutine does not have to be template but we keep it templated so the code is closer
	// to the code which builds frusta, thus making things easier to follow
	template <unsigned Arch>
	void showOccluderFrustum( StateForCamera *stateForCamera, float offsetInFrontOfView, const float *viewOrigin,
							  const float *viewAxis, const OccluderDataEntry *occluderData );
#endif
#endif

	[[nodiscard]]
	auto buildFrustaOfOccluders( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
		-> std::span<const Frustum>;

	void cullSurfacesByOccluders( StateForCamera *stateForCamera, std::span<const unsigned> indicesOfSurfaces,
								  std::span<const Frustum> occluderFrusta,
								  MergedSurfSpan *mergedSurfSpans, uint8_t *surfVisTable );

	[[nodiscard]]
	static auto coPrepareOccluders( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask;

	[[nodiscard]]
	static auto coExecPassUponInitialCullingOfLeaves( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask;

	[[nodiscard]]
	static auto coExecPassUponPreparingOccluders( CoroTask::StartInfo si, Frontend *self, StateForCamera *stateForCamera ) -> CoroTask;

	[[nodiscard]]
	auto cullEntriesWithBounds( StateForCamera *stateForCamera, const void *entries,
								unsigned numEntries, unsigned boundsFieldOffset,
								unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
								std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	// Allows supplying an array of pointers instead of a contignuous array
	[[nodiscard]]
	auto cullEntryPtrsWithBounds( StateForCamera *stateForCamera, const void **entryPtrs,
								  unsigned numEntries, unsigned boundsFieldOffset,
								  const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
								  uint16_t *tmpIndices ) -> std::span<const uint16_t>;

	void markSurfacesOfLeavesAsVisible( std::span<const unsigned> indicesOfLeaves, MergedSurfSpan *mergedSurfSpans, uint8_t *surfVisTable );

	void markLightsOfSurfaces( StateForCamera *stateForCamera, const Scene *scene,
							   std::span<std::span<const unsigned>> spansOfLeaves,
							   std::span<const uint16_t> visibleLightIndices );

	void markLightsOfLeaves( StateForCamera *stateForCamera, const Scene *scene,
							 std::span<const unsigned> indicesOfLeaves,
							 std::span<const uint16_t> visibleLightIndices,
							 unsigned *lightBitsOfSurfaces );

	void addMergedBspSurfToSortList( StateForCamera *stateForCamera, const entity_t *entity,
									 const MergedSurfSpan &surfSpan,
									 unsigned mergedSurfNum,
									 const float *maybeOrigin, std::span<const Scene::DynamicLight> lights );

	[[maybe_unused]]
	auto addEntryToSortList( StateForCamera *stateForCamera, const entity_t *e, const mfog_t *fog,
							 const shader_t *shader, float dist, unsigned order, const portalSurface_t *portalSurf,
							 const void *drawSurf, unsigned surfType, unsigned mergeabilitySeparator = 0, int shaderParamsIndex = -1 )
							 -> std::optional<unsigned>;

	void prepareDrawingPortalSurface( StateForCamera *stateForPrimaryCamera, Scene *scene, portalSurface_t *portalSurface );

	enum DrawPortalFlags : unsigned {
		DrawPortalMirror     = 0x1,
		DrawPortalRefraction = 0x2,
		DrawPortalToTexture  = 0x4,
	};

	[[nodiscard]]
	auto prepareDrawingPortalSurfaceSide( StateForCamera *stateForPrimaryCamera,
										  portalSurface_t *portalSurface, Scene *scene,
										  const entity_t *portalEntity, unsigned drawPortalFlags )
										  -> std::optional<std::pair<StateForCamera *, Texture *>>;

	void prepareDrawingSkyPortal( StateForCamera *stateForPrimaryCamera, Scene *scene );

	[[nodiscard]]
	auto findNearestPortalEntity( const portalSurface_t *portalSurface, Scene *scene ) -> const entity_t *;

	void processSortList( StateForCamera *stateForCamera, Scene *scene );
	void submitDrawActionsList( StateForCamera *stateForCamera, Scene *scene );

	using SubmitBatchedSurfFn = void(*)( const FrontendToBackendShared *, const entity_t *,
		const ShaderParams *, const ShaderParamsTable *,
		const shader_t *, const mfog_t *, const portalSurface_t *, unsigned );

	[[nodiscard]]
	auto registerBuildingBatchedSurf( StateForCamera *stateForCamera, Scene *scene, unsigned surfType, std::span<const sortedDrawSurf_t> batchSpan )
		-> std::pair<SubmitBatchedSurfFn, unsigned>;

	struct DynamicStuffWorkloadStorage;
	void markBuffersOfBatchedDynamicsForUpload( std::span<std::pair<Scene *, StateForCamera *>> scenesAndCameras,
												DynamicStuffWorkloadStorage *workloadStorage );

	struct DynamicMeshFillDataWorkload;
	void prepareDynamicMesh( DynamicMeshFillDataWorkload *workload );

	void prepareBatchedQuadPolys( PrepareBatchedSurfWorkload *workload );
	void prepareBatchedCoronas( PrepareBatchedSurfWorkload *workload );
	void prepareBatchedParticles( PrepareBatchedSurfWorkload *workload );

	void prepareLegacySprite( PrepareSpriteSurfWorkload *workload );

	auto ( Frontend::*m_collectVisibleWorldLeavesArchMethod )( StateForCamera * ) -> std::span<const unsigned>;
	auto ( Frontend::*m_collectVisibleOccludersArchMethod )( StateForCamera * ) -> std::span<const unsigned>;
	auto ( Frontend::*m_buildFrustaOfOccludersArchMethod )( StateForCamera *, std::span<const SortedOccluder> ) -> std::span<const Frustum>;
	void ( Frontend::*m_cullSurfacesByOccludersArchMethod )( StateForCamera *, std::span<const unsigned>,
															 std::span<const Frustum>, MergedSurfSpan *, uint8_t * );
	auto ( Frontend::*m_cullEntriesWithBoundsArchMethod )( StateForCamera *, const void *, unsigned, unsigned, unsigned, const Frustum *,
														   std::span<const Frustum>, uint16_t * ) -> std::span<const uint16_t>;
	auto ( Frontend::*m_cullEntryPtrsWithBoundsArchMethod )( StateForCamera *stateForCamera, const void **, unsigned, unsigned, const Frustum *,
															 std::span<const Frustum>, uint16_t * ) -> std::span<const uint16_t>;

	shader_t *m_coronaShader { nullptr };

	unsigned m_drawSceneFrame { 0 };
	unsigned m_cameraIdCounter { 0 };
	unsigned m_cameraIndexCounter { 0 };
	unsigned m_sceneIndexCounter { 0 };

	static constexpr unsigned kMaxDrawSceneRequests = 16;
	wsw::StaticVector<DrawSceneRequest, kMaxDrawSceneRequests> m_drawSceneRequestsHolder;

	struct StateForCameraStorage {
		~StateForCameraStorage() {
			if( isStateConstructed ) {
				( (StateForCamera *)theStateStorage )->~StateForCamera();
			}
		}

		alignas( alignof( StateForCamera ) ) uint8_t theStateStorage[sizeof( StateForCamera )];
		bool isStateConstructed { false };

		wsw::PodVector<ShaderParams> shaderParamsStorage;
		wsw::PodVector<ShaderParams::Material> materialParamsStorage;

		wsw::PodVector<sortedDrawSurf_t> meshSortList;
		wsw::PodVector<wsw::Function<void( FrontendToBackendShared * )>> drawActionsList;

		wsw::PodVector<PrepareBatchedSurfWorkload> preparePolysWorkloadBuffer;
		wsw::PodVector<PrepareBatchedSurfWorkload> prepareCoronasWorkloadBuffer;
		wsw::PodVector<PrepareBatchedSurfWorkload> prepareParticlesWorkloadBuffer;
		wsw::PodVector<VertElemSpan> batchedSurfVertSpansBuffer;

		wsw::PodVector<PrepareSpriteSurfWorkload> prepareSpritesWorkloadBuffer;
		wsw::PodVector<PreparedSpriteMesh> preparedSpriteMeshesBuffer;

		PodBufferHolder<unsigned> visibleLeavesBuffer;
		PodBufferHolder<unsigned> visibleOccludersBuffer;
		PodBufferHolder<SortedOccluder> sortedOccludersBuffer;

		PodBufferHolder<uint8_t> leafSurfTableBuffer;
		PodBufferHolder<unsigned> leafSurfNumsBuffer;

		PodBufferHolder<DynamicMeshDrawSurface> dynamicMeshDrawSurfacesBuffer;
		PodBufferHolder<drawSurfaceBSP_t> bspDrawSurfacesBuffer;
		PodBufferHolder<MergedSurfSpan> drawSurfSurfSpansBuffer;
		PodBufferHolder<uint8_t> bspSurfVisTableBuffer;
		PodBufferHolder<unsigned> drawSurfSurfSubspansBuffer;
		PodBufferHolder<GLsizei> drawSurfMultiDrawCountsBuffer;
		PodBufferHolder<const GLvoid *> drawSurfMultiDrawIndicesBuffer;

		PodBufferHolder<VisTestedModel> visTestedModelsBuffer;
		PodBufferHolder<uint32_t> leafLightBitsOfSurfacesBuffer;

		PodBufferHolder<ParticleDrawSurface> particleDrawSurfacesBuffer;

		wsw::PodVector<DebugLine> debugLinesBuffer;

		// TODO: Allow storing std::pair<PodType, PodType> in PodBufferHolder
		wsw::PodVector<std::pair<unsigned, unsigned>> lightSpansForParticleAggregatesBuffer;
		PodBufferHolder<uint16_t> lightIndicesForParticleAggregatesBuffer;

		StateForCameraStorage *prev { nullptr }, *next { nullptr };
	};

	wsw::Mutex m_stateAllocLock;
	wsw::Mutex m_portalTextureLock;

	[[nodiscard]]
	auto allocStateForCamera() -> StateForCamera *;
	void recycleFrameCameraStates();
	void disposeCameraStates();

	// Must be stable wrt relocations hence we use intrusive linked lists
	StateForCameraStorage *m_freeStatesForCamera { nullptr };
	StateForCameraStorage *m_usedStatesForCamera { nullptr };

	wsw::StaticVector<std::pair<Scene *, StateForCamera *>, MAX_REF_CAMERAS> m_tmpPortalScenesAndStates;

	struct DynamicMeshFillDataWorkload {
		Scene *scene;
		StateForCamera *stateForCamera;
		DynamicMeshDrawSurface *drawSurface;
	};

	// Note: Both regular and portal stages access these values from the single thread.
	std::pair<unsigned, unsigned> m_dynamicMeshOffsetsOfVerticesAndIndices { 0, 0 };
	std::pair<unsigned, unsigned> m_variousDynamicsOffsetsOfVerticesAndIndices { 0, 0 };

	struct DynamicStuffWorkloadStorage {
		wsw::PodVector<DynamicMeshFillDataWorkload> dynamicMeshFillDataWorkload;
		wsw::PodVector<PrepareBatchedSurfWorkload *> selectedPolysWorkload;
		wsw::PodVector<PrepareBatchedSurfWorkload *> selectedCoronasWorkload;
		wsw::PodVector<PrepareBatchedSurfWorkload *> selectedParticlesWorkload;
		wsw::PodVector<PrepareSpriteSurfWorkload *> selectedSpriteWorkload;
	};

	// For regular and portal stages.
	// (Note that we may start executing tasks of the regular stage while the portal stuff still gets added,
	// hence we split these workload temporaries by stage).
	DynamicStuffWorkloadStorage m_dynamicStuffWorkloadStorage[2];

	// This is not an appropriate place to keep the client-global instance of task system.
	// However, moving it to the client code is complicated due to lifetime issues related to client global vars.
	TaskSystem m_taskSystem;
};

}

#endif