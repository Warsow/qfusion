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
#ifndef R_SURFACE_H
#define R_SURFACE_H

enum drawSurfaceType_t : unsigned {
	ST_NONE,
	ST_BSP,
	ST_ALIAS,
	ST_SKELETAL,
	ST_SPRITE,
	ST_QUAD_POLY,
	ST_DYNAMIC_MESH,
	ST_PARTICLE,
	ST_CORONA,
	ST_NULLMODEL,

	ST_MAX_TYPES,
};

struct MergedBspSurface {
	struct shader_s *shader;
	struct mfog_s *fog;
	struct mesh_vbo_s *vbo;
	struct superLightStyle_s *superLightStyle;
	instancePoint_t *instances;
	unsigned numVerts, numElems;
	unsigned firstVboVert, firstVboElem;
	unsigned firstWorldSurface, numWorldSurfaces;
	unsigned numInstances;
	unsigned numLightmaps;
};

struct VertElemSpan;

typedef struct {
	void *listSurf;                 // only valid if visFrame == rf.frameCount
	MergedBspSurface *mergedBspSurf;
	unsigned dlightBits;
	const VertElemSpan *vertElemSpans;
	unsigned numSpans;
} drawSurfaceBSP_t;

typedef struct {
	struct maliasmesh_s *mesh;
	struct model_s *model;
} drawSurfaceAlias_t;

typedef struct {
	struct mskmesh_s *mesh;
	struct model_s *model;
} drawSurfaceSkeletal_t;

#endif // R_SURFACE_H
