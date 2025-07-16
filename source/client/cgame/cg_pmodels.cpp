/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2007 German Garcia

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

/*
==========================================================================

- SPLITMODELS -

==========================================================================
*/

// - Adding the Player model using Skeletal animation blending
// by Jalisk0

#include "cg_local.h"
#include <common/facilities/configvars.h>
#include <common/facilities/fscompat.h>
#include <common/helpers/parsecompat.h>

using wsw::operator""_asView;

static BoolConfigVar v_debugPlayerModels( "cg_debugPlayerModels"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE | CVAR_CHEAT } );

pmodel_t cg_entPModels[MAX_EDICTS];
pmodelinfo_t *cg_PModelInfos;

//======================================================================
//						PlayerModel Registering
//======================================================================

/*
* CG_PModelsInit
*/
void CG_PModelsInit( void ) {
	memset( cg_entPModels, 0, sizeof( cg_entPModels ) );
}

void CG_PModelsShutdown() {
	pmodelinfo_t *nextInfo;
	for( pmodelinfo_t *info = cg_PModelInfos; info; info = nextInfo ) {
		nextInfo = info->next;
		Q_free( info->name );
		cg_sexedSfx_s *nextSfx = nullptr;
		for( cg_sexedSfx_s *sexedSfx = info->sexedSfx; sexedSfx; sexedSfx = nextSfx ) {
			nextSfx = sexedSfx->next;
			Q_free( sexedSfx->name );
			Q_free( sexedSfx );
		}
		Q_free( info );
	}

	cg_PModelInfos = nullptr;
}

/*
* CG_ResetPModels
*/
void CG_ResetPModels( void ) {
	int i;

	for( i = 0; i < MAX_EDICTS; i++ ) {
		cg_entPModels[i].flash_time = cg_entPModels[i].barrel_time = 0;
		memset( &cg_entPModels[i].animState, 0, sizeof( gs_pmodel_animationstate_t ) );
	}
}

/*
* CG_FindBoneNum
*/
static int CG_FindBoneNum( cgs_skeleton_t *skel, char *bonename ) {
	int j;

	if( !skel || !bonename ) {
		return -1;
	}

	for( j = 0; j < skel->numBones; j++ ) {
		if( !Q_stricmp( skel->bones[j].name, bonename ) ) {
			return j;
		}
	}

	return -1;
}

/*
* CG_ParseRotationBone
*/
static void CG_ParseRotationBone( pmodelinfo_t *pmodelinfo, char *token, int pmpart ) {
	int boneNumber;

	boneNumber = CG_FindBoneNum( CG_SkeletonForModel( pmodelinfo->model ), token );
	if( boneNumber < 0 ) {
		if( v_debugPlayerModels.get() ) {
			Com_Printf( "CG_ParseRotationBone: No such bone name %s\n", token );
		}
		return;
	}

	//register it into pmodelinfo
	if( v_debugPlayerModels.get() ) {
		Com_Printf( "Script: CG_ParseRotationBone: %s is %i\n", token, boneNumber );
	}
	pmodelinfo->rotator[pmpart][pmodelinfo->numRotators[pmpart]] = boneNumber;
	pmodelinfo->numRotators[pmpart]++;
}

/*
* CG_ParseTagMask
*/
static void CG_ParseTagMask( struct model_s *model, int bonenum, char *name, float forward, float right, float up, float pitch, float yaw, float roll ) {
	cg_tagmask_t *tagmask;
	cgs_skeleton_t *skel;

	if( !name || !name[0] ) {
		return;
	}

	skel = CG_SkeletonForModel( model );
	if( !skel || skel->numBones <= bonenum ) {
		return;
	}

	//fixme: check the name isn't already in use, or it isn't the same as a bone name

	//now store it
	tagmask = ( cg_tagmask_t * )Q_malloc( sizeof( cg_tagmask_t ) );
	Q_snprintfz( tagmask->tagname, sizeof( tagmask->tagname ), "%s", name );
	Q_snprintfz( tagmask->bonename, sizeof( tagmask->bonename ), "%s", skel->bones[bonenum].name );
	tagmask->bonenum = bonenum;
	tagmask->offset[FORWARD] = forward;
	tagmask->offset[RIGHT] = right;
	tagmask->offset[UP] = up;
	tagmask->rotate[PITCH] = pitch;
	tagmask->rotate[YAW] = yaw;
	tagmask->rotate[ROLL] = roll;
	tagmask->next = skel->tagmasks;
	skel->tagmasks = tagmask;

	if( v_debugPlayerModels.get() ) {
		Com_Printf( "Added Tagmask: %s -> %s\n", tagmask->tagname, tagmask->bonename );
	}
}

/*
* CG_ParseAnimationScript
*
* Reads the animation config file.
*
* 0 = first frame
* 1 = lastframe
* 2 = looping frames
* 3 = fps
*
* Note: The animations count begins at 1, not 0. I preserve zero for "no animation change"
* ---------------
* keyword:
* alljumps: Uses 3 different jump animations (bunnyhoping)
*/
static bool CG_ParseAnimationScript( pmodelinfo_t *pmodelinfo, char *filename ) {
	uint8_t *buf;
	char *ptr, *token;
	int rounder, counter, i;
	bool debug = true;
	int anim_data[4][PMODEL_TOTAL_ANIMATIONS];
	int rootanims[PMODEL_PARTS];
	int filenum;
	int length;

	memset( rootanims, -1, sizeof( rootanims ) );
	pmodelinfo->sex = GENDER_MALE;
	rounder = 0;
	counter = 1; //reseve 0 for 'no animation'

	if( !v_debugPlayerModels.get() ) {
		debug = false;
	}

	// load the file
	length = FS_FOpenFile( filename, &filenum, FS_READ );
	if( length == -1 ) {
		Com_Printf( "Couldn't find animation script: %s\n", filename );
		return false;
	}

	buf = ( uint8_t * )Q_malloc( length + 1 );
	length = FS_Read( buf, length, filenum );
	FS_FCloseFile( filenum );
	if( !length ) {
		Q_free(   buf );
		Com_Printf( "Couldn't load animation script: %s\n", filename );
		return false;
	}

	// proceed
	ptr = ( char * )buf;
	while( ptr ) {
		token = COM_ParseExt( &ptr, true );
		if( !token[0] ) {
			break;
		}

		if( *token < '0' || *token > '9' ) {

			// gender
			if( !Q_stricmp( token, "sex" ) ) {
				if( debug ) {
					Com_Printf( "Script: %s:", token );
				}

				token = COM_ParseExt( &ptr, false );
				if( !token[0] ) { //Error (fixme)
					break;
				}

				if( token[0] == 'm' || token[0] == 'M' ) {
					pmodelinfo->sex = GENDER_MALE;
					if( debug ) {
						Com_Printf( " %s -Gender set to MALE\n", token );
					}
				} else if( token[0] == 'f' || token[0] == 'F' ) {
					pmodelinfo->sex = GENDER_FEMALE;
					if( debug ) {
						Com_Printf( " %s -Gender set to FEMALE\n", token );
					}
				} else if( token[0] == 'n' || token[0] == 'N' ) {
					pmodelinfo->sex = GENDER_NEUTRAL;
					if( debug ) {
						Com_Printf( " %s -Gender set to NEUTRAL\n", token );
					}
				} else {
					if( debug ) {
						if( token[0] ) {
							Com_Printf( " WARNING: unrecognized token: %s\n", token );
						} else {
							Com_Printf( " WARNING: no value after cmd sex: %s\n", token );
						}
					}
					break; //Error
				}


			}
			// Rotation bone
			else if( !Q_stricmp( token, "rotationbone" ) ) {
				token = COM_ParseExt( &ptr, false );
				if( !token[0] ) {
					break;             //Error (fixme)

				}
				if( !Q_stricmp( token, "upper" ) ) {
					token = COM_ParseExt( &ptr, false );
					if( !token[0] ) {
						break;             //Error (fixme)
					}
					CG_ParseRotationBone( pmodelinfo, token, UPPER );
				} else if( !Q_stricmp( token, "head" ) ) {
					token = COM_ParseExt( &ptr, false );
					if( !token[0] ) {
						break;             //Error (fixme)
					}
					CG_ParseRotationBone( pmodelinfo, token, HEAD );
				} else if( debug ) {
					Com_Printf( "Script: ERROR: Unrecognized rotation pmodel part %s\n", token );
					Com_Printf( "Script: ERROR: Valid names are: 'upper', 'head'\n" );
				}
			}
			// Root animation bone
			else if( !Q_stricmp( token, "rootanim" ) ) {
				token = COM_ParseExt( &ptr, false );
				if( !token[0] ) {
					break;
				}

				if( !Q_stricmp( token, "upper" ) ) {
					rootanims[UPPER] = CG_FindBoneNum( CG_SkeletonForModel( pmodelinfo->model ), COM_ParseExt( &ptr, false ) );
				} else if( !Q_stricmp( token, "head" ) ) {
					rootanims[HEAD] = CG_FindBoneNum( CG_SkeletonForModel( pmodelinfo->model ), COM_ParseExt( &ptr, false ) );
				} else if( !Q_stricmp( token, "lower" ) ) {
					rootanims[LOWER] = CG_FindBoneNum( CG_SkeletonForModel( pmodelinfo->model ), COM_ParseExt( &ptr, false ) );

					//we parse it so it makes no error, but we ignore it later on
					Com_Printf( "Script: WARNING: Ignored rootanim lower: Valid names are: 'upper', 'head' (lower is always skeleton root)\n" );
				} else if( debug ) {
					Com_Printf( "Script: ERROR: Unrecognized root animation pmodel part %s\n", token );
					Com_Printf( "Script: ERROR: Valid names are: 'upper', 'head'\n" );
				}
			}
			// Tag bone (format is: tagmask "bone name" "tag name")
			else if( !Q_stricmp( token, "tagmask" ) ) {
				int bonenum;

				token = COM_ParseExt( &ptr, false );
				if( !token[0] ) {
					break; //Error

				}
				bonenum =  CG_FindBoneNum( CG_SkeletonForModel( pmodelinfo->model ), token );
				if( bonenum != -1 ) {
					char maskname[MAX_QPATH];
					float forward, right, up, pitch, yaw, roll;

					token = COM_ParseExt( &ptr, false );
					if( !token[0] ) {
						Com_Printf( "Script: ERROR: missing maskname in tagmask for bone %i\n", bonenum );
						break; //Error
					}
					Q_strncpyz( maskname, token, sizeof( maskname ) );
					forward = atof( COM_ParseExt( &ptr, false ) );
					right = atof( COM_ParseExt( &ptr, false ) );
					up = atof( COM_ParseExt( &ptr, false ) );
					pitch = atof( COM_ParseExt( &ptr, false ) );
					yaw = atof( COM_ParseExt( &ptr, false ) );
					roll = atof( COM_ParseExt( &ptr, false ) );
					CG_ParseTagMask( pmodelinfo->model, bonenum, maskname, forward, right, up, pitch, yaw, roll );
				} else if( debug ) {
					Com_Printf( "Script: WARNING: Unknown bone name: %s\n", token );
				}

			} else if( token[0] && debug ) {
				Com_Printf( "Script: WARNING: unrecognized token: %s\n", token );
			}

		} else {
			// frame & animation values
			i = (int)atoi( token );
			if( debug ) {
				Com_Printf( "%i - ", i );
			}
			anim_data[rounder][counter] = i;
			rounder++;
			if( rounder > 3 ) {
				rounder = 0;
				if( debug ) {
					Com_Printf( " anim: %i\n", counter );
				}
				counter++;
				if( counter == PMODEL_TOTAL_ANIMATIONS ) {
					break;
				}
			}
		}
	}

	Q_free(   buf );

	//it must contain at least as many animations as a Q3 script to be valid
	if( counter < PMODEL_TOTAL_ANIMATIONS ) {
		Com_Printf( "PModel Error: Not enough animations(%i) at animations script: %s\n", counter, filename );
		return false;
	}

	// animation ANIM_NONE (0) is always at frame 0, and it's never
	// received from the game, but just used on the client when none
	// animation was ever set for a model (head).

	anim_data[0][ANIM_NONE] = 0;
	anim_data[1][ANIM_NONE] = 0;
	anim_data[2][ANIM_NONE] = 1;
	anim_data[3][ANIM_NONE] = 15;

	// reorganize to make my life easier
	for( i = 0; i < counter; i++ ) {
		pmodelinfo->animSet.firstframe[i] = anim_data[0][i];
		pmodelinfo->animSet.lastframe[i] = anim_data[1][i];
		pmodelinfo->animSet.loopingframes[i] = anim_data[2][i];
		pmodelinfo->animSet.frametime[i] = 1000.0f / (float)( anim_data[3][i] > 10 ? anim_data[3][i] : 10 );
	}

	// validate frames inside skeleton range
	{
		cgs_skeleton_t *skel;
		skel = CG_SkeletonForModel( pmodelinfo->model );
		for( i = 0; i < counter; ++i ) {
			Q_clamp( pmodelinfo->animSet.firstframe[i], 0, skel->numFrames - 1 );
			Q_clamp( pmodelinfo->animSet.lastframe[i], 0, skel->numFrames - 1 );
		}
	}

	// store root bones of animations
	rootanims[LOWER] = -1;
	for( i = LOWER; i < PMODEL_PARTS; i++ )
		pmodelinfo->rootanims[i] = rootanims[i];

	return true;
}

/*
* CG_LoadPlayerModel
*/
static bool CG_LoadPlayerModel( pmodelinfo_t *pmodelinfo, const char *filename ) {
	bool loaded_model = false;
	char anim_filename[MAX_QPATH];
	char scratch[MAX_QPATH];

	Q_snprintfz( scratch, sizeof( scratch ), "%s/tris.iqm", filename );
	if( cgs.pure && !FS_IsPureFile( scratch ) ) {
		return false;
	}

	pmodelinfo->model = CG_RegisterModel( scratch );
	if( !cg.renderSystem->getNumBones( pmodelinfo->model, NULL ) ) {
		// pmodels only accept skeletal models
		pmodelinfo->model = NULL;
		return false;
	}

	// load animations script
	if( pmodelinfo->model ) {
		Q_snprintfz( anim_filename, sizeof( anim_filename ), "%s/animation.cfg", filename );
		if( !cgs.pure || FS_IsPureFile( anim_filename ) ) {
			loaded_model = CG_ParseAnimationScript( pmodelinfo, anim_filename );
		}
	}

	// clean up if failed
	if( !loaded_model ) {
		pmodelinfo->model = NULL;
		return false;
	}

	pmodelinfo->name = Q_strdup( filename );

	// load sexed sounds for this model
	CG_UpdateSexedSoundsRegistration( pmodelinfo );
	return true;
}

/*
* CG_RegisterPModel
* PModel is not exactly the model, but the indexes of the
* models contained in the pmodel and it's animation data
*/
struct pmodelinfo_s *CG_RegisterPlayerModel( const char *filename ) {
	pmodelinfo_t *pmodelinfo;

	for( pmodelinfo = cg_PModelInfos; pmodelinfo; pmodelinfo = pmodelinfo->next ) {
		if( !Q_stricmp( pmodelinfo->name, filename ) ) {
			return pmodelinfo;
		}
	}

	pmodelinfo = ( pmodelinfo_t * )Q_malloc( sizeof( pmodelinfo_t ) );
	if( !CG_LoadPlayerModel( pmodelinfo, filename ) ) {
		Q_free(   pmodelinfo );
		return NULL;
	}

	pmodelinfo->next = cg_PModelInfos;
	cg_PModelInfos = pmodelinfo;

	return pmodelinfo;
}

/*
* CG_RegisterBasePModel
* Default fallback replacements
*/
void CG_RegisterBasePModel( void ) {
	char filename[MAX_QPATH];

	// pmodelinfo
	Q_snprintfz( filename, sizeof( filename ), "%s/%s", "models/players", DEFAULT_PLAYERMODEL );
	cgs.basePModelInfo = CG_RegisterPlayerModel( filename );

	Q_snprintfz( filename, sizeof( filename ), "%s/%s/%s", "models/players", DEFAULT_PLAYERMODEL, DEFAULT_PLAYERSKIN );
	cgs.baseSkin = cg.renderSystem->registerSkinFile( filename );
	if( !cgs.baseSkin ) {
		CG_Error( "'Default Player Model'(%s): Skin (%s) failed to load", DEFAULT_PLAYERMODEL, filename );
	}

	if( !cgs.basePModelInfo ) {
		CG_Error( "'Default Player Model'(%s): failed to load", DEFAULT_PLAYERMODEL );
	}
}

//======================================================================
//							tools
//======================================================================


/*
* CG_GrabTag
* In the case of skeletal models, boneposes must
* be transformed prior to calling this function
*/
bool CG_GrabTag( orientation_t *tag, entity_t *ent, const char *tagname ) {
	cgs_skeleton_t *skel;

	if( !ent->model ) {
		return false;
	}

	skel = CG_SkeletonForModel( ent->model );
	if( skel ) {
		return CG_SkeletalPoseGetAttachment( tag, skel, ent->boneposes, tagname );
	}

	return cg.renderSystem->lerpTag( tag, ent->model, ent->frame, ent->oldframe, ent->backlerp, tagname );
}

/*
* CG_PlaceRotatedModelOnTag
*/
void CG_PlaceRotatedModelOnTag( entity_t *ent, entity_t *dest, orientation_t *tag ) {
	int i;
	mat3_t tmpAxis;

	VectorCopy( dest->origin, ent->origin );
	VectorCopy( dest->lightingOrigin, ent->lightingOrigin );

	for( i = 0; i < 3; i++ )
		VectorMA( ent->origin, tag->origin[i] * ent->scale, &dest->axis[i * 3], ent->origin );

	VectorCopy( ent->origin, ent->origin2 );
	Matrix3_Multiply( ent->axis, tag->axis, tmpAxis );
	Matrix3_Multiply( tmpAxis, dest->axis, ent->axis );
}

/*
* CG_PlaceModelOnTag
*/
void CG_PlaceModelOnTag( entity_t *ent, entity_t *dest, orientation_t *tag ) {
	int i;

	VectorCopy( dest->origin, ent->origin );
	VectorCopy( dest->lightingOrigin, ent->lightingOrigin );

	for( i = 0; i < 3; i++ )
		VectorMA( ent->origin, tag->origin[i] * ent->scale, &dest->axis[i * 3], ent->origin );

	VectorCopy( ent->origin, ent->origin2 );
	Matrix3_Multiply( tag->axis, dest->axis, ent->axis );
}

/*
* CG_MoveToTag
* "move" tag must have an axis and origin set up. Use vec3_origin and axis_identity for "nothing"
*/
void CG_MoveToTag( vec3_t move_origin,
				   mat3_t move_axis,
				   const vec3_t space_origin,
				   const mat3_t space_axis,
				   const vec3_t tag_origin,
				   const mat3_t tag_axis ) {
	int i;
	mat3_t tmpAxis;

	VectorCopy( space_origin, move_origin );

	for( i = 0; i < 3; i++ )
		VectorMA( move_origin, tag_origin[i], &space_axis[i * 3], move_origin );

	Matrix3_Multiply( move_axis, tag_axis, tmpAxis );
	Matrix3_Multiply( tmpAxis, space_axis, move_axis );
}

/*
* CG_PModel_GetProjectionSource
* It asumes the player entity is up to date
*/
bool CG_PModel_GetProjectionSource( int entnum, orientation_t *tag_result, const ViewState *viewState ) {
	centity_t *cent;
	pmodel_t *pmodel;

	if( !tag_result ) {
		return false;
	}

	if( entnum < 1 || entnum >= MAX_EDICTS ) {
		return false;
	}

	cent = &cg_entities[entnum];
	if( cent->serverFrame != cg.frame.serverFrame ) {
		return false;
	}

	// see if it's the view weapon
	if( viewState && viewState->isViewerEntity( entnum ) && !viewState->view.thirdperson ) {
		VectorCopy( viewState->weapon.projectionSource.origin, tag_result->origin );
		Matrix3_Copy( viewState->weapon.projectionSource.axis, tag_result->axis );
		return true;
	}

	// it's a 3rd person model
	pmodel = &cg_entPModels[entnum];
	VectorCopy( pmodel->projectionSource.origin, tag_result->origin );
	Matrix3_Copy( pmodel->projectionSource.axis, tag_result->axis );
	return true;
}

/*
* CG_AddRaceGhostShell
*/
static void CG_AddRaceGhostShell( entity_t *ent, DrawSceneRequest *drawSceneRequest ) {
	entity_t shell;
	const float alpha = v_raceGhostsAlpha.get();
	assert( alpha >= 0.0f && alpha <= 1.0f );

	shell = *ent;
	shell.customSkin = NULL;

	if( shell.renderfx & RF_WEAPONMODEL ) {
		return;
	}

	shell.customShader = cgs.media.shaderRaceGhostEffect;
	shell.renderfx |= ( RF_FULLBRIGHT | RF_NOSHADOW );
	shell.outlineHeight = 0;

	shell.color[0] *= alpha;
	shell.color[1] *= alpha;
	shell.color[2] *= alpha;
	shell.color[3] = 255 * alpha;

	CG_AddEntityToScene( &shell, drawSceneRequest );
}

/*
* CG_AddShellEffects
*/
void CG_AddShellEffects( entity_t *ent, int effects, DrawSceneRequest *drawSceneRequest ) {
	if( effects & EF_RACEGHOST ) {
		CG_AddRaceGhostShell( ent, drawSceneRequest );
	}
}

/*
* CG_SetOutlineColor
*/
void CG_SetOutlineColor( byte_vec4_t outlineColor, byte_vec4_t color ) {
	float darken = 0.25f;
	outlineColor[0] = ( uint8_t )( color[0] * darken );
	outlineColor[1] = ( uint8_t )( color[1] * darken );
	outlineColor[2] = ( uint8_t )( color[2] * darken );
	outlineColor[3] = ( uint8_t )( 255 );
}

/*
* CG_OutlineScaleForDist
*/
static float CG_OutlineScaleForDist( entity_t *e, float maxdist, float scale, const ViewState *viewState ) {
	float dist;
	vec3_t dir;

	if( e->renderfx & ( RF_WEAPONMODEL | RF_VIEWERMODEL ) ) {
		return 0.14f;
	}

	// Kill if behind the view or if too far away
	VectorSubtract( e->origin, viewState->view.origin, dir );
	dist = VectorNormalize2( dir, dir ) * viewState->view.fracDistFOV;
	if( dist > maxdist ) {
		return 0;
	}

	if( !( e->renderfx & RF_WEAPONMODEL ) ) {
		if( DotProduct( dir, &viewState->view.axis[AXIS_FORWARD] ) < 0 ) {
			return 0;
		}
	}

	dist *= scale;

	if( dist < 64 ) {
		return 0.14f;
	}
	if( dist < 128 ) {
		return 0.30f;
	}
	if( dist < 256 ) {
		return 0.42f;
	}
	if( dist < 512 ) {
		return 0.56f;
	}
	if( dist < 768 ) {
		return 0.70f;
	}

	return 1.0f;
}

/*
* CG_AddColoredOutLineEffect
*/
void CG_AddColoredOutLineEffect( entity_t *ent, int effects, uint8_t r, uint8_t g, uint8_t b, uint8_t a, const ViewState *viewState ) {
	float scale;
	uint8_t *RGBA;

	if( effects & EF_QUAD ) {
		if( ( effects & EF_EXPIRING_QUAD ) && ( ( cg.time / 100 ) & 4 ) ) {
			effects &= ~EF_QUAD;
		}
	}

	if( effects & EF_SHELL ) {
		if( ( effects & EF_EXPIRING_SHELL ) && ( ( ( cg.time + 500 ) / 100 ) & 4 ) ) {
			effects &= ~EF_SHELL;
		}
	}

	if( effects & EF_REGEN ) {
		if( ( effects & EF_EXPIRING_REGEN ) && ( ( cg.time / 100 ) & 4 ) ) {
			effects &= ~EF_REGEN;
		}
	}


	if( effects & ( EF_QUAD | EF_SHELL | EF_REGEN | EF_GODMODE ) ) {
		float pulse;
		scale = CG_OutlineScaleForDist( ent, 2048, 3.5f, viewState );
		pulse = fabs( sin( cg.time * 0.005f ) );
		scale += 1.25f * scale * pulse * pulse;
	} else if( !v_outlineModels.get() || !( effects & EF_OUTLINE ) ) {
		scale = 0;
	} else {
		scale = CG_OutlineScaleForDist( ent, 1024, 1.0f, viewState );
	}

	if( !scale ) {
		ent->outlineHeight = 0;
		return;
	}

	ent->outlineHeight = scale;
	RGBA = ent->outlineRGBA;

	// All powerups
	if( ( effects & ( EF_QUAD | EF_SHELL | EF_REGEN ) ) == ( EF_QUAD | EF_SHELL | EF_REGEN ) ) {
		if( (int64_t)( cg.time * 0.005 ) & 1 ) {
			effects &= ~EF_SHELL;
		} else if( (int64_t)( cg.time * 0.01 ) & 1 ) {
			effects &= ~EF_REGEN;
		} else {
			effects &= ~EF_QUAD;
		}
	}

	// Quad + regen
	if( ( effects & ( EF_QUAD | EF_REGEN ) ) == ( EF_QUAD | EF_REGEN ) ) {
		if( (int64_t)( cg.time * 0.005 ) & 1 ) {
			effects &= ~EF_REGEN;
		} else {
			effects &= ~EF_QUAD;
		}
	}

	// Shell + regen
	if( ( effects & ( EF_SHELL | EF_REGEN ) ) == ( EF_SHELL | EF_REGEN ) ) {
		if( (int64_t)( cg.time * 0.005 ) & 1 ) {
			effects &= ~EF_REGEN;
		} else {
			effects &= ~EF_SHELL;
		}
	}

	// Shell + quad
	if( ( effects & ( EF_SHELL | EF_QUAD ) ) == ( EF_SHELL | EF_QUAD ) ) {
		if( (int64_t)( cg.time * 0.005 ) & 1 ) {
			effects &= ~EF_REGEN;
		} else {
			effects &= ~EF_QUAD;
		}
	}

	if( effects & EF_GODMODE ) {
		Vector4Set( RGBA, 255, 255, 255, a );
	} else if( effects & EF_QUAD ) {
		Vector4Set( RGBA, 255, 255, 0, a );
	} else if( effects & EF_SHELL ) {
		Vector4Set( RGBA, 125, 200, 255, a );
	} else if( effects & EF_REGEN ) {
		Vector4Set( RGBA, 255, 0, 0, a );
	} else {
		Vector4Set( RGBA, ( uint8_t )r, ( uint8_t )g, ( uint8_t )b, ( uint8_t )a );
	}
}

/*
* CG_PModel_AddFlag
*/
static void CG_PModel_AddFlag( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *viewState ) {
	int flag_team  = ( cent->current.team == TEAM_ALPHA ) ? TEAM_BETA : TEAM_ALPHA;
	vec4_t teamcolor;
	byte_vec4_t col;

	// color for team
	if( cent->current.team != TEAM_BETA && cent->current.team != TEAM_ALPHA ) {
		Vector4Set( teamcolor, 1, 1, 1, 1 );
	} else {
		CG_TeamColor( flag_team, teamcolor );
	}

	Vector4Scale( teamcolor, 255, col );

	CG_AddFlagModelOnTag( cent, col, "tag_flag1", drawSceneRequest, viewState );
}

/*
* CG_PModel_AddHeadIcon
*/
static void CG_AddHeadIcon( centity_t *cent, DrawSceneRequest *drawSceneRequest ) {
	entity_t balloon;
	bool stunned = false, showIcon = false;
	struct shader_s *iconShader = NULL;
	float radius = 6, upoffset = 8;
	orientation_t tag_head;

	if( cent->ent.renderfx & RF_VIEWERMODEL ) {
		return;
	}

	if( cent->effects & EF_BUSYICON ) {
		iconShader = cgs.media.shaderChatBalloon;
		radius = 12;
		upoffset = 2;
	}

	stunned = ( ( cent->effects & EF_PLAYER_STUNNED || cent->prev.effects & EF_PLAYER_STUNNED ) ? true : false );
	if( iconShader != NULL || stunned ) {
		showIcon = true;
	}

	// add the current active icon
	if( showIcon ) {
		memset( &balloon, 0, sizeof( entity_t ) );
		Vector4Set( balloon.shaderRGBA, 255, 255, 255, 255 );
		balloon.renderfx = RF_NOSHADOW;
		balloon.scale = 1.0f;

		Matrix3_Identity( balloon.axis );

		if( CG_GrabTag( &tag_head, &cent->ent, "tag_head" ) ) {
			balloon.origin[0] = tag_head.origin[0];
			balloon.origin[1] = tag_head.origin[1];
			balloon.origin[2] = tag_head.origin[2] + balloon.radius + upoffset;
			VectorCopy( balloon.origin, balloon.origin2 );
			CG_PlaceModelOnTag( &balloon, &cent->ent, &tag_head );
		} else {
			balloon.origin[0] = cent->ent.origin[0];
			balloon.origin[1] = cent->ent.origin[1];
			balloon.origin[2] = cent->ent.origin[2] + playerbox_stand_maxs[2] + balloon.radius + upoffset;
			VectorCopy( balloon.origin, balloon.origin2 );
		}

		if( iconShader ) {
			balloon.rtype = RT_SPRITE;
			balloon.customShader = iconShader;
			balloon.radius = radius;
			balloon.model = NULL;

			drawSceneRequest->addEntity( &balloon );
		}

		// add stun effect: not really a head icon, but there's no point in finding the head location twice
		if( stunned ) {
			balloon.rtype = RT_MODEL;
			balloon.customShader = NULL;
			balloon.radius = 0;
			balloon.model = cgs.media.modHeadStun;

			if( !( cent->current.effects & EF_PLAYER_STUNNED ) ) {
				balloon.shaderRGBA[3] = ( 255 * ( 1.0f - cg.lerpfrac ) );
			}

			drawSceneRequest->addEntity( &balloon );
		}
	}
}


//======================================================================
//							animations
//======================================================================

void CG_PModel_ClearEventAnimations( int entNum ) {
	GS_PlayerModel_ClearEventAnimations( &cg_entPModels[entNum].pmodelinfo->animSet, &cg_entPModels[entNum].animState );
}

void CG_PModel_AddAnimation( int entNum, int loweranim, int upperanim, int headanim, int channel ) {
	GS_PlayerModel_AddAnimation( &cg_entPModels[entNum].animState, loweranim, upperanim, headanim, channel );
}


//======================================================================
//							player model
//======================================================================


void CG_PModel_LeanAngles( centity_t *cent, pmodel_t *pmodel ) {
#define MIN_LEANING_SPEED 10
	mat3_t axis;
	vec3_t hvelocity;
	float speed, front, side, aside, scale;
	vec3_t leanAngles[PMODEL_PARTS];
	int i, j;

	memset( leanAngles, 0, sizeof( leanAngles ) );

	hvelocity[0] = cent->animVelocity[0];
	hvelocity[1] = cent->animVelocity[1];
	hvelocity[2] = 0;

	scale = 0.04f;

	if( ( speed = VectorLengthFast( hvelocity ) ) * scale > 1.0f ) {
		AnglesToAxis( tv( 0, cent->current.angles[YAW], 0 ), axis );

		front = scale * DotProduct( hvelocity, &axis[AXIS_FORWARD] );
		if( front < -0.1 || front > 0.1 ) {
			leanAngles[LOWER][PITCH] += front;
			leanAngles[UPPER][PITCH] -= front * 0.25;
			leanAngles[HEAD][PITCH] -= front * 0.5;
		}

		aside = ( front * 0.001f ) * cent->yawVelocity;

		if( aside ) {
			float asidescale = 75;
			leanAngles[LOWER][ROLL] -= aside * 0.5 * asidescale;
			leanAngles[UPPER][ROLL] += aside * 1.75 * asidescale;
			leanAngles[HEAD][ROLL] -= aside * 0.35 * asidescale;
		}

		side = scale * DotProduct( hvelocity, &axis[AXIS_RIGHT] );

		if( side < -1 || side > 1 ) {
			leanAngles[LOWER][ROLL] -= side * 0.5;
			leanAngles[UPPER][ROLL] += side * 0.5;
			leanAngles[HEAD][ROLL] += side * 0.25;
		}

		Q_clamp( leanAngles[LOWER][PITCH], -45, 45 );
		Q_clamp( leanAngles[LOWER][ROLL], -15, 15 );

		Q_clamp( leanAngles[UPPER][PITCH], -45, 45 );
		Q_clamp( leanAngles[UPPER][ROLL], -20, 20 );

		Q_clamp( leanAngles[HEAD][PITCH], -45, 45 );
		Q_clamp( leanAngles[HEAD][ROLL], -20, 20 );
	}

	for( j = LOWER; j < PMODEL_PARTS; j++ ) {
		for( i = 0; i < 3; i++ )
			pmodel->angles[i][j] = AngleNormalize180( pmodel->angles[i][j] + leanAngles[i][j] );
	}

#undef MIN_LEANING_SPEED
}

/*
* CG_UpdatePModelAnimations
* It's better to delay this set up until the other entities are linked, so they
* can be detected as groundentities by the animation checks
*/
void CG_UpdatePModelAnimations( centity_t *cent ) {
	int newanim[PMODEL_PARTS];
	int frame;

	cent->pendingAnimationsUpdate = false;

	if( cent->current.frame ) { // animation was provided by the server
		frame = cent->current.frame;
	} else {
		frame = GS_UpdateBaseAnims( &cent->current, cent->animVelocity );
	}

	// filter unchanged animations
	newanim[LOWER] = ( frame & 0x3F ) * ( ( frame & 0x3F ) != ( cent->lastAnims & 0x3F ) );
	newanim[UPPER] = ( frame >> 6 & 0x3F ) * ( ( frame >> 6 & 0x3F ) != ( cent->lastAnims >> 6 & 0x3F ) );
	newanim[HEAD] = ( frame >> 12 & 0xF ) * ( ( frame >> 12 & 0xF ) != ( cent->lastAnims >> 12 & 0xF ) );
	cent->lastAnims = frame;

	CG_PModel_AddAnimation( cent->current.number, newanim[LOWER], newanim[UPPER], newanim[HEAD], BASE_CHANNEL );
}

/*
* CG_UpdatePlayerModelEnt
* Called each new serverframe
*/
void CG_UpdatePlayerModelEnt( centity_t *cent ) {
	int i;
	pmodel_t *pmodel;

	// start from clean
	memset( &cent->ent, 0, sizeof( cent->ent ) );
	cent->ent.scale = 1.0f;
	cent->ent.rtype = RT_MODEL;
	cent->ent.renderfx = cent->renderfx;

	pmodel = &cg_entPModels[cent->current.number];
	CG_PModelForCentity( cent, &pmodel->pmodelinfo, &pmodel->skin );

	CG_PlayerColorForEntity( cent->current.number, cent->ent.shaderRGBA );

	// outline color
	CG_SetOutlineColor( cent->outlineColor, cent->ent.shaderRGBA );

	// TODO: Update effects just before submission
	if( v_raceGhosts.get() && !getPrimaryViewState()->isViewerEntity( cent->current.number ) && GS_RaceGametype( *cggs ) ) {
		cent->effects &= ~EF_OUTLINE;
		cent->effects |= EF_RACEGHOST;
	} else {
		if( v_outlinePlayers.get() ) {
			cent->effects |= EF_OUTLINE; // add EF_OUTLINE to players
		} else {
			cent->effects &= ~EF_OUTLINE;
		}
	}

	// fallback
	if( !pmodel->pmodelinfo || !pmodel->skin ) {
		pmodel->pmodelinfo = cgs.basePModelInfo;
		pmodel->skin = cgs.baseSkin;
	}

	// make sure al poses have their memory space
	cent->skel = CG_SkeletonForModel( pmodel->pmodelinfo->model );
	if( !cent->skel ) {
		CG_Error( "CG_PlayerModelEntityNewState: ET_PLAYER without a skeleton\n" );
	}

	// Spawning (teleported bit) forces nobacklerp and the interruption of EVENT_CHANNEL animations
	if( cent->current.teleported ) {
		CG_PModel_ClearEventAnimations( cent->current.number );
	}

	// update parts rotation angles
	for( i = LOWER; i < PMODEL_PARTS; i++ )
		VectorCopy( pmodel->angles[i], pmodel->oldangles[i] );

	if( cent->current.type == ET_CORPSE ) {
		VectorClear( cent->animVelocity );
		cent->yawVelocity = 0;
	} else {
		// update smoothed velocities used for animations and leaning angles
		int count;
		float adelta;

		// rotational yaw velocity
		adelta = AngleDelta( cent->current.angles[YAW], cent->prev.angles[YAW] );
		Q_clamp( adelta, -35, 35 );

		// smooth a velocity vector between the last snaps
		cent->lastVelocities[cg.frame.serverFrame & 3][0] = cent->velocity[0];
		cent->lastVelocities[cg.frame.serverFrame & 3][1] = cent->velocity[1];
		cent->lastVelocities[cg.frame.serverFrame & 3][2] = 0;
		cent->lastVelocities[cg.frame.serverFrame & 3][3] = adelta;
		cent->lastVelocitiesFrames[cg.frame.serverFrame & 3] = cg.frame.serverFrame;

		count = 0;
		VectorClear( cent->animVelocity );
		cent->yawVelocity = 0;
		for( i = cg.frame.serverFrame; ( i >= 0 ) && ( count < 3 ) && ( i == cent->lastVelocitiesFrames[i & 3] ); i-- ) {
			count++;
			cent->animVelocity[0] += cent->lastVelocities[i & 3][0];
			cent->animVelocity[1] += cent->lastVelocities[i & 3][1];
			cent->animVelocity[2] += cent->lastVelocities[i & 3][2];
			cent->yawVelocity += cent->lastVelocities[i & 3][3];
		}

		// safety/static code analysis check
		if( count == 0 ) {
			count = 1;
		}

		VectorScale( cent->animVelocity, 1.0f / (float)count, cent->animVelocity );
		cent->yawVelocity /= (float)count;


		//
		// Calculate angles for each model part
		//

		// lower has horizontal direction, and zeroes vertical
		pmodel->angles[LOWER][PITCH] = 0;
		pmodel->angles[LOWER][YAW] = cent->current.angles[YAW];
		pmodel->angles[LOWER][ROLL] = 0;

		// upper marks vertical direction (total angle, so it fits aim)
		if( cent->current.angles[PITCH] > 180 ) {
			pmodel->angles[UPPER][PITCH] = ( -360 + cent->current.angles[PITCH] );
		} else {
			pmodel->angles[UPPER][PITCH] = cent->current.angles[PITCH];
		}

		pmodel->angles[UPPER][YAW] = 0;
		pmodel->angles[UPPER][ROLL] = 0;

		// head adds a fraction of vertical angle again
		if( cent->current.angles[PITCH] > 180 ) {
			pmodel->angles[HEAD][PITCH] = ( -360 + cent->current.angles[PITCH] ) / 3;
		} else {
			pmodel->angles[HEAD][PITCH] = cent->current.angles[PITCH] / 3;
		}

		pmodel->angles[HEAD][YAW] = 0;
		pmodel->angles[HEAD][ROLL] = 0;

		CG_PModel_LeanAngles( cent, pmodel );
	}


	// Spawning (teleported bit) forces nobacklerp and the interruption of EVENT_CHANNEL animations
	if( cent->current.teleported ) {
		for( i = LOWER; i < PMODEL_PARTS; i++ )
			VectorCopy( pmodel->angles[i], pmodel->oldangles[i] );
	}

	cent->pendingAnimationsUpdate = true;
}

static bonepose_t blendpose[SKM_MAX_BONES];

/*
* CG_AddPModel
*/
void CG_AddPModel( centity_t *cent, DrawSceneRequest *drawSceneRequest, ViewState *viewState ) {
	int i, j;
	pmodel_t *pmodel;
	vec3_t tmpangles;
	orientation_t tag_weapon;
	int rootanim;
	gs_pmodel_animationstate_t *animState;

	if( cent->pendingAnimationsUpdate ) {
		CG_UpdatePModelAnimations( cent );
	}

	pmodel = &cg_entPModels[cent->current.number];

	// if viewer model, and casting shadows, offset the entity to predicted player position
	// for view and shadow accuracy

	if( viewState->isViewerEntity( cent->current.number ) ) {
		vec3_t org;

		if( viewState->view.playerPrediction ) {
			float backlerp = 1.0f - cg.lerpfrac;

			for( i = 0; i < 3; i++ )
				org[i] = viewState->predictedPlayerState.pmove.origin[i] - backlerp * viewState->predictionError[i];

			CG_ViewSmoothPredictedSteps( org, viewState );

			tmpangles[YAW]   = viewState->predictedPlayerState.viewangles[YAW];
			tmpangles[PITCH] = 0;
			tmpangles[ROLL]  = 0;
			AnglesToAxis( tmpangles, cent->ent.axis );
		} else {
			VectorCopy( cent->ent.origin, org );
		}

		// (cheap trick) if not thirdperson offset it some units back so the shadow looks more at our feet
		if( cent->ent.renderfx & RF_VIEWERMODEL && !( cent->renderfx & RF_NOSHADOW ) ) {
			// TODO: This would be useful for shadowmaps
			VectorMA( org, -24, &cent->ent.axis[AXIS_FORWARD], org );
		}

		VectorCopy( org, cent->ent.origin );
		VectorCopy( org, cent->ent.origin2 );
		VectorCopy( org, cent->ent.lightingOrigin );
		VectorCopy( org, cg.lightingOrigin );
	}

	// since origin is displaced in player models set lighting origin to the center of the bbox
#if 1
	for( i = 0; i < 3; i++ )
		cent->ent.lightingOrigin[i] = cent->ent.origin[i] + ( 0.5f * ( playerbox_stand_mins[i] + playerbox_stand_maxs[i] ) );
#else
	if( 1 ) {

		vec3_t mins, maxs;

		GS_BBoxForEntityState( &cent->current, mins, maxs );

		for( i = 0; i < 3; i++ )
			cent->ent.lightingOrigin[i] = cent->ent.origin[i] + ( 0.5f * ( mins[i] + maxs[i] ) );
	}
#endif

	animState = &pmodel->animState;

	// transform animation values into frames, and set up old-current poses pair
	GS_PModel_AnimToFrame( cg.time, &pmodel->pmodelinfo->animSet, animState );

	// register temp boneposes for this skeleton
	if( !cent->skel ) {
		CG_Error( "CG_PlayerModelEntityAddToScene: ET_PLAYER without a skeleton\n" );
	}
	cent->ent.boneposes = CG_RegisterTemporaryExternalBoneposes( cent->skel );
	cent->ent.oldboneposes = cent->ent.boneposes;

	// fill base pose with lower animation already interpolated
	CG_LerpSkeletonPoses( cent->skel, animState->frame[LOWER], animState->oldframe[LOWER], cent->ent.boneposes, animState->lerpFrac[LOWER] );

	// create an interpolated pose of the animation to be blent
	CG_LerpSkeletonPoses( cent->skel, animState->frame[UPPER], animState->oldframe[UPPER], blendpose, animState->lerpFrac[UPPER] );

	// blend it into base pose
	rootanim = pmodel->pmodelinfo->rootanims[UPPER];
	CG_RecurseBlendSkeletalBone( blendpose, cent->ent.boneposes, CG_BoneNodeFromNum( cent->skel, rootanim ), 1.0f );

	// add skeleton effects (pose is unmounted yet)
	if( cent->current.type != ET_CORPSE ) {
		// if it's our client use the predicted angles
		// TODO: The trailing condition is redundant (isn't it?)
		if( viewState->view.playerPrediction && viewState->isViewerEntity( cent->current.number ) && ( (unsigned)viewState->view.POVent == cgs.playerNum + 1 ) ) {
			tmpangles[YAW]   = viewState->predictedPlayerState.viewangles[YAW];
			tmpangles[PITCH] = 0;
			tmpangles[ROLL]  = 0;
		} else {
			// apply interpolated LOWER angles to entity
			for( j = 0; j < 3; j++ )
				tmpangles[j] = LerpAngle( pmodel->oldangles[LOWER][j], pmodel->angles[LOWER][j], cg.lerpfrac );
		}

		AnglesToAxis( tmpangles, cent->ent.axis );

		// apply UPPER and HEAD angles to rotator bones
		// also add rotations from velocity leaning
		for( i = UPPER; i < PMODEL_PARTS; i++ ) {
			// rotator bones
			if( pmodel->pmodelinfo->numRotators[i] ) {
				// lerp rotation and divide angles by the number of rotation bones
				for( j = 0; j < 3; j++ ) {
					tmpangles[j] = LerpAngle( pmodel->oldangles[i][j], pmodel->angles[i][j], cg.lerpfrac );
					tmpangles[j] /= pmodel->pmodelinfo->numRotators[i];
				}

				for( j = 0; j < pmodel->pmodelinfo->numRotators[i]; j++ )
					CG_RotateBonePose( tmpangles, &cent->ent.boneposes[pmodel->pmodelinfo->rotator[i][j]] );
			}
		}
	}

	// finish (mount) pose. Now it's the final skeleton just as it's drawn.
	CG_TransformBoneposes( cent->skel, cent->ent.boneposes, cent->ent.boneposes );

	// Vic: Hack in frame numbers to aid frustum culling
	cent->ent.backlerp = 1.0 - cg.lerpfrac;
	cent->ent.frame = pmodel->animState.frame[LOWER];
	cent->ent.oldframe = pmodel->animState.oldframe[LOWER];

	// Add playermodel ent
	cent->ent.scale = 1.0f;
	cent->ent.rtype = RT_MODEL;
	cent->ent.model = pmodel->pmodelinfo->model;
	cent->ent.customShader = NULL;
	cent->ent.customSkin = pmodel->skin;
	cent->ent.renderfx |= RF_NOSHADOW;

	if( !( cent->effects & EF_RACEGHOST ) ) {
		CG_AddCentityOutLineEffect( cent, viewState );
		CG_AddEntityToScene( &cent->ent, drawSceneRequest );
	}

	if( !cent->ent.model ) {
		return;
	}

	CG_PModel_AddFlag( cent, drawSceneRequest, viewState );

	CG_AddShellEffects( &cent->ent, cent->effects, drawSceneRequest );

	CG_AddHeadIcon( cent, drawSceneRequest );

	if( cent->localEffects[LOCALEFFECT_EV_PLAYER_TELEPORT_IN] | cent->localEffects[LOCALEFFECT_EV_PLAYER_TELEPORT_OUT] ) {
		vec3_t color;
		if( viewState->isViewerEntity( cent->current.number ) ) {
			VectorSet( color, 0.1f, 0.1f, 0.1f );
		} else {
			VectorSet( color, 0.5f, 0.5f, 0.5f );
			if( v_raceGhosts.get() && GS_RaceGametype( *cggs ) ) {
				const float scale = v_raceGhostsAlpha.get();
				assert( scale >= 0.0f && scale <= 1.0f );
				VectorScale( color, scale, color );
			}
		}
		TeleEffectParams params {
			.axis      = cent->ent.axis,
			.model     = cent->ent.model,
			.skel      = cent->skel,
			.colorRgb  = color,
			.animFrame = cent->ent.frame,
		};
		for( int ev = LOCALEFFECT_EV_PLAYER_TELEPORT_IN; ev <= LOCALEFFECT_EV_PLAYER_TELEPORT_OUT; ++ev ) {
			if( cent->localEffects[ev] ) {
				cent->localEffects[ev] = 0;
				if( ev == LOCALEFFECT_EV_PLAYER_TELEPORT_IN ) {
					params.origin = cent->teleportedTo;
					cg.effectsSystem.spawnPlayerTeleInEffect( cent->current.number, cg.time, params );
				} else {
					params.origin = cent->teleportedFrom;
					cg.effectsSystem.spawnPlayerTeleOutEffect( cent->current.number, cg.time, params );
				}
			}
		}
	}

	if( !cent->current.weapon ) {
		return;
	}

	if( !CG_GrabTag( &tag_weapon, &cent->ent, "tag_weapon" ) ) {
		return;
	}

	const bool addCoronaLight = !viewState->isViewerEntity( cent->current.number ) || viewState->view.thirdperson;

	// add weapon model
	CG_AddWeaponOnTag( &cent->ent, &tag_weapon, cent->current.weapon, cent->effects, addCoronaLight,
		&pmodel->projectionSource, pmodel->flash_time, pmodel->barrel_time, drawSceneRequest, viewState );
}

#define MOVEDIREPSILON  0.3f
#define WALKEPSILON 5.0f
#define RUNEPSILON  220.0f

// movement flags for animation control
#define ANIMMOVE_FRONT      0x00000001  //	Player is pressing fordward
#define ANIMMOVE_BACK       0x00000002  //	Player is pressing backpedal
#define ANIMMOVE_LEFT       0x00000004  //	Player is pressing sideleft
#define ANIMMOVE_RIGHT      0x00000008  //	Player is pressing sideright
#define ANIMMOVE_WALK       0x00000010  //	Player is pressing the walk key
#define ANIMMOVE_RUN        0x00000020  //	Player is running
#define ANIMMOVE_DUCK       0x00000040  //	Player is crouching
#define ANIMMOVE_SWIM       0x00000080  //	Player is swimming
#define ANIMMOVE_AIR        0x00000100  //	Player is at air, but not jumping

typedef struct {
	int moveflags;              //moving direction
	int animState[PMODEL_PARTS];
} pm_anim_t;

/*
* GS_SetBaseAnimUpper
*/
static void GS_SetBaseAnimUpper( pm_anim_t *pmanim, int carried_weapon ) {
	//SWIMMING
	if( pmanim->moveflags & ANIMMOVE_SWIM ) {
		pmanim->animState[UPPER] = TORSO_SWIM;
	} else {
		switch( carried_weapon ) {
			case WEAP_NONE:
				pmanim->animState[UPPER] = TORSO_HOLD_BLADE; // fixme: a special animation should exist
				break;
			case WEAP_GUNBLADE:
				pmanim->animState[UPPER] =  TORSO_HOLD_BLADE;
				break;
			case WEAP_LASERGUN:
				pmanim->animState[UPPER] =  TORSO_HOLD_PISTOL;
				break;
			default:
			case WEAP_RIOTGUN:
			case WEAP_PLASMAGUN:
				pmanim->animState[UPPER] =  TORSO_HOLD_LIGHTWEAPON;
				break;
			case WEAP_ROCKETLAUNCHER:
			case WEAP_GRENADELAUNCHER:
				pmanim->animState[UPPER] =  TORSO_HOLD_HEAVYWEAPON;
				break;
			case WEAP_ELECTROBOLT:
				pmanim->animState[UPPER] =  TORSO_HOLD_AIMWEAPON;
				break;
		}
	}
}

/*
* GS_SetBaseAnimLower
*/
static void GS_SetBaseAnimLower( pm_anim_t *pmanim ) {
	//SWIMMING
	if( pmanim->moveflags & ANIMMOVE_SWIM ) {
		if( pmanim->moveflags & ANIMMOVE_FRONT ) {
			pmanim->animState[LOWER] = LEGS_SWIM_FORWARD;
		} else {
			pmanim->animState[LOWER] = LEGS_SWIM_NEUTRAL;
		}
	}
		//CROUCH
	else if( pmanim->moveflags & ANIMMOVE_DUCK ) {
		if( pmanim->moveflags & ( ANIMMOVE_WALK | ANIMMOVE_RUN ) ) {
			pmanim->animState[LOWER] = LEGS_CROUCH_WALK;
		} else {
			pmanim->animState[LOWER] = LEGS_CROUCH_IDLE;
		}
	}
		//FALLING
	else if( pmanim->moveflags & ANIMMOVE_AIR ) {
		pmanim->animState[LOWER] = LEGS_JUMP_NEUTRAL;
	}
		// RUN
	else if( pmanim->moveflags & ANIMMOVE_RUN ) {
		//front/backward has priority over side movements
		if( pmanim->moveflags & ANIMMOVE_FRONT ) {
			pmanim->animState[LOWER] = LEGS_RUN_FORWARD;

		} else if( pmanim->moveflags & ANIMMOVE_BACK ) {
			pmanim->animState[LOWER] = LEGS_RUN_BACK;

		} else if( pmanim->moveflags & ANIMMOVE_RIGHT ) {
			pmanim->animState[LOWER] = LEGS_RUN_RIGHT;

		} else if( pmanim->moveflags & ANIMMOVE_LEFT ) {
			pmanim->animState[LOWER] = LEGS_RUN_LEFT;

		} else {   //is moving by inertia
			pmanim->animState[LOWER] = LEGS_WALK_FORWARD;
		}
	}
		//WALK
	else if( pmanim->moveflags & ANIMMOVE_WALK ) {
		//front/backward has priority over side movements
		if( pmanim->moveflags & ANIMMOVE_FRONT ) {
			pmanim->animState[LOWER] = LEGS_WALK_FORWARD;

		} else if( pmanim->moveflags & ANIMMOVE_BACK ) {
			pmanim->animState[LOWER] = LEGS_WALK_BACK;

		} else if( pmanim->moveflags & ANIMMOVE_RIGHT ) {
			pmanim->animState[LOWER] = LEGS_WALK_RIGHT;

		} else if( pmanim->moveflags & ANIMMOVE_LEFT ) {
			pmanim->animState[LOWER] = LEGS_WALK_LEFT;

		} else {   //is moving by inertia
			pmanim->animState[LOWER] = LEGS_WALK_FORWARD;
		}
	} else {   // STAND
		pmanim->animState[LOWER] = LEGS_STAND_IDLE;
	}
}

/*
* GS_SetBaseAnims
*/
static void GS_SetBaseAnims( pm_anim_t *pmanim, int carried_weapon ) {
	int part;

	for( part = 0; part < PMODEL_PARTS; part++ ) {
		switch( part ) {
			case LOWER:
				GS_SetBaseAnimLower( pmanim );
				break;

			case UPPER:
				GS_SetBaseAnimUpper( pmanim, carried_weapon );
				break;

			case HEAD:
			default:
				pmanim->animState[part] = 0;
				break;
		}
	}
}

/*
* GS_UpdateBaseAnims
*/
int GS_UpdateBaseAnims( entity_state_t *state, vec3_t velocity ) {
	pm_anim_t pmanim;
	vec3_t movedir;
	vec3_t hvel;
	mat3_t viewaxis;
	float xyspeedcheck;
	int waterlevel;
	vec3_t mins, maxs;
	vec3_t point;
	trace_t trace;

	if( !state ) {
		cggs->Error( "GS_UpdateBaseAnims: NULL state\n" );
		return 0;
	}

	GS_BBoxForEntityState( cggs, state, mins, maxs );

	memset( &pmanim, 0, sizeof( pm_anim_t ) );

	// determine if player is at ground, for walking or falling
	// this is not like having groundEntity, we are more generous with
	// the tracing size here to include small steps
	point[0] = state->origin[0];
	point[1] = state->origin[1];
	point[2] = state->origin[2] - ( 1.6 * STEPSIZE );
	cggs->Trace( &trace, state->origin, mins, maxs, point, state->number, MASK_PLAYERSOLID, 0 );
	if( trace.ent == -1 || ( trace.fraction < 1.0f && !ISWALKABLEPLANE( &trace.plane ) && !trace.startsolid ) ) {
		pmanim.moveflags |= ANIMMOVE_AIR;
	}

	// crouching : fixme? : it assumes the entity is using the player box sizes
	if( VectorCompare( maxs, playerbox_crouch_maxs ) ) {
		pmanim.moveflags |= ANIMMOVE_DUCK;
	}

	// find out the water level
	waterlevel = GS_WaterLevel( cggs, state, mins, maxs );
	if( waterlevel >= 2 || ( waterlevel && ( pmanim.moveflags & ANIMMOVE_AIR ) ) ) {
		pmanim.moveflags |= ANIMMOVE_SWIM;
	}

	//find out what are the base movements the model is doing

	hvel[0] = velocity[0];
	hvel[1] = velocity[1];
	hvel[2] = 0;
	xyspeedcheck = VectorNormalize2( hvel, movedir );
	if( xyspeedcheck > WALKEPSILON ) {
		VectorNormalizeFast( movedir );
		Matrix3_FromAngles( tv( 0, state->angles[YAW], 0 ), viewaxis );

		// if it's moving to where is looking, it's moving forward
		if( DotProduct( movedir, &viewaxis[AXIS_RIGHT] ) > MOVEDIREPSILON ) {
			pmanim.moveflags |= ANIMMOVE_RIGHT;
		} else if( -DotProduct( movedir, &viewaxis[AXIS_RIGHT] ) > MOVEDIREPSILON ) {
			pmanim.moveflags |= ANIMMOVE_LEFT;
		}
		if( DotProduct( movedir, &viewaxis[AXIS_FORWARD] ) > MOVEDIREPSILON ) {
			pmanim.moveflags |= ANIMMOVE_FRONT;
		} else if( -DotProduct( movedir, &viewaxis[AXIS_FORWARD] ) > MOVEDIREPSILON ) {
			pmanim.moveflags |= ANIMMOVE_BACK;
		}

		if( xyspeedcheck > RUNEPSILON ) {
			pmanim.moveflags |= ANIMMOVE_RUN;
		} else if( xyspeedcheck > WALKEPSILON ) {
			pmanim.moveflags |= ANIMMOVE_WALK;
		}
	}

	GS_SetBaseAnims( &pmanim, state->weapon );
	return ( ( pmanim.animState[LOWER] & 0x3F ) | ( pmanim.animState[UPPER] & 0x3F ) << 6 | ( pmanim.animState[HEAD] & 0xF ) << 12 );
}

#undef MOVEDIREPSILON
#undef WALKEPSILON
#undef RUNEPSILON

/*
*GS_PModel_AnimToFrame
*
* BASE_CHANEL plays continuous animations forced to loop.
* if the same animation is received twice it will *not* restart
* but continue looping.
*
* EVENT_CHANNEL overrides base channel and plays until
* the animation is finished. Then it returns to base channel.
* If an animation is received twice, it will be restarted.
* If an event channel animation has a loop setting, it will
* continue playing it until a new event chanel animation
* is fired.
*/
void GS_PModel_AnimToFrame( int64_t curTime, gs_pmodel_animationset_t *animSet, gs_pmodel_animationstate_t *anim ) {
	int i, channel = BASE_CHANNEL;

	for( i = LOWER; i < PMODEL_PARTS; i++ ) {
		for( channel = BASE_CHANNEL; channel < PLAYERANIM_CHANNELS; channel++ ) {
			gs_animstate_t *thisAnim = &anim->curAnims[i][channel];

			// see if there are new animations to be played
			if( anim->buffer[channel].newanim[i] != ANIM_NONE ) {
				if( channel == EVENT_CHANNEL ||
					( channel == BASE_CHANNEL && anim->buffer[channel].newanim[i] != thisAnim->anim ) ) {
					thisAnim->anim = anim->buffer[channel].newanim[i];
					thisAnim->startTimestamp = curTime;
				}

				anim->buffer[channel].newanim[i] = ANIM_NONE;
			}

			if( thisAnim->anim ) {
				bool forceLoop = ( channel == BASE_CHANNEL ) ? true : false;

				thisAnim->lerpFrac = GS_FrameForTime( &thisAnim->frame, curTime, thisAnim->startTimestamp,
													  animSet->frametime[thisAnim->anim], animSet->firstframe[thisAnim->anim], animSet->lastframe[thisAnim->anim],
													  animSet->loopingframes[thisAnim->anim], forceLoop );

				// the animation was completed
				if( thisAnim->frame < 0 ) {
					assert( channel != BASE_CHANNEL );
					thisAnim->anim = ANIM_NONE;
				}
			}
		}
	}

	// we set all animations up, but now select which ones are going to be shown
	for( i = LOWER; i < PMODEL_PARTS; i++ ) {
		int lastframe = anim->frame[i];
		channel = ( anim->curAnims[i][EVENT_CHANNEL].anim != ANIM_NONE ) ? EVENT_CHANNEL : BASE_CHANNEL;
		anim->frame[i] = anim->curAnims[i][channel].frame;
		anim->lerpFrac[i] = anim->curAnims[i][channel].lerpFrac;
		if( !lastframe || !anim->oldframe[i] ) {
			anim->oldframe[i] = anim->frame[i];
		} else if( anim->frame[i] != lastframe ) {
			anim->oldframe[i] = lastframe;
		}
	}
}

/*
* GS_PModel_ClearEventAnimations
*/
void GS_PlayerModel_ClearEventAnimations( gs_pmodel_animationset_t *animSet, gs_pmodel_animationstate_t *animState ) {
	int i;

	for( i = LOWER; i < PMODEL_PARTS; i++ ) {
		animState->buffer[EVENT_CHANNEL].newanim[i] = ANIM_NONE;
		animState->curAnims[i][EVENT_CHANNEL].anim = ANIM_NONE;
	}
}

/*
* GS_PModel_AddAnimation
*/
void GS_PlayerModel_AddAnimation( gs_pmodel_animationstate_t *animState, int loweranim, int upperanim, int headanim, int channel ) {
	int i;
	int newanim[PMODEL_PARTS];
	gs_animationbuffer_t *buffer;

	assert( animState != NULL );

	newanim[LOWER] = loweranim;
	newanim[UPPER] = upperanim;
	newanim[HEAD] = headanim;

	buffer = &animState->buffer[channel];

	for( i = LOWER; i < PMODEL_PARTS; i++ ) {
		//ignore new events if in death
		if( channel && buffer->newanim[i] && ( buffer->newanim[i] <= BOTH_DEAD1 ) ) {
			continue;
		}

		if( newanim[i] && ( newanim[i] < PMODEL_TOTAL_ANIMATIONS ) ) {
			buffer->newanim[i] = newanim[i];
		}
	}
}
