/*
Copyright (C) 1997-2001 Id Software, Inc.

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

#include "cg_local.h"
#include <client/ui/uisystem.h>
#include <common/facilities/fscompat.h>
#include <common/helpers/userinfo.h>

static const char *cg_defaultSexedSounds[] =
{
	"*death", //"*death2", "*death3", "*death4",
	"*fall_0", "*fall_1", "*fall_2",
	"*falldeath",
	"*gasp", "*drown",
	"*jump_1", "*jump_2",
	"*pain25", "*pain50", "*pain75", "*pain100",
	"*wj_1", "*wj_2",
	"*dash_1", "*dash_2",
	"*taunt",
	NULL
};


/*
* CG_RegisterPmodelSexedSound
*/
static const SoundSet *CG_RegisterPmodelSexedSound( pmodelinfo_t *pmodelinfo, const char *name ) {
	char *p, *s, model[MAX_QPATH];
	cg_sexedSfx_t *sexedSfx;
	char oname[MAX_QPATH];
	char sexedFilename[MAX_QPATH];

	if( !pmodelinfo ) {
		return NULL;
	}

	model[0] = '\0';

	Q_strncpyz( oname, name, sizeof( oname ) );
	COM_StripExtension( oname );
	for( sexedSfx = pmodelinfo->sexedSfx; sexedSfx; sexedSfx = sexedSfx->next ) {
		if( !Q_stricmp( sexedSfx->name, oname ) ) {
			return sexedSfx->sfx;
		}
	}

	// find out what's the model name
	s = pmodelinfo->name;
	if( s[0] ) {
		p = strchr( s, '/' );
		if( p ) {
			s = p + 1;
			p = strchr( s, '/' );
			if( p ) {
				Q_strncpyz( model, p + 1, sizeof( model ) );
				p = strchr( model, '/' );
				if( p ) {
					*p = 0;
				}
			}
		}
	}

	// if we can't figure it out, they're DEFAULT_PLAYERMODEL
	if( !model[0] ) {
		Q_strncpyz( model, DEFAULT_PLAYERMODEL, sizeof( model ) );
	}

	sexedSfx = ( cg_sexedSfx_t * )Q_malloc( sizeof( cg_sexedSfx_t ) );
	sexedSfx->name = Q_strdup( oname );
	sexedSfx->next = pmodelinfo->sexedSfx;
	pmodelinfo->sexedSfx = sexedSfx;

	// see if we already know of the model specific sound
	Q_snprintfz( sexedFilename, sizeof( sexedFilename ), "sounds/players/%s/%s", model, oname + 1 );

	SoundSystem *soundSystem = SoundSystem::instance();
	if( ( !COM_FileExtension( sexedFilename ) &&
		FS_FirstExtension( sexedFilename, SOUND_EXTENSIONS, std::size( SOUND_EXTENSIONS ) ) ) ||
		FS_FOpenFile( sexedFilename, NULL, FS_READ ) != -1 ) {
		sexedSfx->sfx = soundSystem->registerSound( { .name = SoundSetProps::Exact { wsw::StringView( sexedFilename ) } } );
	} else {   // no, revert to default player sounds folders
		if( pmodelinfo->sex == GENDER_FEMALE ) {
			Q_snprintfz( sexedFilename, sizeof( sexedFilename ), "sounds/players/%s/%s", "female", oname + 1 );
			sexedSfx->sfx = soundSystem->registerSound( { .name = SoundSetProps::Exact { wsw::StringView( sexedFilename ) } } );
		} else {
			Q_snprintfz( sexedFilename, sizeof( sexedFilename ), "sounds/players/%s/%s", "male", oname + 1 );
			sexedSfx->sfx = soundSystem->registerSound( { .name = SoundSetProps::Exact { wsw::StringView( sexedFilename ) } } );
		}
	}

	return sexedSfx->sfx;
}

/*
* CG_UpdateSexedSoundsRegistration
*/
void CG_UpdateSexedSoundsRegistration( pmodelinfo_t *pmodelinfo ) {
	cg_sexedSfx_t *sexedSfx, *next;

	if( !pmodelinfo ) {
		return;
	}

	// free loaded sounds
	for( sexedSfx = pmodelinfo->sexedSfx; sexedSfx; sexedSfx = next ) {
		next = sexedSfx->next;
		Q_free( sexedSfx->name );
		Q_free( sexedSfx );
	}
	pmodelinfo->sexedSfx = NULL;

	// load default sounds
	for( unsigned i = 0;; i++ ) {
		const char *name = cg_defaultSexedSounds[i];
		if( !name ) {
			break;
		}
		CG_RegisterPmodelSexedSound( pmodelinfo, name );
	}

	// load sounds server told us
	for( unsigned i = 1; i < MAX_SOUNDS; i++ ) {
		auto maybeConfigString = cgs.configStrings.get( i );
		if( !maybeConfigString ) {
			break;
		}
		auto string = *maybeConfigString;
		if( string.startsWith( '*' ) ) {
			CG_RegisterPmodelSexedSound( pmodelinfo, string.data() );
		}
	}
}

/*
* CG_RegisterSexedSound
*/
const SoundSet *CG_RegisterSexedSound( int entnum, const char *name ) {
	if( entnum < 0 || entnum >= MAX_EDICTS ) {
		return NULL;
	}
	return CG_RegisterPmodelSexedSound( cg_entPModels[entnum].pmodelinfo, name );
}

void playSexedSoundInPrimaryView( int entnum, int attachmentTag, int entchannel, const char *name, float fvol, float attn ) {
	if( const ViewState *primaryViewState = getPrimaryViewState(); primaryViewState->allowSounds ) {
		const bool fixed = ( entchannel & CHAN_FIXED ) != 0;
		entchannel &= ~CHAN_FIXED;
		const SoundSet *soundSet = CG_RegisterSexedSound( entnum, name );
		if( fixed ) {
			SoundSystem::instance()->startFixedSound( soundSet, cg_entities[entnum].current.origin, entchannel, fvol, attn );
		} else {
			SoundSystem::instance()->startRelativeSound( soundSet, (SoundSystem::AttachmentTag)attachmentTag, entnum, entchannel, fvol, attn );
		}
	}
}

static void CG_LoadClientInfo( unsigned clientNum, const char *info ) {
	cg_clientInfo_t *const ci = &cgs.clientInfo[clientNum];

	if( !Info_Validate( info ) ) {
		CG_Error( "Invalid client info" );
	}

	const char *s = Info_ValueForKey( info, "name" );
	Q_strncpyz( ci->name, s && s[0] ? s : "badname", sizeof( ci->name ) );

	s = Info_ValueForKey( info, "clan" );
	Q_strncpyz( ci->clan, s && s[0] ? s : "", sizeof( ci->clan ) );

	// name with color tokes stripped
	Q_strncpyz( ci->cleanname, COM_RemoveColorTokens( ci->name ), sizeof( ci->cleanname ) );
	Q_strncpyz( ci->cleanclan, COM_RemoveColorTokens( ci->clan ), sizeof( ci->cleanclan ) );

	s = Info_ValueForKey( info, "hand" );
	ci->hand = s && s[0] ? atoi( s ) : 2;

	// color
	s = Info_ValueForKey( info, "color" );
	const int rgbcolor = s && s[0] ? COM_ReadColorRGBString( s ) : -1;
	if( rgbcolor != -1 ) {
		Vector4Set( ci->color, COLOR_R( rgbcolor ), COLOR_G( rgbcolor ), COLOR_B( rgbcolor ), 255 );
	} else {
		Vector4Set( ci->color, 255, 255, 255, 255 );
	}

	wsw::ui::UISystem::instance()->handleClientInfoChanges( clientNum );
}

void CG_LoadClientInfo( unsigned clientNum, const wsw::StringView &info ) {
	assert( info.isZeroTerminated() );
	CG_LoadClientInfo( clientNum, info.data() );
}

/*
* CG_ResetClientInfos
*/
void CG_ResetClientInfos( void ) {
	// TODO: Should we care of resetting the respective data in UI?
	// Note: If yes, we have to reset it upon destruction as well.
	memset( cgs.clientInfo, 0, sizeof( cgs.clientInfo ) );

	for( unsigned clientNum = 0; clientNum < MAX_CLIENTS; ++clientNum ) {
		if( const std::optional<wsw::StringView> maybeInfoString = cgs.configStrings.getPlayerInfo( clientNum ) ) {
			CG_LoadClientInfo( clientNum, *maybeInfoString );
		}
	}
}