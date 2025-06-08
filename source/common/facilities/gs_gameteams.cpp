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

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/q_shared.h>
#include "q_comref.h"
#include "q_collision.h"

#include "gs_public.h"
#include <common/types/stringview.h>

//==================================================
//
//		TEAMS
//
//==================================================

static const char *gs_teamNames[] = {
	"SPECTATOR",
	"PLAYERS",
	"ALPHA",
	"BETA",
	NULL
};

static const char *gs_teamSkinsNames[] = {
	NULL,   //null means user defined skin
	NULL,
	"default",
	"default",
	NULL
};

/*
* GS_TeamName
*/
const char *GS_TeamName( const gs_state_t *gs, int team ) {
	if( team < 0 || team >= GS_MAX_TEAMS ) {
		return NULL;
	}
	return gs->GetConfigString( CS_TEAM_SPECTATOR_NAME + team );
}

const char *GS_DefaultTeamName( const gs_state_t *, int team ) {
	if( team < 0 || team >= GS_MAX_TEAMS ) {
		return NULL;
	}
	return gs_teamNames[team];
}

auto GS_TeamSkinName( const gs_state_t *, int team ) -> std::optional<wsw::StringView> {
	if( team >= 0 && team < GS_MAX_TEAMS ) {
		if( const char *name = gs_teamSkinsNames[team] ) {
			return wsw::StringView( name );
		}
	}
	return std::nullopt;
}

/*
* GS_Teams_TeamFromName
*/
int GS_Teams_TeamFromName( const gs_state_t *gs, const char *teamname ) {
	const char *s;
	int i;

	if( !teamname || !teamname[0] ) {
		return -1; // invalid

	}
	for( i = 0; i < GS_MAX_TEAMS; i++ ) {
		s = gs_teamNames[i];
		if( !Q_stricmp( s, teamname ) ) {
			return i;
		}

		s = gs->GetConfigString( CS_TEAM_SPECTATOR_NAME + i );
		if( s && !Q_stricmp( s, teamname ) ) {
			return i;
		}
	}

	return -1; // invalid
}

/*
* GS_IsTeamDamage
*/
bool GS_IsTeamDamage( const gs_state_t *gs, const entity_state_t *targ, const entity_state_t *attacker ) {
	if( !GS_TeamBasedGametype( *gs ) ) {
		return false;
	}

	assert( targ && attacker );

	if( targ->team && attacker->team &&
		targ->team == attacker->team &&
		targ->number != attacker->number ) {
		return true;
	}

	return false;
}
