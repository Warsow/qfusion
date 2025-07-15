/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2003 Victor Luchits
Copyright (C) 2007 Daniel Lindenfelser
Copyright (C) 2009 German Garcia Fernandez ("Jal")

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
#include <client/client.h>
#include <client/ui/uisystem.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/scopeexitaction.h>
#include <common/types/scopedresource.h>
#include <common/facilities/wswfs.h>
#include <common/facilities/profilerscope.h>
#include <common/facilities/configvars.h>
#include <common/facilities/sysclock.h>

using wsw::operator""_asView;
using wsw::operator""_asHView;

// TODO: ??? what to do with default names (they cannot be known without the FS access)?
// TODO: Let modify declared parameters on the fly?
static StringConfigVar v_crosshairName( "cg_crosshairName"_asView, { .byDefault = "default"_asView, .flags = CVAR_ARCHIVE } );
static StringConfigVar v_strongCrosshairName( "cg_strongCrosshairName"_asView, { .byDefault = "default"_asView, .flags = CVAR_ARCHIVE } );

static UnsignedConfigVar v_crosshairSize( "cg_crosshairSize"_asView, {
	.byDefault = kRegularCrosshairSizeProps.defaultSize,
	.min       = inclusive( kRegularCrosshairSizeProps.minSize ),
	.max       = inclusive( kRegularCrosshairSizeProps.maxSize ),
	.flags     = CVAR_ARCHIVE,
});

static UnsignedConfigVar v_strongCrosshairSize( "cg_strongCrosshairSize"_asView, {
	.byDefault = kStrongCrosshairSizeProps.defaultSize,
	.min       = inclusive( kStrongCrosshairSizeProps.minSize ),
	.max       = inclusive( kStrongCrosshairSizeProps.maxSize ),
	.flags     = CVAR_ARCHIVE,
});

static ColorConfigVar v_crosshairColor( "cg_crosshairColor"_asView, { .byDefault = -1, .flags = CVAR_ARCHIVE } );
static ColorConfigVar v_strongCrosshairColor( "cg_strongCrosshairColor"_asView, { .byDefault = -1, .flags = CVAR_ARCHIVE } );

static ColorConfigVar v_crosshairDamageColor( "cg_crosshairDamageColor"_asView, {
	.byDefault = COLOR_RGBA( 192, 0, 255, 0 ), .flags = CVAR_ARCHIVE
});
// The actual lower bound is limited by snapshot time delta
static UnsignedConfigVar v_crosshairDamageTime( "cg_crosshairDamageTime"_asView, {
	.byDefault = 100u, .min = exclusive( 0u ), .max = exclusive( 1000u ), .flags = CVAR_ARCHIVE
});

static BoolConfigVar v_separateWeaponCrosshairSettings( "cg_separateWeaponCrosshairSettings"_asView, {
	.byDefault = false, .flags = CVAR_ARCHIVE
});

struct CrosshairSettingsTracker {
	CrosshairSettingsTracker() {
		static_assert( WEAP_NONE == 0 && WEAP_GUNBLADE == 1 );
		constexpr const char *kWeaponNames[WEAP_TOTAL - 1] = {
			"GB", "MG", "RG", "GL", "RL", "PG", "LG", "EB", "SW", "IG"
		};

		wsw::StaticString<32> sizeVarNameBuffer( "cg_crosshairSize_"_asView );
		const unsigned sizePrefixSize = sizeVarNameBuffer.size();
		wsw::StaticString<32> colorVarNameBuffer( "cg_crosshairColor_"_asView );
		const unsigned colorPrefixSize = colorVarNameBuffer.size();
		wsw::StaticString<32> nameVarNameBuffer( "cg_crosshairName_"_asView );
		const unsigned namePrefixSize = nameVarNameBuffer.size();

		for( const char *shortName: kWeaponNames ) {
			sizeVarNameBuffer.erase( sizePrefixSize );
			sizeVarNameBuffer.append( shortName, 2 );
			m_nameDataStorage.add( sizeVarNameBuffer.asView() );
			colorVarNameBuffer.erase( colorPrefixSize );
			colorVarNameBuffer.append( shortName, 2 );
			m_nameDataStorage.add( colorVarNameBuffer.asView() );
			nameVarNameBuffer.erase( namePrefixSize );
			nameVarNameBuffer.append( shortName, 2 );
			m_nameDataStorage.add( nameVarNameBuffer.asView() );
		}

		m_nameDataStorage.shrink_to_fit();
		assert( m_nameDataStorage.size() == 3 * ( WEAP_TOTAL - 1 ) );

		for( unsigned i = 0; i < WEAP_TOTAL - 1; ++i ) {
			const wsw::StringView &sizeVarName  = m_nameDataStorage[3 * i + 0];
			const wsw::StringView &colorVarName = m_nameDataStorage[3 * i + 1];
			const wsw::StringView &valueVarName = m_nameDataStorage[3 * i + 2];

			// TODO: Mark constructors of these vars as nothrow as they definitely do not throw
			// (unsafe_grow_back() should not be used with throwing constructors)

			new( m_sizeVarsForWeapons.unsafe_grow_back() )UnsignedConfigVar( sizeVarName, {
				.byDefault = kRegularCrosshairSizeProps.defaultSize,
				.min      = inclusive( kRegularCrosshairSizeProps.minSize ),
				.max      = inclusive( kRegularCrosshairSizeProps.maxSize ),
				.flags    = CVAR_ARCHIVE,
			});
			new( m_colorVarsForWeapons.unsafe_grow_back() )ColorConfigVar( colorVarName, {
				.byDefault = -1, .flags = CVAR_ARCHIVE,
			});
			new( m_nameVarsForWeapons.unsafe_grow_back() )StringConfigVar( valueVarName, {
				.byDefault = "default"_asView, .flags = CVAR_ARCHIVE,
			});
		}
	};

	wsw::StaticVector<UnsignedConfigVar, WEAP_TOTAL - 1> m_sizeVarsForWeapons;
	wsw::StaticVector<ColorConfigVar, WEAP_TOTAL - 1> m_colorVarsForWeapons;
	wsw::StaticVector<StringConfigVar, WEAP_TOTAL - 1> m_nameVarsForWeapons;
	wsw::StringSpanStorage<unsigned, unsigned> m_nameDataStorage;
};

static CrosshairSettingsTracker crosshairSettingsTracker;

// TODO: Should it belong to the same place where prediction gets executed?
void CG_DemoCam_FreeFly() {
	assert( cgs.demoPlaying && cg.isDemoCamFree );
	float maxspeed = 250;

	// run frame
	usercmd_t cmd;
	NET_GetUserCmd( NET_GetCurrentUserCmdNum() - 1, &cmd );
	cmd.msec = cg.realFrameTime;

	vec3_t moveangles;
	for( int i = 0; i < 3; i++ ) {
		moveangles[i] = SHORT2ANGLE( cmd.angles[i] ) + SHORT2ANGLE( cg.demoFreeCamDeltaAngles[i] );
	}

	vec3_t forward, right;
	AngleVectors( moveangles, forward, right, nullptr );
	VectorCopy( moveangles, cg.demoFreeCamAngles );

	const float SPEED = 500;
	float fmove = cmd.forwardmove * SPEED / 127.0f;
	float smove = cmd.sidemove * SPEED / 127.0f;
	float upmove = cmd.upmove * SPEED / 127.0f;
	if( cmd.buttons & BUTTON_SPECIAL ) {
		maxspeed *= 2;
	}

	vec3_t wishvel;
	for( int i = 0; i < 3; i++ ) {
		wishvel[i] = forward[i] * fmove + right[i] * smove;
	}
	wishvel[2] += upmove;

	vec3_t wishdir;
	float wishspeed = VectorNormalize2( wishvel, wishdir );
	if( wishspeed > maxspeed ) {
		wishspeed = maxspeed / wishspeed;
		VectorScale( wishvel, wishspeed, wishvel );
		wishspeed = maxspeed;
	}

	VectorMA( cg.demoFreeCamOrigin, (float)cg.realFrameTime * 0.001f, wishvel, cg.demoFreeCamOrigin );
}


static void CG_DemoFreeFly_Cmd_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() > 1 ) {
		if( !Q_stricmp( Cmd_Argv( 1 ), "on" ) ) {
			cg.isDemoCamFree = true;
		} else if( !Q_stricmp( Cmd_Argv( 1 ), "off" ) ) {
			cg.isDemoCamFree = false;
		}
	} else {
		cg.isDemoCamFree = !cg.isDemoCamFree;
	}

	if( cg.isDemoCamFree ) {
		// Inherit position/angles from the previously followed POV
		const player_state_t &oldPlayerState = getPrimaryViewState()->predictedPlayerState;
		VectorCopy( oldPlayerState.pmove.origin, cg.demoFreeCamOrigin );
		cg.demoFreeCamOrigin[2] += oldPlayerState.viewheight;
		VectorCopy( oldPlayerState.viewangles, cg.demoFreeCamAngles );
		// CAUTION: Modifying view angles of the our own "physical" client (this is harmless though)
		VectorCopy( oldPlayerState.viewangles, cl.viewangles );
		VectorClear( cg.demoFreeCamVelocity );
		VectorClear( cg.demoFreeCamDeltaAngles );
	}
}

static void CG_CamSwitch_Cmd_f( const CmdArgs & ) {

}

void CG_DemocamInit() {
	if( cgs.demoPlaying ) {
		// add console commands
		CL_Cmd_Register( "demoFreeFly"_asView, CG_DemoFreeFly_Cmd_f );
		CL_Cmd_Register( "camswitch"_asView, CG_CamSwitch_Cmd_f );
	}
}

void CG_DemocamShutdown() {
	if( cgs.demoPlaying ) {
		// remove console commands
		CL_Cmd_Unregister( "demoFreeFly"_asView );
		CL_Cmd_Unregister( "camswitch"_asView );
	}
}

[[nodiscard]]
static bool doChaseStep( int step ) {
	// It's always PM_CHASECAM for demos (so far, checked for multipov frames)
	const auto ourActualMoveType = getOurClientViewState()->predictedPlayerState.pmove.pm_type;
	if( ourActualMoveType == PM_SPECTATOR || ourActualMoveType == PM_CHASECAM ) {
		if( cg.frame.multipov ) {
			if( ( !cgs.demoPlaying || !cg.isDemoCamFree ) && cg.chaseMode != CAM_TILED ) {
				const std::optional<unsigned> existingIndex = CG_FindChaseableViewportForPlayernum( cg.chasedPlayerNum );

				std::optional<unsigned> chosenPlayerNum;

				// the POV was lost, find the closer one (may go up or down, but who cares)
				// TODO: Is it going to happen for new MV code?
				if( existingIndex == std::nullopt ) {
					if( const auto maybeIndexAndPlayerNum = CG_FindMultiviewPovToChase() ) {
						chosenPlayerNum = maybeIndexAndPlayerNum->second;
					}
				} else {
					int testedViewStateIndex = (int)existingIndex.value_or( std::numeric_limits<unsigned>::max() );
					for( int i = 0; i < cg.frame.numplayers; i++ ) {
						testedViewStateIndex += step;
						if( testedViewStateIndex < 0 ) {
							testedViewStateIndex = (int)cg.numSnapViewStates;
						} else if( testedViewStateIndex >= cg.frame.numplayers ) {
							testedViewStateIndex = 0;
						}
						// TODO: Are specs even included in snapshots?
						if( testedViewStateIndex == existingIndex ) {
							break;
						}
						if( cg.viewStates[testedViewStateIndex].canBeAMultiviewChaseTarget() ) {
							break;
						}
					}
					if( testedViewStateIndex != existingIndex ) {
						const ViewState &chosenViewState = cg.viewStates[testedViewStateIndex];
						assert( chosenViewState.canBeAMultiviewChaseTarget() );
						chosenPlayerNum = chosenViewState.predictedPlayerState.playerNum;
					}
				}

				if( !chosenPlayerNum ) {
					return false;
				}

				cg.pendingChasedPlayerNum = chosenPlayerNum;
				return true;
			}
		} else {
			if( !cgs.demoPlaying ) {
				CL_Cmd_ExecuteNow( step > 0 ? "chasenext" : "chaseprev" );
				return true;
			}
		}
	}

	return false;
}

static constexpr int64_t kChaseSwitchCooldown = 250;

/*
* CG_ChaseStep
*
* Returns whether the POV was actually requested to be changed.
*/
bool CG_ChaseStep( int step ) {
	if( cg.chaseSwitchTimestamp + kChaseSwitchCooldown < cg.time ) {
		if( doChaseStep( step ) ) {
			cg.chaseSwitchTimestamp = cg.time;
			cg.uiSystem->playForwardSound();
			return true;
		}
	}
	return false;
}

static bool postmatchsilence_set = false, demostream = false, background = false;
static unsigned lastSecond = 0;

static int oldState = -1;
static int oldAlphaScore, oldBetaScore;
static bool scoresSet = false;

// We have to reset this stuff manually, as it's no longer reset upon reloading the DLL
void CG_ShutdownSoundAndMiscState() {
	postmatchsilence_set = false;
	demostream = false;
	background = false;
	oldState = -1;
	oldAlphaScore = 0;
	oldBetaScore = 0;
	scoresSet = false;
}

static void CG_AddLocalSounds() {
	// Note: Most announcements are currently pov-dependent, so we should not play them in tiled mode.
	// See also handling of announcements while processing player state events.
	if( GS_Countdown( *cggs ) && getPrimaryViewState()->allowSounds ) {
		if( GS_MatchDuration( *cggs ) ) {
			const int64_t curtime = GS_MatchPaused( *cggs ) ? cg.frame.serverTime : cg.time;
			int64_t duration = GS_MatchDuration( *cggs );

			if( duration + GS_MatchStartTime( *cggs ) < curtime ) {
				duration = curtime - GS_MatchStartTime( *cggs ); // avoid negative results
			}

			auto seconds = (float)( GS_MatchStartTime( *cggs ) + duration - curtime ) * 0.001f;
			auto remainingSeconds = (unsigned)seconds;

			if( remainingSeconds != lastSecond ) {
				if( 1 + remainingSeconds < 4 ) {
					const wsw::StringView exactName( va( S_ANNOUNCER_COUNTDOWN_COUNT_1_to_3_SET_1_to_2, 1 + remainingSeconds, 1 ) );
					const SoundSet *sound = cg.soundSystem->registerSound( SoundSetProps {
						.name = SoundSetProps::Exact { exactName },
					});
					CG_AddAnnouncerEvent( sound, false );
				}

				lastSecond = remainingSeconds;
			}
		}
	} else {
		lastSecond = 0;
	}

	// add sounds from announcer
	CG_ReleaseAnnouncerEvents();

	// Stop background music in postmatch state
	if( GS_MatchState( *cggs ) >= MATCH_STATE_POSTMATCH ) {
		if( !postmatchsilence_set && !demostream ) {
			cg.soundSystem->stopBackgroundTrack();
			postmatchsilence_set = true;
			background = false;
		}
	} else {
		if( cgs.demoPlaying && cgs.demoAudioStream && !demostream ) {
			cg.soundSystem->startBackgroundTrack( cgs.demoAudioStream, NULL, 0 );
			demostream = true;
		}

		if( postmatchsilence_set ) {
			postmatchsilence_set = false;
			background = false;
		}

		if( ( !postmatchsilence_set && !demostream ) && !background ) {
			CG_StartBackgroundTrack();
			background = true;
		}
	}
}

/*
* CG_FlashGameWindow
*
* Flashes game window in case of important events (match state changes, etc) for user to notice
*/
static void CG_FlashGameWindow() {
	bool flash = false;

	// notify player of important match states
	const int newState = GS_MatchState( *cggs );
	if( oldState != newState ) {
		switch( newState ) {
			case MATCH_STATE_COUNTDOWN:
			case MATCH_STATE_PLAYTIME:
			case MATCH_STATE_POSTMATCH:
				flash = true;
				break;
			default:
				break;
		}

		oldState = newState;
	}

	const auto *const stats = getPrimaryViewState()->predictedPlayerState.stats;
	// notify player of teams scoring in team-based gametypes
	if( !scoresSet || ( oldAlphaScore != stats[STAT_TEAM_ALPHA_SCORE] || oldBetaScore != stats[STAT_TEAM_BETA_SCORE] ) ) {
		oldAlphaScore = stats[STAT_TEAM_ALPHA_SCORE];
		oldBetaScore  = stats[STAT_TEAM_BETA_SCORE];

		flash = scoresSet && GS_TeamBasedGametype( *cggs ) && !GS_IndividualGametype( *cggs );
		scoresSet = true;
	}

	if( flash ) {
		VID_FlashWindow( v_flashWindowCount.get() );
	}
}

/*
* CG_GetSensitivityScale
* Scale sensitivity for different view effects
*/
float CG_GetSensitivityScale( float sens, float zoomSens ) {
	float sensScale = 1.0f;

	if( !cgs.demoPlaying && sens != 0.0f && ( getPrimaryViewState()->predictedPlayerState.pmove.stats[PM_STAT_ZOOMTIME] > 0 ) ) {
		if( zoomSens != 0.0f ) {
			return zoomSens / sens;
		}

		return v_zoomfov.get() / v_fov.get();
	}

	return sensScale;
}

void CG_AddKickAngles( float *viewangles, ViewState *viewState ) {
	for( int i = 0; i < MAX_ANGLES_KICKS; i++ ) {
		if( cg.time <= viewState->kickangles[i].timestamp + viewState->kickangles[i].kicktime ) {
			const float time   = (float)( ( viewState->kickangles[i].timestamp + viewState->kickangles[i].kicktime ) - cg.time );
			const float uptime = ( (float)viewState->kickangles[i].kicktime ) * 0.5f;

			float delta = 1.0f - ( fabs( time - uptime ) / uptime );
			//Com_Printf("Kick Delta:%f\n", delta );
			if( delta > 1.0f ) {
				delta = 1.0f;
			}
			if( delta > 0.0f ) {
				viewangles[PITCH] += viewState->kickangles[i].v_pitch * delta;
				viewangles[ROLL] += viewState->kickangles[i].v_roll * delta;
			}
		}
	}
}

static float CG_CalcViewFov() {
	const float fov      = v_fov.get();
	const float zoomtime = getPrimaryViewState()->predictedPlayerState.pmove.stats[PM_STAT_ZOOMTIME];
	if( zoomtime <= 0.0f ) {
		return fov;
	}
	const float zoomfov = v_zoomfov.get();
	return std::lerp( fov, zoomfov, zoomtime / (float)ZOOMTIME );
}

static void CG_CalcViewBob( ViewState *viewState ) {
	if( !viewState->view.drawWeapon ) {
		return;
	}

	// calculate speed and cycle to be used for all cyclic walking effects
	viewState->xyspeed = sqrt( viewState->predictedPlayerState.pmove.velocity[0] * viewState->predictedPlayerState.pmove.velocity[0] + viewState->predictedPlayerState.pmove.velocity[1] * viewState->predictedPlayerState.pmove.velocity[1] );

	float bobScale = 0.0f;
	if( viewState->xyspeed < 5 ) {
		viewState->oldBobTime = 0;  // start at beginning of cycle again
	} else if( v_gunBob.get() ) {
		if( !viewState->isViewerEntity( viewState->view.POVent ) ) {
			bobScale = 0.0f;
		} else if( CG_PointContents( viewState->view.origin ) & MASK_WATER ) {
			bobScale =  0.75f;
		} else {
			centity_t *cent;
			vec3_t mins, maxs;
			trace_t trace;

			cent = &cg_entities[viewState->view.POVent];
			GS_BBoxForEntityState( cggs, &cent->current, mins, maxs );
			maxs[2] = mins[2];
			mins[2] -= ( 1.6f * STEPSIZE );

			CG_Trace( &trace, viewState->predictedPlayerState.pmove.origin, mins, maxs, viewState->predictedPlayerState.pmove.origin, viewState->view.POVent, MASK_PLAYERSOLID );
			if( trace.startsolid || trace.allsolid ) {
				if( viewState->predictedPlayerState.pmove.stats[PM_STAT_CROUCHTIME] ) {
					bobScale = 1.5f;
				} else {
					bobScale = 2.5f;
				}
			}
		}
	}

	const float bobMove = cg.frameTime * bobScale * 0.001f;
	const float bobTime = ( viewState->oldBobTime += bobMove );

	viewState->bobCycle = (int)bobTime;
	viewState->bobFracSin = fabs( sin( bobTime * M_PI ) );
}

void CG_ResetKickAngles( ViewState *viewState ) {
	memset( viewState->kickangles, 0, sizeof( viewState->kickangles ) );
}

void CG_StartKickAnglesEffect( vec3_t source, float knockback, float radius, int time ) {
	if( knockback <= 0 || time <= 0 || radius <= 0.0f ) {
		return;
	}

	// TODO: !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	// We should iterate over all povs!

	// TODO:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	ViewState *const viewState = getPrimaryViewState();

	// if spectator but not in chasecam, don't get any kick
	if( viewState->snapPlayerState.pmove.pm_type == PM_SPECTATOR ) {
		return;
	}

	// not if dead
	if( cg_entities[viewState->view.POVent].current.type == ET_CORPSE || cg_entities[viewState->view.POVent].current.type == ET_GIB ) {
		return;
	}

	vec3_t playerorigin;
	// predictedPlayerState is predicted only when prediction is enabled, otherwise it is interpolated
	VectorCopy( viewState->predictedPlayerState.pmove.origin, playerorigin );

	vec3_t v;
	VectorSubtract( source, playerorigin, v );
	const float dist = VectorNormalize( v );
	if( dist > radius ) {
		return;
	}

	float delta = 1.0f - ( dist / radius );
	if( delta > 1.0f ) {
		delta = 1.0f;
	}
	if( delta <= 0.0f ) {
		return;
	}

	float kick  = fabs( knockback ) * delta;
	int kicknum = -1;
	if( kick != 0.0f ) {
		// kick of 0 means no view adjust at all
		//find first free kick spot, or the one closer to be finished
		for( int i = 0; i < MAX_ANGLES_KICKS; i++ ) {
			if( cg.time > viewState->kickangles[i].timestamp + viewState->kickangles[i].kicktime ) {
				kicknum = i;
				break;
			}
		}

		// all in use. Choose the closer to be finished
		if( kicknum == -1 ) {
			int best = ( viewState->kickangles[0].timestamp + viewState->kickangles[0].kicktime ) - cg.time;
			kicknum = 0;
			for( int i = 1; i < MAX_ANGLES_KICKS; i++ ) {
				int remaintime = ( viewState->kickangles[i].timestamp + viewState->kickangles[i].kicktime ) - cg.time;
				if( remaintime < best ) {
					best    = remaintime;
					kicknum = i;
				}
			}
		}

		vec3_t forward, right;
		AngleVectors( viewState->snapPlayerState.viewangles, forward, right, nullptr );

		if( kick < 1.0f ) {
			kick = 1.0f;
		}

		float side = DotProduct( v, right );
		viewState->kickangles[kicknum].v_roll = kick * side * 0.3;
		Q_clamp( viewState->kickangles[kicknum].v_roll, -20, 20 );

		side = -DotProduct( v, forward );
		viewState->kickangles[kicknum].v_pitch = kick * side * 0.3;
		Q_clamp( viewState->kickangles[kicknum].v_pitch, -20, 20 );

		viewState->kickangles[kicknum].timestamp = cg.time;
		float ftime = (float)time * delta;
		if( ftime < 100 ) {
			ftime = 100;
		}
		viewState->kickangles[kicknum].kicktime = ftime;
	}
}

void CG_StartFallKickEffect( int bounceTime, ViewState *viewState ) {
	// TODO??? Should it be the opposite?
	if( v_viewBob.get() ) {
		viewState->fallEffectTime = 0;
		viewState->fallEffectRebounceTime = 0;
	} else {
		if( viewState->fallEffectTime > cg.time ) {
			viewState->fallEffectRebounceTime = 0;
		}

		bounceTime += 200;
		clamp_high( bounceTime, 400 );

		viewState->fallEffectTime = cg.time + bounceTime;
		if( viewState->fallEffectRebounceTime ) {
			viewState->fallEffectRebounceTime = cg.time - ( ( cg.time - viewState->fallEffectRebounceTime ) * 0.5 );
		} else {
			viewState->fallEffectRebounceTime = cg.time;
		}
	}
}

void CG_ResetColorBlend( ViewState *viewState ) {
	memset( viewState->colorblends, 0, sizeof( viewState->colorblends ) );
}

void CG_StartColorBlendEffect( float r, float g, float b, float a, int time, ViewState *viewState ) {
	if( a <= 0.0f || time <= 0 ) {
		return;
	}

	int bnum = -1;
	//find first free colorblend spot, or the one closer to be finished
	for( int i = 0; i < MAX_COLORBLENDS; i++ ) {
		if( cg.time > viewState->colorblends[i].timestamp + viewState->colorblends[i].blendtime ) {
			bnum = i;
			break;
		}
	}

	// all in use. Choose the closer to be finished
	if( bnum == -1 ) {
		int best = ( viewState->colorblends[0].timestamp + viewState->colorblends[0].blendtime ) - cg.time;
		bnum = 0;
		for( int i = 1; i < MAX_COLORBLENDS; i++ ) {
			int remaintime = ( viewState->colorblends[i].timestamp + viewState->colorblends[i].blendtime ) - cg.time;
			if( remaintime < best ) {
				best = remaintime;
				bnum = i;
			}
		}
	}

	// assign the color blend
	viewState->colorblends[bnum].blend[0] = r;
	viewState->colorblends[bnum].blend[1] = g;
	viewState->colorblends[bnum].blend[2] = b;
	viewState->colorblends[bnum].blend[3] = a;

	viewState->colorblends[bnum].timestamp = cg.time;
	viewState->colorblends[bnum].blendtime = time;
}

void CG_DamageIndicatorAdd( int damage, const vec3_t dir, ViewState *playerViewState ) {
	if( !v_damageIndicator.get() ) {
		return;
	}

// epsilons are 30 degrees
#define INDICATOR_EPSILON 0.5f
#define INDICATOR_EPSILON_UP 0.85f
#define TOP_BLEND 0
#define RIGHT_BLEND 1
#define BOTTOM_BLEND 2
#define LEFT_BLEND 3

	float blends[4] { 0.0f, 0.0f, 0.0f, 0.0f };

	vec3_t playerAngles;
	playerAngles[PITCH] = 0;
	playerAngles[YAW]   = playerViewState->predictedPlayerState.viewangles[YAW];
	playerAngles[ROLL]  = 0;

	mat3_t playerAxis;
	Matrix3_FromAngles( playerAngles, playerAxis );

	Vector4Set( blends, 0, 0, 0, 0 );
	const float damageTime = damage * v_damageIndicatorTime.get();

	bool considerDistributedEqually = false;
	if( !dir || VectorCompare( dir, vec3_origin ) ) {
		considerDistributedEqually = true;
	} else if( v_damageIndicator.get() == 2 ) {
		considerDistributedEqually = true;
	} else if( GS_Instagib( *cggs ) ) {
		considerDistributedEqually = true;
	} else if( std::abs( DotProduct( dir, &playerAxis[AXIS_UP] ) ) > INDICATOR_EPSILON_UP ) {
		considerDistributedEqually = true;
	}

	if( considerDistributedEqually ) {
		blends[RIGHT_BLEND] += damageTime;
		blends[LEFT_BLEND] += damageTime;
		blends[TOP_BLEND] += damageTime;
		blends[BOTTOM_BLEND] += damageTime;
	} else {
		const float side = DotProduct( dir, &playerAxis[AXIS_RIGHT] );
		if( side > INDICATOR_EPSILON ) {
			blends[LEFT_BLEND] += damageTime;
		} else if( side < -INDICATOR_EPSILON ) {
			blends[RIGHT_BLEND] += damageTime;
		}

		const float forward = DotProduct( dir, &playerAxis[AXIS_FORWARD] );
		if( forward > INDICATOR_EPSILON ) {
			blends[BOTTOM_BLEND] += damageTime;
		} else if( forward < -INDICATOR_EPSILON ) {
			blends[TOP_BLEND] += damageTime;
		}
	}

	for( int i = 0; i < 4; i++ ) {
		if( playerViewState->damageBlends[i] < cg.time + blends[i] ) {
			playerViewState->damageBlends[i] = cg.time + blends[i];
		}
	}
#undef TOP_BLEND
#undef RIGHT_BLEND
#undef BOTTOM_BLEND
#undef LEFT_BLEND
#undef INDICATOR_EPSILON
#undef INDICATOR_EPSILON_UP
}

void CG_ResetDamageIndicator( ViewState *viewState ) {
	for( int i = 0; i < 4; i++ ) {
		viewState->damageBlends[i] = 0;
	}
}

void CG_AddEntityToScene( entity_t *ent, DrawSceneRequest *drawSceneRequest ) {
	if( ent->model && ( !ent->boneposes || !ent->oldboneposes ) ) {
		if( R_SkeletalGetNumBones( ent->model, NULL ) ) {
			CG_SetBoneposesForTemporaryEntity( ent );
		}
	}

	drawSceneRequest->addEntity( ent );
}

int CG_SkyPortal( ViewState *viewState ) {
	if( const std::optional<wsw::StringView> maybeConfigString = cgs.configStrings.getSkyBox() ) {
		float fov   = 0;
		float scale = 0;
		int noents  = 0;
		float pitchspeed = 0, yawspeed = 0, rollspeed = 0;
		skyportal_t *sp = &viewState->view.refdef.skyportal;

		assert( maybeConfigString->isZeroTerminated() );
		if( sscanf( maybeConfigString->data(), "%f %f %f %f %f %i %f %f %f",
					&sp->vieworg[0], &sp->vieworg[1], &sp->vieworg[2], &fov, &scale,
					&noents,
					&pitchspeed, &yawspeed, &rollspeed ) >= 3 ) {
			float off = viewState->view.refdef.time * 0.001f;

			sp->fov = fov;
			sp->noEnts = ( noents ? true : false );
			sp->scale = scale ? 1.0f / scale : 0;
			VectorSet( sp->viewanglesOffset, anglemod( off * pitchspeed ), anglemod( off * yawspeed ), anglemod( off * rollspeed ) );
			return RDF_SKYPORTALINVIEW;
		}
	}

	return 0;
}

static void CG_InterpolatePlayerState( player_state_t *playerState, ViewState *viewState ) {
	const player_state_t *const ps = &viewState->snapPlayerState;
	const player_state_t *const ops = &viewState->oldSnapPlayerState;

	*playerState = *ps;

	bool teleported = ( ps->pmove.pm_flags & PMF_TIME_TELEPORT ) ? true : false;
	if( !teleported ) {
		for( int i = 0; i < 3; ++i ) {
			if( std::abs( ops->pmove.origin[i] - ps->pmove.origin[i] ) > 256 ) {
				teleported = true;
				break;
			}
		}
	}

	// if the player entity was teleported this frame use the final position
	if( !teleported ) {
		for( int i = 0; i < 3; i++ ) {
			playerState->pmove.origin[i]   = ops->pmove.origin[i] + cg.lerpfrac * ( ps->pmove.origin[i] - ops->pmove.origin[i] );
			playerState->pmove.velocity[i] = ops->pmove.velocity[i] + cg.lerpfrac * ( ps->pmove.velocity[i] - ops->pmove.velocity[i] );
			playerState->viewangles[i]     = LerpAngle( ops->viewangles[i], ps->viewangles[i], cg.lerpfrac );
		}

		playerState->viewheight = ops->viewheight + cg.lerpfrac * ( ps->viewheight - ops->viewheight );
		playerState->pmove.stats[PM_STAT_ZOOMTIME] = ops->pmove.stats[PM_STAT_ZOOMTIME] +
			cg.lerpfrac * ( ps->pmove.stats[PM_STAT_ZOOMTIME] - ops->pmove.stats[PM_STAT_ZOOMTIME] );
	}
}

static void CG_ThirdPersonOffsetView( cg_viewdef_t *view ) {
	vec3_t mins = { -4, -4, -4 };
	vec3_t maxs = { 4, 4, 4 };

	// calc exact destination
	vec3_t chase_dest;
	VectorCopy( view->origin, chase_dest );
	float r = DEG2RAD( v_thirdPersonAngle.get() );
	const float f = -cos( r );
	r = -sin( r );
	VectorMA( chase_dest, v_thirdPersonRange.get() * f, &view->axis[AXIS_FORWARD], chase_dest );
	VectorMA( chase_dest, v_thirdPersonRange.get() * r, &view->axis[AXIS_RIGHT], chase_dest );
	chase_dest[2] += 8;

	// find the spot the player is looking at
	vec3_t dest;
	trace_t trace;
	VectorMA( view->origin, 512, &view->axis[AXIS_FORWARD], dest );
	CG_Trace( &trace, view->origin, mins, maxs, dest, view->POVent, MASK_SOLID );

	vec3_t stop;
	// calculate pitch to look at the same spot from camera
	VectorSubtract( trace.endpos, view->origin, stop );
	const float dist = wsw::max( 1.0f, std::sqrt( stop[0] * stop[0] + stop[1] * stop[1] ) );
	view->angles[PITCH] = RAD2DEG( -atan2( stop[2], dist ) );
	view->angles[YAW] -= v_thirdPersonAngle.get();
	Matrix3_FromAngles( view->angles, view->axis );

	// move towards destination
	CG_Trace( &trace, view->origin, mins, maxs, chase_dest, view->POVent, MASK_SOLID );

	if( trace.fraction != 1.0f ) {
		VectorCopy( trace.endpos, stop );
		stop[2] += ( 1.0f - trace.fraction ) * 32;
		CG_Trace( &trace, view->origin, mins, maxs, stop, view->POVent, MASK_SOLID );
		VectorCopy( trace.endpos, chase_dest );
	}

	VectorCopy( chase_dest, view->origin );
}

void CG_ViewSmoothPredictedSteps( vec3_t vieworg, ViewState *viewState ) {
	// TODO: Put an assertion that it's the pov model
	// smooth out stair climbing
	const int64_t timeDelta = cg.realTime - viewState->predictedStepTime;
	if( timeDelta < PREDICTED_STEP_TIME ) {
		vieworg[2] -= viewState->predictedStep * (float)( PREDICTED_STEP_TIME - timeDelta ) / (float)PREDICTED_STEP_TIME;
	}
}

float CG_ViewSmoothFallKick( ViewState *viewState ) {
	// fallkick offset
	if( viewState->fallEffectTime > cg.time ) {
		const float fallfrac = (float)( cg.time - viewState->fallEffectRebounceTime ) / (float)( viewState->fallEffectTime - viewState->fallEffectRebounceTime );
		const float fallkick = -1.0f * std::sin( DEG2RAD( fallfrac * 180 ) ) * ( ( viewState->fallEffectTime - viewState->fallEffectRebounceTime ) * 0.01f );
		return fallkick;
	} else {
		viewState->fallEffectTime = viewState->fallEffectRebounceTime = 0;
	}
	return 0.0f;
}

[[nodiscard]]
static bool doSwitchChaseCamMode() {
	const ViewState *ourClientViewState = getOurClientViewState();
	const auto actualMoveType = ourClientViewState->predictedPlayerState.pmove.pm_type;
	if( ( actualMoveType == PM_SPECTATOR || actualMoveType == PM_CHASECAM ) && ( !cgs.demoPlaying || !cg.isDemoCamFree ) ) {
		const bool chasecam = ourClientViewState->isUsingChasecam();
		const bool realSpec = cgs.demoPlaying || ISREALSPECTATOR( ourClientViewState );

		if( ( cg.frame.multipov || chasecam ) && !cg.isDemoCamFree ) {
			if( chasecam ) {
				if( realSpec ) {
					// TODO: Use well-defined bounds
					// TODO: "camswitch" only if needed
					if( ++cg.chaseMode >= CAM_TILED ) {
						// if exceeds the cycle, start free fly
						CL_Cmd_ExecuteNow( "camswitch" );
						// TODO: Use well-defined bounds
						cg.chaseMode = 0;
					}
					return true;
				}
				return false;
			}

			if( ++cg.chaseMode >= CAM_MODES ) {
				cg.chaseMode = 0;
			}
			return true;
		}

		if( realSpec && ( cg.isDemoCamFree || ourClientViewState->snapPlayerState.pmove.pm_type == PM_SPECTATOR ) ) {
			CL_Cmd_ExecuteNow( "camswitch" );
			return true;
		}

		return false;
	}
	return false;
}

bool CG_SwitchChaseCamMode() {
	if( cg.chaseSwitchTimestamp + kChaseSwitchCooldown < cg.time ) {
		if( doSwitchChaseCamMode() ) {
			cg.chaseSwitchTimestamp = cg.time;
			cg.uiSystem->playForwardSound();
			return true;
		}
	}
	return false;
}

void CG_ClearChaseCam() {
	cg.chaseMode            = 0;
	cg.chaseSwitchTimestamp = 0;
}

static void CG_UpdateChaseCam() {
	const ViewState *const viewState = getOurClientViewState();
	const bool chasecam              = viewState->isUsingChasecam();

	if( !( cg.frame.multipov || chasecam ) || cg.isDemoCamFree ) {
		cg.chaseMode = CAM_INEYES;
	}

	// If it's worth making switch attempt (its handled invidually by respective subroutines but still...)
	if( cg.chaseSwitchTimestamp + kChaseSwitchCooldown < cg.time ) {
		usercmd_t cmd;
		NET_GetUserCmd( NET_GetCurrentUserCmdNum() - 1, &cmd );

		bool switched = false;
		if( cmd.buttons & BUTTON_ATTACK ) {
			switched = CG_SwitchChaseCamMode();
		}
		// Disallow switching mode and pov simultaneously
		if( !switched ) {
			if( cmd.upmove > 0 || cmd.buttons & BUTTON_SPECIAL ) {
				CG_ChaseStep( +1 );
			} else if( cmd.upmove < 0 ) {
				CG_ChaseStep( -1 );
			}
		}
	}
}

static void CG_SetupViewDef( cg_viewdef_t *view, int type, bool thirdperson, ViewState *viewState, const Rect &viewport ) {
	memset( view, 0, sizeof( cg_viewdef_t ) );

	//
	// VIEW SETTINGS
	//

	view->type = type;

	if( view->type == VIEWDEF_PLAYERVIEW ) {
		view->POVent = viewState->snapPlayerState.POVnum;

		view->draw2D      = true;
		view->thirdperson = thirdperson;

		if( cg_entities[view->POVent].serverFrame != cg.frame.serverFrame ) {
			view->thirdperson = false;
		}

		// check for drawing gun
		if( !view->thirdperson && view->POVent > 0 && view->POVent <= cggs->maxclients ) {
			if( ( cg_entities[view->POVent].serverFrame == cg.frame.serverFrame ) &&
				( cg_entities[view->POVent].current.weapon != 0 ) ) {
				view->drawWeapon = v_gun.get() && v_gunAlpha.get() > 0.0f;
			}
		}

		// check for chase cams
		if( !( viewState->snapPlayerState.pmove.pm_flags & PMF_NO_PREDICTION ) ) {
			if( (unsigned)view->POVent == cgs.playerNum + 1 ) {
				if( v_predict.get() && !cgs.demoPlaying ) {
					view->playerPrediction = true;
				}
			}
		}
	} else if( view->type == VIEWDEF_CAMERA ) {
		view->POVent           = 0;
		view->thirdperson      = false;
		view->playerPrediction = false;
		view->drawWeapon       = false;
		view->draw2D           = false;
	} else {
		Com_Error( ERR_DROP, "CG_SetupView: Invalid view type %i\n", view->type );
	}

	//
	// SETUP REFDEF FOR THE VIEW SETTINGS
	//

	if( view->type == VIEWDEF_PLAYERVIEW ) {
		vec3_t viewoffset;
		if( view->playerPrediction ) {
			CG_PredictMovement();

			// fixme: crouching is predicted now, but it looks very ugly
			VectorSet( viewoffset, 0.0f, 0.0f, viewState->predictedPlayerState.viewheight );

			for( int i = 0; i < 3; i++ ) {
				view->origin[i] = viewState->predictedPlayerState.pmove.origin[i] + viewoffset[i] - ( 1.0f - cg.lerpfrac ) * viewState->predictionError[i];
				view->angles[i] = viewState->predictedPlayerState.viewangles[i];
			}

			CG_ViewSmoothPredictedSteps( view->origin, viewState ); // smooth out stair climbing

			if( v_viewBob.get() && !view->thirdperson ) {
				// TODO: Is fall kick applied to non-predicted views?
				view->origin[2] += CG_ViewSmoothFallKick( viewState ) * 6.5f;
			}
		} else {
			viewState->predictingTimeStamp = cg.time;
			viewState->predictFrom = 0;

			// we don't run prediction, but we still set viewState->predictedPlayerState with the interpolation
			CG_InterpolatePlayerState( &viewState->predictedPlayerState, viewState );

			VectorSet( viewoffset, 0.0f, 0.0f, viewState->predictedPlayerState.viewheight );

			VectorAdd( viewState->predictedPlayerState.pmove.origin, viewoffset, view->origin );
			VectorCopy( viewState->predictedPlayerState.viewangles, view->angles );
		}

		view->refdef.fov_x = CG_CalcViewFov();

		CG_CalcViewBob( viewState );

		VectorCopy( viewState->predictedPlayerState.pmove.velocity, view->velocity );
	} else if( view->type == VIEWDEF_CAMERA ) {
		ViewState *const viewState = getOurClientViewState();

		// If the old view type was player view, start from that
		if( viewState->view.type == VIEWDEF_PLAYERVIEW ) {
			VectorCopy( viewState->view.origin, cg.demoFreeCamOrigin );
			VectorCopy( viewState->view.angles, cg.demoFreeCamAngles );
			VectorCopy( viewState->view.velocity, cg.demoFreeCamVelocity );
		}

		// We dislike putting it here, but we do the similar operation (predict movement) in the same enclosing subroutine
		// TODO: Lift it to the caller, as well as the movement prediction?
		// This is not that easy as it sounds as handling of the prediction results may depend of viewState->view
		CG_DemoCam_FreeFly();

		VectorCopy( cg.demoFreeCamAngles, view->angles );
		VectorCopy( cg.demoFreeCamOrigin, view->origin );
		VectorCopy( cg.demoFreeCamVelocity, view->velocity );

		view->refdef.fov_x = v_fov.get();
	}

	Matrix3_FromAngles( view->angles, view->axis );

	// view rectangle size
	view->refdef.x              = viewport.x;
	view->refdef.y              = viewport.y;
	view->refdef.width          = viewport.width;
	view->refdef.height         = viewport.height;
	view->refdef.time           = cg.time;
	view->refdef.areabits       = cg.frame.areabits;
	view->refdef.scissor_x      = viewport.x;
	view->refdef.scissor_y      = viewport.y;
	view->refdef.scissor_width  = viewport.width;
	view->refdef.scissor_height = viewport.height;

	view->refdef.fov_y = CalcFov( view->refdef.fov_x, view->refdef.width, view->refdef.height );

	AdjustFov( &view->refdef.fov_x, &view->refdef.fov_y, view->refdef.width, view->refdef.height, false );

	view->fracDistFOV = tan( view->refdef.fov_x * ( M_PI / 180 ) * 0.5f );

	view->refdef.minLight = 0.3f;

	if( view->thirdperson ) {
		CG_ThirdPersonOffsetView( view );
	}

	if( !view->playerPrediction ) {
		viewState->predictedWeaponSwitch = 0;
	}

	// TODO: This code implies that viewState->view == view
	VectorCopy( viewState->view.origin, view->refdef.vieworg );
	Matrix3_Copy( viewState->view.axis, view->refdef.viewaxis );
	VectorInverse( &view->refdef.viewaxis[AXIS_RIGHT] );

	view->refdef.colorCorrection = NULL;
	if( v_colorCorrection.get() ) {
		int colorCorrection = GS_ColorCorrection( *cggs );
		if( ( colorCorrection > 0 ) && ( colorCorrection < MAX_IMAGES ) ) {
			view->refdef.colorCorrection = cgs.imagePrecache[colorCorrection];
		}
	}
}

static void CG_DrawCrosshair( Draw2DRequest *request, ViewState *viewState, std::optional<float> miniviewScale );
static void drawNamesAndBeacons( Draw2DRequest *request, ViewState *viewState, std::optional<float> miniviewScale );

static void updateFrameLerpProperties( unsigned extrapolationTime ) {
	int snapTime = ( cg.frame.serverTime - cg.oldFrame.serverTime );
	if( !snapTime ) {
		snapTime = cgs.snapFrameTime;
	}

	// moved this from CG_Init here
	cgs.extrapolationTime = extrapolationTime;

	if( cg.oldFrame.serverTime == cg.frame.serverTime ) {
		cg.lerpfrac = 1.0f;
	} else {
		cg.lerpfrac = ( (double)( cg.time - cgs.extrapolationTime ) - (double)cg.oldFrame.serverTime ) / (double)snapTime;
	}

	if( cgs.extrapolationTime ) {
		cg.xerpTime = 0.001f * ( (double)cg.time - (double)cg.frame.serverTime );
		cg.oldXerpTime = 0.001f * ( (double)cg.time - (double)cg.oldFrame.serverTime );

		if( cg.time >= cg.frame.serverTime ) {
			cg.xerpSmoothFrac = (double)( cg.time - cg.frame.serverTime ) / (double)( cgs.extrapolationTime );
			Q_clamp( cg.xerpSmoothFrac, 0.0f, 1.0f );
		} else {
			cg.xerpSmoothFrac = (double)( cg.frame.serverTime - cg.time ) / (double)( cgs.extrapolationTime );
			Q_clamp( cg.xerpSmoothFrac, -1.0f, 0.0f );
			cg.xerpSmoothFrac = 1.0f - cg.xerpSmoothFrac;
		}

		clamp_low( cg.xerpTime, -( cgs.extrapolationTime * 0.001f ) );

		//clamp( cg.xerpTime, -( cgs.extrapolationTime * 0.001f ), ( cgs.extrapolationTime * 0.001f ) );
		//clamp( cg.oldXerpTime, 0, ( ( snapTime + cgs.extrapolationTime ) * 0.001f ) );
	} else {
		cg.xerpTime = 0.0f;
		cg.xerpSmoothFrac = 0.0f;
	}

	if( v_showClamp.get() ) {
		if( cg.lerpfrac > 1.0f ) {
			Com_Printf( "high clamp %f\n", cg.lerpfrac );
		} else if( cg.lerpfrac < 0.0f ) {
			Com_Printf( "low clamp  %f\n", cg.lerpfrac );
		}
	}

	Q_clamp( cg.lerpfrac, 0.0f, 1.0f );
}

[[nodiscard]]
static auto prepareViewRectsAndStateIndices( Rect *viewRects, unsigned *viewStateIndices ) -> std::pair<bool, unsigned> {
	const bool actuallyUseTiledMode = CG_UsesTiledView();
	unsigned numDisplayedViewStates;
	if( actuallyUseTiledMode ) {
		assert( cg.tileMiniviewViewStateIndices.size() == cg.tileMiniviewPositions.size() );
		// TODO: Std::copy?
		for( unsigned i = 0; i < cg.tileMiniviewViewStateIndices.size(); ++i ) {
			viewRects[i]        = cg.tileMiniviewPositions[i];
			viewStateIndices[i] = cg.tileMiniviewViewStateIndices[i];
		}
		numDisplayedViewStates = cg.tileMiniviewViewStateIndices.size();
	} else {
		if( const int size = std::round( v_viewSize.get() ); size < 100 ) {
			// Round to a multiple of 2
			const int regionWidth  = ( ( cgs.vidWidth * size ) / 100 ) & ( ~1 );
			const int regionHeight = ( ( cgs.vidHeight * size ) / 100 ) & ( ~1 );

			viewRects[0].x      = ( cgs.vidWidth - regionWidth ) / 2;
			viewRects[0].y      = ( cgs.vidHeight - regionHeight ) / 2;
			viewRects[0].width  = regionWidth;
			viewRects[0].height = regionHeight;
		} else {
			viewRects[0].x      = 0;
			viewRects[0].y      = 0;
			viewRects[0].width  = cgs.vidWidth;
			viewRects[0].height = cgs.vidHeight;
		}
		viewStateIndices[0] = (unsigned)( getPrimaryViewState() - cg.viewStates );
		numDisplayedViewStates = 1;
		numDisplayedViewStates += cg.uiSystem->retrieveHudControlledMiniviews( viewRects + 1, viewStateIndices + 1 );
	}

	return { actuallyUseTiledMode, numDisplayedViewStates };
}

[[nodiscard]]
static auto createDrawSceneRequests( DrawSceneRequest **drawSceneRequests, bool actuallyUseTiledMode, const Rect *viewRects,
									 const unsigned *viewStateIndices, unsigned numDisplayedViewStates ) -> unsigned {
	WSW_PROFILER_SCOPE();

	unsigned useCachedViewsMask = 0;

	// Set to zero by default so subtraction tests in the main loop don't lead to UB
	int bestMiniviewWidth     = std::numeric_limits<int>::lowest();
	int bestMiniviewHeight    = std::numeric_limits<int>::lowest();
	bool hasDetailedMiniviews = false;
	if( actuallyUseTiledMode ) {
		int worstMiniviewWidth  = std::numeric_limits<int>::max();
		int worstMiniviewHeight = std::numeric_limits<int>::max();
		// TODO: Think of saving flags that the miniview uses "exclusive" mode? This could be useful for HUD as well.
		for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
			const Rect &viewport = viewRects[viewNum];
			bestMiniviewWidth    = wsw::max( bestMiniviewWidth, viewport.width );
			bestMiniviewHeight   = wsw::max( bestMiniviewHeight, viewport.height );
			worstMiniviewWidth   = wsw::min( worstMiniviewWidth, viewport.width );
			worstMiniviewHeight  = wsw::min( worstMiniviewHeight, viewport.height );
		}
		hasDetailedMiniviews = std::abs( bestMiniviewWidth - worstMiniviewWidth ) > 1 ||
							   std::abs( bestMiniviewHeight - worstMiniviewHeight ) > 1;
	}

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		ViewState *const viewState = cg.viewStates + viewStateIndices[viewNum];
		const Rect viewport        = viewRects[viewNum];
		const bool isMiniview      = viewNum != 0 || actuallyUseTiledMode;

		int viewDefType  = VIEWDEF_PLAYERVIEW;
		bool thirdperson = false;
		if( viewState == getPrimaryViewState() ) {
			if( cgs.demoPlaying && cg.isDemoCamFree ) {
				if( !isMiniview ) {
					viewDefType = VIEWDEF_CAMERA;
				}
			} else {
				// Draw from third-person view only if we have an active pov.
				// Otherwise we end up with confusing switching of camera location.
				if( CG_ActivePovOfViewState( cg.chasedViewportIndex ) != std::nullopt ) {
					if( cg.chaseMode == CAM_INEYES ) {
						thirdperson = v_thirdPerson.get();
					} else if( cg.chaseMode == CAM_THIRDPERSON ) {
						thirdperson = true;
					}
				}
			}
		}

		CG_SetupViewDef( &viewState->view, viewDefType, thirdperson, viewState, viewport );

		refdef_t *const rd = &viewState->view.refdef;
		AnglesToAxis( viewState->view.angles, rd->viewaxis );

		rd->rdflags = 0;

		// set the RDF_UNDERWATER and RDF_CROSSINGWATER bitflags
		int contents = CG_PointContents( viewState->view.origin );
		if( contents & MASK_WATER ) {
			rd->rdflags |= RDF_UNDERWATER;
			// undewater, check above
			contents = CG_PointContents( tv( viewState->view.origin[0], viewState->view.origin[1], viewState->view.origin[2] + 9 ) );
			if( !( contents & MASK_WATER ) ) {
				rd->rdflags |= RDF_CROSSINGWATER;
			}
		} else {
			// look down a bit
			contents = CG_PointContents( tv( viewState->view.origin[0], viewState->view.origin[1], viewState->view.origin[2] - 9 ) );
			if( contents & MASK_WATER ) {
				rd->rdflags |= RDF_CROSSINGWATER;
			}
		}

		if( cg.oldAreabits ) {
			rd->rdflags |= RDF_OLDAREABITS;
		}
		if( v_outlineWorld.get() ) {
			rd->rdflags |= RDF_WORLDOUTLINES;
		}

		rd->rdflags |= CG_SkyPortal( viewState );
		if( isMiniview ) {
			rd->rdflags |= RDF_USEAUTOLODSCALE;
			rd->rdflags |= RDF_NOBSPOCCLUSIONCULLING;
			if( !hasDetailedMiniviews || std::abs( viewport.width - bestMiniviewWidth ) > 1 ||
										 std::abs( viewport.height - bestMiniviewHeight ) > 1 ) {
				rd->rdflags = RDF_LOWDETAIL;
			}
		} else {
			if( cg.uiSystem->isShowingModalMenu() || cg.uiSystem->isShowingScoreboard() ) {
				rd->rdflags |= RDF_DRAWBRIGHT;
			}
		}

		// warp if underwater
		if( rd->rdflags & RDF_UNDERWATER ) {
			const float phase = rd->time * 0.001f * 0.6f * M_TWOPI;
			const float v = 0.015f * ( std::sin( phase ) - 1.0f ) + 1.0f;
			rd->fov_x *= v;
			rd->fov_y *= v;
		}

		if( viewNum > 0 && !actuallyUseTiledMode ) {
			viewState->view.refdef.renderTarget = GetMiniviewRenderTarget();
			if( (int)( ( viewNum - 1 ) % 4 ) != (int)( cg.frameCount % 4 ) ) {
				useCachedViewsMask |= 1 << viewNum;
			}
		}

		drawSceneRequests[viewNum] = CreateDrawSceneRequest( viewState->view.refdef );
	}

	for( int pnum = 0; pnum < cg.frame.numEntities; pnum++ ) {
		const int stateIndex  = pnum & ( MAX_PARSE_ENTITIES - 1 );
		entity_state_t *state = &cg.frame.parsedEntities[stateIndex];
		centity_t *cent       = &cg_entities[state->number];

		// TODO: Use separate bins for each type of entities
		if( cent->type == ET_PORTALSURFACE ) {
			if( !VectorCompare( cent->ent.origin, cent->ent.origin2 ) ) { // construct the view matrix for portal view
				if( cent->current.effects & EF_ROTATE_AND_BOB ) {
					float phase = cent->current.frame / 256.0f;
					float speed = cent->current.modelindex2 ? cent->current.modelindex2 : 50;

					Matrix3_Identity( cent->ent.axis );
					Matrix3_Rotate( cent->ent.axis, 5 * sin( ( phase + cg.time * 0.001 * speed * 0.01 ) * M_TWOPI ),
									1, 0, 0, cent->ent.axis );
				}
			}

			for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
				drawSceneRequests[viewNum]->addPortalEntity( &cent->ent );
			}
		}
	}

	return useCachedViewsMask;
}

[[nodiscard]]
static auto coPrepareDrawSceneRequests( CoroTask::StartInfo si, DrawSceneRequest **drawSceneRequests, unsigned useCachedViewsMask,
										const unsigned *viewStateIndices, unsigned numDisplayedViewStates ) -> CoroTask {
	assert( numDisplayedViewStates < MAX_CLIENTS + 1 );

	// Note: We process all draw scene requests as usual for correctness reasons,
	// but exclude "cached view" requests from submission to the renderer.
	// Further optimizations are possible, but this already is a huge win.

	wsw::StaticVector<DrawSceneRequest *, MAX_CLIENTS + 1> tmpDrawSceneRequests;
	std::span<DrawSceneRequest *> requestsToSubmit;

	if( useCachedViewsMask ) {
		// TODO: Add iterators for iterating over bits
		for( unsigned i = 0; i < numDisplayedViewStates; ++i ) {
			if( !( useCachedViewsMask & ( 1 << i ) ) ) {
				tmpDrawSceneRequests.push_back( drawSceneRequests[i] );
			}
		}
		requestsToSubmit = tmpDrawSceneRequests;
	} else {
		requestsToSubmit = { drawSceneRequests, drawSceneRequests + numDisplayedViewStates };
	}

	const TaskHandle beginTask = BeginProcessingDrawSceneRequests( requestsToSubmit );

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		ViewState *const viewState = cg.viewStates + viewStateIndices[viewNum];

		CG_LerpEntities( viewState );

		CG_CalcViewWeapon( &viewState->weapon, viewState );
	}

	CG_FireEvents( false );

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		ViewState *const viewState = cg.viewStates + viewStateIndices[viewNum];

		// This has to be run.
		// As a safety measure, the condition preserves the old well-known behavior for the single-POV mode.
		if( numDisplayedViewStates > 1 ) {
			CG_LerpEntities( viewState );
		}

		CG_AddEntities( drawSceneRequests[viewNum], viewState );
		CG_AddViewWeapon( &viewState->weapon, drawSceneRequests[viewNum], viewState );
	}

	// Perform UI rendering in the main thread, while the actual set of views gets prepared in background
	cg.uiSystem->renderInternally();

	cg.delayedExecutionSystem.run();

	// Make sure all possible effects in this frame are submitted prior to simulation
	// (A simulation timestamp of an effect must match its submission timestamp).
	cg.effectsSystem.simulateFrame( cg.time );
	cg.particleSystem.simulateFrame( cg.time );
	cg.polyEffectsSystem.simulateFrame( cg.time );
	cg.simulatedHullsSystem.simulateFrame( cg.time );

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		DrawSceneRequest *const drawSceneRequest = drawSceneRequests[viewNum];
		ViewState *const viewState = cg.viewStates + viewStateIndices[viewNum];
		unsigned povPlayerMask;
		if( viewState->predictedPlayerState.POVnum > 0 && viewState->predictedPlayerState.POVnum <= MAX_CLIENTS ) {
			povPlayerMask = 1u << ( viewState->predictedPlayerState.POVnum - 1 );
		} else {
			povPlayerMask = ~0u;
		}

		cg.effectsSystem.submitToScene( cg.time, drawSceneRequest, povPlayerMask );
		cg.particleSystem.submitToScene( cg.time, drawSceneRequest );
		cg.polyEffectsSystem.submitToScene( cg.time, drawSceneRequest, povPlayerMask );
		cg.simulatedHullsSystem.submitToScene( cg.time, drawSceneRequest, povPlayerMask );
	}

	const std::span<const TaskHandle> endProcessingDependencies { &beginTask, 1 };
	co_await si.taskSystem->awaiterOf( EndProcessingDrawSceneRequests( requestsToSubmit, endProcessingDependencies ) );
}

static void prepareDrawSceneRequests( DrawSceneRequest **drawSceneRequests, unsigned useCachedViewsMask,
									  const unsigned *viewStateIndices, unsigned numDisplayedViewStates ) {
	WSW_PROFILER_SCOPE();

	TaskSystem *const taskSystem = BeginProcessingOfTasks();
	[[maybe_unused]] volatile wsw::ScopeExitAction callEndProcessingOfTasks( &EndProcessingOfTasks );

	(void)taskSystem->addCoro( [=]() {
		CoroTask::StartInfo si { taskSystem, std::span<const TaskHandle> {}, CoroTask::OnlyMainThread };
		return coPrepareDrawSceneRequests( si, drawSceneRequests, useCachedViewsMask, viewStateIndices, numDisplayedViewStates );
	});

	// Let worker threads execute while we are running GC in the main thread.
	// We must note that this approach is benefitial only for small GC threshold values
	// (otherwise, the spike is way too large to be hidden anyway).
	cg.uiSystem->runGCIfNeeded();
}

static void commitDrawSceneRequests( DrawSceneRequest **drawSceneRequests, bool actuallyUseTiledMode,
									 const unsigned *viewStateIndices, unsigned numDisplayedViewStates ) {
	WSW_PROFILER_SCOPE();

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		if( actuallyUseTiledMode || viewNum == 0 || (int)( cg.frameCount % 4 ) == (int)( ( viewNum - 1 ) % 4 ) ) {
			CommitProcessedDrawSceneRequest( drawSceneRequests[viewNum] );
		} else {
			assert( cg.viewStates[viewStateIndices[viewNum]].view.refdef.renderTarget );
		}
	}
}

[[nodiscard]]
static bool blitPreparedViews( DrawSceneRequest **drawSceneRequests, bool actuallyUseTiledMode, bool hasModalOverlay,
							   const unsigned *viewStateIndices, unsigned numDisplayedViewStates ) {
	WSW_PROFILER_SCOPE();

	bool hasRenderedTheMenu = false;

	const bool shouldDraw2D    = v_draw2D.get();
	const bool shouldDrawHuds  = v_showHud.get();

	assert( cgs.vidWidth > 0 && cgs.vidHeight > 0 );

	const float rcpVidWidth  = 1.0f / (float)cgs.vidWidth;
	const float rcpVidHeight = 1.0f / (float)cgs.vidHeight;

	for( unsigned viewNum = 0; viewNum < numDisplayedViewStates; ++viewNum ) {
		ViewState *const viewState = cg.viewStates + viewStateIndices[viewNum];
		refdef_t &rd = viewState->view.refdef;

		wsw::ScopedResource<Draw2DRequest *, Draw2DRequestScopedOps> request( CreateDraw2DRequest() );

		if( auto *renderTarget = rd.renderTarget ) {
			const float tcX0 = rcpVidWidth * (float)rd.x;
			const float tcY0 = rcpVidHeight * (float)( cgs.vidHeight - rd.y );
			const float tcX1 = rcpVidWidth * (float)( rd.x + rd.width );
			const float tcY1 = rcpVidHeight * (float)( cgs.vidHeight - ( rd.y + rd.height ) );

			shader_s *shader = R_WrapMiniviewRenderTargetInMaterial( renderTarget );

			request->setScissor( rd.x, rd.y, rd.width, rd.height );
			request->drawStretchPic( rd.x, rd.y, rd.width, rd.height, tcX0, tcY0, tcX1, tcY1, colorWhite, shader );
		}

		const bool isMiniview = viewNum != 0 || actuallyUseTiledMode;
		if( shouldDraw2D && viewState->view.draw2D ) {
			bool drawGfx = true;
			if( hasModalOverlay ) {
				drawGfx = false;
				if( cg.chaseMode != CAM_TILED && cg.numSnapViewStates > 0 ) {
					// Check if its a HUD-hosted miniview
					for( const auto &paneViewStateIndices: cg.hudControlledMiniviewViewStateIndicesForPane ) {
						if( wsw::contains( paneViewStateIndices, viewStateIndices[viewNum] ) ) {
							drawGfx = true;
							break;
						}
					}
				}
			}
			bool drawCrosshair = false;
			if( CG_ActivePovOfViewState( viewStateIndices[viewNum] ) != std::nullopt ) {
				if( CG_IsPovAlive( viewStateIndices[viewNum] ) ) {
					drawCrosshair = true;
				}
			}

			request->setScissor( rd.x, rd.y, rd.width, rd.height );

			if( shouldDrawHuds ) {
				if( drawGfx ) {
					std::optional<float> miniviewScale;
					if( isMiniview ) {
						miniviewScale = wsw::min( rcpVidWidth * (float)rd.width, rcpVidHeight * (float)rd.height );
					}
					drawNamesAndBeacons( request.get(), viewState, miniviewScale );
					if( drawCrosshair ) {
						CG_DrawCrosshair( request.get(), viewState, miniviewScale );
					}
				}
			}
		}

		CommitDraw2DRequest( request.get() );
		// Let the UI use the limited resource
		request = {};

		if( !isMiniview ) {
			cg.uiSystem->drawMenuPartInMainContext();
			hasRenderedTheMenu = true;
		}
	}

	return hasRenderedTheMenu;
}

CGRenderViewResult CG_RenderView( int frameTime, int realFrameTime, int64_t realTime, int64_t serverTime, unsigned extrapolationTime ) {
	WSW_PROFILER_SCOPE();

	CGRenderViewResult result {
		.hasBlittedTheMenu       = false,
		.hasBlittedTheHud        = false,
		.hasRenderedUIInternally = false,
		.hasRunGCIfNeeded        = false,
	};

	// update time
	cg.realTime      = realTime;
	cg.frameTime     = frameTime;
	cg.realFrameTime = realFrameTime;
	cg.time          = serverTime;
	cg.frameCount++;

	// TODO: Is there more appropritate place to call it
	CG_CheckSharedCrosshairState( false );

	if( !cgs.precacheDone || !cg.frame.valid ) {
		CG_Precache();
	} else {
		updateFrameLerpProperties( extrapolationTime );

		// TODO: Is it ever going to happen?
		if( cgs.configStrings.getWorldModel() == std::nullopt ) {
			CG_AddLocalSounds();

			wsw::ScopedResource<Draw2DRequest *, Draw2DRequestScopedOps> request( CreateDraw2DRequest() );
			request->drawStretchPic( 0, 0, cgs.vidWidth, cgs.vidHeight, 0, 0, 1, 1, colorBlack, cgs.shaderWhite );
			CommitDraw2DRequest( request.get() );

			CG_ClearEffects();

			cg.soundSystem->updateListener( -1, vec3_origin, vec3_origin, axis_identity );
		} else {
			if( cg.motd && ( cg.time > cg.motd_time ) ) {
				Q_free( cg.motd );
				cg.motd = NULL;
			}

			CG_FlashGameWindow(); // notify player of important game events
			CG_UpdateChaseCam();
			CG_RunLightStyles();
			CG_SetSceneTeamColors(); // update the team colors in the renderer

			CG_ClearFragmentedDecals();

			// Keep updating freecam_delta_angles (is it really needed?)
			if( cgs.demoPlaying && !cg.isDemoCamFree ) {
				usercmd_t cmd;
				NET_GetUserCmd( NET_GetCurrentUserCmdNum() - 1, &cmd );

				for( int i = 0; i < 3; i++ ) {
					cg.demoFreeCamDeltaAngles[i] = ANGLE2SHORT( cg.demoFreeCamAngles[i] ) - cmd.angles[i];
				}
			}

			Rect viewRects[MAX_CLIENTS + 1];
			unsigned viewStateIndices[MAX_CLIENTS + 1];

			const auto [actuallyUseTiledMode, numDisplayedViewStates] = prepareViewRectsAndStateIndices( viewRects, viewStateIndices );

			// TODO: Use the same DrawSceneRequest instance but modify POV-specific data
			DrawSceneRequest *drawSceneRequests[MAX_CLIENTS + 1];
			CG_ResetTemporaryBoneposesCache();

			unsigned useCachedViewsMask = 0;
			do {
				BeginDrawingScenes();
				[[maybe_unused]] volatile wsw::ScopeExitAction callEndDrawingScenes( []() { EndDrawingScenes(); } );

				useCachedViewsMask = createDrawSceneRequests( drawSceneRequests, actuallyUseTiledMode, viewRects,
															  viewStateIndices, numDisplayedViewStates );

				prepareDrawSceneRequests( drawSceneRequests, useCachedViewsMask, viewStateIndices, numDisplayedViewStates );
				result.hasRunGCIfNeeded = true;

				commitDrawSceneRequests( drawSceneRequests, actuallyUseTiledMode, viewStateIndices, numDisplayedViewStates );
			} while( false );

			result.hasRenderedUIInternally = true;

			const bool hasModalOverlay = cg.uiSystem->isShowingModalMenu() || cg.uiSystem->isShowingScoreboard();
			result.hasBlittedTheMenu   = blitPreparedViews( drawSceneRequests, actuallyUseTiledMode, hasModalOverlay,
															viewStateIndices, numDisplayedViewStates );

			CG_ResetTemporaryBoneposesCache();

			// Blit the HUD first in this case (this is a hack for the demo playback menu which must be on top)
			if( actuallyUseTiledMode && !hasModalOverlay ) {
				cg.uiSystem->drawHudPartInMainContext();
				result.hasBlittedTheHud = true;
			}

			CG_AddLocalSounds();

			cg.oldAreabits = true;

			const ViewState *primaryViewState = getPrimaryViewState();
			cg.soundSystem->updateListener( primaryViewState->view.POVent, primaryViewState->view.origin,
													 primaryViewState->view.velocity, primaryViewState->view.axis );
		}
	}

	return result;
}

void CG_LoadingString( const char *str ) {
	Q_strncpyz( cgs.loadingstring, str, sizeof( cgs.loadingstring ) );
}

/*
* CG_LoadingItemName
*
* Allow at least one item per frame to be precached.
* Stop accepting new precaches after the timelimit for this frame has been reached.
*/
bool CG_LoadingItemName( const char *str ) {
	if( cgs.precacheCount > cgs.precacheStart && ( Sys_Milliseconds() > cgs.precacheStartMsec + 33 ) ) {
		return false;
	}
	cgs.precacheCount++;
	return true;
}

static void CG_AddBlend( float r, float g, float b, float a, float *v_blend ) {
	float a2, a3;

	if( a <= 0 ) {
		return;
	}
	a2 = v_blend[3] + ( 1 - v_blend[3] ) * a; // new total alpha
	a3 = v_blend[3] / a2; // fraction of color from old

	v_blend[0] = v_blend[0] * a3 + r * ( 1 - a3 );
	v_blend[1] = v_blend[1] * a3 + g * ( 1 - a3 );
	v_blend[2] = v_blend[2] * a3 + b * ( 1 - a3 );
	v_blend[3] = a2;
}

static void CG_CalcColorBlend( float *color, ViewState *viewState ) {
	//clear old values
	for( int i = 0; i < 4; i++ ) {
		color[i] = 0.0f;
	}

	// Add colorblend based on world position
	const int contents = CG_PointContents( viewState->view.origin );
	if( contents & CONTENTS_WATER ) {
		CG_AddBlend( 0.0f, 0.1f, 8.0f, 0.2f, color );
	}
	if( contents & CONTENTS_LAVA ) {
		CG_AddBlend( 1.0f, 0.3f, 0.0f, 0.6f, color );
	}
	if( contents & CONTENTS_SLIME ) {
		CG_AddBlend( 0.0f, 0.1f, 0.05f, 0.6f, color );
	}

	// Add colorblends from sfx
	for( int i = 0; i < MAX_COLORBLENDS; i++ ) {
		if( cg.time <= viewState->colorblends[i].timestamp + viewState->colorblends[i].blendtime ) {
			const float time   = (float)( ( viewState->colorblends[i].timestamp + viewState->colorblends[i].blendtime ) - cg.time );
			const float uptime = ( (float)viewState->colorblends[i].blendtime ) * 0.5f;
			if( float delta = 1.0f - ( fabs( time - uptime ) / uptime ); delta > 0.0f ) {
				if( delta > 1.0f ) {
					delta = 1.0f;
				}
				CG_AddBlend( viewState->colorblends[i].blend[0],
							 viewState->colorblends[i].blend[1],
					 		 viewState->colorblends[i].blend[2],
					 		 viewState->colorblends[i].blend[3] * delta,
					         color );
			}
		}
	}
}

void CG_ClearPointedNum( ViewState *viewState ) {
	viewState->pointedNum      = 0;
	viewState->pointRemoveTime = 0;
	viewState->pointedHealth   = 0;
	viewState->pointedArmor    = 0;
}

static void CG_UpdatePointedNum( ViewState *viewState ) {
	if( cg.uiSystem->isShowingModalMenu() || cg.uiSystem->isShowingScoreboard() || viewState->view.thirdperson ||
		viewState->view.type != VIEWDEF_PLAYERVIEW || !v_showPointedPlayer.get() ) {
		CG_ClearPointedNum( viewState );
	} else {
		if( viewState->predictedPlayerState.stats[STAT_POINTED_PLAYER] ) {
			bool mega = false;

			viewState->pointedNum      = viewState->predictedPlayerState.stats[STAT_POINTED_PLAYER];
			viewState->pointRemoveTime = cg.time + 150;

			viewState->pointedHealth = 3.2 * ( viewState->predictedPlayerState.stats[STAT_POINTED_TEAMPLAYER] & 0x1F );
			mega = viewState->predictedPlayerState.stats[STAT_POINTED_TEAMPLAYER] & 0x20 ? true : false;
			viewState->pointedArmor = 5 * ( viewState->predictedPlayerState.stats[STAT_POINTED_TEAMPLAYER] >> 6 & 0x3F );
			if( mega ) {
				viewState->pointedHealth += 100;
				if( viewState->pointedHealth > 200 ) {
					viewState->pointedHealth = 200;
				}
			}
		}

		if( viewState->pointRemoveTime <= cg.time ) {
			CG_ClearPointedNum( viewState );
		}

		if( viewState->pointedNum && v_showPointedPlayer.get() == 2 ) {
			if( cg_entities[viewState->pointedNum].current.team != viewState->predictedPlayerState.stats[STAT_TEAM] ) {
				CG_ClearPointedNum( viewState );
			}
		}
	}
}

int CG_HorizontalAlignForWidth( const int x, int align, int width ) {
	int nx = x;

	if( align % 3 == 0 ) { // left
		nx = x;
	}
	if( align % 3 == 1 ) { // center
		nx = x - width / 2;
	}
	if( align % 3 == 2 ) { // right
		nx = x - width;
	}

	return nx;
}

int CG_VerticalAlignForHeight( const int y, int align, int height ) {
	int ny = y;

	if( align / 3 == 0 ) { // top
		ny = y;
	} else if( align / 3 == 1 ) { // middle
		ny = y - height / 2;
	} else if( align / 3 == 2 ) { // bottom
		ny = y - height;
	}

	return ny;
}

static void drawBar( Draw2DRequest *request, int x, int y, int align, int w, int h, int val, int maxval, const vec4_t color, struct shader_s *shader ) {
	if( val < 1 || maxval < 1 || w < 1 || h < 1 ) {
		return;
	}

	if( !shader ) {
		shader = cgs.shaderWhite;
	}

	float frac;
	if( val >= maxval ) {
		frac = 1.0f;
	} else {
		frac = (float)val / (float)maxval;
	}

	vec2_t tc[2];
	tc[0][0] = 0.0f;
	tc[0][1] = 1.0f;
	tc[1][0] = 0.0f;
	tc[1][1] = 1.0f;
	if( h > w ) {
		h = (int)( (float)h * frac + 0.5 );
		if( align / 3 == 0 ) { // top
			tc[1][1] = 1.0f * frac;
		} else if( align / 3 == 1 ) {   // middle
			tc[1][0] = ( 1.0f - ( 1.0f * frac ) ) * 0.5f;
			tc[1][1] = ( 1.0f * frac ) * 0.5f;
		} else if( align / 3 == 2 ) {   // bottom
			tc[1][0] = 1.0f - ( 1.0f * frac );
		}
	} else {
		w = (int)( (float)w * frac + 0.5 );
		if( align % 3 == 0 ) { // left
			tc[0][1] = 1.0f * frac;
		}
		if( align % 3 == 1 ) { // center
			tc[0][0] = ( 1.0f - ( 1.0f * frac ) ) * 0.5f;
			tc[0][1] = ( 1.0f * frac ) * 0.5f;
		}
		if( align % 3 == 2 ) { // right
			tc[0][0] = 1.0f - ( 1.0f * frac );
		}
	}

	x = CG_HorizontalAlignForWidth( x, align, w );
	y = CG_VerticalAlignForHeight( y, align, h );

	request->drawStretchPic( x, y, w, h, tc[0][0], tc[1][0], tc[0][1], tc[1][1], color, shader );
}

static void drawPlayerBars( Draw2DRequest *request, qfontface_s *font, int requestedX, int requestedY, const vec4_t color, int health, int armor ) {
	const int barwidth     = (int)std::round( (float)SCR_strWidth( "_", font, 0 ) * v_showPlayerNames_barWidth.get() ); // size of 8 characters
	const int barheight    = (int)std::round( (float)SCR_FontHeight( font ) * 0.25 ); // quarter of a character height
	const int barseparator = (int)barheight / 3;

	// soften the alpha of the box color
	const vec4_t tmpcolor = { color[0], color[1], color[2], 0.4f * color[3] };

	// we have to align first, then draw as left top, cause we want the bar to grow from left to right
	const int x = CG_HorizontalAlignForWidth( requestedX, ALIGN_CENTER_TOP, barwidth );
	int y = CG_VerticalAlignForHeight( requestedY, ALIGN_CENTER_TOP, barheight );

	// draw the background box
	drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight * 3, 100, 100, tmpcolor, NULL );

	y += barseparator;

	if( health > 100 ) {
		const vec4_t alphagreen   = { 0.0f, 1.0f, 0.0f, 1.0f };
		const vec4_t alphamagenta = { 1.0f, 0.0f, 1.0f, 1.0f };
		drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, 100, 100, alphagreen, NULL );
		drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, health - 100, 100, alphamagenta, NULL );
	} else {
		if( health <= 33 ) {
			const vec4_t alphared { 1.0f, 0.0f, 0.0f, color[3] };
			drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, health, 100, alphared, NULL );
		} else if( health <= 66 ) {
			const vec4_t alphayellow { 1.0f, 1.0f, 0.0f, color[3] };
			drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, health, 100, alphayellow, NULL );
		} else {
			const vec4_t alphagreen { 0.0f, 1.0f, 0.0f, color[3] };
			drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, health, 100, alphagreen, NULL );
		}
	}

	if( armor ) {
		y += barseparator + barheight;
		const vec4_t alphagrey { 0.85, 0.85, 0.85, color[3] };
		drawBar( request, x, y, ALIGN_LEFT_TOP, barwidth, barheight, armor, 150, alphagrey, NULL );
	}
}

static void drawNamesAndBeacons( Draw2DRequest *request, ViewState *viewState, std::optional<float> miniviewScale ) {
	const int showNamesValue           = v_showPlayerNames.get();
	const int showTeamInfoValue        = v_showTeamInfo.get();
	const int showPointedPlayerValue   = v_showPointedPlayer.get();
	const int povTeam                  = viewState->predictedPlayerState.stats[STAT_TEAM];
	const bool shouldCareOfPlayerNames = showNamesValue || showPointedPlayerValue;
	const bool shouldCareOfTeamBeacons = showTeamInfoValue && povTeam > TEAM_PLAYERS;

	if( !shouldCareOfPlayerNames && !shouldCareOfTeamBeacons ) {
		return;
	}

	CG_UpdatePointedNum( viewState );

	int savedCoords[MAX_CLIENTS][2];
	float playerNameAlphaValues[MAX_CLIENTS] {};
	bool shouldDrawPlayerName[MAX_CLIENTS] {};
	bool shouldDrawTeamBeacon[MAX_CLIENTS] {};
	bool hasNamesToDraw   = false;
	bool hasBeaconsToDraw = false;

	struct LazyCachedTrace {
		const ViewState *const m_viewState;
		const centity_t *const m_cent;
		bool m_performedTest { false };
		bool m_passedTest { false };
		[[nodiscard]]
		bool passes() {
			if( !m_performedTest ) {
				trace_t trace;
				// TODO: Should it be a visual trace?
				CG_Trace( &trace, m_viewState->view.origin, vec3_origin, vec3_origin, m_cent->ent.origin,
						  (int)m_viewState->predictedPlayerState.POVnum, MASK_OPAQUE );
				m_passedTest    = trace.fraction == 1.0f || trace.ent == m_cent->current.number;
				m_performedTest = true;
			}
			return m_passedTest;
		}
	};

	const refdef_t &refdef = viewState->view.refdef;
	for( int i = 0; i < cggs->maxclients; i++ ) {
		const centity_t *const cent = &cg_entities[i + 1];
		bool mayBeProjectedToScreen = false;
		if( cgs.clientInfo[i].name[0] && !viewState->isViewerEntity( i + 1 ) ) {
			if( cent->serverFrame == cg.frame.serverFrame ) {
				if( cent->current.modelindex && cent->current.team != TEAM_SPECTATOR ) {
					if( cent->current.solid && cent->current.solid != SOLID_BMODEL ) {
						mayBeProjectedToScreen = true;
					}
				}
			}
		}

		if( mayBeProjectedToScreen ) {
			vec3_t drawOrigin;
			VectorSet( drawOrigin, cent->ent.origin[0], cent->ent.origin[1], cent->ent.origin[2] + playerbox_stand_maxs[2] + 16 );

			vec3_t dir;
			VectorSubtract( drawOrigin, viewState->view.origin, dir );

			if( const float viewDot = DotProduct( dir, &viewState->view.axis[AXIS_FORWARD] ); viewDot > 0.0f ) {
				// find the 3d point in 2d screen
				// TODO: Project on demand, use some kind of cache
				vec2_t tmpCoords { 0.0f, 0.0f };
				// May legitimately out of viewport bounds.
				// The actual clipping relies on scissor test.
				// TODO: Calculate the mvp matrix before the loop
				if( RF_TransformVectorToViewport( &refdef, drawOrigin, tmpCoords ) ) {
					const auto coordX = (int)std::round( tmpCoords[0] );
					const auto coordY = (int)std::round( tmpCoords[1] );
					LazyCachedTrace trace { .m_viewState = viewState, .m_cent = cent };
					if( shouldCareOfPlayerNames ) {
						bool isAKindOfPlayerWeNeed = false;
						if( showNamesValue == 2 && cent->current.team == povTeam ) {
							isAKindOfPlayerWeNeed = true;
						} else if( showNamesValue ) {
							isAKindOfPlayerWeNeed = true;
						} else if( cent->current.number == viewState->pointedNum ) {
							isAKindOfPlayerWeNeed = true;
						}
						if( isAKindOfPlayerWeNeed ) {
							const float dist = VectorNormalize( dir ) * viewState->view.fracDistFOV;
							if( !( cent->current.effects & EF_PLAYER_HIDENAME ) ) {
								float nameAlpha = 0.0f;
								if( cent->current.number != viewState->pointedNum ) {
									const float fadeFrac = ( v_showPlayerNames_zfar.get() - dist ) / ( v_showPlayerNames_zfar.get() * 0.25f );
									nameAlpha = v_showPlayerNames_alpha.get() * wsw::clamp( fadeFrac, 0.0f, 1.0f );
									if( miniviewScale != std::nullopt ) {
										nameAlpha *= viewDot * viewDot;
									}
								} else {
									const float fadeFrac = (float)( viewState->pointRemoveTime - cg.time ) / 150.0f;
									nameAlpha = wsw::clamp( fadeFrac, 0.0f, 1.0f );
								}
								if( nameAlpha > 0.001f ) {
									// Do the expensive trace test last
									if( trace.passes() ) {
										// We fully rely on scissor test for clipping names out.
										// TODO: Adjust the name position like it's performed for beacons?
										shouldDrawPlayerName[i]  = true;
										playerNameAlphaValues[i] = nameAlpha;
										hasNamesToDraw           = true;
									}
								}
							}
						}
						// if not the pointed player we are done
					}
					if( shouldCareOfTeamBeacons ) {
						if( cent->current.team == povTeam ) {
							// Clip the beacon as a point.
							// Note that we adjust its coords while drawing so it's always fully displayed if drawn.
							if( coordX >= refdef.x && coordX <= refdef.x + refdef.width ) {
								if( coordY >= refdef.y && coordY <= refdef.y + refdef.height ) {
									// Do the expensive trace test last
									if( !trace.passes() ) {
										shouldDrawTeamBeacon[i] = true;
										hasBeaconsToDraw        = true;
									}
								}
							}
						}
					}
					if( shouldDrawPlayerName[i] | shouldDrawTeamBeacon[i] ) {
						savedCoords[i][0] = coordX;
						savedCoords[i][1] = coordY;
					}
				}
			}
		}
	}

	if( hasBeaconsToDraw ) {
		vec4_t color;
		CG_TeamColor( viewState->predictedPlayerState.stats[STAT_TEAM], color );
		for( int i = 0; i < cggs->maxclients; ++i ) {
			if( shouldDrawTeamBeacon[i] ) {
				const int picSize = ( miniviewScale != std::nullopt ? 12 : 18 ) * cgs.vidHeight / 600;
				assert( refdef.width > picSize && refdef.height > picSize );
				const int picX = wsw::clamp( savedCoords[i][0] - picSize / 2, refdef.x, refdef.x + refdef.width - picSize );
				const int picY = wsw::clamp( savedCoords[i][1] - picSize / 2, refdef.y, refdef.y + refdef.height - picSize );
				shader_s *shader;
				if( const centity_t *const cent = &cg_entities[i + 1]; cent->current.effects & EF_CARRIER ) {
					shader = cgs.media.shaderTeamCarrierIndicator;
				} else {
					shader = cgs.media.shaderTeamMateIndicator;
				}
				request->drawStretchPic( picX, picY, picSize, picSize, 0.0f, 0.0f, 1.0f, 1.0f, color, shader );
			}
		}
	}

	if( hasNamesToDraw ) {
		qfontface_s *font;
		if( miniviewScale != std::nullopt ) {
			// TODO: Identify HUD miniviews by enums
			if( *miniviewScale < 0.15f ) {
				font = cgs.fontPlayerNameTiny;
			} else {
				font = cgs.fontPlayerNameSmall;
			}
		} else {
			font = cgs.fontPlayerNameLarge;
		}
		for( int i = 0; i < cggs->maxclients; ++i ) {
			const vec4_t color { 1.0f, 1.0f, 1.0f, playerNameAlphaValues[i] };
			const int requestedX = savedCoords[i][0];
			const int requestedY = savedCoords[i][1];
			SCR_DrawString( request, requestedX, requestedY, ALIGN_CENTER_BOTTOM, cgs.clientInfo[i].name, font, color );
			if( showPointedPlayerValue && ( i + 1 == viewState->pointedNum ) ) {
				// pointed player hasn't a health value to be drawn, so skip adding the bars
				if( viewState->pointedHealth && v_showPlayerNames_barWidth.get() > 0 ) {
					drawPlayerBars( request, font, requestedX, requestedY, color, viewState->pointedHealth, viewState->pointedArmor );
				}
			}
		}
	}
}

static void checkCrosshairNameVar( StringConfigVar *var, bool initial, const wsw::StringSpanStorage<unsigned, unsigned> &available ) {
	if( const wsw::StringView actualValue = var->get(); !actualValue.empty() ) {
		bool isValid = false;
		for( const wsw::StringView &value: available ) {
			if( value.equalsIgnoreCase( actualValue ) ) {
				isValid = true;
				break;
			}
		}
		// This also covers the case when the "default" string is not found
		// TODO: How to work with declared config vars with dynamic set of values
		if( !isValid ) {
			var->setImmediately( ( initial && !available.empty() ) ? available.front() : wsw::StringView() );
		}
	}
}

void CG_CheckSharedCrosshairState( bool initial ) {
	checkCrosshairNameVar( &v_strongCrosshairName, initial, getStrongCrosshairFiles() );

	const wsw::StringSpanStorage<unsigned, unsigned> &regularCrosshairFiles = getRegularCrosshairFiles();
	checkCrosshairNameVar( &v_crosshairName, initial, regularCrosshairFiles );
	for( StringConfigVar &var: crosshairSettingsTracker.m_nameVarsForWeapons ) {
		checkCrosshairNameVar( &var, initial, regularCrosshairFiles );
	}
}

static void drawCrosshair( Draw2DRequest *request, int weapon, int fireMode, std::optional<float> miniviewScale, const ViewState *viewState ) {
	const UnsignedConfigVar *sizeVar;
	const ColorConfigVar *colorVar;
	const StringConfigVar *nameVar;
	if( fireMode == FIRE_MODE_STRONG ) {
		sizeVar  = &v_strongCrosshairSize;
		colorVar = &v_strongCrosshairColor;
		nameVar  = &v_strongCrosshairName;
	} else if( v_separateWeaponCrosshairSettings.get() ) {
		sizeVar  = &crosshairSettingsTracker.m_sizeVarsForWeapons[weapon - 1];
		colorVar = &crosshairSettingsTracker.m_colorVarsForWeapons[weapon - 1];
		nameVar  = &crosshairSettingsTracker.m_nameVarsForWeapons[weapon - 1];
	} else {
		sizeVar  = &v_crosshairSize;
		colorVar = &v_crosshairColor;
		nameVar  = &v_crosshairName;
	}

	std::optional<std::pair<shader_s *, std::pair<unsigned, unsigned>>> materialAndDimensions;
	if( const wsw::StringView name = nameVar->get(); !name.empty() ) {
		// Note: We use a fixed downscale factor as changing image size is very expensive and everything relies on caching
		const unsigned sizeVarValue = sizeVar->get();
		const bool isForMiniview    = miniviewScale != std::nullopt;
		const unsigned chosenSize   = ( isForMiniview ? ( sizeVarValue / 2 ) : sizeVarValue ) * cgs.pixelRatio;
		if( fireMode == FIRE_MODE_STRONG ) {
			materialAndDimensions = getStrongCrosshairMaterial( name, isForMiniview, chosenSize );
		} else {
			materialAndDimensions = getRegularCrosshairMaterial( name, isForMiniview, chosenSize );
		}
	}

	if( materialAndDimensions ) {
		auto [material, dimensions]    = *materialAndDimensions;
		auto [imageWidth, imageHeight] = dimensions;

		const int64_t damageTime      = wsw::max( cgs.snapFrameTime, v_crosshairDamageTime.get() );
		const int64_t damageTimestamp = viewState->crosshairDamageTimestamp;

		int encodedColorToUse;
		if( damageTimestamp >= 0 && damageTimestamp <= cg.time && damageTimestamp + damageTime > cg.time ) {
			encodedColorToUse = v_crosshairDamageColor.get();
		} else {
			encodedColorToUse = colorVar->get();
		}

		const vec4_t color {
			COLOR_R( encodedColorToUse ) * ( 1.0f / 255.0f ),
			COLOR_G( encodedColorToUse ) * ( 1.0f / 255.0f ),
			COLOR_B( encodedColorToUse ) * ( 1.0f / 255.0f ),
			1.0f
		};

		const int x = viewState->view.refdef.x + ( viewState->view.refdef.width - (int)imageWidth ) / 2;
		const int y = viewState->view.refdef.y + ( viewState->view.refdef.height - (int)imageHeight ) / 2;

		request->drawStretchPic( x, y, (int)imageWidth, (int)imageHeight, 0, 0, 1, 1, color, material );
	}
}

static void CG_DrawCrosshair( Draw2DRequest *request, ViewState *viewState, std::optional<float> miniviewScale ) {
	const player_state_t *playerState;
	if( viewState->view.playerPrediction ) {
		playerState = &viewState->predictFromPlayerState;
	} else {
		playerState = &viewState->predictedPlayerState;
	}
	if( const auto weapon = playerState->stats[STAT_WEAPON] ) {
		if( const auto *const firedef = GS_FiredefForPlayerState( cggs, playerState, weapon ) ) {
			if( firedef->fire_mode == FIRE_MODE_STRONG ) {
				::drawCrosshair( request, weapon, FIRE_MODE_STRONG, miniviewScale, viewState );
			}
			::drawCrosshair( request, weapon, FIRE_MODE_WEAK, miniviewScale, viewState );
		}
	}
}

static void CG_ViewWeapon_UpdateProjectionSource( const vec3_t hand_origin, const mat3_t hand_axis,
												  const vec3_t weap_origin, const mat3_t weap_axis, ViewState *viewState ) {
	orientation_t tag_weapon;

	VectorCopy( vec3_origin, tag_weapon.origin );
	Matrix3_Copy( axis_identity, tag_weapon.axis );

	// move to tag_weapon
	CG_MoveToTag( tag_weapon.origin, tag_weapon.axis, hand_origin, hand_axis, weap_origin, weap_axis );

	const weaponinfo_t *const weaponInfo = CG_GetWeaponInfo( viewState->weapon.weapon );
	orientation_t *const tag_result      = &viewState->weapon.projectionSource;

	// move to projectionSource tag
	if( weaponInfo ) {
		VectorCopy( vec3_origin, tag_result->origin );
		Matrix3_Copy( axis_identity, tag_result->axis );
		CG_MoveToTag( tag_result->origin, tag_result->axis,
					  tag_weapon.origin, tag_weapon.axis,
					  weaponInfo->tag_projectionsource.origin, weaponInfo->tag_projectionsource.axis );
	} else {
		// fall back: copy gun origin and move it front by 16 units and 8 up
		VectorCopy( tag_weapon.origin, tag_result->origin );
		Matrix3_Copy( tag_weapon.axis, tag_result->axis );
		VectorMA( tag_result->origin, 16, &tag_result->axis[AXIS_FORWARD], tag_result->origin );
		VectorMA( tag_result->origin, 8, &tag_result->axis[AXIS_UP], tag_result->origin );
	}
}

static void CG_ViewWeapon_AddAngleEffects( vec3_t angles, ViewState *viewState ) {
	if( !viewState->view.drawWeapon ) {
		return;
	}

	if( v_gun.get() && v_gunBob.get() ) {
		// gun angles from bobbing
		if( viewState->bobCycle & 1 ) {
			angles[ROLL] -= viewState->xyspeed * viewState->bobFracSin * 0.012;
			angles[YAW] -= viewState->xyspeed * viewState->bobFracSin * 0.006;
		} else {
			angles[ROLL] += viewState->xyspeed * viewState->bobFracSin * 0.012;
			angles[YAW] += viewState->xyspeed * viewState->bobFracSin * 0.006;
		}
		angles[PITCH] += viewState->xyspeed * viewState->bobFracSin * 0.012;

		// gun angles from delta movement
		for( int i = 0; i < 3; i++ ) {
			float delta = ( viewState->oldSnapPlayerState.viewangles[i] - viewState->snapPlayerState.viewangles[i] ) * cg.lerpfrac;
			if( delta > 180 ) {
				delta -= 360;
			}
			if( delta < -180 ) {
				delta += 360;
			}
			Q_clamp( delta, -45, 45 );


			if( i == YAW ) {
				angles[ROLL] += 0.001 * delta;
			}
			angles[i] += 0.002 * delta;
		}

		// gun angles from kicks
		CG_AddKickAngles( angles, viewState );
	}
}

static int CG_ViewWeapon_baseanimFromWeaponState( int weaponState ) {
	int anim;

	switch( weaponState ) {
		case WEAPON_STATE_ACTIVATING:
			anim = WEAPMODEL_WEAPONUP;
			break;

		case WEAPON_STATE_DROPPING:
			anim = WEAPMODEL_WEAPDOWN;
			break;

		case WEAPON_STATE_FIRING:
		case WEAPON_STATE_REFIRE:
		case WEAPON_STATE_REFIRESTRONG:

			/* fall through. Activated by event */
		case WEAPON_STATE_POWERING:
		case WEAPON_STATE_COOLDOWN:
		case WEAPON_STATE_RELOADING:
		case WEAPON_STATE_NOAMMOCLICK:

			/* fall through. Not used */
		default:
		case WEAPON_STATE_READY:
			if( v_gunBob.get() ) {
				anim = WEAPMODEL_STANDBY;
			} else {
				anim = WEAPMODEL_NOANIM;
			}
			break;
	}

	return anim;
}

void CG_ViewWeapon_RefreshAnimation( cg_viewweapon_t *viewweapon, ViewState *viewState ) {
	bool nolerp = false;

	// if the pov changed, or weapon changed, force restart
	if( viewweapon->POVnum != viewState->predictedPlayerState.POVnum ||
		viewweapon->weapon != viewState->predictedPlayerState.stats[STAT_WEAPON] ) {
		nolerp = true;
		viewweapon->eventAnim = 0;
		viewweapon->eventAnimStartTime = 0;
		viewweapon->baseAnim = 0;
		viewweapon->baseAnimStartTime = 0;
	}

	viewweapon->POVnum = viewState->predictedPlayerState.POVnum;
	viewweapon->weapon = viewState->predictedPlayerState.stats[STAT_WEAPON];

	// hack cause of missing animation config
	if( viewweapon->weapon == WEAP_NONE ) {
		viewweapon->ent.frame = viewweapon->ent.oldframe = 0;
		viewweapon->ent.backlerp = 0.0f;
		viewweapon->eventAnim = 0;
		viewweapon->eventAnimStartTime = 0;
		return;
	}

	const int baseAnim = CG_ViewWeapon_baseanimFromWeaponState( viewState->predictedPlayerState.weaponState );
	const weaponinfo_t *weaponInfo = CG_GetWeaponInfo( viewweapon->weapon );

	// Full restart
	if( !viewweapon->baseAnimStartTime ) {
		viewweapon->baseAnim = baseAnim;
		viewweapon->baseAnimStartTime = cg.time;
		nolerp = true;
	}

	// base animation changed?
	if( baseAnim != viewweapon->baseAnim ) {
		viewweapon->baseAnim = baseAnim;
		viewweapon->baseAnimStartTime = cg.time;
	}

	int curframe = 0;
	float framefrac;
	// if a eventual animation is running override the baseAnim
	if( viewweapon->eventAnim ) {
		if( !viewweapon->eventAnimStartTime ) {
			viewweapon->eventAnimStartTime = cg.time;
		}

		framefrac = GS_FrameForTime( &curframe, cg.time, viewweapon->eventAnimStartTime, weaponInfo->frametime[viewweapon->eventAnim],
									 weaponInfo->firstframe[viewweapon->eventAnim], weaponInfo->lastframe[viewweapon->eventAnim],
									 weaponInfo->loopingframes[viewweapon->eventAnim], false );

		if( curframe >= 0 ) {
			goto setupframe;
		}

		// disable event anim and fall through
		viewweapon->eventAnim = 0;
		viewweapon->eventAnimStartTime = 0;
	}

	// find new frame for the current animation
	framefrac = GS_FrameForTime( &curframe, cg.time, viewweapon->baseAnimStartTime, weaponInfo->frametime[viewweapon->baseAnim],
								 weaponInfo->firstframe[viewweapon->baseAnim], weaponInfo->lastframe[viewweapon->baseAnim],
								 weaponInfo->loopingframes[viewweapon->baseAnim], true );

	if( curframe < 0 ) {
		CG_Error( "CG_ViewWeapon_UpdateAnimation(2): Base Animation without a defined loop.\n" );
	}

	setupframe:
	if( nolerp ) {
		framefrac = 0;
		viewweapon->ent.oldframe = curframe;
	} else {
		Q_clamp( framefrac, 0, 1 );
		if( curframe != viewweapon->ent.frame ) {
			viewweapon->ent.oldframe = viewweapon->ent.frame;
		}
	}

	viewweapon->ent.frame = curframe;
	viewweapon->ent.backlerp = 1.0f - framefrac;
}

void CG_ViewWeapon_StartAnimationEvent( int newAnim, ViewState *viewState ) {
	if( !viewState->view.drawWeapon ) {
		return;
	}

	viewState->weapon.eventAnim = newAnim;
	viewState->weapon.eventAnimStartTime = cg.time;
	CG_ViewWeapon_RefreshAnimation( &viewState->weapon, viewState );
}

void CG_CalcViewWeapon( cg_viewweapon_t *viewweapon, ViewState *viewState ) {
	CG_ViewWeapon_RefreshAnimation( viewweapon, viewState );

	//if( cg.view.thirdperson )
	//	return;

	const weaponinfo_t *const weaponInfo = CG_GetWeaponInfo( viewweapon->weapon );
	viewweapon->ent.model = weaponInfo->model[HAND];
	viewweapon->ent.renderfx = RF_MINLIGHT | RF_WEAPONMODEL | RF_FORCENOLOD | RF_NOSHADOW;
	viewweapon->ent.scale = 1.0f;
	viewweapon->ent.customShader = NULL;
	viewweapon->ent.customSkin = NULL;
	viewweapon->ent.rtype = RT_MODEL;
	Vector4Set( viewweapon->ent.shaderRGBA, 255, 255, 255, 255 );

	if( const float alpha = v_gunAlpha.get(); alpha < 1.0f ) {
		viewweapon->ent.renderfx |= RF_ALPHAHACK;
		viewweapon->ent.shaderRGBA[3] = alpha * 255.0f;
	}

		// calculate the entity position
#if 1
	VectorCopy( viewState->view.origin, viewweapon->ent.origin );
#else
	VectorCopy( viewState->predictedPlayerState.pmove.origin, viewweapon->ent.origin );
	viewweapon->ent.origin[2] += viewState->predictedPlayerState.viewheight;
#endif

	vec3_t gunAngles;
	vec3_t gunOffset;

	// weapon config offsets
	VectorAdd( weaponInfo->handpositionAngles, viewState->predictedPlayerState.viewangles, gunAngles );
	gunOffset[FORWARD] = v_gunZ.get() + weaponInfo->handpositionOrigin[FORWARD];
	gunOffset[RIGHT] = v_gunX.get() + weaponInfo->handpositionOrigin[RIGHT];
	gunOffset[UP] = v_gunY.get() + weaponInfo->handpositionOrigin[UP];

	// scale forward gun offset depending on fov and aspect ratio
	gunOffset[FORWARD] = gunOffset[FORWARD] * cgs.vidWidth / ( cgs.vidHeight * viewState->view.fracDistFOV ) ;

	// hand cvar offset
	float handOffset = 0.0f;
	if( cgs.demoPlaying ) {
		if( v_hand.get() == 0 ) {
			handOffset = +v_handOffset.get();
		} else if( v_hand.get() == 1 ) {
			handOffset = -v_handOffset.get();
		}
	} else {
		if( cgs.clientInfo[viewState->view.POVent - 1].hand == 0 ) {
			handOffset = +v_handOffset.get();
		} else if( cgs.clientInfo[viewState->view.POVent - 1].hand == 1 ) {
			handOffset = -v_handOffset.get();
		}
	}

	gunOffset[RIGHT] += handOffset;
	if( v_gun.get() && v_gunBob.get() ) {
		gunOffset[UP] += CG_ViewSmoothFallKick( viewState );
	}

		// apply the offsets
#if 1
	VectorMA( viewweapon->ent.origin, gunOffset[FORWARD], &viewState->view.axis[AXIS_FORWARD], viewweapon->ent.origin );
	VectorMA( viewweapon->ent.origin, gunOffset[RIGHT], &viewState->view.axis[AXIS_RIGHT], viewweapon->ent.origin );
	VectorMA( viewweapon->ent.origin, gunOffset[UP], &viewState->view.axis[AXIS_UP], viewweapon->ent.origin );
#else
	Matrix3_FromAngles( viewState->predictedPlayerState.viewangles, offsetAxis );
	VectorMA( viewweapon->ent.origin, gunOffset[FORWARD], &offsetAxis[AXIS_FORWARD], viewweapon->ent.origin );
	VectorMA( viewweapon->ent.origin, gunOffset[RIGHT], &offsetAxis[AXIS_RIGHT], viewweapon->ent.origin );
	VectorMA( viewweapon->ent.origin, gunOffset[UP], &offsetAxis[AXIS_UP], viewweapon->ent.origin );
#endif

	// add angles effects
	CG_ViewWeapon_AddAngleEffects( gunAngles, viewState );

	// finish
	AnglesToAxis( gunAngles, viewweapon->ent.axis );

	if( v_gunFov.get() > 0.0f && !viewState->predictedPlayerState.pmove.stats[PM_STAT_ZOOMTIME] ) {
		float fracWeapFOV;
		float gun_fov_x = bound( 20, v_gunFov.get(), 160 );
		float gun_fov_y = CalcFov( gun_fov_x, viewState->view.refdef.width, viewState->view.refdef.height );

		AdjustFov( &gun_fov_x, &gun_fov_y, cgs.vidWidth, cgs.vidHeight, false );
		fracWeapFOV = tan( gun_fov_x * ( M_PI / 180 ) * 0.5f ) / viewState->view.fracDistFOV;

		VectorScale( &viewweapon->ent.axis[AXIS_FORWARD], fracWeapFOV, &viewweapon->ent.axis[AXIS_FORWARD] );
	}

	orientation_t tag;
	// if the player doesn't want to view the weapon we still have to build the projection source
	if( CG_GrabTag( &tag, &viewweapon->ent, "tag_weapon" ) ) {
		CG_ViewWeapon_UpdateProjectionSource( viewweapon->ent.origin, viewweapon->ent.axis, tag.origin, tag.axis, viewState );
	} else {
		CG_ViewWeapon_UpdateProjectionSource( viewweapon->ent.origin, viewweapon->ent.axis, vec3_origin, axis_identity, viewState );
	}
}

void CG_AddViewWeapon( cg_viewweapon_t *viewweapon, DrawSceneRequest *drawSceneRequest, ViewState *viewState ) {
	if( !viewState->view.drawWeapon || viewweapon->weapon == WEAP_NONE ) {
		return;
	}

	// update the other origins
	VectorCopy( viewweapon->ent.origin, viewweapon->ent.origin2 );
	VectorCopy( cg_entities[viewweapon->POVnum].ent.lightingOrigin, viewweapon->ent.lightingOrigin );

	CG_AddColoredOutLineEffect( &viewweapon->ent, cg.effects, 0, 0, 0, viewweapon->ent.shaderRGBA[3], viewState );
	CG_AddEntityToScene( &viewweapon->ent, drawSceneRequest );
	CG_AddShellEffects( &viewweapon->ent, cg.effects, drawSceneRequest );

	int64_t flash_time = 0;
	if( v_weaponFlashes.get() == 2 ) {
		flash_time = cg_entPModels[viewweapon->POVnum].flash_time;
	}

	orientation_t tag;
	if( CG_GrabTag( &tag, &viewweapon->ent, "tag_weapon" ) ) {
		// add attached weapon
		CG_AddWeaponOnTag( &viewweapon->ent, &tag, viewweapon->weapon, cg.effects | EF_OUTLINE,
						   false, nullptr, flash_time, cg_entPModels[viewweapon->POVnum].barrel_time, drawSceneRequest, viewState );
	}
}
