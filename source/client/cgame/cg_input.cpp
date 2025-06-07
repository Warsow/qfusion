/*
Copyright (C) 2015 SiPlus, Chasseur de bots

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

/**
 * Warsow-specific input code.
 */

#include "cg_local.h"
#include <client/input.h>
#include <client/keys.h>
#include <client/client.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/configvars.h>
#include <common/helpers/tonum.h>
#include <common/helpers/algorithm.h>

using wsw::operator""_asView;

static FloatConfigVar v_yawSpeed( "cl_yawSpeed"_asView, { .byDefault = 140.0f } );
static FloatConfigVar v_pitchSpeed( "cl_pitchSpeed"_asView, { .byDefault = 150.0f } );
static FloatConfigVar v_angleSpeedKey( "cl_angleSpeedKey"_asView, { .byDefault = 1.5f } );

static BoolConfigVar v_run( "cl_run"_asView, { .byDefault = true, .flags = CVAR_ARCHIVE } );

static FloatConfigVar v_sensitivity( "sensitivity"_asView, { .byDefault = 3.0f, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_zoomsens( "zoomsens"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_accel( "m_accel"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );
static IntConfigVar v_accelStyle( "m_accelStyle"_asView, { .byDefault = 0, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_accelOffset( "m_accelOffset"_asView, { .byDefault = 0.0f, .min = inclusive( 0.0f ), .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_accelPow( "m_accelPow"_asView, { .byDefault = 2.0f, .min = exclusive( 1.0f ), .flags = CVAR_ARCHIVE } );
static BoolConfigVar v_filter( "m_filter"_asView, { .byDefault = false, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_pitch( "m_pitch"_asView, { .byDefault = 0.022f, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_yaw( "m_yaw"_asView, { .byDefault = 0.022f, .flags = CVAR_ARCHIVE } );
static FloatConfigVar v_sensCap( "m_sensCap"_asView, { .byDefault = 0.0f, .flags = CVAR_ARCHIVE } );

static int64_t cg_inputTimestamp;
static int cg_inputKeyboardDelta;
static float cg_inputMouseDelta;
static bool cg_inputCenterView;

static float mouse_x = 0, mouse_y = 0;
static float old_mouse_x = 0, old_mouse_y = 0;

/*
===============================================================================

KEY BUTTONS

Continuous button event tracking is complicated by the fact that two different
input sources (say, mouse button 1 and the control key) can both press the
same button, but the button should only be released when both of the
pressing key have been released.

When a key event issues a button command (+forward, +attack, etc), it appends
its key number as a parameter to the command so it can be matched up with
the release.

state bit 0 is the current state of the key
state bit 1 is edge triggered on the up to down transition
state bit 2 is edge triggered on the down to up transition


Key_Event (int key, bool down, int64_t time);

===============================================================================
*/

struct CommandKeyState {
	CommandKeyState *m_next { nullptr };
	const char *m_name { nullptr };
	int64_t m_downtime { 0 }; // msec timestamp
	int m_keysHeldDown[2] { 0, 0 };   // key nums holding it down
	unsigned m_msec { 0 };    // msec down this frame
	unsigned m_state { 0 };
	static inline CommandKeyState *s_head { nullptr };

	static constexpr unsigned kDownBit     = 0x1;
	static constexpr unsigned kEdgeDownBit = 0x2;
	static constexpr unsigned kEdgeUpBit   = 0x4;

	explicit CommandKeyState( const char *name ) noexcept : m_name( name ) {
		m_next = s_head;
		s_head = this;
	}

	virtual ~CommandKeyState() = default;

	void prepareCommandNames( wsw::StaticString<32> *upName, wsw::StaticString<32> *downName ) const {
		upName->clear(), downName->clear();
		*upName << '-' << wsw::StringView( m_name );
		*downName << '+' << wsw::StringView( m_name );
	}

	virtual void registerCommandCallbacks( wsw::StringView upName, wsw::StringView downName ) = 0;

	void registerCommands() {
		wsw::StaticString<32> upName, downName;
		prepareCommandNames( &upName, &downName );
		registerCommandCallbacks( upName.asView(), downName.asView() );
	}

	void unregisterCommands() {
		wsw::StaticString<32> upName, downName;
		prepareCommandNames( &upName, &downName );
		CL_Cmd_Unregister( upName.asView() );
		CL_Cmd_Unregister( downName.asView() );
	}

	[[nodiscard]]
	bool isDown() const { return m_state & kDownBit; }

	void handleUpCmd( const CmdArgs & );
	void handleDownCmd( const CmdArgs & );
	[[nodiscard]]
	auto takeFramePressedFraction() -> float;
	[[nodiscard]]
	bool checkFrameDownState();
};

// This is a hack to generate different function pointers for registerCommandCallbacks() method.
// Command handling does not currenly accept anything besides free function pointers,
// and using std::function<> should be discouraged.
// TODO: Allow passing user pointers during command registration.

template <unsigned>
struct CommandKeyState_ : public CommandKeyState {
	explicit CommandKeyState_( const char *name_ ) noexcept : CommandKeyState( name_ ) {}
	void registerCommandCallbacks( wsw::StringView upName, wsw::StringView downName ) override {
		static CommandKeyState *myInstance = this;
		static CmdFunc upFn = []( const CmdArgs &cmdArgs ) { myInstance->handleUpCmd( cmdArgs ); };
		static CmdFunc downFn = []( const CmdArgs &cmdArgs ) { myInstance->handleDownCmd( cmdArgs ); };
		CL_Cmd_Register( upName, upFn );
		CL_Cmd_Register( downName, downFn );
	}
};

static CommandKeyState_<0> in_klook( "klook" );
static CommandKeyState_<1> in_left( "left" );
static CommandKeyState_<2> in_right( "right" );
static CommandKeyState_<3> in_forward( "forward" );
static CommandKeyState_<4> in_back( "back" );
static CommandKeyState_<5> in_lookup( "lookup" );
static CommandKeyState_<6> in_lookdown( "lookdown" );
static CommandKeyState_<7> in_moveleft( "moveleft" );
static CommandKeyState_<8> in_moveright( "moveright" );
static CommandKeyState_<9> in_strafe( "strafe" );
static CommandKeyState_<10> in_speed( "speed" );
static CommandKeyState_<11> in_use( "use" );
static CommandKeyState_<12> in_attack( "attack" );
static CommandKeyState_<13> in_up( "moveup" );
static CommandKeyState_<14> in_down( "movedown" );
static CommandKeyState_<15> in_special( "special" );
static CommandKeyState_<16> in_zoom( "zoom" );

void CommandKeyState::handleDownCmd( const CmdArgs &cmdArgs ) {
	// On parsing failure, assuming it being typed manually at the console for continuous down
	const int key = wsw::toNum<int>( cmdArgs[1] ).value_or( -1 );
	if( key != m_keysHeldDown[0] && key != m_keysHeldDown[1] ) {
		// If there are free slots for this "down" key
		if( auto end = m_keysHeldDown + 2, it = wsw::find( m_keysHeldDown, end, 0 ); it != end ) {
			*it = key;
			// If was not down
			if( !isDown() ) {
				// Save the timestamp
				if( const std::optional<int64_t> downtime = wsw::toNum<int64_t>( cmdArgs[2] ) ) {
					m_downtime = *downtime;
				} else {
					m_downtime = cg_inputTimestamp - 100;
				}
				m_state |= ( kDownBit | kEdgeDownBit );
			}
		} else {
			cgWarning() << "Three keys down for a button!";
		}
	}
}

void CommandKeyState::handleUpCmd( const CmdArgs &cmdArgs ) {
	if( const std::optional<int> key = wsw::toNum<int>( cmdArgs[1] ) ) {
		// Find slot of this key
		if( auto end = m_keysHeldDown + 2, it = wsw::find( m_keysHeldDown, end, *key ); it != end ) {
			*it = 0;
			// If it cannot longer be considered down
			if( !m_keysHeldDown[0] && !m_keysHeldDown[1] ) {
				// Must be down (should always pass?)
				if( isDown() ) {
					// save timestamp
					if( const std::optional<int64_t> uptime = wsw::toNum<int>( cmdArgs[2] ) ) {
						m_msec += *uptime - m_downtime;
					} else {
						m_msec += 10;
					}
					m_state &= ~kDownBit;
					m_state |= kEdgeUpBit;
				}
			}
		} else {
			; // key up without corresponding down (menu pass through) TODO is it reachable?
		}
	} else {
		// typed manually at the console, assume for unsticking, so clear all
		m_keysHeldDown[0] = m_keysHeldDown[1] = 0;
		m_state = kEdgeUpBit;
	}
}

auto CommandKeyState::takeFramePressedFraction() -> float {
	// Clear edges
	m_state &= kDownBit;

	auto msec = (int)m_msec;
	m_msec = 0;

	if( isDown() ) {
		// still down
		msec += (int)( cg_inputTimestamp - m_downtime );
		m_downtime = cg_inputTimestamp;
	}

	if( msec > 0 && cg_inputKeyboardDelta > 0 ) {
		return wsw::min( 1.0f, (float)msec / (float)cg_inputKeyboardDelta );
	}

	return 0;
}

bool CommandKeyState::checkFrameDownState() {
	bool result = false;
	if( m_state & ( kDownBit | kEdgeDownBit ) ) {
		result = true;
		m_state &= ~kEdgeDownBit;
	}
	return result;
}

static void CG_AddKeysViewAngles( vec3_t viewAngles ) {
	float speed = 0.001f * (float)cg_inputKeyboardDelta;
	if( in_speed.isDown() ) {
		speed *= v_angleSpeedKey.get();
	}

	if( !in_strafe.isDown() ) {
		const float yawSpeed = speed * v_yawSpeed.get();
		viewAngles[YAW] -= yawSpeed * in_right.takeFramePressedFraction();
		viewAngles[YAW] += yawSpeed * in_left.takeFramePressedFraction();
	}

	const float pitchSpeed = speed * v_pitchSpeed.get();
	if( in_klook.isDown() ) {
		viewAngles[PITCH] -= pitchSpeed * in_forward.takeFramePressedFraction();
		viewAngles[PITCH] += pitchSpeed * in_back.takeFramePressedFraction();
	}

	viewAngles[PITCH] -= pitchSpeed * in_lookup.takeFramePressedFraction();
	viewAngles[PITCH] += pitchSpeed * in_lookdown.takeFramePressedFraction();
}

static void CG_AddKeysMovement( vec3_t movement ) {
	// Checking the "crouch bug" is easy as follows:
	// if some key is kept being pressed, the taken frame fraction must be consistently equal to 1.0,
	// with first and last frames of the pressed timespan being a possible exception.

	if( in_strafe.isDown() ) {
		movement[0] += in_right.takeFramePressedFraction();
		movement[0] -= in_left.takeFramePressedFraction();
	}

	movement[0] += in_moveright.takeFramePressedFraction();
	movement[0] -= in_moveleft.takeFramePressedFraction();

	if( !in_klook.isDown() ) {
		movement[1] += in_forward.takeFramePressedFraction();
		movement[1] -= in_back.takeFramePressedFraction();
	}

	movement[2] += in_up.takeFramePressedFraction();
	if( const float down = in_down.takeFramePressedFraction(); down > movement[2] ) {
		movement[2] -= down;
	}
}

unsigned CG_GetButtonBits() {
	unsigned result = 0;

	if( in_attack.checkFrameDownState() ) {
		result |= BUTTON_ATTACK;
	}
	if( in_special.checkFrameDownState() ) {
		result |= BUTTON_SPECIAL;
	}
	if( in_use.checkFrameDownState() ) {
		result |= BUTTON_USE;
	}
	if( in_speed.isDown() ^ !v_run.get() ) {
		result |= BUTTON_WALK;
	}
	if( in_zoom.checkFrameDownState() ) {
		result |= BUTTON_ZOOM;
	}

	return result;
}

void CG_MouseMove( int mx, int my ) {
	if( v_filter.get() ) {
		mouse_x = 0.5f * ( (float)mx + old_mouse_x );
		mouse_y = 0.5f * ( (float)my + old_mouse_y );
	} else {
		mouse_x = (float)mx;
		mouse_y = (float)my;
	}

	old_mouse_x = (float)mx;
	old_mouse_y = (float)my;

	float resultingSensitivity = v_sensitivity.get();

	if( const float accel = v_accel.get(); accel != 0.0f && cg_inputMouseDelta != 0.0f ) {
		// TODO: Enum
		const int accelStyle = v_accelStyle.get();
		// QuakeLive-style mouse acceleration, ported from ioquake3
		// original patch by Gabriel Schnoering and TTimo
		if( accelStyle == 1 ) {
			const float accelOffset = v_accelOffset.get();
			assert( accelOffset > 0.0f );

			// sensitivity remains pretty much unchanged at low speeds
			// m_accel is a power value to how the acceleration is shaped
			// m_accelOffset is the rate for which the acceleration will have doubled the non accelerated amplification
			// NOTE: decouple the config cvars for independent acceleration setup along X and Y?

			const float baseX = (float)( std::abs( mx ) ) / (float)cg_inputMouseDelta;
			const float baseY = (float)( std::abs( my ) ) / (float)cg_inputMouseDelta;

			const float powerX = std::pow( baseX / accelOffset, accel );
			const float powerY = std::pow( baseY / accelOffset, accel );

			mouse_x = ( mouse_x + std::copysign( mouse_x, powerX ) * accelOffset );
			mouse_y = ( mouse_y + std::copysign( mouse_y, powerY ) * accelOffset );
		} else if( accelStyle == 2 ) {
			// ch : similar to normal acceleration with offset and variable pow mechanisms

			const float accelPow    = v_accelPow.get();
			const float accelOffset = v_accelOffset.get();
			assert( accelPow > 1.0f && accelOffset >= 0.0f );

			float rate = std::sqrt( mouse_x * mouse_x + mouse_y * mouse_y ) / (float)cg_inputMouseDelta;
			rate = std::max( 0.0f, rate - accelOffset );

			// ch : TODO sens += pow( rate * m_accel->value, m_accelPow->value - 1.0 )
			resultingSensitivity += std::pow( rate * accel, accelPow - 1.0f );

			// TODO : move sensCap outside of this branch?
			const float sensCap = v_sensCap.get();
			if( sensCap > 0.0f && resultingSensitivity > sensCap ) {
				resultingSensitivity = sensCap;
			}
		} else {
			const float rate = std::sqrt( mouse_x * mouse_x + mouse_y * mouse_y ) / (float)cg_inputMouseDelta;
			resultingSensitivity += rate * accel;
		}
	}

	resultingSensitivity *= CG_GetSensitivityScale( v_sensitivity.get(), v_zoomsens.get() );

	mouse_x *= resultingSensitivity;
	mouse_y *= resultingSensitivity;
}

static void CG_AddMouseViewAngles( vec3_t viewAngles ) {
	// add mouse X/Y movement to cmd
	if( mouse_x != 0.0f ) {
		viewAngles[YAW] -= v_yaw.get() * mouse_x;
	}
	if( mouse_y != 0.0f ) {
		viewAngles[PITCH] += v_pitch.get() * mouse_y;
	}
}

static void CG_CenterView( const CmdArgs & ) {
	cg_inputCenterView = true;
}

void CG_InitInput() {
	for( CommandKeyState *state = CommandKeyState::s_head; state; state = state->m_next ) {
		state->registerCommands();
	}

	CL_Cmd_Register( "centerview"_asView, CG_CenterView );
}

void CG_ShutdownInput() {
	for( CommandKeyState *state = CommandKeyState::s_head; state; state = state->m_next ) {
		state->unregisterCommands();
	}

	CL_Cmd_Unregister( "centerview"_asView );

	cg_inputTimestamp = 0;
	cg_inputKeyboardDelta = 0;
	cg_inputMouseDelta = 0;
	cg_inputCenterView = false;

	mouse_x = 0, mouse_y = 0;
	old_mouse_x = 0, old_mouse_y = 0;
}

void CG_AddViewAngles( vec3_t viewAngles ) {
	vec3_t deltaAngles { 0.0f, 0.0f, 0.0f };

	CG_AddKeysViewAngles( deltaAngles );
	CG_AddMouseViewAngles( deltaAngles );

	VectorAdd( viewAngles, deltaAngles, viewAngles );

	if( cg_inputCenterView ) {
		viewAngles[PITCH] = (float)-SHORT2ANGLE( getOurClientViewState()->predictedPlayerState.pmove.delta_angles[PITCH] );
		cg_inputCenterView = false;
	}
}

void CG_AddMovement( vec3_t movement ) {
	vec3_t deltaMovement { 0.0f, 0.0f, 0.0f };

	CG_AddKeysMovement( deltaMovement );

	VectorAdd( movement, deltaMovement, movement );
}

void CG_InputFrame( int64_t inputTimestamp, int keyboardDeltaMillis, float mouseDeltaMillis ) {
	cg_inputTimestamp     = inputTimestamp;
	cg_inputKeyboardDelta = keyboardDeltaMillis;
	cg_inputMouseDelta    = mouseDeltaMillis;
}

void CG_ClearInputState() {
	cg_inputKeyboardDelta = 0;
	cg_inputMouseDelta    = 0.0f;
}

void CG_GetBoundKeysString( const char *cmd, char *keys, size_t keysSize ) {
	const wsw::StringView cmdView( cmd );
	wsw::StaticString<32> keyNames[2];

	int numKeys = 0;
	const auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	// TODO: If the routine turns to be really useful,
	// implement such functionality at the bindings system level
	// in an optimized fashion instead of doing a loop over all keys here.
	for( int key = 0; key < 256; key++ ) {
		if( const std::optional<wsw::StringView> maybeBinding = bindingsSystem->getBindingForKey( key ) ) {
			if( maybeBinding->equalsIgnoreCase( cmdView ) ) {
				if( const std::optional<wsw::StringView> maybeName = bindingsSystem->getNameForKey( key ) ) {
					keyNames[numKeys].assign( maybeName->take( keysSize ) );
					numKeys++;
					if( numKeys == 2 ) {
						break;
					}
				}
			}
		}
	}

	if( numKeys == 2 ) {
		Q_snprintfz( keys, keysSize, "%s or %s", keyNames[0].data(), keyNames[1].data() );
	} else if( numKeys == 1 ) {
		Q_strncpyz( keys, keyNames[0].data(), keysSize );
	} else {
		Q_strncpyz( keys, "UNBOUND", keysSize );
	}
}
