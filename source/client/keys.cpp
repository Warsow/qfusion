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
#include "client.h"
#include "ui/uisystem.h"
#include <common/types/staticvector.h>
#include <common/types/staticstring.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/singletonholder.h>
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/cvar.h>

using wsw::operator""_asView;

/*

key up events are sent even if in console mode

*/

static bool key_initialized = false;

static cvar_t *in_debug;

static const wsw::StringView kToggleConsole( "toggleconsole"_asView );

static SingletonHolder<wsw::cl::KeyBindingsSystem> keyBindingsSystemHolder;

namespace wsw::cl {

void KeyBindingsSystem::init() {
	::keyBindingsSystemHolder.init();
}

void KeyBindingsSystem::shutdown() {
	::keyBindingsSystemHolder.shutdown();
}

auto KeyBindingsSystem::instance() -> KeyBindingsSystem * {
	return ::keyBindingsSystemHolder.instance();
}

KeyBindingsSystem::KeysAndNames::KeysAndNames() {
	for( unsigned i = 33; i < 127; ++i ) {
		char *const data = m_printableKeyNames[i - 33].data;
		data[0] = (char)i;
		data[1] = '\0';
	}

	addKeyName( K_TAB, "TAB"_asView );
	addKeyName( K_ENTER, "ENTER"_asView );
	addKeyName( K_ESCAPE, "ESCAPE"_asView );
	addKeyName( K_SPACE, "SPACE"_asView );
	addKeyName( K_CAPSLOCK, "CAPSLOCK"_asView );
	addKeyName( K_SCROLLLOCK, "SCROLLLOCK"_asView );
	addKeyName( K_SCROLLLOCK, "SCROLLOCK"_asView );
	addKeyName( K_NUMLOCK, "NUMLOCK"_asView );
	addKeyName( K_NUMLOCK, "KP_NUMLOCK"_asView );
	addKeyName( K_BACKSPACE, "BACKSPACE"_asView );
	addKeyName( K_UPARROW, "UPARROW"_asView );
	addKeyName( K_DOWNARROW, "DOWNARROW"_asView );
	addKeyName( K_LEFTARROW, "LEFTARROW"_asView );
	addKeyName( K_RIGHTARROW, "RIGHTARROW"_asView );

	addKeyName( K_LALT, "LALT"_asView );
	addKeyName( K_RALT, "RALT"_asView );
	addKeyName( K_LCTRL, "LCTRL"_asView );
	addKeyName( K_RCTRL, "RCTRL"_asView );
	addKeyName( K_LSHIFT, "LSHIFT"_asView );
	addKeyName( K_RSHIFT, "RSHIFT"_asView );

	addKeyName( K_F1, "F1"_asView );
	addKeyName( K_F2, "F2"_asView );
	addKeyName( K_F3, "F3"_asView );
	addKeyName( K_F4, "F4"_asView );
	addKeyName( K_F5, "F5"_asView );
	addKeyName( K_F6, "F6"_asView );
	addKeyName( K_F7, "F7"_asView );
	addKeyName( K_F8, "F8"_asView );
	addKeyName( K_F9, "F9"_asView );
	addKeyName( K_F10, "F10"_asView );
	addKeyName( K_F11, "F11"_asView );
	addKeyName( K_F12, "F12"_asView );
	addKeyName( K_F13, "F13"_asView );
	addKeyName( K_F14, "F14"_asView );
	addKeyName( K_F15, "F15"_asView );

	addKeyName( K_INS, "INS"_asView );
	addKeyName( K_DEL, "DEL"_asView );
	addKeyName( K_PGDN, "PGDN"_asView );
	addKeyName( K_PGUP, "PGUP"_asView );
	addKeyName( K_HOME, "HOME"_asView );
	addKeyName( K_END, "END"_asView );

	addKeyName( K_WIN, "WINKEY"_asView );
	addKeyName( K_MENU, "POPUPMENU"_asView );

	addKeyName( K_COMMAND, "COMMAND"_asView );
	addKeyName( K_OPTION, "OPTION"_asView );

	addKeyName( K_MOUSE1, "MOUSE1"_asView );
	addKeyName( K_MOUSE2, "MOUSE2"_asView );
	addKeyName( K_MOUSE3, "MOUSE3"_asView );
	addKeyName( K_MOUSE4, "MOUSE4"_asView );
	addKeyName( K_MOUSE5, "MOUSE5"_asView );
	addKeyName( K_MOUSE6, "MOUSE6"_asView );
	addKeyName( K_MOUSE7, "MOUSE7"_asView );
	addKeyName( K_MOUSE8, "MOUSE8"_asView );

	addKeyName( KP_HOME, "KP_HOME"_asView );
	addKeyName( KP_UPARROW, "KP_UPARROW"_asView );
	addKeyName( KP_PGUP, "KP_PGUP"_asView );
	addKeyName( KP_LEFTARROW, "KP_LEFTARROW"_asView );
	addKeyName( KP_5, "KP_5"_asView );
	addKeyName( KP_RIGHTARROW, "KP_RIGHTARROW"_asView );
	addKeyName( KP_END, "KP_END"_asView );
	addKeyName( KP_DOWNARROW, "KP_DOWNARROW"_asView );
	addKeyName( KP_PGDN, "KP_PGDN"_asView );
	addKeyName( KP_ENTER, "KP_ENTER"_asView );
	addKeyName( KP_INS, "KP_INS"_asView );
	addKeyName( KP_DEL, "KP_DEL"_asView );
	addKeyName( KP_STAR, "KP_STAR"_asView );
	addKeyName( KP_SLASH, "KP_SLASH"_asView );
	addKeyName( KP_MINUS, "KP_MINUS"_asView );
	addKeyName( KP_PLUS, "KP_PLUS"_asView );

	addKeyName( KP_MULT, "KP_MULT"_asView );
	addKeyName( KP_EQUAL, "KP_EQUAL"_asView );

	addKeyName( K_MWHEELUP, "MWHEELUP"_asView );
	addKeyName( K_MWHEELDOWN, "MWHEELDOWN"_asView );

	addKeyName( K_PAUSE, "PAUSE"_asView );
}

static const wsw::StringView kSemicolon( "SEMICOLON"_asView );

void KeyBindingsSystem::KeysAndNames::addKeyName( int key, const wsw::StringView &name ) {
	assert( (unsigned)key < 256u );

	m_keysAndNamesStorage.emplace_back( { nullptr, name, key } );
	auto *entry = std::addressof( m_keysAndNamesStorage.back() );
	assert( entry->name.equals( name ) && entry->key == key );

	// We actually do not require it as an argument as the constructor
	// is not currently constexpr and we do not want a runtime code bloat.
	wsw::HashedStringView hashedName( name );
	const auto binIndex = hashedName.getHash() % (uint32_t)kNumHashBins;

	// A last name wins if multiple names are supplied
	m_keyToNameTable[key] = entry;

	entry->next = m_nameToKeyHashBins[binIndex];
	m_nameToKeyHashBins[binIndex] = entry;
}

auto KeyBindingsSystem::KeysAndNames::getKeyForName( const wsw::StringView &name ) const -> std::optional<int> {
	const auto len = name.length();
	if( !len ) {
		return std::nullopt;
	}

	const char *const data = name.data();
	if( len == 1 ) {
		unsigned ch = (unsigned char)data[0];
		if( ch - 33u < kNumPrintableKeys ) {
			return (int)ch;
		}
	}

	const auto binIndex = wsw::HashedStringView( data, len ).getHash() % kNumHashBins;
	for( const auto *entry = m_nameToKeyHashBins[binIndex]; entry; entry = entry->next ) {
		if( entry->name.equalsIgnoreCase( name ) ) {
			return entry->key;
		}
	}

	return std::nullopt;
}

auto KeyBindingsSystem::KeysAndNames::getNameForKey( int key ) const -> std::optional<wsw::StringView> {
	if( (unsigned)( key - 33 ) < kNumPrintableKeys ) {
		if( key != ';' ) {
			return m_printableKeyNames[key - 33].asView();
		}
		return kSemicolon;
	}

	if( (unsigned)key < kMaxBindings ) {
		if( auto *entry = m_keyToNameTable[key] ) {
			return entry->name;
		}
	}

	return std::nullopt;
}

auto KeyBindingsSystem::getBindingForKey( int key ) const -> std::optional<wsw::StringView> {
	if( (unsigned)key < kMaxBindings ) {
		if( const auto &b = m_bindings[key]; b.size() > 1 ) {
			return wsw::StringView( b.data(), b.size() - 1, wsw::StringView::ZeroTerminated );
		}
	}
	return std::nullopt;
}

auto KeyBindingsSystem::getBindingAndNameForKey( int key ) const
	-> std::optional<std::pair<wsw::StringView, wsw::StringView>> {
	if( const auto maybeBinding = getBindingForKey( key ) ) {
		return std::make_pair( *maybeBinding, *getNameForKey( key ) );
	}
	return std::nullopt;
}

void KeyBindingsSystem::setBinding( int key, const wsw::StringView &binding ) {
	if( (unsigned)key >= kMaxBindings ) {
		return;
	}

	wsw::StringView oldView = wsw::StringView();
	if( const auto &old = m_bindings[key]; old.size() > 1 ) {
		oldView = wsw::StringView( old.data(), old.size() - 1, wsw::StringView::ZeroTerminated );
	}
	if( oldView.equalsIgnoreCase( binding ) ) {
		return;
	}

	if( oldView.equalsIgnoreCase( kToggleConsole ) ) {
		m_numConsoleBindings--;
	}

	m_bindings[key].assign( binding.data(), binding.size() );
	m_bindings[key].append( '\0' );

	if( binding.equalsIgnoreCase( kToggleConsole ) ) {
		m_numConsoleBindings++;
	}

	m_isModified = true;
}

void KeyBindingsSystem::unbindAll() {
	for( auto &s: m_bindings ) {
		s.clear();
		// TODO: Shrink to fit?
	}
	m_numConsoleBindings = 0;
	m_isModified = true;
}

}

static void Key_Unbind_f( const CmdArgs &cmdArgs ) {
	if( Cmd_Argc() != 2 ) {
		clNotice() << "unbind <key> : remove commands from a key";
		return;
	}

	auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	if( const auto maybeKey = bindingsSystem->getKeyForName( wsw::StringView( Cmd_Argv( 1 ) ) ) ) {
		bindingsSystem->setBinding( *maybeKey, wsw::StringView() );
	} else {
		clNotice() << cmdArgs[1] << "isn't a valid key";
	}
}

static void Key_Unbindall( const CmdArgs & ) {
	wsw::cl::KeyBindingsSystem::instance()->unbindAll();
}

/*
* Key_Bind_f
*/
static void Key_Bind_f( const CmdArgs &cmdArgs ) {
	const int argc = Cmd_Argc();
	if( argc < 2 ) {
		clNotice() << "bind <key> [command] : attach a command to a key";
		return;
	}

	auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	const auto maybeKey = bindingsSystem->getKeyForName( wsw::StringView( Cmd_Argv( 1 ) ) );
	if( !maybeKey ) {
		clNotice() << cmdArgs[1] << "isn't a valid key";
		return;
	}

	if( argc == 2 ) {
		if( const auto maybeBinding = bindingsSystem->getBindingForKey( *maybeKey ) ) {
			Com_Printf( "\"%s\" = \"%s\"\n", Cmd_Argv( 1 ), maybeBinding->data() );
		} else {
			Com_Printf( "\"%s\" is not bound\n", Cmd_Argv( 1 ) );
		}
		return;
	}

	// copy the rest of the command line
	wsw::StaticString<1024> cmd;
	for( int i = 2; i < argc; i++ ) {
		wsw::StringView argView( Cmd_Argv( i ) );
		if( argView.size() + cmd.size() + 1 > cmd.capacity() ) {
			clWarning() << bindingsSystem->getNameForKey( *maybeKey ).value_or( wsw::StringView() ) << ": Binding overflow";
			return;
		}
		cmd << argView;
		if( i != ( argc - 1 ) ) {
			cmd << ' ';
		}
	}

	bindingsSystem->setBinding( *maybeKey, cmd.asView() );
}

/*
* Key_WriteBindings
*
* Writes lines containing "bind key value"
*/
void Key_WriteBindings( int file ) {
	FS_Printf( file, "unbindall\r\n" );

	const auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	for( int i = 0; i < 256; i++ ) {
		if( const auto maybePair = bindingsSystem->getBindingAndNameForKey( i ) ) {
			const auto [binding, name] = *maybePair;
			assert( binding.isZeroTerminated() && name.isZeroTerminated() );
			FS_Printf( file, "bind %s \"%s\"\r\n", name.data(), binding.data() );
		}
	}
}

static void Key_Bindlist_f( const CmdArgs & ) {
	const auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	for( int i = 0; i < 256; i++ ) {
		if( const auto maybePair = bindingsSystem->getBindingAndNameForKey( i ) ) {
			const auto [binding, name] = *maybePair;
			assert( binding.isZeroTerminated() && name.isZeroTerminated() );
			Com_Printf( "%s \"%s\"", name.data(), binding.data() );
		}
	}
}

/*
* Key_Init
*/
void Key_Init() {
	assert( !key_initialized );

	wsw::cl::KeyBindingsSystem::init();
	wsw::cl::KeyHandlingSystem::init();

	//
	// register our functions
	//
	CL_Cmd_Register( "bind"_asView, Key_Bind_f );
	CL_Cmd_Register( "unbind"_asView, Key_Unbind_f );
	CL_Cmd_Register( "unbindall"_asView, Key_Unbindall );
	CL_Cmd_Register( "bindlist"_asView, Key_Bindlist_f );

	in_debug = Cvar_Get( "in_debug", "0", 0 );

	key_initialized = true;
}

void Key_Shutdown() {
	if( !key_initialized ) {
		return;
	}

	CL_Cmd_Unregister( "bind"_asView );
	CL_Cmd_Unregister( "unbind"_asView );
	CL_Cmd_Unregister( "unbindall"_asView );
	CL_Cmd_Unregister( "bindlist"_asView );

	Key_Unbindall( CmdArgs {} );

	wsw::cl::KeyHandlingSystem::shutdown();
	wsw::cl::KeyBindingsSystem::shutdown();
}

static SingletonHolder<wsw::cl::KeyHandlingSystem> keyHandlingSystemHolder;

namespace wsw::cl {

void KeyHandlingSystem::init() {
	::keyHandlingSystemHolder.init();
}

void KeyHandlingSystem::shutdown() {
	::keyHandlingSystemHolder.shutdown();
}

auto KeyHandlingSystem::instance() -> KeyHandlingSystem * {
	return ::keyHandlingSystemHolder.instance();
}

#ifdef _WIN32
auto KeyHandlingSystem::instanceOrNull() -> KeyHandlingSystem * {
	return key_initialized ? ::keyHandlingSystemHolder.instance() : nullptr;
}
#endif

bool KeyHandlingSystem::isAToggleConsoleKey( int key ) const {
	if( (unsigned)key >= kMaxKeys ) {
		return false;
	}

	const auto *const bindingsSystem = wsw::cl::KeyBindingsSystem::instance();
	if( !bindingsSystem->isConsoleBound() ) {
		return key == '`' || key == '~';
	}

	if( const auto maybeBinding = bindingsSystem->getBindingForKey( key ) ) {
		if( maybeBinding->equalsIgnoreCase( kToggleConsole ) ) {
			return true;
		}
	}
	return false;
}

void KeyHandlingSystem::handleCharEvent( int key, wchar_t ch ) {
	if( !isAToggleConsoleKey( key ) ) {
		if( !Con_HandleCharEvent( ch ) ) {
			(void)wsw::ui::UISystem::instance()->handleCharEvent( ch );
		}
	}
}

void KeyHandlingSystem::handleMouseEvent( int key, bool down, int64_t time ) {
	if( key == K_MOUSE1 ) {
		if( down ) {
			if( time && m_lastMouse1ClickTime && ( time - *m_lastMouse1ClickTime < 480 ) ) {
				m_lastMouse1ClickTime = std::nullopt;
				handleKeyEvent( key, true, time );
				handleKeyEvent( K_MOUSE1DBLCLK, true, time );
				handleKeyEvent( K_MOUSE1DBLCLK, false, time );
				return;
			} else {
				m_lastMouse1ClickTime = time;
			}
		}
	} else if( key == K_MOUSE2 || key == K_MOUSE3 ) {
		m_lastMouse1ClickTime = std::nullopt;
	}

	handleKeyEvent( key, down, time );
}

static int kAutoRepeatKeys[] = {
	K_BACKSPACE, K_DEL, K_LEFTARROW, K_RIGHTARROW, K_UPARROW, K_DOWNARROW, K_PGUP, K_PGDN
};

bool KeyHandlingSystem::isAnAutoRepeatKey( int key ) {
	if( key >= 32 && key <= 126 ) {
		return key != '`';
	}
	return wsw::contains( std::begin( kAutoRepeatKeys ), std::end( kAutoRepeatKeys ), key );
}

bool KeyHandlingSystem::updateAutoRepeatStatus( int key, bool down ) {
	if( !down ) {
		m_keyStates[key].repeatCounter = 0;
		return true;
	}

	m_keyStates[key].repeatCounter++;
	if( m_keyStates[key].repeatCounter == 1u ) {
		return true;
	}

	// TODO: Check whether UI won't handle key
	// TODO: Check whether console won't handle key
	// TODO: It is seemingly not needed at all?

	return isAnAutoRepeatKey( key );
}

void KeyHandlingSystem::handleEscapeKey() {
	if( Con_HasKeyboardFocus() ) {
		Con_ToggleConsole_f( {} );
	} else if( cls.state != CA_ACTIVE && cls.state != CA_DISCONNECTED ) {
		CL_Cbuf_AppendCommand( "disconnect\n" );
	} else {
		wsw::ui::UISystem::instance()->handleEscapeKey();
	}
}

void KeyHandlingSystem::runSubsystemHandlers( int key, bool down, bool wasDown, int64_t time ) {
	if( Con_HandleKeyEvent( key, down ) ) {
		return;
	}

	if( wsw::ui::UISystem::instance()->handleKeyEvent( key, down ) ) {
		return;
	}

	// TODO: Split Con_HandleKeyEvent() to Con_CanHandleKeyEvent() and Con_HandleKeyEvent() to reuse keys test here?
	if( cls.state == CA_ACTIVE || ( key >= K_F1 && key <= K_F15 ) || ( key >= K_MOUSE1 || key <= K_MOUSE8 ) ) {
		if( auto maybeBinding = KeyBindingsSystem::instance()->getBindingForKey( key ) ) {
			handleKeyBinding( key, down, wasDown, time, *maybeBinding );
		}
	}
}

void KeyHandlingSystem::handleKeyBinding( int key, bool down, bool wasDown, int64_t time,
										  const wsw::StringView &binding ) {
	assert( binding.isZeroTerminated() );

	if ( in_debug && in_debug->integer ) {
		Com_Printf( "key:%i down:%i time:%" PRIi64 " %s\n", key, down, time, binding.data() );
	}

	if ( binding.startsWith( '+' ) ) {
		wsw::StaticString<1024> cmd;
		if( down ) {
			cmd << binding << ' ' << key << ' ' << time << '\n';
			CL_Cbuf_AppendCommand( cmd.asView() );
		} else if( wasDown ) {
			cmd << '-' << binding.drop( 1 ) << ' ' << key << ' ' << time << '\n';
			CL_Cbuf_AppendCommand( cmd.asView() );
		}
	} else if( down ) {
		CL_Cbuf_AppendCommand( binding.data() );
		CL_Cbuf_AppendCommand( "\n" );
	}
}

void KeyHandlingSystem::handleKeyEvent( int key, bool down, int64_t time ) {
	if( (unsigned)key >= kMaxKeys ) {
		return;
	}

	if( !updateAutoRepeatStatus( key, down ) ) {
		return;
	}

#if !defined( WIN32 ) && !defined( __ANDROID__ )
	// switch between fullscreen/windowed when ALT+ENTER is pressed
	if ( key == K_ENTER && down && ( isKeyDown( K_LALT ) || isKeyDown( K_RALT ) ) ) {
		CL_Cbuf_AppendCommand( "toggle vid_fullscreen\n" );
		return;
	}
#endif

#if defined ( __MACOSX__ )
	// quit the game when Control + q is pressed
	if( key == 'q' && down && isKeyDown( K_COMMAND ) ) {
		CL_Cbuf_AppendCommand( "quit\n" );
		return;
	}
#endif

	if( isAToggleConsoleKey( key ) ) {
		if( down ) {
			Con_ToggleConsole_f( {} );
		}
		return;
	}

	// menu key is hardcoded, so the user can never unbind it
	if( key == K_ESCAPE ) {
		if( down ) {
			handleEscapeKey();
		}
		return;
	}

	const bool wasDown = m_keyStates[key].isDown;
	m_keyStates[key].isDown = down;
	if( down ) {
		if( m_keyStates[key].repeatCounter == 1u ) {
			m_numKeysDown++;
		}
	} else {
		m_numKeysDown = wsw::max( 0, m_numKeysDown - 1 );
	}

	runSubsystemHandlers( key, down, wasDown, time );
}

void KeyHandlingSystem::clearStates() {
	for( unsigned i = 0; i < kMaxKeys; ++i ) {
		if( m_keyStates[i].isSet() ) {
			handleKeyEvent( (int)i, false, 0 );
		}
		m_keyStates[i].clear();
	}
	m_numKeysDown = 0;
}

}
