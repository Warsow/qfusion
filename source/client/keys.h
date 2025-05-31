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

#ifndef WSW_43c64bed_a1ec_442c_a8cb_2fa107edfa89_H
#define WSW_43c64bed_a1ec_442c_a8cb_2fa107edfa89_H

#include <common/q_keycodes.h>
#include <common/wswpodvector.h>
#include <common/wswstaticvector.h>

template <typename> class SingletonHolder;

namespace wsw::cl {

class KeyBindingsSystem {
	template <typename> friend class ::SingletonHolder;

	class KeysAndNames {
		struct Entry {
			Entry *next;
			const wsw::StringView name;
			const int key;
		};

		struct PrintableKeyName {
			char data[2];

			[[nodiscard]]
			auto asView() const -> wsw::StringView {
				return wsw::StringView( data, 1, wsw::StringView::ZeroTerminated );
			}
		};

		static constexpr unsigned kNumPrintableKeys = 127u - 33u;
		PrintableKeyName m_printableKeyNames[kNumPrintableKeys];

		// Exactly as needed... (the number of named keys is known by design)
		// Use the minimal capacity that does not trigger an overflow assertion.
		wsw::StaticVector<Entry, 74> m_keysAndNamesStorage;

		static constexpr unsigned kMaxBindings = 256u;
		Entry *m_keyToNameTable[kMaxBindings] {};

		static constexpr size_t kNumHashBins = 17;
		Entry *m_nameToKeyHashBins[kNumHashBins] {};

		void addKeyName( int key, const wsw::StringView &name );
	public:
		KeysAndNames();

		[[nodiscard]]
		auto getNameForKey( int key ) const -> std::optional<wsw::StringView>;
		[[nodiscard]]
		auto getKeyForName( const wsw::StringView &name ) const -> std::optional<int>;
	};

	KeysAndNames m_keysAndNames;

	static constexpr unsigned kMaxBindings = 256u;
	wsw::PodVector<char> m_bindings[kMaxBindings];

	int m_numConsoleBindings { 0 };
	bool m_isModified { false };
public:
	static void init();
	static void shutdown();
	static auto instance() -> KeyBindingsSystem *;

	void setBinding( int key, const wsw::StringView &binding );
	void unbindAll();

	[[nodiscard]]
	auto getBindingForKey( int key ) const -> std::optional<wsw::StringView>;

	[[nodiscard]]
	auto getBindingAndNameForKey( int key ) const -> std::optional<std::pair<wsw::StringView, wsw::StringView>>;

	[[nodiscard]]
	auto getNameForKey( int key ) const -> std::optional<wsw::StringView> {
		return m_keysAndNames.getNameForKey( key );
	}

	[[nodiscard]]
	auto getKeyForName( const wsw::StringView &name ) const -> std::optional<int> {
		return m_keysAndNames.getKeyForName( name );
	}

	[[nodiscard]]
	auto isConsoleBound() const { return m_numConsoleBindings > 0; }

	[[nodiscard]]
	bool getAndResetModifiedStatus() {
		bool result = m_isModified;
		m_isModified = false;
		return result;
	}
};

class KeyHandlingSystem {
	static constexpr unsigned kMaxKeys = 256;

	struct KeyState {
		unsigned repeatCounter: 31;
		bool isDown: 1;

		[[nodiscard]]
		bool isSet() const {
			return repeatCounter || isDown;
		}

		void clear() {
			repeatCounter = 0;
			isDown = false;
		}
	};

	std::optional<int64_t> m_lastMouse1ClickTime;

	KeyState m_keyStates[kMaxKeys] {};
	int m_numKeysDown { 0 };

	[[nodiscard]]
	static bool isAnAutoRepeatKey( int key );

	[[nodiscard]]
	bool updateAutoRepeatStatus( int key, bool down );

	void handleEscapeKey();

	void runSubsystemHandlers( int key, bool down, bool wasDown, int64_t time );

	void handleKeyBinding( int key, bool down, bool wasDown, int64_t time, const wsw::StringView &binding );
public:
	static void init();
	static void shutdown();
	static auto instance() -> KeyHandlingSystem *;

#ifdef _WIN32
	static auto instanceOrNull() -> KeyHandlingSystem *;
#endif

	[[nodiscard]]
	bool isAToggleConsoleKey( int key ) const;

	void handleCharEvent( int key, wchar_t ch );
	void handleKeyEvent( int key, bool down, int64_t time );
	void handleMouseEvent( int key, bool down, int64_t time );

	void clearStates();

	[[nodiscard]]
	bool isKeyDown( int key ) const {
		return (unsigned)key < kMaxKeys && m_keyStates[key].isDown;
	}
	[[nodiscard]]
	bool isAnyKeyDown() const {
		return m_numKeysDown > 0;
	}
};

}

void Key_WriteBindings( int file );

void Key_Init( void );
void Key_Shutdown( void );

#endif

