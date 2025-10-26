/*
Copyright (C) 2025 vvk2212, Chasseur de bots

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

#ifndef WSW_1a245e4c_e13e_469b_aabe_f9620506705f_H
#define WSW_1a245e4c_e13e_469b_aabe_f9620506705f_H

#include <cassert>
#include <cstdlib>
#include <cstdint>
#include <utility>

namespace wsw {

// This is an optimized alternative to vector<function>.
// It uses (amortized) zero hidden allocations and provides relatively efficient memory layout.
class BaseActionTape {
protected:
	struct BaseAction {
		// TODO: Store its size inline
		virtual ~BaseAction() = default;
		virtual void moveTo( void *newMem ) = 0;

		size_t m_sizeOnTape;
	};

public:
	BaseActionTape() = default;
	~BaseActionTape();

	// TODO: Add/use some Noncopyable utility type
	BaseActionTape( const BaseActionTape & ) = delete;
	auto operator=( const BaseActionTape & ) -> BaseActionTape & = delete;
	BaseActionTape( BaseActionTape && ) = delete;
	auto operator=( BaseActionTape && ) -> BaseActionTape & = delete;


	// Note: We split exec() and clear() because exec() is allowed to throw exceptions
	void clear();
protected:
	[[nodiscard]]
	auto allocActionMem( size_t requiredSize ) -> void *;

	uint8_t *m_basePtr { nullptr };
	unsigned m_sizeInBytes { 0 };
	unsigned m_capacityInBytes { 0 };
};

// TODO: Allow returning action results as well?
template <typename... ActionArgs>
class ActionTape : public BaseActionTape {
	struct Action : public BaseAction {
		virtual void exec( ActionArgs... args ) = 0;
	};
public:
	template <typename Fn>
	void append( Fn &&fn ) {
		struct ActionImpl final : public Action {
			explicit ActionImpl( Fn &&fn ) : m_fn( std::forward<Fn>( fn ) ) {}
			void moveTo( void *newMem ) override {
				auto *newAction = new( newMem )ActionImpl( std::move( m_fn ) );
				newAction->m_sizeOnTape = this->m_sizeOnTape;
			}
			void exec( ActionArgs... args ) override {
				m_fn( args... );
			}
			Fn m_fn;
		};

		size_t requiredSize = sizeof( ActionImpl );
		// TODO: Add/use generic alignment facilities
		// TODO: Pass the required alignment to the allocation subroutine, patch sizeOnTape of the previous action?
		if( auto rem = requiredSize % sizeof( void * ) ) {
			requiredSize += sizeof( void * ) - rem;
		}

		auto *mem = allocActionMem( requiredSize );
		assert( ( (uintptr_t)mem ) % alignof( ActionImpl ) == 0 );

		auto *action         = new( mem )ActionImpl( std::forward<Fn>( fn ) );
		action->m_sizeOnTape = requiredSize;
	}

	// TODO: Allow customizing iteration instead of calling everything at once?
	void exec( ActionArgs... args ) {
		size_t offset = 0;
		while( offset < m_sizeInBytes ) {
			auto *const action = (Action *)( m_basePtr + offset );
			offset += action->m_sizeOnTape;
			action->exec( args... );
		}
		assert( offset == m_sizeInBytes );
	}
};

}

#endif