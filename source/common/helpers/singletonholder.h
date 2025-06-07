#ifndef WSW_4ea8b521_0c4d_4316_9c48_bfd9aee07092_H
#define WSW_4ea8b521_0c4d_4316_9c48_bfd9aee07092_H

#include <cassert>
#include <cstdint>
#include <utility>

#ifndef WSW_ASAN_LOCK
// TODO: Eliminate duplication of this block over the codebase
#ifdef __SANITIZE_ADDRESS__
#include <sanitizer/asan_interface.h>
#define WSW_ASAN_LOCK( address, size ) ASAN_POISON_MEMORY_REGION( address, size )
#define WSW_ASAN_UNLOCK( address, size ) ASAN_UNPOISON_MEMORY_REGION( address, size )
#else
#define WSW_ASAN_LOCK( address, size ) (void)0
#define WSW_ASAN_UNLOCK( address, size ) (void)0
#endif
#endif

//! A helper for holding singleton instances that behave like any regular RAII-using class.
//! <code>init()</code> and <code>shutdown()</code> are expected to be called manually.
//! This is due to the existing lifecycle pattern of dynamic libraries.
//! \tparam T A type of a held instance.
//! \note The held object constructor and destructor are often intended to be private.
//! The line <code>template<typename> friend class SingletonHolder</code>
//! is proposed to be added to such classes. This approach is intrusive
//! but switching from non-structured ways of handling own instances
//! to these holders is intrusive too.
template <class T>
class alignas( alignof( T ) > 16 ? alignof( T ) : 16 )SingletonHolder {
protected:
	alignas( alignof( T ) > 16 ? alignof( T ) : 16 ) uint8_t m_buffer[sizeof( T )];
	bool m_initialized { false };
public:
	SingletonHolder() noexcept { // NOLINT(cppcoreguidelines-pro-type-member-init)
		WSW_ASAN_LOCK( m_buffer, sizeof( m_buffer ) );
	}

	~SingletonHolder() {
		assert( !m_initialized && "There was not an explicit shutdown() call");
		WSW_ASAN_UNLOCK( m_buffer, sizeof( m_buffer ) );
	}

	//! Must be called manually.
	//! Calling it repeatedly without making a <code>shutdown()</code> call in-between is illegal.
	//! Forwards its arguments list to a constructor of the held type.
	template <typename... Args>
	void init( Args&&... args ) {
		assert( !m_initialized && "Attempting to call init() repeatedly on an initialized instance");
		WSW_ASAN_UNLOCK( m_buffer, sizeof( m_buffer ) );
		new( m_buffer )T( std::forward<Args>( args )... );
		m_initialized = true;
	}

	 //! Returns a held instance that is assumed to be constructed (and not destroyed).
	 //! A user is forced to care about the instance lifetime, and that is a good thing in this case.
	 //! Usually held objects are really heavy-weight and important for the program logic,
	 //! so a user has to care about initialization order anyway.
	 //! \note the holder is supposed to be global, and accessing global variables
	 //! in dynamic libraries involves indirection.
	 //! A proposed usage pattern is getting an instance once and saving in a local variable
	 //! if the held instance is used involved in performance-demanding code.
	[[nodiscard]]
	auto instance() noexcept -> T * {
		assert( m_initialized );
		return (T *)( m_buffer );
	}

	//! Must be called manually.
	//! Calling it repeatedly without a prior <code>init()</code> call is legal.
	//! Once this method is called, <code>init()</code> is allowed to be called again.
	void shutdown() {
		if( m_initialized ) {
			instance()->~T();
			WSW_ASAN_LOCK( m_buffer, sizeof( m_buffer ) );
			m_initialized = false;
		}
	}
};

#endif
