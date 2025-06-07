#include "exceptions.h"

#include <stdexcept>

namespace wsw {

[[noreturn]]
void failWithBadAlloc( const char *message ) {
	throw std::bad_alloc();
}

[[noreturn]]
void failWithLogicError( const char *message ) {
	throw std::logic_error( message ? message : "" );
}

[[noreturn]]
void failWithRuntimeError( const char *message ) {
	throw std::runtime_error( message ? message : "" );
}

[[noreturn]]
void failWithOutOfRange( const char *message ) {
	throw std::runtime_error( message ? message : "" );
}

[[noreturn]]
void failWithInvalidArgument( const char *message ) {
	throw std::invalid_argument( message ? message : "" );
}

}