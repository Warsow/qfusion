#ifndef WSW_854613a7_5181_42d8_bf8b_a82dbfbb154b_H
#define WSW_854613a7_5181_42d8_bf8b_a82dbfbb154b_H

namespace wsw {

[[noreturn]]
void failWithBadAlloc( const char *message = nullptr );
[[noreturn]]
void failWithLogicError( const char *message = nullptr );
[[noreturn]]
void failWithRuntimeError( const char *message = nullptr );
[[noreturn]]
void failWithOutOfRange( const char *message = nullptr );
[[noreturn]]
void failWithInvalidArgument( const char *message = nullptr );

}

#endif
