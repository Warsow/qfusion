/*
Copyright (C) 2023 Chasseur de bots

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

#ifndef WSW_5913da14_4804_4aeb_a0a7_18094e9c6583_H
#define WSW_5913da14_4804_4aeb_a0a7_18094e9c6583_H

#include <cstdint>
#include <cassert>
#include <new>

#include "qthreads.h"

class PipeCmd {
public:
	static constexpr unsigned kAlignment       = 8u;
	static constexpr unsigned kResultRewind    = ~0u;
	static constexpr unsigned kResultTerminate = 0u;

	virtual ~PipeCmd() = default;

	[[nodiscard]]
	virtual auto exec() -> unsigned = 0;

	template <typename T>
	[[nodiscard]]
	static constexpr auto sizeWithPadding() -> unsigned {
		return sizeof( T ) % kAlignment ? ( sizeof( T ) + kAlignment - sizeof( T ) % kAlignment ) : sizeof( T );
	}
};

#define WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, ... )                          \
	do {                                                                      \
		const unsigned numBytes = PipeCmd::sizeWithPadding<CmdClosure>();     \
		uint8_t *const mem = QBufPipe_AcquireWritableBytes( pipe, numBytes ); \
		new( mem )CmdClosure( __VA_ARGS__ );                                  \
		QBufPipe_SubmitWrittenBytes( pipe, numBytes );                        \
	} while( 0 );

inline void sendTerminateCmd( qbufPipe_s *pipe ) {
	struct CmdClosure final : public PipeCmd {
		[[nodiscard]]
		virtual auto exec() -> unsigned { return kResultTerminate; }
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe );
}

#if 1

template <typename Func>
void callOverPipe( qbufPipe_t *pipe, Func func ) {
	struct CmdClosure final : public PipeCmd {
		Func m_func;
		explicit CmdClosure( Func func ) : m_func( func ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			m_func();
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, func );
}

template <typename Func, typename Arg1>
void callOverPipe( qbufPipe_t *pipe, Func func, const Arg1 &arg1 ) {
	struct CmdClosure final : public PipeCmd {
		Func m_func;
		Arg1 m_arg1;

		CmdClosure( Func func, const Arg1 &arg1 ) : m_func( func ), m_arg1( arg1 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			m_func( m_arg1 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, func, arg1 );
}

template <typename Func, typename Arg1, typename Arg2>
void callOverPipe( qbufPipe_t *pipe, Func func, const Arg1 &arg1, const Arg2 &arg2 ) {
	struct CmdClosure final : public PipeCmd {
		Func m_func;
		Arg1 m_arg1;
		Arg2 m_arg2;

		CmdClosure( Func func, const Arg1 &arg1, const Arg2 &arg2 ) : m_func( func ), m_arg1( arg1 ), m_arg2( arg2 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			m_func( m_arg1, m_arg2 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, func, arg1, arg2 );
}

template <typename Func, typename Arg1, typename Arg2, typename Arg3>
void callOverPipe( qbufPipe_t *pipe, Func func, const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3 ) {
	struct CmdClosure final : public PipeCmd {
		Func m_func;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;

		CmdClosure( Func func, const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3 )
			: m_func( func ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			m_func( m_arg1, m_arg2, m_arg3 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, func, arg1, arg2, arg3 );
}

template <typename Func, typename Arg1, typename Arg2, typename Arg3, typename Arg4>
void callOverPipe( qbufPipe_t *pipe, Func func, const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4 ) {
	struct CmdClosure final : public PipeCmd {
		Func m_func;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;
		Arg4 m_arg4;

		CmdClosure( Func func, const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4 )
			: m_func( func ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ), m_arg4( arg4 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			m_func( m_arg1, m_arg2, m_arg3, m_arg4 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, func, arg1, arg2, arg3, arg4 );
}

#endif

template <typename HandlerObject, typename HandlerMethod>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;

		CmdClosure( HandlerObject *object, HandlerMethod method ) : m_object( object ), m_method( method ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )();
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod, const Arg1 &arg1 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1 );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1, typename Arg2>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod, const Arg1 &arg1, const Arg2 &arg2 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;
		Arg2 m_arg2;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1, const Arg2 &arg2 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ), m_arg2( arg2 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1, m_arg2 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1, arg2 );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1, typename Arg2, typename Arg3>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod,
						 const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1, m_arg2, m_arg3 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1, arg2, arg3 );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1, typename Arg2, typename Arg3, typename Arg4>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod,
						 const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;
		Arg4 m_arg4;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1,
						const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ), m_arg4( arg4 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1, m_arg2, m_arg3, m_arg4 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1, arg2, arg3, arg4 );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1, typename Arg2, typename Arg3, typename Arg4, typename Arg5>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod,
						 const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4, const Arg5 &arg5 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;
		Arg4 m_arg4;
		Arg5 m_arg5;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1, const Arg2 &arg2,
						const Arg3 &arg3, const Arg4 &arg4, const Arg5 &arg5 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ), m_arg4( arg4 ), m_arg5( arg5 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1, m_arg2, m_arg3, m_arg4, m_arg5 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1, arg2, arg3, arg4, arg5 );
}

template <typename HandlerObject, typename HandlerMethod, typename Arg1, typename Arg2, typename Arg3, typename Arg4, typename Arg5, typename Arg6>
void callMethodOverPipe( qbufPipe_t *pipe, HandlerObject *handlerObject, HandlerMethod handlerMethod,
						 const Arg1 &arg1, const Arg2 &arg2, const Arg3 &arg3, const Arg4 &arg4, const Arg5 &arg5, const Arg6 &arg6 ) {
	struct CmdClosure final : public PipeCmd {
		HandlerObject *m_object;
		HandlerMethod m_method;
		Arg1 m_arg1;
		Arg2 m_arg2;
		Arg3 m_arg3;
		Arg4 m_arg4;
		Arg5 m_arg5;
		Arg6 m_arg6;

		CmdClosure( HandlerObject *object, HandlerMethod method, const Arg1 &arg1, const Arg2 &arg2,
					const Arg3 &arg3, const Arg4 &arg4, const Arg5 &arg5, const Arg6 &arg6 )
			: m_object( object ), m_method( method ), m_arg1( arg1 ), m_arg2( arg2 ), m_arg3( arg3 ), m_arg4( arg4 ), m_arg5( arg5 ), m_arg6( arg6 ) {}

		[[nodiscard]]
		auto exec() -> unsigned {
			( m_object->*m_method )( m_arg1, m_arg2, m_arg3, m_arg4, m_arg5, m_arg6 );
			return sizeWithPadding<CmdClosure>();
		}
	};

	WRITE_CLOSURE_WITH_ARGS_TO_PIPE( pipe, handlerObject, handlerMethod, arg1, arg2, arg3, arg4, arg5, arg6 );
}

#undef WRITE_CLOSURE_WITH_ARGS_TO_PIPE

#endif