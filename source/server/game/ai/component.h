#ifndef WSW_6ea080fc_332b_4b1a_a464_6315a731f70a_H
#define WSW_6ea080fc_332b_4b1a_a464_6315a731f70a_H

#include "ailocal.h"
#include <stdarg.h>

class AiComponent {
	char tag[64];
protected:
#ifndef _MSC_VER
	void SetTag( const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
	void Debug( const char *format, ... ) const __attribute__( ( format( printf, 2, 3 ) ) );
	[[noreturn]] void FailWith( const char *format, ... ) const __attribute__( ( format( printf, 2, 3 ) ) );
#else
	void SetTag( _Printf_format_string_ const char *format, ... );
	void Debug( _Printf_format_string_ const char *format, ... ) const;
	[[noreturn]] void FailWith( _Printf_format_string_ const char *format, ... ) const;
#endif
public:
	AiComponent() {
		// TODO: Always require providing a valid tag
		tag[0] = '\0';
	}

	const char *Tag() const { return tag; }

	virtual ~AiComponent() = default;
};

#endif
