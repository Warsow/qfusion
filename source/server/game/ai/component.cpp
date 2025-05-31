#include "component.h"

void AiComponent::SetTag( const char *format, ... ) {
	va_list va;
	va_start( va, format );
	(void)Q_vsnprintfz( tag, sizeof( tag ), format, va );
	va_end( va );
}

void AiComponent::Debug( const char *format, ... ) const {
	va_list va;
	va_start( va, format );
	AI_Debugv( tag, format, va );
	va_end( va );
}

void AiComponent::FailWith( const char *format, ... ) const {
	va_list va;
	va_start( va, format );
	AI_FailWithv( tag, format, va );
	va_end( va );
}
