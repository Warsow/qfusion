#ifndef CVAR_H
#define CVAR_H

#include <cstdint>

typedef int cvar_flag_t;

// bit-masked cvar flags
#define CVAR_ARCHIVE        1       // set to cause it to be saved to vars.rc
#define CVAR_USERINFO       2       // added to userinfo  when changed
#define CVAR_SERVERINFO     4       // added to serverinfo when changed
#define CVAR_NOSET          8       // don't allow change from console at all,
// but can be set from the command line
#define CVAR_LATCH          16      // save changes until map restart
#define CVAR_LATCH_VIDEO    32      // save changes until video restart
#define CVAR_LATCH_SOUND    64      // save changes until video restart
#define CVAR_CHEAT          128     // will be reset to default unless cheats are enabled
#define CVAR_READONLY       256     // don't allow changing by user, ever
#define CVAR_DEVELOPER      512     // allow changing in dev builds, hide in release builds

class DeclaredConfigVar;

// nothing outside the Cvar_*() functions should access these fields!!!
typedef struct cvar_s {
	// Must be read/written using atomic ops
	volatile uint64_t modificationId;

	DeclaredConfigVar *controller;

	char *name;
	// raw string TODO: Should be an atomic shared ptr
	char *string;
	// default value
	char *dvalue;
	// for CVAR_LATCH* vars
	char *latched_string;
	cvar_flag_t flags;
	bool modified;          // set each time the cvar is changed
	float value;
	int integer;
} cvar_t;

/*

   cvar_t variables are used to hold scalar or string variables that can be changed or displayed at the console or prog code as well as accessed directly
   in C code.

   The user can access cvars from the console in three ways:
   r_draworder			prints the current value
   r_draworder 0		sets the current value to 0
   set r_draworder 0	as above, but creates the cvar if not present
   Cvars are restricted from having the same names as commands to keep this
   interface from being ambiguous.
 */

// checks if the cvar system can still be used
bool Cvar_Initialized( void );

// flag manipulation routines
static inline cvar_flag_t Cvar_FlagSet( cvar_flag_t *flags, cvar_flag_t flag );
static inline cvar_flag_t Cvar_FlagUnset( cvar_flag_t *flags, cvar_flag_t flag );
static inline cvar_flag_t Cvar_FlagsClear( cvar_flag_t *flags );
static inline bool Cvar_FlagIsSet( cvar_flag_t flags, cvar_flag_t flag );

// Medar: undefined untill used, so gcc doesn't whine
//static inline cvar_type_t	Cvar_GetType(const cvar_t *var);

// this is set each time a CVAR_USERINFO variable is changed so
// that the client knows to send it to the server
extern bool userinfo_modified;

struct CmdArgs;

class DeclaredConfigVar;

/*

   cvar_t variables are used to hold scalar or string variables that can be changed or displayed at the console or prog code as well as accessed directly
   in C code.

   The user can access cvars from the console in three ways:
   r_draworder			prints the current value
   r_draworder 0		sets the current value to 0
   set r_draworder 0	as above, but creates the cvar if not present
   Cvars are restricted from having the same names as commands to keep this
   interface from being ambiguous.
 */

cvar_t *Cvar_Get( const char *var_name, const char *value, cvar_flag_t flags, DeclaredConfigVar *controller = nullptr );
cvar_t *Cvar_Set( const char *var_name, const char *value );
cvar_t *Cvar_ForceSet( const char *var_name, const char *value );
cvar_t *Cvar_FullSet( const char *var_name, const char *value, cvar_flag_t flags, bool overwrite_flags );

void        Cvar_SetValue( const char *var_name, float value );
float       Cvar_Value( const char *var_name );
const char *Cvar_String( const char *var_name );
int     Cvar_Integer( const char *var_name );
int     Cvar_Flags( const char *var_name );
cvar_t      *Cvar_Find( const char *var_name );
void        Cvar_GetLatchedVars( cvar_flag_t flags );
void        Cvar_FixCheatVars( void );
bool    Cvar_Command( const CmdArgs & );
void        Cvar_WriteVariables( int file );
void        Cvar_PreInit( void );
void        Cvar_Init( void );
void        Cvar_Shutdown( void );
char *Cvar_Userinfo( void );
char *Cvar_Serverinfo( void );

// inlined function implementations

static inline cvar_flag_t Cvar_FlagSet( cvar_flag_t *flags, cvar_flag_t flag ) {
	return *flags |= flag;
}
static inline cvar_flag_t Cvar_FlagUnset( cvar_flag_t *flags, cvar_flag_t flag ) {
	return *flags &= ~flag;
}
static inline cvar_flag_t Cvar_FlagsClear( cvar_flag_t *flags ) {
	return *flags = 0;
}
static inline bool Cvar_FlagIsSet( cvar_flag_t flags, cvar_flag_t flag ) {
	return ( bool )( ( flags & flag ) != 0 );
}

#endif      // CVAR_H
