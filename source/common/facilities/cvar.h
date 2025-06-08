#ifndef CVAR_H
#define CVAR_H

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
