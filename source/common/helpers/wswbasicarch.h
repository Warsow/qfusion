#ifndef WSW_23548231_6444_4343_8b23_b74fba92c51f_H
#define WSW_23548231_6444_4343_8b23_b74fba92c51f_H

#ifdef _MSC_VER
#define wsw_forceinline __forceinline
#define wsw_noinline __declspec( noinline )
#else
#define wsw_forceinline inline __attribute__( ( always_inline ) )
#define wsw_noinline __attribute__( ( noinline ) )
#endif

#endif
