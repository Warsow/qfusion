/*
Copyright (C) 1997-2001 Id Software, Inc.

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

/*
    Copyright (C) 1999-2000  Brian Paul, All Rights Reserved.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
    BRIAN PAUL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
    AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


    The Mesa OpenGL headers were originally adapted in 2001 for dynamic OpenGL
    binding by Zephaniah E. Hull and later rewritten by Joseph Carter.  This
    version of the file is for the generation 3 DynGL code, and has been
    adapted by Joseph Carter.  He and Zeph have decided to hereby disclaim all
    Copyright of this work.  It is released to the Public Domain WITHOUT ANY
    WARRANTY whatsoever, express or implied, in the hopes that others will use
    it instead of other less-evolved hacks which usually don't work right.  ;)
*/

/*
    The following code is loosely based on DynGL code by Joseph Carter
    and Zephaniah E. Hull. Adapted by Victor Luchits for qfusion project.
*/

/*
** QGL.H
*/
#ifndef QGL_H
#define QGL_H

#if defined(_WIN32) && !defined(__WIN32__) && !defined(__CYGWIN__)
#define __WIN32__
#endif

#if defined(__WIN32__) && !defined(__CYGWIN__)
#  if (defined(_MSC_VER) || defined(__MINGW32__)) && defined(BUILD_GL32) /* tag specify we're building mesa as a DLL */
#    define GLAPI __declspec(dllexport)
#  elif (defined(_MSC_VER) || defined(__MINGW32__)) && defined(_DLL) /* tag specifying we're building for DLL runtime support */
#    define GLAPI __declspec(dllimport)
#  else /* for use with static link lib build of Win32 edition only */
#    define GLAPI extern
#  endif /* _STATIC_MESA support */
#  if defined(__MINGW32__) && defined(GL_NO_STDCALL) || defined(UNDER_CE)  /* The generated DLLs by MingW with STDCALL are not compatible with the ones done by Microsoft's compilers */
#    define GLAPIENTRY
#  else
#    define GLAPIENTRY __stdcall
#  endif
#elif defined(__CYGWIN__) && defined(USE_OPENGL32) /* use native windows opengl32 */
#  define GLAPI extern
#  define GLAPIENTRY __stdcall
#elif (defined(__GNUC__) && __GNUC__ >= 4) || (defined(__SUNPRO_C) && (__SUNPRO_C >= 0x590))
#  define GLAPI __attribute__((visibility("default")))
#  define GLAPIENTRY
#endif /* WIN32 && !CYGWIN */

/*
 * WINDOWS: Include windows.h here to define APIENTRY.
 * It is also useful when applications include this file by
 * including only glut.h, since glut.h depends on windows.h.
 * Applications needing to include windows.h with parms other
 * than "WIN32_LEAN_AND_MEAN" may include windows.h before
 * glut.h or gl.h.
 */
#if defined(_WIN32) && !defined(APIENTRY) && !defined(__CYGWIN__)
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN 1
#define NOMINMAX 1
#endif
#include <windows.h>
#endif

#ifndef GLAPI
#define GLAPI extern
#endif

#ifndef GLAPIENTRY
#define GLAPIENTRY
#endif

#ifndef APIENTRY
#define APIENTRY GLAPIENTRY
#endif

/* "P" suffix to be used for a pointer to a function */
#ifndef APIENTRYP
#define APIENTRYP APIENTRY *
#endif

#ifndef GLAPIENTRYP
#define GLAPIENTRYP GLAPIENTRY *
#endif

#ifdef QGL_VALIDATE_CALLS
#ifndef QGL_USE_CALL_WRAPPERS
#error QGL_USE_CALL_WRAPPERS must be defined if QGL_VALIDATE_CALLS is
#endif
#endif

#ifdef QGL_USE_CALL_WRAPPERS

class QGLFunc {
protected:
	void *address { nullptr };
	const char *name { nullptr };
	QGLFunc *next { nullptr };
	int counter { 0 };

	static QGLFunc *listHead;

#ifdef QGL_VALIDATE_CALLS
	const char *checkForError();
#endif

	template <class R, class Fn, typename std::enable_if<!std::is_void<R>::value>::type * = nullptr, typename... Args>
	R call( Args... args ) {
		R result = ( (Fn)address )( args... );
#ifdef QGL_VALIDATE_CALLS
		if( const char *error = checkForError() ) {
			Com_Error( ERR_FATAL, "Got %s while calling %s\n", error, name );
		}
#endif
		counter++;
		return result;
	}

	template <class R, class Fn, typename std::enable_if<std::is_void<R>::value>::type * = nullptr, typename... Args>
	R call( Args... args ) {
		( (Fn)address )( args... );
#ifdef QGL_VALIDATE_CALLS
		if( const char *error = checkForError() ) {
			Com_Error( ERR_FATAL, "Got %s while calling %s\n", error, name );
		}
#endif
		counter++;
	}
public:
	operator bool() { return address != nullptr; }
};

#define QGL_FUNC_WRAPPER_VAR( type, fname, params )         \
class QGLFunc_ ## fname : public QGLFunc {                  \
	typedef type ( APIENTRY *RawFn ) params;                \
public:                                                     \
	QGLFunc_ ## fname() {                                   \
		this->name = "q" #fname;                            \
		this->next = listHead;                              \
		listHead = this;                                    \
	}                                                       \
	template <typename... Args>                             \
	type operator()( Args... args ) {                       \
		return call<type, RawFn, nullptr>( args... );       \
	};                                                      \
	auto operator=( void *address_ ) {                      \
		this->address = address_; return *this;             \
	}                                                       \
};                                                          \
QGL_EXTERN QGLFunc_ ## fname q ## fname

#define QGL_FUNC_VAR( type, fname, params ) QGL_FUNC_WRAPPER_VAR( type, fname, params )
#define QGL_ASSIGN_VAR( var, addr ) ( var = addr )
#else
#define QGL_FUNC_VAR( type, fname, params ) QGL_EXTERN type( APIENTRY * q ## fname ) params
#define QGL_ASSIGN_VAR( var, addr ) ( var = ( decltype( var ) )addr )
#endif

typedef unsigned int	GLenum;
typedef unsigned char	GLboolean;
typedef unsigned int	GLbitfield;
typedef void		GLvoid;
typedef signed char	GLbyte;		/* 1-byte signed */
typedef short		GLshort;	/* 2-byte signed */
typedef int		GLint;		/* 4-byte signed */
typedef unsigned char	GLubyte;	/* 1-byte unsigned */
typedef unsigned short	GLushort;	/* 2-byte unsigned */
typedef unsigned int	GLuint;		/* 4-byte unsigned */
typedef int		GLsizei;	/* 4-byte signed */
typedef float		GLfloat;	/* single precision float */
typedef float		GLclampf;	/* single precision float in [0,1] */
typedef double		GLdouble;	/* double precision float */
typedef double		GLclampd;	/* double precision float in [0,1] */

typedef char GLchar;

typedef ptrdiff_t GLintptr;
typedef ptrdiff_t GLsizeiptr;
typedef unsigned short GLhalf;

typedef struct qgl_driverinfo_s {
	const char *dllname;        // default driver DLL name
	const char *dllcvarname;    // custom driver DLL cvar name, NULL if can't override driver
} qgl_driverinfo_t;

typedef enum {
	qgl_initerr_ok,
	qgl_initerr_invalid_driver,
	qgl_initerr_unknown
} qgl_initerr_t;

const qgl_driverinfo_t  *QGL_GetDriverInfo();
qgl_initerr_t           QGL_Init( const char *dllname );
qgl_initerr_t           QGL_PostInit();
void                    QGL_Shutdown();

void                    *qglGetProcAddress( const GLubyte * );
const char              *qglGetGLWExtensionsString();

#define GL_FALSE				0
#define GL_TRUE					1

#define GL_UNSIGNED_BYTE			0x1401
#define GL_UNSIGNED_SHORT			0x1403
#define GL_UNSIGNED_INT				0x1405
#define GL_FLOAT				    0x1406
#define GL_HALF_FLOAT               0x140B

#define GL_LINES				0x0001
#define GL_TRIANGLES			0x0004

#define GL_LINE					0x1B01
#define GL_FILL					0x1B02
#define GL_CW					0x0900
#define GL_CCW					0x0901
#define GL_FRONT				0x0404
#define GL_BACK					0x0405
#define GL_FRONT_AND_BACK		0x0408

#define GL_CULL_FACE				0x0B44
#define GL_POLYGON_OFFSET_FILL			0x8037

#define GL_NEVER				0x0200
#define GL_LESS					0x0201
#define GL_EQUAL				0x0202
#define GL_LEQUAL				0x0203
#define GL_GREATER				0x0204
#define GL_NOTEQUAL				0x0205
#define GL_GEQUAL				0x0206
#define GL_ALWAYS				0x0207
#define GL_DEPTH_WRITEMASK			0x0B72
#define GL_DEPTH_COMPONENT			0x1902

/* Blending */
#define GL_BLEND				    0x0BE2
#define GL_BLEND_SRC				0x0BE1
#define GL_BLEND_DST				0x0BE0
#define GL_ZERO					    0
#define GL_ONE					    1
#define GL_SRC_COLOR				0x0300
#define GL_ONE_MINUS_SRC_COLOR		0x0301
#define GL_SRC_ALPHA				0x0302
#define GL_ONE_MINUS_SRC_ALPHA		0x0303
#define GL_DST_ALPHA				0x0304
#define GL_ONE_MINUS_DST_ALPHA		0x0305
#define GL_DST_COLOR				0x0306
#define GL_ONE_MINUS_DST_COLOR		0x0307
#define GL_SRC_ALPHA_SATURATE		0x0308

#define GL_SCISSOR_TEST				0x0C11
#define GL_DEPTH_TEST				0x0B71
#define GL_STENCIL_TEST				0x0B90

#define GL_KEEP					0x1E00
#define GL_REPLACE				0x1E01
#define GL_INCR					0x1E02
#define GL_DECR					0x1E03

/* Utility */
#define GL_VENDOR				0x1F00
#define GL_RENDERER				0x1F01
#define GL_VERSION				0x1F02
#define GL_EXTENSIONS			0x1F03
#define GL_NUM_EXTENSIONS		0x821D

#define GL_NO_ERROR 				0
#define GL_INVALID_ENUM				0x0500
#define GL_INVALID_VALUE			0x0501
#define GL_INVALID_OPERATION		0x0502
#define GL_STACK_OVERFLOW			0x0503
#define GL_STACK_UNDERFLOW			0x0504
#define GL_OUT_OF_MEMORY			0x0505

#define GL_DEPTH_BUFFER_BIT			0x00000100
#define GL_STENCIL_BUFFER_BIT		0x00000400
#define GL_COLOR_BUFFER_BIT			0x00004000


#define GL_PACK_ALIGNMENT			0x0D05
#define GL_UNPACK_ALIGNMENT			0x0CF5

/* Texture mapping */
#define GL_TEXTURE_1D				    0x0DE0
#define GL_TEXTURE_2D				    0x0DE1
#define GL_TEXTURE_3D                   0x806F
#define GL_TEXTURE_WRAP_S			    0x2802
#define GL_TEXTURE_WRAP_T			    0x2803
#define GL_TEXTURE_WRAP_R               0x8072
#define GL_TEXTURE_MAG_FILTER			0x2800
#define GL_TEXTURE_MIN_FILTER			0x2801
#define GL_NEAREST_MIPMAP_NEAREST		0x2700
#define GL_NEAREST_MIPMAP_LINEAR		0x2702
#define GL_LINEAR_MIPMAP_NEAREST		0x2701
#define GL_LINEAR_MIPMAP_LINEAR			0x2703
#define GL_MAX_TEXTURE_SIZE			    0x0D33
#define GL_MAX_3D_TEXTURE_SIZE          0x8073
#define GL_NEAREST				        0x2600
#define GL_LINEAR				        0x2601
#define GL_CLAMP				        0x2900
#define GL_REPEAT				        0x2901

#define GL_NONE					    0
#define GL_BACK_LEFT				0x0402
#define GL_BACK_RIGHT				0x0403

#define GL_RED					   0x1903
#define GL_GREEN				   0x1904
#define GL_BLUE					   0x1905

#define GL_RGB					    0x1907
#define GL_RGBA					    0x1908

#define GL_RGB8					    0x8051
#define GL_RGBA8				    0x8058

#define GL_RG                       0x8227
#define GL_R8                       0x8229
#define GL_RG8                      0x822B

#define GL_ALPHA				    0x1906
#define GL_LUMINANCE				0x1909
#define GL_LUMINANCE_ALPHA			0x190A

/*
** extension constants
*/

#ifndef GL_UNSIGNED_SHORT_4_4_4_4
#define GL_UNSIGNED_SHORT_4_4_4_4                           0x8033
#define GL_UNSIGNED_SHORT_5_5_5_1                           0x8034
#define GL_UNSIGNED_SHORT_5_6_5                             0x8363
#endif

#define GL_TEXTURE0                                     0x84C0
#define GL_MAX_TEXTURE_IMAGE_UNITS                      0x8872

#define GL_COMPRESSED_RGB                               0x84ED
#define GL_COMPRESSED_RGBA                              0x84EE
#define GL_COMPRESSED_RED                               0x8225
#define GL_COMPRESSED_RG                                0x8226

#define GL_ETC1_RGB8_OES                                    0x8D64

#define GL_TEXTURE_MAX_ANISOTROPY_EXT                       0x84FE
#define GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT                   0x84FF

#define GL_CLAMP_TO_EDGE                                    0x812F

#define GL_ARRAY_BUFFER                                 0x8892
#define GL_ELEMENT_ARRAY_BUFFER                         0x8893
#define GL_STATIC_DRAW                                  0x88E4
#define GL_DYNAMIC_DRAW                                 0x88E8

#define GL_TEXTURE_CUBE_MAP                             0x8513
#define GL_TEXTURE_CUBE_MAP_POSITIVE_X                  0x8515
#define GL_MAX_CUBE_MAP_TEXTURE_SIZE                    0x851C

#define GL_BGR_EXT                                          0x80E0
#define GL_BGRA_EXT                                         0x80E1

#define GL_TEXTURE_SWIZZLE_RGBA           0x8E46

#define GL_COMPILE_STATUS                        0x8B81
#define GL_LINK_STATUS                           0x8B82
#define GL_VALIDATE_STATUS                       0x8B83
#define GL_INFO_LOG_LENGTH                       0x8B84

#define GL_VERTEX_SHADER                                0x8B31
#define GL_MAX_VERTEX_UNIFORM_COMPONENTS                0x8B4A
#define GL_MAX_VARYING_FLOATS                           0x8B4B
#define GL_MAX_VERTEX_ATTRIBS                           0x8869
#define GL_MAX_TEXTURE_IMAGE_UNITS                      0x8872

#define GL_FRAGMENT_SHADER                              0x8B30
#define GL_MAX_FRAGMENT_UNIFORM_COMPONENTS              0x8B49
#define GL_MAX_TEXTURE_COORDS                           0x8871
#define GL_MAX_TEXTURE_IMAGE_UNITS                      0x8872
#define GL_FRAGMENT_SHADER_DERIVATIVE_HINT              0x8B8B

#define GL_SHADING_LANGUAGE_VERSION                     0x8B8C

#define GL_DEPTH_COMPONENT16                                0x81A5
#define GL_DEPTH_COMPONENT24                                0x81A6
#define GL_DEPTH_COMPONENT32                                0x81A7
#define GL_TEXTURE_DEPTH_SIZE                               0x884A
#define GL_DEPTH_TEXTURE_MODE                               0x884B

#define GL_DEPTH_TEXTURE_MODE_ARB                           0x884B
#define GL_TEXTURE_COMPARE_MODE_ARB                         0x884C
#define GL_TEXTURE_COMPARE_FUNC_ARB                         0x884D
#define GL_COMPARE_R_TO_TEXTURE_ARB                         0x884E
#define GL_TEXTURE_COMPARE_FAIL_VALUE_ARB                   0x80BF

#define GL_INVALID_FRAMEBUFFER_OPERATION                0x0506
#define GL_MAX_RENDERBUFFER_SIZE                        0x84E8
#define GL_FRAMEBUFFER_COMPLETE                         0x8CD5
#define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT            0x8CD6
#define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT    0x8CD7
#define GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT  0x8CD8
#define GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS            0x8CD9
#define GL_FRAMEBUFFER_INCOMPLETE_FORMATS               0x8CDA
#define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER           0x8CDB
#define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER           0x8CDC
#define GL_FRAMEBUFFER_UNSUPPORTED                      0x8CDD
#define GL_MAX_COLOR_ATTACHMENTS                        0x8CDF
#define GL_COLOR_ATTACHMENT0                            0x8CE0
#define GL_COLOR_ATTACHMENT1                            0x8CE1
#define GL_COLOR_ATTACHMENT2                            0x8CE2
#define GL_COLOR_ATTACHMENT3                            0x8CE3
#define GL_COLOR_ATTACHMENT4                            0x8CE4
#define GL_COLOR_ATTACHMENT5                            0x8CE5
#define GL_COLOR_ATTACHMENT6                            0x8CE6
#define GL_COLOR_ATTACHMENT7                            0x8CE7
#define GL_DEPTH_ATTACHMENT                             0x8D00
#define GL_STENCIL_ATTACHMENT                           0x8D20
#define GL_FRAMEBUFFER                                  0x8D40
#define GL_RENDERBUFFER                                 0x8D41

#define GL_READ_FRAMEBUFFER                             0x8CA8
#define GL_DRAW_FRAMEBUFFER                             0x8CA9

#define GPU_MEMORY_INFO_DEDICATED_VIDMEM_NVX                0x9047
#define GPU_MEMORY_INFO_TOTAL_AVAILABLE_MEMORY_NVX          0x9048
#define GPU_MEMORY_INFO_CURRENT_AVAILABLE_VIDMEM_NVX        0x9049
#define GPU_MEMORY_INFO_EVICTION_COUNT_NVX                  0x904A
#define GPU_MEMORY_INFO_EVICTED_MEMORY_NVX                  0x904B

#define VBO_FREE_MEMORY_ATI                                 0x87FB
#define TEXTURE_FREE_MEMORY_ATI                             0x87FC
#define RENDERBUFFER_FREE_MEMORY_ATI                        0x87FD

#define GL_RGBA16F                    0x881A
#define GL_RGB16F                     0x881B
#define GL_R16F                           0x822D
#define GL_RG16F                          0x822F

#define GL_SRGB                       0x8C40
#define GL_SRGB8                      0x8C41
#define GL_SRGB_ALPHA                 0x8C42
#define GL_SRGB8_ALPHA8               0x8C43
#define GL_SLUMINANCE_ALPHA           0x8C44
#define GL_SLUMINANCE8_ALPHA8         0x8C45
#define GL_SLUMINANCE                 0x8C46
#define GL_SLUMINANCE8                0x8C47
#define GL_COMPRESSED_SRGB            0x8C48
#define GL_COMPRESSED_SRGB_ALPHA      0x8C49
#define GL_COMPRESSED_SLUMINANCE      0x8C4A
#define GL_COMPRESSED_SLUMINANCE_ALPHA 0x8C4B

#define GL_PROGRAM_BINARY_RETRIEVABLE_HINT                  0x8257
#define GL_PROGRAM_BINARY_LENGTH                            0x8741
#define GL_NUM_PROGRAM_BINARY_FORMATS                       0x87FE
#define GL_PROGRAM_BINARY_FORMATS                           0x87FF

#define GL_COMPRESSED_RGB8_ETC2                             0x9274
#define GL_COMPRESSED_SRGB8_ETC2                            0x9275

#define GL_DEPTH_COMPONENT16_NONLINEAR_NV                   0x8E2C
#define EGL_DEPTH_ENCODING_NV                               0x30E2
#define EGL_DEPTH_ENCODING_NONE_NV                          0
#define EGL_DEPTH_ENCODING_NONLINEAR_NV                     0x30E3

#define GL_TEXTURE_2D_ARRAY                             0x8C1A
#define GL_TEXTURE_BINDING_2D_ARRAY                     0x8C1D
#define GL_MAX_ARRAY_TEXTURE_LAYERS                     0x88FF

#define GL_DEPTH_STENCIL                                0x84F9
#define GL_UNSIGNED_INT_24_8                            0x84FA
#define GL_DEPTH24_STENCIL8                             0x88F0

#define GL_TEXTURE_MIN_LOD                             0x813A
#define GL_TEXTURE_MAX_LOD                             0x813B
#define GL_TEXTURE_BASE_LEVEL                          0x813C
#define GL_TEXTURE_MAX_LEVEL                           0x813D

#define GL_MULTISAMPLE                0x809D
#define GL_SAMPLE_ALPHA_TO_COVERAGE   0x809E

#define GL_RENDERBUFFER_SAMPLES                    0x8CAB
#define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE      0x8D56
#define GL_MAX_SAMPLES                             0x8D57

#define GL_MAX_LABEL_LENGTH               0x82E8

#define GL_UNIFORM_BUFFER                 0x8A11
#define GL_INVALID_INDEX                  0xFFFFFFFFu

#endif // QGL_H

#ifndef APIENTRY
#define APIENTRY
#endif

#ifndef QGL_FUNC
#define QGL_FUNC
#endif

// WGL Functions
QGL_WGL( PROC, wglGetProcAddress, ( LPCSTR ) );
QGL_WGL( int, wglChoosePixelFormat, ( HDC, CONST PIXELFORMATDESCRIPTOR * ) );
QGL_WGL( int, wglDescribePixelFormat, ( HDC, int, UINT, LPPIXELFORMATDESCRIPTOR ) );
QGL_WGL( BOOL, wglSetPixelFormat, ( HDC, int, CONST PIXELFORMATDESCRIPTOR * ) );
QGL_WGL( BOOL, wglSwapBuffers, ( HDC ) );
QGL_WGL( HGLRC, wglCreateContext, ( HDC ) );
QGL_WGL( BOOL, wglDeleteContext, ( HGLRC ) );
QGL_WGL( BOOL, wglMakeCurrent, ( HDC, HGLRC ) );
QGL_WGL( BOOL, wglShareLists, ( HGLRC, HGLRC ) );

// GLX Functions
//QGL_GLX( void *, glXGetProcAddressARB, ( const GLubyte * procName ) );
//QGL_GLX( XVisualInfo *, glXChooseVisual, ( Display * dpy, int screen, int *attribList ) );
//QGL_GLX( GLXContext, glXCreateContext, ( Display * dpy, XVisualInfo * vis, GLXContext shareList, Bool direct ) );
//QGL_GLX( void, glXDestroyContext, ( Display * dpy, GLXContext ctx ) );
//QGL_GLX( Bool, glXMakeCurrent, ( Display * dpy, GLXDrawable drawable, GLXContext ctx ) );
//QGL_GLX( Bool, glXCopyContext, ( Display * dpy, GLXContext src, GLXContext dst, GLuint mask ) );
//QGL_GLX( Bool, glXSwapBuffers, ( Display * dpy, GLXDrawable drawable ) );
//QGL_GLX( Bool, glXQueryVersion, ( Display * dpy, int *major, int *minor ) );
//QGL_GLX( const char *, glXQueryExtensionsString, ( Display * dpy, int screen ) );

// EGL Functions
#ifdef EGL_VERSION_1_0
QGL_EGL( void *, eglGetProcAddress, ( const char *procname ) );
QGL_EGL( EGLBoolean, eglChooseConfig, ( EGLDisplay dpy, const EGLint * attrib_list, EGLConfig * configs, EGLint config_size, EGLint * num_config ) );
QGL_EGL( EGLContext, eglCreateContext, ( EGLDisplay dpy, EGLConfig config, EGLContext share_context, const EGLint * attrib_list ) );
QGL_EGL( EGLSurface, eglCreatePbufferSurface, ( EGLDisplay dpy, EGLConfig config, const EGLint * attrib_list ) );
QGL_EGL( EGLSurface, eglCreateWindowSurface, ( EGLDisplay dpy, EGLConfig config, EGLNativeWindowType win, const EGLint * attrib_list ) );
QGL_EGL( EGLBoolean, eglDestroyContext, ( EGLDisplay dpy, EGLContext ctx ) );
QGL_EGL( EGLBoolean, eglDestroySurface, ( EGLDisplay dpy, EGLSurface surface ) );
QGL_EGL( EGLBoolean, eglGetConfigAttrib, ( EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint * value ) );
QGL_EGL( EGLContext, eglGetCurrentContext, ( void ) );
QGL_EGL( EGLDisplay, eglGetCurrentDisplay, ( void ) );
QGL_EGL( EGLDisplay, eglGetDisplay, ( EGLNativeDisplayType display_id ) );
QGL_EGL( EGLint, eglGetError, ( void ) );
QGL_EGL( EGLBoolean, eglInitialize, ( EGLDisplay dpy, EGLint * major, EGLint * minor ) );
QGL_EGL( EGLBoolean, eglMakeCurrent, ( EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx ) );
QGL_EGL( const char *, eglQueryString, ( EGLDisplay dpy, EGLint name ) );
QGL_EGL( EGLBoolean, eglSwapBuffers, ( EGLDisplay dpy, EGLSurface surface ) );
QGL_EGL( EGLBoolean, eglSwapInterval, ( EGLDisplay dpy, EGLint interval ) );
QGL_EGL( EGLBoolean, eglTerminate, ( EGLDisplay dpy ) );
#endif

// GL Functions
QGL_FUNC( void, glBindTexture, ( GLenum target, GLuint texture ) );
QGL_FUNC( void, glClear, ( GLbitfield mask ) );
QGL_FUNC( void, glClearColor, ( GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha ) );
QGL_FUNC( void, glClearStencil, ( GLint s ) );
QGL_FUNC( void, glColorMask, ( GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha ) );
QGL_FUNC( void, glCullFace, ( GLenum mode ) );
QGL_FUNC( void, glGenTextures, ( GLsizei n, const GLuint * textures ) );
QGL_FUNC( void, glDeleteTextures, ( GLsizei n, const GLuint * textures ) );
QGL_FUNC( void, glDepthFunc, ( GLenum func ) );
QGL_FUNC( void, glDepthMask, ( GLboolean flag ) );
QGL_FUNC( void, glDisable, ( GLenum cap ) );
QGL_FUNC( void, glDrawElements, ( GLenum, GLsizei, GLenum, const GLvoid * ) );
QGL_FUNC( void, glEnable, ( GLenum cap ) );
QGL_FUNC( void, glFinish, ( void ) );
QGL_FUNC( void, glFlush, ( void ) );
QGL_FUNC( void, glFrontFace, ( GLenum mode ) );
QGL_FUNC( GLenum, glGetError, ( void ) );
QGL_FUNC( void, glGetIntegerv, ( GLenum pname, GLint * params ) );
QGL_FUNC( const GLubyte *, glGetString, ( GLenum name ) );
QGL_FUNC( const GLubyte *, glGetStringi, ( GLenum name, GLuint index ) );
QGL_FUNC( void, glPixelStorei, ( GLenum pname, GLint param ) );
QGL_FUNC( void, glPolygonOffset, ( GLfloat factor, GLfloat units ) );
QGL_FUNC( void, glReadPixels, ( GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid * pixels ) );
QGL_FUNC( void, glScissor, ( GLint x, GLint y, GLsizei width, GLsizei height ) );
QGL_FUNC( void, glStencilFunc, ( GLenum func, GLint ref, GLuint mask ) );
QGL_FUNC( void, glStencilMask, ( GLuint mask ) );
QGL_FUNC( void, glStencilOp, ( GLenum fail, GLenum zfail, GLenum zpass ) );
QGL_FUNC( void, glTexImage2D, ( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid * pixels ) );
QGL_FUNC( void, glTexParameteri, ( GLenum target, GLenum pname, GLint param ) );
QGL_FUNC( void, glTexParameteriv, ( GLenum target, GLenum pname, const GLint *params ) );
QGL_FUNC( void, glTexSubImage2D, ( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid * pixels ) );
QGL_FUNC( void, glViewport, ( GLint x, GLint y, GLsizei width, GLsizei height ) );

QGL_FUNC( void, glClearDepth, ( GLclampd depth ) );
QGL_FUNC( void, glDepthRange, ( GLclampd zNear, GLclampd zFar ) );
QGL_FUNC( void, glDrawBuffer, ( GLenum mode ) );
QGL_FUNC( void, glReadBuffer, ( GLenum mode ) );
QGL_FUNC( void, glPolygonMode, ( GLenum face, GLenum mode ) );

QGL_FUNC( void, glActiveTexture, ( GLenum ) );
QGL_FUNC( void, glDrawRangeElements, ( GLenum, GLuint, GLuint, GLsizei, GLenum, const GLvoid * ) );
QGL_FUNC( void, glMultiDrawElements, ( GLenum, const GLsizei *, GLenum,  const void * const *, GLsizei ) );
QGL_FUNC( void, glBindBuffer, ( GLenum target, GLuint buffer ) );
QGL_FUNC( void, glDeleteBuffers, ( GLsizei n, const GLuint * buffers ) );
QGL_FUNC( void, glGenBuffers, ( GLsizei n, GLuint * buffers ) );
QGL_FUNC( void, glBufferData, ( GLenum target, GLsizeiptr size, const GLvoid * data, GLenum usage ) );
QGL_FUNC( void, glBufferSubData, ( GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid * data ) );

QGL_FUNC( void, glUniform1f, ( GLint location, GLfloat v0 ) );
QGL_FUNC( void, glUniform2f, ( GLint location, GLfloat v0, GLfloat v1 ) );
QGL_FUNC( void, glUniform3f, ( GLint location, GLfloat v0, GLfloat v1, GLfloat v2 ) );
QGL_FUNC( void, glUniform4f, ( GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3 ) );
QGL_FUNC( void, glUniform1i, ( GLint location, GLint v0 ) );
QGL_FUNC( void, glUniform2i, ( GLint location, GLint v0, GLint v1 ) );
QGL_FUNC( void, glUniform3i, ( GLint location, GLint v0, GLint v1, GLint v2 ) );
QGL_FUNC( void, glUniform4i, ( GLint location, GLint v0, GLint v1, GLint v2, GLint v3 ) );
QGL_FUNC( void, glUniform1fv, ( GLint location, GLsizei count, const GLfloat * value ) );
QGL_FUNC( void, glUniform2fv, ( GLint location, GLsizei count, const GLfloat * value ) );
QGL_FUNC( void, glUniform3fv, ( GLint location, GLsizei count, const GLfloat * value ) );
QGL_FUNC( void, glUniform4fv, ( GLint location, GLsizei count, const GLfloat * value ) );
QGL_FUNC( void, glUniform1iv, ( GLint location, GLsizei count, const GLint * value ) );
QGL_FUNC( void, glUniform2iv, ( GLint location, GLsizei count, const GLint * value ) );
QGL_FUNC( void, glUniform3iv, ( GLint location, GLsizei count, const GLint * value ) );
QGL_FUNC( void, glUniform4iv, ( GLint location, GLsizei count, const GLint * value ) );
QGL_FUNC( void, glUniformMatrix2fv, ( GLint location, GLsizei count, GLboolean transpose, const GLfloat * value ) );
QGL_FUNC( void, glUniformMatrix3fv, ( GLint location, GLsizei count, GLboolean transpose, const GLfloat * value ) );
QGL_FUNC( void, glUniformMatrix4fv, ( GLint location, GLsizei count, GLboolean transpose, const GLfloat * value ) );
QGL_FUNC( GLint, glGetUniformLocation, ( GLuint program, const GLchar * name ) );
QGL_FUNC( void, glGetActiveUniform, ( GLuint program, GLuint index, GLsizei maxLength, GLsizei * length, GLint * size, GLenum * type, GLchar * name ) );
QGL_FUNC( void, glGetUniformfv, ( GLuint program, GLint location, GLfloat * params ) );
QGL_FUNC( void, glGetUniformiv, ( GLuint program, GLint location, GLint * params ) );

QGL_FUNC( void, glShaderSource, ( GLuint shader, GLsizei count, const GLchar *const *string, const GLint * length ) );
QGL_FUNC( void, glGetShaderSource, ( GLuint obj, GLsizei maxLength, GLsizei * length, GLchar * source ) );
QGL_FUNC( void, glCompileShader, ( GLuint shader ) );
QGL_FUNC( void, glLinkProgram, ( GLuint program ) );
QGL_FUNC( void, glValidateProgram, ( GLuint program ) );
QGL_FUNC( void, glDeleteProgram, ( GLuint program ) );
QGL_FUNC( void, glDeleteShader, ( GLuint shader ) );
QGL_FUNC( void, glDetachShader, ( GLuint program, GLuint shader ) );
QGL_FUNC( GLuint, glCreateShader, ( GLenum shaderType ) );
QGL_FUNC( GLuint, glCreateProgram, ( void ) );
QGL_FUNC( void, glAttachShader, ( GLuint program, GLuint shader ) );
QGL_FUNC( void, glUseProgram, ( GLuint program ) );
QGL_FUNC( void, glGetProgramiv, ( GLuint program, GLenum pname, GLint * params ) );
QGL_FUNC( void, glGetShaderiv, ( GLuint shader, GLenum pname, GLint * params ) );
QGL_FUNC( void, glGetProgramInfoLog, ( GLuint program, GLsizei maxLength, GLsizei * length, GLchar * infoLog ) );
QGL_FUNC( void, glGetShaderInfoLog, ( GLuint shader, GLsizei maxLength, GLsizei * length, GLchar * infoLog ) );
QGL_FUNC( void, glGetAttachedShaders, ( GLuint program, GLsizei maxCount, GLsizei * count, GLuint * shaders ) );

QGL_FUNC( void, glVertexAttribPointer, ( GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid * pointer ) );
QGL_FUNC( void, glEnableVertexAttribArray, ( GLuint index ) );
QGL_FUNC( void, glDisableVertexAttribArray, ( GLuint index ) );
QGL_FUNC( void, glBindAttribLocation, ( GLuint program, GLuint index, const GLchar * name ) );
QGL_FUNC( void, glGetActiveAttrib, ( GLuint program, GLuint index, GLsizei maxLength, GLsizei * length, GLint * size, GLenum * type, GLchar * name ) );
QGL_FUNC( GLint, glGetAttribLocation, ( GLuint program, const GLchar * name ) );

QGL_FUNC( void, glBindFragDataLocation, ( GLuint program, GLuint index, const GLchar * name ) );

QGL_FUNC( void, glDrawArraysInstanced, ( GLenum mode, GLint first, GLsizei count, GLsizei primcount ) );
QGL_FUNC( void, glDrawElementsInstanced, ( GLenum mode, GLsizei count, GLenum type, const GLvoid * indices, GLsizei primcount ) );
QGL_FUNC( void, glVertexAttribDivisor, ( GLuint index, GLuint divisor ) );

QGL_FUNC( void, glGenVertexArrays, ( GLsizei n, GLuint *arrays ) );
QGL_FUNC( void, glDeleteVertexArrays, ( GLsizei n, const GLuint *arrays ) );
QGL_FUNC( void, glBindVertexArray, ( GLuint array ) )

QGL_FUNC( GLboolean, glIsRenderbuffer, ( GLuint ) );
QGL_FUNC( void, glBindRenderbuffer, ( GLenum, GLuint ) );
QGL_FUNC( void, glDeleteRenderbuffers, ( GLsizei, const GLuint * ) );
QGL_FUNC( void, glGenRenderbuffers, ( GLsizei, GLuint * ) );
QGL_FUNC( void, glRenderbufferStorage, ( GLenum, GLenum, GLsizei, GLsizei ) );
QGL_FUNC( void, glGetRenderbufferParameteriv, ( GLenum, GLenum, GLint * ) );
QGL_FUNC( GLboolean, glIsFramebuffer, ( GLuint ) );
QGL_FUNC( void, glBindFramebuffer, ( GLenum, GLuint ) );
QGL_FUNC( void, glDeleteFramebuffers, ( GLsizei, const GLuint * ) );
QGL_FUNC( void, glGenFramebuffers, ( GLsizei, GLuint * ) );
QGL_FUNC( GLenum, glCheckFramebufferStatus, ( GLenum ) );
QGL_FUNC( void, glFramebufferTexture1D, ( GLenum, GLenum, GLenum, GLuint, GLint ) );
QGL_FUNC( void, glFramebufferTexture2D, ( GLenum, GLenum, GLenum, GLuint, GLint ) );
QGL_FUNC( void, glFramebufferRenderbuffer, ( GLenum, GLenum, GLenum, GLuint ) );
QGL_FUNC( void, glGetFramebufferAttachmentParameteriv, ( GLenum, GLenum, GLenum, GLint * ) );
QGL_FUNC( void, glGenerateMipmap, ( GLenum ) );

QGL_FUNC( void, glBlitFramebuffer, ( GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLbitfield, GLenum ) );

QGL_FUNC( void, glRenderbufferStorageMultisample, ( GLenum target, GLsizei samples, GLenum internalformat, GLsizei width, GLsizei height ) );

QGL_EXT( void, glProgramParameteri, ( GLuint program, GLenum pname, GLint value ) );
QGL_EXT( void, glGetProgramBinary, ( GLuint program, GLsizei bufSize, GLsizei * length, GLenum * binaryFormat, GLvoid * binary ) );
QGL_EXT( void, glProgramBinary, ( GLuint program, GLenum binaryFormat, const GLvoid * binary, GLsizei length ) );

QGL_FUNC( void, glCompressedTexImage2D, ( GLenum, GLint, GLenum, GLsizei, GLsizei, GLint, GLsizei, const GLvoid * ) );
QGL_FUNC( void, glCompressedTexSubImage2D, ( GLenum, GLint, GLint, GLint, GLsizei, GLsizei, GLenum, GLsizei, const GLvoid * ) );

QGL_FUNC( void, glBlendFuncSeparate, ( GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha ) );

QGL_FUNC( void, glTexImage3D, ( GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid * pixels ) );
QGL_FUNC( void, glTexSubImage3D, ( GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid * pixels ) );

QGL_FUNC( void, glDrawBuffers, ( GLsizei n, const GLenum * bufs ) );

QGL_FUNC( void, glSampleCoverage, ( GLfloat value, GLboolean invert ) );

QGL_EXT( void, glObjectLabel, ( GLenum identifier, GLuint name, GLsizei length, const char *label ) );

QGL_FUNC( GLuint, glGetUniformBlockIndex, ( GLuint program, const GLchar *uniformBlockName ) );
QGL_FUNC( void, glUniformBlockBinding, ( GLuint program, GLuint uniformBlockIndex, GLuint uniformBlockBinding ) );
QGL_FUNC( void, glBindBufferRange, ( GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size ) );
QGL_FUNC( void, glBindBufferBase, ( GLenum target, GLuint index, GLuint buffer ) );