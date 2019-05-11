project(${QFUSION_SERVER_NAME})

include_directories(${ZLIB_INCLUDE_DIR} ${CURL_INCLUDE_DIR})

file(GLOB SERVER_HEADERS
    "*.h"
	"../gameshared/q_*.h"
	"../gameshared/anorms.h"
	"../gameshared/config.h"	
	"../qcommon/*.h"
	"../qalgo/*.h"
	"../matchmaker/*.h"
	"../angelwrap/qas_public.h"
	"../game/g_public.h"
	"../server/*.h"
)

file(GLOB SERVER_SOURCES
	"../qcommon/asyncstream.cpp"
	"../qcommon/autoupdate.cpp"
	"../qcommon/cm_main.cpp"
	"../qcommon/cm_q3bsp.cpp"
	"../qcommon/cm_sample.cpp"
	"../qcommon/cm_trace.cpp"
	"../qcommon/cm_trace_sse42.cpp"
	"../qcommon/compression.cpp"
    "../qcommon/bsp.c"
    "../qcommon/patch.c"
    "../qcommon/common.cpp"
    "../qcommon/files.cpp"
    "../qcommon/cmd.cpp"
    "../qcommon/mem.cpp"
    "../qcommon/net.cpp"
    "../qcommon/net_chan.cpp"
    "../qcommon/msg.cpp"
    "../qcommon/cvar.cpp"
    "../qcommon/dynvar.cpp"
    "../qcommon/library.cpp"
    "../qcommon/mlist.cpp"
    "../qcommon/svnrev.cpp"
    "../qcommon/snap_demos.cpp"
    "../qcommon/snap_write.cpp"
	"../qcommon/snap_tables.cpp"
    "../qcommon/ascript.cpp"
    "../qcommon/anticheat.cpp"
    "../qcommon/wswcurl.cpp"
    "../qcommon/cjson.cpp"
    "../qcommon/threads.cpp"
    "../qcommon/steam.cpp"
    "*.cpp"
    "../null/cl_null.cpp"
    "../gameshared/q_*.c"
    "../qalgo/*.c"
    "../matchmaker/*.cpp"
	"../../third-party/sqlite-amalgamation/sqlite3.c"
)

if (WIN32)
    file(GLOB SERVER_PLATFORM_SOURCES 
        "../win32/win_fs.cpp"
        "../win32/win_net.cpp"
        "../win32/win_sys.cpp"
        "../win32/win_console.cpp"
        "../win32/win_time.cpp"
        "../win32/win_lib.cpp"
        "../win32/win_threads.cpp"
        "../null/sys_vfs_null.cpp"
        "../win32/conproc.cpp"
    )

    # Link Ole32.lib for CoCreateGUID()
    set(SERVER_PLATFORM_LIBRARIES ws2_32.lib winmm.lib Ole32.lib)
    set(SERVER_BINARY_TYPE WIN32)
else()
    file(GLOB SERVER_PLATFORM_SOURCES 
        "../unix/unix_fs.cpp"
        "../unix/unix_net.cpp"
        "../unix/unix_sys.cpp"
        "../unix/unix_console.cpp"
        "../unix/unix_time.cpp"
        "../unix/unix_lib.cpp"
        "../unix/unix_threads.cpp"
        "../null/sys_vfs_null.cpp"
    )

    # Link libuuid
    if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
        set(SERVER_PLATFORM_LIBRARIES pthread dl m uuid)
    else()
        set(SERVER_PLATFORM_LIBRARIES pthread m uuid)
    endif()

    set(SERVER_BINARY_TYPE "")
endif()

if (MSVC)
	set_source_files_properties("../qcommon/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
	set_source_files_properties("../../third-party/sqlite-amalgamation/sqlite3.c" PROPERTIES COMPILE_FLAGS "/fp:precise")
else()
	set_source_files_properties("../qcommon/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "-msse4.2")
	set_source_files_properties("../../third-party/sqlite-amalgamation/sqlite3.c" PROPERTIES COMPILE_FLAGS "-fno-fast-math")
endif()

add_executable(${QFUSION_SERVER_NAME} ${SERVER_BINARY_TYPE} ${SERVER_HEADERS} ${SERVER_SOURCES} ${SERVER_PLATFORM_SOURCES})
add_dependencies(${QFUSION_SERVER_NAME} angelwrap game)
target_link_libraries(${QFUSION_SERVER_NAME} PRIVATE ${CURL_LIBRARY} ${ZLIB_LIBRARY} ${SERVER_PLATFORM_LIBRARIES})
qf_set_output_dir(${QFUSION_SERVER_NAME} "")

set_target_properties(${QFUSION_SERVER_NAME} PROPERTIES COMPILE_DEFINITIONS "DEDICATED_ONLY")
