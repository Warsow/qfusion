project(common)

set(ANGELSCRIPT_DIR ${CMAKE_HOME_DIRECTORY}/../third-party/angelscript/sdk/angelscript)

include_directories(${ANGELSCRIPT_DIR}/include)
link_directories(${ANGELSCRIPT_DIR}/lib)

include_directories(${ZLIB_INCLUDE_DIR} ${CURL_INCLUDE_DIR})

file(GLOB GAME_HEADERS
    "../server/game/*.h"
    "../server/game/ai/*.h"
    "../server/game/ai/awareness/*.h"
    "../server/game/ai/combat/*.h"
    "../server/game/ai/movement/*.h"
    "../server/game/ai/navigation/*.h"
    "../server/game/ai/planning/*.h"
    "../server/game/ai/teamplay/*.h"
    "../server/game/ascript/*.h"
    "../server/game/ascript/addon/*.h")

file(GLOB GAME_SOURCES
    "../server/game/*.cpp"
    "../server/game/ai/*.cpp"
    "../server/game/ai/awareness/*.cpp"
    "../server/game/ai/combat/*.cpp"
    "../server/game/ai/movement/*.cpp"
    "../server/game/ai/navigation/*.cpp"
    "../server/game/ai/planning/*.cpp"
    "../server/game/ai/teamplay/*.cpp"
    "../server/game/ascript/*.cpp"
    "../server/game/ascript/addon/*.cpp")

file(GLOB EXECOMMON_SOURCES
     "asyncstream.cpp"
     "base64.cpp"
     "bsp.cpp"
     "cm_main.cpp"
     "cm_q3bsp.cpp"
     "cm_sample.cpp"
     "cm_trace.cpp"
     "cm_trace_avx.cpp"
     "cm_trace_sse42.cpp"
     "cmdargssplitter.cpp"
     "cmdsystem.cpp"
     "common.cpp"
     "compression.cpp"
     "configstringstorage.cpp"
     "configvars.cpp"
     "cvar.cpp"
     "files.cpp"
     "glob.cpp"
     "gs_gameteams.cpp"
     "gs_items.cpp"
     "gs_misc.cpp"
     "gs_pmove.cpp"
     "gs_slidebox.cpp"
     "gs_weapondefs.cpp"
     "gs_weapons.cpp"
     "half_float.cpp"
     "hash.cpp"
     "library.cpp"
     "maplist.cpp"
     "md5.cpp"
     "mmcommon.cpp"
     "msg.cpp"
     "net.cpp"
     "net_chan.cpp"
     "outputmessages.cpp"
     "patch.cpp"
     "q_math.cpp"
     "q_shared.cpp"
     "q_trie.cpp"
     "snap_demos.cpp"
     "tasksystem.cpp"
     "textstreamwriter.cpp"
     "threads.cpp"
     "userinfo.cpp"
     "wswcurl.cpp"
     "wswexceptions.cpp"
     "wswfs.cpp"
     "wswpodvector.cpp"
     "wswprofiler.cpp"
     "wswsortbyfield.cpp"
     "wswstringview.cpp"
     "../server/sv_web.cpp"
     "../server/sv_main.cpp")

file(GLOB EXECOMMON_HEADERS
     "*.h")

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    file(GLOB EXECOMMON_PLATFORM_SOURCES "win32/*.cpp")
    file(GLOB EXECOMMON_PLATFORM_HEADERS "win32/*.h")
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    file(GLOB EXECOMMON_PLATFORM_SOURCES "linux/*.cpp")
    file(GLOB EXECOMMON_PLATFORM_HEADERS "linux/*.h")
endif()

if (MSVC)
    set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
else()
    set_source_files_properties("../common/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "-msse4.2")
    set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "-mavx")
endif()

add_library(execommon
    ${EXECOMMON_SOURCES}
    ${EXECOMMON_HEADERS}
    ${EXECOMMON_PLATFORM_SOURCES}
    ${EXECOMMON_PLATFORM_HEADERS}
    ${GAME_SOURCES}
    ${GAME_HEADERS})
target_link_libraries(execommon PRIVATE angelscript ${CURL_LIBRARY} ${ZLIB_LIBRARY})
qf_set_output_dir(execommon "")
