project(${QFUSION_SERVER_NAME})

include_directories(${ZLIB_INCLUDE_DIR} ${CURL_INCLUDE_DIR})

set(ANGELSCRIPT_DIR ${CMAKE_HOME_DIRECTORY}/../third-party/angelscript/sdk/angelscript)
link_directories(${ANGELSCRIPT_DIR}/lib)

file(GLOB SERVER_HEADERS
    "*.h")

file(GLOB SERVER_SOURCES
    "../common/lifecycle.cpp")

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    file(GLOB SERVER_PLATFORM_SOURCES
        "platform/win32/conproc.cpp"
        "platform/win32/win_server.cpp"
    )

    # Link Ole32.lib for CoCreateGUID()
    set(SERVER_PLATFORM_LIBRARIES "ws2_32.lib" "winmm.lib" "Ole32.lib")
    set(SERVER_BINARY_TYPE WIN32)
else()
    file(GLOB SERVER_PLATFORM_SOURCES 
        "platform/unix/unix_main.cpp"
    )

    # Link libuuid
    set(SERVER_PLATFORM_LIBRARIES "pthread" ${CMAKE_DL_LIBS} "m" "uuid")
    set(SERVER_BINARY_TYPE "")
endif()

if (MSVC)
	set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
else()
	set_source_files_properties("../common/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "-msse4.2")
	set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "-mavx")
endif()

add_executable(${QFUSION_SERVER_NAME} ${SERVER_BINARY_TYPE} ${SERVER_HEADERS} ${SERVER_SOURCES} ${SERVER_PLATFORM_SOURCES})
add_dependencies(${QFUSION_SERVER_NAME} execommon)
target_link_libraries(${QFUSION_SERVER_NAME} PRIVATE execommon ${SERVER_PLATFORM_LIBRARIES})
qf_set_output_dir(${QFUSION_SERVER_NAME} "")

set_target_properties(${QFUSION_SERVER_NAME} PROPERTIES COMPILE_DEFINITIONS "DEDICATED_ONLY")
