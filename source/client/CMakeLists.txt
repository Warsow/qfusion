project(${QFUSION_CLIENT_NAME})

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

include_directories(${ZLIB_INCLUDE_DIR} ${CURL_INCLUDE_DIR} ${JPEG_INCLUDE_DIR} ${PNG_INCLUDE_DIR})
include_directories(${OGG_INCLUDE_DIR} ${VORBIS_INCLUDE_DIR} ${FREETYPE_INCLUDE_DIRS})
include_directories("../../third-party/magic_enum/include")

if (USE_SDL2 OR (${CMAKE_SYSTEM_NAME} MATCHES "Linux"))
    include_directories(${SDL2_INCLUDE_DIR})
endif()

if (NOT QFUSION_MAC_ICON)
	set(QFUSION_MAC_ICON ../../icons/qfusion.icns)
endif()
if (NOT QFUSION_MAC_INFO_PLIST)
	set(QFUSION_MAC_INFO_PLIST ../mac/Qfusion-Info.plist)
endif()

file(GLOB CLIENT_HEADERS
    "*.h"
	"../gameshared/q_*.h"
	"../gameshared/anorms.h"
	"../gameshared/config.h"
	"../qcommon/*.h"
	"../angelwrap/qas_public.h"
	"../ref/r_public.h"
	"../cgame/cg_public.h"
	"../cgame/ref.h"
	"../game/g_public.h"
	"../ui/ui_public.h"
	"../ftlib/ftlib_public.h"
	"../steamlib/steamlib_public.h"
	"../cin/cin_public.h"
	"../server/*.h"
)

file(GLOB CLIENT_COMMON_SOURCES
	"../qcommon/asyncstream.cpp"
	"../qcommon/autoupdate.cpp"
	"../qcommon/base64.cpp"
	"../qcommon/bsp.cpp"
	"../qcommon/cjson.cpp"
	"../qcommon/cm_*.cpp"
	"../qcommon/cmd.cpp"
	"../qcommon/common.cpp"
	"../qcommon/compression.cpp"
	"../qcommon/configstringstorage.cpp"
	"../qcommon/cvar.cpp"
	"../qcommon/files.cpp"
	"../qcommon/glob.cpp"
	"../qcommon/half_float.cpp"
	"../qcommon/hash.cpp"
	"../qcommon/library.cpp"
	"../qcommon/md5.cpp"
	"../qcommon/maplist.cpp"
	"../qcommon/mm*.cpp"
	"../qcommon/msg.cpp"
	"../qcommon/net.cpp"
	"../qcommon/net_chan.cpp"
	"../qcommon/patch.cpp"
	"../qcommon/q_trie.cpp"
	"../qcommon/snap_*.cpp"
	"../qcommon/threads.cpp"
	"../qcommon/wswcurl.cpp"
	"../qcommon/wswfs.cpp"
    "../server/*.cpp"
    "*.cpp"
    "../gameshared/q_*.cpp"
    # We have to include this for linking with shared MM facilities.
    # This also is required to run listen servers (do we allow ranked listen servers?)
    # Unused stuff should get stripped anyway.
    "../../third-party/sqlite-amalgamation/sqlite3.c"
)

file(GLOB REF_HEADERS
		"../ref/*.h"
		"../gameshared/q_*.h"
		"../gameshared/anorms.h"
		"../gameshared/config.h"
		"../qcommon/bsp.h"
		"../qcommon/patch.h"
		"../qcommon/iqm.h"
		"../qcommon/qfiles.h"
		)

file(GLOB REF_COMMON_SOURCES
		"../ref/*.cpp"
		"../gameshared/q_*.cpp"
		"../qcommon/bsp.cpp"
		"../qcommon/patch.cpp"
		)

if (USE_SDL2)
	file(GLOB REF_PLATFORM_SOURCES
			"../sdl/sdl_glw.cpp"
			"../sdl/sdl_qgl.cpp"
			)

	set(REF_PLATFORM_LIBRARIES ${SDL2_LIBRARY})
else()
	if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
		file(GLOB REF_PLATFORM_SOURCES
				"../win32/win_glw.cpp"
				"../win32/win_qgl.cpp"
				)

		set(REF_PLATFORM_LIBRARIES "")
	endif()

	if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
		file(GLOB REF_PLATFORM_SOURCES
				"../unix/unix_glw.cpp"
				"../unix/unix_qgl.cpp"
				)

		set(REF_PLATFORM_LIBRARIES "X11" "Xext" "Xinerama" "Xrandr" "Xxf86vm" "m")
	endif()
endif()

file(GLOB FTLIB_HEADERS
		"../ftlib/*.h"
		"../gameshared/q_*.h"
		)

file(GLOB FTLIB_SOURCES
		"../ftlib/*.cpp"
		"../gameshared/q_*.c"
		)

file(GLOB CGAME_HEADES
		"../cgame/*.h"
		"../gameshared/*.h"
		)

file(GLOB CGAME_SOURCES
		"../cgame/*.cpp"
		"../gameshared/*.cpp"
		)

file(GLOB UI_HEADERS
		"../ui/*.h"
		)

file(GLOB UI_SOURCES
		"../ui/*.cpp")

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
	file(GLOB CLIENT_PLATFORM_HEADERS
		"../win32/resource.h"
		"../win32/conproc.h"
		"../win32/winquake.h"
		"../win32/win_input.h"
	)

    if (USE_SDL2)
        file(GLOB CLIENT_PLATFORM_SOURCES
            "../win32/win_fs.cpp"
            "../win32/win_net.cpp"
            "../win32/win_sys.cpp"
            "../win32/win_console.cpp"
            "../win32/win_threads.cpp"

            "../sdl/sdl_input.cpp"
            "../sdl/sdl_input_joy.cpp"
            "../sdl/sdl_vid.cpp"
            "../sdl/sdl_sys.cpp"
            "../sdl/sdl_clipboard.cpp"
            "../sdl/sdl_time.cpp"
            "../sdl/sdl_lib.cpp"
            "../sdl/sdl_client.cpp"

            "../win32/qfusion.rc"
        )

        set(CLIENT_PLATFORM_LIBRARIES ${SDL2_LIBRARY} ${SDL2MAIN_LIBRARY} "ws2_32.lib" "winmm.lib" "Ole32.lib")
    else()
        file(GLOB CLIENT_PLATFORM_SOURCES
            "../win32/win_fs.cpp"
            "../win32/win_lib.cpp"
            "../win32/win_net.cpp"
            "../win32/win_sys.cpp"
            "../win32/win_console.cpp"
            "../win32/win_clipboard.cpp"
            "../win32/win_time.cpp"
            "../win32/win_threads.cpp"

            "../win32/win_client.cpp"
            "../win32/win_input.cpp"
            "../win32/win_vid.cpp"
            "../win32/conproc.cpp"

            "../win32/qfusion.rc"
        )

        set(CLIENT_PLATFORM_LIBRARIES "ws2_32.lib" "winmm.lib" "Ole32.lib")
    endif()

    set(CLIENT_BINARY_TYPE WIN32)

    set(BUNDLE_RESOURCES "../ui/ui.qrc")
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(CLIENT_PLATFORM_HEADERS "")
	
    file(GLOB CLIENT_PLATFORM_SOURCES
        "../unix/unix_fs.cpp"
        "../unix/unix_net.cpp"
        "../unix/unix_sys.cpp"
        "../unix/unix_console.cpp"
        "../unix/unix_threads.cpp"

        "../sdl/sdl_input.cpp"
        "../sdl/sdl_input_joy.cpp"
        "../sdl/sdl_input_osx.m"
        "../sdl/sdl_vid.cpp"
        "../sdl/sdl_sys.cpp"
        "../sdl/sdl_clipboard.cpp"
        "../sdl/sdl_time.cpp"
        "../sdl/sdl_lib.cpp"
        "../sdl/sdl_client.cpp"

        "../mac/mac_sys.m"
        "${QFUSION_MAC_INFO_PLIST}"
    )

    set(CLIENT_PLATFORM_LIBRARIES ${SDL2_LIBRARY} "-framework IOKit" "-framework Cocoa")
    set(CLIENT_BINARY_TYPE MACOSX_BUNDLE)

    set(BUNDLE_RESOURCES
        ../../libsrcs/lib/mac/Frameworks
        ${QFUSION_MAC_ICON}
    )
	if (QFUSION_DATA_DIR)
		list(APPEND BUNDLE_RESOURCES
			${QFUSION_DATA_DIR}/${QFUSION_GAME_DIR}
			${QFUSION_DATA_DIR}/docs
		)
	endif()
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
	set(CLIENT_PLATFORM_HEADERS "")

    if (USE_SDL2)
        file(GLOB CLIENT_PLATFORM_SOURCES
            "../unix/unix_fs.cpp"
            "../unix/unix_net.cpp"
            "../unix/unix_sys.cpp"
            "../unix/unix_console.cpp"
            "../unix/unix_threads.cpp"

            "../sdl/sdl_input.cpp"
            "../sdl/sdl_input_joy.cpp"
            "../sdl/sdl_vid.cpp"
            "../sdl/sdl_sys.cpp"
            "../sdl/sdl_clipboard.cpp"
            "../sdl/sdl_time.cpp"
            "../sdl/sdl_lib.cpp"
            "../sdl/sdl_client.cpp"
        )

        set(CLIENT_PLATFORM_LIBRARIES ${SDL2_LIBRARY} "pthread" "m" "dl" "rt" "uuid")
    else()
        file(GLOB CLIENT_PLATFORM_SOURCES
            "../unix/unix_fs.cpp"
            "../unix/unix_lib.cpp"
            "../unix/unix_net.cpp"
            "../unix/unix_sys.cpp"
            "../unix/unix_console.cpp"
            "../unix/unix_clipboard.cpp"
            "../unix/unix_time.cpp"
            "../unix/unix_threads.cpp"

            "../unix/keysym2ucs.c"
            "../unix/unix_client.cpp"
            "../unix/unix_input.cpp"
            "../unix/unix_vid.cpp"

            "../sdl/sdl_input_joy.cpp"
        )
    
        set(CLIENT_PLATFORM_LIBRARIES ${SDL2_LIBRARY} "pthread" "dl" "m" "X11" "Xi" "Xext" "Xrandr" "rt" "uuid")
    endif()
    set(CLIENT_BINARY_TYPE "")

    set(BUNDLE_RESOURCES "../ui/ui.qrc")
endif()

if (MSVC)
	set_source_files_properties("../qcommon/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
	set_source_files_properties("../../third-party/sqlite-amalgamation/sqlite3.c" PROPERTIES COMPILE_FLAGS "/fp:precise")
else()
	set_source_files_properties("../qcommon/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "-msse4.2")
	set_source_files_properties("../qcommon/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "-mavx")
	set_source_files_properties("../../third-party/sqlite-amalgamation/sqlite3.c" PROPERTIES COMPILE_FLAGS "-fno-fast-math")
endif()

add_executable(${QFUSION_CLIENT_NAME} ${CLIENT_BINARY_TYPE} ${CLIENT_HEADERS} ${CLIENT_PLATFORM_HEADERS} ${CLIENT_COMMON_SOURCES} ${CLIENT_PLATFORM_SOURCES} ${REF_HEADERS} ${REF_COMMON_SOURCES} ${REF_PLATFORM_SOURCES} ${FTLIB_SOURCES} ${CIN_SOURCES} ${CGAME_HEADERS} ${CGAME_SOURCES} ${UI_HEADERS} ${UI_SOURCES} ${BUNDLE_RESOURCES})
add_dependencies(${QFUSION_CLIENT_NAME} game snd_openal_soft)

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set_target_properties(${QFUSION_CLIENT_NAME} PROPERTIES MACOSX_BUNDLE_INFO_PLIST ${QFUSION_MAC_INFO_PLIST})
    set_target_properties(${QFUSION_CLIENT_NAME} PROPERTIES XCODE_ATTRIBUTE_LD_RUNPATH_SEARCH_PATHS "@executable_path/../Frameworks @executable_path/../Resources/base @executable_path/../Resources/libs")
    set_source_files_properties(${QFUSION_MAC_ICON} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

    set_source_files_properties(../mac/Frameworks                 PROPERTIES MACOSX_PACKAGE_LOCATION .)
	if (QFUSION_DATA_DIR)
	    set_source_files_properties(${QFUSION_DATA_DIR}/${QFUSION_GAME_DIR}    PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
		set_source_files_properties(${QFUSION_DATA_DIR}/docs    PROPERTIES MACOSX_PACKAGE_LOCATION Resources)
	endif()

    set(BUNDLE_PATH ${CMAKE_HOME_DIRECTORY}/build/$(CONFIGURATION)/${QFUSION_CLIENT_NAME}.app/Contents)
    set(COPY_DYLIBS
        cp -r ${CMAKE_HOME_DIRECTORY}/build/$(CONFIGURATION)/${QFUSION_GAME_DIR}/ ${BUNDLE_PATH}/Resources/${QFUSION_GAME_DIR}/ &&
        cp -r ${CMAKE_HOME_DIRECTORY}/build/$(CONFIGURATION)/libs/ ${BUNDLE_PATH}/Resources/libs/
    )
        
    add_custom_command(TARGET ${QFUSION_CLIENT_NAME} POST_BUILD COMMAND ${COPY_DYLIBS})
endif()

target_link_libraries(${QFUSION_CLIENT_NAME} PRIVATE ${CURL_LIBRARY} ${ZLIB_LIBRARY} ${JPEG_LIBRARIES} ${PNG_LIBRARIES} ${REF_PLATFORM_LIBRARIES} ${OGG_LIBRARY} ${VORBIS_LIBRARIES} ${FREETYPE_LIBRARIES} ${CLIENT_PLATFORM_LIBRARIES} snd_openal_soft Qt5::Core Qt5::Concurrent Qt5::Quick Qt5::Gui Qt5::Svg)
qf_set_output_dir(${QFUSION_CLIENT_NAME} "")
