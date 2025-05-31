project(${QFUSION_CLIENT_NAME})

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

include_directories(${ZLIB_INCLUDE_DIR} ${CURL_INCLUDE_DIR})
include_directories(${OGG_INCLUDE_DIR} ${VORBIS_INCLUDE_DIR} ${FREETYPE_INCLUDE_DIRS})
include_directories("../../third-party/magic_enum/include")
include_directories("../../third-party/recastnavigation/Recast/Include/")

set(OPENAL_SOFT_DIR "../../third-party/openal-soft")
include_directories("${OPENAL_SOFT_DIR}/include")
if (NOT WIN32)
set(OPENAL_SOFT_LIBRARY "openal")
link_directories("${OPENAL_SOFT_DIR}")
else()
set(OPENAL_SOFT_LIBRARY "OpenAL32")
link_directories(${OPENAL_SOFT_DIR}/cmake-build-release/)
endif()

set(ANGELSCRIPT_DIR ${CMAKE_HOME_DIRECTORY}/../third-party/angelscript/sdk/angelscript)
link_directories(${ANGELSCRIPT_DIR}/lib)

# TODO: Is there something smarter
get_filename_component(Qt5_CMAKEDIR ${Qt5_DIR} DIRECTORY)
get_filename_component(Qt5_LIBDIR ${Qt5_CMAKEDIR} DIRECTORY)
get_filename_component(Qt5_HOMEDIR ${Qt5_LIBDIR} DIRECTORY)

link_directories("${Qt5_HOMEDIR}\\lib")
link_directories("${Qt5_HOMEDIR}\\plugins\\imageformats")
link_directories("${Qt5_HOMEDIR}\\plugins\\platforms")
link_directories("${Qt5_HOMEDIR}\\plugins\\xcbglintegrations")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQml")
link_directories("${Qt5_HOMEDIR}\\qml\\QtMultimedia")
link_directories("${Qt5_HOMEDIR}\\qml\\QtGraphicalEffects")
link_directories("${Qt5_HOMEDIR}\\qml\\QtGraphicalEffects\\private")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick.2")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Shapes")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Window.2")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Templates.2")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Layouts")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Controls.2")
link_directories("${Qt5_HOMEDIR}\\qml\\QtQuick\\Controls.2\\Material")

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
	"../common/*.h"
	"../angelwrap/qas_public.h"
	"cgame/cg_public.h"
	"renderer/*.h"
	"ui/*.h"
	"../server/*.h"
	"sound/*.h"
)

file(GLOB CLIENT_COMMON_SOURCES
	"../common/lifecycle.cpp"
	"../common/noise.cpp"
	"*.cpp"
	"ui/*.cpp"
	"cgame/*.cpp"
	"renderer/*.cpp"
	"sound/*.cpp"
	"../../third-party/recastnavigation/Recast/Source/*.cpp"
)

if (USE_SDL2)
	file(GLOB REF_PLATFORM_SOURCES
			"sdl/sdl_glw.cpp"
			"sdl/sdl_qgl.cpp"
			)

	set(REF_PLATFORM_LIBRARIES ${SDL2_LIBRARY})
else()
	if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
		file(GLOB REF_PLATFORM_SOURCES
				"win32/win_glw.cpp"
				"win32/win_qgl.cpp"
				)

		set(REF_PLATFORM_LIBRARIES "")
	endif()
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
	file(GLOB CLIENT_PLATFORM_HEADERS
		"win32/resource.h"
		"win32/conproc.h"
		"win32/winquake.h"
		"win32/win_input.h"
	)

    file(GLOB CLIENT_PLATFORM_SOURCES
        "win32/*.cpp"
        "win32/qfusion.rc"
    )

	# TODO: Get rid of the automation stuff
    set(CLIENT_PLATFORM_LIBRARIES "ws2_32.lib" "winmm.lib" "Ole32.lib" "qwindows" "Wtsapi32" Qt5WindowsUIAutomationSupport)

    set(CLIENT_BINARY_TYPE WIN32)

    set(BUNDLE_RESOURCES "ui/ui.qrc")
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
	set(CLIENT_PLATFORM_HEADERS "")

	# TODO
    file(GLOB CLIENT_PLATFORM_SOURCES
        "sdl/*.cpp"
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

	file(GLOB CLIENT_PLATFORM_SOURCES
		"sdl/*.cpp")

	# TODO: Discover how to get rid of all of this?
	set(QT_XCB_DEPENDENCY_LIBRARIES
		xkbcommon xkbcommon-x11 xcb-keysyms xcb-xkb xcb-sync xcb-shape xcb-shm xcb-randr
		xcb-xfixes xcb-xinput xcb-image xcb-render xcb-render-util xcb-glx xcb-xinerama xcb-icccm xcb X11 X11-xcb)
	# Must get rid of this for the release
	set(QT_PLATFORM_DEPENDENCY_LIBRARIES ${QT_XCB_DEPENDENCY_LIBRARIES})
	set(CLIENT_PLATFORM_LIBRARIES
		${SDL2_LIBRARY} ${FREETYPE_LIBRARY} pthread m dl rt uuid
		qxcb qxcb-glx-integration Qt5XcbQpa Qt5XkbCommonSupport Qt5ServiceSupport Qt5EdidSupport Qt5GlxSupport
		${QT_PLATFORM_DEPENDENCY_LIBRARIES})
    set(CLIENT_BINARY_TYPE "")

    set(BUNDLE_RESOURCES "ui/ui.qrc")
endif()

if (MSVC)
	set_source_files_properties("renderer/frontendavx.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
	set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "/arch:AVX")
else()
	set_source_files_properties("renderer/frontendsse41.cpp" PROPERTIES COMPILE_FLAGS "-msse4.1")
	set_source_files_properties("renderer/frontendavx.cpp" PROPERTIES COMPILE_FLAGS "-mavx")
	set_source_files_properties("../common/cm_trace_sse42.cpp" PROPERTIES COMPILE_FLAGS "-msse4.2")
	set_source_files_properties("../common/cm_trace_avx.cpp" PROPERTIES COMPILE_FLAGS "-mavx")
endif()

add_executable(${QFUSION_CLIENT_NAME} ${CLIENT_BINARY_TYPE}
		       ${CLIENT_HEADERS}
		       ${CLIENT_PLATFORM_HEADERS}
		       ${CLIENT_COMMON_SOURCES}
		       ${CLIENT_PLATFORM_SOURCES}
		       ${REF_PLATFORM_SOURCES}
		       ${BUNDLE_RESOURCES}
)

add_dependencies(${QFUSION_CLIENT_NAME} execommon)

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

set(QT_COMMON_LIBRARIES Qt5::Core Qt5::Concurrent Qt5::Quick Qt5::Gui Qt5::Svg Qt5::Multimedia)

set(QT_STATIC_EXPLICIT_LIBRARIES
	Qt5QuickControls2
	Qt5QuickTemplates2
	Qt5FontDatabaseSupport
	Qt5EventDispatcherSupport
	Qt5ThemeSupport
	Qt5MultimediaQuick
	Qt5QuickShapes
	# Qt5QmlWorkerScript # 5.15
)

set(QT_STATIC_EXPLICIT_PLUGINS
	qtquick2plugin
	windowplugin
	qquicklayoutsplugin
	qtquickcontrols2plugin
	qtquicktemplates2plugin
	qtgraphicaleffectsplugin
	qtgraphicaleffectsprivate
	qtquickcontrols2materialstyleplugin # 5.13
	#qqc2materialstyleplugin # 5.15
	declarative_multimedia
	qmlshapesplugin
	#qmlplugin # 5.15
	qsvg qtga qtfreetype
)

target_link_libraries(${QFUSION_CLIENT_NAME} PRIVATE
					  execommon
					  ${REF_PLATFORM_LIBRARIES}
					  ${OGG_LIBRARY}
					  ${VORBIS_LIBRARIES}
					  ${OPENAL_SOFT_LIBRARY}
					  ${QT_STATIC_EXPLICIT_PLUGINS}
					  ${QT_STATIC_EXPLICIT_LIBRARIES}
					  ${QT_COMMON_LIBRARIES}
					  ${CLIENT_PLATFORM_LIBRARIES}
)

qf_set_output_dir(${QFUSION_CLIENT_NAME} "")
