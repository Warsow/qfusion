project(qfusion)

cmake_minimum_required(VERSION 2.8.12)

set(CMAKE_CXX_STANDARD 20)

message(STATUS "CMAKE_SYSTEM: " ${CMAKE_SYSTEM})
message(STATUS "CMAKE_SYSTEM_NAME: " ${CMAKE_SYSTEM_NAME})
message(STATUS "CMAKE_SYSTEM_VERSION: " ${CMAKE_SYSTEM_VERSION})
message(STATUS "CMAKE_SYSTEM_PROCESSOR: " ${CMAKE_SYSTEM_PROCESSOR})

set(CMAKE_MODULE_PATH ${CMAKE_HOME_DIRECTORY}/cmake)
include(QfusionUtils)

include_directories("${CMAKE_SOURCE_DIR}")

if (QFUSION_GAME)
    if (${QFUSION_GAME} MATCHES "Warsow")
        set(QFUSION_GAME_DIR basewsw)
        set(QFUSION_CLIENT_NAME warsow)
        set(QFUSION_SERVER_NAME wsw_server)
        set(QFUSION_APPLICATION_VERSION_HEADER \"version.warsow.h\")
        set(QFUSION_MAC_ICON ../../icons/warsow.icns)
        set(QFUSION_MAC_INFO_PLIST ../mac/Warsow-Info.plist)
    endif()
    if (${QFUSION_GAME} MATCHES "Mental")
        set(QFUSION_GAME_DIR base)
        set(QFUSION_CLIENT_NAME mental)
        set(QFUSION_SERVER_NAME mental_server)
        set(QFUSION_APPLICATION_VERSION_HEADER \"version.mental.h\")
        set(QFUSION_MAC_ICON ../../icons/qfusion.icns)
        set(QFUSION_MAC_INFO_PLIST ../mac/Mental-Info.plist)
    endif()
endif()

# You can override this var with commandline option -DQFUSION_GAME_DIR=name
if (NOT QFUSION_GAME_DIR)
    set(QFUSION_GAME_DIR base)
endif()

# You can override this var with commandline option -DQFUSION_CLIENT_NAME=name
if (NOT QFUSION_CLIENT_NAME)
    set(QFUSION_CLIENT_NAME qfusion)
endif()

# You can override this var with commandline option -DQFUSION_SERVER_NAME=name
if (NOT QFUSION_SERVER_NAME)
    set(QFUSION_SERVER_NAME qfusion_server)
endif()

if (QFUSION_APPLICATION_VERSION_HEADER)
    add_definitions(-DAPPLICATION_VERSION_HEADER=${QFUSION_APPLICATION_VERSION_HEADER})
endif()

# Note: Defining _USE_MATH_DEFINES in the code is preferable as Recast does it on it's own
add_definitions(-DHALFLAMBERTLIGHTING
                -DAREAPORTALS_MATRIX
                -DTCP_SUPPORT
                -DHTTP_SUPPORT
                -DDOWNSCALE_ITEMS # Ugly hack for the release. Item models are way too big
                -DELECTROBOLT_TEST)

option(USE_SDL2 "Build using SDL2" OFF)
option(GAME_MODULES_ONLY "Only build game modules" OFF)
option(SERVER_ONLY "Only build server binaries and game modules" OFF)

# Windows specific settings
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(QFUSION_WINDOWS_ARCH "x86")

    if ((MINGW AND CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64") OR (CMAKE_SIZEOF_VOID_P EQUAL 8))
        set(QFUSION_WINDOWS_ARCH "x64")
    endif()

    set(CMAKE_INCLUDE_PATH
        "../libsrcs/src/libogg"
        "../libsrcs/src/libvorbis"
        "../libsrcs/src/libtheora"
        "../libsrcs/src/libcurl/include"
        "../libsrcs/src/zlib"
        "../libsrcs/src/libfreetype"
        "../libsrcs/src/SDL2/include"
    )

    add_definitions(-D_CRT_SECURE_NO_WARNINGS -DCURL_STATICLIB -DWIN32 -D_WINDOWS -DNOMINMAX)

    if (MSVC)
        set(CMAKE_LIBRARY_PATH "../libsrcs/lib/win32/${QFUSION_WINDOWS_ARCH}/debug")
    else()
        set(CMAKE_LIBRARY_PATH "../libsrcs/lib/win32/${QFUSION_WINDOWS_ARCH}/mingw")
    endif()

    if (MSVC)
        add_release_link_flags("/LTCG")

        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /source-charset:utf-8 /execution-charset:utf-8")
        set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS} /EHsc")

        set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /MD /W3 /GL /O2 /Ot /Ob2 /Oi")
        set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MDd /W3")
        set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /MD /W3 /GL /O2 /Ot /Ob2 /Oi")
        set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} /MDd /W3")
        set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MD /W3")
        set(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /MD /W3")
        set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} /MD /W3")
        set(CMAKE_C_FLAGS_MINSIZEREL "${CMAKE_C_FLAGS_MINSIZEREL} /MD /W3")
    endif()
endif()

# OS X specific settings
if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_OSX_ARCHITECTURES "$(ARCHS_STANDARD_64_BIT)")        # Compile only 64-bit version
    set(CMAKE_OSX_SYSROOT "macosx")                                # Compile with latest available OS X sdk
    set(CMAKE_XCODE_ATTRIBUTE_MACOSX_DEPLOYMENT_TARGET "10.8")     # Use 10.8 as deployment target
    set(CMAKE_XCODE_ATTRIBUTE_GCC_C_LANGUAGE_STANDARD "gnu99")     # Use GNU99 standart for compiling C files
    set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++17") # Use C++17 standart for compiling C++ files
    set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LIBRARY "libc++")          # Use modern libc++ instead of libstdc++
    set(CMAKE_FRAMEWORK_PATH "../libsrcs/lib/mac/Frameworks")
    set(CMAKE_MACOSX_RPATH 1)
    set(USE_SDL2 1)

    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wno-unused-function -fvisibility=hidden -Winvalid-pch")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -D_DEBUG")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -fno-strict-aliasing -ffast-math -fno-finite-math-only -funroll-loops -msse2")

    set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS}")
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE}")
endif()

# Linux or mingw specific settings
if (${CMAKE_SYSTEM_NAME} MATCHES "Linux" OR MINGW)
    # If no build type was specified - use release build settings
    if (NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE "Release")
    endif()

    add_definitions(-D_LARGEFILE64_SOURCE -D_LARGEFILE_SOURCE)

    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -fno-omit-frame-pointer -pipe -Wall -Wno-unused-function -fvisibility=hidden -Winvalid-pch -Werror=format")
    set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -Og -D_DEBUG")
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -fno-strict-aliasing -ffast-math -fno-finite-math-only -funroll-loops -msse2 -O2")

    set(CMAKE_CXX_FLAGS "${CMAKE_C_FLAGS}")

    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG}")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE}")

    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99 -D_POSIX_C_SOURCE=200112L -D_XOPEN_SOURCE=500")

    if (MINGW)
        message(FATAL_ERROR "MinGW is not going to be supported")
    endif()
endif()

# Linux-only specific settings
if (${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    if (CMAKE_CROSSCOMPILING)
        set(CMAKE_LIBRARY_ARCHITECTURE "${CMAKE_SYSTEM_PROCESSOR}-linux-gnu")
        link_directories("${CMAKE_FIND_ROOT_PATH}/usr/lib/${CMAKE_SYSTEM_PROCESSOR}-linux-gnu")
    endif()

    # Remove compilating flags from link options
    # string(REPLACE "<FLAGS>" "" CMAKE_C_LINK_EXECUTABLE "${CMAKE_C_LINK_EXECUTABLE}")
    # string(REPLACE "<LANGUAGE_COMPILE_FLAGS>" "" CMAKE_C_CREATE_SHARED_LIBRARY "${CMAKE_C_CREATE_SHARED_LIBRARY}")
    # string(REPLACE "<LANGUAGE_COMPILE_FLAGS>" "" CMAKE_C_CREATE_SHARED_MODULE "${CMAKE_C_CREATE_SHARED_MODULE}")

    # Remove soname link flags from library linking options
    string(REPLACE "<SONAME_FLAG><TARGET_SONAME>" "" CMAKE_C_CREATE_SHARED_LIBRARY "${CMAKE_C_CREATE_SHARED_LIBRARY}")
    string(REPLACE "<SONAME_FLAG><TARGET_SONAME>" "" CMAKE_C_CREATE_SHARED_MODULE "${CMAKE_C_CREATE_SHARED_MODULE}")

    # Remove -rdynamic from link options
    string(REPLACE "-rdynamic" "" CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "${CMAKE_SHARED_LIBRARY_LINK_C_FLAGS}")
endif()

if (NOT GAME_MODULES_ONLY)
    find_package(ZLIB REQUIRED)
    find_package(CURL REQUIRED)

    if (NOT SERVER_ONLY)
        find_package(Ogg REQUIRED)
        find_package(Vorbis REQUIRED)
        find_package(Freetype REQUIRED)
        find_package(Qt5 COMPONENTS Core Concurrent Quick Gui Svg Multimedia REQUIRED)

        # We need SDL only on non-Windows platforms or when with SDL2-backend
        if (USE_SDL2 OR (${CMAKE_SYSTEM_NAME} MATCHES "Linux"))
            set(USE_SDL2 1)
            find_package(SDL2 REQUIRED)
        endif()

        if (USE_SDL2)
            add_definitions(-DUSE_SDL2)
        endif()
    endif()
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Linux" OR MINGW)
    if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        STRING(TOLOWER "win32_${QFUSION_WINDOWS_ARCH}.tar.gz" TAR_GZ_NAME)
    else()
        STRING(TOLOWER "${CMAKE_SYSTEM_NAME}_${CMAKE_SYSTEM_PROCESSOR}.tar.gz" TAR_GZ_NAME)
    endif()
    if (NOT QFUSION_TAR_GZ_OUTPUT_DIRECTORY)
        set(TAR_GZ_DIST "${QFUSION_CLIENT_NAME}_${TAR_GZ_NAME}")
    else()
        set(TAR_GZ_DIST "${QFUSION_TAR_GZ_OUTPUT_DIRECTORY}/${QFUSION_CLIENT_NAME}_${TAR_GZ_NAME}")
    endif()
    add_custom_target(tar_gz COMMAND ${CMAKE_COMMAND} -E tar "cfvz" ${TAR_GZ_DIST} "${CMAKE_HOME_DIRECTORY}/build/")
endif()

set(DEFAULT_CMAKE_EXECUTABLE_SUFFIX "${CMAKE_EXECUTABLE_SUFFIX}")

if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
    set(CMAKE_EXECUTABLE_SUFFIX "_${QFUSION_WINDOWS_ARCH}${CMAKE_EXECUTABLE_SUFFIX}")
    set(CMAKE_SHARED_LIBRARY_PREFIX "")
    set(CMAKE_SHARED_LIBRARY_SUFFIX "_${QFUSION_WINDOWS_ARCH}${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()

if (NOT GAME_MODULES_ONLY)
    if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        find_windows_release_libs(ZLIB_LIBRARY)
        find_windows_release_libs(CURL_LIBRARY)
        if (NOT SERVER_ONLY)
            find_windows_release_libs(FREETYPE_LIBRARIES)
            find_windows_release_libs(OGG_LIBRARY)
            find_windows_release_libs(VORBIS_LIBRARIES)
            find_windows_release_libs(SDL2_LIBRARIES)
        endif()
    endif()
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Linux") # Do not add 'OR MINGW' here - we need to use suffix-prefix settings from Windows
    set(CURL_LIBRARY "")
    add_definitions(-DLIBCURL_RUNTIME)

    set(OGG_LIBRARY "")
    add_definitions(-DOGGLIB_RUNTIME)

    set(VORBIS_LIBRARIES "")
    add_definitions(-DVORBISLIB_RUNTIME)

    set(THEORA_LIBRARIES "")
    add_definitions(-DTHEORALIB_RUNTIME)

    set(FREETYPE_LIBRARIES "")
    add_definitions(-DFREETYPELIB_RUNTIME)

    set(ZLIB_LIBRARY "")
    add_definitions(-DZLIB_RUNTIME)

    set(CMAKE_EXECUTABLE_SUFFIX ".${CMAKE_SYSTEM_PROCESSOR}${CMAKE_EXECUTABLE_SUFFIX}")
    set(CMAKE_SHARED_LIBRARY_SUFFIX "_${CMAKE_SYSTEM_PROCESSOR}${CMAKE_SHARED_LIBRARY_SUFFIX}")
endif()

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    set(CMAKE_SHARED_LIBRARY_SUFFIX "_mac${CMAKE_SHARED_LIBRARY_SUFFIX}")

    capitalize(QFUSION_CLIENT_NAME)
endif()

add_subdirectory(common)
add_subdirectory(server)

if (NOT SERVER_ONLY)
    add_subdirectory(client)
endif()
