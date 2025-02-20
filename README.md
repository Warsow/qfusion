
# Contents

- [Contents](#contents)
- [What is qfusion?](#what-is-qfusion)
  - [Main features](#main-features)
- [Build Instructions](#build-instructions)
  - [Building dependencies](#building-dependencies)
    - [Linux](#linux)
    - [Windows](#windows)
  - [Building and Running The Engine](#building-and-running-the-engine)
- [Contributions](#contributions)
  - [Pull Requests](#pull-requests)
  - [Issues and Help](#issues-and-help)
- [License](#license)


# What is qfusion

This is <a href="https://www.warsow.net/">Warsow</a>'s fork
of <a href="http://qfusion.github.io/qfusion/">qfusion</a>, the id Tech 2 derived game engine

## Main Features

- Fully open-source under the GPLv2 and easy to mod
- Runs on Linux, Windows and Android (in development)
- Modern and fast OpenGL 3.0 and OpenGL ES 3.0 renderer, running in a dedicated program thread
- Realtime lighting and shadows via omnidirectional and cascaded shadow maps
- Support for vertex and skeletal animation
- HDR & Bloom support with configurable color correction profiles
- Fullscreen Anti-Aliasing support in the form of MSAA or FXAA
- Powerful multiplayer & eSports features (global stats, friend lists, IRC, TV-server, etc.)
- Ready to go FPS example gametype scripts from Warsow
- Multithreaded sound mixer design
- OpenAL support
- Hardware-accelerated Ogg Theora video playback

# Build Instructions

Clone the repository and its submodules

```
git clone --recursive https://github.com/Warsow/qfusion
```

## Building Dependencies

You have to navigate to `qfusion/third-party` and build CMake-based `angelscript` and `openal-soft` subprojects.
This has to be done once upon initial setup.

We use our custom static build of Qt. You have to download Qt 5.13.2 source bundle separately. MD5 Checksums are

```
7c04c678d4ecd9e9c06747e7c17e0bb9  qt-everywhere-src-5.13.2.tar.xz
39a0465610f70d9f877f42fc5337d1ac  qt-everywhere-src-5.13.2.zip
```

Build instructions follow.

<details>
<summary>Linux</summary>
Assuming that you unpack and build stuff in `/opt/qt/` (which is assumed to be modifiable for your user),
navigate to `/opt/qt/qt-everywhere-src-5.13.2/qtbase/src/platformsupport` and modify `platformsupport.pro`
using this patch

```
-qtConfig(evdev)|qtConfig(tslib)|qtConfig(libinput)|qtConfig(integrityhid) {
+qtConfig(evdev)|qtConfig(tslib)|qtConfig(libinput)|qtConfig(integrityhid)|qtConfig(xkbcommon) {
```

(https://gitweb.gentoo.org/proj/qt.git/commit/?id=1c7312e8264050c2c4e4c4feb7522339e66f3743)

Configure Qt using these feature flags

```
/opt/qt/qt-everywhere-src-5.13.2$ ./configure \
-prefix /opt/qt/5.13.2 -static -release -opensource -confirm-license -opengl desktop \
-no-gif -no-ico -no-libjpeg -no-tiff -no-webp -no-sql-sqlite -no-sql-odbc -no-system-proxies \
-no-icu -no-dbus -no-evdev -no-egl -no-eglfs -no-linuxfb -no-iconv -no-alsa -no-pulseaudio \
-nomake tools -nomake examples -nomake tests -skip wayland -skip qtconnectivity -skip qtscript \
-skip qtdoc -skip qtdocgallery -skip qtactiveqt -skip qtcharts -skip qt3d -skip qtdatavis3d \
-skip qtgamepad -skip qtlocation -skip qtlottie -skip qtandroidextras -skip qtwinextras \
-skip qtx11extras -skip qtmacextras -skip qtnetworkauth -skip qtserialport -skip qtserialbus \
-skip qtpurchasing -skip qttranslations -skip qtremoteobjects -skip qtsensors -skip qtspeech \
-skip qtvirtualkeyboard -skip qtwayland \
-skip qtwebchannel -skip qtwebglplugin -skip qtwebengine -skip qtwebview -skip qtxmlpatterns \
-no-feature-testlib -no-feature-testlib_selfcover -no-feature-sql -no-feature-sqlmodel -no-feature-sessionmanager \
-no-feature-quick-designer -no-feature-quick-canvas -no-feature-qml-debug -no-feature-qml-profiler \
-no-feature-qml-preview -no-feature-qml-worker-script -no-feature-quick-particles \
-no-feature-quickcontrols2-fusion -no-feature-quickcontrols2-imagine -no-feature-quickcontrols2-universal \
-no-feature-codecs -no-feature-big_codecs -no-feature-pdf -no-feature-cssparser -no-feature-textodfwriter \
-no-feature-vulkan \
-qt-freetype -qt-harfbuzz -qt-xcb -qt-pcre -no-avx512 -silent --recheck-all
```

If it fails at "building qmake" stage, you have to modify respective sources/headers,
so they include `<limits>` for modern toolchains.

```
/opt/qt/qt-everywhere-src-5.13.2$ gmake -j$(nproc)
/opt/qt/qt-everywhere-src-5.13.2$ gmake install
```
</details>

<details>

<summary>Windows</summary>
Configure Qt using these feature flags (TODO: Strip more features following the Linux build)

```
configure -static -release -opensource -confirm-license -opengl desktop ^
-no-gif -no-ico -no-libjpeg -no-tiff -no-sql-sqlite -no-sql-odbc -no-qml-debug -no-system-proxies ^
-nomake tools -nomake examples -nomake tests -skip qtconnectivity -skip qtscript -skip qtdoc -skip qtactiveqt ^
-skip qtcharts -skip qt3d -skip qtdatavis3d -skip qtgamepad -skip qtlocation -skip qtlottie -skip qtandroidextras ^
-skip qtwinextras -skip qtx11extras -skip qtmacextras -skip qtnetworkauth -skip qtserialport -skip qtserialbus ^
-skip qtpurchasing -skip qttranslations -skip qtremoteobjects -skip qtsensors -skip qtspeech -skip qtvirtualkeyboard ^
-skip qtwayland -skip qtwebchannel -skip qtwebengine -skip qtwebview -skip qtxmlpatterns ^
-no-feature-testlib -no-feature-testlib_selfcover -no-feature-sql -no-feature-sqlmodel -no-feature-sessionmanager ^
-no-feature-quick-designer -no-feature-quick-canvas -no-feature-qml-profiler -no-feature-qml-preview ^
-no-feature-codecs -no-feature-big_codecs -no-feature-pdf -no-feature-cssparser -no-feature-textodfwriter -silent
```

Visit https://doc.qt.io/qt-5/windows-building.html for more information
</details>

## Building and Running The Engine

Linux command line for configuration looks like this (assuming Qt is built and installed into `/opt/qt/5.13.2`)

```
qfusion/source$ cmake -DQFUSION_GAME="Warsow" -DCMAKE_PREFIX_PATH=/opt/qt/5.13.2 .
```

You can use various CMake configuration tools of your choice (this is aimed to Windows users).
Just pass these two important variables (`QFUSION_GAME`, `CMAKE_PREFIX_PATH`) to the configuration.

```
qfusion/source$ make -j$(nproc)
```

Executables are found in `qfusion/source/build` subdirectory.

Having built the engine, you have run it from a directory which contains `basewsw` subdirectory with data files
of Warsow 2.6+ (visit Warsow website/Warsow Discord for details).


# Contributions

We greatly appreciate any contributions to this project and want to make it as transparent as possible

## Pull Requests
We welcome your pull requests greatly.

- Fork the repo and create your branch from main.
- If you've added code that should be tested, add tests.
- If you've changed APIs, update the documentation.
- Ensure all tests and GitHub Actions workflows checks pass.
- Make sure your code lints.

## Issues and Help
We use GitHub issues to track any bugs, help requests, duplicates, enhancements, or questions. Please ensure your description is clear enough so that an issue can be reproduced or so that someone else can understand how they can help you.

# License

The qfusion software is under <a href="https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html">(GPLv2)</a> license.

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
