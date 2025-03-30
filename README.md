# qfusion

This is <a href="https://www.warsow.net/">Warsow</a>'s fork
of <a href="http://qfusion.github.io/qfusion/">qfusion</a>, the id Tech 2 derived game engine

## License (GPLv2)

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

## Build instructions

Clone the repository and its submodules

```
git clone --recursive https://github.com/Warsow/qfusion
```

### Building dependencies

You have to navigate to `qfusion/third-party` and build CMake-based `angelscript` and `openal-soft` subprojects.
This has to be done once upon initial setup.

We use our custom static build of Qt. You have to download Qt 5.13.2 source bundle separately. MD5 Checksums are

```
7c04c678d4ecd9e9c06747e7c17e0bb9  qt-everywhere-src-5.13.2.tar.xz
39a0465610f70d9f877f42fc5337d1ac  qt-everywhere-src-5.13.2.zip
```

Regardless of the actual platform, you have to apply this patch:

<details>
<summary>Qml GC modification</summary>

```
diff --git a/qtdeclarative/src/qml/memory/qv4mm.cpp b/qtdeclarative/src/qml/memory/qv4mm.cpp
index cac68bdca..171ba74a4 100644
--- a/qtdeclarative/src/qml/memory/qv4mm.cpp
+++ b/qtdeclarative/src/qml/memory/qv4mm.cpp
@@ -64,6 +64,8 @@
 #include "qv4mapobject_p.h"
 #include "qv4setobject_p.h"

+size_t qt_wswHeapSizeGCThreshold;
+
//#define MM_STATS

#if !defined(MM_STATS) && !defined(QT_NO_DEBUG)
diff --git a/qtdeclarative/src/qml/memory/qv4mm_p.h b/qtdeclarative/src/qml/memory/qv4mm_p.h
index 6dfdd81cb..f8099786b 100644
--- a/qtdeclarative/src/qml/memory/qv4mm_p.h
+++ b/qtdeclarative/src/qml/memory/qv4mm_p.h
@@ -64,6 +64,8 @@

#define MM_DEBUG 0

+extern size_t qt_wswHeapSizeGCThreshold;
+
QT_BEGIN_NAMESPACE

namespace QV4 {
@@ -296,36 +298,19 @@ private:

     HeapItem *allocate(BlockAllocator *allocator, std::size_t size)
     {
-        bool didGCRun = false;
         if (aggressiveGC) {
             runGC();
-            didGCRun = true;
-        }
-
-        if (unmanagedHeapSize > unmanagedHeapSizeGCLimit) {
-            if (!didGCRun)
+        } else {
+            // Note: We don't care of hugeItemAllocator as it stays unused
+            // in our setup and tracking of the actual size is complicated.
+            if (unmanagedHeapSize + (blockAllocator.allocatedMem() + icAllocator.allocatedMem()) > qt_wswHeapSizeGCThreshold) {
                 runGC();
-
-            if (3*unmanagedHeapSizeGCLimit <= 4 * unmanagedHeapSize) {
-                // more than 75% full, raise limit
-                unmanagedHeapSizeGCLimit = std::max(unmanagedHeapSizeGCLimit,
-                                                    unmanagedHeapSize) * 2;
-            } else if (unmanagedHeapSize * 4 <= unmanagedHeapSizeGCLimit) {
-                // less than 25% full, lower limit
-                unmanagedHeapSizeGCLimit = qMax(std::size_t(MinUnmanagedHeapSizeGCLimit),
-                                                unmanagedHeapSizeGCLimit/2);
             }
-            didGCRun = true;
         }

-        if (size > Chunk::DataSize)
-            return hugeItemAllocator.allocate(size);
-
-        if (HeapItem *m = allocator->allocate(size))
-            return m;
-
-        if (!didGCRun && shouldRunGC())
-            runGC();
+        if (size > Chunk::DataSize) {
+            return hugeItemAllocator.allocate( size );
+        }

         return allocator->allocate(size, true);
  }
```
</details>

Platform-specific build instructions follow.

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

### Building and running the engine

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