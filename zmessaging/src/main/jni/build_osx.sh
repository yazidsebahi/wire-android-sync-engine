#!/bin/bash
# build native lib for osx - used in tests
cd "$(dirname "$0")"
java_home="${JAVA_HOME-`/usr/libexec/java_home -v 1.8`}"
g++ -Wall -pedantic -Wno-variadic-macros -I $java_home/include/ -I $java_home/include/darwin/ -dynamiclib -o ../../../target/android/intermediates/ndk/jni/osx/liblzw-decoder.dylib LzwDecoder.cpp || echo "OSX build failed"
