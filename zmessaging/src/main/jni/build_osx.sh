#!/bin/bash
# build native lib for osx - used in tests
cd "$(dirname "$0")"
g++ -I $JAVA_HOME/include/ -I $JAVA_HOME/include/darwin/ -dynamiclib -o ../../../target/android/intermediates/ndk/jni/osx/liblzw-decoder.dylib LzwDecoder.cpp
