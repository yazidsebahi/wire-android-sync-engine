/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include <jni.h>

#ifndef LzwDecoder_h
#define LzwDecoder_h

typedef unsigned int uint;
typedef unsigned char byte;

class LzwDecoder {

    public:
        LzwDecoder(byte* image, uint* pixels, uint* colors, uint width, uint height) {
            this->image = image;
            this->pixels = pixels;
            this->colors = colors;
            this->width = width;
            this->height = height;
        };

        void clear(int x, int y, int w, int h, uint color);

        void decode(int x, int y, int w, int h, uint inputSize, int transIndex, uint bgColor, bool interlace, bool transparency);

    private:
        byte* image;
        uint* pixels;  // pixels array for whole image
        uint* colors;  // current color table

        int width;
        int height;

};

#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT long JNICALL Java_com_waz_bitmap_gif_LzwDecoder_init(JNIEnv *env, jobject obj, jobject image, jobject pixels, jobject colors, jint width, jint height) {
    return (long) (new LzwDecoder((byte*) env->GetDirectBufferAddress(image), (uint*) env->GetDirectBufferAddress(pixels), (uint*) env->GetDirectBufferAddress(colors), width, height));
}

JNIEXPORT void JNICALL Java_com_waz_bitmap_gif_LzwDecoder_destroy(JNIEnv *env, jobject obj, jlong decoder) {
    delete (LzwDecoder*) decoder;
}

JNIEXPORT void JNICALL Java_com_waz_bitmap_gif_LzwDecoder_clear(JNIEnv *env, jobject obj, jlong decoder, jint x, jint y, jint w, jint h, jint color) {
    ((LzwDecoder*) decoder)->clear(x, y, w, h, color);
}

JNIEXPORT void JNICALL Java_com_waz_bitmap_gif_LzwDecoder_decode(JNIEnv *env, jobject obj, jlong decoder, jint x, jint y, jint w, jint h, jint inputSize, jint transIndex, jint bgColor, jboolean interlace, jboolean transparency) {
    ((LzwDecoder*) decoder)->decode(x, y, w, h, inputSize, transIndex, bgColor, interlace, transparency);
}

#ifdef __cplusplus
}
#endif
#endif