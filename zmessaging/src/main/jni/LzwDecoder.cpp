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

#include "LzwDecoder.h"

#ifdef __ANDROID__
#include <android/log.h>
#define LOG(...) __android_log_print(ANDROID_LOG_VERBOSE,"LzwDecoder_native",__VA_ARGS__)
#endif

#ifndef __ANDROID__
#include <stdio.h>
#define LOG(...) printf(__VA_ARGS__)
#endif

static const uint MAX_STACK_SIZE = 4096;
static const uint PIXEL_STACK_SIZE = 8192;
static const int NULL_CODE = -1;


void LzwDecoder::clear(int x, int y, int w, int h, uint color) {
    uint* dst = pixels + (x + y * width);
    for (int l = 0; l < h; ++l) {
        for (int i = 0; i < w; ++i) {
            (*dst++) = color;
        }
        dst += width - w - x;
    }
}

void LzwDecoder::decode(int fx, int fy, int fw, int fh, uint inputSize, int transIndex, uint bgColor, bool interlace, bool transparency) {
    uint idx = 0;
    uint npix = fw * fh;
    uint available, clear, code_mask, code_size, end_of_information, in_code, old_code, bits, code, count, i, datum, data_size, first, top, bi, pi;

    unsigned short prefix[MAX_STACK_SIZE];
    byte suffix[MAX_STACK_SIZE];
    byte pixelStack[PIXEL_STACK_SIZE];
    byte block[256];

    // Initialize GIF data stream decoder.
    data_size = image[idx++];
    clear = 1 << data_size;
    end_of_information = clear + 1;
    available = clear + 2;
    old_code = NULL_CODE;
    code_size = data_size + 1;
    code_mask = (1 << code_size) - 1;
    if (clear > MAX_STACK_SIZE) return;
    for (code = 0; code < clear; code++) {
        prefix[code] = 0;
        suffix[code] = (byte)code;
    }

    // Decode GIF pixel stream.
    datum = bits = count = first = top = pi = bi = 0;
    int endX = fx + fw;
    int x = fx;

    // line number with interlacing
    int end = fy + fh;
    int pass = 1;
    int inc = interlace ? 8 : 1;
    int line = fy;

    for (i = 0; i < npix;) {
        if (bits < code_size) {
            // Load bytes until there are enough bits for a code.
            if (count == 0) {
                // Read a new data block.
                if (idx >= inputSize) break; //bounds check
                count = image[idx++];
                if (idx + count > inputSize) break; // bounds check
                for (int k = 0; k < count; ++k) {
                    block[k] = image[idx++];
                }

                if (count <= 0) {
                    break;
                }
                bi = 0;
            }
            datum += (block[bi] & 0xff) << bits;
            bits += 8;
            bi++;
            count--;
            continue;
        }
        // Get the next code.
        code = datum & code_mask;
        datum >>= code_size;
        bits -= code_size;
        if (code >= MAX_STACK_SIZE) {
            return;
        }
        // Interpret the code
        if (code == end_of_information) {
            break;
        }
        if (code == clear) {
            // Reset decoder.
            code_size = data_size + 1;
            code_mask = (1 << code_size) - 1;
            available = clear + 2;
            old_code = NULL_CODE;
            continue;
        }
        if (old_code == NULL_CODE) {
            pixelStack[top++] = suffix[code];
            old_code = code;
            first = code;
            continue;
        }
        in_code = code;
        if (code >= available) {
            pixelStack[top++] = (byte)first;
            code = old_code;
        }
        while (code > clear) {
            if (top >= PIXEL_STACK_SIZE) {
                return;
            }
            pixelStack[top++] = suffix[code];
            code = prefix[code];
        }
        first = suffix[code] & 0xff;
        pixelStack[top++] = (byte)first;

        if (available < MAX_STACK_SIZE) {
            prefix[available] = (short)old_code;
            suffix[available] = (byte)first;
            available++;
            if (((available & code_mask) == 0) && (available < MAX_STACK_SIZE)) {
                code_size++;
                code_mask += available;
            }
        }
        old_code = in_code;

        // Pop pixels off the pixel stack.
        while (top > 0 && i < npix) {
            if (x == endX) {
                x = fx;
                line += inc;
                if (interlace) {
                    while (line >= end && pass < 5) {
                        switch (++pass) {
                            case 2:
                                line = fy + 4;
                                break;
                            case 3:
                                line = fy + 2;
                                inc = 4;
                                break;
                            case 4:
                                line = fy + 1;
                                inc = 2;
                                break;
                            default:
                                line = fy;
                                inc = 1;
                                break;
                        }
                    }
                }
            }

            --top;
            ++i;
            if (line < end && x < endX) {
                if (!transparency || pixelStack[top] != transIndex) {
                    pixels[x + line * width] = colors[pixelStack[top]];
                }
                ++x;
            }
        }
    }
}


