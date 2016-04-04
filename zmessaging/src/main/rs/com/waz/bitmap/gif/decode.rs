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
#pragma version(1)
#pragma rs_fp_inprecise
#pragma rs java_package_name(com.waz.bitmap.gif)

rs_allocation image;
rs_allocation pixels;  // pixels array for whole image
rs_allocation colors;  // current color table

uint32_t width;
uint32_t height;
uint32_t inputSize;

uint32_t fx, fy, fw, fh; // current frame dimensions
uint8_t transIndex; // transparent color index
uchar4 bgColor;
int32_t interlace;
int32_t transparency;

typedef uint8_t byte;

static const uint32_t MAX_STACK_SIZE = 4096;
static const uint32_t PIXEL_STACK_SIZE = 8192;
static const int NULL_CODE = -1;

void __attribute__((kernel)) clear(uint32_t line) { // clear a line (but only in current frame)
    if (line >= fy && line < fy + fh) {
        int end = fx + fw;
        for (int i = fx; i < end; ++i) {
            rsSetElementAt_uchar4(pixels, bgColor, i, line);
        }
    }
}

void __attribute__((kernel)) decode(uint32_t in) {
    int idx = 0;
    int npix = fw * fh;
    int available, clear, code_mask, code_size, end_of_information, in_code, old_code, bits, code, count, i, datum, data_size, first, top, bi, pi;
    uchar4 color = 0;

    short prefix[MAX_STACK_SIZE];
    uint8_t suffix[MAX_STACK_SIZE];
    uint8_t pixelStack[PIXEL_STACK_SIZE];
    uint8_t block[256];

    // Initialize GIF data stream decoder.
    data_size = rsGetElementAt_uchar(image, idx++);
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
                count = rsGetElementAt_uchar(image, idx++);
                if (idx + count > inputSize) break; // bounds check
                for (int k = 0; k < count; ++k) {
                    block[k] = rsGetElementAt_uchar(image, idx++);
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
        if (code >= MAX_STACK_SIZE) return;
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
            if (top >= PIXEL_STACK_SIZE) return;
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
                    color = rsGetElementAt_uchar4(colors, pixelStack[top]).bgra;
                    rsSetElementAt_uchar4(pixels, color, x, line);
                }
                ++x;
            }
        }
    }
}
