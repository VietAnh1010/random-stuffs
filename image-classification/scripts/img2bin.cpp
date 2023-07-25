#include <algorithm>
#include <cmath>
#include <ctime>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <jpeglib.h>

using namespace std;

static bool load_jpeg(const char* filename, vector<int>& out, int& out_height, int& out_width)
{
    // Try to load the jpeg image
    FILE* file = fopen(filename, "rb");
    if (!file) {
        cerr << "Could not open file: " << filename << endl;
        return false;
    }

    jpeg_decompress_struct cinfo;
    jpeg_error_mgr jerr;

    cinfo.err = jpeg_std_error(&jerr);
    jpeg_create_decompress(&cinfo);
    jpeg_stdio_src(&cinfo, file);

    if ((jpeg_read_header(&cinfo, TRUE) != JPEG_HEADER_OK) || (cinfo.data_precision != 8) ||
        !jpeg_start_decompress(&cinfo)) {
        cerr << "Could not decompress jpeg file: " << filename << endl;
        fclose(file);
        return false;
    }

    int width = out_width = cinfo.image_width;
    int height = out_height = cinfo.image_height;
    int depth = cinfo.num_components;

    vector<uint8_t> bits{};
    bits.resize(width * depth);
    out.resize(height * width);

    int i = 0;
    for (int y = 0; y < height; ++y) {
        JSAMPLE* row = static_cast<JSAMPLE*>(bits.data());
        if (jpeg_read_scanlines(&cinfo, &row, 1) != 1) {
            cerr << "Could not decompress jpeg file: " << filename << endl;
            fclose(file);
            return false;
        }
        for (int x = 0; x < width; x++) {
            int R = bits[x * depth + 0];
            int G = bits[x * depth + 1];
            int B = bits[x * depth + 2];
            int v = static_cast<int>(0.2989 * R + 0.5870 * G + 0.1140 * B);
            int b = v >= 200;
            out[i++] = b;
        }
    }
    jpeg_finish_decompress(&cinfo);
    fclose(file);
    return true;
}

void print_to_cout(const vector<int>& in, int height, int width)
{
    cout << "{\n";
    for (int i = 0; i < height; i++) {
        cout << "    ";
        for (int j = 0; j < width; j++) {
            cout << in[i * width + j] << ", ";
        }
        cout << '\n';
    }
    cout << "},\n";
}

std::vector<int> resize(const vector<int>& in, int in_height, int in_width, int out_height, int out_width)
{
    double window_height = static_cast<double>(in_height) / out_height;
    double window_width = static_cast<double>(in_width) / out_width;
    std::vector<int> out(out_height * out_width);
    for (int i = 0; i < out_height; i++) {
        for (int j = 0; j < out_width; j++) {
            int start_i = static_cast<int>(std::round(i * window_height));
            int start_j = static_cast<int>(std::round(j * window_width));

            int end_i = static_cast<int>(std::round((i + 1) * window_height));
            int end_j = static_cast<int>(std::round((j + 1) * window_width));
            int count1 = 0;
            int count0 = 0;

            for (int m = start_i; m < end_i; m++) {
                for (int n = start_j; n < end_j; n++) {
                    int bit = in[m * in_width + n];
                    if (bit)
                        count1++;
                    else
                        count0++;
                }
            }
            out[i * out_width + j] = count1 > count0;
        }
    }
    return out;
}

int main(int argc, char **argv)
{
    vector<int> buffer;
    vector<int> digit;
    int height;
    int width;

    #define PROCESS(n)                                                 \
        do {                                                           \
            load_jpeg("../OCRA/OCRA_" #n ".jpg", buffer, height, width);  \
            digit = resize(buffer, height, width, 24, 16);             \
            print_to_cout(digit, 24, 16);                              \
        } while (0)
    
    PROCESS(0);
    PROCESS(1);
    PROCESS(2);
    PROCESS(3);
    PROCESS(4);
    PROCESS(5);
    PROCESS(6);
    PROCESS(7);
    PROCESS(8);
    PROCESS(9);

    #undef PROCESS
    return 0;
}
