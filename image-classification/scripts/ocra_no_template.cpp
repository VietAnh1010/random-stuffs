#include <string>
#include <iostream>
#include <algorithm>

#include "image.hpp"

using namespace std;

struct Schar
{
    int xmin, ymin, xmax, ymax;
};

bool is_pc(int x, int y, unsigned th, const Image &image)
{
    return x >= 0 && y >= 0 && x < image.width && y < image.height && th > image.pixels[x + y * image.width];
}

bool findchar(int &x, int &y, unsigned th, const Image &image)
{
    for (; x < image.width; x += 8) { // 8 is magic number. I think x++ is fine?
        for (; y < image.height; y++) {
            if (is_pc(x, y, th, image))
                return true;
        }
        y = 0;
    }
    return false;
}

// this is the algorithm to remember and to learn
// use this algorithm for edge tracing
Schar outline(int &sx, int &sy, unsigned th, Image &image)
{
    int xmin = image.width, ymin = image.height, xmax{}, ymax{}, x = sx, y = sy, sd, d{};
    if (is_pc(x, y - 1, th, image))
        d = 3;
    else if (is_pc(x + 1, y, th, image))
        d = 2;
    else if (is_pc(x, y + 1, th, image))
        d = 1;
    sd = d;
    auto moved = [&](int cx, int cy) {
        std::cout << "direction: " << d << '\n';
        std::cout << "moved\n";
        image.pixels[cy * image.width + cx] = 0;
    };
    do
    {
        switch (d)
            do
            {
            case 0:
                if (is_pc(x, y + 1, th, image))
                {
                    y++;
                    ymax = max(ymax, y);
                    d = 3;
                    moved(x, y);
                    break;
                }
            case 1:
                if (is_pc(x + 1, y, th, image))
                {
                    x++;
                    xmax = max(xmax, x);
                    d = 0;
                    moved(x, y);
                    break;
                }
            case 2:
                if (is_pc(x, y - 1, th, image))
                {
                    y--;
                    ymin = min(ymin, y);
                    d = 1;
                    moved(x, y);
                    break;
                }
            case 3:
                if (is_pc(x - 1, y, th, image))
                {
                    x--;
                    xmin = min(xmin, x);
                    d = 2;
                    moved(x, y);
                    break;
                }
                std::cout << "inside inner while loop\n";
            } while (1);
            std::cout << "inside outer while loop\n";
    } while (x != sx || y != sy || d != sd);
    sx = xmax + 1;
    sy = (ymin + ymax) / 2;
    return {xmin, ymin, xmax, ymax};
}

// yb: y begin
// ye: y end
// bool b: represents whether we are scanning the image the normal way, or have we swapped the x and y corrdinate
// sc: boundary of the digit
// th: threshold to binarize the image
// image: reference to the original image
pair<int, int> ninters(int yb, int ye, bool b, Schar sc, unsigned th, const Image &image)
{
    // remember:
    // +-------------------> x
    // |
    // |
    // |
    // |
    // v y


    // imagine that there's a wall to the right of the digit

    int f{}, // magic bitmask
             // the stripe is splitted into 10 parts
             // this number registers which parts contains some pixel inside it
             // we will then use the to identify the digit
    yy{},    // the row index where we find the minimum number of gaps. Remember, the final bound
             // is the "wall" to the right of the digit
    nc = 4,  // the minimum number of gaps
    xw = (sc.xmax - sc.xmin) * 3 / 10 + 1; // I guess this is the windown width, along the x-axis
    for (int c{}, y = yb; y <= ye; y++) // scan row-by-row
    {
        for (int n{}, x = sc.xmin; x <= sc.xmax + 1; x++) // scan column-by-column, but why +1?
        {
            // when scans normally, only the left condition can be true. 
            // vice-versa for the flipped scenario
            if ((b && is_pc(x, y, th, image)) || (!b && is_pc(y, x, th, image)))
            // if the pixel is good
            {
                n++; // number of good pixel?
                if (n > xw) // if the number of pixels (consecutively) 
                            // is greater than the width: break, and we continue with the
                            // next row
                {
                    c = 0; // what does this mean? just reset the variable so that we can safely
                           // continue the next iteration
                    break; // continue with the next row
                }
            }
            else if (n) // if there is at least 1 pixel (in this row) and the pixel that we are
                        // currently considering is in the background
            {
                c++;    // the number of "gaps" between the 2 strokes?
                n = 0;  // set n back to 0, so that this block will not run again if we continue to
                        // hit this condition in the next iteration
            }
        }

        if (c && c < nc) // if there is a gap, and the gap is smaller than the best one so far
        {
            nc = c;      // update the best one
            yy = y;      // and update the row index where we find this gap
        }
        c = 0;           // also, at the end of each iteration, remember to reset c back to 0
                         // to prepare for the next iteration
    }

    // one more time, we scan the column where we find the maximum number of gaps
    for (int n{}, xsum{}, x = sc.xmin; x <= sc.xmax + 1; x++)
    {
        if (is_pc(x, yy, th, image)) // if the pixel is good
        {
            xsum += x - sc.xmin; // sum of the distance between all good pixels and the left bound
                                 // of the image
            n++;                 // increment the number of consecutive pixels
        }
        else if (n)              // if we are on a line of pixels, and suddenly we hit a background
        {
            // f here is yet another magic number? or I think it is just bit manipulation lol
            // let see
            f |= 1 << (xsum * 10 / (n * (sc.xmax - sc.xmin) + 1));
            xsum = 0,  // reset for later iterations
            n = 0;     // reset for later iterations
        }
    }
    return {nc, f};
}

char recz(Schar sc, unsigned th, const Image &image)
{
    // up
    auto [ncu, fu] = ninters(sc.ymin + (sc.ymax - sc.ymin) * 2 / 9, sc.ymin + (sc.ymax - sc.ymin) * 3 / 9 + 1, true, sc, th, image);
    // down
    auto [ncd, fd] = ninters(sc.ymin + (sc.ymax - sc.ymin) * 6 / 9, sc.ymin + (sc.ymax - sc.ymin) * 7 / 9 + 1, true, sc, th, image);
    swap(sc.xmin, sc.ymin);
    swap(sc.xmax, sc.ymax);
    // flip x and y coordinates
    // ff is useless by the way
    auto [ncv, ff] = ninters(sc.ymin + (sc.ymax - sc.ymin) * 2 / 5, sc.ymin + (sc.ymax - sc.ymin) * 3 / 5 + 1, false, sc, th, image);
    switch (ncu + ncd + ncd) // 1 stripe up and 2 stripes down
    {
    case 3:
    {
        if (fu & 896 && fd & 7)
            return '2';
        else if (fu & 896 && fd & 896)
            return '3';
        else if (fu & 30 && fd & 896)
            return '5';
        else if (fu & 896 && fd & 120)
            return '7';
        break;
    }
    case 4:
    {
        if (ncv == 1)
            return '4';
        else if (ncv == 2)
            return '9';
        break;
    }
    case 5:
    {
        if (fu & 120 && fd & 127)    // the reason why we need && is because we need enough
                                     // "evidence" to prove that the image is a digit
                                     // otherwise we will not accept it as a number
                                     // this can be more robust
                                     // we need a method to immediately disprove that a region is
                                     // noises and not digit
            return '1';
        else if (fu & 7 && fd & 903)
            return '6';
        break;
    }
    case 6:
    {
        if (ncv == 2)
            return '0';
        else if (ncv == 3)
            return '8';
        break;
    }
    }
    return 'e';
}

string ocr(Image &image)
{
    auto [tmin, tmax] = minmax_element(image.pixels.begin(), image.pixels.end());
    unsigned th = (*tmax + *tmin) / 2;
    int sx = 8, sy = 0;
    if (!findchar(sx, sy, th, image))
        return "";
    string ans;
    while (sx < image.width)
    {
        ans += recz(outline(sx, sy, th, image), th, image);
        for (; sx < image.width && !is_pc(sx, sy, th, image); sx++)
            ;
    }
    return ans;
}

int main()
{
    int h = 5;
    int w = 5;
    Image image{w, h, {
        2, 1, 2, 1, 1,
        2, 1, 2, 1, 1,
        2, 1, 2, 2, 2,
        2, 1, 1, 1, 1,
        2, 2, 2, 2, 2
    }};

    int sx = 1;
    int sy = 1;
    outline(sx, sy, 2, image);
    std::cout << "sx: " << sx << ", sy: " << sy << '\n';
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            int p = i * w + j;
            if (!image.pixels[p]) {
                std::cout << '_';
            } else {
                std::cout << 'x';
            }
        }
        std::cout << '\n';
    }
    return 0;
}