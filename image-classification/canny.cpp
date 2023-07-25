/**
 * Build canny edge detection algorithm
 */

// What are the steps:
// First: we need to remove the noise, using 5x5 Guassian filter
// Second: find intensity gradient, using Sobel kernel
// Non-max suppression: for each point: we use the direction of the gradient, look at 2 neighbors,
// and put this pixel to 0 (suppressed) iff it's not a local maximum
// Result of this steps are possible "candidates" of the edge
// Hysteresis thresholding:
// We provide maxVal and minVal
// for all pixels that are greater than maxVal, we are 100% sure that they are on the edge
// for all pixels that are lower than minVal, we are 100% sure that they are not on the edge
// for the remaining, undecided pixels: we consider their connectivity. If they are connected to pixels
// that are guranteed on edge, we will then decided that this is an edge

#include <vector>
#include <utility>
#include <cmath>
#include <iostream>

#include "matrix.hpp"

template<typename T>
void print_matrix(const Matrix<T>& matrix)
{
    auto [nr, nc] = matrix.dims();
    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            if (c)
                std::cout << ',';
            std::cout << matrix.get_element(r, c);
        }
        std::cout << '\n';
    }
}

// Matrix<double> guassian_filter(const Matrix<double>& matrix)
// {
//     const Matrix<double> filter{5, 5, {
//         0.013, 0.025, 0.031, 0.025, 0.013,
//         0.025, 0.057, 0.075, 0.057, 0.025, 
//         0.031, 0.075, 0.094, 0.075, 0.031,
//         0.025, 0.057, 0.075, 0.057, 0.025,
//         0.013, 0.025, 0.031, 0.025, 0.013,
//     }};
//     return convolve(matrix, filter);
// }


Matrix<double> sobel_r_filter(const Matrix<double>& matrix)
{
    const Matrix<double> filter(3, 3, {
        -1, -2, -1,
        0, 0, 0,
        1, 2, 1
    });
    return convolve(matrix, filter);
}

Matrix<double> sobel_c_filter(const Matrix<double>& matrix)
{
    const Matrix<double> filter(3, 3, {
        -1, 0, 1,
        -2, 0, 2,
        -1, 0, 1
    });
    return convolve(matrix, filter);
}

template<typename T, typename U, typename R>
Matrix<R> combine(const Matrix<T>& lmatrix, const Matrix<U>& rmatrix, auto fn)
{
    auto [nr, nc] = lmatrix.dims();
    Matrix<R> result{nr, nc};
    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            T le = lmatrix.get_element(r, c);
            U re = rmatrix.get_element(r, c);
            result.set_element(r, c, fn(le, re));
        }
    }
    return result;
}

auto canny_edge_detect(const Matrix<double>& matrix, double max_val = 0.75, double min_val = 0.25)
{
    auto [nr, nc] = matrix.dims();
    const Matrix<double> blured_matrix = guassian_filter(matrix);
    const Matrix<double> gr = sobel_r_filter(blured_matrix); // gradient in the r direction
    const Matrix<double> gc = sobel_c_filter(blured_matrix); // gradient in the c direction
    Matrix<double> ge = combine<double, double, double>(gr, gc, [](double vr, double vc) { 
        return std::sqrt(vr * vr + vc * vc); 
    });
    // do we need to scale back to 0 .. 255?

    double m = ge.max_element();
    ge = apply<double, double>(ge, [m](double ve) { return ve / m; });

    const double pi = std::atan2(0, -1);
    const Matrix<int> gd = combine<double, double, int>(gr, gc, [pi](double vr, double vc) {
        double theta = std::atan2(vr, vc);
        int direction = static_cast<int>(std::round(theta * 4 / pi));
        return (direction % 4 + 4) % 4;
    });


    const int dr[4] = {0, 1, 1, 1};
    const int dc[4] = {1, 1, 0, -1};

    // hysteresis thredsholding
    Matrix<int> result = Matrix<int>{nr, nc};
    for (int r = 0; r < nr; r++) {
        for (int c = 0; c < nc; c++) {
            int dir = gd.get_element(r, c);
            double me = ge.get_element(r, c);
            double le = ge.get_element_or(r - dr[dir], c - dc[dir], 0);
            double re = ge.get_element_or(r + dr[dir], c + dc[dir], 0);
            if (me > le && me > re && me > max_val) // local maximum
                result.set_element(r, c, 1); // 1 means we are on edge
            else
                result.set_element(r, c, 0);
        }
        // check iff local element
    }

    // more stuff later
    return result;
}
/*
int main()
{
    int h = 24;
    int w = 13;
    Matrix<double> test_matrix{
        24, 13, {
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 
			0xff, 0xff, 0xff, 0xc0, 0xad, 0xad, 0xad, 0xad, 0xaf, 0xe8, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xda,  0x2,  0x0,  0x0,  0x0,  0x0,  0x0, 0x3c, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0x28, 0x2e,  0xe,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0xff, 0xff, 0x50,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0xff, 0xff, 0x50,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0xff, 0xff, 0x50,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0xff, 0xff, 0x50,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xff, 0xd4,  0x0,  0x0, 0xff, 0xff, 0x50,  0x0, 0x37, 0xff, 0xff, 0xff,
			0xff, 0xd3, 0x42,  0x0,  0x0, 0xff, 0xff, 0x12,  0x0,  0xe, 0x81, 0xf3, 0xff,
			0xd9,  0xb,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0,  0x0, 0x4f, 0xff,
			0xb0,  0x0,  0xb, 0x43, 0x43, 0x43, 0x43, 0x43, 0x43, 0x35,  0x0,  0xa, 0xf1,
			0xad,  0x0, 0x2b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc8,  0x0,  0x0, 0xe8,
			0xad,  0x0, 0x2b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc8,  0x0,  0x0, 0xe8,
			0xad,  0x0, 0x2b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc8,  0x0,  0x0, 0xe8,
			0xad,  0x0, 0x2b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc8,  0x0,  0x0, 0xe8,
			0xad,  0x0, 0x2b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xc8,  0x0,  0x0, 0xe8,
			0xb2,  0x0,  0x8, 0x32, 0x32, 0x32, 0x32, 0x32, 0x32, 0x28,  0x0,  0xe, 0xf5,
			0xe8, 0x1e, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x15, 0x6b, 0xff
			}};

    test_matrix = apply<double, double>(test_matrix, [](double d) { return 0xff - d; });
    const Matrix<int> edge_pixels = canny_edge_detect(test_matrix);

    for (int i = 0; i < h; i++) {
		for (int j = 0; j < w; j++) {
			int pixel = edge_pixels.get_element(i, j);
			if (pixel)
				std::cout << 'x';
			else
				std::cout << '.';
		}
		std::cout << '\n';
	}
    return 0;
}
*/

