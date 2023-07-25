#include "common.hpp"

#include <cmath>
#include <iostream>

void print_binary(const BinaryMatrix& matrix)
{
	auto [nrow, ncol] = matrix.dims();
	std::cout << "----------\n";
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            std::cout << (matrix.get_element(i, j) ? ' ' : 'x');
        }
        std::cout << '\n';
    }
	std::cout << "----------\n";
}

template<typename T>
void print(const Matrix<T>& matrix)
{
    auto [nrow, ncol] = in.dims();
	std::cout << "----------\n";
    for (int i = 0; i < nrow; i++) {
        for (int j = 0; j < ncol; j++) {
            std::cout << in.get_element(i, j);
        }
        std::cout << '\n';
    }
	std::cout << "----------\n";
}

BinaryMatrix resize_image(const BinaryMatrix& image, int new_nrow, int new_ncol)
{
    auto [nrow, ncol] = image.dims();
    double window_h = static_cast<double>(nrow) / new_nrow;
    double window_w = static_cast<double>(ncol) / new_ncol;
    BinaryMatrix result{new_nrow, new_ncol};

    for (int r = 0; r < new_nrow; r++) {
        int start_i = static_cast<int>(std::round(r * window_h));
        int end_i = static_cast<int>(std::round((r + 1) * window_h));
        for (int c = 0; c < new_ncol; c++) {
            int start_j = static_cast<int>(std::round(c * window_w));
            int end_j = static_cast<int>(std::round((c + 1) * window_w));
            int ones = 0;
            int zeros = 0;
            for (int m = start_i; m < end_i; m++) {
                for (int n = start_j; n < end_j; n++) {
                    if (image.get_element(m, n))
                        ones++;
                    else
                        zeros++;
                }
            }
            result.set_element(r, c, ones >= zeros);
        }
    }
    return result;
}
