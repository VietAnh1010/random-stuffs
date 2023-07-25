#ifndef OTSU_H
#define OTSU_H

#include "matrix.hpp"

Matrix<int> otsu(const Matrix<unsigned>& image);
Matrix<int> use_middle_val(const Matrix<unsigned>& image);
Matrix<int> niblack(const Matrix<unsigned>& image, int n = 5, double k = -0.2);
Matrix<int> sauvola(const Matrix<unsigned>& image, int w = 2, double k = 0.5, double R = 128);

#endif
