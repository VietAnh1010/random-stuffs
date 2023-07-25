#ifndef COMMON_H
#define COMMON_H

#include <cassert>

#include "matrix.hpp"

using BinaryMatrix = Matrix<int>;
using DistanceMap = Matrix<int>;

void print_binary(const BinaryMatrix& matrix);
template<typename T> void print(const Matrix<T>& matrix);
BinaryMatrix resize_image(const BinaryMatrix& image, int new_nrow, int new_ncol);

#endif // COMMON_H
