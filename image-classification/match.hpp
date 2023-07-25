#ifndef MATCH_H
#define MATCH_H

#include "common.hpp"
#include "rectangle.hpp"

bool valid_digit(const BinaryMatrix& digit, int min_zeros, int max_zeros);
DistanceMap distance_map(const BinaryMatrix& image);
int hausdorff_distance_onesided(const BinaryMatrix& image, const DistanceMap& map);
int hausdorff_distance(const BinaryMatrix& first_image, const BinaryMatrix& second_image);
int match(const BinaryMatrix& image);

#endif // MATCH_H
