// pattern matching on numeric features
// if only we can train a neural network for this task... sigh

#include <deque>
#include <cmath>
#include <cassert>
#include <iostream>

#include "match.hpp"
#include "constants.hpp"

bool valid_digit(const BinaryMatrix& digit, int min_zeros, int max_zeros)
{
    int zeros = 0;
    digit.for_each([&zeros](int p) { zeros += !p; });
    return min_zeros <= zeros && zeros <= max_zeros;
}

DistanceMap distance_map(const BinaryMatrix& image)
{
    auto [nrow, ncol] = image.dims();
    Matrix<int> map{nrow, ncol, -1};
    std::deque<std::tuple<int, int, int>> queue{};
    for (int r = 0; r < nrow; r++) {
        for (int c = 0; c < ncol; c++) {
            if (image.get_element(r, c))
                continue;
            queue.emplace_back(r, c, 0);
            map.set_element(r, c, 0);
        }
    }
    int dr[4] = {0, 0, 1, -1};
    int dc[4] = {1, -1, 0, 0};
    while (!queue.empty()) {
        auto [r, c, d] = queue.front();
        queue.pop_front();
        d++;
        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int nc = c + dc[i];
            if (!map.in_bound(nr, nc) || map.get_element(nr, nc) != -1)
                continue;
            map.set_element(nr, nc, d);
            queue.emplace_back(nr, nc, d);
        }
    }
    return map;
}

int hausdorff_distance_onesided(const BinaryMatrix& image, const DistanceMap& map)
{
    int distance = 0;
    auto [nrow, ncol] = image.dims();
    for (int r = 0; r < nrow; r++) {
        for (int c = 0; c < ncol; c++) {
            if (image.get_element(r, c))
                continue;
            distance = std::max(distance, map.get_element(r, c));
        }
    }
    return distance;
}

int hausdorff_distance(const BinaryMatrix& first_image, const BinaryMatrix& second_image)
{
    assert(first_image.dims() == second_image.dims());
    auto [nrow, nccol] = first_image.dims();
    DistanceMap first_map = distance_map(first_image);
    DistanceMap second_map = distance_map(second_image);
    int f2s = hausdorff_distance_onesided(first_image, second_map);
    int s2f = hausdorff_distance_onesided(second_image, first_map);
    return std::max(f2s, s2f);
}

int match(const BinaryMatrix& image)
{
    int best_match = -1;
    int min_distance = std::numeric_limits<int>::max();
    for (int i = 0; i < 10; i++) {
        BinaryMatrix sample_image{
            SAMPLE_NROW, 
            SAMPLE_NCOL, 
            std::vector<int>(SAMPLE_DATA[i].begin(), SAMPLE_DATA[i].end())
        };
        BinaryMatrix prepared_image = resize_image(image, SAMPLE_NROW, SAMPLE_NCOL);
        int distance = hausdorff_distance(prepared_image, sample_image);
        if (distance < min_distance) {
            min_distance = distance;
            best_match = i;
        }
    }
    return best_match;
}
