#ifndef RECTANGLE_H
#define RECTANGLE_H

struct Rectangle {
    int min_r;
    int min_c;
    int max_r;
    int max_c;

    Rectangle() = delete;
    Rectangle(int min_r, int min_c, int max_r, int max_c) : min_r{min_r}, min_c{min_c}, max_r{max_r}, max_c{max_c} {}
};

#endif // RECTANGLE_H
