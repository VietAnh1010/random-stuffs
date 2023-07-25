#ifndef IMAGE_H
#define IMAGE_H

#include <vector>

struct Image
{
	int width;
	int height;
	std::vector<unsigned> pixels;

	Image(int w, int h, unsigned pixval) : width{w}, height{h}, pixels(w * h, pixval) {}
	Image(int w, int h, std::initializer_list<unsigned> data) : width{w}, height{h}, pixels(data) {}
};

#endif
