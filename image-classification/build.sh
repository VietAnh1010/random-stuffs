#!/usr/bin/bash

CPPFLAGS="-Wall -Wconversion -std=c++17 "

optical() {
    g++ ${CPPFLAGS} -o optical optical.cpp
    ./optical
}

canny() {
    g++ ${CPPFLAGS} -o canny canny.cpp
    ./canny
}

otsu() {
    g++ ${CPPFLAGS} -o otsu otsu.cpp && ./otsu
}

optical
