#!/bin/bash

clang++  -Weverything -Wno-c++98-compat           \
    -Wall      \
    -Wextra    \
    -std=c++11 \
    main.cpp

time ./a.out
