#!/bin/bash

g++            \
    -Wall      \
    -Wextra    \
    -std=c++11 \
    main.cpp

time ./a.out
