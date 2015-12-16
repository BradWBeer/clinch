#!/bin/sh

./configure --prefix=/usr --enable-double-precision --enable-shared=yes --enable-libccd --with-box-cylinder=libccd --disable-demos

make -j 
