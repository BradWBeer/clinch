#!/bin/sh
./configure --prefix=/usr --enable-double-precision --enable-shared=yes --with-cylinder-cylinder=yes --with-box-cylinder=yes --with-capsule-cylinder=yes --disable-demos
make -j 
