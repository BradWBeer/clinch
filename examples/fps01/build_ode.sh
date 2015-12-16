#!/bin/sh
#./configure --prefix=/usr --enable-double-precision --enable-shared=yes --with-cylinder-cylinder=libccd --with-box-cylinder=libccd --with-capsule-cylinder=libccd --with-convex-box=libccd --with-convex-capsule=libccd --with-convex-cylinder=libccd --with-convex-sphere=libccd --with-convex-convex=libccd --disable-demos

./configure --prefix=/usr --enable-double-precision --enable-shared=yes --enable-libccd --with-box-cylinder=libccd --disable-demos

make -j 
