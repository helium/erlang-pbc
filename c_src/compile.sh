#!/bin/sh

if [ ! -d c_src/pbc ]; then
    git clone https://github.com/Vagabond/pbc c_src/pbc
fi

cd c_src/pbc

if [ ! -d build ]; then
    mkdir build
fi

if [ ! -f configure ]; then
    autoreconf -ivf
fi

if [ ! -f Makefile ]; then
    ./configure --prefix=$PWD/build --enable-optimized --enable-safe-clean $CONFIGURE_ARGS
    sed -i '/^CFLAGS/ s/$/ -fPIC/' Makefile
fi

make -j
make install
