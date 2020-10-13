#!/bin/sh

set -xe

cd /tmp
rm -rf libff
git clone https://github.com/scipr-lab/libff --recursive
cd libff
ARGS="-DWITH_PROCPS=OFF -DCURVE=ALT_BN128 -DCMAKE_INSTALL_PREFIX:PATH=/usr"
sed -i 's/STATIC/SHARED/' libff/CMakeLists.txt
sed -i 's/STATIC/SHARED/' depends/CMakeLists.txt
mkdir build
cd build
CXXFLAGS="-fPIC $CXXFLAGS" cmake $ARGS ..
make -j $(nproc) && sudo make install
cd /tmp
rm -rf libff
