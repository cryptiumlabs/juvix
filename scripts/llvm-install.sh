#!/usr/bin/env bash
set -eou pipefail

MIRROR="http://releases.llvm.org"

# Download
echo "Downloading llvm-9.0.0.src.tar.xz"
wget -nv -O "llvm-9.0.0.src.tar.xz"     "${MIRROR}/${LLVM_VERSION}/llvm-${LLVM_VERSION}.src.tar.xz"

# Install
# echo "Installing ${TARGET}"
tar xf "llvm-9.0.0.src.tar.xz"

cd ./llvm-9.0.0.src

mkdir build
cd build

cmake ../

cmake --build .

cmake --build . --target install

cd ../..

# Cleanup
rm llvm-9.0.0.src.tar.xz
rm -rf llvm-9.0.0.src
