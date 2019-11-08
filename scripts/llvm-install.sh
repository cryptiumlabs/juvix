#!/usr/bin/env bash
set -eou pipefail

MIRROR="http://releases.llvm.org"

# Download
echo "Downloading: llvm-${LLVM_VERSION}.src.tar.xz"
wget -nv -O "llvm-${LLVM_VERSION}.src.tar.xz"     "${MIRROR}/${LLVM_VERSION}/llvm-${LLVM_VERSION}.src.tar.xz"

# Extract
echo "Extracting: llvm-${LLVM_VERSION}.src.tar.xz"
# No same owner, as the UID and GID are too high for cricle CI :(
tar xf "llvm-${LLVM_VERSION}.src.tar.xz" --no-same-owner

cd ./llvm-${LLVM_VERSION}.src

mkdir build
cd build

# Building
echo "Building: llvm-${LLVM_VERSION}.src.tar.xz"
cmake -DLLVM_INCLUDE_TESTS=false -DLLVM_BUILD_LLVM_DYLIB=true -DLLVM_LINK_LLVM_DYLIB=true ../

cmake --build .

# Building
echo "Installing llvm-${LLVM_VERSION}.src.tar.xz"
cmake --build . --target install

cd ../..

# Cleanup
rm llvm-${LLVM_VERSION}.src.tar.xz
rm -rf llvm-${LLVM_VERSION}.src

# This fixes the error with libLLVM-9.so for some reason!!!!
apt-get update
apt-get -y install ranger
