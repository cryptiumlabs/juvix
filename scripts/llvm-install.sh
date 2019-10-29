#!/usr/bin/env bash
set -eou pipefail

MIRROR="http://releases.llvm.org"

# Determine architecture
ARCH=$(dpkg --print-architecture)

# Download
echo "Downloading llvm-9.0.0.src.tar.xz"
wget -nv -O "llvm-9.0.0.src.tar.xz"     "${MIRROR}/${LLVM_VERSION}/llvm-9.0.0.src.tar.xz"

# Install
# echo "Installing ${TARGET}"
tar xf "llvm-9.0.0.src.tar.xz"

cd ./llvm-9.0.0.src

mkdir build
cd build

cmake ../

# mkdir ../../bin/opt

cmake --build .

cmake --build . --target install

cd ../..


rm llvm-9.0.0.src.tar.xz
rm -rf llvm-9.0.0.src
# Cleanup
# rm -rf "${DOWNLOAD_FILE}" "${TARGET}/" llvm-install.sh
mkdir "juvix"
