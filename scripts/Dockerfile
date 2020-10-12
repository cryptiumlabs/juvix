FROM fpco/stack-build:lts-15.16

RUN git clone https://github.com/Z3Prover/z3.git && cd z3 && python scripts/mk_make.py && cd build && make && make install

# Version info
ENV LLVM_RELEASE 9
ENV LLVM_VERSION 9.0.0

# Install Clang and LLVM
COPY llvm-install.sh .
RUN ./llvm-install.sh

# Install libff
COPY ./build-libff.sh .
RUN ./build-libff.sh

RUN mv /usr/local/lib/libLLVM* /usr/lib
RUN mv /usr/local/bin/llvm* /usr/bin
