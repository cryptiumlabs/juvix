FROM fpco/stack-build:lts-14.12

RUN ln -s /usr/bin/g++-7 /usr/bin/g++

RUN git clone https://github.com/Z3Prover/z3.git && cd z3 && python scripts/mk_make.py && cd build && make && make install

# Version info
ENV LLVM_RELEASE 9
ENV LLVM_VERSION 9.0.0

# Install Clang and LLVM
COPY llvm-install.sh .
RUN ./llvm-install.sh

# Install libff
COPY ./install-libff.sh .
RUN ./install-libff.sh
