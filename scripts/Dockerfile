FROM fpco/stack-build:lts-16.18

RUN git clone https://github.com/Z3Prover/z3.git && cd z3 && python scripts/mk_make.py && cd build && make -j8 && make install

# Version info
ENV LLVM_RELEASE 9
ENV LLVM_VERSION 9.0.0

# Install Clang and LLVM
COPY llvm-install.sh .
RUN ./llvm-install.sh
RUN mv /usr/local/lib/libLLVM* /usr/lib
RUN mv /usr/local/bin/llvm* /usr/bin

# Install libff
COPY ./build-libff.sh .
RUN ./build-libff.sh

# Install roswell & org-generation
COPY ./build-roswell.sh .
RUN ./build-roswell.sh

# Install ormolu
RUN stack install ormolu -j8
RUN stack clean

# Clean things up
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
