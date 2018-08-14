#!/bin/bash

set -e

export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig:$PKG_CONFIG_PATH
export LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH
export PATH=$HOME/.local/bin:$PATH

mkdir -p $HOME/.local/bin
mkdir -p $HOME/.libpostal

curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'

if [ ! -d libpostal ]
then
    git clone https://github.com/openvenues/libpostal
fi

cd libpostal
git checkout v1.1-alpha
./bootstrap.sh
./configure --datadir=$HOME/.libpostal --prefix=$HOME/.local
make
make install
cd ..

stack --no-terminal --install-ghc test --only-dependencies

stack --no-terminal test --haddock --no-haddock-deps
