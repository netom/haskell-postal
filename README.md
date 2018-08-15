[![Build Status](https://travis-ci.org/netom/haskell-postal.svg?branch=master)](https://travis-ci.org/netom/haskell-postal)

# haskell-postal

This binding is in alpha state, expect more functionality to come soon. Currently
it can only be used to do basic parsing and normalization.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import NLP.Postal

main :: IO ()
main = do
    setup
    setupParser
    setupLanguageClassifier

    apdo <- getAddressParserDefaultOptions
    pa <- parseAddress apdo "11 Wall Street New York, NY"
    print pa

    dno <- getDefaultNormalizeOptions
    xa <- expandAddress dno "11 Wall Street New York, NY"
    print xa

    tearDownLanguageClassifier
    tearDownParser
    tearDown
```

## Installation

Before using the Haskell binding, you must install the libpostal C library.
Make sure you have the following prerequisites:

### On Ubuntu/Debian

```
sudo apt-get install curl autoconf automake libtool pkg-config
```

### On CentOS/RHEL

```
sudo yum install curl autoconf automake libtool pkgconfig
```

### On Mac OSX

```
sudo brew install curl autoconf automake libtool pkg-config
```

### Installing libpostal

```
git clone https://github.com/openvenues/libpostal
cd libpostal
./bootstrap.sh
./configure --datadir=/usr/local/share/libpostal # Needs to have a few GBs of space
make
sudo make install

# On Linux it's probably a good idea to run
sudo ldconfig
```

### Installing the haskell-postal package

#### From source

Download the source from github, and use the ```cabal``` utility to install it:

```
git clone https://github.com/netom/haskell-postal
cd haskell-postal
cabal install
```

#### Cabal

Cabal can of course just fetch the package from Hackage:

```
cabal install haskell-postal
```

#### Stack

You can of course use stack to install the package, just replace "cabal install"
with "stack install" anywhere above. Just make sure you try to find the
installed package where the tool *you* use puts it.
