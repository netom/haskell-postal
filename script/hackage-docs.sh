#!/bin/bash

set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal configure --builddir="$dir"
cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
cabal upload  --publish -d $dir/*-docs.tar.gz
