#!/bin/sh
set -e
ghc --make -i../src Blink.hs
./Blink
make $*
