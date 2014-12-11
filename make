#!/bin/sh

set -e

EXAMPLE=$1
TARGET=$2
OUTPUT_DIR=build-output/$EXAMPLE

if [ "$TARGET" == "clean" ]
then
    rm -rf $OUTPUT_DIR
else
    mkdir -p $OUTPUT_DIR
    ghc --make -isrc -outputdir $OUTPUT_DIR -o $OUTPUT_DIR/$EXAMPLE examples/$EXAMPLE.hs
    cd $OUTPUT_DIR
    ./$EXAMPLE
    cat << EOF > Makefile
BOARD_TAG = uno
ARDUINO_LIBS =
include ../../Arduino-Makefile/Arduino.mk
EOF
    make $TARGET
fi
