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
    ghc \
        --make \
        -Werror \
        -fwarn-unused-imports \
        -isrc \
        -outputdir $OUTPUT_DIR \
        -o $OUTPUT_DIR/$EXAMPLE \
        examples/$EXAMPLE.hs
    cd $OUTPUT_DIR
    ./$EXAMPLE
    if ! [ -n "$ARDUINO_MAKEFILE_PATH" ]; then
        ARDUINO_MAKEFILE_PATH="../../Arduino-Makefile/Arduino.mk"
    fi
    cat << EOF > Makefile
NO_CORE = Yes

BOARD_TAG = uno
MCU = atmega328p
F_CPU = 16000000L

AVRDUDE_ARD_PROGRAMMER = arduino
AVRDUDE_ARD_BAUDRATE = 115200

include $ARDUINO_MAKEFILE_PATH

EOF
    if [ "$TARGET" == "dot" ];
    then
        dot -Tpng -odag.png dag.dot
        xdg-open dag.png
    else
        make $TARGET
    fi
fi
