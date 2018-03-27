#!/bin/bash

set -e

SOURCE=$1
TARGET=$2
OUTPUT_DIR=build-output/$SOURCE

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
        -o $OUTPUT_DIR/$SOURCE \
        examples/$SOURCE.hs
    cd $OUTPUT_DIR
    ./$SOURCE

    if ! [ -n "$ARDUINO_MAKEFILE_PATH" ]; then
        ARDUINO_MAKEFILE_PATH="../../Arduino-Makefile/Arduino.mk"
    fi

		# Add new boards as an elif statement here after creating a matching
		# makefile in the makefiles directory.
		if [ "$BOARD" == "Nano" ]
		then
			cp ../../makefiles/Nano ./
			mv Nano Makefile
			echo "include ${ARDUINO_MAKEFILE_PATH}" >> Makefile
		else
			# Default to the Arduino Uno
			cp ../../makefiles/Uno ./
			mv Uno Makefile
			echo "include ${ARDUINO_MAKEFILE_PATH}" >> Makefile
		fi

    if [ "$TARGET" == "dot" ];
    then
        dot -Tpng -odag.png dag.dot
        xdg-open dag.png
    else
        make $TARGET
    fi
fi
