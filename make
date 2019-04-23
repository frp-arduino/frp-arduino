#!/bin/bash

set -e

if [ "$#" -lt 1 ]; then
    echo "usage: BOARD=board_type ./make source_file target"
		echo "    board_type: Type of Arduino [Uno|Nano], this environment variable can be omitted"
		echo "    source_file: frp-arduino source file (.hs)"
		echo "    target: Make target [upload|clean], can be omitted"
		exit 1
fi

SOURCE=$1
TARGET=$2
OUTPUT_DIR=build-output/$(basename "$SOURCE")
BASENAME=$(basename "$SOURCE")

# We do not want the file extension at this time
if [[ "$BASENAME" = *".hs" ]]
then
	echo "Please do not include the .hs extension on your file name."
	exit 0
fi

# Source file name exists as an example
if [ "$SOURCE" = *"examples"* ] || [ -f "examples/$BASENAME.hs" ]
then
	SOURCE_PATH="examples/$BASENAME.hs"
else
	SOURCE_PATH="${SOURCE}.hs"
fi

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
        -o $OUTPUT_DIR/$BASENAME \
        $SOURCE_PATH
    cd $OUTPUT_DIR
    ./$BASENAME

    if ! [ -n "$ARDUINO_MAKEFILE_PATH" ]; then
        case "$(uname -s)" in
            Darwin)
                # Assume Arduino-Makefile was installed with `brew install arduino-mk`
                ARDUINO_MAKEFILE_PATH="/usr/local/opt/arduino-mk/Arduino.mk" ;;
            *)
                # Assume both projects are checked out from source side by side
                ARDUINO_MAKEFILE_PATH="../../Arduino-Makefile/Arduino.mk" ;;
        esac
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
