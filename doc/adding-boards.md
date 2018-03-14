# Adding Boards

Each board is implemented as a module. To add a new board, make a copy of the
`src/Arduino/Uno.hs` module file to use as a base. Once the copy has been made,
rename the file to reflect the board name. Take extra care to ensure that the
module name is unique. For instance, there are two official Arduino Mega boards.
To differentiate between the two, one could be named `Mega2560` and the other
`MegaADK`. Change the module declaration name of `Arduino.Uno` to match the
board being added. In the example above, the new name might be `Arduino.Mega2560`.

The proper pin mapping diagram is needed to update the rest of the module code.
Look at the Atmel chip on the Arduino board itself, and then search for the
corresponding pin mapping. Below are diagrams for some common Arduino chips.

- [ATmega168/328P](https://www.arduino.cc/en/Hacking/PinMapping168)
- [ATmega 32U4](https://www.arduino.cc/en/Hacking/PinMapping32u4)
- [ATmega2560](https://www.arduino.cc/en/Hacking/PinMapping2560)

In addition to the pin mapping, the port register information for each pin
will need to be determined. The port register information can be found
[here](https://www.arduino.cc/en/Reference/PortManipulation), and the original
Uno module shows examples of how to combine this information into a pin declaration.

Beyond the creation of a module for a new board, the makefile system has to be
set up to build code for the new board properly. The first step in this process
is to make a copy of the `makefiles/Uno` makefile. Some properties that might
need to be altered in the makefile copy are as follows. For example properties, please
see the [Arduino-Makefile examples](https://github.com/sudar/Arduino-Makefile/blob/master/examples/Blink/Makefile).

- `BOARD_TAG`  # Identifies the board to Arduino-Makefile and marks output files
- `MCU` # Must match the chip on the board that is being added
- `F_CPU` # Must match the frequency of the CPU on the board that is being added
- `AVRDUDE_ARD_BAUDRATE` # Set to match the default serial communication baud rate of the board

The last step in adding support for a board is to add a conditional section to
the `make` script. Copy an existing `elif` statement for another board and copy
it below the else statement. Below shows an example snippet with `[board name]`
as placeholder text.

```bash
...
elif [ "$BOARD" == "[board name]" ]
then
  cp ../../makefiles/[board name] ./
  mv [board name] Makefile
  echo "include ${ARDUINO_MAKEFILE_PATH}" >> Makefile
else
...
```

Once the new board has been tested, feel free to submit a pull request to have it
pulled into the main repository.
