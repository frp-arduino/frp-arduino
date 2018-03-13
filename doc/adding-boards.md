# Adding Boards

Each board is implemented as a module. To add a new board, make a copy of the
`Uno.hs` module to use as a base. Once the copy has been made, rename the file
to match the board name. Take extra care to ensure that the module name is
unique. For instance, there are two official Arduino Mega boards. To
differentiate between the two, one could be named `Mega2560` and the other
`MegaADK`. Change the module declaration name of `Arduino.Uno` to match the
board being added. In the example above, the new name might be
`Arduino.Mega2560`.

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
