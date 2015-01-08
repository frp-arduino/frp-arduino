FRP
---

This sections introduces FRP and how it fits in the domain of programming an
Arduino.

The central building block in FRP is a **stream**. A stream contains values
that change over time. Consider an input port on the Arduino. If you constantly
read the value of the input you will get different values (high or low) over
time depending on if a button connected to that input is pressed or not.

... insert image of stream ...
