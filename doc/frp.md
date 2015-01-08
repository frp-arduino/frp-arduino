FRP

This sections introduces FRP and shows how it fits in the domain of programming
an Arduino.

The central building block in FRP is a **stream**. A stream contains values
that change over time. Consider an input pin on the Arduino. If you constantly
read the value of the pin you will get different values (high or low) over time
depending on if a button connected to that pin is pressed or not:

![Example input stream.](doc/input-stream.png)

We could take this stream and assign it to an output pin. Whenever there is a
new value on the input stream, that value will be sent to the output pin.

The most common thing we to with streams is to convert the values in some way.
It is called map. There is a built in stream called `clock` that increments an
integer at some time interval:

![The clock stream.](doc/clock-stream.png)

We can convert this stream to a stream of booleans by mapping the function
even on it:

![The clock stream mapped with the even function.](doc/map-even-clock-stream.png)

We now have a stream that alternates its boolean value at a time interval.
This stream can be connected to an output pin that has a led connected to it to
make the led blink.
