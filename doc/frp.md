# FRP

This section introduces FRP and shows how it fits in the domain of programming
an Arduino.

The central building block in FRP is a **stream**. A stream contains values
that change over time. Consider an input pin on the Arduino. If we constantly
read the value of the pin we will get different values (high or low) over time:

![Example input stream.](doc/input-stream.png)

We could take this stream and assign it to an output pin. Whenever there is a
new value on the input stream, that value will be sent to the output pin. In
this example we have a led connected to the output pin:

![Stream connected to Arduino.](doc/stream-arduino.png)

So building an Arduino application using FRP involves capturing inputs as
streams, doing some interesting calculations (we'll come to that), and
assigning streams to outputs.

## Transforming

The most common thing we do with streams is to transform the values in some
way. This operation is called map (`api:mapS`). Let's say we have a stream of
numbers:

![A stream of numbers.](doc/number-stream.png)

We can transform this stream to a stream of booleans by mapping a function that
converts even numbers to true and odd numbers to false:

![Mapping numbers to booleans.](doc/map-number-stream.png)

We now have a stream that alternates its boolean value at a time interval.

Mapping is always a one-to-one conversion.
