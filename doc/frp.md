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
way. This operation is called map (`api:Arduino.DSL.mapS`). Let's say we have a
stream of numbers:

![A stream of numbers.](doc/number-stream.png)

We can transform this stream to a stream of booleans by mapping a function that
converts even numbers to true and odd numbers to false:

![Mapping numbers to booleans.](doc/map-number-stream.png)

We now have a stream that alternates its boolean value at a time interval.

Mapping is always a one-to-one conversion.

## Keeping state

Streams can also be used to keep track of state. We achieve that with the fold
(`api:Arduino.DSL.foldpS`) operation.

A fold is like a map where we also have access to a state and the output is the
new state.

Let's say we have a stream of booleans representing if a button is pressed or
not. Now we want a stream that keeps track of the number of button presses. We
can do that by folding the following function (pseudo code) with an initial
`clickCount` value of 0:

    if buttonIsPressed
        clickCount + 1
    else
        clickCount

![Counting number of clicks.](doc/stream-fold.png)

The very first time `clickCount` is 0. Subsequent values are incremented by one
if the boolean value is true, otherwise we just pass the current `clickCount`
along.

## Filtering

Sometimes we would like to discard values from a stream. We do that with the
filter (`api:Arduino.DSL.filterS`) operation.

We can for example keep all even numbers in a stream:

![Filtering a stream.](doc/stream-filter.png)
