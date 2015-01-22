* [Introduction](#introduction)
* [The language](#the-language)
  * [FRP](#frp)
    * [Transforming](#transforming)
    * [Keeping state](#keeping-state)
    * [Filtering](#filtering)
  * [EDSL](#edsl)
  * [Compiles to C](#compiles-to-c)
* [Examples](#examples)
  * [Running the examples](#running-the-examples)
  * [Example: Blinking a led](#example-blinking-a-led)
  * [Example: Blinking two leds](#example-blinking-two-leds)
* [API](#api)
  * [Actions](#actions)
  * [Stream operations](#stream-operations)
  * [Expression operators](#expression-operators)
  * [Uno outputs and streams](#uno-outputs-and-streams)
* [Contributing](#contributing)
* [Resources](#resources)
* [License](#license)
* [This document](#this-document)

## Introduction

We believe that programming the [Arduino](http://arduino.cc/) can be *more fun*
if we don't have to use the C language to program it. We aim to create a new
language that allows us to program the Arduino using higher-level
constructs. Our mission:

**Arduino programming without the hassle of C**

## The language

The language we create has the following properties:

* It is based on the functional reactive programming (FRP) paradigm
* It is implemented as a deeply embedded domain specific language (EDSL) in
  Haskell
* It compiles to C code

Lets explore them in more detail.

### FRP

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

#### Transforming

The most common thing we do with streams is to transform the values in some
way. This operation is called map ([`mapS`](#api-mapS)). Let's say we have a stream of
numbers:

![A stream of numbers.](doc/number-stream.png)

We can transform this stream to a stream of booleans by mapping a function that
converts even numbers to true and odd numbers to false:

![Mapping numbers to booleans.](doc/map-number-stream.png)

We now have a stream that alternates its boolean value at a time interval.

Mapping is always a one-to-one conversion.

#### Keeping state

Streams can also be used to keep track of state. We achieve that with the fold
([`foldpS`](#api-foldpS)) operation.

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

#### Filtering

Sometimes we would like to discard values from a stream. We do that with the
filter ([`filterS`](#api-filterS)) operation.

We can for example keep all even numbers in a stream:

![Filtering a stream.](doc/stream-filter.png)

### EDSL

Our language is embedded in the Haskell language. That means that when we write
programs in our language, we are actually writing Haskell programs.

However, our programs will not look like standard Haskell because they use
custom operators that are more suited to the FRP paradigm.

By hosting our language inside Haskell, as opposed to making up our own custom
syntax, we gain a few things:

* We don't have to write our own parser
* We can take advantage of Haskell's advanced type system

When we combine our program with the language library, we get an executable
that, when run, will produce a C file:

![The EDSL workflow.](doc/edsl.png)

The executable is a compiler from our EDSL to C.

### Compiles to C

In order to make our EDSL execute on the Arduino, we compile it to a C source
file which we then turn into avr assembly code by using the avr gcc toolchain.

## Examples

In this section we will see what our EDSL looks like and what kinds of programs
we can write using it.

### Running the examples

Command to compile an example:

    ./make [name of example]

Command to compile and upload an example to a connected Arduino:

    ./make [name of example] upload

Before we can run these commands, we need to install a few dependencies:

* [The Haskell platform](https://www.haskell.org/platform/)
* [Arduino-Makefile](https://github.com/sudar/Arduino-Makefile)

Haskell should be installed system wide, but Arduino-Makefile should just be
copied to the root of this repository.

In order to use Arduino-Makefile, we also need standard build tools like make
and gcc, and in particular, the [gcc toolchain for avr](http://www.nongnu.org/avr-libc/).

On a Fedora system, we can install all dependencies with the following
commands:

    yum install haskell-platform
    yum install arduino-core
    git clone https://github.com/sudar/Arduino-Makefile.git

The arduino-core package depends on the following packages:

* avr-gcc
* avr-gcc-c++
* avr-libc
* avrdude

### Example: Blinking a led

<p align="center">
  <a href="http://youtu.be/UdIXmmp-6tw">
      <img src="http://img.youtube.com/vi/UdIXmmp-6tw/0.jpg">
  </a>
</p>

```haskell
import Arduino.Uno

main = compileProgram $ do

    pin13 =: clock ~> toggle
```

* Source code: [examples/Blink.hs](examples/Blink.hs)
* Generated C code (no need to understand this): [examples/Blink.c](examples/Blink.c)
* Compile and upload command: `./make Blink upload`

Lets examine this example line by line:

```haskell
import Arduino.Uno
```

This imports functions that allow us to define a program in the EDSL.

```haskell
main = compileProgram $ do
```

The `main` function is the standard `main` function in Haskell. The
[`compileProgram`](#api-compileProgram) function has the following type:

```haskell
compileProgram :: Action a -> IO ()
```

That means that we can define a set of actions in the do-block that we pass to
[`compileProgram`](#api-compileProgram). It takes those actions, builds an internal representation
of the program, and then generates C code and writes that to a file.

So what action is defined by the last line in the example?

```haskell
pin13 =: clock ~> toggle
```

Let's look at the type for the [`=:`](#api--61-:) operator:

```haskell
(=:) :: Output a -> Stream a -> Action ()
```

It takes an output of a specific type and connects it to a stream of values of
the same type.

The type of [`pin13`](#api-pin13) reveals that it accepts booleans:

```haskell
pin13 :: Output Bool
```

That means that the stream we define on the right hand side has to be a stream
of booleans. The stream is created with the following expression:

```haskell
clock ~> toggle
```

Let's look at the types of the individual components:

```haskell
clock :: Stream Int

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b

toggle :: Stream Int -> Stream Bool
```

[`clock`](#api-clock) is a built in stream that produces incrementing integers at a given
time interval.

[`toggle`](#api-toggle) is a function that converts a stream of integers to a stream of
booleans by mapping the [`isEven`](#api-isEven) function: Even integers are converted to
true and odd integers are converted to false.

[`~>`](#api--126--62-) is an operator that takes a stream on the left hand side and a function on
the right hand side. The result is a stream that we get by applying the
function to the stream on the left hand side.

The resulting stream in the example is a stream of booleans that toggles
between true and false values at a specific time interval. When we connect that
stream to the pin where the led is connect, the led will blink at a specific
time interval.

### Example: Blinking two leds

<p align="center">
  <a href="http://youtu.be/dWl3nfAJy08">
      <img src="http://img.youtube.com/vi/dWl3nfAJy08/0.jpg">
  </a>
</p>

```haskell
import Arduino.Uno

main = compileProgram $ do

    toggled <- def $ clock ~> toggle

    pin13 =: toggled

    pin12 =: toggled ~> invert
```

* Source code: [examples/DoubleBlink.hs](examples/DoubleBlink.hs)
* Generated C code (no need to understand this): [examples/DoubleBlink.c](examples/DoubleBlink.c)
* Compile and upload command: `./make DoubleBlink upload`

## API

### Actions

<a name="api-compileProgram"></a>**compileProgram**

```haskell
compileProgram :: Action a -> IO ()
```

<a name="api-(-61-:)"></a>**(=:)**

```haskell
(=:) :: Output a -> Stream a -> Action ()
```

### Stream operations

<a name="api-(-126--62-)"></a>**(~>)**

```haskell
(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
```

<a name="api-mapS"></a>**mapS**

```haskell
mapS :: (Expression a -> Expression b) -> Stream a -> Stream b
```

Similar to map in Haskell. "S" is for stream.

<a name="api-foldpS"></a>**foldpS**

```haskell
foldpS :: (Expression a -> Expression b -> Expression b)
```

Similar to fold in Haskell. "S" is for stream.
Inspired by [Elm's](http://elm-lang.org/)
[foldp](http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Signal#foldp).

<a name="api-filterS"></a>**filterS**

```haskell
filterS :: (Expression a -> Expression Bool) -> Stream a -> Stream a
```

### Expression operators

<a name="api-toggle"></a>**toggle**

```haskell
toggle :: Stream Word -> Stream Bit
```

<a name="api-isEven"></a>**isEven**

```haskell
isEven :: Expression DAG.Word -> Expression Bool
```

### Uno outputs and streams

<a name="api-pin12"></a>**pin12**

```haskell
pin12 :: Output Bit
```

<a name="api-pin13"></a>**pin13**

```haskell
pin13 :: Output Bit
```

<a name="api-clock"></a>**clock**

```haskell
clock :: Stream Word
```

## Contributing

The contributors are listed in [AUTHORS](AUTHORS) (add yourself).

We use the [C4.1 (Collective Code Construction Contract)](http://rfc.zeromq.org/spec:22)
process for contributions.
More discussions and explanations of the process can be found in the
[The ZeroMQ Community](http://zguide.zeromq.org/page:all#Chapter-The-ZeroMQ-Community),
in particular [here](http://zguide.zeromq.org/page:all#The-ZeroMQ-Process-C).

Comments on the process:

> A patch MUST compile cleanly and pass project self-tests on at least the
> principle target platform.

In our case, this means that `./test` should run without failure.

[![Build Status](https://travis-ci.org/frp-arduino/frp-arduino.svg)](https://travis-ci.org/frp-arduino/frp-arduino)

## Resources

* http://augustss.blogspot.se/2007/06/representing-dsl-expressions-in-haskell.html

## License

The Haskell library that implements the language and all examples are free
software, distributed under the GNU General Public License, version 3. For more
information, see [COPYING](COPYING).

## This document

This document ([README.md](README.md)) is automatically generated from the
sources in the [doc](doc) folder by running `python doc/generate_readme.py`.
