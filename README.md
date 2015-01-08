* [Introduction](#introduction)
* [The language](#the-language)
  * [FRP](#frp)
  * [EDSL](#edsl)
  * [Compiles to C](#compiles-to-c)
* [Examples](#examples)
  * [Running the examples](#running-the-examples)
  * [Blink](#blink)
* [Contributing](#contributing)
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

* Based on the functional reactive programming (FRP) paradigm
* Implemented as a deeply embedded domain specific language (EDSL) in Haskell
* Compiles to C code

Lets explore them in more detail.

### FRP

This sections introduces FRP and how it fits in the domain of programming an
Arduino.

The central building block in FRP is a **stream**. A stream contains values
that change over time. Consider an input port on the Arduino. If you constantly
read the value of the input you will get different values (high or low) over
time depending on if a button connected to that input is pressed or not.

... insert image of stream ...

### EDSL

### Compiles to C

## Examples

### Running the examples

### Blink

```haskell
import Arduino.Uno

main = compileProgram $ do

    pin13 =: (clock ~> toggle)
```

Source code: (examples/Blink.hs.hs)[examples/Blink.hs.hs]. Generated C code: (examples/Blink.hs.c)[examples/Blink.hs.c].

## Contributing

The contributors are listed in [AUTHORS](AUTHORS) (add yourself).

We use the [C4.1 (Collective Code Construction Contract)](http://rfc.zeromq.org/spec:22) process for contributions.

Comments on the process:

    A patch MUST compile cleanly and pass project self-tests on at least the
    principle target platform.

In our case, this means that `./test` should run without failure.

## License

The Haskell library that implements the language and all examples are free
software, distributed under the GNU General Public License, version 3. For more
information, see [COPYING](COPYING).

## This document

This document ([README.md](README.md)) is automatically generated from the
sources in the [doc](doc) folder by running `python doc/generate_readme.py`.
