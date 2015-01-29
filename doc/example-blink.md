# Example: Blinking led

INCLUDE_VIDEO:UdIXmmp-6tw

INCLUDE_EXAMPLE:Blink.hs

This is the hello world of Arduino programs.

Lets examine this example line by line:

```haskell
import Arduino.Uno
```

This imports functions that allow us to define a program in the EDSL.

```haskell
main = compileProgram $ do
```

The `main` function is the standard `main` function in Haskell. The
`api:Arduino.DSL.compileProgram` function has the following type:

```haskell
compileProgram :: Action a -> IO ()
```

That means that we can define a set of actions in the do-block that we pass to
`compileProgram`. It takes those actions, builds an internal representation of
the program, and then generates C code and writes that to a file.

So what action is defined by the last line in the example?

```haskell
digitalOutput pin13 =: clock ~> toggle
```

Let's look at the type for the `api:Arduino.DSL.=:` operator:

```haskell
(=:) :: Output a -> Stream a -> Action ()
```

It takes an output of a specific type and connects it to a stream of values of
the same type.

The types of `api:Arduino.Uno.digitalOutput` and `api:Arduino.Uno.pin13`
reveal that we have an output for bits:

```haskell
digitalOutput :: GPIO -> Output Bit

pin13 :: GPIO
```

That means that the stream we define on the right hand side has to be a stream
of bits. The stream is created with the following expression:

```haskell
clock ~> toggle
```

Let's look at the types of the individual components:

```haskell
clock :: Stream Word

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b

toggle :: Stream Word -> Stream Bit
```

`api:Arduino.Uno.clock` is a built in stream that produces incrementing
integers at a given time interval.

`api:Arduino.Library.toggle` is a function that converts a stream of words to a
stream of bits by mapping the `api:Arduino.DSL.isEven` function: Even words are
converted to 1 and odd words are converted to 0.

`api:Arduino.DSL.~>` is an operator that takes a stream on the left hand side
and a function on the right hand side. The result is a stream that we get by
applying the function to the stream on the left hand side.

The resulting stream in the example is a stream of bits that toggles between 1
and 0 values at a specific time interval. When we connect that stream to the
pin where the led is connect, the led will blink at a specific time interval.
