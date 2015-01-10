Example: Blinking a led

INCLUDE_EXAMPLE:Blink.hs

[![Blink demo video.](http://img.youtube.com/vi/UdIXmmp-6tw/0.jpg)](http://youtu.be/UdIXmmp-6tw)

Lets examine this example line by line:

```haskell
import Arduino.Uno
```

This imports functions that allow us to define a program in the EDSL.

```haskell
main = compileProgram $ do
```

The `main` function is the standard `main` function in Haskell. The
`compileProgram` function has the following type:

```haskell
compileProgram :: Action a -> IO ()
```

That means that we can define a set of actions in the do-block that we pass to
`compileProgram`. It will then take those actions, build an internal
representation of the program, and then generate C code and write that to a
file.

So what action is defined by the last line in the example?

```haskell
pin13 =: clock ~> toggle
```

Let's look at the type for the `=:` operator:

```haskell
(=:) :: Output a -> Stream a -> Action ()
```

It takes an `Output` of a specific type `a` and connects it to a `Stream` of
values of the same type.

The type of `pin13` reveals that it accepts booleans:

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

`clock` is a built in stream that produces incrementing integers at a given
time interval.

`toggle` is a function that converts a stream of integers to a stream of
booleans. Even integers are converted to True and odd integers are converted to
False.

`~>` is an operator that takes a stream on the left hand side and a function on
the right hand side. The result is a stream that we get by applying the
function to the stream on the left hand side.
