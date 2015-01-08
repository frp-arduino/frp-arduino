* [Arduino programming without the hassle of C](README.md#arduino-programming-without-the-hassle-of-c)

Arduino programming without the hassle of C
-------------------------------------------

```haskell
import Arduino.Uno

main = compileProgram $ do

    pin13 =: (clock ~> toggle)
```
