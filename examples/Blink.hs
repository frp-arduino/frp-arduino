import Arduino.Uno

main = compileProgram $ do

    clock ~> toggle ~> pin13
