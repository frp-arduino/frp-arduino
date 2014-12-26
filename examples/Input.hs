import Arduino.Uno

main = compileProgram $ do

    pin12in ~> pin13

    clock ~> toggle ~> pin11
