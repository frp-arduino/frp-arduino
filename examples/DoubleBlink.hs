import Arduino.Uno

main = compileProgram $ do

    clock ~> toggle ~> \toggled -> do

        toggled ~> pin13

        toggled ~> invert ~> pin12
