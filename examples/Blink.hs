import Arduino.Uno

main = compileProgram $ do

    pin13 =: (clock ~> toggle)
