import Arduino.Uno

main = compileProgram $ do

    pin13 =: pin12in

    pin11 =: (clock ~> toggle)
