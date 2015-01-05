import Arduino.Uno

main = compileProgram $ do

    toggled <- def $ clock ~> toggle

    pin13 =: toggled

    pin12 =: (toggled ~> invert)
