import Arduino

main = arduinoProgram $ do

    pin13 =: toggle clock

    pin12 =: invert (toggle clock)
