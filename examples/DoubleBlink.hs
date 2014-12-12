import Arduino

main = compileProgram $ do

    pin13 =: toggle clock

    pin12 =: invert (toggle clock)
