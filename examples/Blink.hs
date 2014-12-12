import Arduino

main = compileProgram $ do

    pin13 =: toggle clock
