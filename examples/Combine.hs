import Arduino.Uno

main = compileProgram $ do

    let button = pin12in
    let greenLed = pin13
    let redLed1 = pin11
    let redLed2 = pin10

    clock ~> toggle ~> \toggled -> do

        button ~> greenLed

        combine pick button toggled ~> redLed1

        combine pick button (invert toggled) ~> redLed2

pick :: Expression Bool -> Expression Bool -> Expression Bool
pick first second = if_ first second (boolConstant False)
