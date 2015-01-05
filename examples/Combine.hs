import Arduino.Uno

main = compileProgram $ do

    let button = pin12in
    let greenLed = pin13
    let redLed1 = pin11
    let redLed2 = pin10

    toggled <- def $ clock ~> toggle

    greenLed =: button

    redLed1 =: combine pick button toggled

    redLed2 =: combine pick button (invert toggled)

pick :: Expression Bool -> Expression Bool -> Expression Bool
pick first second = if_ first second (boolConstant False)
