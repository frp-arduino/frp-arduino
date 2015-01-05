import Arduino.Uno

main = compileProgram $ do

    let buttonPressStream = pin12in
    let greenLed          = pin13
    let redLed1           = pin11
    let redLed2           = pin10

    blinkStream <- def $ clock ~> toggle

    greenLed =: buttonPressStream

    redLed1 =: keepWhen buttonPressStream (boolConstant False) blinkStream

    redLed2 =: keepWhen buttonPressStream (boolConstant False) (invert blinkStream)
