import Arduino.Uno

main = compileProgram $ do

    pin13 =: (clock ~> toggle)

    uart =: (clock ~> streamMap (\_ -> stringConstant "hello\r\n"))
