import Arduino.Uno

main = compileProgram $ do

    pin13 =: (clock ~> toggle)

    uart =: (clock ~> mapS (\_ -> stringConstant "hello\r\n"))
