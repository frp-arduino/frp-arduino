import Arduino.Uno

main = compileProgram $ do

    pin13 =: toggle clock

    uart =: streamMap (\_ -> stringConstant "hello\r\n") clock
