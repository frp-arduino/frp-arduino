import Arduino.Uno

main = compileProgram $ do

    clock ~> toggle ~> pin13

    clock ~> streamMap (\_ -> stringConstant "hello\r\n") ~> uart
