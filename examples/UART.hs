import Arduino.Uno

main = compileProgram $ do

    def $ clock ~> toggle ~> pin13

    def $ clock ~> streamMap (\_ -> stringConstant "hello\r\n") ~> uart
