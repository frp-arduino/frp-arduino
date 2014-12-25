import Arduino.Uno

main = compileProgram $ do

    def $ clock ~> toggle ~> pin13
