import Arduino.Uno

main = compileProgram $ do

    def $ pin12in ~> pin13

    def $ clock ~> toggle ~> pin11
