import Arduino.Uno

main = compileProgram $ do

    toggled <- def $ clock ~> toggle

    def $ toggled ~> pin13

    def $ toggled ~> invert ~> pin12
