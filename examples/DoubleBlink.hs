import Arduino

main = arduinoProgram $

    (pin13 =: toggle clock)

    <->

    (pin12 =: invert (toggle clock))
