import Arduino
import AST

main = frpArduino $

    (pin13 =: toggle clock)

    <->

    (pin12 =: invert (toggle clock))
