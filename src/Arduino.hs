module Arduino where

import AST
import CodeGen (programToC)

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: Pin
pin13 = Pin -- pin13 (arduino) = pb5 (atmega328p)
    { name              = "pin13"
    , portRegister      = "PORTB"
    , directionRegister = "DDRB"
    , pinMask           = "0x20U"
    }

pin12 :: Pin
pin12 = Pin -- pin12 (arduino) = pb4 (atmega328p)
    { name              = "pin12"
    , portRegister      = "PORTB"
    , directionRegister = "DDRB"
    , pinMask           = "0x10U"
    }

frpArduino :: Program -> IO ()
frpArduino program = writeFile "main.c" (programToC program)
