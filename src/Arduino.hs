module Arduino
    ( module Stages.DSL
    , module Library
    , pin13
    , pin12
    , uart
    ) where

import Library
import Stages.DSL
import Types.AST (Output(..))

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: Output
pin13 = Pin "pin13" "PORTB" "DDRB" "0x20U" -- pin13 (arduino) = pb5 (atmega328p)

pin12 :: Output
pin12 = Pin "pin12" "PORTB" "DDRB" "0x10U" -- pin12 (arduino) = pb4 (atmega328p)

uart :: Output
uart = UART
