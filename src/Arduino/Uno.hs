module Arduino.Uno
    ( module Arduino.Language
    , module Arduino.Library
    , module Arduino.Uno
    ) where

import Arduino.Language
import Arduino.Library
import qualified Arduino.Internal.DAG as DAG
import qualified Arduino.Internal.DSL as DSL

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: DSL.Output Bool
pin13 = DSL.Output $ DAG.OutputPin $ DAG.Pin "pin13" "PORTB" "PINB" "DDRB" "0x20U"

pin12 :: DSL.Output Bool
pin12 = DSL.Output $ DAG.OutputPin $ DAG.Pin "pin12" "PORTB" "PINB" "DDRB" "0x10U"

pin11 :: DSL.Output Bool
pin11 = DSL.Output $ DAG.OutputPin $ DAG.Pin "pin11" "PORTB" "PINB" "DDRB" "0x08U"

pin10 :: DSL.Output Bool
pin10 = DSL.Output $ DAG.OutputPin $ DAG.Pin "pin10" "PORTB" "PINB" "DDRB" "0x04U"

pin12in :: Stream Bool
pin12in = DSL.input $ DAG.InputPin $ DAG.Pin "pin12" "PORTB" "PINB" "DDRB" "0x10U"

uart :: DSL.Output String
uart = DSL.Output $ DAG.OutputPin $ DAG.UART
