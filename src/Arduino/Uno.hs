module Arduino.Uno
    ( module Arduino.Internal.DSL
    , module Arduino.Library
    , pin13
    , pin12
    , uart
    ) where

import Arduino.Internal.DSL
import Arduino.Library
import qualified Arduino.Internal.DAG as DAG

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: Output Bool
pin13 = Output $ DAG.Pin "pin13" "PORTB" "DDRB" "0x20U"

pin12 :: Output Bool
pin12 = Output $ DAG.Pin "pin12" "PORTB" "DDRB" "0x10U"

uart :: Output String
uart = Output $ DAG.UART
