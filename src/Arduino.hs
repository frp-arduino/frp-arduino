module Arduino
    ( module Stages.DSL
    , module Library
    , pin13
    , pin12
    , uart
    ) where

import Library
import qualified Types.AST as AST
import Stages.DSL
import Types.Phantom

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: Output Bool
pin13 = Output $ AST.Pin "pin13" "PORTB" "DDRB" "0x20U"

pin12 :: Output Bool
pin12 = Output $ AST.Pin "pin12" "PORTB" "DDRB" "0x10U"

uart :: Output String
uart = Output $ AST.UART
