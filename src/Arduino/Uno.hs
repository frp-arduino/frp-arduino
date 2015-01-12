-- Copyright (c) 2014 Contributors as noted in the AUTHORS file
--
-- This file is part of frp-arduino.
--
-- frp-arduino is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- frp-arduino is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with frp-arduino.  If not, see <http://www.gnu.org/licenses/>.

module Arduino.Uno
    ( module Arduino.Language
    , module Arduino.Library
    -- * Uno outputs and streams
    , module Arduino.Uno
    ) where

import Arduino.Language
import Arduino.Library
import CCodeGen
import qualified Arduino.Internal.DAG as DAG
import qualified Arduino.Internal.DSL as DSL

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin13 :: DSL.Output Bool
pin13 = DSL.outputPin "pin13" "DDRB" "PORTB" "0x20U"

pin12 :: DSL.Output Bool
pin12 = DSL.outputPin "pin12" "DDRB" "PORTB" "0x10U"

pin11 :: DSL.Output Bool
pin11 = DSL.outputPin "pin11" "DDRB" "PORTB" "0x08U"

pin10 :: DSL.Output Bool
pin10 = DSL.outputPin "pin10" "DDRB" "PORTB" "0x04U"

pin12in :: Stream Bool
pin12in = DSL.inputPin "pin12" "DDRB" "PORTB" "PINB" "PB4"

uart :: DSL.Output String
uart = DSL.Output $ DAG.Pin $ DAG.PinDefinition
    { DAG.pinName  = "uart"
    , DAG.cType    = "char *"
    , DAG.initCode = do
        line $ "#define F_CPU 16000000UL"
        line $ "#define BAUD 9600"
        line $ "#include <util/setbaud.h>"
        line $ "UBRR0H = UBRRH_VALUE;"
        line $ "UBRR0L = UBRRL_VALUE;"
        block "#if USE_2X" $ do
            line $ "UCSR0A |= (1 << U2X0);"
        block "#else" $ do
            line $ "UCSR0A &= ~((1 << U2X0));"
        line $ "#endif"
        line $ "UCSR0C = (1 << UCSZ01) |(1 << UCSZ00);"
        line $ "UCSR0B = (1 << RXEN0) | (1 << TXEN0);"
    , DAG.bodyCode = do
        block "while (*input_0 != 0) {" $ do
            line $ "while ((UCSR0A & (1 << UDRE0)) == 0) {"
            line $ "}"
            line $ "UDR0 = *input_0;"
            line $ "input_0++;"
        line $ "}"
        return Nothing
    }
