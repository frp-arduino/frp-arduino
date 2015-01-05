module Arduino.Uno
    ( module Arduino.Language
    , module Arduino.Library
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
pin12in = DSL.inputPin "pin12in" "DDRB" "PORTB" "PINB" "0x10U"

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
    }
