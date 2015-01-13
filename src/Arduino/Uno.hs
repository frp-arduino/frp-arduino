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
import Data.Bits
import qualified Arduino.Internal.DAG as DAG
import qualified Arduino.Internal.DSL as DSL

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

data GPIO = GPIO
    { name              :: String
    , directionRegister :: String
    , portRegister      :: String
    , pinRegister       :: String
    , bitName           :: String
    }

pin10GPIO = GPIO
    { name              = "pin12"
    , directionRegister = "DDRB"
    , portRegister      = "PORTB"
    , pinRegister       = "PINB"
    , bitName           = "PB2"
    }

pin11GPIO = GPIO
    { name              = "pin13"
    , directionRegister = "DDRB"
    , portRegister      = "PORTB"
    , pinRegister       = "PINB"
    , bitName           = "PB3"
    }

pin12GPIO = GPIO
    { name              = "pin12"
    , directionRegister = "DDRB"
    , portRegister      = "PORTB"
    , pinRegister       = "PINB"
    , bitName           = "PB4"
    }

pin13GPIO = GPIO
    { name              = "pin13"
    , directionRegister = "DDRB"
    , portRegister      = "PORTB"
    , pinRegister       = "PINB"
    , bitName           = "PB5"
    }

pin13 :: DSL.Output Bool
pin13 = gpioOutput pin13GPIO

pin12 :: DSL.Output Bool
pin12 = gpioOutput pin12GPIO

pin11 :: DSL.Output Bool
pin11 = gpioOutput pin11GPIO

pin10 :: DSL.Output Bool
pin10 = gpioOutput pin10GPIO

pin12in :: Stream Bool
pin12in = gpioInput pin12GPIO

clock :: Stream Int
clock = foldpS (\tick state -> if_ (greater state (numberConstant n))
                                   (sub (add tick state)
                                        (numberConstant n))
                                   (add tick state))
                      (numberConstant 0)
                      timerDelta
            ~> filterS (\value -> greater value (numberConstant n))
            ~> foldpS (\tick state -> add state (numberConstant 1))
                      (numberConstant 0)
    where
        n = 10000

uart :: DSL.Output Char
uart =
    let ubrr = floor ((16000000 / (16 * 9600)) - 1)
        ubrrlValue = ubrr .&. 0xFF :: Int
        ubrrhValue = shiftR ubrr 8 .&. 0xFF :: Int
    in
    DSL.createOutput
        "uart"
        (DSL.writeByte "UBRR0H" (show ubrrhValue) $
         DSL.writeByte "UBRR0L" (show ubrrlValue) $
         DSL.writeBit "UCSR0C" "UCSZ01" DAG.High $
         DSL.writeBit "UCSR0C" "UCSZ00" DAG.High $
         DSL.writeBit "UCSR0B" "RXEN0" DAG.High $
         DSL.writeBit "UCSR0B" "TXEN0" DAG.High $
         DSL.end)
        (DSL.waitBit "UCSR0A" "UDRE0" DAG.High $
         DSL.writeByte "UDR0" identifier $
         DSL.end)
    where
        identifier = "input_0"

gpioOutput :: GPIO -> Output Bool
gpioOutput gpio =
    DSL.createOutput
        (name gpio)
        (DSL.writeBit (directionRegister gpio) (bitName gpio) DAG.High $
         DSL.end)
        (DSL.switch
           identifier
           (DSL.writeBit (portRegister gpio) (bitName gpio) DAG.High DSL.end)
           (DSL.writeBit (portRegister gpio) (bitName gpio) DAG.Low DSL.end) $
         DSL.end)
    where
        identifier = "input_0"

gpioInput :: GPIO -> Stream Bool
gpioInput gpio = DSL.createInput
    (name gpio)
    (DSL.writeBit (directionRegister gpio) (bitName gpio) DAG.Low $
     DSL.writeBit (portRegister gpio) (bitName gpio) DAG.High $
     DSL.end)
    (DSL.readBit (pinRegister gpio) (bitName gpio))

timerDelta :: Stream Int
timerDelta = DSL.createInput
    "timer"
    (DSL.writeBit "TCCR1B" "CS12" DAG.High $
     DSL.writeBit "TCCR1B" "CS10" DAG.High $
     DSL.end)
    (DSL.readWord "TCNT1" $
     DSL.writeWord "TCNT1" "0" $
     DSL.end)
