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

import Arduino.Internal.DAG (Bit)
import Arduino.Internal.DSL
import Arduino.Language
import Arduino.Library
import CCodeGen
import Data.Bits (shiftR, (.&.))
import Prelude hiding (const)

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

data GPIO = GPIO
    { name              :: String
    , directionRegister :: String
    , portRegister      :: String
    , pinRegister       :: String
    , bitName           :: String
    }

pin10GPIO = GPIO "pin10" "DDRB" "PORTB" "PINB" "PB2"
pin11GPIO = GPIO "pin11" "DDRB" "PORTB" "PINB" "PB3"
pin12GPIO = GPIO "pin12" "DDRB" "PORTB" "PINB" "PB4"
pin13GPIO = GPIO "pin13" "DDRB" "PORTB" "PINB" "PB5"

pin13 :: Output Bit
pin13 = gpioOutput pin13GPIO

pin12 :: Output Bit
pin12 = gpioOutput pin12GPIO

pin11 :: Output Bit
pin11 = gpioOutput pin11GPIO

pin10 :: Output Bit
pin10 = gpioOutput pin10GPIO

pin12in :: Stream Bit
pin12in = gpioInput pin12GPIO

clock :: Stream Int
clock = every 10000

every :: Expression Int -> Stream Int
every limit = timerDelta ~> accumulate ~> keepOverflowing ~> count
    where
        accumulate = foldpS (\delta total -> if_ (greater total limit)
                                                 (delta + total - limit)
                                                 (delta + total))
                            0
        keepOverflowing = filterS (\value -> greater value limit)

uart :: Output Char
uart =
    let ubrr = floor ((16000000 / (16 * 9600)) - 1)
        ubrrlValue = ubrr .&. 0xFF :: Int
        ubrrhValue = shiftR ubrr 8 .&. 0xFF :: Int
    in
    createOutput
        "uart"
        (writeByte "UBRR0H" (const $ show ubrrhValue) $
         writeByte "UBRR0L" (const $ show ubrrlValue) $
         setBit "UCSR0C" "UCSZ01" $
         setBit "UCSR0C" "UCSZ00" $
         setBit "UCSR0B" "RXEN0" $
         setBit "UCSR0B" "TXEN0" $
         end)
        (waitBitSet "UCSR0A" "UDRE0" $
         writeByte "UDR0" inputValue $
         end)

gpioOutput :: GPIO -> Output Bit
gpioOutput gpio =
    createOutput
        (name gpio)
        (setBit (directionRegister gpio) (bitName gpio) $
         end)
        (switch
           inputValue
           (setBit (portRegister gpio) (bitName gpio) end)
           (clearBit (portRegister gpio) (bitName gpio) end) $
         end)

gpioInput :: GPIO -> Stream Bit
gpioInput gpio = createInput
    (name gpio)
    (clearBit (directionRegister gpio) (bitName gpio) $
     setBit (portRegister gpio) (bitName gpio) $
     end)
    (readBit (pinRegister gpio) (bitName gpio))

timerDelta :: Stream Int
timerDelta = createInput
    "timer"
    (setBit "TCCR1B" "CS12" $
     setBit "TCCR1B" "CS10" $
     end)
    (readWord "TCNT1" $
     writeWord "TCNT1" (const "0") $
     end)
