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
    ( module Arduino.DSL
    , module Arduino.Library
    -- * GPIO
    , GPIO()
    , digitalOutput
    , digitalRead
    , pin3
    , pin4
    , pin5
    , pin6
    , pin7
    , pin8
    , pin10
    , pin11
    , pin12
    , pin13
    -- * Analog input
    , AnalogInput()
    , analogRead
    , a0
    -- * UART
    , uart
    -- * Clock
    -- | Uses TCNT1 on the Uno to make things happend at specific time
    -- intervals.
    , timerDelta
    , every
    , clock
    ) where

import Arduino.DSL
import Arduino.Library
import Data.Bits (shiftR, (.&.), testBit)
import Prelude hiding (Word)

data GPIO = GPIO
    { name              :: String
    , directionRegister :: String
    , portRegister      :: String
    , pinRegister       :: String
    , bitName           :: String
    }

data AnalogInput = AnalogInput
    { analogName :: String
    , mux        :: Int
    }

-- For mappings, see http://arduino.cc/en/Hacking/PinMapping168

pin3 :: GPIO
pin3 = GPIO "pin3" "DDRD" "PORTD" "PIND" "PD3"

pin4 :: GPIO
pin4 = GPIO "pin4" "DDRD" "PORTD" "PIND" "PD4"

pin5 :: GPIO
pin5 = GPIO "pin5" "DDRD" "PORTD" "PIND" "PD5"

pin6 :: GPIO
pin6 = GPIO "pin6" "DDRD" "PORTD" "PIND" "PD6"

pin7 :: GPIO
pin7 = GPIO "pin7" "DDRD" "PORTD" "PIND" "PD7"

pin8 :: GPIO
pin8 = GPIO "pin8" "DDRB" "PORTB" "PINB" "PB0"

pin10 :: GPIO
pin10 = GPIO "pin10" "DDRB" "PORTB" "PINB" "PB2"

pin11 :: GPIO
pin11 = GPIO "pin11" "DDRB" "PORTB" "PINB" "PB3"

pin12 :: GPIO
pin12 = GPIO "pin12" "DDRB" "PORTB" "PINB" "PB4"

pin13 :: GPIO
pin13 = GPIO "pin13" "DDRB" "PORTB" "PINB" "PB5"

uart :: Output Byte
uart =
    let ubrr = floor ((16000000 / (16 * 9600)) - 1)
        ubrrlValue = ubrr .&. 0xFF
        ubrrhValue = shiftR ubrr 8 .&. 0xFF
    in
    createOutput
        "uart"
        (writeByte "UBRR0H" (byteConstant ubrrhValue) $
         writeByte "UBRR0L" (byteConstant ubrrlValue) $
         setBit "UCSR0C" "UCSZ01" $
         setBit "UCSR0C" "UCSZ00" $
         setBit "UCSR0B" "RXEN0" $
         setBit "UCSR0B" "TXEN0" $
         end)
        (\byte ->
         waitBitSet "UCSR0A" "UDRE0" $
         writeByte "UDR0" byte $
         end)

digitalOutput :: GPIO -> Output Bit
digitalOutput gpio =
    createOutput
        (name gpio)
        (setBit (directionRegister gpio) (bitName gpio) $
         end)
        (\bit ->
         writeBit (portRegister gpio) (bitName gpio) bit $
         end)

digitalRead :: GPIO -> Stream Bit
digitalRead gpio = createInput
    (name gpio)
    (clearBit (directionRegister gpio) (bitName gpio) $
     setBit (portRegister gpio) (bitName gpio) $
     end)
    (readBit (pinRegister gpio) (bitName gpio))

a0 :: AnalogInput
a0 = AnalogInput "a0" 0

analogRead :: AnalogInput -> Stream Word
analogRead an = createInput
    (analogName an)
    (setBit "ADCSRA" "ADEN" $
     setBit "ADMUX" "REFS0" $
     setBit "ADCSRA" "ADPS2" $
     setBit "ADCSRA" "ADPS1" $
     setBit "ADCSRA" "ADPS0" $
     end)
    ((if (testBit (mux an) 3) then setBit else clearBit) "ADMUX" "MUX3" $
     (if (testBit (mux an) 2) then setBit else clearBit) "ADMUX" "MUX2" $
     (if (testBit (mux an) 1) then setBit else clearBit) "ADMUX" "MUX1" $
     (if (testBit (mux an) 0) then setBit else clearBit) "ADMUX" "MUX0" $
     setBit "ADCSRA" "ADSC" $
     waitBitCleared "ADCSRA" "ADSC" $
     readTwoPartWord "ADCL" "ADCH" $
     end)

timerDelta :: Stream Word
timerDelta = createInput
    "timer"
    (setBit "TCCR1B" "CS12" $
     setBit "TCCR1B" "CS10" $
     end)
    (readWord "TCNT1" $
     writeWord "TCNT1" (wordConstant 0) $
     end)

every :: Expression Word -> Stream Word
every limit = accumulatorConstLimit limit timerDelta ~> count

clock :: Stream Word
clock = every 10000
