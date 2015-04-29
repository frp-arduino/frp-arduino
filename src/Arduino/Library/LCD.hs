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

module Arduino.Library.LCD
    ( Command
    , output
    , position
    , text
    ) where

import Arduino.DSL
import Data.Bits
import Data.Char (ord)
import Prelude hiding (init, Word)

type Command = (Bit, Bit, Bit, Bit, Bit, Word)

data CommandType = Command | Data

output :: Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Command
output rs d4 d5 d6 d7 enable = mergeBootup (output6 rs d7 d6 d5 d4 (pulse enable))
    where
        mergeBootup :: Output Command -> Output Command
        mergeBootup = prefixOutput $ \command ->
            mergeS [ (bootup ~> mapSMany (\_ -> init))
                   , command
                   ]

        pulse :: Output Bit -> Output Word
        pulse = prefixOutput $ \word ->
            word ~> mapSMany (\delay -> [ pack2 (bitHigh, 1)
                                        , pack2 (bitLow, delay)
                                        ])
                 ~> delay

init :: [Expression Command]
init =
    [ command Command 0 0 1 1 5600
    , command Command 0 0 1 1 5600
    , command Command 0 0 1 1 250
    , command Command 0 0 1 0 100
    -- Display lines and character font
    , command Command 0 0 1 0 100
    , command Command 1 0 0 0 100
    -- Display off
    , command Command 0 0 0 0 100
    , command Command 1 0 0 0 100
    -- Display clear
    , command Command 0 0 0 0 100
    , command Command 0 0 0 1 2100
    -- Entry mode set
    , command Command 0 0 0 0 100
    , command Command 0 1 1 0 100
    -- Display on
    , command Command 0 0 0 0 100
    , command Command 1 1 1 1 100
    ]

position :: Int -> Int -> [Expression Command]
position row column = byteToCommands Command (address .|. 0x80)
    where
        address = rowOffset row + column
        rowOffset 0 = 0x00
        rowOffset 1 = 0x40

text :: String -> [Expression Command]
text = concatMap (byteToCommands Data . ord)

byteToCommands :: CommandType -> Int -> [Expression Command]
byteToCommands commandType byte =
    [ command commandType (bit7 byte) (bit6 byte) (bit5 byte) (bit4 byte) 100
    , command commandType (bit3 byte) (bit2 byte) (bit1 byte) (bit0 byte) 100
    ]
    where
        bit0 = getBit 0
        bit1 = getBit 1
        bit2 = getBit 2
        bit3 = getBit 3
        bit4 = getBit 4
        bit5 = getBit 5
        bit6 = getBit 6
        bit7 = getBit 7

getBit :: Int -> Int -> Int
getBit bit number = (number `shiftR` bit) .&. 0x1

command :: CommandType
        -> Int
        -> Int
        -> Int
        -> Int
        -> Word
        -> Expression Command
command commandType b3 b2 b1 b0 delay =
    pack6 ( typeToBit commandType
          , bitFromValue b3
          , bitFromValue b2
          , bitFromValue b1
          , bitFromValue b0
          , fromIntegral delay
          )
    where
        typeToBit :: CommandType -> Expression Bit
        typeToBit Command = bitLow
        typeToBit Data = bitHigh
        bitFromValue :: Int -> Expression Bit
        bitFromValue 0 = bitLow
        bitFromValue 1 = bitHigh
