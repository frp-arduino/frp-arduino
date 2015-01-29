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
    , init
    , text
    ) where

import Arduino.DSL
import Data.Bits
import Data.Char (ord)
import Prelude hiding (init)

type Command = (Bit, Bit, Bit, Bit, Bit, Word)

data CommandType = Command | Data

output :: Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Bit
       -> Output Command
output rs d4 d5 d6 d7 enable =
    let pulse =  foo enable $ \wordStream -> wordStream
                           ~> mapSMany (\delay -> [ pack2 (bitHigh, 1)
                                                  , pack2 (bitLow, delay)
                                                  ])
                           ~> delay
    in output6 rs d7 d6 d5 d4 pulse

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

text :: String -> [Expression Command]
text string = concatMap charToCommands string
    where
        charToCommands char = let x = ord char in
            [ command Data (getBit 7 x) (getBit 6 x) (getBit 5 x) (getBit 4 x) 100
            , command Data (getBit 3 x) (getBit 2 x) (getBit 1 x) (getBit 0 x) 100
            ]

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
