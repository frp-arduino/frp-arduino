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

module Arduino.Library
    (
    -- * Standard library
      module Arduino.Library.Time
    , module Arduino.Library.Tuples
    , module Arduino.Library
    ) where

import Arduino.DSL
import Arduino.Library.Time
import Arduino.Library.Tuples
import Prelude hiding (Word)

toggle :: Stream Word -> Stream Bit
toggle = mapS (boolToBit . isEven)

invert :: Stream Bit -> Stream Bit
invert = mapS flipBit

count :: Stream a -> Stream Word
count = foldpS (\_ state -> state + 1) 0

keepWhen :: Stream Bit
         -> Expression a
         -> Stream a
         -> Stream a
keepWhen filterStream defaultValue valueStream =
    mapS2 (pick defaultValue) filterStream valueStream
    where
        pick :: Expression a -> Expression Bit -> Expression a -> Expression a
        pick defaultValue first second = if_ (isHigh first) second defaultValue
