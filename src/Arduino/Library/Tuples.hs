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

module Arduino.Library.Tuples
    ( filterS2Tuple
    , foldpS2Tuple
    , pack2Stream
    ) where

import Arduino.DSL

filterS2Tuple :: ((Expression a, Expression b) -> Expression Bool)
              -> Stream (a, b)
              -> Stream (a, b)
filterS2Tuple fn = filterS (fn . unpack2)

foldpS2Tuple :: (Expression a -> (Expression b, Expression c) -> (Expression b, Expression c))
             -> (Expression b, Expression c)
             -> Stream a
             -> Stream (b, c)
foldpS2Tuple fn startValue = foldpS (\x state -> pack2 $ fn x (unpack2 state)) (pack2 startValue)

pack2Stream :: Stream a -> Stream b -> Stream (a, b)
pack2Stream = mapS2 (\a b -> pack2 (a, b))
