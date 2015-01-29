-- Copyright (c) 2014 Contributors as noted in the AUTHORS file
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Arduino.Uno

main = compileProgram $ do

    let doubleOutput = output2 (digitalOutput pin12) (digitalOutput pin13)

    doubleOutput =: every 5000 ~> flip2TupleStream

flip2TupleStream :: Stream a -> Stream (Bit, Bit)
flip2TupleStream = foldpS (\_ -> flip2Tuple) (pack2 (bitLow, bitHigh))
    where
        flip2Tuple :: Expression (a, b) -> Expression (b, a)
        flip2Tuple tuple = let (aValue, bValue) = unpack2 tuple
                           in pack2 (bValue, aValue)
