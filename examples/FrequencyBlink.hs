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
import Data.Tuple (swap)

main = compileProgram $ do

    setupAlternateBlink pin11 pin12 (createVariableTick a0)

setupAlternateBlink :: GPIO -> GPIO -> Stream a -> Action ()
setupAlternateBlink pin1 pin2 triggerStream = do
    output2 (digitalOutput pin1) (digitalOutput pin2) =: alternate triggerStream
    where
        alternate :: Stream a -> Stream (Bit, Bit)
        alternate = foldpS2Tuple (\_ -> swap) (bitLow, bitHigh)

createVariableTick :: AnalogInput -> Stream ()
createVariableTick limitInput = accumulator limitStream timerDelta
    where
        limitStream :: Stream Arduino.Uno.Word
        limitStream = analogRead limitInput ~> mapS analogToLimit
        analogToLimit :: Expression Arduino.Uno.Word -> Expression Arduino.Uno.Word
        analogToLimit analog = 1000 + analog * 20
