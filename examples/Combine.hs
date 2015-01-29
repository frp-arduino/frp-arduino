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

    let buttonPressStream = digitalRead pin12
    let greenLed          = digitalOutput pin13
    let redLed1           = digitalOutput pin11
    let redLed2           = digitalOutput pin10

    blinkStream <- def $ clock ~> toggle

    greenLed =: buttonPressStream

    redLed1 =: keepWhen buttonPressStream bitLow blinkStream

    redLed2 =: keepWhen buttonPressStream bitLow (invert blinkStream)
