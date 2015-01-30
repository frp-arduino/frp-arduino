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
import qualified Arduino.Library.LCD as LCD

main = compileProgram $ do

    let rs     = digitalOutput pin3
    let d4     = digitalOutput pin5
    let d5     = digitalOutput pin6
    let d6     = digitalOutput pin7
    let d7     = digitalOutput pin8
    let enable = digitalOutput pin4

    tick <- def clock

    digitalOutput pin13 =: tick ~> toggle

    LCD.output rs d4 d5 d6 d7 enable =: tick ~> mapSMany (\_ ->
        LCD.text "FRP Arduino :)")
