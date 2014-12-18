module Arduino.Library where

import Prelude hiding (not)

import Arduino.Language

toggle :: Stream Int -> Stream Bool
toggle = streamMap isEven

invert :: Stream Bool -> Stream Bool
invert = streamMap not
