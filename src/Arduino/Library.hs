module Arduino.Library where

import Prelude hiding (not)

import Arduino.Language

toggle :: Action (Stream Int) -> Action (Stream Bool)
toggle = streamMap isEven

invert :: Action (Stream Bool) -> Action (Stream Bool)
invert = streamMap not
