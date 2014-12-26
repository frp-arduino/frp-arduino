module Arduino.Library where

import Prelude hiding (not)

import Arduino.Language

toggle :: Statement (Stream Int) -> Statement (Stream Bool)
toggle = streamMap isEven

invert :: Statement (Stream Bool) -> Statement (Stream Bool)
invert = streamMap not
