module Arduino.Library where

import Prelude hiding (not)

import Arduino.Language

toggle :: Stream Int -> Stream Bool
toggle = mapS isEven

invert :: Stream Bool -> Stream Bool
invert = mapS not

keepWhen :: Stream Bool
         -> Expression a
         -> Stream a
         -> Stream a
keepWhen filterStream defaultValue valueStream =
    mapS2 (pick defaultValue) filterStream valueStream
    where
        pick :: Expression a -> Expression Bool -> Expression a -> Expression a
        pick defaultValue first second = if_ first second defaultValue
