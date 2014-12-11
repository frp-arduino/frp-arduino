module AST where

data Program = Assignment Pin (Stream Bool)
    deriving (Show)

data Stream a = BuiltinStream String
              | BuiltinStreamFunction String (Stream ())
    deriving (Show)

data Pin = Pin
    { name :: String
    , portRegister :: String
    , directionRegister :: String
    , pinMask :: String
    }
    deriving (Show)

(=:) :: Pin -> Stream Bool -> Program
(=:) pin expression = Assignment pin expression

clock :: Stream ()
clock = BuiltinStream "clock"

toggle :: Stream () -> Stream Bool
toggle stream = BuiltinStreamFunction "toggle" stream
