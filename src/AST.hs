module AST where

data Program = Program [Statement]

data Statement = Assignment Pin (Stream Bool)

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
(=:) pin expression = Program $ [Assignment pin expression]

(<->) :: Program -> Program -> Program
(<->) (Program leftStatements) (Program rightStatements) =
    Program $ leftStatements ++ rightStatements

clock :: Stream ()
clock = BuiltinStream "clock"

toggle :: Stream () -> Stream Bool
toggle stream = BuiltinStreamFunction "toggle" stream
