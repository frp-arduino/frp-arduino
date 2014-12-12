module AST where

data Program = Program [Statement]

data Statement = Assignment Pin Expression

data Expression = Builtin String
                | Application Expression Expression

data Pin = Pin
    { name :: String
    , portRegister :: String
    , directionRegister :: String
    , pinMask :: String
    }
    deriving (Show)

(=:) :: Pin -> Expression -> Program
(=:) pin expression = Program $ [Assignment pin expression]

(<->) :: Program -> Program -> Program
(<->) (Program leftStatements) (Program rightStatements) =
    Program $ leftStatements ++ rightStatements

clock :: Expression
clock = Builtin "clock"

toggle :: Expression -> Expression
toggle expression = Application (Builtin "toggle") expression
