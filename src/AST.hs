module AST where

data Program = Program [Statement]

data Statement = Assignment Pin Expression

data Expression = Builtin String
                | Application Expression Expression
                | Map (Expression -> Expression) Expression
                | Not Expression
                | Variable String

data Pin = Pin
    { name              :: String
    , portRegister      :: String
    , directionRegister :: String
    , pinMask           :: String
    }
