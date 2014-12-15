module AST where

data Program = Program [Statement]

data Statement = Assignment Pin Stream

data Stream = Builtin String
            | Custom [Stream] Expression

data Expression = Input Int
                | Not Expression
                | Even Expression

data Pin = Pin
    { name              :: String
    , portRegister      :: String
    , directionRegister :: String
    , pinMask           :: String
    }
