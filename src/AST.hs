module AST where

data Program = Program [Statement]

data Statement = Assignment Pin Expression

data Expression = Builtin String
                | Application Expression Expression

data Pin = Pin
    { name              :: String
    , portRegister      :: String
    , directionRegister :: String
    , pinMask           :: String
    }
    deriving (Show)

-- Type safe constructors

newtype EExpression a = EExpression { getExpression :: Expression }

(=:) :: Pin -> EExpression Bool -> Program
(=:) pin eexpression = Program $ [Assignment pin (getExpression eexpression)]

(<->) :: Program -> Program -> Program
(<->) (Program leftStatements) (Program rightStatements) =
    Program $ leftStatements ++ rightStatements

clock :: EExpression ()
clock = EExpression $ Builtin "clock"

toggle :: EExpression () -> EExpression Bool
toggle eexpression = EExpression $ Application (Builtin "toggle") $ getExpression eexpression

invert :: EExpression Bool -> EExpression Bool
invert eexpression = EExpression $ Application (Builtin "invert") $ getExpression eexpression
