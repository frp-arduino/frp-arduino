module AST where

import Control.Monad.State

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

newtype Stream a = Stream { getExpression :: Expression }

(=:) :: Pin -> Stream Bool -> State Program ()
(=:) pin stream = do
    modify $ addStatement $ Assignment pin (getExpression stream)
    where
        addStatement :: Statement -> Program -> Program
        addStatement x (Program xs) = Program $ xs ++ [x]

clock :: Stream ()
clock = Stream $ Builtin "clock"

toggle :: Stream () -> Stream Bool
toggle = Stream . Application (Builtin "toggle") . getExpression

invert :: Stream Bool -> Stream Bool
invert = Stream . Application (Builtin "invert") . getExpression
