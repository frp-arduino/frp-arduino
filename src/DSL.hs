module DSL
    ( (=:)
    , clock
    , toggle
    , invert
    , compileProgram
    ) where

import Control.Monad.State

import AST
import CodeGen (programToC)

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
invert = Stream . Map (\e -> Not e) . getExpression

compileProgram :: State Program () -> IO ()
compileProgram state = do
    let program = execState state (Program [])
    writeFile "main.c" (programToC program)
