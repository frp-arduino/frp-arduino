module DSL
    ( (=:)
    , clock
    , toggle
    , invert
    , compileProgram
    ) where

import Control.Monad.State

import qualified AST
import CodeGen (programToC)

newtype Stream a = Stream { getExpression :: AST.Expression }

(=:) :: AST.Pin -> Stream Bool -> State AST.Program ()
(=:) pin stream = do
    modify $ addStatement $ AST.Assignment pin (AST.Custom (getExpression stream))
    where
        addStatement :: AST.Statement -> AST.Program -> AST.Program
        addStatement x (AST.Program xs) = AST.Program $ xs ++ [x]

clock :: Stream ()
clock = Stream $ AST.Builtin "clock"

toggle :: Stream () -> Stream Bool
toggle = Stream . AST.Application (AST.Builtin "toggle") . getExpression

invert :: Stream Bool -> Stream Bool
invert = Stream . AST.Map (\e -> AST.Not e) . getExpression

compileProgram :: State AST.Program () -> IO ()
compileProgram state = do
    let program = execState state (AST.Program [])
    writeFile "main.c" (programToC program)
