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

newtype Stream a = Stream { unStream :: AST.Stream }

(=:) :: AST.Pin -> Stream Bool -> State AST.Program ()
(=:) pin stream = do
    modify $ addStatement $ AST.Assignment pin (unStream stream)
    where
        addStatement :: AST.Statement -> AST.Program -> AST.Program
        addStatement x (AST.Program xs) = AST.Program $ xs ++ [x]

clock :: Stream Int
clock = Stream $ AST.Builtin "clock"

toggle :: Stream Int -> Stream Bool
toggle stream = Stream (AST.Custom [unStream stream] (AST.Even (AST.Input 0)))

invert :: Stream Bool -> Stream Bool
invert stream = Stream (AST.Custom [unStream stream] (AST.Not (AST.Input 0)))

compileProgram :: State AST.Program () -> IO ()
compileProgram state = do
    let program = execState state (AST.Program [])
    writeFile "main.c" (programToC program)
