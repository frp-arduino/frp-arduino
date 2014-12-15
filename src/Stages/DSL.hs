module Stages.DSL
    ( (=:)
    , clock
    , toggle
    , invert
    , compileProgram
    ) where

import Control.Monad.State

import Prelude hiding (not)
import qualified Types.AST as AST
import Stages.Analyze (programToStreams)
import Stages.CodeGen (streamsToC)

newtype Stream a = Stream { unStream :: AST.Stream }

newtype Expression a = Expression { unExpression :: AST.Expression }

(=:) :: AST.Pin -> Stream Bool -> State AST.Program ()
(=:) pin stream = do
    modify $ addStatement $ AST.Assignment pin (unStream stream)
    where
        addStatement :: AST.Statement -> AST.Program -> AST.Program
        addStatement x (AST.Program xs) = AST.Program $ xs ++ [x]

clock :: Stream Int
clock = Stream $ AST.Builtin "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = Stream $ AST.Custom [unStream stream] expression
    where
        expression = unExpression $ fn $ Expression $ AST.Input 0

toggle :: Stream Int -> Stream Bool
toggle = streamMap isEven

invert :: Stream Bool -> Stream Bool
invert = streamMap not

not :: Expression Bool -> Expression Bool
not = Expression . AST.Not . unExpression

isEven :: Expression Int -> Expression Bool
isEven = Expression . AST.Even . unExpression

compileProgram :: State AST.Program () -> IO ()
compileProgram state = do
    let ast = execState state (AST.Program [])
    let streams = programToStreams ast
    let cCode = streamsToC streams
    writeFile "main.c" cCode
