module Stages.DSL
    ( compileProgram
    , Stream
    , Expression
    , Output
    , (=:)
    , clock
    , streamMap
    , isEven
    , not
    , stringConstant
    ) where

import Control.Monad.State

import Prelude hiding (not)
import qualified Types.DAG as DAG
import Stages.CodeGen (streamsToC)
import Types.Phantom

compileProgram :: DAG.Action () -> IO ()
compileProgram state = do
    let x = execState state (DAG.DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (DAG.dag x)
    writeFile "main.c" cCode

(=:) :: Output a -> Stream a -> DAG.Action ()
(=:) output stream = do
    outputName <- DAG.addAnonymousStream (DAG.OutputPin (unOutput output))
    streamName <- unStream stream
    DAG.addDependency streamName outputName

clock :: Stream Int
clock = Stream $ DAG.addBuiltinStream "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = Stream $ do
    lastName <- unStream stream
    thisName <- DAG.addAnonymousStream (DAG.Transform expression)
    DAG.addDependency lastName thisName
    return thisName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

not :: Expression Bool -> Expression Bool
not = Expression . DAG.Not . unExpression

isEven :: Expression Int -> Expression Bool
isEven = Expression . DAG.Even . unExpression

stringConstant :: String -> Expression String
stringConstant = Expression . DAG.StringConstant
