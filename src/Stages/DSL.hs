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
import qualified Types.AST as AST
import qualified Types.DAG as DAG
import Stages.CodeGen (streamsToC)
import Types.Phantom

compileProgram :: Action () -> IO ()
compileProgram state = do
    let x = execState state (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag x)
    writeFile "main.c" cCode

(=:) :: Output a -> Stream a -> Action ()
(=:) output stream = do
    restName <- buildNewStream (unStream stream)
    thisName <- buildStream "pin" (DAG.OutputPin (unOutput output))
    buildDependency restName thisName
    buildInput thisName restName

clock :: Stream Int
clock = Stream $ AST.Builtin "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = Stream $ AST.Custom [unStream stream] expression
    where
        expression = unExpression $ fn $ Expression $ AST.Input 0

not :: Expression Bool -> Expression Bool
not = Expression . AST.Not . unExpression

isEven :: Expression Int -> Expression Bool
isEven = Expression . AST.Even . unExpression

stringConstant :: String -> Expression String
stringConstant = Expression . AST.StringConstant

data DAGState = DAGState
    { idCounter :: Int
    , dag       :: DAG.Streams
    }

type Action a = State DAGState a

buildNewStream :: AST.Stream -> Action DAG.Identifier
buildNewStream stream = case stream of
    (AST.Builtin name) -> do
        buildBuiltinStream name
    (AST.Custom [input] expression) -> do
        lastName <- buildNewStream input
        thisName <- buildStream "stream" (DAG.Transform expression)
        buildDependency lastName thisName
        buildInput thisName lastName
        return thisName

buildInput :: DAG.Identifier -> DAG.Identifier -> Action ()
buildInput srcStream inputStream = do
    modify $ nodesAddInput srcStream inputStream
    where
        nodesAddInput :: String -> String -> DAGState -> DAGState
        nodesAddInput nodeName inputName nodes = nodes
            { dag = DAG.addInput (dag nodes) nodeName inputName
            }

buildDependency :: DAG.Identifier -> DAG.Identifier -> Action ()
buildDependency srcStream destStream = do
    modify $ nodesAddDependency srcStream destStream
    where
        nodesAddDependency :: String -> String -> DAGState -> DAGState
        nodesAddDependency nodeName dependencyName nodes = nodes
            { dag = DAG.addOutput (dag nodes) nodeName dependencyName
            }

buildBuiltinStream :: DAG.Identifier -> Action DAG.Identifier
buildBuiltinStream name = do
    streamTreeState <- get
    unless (DAG.hasStream (dag streamTreeState) name) $ do
        modify $ insertStream $ DAG.Stream name [] (DAG.Builtin name) []
    return name

buildStream :: String -> DAG.Body -> Action DAG.Identifier
buildStream baseName body = do
    uniqName <- buildUniqIdentifier baseName
    modify $ insertStream $ DAG.Stream uniqName [] body []
    return uniqName

insertStream :: DAG.Stream -> DAGState -> DAGState
insertStream stream x = x { dag = DAG.addStream (dag x) stream }

buildUniqIdentifier :: String -> Action DAG.Identifier
buildUniqIdentifier baseName = do
    dag <- get
    let id = idCounter dag
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc dag = dag { idCounter = idCounter dag + 1 }
