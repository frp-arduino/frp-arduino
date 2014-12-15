module Stages.Analyze where

import Control.Monad.State

import qualified Types.AST as AST
import Types.DAG

data StreamDAGState = StreamDAGState
    { idCounter  :: Int
    , streamTree :: Streams
    }

type StreamDAGBuilder a = State StreamDAGState a

programToStreamTree :: AST.Program -> Streams
programToStreamTree program = streamTree $
    execState (buildProgram program) (StreamDAGState 1 emptyStreams)

buildProgram :: AST.Program -> StreamDAGBuilder ()
buildProgram (AST.Program statements) = do
    mapM_ buildStatement statements

buildStatement :: AST.Statement -> StreamDAGBuilder Identifier
buildStatement statement = case statement of
    (AST.Assignment pin stream) -> do
        restName <- buildNewStream stream
        thisName <- buildStream (AST.name pin) (OutputPin pin)
        buildDependency restName thisName
        buildInput thisName restName
        return thisName

buildNewStream :: AST.Stream -> StreamDAGBuilder Identifier
buildNewStream stream = case stream of
    (AST.Builtin name) -> do
        buildBuiltinStream name
    (AST.Custom [input] expression) -> do
        lastName <- buildNewStream input
        thisName <- buildStream "stream" (Transform expression)
        buildDependency lastName thisName
        buildInput thisName lastName
        return thisName

buildBuiltinStream :: Identifier -> StreamDAGBuilder Identifier
buildBuiltinStream name = do
    streamTreeState <- get
    when (not $ hasStream (streamTree streamTreeState) name) $ do
        modify $ insertStream $ Stream name [] (Builtin name) []
    return name

buildStream :: String -> Body -> StreamDAGBuilder Identifier
buildStream baseName body = do
    uniqName <- buildUniqIdentifier baseName
    modify $ insertStream $ Stream uniqName [] body []
    return uniqName

buildDependency :: Identifier -> Identifier -> StreamDAGBuilder ()
buildDependency srcStream destStream = do
    modify $ nodesAddDependency srcStream destStream
    where
        nodesAddDependency :: String -> String -> StreamDAGState -> StreamDAGState
        nodesAddDependency nodeName dependencyName nodes = nodes
            { streamTree = addOutput (streamTree nodes) nodeName dependencyName
            }

buildInput :: Identifier -> Identifier -> StreamDAGBuilder ()
buildInput srcStream inputStream = do
    modify $ nodesAddInput srcStream inputStream
    where
        nodesAddInput :: String -> String -> StreamDAGState -> StreamDAGState
        nodesAddInput nodeName inputName nodes = nodes
            { streamTree = addInput (streamTree nodes) nodeName inputName
            }

buildUniqIdentifier :: String -> StreamDAGBuilder Identifier
buildUniqIdentifier baseName = do
    streamTree <- get
    let id = idCounter streamTree
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc streamTree = streamTree { idCounter = idCounter streamTree + 1 }

insertStream :: Stream -> StreamDAGState -> StreamDAGState
insertStream stream x = x { streamTree = addStream (streamTree x) stream }
