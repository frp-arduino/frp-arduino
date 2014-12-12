module Stream where

import Control.Monad.State
import qualified Data.Map as M

import qualified AST

type StreamTree = M.Map Identifier Stream

data Stream = Stream
    { name         :: Identifier
    , body         :: Body
    , dependencies :: [Identifier]
    }

data Body = OutputPin AST.Pin
          | Builtin String

type Identifier = String

streamsInTree :: StreamTree -> [Stream]
streamsInTree = M.elems

outputPins :: StreamTree -> [AST.Pin]
outputPins = M.elems . M.mapMaybe getOutputPin

getOutputPin :: Stream -> Maybe AST.Pin
getOutputPin stream = case body stream of
    (OutputPin pin) -> Just pin
    _               -> Nothing

data StreamTreeState = StreamTreeState
    { idCounter  :: Int
    , streamTree :: StreamTree
    }

type StreamTreeBuilder a = State StreamTreeState a

programToStreamTree :: AST.Program -> StreamTree
programToStreamTree program = streamTree $
    execState (buildProgram program) (StreamTreeState 1 M.empty)

buildProgram :: AST.Program -> StreamTreeBuilder ()
buildProgram (AST.Program statements) = do
    mapM_ buildStatement statements

buildStatement :: AST.Statement -> StreamTreeBuilder Identifier
buildStatement statement = case statement of
    (AST.Assignment pin expression) -> do
        restName <- buildExpression expression
        thisName <- buildStream (AST.name pin) (OutputPin pin)
        buildDependency restName thisName
        return thisName

buildExpression :: AST.Expression -> StreamTreeBuilder Identifier
buildExpression expression = case expression of
    (AST.Builtin "clock") -> do
        buildBuiltinStream "clock"
    (AST.Application (AST.Builtin "toggle") expression) -> do
        lastName <- buildExpression expression
        thisName <- buildStream "toggle" (Builtin "toggle")
        buildDependency lastName thisName
        return thisName
    (AST.Application (AST.Builtin "invert") expression) -> do
        lastName <- buildExpression expression
        thisName <- buildStream "invert" (Builtin "invert")
        buildDependency lastName thisName
        return thisName

buildBuiltinStream :: Identifier -> StreamTreeBuilder Identifier
buildBuiltinStream name = do
    streamTreeState <- get
    when (not $ M.member name $ streamTree streamTreeState) $ do
        modify $ insertStream $ Stream name (Builtin name) []
    return name

buildStream :: String -> Body -> StreamTreeBuilder Identifier
buildStream baseName body = do
    uniqName <- buildUniqIdentifier baseName
    modify $ insertStream $ Stream uniqName body []
    return uniqName

buildDependency :: Identifier -> Identifier -> StreamTreeBuilder ()
buildDependency srcStream destStream = do
    modify $ nodesAddDependency srcStream destStream
    where
        nodesAddDependency :: String -> String -> StreamTreeState -> StreamTreeState
        nodesAddDependency nodeName dependencyName nodes = nodes
            { streamTree = M.adjust (addDependency dependencyName) nodeName (streamTree nodes)
            }
        addDependency :: Identifier -> Stream -> Stream
        addDependency name stream = stream
            { dependencies = name : dependencies stream
            }

buildUniqIdentifier :: String -> StreamTreeBuilder Identifier
buildUniqIdentifier baseName = do
    streamTree <- get
    let id = idCounter streamTree
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc streamTree = streamTree { idCounter = idCounter streamTree + 1 }

insertStream :: Stream -> StreamTreeState -> StreamTreeState
insertStream stream x = x { streamTree = M.insert (name stream) stream (streamTree x) }
