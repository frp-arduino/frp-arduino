module Stream where

import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as M

import qualified AST

type StreamTree = M.Map Identifier Stream

data Stream = Stream
    { name         :: Identifier
    , body         :: Body
    , dependencies :: [Identifier]
    , inputs       :: [Identifier]
    }

data Body = OutputPin AST.Pin
          | StreamBody AST.Stream

type Identifier = String

streamsInTree :: StreamTree -> [Stream]
streamsInTree = M.elems

streamFromId :: StreamTree -> Identifier -> Stream
streamFromId tree id = fromJust $ M.lookup id tree

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
    (AST.Assignment pin stream) -> do
        restName <- buildNewStream stream
        thisName <- buildStream (AST.name pin) (OutputPin pin)
        buildDependency restName thisName
        buildInput thisName restName
        return thisName

buildNewStream :: AST.Stream -> StreamTreeBuilder Identifier
buildNewStream stream = case stream of
    (AST.Builtin name) -> do
        buildBuiltinStream name
    (AST.Custom [input] expression) -> do
        lastName <- buildNewStream input
        thisName <- buildStream "stream" (StreamBody (AST.Custom [input] expression))
        buildDependency lastName thisName
        buildInput thisName lastName
        return thisName

buildBuiltinStream :: Identifier -> StreamTreeBuilder Identifier
buildBuiltinStream name = do
    streamTreeState <- get
    when (not $ M.member name $ streamTree streamTreeState) $ do
        modify $ insertStream $ Stream name (StreamBody (AST.Builtin name)) [] []
    return name

buildStream :: String -> Body -> StreamTreeBuilder Identifier
buildStream baseName body = do
    uniqName <- buildUniqIdentifier baseName
    modify $ insertStream $ Stream uniqName body [] []
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

buildInput :: Identifier -> Identifier -> StreamTreeBuilder ()
buildInput srcStream inputStream = do
    modify $ nodesAddInput srcStream inputStream
    where
        nodesAddInput :: String -> String -> StreamTreeState -> StreamTreeState
        nodesAddInput nodeName inputName nodes = nodes
            { streamTree = M.adjust (addInput inputName) nodeName (streamTree nodes)
            }
        addInput :: Identifier -> Stream -> Stream
        addInput name stream = stream
            { inputs = name : inputs stream
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
