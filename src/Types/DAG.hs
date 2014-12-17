module Types.DAG where

import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Streams = M.Map Identifier Stream

data Stream = Stream
    { name    :: Identifier
    , inputs  :: [Identifier]
    , body    :: Body
    , outputs :: [Identifier]
    }

data Body = OutputPin Output
          | Builtin String
          | Transform Expression

data Output = Pin String String String String
            | UART

data Expression = Input Int
                | Not Expression
                | Even Expression
                | StringConstant String

type Identifier = String

data DAGState = DAGState
    { idCounter :: Int
    , dag       :: Streams
    }

type Action a = State DAGState a

emptyStreams :: Streams
emptyStreams = M.empty

hasStream :: Streams -> Identifier -> Bool
hasStream streams name = M.member name streams

streamsInTree :: Streams -> [Stream]
streamsInTree = M.elems

streamFromId :: Streams -> Identifier -> Stream
streamFromId tree id = fromJust $ M.lookup id tree

outputPins :: Streams -> [Output]
outputPins = M.elems . M.mapMaybe getOutputPin

getOutputPin :: Stream -> Maybe Output
getOutputPin stream = case body stream of
    (OutputPin pin) -> Just pin
    _               -> Nothing

addBuiltinStream :: Identifier -> Action Identifier
addBuiltinStream name = addStream name (Builtin name)

addAnonymousStream :: Body -> Action Identifier
addAnonymousStream body = do
    name <- buildUniqIdentifier "stream"
    addStream name body

buildUniqIdentifier :: String -> Action Identifier
buildUniqIdentifier baseName = do
    dag <- get
    let id = idCounter dag
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc dag = dag { idCounter = idCounter dag + 1 }

addStream :: Identifier -> Body -> Action Identifier
addStream name body = do
    streamTreeState <- get
    unless (hasStream (dag streamTreeState) name) $ do
        modify $ insertStream $ Stream name [] body []
    return name
    where
        insertStream :: Stream -> DAGState -> DAGState
        insertStream stream x = x { dag = addStreamX (dag x) stream }

addStreamX :: Streams -> Stream -> Streams
addStreamX streams stream = M.insert (name stream) stream streams

addDependency :: Identifier -> Identifier -> Action ()
addDependency source destination = do
    addOutput source destination
    addInput destination source

addInput :: Identifier -> Identifier -> Action ()
addInput srcStream inputStream = do
    modify $ nodesAddInput srcStream inputStream
    where
        nodesAddInput :: String -> String -> DAGState -> DAGState
        nodesAddInput nodeName inputName nodes = nodes
            { dag = addInputInner (dag nodes) nodeName inputName
            }
        addInputInner :: Streams -> Identifier -> Identifier -> Streams
        addInputInner streams stream input = M.adjust foo stream streams
            where
                foo stream = stream { inputs = inputs stream ++ [input] }

addOutput :: Identifier -> Identifier -> Action ()
addOutput srcStream destStream = do
    modify $ nodesAddDependency srcStream destStream
    where
        nodesAddDependency :: String -> String -> DAGState -> DAGState
        nodesAddDependency nodeName dependencyName nodes = nodes
            { dag = addOutputInner (dag nodes) nodeName dependencyName
            }
        addOutputInner :: Streams -> Identifier -> Identifier -> Streams
        addOutputInner streams stream output = M.adjust foo stream streams
            where
                foo stream = stream { outputs = outputs stream ++ [output] }
