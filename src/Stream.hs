module Stream where

import qualified Data.Map as M

import qualified AST

data StreamTree = StreamTree
    { counter  :: Int
    , lastNode :: Identifier
    , streams  :: M.Map Identifier Stream
    }
    deriving (Show)

data StreamType = StreamTypeVoid
                | StreamTypeBool
    deriving (Show)

data Body = OutputPin AST.Pin
          | Builtin String
    deriving (Show)

data Stream = Stream
    { name         :: Identifier
    , inputType    :: StreamType
    , outputType   :: StreamType
    , body         :: Body
    , dependencies :: [Identifier]
    }
    deriving (Show)

type Identifier = String

nodesEmpty :: StreamTree
nodesEmpty = StreamTree 1 "" M.empty

streamsInTree :: StreamTree -> [Stream]
streamsInTree = M.elems . streams

outputPins :: StreamTree -> [AST.Pin]
outputPins = M.elems . M.mapMaybe getOutputPin . streams

getOutputPin :: Stream -> Maybe AST.Pin
getOutputPin stream = case body stream of
    (OutputPin pin) -> Just pin
    _               -> Nothing

addStream :: String -> StreamType -> StreamType -> Body -> StreamTree -> StreamTree
addStream streamName inputType outputType body nodes = nodes
    { counter  = counter nodes + 1
    , lastNode = nodeName
    , streams = M.insert nodeName (Stream nodeName inputType outputType body []) (streams nodes)
    }
    where
        nodeName = streamName ++ "_" ++ show (counter nodes)

addBuiltinStream :: String -> StreamType -> StreamTree -> StreamTree
addBuiltinStream streamName outputType nodes = nodes
    { lastNode = streamName
    , streams = if M.member streamName (streams nodes)
                    then (streams nodes)
                    else M.insert streamName (Stream streamName StreamTypeVoid outputType (Builtin streamName) []) (streams nodes)
    }

addDependency :: Identifier -> Stream -> Stream
addDependency name stream = stream
    { dependencies = name : dependencies stream
    }

nodesAddDependency :: String -> String -> StreamTree -> StreamTree
nodesAddDependency nodeName dependencyName nodes = nodes
    { streams = M.adjust (addDependency dependencyName) nodeName (streams nodes)
    }

programToNodes :: AST.Program -> StreamTree
programToNodes (AST.Program statements) =
    foldl statementToNodes nodesEmpty statements

statementToNodes :: StreamTree -> AST.Statement -> StreamTree
statementToNodes existing (AST.Assignment pin stream) = final
    where
        withThisAdded = addStream (AST.name pin) StreamTypeBool StreamTypeVoid (OutputPin pin) existing
        thisName = lastNode withThisAdded
        rest = streamToNodes stream withThisAdded
        final = nodesAddDependency (lastNode rest) thisName rest

streamToNodes :: AST.Stream a -> StreamTree -> StreamTree
streamToNodes (AST.BuiltinStream "clock") nodes =
    addBuiltinStream "clock" StreamTypeVoid nodes
streamToNodes (AST.BuiltinStreamFunction "toggle" stream) nodes = final
    where
        rest = streamToNodes stream nodes
        lastName = lastNode rest
        withThisAdded = addStream "toggle" StreamTypeVoid StreamTypeBool (Builtin "toggle") rest
        thisName = lastNode withThisAdded
        final = nodesAddDependency lastName thisName withThisAdded
