module Types.DAG where

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

type Identifier = String

data StreamExpression = BuiltinStream String
                      | Custom [StreamExpression] Expression

data Expression = Input Int
                | Not Expression
                | Even Expression
                | StringConstant String

data Output = Pin String String String String
            | UART

emptyStreams :: Streams
emptyStreams = M.empty

addStream :: Streams -> Stream -> Streams
addStream streams stream = M.insert (name stream) stream streams

hasStream :: Streams -> Identifier -> Bool
hasStream streams name = M.member name streams

addInput :: Streams -> Identifier -> Identifier -> Streams
addInput streams stream input = M.adjust foo stream streams
    where
        foo stream = stream { inputs = inputs stream ++ [input] }

addOutput :: Streams -> Identifier -> Identifier -> Streams
addOutput streams stream output = M.adjust foo stream streams
    where
        foo stream = stream { outputs = outputs stream ++ [output] }

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
