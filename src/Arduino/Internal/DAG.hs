module Arduino.Internal.DAG where

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
          | InputPin Output
          | Builtin String
          | Transform Expression

data Output = Pin String String String String String
            | UART

data Expression = Input Int
                | Not Expression
                | Even Expression
                | StringConstant String

type Identifier = String

emptyStreams :: Streams
emptyStreams = M.empty

addStream :: Streams -> Stream -> Streams
addStream streams stream = M.insert (name stream) stream streams

addDependency :: Identifier -> Identifier -> Streams -> Streams
addDependency source destination =
    (M.adjust (\x -> x { outputs = outputs x ++ [destination] }) source) .
    (M.adjust (\x -> x { inputs  = inputs  x ++ [source]      }) destination)

hasStream :: Streams -> Identifier -> Bool
hasStream streams name = M.member name streams

streamsInTree :: Streams -> [Stream]
streamsInTree = M.elems

streamFromId :: Streams -> Identifier -> Stream
streamFromId tree id = fromJust $ M.lookup id tree
