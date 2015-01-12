-- Copyright (c) 2014 Contributors as noted in the AUTHORS file
--
-- This file is part of frp-arduino.
--
-- frp-arduino is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- frp-arduino is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with frp-arduino.  If not, see <http://www.gnu.org/licenses/>.

module Arduino.Internal.DAG where

import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as M

import CCodeGen (Gen)

type Streams = M.Map Identifier Stream

data Stream = Stream
    { name    :: Identifier
    , inputs  :: [Identifier]
    , body    :: Body
    , outputs :: [Identifier]
    }

data Body = Builtin String
          | Transform Expression
          | Driver LLI LLI

data LLI = WriteBit String String Bit LLI
         | WriteByte String String LLI
         | ReadBit String String
         | WaitBit String String Bit LLI
         | Switch String LLI LLI LLI
         | End

data Bit = High | Low

data Expression = Input Int
                | Not Expression
                | Even Expression
                | CharConstant Char
                | BoolConstant Bool
                | If Expression Expression Expression
                | Many [Expression]

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
