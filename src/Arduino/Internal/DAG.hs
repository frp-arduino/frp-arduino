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
import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.Map as M

type Streams = M.Map Identifier Stream

data Stream = Stream
    { name    :: Identifier
    , inputs  :: [Identifier]
    , body    :: Body
    , outputs :: [Identifier]
    }

data Body = Transform Expression
          | Driver LLI LLI

data LLI = WriteBit String String Bit LLI
         | WriteByte String LLI LLI
         | WriteWord String LLI LLI
         | ReadBit String String
         | ReadWord String LLI
         | WaitBit String String Bit LLI
         | Switch LLI LLI LLI LLI
         | Const String
         | InputValue
         | End

data Bit = High | Low

data Expression = Input Int
                | FoldState
                | Many [Expression]
                -- Stream transformations
                | Fold Expression Expression
                | Filter Expression Expression
                -- Expression transformations
                | If Expression Expression Expression
                -- Unary operations
                | Not    Expression
                | Even   Expression
                | IsHigh Expression
                -- Binary operations
                | Add     Expression Expression
                | Sub     Expression Expression
                | Greater Expression Expression
                -- Conversion
                | BoolToBit Expression
                -- Constants
                | CharConstant Char
                | BitConstant Bit
                | NumberConstant Int

type Identifier = String

emptyStreams :: Streams
emptyStreams = M.empty

liftStream :: Stream -> Streams
liftStream = addStream emptyStreams

liftStreams :: [Stream] -> Streams
liftStreams = mconcat . map liftStream

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
