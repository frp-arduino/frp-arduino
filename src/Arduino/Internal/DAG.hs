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

import Data.List (partition)
import Data.Maybe (fromJust)
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Word as W
import Prelude hiding (Word)

type Streams = M.Map Identifier Stream

data Stream = Stream
    { name    :: Identifier
    , inputs  :: [Identifier]
    , body    :: Body
    , outputs :: [(Int, Identifier)]
    }
    deriving (Show, Eq)

data Body = Map Expression
          | MapMany [Expression]
          | Fold Expression Expression
          | Filter Expression
          | Flatten Expression
          | DelayMicroseconds Expression Expression
          | Driver [String] LLI LLI
          | Merge Expression
          | Bootup
          deriving (Show, Eq)

data Expression = Input Int
                | Unit
                | BitConstant Bit
                | ByteConstant Byte
                | WordConstant Word
                | ListConstant [Expression]
                | TupleConstant [Expression]
                | BoolToBit Expression
                | NumberToByteArray Expression
                | TupleValue Int Expression
                | Not Expression
                | Even Expression
                | IsHigh Expression
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Greater Expression Expression
                | Equal Expression Expression
                | If Expression Expression Expression
                deriving (Show, Eq)

data LLI = WriteBit String String LLI LLI
         | WriteByte String LLI LLI
         | WriteWord String LLI LLI
         | ReadBit String String
         | ReadWord String LLI
         | ReadTwoPartWord String String LLI
         | WaitBit String String Bit LLI
         | Const String
         | ConstBit Bit
         | InputValue
         | End
         deriving (Show, Eq)

data Bit = High
         | Low
         deriving (Show, Eq)

type Byte = W.Word8

type Word = W.Word16

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
addDependency source destination streams =
    let n = case M.lookup destination streams of
                Just stream -> length (inputs stream)
                Nothing     -> 0
    in
    (M.adjust (\x -> x { outputs = outputs x ++ [(n, destination)] }) source) $
    (M.adjust (\x -> x { inputs  = inputs  x ++ [source]      }) destination) $
    streams

hasStream :: Streams -> Identifier -> Bool
hasStream streams name = M.member name streams

streamsInTree :: Streams -> [Stream]
streamsInTree = M.elems

sortStreams :: Streams -> [Stream]
sortStreams streams = pick (streamsInTree streams) []
    where
        pick :: [Stream] -> [Stream] -> [Stream]
        pick []   taken = taken
        pick left taken = let (newTaken, newLeft) = split left taken
                          in pick newLeft (taken ++ newTaken)

        split :: [Stream] -> [Stream] -> ([Stream], [Stream])
        split left taken = partition (canTake taken) left

        canTake :: [Stream] -> Stream -> Bool
        canTake streams stream = S.isSubsetOf (S.fromList $ inputs stream)
                                              (S.fromList $ map name streams)

streamFromId :: Streams -> Identifier -> Stream
streamFromId tree id = fromJust $ M.lookup id tree
