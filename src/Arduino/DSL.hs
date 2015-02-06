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

module Arduino.DSL
    (
    -- * Core
      Action
    , Stream
    , Output
    , LLI
    , compileProgram
    , parseProgram
    , def
    , (=:)
    , prefixOutput
    , bootup
    , constStream

    -- * Expressions
    , Expression

    -- ** Bits
    , DAG.Bit
    , bitHigh
    , bitLow
    , flipBit
    , isHigh
    , boolToBit

    -- ** Bytes
    , DAG.Byte

    -- ** Words
    , DAG.Word
    , isEven
    , greater

    -- ** Byte arrays
    , formatString
    , formatNumber

    -- ** Tuples
    , pack2
    , pack6
    , unpack2
    , unpack6
    , output2
    , output6

    -- ** Misc
    , unit
    , isEqual

    -- ** Conditionals
    , if_

    -- * Streams

    -- ** Mapping
    , mapS
    , mapSMany
    , mapS2

    -- ** Merging
    , mergeS

    -- ** Filtering
    , filterS

    -- ** Folding
    , foldpS

    -- ** Flattering
    , flattenS

    -- ** Delaying
    , delay

    -- ** Syntactic sugar
    , (~>)

    -- * Low Level Instructions (LLI)
    -- | The glue between streams and harware.
    , createOutput
    , createInput
    , setBit
    , clearBit
    , writeBit
    , writeByte
    , writeWord
    , readBit
    , readWord
    , readTwoPartWord
    , waitBitSet
    , waitBitCleared
    , byteConstant
    , wordConstant
    , end
    ) where

import Arduino.Internal.CodeGen.C (streamsToC)
import Arduino.Internal.CodeGen.Dot(streamsToDot)
import Control.Monad.State
import Data.Char (ord)
import qualified Arduino.Internal.DAG as DAG
import System.Exit (exitFailure)

data DAGState = DAGState
    { idCounter :: Int
    , dag       :: DAG.Streams
    , resources :: [String]
    , errors    :: [String]
    }

type Action a = State DAGState a

newtype Stream a = Stream { unStream :: Action DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

newtype Output a = Output { unOutput :: Stream a -> Action () }

newtype LLI a = LLI { unLLI :: DAG.LLI }

instance Num (Expression a) where
    (+) left right = Expression $ DAG.Add (unExpression left) (unExpression right)
    (-) left right = Expression $ DAG.Sub (unExpression left) (unExpression right)
    (*) left right = Expression $ DAG.Mul (unExpression left) (unExpression right)
    abs = error "abs not yet implemented"
    signum = error "signum not yet implemented"
    fromInteger value = Expression $ DAG.WordConstant $ fromIntegral value

compileProgram :: Action a -> IO ()
compileProgram action = do
    case parseProgram action of
        Right dag -> do
            writeFile "main.c" $ streamsToC dag
            writeFile "dag.dot" $ streamsToDot dag
        Left errors -> do
            putStrLn "Errors:"
            mapM_ putStrLn errors
            exitFailure

parseProgram :: Action a -> Either [String] DAG.Streams
parseProgram action =
    case errors dagState of
        [] -> Right $ dag dagState
        x  -> Left x
    where
        dagState = execState action (DAGState 1 DAG.emptyStreams [] [])

def :: Stream a -> Action (Stream a)
def stream = do
    name <- unStream stream
    return $ Stream $ return name

(=:) :: Output a -> Stream a -> Action ()
(=:) = unOutput

infixr 0 =:

prefixOutput :: (Stream b -> Stream a) -> Output a -> Output b
prefixOutput fn output = Output $ \stream -> do
    output =: fn stream

bootup :: Stream ()
bootup = Stream $ addStream "bootup" DAG.Bootup

constStream :: Expression a -> Stream a
constStream value = mapS (const value) bootup

output2 :: Output a1
        -> Output a2
        -> Output (a1, a2)
output2 output1 output2 =
    Output $ \stream -> do
        x <- def stream
        output1 =: x ~> mapS (\x -> let (a, _) = unpack2 x in a)
        output2 =: x ~> mapS (\x -> let (_, a) = unpack2 x in a)

output6 :: Output a1
        -> Output a2
        -> Output a3
        -> Output a4
        -> Output a5
        -> Output a6
        -> Output (a1, a2, a3, a4, a5, a6)
output6 output1 output2 output3 output4 output5 output6 =
    Output $ \stream -> do
        x <- def stream
        output1 =: x ~> mapS (\x -> let (a, _, _, _, _, _) = unpack6 x in a)
        output2 =: x ~> mapS (\x -> let (_, a, _, _, _, _) = unpack6 x in a)
        output3 =: x ~> mapS (\x -> let (_, _, a, _, _, _) = unpack6 x in a)
        output4 =: x ~> mapS (\x -> let (_, _, _, a, _, _) = unpack6 x in a)
        output5 =: x ~> mapS (\x -> let (_, _, _, _, a, _) = unpack6 x in a)
        output6 =: x ~> mapS (\x -> let (_, _, _, _, _, a) = unpack6 x in a)

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
(~>) stream fn = Stream $ do
    streamName <- unStream stream
    let outputStream = fn (Stream (return streamName))
    unStream $ outputStream

-- | Similar to map in Haskell. \"S\" is for stream.
mapS :: (Expression a -> Expression b) -> Stream a -> Stream b
mapS fn stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream (DAG.Map expression)
    addDependency streamName expressionStreamName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

-- | Contrast with 'flattenS'.
mapSMany :: (Expression a -> [Expression b]) -> Stream a -> Stream b
mapSMany fn stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream (DAG.MapMany expression)
    addDependency streamName expressionStreamName
    where
        expression = map unExpression $ fn $ Expression $ DAG.Input 0

mapS2 :: (Expression a -> Expression b -> Expression c)
      -> Stream a
      -> Stream b
      -> Stream c
mapS2 fn left right = Stream $ do
    leftName <- unStream left
    rightName <- unStream right
    expressionStreamName <- addAnonymousStream (DAG.Map expression)
    addDependency leftName expressionStreamName
    addDependency rightName expressionStreamName
    where
        expression = unExpression $ fn (Expression $ DAG.Input 0)
                                       (Expression $ DAG.Input 1)

mergeS :: [Stream a] -> Stream a
mergeS streams = Stream $ do
    names <- mapM unStream streams
    expressionStreamName <- addAnonymousStream (DAG.Merge $ DAG.Input 0)
    mapM_ (\x -> addDependency x expressionStreamName) names
    return expressionStreamName

-- | Needs a tuple created with 'pack2'.
delay :: Stream (a, DAG.Word) -> Stream a
delay stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream expression
    addDependency streamName expressionStreamName
    where
        expression = DAG.DelayMicroseconds (DAG.TupleValue 1 (DAG.Input 0))
                                           (DAG.TupleValue 0 (DAG.Input 0))

filterS :: (Expression a -> Expression Bool) -> Stream a -> Stream a
filterS fn stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream filterTransform
    addDependency streamName expressionStreamName
    where
        filterTransform = DAG.Filter expression
        expression = unExpression $ fn $ Expression $ DAG.Input 0

-- | Similar to fold in Haskell. \"S\" is for stream.
--
-- Inspired by <http://elm-lang.org/ Elm's>
-- <http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Signal#foldp foldp>.
foldpS :: (Expression a -> Expression b -> Expression b)
       -> Expression b
       -> Stream a
       -> Stream b
foldpS fn startValue stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream foldTransform
    addDependency streamName expressionStreamName
    where
        foldTransform = DAG.Fold expression startExpression
        expression = unExpression $ fn (Expression $ DAG.Input 0)
                                       (Expression $ DAG.Input 1)
        startExpression = unExpression startValue

-- | Contrast with 'mapSMany'.
flattenS :: Stream [a] -> Stream a
flattenS stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream expression
    addDependency streamName expressionStreamName
    where
        expression = DAG.Flatten $ DAG.Input 0

unit :: Expression ()
unit = Expression $ DAG.Unit

isEqual :: Expression a -> Expression a -> Expression Bool
isEqual left right =
    Expression $ DAG.Equal (unExpression left) (unExpression right)

if_ :: Expression Bool -> Expression a -> Expression a -> Expression a
if_ condition trueExpression falseExpression =
    Expression (DAG.If (unExpression condition)
                       (unExpression trueExpression)
                       (unExpression falseExpression))

greater :: Expression DAG.Word -> Expression DAG.Word -> Expression Bool
greater left right = Expression $ DAG.Greater (unExpression left) (unExpression right)

flipBit :: Expression DAG.Bit -> Expression DAG.Bit
flipBit = Expression . DAG.Not . unExpression

isEven :: Expression DAG.Word -> Expression Bool
isEven = Expression . DAG.Even . unExpression

boolToBit :: Expression Bool -> Expression DAG.Bit
boolToBit = Expression . DAG.BoolToBit . unExpression

isHigh :: Expression DAG.Bit -> Expression Bool
isHigh = Expression . DAG.IsHigh . unExpression

formatString :: String -> Expression [DAG.Byte]
formatString = Expression . DAG.ListConstant . map (DAG.ByteConstant . fromIntegral . ord)

formatNumber :: Expression DAG.Word -> Expression [DAG.Byte]
formatNumber = Expression . DAG.NumberToByteArray . unExpression

pack2 :: (Expression a1, Expression a2) -> Expression (a1, a2)
pack2 (a1, a2) = Expression $ DAG.TupleConstant $
    [ unExpression a1
    , unExpression a2
    ]

pack6 :: (Expression a1, Expression a2, Expression a3, Expression a4, Expression a5, Expression a6)
      -> Expression (a1, a2, a3, a4, a5, a6)
pack6 (a1, a2, a3, a4, a5, a6) = Expression $ DAG.TupleConstant $
    [ unExpression a1
    , unExpression a2
    , unExpression a3
    , unExpression a4
    , unExpression a5
    , unExpression a6
    ]

unpack2 :: Expression (a1, a2) -> (Expression a1, Expression a2)
unpack2 expression =
    ( Expression $ DAG.TupleValue 0 (unExpression expression)
    , Expression $ DAG.TupleValue 1 (unExpression expression)
    )

unpack6 :: Expression (a1, a2, a3, a4, a5, a6)
        -> (Expression a1, Expression a2, Expression a3, Expression a4, Expression a5, Expression a6)
unpack6 expression =
    ( Expression $ DAG.TupleValue 0 (unExpression expression)
    , Expression $ DAG.TupleValue 1 (unExpression expression)
    , Expression $ DAG.TupleValue 2 (unExpression expression)
    , Expression $ DAG.TupleValue 3 (unExpression expression)
    , Expression $ DAG.TupleValue 4 (unExpression expression)
    , Expression $ DAG.TupleValue 5 (unExpression expression)
    )

bitLow :: Expression DAG.Bit
bitLow = Expression $ DAG.BitConstant DAG.Low

bitHigh :: Expression DAG.Bit
bitHigh = Expression $ DAG.BitConstant DAG.High

addAnonymousStream :: DAG.Body -> Action DAG.Identifier
addAnonymousStream body = do
    name <- buildUniqIdentifier "stream"
    addStream name body

buildUniqIdentifier :: String -> Action DAG.Identifier
buildUniqIdentifier baseName = do
    dag <- get
    let id = idCounter dag
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc dag = dag { idCounter = idCounter dag + 1 }

addStream :: DAG.Identifier -> DAG.Body -> Action DAG.Identifier
addStream name body = do
    streamTreeState <- get
    unless (DAG.hasStream (dag streamTreeState) name) $ do
        mapM_ addResource (getResources body)
        modify $ insertStream $ DAG.Stream name [] body []
    return name
    where
        insertStream :: DAG.Stream -> DAGState -> DAGState
        insertStream stream x = x { dag = DAG.addStream (dag x) stream }

getResources :: DAG.Body -> [String]
getResources (DAG.Driver resources _ _) = resources
getResources _                          = []

addDependency :: DAG.Identifier -> DAG.Identifier -> Action DAG.Identifier
addDependency source destination = do
    modify (\x -> x { dag = DAG.addDependency source destination (dag x) })
    return destination

addResource :: String -> Action ()
addResource name = do
    modify addResource'
    return ()
    where
        addResource' dagState =
            if name `elem` resources dagState
                then dagState { errors = errors dagState ++ [name ++ " used twice"]}
                else dagState { resources = name : (resources dagState) }

createInput :: String -> LLI () -> LLI a -> Stream a
createInput name initLLI bodyLLI =
    Stream $ addStream ("input_" ++ name) body
    where
        body = DAG.Driver [name] (unLLI initLLI) (unLLI bodyLLI)

createOutput :: String -> LLI () -> (LLI a -> LLI ()) -> Output a
createOutput name initLLI bodyLLI = Output $ \stream -> do
    streamName <- unStream stream
    outputName <- addAnonymousStream $ DAG.Driver [name] (unLLI initLLI) (unLLI (bodyLLI (LLI DAG.InputValue)))
    addDependency streamName outputName
    return ()

setBit :: String -> String -> LLI a -> LLI a
setBit register bit next = writeBit register bit (constBit DAG.High) next

clearBit :: String -> String -> LLI a -> LLI a
clearBit register bit next = writeBit register bit (constBit DAG.Low) next

writeByte :: String -> LLI DAG.Byte -> LLI a -> LLI a
writeByte register value next = LLI $ DAG.WriteByte register (unLLI value) (unLLI next)

writeWord :: String -> LLI DAG.Word -> LLI a -> LLI a
writeWord register value next = LLI $ DAG.WriteWord register (unLLI value) (unLLI next)

readBit :: String -> String -> LLI DAG.Bit
readBit register bit = LLI $ DAG.ReadBit register bit

readWord :: String -> LLI a -> LLI DAG.Word
readWord register next = LLI $ DAG.ReadWord register (unLLI next)

readTwoPartWord :: String -> String -> LLI a -> LLI DAG.Word
readTwoPartWord lowRegister highRegister next = LLI $ DAG.ReadTwoPartWord lowRegister highRegister (unLLI next)

waitBitSet :: String -> String -> LLI a -> LLI a
waitBitSet register bit next = waitBit register bit DAG.High next

waitBitCleared :: String -> String -> LLI a -> LLI a
waitBitCleared register bit next = waitBit register bit DAG.Low next

waitBit :: String -> String -> DAG.Bit -> LLI a -> LLI a
waitBit register bit value next = LLI $ DAG.WaitBit register bit value (unLLI next)

writeBit :: String -> String -> LLI a -> LLI b -> LLI b
writeBit register bit var next = LLI $ DAG.WriteBit register bit (unLLI var) (unLLI next)

byteConstant :: DAG.Byte -> LLI DAG.Byte
byteConstant = LLI . DAG.Const . show

wordConstant :: DAG.Word -> LLI DAG.Word
wordConstant = LLI . DAG.Const . show

constBit :: DAG.Bit -> LLI DAG.Bit
constBit = LLI . DAG.ConstBit

end :: LLI ()
end = LLI $ DAG.End
