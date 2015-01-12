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

module Arduino.Internal.DSL where

import Control.Monad.State
import qualified Data.Map as M
import Prelude hiding (not)

import Arduino.Internal.CodeGen (streamsToC)
import CCodeGen
import qualified Arduino.Internal.DAG as DAG

data DAGState = DAGState
    { idCounter :: Int
    , dag       :: DAG.Streams
    }

type Action a = State DAGState a

newtype Stream a = Stream { unStream :: Action DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

newtype Output a = Output { unOutput :: DAG.Body }

newtype LLI a = LLI { unLLI :: DAG.LLI }

compileProgram :: Action a -> IO ()
compileProgram action = do
    let dagState = execState action (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag dagState)
    writeFile "main.c" cCode

def :: Stream a -> Action (Stream a)
def stream = do
    name <- unStream stream
    return $ Stream $ return name

(=:) :: Output a -> Stream a -> Action ()
(=:) output stream = do
    streamName <- unStream stream
    outputName <- addAnonymousStream (unOutput output)
    addDependency streamName outputName
    return ()

infixr 0 =:

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
(~>) stream fn = Stream $ do
    streamName <- unStream stream
    let outputStream = fn (Stream (return streamName))
    unStream $ outputStream

clock :: Stream Int
clock = Stream $ do
    addBuiltinStream "clock"

mapS :: (Expression a -> Expression b) -> Stream a -> Stream b
mapS fn stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream (DAG.Transform expression)
    addDependency streamName expressionStreamName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

mapS2 :: (Expression a -> Expression b -> Expression c)
      -> Stream a
      -> Stream b
      -> Stream c
mapS2 fn left right = Stream $ do
    leftName <- unStream left
    rightName <- unStream right
    expressionStreamName <- addAnonymousStream (DAG.Transform expression)
    addDependency leftName expressionStreamName
    addDependency rightName expressionStreamName
    where
        expression = unExpression $ fn (Expression $ DAG.Input 0)
                                       (Expression $ DAG.Input 1)

if_ :: Expression Bool -> Expression a -> Expression a -> Expression a
if_ condition trueExpression falseExpression =
    Expression (DAG.If (unExpression condition)
                       (unExpression trueExpression)
                       (unExpression falseExpression))

not :: Expression Bool -> Expression Bool
not = Expression . DAG.Not . unExpression

isEven :: Expression Int -> Expression Bool
isEven = Expression . DAG.Even . unExpression

stringConstant :: String -> Expression String
stringConstant = Expression . DAG.StringConstant

boolConstant :: Bool -> Expression Bool
boolConstant = Expression . DAG.BoolConstant

addBuiltinStream :: DAG.Identifier -> Action DAG.Identifier
addBuiltinStream name = addStream name (DAG.Builtin name)

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
        modify $ insertStream $ DAG.Stream name [] body []
    return name
    where
        insertStream :: DAG.Stream -> DAGState -> DAGState
        insertStream stream x = x { dag = DAG.addStream (dag x) stream }

addDependency :: DAG.Identifier -> DAG.Identifier -> Action DAG.Identifier
addDependency source destination = do
    modify (\x -> x { dag = DAG.addDependency source destination (dag x) })
    return destination

outputPin :: String -> String -> String -> String -> Output Bool
outputPin name directionRegister portRegister pinMask =
    Output $ DAG.Pin $ DAG.PinDefinition
    { DAG.pinName  = name
    , DAG.cType    = "void"
    , DAG.initCode = do
        line $ directionRegister ++ " |= " ++ pinMask ++ ";"
    , DAG.bodyCode = do
        block "if (input_0) {" $ do
            line $ portRegister ++ " |= " ++ pinMask ++ ";"
        block "} else {" $ do
            line $ portRegister ++ " &= ~(" ++ pinMask ++ ");"
        line "}"
        return Nothing
    }

createInput :: String -> LLI () -> LLI a -> Stream a
createInput name initLLI bodyLLI =
    Stream $ addStream ("input_" ++ name) body
    where
        body = DAG.Driver (unLLI initLLI) (unLLI bodyLLI)

writeBit :: String -> String -> DAG.Bit -> LLI a -> LLI a
writeBit register bit value next = LLI $ DAG.WriteBit register bit value (unLLI next)

readBit :: String -> String -> LLI Bool
readBit register bit = LLI $ DAG.ReadBit register bit

end :: LLI ()
end = LLI $ DAG.End
