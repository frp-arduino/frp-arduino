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

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
(~>) stream fn = Stream $ do
    streamName <- unStream stream
    let outputStream = fn (Stream (return streamName))
    unStream $ outputStream

input :: DAG.Body -> Stream a
input body = Stream $ do
    addAnonymousStream body

clock :: Stream Int
clock = Stream $ do
    addBuiltinStream "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = Stream $ do
    streamName <- unStream stream
    expressionStreamName <- addAnonymousStream (DAG.Transform expression)
    addDependency streamName expressionStreamName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

combine :: (Expression a -> Expression b -> Expression c)
        -> Stream a
        -> Stream b
        -> Stream c
combine fn left right = Stream $ do
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
    , DAG.cType    = "bool"
    , DAG.initCode = do
        line $ directionRegister ++ " |= " ++ pinMask ++ ";"
    , DAG.bodyCode = do
        block "if (input_0) {" $ do
            line $ portRegister ++ " |= " ++ pinMask ++ ";"
        block "} else {" $ do
            line $ portRegister ++ " &= ~(" ++ pinMask ++ ");"
        line "}"
    }

inputPin :: String -> String -> String -> String -> String -> Stream Bool
inputPin name directionRegister portRegister pinRegister pinMask =
    input $ DAG.Pin $ DAG.PinDefinition
    { DAG.pinName  = name
    , DAG.cType    = "bool"
    , DAG.initCode = do
        line $ directionRegister ++ " &= ~(" ++ pinMask ++ ");"
        line $ portRegister ++ " |= " ++ pinMask ++ ";"
    , DAG.bodyCode = do
        line $ "output = (" ++ pinRegister ++ " & " ++ pinMask ++ ") == 0U;"
    }
