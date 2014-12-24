module Arduino.Internal.DSL where

import Control.Monad.State
import qualified Data.Map as M
import Prelude hiding (not)

import Arduino.Internal.CodeGen (streamsToC)
import qualified Arduino.Internal.DAG as DAG

data DAGState = DAGState
    { idCounter :: Int
    , dag       :: DAG.Streams
    }

type Stream a = State DAGState DAG.Identifier

newtype Expression a = Expression { unExpression :: DAG.Expression }

newtype Output a = Output { unOutput :: DAG.Output }

compileProgram :: Stream a -> IO ()
compileProgram state = do
    let x = execState state (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag x)
    writeFile "main.c" cCode

(=:) :: Output a -> Stream a -> Stream a
(=:) x stream = stream ~> output x

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
(~>) input fn = fn input

input :: Output a -> Stream a
input input = do
    addAnonymousStream (DAG.InputPin (unOutput input))

output :: Output a -> Stream a -> Stream a
output output stream = do
    outputName <- addAnonymousStream (DAG.OutputPin (unOutput output))
    streamName <- stream
    addDependency streamName outputName

clock :: Stream Int
clock = addBuiltinStream "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = do
    lastName <- stream
    thisName <- addAnonymousStream (DAG.Transform expression)
    addDependency lastName thisName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

not :: Expression Bool -> Expression Bool
not = Expression . DAG.Not . unExpression

isEven :: Expression Int -> Expression Bool
isEven = Expression . DAG.Even . unExpression

stringConstant :: String -> Expression String
stringConstant = Expression . DAG.StringConstant

addBuiltinStream :: DAG.Identifier -> Stream a
addBuiltinStream name = addStream name (DAG.Builtin name)

addAnonymousStream :: DAG.Body -> Stream a
addAnonymousStream body = do
    name <- buildUniqIdentifier "stream"
    addStream name body

buildUniqIdentifier :: String -> Stream a
buildUniqIdentifier baseName = do
    dag <- get
    let id = idCounter dag
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc dag = dag { idCounter = idCounter dag + 1 }

addStream :: DAG.Identifier -> DAG.Body -> Stream a
addStream name body = do
    streamTreeState <- get
    unless (DAG.hasStream (dag streamTreeState) name) $ do
        modify $ insertStream $ DAG.Stream name [] body []
    return name
    where
        insertStream :: DAG.Stream -> DAGState -> DAGState
        insertStream stream x = x { dag = DAG.addStream (dag x) stream }

addDependency :: DAG.Identifier -> DAG.Identifier -> Stream a
addDependency source destination = do
    modify (\x -> x { dag = DAG.addDependency source destination (dag x) })
    return destination
