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

type Action a = State DAGState a

newtype Stream a = Stream { unStream :: Action DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

compileProgram :: Action a -> IO ()
compileProgram action = do
    let x = execState action (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag x)
    writeFile "main.c" cCode

def :: Stream a -> Action (Stream a)
def stream = do
    identifier <- unStream stream
    return $ Stream $ return identifier

(~>) :: Stream a -> (Stream a -> Stream b) -> Stream b
(~>) input fn = fn input

input :: DAG.Body -> Stream a
input input = Stream $ do
    addAnonymousStream input

output :: DAG.Body -> Stream a -> Stream a
output output stream = Stream $ do
    outputName <- addAnonymousStream output
    streamName <- unStream stream
    addDependency streamName outputName

clock :: Stream Int
clock = Stream $ do
    addBuiltinStream "clock"

streamMap :: (Expression a -> Expression b) -> Stream a -> Stream b
streamMap fn stream = Stream $ do
    lastName <- unStream stream
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
