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

newtype Stream a = Stream { unStream :: DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

compileProgram :: Action a -> IO ()
compileProgram action = do
    let x = execState action (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag x)
    writeFile "main.c" cCode

(~>) :: Action (Stream a) -> (Action (Stream a) -> Action (Stream b)) -> Action (Stream b)
(~>) input fn = do
    x <- input
    fn (return x)

input :: DAG.Body -> Action (Stream a)
input input = do
    x <- addAnonymousStream input
    return $ Stream x

output :: DAG.Body -> Action (Stream a) -> Action (Stream a)
output output stream = do
    outputName <- addAnonymousStream output
    streamName <- stream
    x <- addDependency (unStream streamName) outputName
    return $ Stream x

clock :: Action (Stream Int)
clock = do
    x <- addBuiltinStream "clock"
    return $ Stream x

streamMap :: (Expression a -> Expression b) -> Action (Stream a) -> Action (Stream b)
streamMap fn stream = do
    lastName <- stream
    thisName <- addAnonymousStream (DAG.Transform expression)
    x <- addDependency (unStream lastName) thisName
    return $ Stream x
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
