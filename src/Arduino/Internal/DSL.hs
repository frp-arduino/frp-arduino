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

type Statement a = State DAGState a

newtype Stream a = Stream { unStream :: DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

compileProgram :: Statement a -> IO ()
compileProgram statement = do
    let dagState = execState statement (DAGState 1 DAG.emptyStreams)
    let cCode = streamsToC (dag dagState)
    writeFile "main.c" cCode

(~>) :: Statement (Stream a) -> (Statement (Stream a) -> Statement (Stream b)) -> Statement (Stream b)
(~>) inputStatement fn = do
    stream <- inputStatement
    fn (return stream)

input :: DAG.Body -> Statement (Stream a)
input body = fmap Stream $ do
    addAnonymousStream body

output :: DAG.Body -> Statement (Stream a) -> Statement (Stream a)
output output streamStatement = fmap Stream $ do
    outputName <- addAnonymousStream output
    stream <- streamStatement
    addDependency (unStream stream) outputName

clock :: Statement (Stream Int)
clock = fmap Stream $ do
    addBuiltinStream "clock"

streamMap :: (Expression a -> Expression b) -> Statement (Stream a) -> Statement (Stream b)
streamMap fn inputStatement = fmap Stream $ do
    inputStream <- inputStatement
    expressionStreamName <- addAnonymousStream (DAG.Transform expression)
    addDependency (unStream inputStream) expressionStreamName
    where
        expression = unExpression $ fn $ Expression $ DAG.Input 0

combine :: (Expression a -> Expression b -> Expression c)
        -> Statement (Stream a)
        -> Statement (Stream b)
        -> Statement (Stream c)
combine fn left right = fmap Stream $ do
    leftStream <- left
    rightStream <- right
    expressionStreamName <- addAnonymousStream (DAG.Transform expression)
    addDependency (unStream leftStream) expressionStreamName
    addDependency (unStream rightStream) expressionStreamName
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

addBuiltinStream :: DAG.Identifier -> Statement DAG.Identifier
addBuiltinStream name = addStream name (DAG.Builtin name)

addAnonymousStream :: DAG.Body -> Statement DAG.Identifier
addAnonymousStream body = do
    name <- buildUniqIdentifier "stream"
    addStream name body

buildUniqIdentifier :: String -> Statement DAG.Identifier
buildUniqIdentifier baseName = do
    dag <- get
    let id = idCounter dag
    modify inc
    return $ baseName ++ "_" ++ show id
    where
        inc dag = dag { idCounter = idCounter dag + 1 }

addStream :: DAG.Identifier -> DAG.Body -> Statement DAG.Identifier
addStream name body = do
    streamTreeState <- get
    unless (DAG.hasStream (dag streamTreeState) name) $ do
        modify $ insertStream $ DAG.Stream name [] body []
    return name
    where
        insertStream :: DAG.Stream -> DAGState -> DAGState
        insertStream stream x = x { dag = DAG.addStream (dag x) stream }

addDependency :: DAG.Identifier -> DAG.Identifier -> Statement DAG.Identifier
addDependency source destination = do
    modify (\x -> x { dag = DAG.addDependency source destination (dag x) })
    return destination
