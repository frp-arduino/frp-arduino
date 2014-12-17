module Types.Phantom where

import qualified Types.DAG as DAG

newtype Stream a = Stream { unStream :: DAG.Action DAG.Identifier }

newtype Expression a = Expression { unExpression :: DAG.Expression }

newtype Output a = Output { unOutput :: DAG.Output }
