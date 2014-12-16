module Types.Phantom where

import qualified Types.AST as AST

newtype Stream a = Stream { unStream :: AST.Stream }

newtype Expression a = Expression { unExpression :: AST.Expression }

newtype Output a = Output { unOutput :: AST.Output }
