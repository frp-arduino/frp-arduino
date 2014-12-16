module Types.AST where

data Program = Program [Statement]

data Statement = Assignment Output Stream

data Stream = Builtin String
            | Custom [Stream] Expression

data Expression = Input Int
                | Not Expression
                | Even Expression
                | StringConstant String

data Output = Pin String String String String
            | UART
