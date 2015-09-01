module AST where

data AddOp
  = Plus
  | Minus
  deriving Show

data MulOp
  = Mul
  | Div
  deriving Show

data Expression
  = Sum AddOp Term Expression
  | Term Term
  deriving Show

data Term
  = Product MulOp Factor Term
  | Factor Factor
  deriving Show

data Factor
  = Expression Expression
  | Literal Literal
  | Variable Variable
  deriving Show

data Literal
  = String String
  | Integer Integer
  | List [Expression]
  | Object [(String, Expression)]
  deriving Show

type Variable = String

data Statement
  = Assign Variable Expression
  | EmptyStatement
  deriving Show

data Module
  = Module [Statement]
  deriving Show
