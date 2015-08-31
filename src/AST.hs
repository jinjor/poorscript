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
  | Variable String
  deriving Show

data Literal
  = String String
  | Integer Integer
  deriving Show

data Module
  = Module Expression
  deriving Show
