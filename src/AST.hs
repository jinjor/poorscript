module AST where

data AddOp
  = Plus
  | Minus
  deriving Show

data MulOp
  = Mul
  | Div
  deriving Show

data EqOp
  = Eq
  | NonEq
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
  = BinEq EqOp PrimaryExpression Factor
  | PrimaryExpression PrimaryExpression
  deriving Show

data PrimaryExpression
  = Expression Expression
  | Literal Literal
  | Variable Variable
  | Function [Variable] [Statement]
  | PropertyAccess PrimaryExpression String
  | Call PrimaryExpression [Expression]
  | If Expression [Statement] [Statement]
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
  | Return Expression
  | EmptyStatement
  deriving Show

data Module
  = Module [Statement]
  deriving Show
