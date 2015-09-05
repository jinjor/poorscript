module AST where

data BinOp
  = Plus
  | Minus
  | Mul
  | Div
  | Eq
  | NonEq
  deriving Show

data Expression
  = BinaryExpression BinOp Expression Expression
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
