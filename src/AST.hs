module AST where

data BinOp
  = Extend
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | NonEq
  | And
  | Or
  deriving Show

data Expression
  = BinaryExpression BinOp Expression Expression
  | PrimaryExpression PrimaryExpression
  deriving Show

data PrimaryExpression
  = Expression Expression
  | PrefixedExpression Prefix PrimaryExpression
  | BlockExpression [Statement]
  | Literal Literal
  | Variable Variable
  | Function [Variable] [Statement]
  | PropertyAccess PrimaryExpression String
  | Call PrimaryExpression [Expression]
  | If Expression Expression Expression
  | Null
  deriving Show

data Prefix = Not
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

data TopStatement
  = Statement Statement
  | Import ModuleName
  deriving Show

data Module
  = Module [TopStatement]
  deriving Show

type ModuleName = [String]
