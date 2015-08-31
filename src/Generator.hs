module Generator(generate) where

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint

import qualified AST as A

generate :: A.Module -> String
generate modul = show $ prettyPrint $ generate' modul


generate' :: A.Module -> Expression ()
generate' modul =
  case modul of
    A.Module exp -> generateExpression exp

generateExpression :: A.Expression -> Expression ()
generateExpression exp =
  case exp of
    A.Sum op term exp ->
      InfixExpr () (toJSAddOp op) (generateTerm term) (generateExpression exp)
    A.Term term -> generateTerm term

generateTerm :: A.Term -> Expression ()
generateTerm term =
  case term of
    A.Product op factor term ->
      InfixExpr () (toJSMulOp op) (generateFactor factor) (generateTerm term)
    A.Factor factor -> generateFactor factor

generateFactor :: A.Factor -> Expression ()
generateFactor factor =
  case factor of
    A.Expression exp -> generateExpression exp
    A.Literal literal -> generateLiteral literal
    A.Variable name -> VarRef () $ Id () name


generateLiteral :: A.Literal -> Expression ()
generateLiteral literal =
  case literal of
    A.String s -> StringLit () s
    A.Integer i -> IntLit () $ fromInteger i


toJSAddOp :: A.AddOp -> InfixOp
toJSAddOp op =
  case op of
    A.Plus -> OpAdd
    A.Minus -> OpSub

toJSMulOp :: A.MulOp -> InfixOp
toJSMulOp op =
  case op of
    A.Mul -> OpMul
    A.Div -> OpDiv
