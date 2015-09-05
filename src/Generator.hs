module Generator(generate) where

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Data.List

import qualified AST as A

generate :: A.Module -> String
generate modul = show $ prettyPrint $ generate' modul


generate' :: A.Module -> [Statement ()]
generate' modul =
  case modul of
    A.Module statements -> map generateStatement statements


generateStatement :: A.Statement -> Statement ()
generateStatement statement =
  case statement of
    A.Assign left right ->
      VarDeclStmt () [VarDecl () (Id () left) $ Just (generateExpression right)]
    A.Return right -> ReturnStmt () $ Just $ generateExpression right
    A.EmptyStatement ->
      EmptyStmt ()

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
    A.Function args statements ->
      FuncExpr () Nothing (map (Id ()) args) (map generateStatement statements)
    A.PropertyAccess fac name ->
      DotRef () (generateFactor fac) (Id () name)
    A.Call fac args ->
      CallExpr () (generateFactor fac) (map generateExpression args)
    A.Expression exp -> generateExpression exp
    A.Literal literal -> generateLiteral literal
    A.Variable name -> VarRef () $ Id () name
    A.If exp statements1 statements2 ->
      CallExpr () (VarRef () $ Id () "$if")
        [ generateExpression exp
        , FuncExpr () Nothing [] (map generateStatement statements1)
        , FuncExpr () Nothing [] (map generateStatement statements2)
        ]

generateLiteral :: A.Literal -> Expression ()
generateLiteral literal =
  case literal of
    A.String s -> StringLit () s
    A.Integer i -> IntLit () $ fromInteger i
    A.List list -> CallExpr () (VarRef () $ Id () "$toList") [ArrayLit () (map generateExpression list)]
    A.Object list -> CallExpr () (VarRef () $ Id () "$toObject") [ObjectLit () (map generateKeyValue list)]

generateKeyValue :: (String, A.Expression) -> (Prop (), Expression ())
generateKeyValue (key, value) =
  (PropString () key, generateExpression value)

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
