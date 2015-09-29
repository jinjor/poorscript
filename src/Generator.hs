module Generator(generate) where

import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Data.List
import Util

import qualified AST as A

generate :: A.Module -> String
generate modul = show $ prettyPrint $ generateModule modul


generateModule :: A.Module -> [Statement ()]
generateModule modul =
  case modul of
    A.Module statements ->
      let
        (imports, inner) = separateTopStatements statements
        inner' = map generateStatement inner
        exposed = exposedFunctions statements
        keyValue name = (PropString () name, VarRef () $ Id () name)
        obj = ObjectLit () (map keyValue exposed)
        ret = ReturnStmt () $ Just $ obj
        importRefs = map (\moduleName -> Id () $ intercalate "$" moduleName) imports
        func = FuncExpr () Nothing importRefs (inner' ++ [ret])
        apply = CallExpr () (VarRef () $ Id () "$apply") [func]
        importArgs = ArrayLit () $ map (\moduleName -> StringLit () $ intercalate "." moduleName) imports
        register = CallExpr () (VarRef () $ Id () "$register") [StringLit () "Main", importArgs, apply]
      in
        [ExprStmt () register]

separateTopStatements :: [A.TopStatement] -> ([A.ModuleName], [A.Statement])
separateTopStatements topStatements =
  separateMap (\topStatement -> case topStatement of
    A.Import name -> Left name
    A.Statement s -> Right s
  ) topStatements


exposedFunctions :: [A.TopStatement] -> [A.Variable]
exposedFunctions statements =
  filterMap (\s -> case s of
      A.Statement (A.Assign left _) -> Just left
      _ -> Nothing) statements

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
    A.BinaryExpression A.Extend exp1 exp2 ->
      CallExpr () (VarRef () $ Id () "$extend") [(generateExpression exp1), (generateExpression exp2)]
    A.BinaryExpression op exp1 exp2 ->
      InfixExpr () (toJSOp op) (generateExpression exp1) (generateExpression exp2)
    A.PrimaryExpression exp -> generatePrimaryExpression exp

generatePrimaryExpression :: A.PrimaryExpression -> Expression ()
generatePrimaryExpression pexp =
  case pexp of
    A.Null -> NullLit ()
    A.PrefixedExpression op exp ->
      PrefixExpr () (generatePrefix op) (generateExpression exp)
    A.BlockExpression statements ->
      CallExpr () (VarRef () $ Id () "$apply")
        [FuncExpr () Nothing [] (map generateStatement statements)]
      -- FuncExpr () Nothing [] (map generateStatement statements)
    A.Function args statements ->
      FuncExpr () Nothing (map (Id ()) args) (map generateStatement statements)
    A.PropertyAccess pexp name ->
      DotRef () (generatePrimaryExpression pexp) (Id () name)
    A.Call pexp args ->
      CallExpr () (generatePrimaryExpression pexp) (map generateExpression args)
    A.Expression exp -> generateExpression exp
    A.Literal literal -> generateLiteral literal
    A.Variable name -> VarRef () $ Id () name
    A.If exp exp1 exp2 ->
      CallExpr () (VarRef () $ Id () "$if")
        [ generateExpression exp
        , FuncExpr () Nothing [] [ReturnStmt () $ Just $ generateExpression exp1]
        , FuncExpr () Nothing [] [ReturnStmt () $ Just $ generateExpression exp2]
        ]

generatePrefix :: A.Prefix -> PrefixOp
generatePrefix op =
  case op of
    A.Not -> PrefixLNot


generateLiteral :: A.Literal -> Expression ()
generateLiteral literal =
  case literal of
    A.String s -> StringLit () s
    A.Integer i -> IntLit () $ fromInteger i
    -- A.List list -> CallExpr () (VarRef () $ Id () "$toList") [ArrayLit () (map generateExpression list)]
    A.List list -> ArrayLit () (map generateExpression list)
    -- A.Object list -> CallExpr () (VarRef () $ Id () "$toObject") [ObjectLit () (map generateKeyValue list)]
    A.Object list -> ObjectLit () (map generateKeyValue list)

generateKeyValue :: (String, A.Expression) -> (Prop (), Expression ())
generateKeyValue (key, value) =
  (PropString () key, generateExpression value)

toJSOp :: A.BinOp -> InfixOp
toJSOp op =
  case op of
    A.Plus -> OpAdd
    A.Minus -> OpSub
    A.Mul -> OpMul
    A.Div -> OpDiv
    A.Eq -> OpStrictEq
    A.NonEq -> OpStrictNEq
    A.And -> OpLAnd
    A.Or -> OpLOr
