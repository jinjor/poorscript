module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Debug.Trace (trace)

import qualified AST as A

lexer = makeTokenParser(emptyDef)


type Parser a = Parsec String () a

number :: Parser Integer
number = natural lexer

ws :: Parser ()
-- ws = whiteSpace lexer
ws = Text.Parsec.spaces

padded :: Parser a -> Parser a
padded p =
  do  ws
      out <- p
      ws
      return out

surround :: Char -> Char -> String -> Parser a -> Parser a
surround a z name p = do
  char a
  v <- padded p
  char z <?> unwords ["a closing", name, show z]
  return v

braces' :: Parser a -> Parser a
braces' =
  surround '[' ']' "brace"


parens' :: Parser a -> Parser a
parens' =
  surround '(' ')' "paren"


brackets' :: Parser a -> Parser a
brackets' =
  surround '{' '}' "bracket"



expression :: Parser A.Expression
expression = chainl1 term' $ padded addop


term' :: Parser A.Expression
term' = chainl1 factor $ padded mulop

factor :: Parser A.Expression
factor = chainl1 primaryExpression $ padded eqop


addop :: Parser (A.Expression -> A.Expression -> A.Expression)
addop = (try $
    do
      char '+'
      return (\a b-> A.BinaryExpression A.Plus a b))
  <|> (
    do
      char '-'
      return (\a b-> A.BinaryExpression A.Minus a b))


mulop :: Parser (A.Expression -> A.Expression -> A.Expression)
mulop = (try $
    do
      char '*'
      return (\a b-> A.BinaryExpression A.Mul a b))
  <|> (
    do
      char '/'
      return (\a b-> A.BinaryExpression A.Div a b))

eqop :: Parser (A.Expression -> A.Expression -> A.Expression)
eqop = (try $
    do
      _ <- string "=="
      return (\a b-> A.BinaryExpression A.Eq a b))
  <|> (
    do
      _ <- string "!="
      return (\a b-> A.BinaryExpression A.NonEq a b))

primaryExpression :: Parser A.Expression
primaryExpression =
  (try $ function >>= (\(args, statements) -> return $ A.PrimaryExpression $ A.Function args statements))
  <|> (try $ do
    fac <- primaryExpression'
    ws
    tail <- dotTail
    return $ A.PrimaryExpression $ buildPropertyAccess fac tail
  )
  <|> (do
    x <- primaryExpression'
    return $ A.PrimaryExpression x)

buildPropertyAccess :: A.PrimaryExpression -> DotTail -> A.PrimaryExpression
buildPropertyAccess pexp t =
  case t of
    Last -> pexp
    Init name tail -> buildPropertyAccess (A.PropertyAccess pexp name) tail
    CallArgs args tail -> buildPropertyAccess (A.Call pexp args) tail

dotTail :: Parser DotTail
dotTail = (try $ do
    char '.'
    ws
    name <- variable
    ws
    tail <- dotTail
    return $ Init name tail)
  <|> (try $ do
    args <- parens' $ expression `sepBy` comma'
    ws
    tail <- dotTail
    return $ CallArgs args tail)
  <|>
    return Last


data DotTail
  = Init String DotTail
  | CallArgs [A.Expression] DotTail
  | Last

primaryExpression' :: Parser A.PrimaryExpression
primaryExpression' =
  (try $ ifExpr >>= (\(exp, _then, _else) -> return $ A.If exp _then _else))
  <|> (try $ statementsBlock >>= return . A.BlockExpression)
  <|> (try $ do
    x <- parens' $ expression
    -- notFollowedBy $ padded $ string "=>"
    return $ A.Expression x)
  <|> (try $ literal >>= return . A.Literal)
  <|> (variable >>= return . A.Variable)

ifExpr :: Parser (A.Expression, [A.Statement], [A.Statement])
ifExpr = do
    string "if"
    ws
    exp <- parens' $ expression
    ws
    statements1 <- statementsBlock
    ws
    string "else"
    ws
    statements2 <- statementsBlock
    return (exp, statements1, statements2)


variable :: Parser A.Variable
variable = do
    head <- lower
    tail <- many alphaNum
    return $ head : tail

assign :: Parser (A.Variable, A.Expression)
assign = do
    left <- variable
    ws
    char '='
    ws
    right <- expression
    return $ (left, right)

statement :: Parser A.Statement
statement = (try $ do
    (left, right) <- padded assign
    return $ A.Assign left right)
  <|> (try $ do
    exp <- padded expression
    return $ A.Return exp)
  <|> (ws >>= (\_ -> return A.EmptyStatement))


statements :: Parser [A.Statement]
statements = sepBy statement $ char ';'

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

function :: Parser ([A.Variable], [A.Statement])
function =
  do
    args <- parens' $ sepBy variable comma'
    padded $ string "=>"
    statements <- statementsBlock
    return (args, statements)

statementsBlock :: Parser [A.Statement]
statementsBlock = brackets' statements

stringLiteral' :: Parser A.Literal
stringLiteral' =
  do
    char '"'
    strings <- many character
    char '"'
    return $ A.String $ concat strings


intLiteral :: Parser A.Literal
intLiteral =
  do
    x <- number
    return (A.Integer x)


listLiteral :: Parser A.Literal
listLiteral =
  do
    x <- braces' $ sepBy expression comma'
    return (A.List x)

objectLiteral :: Parser A.Literal
objectLiteral =
  do
    x <- brackets' $ sepBy keyValue comma'
    return (A.Object x)

keyValue :: Parser (String, A.Expression)
keyValue =
  do
    key <- many alphaNum
    padded $ char ':'
    value <- expression
    return (key, value)

comma' :: Parser ()
comma' =
  do
    x <- padded $ char ','
    return ()


literal :: Parser A.Literal
literal = try intLiteral
  <|> try stringLiteral'
  <|> try listLiteral
  <|> objectLiteral


all' :: Parser A.Module
all' = do x <- statements
          eof
          return $ A.Module x
