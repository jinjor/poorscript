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
expression = chainl1 extension $ padded extendop

extension :: Parser A.Expression
extension = chainl1 term' $ padded addop

term' :: Parser A.Expression
term' = chainl1 factor $ padded mulop

factor :: Parser A.Expression
factor = chainl1 factor' $ padded orop

factor' :: Parser A.Expression
factor' = chainl1 factor'' $ padded andop

factor'' :: Parser A.Expression
factor'' = chainl1 primaryExpression $ padded eqop


extendop :: Parser (A.Expression -> A.Expression -> A.Expression)
extendop = (try $
    do
      char '#'
      return (\a b-> A.BinaryExpression A.Extend a b))


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

andop :: Parser (A.Expression -> A.Expression -> A.Expression)
andop =
    do
      _ <- string "&&"
      return (\a b-> A.BinaryExpression A.And a b)

orop :: Parser (A.Expression -> A.Expression -> A.Expression)
orop =
    do
      _ <- string "||"
      return (\a b-> A.BinaryExpression A.Or a b)

primaryExpression :: Parser A.Expression
primaryExpression =
  (try $ function >>= (\(args, statements) -> return $ A.PrimaryExpression $ A.Function args statements))
  <|> (try $ prefixedExpression >>= return . A.PrimaryExpression)
  <|> (try $ do
    exp <- primaryExpression'
    ws
    tail <- expTail
    return $ A.PrimaryExpression $ buildPropertyAccess exp tail
  )
  <|> (do
    x <- primaryExpression'
    return $ A.PrimaryExpression x)

buildPropertyAccess :: A.PrimaryExpression -> ExpTail -> A.PrimaryExpression
buildPropertyAccess pexp t =
  case t of
    Last -> pexp
    Dot name tail ->
      buildPropertyAccess (A.PropertyAccess pexp name) tail
    CallArgs args tail ->
      buildPropertyAccess (A.Call pexp args) tail

expTail :: Parser ExpTail
expTail = (try $ do
    char '.'
    ws
    name <- variable
    ws
    tail <- expTail
    return $ Dot name tail)
  <|> (try $ do
    args <- parens' $ expression `sepBy` comma'
    ws
    tail <- expTail
    return $ CallArgs args tail)
  <|>
    return Last

data ExpTail
  = Dot String ExpTail
  | CallArgs [A.Expression] ExpTail
  | Last

primaryExpression' :: Parser A.PrimaryExpression
primaryExpression' =
  (try $ (string "null") >>= (\_ -> return A.Null))
  <|> (try $ ifExpr >>= (\(exp, _then, _else) -> return $ A.If exp _then _else))
  <|> (try $ statementsBlock >>= return . A.BlockExpression)
  <|> (try $ parens' $ expression >>= return . A.Expression)
  <|> (try $ literal >>= return . A.Literal)
  <|> (variable >>= return . A.Variable)


prefixedExpression :: Parser A.PrimaryExpression
prefixedExpression = (try $ do
  op <- prefixOp
  ws
  pexp <- primaryExpression
  return $ A.PrefixedExpression op pexp)

prefixOp :: Parser A.Prefix
prefixOp = (try $ do char '!' >>= (\_ -> return A.Not))

ifExpr :: Parser (A.Expression, A.Expression, A.Expression)
ifExpr = do
    string "if"
    ws
    exp <- parens' $ expression
    ws
    exp1 <- expression
    ws
    string "else"
    ws
    exp2 <- expression
    return (exp, exp1, exp2)


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
    (left, right) <- assign
    return $ A.Assign left right)
  <|> (try $ do
    exp <- expression
    return $ A.Return exp)


statements :: Parser [A.Statement]
statements = endBy statement $ lineSep

statements1 :: Parser [A.Statement]
statements1 = endBy1 statement $ lineSep

topStatement :: Parser A.TopStatement
topStatement = (try $ do
    imprt' <- string "import"
    ws
    name <- moduleName
    return $ A.Import name)
  <|> (statement >>= return . A.Statement)

moduleName :: Parser A.ModuleName
moduleName = sepBy1 (many1 alphaNum) $ char '.'


topStatements :: Parser [A.TopStatement]
topStatements = endBy topStatement $ lineSep

lineSep :: Parser ()
lineSep = do
  padded $ char ';'
  return ()

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
statementsBlock = brackets' statements1

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
all' = do x <- topStatements
          eof
          return $ A.Module x
