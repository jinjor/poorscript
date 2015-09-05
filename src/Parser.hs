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
expression = (try $
        do x <- term'
           ws
           op <- addop
           ws
           y <- expression
           return $ A.BinaryExpression op x y)
        <|> term'


term' :: Parser A.Expression
term' = (try $
        do x <- factor
           ws
           op <- mulop
           ws
           y <- term'
           return $ A.BinaryExpression op x y)
        <|> factor

factor :: Parser A.Expression
factor = (try $
        do x <- primaryExpression
           ws
           op <- eqop
           ws
           y <- factor
           return $ A.BinaryExpression op (A.PrimaryExpression x) y)
        <|> (primaryExpression >>= return . A.PrimaryExpression)


addop :: Parser A.BinOp
addop = (try $
    do
      _ <- char '+'
      return A.Plus)
  <|> (try $
    do
      _ <- char '-'
      return A.Minus)


mulop :: Parser A.BinOp
mulop = (try $
    do
      _ <- char '*'
      return A.Mul)
  <|> (try $
    do
      _ <- char '/'
      return A.Div)

eqop :: Parser A.BinOp
eqop = (try $
    do
      _ <- string "=="
      return A.Eq)
  <|> (try $
    do
      _ <- string "!="
      return A.NonEq)

primaryExpression :: Parser A.PrimaryExpression
primaryExpression =
  (try $ function >>= (\(args, statements) -> return $ A.Function args statements))
  <|> (try $ do
    fac <- primaryExpression'
    ws
    tail <- dotTail
    return $ buildPropertyAccess fac tail
  )
  <|> (do
    x <- primaryExpression'
    return x)

buildPropertyAccess :: A.PrimaryExpression -> DotTail -> A.PrimaryExpression
buildPropertyAccess pexp Last = pexp
buildPropertyAccess pexp (Init name tail) =
    buildPropertyAccess (A.PropertyAccess pexp name) tail
buildPropertyAccess pexp (CallArgs args tail) =
    trace ("here") $ buildPropertyAccess (A.Call pexp args) tail

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
  <|> (try $ do
    x <- parens' $ expression
    return $ A.Expression x)
  <|> (try $ literal >>= return . A.Literal)
  <|> (try $ variable >>= return . A.Variable)

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
    ws
    -- string "return"
    -- ws
    right <- expression
    ws
    return $ A.Return right)
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
literal = try intLiteral <|> try stringLiteral' <|> try listLiteral <|> objectLiteral


all' :: Parser A.Module
all' = do x <- statements
          eof
          return $ A.Module x
