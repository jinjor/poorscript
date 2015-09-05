module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import qualified AST as A

lexer = makeTokenParser(emptyDef)


type Parser a = Parsec String () a

number :: Parser Integer
number = natural lexer

ws :: Parser ()
ws = whiteSpace lexer

expression :: Parser A.Expression
expression = (try $
        do x <- term'
           ws
           op <- addop
           ws
           y <- expression
           return $ A.Sum op x y)
        <|> (term' >>= return . A.Term)


term' :: Parser A.Term
term' = (try $
        do x <- factor'
           ws
           op <- mulop
           ws
           y <- term'
           return $ A.Product op x y)
        <|> (factor' >>= return . A.Factor)

addop :: Parser A.AddOp
addop = (try $
    do
      _ <- char '+'
      return A.Plus)
  <|> (try $
    do
      _ <- char '-'
      return A.Minus)


mulop :: Parser A.MulOp
mulop = (try $
    do
      _ <- char '*'
      return A.Mul)
  <|> (try $
    do
      _ <- char '/'
      return A.Div)



factor' :: Parser A.Factor
factor' =
  (try $ function >>= (\(args, statements) -> return $ A.Function args statements))
  <|> (try $ do
    fac <- factor''
    ws
    tail <- dotTail
    return $ buildPropertyAccess fac tail
  )
  <|> (do
    x <- factor''
    return x)

buildPropertyAccess :: A.Factor -> DotTail -> A.Factor
buildPropertyAccess fac Last = fac
buildPropertyAccess fac (Init name tail) =
    buildPropertyAccess (A.PropertyAccess fac name) tail
buildPropertyAccess fac (CallArgs args tail) =
    buildPropertyAccess (A.Call fac args) tail

dotTail :: Parser DotTail
dotTail = (try $ do
    char '.'
    ws
    name <- variable
    ws
    tail <- dotTail
    return $ Init name tail)
  <|> (try $ do
    char '('
    ws
    args <- expression `sepBy` comma'
    ws
    char ')'
    ws
    tail <- dotTail
    return $ CallArgs args tail)
  <|>
    return Last


data DotTail
  = Init String DotTail
  | CallArgs [A.Expression] DotTail
  | Last

factor'' :: Parser A.Factor
factor'' =
  (try $ ifExpr >>= (\(exp, _then, _else) -> return $ A.If exp _then _else))
  <|> (try $ do
    char '('
    ws
    x <- expression
    ws
    char ')'
    return $ A.Expression x)
  <|> (try $ literal >>= return . A.Literal)
  <|> (try $ variable >>= return . A.Variable)

ifExpr :: Parser (A.Expression, [A.Statement], [A.Statement])
ifExpr = do
    string "if"
    ws
    char '('
    ws
    exp <- expression
    ws
    char ')'
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
    ws
    (left, right) <- assign
    ws
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
    char '('
    ws
    args <- sepBy variable comma'
    ws
    char ')'
    ws
    string "=>"
    ws
    statements <- statementsBlock
    return (args, statements)

statementsBlock :: Parser [A.Statement]
statementsBlock =
  do
    char '{'
    statements' <- statements
    char '}'
    return statements'

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
    char '['
    ws
    x <- sepBy expression comma'
    ws
    char ']'
    return (A.List x)

objectLiteral :: Parser A.Literal
objectLiteral =
  do
    char '{'
    ws
    x <- sepBy keyValue comma'
    ws
    char '}'
    return (A.Object x)

keyValue :: Parser (String, A.Expression)
keyValue =
  do
    key <- many alphaNum
    ws
    char ':'
    ws
    value <- expression
    return (key, value)

comma' :: Parser ()
comma' =
  do
    ws
    x <- char ','
    ws
    return ()


literal :: Parser A.Literal
literal = try intLiteral <|> try stringLiteral' <|> try listLiteral <|> objectLiteral


all' :: Parser A.Module
all' = do x <- statements
          eof
          return $ A.Module x
