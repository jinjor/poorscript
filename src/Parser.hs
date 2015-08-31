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
factor' = (try $
  do char '('
     ws
     x <- expression
     ws
     char ')'
     return $ A.Expression x)
  <|> (try $ literal >>= return . A.Literal)
  <|> (variable >>= return . A.Variable)

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
    return $ A.Assign left right) <|> (ws >>= (\_ -> return A.EmptyStatement))


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

stringLiteral' :: Parser A.Literal
stringLiteral' = do
    char '"'
    strings <- many character
    char '"'
    return $ A.String $ concat strings


intLiteral :: Parser A.Literal
intLiteral =
  do
    x <- number
    return (A.Integer x)

literal :: Parser A.Literal
literal = try intLiteral <|> stringLiteral'


all' :: Parser A.Module
all' = do x <- statements
          eof
          return $ A.Module x
