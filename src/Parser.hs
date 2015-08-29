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
           return $ A.Sum op x y) <|> (term' >>= return . A.Term)

term' :: Parser A.Term
term' = (try $
        do x <- factor'
           ws
           op <- mulop
           ws
           y <- term'
           return $ A.Product op x y) <|> (factor' >>= return . A.Factor)

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
     return $ A.Expression x) <|> (literal >>= return . A.Literal)

literal :: Parser A.Literal
literal = do x <- number
             return (A.Integer x)

all' :: Parser A.Module
all' = do ws
          x <- expression
          ws
          eof
          return $ A.Module x
