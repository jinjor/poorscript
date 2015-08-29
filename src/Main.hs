
import Text.Parsec
import Parser
import AST
import Generator

main =
  do
    let
      s = " \n 1000 - (3 + 50) * 5 "
    case parseAll s of
      Right x -> writeFile "out.js" $ generate x
      Left e -> print e

parseAll :: String -> Either ParseError AST.Module
parseAll src = parse all' "* ParseError *" src
