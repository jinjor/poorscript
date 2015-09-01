
import Text.Parsec
import Parser
import AST
import Generator

main =
  do
    let
      s = " ;\n a = 1000 * { a : 5, ab : 1-2} - ([ 3, \"aaa\"] + \"bb\") * a0bc; "
    case parseAll s of
      Right x -> writeFile "out.js" $ generate x
      Left e -> print e

parseAll :: String -> Either ParseError AST.Module
parseAll src = parse all' "* ParseError *" src
