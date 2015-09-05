
import Text.Parsec
import Parser
import AST
import Generator

main =
  do
    s <- readFile "in.js"
    let result = parseAll s
    -- print result
    case result of
      Right x -> writeFile "out.js" $ generate x
      Left e -> print e

parseAll :: String -> Either ParseError AST.Module
parseAll src = parse all' "* ParseError *" src
