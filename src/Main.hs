
import Text.Parsec
import Parser
import AST
import Generator
import Paths_poorscript

main =
  do
    s <- readFile "in.js"
    coreFile <- Paths_poorscript.getDataFileName "core.js"
    core <- readFile coreFile
    let result = parseAll s
    putStrLn "start"
    case result of
      Right x -> do
        writeFile "out.js" $ generate x
        writeFile "core.js" core
      Left e -> print e

parseAll :: String -> Either ParseError AST.Module
parseAll src = parse all' "* ParseError *" src
