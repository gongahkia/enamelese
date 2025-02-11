module Main where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import Lexer (lexer)
import Parser (parse)
import Interpreter (interpret)
import Transpiler (transpile)
import REPL (repl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["repl"] -> repl
    ["run", file] -> do
      content <- TIO.readFile file
      let tokens = lexer content
      let ast = parse tokens
      interpret ast
    ["transpile", file] -> do
      content <- TIO.readFile file
      let tokens = lexer content
      let ast = parse tokens
      let pythonCode = transpile ast
      writeFile (file ++ ".py") pythonCode
    _ -> putStrLn "Usage: enamel [repl|run|transpile] [file]"
