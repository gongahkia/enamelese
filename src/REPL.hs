module REPL (repl) where

import Lexer (lexer)
import Parser (parse)
import Interpreter (interpret)
import Transpiler (transpile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

repl :: IO ()
repl = do
  putStr "Enamelese> "
  input <- TIO.getLine
  case T.unpack input of
    ":q" -> putStrLn "Goodbye!"
    ":t" -> do
      putStrLn "Enter Enamelese code to transpile:"
      enamelCode <- TIO.getLine
      let tokens = lexer enamelCode
      let ast = parse tokens
      putStrLn $ transpile ast
      repl
    _ -> do
      let tokens = lexer input
      let ast = parse tokens
      interpret ast
      repl