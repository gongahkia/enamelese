module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Lexer (lexer)
import Parser (parse)
import Interpreter (interpret)
import Transpiler (transpile)
import REPL (repl)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Enamel Interpreter and Transpiler"
      putStrLn "Usage: enamel [repl|run|transpile] [file]"
      putStrLn "  repl          Start the Enamel REPL"
      putStrLn "  run <file>    Run an Enamel file"
      putStrLn "  transpile <file> Transpile an Enamel file to Python"
    ["repl"] -> repl
    ["run", file] -> runFile file
    ["transpile", file] -> transpileFile file
    _ -> putStrLn "Invalid arguments. Use 'enamel' without arguments for usage information."

runFile :: FilePath -> IO ()
runFile file = do
  content <- TIO.readFile file
  let tokens = lexer content
  let ast = parse tokens
  interpret ast

transpileFile :: FilePath -> IO ()
transpileFile file = do
  content <- TIO.readFile file
  let tokens = lexer content
  let ast = parse tokens
  let pythonCode = transpile ast
  let outputFile = file ++ ".py"
  writeFile outputFile pythonCode
  putStrLn $ "Transpiled code written to " ++ outputFile