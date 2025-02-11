module REPL (repl) where

import Lexer (lexer)
import Parser (parse)
import Interpreter (interpret, evalExpr)
import Transpiler (transpile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)

repl :: IO ()
repl = do
  putStrLn "Welcome to the Enamelese REPL! Type ':q' to quit, ':t' to transpile, or ':h' for help."
  replLoop M.empty

replLoop :: M.Map T.Text Value -> IO ()
replLoop env = do
  putStr "Enamel> "
  hFlush stdout
  input <- TIO.getLine
  case T.unpack input of
    ":q" -> putStrLn "Goodbye!"
    ":h" -> do
      putStrLn "Commands:"
      putStrLn "  :q  - Quit the REPL"
      putStrLn "  :t  - Enter transpile mode"
      putStrLn "  :h  - Show this help message"
      putStrLn "  :env - Show current environment"
      replLoop env
    ":t" -> do
      putStrLn "Enter Enamel code to transpile (empty line to finish):"
      enamelCode <- readMultipleLines
      let tokens = lexer enamelCode
      let ast = parse tokens
      putStrLn "Transpiled Python code:"
      putStrLn $ transpile ast
      replLoop env
    ":env" -> do
      putStrLn "Current environment:"
      mapM_ (\(k, v) -> putStrLn $ T.unpack k ++ " = " ++ show v) (M.toList env)
      replLoop env
    _ -> do
      let tokens = lexer input
      let ast = parse tokens
      newEnv <- catch
        (evalExpr ast env)
        (\(e :: SomeException) -> do
          putStrLn $ "Error: " ++ show e
          return env)
      replLoop newEnv

readMultipleLines :: IO T.Text
readMultipleLines = do
  line <- TIO.getLine
  if T.null line
    then return T.empty
    else do
      rest <- readMultipleLines
      return $ line `T.append` "\n" `T.append` rest
