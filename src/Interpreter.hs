module Interpreter (interpret) where

import Parser (Expr(..))
import Lexer (Token(..))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Environment = M.Map T.Text Value

data Value
  = VString T.Text
  | VNumber Double
  | VBoolean Bool
  | VList [Value]
  | VDict (M.Map T.Text Value)
  | VFunction [T.Text] [Expr] Environment
  deriving (Show)

interpret :: Expr -> IO ()
interpret expr = do
  _ <- evalExpr expr M.empty
  return ()

evalExpr :: Expr -> Environment -> IO Environment
evalExpr (Program stmts) env = foldl (\e s -> e >>= evalExpr s) (return env) stmts
evalExpr (VarDecl name expr) env = do
  value <- evalLiteral expr env
  return $ M.insert name value env
evalExpr (VarAssign name expr) env = do
  value <- evalLiteral expr env
  return $ M.insert name value env
evalExpr (If cond thenStmts elseStmts) env = do
  condValue <- evalLiteral cond env
  case condValue of
    VBoolean True -> foldl (\e s -> e >>= evalExpr s) (return env) thenStmts
    VBoolean False -> foldl (\e s -> e >>= evalExpr s) (return env) elseStmts
    _ -> error "Condition must be a boolean"
evalExpr (Loop cond body) env = evalLoop cond body env
evalExpr (FuncDef name params body) env = return $ M.insert name (VFunction params body env) env
evalExpr (FuncCall name args) env = do
  case M.lookup name env of
    Just (VFunction params body closureEnv) -> do
      argValues <- mapM (`evalLiteral` env) args
      let localEnv = M.fromList (zip params argValues) `M.union` closureEnv
      foldl (\e s -> e >>= evalExpr s) (return localEnv) body
    _ -> error $ "Undefined function: " ++ T.unpack name
evalExpr (Print expr) env = do
  value <- evalLiteral expr env
  print value
  return env
evalExpr (Input name) env = do
  input <- TIO.getLine
  return $ M.insert name (VString input) env
evalExpr expr env = do
  value <- evalLiteral expr env
  return env

evalLiteral :: Expr -> Environment -> IO Value
evalLiteral (Literal (TString s)) _ = return $ VString s
evalLiteral (Literal (TNumber n)) _ = return $ VNumber n
evalLiteral (Literal (TBoolean b)) _ = return $ VBoolean b
evalLiteral (Literal (TIdentifier name)) env =
  case M.lookup name env of
    Just value -> return value
    Nothing -> error $ "Undefined variable: " ++ T.unpack name
evalLiteral (List exprs) env = do
  values <- mapM (`evalLiteral` env) exprs
  return $ VList values
evalLiteral (Dict pairs) env = do
  evaledPairs <- mapM (\(k, v) -> do
    key <- evalLiteral k env
    value <- evalLiteral v env
    case key of
      VString s -> return (s, value)
      _ -> error "Dictionary keys must be strings") pairs
  return $ VDict (M.fromList evaledPairs)
evalLiteral expr _ = error $ "Invalid literal: " ++ show expr

evalLoop :: Expr -> [Expr] -> Environment -> IO Environment
evalLoop cond body env = do
  condValue <- evalLiteral cond env
  case condValue of
    VBoolean True -> do
      newEnv <- foldl (\e s -> e >>= evalExpr s) (return env) body
      evalLoop cond body newEnv
    VBoolean False -> return env
    _ -> error "Loop condition must be a boolean"