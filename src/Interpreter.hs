module Interpreter (interpret) where

import Parser (Expr(..))
import qualified Data.Map as M
import qualified Data.Text as T

type Environment = M.Map T.Text Value

data Value
  = VString T.Text
  | VNumber Double
  | VBoolean Bool
  | VList [Value]
  | VDict (M.Map T.Text Value)
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
evalExpr (FuncDef name params body) env = return $ M.insert name (VFunction params body) env
evalExpr (FuncCall name args) env = do
  case M.lookup name env of
    Just (VFunction params body) -> do
      argValues <- mapM (`evalLiteral` env) args
      let localEnv = M.fromList (zip params argValues) `M.union` env
      foldl (\e s -> e >>= evalExpr s) (return localEnv) body
    _ -> error $ "Undefined function: " ++ T.unpack name
evalExpr expr env = do
  value <- evalLiteral expr env
  print value
  return env

evalLiteral :: Expr -> Environment -> IO Value
evalLiteral (Literal (TString s)) _ = return $ VString s
evalLiteral (Literal (TNumber n)) _ = return $ VNumber n
evalLiteral (Literal (TBoolean b)) _ = return $ VBoolean b
evalLiteral (Literal (TIdentifier name)) env =
  case M.lookup name env of
    Just value -> return value
    Nothing -> error $ "Undefined variable: " ++ T.unpack name
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