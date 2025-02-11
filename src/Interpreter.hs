module Interpreter (interpret) where

import Parser (Expr(..))
import Lexer (Token(..))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random (randomRIO)
import Control.Exception (catch, SomeException)

type Environment = M.Map T.Text Value

data Value
  = VString T.Text
  | VNumber Double
  | VBoolean Bool
  | VList [Value]
  | VDict (M.Map T.Text Value)
  | VFunction [T.Text] [Expr] Environment
  | VClass T.Text Environment
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
evalExpr (ClassDef name body) env = do
  classEnv <- foldl (\e s -> e >>= evalExpr s) (return M.empty) body
  return $ M.insert name (VClass name classEnv) env
evalExpr (Print expr) env = do
  value <- evalLiteral expr env
  print value
  return env
evalExpr (Input name) env = do
  input <- TIO.getLine
  return $ M.insert name (VString input) env
evalExpr (BinaryOp op left right) env = do
  leftVal <- evalLiteral left env
  rightVal <- evalLiteral right env
  result <- evalBinaryOp op leftVal rightVal
  return $ M.insert "result" result env
evalExpr (TryCatch tryStmts errName catchStmts) env = do
  tryResult <- catch
    (foldl (\e s -> e >>= evalExpr s) (return env) tryStmts)
    (\(e :: SomeException) -> do
      let errorEnv = M.insert errName (VString $ T.pack $ show e) env
      foldl (\e s -> e >>= evalExpr s) (return errorEnv) catchStmts)
  return tryResult
evalExpr (Random min max) env = do
  minVal <- evalLiteral min env
  maxVal <- evalLiteral max env
  case (minVal, maxVal) of
    (VNumber minNum, VNumber maxNum) -> do
      randomNum <- randomRIO (minNum, maxNum)
      return $ M.insert "result" (VNumber randomNum) env
    _ -> error "Random arguments must be numbers"
evalExpr (Length expr) env = do
  value <- evalLiteral expr env
  let len = case value of
        VString s -> fromIntegral $ T.length s
        VList l -> fromIntegral $ length l
        VDict d -> fromIntegral $ M.size d
        _ -> error "Length can only be applied to strings, lists, or dictionaries"
  return $ M.insert "result" (VNumber len) env
evalExpr (TypeConversion expr targetType) env = do
  value <- evalLiteral expr env
  convertedValue <- convertType value targetType
  return $ M.insert "result" convertedValue env
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

evalBinaryOp :: T.Text -> Value -> Value -> IO Value
evalBinaryOp "🍎" (VNumber a) (VNumber b) = return $ VNumber (a + b)
evalBinaryOp "🍐" (VNumber a) (VNumber b) = return $ VNumber (a - b)
evalBinaryOp "🍊" (VNumber a) (VNumber b) = return $ VNumber (a * b)
evalBinaryOp "🍑" (VNumber a) (VNumber b) = return $ VNumber (a / b)
evalBinaryOp "🥥" (VNumber a) (VNumber b) = return $ VNumber (fromIntegral $ floor a `mod` floor b)
evalBinaryOp "🐠" a b = return $ VBoolean (a == b)
evalBinaryOp "🦈" a b = return $ VBoolean (a /= b)
evalBinaryOp "🐙" (VNumber a) (VNumber b) = return $ VBoolean (a > b)
evalBinaryOp "🦀" (VNumber a) (VNumber b) = return $ VBoolean (a < b)
evalBinaryOp "🦋" (VBoolean a) (VBoolean b) = return $ VBoolean (a && b)
evalBinaryOp "🐝" (VBoolean a) (VBoolean b) = return $ VBoolean (a || b)
evalBinaryOp "🐞" (VBoolean a) _ = return $ VBoolean (not a)
evalBinaryOp op _ _ = error $ "Unsupported operator: " ++ T.unpack op

convertType :: Value -> Expr -> IO Value
convertType value (Literal (TIdentifier "string")) = return $ VString $ T.pack $ show value
convertType value (Literal (TIdentifier "number")) = case value of
  VString s -> return $ VNumber $ read $ T.unpack s
  VNumber n -> return $ VNumber n
  VBoolean True -> return $ VNumber 1
  VBoolean False -> return $ VNumber 0
  _ -> error "Cannot convert to number"
convertType value (Literal (TIdentifier "boolean")) = return $ VBoolean $ value /= VBoolean False
convertType _ targetType = error $ "Unsupported type conversion: " ++ show targetType