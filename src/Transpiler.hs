module Transpiler (transpile) where

import Parser (Expr(..))
import Lexer (Token(..))
import qualified Data.Text as T

transpile :: Expr -> String
transpile (Program stmts) = unlines (map transpileStmt stmts)

transpileStmt :: Expr -> String
transpileStmt (VarDecl name expr) = T.unpack name ++ " = " ++ transpileExpr expr
transpileStmt (VarAssign name expr) = T.unpack name ++ " = " ++ transpileExpr expr
transpileStmt (If cond thenStmts elseStmts) =
  "if " ++ transpileExpr cond ++ ":\n" ++
  indent (concatMap transpileStmt thenStmts) ++
  "else:\n" ++
  indent (concatMap transpileStmt elseStmts)
transpileStmt (Loop cond body) =
  "while " ++ transpileExpr cond ++ ":\n" ++
  indent (concatMap transpileStmt body)
transpileStmt (FuncDef name params body) =
  "def " ++ T.unpack name ++ "(" ++ T.unpack (T.intercalate ", " params) ++ "):\n" ++
  indent (concatMap transpileStmt body)
transpileStmt (FuncCall name args) =
  T.unpack name ++ "(" ++ intercalate ", " (map transpileExpr args) ++ ")"
transpileStmt expr = transpileExpr expr

transpileExpr :: Expr -> String
transpileExpr (Literal (TString s)) = show (T.unpack s)
transpileExpr (Literal (TNumber n)) = show n
transpileExpr (Literal (TBoolean True)) = "True"
transpileExpr (Literal (TBoolean False)) = "False"
transpileExpr (Literal (TIdentifier name)) = T.unpack name
transpileExpr expr = error $ "Unsupported expression: " ++ show expr

indent :: String -> String
indent = unlines . map ("    " ++) . lines