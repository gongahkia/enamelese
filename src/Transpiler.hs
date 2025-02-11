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
transpileStmt (ClassDef name body) =
  "class " ++ T.unpack name ++ ":\n" ++
  indent (concatMap transpileStmt body)
transpileStmt (Print expr) = "print(" ++ transpileExpr expr ++ ")"
transpileStmt (Input name) = T.unpack name ++ " = input()"
transpileStmt (TryCatch tryStmts errName catchStmts) =
  "try:\n" ++
  indent (concatMap transpileStmt tryStmts) ++
  "except Exception as " ++ T.unpack errName ++ ":\n" ++
  indent (concatMap transpileStmt catchStmts)
transpileStmt expr = transpileExpr expr

transpileExpr :: Expr -> String
transpileExpr (Literal (TString s)) = show (T.unpack s)
transpileExpr (Literal (TNumber n)) = show n
transpileExpr (Literal (TBoolean True)) = "True"
transpileExpr (Literal (TBoolean False)) = "False"
transpileExpr (Literal (TIdentifier name)) = T.unpack name
transpileExpr (List exprs) = "[" ++ intercalate ", " (map transpileExpr exprs) ++ "]"
transpileExpr (Dict pairs) = "{" ++ intercalate ", " (map transpileDictPair pairs) ++ "}"
transpileExpr (FuncCall name args) = T.unpack name ++ "(" ++ intercalate ", " (map transpileExpr args) ++ ")"
transpileExpr (BinaryOp op left right) = "(" ++ transpileExpr left ++ " " ++ transpileOperator op ++ " " ++ transpileExpr right ++ ")"
transpileExpr (Random min max) = "random.uniform(" ++ transpileExpr min ++ ", " ++ transpileExpr max ++ ")"
transpileExpr (Length expr) = "len(" ++ transpileExpr expr ++ ")"
transpileExpr (TypeConversion expr targetType) = transpileTypeConversion expr targetType
transpileExpr expr = error $ "Unsupported expression in transpiler: " ++ show expr

transpileDictPair :: (Expr, Expr) -> String
transpileDictPair (key, value) = transpileExpr key ++ ": " ++ transpileExpr value

transpileOperator :: T.Text -> String
transpileOperator "🍎" = "+"
transpileOperator "🍐" = "-"
transpileOperator "🍊" = "*"
transpileOperator "🍑" = "/"
transpileOperator "🥥" = "%"
transpileOperator "🐠" = "=="
transpileOperator "🦈" = "!="
transpileOperator "🐙" = ">"
transpileOperator "🦀" = "<"
transpileOperator "🦋" = "and"
transpileOperator "🐝" = "or"
transpileOperator "🐞" = "not"
transpileOperator op = error $ "Unsupported operator in transpiler: " ++ T.unpack op

transpileTypeConversion :: Expr -> Expr -> String
transpileTypeConversion expr (Literal (TIdentifier "string")) = "str(" ++ transpileExpr expr ++ ")"
transpileTypeConversion expr (Literal (TIdentifier "number")) = "float(" ++ transpileExpr expr ++ ")"
transpileTypeConversion expr (Literal (TIdentifier "boolean")) = "bool(" ++ transpileExpr expr ++ ")"
transpileTypeConversion _ targetType = error $ "Unsupported type conversion in transpiler: " ++ show targetType

indent :: String -> String
indent = unlines . map ("    " ++) . lines