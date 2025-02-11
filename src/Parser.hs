module Parser (Expr(..), parse) where

import Lexer (Token(..))
import qualified Data.Text as T

data Expr
  = Program [Expr]
  | VarDecl T.Text Expr
  | VarAssign T.Text Expr
  | FuncCall T.Text [Expr]
  | If Expr [Expr] [Expr]
  | Loop Expr [Expr]
  | FuncDef T.Text [T.Text] [Expr]
  | Literal Token
  deriving (Show)

parse :: [Token] -> Expr
parse tokens = parseProgram tokens

parseProgram :: [Token] -> Expr
parseProgram (TProgStart : rest) =
  let (body, TProgEnd : _) = break (== TProgEnd) rest
  in Program (parseStatements body)
parseProgram _ = error "Program must start with 🏝️"

parseStatements :: [Token] -> [Expr]
parseStatements [] = []
parseStatements tokens =
  let (stmt, rest) = parseStatement tokens
  in stmt : parseStatements rest

parseStatement :: [Token] -> (Expr, [Token])
parseStatement (TIdentifier name : TString "moves in:" : value : rest) =
  (VarDecl name (parseLiteral value), rest)
parseStatement (TIdentifier name : TString "learns:" : value : rest) =
  (VarAssign name (parseLiteral value), rest)
parseStatement (TIf : cond : body) =
  let (ifBody, elseBody) = break (== TElse) body
      (elseStmts, rest) = span (/= TEndIf) (drop 1 elseBody)
  in (If (parseLiteral cond) (parseStatements ifBody) (parseStatements elseStmts), tail rest)
parseStatement (TLoop : cond : body) =
  let (loopBody, rest) = span (/= TEndLoop) body
  in (Loop (parseLiteral cond) (parseStatements loopBody), tail rest)
parseStatement (TFuncDef : TIdentifier name : params) =
  let (paramNames, body) = span (\t -> case t of TIdentifier _ -> True; _ -> False) params
      (funcBody, rest) = span (/= TFuncEnd) body
  in (FuncDef name (map (\(TIdentifier t) -> t) paramNames) (parseStatements funcBody), tail rest)
parseStatement tokens = error $ "Unexpected tokens: " ++ show tokens

parseLiteral :: Token -> Expr
parseLiteral t@(TString _) = Literal t
parseLiteral t@(TNumber _) = Literal t
parseLiteral t@(TBoolean _) = Literal t
parseLiteral t@(TIdentifier _) = Literal t
parseLiteral t = error $ "Unexpected token: " ++ show t