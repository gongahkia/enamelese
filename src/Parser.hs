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
  | ClassDef T.Text [Expr]
  | Literal Token
  | List [Expr]
  | Dict [(Expr, Expr)]
  | Print Expr
  | Input T.Text
  | BinaryOp T.Text Expr Expr
  | TryCatch [Expr] T.Text [Expr]
  | Random Expr Expr
  | Length Expr
  | TypeConversion Expr Expr
  deriving (Show)

parse :: [Token] -> Expr
parse tokens = fst $ parseProgram tokens

parseProgram :: [Token] -> (Expr, [Token])
parseProgram (TProgStart : rest) =
  let (body, remaining) = parseStatements rest
  in case remaining of
       (TProgEnd : rest') -> (Program body, rest')
       _ -> error "Program must end with 🛫"
parseProgram _ = error "Program must start with 🏝️"

parseStatements :: [Token] -> ([Expr], [Token])
parseStatements tokens = 
  case parseStatement tokens of
    (stmt, TProgEnd : rest) -> ([stmt], TProgEnd : rest)
    (stmt, rest) -> 
      let (stmts, remaining) = parseStatements rest
      in (stmt : stmts, remaining)

parseStatement :: [Token] -> (Expr, [Token])
parseStatement (TIdentifier name : TMovesIn : value : rest) =
  let (expr, remaining) = parseExpr value rest
  in (VarDecl name expr, remaining)
parseStatement (TIdentifier name : TLearns : value : rest) =
  let (expr, remaining) = parseExpr value rest
  in (VarAssign name expr, remaining)
parseStatement (TIdentifier name : TSays : value : rest) =
  let (expr, remaining) = parseExpr value rest
  in (Print expr, remaining)
parseStatement (TIdentifier name : TListens : rest) =
  (Input name, rest)
parseStatement (TIf : rest) =
  let (cond, thenBody) = parseExpr (head rest) (tail rest)
      (thenStmts, elseTokens) = parseStatements thenBody
      (elseStmts, remaining) = case elseTokens of
        (TElse : rest') -> parseStatements rest'
        _ -> ([], elseTokens)
  in (If cond thenStmts elseStmts, remaining)
parseStatement (TLoop : rest) =
  let (cond, loopBody) = parseExpr (head rest) (tail rest)
      (stmts, remaining) = parseStatements loopBody
  in (Loop cond stmts, remaining)
parseStatement (TFuncDef : TIdentifier name : rest) =
  let (params, body) = span (\t -> case t of TIdentifier _ -> True; _ -> False) rest
      paramNames = map (\(TIdentifier t) -> t) params
      (stmts, remaining) = parseStatements body
  in (FuncDef name paramNames stmts, remaining)
parseStatement (TClassDef : TIdentifier name : rest) =
  let (body, remaining) = parseStatements rest
  in (ClassDef name body, remaining)
parseStatement (TTryCatch : rest) =
  let (tryBody, catchTokens) = break (== TCatch) rest
      (tryStmts, _) = parseStatements tryBody
      (TCatch : TIdentifier errName : catchBody) = catchTokens
      (catchStmts, remaining) = parseStatements catchBody
  in (TryCatch tryStmts errName catchStmts, remaining)
parseStatement tokens = parseExpr (head tokens) (tail tokens)

parseExpr :: Token -> [Token] -> (Expr, [Token])
parseExpr (TListStart) rest =
  let (items, remaining) = parseList rest
  in (List items, remaining)
parseExpr (TDictStart) rest =
  let (pairs, remaining) = parseDict rest
  in (Dict pairs, remaining)
parseExpr (TRandom) (a : b : rest) =
  let (exprA, _) = parseExpr a []
      (exprB, _) = parseExpr b []
  in (Random exprA exprB, rest)
parseExpr (TLength) (a : rest) =
  let (expr, _) = parseExpr a []
  in (Length expr, rest)
parseExpr (TTypeConversion) (a : b : rest) =
  let (exprA, _) = parseExpr a []
      (exprB, _) = parseExpr b []
  in (TypeConversion exprA exprB, rest)
parseExpr token rest =
  case rest of
    (TOperator op : value : remaining) ->
      let (left, _) = parseExpr token []
          (right, _) = parseExpr value []
      in (BinaryOp op left right, remaining)
    _ -> (Literal token, rest)

parseList :: [Token] -> ([Expr], [Token])
parseList tokens = parseListItems tokens []

parseListItems :: [Token] -> [Expr] -> ([Expr], [Token])
parseListItems (TListEnd : rest) acc = (reverse acc, rest)
parseListItems (item : TComma : rest) acc =
  let (expr, _) = parseExpr item []
  in parseListItems rest (expr : acc)
parseListItems (item : rest) acc =
  let (expr, _) = parseExpr item []
  in parseListItems rest (expr : acc)
parseListItems [] _ = error "Unterminated list"

parseDict :: [Token] -> ([(Expr, Expr)], [Token])
parseDict tokens = parseDictItems tokens []

parseDictItems :: [Token] -> [(Expr, Expr)] -> ([(Expr, Expr)], [Token])
parseDictItems (TDictEnd : rest) acc = (reverse acc, rest)
parseDictItems (key : TColon : value : TComma : rest) acc =
  let (keyExpr, _) = parseExpr key []
      (valueExpr, _) = parseExpr value []
  in parseDictItems rest ((keyExpr, valueExpr) : acc)
parseDictItems (key : TColon : value : rest) acc =
  let (keyExpr, _) = parseExpr key []
      (valueExpr, _) = parseExpr value []
  in parseDictItems rest ((keyExpr, valueExpr) : acc)
parseDictItems [] _ = error "Unterminated dictionary"
parseDictItems _ _ = error "Invalid dictionary syntax"