module Lexer (Token(..), lexer) where

import qualified Data.Text as T
import Data.Char (isAlphaNum)

data Token
  = TProgStart
  | TProgEnd
  | TComment T.Text
  | TIdentifier T.Text
  | TString T.Text
  | TNumber Double
  | TBoolean Bool
  | TIf
  | TElse
  | TEndIf
  | TLoop
  | TEndLoop
  | TFuncDef
  | TFuncEnd
  | TClassDef
  | TClassEnd
  | TOperator T.Text
  | TMovesIn
  | TLearns
  | TSays
  | TListens
  | TListStart
  | TListEnd
  | TDictStart
  | TDictEnd
  | TComma
  | TColon
  | TTryCatch
  | TCatch
  | TRandom
  | TLength
  | TTypeConversion
  | TEOF
  deriving (Show, Eq)

lexer :: T.Text -> [Token]
lexer input = lexHelper (T.unpack input) []

lexHelper :: String -> [Token] -> [Token]
lexHelper [] tokens = reverse (TEOF : tokens)
lexHelper (c:cs) tokens
  | c == '🏝️' = lexHelper cs (TProgStart : tokens)
  | c == '🛫' = lexHelper cs (TProgEnd : tokens)
  | c == '🎣' = lexComment cs tokens
  | c == '🤔' = lexHelper cs (TIf : tokens)
  | c == '🙃' = lexHelper cs (TElse : tokens)
  | c == '😌' = lexHelper cs (TEndIf : tokens)
  | c == '🏃' = lexHelper (dropWhile (=='‍') $ dropWhile (=='♂') cs) (TLoop : tokens)
  | c == '😴' = lexHelper cs (TEndLoop : tokens)
  | c == '🎁' = lexHelper cs (TFuncDef : tokens)
  | c == '🎀' = lexHelper cs (TFuncEnd : tokens)
  | c == '🏠' = lexHelper cs (TClassDef : tokens)
  | c == '🌴' = lexHelper cs (TListStart : tokens)
  | c == ',' = lexHelper cs (TComma : tokens)
  | c == ':' = lexHelper cs (TColon : tokens)
  | c `elem` "🍎🍐🍊🍑🥥🐠🦈🐙🦀🦋🐝🐞" = lexOperator (c:cs) tokens
  | c == '💬' = lexString cs tokens
  | c == '🔔' = lexNumber cs tokens
  | c == '🦉' = lexHelper cs (TBoolean True : tokens)
  | c == '🦝' = lexHelper cs (TBoolean False : tokens)
  | c == '🎭' = lexHelper cs (TTryCatch : tokens)
  | c == '🃏' = lexHelper cs (TCatch : tokens)
  | c == '🎲' = lexHelper cs (TRandom : tokens)
  | c == '📏' = lexHelper cs (TLength : tokens)
  | c == '🧪' = lexHelper cs (TTypeConversion : tokens)
  | isAlphaNum c = lexIdentifier (c:cs) tokens
  | otherwise = lexHelper cs tokens

lexComment :: String -> [Token] -> [Token]
lexComment cs tokens = 
  let (comment, rest) = span (/= '\n') cs
  in lexHelper rest (TComment (T.pack comment) : tokens)

lexOperator :: String -> [Token] -> [Token]
lexOperator cs tokens =
  let (op, rest) = span (`elem` "🍎🍐🍊🍑🥥🐠🦈🐙🦀🦋🐝🐞") cs
  in lexHelper rest (TOperator (T.pack op) : tokens)

lexString :: String -> [Token] -> [Token]
lexString cs tokens =
  let (str, rest) = span (/= '💬') cs
  in lexHelper (tail rest) (TString (T.pack str) : tokens)

lexNumber :: String -> [Token] -> [Token]
lexNumber cs tokens =
  let (num, rest) = span (/= '🔔') cs
  in lexHelper (tail rest) (TNumber (read num) : tokens)

lexIdentifier :: String -> [Token] -> [Token]
lexIdentifier cs tokens =
  let (ident, rest) = span isAlphaNum cs
      token = case ident of
        "moves" -> TMovesIn
        "learns" -> TLearns
        "says" -> TSays
        "listens" -> TListens
        _ -> TIdentifier (T.pack ident)
  in lexHelper rest (token : tokens)