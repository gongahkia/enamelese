module Lexer (Token(..), lexer) where

import qualified Data.Text as T

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
  | TOperator T.Text
  deriving (Show, Eq)

lexer :: T.Text -> [Token]
lexer input = lexHelper (T.unpack input) []

lexHelper :: String -> [Token] -> [Token]
lexHelper [] tokens = reverse tokens
lexHelper (c:cs) tokens
  | c == '🏝️' = lexHelper cs (TProgStart : tokens)
  | c == '🛫' = lexHelper cs (TProgEnd : tokens)
  | c == '🎣' = lexComment cs tokens
  | c == '🤔' = lexHelper cs (TIf : tokens)
  | c == '🙃' = lexHelper cs (TElse : tokens)
  | c == '😌' = lexHelper cs (TEndIf : tokens)
  | c == '🏃' = lexHelper cs (TLoop : tokens)
  | c == '😴' = lexHelper cs (TEndLoop : tokens)
  | c == '🎁' = lexHelper cs (TFuncDef : tokens)
  | c == '🎀' = lexHelper cs (TFuncEnd : tokens)
  | c `elem` "🍎🍐🍊🍑🥥🐠🦈🐙🦀🦋🐝🐞" = lexOperator (c:cs) tokens
  | otherwise = lexHelper cs tokens

lexComment :: String -> [Token] -> [Token]
lexComment cs tokens = 
  let (comment, rest) = span (/= '\n') cs
  in lexHelper rest (TComment (T.pack comment) : tokens)

lexOperator :: String -> [Token] -> [Token]
lexOperator cs tokens =
  let (op, rest) = span (`elem` "🍎🍐🍊🍑🥥🐠🦈🐙🦀🦋🐝🐞") cs
  in lexHelper rest (TOperator (T.pack op) : tokens)