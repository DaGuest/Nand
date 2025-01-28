module JackTokenizer where

import Data.Char
import System.IO

data Token
  = TokSymbol String
  | TokIdent String
  | TokKey String
  | TokInt String
  | TokStr String
  | TokCond Int String
  deriving (Show, Eq, Ord)

tokenize :: Int -> String -> [Token]
tokenize i [] = []
tokenize i (c : cs)
  | c == '/' = comment i c cs
  | c `elem` "+-*/&|<>=~{}()[].,;" = TokSymbol [c] : tokenize (i + 1) cs
  | c == '"' = strConstant i cs
  | isDigit c = number i c cs
  | isAlpha c = keywordOrIdent i c cs
  | isSpace c = tokenize (i + 1) cs
  | c == '\n' = tokenize (i + 1) cs
  | otherwise = error $ "Symbol not recognized: " ++ [c] ++ " - Add this symbol to tokenizer."

-- Get string constant out from between the "" chars.
strConstant :: Int -> [Char] -> [Token]
strConstant i (c : cs) =
  let (str, _ : cs') = span isStrChar cs
   in TokStr (c : str) : tokenize (i + 1) cs'

-- Identifies multiple character and combines them into a single identifier.
identifier :: Int -> Char -> [Char] -> [Token]
identifier i c cs =
  let (str, cs') = span isAlphaNumUnderscore cs
   in TokIdent (c : str) : tokenize (i + 1) cs'

-- Checks for known keywords, if not it is labeled as identifier
keywordOrIdent :: Int -> Char -> [Char] -> [Token]
keywordOrIdent i c cs
  | k
      `elem` [ "class",
               "constructor",
               "function",
               "method",
               "field",
               "static",
               "var",
               "int",
               "char",
               "boolean",
               "void",
               "true",
               "false",
               "null",
               "this",
               "let",
               "do",
               "else",
               "return"
             ] =
      TokKey k : rest
  | k `elem` ["while", "if"] = TokCond i k : rest
  | otherwise = TokIdent k : rest
  where
    (TokIdent k : rest) = identifier i c cs

-- Identifies multiple digits and turns them into a single number.
number :: Int -> Char -> [Char] -> [Token]
number i c cs =
  let (digs, cs') = span isDigit cs
   in TokInt (c : digs) : tokenize (i + 1) cs'

-- Identifies a comment.
comment :: Int -> Char -> [Char] -> [Token]
comment i c (c' : cs)
  | c' == '/' = skipTillNewLine i cs
  | c' == '*' = skipTillClosingComment i cs
  | otherwise = TokSymbol [c] : tokenize (i + 1) (c' : cs)

skipTillClosingComment :: Int -> [Char] -> [Token]
skipTillClosingComment i ('*' : '/' : cs) = tokenize (i + 1) cs
skipTillClosingComment i (_ : cs) = skipTillClosingComment i cs

-- A helper function to run though a comment until it encounters a newline symbol or the rest string is empty
skipTillNewLine :: Int -> [Char] -> [Token]
skipTillNewLine i (c : cs)
  | c == '\n' = tokenize (i + 1) cs
  | null cs = []
  | otherwise = skipTillNewLine i cs

-- A helper function to check if a given char is a alphanum or '_' char.
isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c
  | c == '_' = True
  | otherwise = isAlphaNum c

isStrChar :: Char -> Bool
isStrChar c
  | c == '"' = False
  | otherwise = True

getTokenString :: Token -> String
getTokenString (TokIdent s) = s
getTokenString (TokKey s) = s
getTokenString (TokSymbol s) = s
getTokenString (TokStr s) = s
getTokenString (TokInt s) = s
getTokenString (TokCond i s) = s ++ show i

-- GENERAL HELPER FUNCTIONS]

isTermToken :: Token -> Bool
isTermToken (TokKey t) = t `elem` ["true", "false", "null", "this"]
isTermToken (TokInt _) = True
isTermToken (TokIdent _) = True
isTermToken (TokStr _) = True
isTermToken _ = False

isOpToken :: Token -> Bool
isOpToken (TokSymbol s) = s `elem` ["<", ">", "=", "+", "-", "*", "/", "&", "|"]
isOpToken _ = False

isGivenKeyToken :: String -> Token -> Bool
isGivenKeyToken s (TokKey t) = t == s
isGivenKeyToken _ _ = False

isIdentToken :: Token -> Bool
isIdentToken (TokIdent _) = True
isIdentToken _ = False

isGivenSymbol :: String -> Token -> Bool
isGivenSymbol s (TokSymbol t) = t == s
isGivenSymbol _ _ = False

isUnaryOpToken :: Token -> Bool
isUnaryOpToken (TokSymbol s) = s `elem` ["~", "-"]
isUnaryOpToken _ = False

isWhileToken :: Token -> Bool
isWhileToken (TokCond _ "while") = True
isWhileToken (TokCond _ _) = False
isWhileToken _ = False

isIfToken :: Token -> Bool
isIfToken (TokCond _ c)
  | c == "if" = True
  | otherwise = False
isIfToken _ = False
