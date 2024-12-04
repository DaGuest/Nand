import Control.Arrow (Arrow (second))
import Data.Char
import System.IO

data Token
  = TokSymbol Char
  | TokIdent String
  | TokKey String
  | TokInt Int
  | TokStr String
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | c == '/' = comment c cs
  | c `elem` "+-*/&|<>=~[]()[].,;" = TokSymbol c : tokenize cs
  | c == '"' = strConstant cs
  | isDigit c = number c cs
  | isAlpha c = keywordOrIdent c cs
  | isSpace c = tokenize cs
  | otherwise = error $ "Cannot tokenize " ++ [c]

-- Get string constant out from between the "" chars.
strConstant :: [Char] -> [Token]
strConstant (c : cs) =
  let (str, _ : cs') = span isAlphaNum cs
   in TokStr (c : str) : tokenize cs'

-- Identifies multiple character and combines them into a single identifier.
identifier :: Char -> [Char] -> [Token]
identifier c cs =
  let (str, cs') = span isAlphaNumUnderscore cs
   in TokIdent (c : str) : tokenize cs'

keywordOrIdent :: Char -> [Char] -> [Token]
keywordOrIdent c cs
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
               "if",
               "else",
               "while",
               "return"
             ] =
      TokKey k : rest
  | otherwise = TokIdent k : rest
  where
    (TokIdent k : rest) = identifier c cs

-- Identifies multiple digits and turns them into a single number.
number :: Char -> [Char] -> [Token]
number c cs =
  let (digs, cs') = span isDigit cs
   in TokInt (read (c : digs)) : tokenize cs'

-- Identifies a comment (it should have \n at the end of the comment)
comment :: Char -> [Char] -> [Token]
comment c (c' : cs)
  | c' == '/' = skipTillNewLine cs
  | otherwise = TokSymbol c : tokenize (c' : cs)

-- A helper function to run though a comment until it encounters a newline symbol or the rest string is empty
skipTillNewLine (c : cs)
  | c == '\n' = tokenize cs
  | null cs = []
  | otherwise = skipTillNewLine cs

-- A helper function to check if a given char is a alphanum or '_' char.
isAlphaNumUnderscore :: Char -> Bool
isAlphaNumUnderscore c
  | c == '_' = True
  | otherwise = isAlphaNum c
