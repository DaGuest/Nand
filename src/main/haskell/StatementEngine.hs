module StatementEngine where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (isPrefixOf, isSuffixOf)
import ExpressionEngine
import JackTokenizer
import SymbolTable (split)
import TokenParser

-- STATEMENTS --
statements :: Parser [String]
statements = do
  concat <$> many statement

statement :: Parser [String]
statement = do
  letSt <|> ifSt <|> whileSt <|> doSt <|> returnSt

--  LET STATEMENT
letSt :: Parser [String]
letSt = do
  sat (isGivenKeyToken "let")
  vn <- getTokenString <$> sat isIdentToken
  eh <- concat <$> many exprHook
  sat (isGivenSymbol "=")
  e <- expr
  sat (isGivenSymbol ";")
  return $ compileLet vn eh e

--  WHILE STATEMENT
whileSt :: Parser [String]
whileSt = do
  w <- getTokenString <$> sat isWhileToken
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  return $ ("label " ++ w ++ "_EXP") : eh ++ ["not", "if-goto " ++ w ++ "_END"] ++ ss ++ ["goto " ++ w ++ "_EXP", "label " ++ w ++ "_END"]

--  IF STATEMENT
ifSt :: Parser [String]
ifSt = do
  i <- getTokenString <$> sat isIfToken
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  el <- concat <$> many elseSt
  return $ eh ++ ["not", "if-goto " ++ i ++ "_FALSE"] ++ ss ++ ["goto " ++ i ++ "_END", "label " ++ i ++ "_FALSE"] ++ el ++ ["label " ++ i ++ "_END"]

elseSt :: Parser [String]
elseSt = do
  sat (isGivenKeyToken "else")
  bracketStatements

--  DO STATEMENT
doSt :: Parser [String]
doSt = do
  sat (isGivenKeyToken "do")
  sb <- subroutineCall
  sat (isGivenSymbol ";")
  return $ sb ++ ["pop temp 0"]

--  RETURN STATEMENT
returnSt :: Parser [String]
returnSt = do
  sat (isGivenKeyToken "return")
  e <- concat <$> many expr
  sat (isGivenSymbol ";")
  return $ compileReturn e

--  STATEMENT HELPER FUNCTIONS

-- '{ statements }'
bracketStatements :: Parser [String]
bracketStatements = do
  sat (isGivenSymbol "{")
  ss <- statements
  sat (isGivenSymbol "}")
  return ss

--  COMPILER FUNCTIONS --

compileReturn :: [String] -> [String]
compileReturn [] = ["push constant 0", "return"]
compileReturn e = e ++ ["return"]

compileLet :: String -> [String] -> [String] -> [String]
compileLet vn [] e = e ++ ["pop " ++ vn]
compileLet vn eh e = ["push " ++ vn] ++ eh ++ e ++ ["pop temp 0", "pop pointer 1", "push temp 0", "pop that 0"]