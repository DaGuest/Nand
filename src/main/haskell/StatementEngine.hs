module StatementEngine where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (isPrefixOf, isSuffixOf)
import ExpressionEngine
import JackTokenizer
import SymbolTable (split)
import TokenParser

-- STATEMENTS --
statements :: String -> Parser [String]
statements cn = do
  concat <$> many (statement cn)

statement :: String -> Parser [String]
statement cn = do
  letSt <|> ifSt cn <|> whileSt cn <|> doSt <|> returnSt

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
whileSt :: String -> Parser [String]
whileSt cn = do
  w <- getTokenString <$> sat isWhileToken
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements cn
  return $ ("label " ++ w ++ cn ++ "_EXP") : eh ++ ["not", "if-goto " ++ w ++ cn ++ "_END"] ++ ss ++ ["goto " ++ w ++ cn ++ "_EXP", "label " ++ w ++ cn ++ "_END"]

--  IF STATEMENT
ifSt :: String -> Parser [String]
ifSt cn = do
  i <- getTokenString <$> sat isIfToken
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements cn
  el <- concat <$> many (elseSt cn)
  return $ eh ++ ["not", "if-goto " ++ i ++ cn ++ "_FALSE"] ++ ss ++ ["goto " ++ i ++ cn ++ "_END", "label " ++ i ++ cn ++ "_FALSE"] ++ el ++ ["label " ++ i ++ cn ++ "_END"]

elseSt :: String -> Parser [String]
elseSt cn = do
  sat (isGivenKeyToken "else")
  bracketStatements cn

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
bracketStatements :: String -> Parser [String]
bracketStatements cn = do
  sat (isGivenSymbol "{")
  ss <- statements cn
  sat (isGivenSymbol "}")
  return ss

--  COMPILER FUNCTIONS --

compileReturn :: [String] -> [String]
compileReturn [] = ["push constant 0", "return"]
compileReturn e = e ++ ["return"]

compileLet :: String -> [String] -> [String] -> [String]
compileLet vn [] e = e ++ ["pop " ++ vn]
compileLet vn eh e = ["push " ++ vn] ++ eh ++ "add" : e ++ ["pop temp 0", "pop pointer 1", "push temp 0", "pop that 0"]