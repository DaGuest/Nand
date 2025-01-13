module StatementEngine where

import Control.Applicative
import ExpressionEngine
import JackTokenizer
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
  sat (isGivenKeyToken "while")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  return $ "label WHILE_EXP" : eh ++ ["not", "if-goto WHILE_END"] ++ ss ++ ["goto WHILE_EXP", "label WHILE_END"]

--  IF STATEMENT
ifSt :: Parser [String]
ifSt = do
  sat (isGivenKeyToken "if")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  el <- concat <$> many elseSt
  return $ eh ++ ["not", "if-goto IF_FALSE"] ++ ss ++ ["goto IF_END", "label IF_FALSE"] ++ el ++ ["label IF_END"]

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
  e <- returnCheckZero . concat <$> many expr
  sat (isGivenSymbol ";")
  return e

--  STATEMENT HELPER FUNCTIONS

-- '{ statements }'
bracketStatements :: Parser [String]
bracketStatements = do
  sat (isGivenSymbol "{")
  ss <- statements
  sat (isGivenSymbol "}")
  return ss

--  COMPILER FUNCTIONS --

returnCheckZero :: [String] -> [String]
returnCheckZero [] = ["push constant 0", "return"]
returnCheckZero e = e ++ ["return"]

compileLet :: String -> [String] -> [String] -> [String]
compileLet vn [] e = e ++ ["pop " ++ vn]
compileLet vn eh e = ["push " ++ vn] ++ eh ++ e ++ ["pop temp 0", "pop pointer 1", "push temp 0", "pop that 0"]