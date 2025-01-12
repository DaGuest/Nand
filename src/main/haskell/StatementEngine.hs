module StatementEngine where

import Control.Applicative
import ExpressionEngine
import JackTokenizer
import TokenParser

-- STATEMENTS --
statements :: Parser [String]
statements = do
  s <- concat <$> many statement
  return $ wrapXML "statements" s

statement :: Parser [String]
statement = do
  letSt <|> ifSt <|> whileSt <|> doSt <|> returnSt

--  LET STATEMENT
letSt :: Parser [String]
letSt = do
  getWrappedToken <$> sat (isGivenKeyToken "let")
  vn <- getTokenString <$> sat isIdentToken
  eh <- many $ exprHookOrBrack ("[", "]") -- Aanpassen want dit zorgt voor een 'push _' output
  getWrappedToken <$> sat (isGivenSymbol "=")
  e <- expr
  getWrappedToken <$> sat (isGivenSymbol ";")
  return $ e ++ ["pop " ++ vn]

--  WHILE STATEMENT
whileSt :: Parser [String]
whileSt = do
  w <- getWrappedToken <$> sat (isGivenKeyToken "while")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  return $ wrapXML "whileStatement" $ w : eh ++ ss

--  IF STATEMENT
ifSt :: Parser [String]
ifSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "if")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  el <- concat <$> many elseSt
  return $ wrapXML "ifStatement" $ k : eh ++ ss ++ el

elseSt :: Parser [String]
elseSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "else")
  ss <- bracketStatements
  return $ k : ss

--  DO STATEMENT
doSt :: Parser [String]
doSt = do
  d <- getWrappedToken <$> sat (isGivenKeyToken "do")
  sb <- subroutineCall
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "doStatement" $ d : sb ++ [z]

--  RETURN STATEMENT
returnSt :: Parser [String]
returnSt = do
  getWrappedToken <$> sat (isGivenKeyToken "return")
  e <- returnCheckZero . concat <$> many expr
  getWrappedToken <$> sat (isGivenSymbol ";")
  return e

--  STATEMENT HELPER FUNCTIONS

-- '{ statements }'
bracketStatements :: Parser [String]
bracketStatements = do
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ hl : ss ++ [hr]

returnCheckZero :: [String] -> [String]
returnCheckZero [] = ["push constant 0", "return"]
returnCheckZero e = e ++ ["return"]
