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
  k <- getWrappedToken <$> sat (isGivenKeyToken "let")
  vn <- getWrappedToken <$> sat isIdentToken
  eh <- many $ exprHookOrBrack ("[", "]")
  y <- getWrappedToken <$> sat (isGivenSymbol "=")
  e <- expr
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "letStatement" $ k : vn : concat eh ++ (y : e) ++ [z]

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
  r <- getWrappedToken <$> sat (isGivenKeyToken "return")
  e <- concat <$> many expr
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "returnStatement" $ r : e ++ [z]

--  STATEMENT HELPER FUNCTIONS

-- '{ statements }'
bracketStatements :: Parser [String]
bracketStatements = do
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ hl : ss ++ [hr]