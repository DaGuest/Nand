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
  eh <- many $ exprHookOrBrack ("[", "]") -- Aanpassen want dit zorgt voor een 'push _' output
  sat (isGivenSymbol "=")
  e <- expr
  sat (isGivenSymbol ";")
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
  sat (isGivenKeyToken "if")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  el <- concat <$> many elseSt
  return $ eh ++ ["not", "if-goto IF-LABEL"] ++ ss ++ ["goto ELSE-LABEL", "IF-LABEL"] ++ el ++ ["ELSE-LABEL"]

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

returnCheckZero :: [String] -> [String]
returnCheckZero [] = ["push constant 0", "return"]
returnCheckZero e = e ++ ["return"]
