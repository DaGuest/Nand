module CompilationEngine where

import Control.Applicative
import JackTokenizer
import TokenParser

-- EXPRESSIONS --

--  TERM
term :: Parser [String]
term = do
  termExpr <|> termUnaryOp <|> subroutineCall <|> termList <|> termSingle

termExpr :: Parser [String]
termExpr = do
  e <- exprHookOrBrack ("(", ")")
  return $ wrapXML "term" e

termList :: Parser [String]
termList = do
  v <- getWrappedToken <$> sat isVarNameToken
  e <- exprHookOrBrack ("[", "]")
  return $ wrapXML "term" (v : e)

termSingle :: Parser [String]
termSingle = do
  t <- getWrappedToken <$> sat isTermToken
  return $ wrapXML "term" [t]

termUnaryOp :: Parser [String]
termUnaryOp = do
  u <- getWrappedToken <$> sat isUnaryOpToken
  t <- term
  return $ wrapXML "term" (u : t)

--  EXPRESSION
expr :: Parser [String]
expr = do
  x <- term
  y <- many exprOpTerm
  return $ wrapXML "expression" $ concat (x : y)

exprHookOrBrack :: (String, String) -> Parser [String]
exprHookOrBrack t = do
  pl <- getWrappedToken <$> sat (isGivenSymbol $ fst t)
  e <- expr
  pr <- getWrappedToken <$> sat (isGivenSymbol $ snd t)
  return $ (pl : e) ++ [pr]

exprOpTerm :: Parser [String]
exprOpTerm = do
  x <- op
  z <- term
  return $ x : z

exprList :: Parser [String]
exprList = do
  e <- concat <$> many expr
  es <- concat <$> many exprListMult
  return $ wrapXML "expressionList" $ e ++ es

exprListMult :: Parser [String]
exprListMult = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  e <- expr
  return (s : e)

--  OP
op :: Parser String
op = do
  getWrappedToken <$> sat isOpToken

-- SUBROUTINECALL --
subroutineCall :: Parser [String]
subroutineCall = do
  s <- subroutineCallByName <|> subSubroutineCall
  return $ wrapXML "subroutineCall" s

subroutineCallByName :: Parser [String]
subroutineCallByName = do
  sn <- getWrappedToken <$> sat isVarNameToken
  pl <- getWrappedToken <$> sat (isGivenSymbol "(")
  el <- exprList
  pr <- getWrappedToken <$> sat (isGivenSymbol ")")
  return $ (sn : pl : el) ++ [pr]

subSubroutineCall :: Parser [String]
subSubroutineCall = do
  n <- getWrappedToken <$> sat isVarNameToken
  p <- getWrappedToken <$> sat (isGivenSymbol ".")
  sr <- subroutineCallByName
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ n : p : sr ++ [z]

-- STATEMENTS --
statements :: Parser [String]
statements = do
  letSt <|> ifSt <|> whileSt <|> doSt <|> returnSt

--  LET STATEMENT
letSt :: Parser [String]
letSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "let")
  x <- getWrappedToken <$> sat isVarNameToken
  t <- many $ exprHookOrBrack ("[", "]")
  y <- getWrappedToken <$> sat (isGivenSymbol "=")
  e <- expr
  z <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ k : x : concat t ++ (y : e) ++ [z]

--  WHILE STATEMENT
whileSt :: Parser [String]
whileSt = do
  w <- getWrappedToken <$> sat (isGivenKeyToken "while")
  eh <- exprHookOrBrack ("(", ")")
  ss <- bracketStatements
  return $ w : eh ++ ss

--  IF STATEMENT
ifSt :: Parser [String]
ifSt = do
  -- 'if'
  k <- getWrappedToken <$> sat (isGivenKeyToken "if")
  -- '( expression )'
  eh <- exprHookOrBrack ("(", ")")
  -- '{ statements }'
  ss <- bracketStatements
  -- ( 'else { statements }' )? (0 or 1)
  el <- concat <$> many elseSt
  return $ k : eh ++ ss ++ el

elseSt :: Parser [String]
elseSt = do
  k <- getWrappedToken <$> sat (isGivenKeyToken "else")
  ss <- bracketStatements
  return $ k : ss

--  DO STATEMENT
doSt :: Parser [String]
doSt = do
  d <- getWrappedToken <$> sat (isGivenKeyToken "do")
  sb <- subSubroutineCall
  return $ wrapXML "doStatement" $ d : sb

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
