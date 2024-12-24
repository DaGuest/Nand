module ExpressionEngine where

import Control.Applicative
import JackTokenizer
import TokenParser

-- EXPRESSIONS --

--  TERM
term :: Parser [String]
term = do
  t <- termExpr <|> termUnaryOp <|> subroutineCall <|> termList <|> termSingle
  return $ wrapXML "term" t

termExpr :: Parser [String]
termExpr = do
  exprHookOrBrack ("(", ")")

termList :: Parser [String]
termList = do
  v <- getWrappedToken <$> sat isVarNameToken
  e <- exprHookOrBrack ("[", "]")
  return (v : e)

termSingle :: Parser [String]
termSingle = do
  t <- getWrappedToken <$> sat isTermToken
  return [t]

termUnaryOp :: Parser [String]
termUnaryOp = do
  u <- getWrappedToken <$> sat isUnaryOpToken
  t <- term
  return (u : t)

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
  subroutineCallByName <|> subSubroutineCall

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
  return $ n : p : sr
