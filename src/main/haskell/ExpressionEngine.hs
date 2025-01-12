module ExpressionEngine where

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
  exprHookOrBrack ("(", ")")

termList :: Parser [String]
termList = do
  v <- getWrappedToken <$> sat isIdentToken
  e <- exprHookOrBrack ("[", "]")
  return (v : e)

termSingle :: Parser [String]
termSingle = do
  t <- compileSingleTerm <$> sat isTermToken
  return [t]

termUnaryOp :: Parser [String]
termUnaryOp = do
  u <- compileUnaryOpTerm <$> sat isUnaryOpToken
  t <- term
  return (t ++ [u])

--  EXPRESSION
expr :: Parser [String]
expr = do
  x <- term
  y <- many exprOpTerm
  return $ concat (x : y)

exprHookOrBrack :: (String, String) -> Parser [String]
exprHookOrBrack t = do
  sat (isGivenSymbol $ fst t)
  e <- expr
  sat (isGivenSymbol $ snd t)
  return e

exprOpTerm :: Parser [String]
exprOpTerm = do
  o <- op
  t <- term
  return $ t ++ [o]

exprList :: Parser [[String]]
exprList = do
  e <- many expr
  es <- many exprListMult
  return $ e ++ es

exprListMult :: Parser [String]
exprListMult = do
  sat (isGivenSymbol ",")
  expr

--  OP
op :: Parser String
op = do
  compileOpTerm <$> sat isOpToken

-- SUBROUTINECALL --
subroutineCall :: Parser [String]
subroutineCall = do
  s <- subroutineCallByName <|> subSubroutineCall
  return $ compileSubroutineCall s

subroutineCallByName :: Parser [String]
subroutineCallByName = do
  sn <- getTokenString <$> sat isIdentToken
  sat (isGivenSymbol "(")
  el <- exprList
  sat (isGivenSymbol ")")
  return $ (sn ++ " " ++ show (length el)) : concat el

subSubroutineCall :: Parser [String]
subSubroutineCall = do
  n <- getTokenString <$> sat isIdentToken
  p <- getTokenString <$> sat (isGivenSymbol ".")
  compileSubsubroutineCall (n ++ p) <$> subroutineCallByName

--  COMPILER FUNCTIONS --

compileSingleTerm :: Token -> String
compileSingleTerm (TokIdent s) = "push " ++ s
compileSingleTerm (TokInt i) = "push constant " ++ i
compileSingleTerm _ = "ERROR"

compileUnaryOpTerm :: Token -> String
compileUnaryOpTerm (TokSymbol "~") = "not "
compileUnaryOpTerm (TokSymbol "-") = "neg "

compileOpTerm :: Token -> String
compileOpTerm (TokSymbol s)
  | s == "+" = "add "
  | s == "-" = "sub "
  | s == "<" = "lt "
  | s == ">" = "gt "
  | s == "*" = "call Math.multiply 2"
  | s == "/" = "call Math.divide 2"
  | s == "|" = "or "
  | s == "&" = "and "
  | s == "=" = "eq "

compileSubsubroutineCall :: String -> [String] -> [String]
compileSubsubroutineCall prefix (x : xs) = (prefix ++ x) : xs

compileSubroutineCall :: [String] -> [String]
compileSubroutineCall (x : xs) = xs ++ ["call " ++ x]