module ExpressionEngine where

import Control.Applicative
import Data.Char (isLower, isUpper, ord)
import JackTokenizer
import SymbolTable
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
  v <- compileSingleTerm <$> sat isIdentToken
  e <- exprHook
  return $ (v ++ e) ++ ["add", "pop pointer 1", "push that 0"]

termSingle :: Parser [String]
termSingle = do
  compileSingleTerm <$> sat isTermToken

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

exprHook :: Parser [String]
exprHook = do
  sat (isGivenSymbol "[")
  e <- expr
  sat (isGivenSymbol "]")
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
  compileSubsubroutineCall (n ++ p) <$> (subroutineCallByName <|> subSubroutineCall)

--  COMPILER FUNCTIONS --

compileSingleTerm :: Token -> [String]
compileSingleTerm (TokIdent s) = ["push " ++ s]
compileSingleTerm (TokInt i) = ["push constant " ++ i]
compileSingleTerm (TokKey "this") = ["push pointer 0"]
compileSingleTerm (TokKey "true") = ["push constant 0", "not"]
compileSingleTerm (TokKey "false") = ["push constant 0"]
compileSingleTerm (TokKey "null") = ["push constant 0"]
compileSingleTerm (TokStr s) = compileStringConstant s
compileSingleTerm _ = ["ERROR"]

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
compileSubroutineCall (x : xs) = let (a : as) = addTarget x in as ++ xs ++ [a]

compileStringConstant s = ["push constant " ++ show (length s), "call String.new 1"] ++ concatMap compileSingleStringConstant s

compileSingleStringConstant :: Char -> [String]
compileSingleStringConstant c = ["push constant " ++ show (ord c), "call String.appendChar 2"]

addTarget :: String -> [String]
addTarget s
  | length (split s '.') > 1 && head (split s '.') `elem` ["Memory", "Math", "String", "Screen", "Output", "Keyboard", "Array", "Sys"] = ["call " ++ s]
  | length (split s '.') > 1 && isLower (head $ head (split s '.')) = ["call " ++ addOne s, "push " ++ head (split s '.')]
  | length (split s '.') > 1 && isUpper (head $ head (split s '.')) = ["call " ++ s]
  | otherwise = ["call " ++ addOne s, "push pointer 0"]

addOne :: String -> String
addOne s = let sp = show ((read (last $ words s) :: Integer) + 1) in unwords $ reverse (sp : tail (reverse $ words s))