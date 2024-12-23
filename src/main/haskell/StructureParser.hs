module StructureParser where

import CompilationEngine
import Control.Applicative
import JackTokenizer
import TokenParser

classDec :: Parser [String]
classDec = do
  c <- getWrappedToken <$> sat (isGivenKeyToken "class")
  cn <- getWrappedToken <$> sat isVarNameToken
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  cvd <- concat <$> many classVarDec
  srd <- concat <$> many subroutineDec
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ wrapXML "class" $ c : cn : hl : cvd ++ srd ++ [hr]

classVarDec :: Parser [String]
classVarDec = do
  h <- concat <$> many classVarDecHead
  t <- typespec
  vn <- getWrappedToken <$> sat isVarNameToken
  vnn <- concat <$> many vardecHelper
  e <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "classVarDec" $ h : t : vn : vnn ++ [e]

classVarDecHead :: Parser String
classVarDecHead = do
  getWrappedToken <$> (sat (isGivenKeyToken "static") <|> sat (isGivenKeyToken "field"))

vardec :: Parser [String]
vardec = do
  v <- getWrappedToken <$> sat (isGivenKeyToken "var")
  t <- typespec
  vn <- getWrappedToken <$> sat isVarNameToken
  m <- concat <$> many vardecHelper
  e <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "varDec" $ v : t : vn : m ++ [e]

vardecHelper :: Parser [String]
vardecHelper = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  vn <- getWrappedToken <$> sat isVarNameToken
  return [s, vn]

typespec :: Parser String
typespec = do
  getWrappedToken <$> (sat (isGivenKeyToken "int") <|> sat (isGivenKeyToken "char") <|> sat (isGivenKeyToken "boolean") <|> sat isVarNameToken)

subroutineDec :: Parser [String]
subroutineDec = do
  h <- subroutineDecHead
  t <- subroutineDecType
  n <- getWrappedToken <$> sat isVarNameToken
  pl <- getWrappedToken <$> sat (isGivenSymbol "(")
  p <- parameterList
  pr <- getWrappedToken <$> sat (isGivenSymbol ")")
  b <- subroutineBody
  return $ wrapXML "subroutineDec" $ h : t : n : pl : p ++ [pr] ++ b

subroutineDecHead :: Parser String
subroutineDecHead = do
  getWrappedToken <$> (sat (isGivenKeyToken "constructor") <|> sat (isGivenKeyToken "function") <|> sat (isGivenKeyToken "method"))

subroutineDecType :: Parser String
subroutineDecType = do
  (getWrappedToken <$> sat (isGivenKeyToken "void")) <|> typespec

subroutineBody :: Parser [String]
subroutineBody = do
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  v <- many vardec
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ wrapXML "subroutineBody" $ hl : concat v ++ ss ++ [hr]

parameterList :: Parser [String]
parameterList = do
  pl <- concat <$> many parameterListHead
  return $ wrapXML "parameterList" pl

parameterListHead :: Parser [String]
parameterListHead = do
  t <- typespec
  vn <- varName
  p <- concat <$> many parameterListHelper
  return $ t : vn : p

parameterListHelper :: Parser [String]
parameterListHelper = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  t <- typespec
  v <- varName
  return $ s : t : [v]

varName :: Parser String
varName = do
  getWrappedToken <$> sat isVarNameToken

eval :: [Token] -> [String]
eval ts = case parse classDec ts of
  [(xs, [])] -> xs
  [(xs, _)] -> xs
  [] -> error "Invalid input"