module StructureEngine where

import Control.Applicative
import ExpressionEngine
import JackTokenizer
import StatementEngine
import TokenParser

-- Parses a class declaration
classDec :: Parser [String]
classDec = do
  c <- getWrappedToken <$> sat (isGivenKeyToken "class")
  cn <- className
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  cvd <- concat <$> many classVarDec
  srd <- concat <$> many subroutineDec
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ wrapXML "class" $ c : cn : hl : cvd ++ srd ++ [hr]

-- Helper that takes care of variable declaration inside classes
classVarDec :: Parser [String]
classVarDec = do
  h <- concat <$> many classVarDecHead
  t <- typespec
  vn <- varName
  vnn <- concat <$> many vardecHelper
  e <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "classVarDec" $ h : t : vn : vnn ++ [e]

-- Helper for class variable declaration that determines static or field is given
classVarDecHead :: Parser String
classVarDecHead = do
  getWrappedToken <$> (sat (isGivenKeyToken "static") <|> sat (isGivenKeyToken "field"))

-- Parses variable declaration inside subroutines
vardec :: Parser [String]
vardec = do
  v <- getWrappedToken <$> sat (isGivenKeyToken "var")
  t <- typespec
  vn <- varName
  m <- concat <$> many vardecHelper
  e <- getWrappedToken <$> sat (isGivenSymbol ";")
  return $ wrapXML "varDec" $ v : t : vn : m ++ [e]

-- Helper for variable declaration that takes care of multiple variables declared
vardecHelper :: Parser [String]
vardecHelper = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  vn <- varName
  return [s, vn]

-- Helper that checks for different type specifications (int,char,boolean of other type)
typespec :: Parser String
typespec = do
  getWrappedToken <$> (sat (isGivenKeyToken "int") <|> sat (isGivenKeyToken "char") <|> sat (isGivenKeyToken "boolean")) <|> varName

-- Parses a subroutine declaration
subroutineDec :: Parser [String]
subroutineDec = do
  h <- subroutineDecHead
  t <- subroutineDecType
  n <- varName
  pl <- getWrappedToken <$> sat (isGivenSymbol "(")
  p <- parameterList
  pr <- getWrappedToken <$> sat (isGivenSymbol ")")
  b <- subroutineBody
  return $ wrapXML "subroutineDec" $ h : t : n : pl : p ++ [pr] ++ b

-- Helper for subroutine declaration that checks the head of the subroutine declaration (constructor, function or method)
subroutineDecHead :: Parser String
subroutineDecHead = do
  getWrappedToken <$> (sat (isGivenKeyToken "constructor") <|> sat (isGivenKeyToken "function") <|> sat (isGivenKeyToken "method"))

-- Helper to check the given subroutine type
subroutineDecType :: Parser String
subroutineDecType = do
  (getWrappedToken <$> sat (isGivenKeyToken "void")) <|> typespec

-- Parses the subroutine body
subroutineBody :: Parser [String]
subroutineBody = do
  hl <- getWrappedToken <$> sat (isGivenSymbol "{")
  v <- many vardec
  ss <- statements
  hr <- getWrappedToken <$> sat (isGivenSymbol "}")
  return $ wrapXML "subroutineBody" $ hl : concat v ++ ss ++ [hr]

-- Parses zero or multiple parameters declared in the subroutine title
parameterList :: Parser [String]
parameterList = do
  pl <- concat <$> many parameterListHead
  return $ wrapXML "parameterList" pl

-- A helper that takes care of the head of a parameter declaration
parameterListHead :: Parser [String]
parameterListHead = do
  t <- typespec
  vn <- varName
  p <- concat <$> many parameterListHelper
  return $ t : vn : p

-- A helper that takes care of multiple parameter declarations
parameterListHelper :: Parser [String]
parameterListHelper = do
  s <- getWrappedToken <$> sat (isGivenSymbol ",")
  t <- typespec
  vn <- varName
  return $ s : t : [vn]

-- Parses a variable name.
varName :: Parser String
varName = do
  getWrappedToken <$> sat isIdentToken

-- Parses a class name.
className :: Parser String
className = do
  getWrappedToken <$> sat isIdentToken