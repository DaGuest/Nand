module StructureEngine where

import Control.Applicative
import ExpressionEngine
import JackTokenizer
import StatementEngine
import SymbolTable
import TokenParser

-- Parses a class declaration
classDec :: Parser [String]
classDec = do
  sat (isGivenKeyToken "class")
  cn <- className
  sat (isGivenSymbol "{")
  st <- createClassEntryTable . labelStaticAndField . concat <$> many classVarDec
  srd <- concat <$> many (subroutineDec st cn)
  sat (isGivenSymbol "}")
  return srd

-- Helper that takes care of variable declaration inside classes
classVarDec :: Parser [String]
classVarDec = do
  -- list of class variable declarations
  h <- getTokenString <$> (sat (isGivenKeyToken "static") <|> sat (isGivenKeyToken "field"))
  t <- typespec
  vn <- varName
  vnn <- concat <$> many vardecTail
  sat (isGivenSymbol ";")
  return $ map ((h ++ " " ++ t ++ " ") ++) (vn : vnn)

-- Parses variable declaration inside subroutines
vardec :: Parser [String]
vardec = do
  sat (isGivenKeyToken "var")
  t <- typespec
  vn <- varName
  m <- concat <$> many vardecTail
  sat (isGivenSymbol ";")
  return $ (vn ++ " " ++ t ++ " ") : m

-- Helper for variable declaration that takes care of multiple variables declared
vardecTail :: Parser [String]
vardecTail = do
  sat (isGivenSymbol ",")
  vn <- varName
  return [vn]

-- Helper that checks for different type specifications (int,char,boolean of other type)
typespec :: Parser String
typespec = do
  getTokenString <$> (sat (isGivenKeyToken "int") <|> sat (isGivenKeyToken "char") <|> sat (isGivenKeyToken "boolean")) <|> varName

-- Parses a subroutine declaration
subroutineDec :: [Entry] -> String -> Parser [String]
subroutineDec st cn = do
  -- Method or constructor head
  h <- subroutineDecHead
  t <- subroutineDecType
  n <- varName
  -- Parameters for the function
  sat (isGivenSymbol "(")
  p <- createEntryTable . addLabel "argument" <$> parameterList h
  sat (isGivenSymbol ")")
  sat (isGivenSymbol "{")
  -- Subroutine body
  v <- createEntryTable . addLabel "local" . concat <$> many vardec
  ss <- map (replaceClassType (v ++ p ++ st) cn) . replaceVar (v ++ p ++ st) <$> statements
  sat (isGivenSymbol "}")
  return $ compileSubroutine h cn t n (length v) (length p) (length $ filter (\(_, _, k, _) -> k == "this") st) ++ ss

-- Helper for subroutine declaration that checks the head of the subroutine declaration (constructor, function or method)
subroutineDecHead :: Parser String
subroutineDecHead = do
  getTokenString <$> (sat (isGivenKeyToken "constructor") <|> sat (isGivenKeyToken "function") <|> sat (isGivenKeyToken "method"))

-- Helper to check the given subroutine type
subroutineDecType :: Parser String
subroutineDecType = do
  (getTokenString <$> sat (isGivenKeyToken "void")) <|> typespec

-- Parses zero or multiple parameters declared in the subroutine title
parameterList :: String -> Parser [String]
parameterList h = do
  p <- concat <$> many parameterListHead
  return $ addObjectToParameter h p

-- A helper that takes care of the head of a parameter declaration
parameterListHead :: Parser [String]
parameterListHead = do
  t <- typespec
  vn <- varName
  p <- concat <$> many parameterListTail
  return $ (vn ++ " " ++ t ++ " ") : p

-- A helper that takes care of multiple parameter declarations
parameterListTail :: Parser [String]
parameterListTail = do
  sat (isGivenSymbol ",")
  t <- typespec
  vn <- varName
  return [vn ++ " " ++ t ++ " "]

-- Parses a variable name.
varName :: Parser String
varName = do
  getTokenString <$> sat isIdentToken

-- Parses a class name.
className :: Parser String
className = do
  getTokenString <$> sat isIdentToken

--  COMPILER FUNCTIONS --

compileSubroutine :: String -> String -> String -> String -> Int -> Int -> Int -> [String]
compileSubroutine "constructor" cn t v varlength argLength cargLength = ["function " ++ t ++ "." ++ v ++ " " ++ show varlength, "push constant " ++ show cargLength, "call Memory.alloc 1", "pop pointer 0"]
compileSubroutine "method" cn t v varlength argLength _ = ["function " ++ cn ++ "." ++ v ++ " " ++ show varlength, "push argument 0", "pop pointer 0"]
compileSubroutine "function" cn t v varlength argLength cargLength = ["function " ++ cn ++ "." ++ v ++ " " ++ show varlength]

addObjectToParameter :: String -> [String] -> [String]
addObjectToParameter "method" cs = "this" : cs
addObjectToParameter _ cs = cs
