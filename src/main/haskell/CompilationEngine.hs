module CompilationEngine where

import Control.Applicative (Alternative (many))
import Data.List (sort)
import JackTokenizer
import StructureEngine
import TokenParser

-- Compiles the given list of tokens into a list of xml strings
compile :: [Token] -> [String]
compile ts = case parse classDec ts of
  [(xs, [])] -> xs
  [(xs, _)] -> xs
  [] -> error "Invalid input"

tester :: Parser [String]
tester = do
  t <- concat <$> many classVarDec
  return $ wrapXML "test" t