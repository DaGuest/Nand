module CompilationEngine where

import JackTokenizer
import StructureEngine
import TokenParser

-- Compiles the given list of tokens into a list of xml strings
compile :: [Token] -> [String]
compile ts = case parse classDec ts of
  [(xs, [])] -> xs
  [(xs, _)] -> xs
  [] -> error "Invalid input"