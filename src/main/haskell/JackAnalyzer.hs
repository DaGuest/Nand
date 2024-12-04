import JackTokenizer
import System.Environment (getArgs)
import System.IO

main = do
  (args : _) <- getArgs
  contents <- readFile args
  print $ tokenize contents

-- TODO

-- A function to enumerate a folder and get all .jack files

-- A function to compile all the .jack files into the .xml output file
