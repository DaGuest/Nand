import Control.Monad (filterM)
import JackTokenizer
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO

main = do
  (args : _) <- getArgs
  files <- fileNames args
  fileContents <- mapM readFile files
  let fileNameWithTokens = zip files $ map tokenize fileContents
  mapM (\(f, t) -> writeFile "test.txt" (concat (map showTokens t))) fileNameWithTokens

-- TODO

-- Helper function that checks if the given argument is a folder
isFolder :: FilePath -> Bool
isFolder arg = takeExtension arg == ""

-- Enumerate a folder and get all .jack files
getFiles :: FilePath -> IO [FilePath]
getFiles dir = filterM (\n -> return $ takeExtension n == ".jack") =<< getDirectoryContents dir

-- Iterate through all the filenames
fileNames :: FilePath -> IO [FilePath]
fileNames dir
  | isFolder dir = getFiles dir
  | otherwise = return [dir]

-- A function to compile all the .jack files into the .xml output file
showTokens :: Token -> String
showTokens (TokIdent s) = "<identifier>" ++ s ++ "</identifier>\n"
showTokens (TokSymbol c) = "<symbol>" ++ [c] ++ "</symbol>\n"
showTokens (TokKey s) = "<keyword>" ++ s ++ "</keyword>\n"
showTokens (TokInt i) = "<keyword>" ++ "</keyword>\n"
showTokens (TokStr s) = "<string>" ++ s ++ "</string>\n"
