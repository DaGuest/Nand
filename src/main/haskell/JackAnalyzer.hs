import CompilationEngine
import Control.Monad (filterM)
import Data.List (intercalate)
import JackTokenizer
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO

-- Entry point for the compiler
main :: IO [()]
main = do
  (args : _) <- getArgs
  files <- fileNames args
  fileContents <- mapM readFile files
  let fileNameWithTokens = zip files $ map tokenize fileContents
  mapM (\(f, t) -> writeFile (replaceExtension f ".xml") $ intercalate "\n" $ compile t) fileNameWithTokens

-- Helper function that checks if the given argument is a folder
isFolder :: FilePath -> Bool
isFolder "." = True
isFolder arg = takeExtension arg == ""

-- Checks if argument has a path or not
hasPath :: FilePath -> Bool
hasPath arg = takeDirectory arg == ""

-- Checks if the given argument has a folder specification
getFolder :: FilePath -> IO FilePath
getFolder arg
  | takeDirectory arg == "" = getCurrentDirectory
  | otherwise = return $ takeDirectory arg

-- Enumerate a folder and get all .jack files
getFiles :: FilePath -> IO [FilePath]
getFiles dir = filterM (\n -> return $ takeExtension n == ".jack") =<< getDirectoryContents dir

-- Iterate through all the filenames
fileNames :: FilePath -> IO [FilePath]
fileNames arg
  | isFolder arg = do
      let d = takeDirectory arg
      let e = d ++ "/" ++ takeFileName arg
      fns <- getFiles e
      return $ map (\fn -> e ++ "/" ++ fn) fns
  | otherwise = do
      let d = takeDirectory arg
      return [d ++ "/" ++ takeFileName arg]