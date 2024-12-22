import CompilationEngine
import Control.Monad (filterM)
import Data.List (intercalate)
import JackTokenizer
import StructureParser
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO

main = do
  (args : _) <- getArgs
  files <- fileNames args
  fileContents <- mapM readFile files
  let fileNameWithTokens = zip files $ map tokenize fileContents
  -- mapM (\(f, t) -> writeFile (replaceExtension f ".xml") $ concatMap showTokens t) fileNameWithTokens
  mapM (\(f, t) -> writeFile (replaceExtension f ".xml") $ intercalate "\n" $ eval t) fileNameWithTokens

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
