module SymbolTable where

import Data.List (intercalate, isPrefixOf)

type Name = String

type TypeName = String

type Kind = String

type Index = String

type Entry = (Name, TypeName, Kind, Index)

splitStaticAndField :: [String] -> [[String]]
splitStaticAndField xs = filter (isPrefixOf "field") xs : [filter (isPrefixOf "static") xs]

renameField :: [String] -> [String]
renameField ("field" : xs) = "this" : xs
renameField xs = xs

labelVars :: [String] -> [[String]]
labelVars xs = map (\(n, v : vs) -> vs ++ [v ++ " " ++ show n]) (zip [0 .. (length xs)] (map (renameField . words) xs))

labelStaticAndField :: [String] -> [[String]]
labelStaticAndField = concatMap labelVars . splitStaticAndField

addLabel :: String -> [String] -> [[String]]
addLabel labelName = labelVars . map ((labelName ++ " ") ++)

lookUpVar :: String -> [Entry] -> [Entry]
lookUpVar varName = filter (isVarName varName)

isVarName :: String -> Entry -> Bool
isVarName varName (x, _, _, _) = x == varName

getVarCode :: String -> [Entry] -> String
getVarCode varName ts
  | lookUpVar varName ts == [] = varName
  | otherwise = let x = head $ lookUpVar varName ts in getKind x ++ " " ++ getIndex x

replaceLastVar :: String -> String -> String
replaceLastVar var s = unwords $ reverse $ var : tail (reverse (words s))

replaceVar :: [Entry] -> [String] -> [String]
replaceVar ts = map (\s -> replaceLastVar (getVarCode (last $ words s) ts) s)

createClassEntryTable :: [[String]] -> [Entry]
createClassEntryTable = map (\(x : xs) -> let lastWord = words $ last xs in (head xs, x, head lastWord, last lastWord))

createEntryTable :: [[String]] -> [Entry]
createEntryTable = map (\(x : xs) -> let lastWord = words $ last xs in (x, head xs, head lastWord, last lastWord))

getName :: Entry -> String
getName (n, _, _, _) = n

getType :: Entry -> String
getType (_, t, _, _) = t

getKind :: Entry -> String
getKind (_, _, k, _) = k

getIndex :: Entry -> String
getIndex (_, _, _, i) = i

replaceClassType :: [Entry] -> String -> String -> String
replaceClassType ts cn s
  | isPrefixOf "call" s && (length (split s '.') == 1) = let varName = head $ split (head $ tail $ words s) '.' in getTypeName varName ts s cn
  | isPrefixOf "call" s && (length (split s '.') > 1) = let varName = head $ split (head $ tail $ words $ head (split s '.')) '.' in getTypeUpper varName ts s
  | otherwise = s

getTypeName :: String -> [Entry] -> String -> String -> String
getTypeName varName ts orig cn
  | lookUpVar varName ts == [] = addClassNameToCall orig cn
  | otherwise = "call " ++ getType (head $ lookUpVar varName ts) ++ "." ++ last (split varName '.')

getTypeUpper varName ts orig
  | lookUpVar varName ts == [] = orig
  | otherwise = "call " ++ getType (head $ lookUpVar varName ts) ++ "." ++ last (split orig '.')

addClassNameToCall :: String -> String -> String
addClassNameToCall com cn = let wordList = words com in concat $ head wordList : " " : cn : "." : [unwords (tail wordList)]

split :: String -> Char -> [String]
split "" _ = []
split xs c =
  let (ys, zs) = break (== c) xs
   in if null zs then [ys] else ys : split (tail zs) c